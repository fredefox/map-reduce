-module(dist_map_reduce).
-compile(export_all).

-record(io_info, {num_reducers = 1, num_mappers = 1, nodes = nodes()}).

map_reduce(Map, Reduce, Input, IO_Info) ->
  Chunks = par_map:split_into(IO_Info#io_info.num_mappers, Input),
  Call = make_ref(),
  Master = self(),
  Reducers = spawn_reducers(Reduce, Call, Master, IO_Info),
  spawn_mappers(Map, Chunks, Call, Reducers, IO_Info),
  lists:sort(receive_reducer_output(Call, IO_Info#io_info.num_reducers)).

spawn_reducers(Reduce, Call, Master, IO_Info) ->
  Nodes = IO_Info#io_info.nodes,
  NumReducers = IO_Info#io_info.num_reducers,
  NumMappers = IO_Info#io_info.num_mappers,
  spawn_reducers(Reduce,Call, Master, Nodes, Nodes, NumReducers, NumMappers).

spawn_reducers(Reduce,Call,Master,[],Nodes,NR, NM) ->
  spawn_reducers(Reduce, Call, Master, Nodes, Nodes, NR, NM);
spawn_reducers(_,_,_,_,_, 0, _) -> [];
spawn_reducers(Reduce,Call,Master,[N|Ns], Nodes, NR, NM) ->
  [rpc:call(N, erlang, spawn, [fun () -> reducer(Reduce,Call,Master,NM) end]) ||
   spawn_reducers(Reduce,Call,Master,Ns,Nodes,NR-1, NM)].

spawn_mappers(Map, Chunks, Call, Reducers, IO_Info) ->
  Zipd = zip_round_robin(Chunks, IO_Info#io_info.nodes),
  Red = list_to_tuple(Reducers),
  [rpc:call(N, erlang, spawn, [fun () -> mapper(Map, C, Red, Call) end]) || {C, N} <- Zipd].
%%DS[k] += v

% Reducers :: Array Pid
mapper(F, Xs, Reducers, Call) ->
  N = length(Reducers),
  [ R ! {Call, more_stuff,  X}
    || X = {K, _} <- lists:map(F, Xs)
    ,  RIndx = erlang:phash2(K, N)
    ,  R <- element(RIndx+1, Reducers)
  ],
  [ R ! {Call, done} || R <- tuple_to_list(Reducers)].

zip_round_robin(As, Bs) -> zip_round_robin(As, Bs, Bs).
zip_round_robin([], _, _) -> [];
zip_round_robin(As, [], Bs2) -> zip_round_robin(As, Bs2, Bs2);
zip_round_robin([A|As], [B|Bs], Bs2) -> [{A, B} | zip_round_robin(As, Bs, Bs2)].

receive_reducer_output(Call, NR) ->
  [receive {Call,KV} -> KV end || _ <- lists:seq(1,NR)].

reducer(Reduce,Call,Master,NM) ->
  Mapper_Outputs = receive_mapper_output(Call,NM),
  KLists = group(lists:sort(lists:concat(Mapper_Outputs))),
  Master ! {Call,[{Key, Reduce(List)} || {Key,List} <- KLists]}.

receive_mapper_output(Call,NM) -> receive_mapper_output(Call,NM, []).

receive_mapper_output(_,0,Kvs) -> Kvs;
receive_mapper_output(Call, NM, Kvs) ->
%   [receive {Call,KV} -> KV end || _ <- lists:seq(1,NM)].
  receive
    {Call, more_stuff, Kv} ->
      receive_mapper_output(Call,NM, [Kv | Kvs]);
    {Call, done} -> receive_mapper_output(Call,NM-1,Kvs)
  end.

group([]) -> [];
group([{K, V} | Xs]) -> group(K, [V], Xs).
group(K, Vs, []) -> [{K,Vs}];
group(K, Vs, [{K,V}|Xs])-> group(K, [V|Vs], Xs);
group(K, Vs, [{K2,V}|Xs]) -> [{K,Vs} | group(K2,[V],Xs)].
