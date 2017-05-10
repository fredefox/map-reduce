-module(dist_map_reduce).
-compile(export_all).

-record(io_info, {num_reducers = 1, num_mappers = 1, nodes = [node(self()) | nodes()]}).

% The maximum number of workers to spawn simultaneously.
% No more than the amount of available workers will be
% spawned at a give time.
-define(POOL_SIZE, 100).

map_reduce(Map, Reduce, Input, IO_Info) ->
  Chunks = map_reduce:split_into(IO_Info#io_info.num_mappers, Input),
  Call = make_ref(),
  Master = self(),
  Reducers = spawn_reducers(Reduce, Call, Master, IO_Info),
  spawn_mappers(Map, Chunks, Call, Reducers, IO_Info),
  Res = receive_reducer_output(Call, IO_Info#io_info.num_reducers),
  lists:sort(lists:flatten(Res)).

spawn_reducers(Reduce, Call, Master, IO_Info) ->
  Nodes = IO_Info#io_info.nodes,
  NumReducers = IO_Info#io_info.num_reducers,
  NumMappers = IO_Info#io_info.num_mappers,
%  Ns = zip_round_robin(lists:seq(1, NumReducers), Nodes),
  F  = fun() -> reducer(Reduce, Call, Master, NumMappers) end,
  Funs  = repeat(NumReducers, F),
  worker_pool(Nodes, ?POOL_SIZE, Funs).
%  [spawn_link(N, F) || {_, N} <- Ns].

repeat(0, _) -> [];
repeat(N, X) -> [X | repeat(N-1, X)].

spawn_mappers(Map, Chunks, Call, Reducers, IO_Info) ->
  Ns = IO_Info#io_info.nodes,
  Red = list_to_tuple(Reducers),
  Funs = [fun () -> mapper(Map, C, Red, Call) end || C <- Chunks],
  worker_pool(Ns, ?POOL_SIZE, Funs).

% Workers are spawned in a round-robin fashion from the worker pool - the first
% argument to worker_pool/3. An empty pool is an error - we must be able to make
% progress. When the total number of active workers exceed the size of the pool
% this call blocks until another worker becomes available.

% TODO With our current implementation the mappers need to synchronously hand
% off work to the reducers. This is a problem since if the size of the worker
% pool is smaller than the number of mappers (here functions to execute) then
% they will block because the reducers are never spawned and hence cannot
% receive the work that the mappers want to hand off.
worker_pool([], _, _) -> error(badarg);
worker_pool(Pool, N, Fs) -> worker_pool(Pool, Pool, N, Fs).

worker_pool(Pool, Xs, 0, Fs) ->
  receive {done, _} -> worker_pool(Pool, Xs, 1, Fs) end;
worker_pool(Pool, [], N, Fs) ->
    worker_pool(Pool, Pool, N + 1, Fs);
worker_pool(_, _, _, []) -> [];
worker_pool(Pool, [Nd | Nds], N, [F | Fs]) ->
  [ worker(Nd, F) | worker_pool(Pool, Nds, N - 1, Fs) ].

worker(N, Work) ->
  Master = self(),
  F = fun() -> Work(), Master ! {done, N} end,
  restart_on_death(N, F).

% TODO The pid of the process spawned after death will be lost and therefore
% tamper with the correctness of map-reduce.
restart_on_death(N, F) ->
  Pid = spawn_link(N, F),
  on_exit(Pid, fun(Reason) -> restart_on_death(Reason, N, F) end),
  Pid.

restart_on_death(normal, _, _) -> ok;
restart_on_death(_, N, F) -> restart_on_death(N, F).

on_exit(Pid,Fun) ->
    spawn(fun() -> process_flag(trap_exit,true),
                   link(Pid),
                   receive
                       {'EXIT',Pid,Why} -> Fun(Why)
                   end
          end).

% Reducers :: Array Pid
mapper(F, Xs, Reducers, Call) ->
  N = tuple_size(Reducers),
    [ element((erlang:phash2(K2, N))+1, Reducers) ! {Call, more_stuff, Y}
    % TODO Two problems here; 1) F has arity 2 and 2) it returns a list of pairs, not a pair.
    || {K, V} <- Xs
    ,  Y = {K2, _} <- F(K, V)
  ],
  [ R ! {Call, done} || R <- tuple_to_list(Reducers)].

zip_round_robin(_, []) -> error(badarg);
zip_round_robin(As, Bs) -> zip_round_robin(As, Bs, Bs).
zip_round_robin([], _, _) -> [];
zip_round_robin(As, [], Bs2) -> zip_round_robin(As, Bs2, Bs2);
zip_round_robin([A|As], [B|Bs], Bs2) -> [{A, B} | zip_round_robin(As, Bs, Bs2)].

receive_reducer_output(Call, NR) ->
  [receive {Call,KV} -> KV end || _ <- lists:seq(1,NR)].

reducer(Reduce,Call,Master,NM) ->
  Mapper_Outputs = receive_mapper_output(Call,NM),
  KLists = group(lists:sort(Mapper_Outputs)),
  Reply = [Kv || {K,Vs} <- KLists, Kv <- Reduce(K, Vs)],
  Master ! {Call,Reply}.

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
