%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This implements a page rank algorithm using map-reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(page_rank).
-compile(export_all).

%% Use map_reduce to count word occurrences

% This function assumes that the file "web.dat" is in the CWD.
map(Url,Body) ->
    Urls = crawl:find_urls(Url,Body),
    [{U,1} || U <- Urls].

reduce(Url,Ns) ->
    [{Url,lists:sum(Ns)}].

page_rank(File) ->
    map_reduce:map_reduce_seq(fun map/2, fun reduce/2, dets_stuff(File)).

page_rank_par(File) ->
    map_reduce:map_reduce_par(fun map/2, 32, fun reduce/2, 32,
			      dets_stuff(File)).

-record(io_info, {num_reducers = 1, num_mappers = 1, nodes = nodes()}).

page_rank_dist(File) ->
    UBs = dets_stuff(File),
    Nodes = [node(self()) | nodes()],
    % Emulate multiple nodes:
    % Nodes = dist_map_reduce:repeat(40, node(self())),
    Nfo = #io_info{num_reducers = 32, num_mappers = 32, nodes = Nodes},
    dist_map_reduce:map_reduce(fun map/2, fun reduce/2, UBs, Nfo).

dets_stuff(File) ->
    {ok,web} = dets:open_file(web,[{file,File}]),
    dets:foldl(fun(KV,KVs)->[KV|KVs] end,[],web).
