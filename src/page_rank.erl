%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This implements a page rank algorithm using map-reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(page_rank).
-compile(export_all).

%% Use map_reduce to count word occurrences

% This function assumes that the file "web.dat" is in the CWD.
map(Url,ok) ->
    dets:open_file(web,[{file,"web.dat"}]),
    [{Url,Body}] = dets:lookup(web,Url),
    Urls = crawl:find_urls(Url,Body),
    [{U,1} || U <- Urls].

reduce(Url,Ns) ->
    [{Url,lists:sum(Ns)}].

page_rank(File) ->
    {ok,web} = dets:open_file(web,[{file,File}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_seq(fun map/2, fun reduce/2,
                  [{Url,ok} || Url <- Urls]).

page_rank_par(File) ->
    dets:open_file(web,[{file,File}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    map_reduce:map_reduce_par(fun map/2, 32, fun reduce/2, 32,
                  [{Url,ok} || Url <- Urls]).

-record(io_info, {num_reducers = 1, num_mappers = 1, nodes = nodes()}).

page_rank_dist(File) ->
    dets:open_file(web,[{file,File}]),
    Urls = dets:foldl(fun({K,_},Keys)->[K|Keys] end,[],web),
    Nfo = #io_info{num_reducers = 32, num_mappers = 32, nodes = [node(self()) | nodes()]},
    dist_map_reduce:map_reduce(fun map/2, fun reduce/2,
                              [{Url,ok} || Url <- Urls], Nfo).
