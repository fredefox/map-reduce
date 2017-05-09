%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This module defines a simple web crawler using map-reduce.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(crawl).
-compile(export_all).

dbg(Frmt) -> dbg(Frmt, []).
dbg(Frmt, X) -> io:format(Frmt, X).
prnt1(X) -> dbg("~p~n", [X]).

%% Crawl from a URL, following links to depth D.
%% Before calling this function, the inets service must
%% be started using inets:start().
crawl(Url, D) -> crawlUsing(Url, D, fun map_reduce:map_reduce_par/5).

crawlUsing(Url,D,F) ->
    Pages = followUsing(D,[{Url,undefined}],F),
    [{U,Body} || {U,Body} <- Pages, Body /= undefined].

followUsing(0,KVs,_) ->
    KVs;
followUsing(D,KVs,F) ->
    followUsing(D-1,
       F(fun map/2, 20, fun reduce/2, 1, KVs), F).

map(Url,undefined) ->
    Body = fetch_url(Url),
    Urls = find_urls(Url,Body),
    [{Url,Body}] ++ [{U,undefined} || U <- Urls];
map(Url,Body) ->
    [{Url,Body}].

reduce(Url,Bodies) ->
    case [B || B <- Bodies, B/=undefined] of
    [] ->
        [{Url,undefined}];
    [Body] ->
        [{Url,Body}]
    end.

fetch_url(Url) ->
    case httpc:request(Url) of
    {ok,{_,_Headers,Body}}  ->
        Body;
    _ ->
        ""
    end.

%% Find all the urls in an Html page with a given Url.
find_urls(Url,Html) ->
    Lower = string:to_lower(Html),
    %% Find all the complete URLs that occur anywhere in the page
    Absolute = case re:run(Lower,"http://.*?(?=\")",[global]) of
           {match,Locs} ->
               [lists:sublist(Html,Pos+1,Len)
            || [{Pos,Len}] <- Locs];
           _ ->
               []
           end,
    %% Find links to files in the same directory, which need to be
    %% turned into complete URLs.
    % WARN: Relative links are not handled correctly. href=a from site /a
    % resolves to a/a, but should refer to the document itself.
    Relative = case re:run(Lower,"href *= *\"(?!http:).*?(?=\")",[global]) of
           {match,RLocs} ->
               [lists:sublist(Html,Pos+1,Len)
            || [{Pos,Len}] <- RLocs];
           _ ->
               []
           end,
    Absolute ++ [Url++"/"++
             lists:dropwhile(
               fun(Char)->Char==$/ end,
               tl(lists:dropwhile(fun(Char)->Char/=$" end, R)))
         || R <- Relative].
