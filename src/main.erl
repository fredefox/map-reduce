-module(main).
-compile(export_all).

% -define(SITE, "http://erlang.org/").
% -define(SITE, "http://web.student.chalmers.se/~hanghj/map-reduce/a").
% -define(SITE, "http://127.0.0.1:8080").
% -define(SITE, "http://politiken.dk").
-define(SITE, "http://su.dk").
-define(DEPTH, 2).
-define(DATA_FILE, "./web.dat").

dbg(Frmt) -> dbg(Frmt, []).
dbg(Frmt, X) -> io:format(Frmt, X).

main() -> benchmark().

crawl() ->
    crawl(?SITE, ?DEPTH).

crawl(S, D) ->
    inets:start(),
    Res = crawl:crawl(S, D),
    {ok, F} = dets:open_file(?DATA_FILE, []),
    dets:insert(F, Res),
    inets:stop().

benchmark() ->
    Res =
	[{Atom, timer:tc(page_rank,Atom,[?DATA_FILE])}
	 || Atom <- [page_rank,page_rank_par,page_rank_dist]],
    % Lens = [ {A, T, length(V)} || {A, {T, V}} <- Res],
    % io:format("~p~n", [Lens]),
    % Ensure correctness:
    [ X | Xs ] = [ V || {_, {_, V}} <- Res],
    [ X = Y || Y <- Xs],
    Res1 = [ {A, T} || {A, {T, _}} <- Res],
    % io:format("foo ~p~nbar ~p~n",[length(V),length(V2)]),
    io:format("Runtime: ~p~n", [Res1]),
    [{_, Idx}|_] = Res1,
    Res2 = [ {A, T / Idx} || {A, T} <- Res1],
    io:format("Speedups: ~p~n", [Res2]).
