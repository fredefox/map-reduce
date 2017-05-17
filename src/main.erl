-module(main).
-compile(export_all).

% -define(SITE, "http://erlang.org/").
% -define(SITE, "http://web.student.chalmers.se/~hanghj/map-reduce/a").
% -define(SITE, "http://127.0.0.1:8080").
-define(SITE, "http://politiken.dk").
-define(DEPTH, 2).
-define(DATA_FILE, "./web.dat").

dbg(Frmt) -> dbg(Frmt, []).
dbg(Frmt, X) -> io:format(Frmt, X).

main() -> benchmark().

benchmark() ->
    [timer:tc(page_rank,Atom,[?DATA_FILE]) ||
	Atom <- [page_rank,page_rank_par,page_rank_dist]].
