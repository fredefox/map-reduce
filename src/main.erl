-module(main).
-compile(export_all).

% -define(SITE, "http://erlang.org/").
% -define(SITE, "http://web.student.chalmers.se/~hanghj/map-reduce/a").
-define(SITE, "http://127.0.0.1:8080").
-define(DEPTH, 4).

dbg(Frmt) -> dbg(Frmt, []).
dbg(Frmt, X) -> io:format(Frmt, X).

main() ->
  inets:start(),
  Res = crawl:crawl(?SITE, ?DEPTH),
  dbg("~p~n", [Res]),
  {ok, F} = dets:open_file("./web.dat", []),
  dets:insert(F, Res),
  Lkp = dets:lookup(F, ?SITE),
  inets:stop(),
  Lkp.
