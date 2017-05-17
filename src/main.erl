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
    Map = fun page_rank:map/2,
    Reduce = fun page_rank:reduce/2,
    UBs = page_rank:dets_stuff("./web.dat"),
    [{T1,V},{T2,V},{T3,V2}] = 
    %[timer:tc(page_rank,Atom,[?DATA_FILE])
    % || Atom <- [page_rank,page_rank_par,page_rank_dist]],
    [timer:tc(map_reduce,map_reduce_seq,[Map,Reduce,UBs]),
     timer:tc(map_reduce,map_reduce_par,[Map,32,Reduce,32,UBs]),
     timer:tc(dist_map_reduce,map_reduce,[Map,Reduce,UBs,
					  {io_info,32,32,[node()]}])],
    io:format("foo ~p~nbar ~p~n",[length(V),length(V2)]),
    [T1,T2,T3].
