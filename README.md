---
title: Distributed map-reduce
author:
- William Hughes
- Frederik Hanghøj Iversen
date: Thu May 11, 2017
...

Implementation
==============

Our implementation can be found in `src/dist_map_reduce.erl`.

Our implementation works by divying up up the input-list into `M` chunks
corresponding to the amount of mappers (a parameter to `map_reduce/4`). Then `R`
reducers are put into a pool (`R` being the number of reducers to spawn). This
pool works by spawning at most `POOL_SIZE` amount of workers at a given time.
Spawning processes in this pool will block upon reaching this cap.

After the reducers have been spawned the pids of these processes are passed unto
the mappers. The mappers needs the pids of the reducers as they hand off work to
them in a synchronized fashion (reducers block on input coming from each mapper,
and will continue once all mappers have reported that they have handed off all
data).

If any worker dies it will simply be respawned. This is problematic since if a
reducer dies and is then respawned, then the PID of that worker changes and this
will not be reported to mapper.

The limitations are listed in the backlog[^1] included in this report.

Testing setup
=============

The method that we are testing is page-rank of already downloaded
search-results.

Please note that the dets file `web.dat` must already be present on all nodes
running this program.

The test-results that we submit stem from running the program on one dual-core
machine. The output of the benchmark is:

    (master@ultrafox)1> main:main().
    Runtime: [{page_rank,5653846},
              {page_rank_par,3549150},
              {page_rank_dist,3262322}]
    Speedups: [{page_rank,1.0},
               {page_rank_par,0.6277408334079139},
               {page_rank_dist,0.577009349034268}]
    ok

The first output is the absolute run-time and the second output is the runtime
relative to the first run (the sequential version).

The `web.dat` cotains ~10M of data:

    § du -h src/web.dat
    9,9M	src/web.dat

When we ran the tests previously we had less data in `web.dat` and we believe
that increasing the size of this file has enabled us to demonstrate a speed-up.
The map-reduce algorithm requires that there is a substantial amount of work to
be done so that it can be split up.

The result of the distributed version is comparable to that of the parallel
version - which can be attributed to the fact that there is only one physical
node.

Conclusion
----------

Our implementation suffers from some serious issues (c.f. the backlog) and as we
have not run the program on multiple machines nor tested failure mechanism we
can also not conclude anything about that.

[^1]: [`./BACKLOG.md`](./BACKLOG.md).
