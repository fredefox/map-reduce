Distributed map-reduce
======================

Description of implementation
-----------------------------
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
