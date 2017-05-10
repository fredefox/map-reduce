Backlog
=======

* With our current implementation the mappers need to synchronously hand off
  work to the reducers. This is a problem since if the size of the worker pool
  is smaller than the number of mappers (here functions to execute) then they
  will block because the reducers are never spawned and hence cannot receive the
  work that the mappers want to hand off.

  Cf.: `dist_map_reduce:worker_pool/3`

* The pid of the process spawned after death will be lost and therefore tamper
  with the correctness of map-reduce.

  Cf.: `dist_map_reduce:restart_on_death/2`
