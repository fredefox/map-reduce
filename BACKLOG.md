Backlog
=======

* With our current implementation the mappers need to synchronously hand off
  work to the reducers. This is a problem since if the size of the worker pool
  is smaller than the number of mappers (here functions to execute) then they
  will block because the reducers are never spawned and hence cannot receive the
  work that the mappers want to hand off.
