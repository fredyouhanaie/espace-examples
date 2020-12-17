# barrier

A barrier allows multiple processes working on a computation to
sychronize with each other before proceeding to the next stage.

The processes using the barrier do not need to be part of an `espace`
application. They just need an `espace` server to be running before
the barrier. The server can be a one off instance, or one that is
started with the application and kept running for multiple barrier
calls.

## barrier_0 (butterfly barrier)

The first of the examples is the `barrier_0` module, which implements
the butterfly barrier. This is an efficient algorithm that involves
pair-wise synchronisation of processes, where each process in the
group only needs to synchronize with `log2(N)` processes for a group
of `N` processes.

Each process within the group needs to know the total number of
processes in the group (`N`). The processes are numbered starting from
`0` to `N-1`. These values can be communicated to the individual
processes at the start, or at some point before they need to
synchronize.

Also needed is a barrier tag that all the processes will use during
the synchronization. The tag can be any erlang `term()`, and it helps
identify the barrier related tuples within the tuple space. if the
processes are likely to go through multiple barriers, then it would be
best to use a different `Tag` for each, perhaps something like `{Tag,
Phase}`.

## Build and test

As with most Erlang projects, `rebar3` is used for building and
testing the code:

    $ rebar3 compile
	$ rebar3 dialyzer
	$ rebar3 eunit
	$ rebar3 edoc
