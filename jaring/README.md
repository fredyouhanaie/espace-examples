# `jaring` - The ring benchmark from Joe Armstrong's book

This is the `espace` based implementation of the ring test from the
problem section (8.11) of Joe Armstrong's book:

> Armstrong, J. Programming Erlang, Software for a Concurrent World, 2007, p150.

The problem statement is as follows:

> Write a ring benchmark. Create `N` processes in a ring. Send a
> message round the ring `M` times so that a total of `N x M` messages
> get sent. Time how long this takes for different values of `N` and
> `M`.

## How it works

The benchmark is performed with `N+1` worker processes, the first node (node
`0`) and `N` relay nodes (`1..N`).

Each message being sent through the ring is a single integer. Its value starts
from `M` and is decremented before each new round. The last message to go around
has value `1`.

All communication between the nodes is carried out via the `espace` tuple space.
A `send` is an `espace:out/1` operation with the destination node number as the
first element, while a `receive` is an `espace:in/` operation. Node workers
receive messages by waiting for tuples with their own node number, i.e.
`{Node_num, Msg}`.


## Build and test 

All testing and verification is achieved with `rebar`. Please ensure that youve
a recent version of `rebar3` available.

    $ rebar3 compile
    $ rebar3 dialyzer
    $ rebar3 eunit
    $ rebar3 shell

From within the shell use `jaring:start()` or `jaring:start(N, M)` to run the
test for `N` nodes and `M` messages. If not supplied, 3 and 4 are assumed for
`N` and `M`, respectively. Both `start/0` and `start/2` return a `map` of
performance values.

---

