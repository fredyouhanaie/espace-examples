# `jaring` - The ring benchmark from Joe Armstrong's book

This is the `espace` based implementation of the ring test from the
problem section (8.11) of Joe Armstrong's book:

> Armstrong, J. Programming Erlang, Software for a Concurrent World, 2007, p150.

The problem statement is as follows:

> Write a ring benchmark. Create `N` processes in a ring. Send a
> message round the ring `M` times so that a total of `N x M` messages
> get sent. Time how long this takes for different values of `N` and
> `M`.

## Build and test 

All testing and verification is achieved with `rebar`. Please ensure that youve
a recent version of `rebar3` available.

    $ rebar3 compile
    $ rebar3 dialyzer
    $ rebar3 eunit
    $ rebar3 shell

From within the shell use `jaring:start()` or `jaring:start(N, M)` to run the
test for `N` nodes and `M` messages. If not supplied, 3 and 4 are assumed for
`N` and `M`, respectively.

---

