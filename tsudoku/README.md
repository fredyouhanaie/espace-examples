# tsudoku

Tuple space based sudoku solvers.

_This is work in progress. And, is likely to be for sometime._

The goal is to provide examples of sudoku solvers that perform as much
of the work as is possible concurrently, using `espace` as the main
coordination facility among the concurrent processes. Additionally, we
can also use these solvers to assess the performance of various
`espace` patterns.

The current implementation include two main modules:

* `tsudoku_lib` provides most of the general functions that can be
  used by various types solvers.

* `tsudoku1` is the first of the solver modules. It implements the
  simplest method of solving the puzzle.

Further modules will provide more complex solvers.

## Trying it out

`rebar3` is used throughout for all project work, e.g.

    $ rebar3 dialyzer
    $ rebar3 eunit
    $ rebar3 edoc
    $ rebar3 shell

## Documentation

The main documentation is in the modules. Use `rebar3 edoc` to
generate the `prod` docs, or `rebar3 as dev edoc` to include all the
functions.

Once the doc is generated, use the browser to view the local file
`doc/index.html`.

Enjoy!

Fred
