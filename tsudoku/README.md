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

* `tsudoku2` is an alternative solver that keeps all the cell data in the tuple
  space. In contrast to `tsudoku` where each cell worker maintains its own data.

Further modules will provide more complex solvers.

## Trying it out

`rebar3` is used throughout for all project work, e.g.

    $ rebar3 dialyzer
    $ rebar3 eunit
    $ rebar3 edoc

    $ rebar3 shell
    > tsudoku1:solve_file("test/puzzle_4x4_1.dat").
    > espace:rdp({done, '$1'}). %% may return nomatch, if solution not found (yet)
    > espace:stop(). %% required before another run of the solver

There is also an `escript` to do everything from command line:

    $ rebar3 esciptize
    $ ./_build/default/bin/tsudoku --help
    $ ./_build/default/bin/tsudoku solve test/puzzle_4x4_1.dat

## Documentation

The main documentation is in the modules. Use `rebar3 edoc` to
generate the `prod` docs, or `rebar3 as dev edoc` to include all the
functions.

Once the doc is generated, use the browser to view the local file
`doc/index.html`.

Enjoy!

Fred
