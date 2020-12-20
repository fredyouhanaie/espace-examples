# queens - solves the Eight Queens puzzle

## Introduction

From the [Wikipedia](https://en.wikipedia.org/wiki/Eight_queens_puzzle) description:

	The eight queens puzzle is the problem of placing eight chess queens
	on an 8×8 chessboard so that no two queens threaten each other; thus,
	a solution requires that no two queens share the same row, column, or
	diagonal.

For this example we start with a simple solver for 8 queens,
`queens8`. A generalized version for `N` queens will be added in due
course.

The best way to try this is to clone the repo, and run rebart from this directory.

The main documentation is in the source code. Use `rebar3` to produce and view it:

	$ rebar3 edoc
	$ firefox doc/index.html # or any browser you prefer

The solver can be run using the `rebar3 shell`. The solver runs in the
background, and the results are available in the tuple space
immediately. Use the `print_solution/0` to print the results, one
solution at a time. For 8 queens there are 92 distinct solutions.

	$ rebar3 shell

	1> queens8:start().
    done
    2> queens8:print_solution().
    +-+-+-+-+-+-+-+-+
    |X| | | | | | | |
    +-+-+-+-+-+-+-+-+
    | | | | |X| | | |
    +-+-+-+-+-+-+-+-+
    | | | | | | | |X|
    +-+-+-+-+-+-+-+-+
    | | | | | |X| | |
    +-+-+-+-+-+-+-+-+
    | | |X| | | | | |
    +-+-+-+-+-+-+-+-+
    | | | | | | |X| |
    +-+-+-+-+-+-+-+-+
    | |X| | | | | | |
    +-+-+-+-+-+-+-+-+
    | | | |X| | | | |
    +-+-+-+-+-+-+-+-+
    ok
    3> queens8:print_solution().
    +-+-+-+-+-+-+-+-+
    |X| | | | | | | |
    +-+-+-+-+-+-+-+-+
    | | | | | |X| | |
    +-+-+-+-+-+-+-+-+
    | | | | | | | |X|
    +-+-+-+-+-+-+-+-+
    | | |X| | | | | |
    +-+-+-+-+-+-+-+-+
    | | | | | | |X| |
    +-+-+-+-+-+-+-+-+
    | | | |X| | | | |
    +-+-+-+-+-+-+-+-+
    | |X| | | | | | |
    +-+-+-+-+-+-+-+-+
    | | | | |X| | | |
    +-+-+-+-+-+-+-+-+
    ok
    4> 
