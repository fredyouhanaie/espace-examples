% -*- indent-tabs-mode:nil; -*-
%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2020, Fred Youhanaie
%%% @doc
%%%
%%% Solve the 8 queens puzzle concurrently using tuple space.
%%%
%%% The chessboard is represented as a `map' of `Row=>Col'
%%% elements. Rows and column numbers start at `1', similar to a
%%% typical chessboard.
%%%
%%% The puzzle is solved by one or more `solver' workers, each taking
%%% a partially covered unique chessboard and generating zero or more
%%% unique boards with the placement of another non-attacking queen.
%%%
%%% For example, starting with the empty chessboard tuple, `{new,
%%% #{}}', one of the solvers will replace it with 8 tuples of the
%%% form `{new, #{1=>Col}}', where `Col' takes the values 1 to
%%% 8. Next, `{new, #{1=>1}}' will be replaced with `{new, #{1=>1,
%%% 2=>3}}', etc.
%%%
%%% During the course of solving the puzzle various tuples will appear
%%% in the tuple space. These are:
%%%
%%% `{new, Board}': `Board' is a new partial solution, which will be
%%% processed by the solver.
%%%
%%% `{deadend, Board}': `Board' is a partial solution that has no free
%%% square that cannot be attacked by the current queens.
%%%
%%% `{solution, Board}': `Board' is a completed solution.
%%%
%%% @end
%%% Created : 17 Dec 2020 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(queens8).

-include_lib("kernel/include/logger.hrl").

-define(Num_queens, 8).
-define(Num_solvers, 2).

-export([start/0, get_solutions/0, print_solution/0]).

%%--------------------------------------------------------------------
%% @doc Given the positions of a queen and a list of queens, returns
%% `true' if the single queen can attack any of the queens in the
%% list.
%%
%% We do not check if the queens within the list can attack each
%% other.
%%
%% Two queens can attack each other if they are on the same row, or
%% the same column, or on the same diagonal (the differences between
%% rows and columns are the same).
%%
%% We expect the row and column numbers to be non-zero.
%%
%% @end
%%--------------------------------------------------------------------
-spec attacks_one(tuple(), tuple()) -> boolean().
attacks_one({R, _C1}, {R, _C2}) ->
    true;  %% same row

attacks_one({_R1, C}, {_R2, C}) ->
    true;  %% same column

attacks_one({R1, C1}, {R2, C2}) ->
    abs(R1-R2) == abs(C1-C2). %% same diagonal?

%%--------------------------------------------------------------------
%% @doc check if the `Queen' can attack any of the queens on `Board'.
%%
%% The board is a map of `{Row=>Col}' elements.
%%
%% @end
%%--------------------------------------------------------------------
-spec attacks_any(tuple(), map()) -> boolean().
attacks_any(Queen, Board) ->
    maps:fold(fun (R, C, Acc) ->
                      Acc orelse attacks_one(Queen, {R, C})
              end,
              false,
              Board).
                 
%%--------------------------------------------------------------------
%% @doc Start the processes.
%%
%% We start `espace', along with the `solver' worker processes.
%%
%% The initial `new' tuple, which is a board with no queens, is added
%% to the tuple space. This will kick off the whole process of finding
%% valid solutions.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> done.
start() ->
    case application:ensure_all_started(espace) of
        {ok, Started} ->
            ?LOG_INFO("espace apps started: ~p.", [Started]);
        {error, Reason} ->
            ?LOG_ERROR("could not start espace: ~p", [Reason]),
            exit("startup failed")
    end,
    Worker = fun (_) ->
                     espace:worker({fun worker_solver/0, []})
             end,
    lists:foreach(Worker, lists:seq(1, ?Num_solvers)),
    espace:out({new, #{}}).

%%--------------------------------------------------------------------
%% @doc Wait for a `new' tuple and process it.
%%
%% @end
%%--------------------------------------------------------------------
-spec worker_solver() -> term().
worker_solver() ->
    {[Board], _} = espace:in({new, '$1'}),
    check_board(Board),
    worker_solver().

%%--------------------------------------------------------------------
%% @doc Given the positions of queens on a chessboard, generate the
%% next set of position from this one.
%%
%% The input, `Board', is a `map' of `Row => Col' elements, each
%% representing the position of a queens on the chessboard.
%%
%% We can always expect a partial board, since a full board would have
%% been caught during a previous iteration.
%%
%% We pick the lowest numbered free row. We then check all columns of
%% that row looking for positions that cannot attack any of the
%% existing queens.
%%
%% If the board has all the queens in place, we have a solution
%% so we produce a `{solution, Board}' tuple.
%%
%% For each non-attacking position found, we generate a `new'
%% tuple. If no such postions are found, we generate a `deadend'
%% tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_board(map()) -> ok.
check_board(Board) ->
    %% find the next available row
    Next_row = maps:size(Board)+1,

    Cols_avail = lists:seq(1, ?Num_queens) -- maps:values(Board),
    Squares_avail = [{Next_row, Col} || Col <- Cols_avail],

    Non_attacking = fun ({R,C}) -> not attacks_any({R,C}, Board) end,
    Squares_to_check = lists:filter(Non_attacking, Squares_avail),
    case Squares_to_check of
        [] -> %% we cannot go any further
            espace:out({deadend, Board});
        [{R=?Num_queens,C}] -> %% solution! we've reached the last row with a non-attacking column!
            espace:out({solution, Board#{R=>C}});
        _ -> %% at least one more row can be checked after the current one
            New_board = fun ({R,C}) -> espace:out({new, Board#{R=>C}}) end,
            lists:foreach(New_board, Squares_to_check)
    end.

%%--------------------------------------------------------------------
%% @doc Extract the solutions from the tuple space.
%%
%% The solutions are returned as a list of maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_solutions() -> list().
get_solutions() ->
    get_solutions([]).

%%--------------------------------------------------------------------
%% @doc Extract the list of solutions from the tuple space.
%%
%% We repeatedly extract the `solution' tuples and build a list of the
%% solutions.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_solutions(list()) -> list().
get_solutions(Solutions) ->
    case espace:inp({solution, '$1'}) of
        nomatch ->
            Solutions;
        {[S], _} ->
            get_solutions([S|Solutions])
    end.

%%--------------------------------------------------------------------
%% @doc If a solution exists in the tuple space, extract and print it.
%%
%% @end
%%--------------------------------------------------------------------
-spec print_solution() -> ok.
print_solution() ->
    case espace:inp({solution, '$1'}) of
        nomatch ->
            io:format("No solutions found.~n");
        {[Board], _} ->
            print_board(Board)
    end.
    
%%--------------------------------------------------------------------
%% @doc Print a solution map as a chessboard.
%%
%% Once the rows are printed, a horizontal line is printed at the end.
%%
%% @end
%%--------------------------------------------------------------------
-spec print_board(map()) -> ok.
print_board(Board) ->
    lists:foreach(fun print_row/1,
                  lists:sort(maps:to_list(Board))
                 ),
    print_hline().

%%--------------------------------------------------------------------
%% @doc Print a single row.
%%
%% Prints a horizontal line followed by a row of the solution.
%%
%% @end
%%--------------------------------------------------------------------
-spec print_row({integer(), integer()}) -> ok.
print_row({_, Col}) ->
    Left_cols  = lists:duplicate(Col-1, " |"),
    Right_cols = lists:duplicate(?Num_queens-Col, " |"),
    Row = erlang:list_to_binary(Left_cols ++ ["X|" | Right_cols]),
    print_hline(),
    io:format("|~s~n", [Row]).

%%--------------------------------------------------------------------
%% @doc Print a horizontal line.
%%
%% @end
%%--------------------------------------------------------------------
-spec print_hline() -> ok.
print_hline() ->
    Hline = erlang:list_to_binary(lists:duplicate(?Num_queens, "-+")),
    io:format("+~s~n", [Hline]).

%%--------------------------------------------------------------------
