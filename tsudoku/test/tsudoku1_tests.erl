%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc Run the unit tests
%%%
%%% @end
%%% Created : 07 Aug 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsudoku1_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------

-define(Bad_file_1,   "test/non-existent-file"). %% this files should never exist!
-define(Puzzle_bad_1, "test/puzzle_bad_1.dat").

%%--------------------------------------------------------------------

setup() ->
    logger:set_primary_config(#{level => critical}).

cleanup(_) ->
    ok.

%%--------------------------------------------------------------------

get_puzzle_test_() ->
    {"Read puzzle file (the bad files!)",
     {setup, fun setup/0, fun cleanup/1,
      [ {"no file",    puzzle_read_file(?Bad_file_1,   {error,enoent})},
        {"bad format", puzzle_read_file(?Puzzle_bad_1, {error,bad_format})}
      ]}}.

%%--------------------------------------------------------------------

puzzle_read_file(File, Exp_result) ->
    Act_result = tsudoku1:solve_file(File),
    espace:stop(),
    ?_assertEqual(Exp_result, Act_result).

%%--------------------------------------------------------------------

-define(Puzzle_1_1, "test/puzzle_4x4_1.dat").
-define(Solution_1_1,
        [[2, 1, 4, 3],
         [4, 3, 1, 2],
         [3, 4, 2, 1],
         [1, 2, 3, 4]]).

-define(Puzzle_1_2, "test/puzzle_4x4_2.dat").
-define(Solution_1_2, ?Solution_1_1).

-define(Puzzle_2, "test/puzzle_6x6_3.dat").
-define(Solution_2,
        [[4, 3, 2, 5, 1, 6],
         [6, 5, 1, 4, 3, 2],
         [1, 4, 5, 6, 2, 3],
         [2, 6, 3, 1, 4, 5],
         [3, 1, 6, 2, 5, 4],
         [5, 2, 4, 3, 6, 1]]).

%%--------------------------------------------------------------------

solver_test_() ->
    {"solver test",
     {setup, fun setup/0, fun cleanup/1,
      [ {"puzzle 4x4 1", check_solver(?Puzzle_1_1, ?Solution_1_1)},
        {"puzzle 4x4 1", check_solver(?Puzzle_1_2, ?Solution_1_2)},
        {"puzzle 6x6 3", check_solver(?Puzzle_2,   ?Solution_2)}
      ]}}.

%%--------------------------------------------------------------------

check_solver(Puzzle_file, Exp_solution) ->
    tsudoku1:solve_file(Puzzle_file),
    {[Act_solution], _} = espace:in({done, '$1'}),
    espace:stop(),
    ?_assertEqual({ok, Exp_solution}, Act_solution).

%%--------------------------------------------------------------------
