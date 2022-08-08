%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc Run the unit tests
%%%
%%% @end
%%% Created : 07 Oct 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsudoku1_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------

-define(Puzzle_1, "test/puzzle_4x4_1.dat").
-define(Solution_1,
        [[2, 1, 4, 3],
         [4, 3, 1, 2],
         [3, 4, 2, 1],
         [1, 2, 3, 4]]).

puzzle_1_test() ->
    ?assertEqual(tsudoku1:solve_file(?Puzzle_1), {ok, ?Solution_1}),
    espace:stop().

%%%-------------------------------------------------------------------

-define(Puzzle_2, "test/puzzle_6x6_3.dat").
-define(Solution_2,
        [[4, 3, 2, 5, 1, 6],
         [6, 5, 1, 4, 3, 2],
         [1, 4, 5, 6, 2, 3],
         [2, 6, 3, 1, 4, 5],
         [3, 1, 6, 2, 5, 4],
         [5, 2, 4, 3, 6, 1]]).

puzzle_2_test() ->
    ?assertEqual(tsudoku1:solve_file(?Puzzle_2), {ok, ?Solution_2}),
    espace:stop().

%%%-------------------------------------------------------------------
