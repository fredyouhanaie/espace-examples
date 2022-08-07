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

-define(Puzzle_1, "test/puzzle_4x4_1.dat").
-define(Solution_1,
        [[2, 1, 4, 3],
         [4, 3, 1, 2],
         [3, 4, 2, 1],
         [1, 2, 3, 4]]).

puzzle_1_test() ->
    ?assertEqual(tsudoku1:solve_file(?Puzzle_1), {ok, ?Solution_1}).
