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

%%--------------------------------------------------------------------

-define(Bad_file_1, "test/non-existent-file"). %% this files should never exist!
read_1_test() ->
    ?assertEqual({error,enoent}, tsudoku1:solve_file(?Bad_file_1)),
    espace:stop().

%%--------------------------------------------------------------------

-define(Puzzle_bad_1, "test/puzzle_bad_1.dat").
read_2_test() ->
    ?assertEqual({error,bad_format}, tsudoku1:solve_file(?Puzzle_bad_1)),
    espace:stop().

%%--------------------------------------------------------------------

-define(Puzzle_1, "test/puzzle_4x4_1.dat").
-define(Solution_1,
        [[2, 1, 4, 3],
         [4, 3, 1, 2],
         [3, 4, 2, 1],
         [1, 2, 3, 4]]).

puzzle_1_test() ->
    tsudoku1:solve_file(?Puzzle_1),
    {[Solution_1], _} = espace:in({done, '$1'}),
    ?assertEqual(Solution_1, {ok, ?Solution_1}),
    espace:stop().

%%--------------------------------------------------------------------

-define(Puzzle_1_2, "test/puzzle_4x4_2.dat").

puzzle_1_2_test() ->
    tsudoku1:solve_file(?Puzzle_1_2),
    {[Solution], _} = espace:in({done, '$1'}),
    ?assertEqual(Solution, {ok, ?Solution_1}),
    espace:stop().

%%--------------------------------------------------------------------

-define(Puzzle_2, "test/puzzle_6x6_3.dat").
-define(Solution_2,
        [[4, 3, 2, 5, 1, 6],
         [6, 5, 1, 4, 3, 2],
         [1, 4, 5, 6, 2, 3],
         [2, 6, 3, 1, 4, 5],
         [3, 1, 6, 2, 5, 4],
         [5, 2, 4, 3, 6, 1]]).

puzzle_2_test() ->
    tsudoku1:solve_file(?Puzzle_2),
    {[Solution_2], _} = espace:in({done, '$1'}),
    ?assertEqual(Solution_2, {ok, ?Solution_2}),
    espace:stop().

%%--------------------------------------------------------------------
