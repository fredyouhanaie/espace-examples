%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc Run the unit tests
%%%
%%% @end
%%% Created : 20 Aug 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsudoku2_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------

-define(Bad_file_1, "test/non-existent-file"). %% this files should never exist!
read_1_test() ->
    ?assertEqual({error,enoent}, tsudoku2:get_puzzle_file(?Bad_file_1)),
    espace:stop().

%%--------------------------------------------------------------------

-define(Puzzle_bad_1, "test/puzzle_bad_1.dat").
read_2_test() ->
    ?assertEqual({error,bad_format}, tsudoku2:get_puzzle_file(?Puzzle_bad_1)),
    espace:stop().

%%--------------------------------------------------------------------

puzzle_check_test_() ->
    [{"puzzle 4x4 1",   check_puzzle_good("test/puzzle_4x4_1.dat")},
     {"puzzle 4x4 2",   check_puzzle_good("test/puzzle_4x4_2.dat")},
     {"puzzle 6x6 1",   check_puzzle_bad("test/puzzle_6x6_1.dat")},
     {"puzzle 6x6 2",   check_puzzle_good("test/puzzle_6x6_2.dat")},
     {"puzzle 6x6 3",   check_puzzle_good("test/puzzle_6x6_3.dat")}
    ].

%%--------------------------------------------------------------------

check_puzzle_good(Puzzle_file) ->
    espace:stop(),
    ok = tsudoku2:get_puzzle_file(Puzzle_file),
    ?_assert(tsudoku2:check_puzzle()).

%%--------------------------------------------------------------------

check_puzzle_bad(Puzzle_file) ->
    espace:stop(),
    ok = tsudoku2:get_puzzle_file(Puzzle_file),
    ?_assertNot(tsudoku2:check_puzzle()).

%%--------------------------------------------------------------------
