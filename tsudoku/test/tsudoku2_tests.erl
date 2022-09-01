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
-define(Puzzle_bad_1, "test/puzzle_bad_1.dat").

%%--------------------------------------------------------------------

setup() ->
    logger:set_primary_config(#{level => error}).

cleanup(_) ->
    ok.

%%--------------------------------------------------------------------

get_puzzle_test_() ->
    {"Get puzzle file",
     {setup, fun setup/0, fun cleanup/1,
      [ {"no file",    puzzle_get_file(?Bad_file_1, {error,enoent})},
        {"bad format", puzzle_get_file(?Puzzle_bad_1, {error,bad_format})}
      ]}}.

%%--------------------------------------------------------------------

puzzle_get_file(File, Exp_result) ->
    Act_result = tsudoku2:get_puzzle_file(File),
    espace:stop(),
    ?_assertEqual(Exp_result, Act_result).

%%--------------------------------------------------------------------

puzzle_check_test_() ->
    {"Check puzzle",
     {setup, fun setup/0, fun cleanup/1,
      [ {"puzzle 4x4 1", check_puzzle_good("test/puzzle_4x4_1.dat", true)},
        {"puzzle 4x4 2", check_puzzle_good("test/puzzle_4x4_2.dat", true)},
        {"puzzle 6x6 1", check_puzzle_good("test/puzzle_6x6_1.dat", false)},
        {"puzzle 6x6 2", check_puzzle_good("test/puzzle_6x6_2.dat", true)},
        {"puzzle 6x6 3", check_puzzle_good("test/puzzle_6x6_3.dat", true)}
      ]}}.

%%--------------------------------------------------------------------

check_puzzle_good(Puzzle_file, Good) ->
    Puzzle_check = tsudoku2:check_puzzle(Puzzle_file),
    espace:stop(),
    ?_assertEqual(Good, Puzzle_check).

%%--------------------------------------------------------------------
