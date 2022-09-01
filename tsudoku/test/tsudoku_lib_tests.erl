%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc Run the unit tests
%%%
%%% @end
%%% Created : 20 Aug 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsudoku_lib_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------

-define(Puzzle_1_map_good,
        { {2,2},
          #{ {0,0} => 4,
             {1,1} => 1,
             {2,2} => 2,
             {3,3} => 3 } }
       ).

-define(Puzzle_2_map_bad,
        { {2,2},
          #{ {4,4} => 4,
             {1,1} => 1,
             {2,2} => 2,
             {3,3} => 3 } }
       ).

-define(Puzzle_3_lol_good,
        { {2,2},
          [ [4, 0, 0, 0],
            [0, 1, 0, 0],
            [0, 0, 2, 0],
            [0, 0, 0, 3] ] }
       ).

-define(Puzzle_4_lol_bad,
        { {2,2},
          [ [4, 0, 0, 0],
            [0, 1, 0, 0],
            [0, 0, 0, 3] ] }
       ).

%%--------------------------------------------------------------------

puzzle_ok_test_() ->
    {"Puzzle OK",
     {setup, fun setup/0, fun cleanup/1,
      [ {"puzzle 1 map good",   check_puzzle_OK(?Puzzle_1_map_good, true)},
        {"puzzle 2 map bad",    check_puzzle_OK(?Puzzle_2_map_bad,  false)},
        {"puzzle 3 list good",  check_puzzle_OK(?Puzzle_3_lol_good, true)},
        {"puzzle 4 list bad",   check_puzzle_OK(?Puzzle_4_lol_bad,  false)}
      ]}}.

%%--------------------------------------------------------------------

check_puzzle_OK({{Box_rows, Box_cols}, Puzzle}, OK) ->
    ?_assertEqual(OK, tsudoku_lib:puzzle_ok(Puzzle, Box_rows, Box_cols)).

%%--------------------------------------------------------------------

setup() ->
    logger:set_primary_config(#{level => critical}).

cleanup(_) ->
    ok.

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
    {{Box_rows, Box_cols}, Puzzle} = tsudoku_lib:read_puzzle(Puzzle_file),
    Puzzle_check = tsudoku_lib:check_puzzle(Puzzle, Box_rows, Box_cols),
    espace:stop(),
    ?_assertEqual(Good, Puzzle_check).

%%--------------------------------------------------------------------
