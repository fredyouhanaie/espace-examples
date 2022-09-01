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

puzzle_ok_1_test() ->
    {{Box_rows, Box_cols}, Puzzle} = ?Puzzle_1_map_good,
    ?assert(tsudoku_lib:puzzle_ok(Puzzle, Box_rows, Box_cols)).

%%--------------------------------------------------------------------

puzzle_ok_2_test() ->
    {{Box_rows, Box_cols}, Puzzle} = ?Puzzle_2_map_bad,
    ?assertNot(tsudoku_lib:puzzle_ok(Puzzle, Box_rows, Box_cols)).

%%--------------------------------------------------------------------

puzzle_ok_3_test() ->
    {{Box_rows, Box_cols}, Puzzle} = ?Puzzle_3_lol_good,
    ?assert(tsudoku_lib:puzzle_ok(Puzzle, Box_rows, Box_cols)).

%%--------------------------------------------------------------------

puzzle_ok_4_test() ->
    {{Box_rows, Box_cols}, Puzzle} = ?Puzzle_4_lol_bad,
    ?assertNot(tsudoku_lib:puzzle_ok(Puzzle, Box_rows, Box_cols)).

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
    {{Box_rows, Box_cols}, Puzzle} = tsudoku_lib:read_puzzle(Puzzle_file),
     ?_assert(tsudoku_lib:check_puzzle(Puzzle, Box_rows, Box_cols)).

%%--------------------------------------------------------------------

check_puzzle_bad(Puzzle_file) ->
    {{Box_rows, Box_cols}, Puzzle} = tsudoku_lib:read_puzzle(Puzzle_file),
     ?_assertNot(tsudoku_lib:check_puzzle(Puzzle, Box_rows, Box_cols)).

%%--------------------------------------------------------------------
