%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc Run the unit tests
%%%
%%% @end
%%% Created : 20 Aug 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsudoku_lib_test).

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
