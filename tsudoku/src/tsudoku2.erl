%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc
%%%
%%% Another solver, but improved.
%%%
%%% Throughout this library all cell addresses are `zero-based', for example the
%%% common 9x9 puzzle has row and column numbers in the range `0..8'.
%%%
%%% The puzzle grid is kept in the TS all the time.
%%%
%%% @end
%%% Created : 20 August 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsudoku2).

-export([check_puzzle/0, get_puzzle_file/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("tsudoku.hrl").

%%--------------------------------------------------------------------
%% @doc Read puzzle from file and save it to the tuple space.
%%
%% For each element of the puzzle a new tuple will be added to the TS
%% with the following format:
%%
%% `{cell, Row, Col, Num}'
%%
%% where, `Row' and `Col' are the row and column numbers starting with
%% `0' for the first row/column, and `Num' is either the clue from the
%% puzzle, or `0' for a blank cell/square.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_puzzle_file(file:name_all()) -> ok | {error, term()}.
get_puzzle_file(File) ->
    case tsudoku_lib:read_puzzle(File) of
        {{Box_rows, Box_cols}, Puzzle} ->
            application:ensure_started(espace),
            espace:out({box_dims, Box_rows, Box_cols}),
            out_puzzle(Puzzle, Box_rows*Box_cols);

        Error = {error, _} ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Output the puzzle cells to the tuple space.
%%
%% We generate tuples of the form `{cell, Row, Col, Num}' for all the
%% cells. If `Puzzle' is a map, then we generate `cell' tuples for the
%% missing cells with the content `0'.
%%
%% @end
%%--------------------------------------------------------------------
-spec out_puzzle(puzzle_map()|puzzle_list(), integer()) -> ok | {error, term()}.
out_puzzle(Puzzle, NN) when is_map(Puzzle) ->
    Nums = lists:seq(0, NN-1),
    FN = fun (R, C) -> maps:get({R, C}, Puzzle, 0) end,
    [ espace:out({cell, RR, CC, FN(RR, CC)}) || RR <- Nums, CC <- Nums ],
    ok;

out_puzzle(Puzzle, _NN) when is_list(Puzzle) ->
    Out_cell = fun (Num, {Row_num, Col_num}) ->
                       espace:out({cell, Row_num, Col_num, Num}),
                       {Row_num, Col_num+1}
               end,
    Out_row = fun (Row, Row_num) ->
                      lists:foldl(Out_cell, {Row_num, 0}, Row),
                      Row_num+1
              end,
    lists:foldl(Out_row, 0, Puzzle),
    ok.

%%--------------------------------------------------------------------
%% @doc Check a puzzle for duplicates.
%%
%% We chack all the rows, columns and boxes, looking for duplicate
%% values, except for `0' values.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_puzzle() -> boolean().
check_puzzle() ->
    {[Box_rows, Box_cols], _} = espace:rd({box_dims, '$1', '$2'}),
    NN = Box_rows*Box_cols,
    Nums = lists:seq(0, NN-1),
    True = fun (X) -> X end,
    Row_ok_all = lists:all(True, [puzzle_row_ok(R, NN) || R <- Nums ]),
    Col_ok_all = lists:all(True, [puzzle_col_ok(C, NN) || C <- Nums ]),
    Box_ok_all = lists:all(True, [puzzle_box_ok(R, C, Box_rows, Box_cols) ||
                                     R <- lists:seq(0, Box_cols-1, Box_rows),
                                     C <- lists:seq(0, Box_rows-1, Box_cols)]),
    (Row_ok_all andalso Col_ok_all andalso Box_ok_all).

%%--------------------------------------------------------------------

-spec puzzle_row_ok(integer(), integer()) -> boolean().
puzzle_row_ok(Row, N_cols) ->
    Nums = [ get_cell(Row, Col) || Col <- lists:seq(0, N_cols-1) ],
    puzzle_group_ok(Nums, N_cols).

%%--------------------------------------------------------------------

-spec puzzle_col_ok(integer(), integer()) -> boolean().
puzzle_col_ok(Col, N_rows) ->
    Nums = [ get_cell(Row, Col) || Row <- lists:seq(0, N_rows-1) ],
    puzzle_group_ok(Nums, N_rows).

%%--------------------------------------------------------------------

-spec puzzle_box_ok(integer(), integer(), integer(), integer()) -> boolean().
puzzle_box_ok(Row_base, Col_base, Box_rows, Box_cols) ->
    R_list = lists:seq(Row_base, Row_base+Box_rows-1),
    C_list = lists:seq(Col_base, Col_base+Box_cols-1),
    Nums = [ get_cell(Row, Col) || Row <- R_list, Col <- C_list ],
    puzzle_group_ok(Nums, Box_rows*Box_cols).
              
%%--------------------------------------------------------------------

get_cell(Row, Col) ->
    {[Num], _} = espace:rd({cell, Row, Col, '$1'}),
    Num.
    
%%--------------------------------------------------------------------
%% @doc Check group of numbers in a single row/column/box of puzzle
%%
%% The group should only contain numbers `0..NN', no duplicates other
%% than `0's.
%%
%% @end
%%--------------------------------------------------------------------
-spec puzzle_group_ok([integer()], integer()) -> boolean().
puzzle_group_ok(Group, NN) ->
    Nums = lists:seq(0, NN),
    G_N = Group--Nums,
    (length(Group) == NN) andalso (G_N == []) orelse (lists:uniq(G_N) == [0]).

%%--------------------------------------------------------------------
