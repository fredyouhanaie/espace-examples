%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc
%%%
%%% This module is a collection of common functions for solving sudoku puzzles.
%%%
%%% The functions are used by other specialist tuple space based sudoko solvers.
%%%
%%% Throughout this library all cell addresses are `zero-based', for example the
%%% common 9x9 puzzle has row and column numbers in the range `0..8'.
%%%
%%% @end
%%% Created : 07 August 2022 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsudoku_lib).

-export([check_solution/3, check_puzzle/3]).
-export([box_of/4, puzzle_to_list/1]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").
-include_lib("tsudoku.hrl").

%%--------------------------------------------------------------------
%% @doc Check that a puzzle is valid.
%%
%% We check that there are no dupicates in a row/col/box other than zeros.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_puzzle(list()|map(), integer(), integer()) -> ok.
check_puzzle(Puzzle, _Box_rows, _Box_cols) when is_map(Puzzle)->
    ok;
check_puzzle(Puzzle, _Box_rows, _Box_cols) when is_list(Puzzle) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Check that the `Solution' is correct.
%%
%% We check that there are no duplicates in any row, column or box.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_solution(puzzle_map(), integer(), integer()) -> puzzle_check().
check_solution(Solution, Box_rows, Box_cols) ->
    Numbers = lists:seq(1, Box_rows * Box_cols),
    Cell_groups = classify_cells(Solution, Box_rows, Box_cols),
    check_numbers(Cell_groups, Numbers).

%%--------------------------------------------------------------------
%% @doc Check the list of `Nums' within a `Group'.
%%
%% We chack each `{Group, Nums}' of the input list against the expected number,
%% which is typically the sequence `1..N'
%%
%% @end
%%--------------------------------------------------------------------
-spec check_numbers([{cell_group(), [integer()]}], [integer()]) ->
          ok | {not_ok, [{cell_group(), [integer()]}]}.
check_numbers(Nums_list, Numbers) ->
    Nums_bad = fun ({_Group, Nums}) -> (Nums--Numbers) =/= [] end,
    case lists:filter(Nums_bad, Nums_list) of
        [] ->
            ok;
        Bad_cells ->
            {not_ok, Bad_cells}
    end.

%%--------------------------------------------------------------------
%% @doc Classify the cells into row/col/box groups.
%%
%% Each cell belongs to a row, a column and a box. For each row, column and box
%% we create the list of numbers in that group.
%%
%% We return a list of key/value pairs, where each key identifies a group, i.e.
%% `{row, Row}', `{col, Col}' and `{box, Box}', and the corresponding value is
%% the list of numbers in that group.
%%
%% @end
%%--------------------------------------------------------------------
-spec classify_cells(puzzle_map(), integer(), integer()) -> [{cell_group(), [integer()]}].
classify_cells(Solution, Box_rows, Box_cols) ->
    Cell_groups = maps:fold(fun ({R,C}, N, Cells) ->
                                     update_cells(R, C, N, Box_rows, Box_cols, Cells)
                             end, #{}, Solution),
    maps:to_list(Cell_groups).

%%--------------------------------------------------------------------
%% @doc Return the box address of a cell.
%%
%% Given a cell address, `Row'/`Col', determine its box address. The box
%% addresses are the row/col numbers of the top left corner of each box. Form
%% example for a `9x9' puzzle the box addresses are the nine pairs of
%% `0..3'x`0..3'.
%%
%% @end
%%--------------------------------------------------------------------
-spec box_of(integer(), integer(), integer(), integer()) ->
          {integer(), integer()}.
box_of(Row, Col, Box_rows, Box_cols) ->
    Row_base = floor(Row/Box_rows)*Box_rows,
    Col_base = floor(Col/Box_cols)*Box_cols,
    {Row_base, Col_base}.

%%--------------------------------------------------------------------
%% @doc Update the numbers in the groups of the given cell.
%%
%% Update the `Cells' map with the three groups of `{Row, Col, Num}'.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_cells(integer(), integer(), integer(), integer(), integer(), map()) -> map().
update_cells(Row, Col, Num, Box_rows, Box_cols, Cells) ->
    Box = box_of(Row, Col, Box_rows, Box_cols),
    Cells2 = update_map({row, Row}, Num, Cells),
    Cells3 = update_map({col, Col}, Num, Cells2),
    Cells4 = update_map({box, Box}, Num, Cells3),
    Cells4.

%%--------------------------------------------------------------------
%% @doc Add `Num' to the list of numbers in `Cells' corresponding to `Group'.
%%
%% This is used during the classification of the cells.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_map(cell_group(), integer(), map()) -> map().
update_map(Group, Num, Cells) ->
    maps:update_with(Group,
                     fun (Numbers) -> [Num|Numbers] end,
                     [Num],
                     Cells).

%%--------------------------------------------------------------------
%% @doc Convert a puzzle from map type to list of lists.
%%
%% `Puzzle' is a map with elements of the form `{Row,Col} => Num'.
%%
%% @end
%%--------------------------------------------------------------------
-spec puzzle_to_list(puzzle_map()) -> puzzle_list().
puzzle_to_list(Puzzle) ->
    Puzzle_list = maps:to_list(Puzzle),
    Puzzle_map  = lists:foldl(fun update_row/2, #{}, Puzzle_list),
    Rows = lists:map(fun ({_Row, Col}) -> Col end,
                    lists:sort(maps:to_list(Puzzle_map))),
    lists:map(fun cols_to_list/1, Rows).

%%--------------------------------------------------------------------
%% @doc Update the row of a nested puzzle map with a cell's contents.
%%
%% `Puzzle_map' is a nested map of row number to column map elements, where each
%% column map is of the form `{Col => Num}', e.g.
%%
%% `#{R1 => #{C1 => N11, C2 => N12}, R2 => #{C1 => N21, C2 => N22}, ...}'
%%
%% @end
%%--------------------------------------------------------------------
-spec update_row({{integer(), integer()}, integer()}, puzzle_map_nested()) -> puzzle_map_nested().
update_row({{Row, Col}, Num}, Puzzle_map) ->
    Row_map_cur = maps:get(Row, Puzzle_map, #{}),
    Row_map_new = maps:put(Col, Num, Row_map_cur),
    maps:put(Row, Row_map_new, Puzzle_map).

%%--------------------------------------------------------------------
%% @doc Convert column map to list of column values.
%%
%% Given a column map, return a list of the column values in the order of column
%% numbers.
%%
%% @end
%%--------------------------------------------------------------------
-spec cols_to_list(map()) -> list().
cols_to_list(Cols_map) ->
    lists:map(fun ({_Col, Num}) -> Num end,
              lists:sort(maps:to_list(Cols_map))).

%%--------------------------------------------------------------------
