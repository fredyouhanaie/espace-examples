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

-export([read_puzzle/1, puzzle_ok/3]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").
-include_lib("tsudoku.hrl").

%%--------------------------------------------------------------------
%% @doc Read and return a puzzle definition from a file.
%%
%% The input file should have two high level terms. The first is a tuple
%% indicating the size of the puzzle in the form `{Box_rows,Box_cols}'. These
%% define the dimension of the inner grid box of the puzzle. The actual puzzle
%% size is the product of these numbers, i.e. `N = Box_rows * Box_cols', e.g.
%% `{2,3}' and `{3,3}' for `6x6' and `9x9' puzzles respectively.
%%
%% The second term is the puzzle itself, it can be a list of lists, or a map. In
%% the case of list of lists, each list is a row of the puzzle, there should be
%% `N' lists of `N' elements each. The list elements are numbers in the range
%% `0..N', where `0' indicates a blank square (cell).
%%
%% In the case of a map, each element is of the form `{Row,Col} => Num'. This
%% can be space efficient for puzzles with minimal number of clues, since we
%% only need to include the clues.
%%
%% @end
%%--------------------------------------------------------------------
-spec read_puzzle(string()) ->
          {{integer(), integer()}, puzzle_map()|puzzle_list()} |
          {error, term()}.
read_puzzle(File) ->
    case file:consult(File) of
        {ok, [{Box_rows, Box_cols}, Puzzle]} ->
            case puzzle_ok(Puzzle, Box_rows, Box_cols) of
                true ->
                    {{Box_rows, Box_cols}, Puzzle};
                false ->
                    {error, bad_format}
            end;
        {ok, _} ->
            ?LOG_ERROR(#{func=>?FUNCTION_NAME, file=>File, reason=>"Bad format"}),
            {error, bad_format};
        {error, Reason} ->
            ?LOG_ERROR(#{func=>?FUNCTION_NAME, file=>File, reason=>file:format_error(Reason)}),
            {error, Reason}
    end.

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
%% We check each `{Group, Nums}' of the input list against the expected number,
%% which is typically the sequence `1..N'
%%
%% @end
%%--------------------------------------------------------------------
-spec check_numbers([{cell_group(), [integer()]}], [integer()]) -> puzzle_check().
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
%% @doc Return true if `Puzzle' has the correct structure
%%
%% For list of lists the inner and outer lists must be of correct length.
%%
%% For maps the row and column numbers of keys must be within range.
%%
%% The contents of the cells are not checked. These are left for
%% `check_puzzle', which will look for duplicates, etc.
%%
%% @end
%%--------------------------------------------------------------------
-spec puzzle_ok(puzzle_list()|puzzle_map(), integer(), integer()) -> boolean().
puzzle_ok(Puzzle, Box_rows, Box_cols) when is_list(Puzzle) ->
    Order = Box_rows*Box_cols,
    Row_is_good = fun (X) -> is_list(X) andalso length(X) == Order end,
    Puzzle_ok = (length(Puzzle) == Order) andalso lists:all(Row_is_good, Puzzle),
    Puzzle_ok;

puzzle_ok(Puzzle, Box_rows, Box_cols) when is_map(Puzzle) ->
    NN = Box_rows*Box_cols,
    Within_range = fun (X) -> X >= 0 andalso X < NN end,
    Key_is_good = fun ({R, C}) ->  Within_range(R) andalso Within_range(C) end,
    lists:all(Key_is_good, maps:keys(Puzzle)).

%%--------------------------------------------------------------------

-spec puzzle_group_ok([integer()]) -> boolean().
puzzle_group_ok(Group) ->
    Nums = lists:seq(0, length(Group)),
    G_N = Group--Nums,
    (G_N == []) orelse (lists:uniq(G_N) == [0]).

%%--------------------------------------------------------------------

-spec puzzle_row_ok(puzzle_map()|puzzle_list(), integer()) -> boolean().
puzzle_row_ok(Puzzle, Row) when is_list(Puzzle) ->
    Nums = lists:nth(Row+1, Puzzle),
    puzzle_group_ok(Nums);

puzzle_row_ok(Puzzle, Row) when is_map(Puzzle) ->
    Filter = fun ({R, _C}, _Num) -> R == Row;
                 (_, _) -> false
             end,
    Nums = maps:values(maps:filter(Filter, Puzzle)),
    puzzle_group_ok(Nums).

%%--------------------------------------------------------------------

-spec puzzle_col_ok(puzzle_map()|puzzle_list(), integer()) -> boolean().
puzzle_col_ok(Puzzle, Col) when is_list(Puzzle) ->
    Nums = [ lists:nth(Col+1, Row) || Row <- Puzzle ],
    puzzle_group_ok(Nums);

puzzle_col_ok(Puzzle, Col) when is_map(Puzzle) ->
    Filter = fun ({_R, C}, _Num) -> C == Col;
                 (_, _) -> false
             end,
    Nums = maps:values(maps:filter(Filter, Puzzle)),
    puzzle_group_ok(Nums).

%%--------------------------------------------------------------------

-spec puzzle_box_ok(puzzle_map()|puzzle_list(), integer(), integer(), integer(), integer()) -> boolean().
puzzle_box_ok(Puzzle, Row_base, Col_base, Box_rows, Box_cols) when is_list(Puzzle) ->
    %% extract the rows of box
    Rows = lists:sublist(Puzzle, Row_base+1, Box_rows),
    %% extract the cols of the extracted rows
    Cols = [ lists:sublist(RR, Col_base+1, Box_cols) || RR <- Rows ],
    Nums = lists:flatten(Cols),
    puzzle_group_ok(Nums);

puzzle_box_ok(Puzzle, Row_base, Col_base, Box_rows, Box_cols) when is_map(Puzzle) ->
    Box_coords = [ {R, C} || R <- lists:seq(Row_base+1, Row_base+Box_rows),
                             C <- lists:seq(Col_base+1, Col_base+Box_cols) ],
    Nums = [ maps:get({R,C}, Puzzle, 0) || {R,C} <- Box_coords ],
    puzzle_group_ok(Nums).

%%--------------------------------------------------------------------

-spec check_puzzle(puzzle_map()|puzzle_list(), integer(), integer()) -> boolean().
check_puzzle(Puzzle, Box_rows, Box_cols) ->
    NN = Box_rows*Box_cols,
    Nums = lists:seq(0, NN-1),
    True = fun (X) -> X end,
    Row_ok_all = lists:all(True, [puzzle_row_ok(Puzzle, R) || R <- Nums ]),
    Col_ok_all = lists:all(True, [puzzle_col_ok(Puzzle, C) || C <- Nums ]),
    Box_ok_all = lists:all(True, [puzzle_box_ok(Puzzle, R, C, Box_rows, Box_cols) ||
                                     R <- lists:seq(0, Box_cols-1, Box_rows),
                                     C <- lists:seq(0, Box_rows-1, Box_cols)]),
    (Row_ok_all andalso Col_ok_all andalso Box_ok_all).

%%--------------------------------------------------------------------
