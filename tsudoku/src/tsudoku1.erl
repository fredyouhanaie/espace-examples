%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%%
%%% This module is a collection of functions for solving sudoku puzzles. We use
%%% one of the simpler methods of solving the puzzle. Initially all cells are
%%% given their assigned number from the puzzle, or the set of all numbers
%%% `1..N' as candidates. Then, the numbers assigned to the buddy cells of a
%%% cell are eliminated from the set of candidate numbers of that cell until all
%%% the cells are left with one number each.
%%%
%%% Multiple specialist workers carry out the computation by broadcasting
%%% specific facts about the cells via the tuple space.
%%%
%%% <ul>
%%%
%%% <li>The main solver process starts the worker processes and assigns numbers
%%% to individual cells as defined by the puzzle. It then waits for the
%%% `{solved,...}' tuples, one per cell. For each `{solved,...}' tuple received
%%% a `{cellcast,...}' tuple is generated. Which in turn is picked up by the
%%% various relay workers.</li>
%%%
%%% <li>For each cell there is a worker that waits for `{cell,...}' tuples and
%%% updates its own state accordingly. Once the cell worker is left with one
%%% number, it outputs a `{solved,...}' tuple and terminates.</li>
%%%
%%% </ul>
%%%
%%% There are 4 relay workers that receive special broadcast tuples and generate
%%% further broadcast tuples, these are:
%%%
%%% <ul>
%%%
%%% <li>`relay_cell' waits for `{cellcast,...}' tuples and generates appropriate
%%% tuples for `relay_row', `relay_col' and `relay_box'.</li>
%%%
%%% <li>`relay_row' waits for `{rowcast,...}' tuples and generates `{cell,...}'
%%% tuples for all the cells in the same row as the cell.</li>
%%%
%%% <li>`relay_col' waits for `{colcast,...}' tuples and generates `{cell,...}'
%%% tuples for all the cells in the same column as the cell.</li>
%%%
%%% <li>`relay_box' waits for `{boxcast,...}' tuples and generates `{cell,...}'
%%% tuples for all the cells in the same box as the cell.</li>
%%%
%%% </ul>
%%%
%%% The overall flow of `[tuples]' among `(workers)' is as follows:
%%%
%%% <ol>
%%%
%%% <li> (solver) -> [cell] -> (cell)</li>
%%% <li> (cell) -> [solved] -> (solver) -> [cellcast] -> (relay_cell)</li>
%%% <li> (relay_cell) -> [rowcast] -> (relay_row) -> [cell] -> (cell)</li>
%%% <li> (relay_cell) -> [colcast] -> (relay_col) -> [cell] -> (cell)</li>
%%% <li> (relay_cell) -> [boxcast] -> (relay_box) -> [cell] -> (cell)</li>
%%%
%%% </ol>
%%%
%%% The tuple types used for communication are listed below. Currently `Msg' is
%%% of the form `{R, C, N}' indicating that the cell in row `R' and column `C'
%%% has the number `N'.
%%%
%%% <ul>
%%%
%%% <li><code>{cellcast, Row, Col, Num}</code>: Informs the relay workers that
%%% the cell at `Row'/`Col' has `Num'.</li>
%%%
%%% <li><code>{rowcast, Row, Msg</code>: The `Msg' is relayed to all cells of
%%% row number `Row' by the `relay_rowcast' worker.</li>
%%%
%%% <li><code>{colcast, Col, Msg}</code>: The `Msg' is relayed to all cells of
%%% column number `Col' by the `relay_colcast' worker.</li>
%%%
%%% <li><code>{boxcast, Row, Col, Msg}</code>: The `Msg' is relayed to all cells
%%% in the same box as `{Row, Col}' by the `relay_boxcast' worker.</li>
%%%
%%% <li><code>{cell, Row, Col, Msg}</code>: `Msg' is picked up by the cell
%%% worker at `{Row,Col}'.</li>
%%%
%%% </ul>
%%%
%%% Note: all cell addresses are `zero-based', for example the common 9x9 puzzle
%%% has row and column numbers in the range `0..8'.
%%%
%%% @end
%%% Created : 13 Mar 2021 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(tsudoku1).

-export([solve/3, solve_file/1]).
-export([check_solution/3]).

-include_lib("kernel/include/logger.hrl").

%%-------------------------------------------------------------------

-type(puzzle_list() :: [[ integer() ]]).
-type(puzzle_map() :: {integer(), integer(), #{{integer(), integer()} => integer()}}).

-type(solution_list() :: [[ integer() ]]).
-type(solution_map() :: #{{integer(), integer()} => integer()}).

-type(solution_check() :: ok | {not_ok, [{tuple(), [integer()]}]}).

%%-------------------------------------------------------------------

-define(Log_level, warning).

%%-------------------------------------------------------------------
%%
%% Read a puzzle definition from a file and start the solver.
%%
-spec solve_file(string()) -> {solution_check(), solution_list()}.
solve_file(File) ->
    logger:set_primary_config(level, ?Log_level),
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    {ok, [{Box_rows, Box_cols}, Puzzle]} = file:consult(File),
    application:ensure_started(espace),
    solve(Puzzle, Box_rows, Box_cols).

%%-------------------------------------------------------------------
%%
%% Solve a puzzle. We expect the espace application to be running.
%%
-spec solve(puzzle_map()|puzzle_list(), integer(), integer()) ->
          {solution_check(), solution_list()}.
solve(Puzzle, Box_rows, Box_cols) ->
    N_rows_cols = Box_rows * Box_cols,

    %% start the relay workers
    %%
    espace:worker({fun relay_cellcast/0, []}),
    espace:worker({fun relay_rowcast/1, [N_rows_cols]}),
    espace:worker({fun relay_colcast/1, [N_rows_cols]}),
    espace:worker({fun relay_boxcast/2, [Box_rows,Box_cols]}),

    %% start the individual cell workers
    %%
    Numbers = lists:seq(1, N_rows_cols),
    Cell_fun = fun ({R,C}) ->
                       espace:worker({fun cell/3, [R, C, Numbers]})
               end,
    lists:foreach(Cell_fun, [{R,C} ||
                                R <- lists:seq(0, N_rows_cols-1),
                                C <- lists:seq(0, N_rows_cols-1)]),

    %% inform the cell workers of their known numbers, where known
    %%
    assign_cells(Puzzle),

    N_cells = N_rows_cols * N_rows_cols,
    Solution = collect_solutions(N_cells, #{}),
    Solution_ok = check_solution(Solution, Box_rows, Box_cols),
    {Solution_ok, solution_to_list(Solution)}.

%%-------------------------------------------------------------------
%%
%% assign the known cell numbers to the cells. We output `{cell,...}' tuples
%% which is picked up by the cell workers.
%%
-spec assign_cells(puzzle_map()|puzzle_list()) -> integer().
assign_cells(Puzzle) when is_map(Puzzle) ->
    lists:foreach(fun ({{Row, Col}, Num}) ->
                          out_cell(Row, Col, {Row, Col, Num})
                  end,
                  maps:to_list(Puzzle));

assign_cells(Puzzle) when is_list(Puzzle) ->
    lists:foldl(fun assign_rows/2, 0, Puzzle).

%%-------------------------------------------------------------------
%%
%%
%%
-spec assign_rows([integer()], integer()) -> integer().
assign_rows(Row_cells, Row_num) ->
    ?LOG_INFO(#{func=>?FUNCTION_NAME, row_num=>Row_num}),
    Out_fun = fun (0, Col_num) ->
                      Col_num+1;
                  (Cell_num, Col_num) ->
                      Cell_data = {Row_num, Col_num, Cell_num},
                      out_cell(Row_num, Col_num, Cell_data),
                      Col_num+1
              end,
    lists:foldl(Out_fun, 0, Row_cells),
    Row_num+1.

%%-------------------------------------------------------------------
%%
%% Check for and warn about left over `{solved,...}' tuples.
%%
%% This is precautionary, shouldn't need to call this! It can be removed in
%% future.
%%
-spec drain_solved() -> ok.
drain_solved() ->
    case espace:inp({solved, '_', '_', '_'}) of
        nomatch ->
            ok;
        {[], Tuple} ->
            ?LOG_WARNING(#{func => ?FUNCTION_NAME,
                           msg => "surplus solved tuple!",
                           tuple => Tuple}),
            drain_solved()
    end.

%%-------------------------------------------------------------------
%%
%% wait for and collect all the `{solved, Row, Col, Num}' tuples. We expect
%% `N_solutions` tuples. The function will block until ALL tuples have been
%% collected.
%%
%% Each time a cell is solved, we broadcast the fact to the relay workers via
%% tuples of type `{cellcast, Row, Col, Num}'.
%%
%% The solved puzzle is returned as a map. It can later be converted to a list
%% of lists with `solution_to_list/1'.
%%
-spec collect_solutions(integer(), solution_map()) -> solution_map().
collect_solutions(0, Solutions) ->
    drain_solved(),
    Solutions;

collect_solutions(N_solutions, Solutions) ->
    ?LOG_NOTICE(#{func=>?FUNCTION_NAME, n_sol=>N_solutions}),
    {[Row, Col, Num], _} = espace:in({solved, '$1', '$2', '$3'}),
    espace:out({cellcast, Row, Col, Num}),
    collect_solutions(N_solutions-1, maps:put({Row, Col}, Num, Solutions)).

%%-------------------------------------------------------------------
%%
%% Convert a solved puzzle from map type to list of lists. This is mainly for
%% display purposes.
%%
-spec solution_to_list(solution_map()) -> solution_list().
solution_to_list(Solution_map) ->
    Sol_list = maps:to_list(Solution_map),
    Sol_map = lists:foldl(fun update_row/2, #{}, Sol_list),
    Rows = lists:map(fun ({_Row, Col}) -> Col end,
                    lists:sort(maps:to_list(Sol_map))),
    lists:map(fun cols_to_list/1, Rows).

%%-------------------------------------------------------------------
%%
%% Update the row of a solution map with a cell's contents.
%%
%% The solution map is a nested map of row number to column map elements, where
%% each column map is column to cell elements.
%%
-spec update_row({{integer(), integer()}, integer()}, solution_map()) -> solution_map().
update_row({{Row, Col}, Num}, Sol_map) ->
    Row_map1 = maps:get(Row, Sol_map, #{}),
    Row_map2 = maps:put(Col, Num, Row_map1),
    maps:put(Row, Row_map2, Sol_map).

%%-------------------------------------------------------------------
%%
%% Given a column map, return a list of the column values in the order of column
%% numbers.
%%
-spec cols_to_list(map()) -> list().
cols_to_list(Cols_map) ->
    lists:map(fun ({_Col, Num}) -> Num end,
              lists:sort(maps:to_list(Cols_map))).

%%-------------------------------------------------------------------
%%
%% The cell worker. One worker process per cell, identified by `Row'/`Col'. Each
%% worker keeps track of its own set of candidate numbers, `Numbers'. Once our
%% set of candidates becomes a single number, we broadcast that we're done.
%%
%% On start up, `Numbers' will be the set of number `1..N'. We wait for tuples
%% of the form `{cell, Row, Col, {RR, CC, Num}}'. If `RR'/`CC' is our address,
%% then we've been assigned a number, which is what happens during the start up.
%%
%% Otherwise, we're being told that one of our buddies has got `Num', so we take
%% it out of our list of candidates.
%%
-spec cell(integer(), integer(), [integer()]) -> ok.
cell(Row, Col, [Num]) when is_integer(Num) -> %% only one number left, we're done :-)
    ?LOG_NOTICE(#{func=>?FUNCTION_NAME, row_col=>{Row, Col}, num=>Num}),
    espace:out({solved, Row, Col, Num});

cell(Row, Col, Numbers) ->
    ?LOG_NOTICE(#{func=>?FUNCTION_NAME, row_col=>{Row, Col}, numbers=>Numbers}),
    {[Msg], _} = espace:in({cell, Row, Col, '$3'}),
    case Msg of
        {Row, Col, Num} -> %% this is us, we're assigned a number
            espace:out({solved, Row, Col, Num});
        {_RR, _CC, Num} -> %% it must be one of our buddies
            cell(Row, Col, Numbers -- [Num])%% take it out of our list of candidates
    end.

%%-------------------------------------------------------------------
%%
%% Drop a `{cell,...}' tuple in the tuple space.
%%
-spec out_cell(integer(), integer(), {integer(), integer(), integer()}) -> ok.
out_cell(Row, Col, Msg) ->
    ?LOG_NOTICE(#{func=>?FUNCTION_NAME, row=>Row, col=>Col, msg=>Msg}),
    espace:out({cell, Row, Col, Msg}).

%%-------------------------------------------------------------------
%%
%% Wait for a `{cellcast, Row, Col, Num}' tuple and forward it to the row, col
%% and box relay workers corresponding to the cell at `Row'/`Col'.
%%
-spec relay_cellcast() -> none.
relay_cellcast() ->
    {[Row, Col, Num], _} = espace:in({cellcast, '$1', '$2', '$3'}),
    Msg = {Row, Col, Num},
    espace:out({rowcast, Row, Msg}),
    espace:out({colcast, Col, Msg}),
    espace:out({boxcast, Row, Col, Msg}),
    relay_cellcast().

%%-------------------------------------------------------------------
%%
%% Wait for a `{rowcast, Row, Msg}' tuple and forward it to the cells within
%% that row.
%%
-spec relay_rowcast(integer()) -> none.
relay_rowcast(N_cols) ->
    {[Row, Msg], _} = espace:in({rowcast, '$1', '$2'}),
    Out_fun = fun (Col) -> out_cell(Row, Col, Msg) end,
    lists:foreach(Out_fun, lists:seq(0, N_cols-1)),
    relay_rowcast(N_cols).

%%-------------------------------------------------------------------
%%
%% Wait for a `{colcast, Col, Msg}' tuple and forward it to the cells within
%% that column.
%%
-spec relay_colcast(integer()) -> none.
relay_colcast(N_rows) ->
    {[Col, Msg], _} = espace:in({colcast, '$1', '$2'}),
    Out_fun = fun (Row) -> out_cell(Row, Col, Msg) end,
    lists:foreach(Out_fun, lists:seq(0, N_rows-1)),
    relay_colcast(N_rows).

%%-------------------------------------------------------------------
%%
%% Wait for a `{boxcast, Row, Col, Msg}' tuple and forward it to the cells
%% within that box.
%%
-spec relay_boxcast(integer(), integer()) -> none.
relay_boxcast(Box_rows, Box_cols) ->
    {[Row, Col, Msg], _} = espace:in({boxcast, '$1', '$2', '$3'}),
    {R_base, C_base} = box_of(Row, Col, Box_rows, Box_cols),
    Out_fun = fun ({R, C}) -> out_cell(R, C, Msg) end,
    lists:foreach(Out_fun, [{R_base+R, C_base+C} ||
                               R <- lists:seq(0,Box_cols-1),
                               C <- lists:seq(0,Box_rows-1)]),
    relay_boxcast(Box_rows, Box_cols).

%%-------------------------------------------------------------------
%%
%% Given a cell address, `Row' and `Col', determine its box address. The box
%% addresses are the row/col numbers of the top left corner of each box.
%%
-spec box_of(integer(), integer(), integer(), integer()) ->
          {integer(), integer()}.
box_of(Row, Col, Box_rows, Box_cols) ->
    Row_base = floor(Row/Box_rows)*Box_rows,
    Col_base = floor(Col/Box_cols)*Box_cols,
    {Row_base, Col_base}.

%%-------------------------------------------------------------------
%%
%% Check that the `Solution' is correct, ie no duplicates in any row, column or
%% box.
%%
-spec check_solution(solution_map(), integer(), integer()) -> solution_check().
check_solution(Solution, Box_rows, Box_cols) ->
    Numbers = lists:seq(1, Box_rows * Box_cols),
    Numbers_list = classify_cells(Solution, Box_rows, Box_cols),
    check_numbers(Numbers_list, Numbers, []).

%%-------------------------------------------------------------------
%%
%% Check that the list of `Nums' within each `Group' is same as the sequence
%% `1..N'.
%%
-spec check_numbers([[integer()]], [integer()], list()) ->
          ok | {not_ok, [{tuple(), [integer()]}]}.
check_numbers([], _Numbers, []) ->
    ok;

check_numbers([], _Numbers, Bad_cells) ->
    {not_ok, Bad_cells};

check_numbers([{Group, Nums}|Nums_list], Numbers, Bad_cells) ->
    Nums2 = lists:sort(Nums),
    case Nums2 of
        Numbers ->
            check_numbers(Nums_list, Numbers, Bad_cells);
        _ ->
            ?LOG_WARNING(#{func => ?FUNCTION_NAME,
                           group => group,
                           numbers => Nums2}),
            check_numbers(Nums_list, Numbers,
                          [{Group, Nums2}|Bad_cells])
    end.

%%-------------------------------------------------------------------
%%
%% Each cell belongs to a row, a column and a box. For each row, column and box
%% we create the list of numbers in each group.
%%
%% We return a list key/value pairs, where each key identifies a group, i.e.
%% `{row, Row}', `{col, Col}' and `{box, Box}', and the corresponding value is
%% the list of numbers in that group.
%%
-spec classify_cells(solution_map(), integer(), integer()) ->
          [{tuple(), [integer()]}].
classify_cells(Solution, Box_rows, Box_cols) ->
    Cell_numbers = maps:fold(fun ({R,C}, N, Cells) ->
                                     update_cells(R, C, N, Box_rows, Box_cols, Cells)
                             end, #{}, Solution),
    maps:to_list(Cell_numbers).

%%-------------------------------------------------------------------
%%
%% Update the groups of the given cell and its number.
%%
-spec update_cells(integer(), integer(), integer(), integer(), integer(), map()) -> map().
update_cells(Row, Col, Num, Box_rows, Box_cols, Cells) ->
    Box = box_of(Row, Col, Box_rows, Box_cols),
    Cells2 = update_map({row, Row}, Num, Cells),
    Cells3 = update_map({col, Col}, Num, Cells2),
    Cells4 = update_map({box, Box}, Num, Cells3),
    Cells4.

%%-------------------------------------------------------------------
%%
%% Add `Num' to the list of numbers in `Cells' corresponding to `Key'. This is
%% used for the classification of the cells.
%%
-spec update_map(tuple(), integer(), map()) -> map().
update_map(Key, Num, Cells) ->
    maps:update_with(Key,
                     fun (Numbers) -> [Num|Numbers] end,
                     [Num],
                     Cells).

%%-------------------------------------------------------------------
