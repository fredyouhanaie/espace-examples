%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc
%%%
%%% This module is a collection of functions for solving sudoku puzzles using
%%% one of the simplest known methods. Initially all cells are given either
%%% their assigned number from the puzzle, or the set of all numbers `1..N' as
%%% candidates. Then, the numbers assigned to the buddy cells of a cell, i.e.
%%% those in the same row/column/box as the cell, are eliminated from the set of
%%% candidate numbers of that cell until all the cells are left with one number
%%% each.
%%%
%%% Multiple specialist workers carry out the computation by broadcasting facts
%%% about specific cells via the tuple space:
%%%
%%% <ul>
%%%
%%% <li>The main solver function starts the worker processes and assigns numbers
%%% to individual cells as defined by the puzzle. The rest of the computation
%%% will then continue in the background. The process that initiates the solver,
%%% can then wait for the `{done, Solution}' tuple.</li>
%%%
%%% <li>`collector' will wait (forever) for the `NxN' `{solved,...}' tuples (one
%%% per cell). For each `{solved,...}' tuple received a `{cellcast,...}' tuple
%%% will be generated. Which in turn will be picked up by the various relay
%%% workers. Once the worker has collected all the `NxN' tuples, it will clean
%%% up the TS, check the solution, and drom the solution tuple, `{solution,
%%% Solution}', into the tuple space.</li>
%%%
%%% <li>For each cell there is a worker that will wait for `{cell,...}' tuples
%%% targeted at its row/col number and updates its own state accordingly (list
%%% of candidate numbers). Once the cell worker is left with only one number, it
%%% will output a `{solved,...}' tuple and terminates.</li>
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
%%% tuples for all the cells in the same row as the cell in the tuple.</li>
%%%
%%% <li>`relay_col' waits for `{colcast,...}' tuples and generates `{cell,...}'
%%% tuples for all the cells in the same column as the cell in the tuple.</li>
%%%
%%% <li>`relay_box' waits for `{boxcast,...}' tuples and generates `{cell,...}'
%%% tuples for all the cells in the same box as the cell in the tuple.</li>
%%%
%%% </ul>
%%%
%%% The overall flow of a `[tuple]' between `(workers)' is as follows:
%%%
%%% <ol>
%%%
%%% <li> (solver) -> [cell] -> (cell)</li>
%%% <li> (cell) -> [solved] -> (collector) -> [cellcast] -> (relay_cell)</li>
%%% <li> (relay_cell) -> [rowcast] -> (relay_row) -> [cell] -> (cell)</li>
%%% <li> (relay_cell) -> [colcast] -> (relay_col) -> [cell] -> (cell)</li>
%%% <li> (relay_cell) -> [boxcast] -> (relay_box) -> [cell] -> (cell)</li>
%%%
%%% </ol>
%%%
%%% The relay workers are stateless, so we can run multiple relays for very
%%% large puzzles to improve performance. Currently there is no provision for
%%% multiple relay workers, however, this can easily be achieved with startup
%%% config parameters, or automatically computed based on the size of the
%%% puzzle.
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
%%% worker at `{Row,Col}'. The `Msg' either addresses the cell itself or one of
%%% its buddies.</li>
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

-include_lib("kernel/include/logger.hrl").
-include_lib("tsudoku.hrl").

%%-------------------------------------------------------------------

-define(Log_level, warning).

%%-------------------------------------------------------------------
%% @doc Read a puzzle definition from a file and start the solver.
%%
%% The format of the file is described in `tsudoku_lib:read_file/1'.
%%
%% @end
%%-------------------------------------------------------------------
-spec solve_file(string()) -> {error, term()} | ok.
solve_file(File) ->
    logger:set_primary_config(level, ?Log_level),
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    case tsudoku_lib:read_puzzle(File) of
        Error = {error, _} ->
            Error;
        {{Box_rows, Box_cols}, Puzzle} ->
            application:ensure_started(espace),
            solve(Puzzle, Box_rows, Box_cols)
    end.

%%-------------------------------------------------------------------
%% @doc Solve a puzzle.
%%
%% We expect the espace application to be running.
%%
%% @end
%%-------------------------------------------------------------------
-spec solve(puzzle_map()|puzzle_list(), integer(), integer()) -> ok.
solve(Puzzle, Box_rows, Box_cols) ->
    N_rows_cols = Box_rows * Box_cols,

    %% start the relay workers
    %%
    espace:worker({fun relay_cellcast/0, []}),
    espace:worker({fun relay_rowcast/1, [N_rows_cols]}),
    espace:worker({fun relay_colcast/1, [N_rows_cols]}),
    espace:worker({fun relay_boxcast/3, [Box_rows,Box_cols, #{}]}),

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

    %% start the collector
    %%
    espace:worker({fun collector/4, [N_cells, #{}, Box_rows, Box_cols]}),
    ok.

%%-------------------------------------------------------------------
%% @doc Assign the known cell numbers to the cells.
%%
%% For each cell with non-zero number we output a `{cell,...}' tuple which is
%% picked up by the corresponding cell worker.
%%
%% @end
%%-------------------------------------------------------------------
-spec assign_cells(puzzle_map()|puzzle_list()) -> integer().
assign_cells(Puzzle) when is_map(Puzzle) ->
    lists:foreach(fun ({{_Row, _Col}, 0}) ->
                          ok;
                      ({{Row, Col}, Num}) ->
                          out_cell(Row, Col, {Row, Col, Num})
                  end,
                  maps:to_list(Puzzle));

assign_cells(Puzzle) when is_list(Puzzle) ->
    lists:foldl(fun assign_rows/2, 0, Puzzle).

%%-------------------------------------------------------------------
%% @doc Assign the known cell numbers for one row of a puzzle.
%%
%% For each cell with non-zero number we output a `{cell,...}' tuple which is
%% picked up by the corresponding cell worker.
%%
%% @end
%%-------------------------------------------------------------------
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
%% @doc Check for, warn about and remove the leftover `{solved,...}' tuples.
%%
%% This is precautionary, shouldn't need to call this! It can be removed in
%% future.
%%
%% @end
%%-------------------------------------------------------------------
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
%% @doc Remove the leftover `{cell,...}' tuples from the tuple space.
%%
%% The cell workers terminate when they have one number left, however, the relay
%% workers will still generate `{cell,...}' tuples for those cells. So, we need
%% to clean up these tuples in order to finish off with a clean tuple space.
%%
%% @end
%%-------------------------------------------------------------------
-spec drain_cells() -> ok.
drain_cells() ->
    case espace:inp({cell, '_', '_', '_'}) of
        nomatch ->
            ok;
        {[], _Tuple} ->
            drain_cells()
    end.

%%-------------------------------------------------------------------
%% @doc Collect the solution tuples.
%%
%% wait for and collect all the `{solved, Row, Col, Num}' tuples. We expect
%% `N_solutions` tuples. The function will block until ALL tuples have been
%% collected.
%%
%% Each time a cell is solved, we broadcast the fact to the relay workers via
%% tuples of type `{cellcast, Row, Col, Num}'.
%%
%% The solved puzzle is returned as a map. It can later be converted to a list
%% of lists with `puzzle_to_list/1'.
%%
%% @end
%%-------------------------------------------------------------------
-spec collector(integer(), puzzle_map(), integer(), integer()) -> ok.
collector(0, Solution, Box_rows, Box_cols) ->
    drain_solved(),
    drain_cells(),
    Solution_ok = tsudoku_lib:check_solution(Solution, Box_rows, Box_cols),
    espace:out({done, {Solution_ok, tsudoku_lib:puzzle_to_list(Solution)}});

collector(N_solutions, Solution, Box_rows, Box_cols) ->
    case espace:in({solved, '$1', '$2', '$3'}) of
        quit ->
            ok;
        {[Row, Col, Num], _} ->
            espace:out({cellcast, Row, Col, Num}),
            collector(N_solutions-1, maps:put({Row, Col}, Num, Solution), Box_rows, Box_cols)
    end.

%%-------------------------------------------------------------------
%% @doc The cell worker.
%%
%% There is one worker process per cell, identified by `Row'/`Col'. Each worker
%% keeps track of its own set of candidate numbers, `Numbers'. Once our set of
%% candidates becomes a single number, we broadcast that we're done.
%%
%% On start up, `Numbers' will be the set of number `1..N'. We wait for tuples
%% of the form `{cell, Row, Col, {RR, CC, Num}}'. If `RR'/`CC' is our address,
%% then we've been assigned a number, which is what happens during the start up.
%%
%% Otherwise, we're being told that one of our buddies has got `Num', so we take
%% it out of our list of candidates.
%%
%% @end
%%-------------------------------------------------------------------
-spec cell(integer(), integer(), [integer()]) -> ok.
cell(Row, Col, [Num]) when is_integer(Num) -> %% only one number left, we're done :-)
    ?LOG_NOTICE(#{func=>?FUNCTION_NAME, row_col=>{Row, Col}, num=>Num}),
    espace:out({solved, Row, Col, Num});

cell(Row, Col, Numbers) ->
    ?LOG_NOTICE(#{func=>?FUNCTION_NAME, row_col=>{Row, Col}, numbers=>Numbers}),
    case espace:in({cell, Row, Col, '$3'}) of
        quit ->
            ok;
        {[Msg], _} ->
            case Msg of
                {Row, Col, Num} -> %% this is us, we're assigned a number
                    espace:out({solved, Row, Col, Num});
                {_RR, _CC, Num} -> %% it must be one of our buddies
                    cell(Row, Col, Numbers -- [Num]) %% take it out of our list of candidates
            end
    end.

%%-------------------------------------------------------------------
%% @doc Drop a `{cell,...}' tuple into the tuple space.
%%
%% @end
%%-------------------------------------------------------------------
-spec out_cell(integer(), integer(), {integer(), integer(), integer()}) -> done.
out_cell(Row, Col, Msg) ->
    ?LOG_NOTICE(#{func=>?FUNCTION_NAME, row=>Row, col=>Col, msg=>Msg}),
    espace:out({cell, Row, Col, Msg}).

%%-------------------------------------------------------------------
%% @doc Relay a cell message to the row, column and box of cell.
%%
%% Wait for a `{cellcast, Row, Col, Num}' tuple and forward it to the row,
%% column and box relay workers corresponding to the cell at `Row'/`Col'.
%%
%% @end
%%-------------------------------------------------------------------
-spec relay_cellcast() -> ok.
relay_cellcast() ->
    case espace:in({cellcast, '$1', '$2', '$3'}) of
        quit ->
            ok;
        {[Row, Col, Num], _} ->
            Msg = {Row, Col, Num},
            espace:out({rowcast, Row, Msg}),
            espace:out({colcast, Col, Msg}),
            espace:out({boxcast, Row, Col, Msg}),
            relay_cellcast()
    end.

%%-------------------------------------------------------------------
%% @doc Relay `Msg' to the cells within a row.
%%
%% Wait for a `{rowcast, Row, Msg}' tuple and forward it to the cells within
%% that row.
%%
%% @end
%%-------------------------------------------------------------------
-spec relay_rowcast(integer()) -> ok.
relay_rowcast(N_cols) ->
    case espace:in({rowcast, '$1', '$2'}) of
        quit ->
            ok;
        {[Row, Msg], _} ->
            Out_fun = fun (Col) -> out_cell(Row, Col, Msg) end,
            lists:foreach(Out_fun, lists:seq(0, N_cols-1)),
            relay_rowcast(N_cols)
    end.

%%-------------------------------------------------------------------
%% @doc Relay `Msg' to the cells within a column.
%%
%% Wait for a `{colcast, Col, Msg}' tuple and forward it to the cells within
%% that column.
%%
%% @end
%%-------------------------------------------------------------------
-spec relay_colcast(integer()) -> ok.
relay_colcast(N_rows) ->
    case espace:in({colcast, '$1', '$2'}) of
        quit ->
            ok;
        {[Col, Msg], _} ->
            Out_fun = fun (Row) -> out_cell(Row, Col, Msg) end,
            lists:foreach(Out_fun, lists:seq(0, N_rows-1)),
            relay_colcast(N_rows)
    end.

%%-------------------------------------------------------------------
%% @doc Relay a `Msg' to the cells within a box.
%%
%% Wait for a `{boxcast, Row, Col, Msg}' tuple and forward it to the cells
%% within that box.
%%
%% The parameter `Box_map' acts as a cache of results from previous
%% calls to `box_of/4'. When the function (worker) is first called,
%% `Box_map' should have the value `#{}'.
%%
%% @end
%%-------------------------------------------------------------------
-spec relay_boxcast(integer(), integer(), map()) -> ok.
relay_boxcast(Box_rows, Box_cols, Box_map) ->
    case espace:in({boxcast, '$1', '$2', '$3'}) of
        quit ->
            ok;
        {[Row, Col, Msg], _} ->
            Box_map2 = case maps:get({Row, Col}, Box_map, none) of
                           none ->
                               {R_base, C_base} = tsudoku_lib:box_of(Row, Col, Box_rows, Box_cols),
                               maps:put({Row, Col}, {R_base, C_base}, Box_map);
                           {R_base, C_base} ->
                               Box_map
                       end,
            Out_fun = fun ({R, C}) -> out_cell(R, C, Msg) end,
            lists:foreach(Out_fun, [{R_base+R, C_base+C} ||
                                       R <- lists:seq(0,Box_cols-1),
                                       C <- lists:seq(0,Box_rows-1)]),
            relay_boxcast(Box_rows, Box_cols, Box_map2)
    end.

%%-------------------------------------------------------------------
