%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2022, Fred Youhanaie
%%% @doc
%%%
%%% Common definitions for the `tsudoku' modules.
%%%
%%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% The puzzle and the solution grid can be list of lists, or a map of
%% `{Row,Col}=>Num' elements.
%%
-type(puzzle_list() :: [[ integer() ]]).
-type(puzzle_map() :: #{{integer(), integer()} => integer()}).
-type(puzzle_map_nested() :: #{integer() => #{integer() => integer()}}).

%%--------------------------------------------------------------------
%% The result of a puzzle/solution check.
%%
-type(puzzle_check() :: ok | {not_ok, [{tuple(), [integer()]}]}).

%%--------------------------------------------------------------------
%% Each cell belongs to three groups: row, column and box.
%%
-type(cell_group() :: {row, integer()} |
                      {col, integer()} |
                      {box, {integer(), integer()}}).

%%--------------------------------------------------------------------
