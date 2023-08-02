%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2023, Fred Youhanaie
%%% @doc Barrier Synchronization.
%%%
%%% A barrier allows multiple processes working on a computation to
%%% sychronize with each other before proceeding to the next stage.
%%%
%%% This module is for demonstration only. It has been included here
%%% for comparison only. It uses one of the slowest possible methods.
%%%
%%% @end
%%% Created : 1 Aug 2023 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(barrier_1).

-export([start/0, stop/0]).
-export([sync/3, run_N/2]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------

-spec start() -> ok.
start() ->
    logger:set_primary_config(#{level => notice}),
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    espace:start(),
    ok.

%%--------------------------------------------------------------------

-spec stop() -> ok.
stop() ->
    espace:stop(),
    ok.

%%--------------------------------------------------------------------

-spec sync(term(), integer(), integer()) -> ok.
sync(Tag, My_proc, N_procs) ->
    espace:out({Tag, My_proc, ready}),
    [ espace:rd({Tag, I, ready}) || I <- lists:seq(0, N_procs-1)--[My_proc] ],
    espace:in({Tag, My_proc, ready}),
    ok.

%%--------------------------------------------------------------------
%% @doc Reset the barrier for the given `Tag'.
%%
%% All the `{Tag, Proc_num, Buddy_num}' tuples will be removed from
%% the tuple space.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(term()) -> ok.
reset(Tag) ->
    case espace:inp({Tag, '_', '_'}) of
        nomatch ->
            ok;
        _ ->
            reset(Tag)
    end.

%%--------------------------------------------------------------------
%% @doc simple function for testing and troubleshooting.
%%
%% Creates `N_procs' `eval's, each in turn will synchronize with the
%% rest.
%%
%% each `eval' will result in a tuple of the following form in the
%% tuple space `{done, Proc_num, ok}'.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_N(term(), integer()) -> ok.
run_N(Tag, N_procs) ->
    reset(Tag),
    %% kick off the processes
    [ espace:eval({done, I, {fun run_1/3, [Tag, I, N_procs]} })
      || I <- lists:seq(0, N_procs-1) ],
    %% wait for and collect the left-over tuples
    [ espace:in({done, I, ok})
      || I <- lists:seq(0, N_procs-1) ],
    ok.

%%--------------------------------------------------------------------
%% @doc A single process with rank `Proc_N' to sync with the rest of
%% the `N_procs' partners'
%%
%% @end
%%--------------------------------------------------------------------
-spec run_1(term(), integer(), integer()) -> ok.
run_1(Tag, Proc_N, N_procs) ->
    ?LOG_NOTICE("Proc ~p started, pre-sync.", [Proc_N]),
    sync(Tag, Proc_N, N_procs),
    ?LOG_NOTICE("Proc ~p post-sync, stopped", [Proc_N]),
    ok.

%%--------------------------------------------------------------------
