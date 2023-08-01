%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc Barrier Synchronization using the Butterfly Barrier.
%%%
%%% A barrier allows multiple processes working on a computation to
%%% sychronize with each other before proceeding to the next stage.
%%%
%%% The butterfly barrier is an efficient method of synchronization
%%% where each process only needs to synchronize with `log2(N)' other
%%% processes for a group of `N' processes.
%%%
%%% A worker process, when ready, should call `sync/3' to
%%% synchronize. Return from the function is a confirmation that ALL
%%% the other processes are ready too.
%%%
%%% @end
%%% Created : 15 Oct 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(barrier_0).

-export([start/0, stop/0]).
-export([sync/3, reset/1, run_N/2]).

%%--------------------------------------------------------------------

start() ->
    logger:set_primary_config(#{level => notice}),
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    espace:start().

%%--------------------------------------------------------------------

stop() ->
    espace:stop().

%%--------------------------------------------------------------------
%% @doc Sync function to be called by each process.
%%
%% Once the `sync/3' return, we can be sure that all the other partner
%% processes have reached the barrier.
%%
%% All the partner processes should use the same `Tag'. This can be
%% any Erlang `term'.
%%
%% Each partner process is identified by a unique rank, `My_proc'. The
%% rank starts from `0' and goes up to `N_procs-1'.
%%
%% The synchronization takes `log2' steps. This is rounded to the next
%% integer, if `N_procs' is not a power of 2.
%%
%% @end
%%--------------------------------------------------------------------
-spec sync(term(), integer(), integer()) -> ok.
sync(Tag, My_proc, N_procs) ->
    logger:notice("Proc ~p: sync started", [My_proc]),
    espace:out({Tag, My_proc}),
    Steps = trunc(math:ceil(math:log2(N_procs))),
    sync(Tag, My_proc, N_procs, Steps, 1),
    logger:notice("Proc ~p: sync completed", [My_proc]).


%%--------------------------------------------------------------------
%% @doc Wait for each of the `N_procs' buddies to become ready.
%%
%% Each partner process is identified by a unique rank in the range
%% `0' to `N_procs-1'.
%%
%% The buddies of a process are identified by flipping the individual
%% bits of the process number during each step, starting with the
%% least significant bit.
%%
%% For example, in a group of 8 processes, process `0' (`2#000') will
%% in turn check the status of `2#001' (1), `2#010' (2) and `2#100'
%% (4). Likewise process `3' (`2#011') will check `2#010', `2#001' and
%% `2#111'.
%%
%% If the number of processes is not a power of 2, then a real process
%% skips checking the buddy at the non-existent rank.
%%
%% @end
%%--------------------------------------------------------------------
-spec sync(term(), integer(), integer(), integer(), integer()) -> ok.
sync(_Tag, _My_proc, _N_procs, 0, _Mask) ->
    ok;

sync(Tag, My_proc, N_procs, Steps, Mask) ->
    Buddy = My_proc bxor Mask,
    if
	Buddy < N_procs ->
	    espace:rd({Tag, Buddy});
	true ->
	    ok
    end,
    sync(Tag, My_proc, N_procs, Steps-1, Mask bsl 1).


%%--------------------------------------------------------------------
%% @doc Reset the barrier for the given `Tag'.
%%
%% All the `{Tag, Proc_num}' tuples will be removed from the tuple
%% space.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(term()) -> ok.
reset(Tag) ->
    case espace:inp({Tag, '_'}) of
	nomatch ->
	    ok;
	_ ->
	    reset(Tag)
    end.


%%--------------------------------------------------------------------
%% @doc simple function for testing and troubleshooting.
%%
%% Creates `N_procs' `eval's, which in turn will synchronize with the
%% rest.
%%
%% @end
%%--------------------------------------------------------------------
-spec run_N(term(), integer()) -> ok.
run_N(Tag, N_procs) ->
    reset(Tag),
    F = fun(I) -> espace:eval({done, I, {fun run_1/3, [Tag, I, N_procs]} }) end,
    lists:foreach(F, lists:seq(0, N_procs-1)),
    ok.


%%--------------------------------------------------------------------
%% @doc A single process with rank `Proc_N' to sync with the rest of
%% the `N_procs' partners'
%%
%% @end
%%--------------------------------------------------------------------
-spec run_1(term(), integer(), integer()) -> ok.
run_1(Tag, Proc_N, N_procs) ->
    logger:notice("Proc ~p started, pre-sync.", [Proc_N]),
    sync(Tag, Proc_N, N_procs),
    logger:notice("Proc ~p post-sync, stopped", [Proc_N]),
    ok.
