%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc Run the unit tests
%%%
%%% @end
%%% Created : 16 Oct 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-module(barrier_1_tests).

-include_lib("eunit/include/eunit.hrl").

%% per process function to sync with the rest
-define(Sync_fun, fun (P,N) -> barrier_1:sync(ready, P, N) end).

%% check that process P has reached the end
-define(Done_fun, fun(P) -> ?assert(espace:in({done, P, ok}) == {[], {done, P, ok}}) end).

%% test for N process barrier
-define(Sync_N_test(N_procs), 
	logger:set_primary_config(level, error),
	espace:start(),

	Proc = fun (I) -> espace:eval({done, I, {?Sync_fun, [I, N_procs]}}) end,
	lists:foreach(Proc, lists:seq(0, N_procs-1)),

	lists:foreach(?Done_fun, lists:seq(0, N_procs-1)),

	espace:stop()
).

%% test for various process group sizes
sync_1_test() -> ?Sync_N_test(1).
sync_2_test() -> ?Sync_N_test(2).
sync_4_test() -> ?Sync_N_test(4).
sync_6_test() -> ?Sync_N_test(6).
sync_8_test() -> ?Sync_N_test(8).
sync_9_test() -> ?Sync_N_test(9).
