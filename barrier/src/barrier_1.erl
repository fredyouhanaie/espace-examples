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

%%--------------------------------------------------------------------

start() ->
    espace:start().

%%--------------------------------------------------------------------

stop() ->
    espace:stop().

%%--------------------------------------------------------------------
