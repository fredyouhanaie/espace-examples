%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% The simulation for 5 philosophers can be started with
%%% `init(5)'. While running, every time a philosopher changes state,
%%% its new state will be printed on the console.
%%%
%%% The easiest way to stop the run is by quitting the shell, or by
%%% stopping espace with `espace:stop()'.
%%%
%%% @end
%%% Created : 13 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(dinphil_0).

%% API exports
-export([phil/1, init/1]).


%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initialize the tuple space, and kick off the workers.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(integer()) -> done.
init(N) when N > 0 ->
    espace:start(),

    Philosophers = lists:seq(0, N-1),
    lists:foreach(fun (I) -> espace:out({chopstick, I}) end, Philosophers),
    lists:foreach(fun (I) -> espace:worker({?MODULE, phil, [I]}) end, Philosophers),

    Room_tickets = lists:seq(1, N-1),
    lists:foreach(fun (_I) -> espace:out({room_ticket}) end, Room_tickets),

    espace:out({num, N}).

%%--------------------------------------------------------------------
%% @doc Single philosopher process.
%%
%% @end
%%--------------------------------------------------------------------
-spec phil(integer()) -> bottom.
phil(I) ->
    {[Num], _} = espace:rd({num, '$1'}),

    think(I, Num),

    espace:in({room_ticket}),
    espace:in({chopstick, I}),
    espace:in({chopstick, (I+1) rem Num}),

    eat(I, Num),

    espace:out({chopstick, I}),
    espace:out({chopstick, (I+1) rem Num}),
    espace:out({room_ticket}),

    phil(I).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc simulate thinking as a one second delay.
%%
%% @end
%%--------------------------------------------------------------------
-spec think(integer(), integer()) -> ok.
think(I, Num) ->
    io:format("~p~n", [state(I, Num, $T)]),
    timer:sleep(1000),
    io:format("~p~n", [state(I, Num, $W)]),
    ok.

%%--------------------------------------------------------------------
%% @doc simulate eating as a one second delay.
%%
%% @end
%%--------------------------------------------------------------------
-spec eat(integer(), integer()) -> ok.
eat(I, Num) ->
    io:format("~p~n", [state(I, Num, $E)]),
    timer:sleep(1000),
    ok.

%%--------------------------------------------------------------------
%% @doc construct a string representing the state of philosopher `I'.
%%
%% The state is a string of 'N' dots with the `S' representing the `I'th philosopher.
%%
%% @end
%%--------------------------------------------------------------------
-spec state(integer(), integer(), char()) -> string().
state(I, N, S) ->
    lists:duplicate(I, $.) ++ [S] ++ lists:duplicate(N-I-1, $.).
    
