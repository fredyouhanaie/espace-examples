%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% This is same as dinphil_0, but with the io:format/2 calls replaced
%%% with event tuples being added to the tuple space. These event
%%% tuples can in turn be picked up by a monitor process.
%%%
%%% The function `philmon:tail/0,1' can be used to continuously update and list
%%% the pholosopher states.
%%%
%%% Each philospher can be in one of the following states:
%%%
%%% <ul>
%%% <li>`_T_' thinking</li>
%%% <li>`_W_' waiting to enter the dining room, only `N-1'
%%% philosophers allowed to enter the room.</li>
%%% <li>`_s_' sitting, waiting for the left chopstick</li>
%%% <li>`Ls_' got the left chopstick, waiting for the right chopstick</li>
%%% <li>`LeR' got both chopsticks, now eating </li>
%%% <li>`_.R' left chopstick down, about to put down the right chopstick</li>
%%% <li>`_._' both chopsticks down, about to leave the room</li>
%%% </ul>
%%%
%%% The easiest way to stop the run is by quitting the erl shell.
%%%
%%% @end
%%% Created : 19 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(dinphil_1).

%% API exports
-export([phil/1, init/1]).

% default thinking or eating time, in seconds.
-define(Delay, 2).


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

    espace:out({eventnum, 0}),
    espace:out({num, N}).


%%--------------------------------------------------------------------
%% @doc Single philosopher process.
%%
%% @end
%%--------------------------------------------------------------------
-spec phil(integer()) -> bottom.
phil(I) ->
    {[Num], _} = espace:rd({num, '$1'}),

    phil_event(I, '_T_'),
    think(),

    phil_event(I, '_W_'),
    espace:in({room_ticket}),

    phil_event(I, '_s_'),
    espace:in({chopstick, I}),

    phil_event(I, 'Ls_'),
    espace:in({chopstick, (I+1) rem Num}),

    phil_event(I, 'LeR'),
    eat(),

    espace:out({chopstick, I}),
    phil_event(I, '_.R'),

    espace:out({chopstick, (I+1) rem Num}),
    phil_event(I, '_._'),

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
-spec think() -> ok.
think() ->
    timer:sleep(1000*?Delay).


%%--------------------------------------------------------------------
%% @doc simulate eating as a one second delay.
%%
%% @end
%%--------------------------------------------------------------------
-spec eat() -> ok.
eat() ->
    timer:sleep(1000*?Delay).


%%--------------------------------------------------------------------
%% @doc Record a philosopher event.
%%
%% All events are numbered starting at 1.
%%
%% @end
%%--------------------------------------------------------------------

phil_event(Phil, Event) ->
    {[Event_num], _} = espace:in({eventnum, '$1'}),
    espace:out({event, Event_num+1, Phil, Event}),
    espace:out({eventnum, Event_num+1}).
