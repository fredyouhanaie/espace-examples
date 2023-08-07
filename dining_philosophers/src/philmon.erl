%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% Monitor the progress of the dining philosophers.
%%%
%%% Use `start/0,1' to start the whole simulation and tail the events
%%% log.
%%%
%%% Use `tail/0,1' to tail the events log for a simulation that is
%%% already running.
%%%
%%% @end
%%% Created : 20 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------

-module(philmon).

%% API exports
-export([start/0, start/1, tail/0, tail/1]).

% default thinking or eating time, in seconds.
-define(Delay, 2).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start the simulation for 5 philosophers and tail the events.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> bottom.
start() ->
    start(5).

%%--------------------------------------------------------------------
%% @doc Start the simulation for `Nphil' philosophers and tail the
%% events.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(integer()) -> bottom.
start(Nphil) ->
    dinphil_1:init(Nphil),
    tail(1).

%%--------------------------------------------------------------------
%% @doc read and process the events, starting from the last event.
%%
%% If event number is `0', i.e. no events yet, then we wait for the
%% first one.
%%
%% @end
%%--------------------------------------------------------------------
-spec tail() -> bottom.
tail() ->
    {[Eventnum], _} = espace:rd({eventnum, '$1'}),
    case Eventnum of
	0 ->
	    tail(1);
	_ ->
	    tail(Eventnum)
    end.

%%--------------------------------------------------------------------
%% @doc read and process the events, start with `Eventnum'.
%%
%% This is the initial call, we create a default list of philosopher
%% states and pass it to `tail/2'.
%%
%% @end
%%--------------------------------------------------------------------
-spec tail(integer()) -> bottom.
tail(Eventnum) ->
    {[Num], _} = espace:rd({num, '$1'}),
    Phil_status = lists:duplicate(Num, '...'),
    tail(Eventnum, Phil_status).

%%--------------------------------------------------------------------
%% @doc read an event, and process the events.
%%
%% We wait for the next event, with id `Eventnum', and update the
%% philosopher status list. The list is then printed out.
%%
%% @end
%%--------------------------------------------------------------------
-spec tail(integer(), string()) -> bottom.
tail(Eventnum, Status) ->
    {[Phil, Event], _} = espace:rd({event, Eventnum, '$1', '$2'}),
    Status2 = lists:sublist(Status, Phil) ++ [Event] ++ lists:nthtail(Phil+1, Status),
    Out_status = lists:flatten(
		   lists:join(" ",
			      lists:map(fun erlang:atom_to_list/1, Status2)
			     )),
    io:format("~3b ~s~n", [Eventnum, Out_status]),
    tail(Eventnum+1, Status2).

%%--------------------------------------------------------------------
