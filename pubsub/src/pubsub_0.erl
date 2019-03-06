%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%% A very simple pubsub example using espace.
%%%
%%% This variant will only run on the unnamed espace instance. It does
%%% not expect other `pubsub_0' instances to be active. We do not
%%% maintain a list of subscribers. Processes interested in receiving
%%% the published data can come and go at will.
%%%
%%% `pubsub_0:start/0' is used to start and initialize the application.
%%%
%%% `pubsub_0:publish/1' is used to publish new data
%%% items. `publish/1' effectively adds the data item to the tuple
%%% space, along with a sequence number, e.g. `{pubsub, data, 5,
%%% "Hello, World!"}' would be the fifth item published.
%%%
%%% A single tuple is used to keep track of the last sequence number
%%% published. Zero indicates that no data has been published.
%%%
%%% A "subscriber" can use `obtain_curr/0' to get the latest data that
%%% has been published. `obtain_next/0' can be used to wait and
%%% receive the next item to be published.
%%%
%%% `demo/0' can be used to kick everything off. This can be followed
%%% by a series of `publish/1' calls. With every `publish/1' call
%%% there should be a log message from each of the clients.
%%%
%%% Further clients can be initiated with the `client/1' function.
%%%
%%% @end
%%% Created :  6 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>

-module(pubsub_0).

%% API exports
-export([start/0, stop/0]).
-export([publish/1, obtain_curr/0, obtain_next/0]).
-export([demo/0, demo/1, client/1, client_init/1]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start and initialize the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
    espace:start(),
    espace:out({pubsub, seq, 0}).

%%--------------------------------------------------------------------
%% @doc
%% Stop the application.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    espace:stop().

%%--------------------------------------------------------------------
%% @doc
%% Publish a data item.
%%
%% The data item is added to the tuple space, and the sequence number
%% incremented.
%%
%% @end
%%--------------------------------------------------------------------
-spec publish(term()) -> ok.
publish(Data) ->
    {[Seq], _} = espace:in({pubsub, seq, '$1'}),
    espace:out({pubsub, data, Seq+1, Data}),
    espace:out({pubsub, seq, Seq+1}).

%%--------------------------------------------------------------------
%% @doc
%% Return the most recent published data item.
%%
%% If nothing has been published, `nodata' is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec obtain_curr() -> nodata | {data, integer(), term()}.
obtain_curr()->
    {[Seq], _} = espace:rd({pubsub, seq, '$1'}),
    case Seq of
	0 ->
	    nodata;
	_ ->
	    {[Data], _} = espace:rd({pubsub, data, Seq, '$1'}),
	    {data, Seq, Data}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Wait and return the next published data item.
%%
%% @end
%%--------------------------------------------------------------------
-spec obtain_next() -> {data, integer(), term()}.
obtain_next()->
    {[Seq], _} = espace:rd({pubsub, seq, '$1'}),
    {[Data], _} = espace:rd({pubsub, data, Seq+1, '$1'}),
    {data, Seq+1, Data}.

%%--------------------------------------------------------------------
%% @doc
%% Start the demo with three clients.
%%
%% The clients are named `alpha', `beta' and `gamma'.
%%
%% @end
%%--------------------------------------------------------------------
-spec demo() -> ok.
demo() ->
    demo([alpha, beta, gamma]).

%%--------------------------------------------------------------------
%% @doc
%% Initialize and start a set of clients.
%%
%% The clients are identified by the list of names supplied as
%% argument.
%%
%% @end
%%--------------------------------------------------------------------
-spec demo(list()) -> ok.
demo(Names) ->
    start(),
    lists:foreach(fun (Name) -> client(Name) end, Names).

%%--------------------------------------------------------------------
%% @doc
%% Start a pubsub client worker.
%%
%% The client, which will run in the background, will wait for new
%% data, then send them to the logger. The log entries for a
%% particular client are identified by the supplied `Name'.
%%
%% This is the best way to start a new client from the shell.
%%
%% @end
%%--------------------------------------------------------------------
-spec client(term()) -> ok.
client(Name) ->
    espace:worker({?MODULE, client_init, [Name]}).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start a new client.
%%
%% On start up the latest published data is logged. There after the
%% process will wait (forever) for the next data item to be published.
%%
%% If called from the shell, the shell will become inaccessible to the
%% interactive user.
%%
%% @end
%%--------------------------------------------------------------------
-spec client_init(term()) -> undefined.
client_init(Name) ->
    case obtain_curr() of
	nodata ->
	    logger:notice("~p: No data yet.~n", [Name]);
	{data, Seq, Data} ->
	    logger:notice("~p: Current data (#~b): ~p.~n", [Name, Seq, Data])
    end,
    client_next(Name).

%%--------------------------------------------------------------------
%% @doc
%% Wait for the next data item, then send it to the logger.
%%
%% @end
%%--------------------------------------------------------------------
-spec client_next(term()) -> undefined.
client_next(Name) ->
    {data, Seq, Data} = obtain_next(),
    logger:notice("~p: New data (#~b): ~p.~n", [Name, Seq, Data]),
    client_next(Name).
