%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%% A simple pubsub example using espace, but not as simple as
%%% `pubsub_0'.
%%%
%%% This variant can run on any named/unnamed espace instance. It can
%%% coexist with other independent `pubsub_1' instances within the
%%% same tuple space. The `pubsub_1' instances will be identified by
%%% unique `channel' names (atoms).
%%%
%%% There are no active server processes. Any process can use
%%% `publish/3' to send new data. They just need to know the tuple
%%% space and channel names.
%%%
%%% We will not maintain a list of subscribers. Processes interested
%%% in receiving the published data can come and go at will. In fact
%%% any process that uses the `obtain_curr/2' and/or `obtain_next/2'
%%% functions once, will be considered a subscriber!
%%%
%%% There are two groups of functions in this module, those for the
%%% service, i.e. `start/2', `stop/2', `publish/3', `obtain_curr/2'
%%% and `obtain_next/2', and those written specifically for the demo,
%%% i.e. the rest of the functions.
%%%
%%% After a channel is initialized with `start/2', `publish/3' can be
%%% used to add data items to the channel, and `obtain_*/2' can be
%%% used to receive copies of the data.
%%%
%%% `demo_1/0' can be used to kick off a single end to end demo.
%%%
%%% @end
%%% Created :  7 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>

-module(pubsub_1).

%% API exports
-export([start/2, stop/2]).
-export([publish/3, obtain_curr/2, obtain_next/2]).

%% Demo specific exports
-export([demo_1/0, demo/1, client/1, client_init/1]).
-define(TS, demo_1).
-define(CH, channel_1).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Initialize a single Channel within an existing tuple space.
%%
%% We expect the espace instance to have been started prior to this
%% call.
%%
%% We do not expect, and do not check, that the `Channel' has already
%% been creaeted.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(), atom()) -> ok | {error, channel_exists}.
start(Tspace, Channel) ->
    espace:out(Tspace, {pubsub, Channel, seq, 0}).

%%--------------------------------------------------------------------
%% @doc
%% Stop the application.
%%
%% We stop the espace instance, which in turn will remove all data and
%% workers directly started by espace.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom(), atom()) -> ok.
stop(Tspace, Channel) ->
    remove_channel(Tspace, Channel).

%%--------------------------------------------------------------------
%% @doc Remove a channel and all its data
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_channel(atom(), atom()) -> ok.
remove_channel(Tspace, Channel) ->
    case espace:inp(Tspace, {pubsub, Channel, seq, '$1'}) of
	nomatch ->
	    logger:warning("no channel to remove (~p)", [Channel]);
	{[Seq], _} ->
	    logger:notice("removing ~b messages from ~p.", [Seq, Channel])
    end,
    remove_data(Tspace, Channel).

%%--------------------------------------------------------------------
%% @doc Remove all the data on a channel.
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_data(atom(), atom()) -> ok.
remove_data(Tspace, Channel) ->
    case espace:inp(Tspace, {pubsub, Channel, data, '$1', '$2'}) of
	nomatch ->
	    logger:warning("no more data to remove (~p)", [Channel]),
	    ok;
	{[Seq, Data], _} ->
	    logger:notice("removed messages ~b (~p) from ~p.", [Seq, Data, Channel]),
	    remove_data(Tspace, Channel)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Publish a data item on a given channel and tuple space.
%%
%% The data item is added to the tuple space, and the sequence number
%% incremented.
%%
%% @end
%%--------------------------------------------------------------------
-spec publish(atom(), atom(), term()) -> ok.
publish(Tspace, Channel, Data) ->
    {[Seq], _} = espace:in(Tspace, {pubsub, Channel, seq, '$1'}),
    espace:out(Tspace, {pubsub, Channel, data, Seq+1, Data}),
    espace:out(Tspace, {pubsub, Channel, seq, Seq+1}).

%%--------------------------------------------------------------------
%% @doc
%% Return the most recent published data item.
%%
%% If nothing has been published, `nodata' is returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec obtain_curr(atom(), atom()) -> nodata | {data, integer(), term()}.
obtain_curr(Tspace, Channel)->
    {[Seq], _} = espace:rd(Tspace, {pubsub, Channel, seq, '$1'}),
    case Seq of
	0 ->
	    nodata;
	_ ->
	    {[Data], _} = espace:rd(Tspace, {pubsub, Channel, data, Seq, '$1'}),
	    {data, Seq, Data}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Wait for and return the next published data item.
%%
%% @end
%%--------------------------------------------------------------------
-spec obtain_next(atom(), atom()) -> {data, integer(), term()}.
obtain_next(Tspace, Channel)->
    {[Seq], _} = espace:rd(Tspace, {pubsub, Channel, seq, '$1'}),
    {[Data], _} = espace:rd(Tspace, {pubsub, Channel, data, Seq+1, '$1'}),
    {data, Seq+1, Data}.

%%====================================================================
%% Demo functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Run the end to end demo.
%%
%% @end
%%--------------------------------------------------------------------
-spec demo_1() -> ok.
demo_1() ->
    logger:notice("======= Starting demo_1"),
    espace:start(?TS),
    demo([alpha, beta, gamma]),
    timer:sleep(1000),

    logger:notice("======= First message"),
    publish(?TS, ?CH, "Hello, World!"),
    timer:sleep(1000),

    logger:notice("======= Second message"),
    publish(?TS, ?CH, "Hello, World, again!"),
    timer:sleep(1000),

    logger:notice("======= Adding another subscriber"),
    client(delta),
    timer:sleep(1000),

    logger:notice("======= One last message"),
    publish(?TS, ?CH, "Safety in numbers :-)"),
    timer:sleep(1000),

    logger:notice("======= End of demo, bye!"),
    stop(?TS, ?CH),
    espace:stop(?TS).

%%--------------------------------------------------------------------
%% @doc
%% Initialize and start a set of subscribers.
%%
%% The subscribers are identified by the list of names supplied as
%% argument.
%%
%% @end
%%--------------------------------------------------------------------
-spec demo(list()) -> ok.
demo(Names) ->
    start(?TS, ?CH),
    lists:foreach(fun (Name) -> client(Name) end, Names).

%%--------------------------------------------------------------------
%% @doc
%% Start a pubsub subscriber worker.
%%
%% The subscriber, which will run in the background, will wait for new
%% data, then send them to the logger. The log entries for a
%% particular subscriber are identified by the supplied `Name'.
%%
%% This is the best way to start a new subscriber from the shell,
%% since it will not block the shell.
%%
%% @end
%%--------------------------------------------------------------------
-spec client(term()) -> ok.
client(Name) ->
    espace:worker(?TS, {?MODULE, client_init, [Name]}).

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start a new subscriber.
%%
%% On start up the latest published data is logged. Thereafter, the
%% process will wait (forever) for the next data item to be published.
%%
%% If called from the shell, the shell will become inaccessible to the
%% interactive user.
%%
%% @end
%%--------------------------------------------------------------------
-spec client_init(term()) -> bottom.
client_init(Name) ->
    case obtain_curr(?TS, ?CH) of
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
-spec client_next(term()) -> bottom.
client_next(Name) ->
    {data, Seq, Data} = obtain_next(?TS, ?CH),
    logger:notice("~p: New data (#~b): ~p.~n", [Name, Seq, Data]),
    client_next(Name).
