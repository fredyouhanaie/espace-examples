%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% demo_0 is a generic pubsub demo module.
%%%
%%% `demo/0' can be used to kick everything off.
%%%
%%% This can be followed by a series of `publish/1' calls.
%%%
%%% With every `publish/1' call there should be a log message from
%%% each of the subscribers.
%%%
%%% Further subscribers can be initiated with the `client/1' function.
%%%
%%% @end
%%% Created :  6 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>

-module(demo_0).

%% API exports
-export([start/0, start/1, demo/1, client/1, client_init/1]).

-define(pubsub, pubsub_mod).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc default start up, run with the pubsub_0 module.
%%
%% `start' can be called from command line with `-s demo_1'.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
    start(pubsub_0).

%%--------------------------------------------------------------------
%% @doc
%% Start the demo with three subscribers for the supplied pubsub module.
%%
%% `start' can be called from command line with `-s demo_1 pubsub_0'.
%%
%% The subscribers are named `alpha', `beta' and `gamma'.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(list()|atom()) -> ok.
start([Pubsub]) when is_atom(Pubsub) ->
    start(Pubsub);

start(Pubsub) when is_atom(Pubsub) ->
    espace:start(),
    espace:out({?pubsub, Pubsub}),
    demo_1([alpha, beta, gamma]).

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
    Pubsub = get_pubsub_mod(),
    Pubsub:start(),
    lists:foreach(fun (Name) -> client(Name) end, Names).

%%--------------------------------------------------------------------
%% @doc
%% Run the end to end demo.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec demo_1(list()) -> ok.
demo_1(Names) ->
    Pubsub = get_pubsub_mod(),

    logger:notice("======= Starting demo_1"),
    demo(Names),
    timer:sleep(1000),

    logger:notice("======= First message"),
    Pubsub:publish("Hello, World!"),
    timer:sleep(1000),

    logger:notice("======= Second message"),
    Pubsub:publish("Hello, World, again!"),
    timer:sleep(1000),

    logger:notice("======= Adding another subscriber"),
    client(delta),
    timer:sleep(1000),

    logger:notice("======= One last message"),
    Pubsub:publish("Safety in numbers :-)"),
    timer:sleep(1000),

    logger:notice("======= End of demo, bye!"),
    Pubsub:stop().

%%--------------------------------------------------------------------
%% @doc
%% Start a pubsub subscriber worker.
%%
%% The subscriber, which will run in the background, will wait for new
%% data, then send the data to the logger. The log entries for a
%% particular subscriber are identified by the supplied `Name' and the
%% data sequence number.
%%
%% This is the best way to start a new subscriber from the shell,
%% since it will not block the shell.
%%
%% @end
%%--------------------------------------------------------------------
-spec client(atom()) -> ok.
client(Name) ->
    espace:worker({?MODULE, client_init, [Name]}).

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
-spec client_init(atom()) -> bottom.
client_init(Name) ->
    Pubsub = get_pubsub_mod(),
    case Pubsub:obtain_curr() of
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
-spec client_next(atom()) -> bottom.
client_next(Name) ->
    Pubsub = get_pubsub_mod(),
    {data, Seq, Data} = Pubsub:obtain_next(),
    logger:notice("~p: New data (#~b): ~p.~n", [Name, Seq, Data]),
    client_next(Name).

%%--------------------------------------------------------------------
%% @doc Get and return the pubsub module name.
%%
%% @end
%%--------------------------------------------------------------------

-spec get_pubsub_mod() -> atom().
get_pubsub_mod() ->
    logger:debug("getting pubsub module"),
    {[Pubsub], _} = espace:rd({?pubsub, '$1'}),
    logger:debug("got it: ~p.", [Pubsub]),
    Pubsub.
