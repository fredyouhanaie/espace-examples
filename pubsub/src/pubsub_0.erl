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
%%% @end
%%% Created :  6 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>

-module(pubsub_0).

%% API exports
-export([start/0, stop/0]).
-export([publish/1, obtain_curr/0, obtain_next/0]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start and initialize the application.
%%
%% We expect the unnamed espace instance to have been started.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
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
%% Wait for and return the next published data item.
%%
%% @end
%%--------------------------------------------------------------------
-spec obtain_next() -> {data, integer(), term()}.
obtain_next()->
    {[Seq], _} = espace:rd({pubsub, seq, '$1'}),
    {[Data], _} = espace:rd({pubsub, data, Seq+1, '$1'}),
    {data, Seq+1, Data}.
