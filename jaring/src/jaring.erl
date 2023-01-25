%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2021, Fred Youhanaie
%%% @doc
%%%
%%% The `espace' based implementation of the `ring bencmark' from Joe's book.
%%%
%%% The benchmark is performed with `N+1' process. The first node and `N' relay
%%% nodes. The nodes communicate in a unidirectional ring formation, where node
%%% `X' forwards messages to node `X+1'. The last node forwards messages to node
%%% `0'.
%%%
%%% All communication between nodes is carried out via the `espace' tuple space.
%%% A send is just an `out' operation, while a receive is an `in' operation.
%%%
%%% Each node worker is given a node number, a next node number, and the message
%%% number to wait for. The node waits for
%%%
%%% @end
%%% Created :  2021-12-03 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(jaring).

-export([start/0, start/2]).

%%--------------------------------------------------------------------

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% default parameters
%%
-define(Nodes, 3).
-define(Messages, 4).
-define(Log_level, error).

%%--------------------------------------------------------------------
%% start a run with the default parameters
%%
start() ->
    start(?Nodes, ?Messages).

%%--------------------------------------------------------------------
%% start a run with the supplied paramater
%%
start(Nodes, Messages) ->
    logger:set_handler_config(default, formatter, {logger_formatter, #{}}),
    logger:set_primary_config(level, ?Log_level),

    espace:start(),

    {Elapsed, ok} = timer:tc(fun run/2, [Nodes, Messages]),
    Counts = espace_opcount:counts(),

    espace:stop(),

    #{counts => Counts,
      seconds => Elapsed / 1_000_000,
      us_per_msg => Elapsed / (Nodes*Messages),
      nodes => Nodes,
      messages => Messages
     }.

%%--------------------------------------------------------------------
%% @doc Spawn N+1 workers.
%%
%% The first node (node no. `0') is a special one, it initiates the whole relay
%% process.
%%
%% The rest of the nodes, except the last one, receive a message and
%% send it to the next in sequence.
%%
%% The last node (node no. `Nodes') is a relay node that always
%% forwards to the first node.
%%
%% @end
%%--------------------------------------------------------------------
-spec run(integer(), integer()) -> ok.
run(Nodes, Msgs) ->
    %% start the first node of the ring
    espace:worker({fun first_node/1, [Msgs]}),
    %% start the rest of the nodes (except the last one)
    [ espace:worker({fun relay_node/3, [N, N+1, Msgs]}) ||
        N <- lists:seq(1, Nodes-1) ],
    %% start the last node of the ring
    espace:worker({fun relay_node/3, [Nodes, 0, Msgs]}),

    {[], {all_done}} = espace:in({all_done}),
    ok.

%%--------------------------------------------------------------------
%% @doc The first worker node of the ring.
%%
%% Unlike the rest of the nodes, `first_node' will `out' a message
%% then block (`in') until the same message is received.
%%
%% the message is a positive integer, and we decrement it on each
%% iteration, until we arrive at zero. The zero message is never sent.
%%
%% @end
%%--------------------------------------------------------------------
-spec first_node(integer()) -> ok.
first_node(0) ->
    espace:out({all_done}),
    ok;

first_node(Msg_num) when Msg_num > 0 ->
    espace:out({1, Msg_num}),
    {[], _} = espace:in({0, Msg_num}),
    first_node(Msg_num-1).

%%--------------------------------------------------------------------
%% @doc The normal worker nodes of the ring.
%%
%% A relay node blocks (`in') on the message, then forwards to the `Next'
%% process.
%%
%% The worker exits, without forwarding the message, when it receives `Msg_num' 0.
%%
%% @end
%%--------------------------------------------------------------------
-spec relay_node(integer(), integer(), integer()) -> ok.
relay_node(_Me, _Next, 0) ->
    ok;

relay_node(Me, Next, Msg_num) when Msg_num > 0 ->
    {[], _} = espace:in({Me, Msg_num}),
    espace:out({Next, Msg_num}),
    relay_node(Me, Next, Msg_num-1).

%%--------------------------------------------------------------------
%% Local Tests
%%--------------------------------------------------------------------

bench_0_test() -> start().
bench_1_test() -> start(10, 10).
bench_2_test() -> start(100, 100).

%%--------------------------------------------------------------------
