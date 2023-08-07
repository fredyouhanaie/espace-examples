%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright (C) 2019, Fred Youhanaie
%%% @doc
%%%
%%% The dining philosophers simulation with `et' event tracer
%%% diagrams.
%%%
%%% To run the example:
%%% <ol>
%%% <li>Start with `rebar3 shell'</li>
%%% <li>Bring up the `et' display: `dinphil_2:et().'</li>
%%% <li>Run a 10 second simulation for 5 philosophers: `dinphil_2:start(5,10).'</li>
%%% </ol>
%%%
%%% @end
%%% Created : 21 Mar 2019 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(dinphil_2).

%% API exports
-export([et/0, start/2, init/1, phil/1]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start an et_viewer.
%%
%% The hard-coded configuration is suitable for five philosphers.
%%
%% @end
%%--------------------------------------------------------------------
-spec et() -> {ok, pid()}.
et() ->
    et_viewer:start(
      [
       {title, "The Dining Philosophers"},
       {trace_global, true},
       {trace_pattern, {et, max}},
       {max_actors, 10},
       {actors, ["Chop_0", "Phil_0",
       		 "Chop_1", "Phil_1",
       		 "Chop_2", "Phil_2",
       		 "Chop_3", "Phil_3",
       		 "Chop_4", "Phil_4"]}
      ]
     ).

%%--------------------------------------------------------------------
%% @doc Run a  `Duration' seconds simulation for `Nphil' philosophers.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(integer(), integer()) -> ok.
start(Nphil, Duration) ->
    init(Nphil),
    timer:sleep(1000*Duration),
    espace:stop().

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
%% Each philosopher goes through the following states:
%% <ol>
%% <li>Think</li>
%% <li>Wait to enter the dining room, need a room_ticket for this></li>
%% <li>Pick up the left chopstick</li>
%% <li>Pick up the right chopstick</li>
%% <li>Eat</li>
%% <li>Put down the left chopstick</li>
%% <li>Put down the right chopstick</li>
%% <li>Leave the room (return the room_ticket)</li>
%% <li>Go back to thinking</li>
%% </ol>
%%
%% We record the events, which are in fact "virtual" interactions
%% between philosophers and chopsticks.
%%
%% As the purpose of this module is to serve as an example, we have
%% skipped a few opportunites for optimized erlang. These
%% optimizations have been deferred to another day.
%%
%% @end
%%--------------------------------------------------------------------
-spec phil(integer()) -> bottom.
phil(I) ->
    {[Num], _} = espace:rd({num, '$1'}),

    L = I,
    R = (I+1) rem Num,

    %% We are deriving these on every iteration, we could instead
    %% derive them once and retain them in a state tuple.
    Philo = io_lib:format("Phil_~b", [I]),
    Lchop = io_lib:format("Chop_~b", [L]),
    Rchop = io_lib:format("Chop_~b", [R]),

    et:trace_me(50, Philo, thinking, thinking),
    think(),

    et:trace_me(50, Philo, at_door, at_door),
    espace:in({room_ticket}),
    et:trace_me(50, Philo, at_table, at_table),

    et:trace_me(50, Philo, Lchop, l_request, left_chopstick),
    espace:in({chopstick, L}),
    et:trace_me(50, Lchop, Philo, l_recieve, left_chopstick),

    et:trace_me(50, Philo, Rchop, r_request, right_chopstick),
    espace:in({chopstick, R}),
    et:trace_me(50, Rchop, Philo, r_recieve, right_chopstick),

    et:trace_me(50, Philo, eating, eating),
    eat(),

    espace:out({chopstick, L}),
    et:trace_me(50, Philo, Lchop, l_return, left_chopstick),

    espace:out({chopstick, R}),
    et:trace_me(50, Philo, Rchop, r_return, right_chopstick),

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
    timer:sleep(1000).

%%--------------------------------------------------------------------
%% @doc simulate eating as a one second delay.
%%
%% @end
%%--------------------------------------------------------------------
-spec eat() -> ok.
eat() ->
    timer:sleep(1000).

%%--------------------------------------------------------------------
