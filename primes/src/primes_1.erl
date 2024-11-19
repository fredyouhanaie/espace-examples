%%%-------------------------------------------------------------------
%%% @author Fred Youhanaie <fyrlang@anydata.co.uk>
%%% @copyright 2024, Fred Youhanaie
%%% @doc
%%%
%%% The first example in the book is very simple, but not scalable.
%%%
%%% Basically, we start multiple `eval' processes, one per the number
%%% in the range -- the default is 2 to 1000. Each eval checks if its
%%% assigned number, `N', is prime and produces tuple in the tuple
%%% space of the form
%%%
%%% `{primes, N::integer(), Prime::boolean()}'
%%%
%%% @end
%%% Created : 2024-11-16 by Fred Youhanaie <fyrlang@anydata.co.uk>
%%%-------------------------------------------------------------------
-module(primes_1).

-export([start/0, start/1, stop/0]).
-export([get_primes/1]).

%%--------------------------------------------------------------------

-define(LIMIT, 1000).

%%--------------------------------------------------------------------
%% Start the process for the default range of numbers.
%%
start() ->
    start(?LIMIT).

%%--------------------------------------------------------------------
%% Start the process for the given range of numbers.
%%
start(Limit) ->
    espace:start(),
    get_primes(Limit).

%%--------------------------------------------------------------------
%% stop and clean up
stop() ->
    espace:stop().

%%--------------------------------------------------------------------
%%
get_primes(Limit) ->
    do_evals(Limit),
    collect_primes(Limit).

%%--------------------------------------------------------------------
%% Kick off all the `eval' processes in the background.
%%
do_evals(Limit) ->
    [ espace:eval({primes, I, {fun is_prime/1, [I]}})
      || I <- lists:seq(2, Limit) ],
    ok.
    
%%--------------------------------------------------------------------
%% Wait for and collect the results.
%%
collect_primes(Limit) ->
    [ {I, get_prime(I)} || I <- lists:seq(2, Limit) ].

%%--------------------------------------------------------------------
%% Get the computed primeness of a number.
%%
get_prime(N) ->
    {[Is_prime], _} = espace:rd({primes, N, '$1'}),
    Is_prime.

%%--------------------------------------------------------------------
%% Check if a number is prime
%%
is_prime(Me) ->
    Limit = floor(math:sqrt(Me)),
    Primes = [ get_prime(I) andalso ((Me rem I) == 0)
               || I <- lists:seq(2, Limit) ],
    lists:all(fun (X) -> not X end, Primes).

%%--------------------------------------------------------------------
