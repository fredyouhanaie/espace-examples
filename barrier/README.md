# barrier

A barrier allows multiple processes working on a computation to
sychronize with each other before proceeding to the next stage.

The processes using the barrier do not need to be `espace`
application. They just need a `espace` server to be running before the
barrier. The server can be a one off instance, or one that is start
with the application and kept running for multiple barrier calls.

## Build and test

As with most Erlang projects, `rebar3` is used for building and
testing the code:

    $ rebar3 compile
	$ rebar3 dialyzer
	$ rebar3 eunit
	$ rebar3 edoc
