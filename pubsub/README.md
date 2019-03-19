# pubsub

## Introduction

These are a series of example modules to demonstrate use of `espace'
as a platform for publisher/subscriber services.

Each module implements the same functionality, however, the modules
with higher suffix number have more features.

The intention here is to demonstrate, and experiment with, various
techniques for developing `espace' based applications.

## Build

`rebar3` is used for compilation and testing.

```
rebar3 compile
rebar3 dialyzer
rebar3 edoc
```

## Trying out the demo

### pubsub_0

The `demo_0:start/0` function will run a demonstration of `pubsub_0`
steps listed below.

* from the pubsub directory, start the shell

```
$ rebar3 shell
> demo_0:start().
```

The demo will generate `logger` messages when running. The following
events will take place at one second intervals:

* The `pubsub` service will be started and initialized with three
  subscribers, `alpha`, `beta` and `gamma`.
* Two messages will be published. We expect the two messages to be
  reported by all three subscribers.
* Another subscriber, `delta`, will be added to the group. The last
  message will be reported by `delta`.
* One last message will be published. All four subscribers will report
  the message.
* The service will be stopped.

## pubsub_1

This is a slightly more sophisticated version of `pubsub_0`. Use
`pubsub_1:demo_1/0` to run the demo.

Enjoy!

Fred
