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

The `demo_1/0` function will automatically step through the manual
steps listed below.

* from the pubsub directory, start the shell

```
$ rebar3 shell
```

* Initialize the demo, which should generate messages from the three
  subscribers `alpha`, `beta` and `gamma`:

```
> pubsub_0:demo().
```

* Publish a couple of messages:

```
> pubsub_0:publish("Hello, World!").
> pubsub_0:publish("Hello, World, again!").
```

* Start a fourth subscriber, which should report the last message
  published:

```
> pubsub_0:client(delta).
```

* One more message for all four subscribers:

```
> pubsub_0:publish("Safety in numbers :-)").
```

* Once we stop the application, all data and workers will disappear:

```
> pubsub_0:stop().
```

## pubsub_1

This is a slightly more sophisticated version of `pubsub_0`. Use
`pubsub_1:demo_1/0` to run the demo.

Enjoy!

Fred
