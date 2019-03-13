The Dining Philosophers
=====

This is the `espace' version of the Dining Philosophers example
found in the following paper:

> Carriero, Nicholas &amp; Gelernter, David. (1989).
> Linda in Context.
> Communications of the ACM. Volume 32 Issue 4. April 1989.
> Pages 444-458

The code here is almost verbatim copy of the C-Linda version, with
some minor adjustments.

The tuple space is initialized as follows:

* `{num, N}`, indicates the number of philosophers. In the C-Linda
  version this is a global variable `Num`.

* `N-1` room ticket tuples, `{room_ticket}`, which will prevent all
  `N` philosophers from entering the room.

* `N` chopstick tuples, `{chopstick, I}`.

For each phisopher a worker process, `phil(I)` is started.


To try the example
-----

* This will start the simulation with five philosophers.

```
$ rebar3 shell
> dinphil_0:init(5).
```

* To stop the simulation, either quit the shell, or stop the espace
  server:

```
> espace:stop().
```

* While running in the shell, output will appear indicating the change
  of state in the philosophers, T=Thinking, W=Waiting to eat, E=Eating
  and e=finished eating.

```
"....T"
"T...."
"...T."
"..T.."
".T..."
"....W"
"W...."
"...W."
"..W.."
".W..."
"E...."
"T...."
"....E"
"W...."
"...E."
"....T"
"E...."
"....W"
```

Enjoy!

Fred.
