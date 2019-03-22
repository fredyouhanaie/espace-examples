The Dining Philosophers
=====

This is the `espace' version of the Dining Philosophers example
found in the following paper:

> Carriero, Nicholas &amp; Gelernter, David. (1989).
> Linda in Context.
> Communications of the ACM. Volume 32 Issue 4. April 1989.
> Pages 444-458

The code in `dinphil_0` is an almost verbatim copy of the C-Linda
version, with some minor adjustments.


rebar3
-----

Use `rebar3` to build and run the demos.

Please note that `rebar3` will automatically download, and cache, the
`espace` source code from github. You can modify this behaviour by
adjusting the corresponding setting in `rebar.config`.

## dinphil_0 ##

This is the simplest variant. The purpose of the example, is to show
how coordination can be achieved using tuple spaces.

The tuple space is initialized as follows:

* `{num, N}`, indicates the number of philosophers. In the C-Linda
  version this is a global integer variable `Num`.

* `N-1` room ticket tuples, `{room_ticket}`, which will prevent all
  `N` philosophers from entering the room. The `N` tuples act like a
  semaphore with initial value of `N`.

* `N` chopstick tuples, `{chopstick, I}`.

For each philosopher a worker process, `phil(I)`, is started.

While running in the shell, the output will appear indicating the
change of state of the philosophers, T=Thinking, W=Waiting to enter
the dining room, E=Eating.

To try the example, below is a sample run:

```
$ rebar3 shell
> dinphil_0:init(5).
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

To stop the simulation, either quit the shell, or stop the espace
server:

```
> espace:stop().
```

## dinphil_1 ##

This is same as `dinphil_0`, but it uses an event logger in the tuple
space. Each time a philosopher changes state, it drops an `event`
tuple in the tuple space. The events are numbered, starting with 1.

Another module, `philmon` is used to continuously grab the newly
arrived events and print the updated state of all the philosophers.

The purpose of the example is to demonstrate use of a counter tuple to
create a sequence of event tuples in the tuple space.

Here is a sample run:

```
$ rebar3 shell
> philmon:start(5).
  1 _T_ ... ... ... ...
  2 _T_ _T_ ... ... ...
  3 _T_ _T_ _T_ ... ...
  4 _T_ _T_ _T_ _T_ ...
  5 _T_ _T_ _T_ _T_ _T_
  6 _W_ _T_ _T_ _T_ _T_
  7 _W_ _W_ _T_ _T_ _T_
  8 _W_ _W_ _T_ _T_ _W_
  9 _W_ _s_ _T_ _T_ _W_
 10 _W_ Ls_ _T_ _T_ _W_
 11 _W_ LeR _T_ _T_ _W_
 12 _W_ LeR _W_ _T_ _W_
 13 _W_ LeR _W_ _T_ _s_
 14 _W_ LeR _s_ _T_ _s_
 15 _s_ LeR _s_ _T_ _s_
 16 _s_ LeR _s_ _W_ _s_
 17 Ls_ LeR _s_ _W_ _s_
 18 Ls_ LeR _s_ _W_ Ls_
 19 Ls_ _.R _s_ _W_ Ls_
 20 LeR _.R _s_ _W_ Ls_
 21 LeR _.R Ls_ _W_ Ls_
 22 LeR _._ Ls_ _W_ Ls_
 23 LeR _._ LeR _W_ Ls_
```

## dinphil_2 ##

In this variant, which is based on `dinphil_0`, we are making use of
OTP's `et` module to generate sequence diagrams of a simulation.

The purpose of the example is to show the interaction among the `N`
passive elements (chopsticks) and `N` active workers
(philosophers). On the diagram all `2N` elements appear as active
processes/objects communicating with each other.

Here is a sample run with 5 philosphers that automatically stops after
10 seconds. The function `dinphil_2:et/0` brings up `et_viewer`
window, configured for 5 philosophers. `dinphil_2:start/2` runs the
simulation.


```
$ rebar3 shell
> dinphil_2:et().
> dinphil_2:start(5, 10).
```


Enjoy!

Fred.
