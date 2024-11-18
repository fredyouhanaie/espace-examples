# `primes`

Example implementation of finding prime numbers based on chapter five
of the Carriero/Glernter book, How to write parallel programs.

> How to Write Parallel Programs: A First Course.
> Nicholas Carriero, David Gelernter.
> MIT Press, 1990.

The intention here is to demonstrate the implementation of all the
_C-Linda_ based examples in Erlang and _espace_.

## `primes_1`

The first example in the book is a very simplistic and fairly
inefficient way of computing a list of primes. It demonstrates use of
C-Linda for performing parallel tasks.

## Build

```console
$ rebar3 dialyzer
$ rebar3 shell
1> primes_1:start(20).
=INFO REPORT==== 18-Nov-2024::23:50:45.518939 ===
application: espace
exited: stopped
type: temporary

[{2,true},
 {3,true},
 {4,false},
 {5,true},
 {6,false},
 {7,true},
 {8,false},
 {9,false},
 {10,false},
 {11,true},
 {12,false},
 {13,true},
 {14,false},
 {15,false},
 {16,false},
 {17,true},
 {18,false},
 {19,true},
 {20,false}]

```
