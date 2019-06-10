erlzmq2
=======
NIF based Erlang bindings for the ZeroMQ messaging library.

Copyright (c) 2011 Yurii Rashkovskii, Evax Software and Michael Truog

Overview
--------

The erlzmq2 application provides high-performance NIF based Erlang
bindings for the ZeroMQ messaging library.

Downloading
-----------

The erlzmq source code can be found on
[GitHub](https://github.com/esl/erlzmq)

    $ git clone http://github.com/esl/erlzmq.git

Building
--------

Please note that to behave properly on your system ZeroMQ might
require [some tuning](http://www.zeromq.org/docs:tuning-zeromq).

Install zeromq-dev package for your distro.

For examples see `.travis*’ and `verification/Dockerfile*’.

Build the code

    $ rebar3 compile

Build the docs

    $ rebar3 docs

Run the test suite

    $ rebar3 eunit

Run the benchmarks (requires [python](http://www.python.org) and
[matplotlib](http://matplotlib.sourceforge.net/))

    TODO

This will run performance tests and output png graphs in the graphs
directory.

Architecture
------------

The bindings use Erlang's
[NIF (native implemented functions)](http://www.erlang.org/doc/man/erl_nif.html)
interface to achieve the best performance. One extra OS thread and one
pair of inproc sockets by context are used to simulate blocking recv
calls without affecting the Erlang virtual machine's responsiveness.

License
-------

The project is released under the MIT license.

