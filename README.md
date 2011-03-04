ezmq
====
NIF based Erlang bindings for the ZeroMQ messaging library.

Copyright (c) 2011 Yurii Rashkovskii and Evax Sofware

Overview
========

The ezmq application provides high-performance NIF based Erlang bindings
for the ZeroMQ messaging library.

Downloading
===========

The ezmq source code can be found on [GitHub](https://github.com/yrashk/ezmq)

    $ git clone http://github.com/yrashk/ezmq.git

It is also available on [Agner](http://erlagner.org/):

    $ agner build ezmq

In order to build ezmq against a specific version of ZeroMQ (not `master`), use this:

    $ ZEROMQ_VERSION=v<VERSION> agner build ezmq

Building
========

Build the code

    $ make

If you want to build against a specific version of ZeroMQ (not `master`), use this:

    $ ZEROMQ_VERSION=v<VERSION> make

Build the docs

    $ make docs

Run the test suite

    $ make test


