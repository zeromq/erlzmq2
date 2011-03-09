erlzmq2
====
NIF based Erlang bindings for the ZeroMQ messaging library.

Copyright (c) 2011 Yurii Rashkovskii and Evax Sofware

Overview
========

The erlzmq2 application provides high-performance NIF based Erlang bindings
for the ZeroMQ messaging library.

Downloading
===========

The erlzmq2 source code can be found on [GitHub](https://github.com/yrashk/erlzmq2)

    $ git clone http://github.com/yrashk/erlzmq2.git

It is also available on [Agner](http://erlagner.org/):

    $ agner build erlzmq

In order to build erlzmq2 against a specific version of ZeroMQ (not `master`), use this:

    $ ZEROMQ_VERSION=v<VERSION> agner build erlzmq

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


