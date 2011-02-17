LINUX=$(shell uname | grep Linux | wc -l | xargs echo)


ifeq ($(LINUX),1)
ZMQ_FLAGS=--with-pic
else
ZMQ_FLAGS=
endif

all: compile 

deps/zeromq2/.git/HEAD:
	@git submodule init
	@git submodule update
deps/zeromq2/src/.libs/libzmq.a: deps/zeromq2/.git/HEAD
	@cd deps/zeromq2 && ./autogen.sh && ./configure $(ZMQ_FLAGS) && make

dependencies: deps/zeromq2/src/.libs/libzmq.a

compile: dependencies
	@./rebar compile
