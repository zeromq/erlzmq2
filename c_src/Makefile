LINUX=$(shell uname | grep Linux | wc -l | xargs echo)


ifeq ($(LINUX),1)
ZMQ_FLAGS=--with-pic
else
ZMQ_FLAGS=
endif

ifndef ZEROMQ_VERSION
ZEROMQ_VERSION=v2.1.7
endif

all: deps/zeromq2/src/.libs/libzmq.a

clean:
	@cd deps/zeromq2; make clean

distclean:
	@rm -rf deps

deps/zeromq2:
	@mkdir -p deps
	@git clone git://github.com/zeromq/zeromq2-1.git deps/zeromq2
	@echo $(ZEROMQ_VERSION)
	@cd deps/zeromq2 && git checkout $(ZEROMQ_VERSION)

deps/zeromq2/src/.libs/libzmq.a: deps/zeromq2
	@cd deps/zeromq2 && ./autogen.sh && ./configure $(ZMQ_FLAGS) && make
