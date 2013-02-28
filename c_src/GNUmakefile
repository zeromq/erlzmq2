LINUX=$(shell uname | grep -E "(Linux|NetBSD)" | wc -l | xargs echo)
DEPS=../deps

ifeq ($(LINUX),1)
ZMQ_FLAGS=--with-pic
else
ZMQ_FLAGS=
endif

ifndef ZEROMQ_VERSION
endif

all: $(DEPS)/libzmq/src/.libs/libzmq.a

clean:
	if test -e $(DEPS)/libzmq/Makefile; then \
		cd $(DEPS)/libzmq; make clean; \
	else \
		true; \
	fi

distclean:
	@rm -rf $(DEPS)

$(DEPS)/libzmq:
	@mkdir $(DEPS)
	@git clone git://github.com/zeromq/libzmq.git $(DEPS)/libzmq

$(DEPS)/libzmq/src/.libs/libzmq.a: $(DEPS)/libzmq
	@cd $(DEPS)/libzmq && ./autogen.sh && ./configure $(ZMQ_FLAGS) && make
