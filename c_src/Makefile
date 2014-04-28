LINUX=$(shell uname | grep Linux | wc -l | xargs echo)
DEPS=../deps

ifeq ($(LINUX),1)
ZMQ_FLAGS=--with-pic --with-pgm
else
ZMQ_FLAGS=
endif

ifndef ZEROMQ_VERSION
ZEROMQ_VERSION=4.0.5
endif

all: $(DEPS)/zeromq4/src/.libs/libzmq.a

clean:
	if test -e $(DEPS)/zeromq4/Makefile; then \
		cd $(DEPS)/zeromq4; make clean; \
	else \
		true; \
	fi

distclean:
	@rm -rf $(DEPS)

$(DEPS)/zeromq4:
	@mkdir -p $(DEPS)
	@curl http://download.zeromq.org/zeromq-$(ZEROMQ_VERSION).tar.gz -o $(DEPS)/zeromq-$(ZEROMQ_VERSION).tar.gz
	@cd $(DEPS) && tar xzvfp zeromq-$(ZEROMQ_VERSION).tar.gz && mv zeromq-$(ZEROMQ_VERSION) zeromq4

$(DEPS)/zeromq4/src/.libs/libzmq.a: $(DEPS)/zeromq4
	@cd $(DEPS)/zeromq4 && ./configure $(ZMQ_FLAGS) && make
