LINUX=$(shell uname | grep Linux | wc -l | xargs echo)
DEPS=../deps

ifeq ($(LINUX),1)
ZMQ_FLAGS=--with-pic
else
ZMQ_FLAGS=
endif

ifndef ZEROMQ_VERSION
ZEROMQ_VERSION=3.2.2
endif

all: $(DEPS)/zeromq3/src/.libs/libzmq.a

clean:
	if test -e $(DEPS)/zeromq3/Makefile; then \
		cd $(DEPS)/zeromq3; make clean; \
	else \
		true; \
	fi

distclean:
	@rm -rf $(DEPS)

$(DEPS):
	@mkdir -p $(DEPS)

$(DEPS)/zeromq-$(ZEROMQ_VERSION).tar.gz: $(DEPS)
	@curl http://download.zeromq.org/zeromq-$(ZEROMQ_VERSION).tar.gz -o $(DEPS)/zeromq-$(ZEROMQ_VERSION).tar.gz

$(DEPS)/zeromq3: $(DEPS)/zeromq-$(ZEROMQ_VERSION).tar.gz
	@cd $(DEPS) && tar xzvfp zeromq-$(ZEROMQ_VERSION).tar.gz && mv zeromq-$(ZEROMQ_VERSION) zeromq3

$(DEPS)/zeromq3/src/.libs/libzmq.a: $(DEPS)/zeromq3
	@cd $(DEPS)/zeromq3 && ./configure $(ZMQ_FLAGS) && make
