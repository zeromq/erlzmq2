LINUX=$(shell uname | grep Linux | wc -l | xargs echo)
DEPS=../deps

ifeq ($(LINUX),1)
ZMQ_FLAGS=--with-pic
else
ZMQ_FLAGS=
endif

ifndef ZEROMQ_VERSION
ZEROMQ_VERSION=v2.1.7
endif

all: $(DEPS)/zeromq2/src/.libs/libzmq.a

clean:
	@cd $(DEPS)/zeromq2; make clean

distclean:
	@rm -rf $(DEPS)

ifndef ZEROMQ_LIB
$(DEPS)/zeromq2:
	@mkdir $(DEPS)
	@git clone git://github.com/zeromq/zeromq2-1.git $(DEPS)/zeromq2
	@echo $(ZEROMQ_VERSION)
	@cd $(DEPS)/zeromq2 && git checkout $(ZEROMQ_VERSION)

$(DEPS)/zeromq2/src/.libs/libzmq.a: $(DEPS)/zeromq2
	@cd $(DEPS)/zeromq2 && ./autogen.sh && ./configure $(ZMQ_FLAGS) && make

else

$(DEPS)/zeromq2/src/.libs/libzmq.a:
	@mkdir -p $(DEPS)/zeromq2/src/.libs/
	@ln -s $(ZEROMQ_LIB) $(DEPS)/zeromq2/src/.libs/libzmq.a
endif