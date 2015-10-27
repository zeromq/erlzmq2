REBAR=rebar

all: compile

compile:
	@$(REBAR) compile

perftest: compile
	@cd perf && erlc erlzmq_perf.erl

clean:
	@$(REBAR) clean

distclean: clean
	@cd c_src;make distclean

test: compile
	@$(REBAR) eunit

docs:
	@$(REBAR) doc

bench: perftest
	@echo 'Running benchmarks, this could take some time...'
	@mkdir -p graphs
	@./perf/perfgraphs.py
	@mv -f *.png graphs/

