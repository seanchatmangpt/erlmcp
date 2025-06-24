.PHONY: compile test clean dialyzer xref shell

REBAR3 = rebar3

compile:
	$(REBAR3) compile

test:
	$(REBAR3) eunit

clean:
	$(REBAR3) clean

dialyzer:
	$(REBAR3) dialyzer

xref:
	$(REBAR3) xref

shell:
	$(REBAR3) shell

deps:
	$(REBAR3) deps

upgrade:
	$(REBAR3) upgrade

check: compile test dialyzer xref
	@echo "All checks passed!"

.DEFAULT_GOAL := compile
