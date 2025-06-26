.PHONY: all compile clean test run shell release help

REBAR := rebar3
APP_NAME := everything_server

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -rf _build logs erl_crash.dump

test:
	@$(REBAR) eunit

run: compile
	@$(REBAR) run

shell: compile
	@$(REBAR) shell

release:
	@$(REBAR) as prod release

run-tcp: compile
	@ERL_FLAGS="-everything_server transport '{tcp,8080}'" $(REBAR) run

help:
	@echo "Everything Server - Available targets:"
	@echo "  make compile    - Compile the project"
	@echo "  make test       - Run tests"
	@echo "  make run        - Run with stdio transport"
	@echo "  make run-tcp    - Run with TCP transport on port 8080"
	@echo "  make shell      - Start Erlang shell"
	@echo "  make release    - Build production release"
	@echo "  make clean      - Clean build artifacts"
	@echo "  make help       - Show this help"
