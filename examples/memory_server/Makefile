.PHONY: all compile clean test run shell release docker help

REBAR := rebar3
APP_NAME := memory_server

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -rf _build logs erl_crash.dump $(APP_NAME)

test:
	@$(REBAR) eunit

run: compile
	@$(REBAR) run

shell: compile
	@$(REBAR) shell

release:
	@$(REBAR) as prod release

escript: compile
	@$(REBAR) escriptize
	@chmod +x $(APP_NAME)

docker-build:
	docker build -t $(APP_NAME):latest .

docker-run:
	docker run -it --rm -v $(PWD)/memory.json:/memory.json $(APP_NAME):latest

help:
	@echo "Memory Server - Available targets:"
	@echo "  make compile    - Compile the project"
	@echo "  make test       - Run tests"
	@echo "  make run        - Run with stdio transport"
	@echo "  make shell      - Start Erlang shell"
	@echo "  make release    - Build production release"
	@echo "  make escript    - Build executable script"
	@echo "  make clean      - Clean build artifacts"
	@echo "  make help       - Show this help"
