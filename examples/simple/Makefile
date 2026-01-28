# Simple MCP Example Makefile (v2.0)

ROOT := ../..
INCLUDE := $(ROOT)/include
BUILD := $(ROOT)/_build/default/lib

# Build path list from _build directory (all libs including umbrella apps)
LIB_DIRS := $(wildcard $(BUILD)/*/ebin)
PA_FLAGS := $(foreach dir,$(LIB_DIRS),-pa $(dir))

ERLC_OPTS := +debug_info -I$(INCLUDE) $(PA_FLAGS)

MODULES := simple_server_stdio simple_client simple_direct_test simple_server_tcp

.PHONY: all compile clean run shell test

all: compile

compile: $(MODULES:%=%.beam)

%.beam: %.erl
	erlc $(ERLC_OPTS) $<

run: compile
	@echo "Starting simple_server_stdio..."
	@erl -noshell $(PA_FLAGS) -pa . -eval "simple_server_stdio:start()"

shell: compile
	@erl $(PA_FLAGS) -pa .

test: compile
	@erl -noshell $(PA_FLAGS) -pa . -eval "simple_direct_test:test(), halt()"

clean:
	rm -f *.beam erl_crash.dump
