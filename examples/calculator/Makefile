# Calculator MCP Example Makefile (v2.0)

ROOT := ../..
INCLUDE := $(ROOT)/include
BUILD := $(ROOT)/_build/default/lib

# Build path list from _build directory (all libs including umbrella apps)
LIB_DIRS := $(wildcard $(BUILD)/*/ebin)
PA_FLAGS := $(foreach dir,$(LIB_DIRS),-pa $(dir))

ERLC_OPTS := +debug_info -I$(INCLUDE) $(PA_FLAGS)

MODULES := calculator_server_stdio calculator_client calculator_demo \
	calculator_test calculator_comprehensive_test

.PHONY: all compile clean run shell test

all: compile

compile: $(MODULES:%=%.beam)

%.beam: %.erl
	erlc $(ERLC_OPTS) $<

run: compile
	@echo "Starting calculator_server_stdio..."
	@erl -noshell $(PA_FLAGS) -pa . -eval "calculator_server_stdio:start()"

shell: compile
	@erl $(PA_FLAGS) -pa .

test: compile
	@erl -noshell $(PA_FLAGS) -pa . -eval "calculator_test:test(), halt()"

clean:
	rm -f *.beam erl_crash.dump
