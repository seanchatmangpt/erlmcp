# Weather MCP Example Makefile (v2.0)

ROOT := ../..
INCLUDE := $(ROOT)/include
BUILD := $(ROOT)/_build/default/lib

# Build path list from _build directory (all libs including umbrella apps)
LIB_DIRS := $(wildcard $(BUILD)/*/ebin)
PA_FLAGS := $(foreach dir,$(LIB_DIRS),-pa $(dir))

ERLC_OPTS := +debug_info -I$(INCLUDE) $(PA_FLAGS)

MODULES := weather_server_stdio weather_server weather_test weather_comprehensive_test

.PHONY: all compile clean run shell test

all: compile

compile: $(MODULES:%=%.beam)

%.beam: %.erl
	erlc $(ERLC_OPTS) $<

run: compile
	@echo "Starting weather_server_stdio..."
	@erl -noshell $(PA_FLAGS) -pa . -eval "weather_server_stdio:start()"

shell: compile
	@erl $(PA_FLAGS) -pa .

test: compile
	@erl -noshell $(PA_FLAGS) -pa . -eval "weather_test:test(), halt()"

clean:
	rm -f *.beam erl_crash.dump
