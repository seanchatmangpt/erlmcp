# POC Quick Start - 5-Minute Guide

**Goal**: Evaluate 80/20 dependency simplification in 5 minutes

**Audience**: Developers evaluating POC recommendations

**Time**: 5 minutes

---

## Prerequisites

### System Requirements

```bash
# Check Erlang/OTP version (25-28 required)
erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell

# Check rebar3 is installed
rebar3 version

# Verify git is available
git --version
```

### Clone Repository

```bash
# Clone if not already
git clone https://github.com/your-org/erlmcp.git
cd erlmcp

# Checkout POC branch (if exists) or stay on main
git checkout main
```

### Build Project

```bash
# Compile all applications
TERM=dumb rebar3 compile

# Expected output:
# ===> Analyzing applications...
# ===> Compiling erlmcp_core
# ===> Compiling erlmcp_transports
# ===> Compiling erlmcp_observability
# ===> Compiling erlmcp_validation
```

---

## 5-Minute Quick Start

### Step 1: POC 1 - JSON Schema Validation (60 seconds)

**Test current wrapper approach**:

```bash
# Start Erlang shell
rebar3 shell
```

```erlang
% In Erlang shell:

%% Load sample MCP request
Request = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"method">> => <<"tools/call">>,
    <<"params">> => #{<<"name">> => <<"test_tool">>}
}.

%% Current approach: Custom wrapper (if exists)
%% erlmcp_transport_validation:validate_message(Request, mcp_request).

%% POC approach: Direct jesse
Schema = jesse:load_schema(<<"schemas/mcp_request.json">>).
{ok, _} = jesse:validate(Schema, Request).

%% Output:
%% {ok, #{<<"jsonrpc">> => <<"2.0">>, ...}}

%% Test invalid request
InvalidRequest = #{<<"method">> => 123}.
{error, Reasons} = jesse:validate(Schema, InvalidRequest).

%% Output:
%% {error, [{data_invalid, ...}]}
```

**Expected result**: ✅ jesse validation works directly, no wrapper needed

---

### Step 2: POC 2 - Connection Pooling (60 seconds)

**Test poolboy directly**:

```erlang
% In Erlang shell (continue from Step 1):

%% Define simple worker module (inline)
-module(test_worker).
-export([start_link/1]).

start_link(_Args) ->
    {ok, spawn_link(fun() -> receive stop -> ok end end)}.

%% Start poolboy pool
PoolArgs = [
    {name, {local, test_pool}},
    {worker_module, test_worker},
    {size, 5},
    {max_overflow, 10}
].

{ok, Pool} = poolboy:start_link(PoolArgs, []).

%% POC approach: Direct poolboy checkout
Worker = poolboy:checkout(test_pool, true, 5000).
io:format("Checked out worker: ~p~n", [Worker]).
poolboy:checkin(test_pool, Worker).

%% Check pool status
poolboy:status(test_pool).

%% Output:
%% [{workers,5},{overflow,0},{monitors,0}]

%% Cleanup
poolboy:stop(test_pool).
```

**Expected result**: ✅ poolboy works directly, no wrapper needed

---

### Step 3: POC 3 - Process Registry (60 seconds)

**Test gproc directly vs erlmcp_registry**:

```erlang
% In Erlang shell:

%% Start a test process
TestPid = spawn(fun() -> receive stop -> ok end end).

%% POC: Direct gproc (simple use case)
gproc:reg({n, l, my_test_process}, TestPid).
TestPid = gproc:lookup_pid({n, l, my_test_process}).

%% Compare: erlmcp_registry (application logic)
%% erlmcp_registry:register_server(my_server, TestPid, #{capabilities => []}).

%% The difference:
%% - gproc: Just registration
%% - erlmcp_registry: Registration + capability indexing + monitoring + events

%% This shows WHY erlmcp_registry is worth keeping (adds value)

%% Cleanup
gproc:unreg({n, l, my_test_process}).
TestPid ! stop.
```

**Expected result**: ✅ gproc is simple, but erlmcp_registry adds MCP-specific logic

---

### Step 4: POC 4 - Remove Adapter Wrapper (60 seconds)

**Compare wrapper vs direct gen_server**:

```erlang
% In Erlang shell:

%% Start a simple gen_server
-module(test_server).
-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, test_server}, ?MODULE, [], []).

init([]) -> {ok, #{}}.

handle_call(ping, _From, State) ->
    {reply, pong, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Start server
{ok, Pid} = test_server:start_link().

%% Current approach: Wrapper (if exists)
%% erlmcp_transport_adapter:call(Pid, ping).

%% POC approach: Direct gen_server
pong = gen_server:call(test_server, ping).

%% Observation: The wrapper adds ZERO value
%% It's literally: call(Pid, Msg) -> gen_server:call(Pid, Msg).

%% Cleanup
gen_server:stop(test_server).
```

**Expected result**: ✅ Direct gen_server is simpler, no wrapper needed

---

### Step 5: POC 5 - Static Configuration (60 seconds)

**Compare dynamic discovery vs static config**:

```erlang
% In Erlang shell:

%% Current approach: Dynamic discovery (if implemented)
%% Transports = erlmcp_transport_discovery:discover_transports().

%% POC approach: Static configuration
Config = [
    {stdio, #{enabled => true}},
    {tcp, #{enabled => true, port => 8080}},
    {http, #{enabled => true, port => 8081}},
    {websocket, #{enabled => false}}
].

%% Load enabled transports
EnabledTransports = [Type || {Type, Opts} <- Config, maps:get(enabled, Opts, false)].

%% Output:
%% [stdio, tcp, http]

%% Observation:
%% - Static config: 3 lines, easy to read/debug
%% - Dynamic discovery: 588 LOC, complex, runtime failures
```

**Expected result**: ✅ Static config is simpler and more reliable

---

## Expected Output Summary

After 5 minutes, you should have seen:

| POC | Test | Result | Conclusion |
|-----|------|--------|------------|
| 1 | jesse:validate/3 | ✅ Works | No wrapper needed |
| 2 | poolboy:checkout/2 | ✅ Works | No wrapper needed |
| 3 | gproc:reg/2 | ✅ Works | But erlmcp_registry adds value |
| 4 | gen_server:call/2 | ✅ Works | No wrapper needed |
| 5 | Static config | ✅ Works | Simpler than discovery |

**Overall conclusion**: **80% of wrappers can be removed** (4 out of 5)

---

## Benchmarking (Optional - 2 minutes)

### Quick Performance Test

```bash
# Exit Erlang shell (Ctrl+C, a)

# Create benchmark script
cat > /tmp/bench_validation.erl << 'EOF'
-module(bench_validation).
-export([run/0]).

run() ->
    % Load schema
    Schema = jesse:load_schema(<<"schemas/mcp_request.json">>),
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{}
    },

    % Benchmark direct jesse
    {Time, _} = timer:tc(fun() ->
        [jesse:validate(Schema, Request) || _ <- lists:seq(1, 100000)]
    end),

    Rate = 100000 / (Time / 1_000_000),
    io:format("Validation rate: ~.0f validations/sec~n", [Rate]).
EOF

# Run benchmark
erl -pa _build/default/lib/*/ebin -noshell -s bench_validation run -s init stop

# Expected output:
# Validation rate: 900000 validations/sec
```

**Interpretation**:
- **>500K/sec**: Excellent (jesse is fast)
- **<100K/sec**: Something is wrong (check schema loading)

---

## Next Steps

### If POCs Look Good

```bash
# Proceed with migration (see POC_DEPENDENCIES.md)

# Phase 1: Remove adapter wrapper (quick win)
git grep "erlmcp_transport_adapter:" apps/

# Replace with gen_server:call/2
# ...
```

### If POCs Need Adjustment

```bash
# Open an issue with questions
gh issue create --title "POC Questions: Dependency Simplification" \
  --body "I tested the POCs and have questions about..."

# Or discuss with team
# ...
```

### If POCs Failed

```bash
# Check error messages
# Most common issues:

# 1. Schema file not found
ls -la schemas/mcp_request.json

# 2. jesse not in dependencies
grep jesse rebar.config

# 3. poolboy not started
application:ensure_all_started(poolboy).

# 4. gproc not started
application:ensure_all_started(gproc).
```

---

## Detailed Demos (Optional - 10 minutes)

### Demo 1: Full Validation Workflow

```bash
# Create a full example module
mkdir -p examples/poc_demos
cat > examples/poc_demos/validation_demo.erl << 'EOF'
-module(validation_demo).
-export([run/0]).

run() ->
    io:format("=== POC 1: JSON Schema Validation ===~n"),

    % Load schema
    Schema = jesse:load_schema(<<"schemas/mcp_request.json">>),
    io:format("Schema loaded: ~p~n", [maps:get(<<"$schema">>, Schema, unknown)]),

    % Test valid request
    ValidRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{<<"name">> => <<"test">>}
    },
    {ok, _} = jesse:validate(Schema, ValidRequest),
    io:format("✓ Valid request passed~n"),

    % Test invalid request
    InvalidRequest = #{<<"method">> => 123},
    {error, Reasons} = jesse:validate(Schema, InvalidRequest),
    io:format("✓ Invalid request caught: ~p errors~n", [length(Reasons)]),

    % Benchmark
    {Time, _} = timer:tc(fun() ->
        [jesse:validate(Schema, ValidRequest) || _ <- lists:seq(1, 10000)]
    end),
    Rate = 10000 / (Time / 1_000_000),
    io:format("✓ Validation rate: ~.0f validations/sec~n", [Rate]),

    io:format("~n=== POC 1 PASSED ===~n").
EOF

# Compile and run
erlc -o examples/poc_demos examples/poc_demos/validation_demo.erl
erl -pa _build/default/lib/*/ebin -pa examples/poc_demos -noshell -s validation_demo run -s init stop
```

### Demo 2: Connection Pool Workflow

```bash
cat > examples/poc_demos/pool_demo.erl << 'EOF'
-module(pool_demo).
-export([run/0]).

run() ->
    io:format("=== POC 2: Connection Pooling ===~n"),

    % Start poolboy pool
    PoolArgs = [
        {name, {local, demo_pool}},
        {worker_module, pool_demo_worker},
        {size, 5},
        {max_overflow, 10}
    ],
    {ok, _} = poolboy:start_link(PoolArgs, []),
    io:format("✓ Pool started (size: 5, max_overflow: 10)~n"),

    % Check initial status
    Status = poolboy:status(demo_pool),
    io:format("✓ Pool status: ~p~n", [Status]),

    % Checkout/checkin test
    Worker = poolboy:checkout(demo_pool, true, 5000),
    io:format("✓ Checked out worker: ~p~n", [Worker]),
    poolboy:checkin(demo_pool, Worker),
    io:format("✓ Checked in worker~n"),

    % Benchmark
    {Time, _} = timer:tc(fun() ->
        [begin
            W = poolboy:checkout(demo_pool),
            poolboy:checkin(demo_pool, W)
        end || _ <- lists:seq(1, 10000)]
    end),
    Rate = 10000 / (Time / 1_000_000),
    io:format("✓ Checkout/checkin rate: ~.0f ops/sec~n", [Rate]),

    % Cleanup
    poolboy:stop(demo_pool),
    io:format("~n=== POC 2 PASSED ===~n").

% Simple worker
-module(pool_demo_worker).
-export([start_link/1]).
start_link(_Args) ->
    {ok, spawn_link(fun() -> loop() end)}.
loop() ->
    receive stop -> ok; _ -> loop() end.
EOF

# Run
erlc -o examples/poc_demos examples/poc_demos/pool_demo.erl
erl -pa _build/default/lib/*/ebin -pa examples/poc_demos -noshell -s pool_demo run -s init stop
```

---

## Troubleshooting

### Issue: "Schema file not found"

```bash
# Solution: Create schemas directory
mkdir -p schemas

# Download MCP spec schema
curl -o schemas/mcp_request.json \
  https://raw.githubusercontent.com/modelcontextprotocol/specification/main/schema/mcp_request.json

# Or create a minimal test schema
cat > schemas/mcp_request.json << 'EOF'
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "required": ["jsonrpc", "method"],
  "properties": {
    "jsonrpc": {"type": "string", "const": "2.0"},
    "id": {"type": "number"},
    "method": {"type": "string"},
    "params": {"type": "object"}
  }
}
EOF
```

### Issue: "jesse not found"

```bash
# Solution: Ensure dependencies are fetched
rebar3 deps

# Check jesse is in _build/default/lib/
ls _build/default/lib/jesse/

# If missing, add to rebar.config:
# {deps, [{jesse, "1.8.1"}]}
# rebar3 compile
```

### Issue: "poolboy not found"

```bash
# Solution: Ensure poolboy dependency
rebar3 deps

# Check poolboy is in _build/default/lib/
ls _build/default/lib/poolboy/

# If missing, add to rebar.config:
# {deps, [{poolboy, "1.5.2"}]}
# rebar3 compile
```

### Issue: "gproc not started"

```bash
# Solution: Start gproc application
erl -pa _build/default/lib/*/ebin
application:ensure_all_started(gproc).

# Or in sys.config:
# {applications, [kernel, stdlib, gproc, ...]}.
```

---

## Command Summary

**Complete 5-minute workflow**:

```bash
# 1. Prerequisites (30 seconds)
rebar3 compile

# 2. Start shell (10 seconds)
rebar3 shell

# 3. Run all POC tests (4 minutes)
# [Copy/paste Erlang commands from Steps 1-5 above]

# 4. Exit and review (30 seconds)
% Ctrl+C, a
cat docs/POC_DEPENDENCIES.md
```

**Expected time**: **5 minutes**

**Expected result**: **Understanding that 80% of wrappers can be removed**

---

## Resources

- **Full POC Documentation**: `docs/POC_DEPENDENCIES.md`
- **80/20 Analysis**: `docs/80_20_CONSOLIDATION_PLAN.md`
- **Migration Guide**: `docs/80_20_PRIORITIZED_ACTIONS.md`
- **jesse Documentation**: https://github.com/for-GET/jesse
- **poolboy Documentation**: https://github.com/devinus/poolboy
- **gproc Documentation**: https://github.com/uwiger/gproc

---

**Document Version**: 1.0
**Last Updated**: 2026-01-31
**Time to Complete**: 5 minutes
**Difficulty**: Easy
