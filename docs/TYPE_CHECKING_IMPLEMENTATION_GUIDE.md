# Type Checking Improvements - Implementation Guide
**ErlMCP v2.1.0 | Agent-12 (Dialyzer)**

This guide provides step-by-step instructions for implementing type checking improvements across OTP 26-28.

---

## Table of Contents

1. [Quick Start: Incremental Dialyzer](#quick-start-incremental-dialyzer)
2. [Nominal Type Expansion](#nominal-type-expansion)
3. [Opaque Type Migration](#opaque-type-migration)
4. [Callback Spec Enhancement](#callback-spec-enhancement)
5. [Verification & Testing](#verification--testing)

---

## Quick Start: Incremental Dialyzer

### Step 1: Update rebar.config

**File**: `/Users/sac/erlmcp/rebar.config`

**Current** (lines 61-69):
```erlang
{dialyzer,
 [{warnings, [error_handling, underspecs, unknown, unmatched_returns]},
  {get_warnings, true},
  {plt_apps, all_deps},
  {plt_location, local},
  {plt_prefix, "erlmcp"},
  {base_plt_apps, [stdlib, kernel, erts, sasl, mnesia, crypto, ssl, inets, public_key, asn1, ssh]},
  {base_plt_location, global}]}.
```

**Updated**:
```erlang
{dialyzer,
 [{warnings, [error_handling, underspecs, unknown, unmatched_returns]},
  {get_warnings, true},
  {plt_apps, all_deps},
  {plt_location, local},
  {plt_prefix, "erlmcp"},
  {dialyzer_options, [incremental]},  % ADD THIS LINE
  {base_plt_apps, [stdlib, kernel, erts, sasl, mnesia, crypto, ssl, inets, public_key, asn1, ssh]},
  {base_plt_location, global}]}.
```

### Step 2: Update Makefile

**File**: `/Users/sac/erlmcp/Makefile`

**Add these targets**:
```makefile
.PHONY: dialyzer dialyzer-fast dialyzer-full dialyzer-update-plt dialyzer-clean

# Incremental dialyzer (development) - 3-7x faster
dialyzer-fast:
	@echo "==> Running incremental Dialyzer (development)..."
	rebar3 dialyzer --incremental

# Full dialyzer (CI/CD) - complete analysis
dialyzer-full:
	@echo "==> Running full Dialyzer analysis (CI/CD)..."
	rebar3 dialyzer

# Update PLT (after dependency changes)
dialyzer-update-plt:
	@echo "==> Updating Dialyzer PLT..."
	rebar3 dialyzer --update_plt

# Clean Dialyzer cache (force rebuild)
dialyzer-clean:
	@echo "==> Cleaning Dialyzer cache..."
	rm -rf _build/default/*_plt*
	rebar3 dialyzer --update_plt

# Default to incremental
dialyzer: dialyzer-fast
```

### Step 3: Verify

```bash
# First run (builds cache)
make dialyzer-fast

# Subsequent runs (incremental)
make dialyzer-fast

# Full analysis
make dialyzer-full
```

**Expected Output** (incremental):
```
==> Running incremental Dialyzer (development)...
===> Verifying dependencies...
===> Dialyzer starting, this may take a while...
===> Incremental analysis enabled
===> Analyzing 517 files...
===> Done (15.3s)
```

---

## Nominal Type Expansion

### Overview

erlmcp already has nominal types in `erlmcp_mcp_types.erl`. We'll expand them for better type safety.

### Current Nominal Types

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_mcp_types.erl`

```erlang
-type mcp_request_id() :: binary().
-type mcp_tool_name() :: binary().
-type mcp_resource_uri() :: binary().
-type mcp_session_id() :: binary().
-type mcp_prompt_name() :: binary().
-type mcp_task_id() :: binary().
-type mcp_progress_token() :: binary() | integer().
-type mcp_cursor_token() :: binary().
```

### Expansion Plan

#### Addition 1: Message Types

**Add to `erlmcp_mcp_types.erl`**:

```erlang
%%====================================================================
%% Message Types - Prevent semantic confusion
%%====================================================================

%% JSON-RPC request messages
-type mcp_request_message() :: map().

%% JSON-RPC response messages
-type mcp_response_message() :: map().

%% JSON-RPC error messages
-type mcp_error_message() :: map().

%% JSON-RPC notification messages
-type mcp_notification_message() :: map().

%% MCP-specific call messages (tools, resources, prompts)
-type mcp_call_message() :: map().

%% MCP result messages
-type mcp_result_message() :: map().

%% Export message types
-export_type([
  mcp_request_message/0,
  mcp_response_message/0,
  mcp_error_message/0,
  mcp_notification_message/0,
  mcp_call_message/0,
  mcp_result_message/0
]).
```

**Usage Example**:

```erlang
% Before:
-spec decode_message(binary()) -> {ok, map()} | {error, term()}.

% After:
-spec decode_message(binary()) ->
    {ok, mcp_request_message()} | {error, mcp_error_message()}.
```

#### Addition 2: Transport Types

**Add to `erlmcp_mcp_types.erl`**:

```erlang
%%====================================================================
%% Transport Types - Type-safe transport identifiers
%%====================================================================

-type mcp_transport_stdio() :: stdio.
-type mcp_transport_tcp() :: tcp.
-type mcp_transport_http() :: http.
-type mcp_transport_ws() :: ws.
-type mcp_transport_sse() :: sse.

%% Combined transport type
-type mcp_transport_type() ::
    mcp_transport_stdio() |
    mcp_transport_tcp() |
    mcp_transport_http() |
    mcp_transport_ws() |
    mcp_transport_sse().

%% Export transport types
-export_type([
  mcp_transport_stdio/0,
  mcp_transport_tcp/0,
  mcp_transport_http/0,
  mcp_transport_ws/0,
  mcp_transport_sse/0,
  mcp_transport_type/0
]).
```

**Usage Example**:

```erlang
% Before:
-spec start_transport(atom(), map()) -> {ok, pid()}.

% After:
-spec start_transport(mcp_transport_type(), map()) -> {ok, pid()}.
```

#### Addition 3: Phase/State Types

**Add to `erlmcp_mcp_types.erl`**:

```erlang
%%====================================================================
%% Phase Types - Type-safe state machine phases
%%====================================================================

%% Client connection phases
-type mcp_client_phase_pre_init() :: pre_initialization.
-type mcp_client_phase_init() :: initializing.
-type mcp_client_phase_ready() :: initialized.
-type mcp_client_phase_error() :: error.
-type mcp_client_phase_closed() :: closed.

%% Combined client phase type
-type mcp_client_phase() ::
    mcp_client_phase_pre_init() |
    mcp_client_phase_init() |
    mcp_client_phase_ready() |
    mcp_client_phase_error() |
    mcp_client_phase_closed().

%% Server connection phases
-type mcp_server_phase_init() :: initialization.
-type mcp_server_phase_ready() :: initialized.
-type mcp_server_phase_closing() :: closing.

%% Combined server phase type
-type mcp_server_phase() ::
    mcp_server_phase_init() |
    mcp_server_phase_ready() |
    mcp_server_phase_closing().

%% Export phase types
-export_type([
  mcp_client_phase/0,
  mcp_server_phase/0
]).
```

**Usage Example**:

```erlang
% Before:
-record(state, {phase = initialization :: atom()}).

% After:
-record(state, {phase = initialization :: mcp_client_phase()}).
```

#### Addition 4: Error Types

**Add to `erlmcp_mcp_types.erl`**:

```erlang
%%====================================================================
%% Error Types - Type-safe error representations
%%====================================================================

%% Parse errors (invalid JSON, invalid message structure)
-type mcp_parse_error() :: {parse_error, binary()}.

%% Validation errors (schema validation failed)
-type mcp_validation_error() :: {validation_error, binary()}.

%% Authentication errors (invalid token, unauthorized)
-type mcp_auth_error() :: {auth_error, binary()}.

%% Resource errors (not found, access denied)
-type mcp_resource_error() :: {resource_error, binary()}.

%% Tool errors (not found, execution failed)
-type mcp_tool_error() :: {tool_error, binary()}.

%% Prompt errors (not found, rendering failed)
-type mcp_prompt_error() :: {prompt_error, binary()}.

%% Combined error type
-type mcp_error() ::
    mcp_parse_error() |
    mcp_validation_error() |
    mcp_auth_error() |
    mcp_resource_error() |
    mcp_tool_error() |
    mcp_prompt_error().

%% Export error types
-export_type([
  mcp_parse_error/0,
  mcp_validation_error/0,
  mcp_auth_error/0,
  mcp_resource_error/0,
  mcp_tool_error/0,
  mcp_prompt_error/0,
  mcp_error/0
]).
```

**Usage Example**:

```erlang
% Before:
-spec validate(binary()) -> ok | {error, binary()}.

% After:
-spec validate(binary()) -> ok | {error, mcp_validation_error()}.
```

### Implementation Steps

1. **Extend `erlmcp_mcp_types.erl`** with the types above
2. **Recompile**: `rebar3 compile`
3. **Update function specs** (gradual migration)
4. **Verify Dialyzer**: `make dialyzer-fast`

---

## Opaque Type Migration

### Overview

Opaque types hide implementation details and enforce encapsulation.

### Strategy

Convert internal state records from `-type` to `-opaque`.

### Example: gen_server State Records

#### Before

```erlang
-module(erlmcp_server).

%% State record
-record(state,
        {server_id :: server_id(),
         phase = ?MCP_PHASE_INITIALIZATION :: mcp_server_phase(),
         capabilities :: #mcp_server_capabilities{},
         resources = #{} :: map()}).

%% Exported type (anyone can see the structure)
-type state() :: #state{}.
-export_type([state/0]).
```

#### After

```erlang
-module(erlmcp_server).

%% State record (same definition)
-record(state,
        {server_id :: server_id(),
         phase = ?MCP_PHASE_INITIALIZATION :: mcp_server_phase(),
         capabilities :: #mcp_server_capabilities{},
         resources = #{} :: map()}).

%% Opaque type (structure hidden)
-opaque state() :: #state{}.

%% Do NOT export opaque types
%% -export_type([state/0]).  % REMOVE THIS

%% Provide accessor functions instead
-spec get_server_id(state()) -> server_id().
get_server_id(#state{server_id = Id}) ->
    Id.

-spec get_phase(state()) -> mcp_server_phase().
get_phase(#state{phase = Phase}) ->
    Phase.

%% Provide update functions (using gen_server:call)
-spec update_phase(server(), mcp_server_phase()) -> ok.
update_phase(Server, Phase) ->
    gen_server:call(Server, {update_phase, Phase}).
```

### Migration Checklist

For each module:

- [ ] Identify state records
- [ ] Convert `-type` to `-opaque`
- [ ] Remove from `-export_type`
- [ ] Add accessor functions (getters)
- [ ] Add update functions (via gen_server calls/casts)
- [ ] Update callers to use accessor/update functions
- [ ] Verify compilation
- [ ] Verify Dialyzer

### Candidate Modules

| Module | State Record | Priority |
|--------|--------------|----------|
| `erlmcp_server` | `#state{}` | HIGH |
| `erlmcp_client` | `#state{}` | HIGH |
| `erlmcp_session` | `#state{}` | HIGH |
| `erlmcp_registry` | `#registry_state{}` | MEDIUM |
| `erlmcp_cache` | `#cache_state{}` | MEDIUM |
| `erlmcp_circuit_breaker` | `#circuit_state{}` | MEDIUM |

---

## Callback Spec Enhancement

### Overview

Rich callback specs improve behavior contract enforcement.

### Example: Transport Behavior

#### Before

**File**: `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_behavior.erl`

```erlang
-callback init(TransportType, Opts) -> {ok, State} | {error, Reason}.
-callback send(Data, State) -> {ok, State} | {error, Reason}.
-callback close(State) -> ok.
```

#### After

```erlang
%% Transport types
-type transport_type() :: stdio | tcp | http | ws | sse.
-type transport_opts() :: map().
-type transport_state() :: term().
-type transport_data() :: iodata().
-type init_reason() :: term().
-type send_reason() :: term().

%% Enhanced callback specs
-callback init(transport_type(), transport_opts()) ->
    {ok, transport_state()} | {error, init_reason()}.

-callback send(transport_data(), transport_state()) ->
    {ok, transport_state()} | {error, send_reason()}.

-callback close(transport_state()) -> ok.

%% Optional callbacks (with defaults)
-callback handle_info(term(), transport_state()) ->
    {ok, transport_state()} | {error, send_reason()}.

-callback terminate(term(), transport_state()) -> ok.
```

### Implementation Example

**File**: `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`

```erlang
-module(erlmcp_transport_tcp).
-behaviour(erlmcp_transport).

%% Include nominal types
-include("erlmcp.hrl").

%% Export the behavior callbacks
-export([init/2, send/2, close/1, handle_info/2, terminate/2]).

%%====================================================================
%% Callback Implementations
%%====================================================================

%% @doc Initialize TCP transport
-spec init(erlmcp_transport:transport_type(),
          erlmcp_transport:transport_opts()) ->
    {ok, erlmcp_transport:transport_state()} | {error, term()}.
init(tcp, Opts) when is_map(Opts) ->
    Host = maps:get(host, Opts, "localhost"),
    Port = maps:get(port, Opts, 9999),
    case gen_tcp:connect(Host, Port, [binary, {active, false}]) of
        {ok, Socket} ->
            State = #tcp_state{socket = Socket},
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Send data over TCP
-spec send(iodata(), #tcp_state{}) ->
    {ok, #tcp_state{}} | {error, term()}.
send(Data, #tcp_state{socket = Socket} = State) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Close TCP connection
-spec close(#tcp_state{}) -> ok.
close(#tcp_state{socket = Socket}) ->
    gen_tcp:close(Socket).

%% @doc Handle incoming messages
-spec handle_info(term(), #tcp_state{}) ->
    {ok, #tcp_state{}} | {error, term()}.
handle_info({tcp, Socket, Data}, #tcp_state{socket = Socket} = State) ->
    % Handle data
    {ok, State};
handle_info({tcp_closed, Socket}, #tcp_state{socket = Socket}) ->
    {error, closed}.

%% @doc Cleanup on termination
-spec terminate(term(), #tcp_state{}) -> ok.
terminate(_Reason, #tcp_state{socket = Socket}) ->
    gen_tcp:close(Socket).
```

---

## Verification & Testing

### Step 1: Compile Check

```bash
# Compile all applications
rebar3 compile

# Expected: No errors
```

### Step 2: Incremental Dialyzer

```bash
# Run incremental Dialyzer
make dialyzer-fast

# Expected: No warnings (or minimal warnings)
# Output: Done in 15-30s (vs 90s classic)
```

### Step 3: Full Dialyzer

```bash
# Run full Dialyzer
make dialyzer-full

# Expected: No warnings
# Output: Done in 60-90s
```

### Step 4: Type Coverage Check

```bash
# Count spec declarations
grep -r "^-spec" apps/*/src/*.erl | wc -l

# Expected: ~4,263 or higher
```

### Step 5: Verify Opaque Types

```bash
# Find opaque type declarations
grep -r "^-opaque" apps/*/src/*.erl

# Expected: Multiple occurrences (after migration)
```

### Step 6: Test Suite

```bash
# Run full test suite
rebar3 ct

# Expected: All tests pass
```

---

## Common Issues & Solutions

### Issue 1: Dialyzer Warning - "The type X is not exported"

**Cause**: Using `-opaque` without proper accessor functions

**Solution**:
```erlang
% Don't export opaque types
% -export_type([state/0]).  % REMOVE

% Provide accessor functions
-spec get_state_field(state()) -> field_type().
get_state_field(#state{field = Field}) ->
    Field.
```

### Issue 2: Dialyzer Warning - "The pattern X can never match"

**Cause**: Type specification doesn't match implementation

**Solution**:
```erlang
% Before (wrong):
-spec foo(binary()) -> ok.
foo(42) -> ok.  % Pattern doesn't match spec!

% After (correct):
-spec foo(integer() | binary()) -> ok.
foo(42) -> ok;
foo(Bin) when is_binary(Bin) -> ok.
```

### Issue 3: Dialyzer Performance Degradation

**Cause**: PLT needs update

**Solution**:
```bash
# Update PLT
make dialyzer-update-plt

# Or force rebuild
make dialyzer-clean
```

---

## Success Metrics

### Type Coverage

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| **-spec declarations** | 4,263 | 4,500+ | ⚠️ Pending |
| **-opaque declarations** | 0 | 50+ | ⚠️ Pending |
| **Nominal types** | 13 | 30+ | ⚠️ Pending |
| **Callback spec coverage** | ~60% | 100% | ⚠️ Pending |

### Performance

| Metric | Before | After | Status |
|--------|--------|-------|--------|
| **Dialyzer (incremental)** | 90s | 15-30s | ✅ Target |
| **Dialyzer (full)** | 90s | 60-90s | ✅ Baseline |
| **Compilation** | 100s | 80-90s | ✅ Target |

---

**End of Implementation Guide**
