# OTP 28 Experimental Native Debugger API

## Status: EXPERIMENTAL (OTP 28.0+)

**This feature requires Erlang/OTP 28.0+ and is highly experimental.**
The underlying `erl_debugger` API may change in future OTP versions.

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Quick Start](#quick-start)
4. [Architecture](#architecture)
5. [API Reference](#api-reference)
6. [MCP Debugging Patterns](#mcp-debugging-patterns)
7. [CLI Usage](#cli-usage)
8. [Compilation](#compilation)
9. [Examples](#examples)
10. [Limitations](#limitations)
11. [Troubleshooting](#troubleshooting)

---

## Overview

The OTP 28 native debugger provides **line-by-line debugging** for Erlang code using the experimental `erl_debugger` module. This is a significant improvement over traditional debugging methods:

### Traditional Debugging vs. OTP 28 Native Debugger

| Feature | Traditional (dbg, tracer) | OTP 28 Native |
|---------|--------------------------|---------------|
| Line Breakpoints | ❌ No | ✅ Yes |
| Process Inspection | Limited | Full stack frames |
| Variable Inspection | ❌ No | ✅ Yes (Y registers) |
| Step Execution | ❌ No | ✅ Yes |
| Performance Impact | Low | Medium |
| OTP Version | All | 28.0+ only |
| VM Flag Required | None | `+D` |

### Use Cases

**MCP-Specific Debugging:**
- Debug complex model context workflows
- Trace tool invocation chains
- Inspect request/response state at breakpoints
- Debug session management
- Debug JSON-RPC protocol handling

**General Erlang Debugging:**
- Line-by-line debugging of gen_servers
- Inspect process state at breakpoints
- Variable inspection in stack frames
- Step-through execution for complex logic

---

## Prerequisites

### 1. Erlang/OTP 28.0+

```bash
# Check OTP version
erl -version
# Expected: Erlang/OTP 28[.0.1] or higher
```

### 2. VM Started with +D Flag

```bash
# Start with debugger support
erl +D

# Or use vm.args
echo "+D" >> vm.args

# Verify
erl -eval "erlang:display(erl_debugger:supported()), init:stop()."
# Expected: true
```

### 3. Modules Compiled with Debug Info

```erlang
%% In rebar.config
{erl_opts, [
    debug_info,      % Required
    bin_opt_info     % Required for breakpoints
]}.
```

### 4. Not Supported in Production

The native debugger is **not suitable for production**:
- Performance overhead (~10-20%)
- Requires VM restart with +D flag
- Experimental API (may change)

---

## Quick Start

### 1. Start Debugger (in Erlang Shell)

```erlang
%% Check support
erlmcp_otp_debugger:is_supported().
% => true

%% Start debugger
{ok, Session} = erlmcp_otp_debugger:start().
% => {ok, 0}

%% Load module
ok = erlmcp_otp_debugger:load_module(Session, erlmcp_server).

%% Set breakpoint (line 127 in erlmcp_server.erl)
ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_server, 127).

%% When breakpoint is hit, inspect process
{ok, Frames} = erlmcp_otp_debugger:inspect_stack(Pid).

%% Get variable value from frame 0, slot 2
{ok, Value} = erlmcp_otp_debugger:inspect_variable(Pid, 0, 2, 1000).

%% Continue execution
ok = erlmcp_otp_debugger:continue(Pid).

%% Stop debugger
ok = erlmcp_otp_debugger:stop(Session).
```

### 2. CLI Usage

```bash
# Start VM with debugger support
erl +D -s erlmcp_cli_debugger start_cmd

# Set breakpoint
erl -s erlmcp_cli_debugger breakpoint_cmd erlmcp_server 127

# List breakpoints
erl -s erlmcp_cli_debugger list_breakpoints_cmd

# Inspect process
erl -s erlmcp_cli_debugger inspect_cmd "<0.123.0>"
```

---

## Architecture

### Components

```
┌─────────────────────────────────────────────────────────────┐
│                    erlmcp_otp_debugger                      │
│                    (gen_server wrapper)                     │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐  ┌──────────────────┐                │
│  │ Session Manager │  │ Breakpoint Store │                │
│  └─────────────────┘  └──────────────────┘                │
│  ┌─────────────────┐  ┌──────────────────┐                │
│  │ Event Handler   │  │ Module Registry  │                │
│  └─────────────────┘  └──────────────────┘                │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    erl_debugger (OTP 28)                    │
│  - register/1, unregister/2                                 │
│  - breakpoint/3, stack_frames/2                             │
│  - peek_stack_frame_slot/4, peek_xreg/3                     │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     BEAM VM (with +D)                       │
│  - Line instrumentation                                     │
│  - Breakpoint handling                                      │
│  - Process suspension                                       │
└─────────────────────────────────────────────────────────────┘
```

### Event Flow

```
Process executes
     │
     ▼
Hit breakpoint (line 127)
     │
     ├─→ Process suspended
     ├─→ {debugger_event, Session, {breakpoint, Pid, MFA, Line, Resume}}
     │         │
     │         ▼
     │   erlmcp_otp_debugger
     │         │
     │         ├─→ Log event
     │         ├─→ Store resume function
     │         └─→ Inspect process state
     │
     ▼
Developer inspects state
     │
     ├─→ inspect_stack(Pid)
     ├─→ inspect_variable(Pid, Frame, Slot)
     └─→ inspect_xreg(Pid, Reg)
     │
     ▼
Continue execution
     │
     └─→ continue(Pid) → ResumeFun()
```

---

## API Reference

### Debugger Lifecycle

#### `start() -> {ok, Session} | {error, Reason}`

Start the OTP 28 native debugger.

**Returns:**
- `{ok, Session}` - Debugger started
- `{error, not_supported}` - OTP < 28 or +D flag not set
- `{error, {already_registered, Pid}}` - Already running

**Example:**
```erlang
{ok, Session} = erlmcp_otp_debugger:start().
```

#### `stop(Session) -> ok`

Stop the debugger and clear all breakpoints.

**Example:**
```erlang
ok = erlmcp_otp_debugger:stop(Session).
```

#### `is_supported() -> boolean()`

Check if debugger is supported.

**Example:**
```erlang
true = erlmcp_otp_debugger:is_supported().
```

### Module Management

#### `load_module(Session, Module) -> ok | {error, Reason}`

Load a module for debugging.

**Errors:**
- `{error, module_not_loaded}` - Module not in code path
- `{error, already_loaded}` - Already loaded

**Example:**
```erlang
ok = erlmcp_otp_debugger:load_module(Session, erlmcp_server).
```

#### `list_loaded_modules(Session) -> {ok, [Module()]}`

List all loaded modules.

**Example:**
```erlang
{ok, [erlmcp_server, erlmcp_tool]} =
    erlmcp_otp_debugger:list_loaded_modules(Session).
```

### Breakpoints

#### `set_breakpoint(Module, Line) -> ok | {error, Reason}`

Set a breakpoint on `Module:Line`.

**Errors:**
- `{error, {badkey, Module}}` - Module not loaded
- `{error, {badkey, Line}}` - Line doesn't exist
- `{error, {unsupported, Line}}` - Line doesn't support breakpoints (e.g., function head)
- `{error, {unsupported, Module}}` - Module compiled without line breakpoint support

**Example:**
```erlang
ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_server, 127).
```

#### `clear_breakpoint(Module, Line) -> ok | {error, Reason}`

Clear a breakpoint.

**Example:**
```erlang
ok = erlmcp_otp_debugger:clear_breakpoint(erlmcp_server, 127).
```

#### `list_breakpoints() -> {ok, [BreakpointInfo]}`

List all breakpoints.

**Example:**
```erlang
{ok, [#{module := erlmcp_server, line := 127, enabled := true}]} =
    erlmcp_otp_debugger:list_breakpoints().
```

#### `list_breakpoints(Module) -> {ok, BreakpointsMap}`

List breakpoints for a module.

**Returns:**
```erlang
{ok, #{{function_name, arity} => #{Line => Enabled}}}
```

**Example:**
```erlang
{ok, #{{execute, 4} => #{127 => true}}} =
    erlmcp_otp_debugger:list_breakpoints(erlmcp_tool).
```

### Process Inspection

#### `inspect_stack(Pid) -> {ok, [StackFrame]} | {error, Reason}`

Inspect stack frames of suspended process (default max term size: 1000).

**Stack Frame Format:**
```erlang
{FrameNo, FrameFun, Info}
```

**FrameFun:**
```erlang
#{function := {Module, Function, Arity}, line := Line} |
'<terminate process>' |
'<continue terminate process>' |
'<breakpoint>' |
'unknown function'
```

**Info:**
```erlang
#{slots := [Slot], code := CodeAddress}
```

**Example:**
```erlang
{ok, [{0, #{function := {erlmcp_server, handle_call, 3}, line := 127},
        #{slots := [{value, Request}], code := 16#12345}}]} =
    erlmcp_otp_debugger:inspect_stack(Pid).
```

#### `inspect_variable(Pid, FrameNo, SlotNo, MaxSize) -> {ok, VariableValue} | {error, Reason}`

Get variable value from stack frame slot.

**Returns:**
- `{ok, {value, Term}}` - Variable value
- `{ok, {too_large, Size}}` - Value too large
- `{error, process_running}` - Process not suspended
- `{error, undefined_slot}` - Slot doesn't exist

**Example:**
```erlang
{ok, {value, #{jsonrpc => "2.0", id => 1}}} =
    erlmcp_otp_debugger:inspect_variable(Pid, 0, 0, 1000).
```

#### `inspect_xreg(Pid, Reg, MaxSize) -> {ok, RegValue} | {error, Reason}`

Inspect X register value.

**Example:**
```erlang
{ok, {value, State}} =
    erlmcp_otp_debugger:inspect_xreg(Pid, 0, 1000).
```

#### `inspect_process(Pid) -> {ok, ProcessInfo} | {error, Reason}`

Comprehensive process inspection.

**Returns:**
```erlang
{ok, #{stack_frames := [StackFrame], xregs_count := Count}}
```

**Example:**
```erlang
{ok, #{stack_frames := Frames, xregs_count := 5}} =
    erlmcp_otp_debugger:inspect_process(Pid).
```

### Execution Control

#### `continue(Pid) -> ok | {error, Reason}`

Continue execution of suspended process.

**Example:**
```erlang
ok = erlmcp_otp_debugger:continue(Pid).
```

---

## MCP Debugging Patterns

### Pattern 1: Debug Tool Invocation Chain

```erlang
%% Start debugger
{ok, Session} = erlmcp_otp_debugger:start().

%% Load relevant modules
ok = erlmcp_otp_debugger:load_module(Session, erlmcp_tool).
ok = erlmcp_otp_debugger:load_module(Session, erlmcp_json_rpc).
ok = erlmcp_otp_debugger:load_module(Session, erlmcp_server).

%% Set breakpoints at key points
ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_tool, 127).  % execute/4
ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_json_rpc, 89).  % encode_request

%% Trigger tool invocation via MCP client
%% When breakpoint is hit, inspect request state
{ok, Frames} = erlmcp_otp_debugger:inspect_stack(Pid).

%% Get tool name from stack frame
{ok, {value, ToolState}} = erlmcp_otp_debugger:inspect_variable(Pid, 0, 0, 1000).

%% Continue
ok = erlmcp_otp_debugger:continue(Pid).
```

### Pattern 2: Debug Session State

```erlang
%% Break on session creation
ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_session_backend, 85).

%% When hit, inspect session state
{ok, Frames} = erlmcp_otp_debugger:inspect_stack(Pid),
{ok, {value, SessionState}} = erlmcp_otp_debugger:inspect_variable(Pid, 0, 0, 1000).

%% Inspect session metadata
Maps = maps:get(sequences, SessionState),
io:format("Session sequences: ~p~n", [Maps]).
```

### Pattern 3: Debug JSON-RPC Protocol

```erlang
%% Break on request decode
ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_json_rpc, 67).  % decode_request

%% Inspect raw request
{ok, Frames} = erlmcp_otp_debugger:inspect_stack(Pid),
{ok, {value, RawJSON}} = erlmcp_otp_debugger:inspect_variable(Pid, 0, 1, 1000).

%% Break on response encode
ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_json_rpc, 89).  % encode_response

%% Inspect response
{ok, {value, Response}} = erlmcp_otp_debugger:inspect_variable(Pid, 0, 0, 1000).
```

---

## CLI Usage

### Available Commands

```bash
# Lifecycle
erlmcp_cli_debugger:start_cmd().           # Start debugger
erlmcp_cli_debugger:stop_cmd().            # Stop debugger
erlmcp_cli_debugger:status_cmd().          # Show status
erlmcp_cli_debugger:help_cmd().            # Show help

# Breakpoints
erlmcp_cli_debugger:breakpoint_cmd("erlmcp_server", "127").
erlmcp_cli_debugger:clear_breakpoint_cmd("erlmcp_server", "127").
erlmcp_cli_debugger:list_breakpoints_cmd().
erlmcp_cli_debugger:list_module_breakpoints_cmd("erlmcp_server").

# Process Inspection
erlmcp_cli_debugger:inspect_cmd("<0.123.0>").
erlmcp_cli_debugger:inspect_stack_cmd("<0.123.0>").
erlmcp_cli_debugger:variables_cmd("<0.123.0>").

# Execution Control
erlmcp_cli_debugger:continue_cmd("<0.123.0>").

# Module Loading
erlmcp_cli_debugger:load_module_cmd("erlmcp_server").
erlmcp_cli_debugger:unload_module_cmd("erlmcp_server").
```

### Interactive Session Example

```erlang
%% Start VM with debugger
$ erl +D -s erlmcp_cli debug start

Erlang/OTP 28 [erts-15.0] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

%% Start debugger
1> erlmcp_cli_debugger:start_cmd().
Debugger started: Session 0
Instrumentations: #{line_breakpoint => true}
ok

%% Load module
2> erlmcp_cli_debugger:load_module_cmd("erlmcp_server").
Module erlmcp_server loaded for debugging
ok

%% Set breakpoint
3> erlmcp_cli_debugger:breakpoint_cmd("erlmcp_server", "127").
Breakpoint set: erlmcp_server:127
ok

%% List breakpoints
4> erlmcp_cli_debugger:list_breakpoints_cmd().
Module                           Line       Enabled
----------------------------------------------------
erlmcp_server                    127        true
ok

%% When breakpoint is hit, inspect process
5> erlmcp_cli_debugger:inspect_cmd("<0.123.0>").

Process: <0.123.0>
X Registers: 5

Stack Frames:
  Frame 0: #{function := {erlmcp_server,handle_call,3}, line := 127}
    Code: 0x12345
    Slots: 5 slots

  Frame 1: #{function := {gen_server,try_handle_call,4}, line := 645}
    Code: 0x67890
    Slots: 3 slots

%% Continue execution
6> erlmcp_cli_debugger:continue_cmd("<0.123.0>").
Process <0.123.0> resumed
ok

%% Stop debugger
7> erlmcp_cli_debugger:stop_cmd().
Debugger stopped
ok
```

---

## Compilation

### Debug Profile

The `debug` profile in `rebar.config` enables the necessary compiler flags:

```erlang
{debug,
 [{erl_opts,
   [debug_info,
    bin_opt_info,              % Required for breakpoints
    warnings_as_errors,
    {d, 'DEBUG'},
    {d, 'OTP_DEBUG_ENABLED'}]},
  {deps, [{proper, "1.4.0"}, {meck, "0.9.2"}]},
  {cover_enabled, true},
  {cover_export_enabled, true}]}.
```

### Compile with Debug Profile

```bash
# Clean compile with debug profile
rebar3 clean -p debug
rebar3 compile -p debug

# Run tests
rebar3 eunit -p debug
rebar3 ct -p debug

# Start shell with debug profile
rebar3 shell -p debug
```

### VM Args

Add to `vm.args`:

```
+D              % Enable native debugger
+JPperf true    % Enable JIT performance monitoring
```

---

## Examples

### Example 1: Debug gen_server

```erlang
%% Create test gen_server
-module(test_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(Request, _From, State) ->
    %% Line 18 - Set breakpoint here
    Reply = process_request(Request),
    {reply, Reply, State}.

process_request(Request) ->
    %% Line 23 - Or here
    {ok, Request}.

%% Debug session
{ok, Session} = erlmcp_otp_debugger:start(),
ok = erlmcp_otp_debugger:load_module(Session, test_server),
ok = erlmcp_otp_debugger:set_breakpoint(test_server, 18),  % handle_call

%% Trigger call
gen_server:call(test_server, {test_request}).

%% When breakpoint hits:
{ok, Frames} = erlmcp_otp_debugger:inspect_stack(Pid),
{ok, {value, Request}} = erlmcp_otp_debugger:inspect_variable(Pid, 0, 0, 1000),
io:format("Request at breakpoint: ~p~n", [Request]),

ok = erlmcp_otp_debugger:continue(Pid).
```

### Example 2: Debug Multi-Process Workflow

```erlang
%% Break on process spawn
ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_server, 245).  % spawn_worker

%% When breakpoint hits, get spawned PID
{ok, Frames} = erlmcp_otp_debugger:inspect_stack(Pid),
{ok, {value, WorkerPid}} = erlmcp_otp_debugger:inspect_variable(Pid, 0, 1, 1000),

%% Continue worker PID
ok = erlmcp_otp_debugger:continue(Pid),

%% Now debug the worker process
ok = erlmcp_otp_debugger:set_breakpoint(erlmcp_worker, 56),  % init/1

%% Wait for worker init breakpoint
%% ...
```

---

## Limitations

### 1. OTP Version
- **Only OTP 28.0+**
- Experimental API - may change in future versions

### 2. VM Restart Required
- Must start with `+D` flag
- Cannot enable/disable at runtime

### 3. Performance Overhead
- ~10-20% performance impact
- Not suitable for production

### 4. Line Breakpoint Constraints
- Cannot set on:
  - Function heads
  - Comments
  - Empty lines
  - Compile-time attributes

### 5. Process Suspension
- Only processes at breakpoints can be inspected
- Running processes return `{error, process_running}`

### 6. Variable Size Limits
- Large variables truncated with `{too_large, Size}`
- Use `MaxSize` parameter to limit

### 7. Module Compilation
- Must be compiled with `debug_info`
- Must be compiled with `bin_opt_info` for breakpoints

---

## Troubleshooting

### "not_supported" Error

**Symptoms:**
```erlang
{error, not_supported} = erlmcp_otp_debugger:start().
```

**Solutions:**
1. Check OTP version:
   ```erlang
   erlang:system_info(otp_release).
   % Must be "28" or higher
   ```

2. Check VM flags:
   ```bash
   ps aux | grep beam | grep "+D"
   % If missing, start with: erl +D
   ```

3. Verify emulator type:
   ```erlang
   erlang:system_info(emu_type).
   % Should return: jit
   ```

### "badkey" Error Setting Breakpoint

**Symptoms:**
```erlang
{error, {badkey, erlmcp_server}} = erlmcp_otp_debugger:set_breakpoint(erlmcp_server, 127).
```

**Solutions:**
1. Load module first:
   ```erlang
   ok = erlmcp_otp_debugger:load_module(Session, erlmcp_server).
   ```

2. Verify module exists:
   ```erlang
   code:is_loaded(erlmcp_server).
   % Should return: {file, "..."}
   ```

3. Recompile with debug info:
   ```bash
   rebar3 clean -p debug
   rebar3 compile -p debug
   ```

### "unsupported" Error for Line

**Symptoms:**
```erlang
{error, {unsupported, 127}} = erlmcp_otp_debugger:set_breakpoint(erlmcp_server, 127).
```

**Solutions:**
1. Line is a function head - move breakpoint to function body
2. Line is a comment/attribute - choose different line
3. Module not compiled with `bin_opt_info` - use debug profile

### "process_running" Error

**Symptoms:**
```erlang
{error, process_running} = erlmcp_otp_debugger:inspect_stack(Pid).
```

**Solutions:**
- Process not at breakpoint
- Wait for breakpoint to be hit
- Check `erlmcp_otp_debugger:list_breakpoints()`

### No Events Received

**Symptoms:**
- Breakpoint set but no events

**Solutions:**
1. Verify instrumentation:
   ```erlang
   erlmcp_otp_debugger:get_instrumentations().
   % Should return: #{line_breakpoint => true}
   ```

2. Enable if needed:
   ```erlang
   erlmcp_otp_debugger:toggle_instrumentations(#{line_breakpoint => true}).
   ```

3. Verify code path is executed

---

## References

- [OTP 28 Release Notes](https://www.erlang.org/doc/system_principles/versions.html)
- [erl_debugger Module](https://www.erlang.org/doc/man/erl_debugger.html)
- [erlmcp Debugging Guide](./DEBUGGING.md)
- [MCP Protocol Specification](https://modelcontextprotocol.io/docs/)

---

**Status:** EXPERIMENTAL | **OTP Version:** 28.0+ | **Last Updated:** 2025-01-XX
