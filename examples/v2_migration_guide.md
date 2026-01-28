# erlmcp v2.0 Migration Guide for Examples

## Overview

erlmcp v2.0 introduces an umbrella project structure with separate applications for different concerns. This guide explains how to migrate existing v1 examples to work with the new v2 structure.

## Architecture Changes

### v1 Structure (Monolithic)
```
erlmcp/
├── src/               # All modules together
│   ├── erlmcp_client.erl
│   ├── erlmcp_server.erl
│   ├── erlmcp_stdio.erl
│   └── ...
├── include/
│   └── erlmcp.hrl
└── examples/
```

### v2 Structure (Umbrella)
```
erlmcp/
├── apps/
│   ├── erlmcp_core/           # Core protocol & client/server
│   │   └── src/
│   ├── erlmcp_transports/     # Transport implementations
│   │   └── src/
│   ├── erlmcp_observability/  # Metrics, traces, receipts
│   │   └── src/
│   └── tcps_erlmcp/           # Toyota Code Production System
│       └── src/
├── include/                   # Shared headers
│   └── erlmcp.hrl
└── examples/
```

## What Changed

### 1. Module Organization

| v1 Location | v2 App | Module Examples |
|-------------|--------|-----------------|
| `src/erlmcp_client.erl` | `erlmcp_core` | `erlmcp_client`, `erlmcp_server`, `erlmcp_registry` |
| `src/erlmcp_stdio.erl` | `erlmcp_transports` | `erlmcp_stdio`, `erlmcp_tcp`, `erlmcp_http` |
| `src/erlmcp_json_rpc.erl` | `erlmcp_core` | `erlmcp_json_rpc`, `erlmcp_protocol` |
| N/A | `erlmcp_observability` | `erlmcp_metrics`, `erlmcp_traces`, `erlmcp_receipt` |
| N/A | `tcps_erlmcp` | TCPS manufacturing system modules |

### 2. Dependency Changes

**v1 rebar.config (examples had no deps):**
```erlang
%% Examples compiled against root project
{deps, []}.
```

**v2 rebar.config (examples reference umbrella apps):**
```erlang
{deps, [
    {erlmcp_core, {path, "../../apps/erlmcp_core"}},
    {erlmcp_transports, {path, "../../apps/erlmcp_transports"}}
]}.
```

### 3. Application Startup

**v1 (implicit):**
```erlang
%% Modules available directly
erlmcp_stdio:start().
```

**v2 (explicit app dependencies):**
```erlang
%% Start required applications
application:ensure_all_started(erlmcp_core).
application:ensure_all_started(erlmcp_transports).
erlmcp_stdio:start().
```

### 4. Include Paths

**v1:**
```erlang
-include("erlmcp.hrl").  % From project root
```

**v2 (unchanged):**
```erlang
-include("erlmcp.hrl").  % Still from project root include/
```

## Migration Steps

### Step 1: Update rebar.config

Create or update `rebar.config` in your example directory:

```erlang
{erl_opts, [
    debug_info,
    {i, "../../include"}
]}.

{deps, [
    {erlmcp_core, {path, "../../apps/erlmcp_core"}},
    {erlmcp_transports, {path, "../../apps/erlmcp_transports"}}
]}.

{shell, [
    {apps, [erlmcp_core, erlmcp_transports]}
]}.
```

### Step 2: Update Module Imports (No Changes Needed)

Module names remain the same:
- `erlmcp_client` - still works
- `erlmcp_server` - still works
- `erlmcp_stdio` - still works
- `erlmcp_json_rpc` - still works

### Step 3: Update Application Startup

If your example starts the MCP system, add explicit app starts:

**Before (v1):**
```erlang
start() ->
    erlmcp_stdio:start(),
    setup_server().
```

**After (v2):**
```erlang
start() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    erlmcp_stdio:start(),
    setup_server().
```

### Step 4: Update Compilation Commands

**v1:**
```bash
cd examples/simple
erlc -I../../include -pa ../../_build/default/lib/*/ebin *.erl
```

**v2:**
```bash
cd examples/simple
rebar3 compile
rebar3 shell
```

## Breaking Changes

### None!

The v2 migration is **backwards compatible** at the module level:
- All module names unchanged
- All function signatures unchanged
- All include files unchanged
- All protocol behavior unchanged

The only changes are:
1. **Build system** - examples need `rebar.config` with umbrella deps
2. **Application startup** - explicit `application:ensure_all_started/1` calls
3. **Compilation** - use `rebar3` instead of manual `erlc`

## Example Migration: simple_server_stdio

### Old (v1) - Manual Compilation
```bash
cd examples/simple
erlc -I../../include -pa ../../_build/default/lib/*/ebin simple_server_stdio.erl
erl -pa ../../_build/default/lib/*/ebin -eval "simple_server_stdio:start()."
```

### New (v2) - rebar3 Project
```bash
cd examples/simple
rebar3 compile
rebar3 shell
1> simple_server_stdio:start().
```

**File: examples/simple/rebar.config**
```erlang
{erl_opts, [
    debug_info,
    {i, "../../include"}
]}.

{deps, [
    {erlmcp_core, {path, "../../apps/erlmcp_core"}},
    {erlmcp_transports, {path, "../../apps/erlmcp_transports"}}
]}.
```

## Testing Your Migration

### 1. Compile Test
```bash
cd examples/<your_example>
rebar3 compile
```

Expected output:
```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling <your_example>
```

### 2. Module Load Test
```bash
rebar3 shell
```

```erlang
1> application:which_applications().
[{erlmcp_transports,...}, {erlmcp_core,...}, ...]

2> code:which(erlmcp_client).
"/path/to/erlmcp/apps/erlmcp_core/ebin/erlmcp_client.beam"

3> code:which(erlmcp_stdio).
"/path/to/erlmcp/apps/erlmcp_transports/ebin/erlmcp_stdio.beam"
```

### 3. Functionality Test
Run your example's main function and verify it works as before.

## Common Issues

### Issue 1: Module Not Found
**Error:**
```
=ERROR REPORT==== undefined function erlmcp_stdio:start/0
```

**Solution:** Add missing app to deps in rebar.config:
```erlang
{deps, [
    {erlmcp_transports, {path, "../../apps/erlmcp_transports"}}
]}.
```

### Issue 2: Include File Not Found
**Error:**
```
simple_client.erl:4: can't find include file "erlmcp.hrl"
```

**Solution:** Add include path to erl_opts:
```erlang
{erl_opts, [
    {i, "../../include"}
]}.
```

### Issue 3: Application Not Started
**Error:**
```
** exception error: undefined function erlmcp_registry:register/2
```

**Solution:** Start required applications before using modules:
```erlang
application:ensure_all_started(erlmcp_core).
```

## App Dependency Matrix

| Example Needs | Required Apps |
|---------------|---------------|
| Client/Server | `erlmcp_core` |
| STDIO Transport | `erlmcp_core`, `erlmcp_transports` |
| TCP Transport | `erlmcp_core`, `erlmcp_transports` |
| HTTP Transport | `erlmcp_core`, `erlmcp_transports` |
| Metrics | `erlmcp_core`, `erlmcp_observability` |
| TCPS System | `erlmcp_core`, `tcps_erlmcp` |

## Quick Reference

### Minimal rebar.config Template
```erlang
{erl_opts, [debug_info, {i, "../../include"}]}.
{deps, [
    {erlmcp_core, {path, "../../apps/erlmcp_core"}},
    {erlmcp_transports, {path, "../../apps/erlmcp_transports"}}
]}.
```

### Startup Template
```erlang
start() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    %% Your code here
    ok.
```

### Build Commands
```bash
rebar3 compile          # Compile example
rebar3 shell            # Start Erlang shell with deps
rebar3 clean            # Clean build artifacts
rebar3 as prod release  # Create release (if configured)
```

## Benefits of v2 Structure

1. **Modular Dependencies** - Only include what you need
2. **Clearer Boundaries** - Core vs Transport vs Observability
3. **Independent Versioning** - Apps can evolve separately
4. **Better Testing** - Test apps in isolation
5. **Optional Components** - TCPS can be excluded if not needed
6. **Production Ready** - Proper OTP application structure

## Resources

- **Architecture Documentation**: `docs/architecture.md`
- **OTP Patterns**: `docs/otp-patterns.md`
- **Umbrella Configuration**: Root `rebar.config`
- **App Specifications**: `apps/*/src/*.app.src`

## Questions?

If you encounter issues during migration:
1. Check this guide's "Common Issues" section
2. Review working examples: `examples/simple/`, `examples/calculator/`
3. Verify your `rebar.config` matches the template
4. Ensure all required apps are in deps list
5. Run `rebar3 compile` to see detailed error messages
