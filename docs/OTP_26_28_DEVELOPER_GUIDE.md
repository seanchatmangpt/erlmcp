# Erlang/OTP 26-28 Developer Guide for erlmcp

**Document Version**: 1.0.0
**Target OTP**: 28.3.1 (Primary), 27.3.4 (Transitional), 26 (Legacy)
**Date**: 2026-02-02
**Status**: Comprehensive Guide

---

## Table of Contents

1. [Overview](#overview)
2. [Quick Reference](#quick-reference)
3. [What's New by Version](#whats-new-by-version)
4. [Feature Deep Dives](#feature-deep-dives)
5. [Code Examples](#code-examples)
6. [Best Practices](#best-practices)
7. [Migration Guide](#migration-guide)
8. [Troubleshooting](#troubleshooting)
9. [Performance Tips](#performance-tips)

---

## Overview

This guide consolidates all Erlang/OTP 26-28 documentation for the erlmcp (Erlang MCP Server) project. It provides a single source of truth for developers working with OTP features across versions 26, 27, and 28.

### Version Support Policy

| OTP Version | Support Status | EOL Date | Recommended For |
|-------------|----------------|----------|-----------------|
| **OTP 26** | Legacy Only | 2026-06 | Existing legacy systems only |
| **OTP 27** | Transitional | 2027-05 | Gradual migration from 26 |
| **OTP 28** | Primary | 2028-05 | **All new development** |

### Key Architecture Points

- **Target OTP**: 28.3.1 (ERTS 16.2)
- **Minimum OTP**: 27 (enforced in rebar.config)
- **Compatibility Layer**: `include/otp_compat.hrl` for multi-version support
- **3-Tier Supervision**: One-for-all root, simple_one_for_one dynamic workers
- **Process-per-Connection**: Isolated client/server processes

---

## Quick Reference

### Feature Availability Matrix

| Feature | OTP 26 | OTP 27 | OTP 28 | Macro/Function |
|---------|--------|--------|--------|---------------|
| **Native JSON** | JSX only | `json:encode/1` | `json:encode/1` | `?JSON_ENCODE()` |
| **Priority Messages** | No | No | `alias([priority])` | `?SEND_PRIORITY()` |
| **Process Iterator** | No | No | `erlang:processes/1` | `?SAFE_PROCESSES()` |
| **Triple-Quoted Strings** | No | `"""text"""` | `"""text"""` | Direct syntax |
| **PCRE2 Regex** | PCRE | PCRE | PCRE2 | `re:compile/2` |
| **Concurrent Startup** | No | `ensure_all_started/3` | `ensure_all_started/3` | `concurrent` mode |
| **Persistent Config** | No | `set_env/4` | `set_env/4` | `[{persistent, true}]` |
| **Hibernate/0** | No | No | `erlang:hibernate/0` | Direct call |
| **Zip Generators** | No | No | `&&` in list comp | `K && V <- L` |
| **Strict Generators** | No | No | `<:-` in list comp | `[X || X <:- L]` |
| **Nominal Types** | No | No | `-nominal type` | `-nominal` |
| **MPTCP** | No | No | `{protocol, mptcp}` | gen_tcp option |

### Migration Checklist

#### From OTP 26 to 27
- [ ] Update `rebar.config` minimum version to 27
- [ ] Test native JSON availability
- [ ] Update documentation to triple-quoted strings
- [ ] Verify no HiPE compiler usage
- [ ] Update SSL/TLS configuration for new defaults

#### From OTP 27 to 28 (Primary Path)
- [ ] **CRITICAL**: Audit all regex patterns for PCRE2 compatibility
- [ ] Add priority message support to critical paths
- [ ] Replace `erlang:processes/0` with iterator API
- [ ] Enable process hibernation for idle connections
- [ ] Update to use native JSON module
- [ ] Implement concurrent application startup
- [ ] Add nominal types for critical identifiers
- [ ] Enable incremental Dialyzer mode

### Quick Commands

```bash
# Check OTP version
erl +V

# Check available features
erl +features

# Install OTP 28.3.1 (kerl)
kerl build 28.3.1 28.3.1
kerl install 28.3.1 ~/.kerl/28.3.1

# Set active version
kerl use 28.3.1

# Build erlmcp on specific OTP
OTP_VSN=28.3.1 rebar3 compile

# Run tests
rebar3 ct
rebar3 eunit

# Type checking
rebar3 dialyzer --incremental

# Cross-reference check
rebar3 xref
```

---

## What's New by Version

### OTP 26 (May 2023)

**Status**: Legacy - Use only for existing systems

#### Key Features
1. **Incremental Dialyzer** - 3-7x faster type analysis
2. **Enhanced Erlang Shell** - Better auto-complete
3. **General Performance Optimizations** - 10-20% improvement

#### API Additions
```erlang
%% Concurrent application startup
application:ensure_all_started(Apps, permanent, concurrent).

%% Persistent configuration
application:set_env(App, Key, Value, [{persistent, true}]).

%% Prep/stop callback
prep_stop(State) -> State.
```

#### Breaking Changes
- Old distribution protocol removed
- HiPE compiler completely removed

---

### OTP 27 (May 2024)

**Status**: Transitional - Migration path to OTP 28

#### Key Features
1. **Native JSON Module** - `json:encode/1`, `json:decode/1`
2. **Triple-Quoted Strings** - Multi-line string literals
3. **Sigils** - Enhanced string and binary literals
4. **Process Labels** - Better debugging with `proc_lib:set_label/1`
5. **Maybe Expression Default** - No need to enable feature

#### Native JSON Module
```erlang
%% Before (JSX)
Data = jsx:decode(Binary, [return_maps]),
Encoded = jsx:encode(Map).

%% After (Native)
Data = json:decode(Binary),
Encoded = json:encode(Map).
```

#### Triple-Quoted Strings
```erlang
%% Before
-doc "<p>Function description with \"escaped\" quotes.</p>".

%% After (OTP 27+)
-doc """
Returns the result for the given argument.

Example:
    > my_function(test).
    ok
""".
```

#### Performance Improvements
- 10-20% faster compilation
- Better type inference
- Optimized record update merging

---

### OTP 28 (May 2025) - PRIMARY VERSION

**Status**: Recommended for all new development

#### Major Feature Categories

##### 1. Priority Messages (EEP-76)
**Impact**: HIGH - Critical system stability
**Use Case**: Health checks, circuit breaker signals, graceful shutdown

```erlang
%% Create priority alias
PriorityAlias = alias([priority, {reply, explicit}]),

%% Send priority message
erlang:send(PriorityAlias, {health_check, ComponentId}, [priority]),

%% Monitor with priority
erlang:monitor(process, CriticalPid, [priority]),
```

**Benefits**:
- <1ms p99 latency for critical messages under load
- Bypasses normal message queue
- FIFO ordering within priority class

##### 2. Process Table Scalability
**Impact**: HIGH - Systems with 100K+ processes
**Use Case**: Health monitoring, process discovery

```erlang
%% Before (O(N) memory)
Pids = erlang:processes(),  % Allocates full list

%% After (O(1) memory)
Iterator = erlang:processes(#{max_iterations => 1000}),
%% Returns: {Pids, NextIterator} | done
```

**Benefits**:
- No scheduler blocking
- Safe iteration at scale
- 90% memory reduction for 1M processes

##### 3. Nominal Types (EEP-69)
**Impact**: MEDIUM - Type safety
**Use Case**: Protocol type enforcement

```erlang
%% Before (structural - silent bugs)
-type request_id() :: binary().
-type tool_name() :: binary().
%% Dialyzer treats these as identical!

%% After (nominal - compile-time detection)
-nominal type request_id() :: binary().
-nominal type tool_name() :: binary().
%% Dialyzer enforces type distinction
```

##### 4. Process Hibernation
**Impact**: MEDIUM - Memory optimization
**Use Case**: Idle connections, session management

```erlang
%% Simple API
erlang:hibernate(Module, Function, Args),

%% Or via gen_server option
{hibernate_after, 30000}  % 30 seconds of inactivity
```

**Benefits**:
- 75-90% memory reduction for idle processes
- 50KB → 5KB per hibernated connection
- GC pressure reduction

##### 5. Socket Optimizations (OTP 28.3)
**Impact**: MEDIUM - Network performance
**Features**:
- Receive buffer allocation optimization (94% waste reduction)
- MPTCP support for multipath networking
- TCP keep-alive tuning options

```erlang
%% MPTCP support
{ok, Socket} = gen_tcp:connect(Host, Port, [{protocol, mptcp}]),

%% TCP keep-alive
{ok, Socket} = gen_tcp:connect(Host, Port, [
    {tcp_keepcnt, 5},
    {tcp_keepidle, 60},
    {tcp_keepintvl, 10},
    {tcp_user_timeout, 30000}
]),
```

##### 6. Comprehension Enhancements
**Impact**: LOW - Code quality
**Features**:
- Zip generators (`&&`) for parallel iteration
- Strict generators (`<:-`) for fail-fast pattern matching

```erlang
%% Zip generators
[{K, V} || K <- Keys && V <- Values],  % Parallel iteration

%% Strict generators (fail on mismatch)
[X || X <-: MaybeResults, is_valid(X)],
```

##### 7. Post-Quantum Cryptography (OTP 28.3)
**Impact**: LOW - Future-proofing
**Features**:
- MLKEM-768/1024 hybrid key exchange
- SLH-DSA post-quantum signatures

```erlang
%% Hybrid TLS configuration
{key_exchange_algorithms, [x25519mlkem768, secp384r1mlkem1024]},
```

##### 8. Other Improvements
- Compiler error suggestions
- Zstandard compression (`zstd` module)
- PCRE → PCRE2 migration
- Enhanced observability
- Binary `join/2` function

---

## Feature Deep Dives

### 1. Native JSON Module (OTP 27+)

#### Overview
OTP 27 introduces a native JSON module, eliminating the need for third-party libraries like JSX.

#### Performance Comparison
| Operation | JSX | Native (OTP 27+) | Improvement |
|-----------|-----|------------------|-------------|
| Encode 1K objects | 150 μs | 50 μs | 3x faster |
| Decode 1K objects | 180 μs | 60 μs | 3x faster |
| Memory overhead | High | Low | 40% reduction |

#### API Comparison
```erlang
%% JSX (legacy)
jsx:encode(Map) -> Binary
jsx:decode(Binary, [return_maps]) -> Map

%% Native (OTP 27+)
json:encode(Map) -> Binary
json:decode(Binary) -> Map

%% With options
json:encode(Map, [binary, {escape, unicode}])
json:decode(Binary, [{objects, maps}])
```

#### Migration Pattern
```erlang
%% Compatibility layer
-ifdef(OTP_27_PLUS).
    -define(JSON_ENCODE(Data), json:encode(Data)).
    -define(JSON_DECODE(Binary), json:decode(Binary)).
-else.
    -define(JSON_ENCODE(Data), jsx:encode(Data)).
    -define(JSON_DECODE(Binary), jsx:decode(Binary, [return_maps])).
-endif.
```

---

### 2. Priority Messages (OTP 28)

#### Use Cases
1. **Health Checks** - Ensure monitoring reaches overloaded processes
2. **Circuit Breakers** - State transitions must be reliable
3. **Graceful Shutdown** - Shutdown signals must complete
4. **Alert Notifications** - Critical alerts must be delivered

#### Implementation Pattern
```erlang
-module(my_priority_server).
-behaviour(gen_server).

init([]) ->
    %% Create priority alias
    PrioAlias = alias([priority]),
    {ok, #{priority_alias => PrioAlias}}.

handle_call({critical_request}, _From, State = #{priority_alias := PrioAlias}) ->
    %% Send response with priority
    Reply = {critical_response, self()},
    _ = erlang:send(PrioAlias, Reply, [priority]),
    {reply, ok, State};

%% Handle priority messages
handle_info({critical_response, From}, State) ->
    %% Process high-priority message
    {noreply, State}.
```

#### Best Practices
- **DO** use for health checks and circuit breakers
- **DO** ensure priority alias is created in init/1
- **DON'T** overuse (only ~10% of messages should be priority)
- **DON'T** use for normal application data

---

### 3. Process Iterator (OTP 28)

#### Problem Solved
Legacy `erlang:processes/0` allocates a full list of all PIDs, causing:
- O(N) memory allocation
- Scheduler blocking during iteration
- Unusable at scale (>100K processes)

#### Solution
```erlang
%% Create iterator
Iterator = erlang:processes(#{max_iterations => 1000}),

%% Iterate
collect_processes(Iterator, Acc) ->
    case erlang:processes(Iterator) of
        {Pids, NextIterator} ->
            collect_processes(NextIterator, [Pids | Acc]);
        done ->
            lists:flatten(Acc)
    end.
```

#### Performance Comparison
| Process Count | processes/0 Memory | processes/1 Memory | Improvement |
|---------------|-------------------|--------------------|-------------|
| 10K | 320 KB | 3.2 KB | 100x |
| 100K | 3.2 MB | 3.2 KB | 1000x |
| 1M | 32 MB | 3.2 KB | 10000x |

#### Use Cases in erlmcp
- **Health monitoring** - Safe process enumeration at scale
- **Debug tools** - Process introspection without blocking
- **Admin APIs** - Process listing for dashboard

---

### 4. Nominal Types (OTP 28)

#### Concept
Erlang traditionally uses structural typing - types with the same structure are compatible. Nominal types distinguish by name, not structure.

#### Example Bug Prevention
```erlang
%% Structural typing (OTP < 28)
-type user_id() :: binary().
-type session_id() :: binary().

%% This BUG compiles without error:
process(user_id, session_id).  % Arguments swapped!

%% Nominal typing (OTP 28+)
-nominal type user_id() :: binary().
-nominal type session_id() :: binary().

%% Now Dialyzer CATCHES the bug:
process(user_id, session_id).  % Compile-time error!
```

#### erlmcp Implementation
```erlang
%% File: erlmcp_mcp_types.erl
-nominal type mcp_request_id() :: binary().
-nominal type mcp_tool_name() :: binary().
-nominal type mcp_resource_uri() :: binary().
-nominal type mcp_session_id() :: binary().

%% Constructor functions
-spec new_request_id(binary()) -> mcp_request_id().
new_request_id(Binary) when is_binary(Binary) -> Binary.

%% Usage in code
-spec invoke_tool(mcp_tool_name(), mcp_request_id()) -> ok.
invoke_tool(ToolName, RequestId) ->
    %% Type-safe: Cannot swap arguments
    mcp_protocol:invoke(ToolName, RequestId).
```

---

### 5. Process Hibernation (OTP 28)

#### Memory Impact
| State | Memory per Process | Connections (2GB) |
|-------|-------------------|-------------------|
| Active | 45-50 KB | ~40K |
| Hibernating | 5 KB | ~400K (theoretical) |

#### Implementation Patterns
```erlang
%% Pattern 1: Automatic hibernation (gen_server)
start_link() ->
    gen_server:start_link(?MODULE, [], [{hibernate_after, 30000}]).

%% Pattern 2: Manual hibernation
handle_info(idle_timeout, State) ->
    erlang:hibernate(?MODULE, awake, [State]).

awake(State) ->
    %% Process woke up after hibernation
    {noreply, State}.
```

#### When to Use
- **YES**: Idle connection processes (30s+ inactivity)
- **YES**: Session backends with sparse activity
- **YES**: Circuit breakers with long quiet periods
- **NO**: High-frequency workers (polling, etc.)
- **NO**: Processes with <5s idle time (hibernation overhead)

---

### 6. PCRE2 Migration (OTP 28)

#### Breaking Changes
PCRE2 is stricter than PCRE. These patterns no longer work:
- `\i` - PCRE-specific escape
- `\B8` - Invalid in PCRE2
- `\8` - Octal not supported

#### Migration Pattern
```erlang
%% Before (PCRE - OTP 27)
Pattern = "\\w+\\s+(?=\\d+)",

%% After (PCRE2 - OTP 28)
Pattern = "\\w+\\s+(?=\\d+)",  % Double-escape in strings

%% Or use raw binary (OTP 27+)
Pattern = <<"\w+\s+(?=\d+)">>,
```

#### Testing Pattern
```erlang
validate_regex(Pattern) ->
    case re:compile(Pattern, [unicode]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, {invalid_regex, Pattern, Reason}}
    end.
```

---

### 7. Concurrent Application Startup (OTP 26+)

#### Feature
Start independent applications concurrently instead of serially.

#### Usage
```erlang
%% Before (serial)
{ok, _} = application:ensure_all_started([crypto, ssl, ranch]),
%% Sequential start, 450ms total

%% After (concurrent - OTP 26+)
{ok, _} = application:ensure_all_started([crypto, ssl, ranch], permanent, concurrent),
%% Parallel start, 160ms total (64% faster)
```

#### When to Use
- **YES**: Independent applications (crypto, ssl, ranch)
- **YES**: Applications with no dependencies
- **NO**: Applications with clear dependency chain

---

### 8. Persistent Configuration (OTP 26+)

#### Feature
Configuration that survives application reloads.

#### Usage
```erlang
%% Non-persistent (reset on reload)
application:set_env(myapp, debug_mode, false),

%% Persistent (survives reload)
application:set_env(myapp, max_connections, 10000, [{persistent, true}]),
```

#### Use Cases
- Production tuning values
- Cluster configuration
- Runtime limits and quotas

---

## Code Examples

### Before/After Comparisons

#### JSON Encoding
```erlang
%% Before (JSX - OTP 26)
encode_message(Data) ->
    jsx:encode(Data, [force_utf8]).

%% After (Native - OTP 27+)
encode_message(Data) ->
    ?JSON_ENCODE(Data).
```

#### Process Enumeration
```erlang
%% Before (O(N) memory - OTP 26-27)
count_processes() ->
    length(erlang:processes()).

%% After (O(1) memory - OTP 28)
count_processes() ->
    Iterator = erlang:processes(#{max_iterations => 1000}),
    count_iterator(Iterator, 0).

count_iterator(done, Count) -> Count;
count_iterator(Iterator, Count) ->
    case erlang:processes(Iterator) of
        {Pids, Next} -> count_iterator(Next, Count + length(Pids));
        done -> Count
    end.
```

#### Health Checks
```erlang
%% Before (may timeout under load)
send_health_check(Pid) ->
    Pid ! {health_check, self()},
    receive
        {health_check_result, Result} -> Result
    after 5000 ->
        timeout
    end.

%% After (priority - OTP 28)
send_health_check(Pid) ->
    case erlang:function_exported(erlang, send, 3) of
        true ->
            PriorityAlias = alias([priority]),
            erlang:send(PriorityAlias, {health_check, self()}, [priority]);
        false ->
            Pid ! {health_check, self()}
    end.
```

#### Session Hibernation
```erlang
%% Before (always in memory)
handle_cast({save_session, Data}, State) ->
    NewState = State#state{session_data = Data},
    {noreply, NewState}.

%% After (hibernate when idle - OTP 28)
handle_cast({save_session, Data}, State) ->
    NewState = State#state{session_data = Data, last_activity = os:system_time(millisecond)},
    {noreply, NewState, hibernate}.
```

---

## Best Practices

### 1. Always Use Compatibility Layer

```erlang
%% include/otp_compat.hrl provides:
%% ?JSON_ENCODE(Data)
%% ?JSON_DECODE(Binary)
%% ?SAFE_PROCESS_COUNT()
%% ?SAFE_PROCESSES()
%% ?SEND_PRIORITY(Pid, Msg)
```

### 2. Test on Multiple OTP Versions

```bash
%% CI/CD Pipeline
- run: OTP_VSN=27.3.4 rebar3 compile
- run: OTP_VSN=27.3.4 rebar3 ct

- run: OTP_VSN=28.3.1 rebar3 compile
- run: OTP_VSN=28.3.1 rebar3 ct
```

### 3. Feature Detection, Not Version Detection

```erlang
%% Good: Feature detection
have_native_json() ->
    erlang:function_exported(json, encode, 1).

%% Avoid: Version detection (brittle)
is_otp_27() ->
    "27" =:= erlang:system_info(otp_release).
```

### 4. Gradual Migration Path

```erlang
%% Phase 1: Add compatibility
-ifdef(OTP_28_PLUS).
    -define(USE_ITERATOR, true).
-else.
    -define(USE_ITERATOR, false).
-endif.

%% Phase 2: Conditional code
safe_process_count() ->
    case ?USE_ITERATOR of
        true -> use_iterator();
        false -> erlang:system_info(process_count)
    end.

%% Phase 3: Remove old code after migration complete
```

### 5. Document Version-Specific Code

```erlang
%% @doc Process iteration using OTP 28+ iterator API.
%% Falls back to system_info for OTP 26-27.
%% @end
-spec count_processes() -> non_neg_integer().
```

---

## Migration Guide

### Phase 1: Preparation (Week 1)

1. **Audit Current Codebase**
   ```bash
   # Find all regex usage
   grep -r "re:run\|re:compile" apps/ --include="*.erl"

   # Find JSON encoding
   grep -r "jsx:" apps/ --include="*.erl"

   # Find process enumeration
   grep -r "erlang:processes()" apps/ --include="*.erl"
   ```

2. **Update Build Configuration**
   ```erlang
   %% rebar.config
   {minimum_otp_vsn, "27"},
   {platform_define, "^2[7]", 'OTP_27_PLUS'},
   {platform_define, "^2[8-9]|^[3-9]", 'OTP_28_PLUS'},
   ```

3. **Establish Baseline**
   ```bash
   # Run full test suite
   make test

   # Benchmark performance
   make benchmark-quick

   # Save results
   mv bench/results bench/results-baseline
   ```

### Phase 2: Core Migration (Weeks 2-4)

1. **Regex Pattern Updates** (High Priority)
   - Test all patterns with PCRE2
   - Update escaping as needed
   - Add validation tests

2. **Enable Native JSON** (Medium Priority)
   - Update `?JSON_ENCODE()` usage
   - Remove JSX dependency
   - Benchmark JSON performance

3. **Add Process Iterators** (High Priority)
   - Replace `erlang:processes/0` calls
   - Update monitoring code
   - Add fallback for OTP 27

### Phase 3: Feature Adoption (Weeks 5-8)

1. **Priority Messages**
   - Add to health check paths
   - Implement for circuit breakers
   - Measure latency improvement

2. **Process Hibernation**
   - Add to session backend
   - Add to idle connections
   - Measure memory reduction

3. **Nominal Types**
   - Add to critical identifiers
   - Update type specifications
   - Run Dialyzer verification

### Phase 4: Validation (Week 9)

1. **Full Test Suite**
   ```bash
   rebar3 ct
   rebar3 eunit
   rebar3 cover
   ```

2. **Performance Validation**
   ```bash
   make benchmark
   # Compare against baseline
   ```

3. **Production Readiness**
   ```bash
   make check  # compile + xref + dialyzer + tests
   ```

---

## Troubleshooting

### Common Issues

#### Issue 1: PCRE2 Pattern Compilation Error

**Symptom**: `{error, {bad_regex, {bad_pattern, ...}}}`

**Solution**:
```erlang
%% Test pattern before use
validate_pattern(Pattern) ->
    case re:compile(Pattern, [unicode]) of
        {ok, _MP} -> ok;
        {error, Reason} -> {error, {invalid_pattern, Reason}}
    end.
```

#### Issue 2: Priority Message Not Supported

**Symptom**: `badarg` when using `[priority]` option

**Solution**:
```erlang
%% Check before using
send_priority_if_supported(Pid, Msg) ->
    case erlang:function_exported(erlang, send, 3) of
        true when is_list(Msg) ->
            PriorityAlias = alias([priority]),
            erlang:send(PriorityAlias, Msg, [priority]);
        _ ->
            Pid ! Msg
    end.
```

#### Issue 3: Process Iterator Not Available

**Symptom**: `undefined function erlang:processes/1`

**Solution**: Use feature detection:
```erlang
safe_process_iterator() ->
    case erlang:function_exported(erlang, processes, 1) of
        true -> erlang:processes(#{});
        false -> erlang:processes()
    end.
```

### Debug Commands

```bash
# Check OTP version
erl -noshell -eval 'io:format("~p~n", [erlang:system_info(otp_release)]), halt().'

# Test regex compilation
erl -eval 're:compile("\\w+", [unicode])' -s init stop

# Check for priority support
erl -eval 'io:format("~p~n", [erlang:function_exported(erlang, alias, 1)])' -s init stop

# Check for iterator support
erl -eval 'io:format("~p~n", [erlang:function_exported(erlang, processes, 1)])' -s init stop
```

---

## Performance Tips

### 1. Use Native JSON (OTP 27+)

```erlang
%% Good: Native JSON (3x faster)
Data = json:decode(Binary),

%% Avoid: JSX (slower)
Data = jsx:decode(Binary, [return_maps]),
```

### 2. Use Process Iterator (OTP 28)

```erlang
%% Good: O(1) memory
Iterator = erlang:processes(#{}),

%% Avoid: O(N) memory
Pids = erlang:processes(),
```

### 3. Enable Incremental Dialyzer (OTP 26+)

```erlang
%% rebar.config
{dialyzer, [
    {dialyzer_options, [incremental]}  % 3-7x faster
]}.
```

### 4. Use Hibernation for Idle Processes

```erlang
%% Good: Memory efficient
{ok, Pid} = gen_server:start_link(my_server, [], [{hibernate_after, 30000}]),

%% Avoid: Always in memory
{ok, Pid} = gen_server:start_link(my_server, [], []),
```

### 5. Concurrent Application Startup

```erlang
%% Good: Parallel (64% faster)
application:ensure_all_started(Apps, permanent, concurrent),

%% Avoid: Serial (slower)
application:ensure_all_started(Apps),
```

---

## References

### Official Documentation
- [OTP 26 Release Notes](https://www.erlang.org/doc/apps/erts/notes-26.html)
- [OTP 27 Release Notes](https://www.erlang.org/doc/apps/erts/notes-27.html)
- [OTP 28 Release Notes](https://www.erlang.org/doc/apps/erts/notes-28.html)
- [EEP-69: Nominal Types](https://www.erlang.org/eeps/eep-0069)
- [EEP-76: Priority Messages](https://www.erlang.org/eeps/eep-0076)

### erlmcp Internal
- [Compatibility Analysis](./OTP_COMPATIBILITY_ANALYSIS.md)
- [Type Checking Analysis](./OTP_26_28_TYPE_CHECKING_ANALYSIS.md)
- [Architecture Recommendations](./OTP_ARCHITECTURE_RECOMMENDATIONS.md)
- [Build System Optimization](./OTP_28_BUILD_SYSTEM_OPTIMIZATION.md)
- [Performance Baselines](./benchmarks/OTP_28_BASELINES.md)

### Quick Reference
- [Compatibility Cheat Sheet](./OTP_COMPATIBILITY_CHEAT_SHEET.md)
- [28 Quick Reference](./OTP_28_QUICK_REFERENCE.md)
- [Best Practices Compliance](./OTP_BEST_PRACTICES_COMPLIANCE.md)

---

**Document End**

For the most up-to-date information, always refer to the official Erlang/OTP documentation at https://www.erlang.org/
