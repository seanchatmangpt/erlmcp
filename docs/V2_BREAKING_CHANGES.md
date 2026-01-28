# erlmcp v2.0 Breaking Changes

**Version:** v2.0.0
**Date:** 2026-01-27
**Migration Difficulty:** Medium
**Estimated Migration Time:** 2-4 hours for typical projects

---

## Overview

erlmcp v2.0 introduces a complete architectural restructure from monolithic to umbrella application design. While module names remain unchanged, file paths, configuration format, and some APIs have breaking changes.

**Key Impact Areas:**
1. File system paths (modules moved to app directories)
2. Configuration format (split into per-app sections)
3. Client capability encoding (API change)
4. Dependency declarations (single dep → umbrella apps)
5. Supervision tree structure (internal, mostly transparent)

---

## 1. File System Structure Changes

### Before v1.x (Monolithic)
```
erlmcp/
├── src/
│   ├── erlmcp_client.erl
│   ├── erlmcp_server.erl
│   ├── erlmcp_otel.erl
│   ├── tcps_jidoka.erl
│   └── ...
├── test/
│   ├── erlmcp_client_tests.erl
│   └── ...
├── priv/
└── rebar.config
```

### After v2.0 (Umbrella)
```
erlmcp/
├── apps/
│   ├── erlmcp_core/
│   │   ├── src/
│   │   │   ├── erlmcp_client.erl
│   │   │   ├── erlmcp_server.erl
│   │   │   └── ...
│   │   ├── test/
│   │   └── priv/
│   ├── erlmcp_observability/
│   │   ├── src/
│   │   │   ├── erlmcp_otel.erl
│   │   │   └── ...
│   │   └── test/
│   ├── tcps_erlmcp/
│   │   ├── src/
│   │   │   ├── tcps_jidoka.erl
│   │   │   └── ...
│   │   └── test/
│   └── erlmcp_transports/
│       └── src/
└── rebar.config (umbrella config)
```

**Impact:** File paths changed, **module names unchanged**.

**Migration:**
- **For Erlang code:** No changes required (loads by module name)
- **For build scripts:** Update paths if they reference specific files
- **For documentation:** Update file path references

---

## 2. Dependency Declaration Changes

### Before v1.x
```erlang
% rebar.config
{deps, [
    {erlmcp, {git, "https://github.com/user/erlmcp.git", {branch, "main"}}}
]}.
```

### After v2.0 (Option 1: All apps)
```erlang
% rebar.config
{deps, [
    {erlmcp_core, {git, "https://github.com/user/erlmcp.git", {branch, "main"}}},
    {erlmcp_observability, {git, "https://github.com/user/erlmcp.git", {branch, "main"}}},
    {tcps_erlmcp, {git, "https://github.com/user/erlmcp.git", {branch, "main"}}},
    {erlmcp_transports, {git, "https://github.com/user/erlmcp.git", {branch, "main"}}}
]}.
```

### After v2.0 (Option 2: Selective)
```erlang
% rebar.config
% Only include apps you need
{deps, [
    {erlmcp_core, {git, "https://github.com/user/erlmcp.git", {branch, "main"}}}
    % Omit erlmcp_observability if you don't need OTEL
    % Omit tcps_erlmcp if you don't need TCPS commands
]}.
```

**Impact:** Requires explicit declaration of umbrella apps.

**Migration:**
1. Update `rebar.config` to reference specific apps
2. Run `rebar3 upgrade` to fetch new structure
3. Verify application start dependencies in `your_app.app.src`:
   ```erlang
   {applications, [
       kernel,
       stdlib,
       erlmcp_core          % Was: erlmcp
       % erlmcp_observability  % Optional
       % tcps_erlmcp           % Optional
   ]}.
   ```

---

## 3. Configuration Format Changes

### Before v1.x
```erlang
% config/sys.config
[
    {erlmcp, [
        {transport, stdio},
        {log_level, info},
        {metrics_enabled, true},
        {otel_endpoint, "http://localhost:4318"},
        {tcps_enabled, false},
        {web_port, 8080}
    ]}
].
```

### After v2.0
```erlang
% config/sys.config
[
    {erlmcp_core, [
        {transport, stdio},
        {log_level, info}
    ]},
    {erlmcp_observability, [
        {metrics_enabled, true},
        {otel_exporter, otlp},
        {otlp_endpoint, "http://localhost:4318"},
        {otlp_protocol, http_protobuf}
    ]},
    {tcps_erlmcp, [
        {enabled, false},
        {web_port, 8080},
        {web_interface, "0.0.0.0"}
    ]},
    {erlmcp_transports, [
        {tcp_port, 5555},
        {tcp_interface, "127.0.0.1"}
    ]}
].
```

**Impact:** Configuration keys moved to per-app sections.

**Migration:**
1. Split your `erlmcp` config block into 4 app-specific blocks
2. Move keys to appropriate apps:
   - **erlmcp_core:** transport, log_level, registry settings
   - **erlmcp_observability:** metrics, OTEL settings
   - **tcps_erlmcp:** TCPS commands, web server settings
   - **erlmcp_transports:** TCP/HTTP transport settings

**Compatibility:** Old format ignored (no errors, just unused).

---

## 4. Application Start Changes

### Before v1.x
```erlang
% Start single application
application:ensure_all_started(erlmcp).
```

### After v2.0
```erlang
% Start core (other apps start automatically via dependencies)
application:ensure_all_started(erlmcp_core).

% Or start specific apps
application:ensure_all_started(erlmcp_observability).
application:ensure_all_started(tcps_erlmcp).
```

**Impact:** Primary application name changed from `erlmcp` to `erlmcp_core`.

**Migration:**
1. Update `application:start/1` calls to reference `erlmcp_core`
2. Verify `.app.src` dependencies list `erlmcp_core` instead of `erlmcp`
3. If using releases, update `relx` config:
   ```erlang
   % Before
   {release, {myapp, "1.0.0"}, [myapp, erlmcp]}.

   % After
   {release, {myapp, "1.0.0"}, [myapp, erlmcp_core]}.
   ```

---

## 5. Client API Changes (BREAKING)

### erlmcp_client:start_link/1 Capability Encoding

**Before v1.x (Accepted arbitrary maps):**
```erlang
{ok, Client} = erlmcp_client:start_link(#{
    transport => stdio,
    capabilities => #{
        name => <<"my_client">>,
        version => <<"1.0.0">>
    }
}).
```

**After v2.0 (Requires structured format):**
```erlang
{ok, Client} = erlmcp_client:start_link(#{
    transport => stdio,
    client_info => #{
        name => <<"my_client">>,
        version => <<"1.0.0">>
    },
    capabilities => #{
        tools => [],
        resources => [],
        prompts => []
    }
}).
```

**Issue:** Function `erlmcp_client:encode_capabilities/1` expects specific keys.

**Migration:**
1. Rename `capabilities` → `client_info` for name/version
2. Add explicit `capabilities` map with `tools`, `resources`, `prompts` keys
3. Example fixed call:
   ```erlang
   erlmcp_client:start_link(#{
       transport => stdio,
       client_info => #{name => <<"test">>, version => <<"1.0">>},
       capabilities => #{tools => [], resources => [], prompts => []}
   }).
   ```

**Workaround (if blocked):**
Apply this patch to `apps/erlmcp_core/src/erlmcp_client.erl`:
```erlang
% Line ~531
encode_capabilities(#{name := _, version := _} = Info) ->
    % Treat as client_info, not capabilities
    Info;
encode_capabilities(#{tools := _, resources := _, prompts := _} = Caps) ->
    Caps;
encode_capabilities(Map) ->
    % Default empty capabilities
    #{tools => [], resources => [], prompts => []}.
```

---

## 6. Supervision Tree Structure Changes (INTERNAL)

### Before v1.x
```
erlmcp_sup (one_for_all)
├── erlmcp_registry
├── erlmcp_client_sup (simple_one_for_one)
└── erlmcp_server_sup (simple_one_for_one)
```

### After v2.0
```
erlmcp_core:
  erlmcp_sup (one_for_all)
  ├── erlmcp_registry
  ├── erlmcp_client_sup (simple_one_for_one)
  └── erlmcp_server_sup (simple_one_for_one)

erlmcp_observability:
  erlmcp_observability_sup (one_for_one)
  ├── erlmcp_metrics_server
  ├── erlmcp_health_monitor
  └── erlmcp_recovery_manager

tcps_erlmcp:
  tcps_sup (one_for_one)
  ├── tcps_work_order_server
  ├── tcps_registry
  └── tcps_web_server (if enabled)
```

**Impact:** Each app has its own supervision tree. Process isolation improved.

**Migration:** Mostly transparent. Update code that directly accesses supervisor PIDs:
```erlang
% Before
{ok, Sup} = erlmcp_sup:start_link().

% After
{ok, CoreSup} = erlmcp_sup:start_link(),
{ok, ObsSup} = erlmcp_observability_sup:start_link(),
{ok, TcpsSup} = tcps_sup:start_link().
```

---

## 7. Transport Behavior Changes (EXTENSION)

### New Callback Required

**Before v1.x (3 callbacks):**
```erlang
-behavior(erlmcp_transport).

-export([send/2, close/1, handle_data/2]).
```

**After v2.0 (4 callbacks):**
```erlang
-behaviour(erlmcp_transport).

-export([init/1, send/2, close/1, handle_data/2]).

init(Options) ->
    % New: Initialize transport state
    {ok, State}.
```

**Impact:** Custom transport modules must implement `init/1`.

**Migration:**
1. Add `init/1` callback to custom transport modules
2. Move initialization logic from `send/2` to `init/1`
3. Example:
   ```erlang
   init(#{port := Port}) ->
       case gen_tcp:listen(Port, [binary, {active, true}]) of
           {ok, Socket} -> {ok, #{socket => Socket}};
           {error, Reason} -> {error, Reason}
       end.
   ```

---

## 8. Removed/Deprecated Modules

### Removed (119 modules deleted)

**Legacy benchmarks:**
- `erlmcp_benchmark.erl` → Use `erlmcp_bench_core_ops.erl`
- `erlmcp_stress_test.erl` → Use `erlmcp_bench_stress.erl`
- All `benchmark_*.erl` → Consolidated into 5 modules

**Duplicate code:**
- `erlmcp_session_manager.erl` → Use `erlmcp_session.erl`
- `erlmcp_old_registry.erl` → Use `erlmcp_registry.erl` (gproc-based)

**Migration:** Update module references to new consolidated modules.

---

## 9. Metrology System (NEW)

### Canonical Unit Requirements

**Before v1.x (Ambiguous units):**
```erlang
#{throughput => 1000, latency => 50}.
% Units unclear: requests/sec? ops/sec? milliseconds? microseconds?
```

**After v2.0 (Enforced canonical units):**
```erlang
#{
    throughput_msg_per_s => 1000.0,    % Messages per second
    latency_p50_us => 50000,            % Microseconds
    memory_heap_mib_per_conn => 2.5     % Mebibytes per connection
}.
% Validated by erlmcp_metrology_validator
```

**Impact:** All metrics must use canonical keys with units in the name.

**Migration:**
1. Rename metric keys to include units:
   - `throughput` → `throughput_msg_per_s` or `throughput_req_per_s`
   - `latency` → `latency_p50_us`, `latency_p95_us`, `latency_p99_us`
   - `memory` → `memory_heap_mib_per_conn`, `memory_rss_mib_per_node`
2. Add scope to memory metrics: `_per_conn`, `_per_node`
3. Use floats for rates, integers for counts
4. See `docs/metrology/METRICS_GLOSSARY.md` for full spec

---

## 10. TCPS CLI Command Changes

### Renamed Commands

**Before v1.x (SPARC-style):**
```bash
./erlmcp sparc spec
./erlmcp sparc code
./erlmcp perf analyze
```

**After v2.0 (TCPS-style):**
```bash
./erlmcp tcps-pull           # Pull work orders (JIT)
./erlmcp tcps-build          # Execute standard work
./erlmcp poka-yoke-validate  # Error-proofing validation
```

**Impact:** Command names changed to authentic Japanese manufacturing terms.

**Migration:**
1. Update scripts to use new command names:
   - `/sparc spec` → `/tcps-pull`
   - `/sparc code` → `/tcps-build`
   - `/sparc test` → `/tcps-jidoka`
   - `/perf analyze` → `/poka-yoke-validate`
2. See `docs/tcps/TCPS_QUICK_START.txt` for full command mapping

---

## Migration Checklist

### Phase 1: Update Dependencies (15 minutes)
- [ ] Update `rebar.config` to reference umbrella apps
- [ ] Update `.app.src` to reference `erlmcp_core` instead of `erlmcp`
- [ ] Run `rebar3 upgrade` and verify compilation

### Phase 2: Update Configuration (15 minutes)
- [ ] Split `config/sys.config` into per-app sections
- [ ] Move OTEL settings to `erlmcp_observability` block
- [ ] Move TCPS settings to `tcps_erlmcp` block
- [ ] Move transport settings to `erlmcp_transports` block

### Phase 3: Fix API Calls (30-60 minutes)
- [ ] Update `application:start(erlmcp)` → `application:start(erlmcp_core)`
- [ ] Fix `erlmcp_client:start_link/1` capability encoding
- [ ] Update custom transport modules to implement `init/1`
- [ ] Rename metrics to canonical units (if using metrics)

### Phase 4: Update Build Scripts (15 minutes)
- [ ] Update file path references in scripts
- [ ] Update release config (`relx`) to reference `erlmcp_core`
- [ ] Update Dockerfiles to copy `apps/` directory

### Phase 5: Update Documentation (30 minutes)
- [ ] Update README with new structure
- [ ] Update API docs with new paths
- [ ] Update example code

### Phase 6: Test & Validate (1-2 hours)
- [ ] Run `rebar3 compile` and fix warnings
- [ ] Run `rebar3 eunit` and fix failing tests
- [ ] Run `rebar3 dialyzer` and fix type errors
- [ ] Run integration tests
- [ ] Verify production deployment

**Total estimated time:** 2-4 hours for typical projects.

---

## Support & Resources

### Documentation
- **Migration guide:** `docs/V2_IMPLEMENTATION_REPORT.md`
- **TCPS commands:** `docs/tcps/TCPS_QUICK_START.txt`
- **Metrology spec:** `docs/metrology/METRICS_GLOSSARY.md`
- **API reference:** `docs/api-reference.md`

### Getting Help
- **GitHub Issues:** https://github.com/user/erlmcp/issues
- **Discussions:** https://github.com/user/erlmcp/discussions

### Rollback Plan
If migration fails, revert to v1.x:
```bash
# In rebar.config
{deps, [
    {erlmcp, {git, "https://github.com/user/erlmcp.git", {tag, "v1.5.0"}}}
]}.

rebar3 upgrade
```

---

## Compatibility Matrix

| Component | v1.x | v2.0 | Compatible? |
|-----------|------|------|-------------|
| Module names | Same | Same | ✅ Yes |
| File paths | `src/` | `apps/*/src/` | ❌ No |
| Config format | Single block | Per-app | ❌ No |
| Client API | Loose maps | Structured | ❌ No |
| Transport behavior | 3 callbacks | 4 callbacks | ⚠️ Partial |
| CLI commands | SPARC-style | TCPS-style | ❌ No |
| Metrics | Ambiguous | Canonical | ❌ No |
| Dependencies | Single dep | Umbrella | ❌ No |

---

**Breaking changes documented:** 2026-01-27
**Next release (v2.1):** Expected to stabilize APIs and complete test migration
