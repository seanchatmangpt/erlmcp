# erlmcp v2.0.0 Migration Plan

**Target:** Umbrella project with 4 separate OTP applications
**Timeline:** 3-phase migration (foundation → extraction → integration)
**Risk:** Medium (requires careful module movement and dependency tracking)

## Current Structure (v1.x - Monolith)

```
erlmcp/
├── src/          (228 modules - ALL in one app)
├── test/         (343 test files)
├── bench/        (32 benchmarks)
├── examples/     (3 examples)
└── rebar.config  (single app config)
```

**Issues:**
- Single application = tight coupling
- Cannot version subsystems independently
- Difficult to swap transport implementations
- TCPS mixed with core protocol
- Observability scattered across codebase

## Target Structure (v2.0.0 - Umbrella)

```
erlmcp/
├── apps/
│   ├── erlmcp_core/          (15 modules - Protocol core)
│   ├── erlmcp_transports/    (9 modules - Transport layer)
│   ├── erlmcp_observability/ (15 modules - Metrics/OTEL)
│   └── tcps_erlmcp/          (70+ modules - TCPS system)
├── rebar.config              (umbrella config)
└── config/sys.config         (global config)
```

## Module Distribution

### erlmcp_core (15 modules)
**Purpose:** Core MCP protocol, JSON-RPC, client/server, registry

| Module | Current Location | New Location |
|--------|-----------------|--------------|
| `erlmcp_client.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_server.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_json_rpc.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_registry.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_capabilities.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_capability_cache.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_client_sup.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_server_sup.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_registry_health_check.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_validation.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_app.erl` | `src/` | `apps/erlmcp_core/src/erlmcp_core_app.erl` |
| `erlmcp_sup.erl` | `src/` | `apps/erlmcp_core/src/erlmcp_core_sup.erl` |
| `erlmcp_util.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_version.erl` | `src/` | `apps/erlmcp_core/src/` |
| `erlmcp_session_manager.erl` | `src/` | `apps/erlmcp_core/src/` |

**Dependencies:** jsx, jesse, gproc

### erlmcp_transports (9 modules)
**Purpose:** Transport implementations (STDIO, TCP, HTTP, WebSocket)

| Module | Current Location | New Location |
|--------|-----------------|--------------|
| `erlmcp_transport_stdio.erl` | `src/` | `apps/erlmcp_transports/src/` |
| `erlmcp_transport_tcp.erl` | `src/` | `apps/erlmcp_transports/src/` |
| `erlmcp_transport_http.erl` | `src/` | `apps/erlmcp_transports/src/` |
| `erlmcp_transport_http_server.erl` | `src/` | `apps/erlmcp_transports/src/` |
| `erlmcp_transport_behavior.erl` | `src/` | `apps/erlmcp_transports/src/` |
| `erlmcp_transport_validation.erl` | `src/` | `apps/erlmcp_transports/src/` |
| `erlmcp_transport_api.erl` | `src/` | `apps/erlmcp_transports/src/` |
| `erlmcp_transport_sup.erl` | `src/` | `apps/erlmcp_transports/src/` |
| `erlmcp_transport_ws.erl` (NEW) | - | `apps/erlmcp_transports/src/` |

**Dependencies:** gun, ranch, poolboy, erlmcp_core

### erlmcp_observability (15 modules)
**Purpose:** Metrics, OpenTelemetry, receipt chains, monitoring

| Module | Current Location | New Location |
|--------|-----------------|--------------|
| `erlmcp_metrics.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_simple_metrics.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_routing_metrics.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_otel.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_simple_trace.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_sampling.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_receipt_chain.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_health_monitor.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_simple_monitor.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_monitor_dashboard.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_recovery_manager.erl` | `src/` | `apps/erlmcp_observability/src/` |
| `erlmcp_prometheus.erl` (NEW) | - | `apps/erlmcp_observability/src/` |
| `erlmcp_receipt_storage.erl` (NEW) | - | `apps/erlmcp_observability/src/` |
| `erlmcp_receipt_validator.erl` (NEW) | - | `apps/erlmcp_observability/src/` |
| `erlmcp_observability_app.erl` (NEW) | - | `apps/erlmcp_observability/src/` |
| `erlmcp_observability_sup.erl` (NEW) | - | `apps/erlmcp_observability/src/` |

**Dependencies:** opentelemetry_api, opentelemetry, opentelemetry_exporter, erlmcp_core

### tcps_erlmcp (70+ modules)
**Purpose:** TCPS quality system (separate from protocol)

| Pattern | Current Location | New Location |
|---------|-----------------|--------------|
| `tcps_*.erl` (all TCPS modules) | `src/` | `apps/tcps_erlmcp/src/` |
| `rebar3_tcps_*.erl` (plugins) | `src/` | `apps/tcps_erlmcp/src/` |

**Dependencies:** bbmustache, cowboy, jobs, fs, erlmcp_core, erlmcp_observability

## Migration Phases

### Phase 1: Foundation (COMPLETED - This Agent)
**Status:** ✅ DONE
**Deliverables:**
- [x] Create `apps/` directory structure
- [x] Create 4 app.src files with proper dependencies
- [x] Create 4 rebar.config files
- [x] Create umbrella rebar.config.v2
- [x] Create 4 README.md files
- [x] Verify directory structure

**Verification:**
```bash
tree -L 3 apps/
ls -la apps/*/src/*.app.src
find apps -name "rebar.config"
```

### Phase 2: Module Extraction (Agent 2)
**Goal:** Move modules from `src/` to `apps/*/src/` based on table above

**Workflow:**
1. Extract erlmcp_core modules (15 files)
   - Move client/server/registry/json_rpc
   - Update module references (erlmcp_app → erlmcp_core_app)
   - Create erlmcp_core_sup.erl, erlmcp_core_app.erl
2. Extract erlmcp_transports modules (9 files)
   - Move all transport_* modules
   - Create erlmcp_transports_app.erl, erlmcp_transport_sup.erl
3. Extract erlmcp_observability modules (15 files)
   - Move metrics/otel/receipt/monitoring
   - Create new supervisor and app modules
4. Extract tcps_erlmcp modules (70+ files)
   - Move all tcps_* and rebar3_tcps_* modules
   - Create tcps_erlmcp_app.erl, tcps_erlmcp_sup.erl

**Verification:**
```bash
rebar3 compile  # Must compile all 4 apps
rebar3 eunit --dir=apps/erlmcp_core
rebar3 dialyzer
```

### Phase 3: Integration & Testing (Agent 3)
**Goal:** Ensure all tests pass, benchmarks work, examples functional

**Workflow:**
1. Update test files (move to apps/*/test/)
2. Update benchmarks (ensure they reference new modules)
3. Update examples (simple/calculator/weather)
4. Update documentation (docs/architecture.md, etc.)
5. Create migration guide for users
6. Update CI/CD pipeline (.github/workflows/)

**Verification:**
```bash
rebar3 eunit
rebar3 ct
rebar3 dialyzer
rebar3 xref
make benchmark-quick
```

## Dependency Graph

```
tcps_erlmcp
    ├─> erlmcp_core
    └─> erlmcp_observability
        └─> erlmcp_core

erlmcp_transports
    └─> erlmcp_core

erlmcp_observability
    └─> erlmcp_core

erlmcp_core
    └─> [jsx, jesse, gproc]
```

**Critical:** No circular dependencies. Core is foundation.

## Breaking Changes

### For Library Users
**Old (v1.x):**
```erlang
{deps, [
    {erlmcp, "0.7.0"}
]}.

application:start(erlmcp).
```

**New (v2.0.0):**
```erlang
{deps, [
    {erlmcp_core, "2.0.0"},
    {erlmcp_transports, "2.0.0"}  % Optional
]}.

application:start(erlmcp_core).
application:start(erlmcp_transports).
```

### Module Renames
- `erlmcp_app.erl` → `erlmcp_core_app.erl`
- `erlmcp_sup.erl` → `erlmcp_core_sup.erl`
- TCPS modules: No renames (already prefixed `tcps_`)

## Risk Mitigation

### Risk 1: Circular Dependencies
**Mitigation:** Strict layering (core → observability → transports → tcps)
**Test:** `rebar3 compile` and `rebar3 xref`

### Risk 2: Missing Module References
**Mitigation:** Comprehensive grep for all module:function calls
**Test:** `rebar3 dialyzer` and runtime tests

### Risk 3: Test Breakage
**Mitigation:** Move tests alongside modules, update references
**Test:** `rebar3 eunit && rebar3 ct`

### Risk 4: Example Breakage
**Mitigation:** Update examples after module migration
**Test:** Manual execution of simple/calculator/weather

## Rollback Plan

If v2.0.0 migration fails:
1. Keep v1.x monolith in `main` branch
2. Develop v2.0.0 in `feature/v2-umbrella` branch
3. Only merge when ALL tests pass
4. Tag v1.7.0 as "stable monolith" before migration

## Success Criteria

- [x] ✅ **Phase 1:** Directory structure created
- [ ] ⏳ **Phase 2:** All modules extracted and compile
- [ ] ⏳ **Phase 3:** All tests pass (100% of v1.x pass rate)
- [ ] ⏳ **Phase 3:** All benchmarks pass (no >10% regression)
- [ ] ⏳ **Phase 3:** Documentation updated
- [ ] ⏳ **Phase 3:** Examples functional

## Next Steps

**Agent 2 (Module Extraction):**
```bash
# Start with erlmcp_core (smallest, most critical)
cd apps/erlmcp_core/src
# Copy 15 core modules from ../../src/
# Update erlmcp_app.erl → erlmcp_core_app.erl
# Test: rebar3 compile
```

**Agent 3 (Integration):**
```bash
# After Agent 2 completes
# Move tests: test/ → apps/*/test/
# Update benchmarks
# Verify examples
# Test: full quality pipeline
```

---

**Document Version:** 1.0.0
**Created:** 2026-01-27
**Status:** Phase 1 COMPLETE ✅
