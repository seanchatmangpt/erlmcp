# erlmcp v2.0.0 Risks

**Identified Risks from Structure Analysis**

Version: v2.0.0-draft
Status: CANONICAL REFERENCE
Date: 2026-01-27

---

## Purpose

This document identifies **risks discovered during v2 architecture analysis**, categorized by severity, with mitigation strategies.

**Risk Assessment Methodology**: Based on C4 diagram analysis, module dependencies, and performance baseline validation.

---

## Risk Summary

| Risk ID | Category | Severity | Likelihood | Impact | Mitigation Status |
|---------|----------|----------|------------|--------|-------------------|
| R1 | Dependency | HIGH | MEDIUM | HIGH | Mitigated |
| R2 | Performance | MEDIUM | LOW | HIGH | Mitigated |
| R3 | Testing | HIGH | MEDIUM | HIGH | Planned |
| R4 | Library Migration | MEDIUM | MEDIUM | MEDIUM | In Progress |
| R5 | TCPS Separation | LOW | LOW | MEDIUM | Mitigated |
| R6 | API Compatibility | MEDIUM | MEDIUM | HIGH | Planned |
| R7 | Documentation | LOW | LOW | MEDIUM | In Progress |
| R8 | Deployment | MEDIUM | LOW | MEDIUM | Planned |

---

## R1: Hidden Dependencies in Legacy Code

### Description
**Risk**: Deleted modules may have hidden dependencies not visible in grep analysis.

**Scenario**:
1. Delete `erlmcp_complex_routing.erl`
2. Runtime crash: `undef` error in production
3. Dependency was dynamically referenced via `apply/3`

**Likelihood**: MEDIUM (247 modules, complex callgraph)
**Impact**: HIGH (runtime failures, service unavailability)

---

### Mitigation Strategy

**Phase 1: Static Analysis (Before Deletion)**
```bash
# Check explicit imports
grep -r "erlmcp_complex_routing" src/

# Check dynamic calls (apply, spawn, etc.)
grep -r "apply.*complex_routing\|spawn.*complex_routing" src/

# Check behavior callbacks
grep -r "behaviour.*complex_routing" src/
```

**Phase 2: Compilation Validation**
```bash
# After each deletion batch
rebar3 compile
rebar3 xref
rebar3 dialyzer
```

**Phase 3: Runtime Validation**
```bash
# Run full test suite
rebar3 eunit
rebar3 ct

# Run benchmarks (detect performance regressions)
./scripts/bench/run_all_benchmarks.sh

# Integration tests with real MCP clients
./test/integration/run_mcp_client_tests.sh
```

**Phase 4: Gradual Rollout**
- ✅ Delete in batches (10-20 modules per batch)
- ✅ Run full test suite after each batch
- ✅ Deploy to staging environment before production
- ✅ Monitor for `undef` errors in logs

---

### Mitigation Status

**Current**: IN PROGRESS
- [x] C4 diagrams created (identify dependencies)
- [x] Module classification (canonical vs. legacy)
- [ ] Static analysis scripts (grep, xref)
- [ ] Batch deletion plan (10-20 modules/batch)
- [ ] Integration test suite validation

**Confidence**: MEDIUM (C4 diagrams provide high-level visibility, but dynamic calls require runtime validation)

---

## R2: Performance Regression After Library Migration

### Description
**Risk**: Migrating to gproc/gun/ranch/poolboy may introduce performance regressions.

**Scenario**:
1. Migrate registry to gproc
2. Throughput drops from 553K msg/s to 400K msg/s (27% regression)
3. Breach of v2 principle: "no regressions >10%"

**Likelihood**: LOW (production libraries are well-tested)
**Impact**: HIGH (violates v2 performance targets)

---

### Mitigation Strategy

**Phase 1: Baseline Measurement**
```bash
# Before migration: Run canonical benchmarks
./scripts/bench/run_all_benchmarks.sh > baseline_v1.5.0.json

# Extract key metrics
jq '.throughput_msg_per_s' bench/results/*.json
```

**Baseline Targets** (from v1.5.0):
- Registry: 553K msg/s (`core_ops_100k`)
- Network I/O: 43K msg/s (`tcp_sustained_10k_1kib`)
- Session: 242K msg/s (`core_ops_100k`)

**Phase 2: Incremental Migration**
- ✅ Migrate one component at a time (registry → HTTP → TCP → pool)
- ✅ Run benchmarks after each migration
- ✅ Rollback if regression >10%

**Phase 3: Performance Profiling**
```bash
# If regression detected, profile with recon
erl -pa _build/default/lib/*/ebin
> recon:proc_count(memory, 10).  % Top 10 memory consumers
> recon:proc_window(reductions, 10, 1000).  % Top CPU consumers
```

**Phase 4: Library Tuning**
```erlang
% gproc tuning (if needed)
gproc:set_env(gproc, pool_size, 100).

% gun tuning (HTTP/2 connection pooling)
GunOpts = #{
    protocols => [http2, http],
    http2_opts => #{
        max_concurrent_streams => 100,
        initial_connection_window_size => 1048576
    }
}.

% ranch tuning (TCP accept pool)
ranch:set_max_connections(tcp_listener, 10000).
```

---

### Mitigation Status

**Current**: MITIGATED (benchmarks exist, baselines documented)
- [x] Baseline benchmarks (v1.5.0, `core_ops_100k`, `tcp_sustained_10k_1kib`)
- [x] Performance targets documented (GLOSSARY.md, v2_principles.md)
- [ ] Incremental migration plan (registry first, then HTTP, TCP, pool)
- [ ] Regression detection scripts (compare baseline vs. post-migration)

**Confidence**: HIGH (benchmarks are comprehensive, targets are clear)

---

## R3: Inadequate Test Coverage for Edge Cases

### Description
**Risk**: Deleting 177 modules may expose untested code paths in remaining 70 modules.

**Scenario**:
1. Delete `erlmcp_complex_routing.erl`
2. Simple routing works in tests
3. Production: Complex multi-hop routing fails (not covered by tests)

**Likelihood**: MEDIUM (80% test coverage, but gaps in integration tests)
**Impact**: HIGH (production failures, data loss)

---

### Mitigation Strategy

**Phase 1: Coverage Analysis**
```bash
# Generate coverage report
rebar3 cover
rebar3 covertool generate

# Identify uncovered lines
grep "0%" _build/test/cover/*.html
```

**Target**: ≥80% line coverage (maintain v1 levels)

**Phase 2: Integration Test Expansion**
```erlang
% Add MCP protocol conformance tests
test_mcp_initialize_sequence() ->
    {ok, Client} = erlmcp_client:start_link(...),
    {ok, Response} = erlmcp_client:initialize(Client, ...),
    ?assertEqual(#{<<"protocolVersion">> => <<"2024-11-05">>}, Response).

% Add supervision restart tests
test_registry_failure_recovery() ->
    {ok, RegistryPid} = erlmcp_registry:whereis(),
    exit(RegistryPid, kill),
    timer:sleep(1000),  % Wait for supervisor restart
    {ok, NewPid} = erlmcp_registry:whereis(),
    ?assertNotEqual(RegistryPid, NewPid).

% Add multi-transport tests
test_stdio_tcp_http_ws_parallel() ->
    % Spawn 4 clients (stdio, TCP, HTTP, WebSocket)
    % Verify all can call same server concurrently
    ...
```

**Phase 3: Property-Based Testing (PropEr)**
```erlang
prop_resource_handler_never_crashes() ->
    ?FORALL({Uri, Handler},
            {uri(), handler_fun()},
            begin
                {ok, Server} = erlmcp_server:start_link(),
                ok = erlmcp_server:add_resource(Server, Uri, Handler),
                Result = erlmcp_server:read_resource(Server, Uri),
                is_tuple(Result)  % Always returns {ok, _} or {error, _}
            end).
```

---

### Mitigation Status

**Current**: PLANNED (tests exist, but expansion needed)
- [x] Existing test suite (EUnit, Common Test)
- [x] Existing benchmarks (5 canonical benchmarks)
- [ ] Integration test expansion (MCP protocol conformance)
- [ ] Supervision restart tests (all 5 tiers)
- [ ] Property-based tests (PropEr for critical paths)
- [ ] Coverage target validation (≥80%)

**Confidence**: MEDIUM (existing tests are good, but gaps in integration scenarios)

---

## R4: Library Migration Breaking Changes

### Description
**Risk**: gproc/gun/ranch/poolboy APIs may differ from custom implementations, breaking existing code.

**Scenario**:
1. Migrate registry to gproc
2. `erlmcp_registry:register_server/3` signature changes
3. All 15 modules calling registry API must be updated
4. Missed update in `erlmcp_session_replicator.erl` → runtime crash

**Likelihood**: MEDIUM (API changes likely)
**Impact**: MEDIUM (fixable with refactoring, but tedious)

---

### Mitigation Strategy

**Phase 1: API Wrapper Pattern**
```erlang
% Maintain existing erlmcp_registry API (facade pattern)
-module(erlmcp_registry).
-export([register_server/3, find_server/1]).

% Wrap gproc calls
register_server(ServerId, Pid, Config) ->
    % Old API: erlmcp_registry:register_server(ServerId, Pid, Config)
    % New implementation: gproc
    gproc:reg({n, l, {mcp_server, ServerId}}, Pid),
    gproc:set_value({p, l, {mcp_server_config, ServerId}}, Config),
    ok.

find_server(ServerId) ->
    % Old API: erlmcp_registry:find_server(ServerId) -> {ok, Pid} | {error, not_found}
    % New implementation: gproc
    case gproc:lookup_local_name({mcp_server, ServerId}) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.
```

**Benefits**:
- ✅ Existing code unchanged (no caller refactoring)
- ✅ Gradual migration (internal implementation only)
- ✅ Easy rollback (swap implementation back to custom)

**Phase 2: Compiler-Enforced Validation**
```bash
# After API changes, recompile all modules
rebar3 clean
rebar3 compile

# Dialyzer will catch type mismatches
rebar3 dialyzer
```

---

### Mitigation Status

**Current**: IN PROGRESS (API wrapper pattern documented)
- [x] API wrapper pattern documented (otp-patterns.md, library-migration-guide.md)
- [x] gproc pattern examples (otp-patterns.md:L221-L240)
- [ ] API wrapper implementation (erlmcp_registry.erl migration)
- [ ] API compatibility tests (ensure old API still works)

**Confidence**: HIGH (wrapper pattern is standard practice, low risk)

---

## R5: TCPS Separation Breaking Existing Users

### Description
**Risk**: Users relying on TCPS features may be surprised when v2 requires separate `tcps_erlmcp` dependency.

**Scenario**:
1. User upgrades erlmcp v1.5.0 → v2.0.0
2. TCPS modules missing: `tcps_dashboard:start()` → `undef`
3. User reports "v2 is broken"

**Likelihood**: LOW (TCPS users are minority, clear migration guide)
**Impact**: MEDIUM (user frustration, migration effort)

---

### Mitigation Strategy

**Phase 1: Clear Communication**
```markdown
# BREAKING CHANGE: TCPS Separated

**v2.0.0 BREAKING CHANGE**: TCPS (Toyota Code Production System) is now a separate OTP application.

**If you use TCPS features (dashboard, receipt chain, work orders, etc.)**:

1. Add `tcps_erlmcp` to rebar.config:
   ```erlang
   {deps, [
       {erlmcp, "2.0.0"},
       {tcps_erlmcp, "2.0.0"}  % NEW DEPENDENCY
   ]}.
   ```

2. Update application.src:
   ```erlang
   {applications, [
       kernel, stdlib, erlmcp, tcps_erlmcp  % ADD tcps_erlmcp
   ]}.
   ```

**If you do NOT use TCPS**: No action required.
```

**Phase 2: Migration Guide**
- ✅ Document TCPS separation in CHANGELOG.md
- ✅ Provide `rebar.config` migration examples
- ✅ List all TCPS modules in migration guide

**Phase 3: Gradual Deprecation (v1.6.0 → v2.0.0)**
```erlang
% In v1.6.0 (optional transitional release):
-module(tcps_dashboard).
-deprecated({start, 0, "Use tcps_erlmcp:tcps_dashboard:start/0 instead"}).
```

---

### Mitigation Status

**Current**: MITIGATED (clear communication plan)
- [x] TCPS separation documented (v2_principles.md, v2_deletions.md)
- [x] TCPS module list (85+ modules in v2_deletions.md)
- [ ] CHANGELOG.md entry (BREAKING CHANGE section)
- [ ] Migration guide (docs/MIGRATION_v1_to_v2.md)
- [ ] Deprecation warnings (v1.6.0 optional transitional release)

**Confidence**: HIGH (clear documentation, gradual deprecation path)

---

## R6: API Compatibility Breaks

### Description
**Risk**: Deleting "optional" modules (e.g., `erlmcp_completion_context.erl`) may break users relying on them.

**Scenario**:
1. User calls `erlmcp_completion_context:create/1`
2. v2: Module deleted → `undef` error
3. User reports: "v2 broke my code"

**Likelihood**: MEDIUM (some users use optional features)
**Impact**: HIGH (user frustration, migration effort)

---

### Mitigation Strategy

**Phase 1: Deprecation Audit**
```bash
# Identify public API functions
grep -r "^-export" src/erlmcp_*.erl | grep -v "_tests.erl"

# Check usage in examples/
grep -r "erlmcp_completion_context" examples/
```

**Phase 2: Classify as KEEP vs. DELETE**
| Module | Public API? | Used in Examples? | Decision |
|--------|-------------|-------------------|----------|
| `erlmcp_completion_context.erl` | YES | NO | KEEP (optional) |
| `erlmcp_audio.erl` | YES | NO | DELETE (not in MCP spec) |
| `erlmcp_icon_cache.erl` | YES | NO | DELETE (niche) |

**Phase 3: Graceful Deprecation**
```erlang
% In v1.6.0: Add deprecation warnings
-module(erlmcp_audio).
-deprecated({play, 1, "Audio not supported in MCP spec 2024-11-05. Will be removed in v2.0.0."}).
```

**Phase 4: API Stability Contract**
```markdown
# erlmcp v2.0.0 API Stability

**Guaranteed Stable APIs** (will not change in v2.x):
- `erlmcp_server:add_resource/3`
- `erlmcp_server:add_tool/3`
- `erlmcp_server:add_prompt/3`
- `erlmcp_client:call_tool/3`
- `erlmcp_client:read_resource/2`

**Deprecated APIs** (removed in v2.0.0):
- `erlmcp_audio:play/1`
- `erlmcp_icon_cache:get/1`
- `erlmcp_complex_routing:route/2`
```

---

### Mitigation Status

**Current**: PLANNED (requires public API audit)
- [ ] Public API audit (identify all exported functions)
- [ ] Usage analysis (grep examples/, test/)
- [ ] Deprecation warnings (v1.6.0)
- [ ] API stability contract (docs/API_STABILITY.md)
- [ ] Migration guide (docs/MIGRATION_v1_to_v2.md)

**Confidence**: MEDIUM (requires audit, but process is clear)

---

## R7: Documentation Drift

### Description
**Risk**: Existing documentation (v1.x) may contradict v2 architecture docs.

**Scenario**:
1. Developer reads `docs/api-reference.md` (v1.x)
2. Tries to use `erlmcp_complex_routing.erl` (deleted in v2)
3. Confusion: "docs say it exists, but module not found"

**Likelihood**: LOW (C4 diagrams are comprehensive)
**Impact**: MEDIUM (developer confusion, onboarding friction)

---

### Mitigation Strategy

**Phase 1: Documentation Audit**
```bash
# Find references to deleted modules in docs
grep -r "erlmcp_complex_routing\|erlmcp_audio\|erlmcp_icon_cache" docs/

# Find TCPS references
grep -r "tcps_" docs/
```

**Phase 2: Update Existing Docs**
- ✅ Update `docs/api-reference.md` to v2 modules
- ✅ Add v2 migration guide: `docs/MIGRATION_v1_to_v2.md`
- ✅ Update `README.md` with v2 quick start

**Phase 3: Version Prefixes**
```markdown
# docs/api-reference.md

**Version**: erlmcp v2.0.0 (MCP spec 2024-11-05)

**For v1.x documentation, see**: [v1.5.0 branch](https://github.com/erlmcp/erlmcp/tree/v1.5.0/docs)
```

---

### Mitigation Status

**Current**: IN PROGRESS (C4 diagrams complete, old docs not yet updated)
- [x] v2 C4 diagrams (5 diagrams: L1-L4, INDEX)
- [x] v2 glossary (GLOSSARY.md)
- [x] v2 design inputs (4 documents: principles, required, deletions, risks)
- [ ] Update api-reference.md (remove deleted modules)
- [ ] Create MIGRATION_v1_to_v2.md
- [ ] Update README.md (v2 quick start)

**Confidence**: HIGH (C4 diagrams are authoritative, old docs can be updated incrementally)

---

## R8: Deployment Complexity

### Description
**Risk**: v2 requires deploying `erlmcp` + `tcps_erlmcp` (2 packages), increasing deployment complexity.

**Scenario**:
1. User deploys erlmcp v2 Docker image
2. TCPS features missing (forgot to add `tcps_erlmcp` dependency)
3. Runtime crash: `tcps_dashboard:start()` → `undef`

**Likelihood**: LOW (clear documentation, TCPS users are minority)
**Impact**: MEDIUM (deployment friction)

---

### Mitigation Strategy

**Phase 1: Docker Images**
```dockerfile
# Dockerfile.erlmcp (core, no TCPS)
FROM erlang:27-alpine
COPY _build/default/rel/erlmcp /app
CMD ["/app/bin/erlmcp", "foreground"]

# Dockerfile.erlmcp-tcps (includes TCPS)
FROM erlang:27-alpine
RUN apk add --no-cache tcps_erlmcp
COPY _build/default/rel/erlmcp /app
CMD ["/app/bin/erlmcp", "foreground"]
```

**Phase 2: Deployment Documentation**
```markdown
# Deployment Options

**Option 1: erlmcp Core (no TCPS)**
- Use: `erlmcp/erlmcp:2.0.0` Docker image
- Size: ~50 MB (base + erlmcp)

**Option 2: erlmcp + TCPS**
- Use: `erlmcp/erlmcp-tcps:2.0.0` Docker image
- Size: ~80 MB (base + erlmcp + tcps_erlmcp)
- OR: Add `{tcps_erlmcp, "2.0.0"}` to rebar.config
```

---

### Mitigation Status

**Current**: PLANNED (Docker images to be created)
- [ ] Docker images (erlmcp-core, erlmcp-tcps)
- [ ] Kubernetes YAML examples
- [ ] Helm charts (optional)
- [ ] Deployment documentation (docs/DEPLOYMENT.md update)

**Confidence**: HIGH (straightforward Docker multi-stage builds)

---

## Risk Matrix

```
                    IMPACT
            LOW         MEDIUM       HIGH
          +----------------------------+
LIKELIHOOD|           |              |
HIGH      |           | R3 (Testing) |      |
          |           |              |      |
          +----------------------------+
MEDIUM    |           | R4 (Library) | R1 (Deps)   |
          |           | R6 (API)     |      |
          |           | R8 (Deploy)  |      |
          +----------------------------+
LOW       | R7 (Docs) | R5 (TCPS)    | R2 (Perf)   |
          |           |              |      |
          +----------------------------+
```

**Priority**:
1. **HIGH/HIGH**: R3 (Testing) - Expand integration tests
2. **HIGH/MEDIUM**: R1 (Dependencies) - Static analysis + batch deletion
3. **MEDIUM/MEDIUM**: R4 (Library Migration) - API wrapper pattern
4. **MEDIUM/MEDIUM**: R6 (API Compatibility) - Deprecation audit
5. **LOW/HIGH**: R2 (Performance) - Benchmark validation
6. **LOW/MEDIUM**: R5 (TCPS Separation) - Clear communication
7. **LOW/MEDIUM**: R8 (Deployment) - Docker images
8. **LOW/LOW**: R7 (Documentation) - Update existing docs

---

## References

### Source Documents
- **Principles**: [v2_principles.md](./v2_principles.md)
- **Required Modules**: [v2_required_modules.md](./v2_required_modules.md)
- **Deletions**: [v2_deletions.md](./v2_deletions.md)
- **Code Map**: [L4-code-map.md](../C4/L4-code-map.md)
- **Glossary**: [GLOSSARY.md](../GLOSSARY.md)

### Source Code
- **Supervision**: `src/erlmcp_sup.erl:L111-L211`
- **Benchmarks**: `bench/erlmcp_bench_*.erl`
- **Tests**: `test/*.erl`

---

**Document Status**: CANONICAL (v2.0.0-draft)
**Last Updated**: 2026-01-27
**Purpose**: Identify and mitigate risks for erlmcp v2.0.0 refactoring
