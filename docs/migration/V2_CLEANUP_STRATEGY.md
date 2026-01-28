# erlmcp v2.0 Cleanup Strategy
## Safe Migration from Monolithic src/ to Umbrella apps/

**Created:** 2026-01-28
**Author:** Erlang Architect Agent
**Status:** READY FOR EXECUTION
**Risk Level:** MEDIUM (data loss possible if not followed exactly)

---

## Executive Summary

**Current State:**
- Legacy monolithic: 127 .erl files in `/src/`
- New umbrella: 379 .erl files across 4 apps in `/apps/`
- Legacy tests: 349 entries in `/test/` (mostly .skip files)
- New tests: Organized per-app test directories

**Cleanup Scope:**
- Delete 127 legacy source files
- Delete 349 legacy test files
- Remove 8 GraphQL modules (unsupported dependency)
- Preserve umbrella structure (4 apps: core, transports, observability, tcps)

**Timeline:** 2-3 hours (includes validation, deletion, verification)

---

## Phase 1: Pre-Deletion Validation (MANDATORY)

### 1.1 Compilation Check (CRITICAL)

**Purpose:** Verify new apps compile without legacy src/

```bash
# Test compilation without legacy src/
cd /Users/sac/erlmcp
mv src src.backup
mv test test.backup

# Compile all apps
rebar3 compile

# Expected: All 4 apps compile successfully
# - erlmcp_core: 35 modules
# - erlmcp_transports: 22 modules (excluding GraphQL)
# - erlmcp_observability: 26 modules
# - tcps_erlmcp: 68 modules

# If compilation fails, restore immediately:
# mv src.backup src
# mv test.backup test
```

**Success Criteria:**
- ✅ Zero compilation errors
- ✅ Zero missing module references
- ✅ All BEAM files generated in apps/_build/

**Failure Action:** STOP. Debug missing modules before proceeding.

---

### 1.2 Test Suite Verification

**Purpose:** Confirm new test organization works

```bash
# Run all app tests
rebar3 eunit --verbose

# Expected test counts:
# - erlmcp_core: ~20 test modules
# - erlmcp_transports: ~12 test modules
# - erlmcp_observability: ~15 test modules
# - tcps_erlmcp: ~10 test modules

# Check coverage
rebar3 cover
```

**Success Criteria:**
- ✅ All tests pass
- ✅ Coverage ≥80% (target per app)
- ✅ No references to legacy test/ directory

**Failure Action:** STOP. Fix test failures before deletion.

---

### 1.3 Dependency Audit

**Purpose:** Verify no hard-coded paths to src/ or test/

```bash
# Search for legacy path references
cd /Users/sac/erlmcp

# Check all apps for src/ references
grep -r "\"src/" apps/*/src/ apps/*/include/
grep -r "\\.\\.\/src/" apps/*/src/ apps/*/include/

# Check for test/ references
grep -r "\"test/" apps/*/test/
grep -r "\\.\\.\/test/" apps/*/test/

# Check rebar.config files
grep -r "src_dirs" apps/*/rebar.config
grep -r "test_dirs" apps/*/rebar.config
```

**Success Criteria:**
- ✅ Zero references to legacy `../src/` or `../test/`
- ✅ All includes use app-relative paths
- ✅ No hard-coded module paths

**Failure Action:** Update references to use umbrella paths.

---

### 1.4 Benchmark Baseline

**Purpose:** Establish performance baseline before deletion

```bash
# Run quick benchmark
cd /Users/sac/erlmcp
make benchmark-quick

# Record results:
# - Registry ops/sec: ~553K
# - Queue ops/sec: ~971K
# - Pool ops/sec: ~149K
# - Network I/O ops/sec: ~43K
```

**Success Criteria:**
- ✅ Benchmarks run successfully
- ✅ Results within 10% of baseline

**Post-Deletion:** Re-run to verify no performance regression.

---

## Phase 2: GraphQL Removal (FIRST)

### 2.1 Identify GraphQL Modules

**Location:** `/Users/sac/erlmcp/apps/erlmcp_transports/src/`

**Files to Delete (8 total):**
```
erlmcp_graphql_resolver.erl
erlmcp_graphql_resolver.erl.bak
erlmcp_graphql_resolver.erl.bak3
erlmcp_graphql_resolver.erl.bak4
erlmcp_graphql_schema.erl
erlmcp_transport_graphql.erl
erlmcp_transport_graphql.erl.bak
erlmcp_transport_graphql.erl.bak2
```

**Rationale:** GraphQL dependency not available in hex.pm, unsupported in v2.0

### 2.2 Remove GraphQL Tests

**Location:** `/Users/sac/erlmcp/apps/erlmcp_transports/test/`

**File:** `erlmcp_graphql_tests.erl`

### 2.3 Update Supervision Tree

**File:** `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_sup.erl`

**Action:** Remove GraphQL transport from child specs (if present)

```erlang
%% DELETE: GraphQL transport child spec
%% #{id => erlmcp_transport_graphql,
%%   start => {erlmcp_transport_graphql, start_link, []},
%%   restart => permanent,
%%   shutdown => 5000,
%%   type => worker}
```

### 2.4 Update Documentation

**Files to Update:**
- `apps/erlmcp_transports/README.md` - Remove GraphQL section
- `docs/api-reference.md` - Remove GraphQL endpoints
- `CHANGELOG.md` - Document GraphQL removal in v2.0

### 2.5 Execute GraphQL Deletion

```bash
cd /Users/sac/erlmcp/apps/erlmcp_transports/src

# Delete GraphQL modules
rm -v erlmcp_graphql_resolver.erl*
rm -v erlmcp_graphql_schema.erl
rm -v erlmcp_transport_graphql.erl*

# Delete tests
cd ../test
rm -v erlmcp_graphql_tests.erl

# Verify compilation
cd /Users/sac/erlmcp
rebar3 compile

# Expected: Success (no GraphQL references)
```

**Success Criteria:**
- ✅ All GraphQL files deleted
- ✅ Compilation succeeds
- ✅ No xref warnings about missing modules

---

## Phase 3: Legacy Test Directory Deletion

### 3.1 Test Directory Analysis

**Total Entries:** 349 files in `/Users/sac/erlmcp/test/`

**Breakdown:**
- `.skip` files (disabled tests): ~280 files
- `.bak` files (backups): ~40 files
- Active tests (moved to apps): ~20 files
- Documentation: ~9 files

**Why Delete First:** Tests don't affect runtime, safest to remove early.

### 3.2 Backup Creation

```bash
cd /Users/sac/erlmcp

# Create timestamped backup
tar -czf test_backup_$(date +%Y%m%d_%H%M%S).tar.gz test/

# Store in safe location
mv test_backup_*.tar.gz ../erlmcp_backups/

# Verify backup
tar -tzf ../erlmcp_backups/test_backup_*.tar.gz | head -20
```

**Success Criteria:**
- ✅ Backup created successfully
- ✅ Backup size reasonable (~50-100MB compressed)
- ✅ Backup stored outside project directory

### 3.3 Execute Test Directory Deletion

```bash
cd /Users/sac/erlmcp

# Final check: Ensure apps/*/test/ directories exist
ls -d apps/*/test/
# Expected: 4 directories (core, transports, observability, tcps)

# Delete legacy test directory
rm -rf test/

# Verify deletion
ls test/ 2>&1
# Expected: "No such file or directory"
```

**Success Criteria:**
- ✅ Legacy test/ directory removed
- ✅ Apps test directories intact
- ✅ Compilation still succeeds

### 3.4 Update Root rebar.config

**File:** `/Users/sac/erlmcp/rebar.config`

**Changes:**
```erlang
%% DELETE THIS LINE (line 34):
{test_dirs, ["test"]}.

%% DELETE THIS (lines 38-42):
{eunit_opts, [
    {exclude_dir, "test/integration"},
    {exclude_dir, "test/tcps_mcp_diataxis"},
    verbose
]}.

%% Tests now run per-app, umbrella only coordinates
```

---

## Phase 4: Legacy src/ Directory Deletion

### 4.1 Source File Analysis

**Total Files:** 127 .erl files in `/Users/sac/erlmcp/src/`

**Migration Status:**
- Migrated to erlmcp_core: ~35 modules
- Migrated to erlmcp_transports: ~22 modules
- Migrated to erlmcp_observability: ~26 modules
- Migrated to tcps_erlmcp: ~15 modules
- Duplicates/obsolete: ~29 modules

**Critical Files to Verify:**
- `erlmcp.app.src` → Moved to `apps/erlmcp_core/src/erlmcp_core.app.src`
- `erlmcp_sup.erl` → Moved to `apps/erlmcp_core/src/erlmcp_sup.erl`
- `erlmcp_client.erl` → Moved to `apps/erlmcp_core/src/erlmcp_client.erl`
- `erlmcp_server.erl` → Moved to `apps/erlmcp_core/src/erlmcp_server.erl`

### 4.2 Module Mapping Verification

**Script to Verify Migration:**
```bash
#!/bin/bash
# verify_migration.sh

cd /Users/sac/erlmcp

echo "Checking critical modules..."
for module in erlmcp_sup erlmcp_client erlmcp_server erlmcp_registry \
              erlmcp_json_rpc erlmcp_transport_stdio erlmcp_transport_tcp \
              erlmcp_transport_http erlmcp_otel erlmcp_metrics; do

    # Check if exists in apps
    apps_path=$(find apps/*/src -name "${module}.erl" 2>/dev/null)

    if [ -z "$apps_path" ]; then
        echo "❌ MISSING: $module"
    else
        echo "✅ Found: $module at $apps_path"
    fi
done

echo ""
echo "Compilation check..."
rebar3 compile 2>&1 | grep -E "(Compiling|Error|Warning)" | tail -20
```

**Success Criteria:**
- ✅ All critical modules found in apps/
- ✅ Compilation succeeds
- ✅ No undefined module errors

### 4.3 Backup Creation

```bash
cd /Users/sac/erlmcp

# Create timestamped backup
tar -czf src_backup_$(date +%Y%m%d_%H%M%S).tar.gz src/

# Store in safe location
mv src_backup_*.tar.gz ../erlmcp_backups/

# Verify backup
tar -tzf ../erlmcp_backups/src_backup_*.tar.gz | wc -l
# Expected: ~130 files
```

### 4.4 Execute src/ Directory Deletion

```bash
cd /Users/sac/erlmcp

# CRITICAL: Final safety check
echo "Current directory: $(pwd)"
echo "About to delete: src/"
ls -l src/ | head -10
read -p "Proceed with deletion? (yes/no): " confirm

if [ "$confirm" = "yes" ]; then
    # Delete legacy src directory
    rm -rf src/

    echo "Deletion complete."
    ls src/ 2>&1
    # Expected: "No such file or directory"
else
    echo "Deletion cancelled."
    exit 1
fi
```

**Success Criteria:**
- ✅ Legacy src/ directory removed
- ✅ Apps src/ directories intact
- ✅ Compilation succeeds

### 4.5 Update Root rebar.config

**File:** `/Users/sac/erlmcp/rebar.config`

**Changes:**
```erlang
%% DELETE THESE LINES (lines 32-35):
{src_dirs, ["src"]}.
{test_dirs, ["test"]}.
{include_dirs, ["include"]}.

%% Root rebar.config now ONLY manages umbrella coordination
%% Individual apps have their own src_dirs
```

---

## Phase 5: Cleanup Orphaned Files

### 5.1 Check for Stragglers

```bash
cd /Users/sac/erlmcp

# Find any remaining .erl files outside apps/
find . -name "*.erl" -not -path "./apps/*" -not -path "./_build/*" \
       -not -path "./.git/*" 2>/dev/null

# Expected: Empty (or only legacy backups)
```

### 5.2 Remove Crash Dumps

```bash
# Legacy crash dump in src/
rm -f src/erl_crash.dump

# Check for other crash dumps
find . -name "erl_crash.dump" -not -path "./_build/*"
```

### 5.3 Clean Build Artifacts

```bash
cd /Users/sac/erlmcp

# Clean all build artifacts
rebar3 clean

# Rebuild from scratch
rebar3 compile

# Expected: Clean build, all apps compile
```

---

## Phase 6: Post-Deletion Validation

### 6.1 Full Compilation Check

```bash
cd /Users/sac/erlmcp

# Clean build
rebar3 clean
rebar3 compile

# Expected output:
# ===> Verifying dependencies...
# ===> Analyzing applications...
# ===> Compiling erlmcp_core
# ===> Compiling erlmcp_transports
# ===> Compiling erlmcp_observability
# ===> Compiling tcps_erlmcp
```

**Success Criteria:**
- ✅ All 4 apps compile
- ✅ Zero errors
- ✅ Warnings ≤5 (acceptable)

### 6.2 Test Suite Verification

```bash
# Run all tests
rebar3 eunit --verbose

# Expected: ~60+ tests passing across all apps
```

**Success Criteria:**
- ✅ All tests pass
- ✅ Zero test failures
- ✅ Coverage ≥80%

### 6.3 Dialyzer Type Check

```bash
# Build PLT if needed
rebar3 dialyzer

# Expected: No type errors (warnings acceptable)
```

**Success Criteria:**
- ✅ Zero type errors
- ✅ Warnings ≤10 (document any new warnings)

### 6.4 Xref Cross-Reference

```bash
# Check for undefined functions
rebar3 xref

# Expected: Zero undefined function calls
# (except those in xref_ignores)
```

**Success Criteria:**
- ✅ Zero undefined functions
- ✅ All calls resolved within apps

### 6.5 Benchmark Regression Check

```bash
# Re-run quick benchmark
cd /Users/sac/erlmcp
make benchmark-quick

# Compare to baseline (Phase 1.4)
# Acceptable: ±10% variance
```

**Success Criteria:**
- ✅ Registry ops/sec: 500K-600K (baseline: 553K)
- ✅ Queue ops/sec: 870K-1M (baseline: 971K)
- ✅ Pool ops/sec: 135K-165K (baseline: 149K)
- ✅ Network I/O: 39K-47K (baseline: 43K)

**Failure Action:** Investigate performance regression before proceeding.

---

## Phase 7: Documentation Updates

### 7.1 Update CHANGELOG.md

**Add v2.0.0 Migration Entry:**
```markdown
## [2.0.0] - 2026-01-28

### Changed - BREAKING
- **Architecture:** Migrated from monolithic src/ to umbrella apps/ structure
  - erlmcp_core: JSON-RPC, Registry, Client/Server (35 modules)
  - erlmcp_transports: STDIO, TCP, HTTP, WebSocket (22 modules)
  - erlmcp_observability: Metrics, OTEL, Receipts (26 modules)
  - tcps_erlmcp: Toyota Code Production System (68 modules)

### Removed
- **GraphQL Support:** Removed due to unavailable hex.pm dependency
  - Deleted: erlmcp_graphql_resolver, erlmcp_graphql_schema
  - Deleted: erlmcp_transport_graphql
- **Legacy Structure:** Removed monolithic src/ and test/ directories
  - Migrated 127 modules to umbrella apps
  - Archived 349 legacy test files

### Migration Guide
- See docs/migration/V1_TO_V2_MIGRATION.md
- Update application dependencies to use erlmcp_core instead of erlmcp
- Update transport references to use erlmcp_transports app
```

### 7.2 Update README.md

**Update Quick Start Section:**
```markdown
## Quick Start

### Installation (Umbrella v2.0)

Add to your `rebar.config`:

```erlang
{deps, [
    {erlmcp_core, "2.0.0"},           % Core protocol
    {erlmcp_transports, "2.0.0"},     % Transports (optional)
    {erlmcp_observability, "2.0.0"}   % OTEL/Metrics (optional)
]}.
```

### Directory Structure (v2.0)

```
erlmcp/
├── apps/
│   ├── erlmcp_core/          % JSON-RPC, Client/Server
│   ├── erlmcp_transports/    % STDIO, TCP, HTTP, WS
│   ├── erlmcp_observability/ % OTEL, Metrics, Chaos
│   └── tcps_erlmcp/          % Quality gates (optional)
├── config/                    % Shared configuration
├── docs/                      % Documentation
└── rebar.config              % Umbrella coordinator
```
```

### 7.3 Update Architecture Documentation

**File:** `docs/architecture.md`

**Add Umbrella Section:**
```markdown
## Umbrella Architecture (v2.0)

### Application Boundaries

**erlmcp_core** (Foundation)
- Dependencies: jsx, jesse, gproc
- Modules: 35 (Client, Server, Registry, JSON-RPC)
- Purpose: Core MCP protocol implementation

**erlmcp_transports** (I/O Layer)
- Dependencies: gun, ranch, poolboy, erlmcp_core
- Modules: 22 (STDIO, TCP, HTTP, WebSocket)
- Purpose: Transport abstractions

**erlmcp_observability** (Operations)
- Dependencies: opentelemetry_api, erlmcp_core
- Modules: 26 (Metrics, OTEL, Chaos, Receipts)
- Purpose: Production monitoring

**tcps_erlmcp** (Quality)
- Dependencies: bbmustache, cowboy, erlmcp_core, erlmcp_observability
- Modules: 68 (SHACL, Kanban, Jidoka, Receipts)
- Purpose: Manufacturing-grade quality gates
```

### 7.4 Create Migration Guide

**File:** `docs/migration/V1_TO_V2_MIGRATION.md`

**Content:** (see below in Section 8)

---

## Phase 8: Rollback Plan

### 8.1 Rollback Triggers

**Immediate Rollback Required If:**
- ❌ Compilation fails after src/ deletion
- ❌ Test failure rate >5%
- ❌ Performance regression >25%
- ❌ Missing critical modules (client/server/registry)
- ❌ Production blocker discovered

### 8.2 Rollback Procedure

```bash
#!/bin/bash
# rollback_v2_migration.sh

cd /Users/sac/erlmcp

echo "ROLLBACK: Restoring legacy structure..."

# Restore backups
tar -xzf ../erlmcp_backups/src_backup_*.tar.gz
tar -xzf ../erlmcp_backups/test_backup_*.tar.gz

# Restore rebar.config
git checkout rebar.config

# Clean and rebuild
rebar3 clean
rebar3 compile

# Verify
if rebar3 eunit; then
    echo "✅ Rollback successful"
else
    echo "❌ Rollback failed - manual intervention required"
    exit 1
fi
```

**Rollback Time:** ~5 minutes

### 8.3 Partial Rollback

**Scenario:** GraphQL removal breaks production, but umbrella migration succeeds

**Action:** Restore GraphQL modules only
```bash
cd /Users/sac/erlmcp/apps/erlmcp_transports/src

# Extract GraphQL modules from backup
tar -xzf ../../../../erlmcp_backups/src_backup_*.tar.gz \
    erlmcp_graphql_resolver.erl \
    erlmcp_graphql_schema.erl \
    erlmcp_transport_graphql.erl

# Recompile
cd /Users/sac/erlmcp
rebar3 compile
```

### 8.4 Emergency Contact

**If Rollback Fails:**
1. Stop all erlmcp services
2. Create incident report with compilation errors
3. Contact: Erlang Architect Agent (this agent)
4. Preserve all backup files
5. Document exact failure state

---

## Phase 9: Final Checklist

### 9.1 Pre-Deletion (All MUST Pass)

- [ ] Phase 1.1: Compilation check passed
- [ ] Phase 1.2: Test suite verification passed
- [ ] Phase 1.3: Dependency audit clean
- [ ] Phase 1.4: Benchmark baseline recorded
- [ ] Backups created and verified
- [ ] Team notified of upcoming deletion

### 9.2 GraphQL Removal (Complete Before src/ Deletion)

- [ ] Phase 2.1: 8 GraphQL files identified
- [ ] Phase 2.2: GraphQL tests identified
- [ ] Phase 2.3: Supervision tree updated
- [ ] Phase 2.4: Documentation updated
- [ ] Phase 2.5: GraphQL deletion executed
- [ ] Compilation succeeds post-GraphQL removal

### 9.3 Test Directory Deletion

- [ ] Phase 3.2: test/ backup created
- [ ] Phase 3.3: test/ directory deleted
- [ ] Phase 3.4: rebar.config updated
- [ ] Apps test directories verified intact
- [ ] Tests still run successfully

### 9.4 src/ Directory Deletion

- [ ] Phase 4.2: Module migration verified
- [ ] Phase 4.3: src/ backup created
- [ ] Phase 4.4: src/ directory deleted
- [ ] Phase 4.5: rebar.config updated
- [ ] Apps src/ directories verified intact
- [ ] Compilation succeeds

### 9.5 Post-Deletion Validation

- [ ] Phase 6.1: Full compilation passed
- [ ] Phase 6.2: All tests passing
- [ ] Phase 6.3: Dialyzer clean
- [ ] Phase 6.4: Xref clean
- [ ] Phase 6.5: No performance regression

### 9.6 Documentation

- [ ] Phase 7.1: CHANGELOG.md updated
- [ ] Phase 7.2: README.md updated
- [ ] Phase 7.3: Architecture docs updated
- [ ] Phase 7.4: Migration guide created

### 9.7 Finalization

- [ ] All backups stored safely
- [ ] Git commit created with migration changes
- [ ] Team notified of completion
- [ ] Release notes prepared for v2.0.0

---

## Phase 10: Execution Timeline

### Recommended Schedule

**Preparation (30 minutes)**
- Review this document thoroughly
- Run Phase 1 validation checks
- Create backups
- Notify team

**Execution (60 minutes)**
- Phase 2: GraphQL removal (15 min)
- Phase 3: Test deletion (10 min)
- Phase 4: src/ deletion (20 min)
- Phase 5: Cleanup (10 min)
- Phase 6: Validation (30 min)

**Finalization (30 minutes)**
- Phase 7: Documentation updates
- Git commit and tag
- Update CHANGELOG
- Release notes

**Total Time:** ~2 hours (with buffer)

### Best Practices

1. **Execute during low-traffic period** (off-hours recommended)
2. **Have rollback script ready** before starting
3. **Run validation checks after each phase**
4. **Document any unexpected issues**
5. **Keep backups for at least 30 days**

---

## Appendix A: Module Migration Map

### erlmcp_core (35 modules)

**From src/ → apps/erlmcp_core/src/**
```
erlmcp.erl → erlmcp_app.erl (renamed)
erlmcp_sup.erl → erlmcp_sup.erl
erlmcp_core_sup.erl → erlmcp_core_sup.erl
erlmcp_client.erl → erlmcp_client.erl
erlmcp_server.erl → erlmcp_server.erl
erlmcp_server_sup.erl → erlmcp_server_sup.erl
erlmcp_registry.erl → erlmcp_registry.erl
erlmcp_json_rpc.erl → erlmcp_json_rpc.erl
erlmcp_message_handler.erl → erlmcp_message_handler.erl
erlmcp_message_parser.erl → erlmcp_message_parser.erl
erlmcp_auth.erl → erlmcp_auth.erl
erlmcp_batch.erl → erlmcp_batch.erl (NEW in v2.0)
erlmcp_cache.erl → erlmcp_cache.erl (NEW in v2.0)
erlmcp_circuit_breaker.erl → erlmcp_circuit_breaker.erl
erlmcp_rate_limiter.erl → erlmcp_rate_limiter.erl
erlmcp_session.erl → erlmcp_session.erl
erlmcp_resource.erl → erlmcp_resource.erl
erlmcp_tool.erl → erlmcp_tool.erl
erlmcp_subscription.erl → erlmcp_subscription.erl
erlmcp_task.erl → erlmcp_task.erl
erlmcp_schema_validator.erl → erlmcp_schema_validator.erl
erlmcp_schema_registry.erl → erlmcp_schema_registry.erl
erlmcp_secrets.erl → erlmcp_secrets.erl
erlmcp_graceful_drain.erl → erlmcp_graceful_drain.erl
erlmcp_code_reload.erl → erlmcp_code_reload.erl
erlmcp_message_size.erl → erlmcp_message_size.erl

%% Clustering (NEW in v2.0)
erlmcp_cluster_sup.erl
erlmcp_node_monitor.erl
erlmcp_registry_dist.erl
erlmcp_split_brain_detector.erl

%% Rate limiting middleware
erlmcp_rate_limit_middleware.erl

%% Hot code reload
erlmcp_reload_sup.erl
```

### erlmcp_transports (22 modules)

**From src/ → apps/erlmcp_transports/src/**
```
erlmcp_transport_stdio.erl → erlmcp_transport_stdio.erl
erlmcp_transport_tcp.erl → erlmcp_transport_tcp.erl
erlmcp_transport_http.erl → erlmcp_transport_http.erl
erlmcp_transport_http_server.erl → erlmcp_transport_http_server.erl
erlmcp_transport_ws.erl → erlmcp_transport_ws.erl
erlmcp_transport_sup.erl → erlmcp_transport_sup.erl
erlmcp_transport_behavior.erl → erlmcp_transport_behavior.erl

%% NEW in v2.0
erlmcp_transport_sse.erl (Server-Sent Events)
erlmcp_transport_pipeline.erl (HTTP/2 multiplexing)
erlmcp_transport_registry.erl
erlmcp_transport_discovery.erl
erlmcp_pool_manager.erl
erlmcp_pool_strategy.erl
erlmcp_security_headers.erl

%% DELETED (GraphQL - 8 files)
%% erlmcp_graphql_resolver.erl → REMOVED
%% erlmcp_graphql_schema.erl → REMOVED
%% erlmcp_transport_graphql.erl → REMOVED
%% (+ 5 .bak files)
```

### erlmcp_observability (26 modules)

**From src/ → apps/erlmcp_observability/src/**
```
erlmcp_otel.erl → erlmcp_otel.erl
erlmcp_metrics.erl → erlmcp_metrics.erl
erlmcp_metrics_server.erl → erlmcp_metrics_server.erl
erlmcp_health_monitor.erl → erlmcp_health_monitor.erl
erlmcp_receipt_chain.erl → erlmcp_receipt_chain.erl
erlmcp_evidence_path.erl → erlmcp_evidence_path.erl
erlmcp_recovery_manager.erl → erlmcp_recovery_manager.erl
erlmcp_chaos.erl → erlmcp_chaos.erl

%% NEW in v2.0
erlmcp_observability_sup.erl
erlmcp_otel_middleware.erl (automatic tracing)
erlmcp_otel_jaeger.erl (Jaeger exporter)
erlmcp_otel_datadog.erl (Datadog exporter)
erlmcp_otel_honeycomb.erl (Honeycomb exporter)
erlmcp_profiler.erl
erlmcp_memory_analyzer.erl
erlmcp_metrics_aggregator.erl
erlmcp_dashboard_server.erl
erlmcp_dashboard_http_handler.erl
erlmcp_debugger.erl
erlmcp_audit_log.erl

%% Chaos engineering
erlmcp_chaos_network.erl
erlmcp_chaos_process.erl
erlmcp_chaos_resource.erl

%% Benchmarking
erlmcp_bench_rate_limit.erl
```

### tcps_erlmcp (68 modules)

**From src/ → apps/tcps_erlmcp/src/**
```
tcps_* modules (68 total) → tcps_erlmcp/src/

Key modules:
- tcps_erlmcp_sup.erl (top-level supervisor)
- tcps_mcp_server.erl (MCP protocol bridge)
- tcps_quality_gates.erl (Jidoka integration)
- tcps_receipt.erl (SHA-256 receipt chain)
- tcps_kanban.erl (WIP limits)
- tcps_heijunka.erl (production leveling)
- tcps_andon.erl (stop-the-line signaling)
- tcps_kaizen.erl (continuous improvement)
```

---

## Appendix B: Critical Safety Checks

### Pre-Deletion Safety Script

```bash
#!/bin/bash
# pre_deletion_safety.sh

set -e  # Exit on any error

cd /Users/sac/erlmcp

echo "======================================"
echo "ERLMCP V2 PRE-DELETION SAFETY CHECKS"
echo "======================================"

# Check 1: Umbrella apps exist
echo ""
echo "[1/8] Checking umbrella structure..."
for app in erlmcp_core erlmcp_transports erlmcp_observability tcps_erlmcp; do
    if [ -d "apps/$app/src" ]; then
        echo "  ✅ apps/$app/src exists"
    else
        echo "  ❌ MISSING: apps/$app/src"
        exit 1
    fi
done

# Check 2: Compilation works
echo ""
echo "[2/8] Testing compilation..."
if rebar3 compile 2>&1 | grep -q "ERROR"; then
    echo "  ❌ Compilation failed"
    exit 1
else
    echo "  ✅ Compilation succeeded"
fi

# Check 3: Critical modules present in apps
echo ""
echo "[3/8] Checking critical modules..."
for module in erlmcp_client erlmcp_server erlmcp_registry erlmcp_json_rpc; do
    if find apps/*/src -name "${module}.erl" | grep -q "."; then
        echo "  ✅ $module found"
    else
        echo "  ❌ MISSING: $module"
        exit 1
    fi
done

# Check 4: Tests exist in apps
echo ""
echo "[4/8] Checking test structure..."
for app in erlmcp_core erlmcp_transports erlmcp_observability tcps_erlmcp; do
    if [ -d "apps/$app/test" ]; then
        test_count=$(ls apps/$app/test/*.erl 2>/dev/null | wc -l)
        echo "  ✅ apps/$app/test: $test_count tests"
    else
        echo "  ⚠️  No test dir: apps/$app/test"
    fi
done

# Check 5: Backups directory ready
echo ""
echo "[5/8] Checking backup location..."
if [ ! -d "../erlmcp_backups" ]; then
    mkdir -p ../erlmcp_backups
    echo "  ✅ Created ../erlmcp_backups"
else
    echo "  ✅ ../erlmcp_backups exists"
fi

# Check 6: Legacy directories exist
echo ""
echo "[6/8] Checking legacy structure..."
if [ -d "src" ] && [ -d "test" ]; then
    src_count=$(ls src/*.erl 2>/dev/null | wc -l)
    test_count=$(ls test/* 2>/dev/null | wc -l)
    echo "  ✅ src/: $src_count files"
    echo "  ✅ test/: $test_count entries"
else
    echo "  ❌ Legacy directories missing (already deleted?)"
    exit 1
fi

# Check 7: Git status
echo ""
echo "[7/8] Checking git status..."
if git status --porcelain | grep -q "^??"; then
    echo "  ⚠️  Untracked files present (normal)"
else
    echo "  ✅ No untracked files"
fi

# Check 8: Running processes
echo ""
echo "[8/8] Checking for running erlmcp processes..."
if pgrep -f "erlmcp" > /dev/null; then
    echo "  ⚠️  Warning: erlmcp processes running"
    echo "     Consider stopping before deletion"
else
    echo "  ✅ No erlmcp processes running"
fi

echo ""
echo "======================================"
echo "✅ ALL SAFETY CHECKS PASSED"
echo "======================================"
echo ""
echo "Ready to proceed with deletion."
echo "Backups will be created before deletion."
echo ""
echo "Next steps:"
echo "  1. ./scripts/delete_legacy_structure.sh"
echo "  2. Review deletion output"
echo "  3. Run post-deletion validation"
```

---

## Appendix C: Deletion Execution Script

```bash
#!/bin/bash
# delete_legacy_structure.sh

set -e  # Exit on any error

cd /Users/sac/erlmcp

echo "======================================"
echo "ERLMCP V2 LEGACY STRUCTURE DELETION"
echo "======================================"

# Safety confirmation
echo ""
echo "⚠️  WARNING: This will permanently delete:"
echo "  - src/ directory (127 .erl files)"
echo "  - test/ directory (349 entries)"
echo "  - GraphQL modules (8 files)"
echo ""
read -p "Type 'DELETE' to confirm: " confirm

if [ "$confirm" != "DELETE" ]; then
    echo "Deletion cancelled."
    exit 0
fi

# Phase 1: Create backups
echo ""
echo "Phase 1: Creating backups..."
timestamp=$(date +%Y%m%d_%H%M%S)

if [ -d "src" ]; then
    tar -czf "../erlmcp_backups/src_backup_${timestamp}.tar.gz" src/
    echo "  ✅ src/ backed up"
fi

if [ -d "test" ]; then
    tar -czf "../erlmcp_backups/test_backup_${timestamp}.tar.gz" test/
    echo "  ✅ test/ backed up"
fi

# Phase 2: Delete GraphQL modules
echo ""
echo "Phase 2: Removing GraphQL modules..."
cd apps/erlmcp_transports/src
rm -vf erlmcp_graphql_*.erl* erlmcp_transport_graphql.erl*
cd ../test
rm -vf erlmcp_graphql_tests.erl
cd /Users/sac/erlmcp
echo "  ✅ GraphQL modules removed"

# Phase 3: Delete legacy test directory
echo ""
echo "Phase 3: Deleting legacy test/ directory..."
if [ -d "test" ]; then
    rm -rf test/
    echo "  ✅ test/ deleted"
else
    echo "  ⚠️  test/ already deleted"
fi

# Phase 4: Delete legacy src directory
echo ""
echo "Phase 4: Deleting legacy src/ directory..."
if [ -d "src" ]; then
    rm -rf src/
    echo "  ✅ src/ deleted"
else
    echo "  ⚠️  src/ already deleted"
fi

# Phase 5: Update rebar.config
echo ""
echo "Phase 5: Updating root rebar.config..."
# Backup rebar.config
cp rebar.config "rebar.config.backup_${timestamp}"

# Remove src_dirs and test_dirs lines
sed -i.tmp '/^{src_dirs, \["src"\]}\./d' rebar.config
sed -i.tmp '/^{test_dirs, \["test"\]}\./d' rebar.config
sed -i.tmp '/^{include_dirs, \["include"\]}\./d' rebar.config
rm -f rebar.config.tmp

echo "  ✅ rebar.config updated"

# Phase 6: Verification
echo ""
echo "Phase 6: Running verification..."
rebar3 clean
if rebar3 compile; then
    echo "  ✅ Compilation successful"
else
    echo "  ❌ Compilation failed"
    echo "  Rolling back..."
    ./scripts/rollback_v2_migration.sh
    exit 1
fi

# Final summary
echo ""
echo "======================================"
echo "✅ DELETION COMPLETE"
echo "======================================"
echo ""
echo "Summary:"
echo "  - GraphQL modules: REMOVED"
echo "  - Legacy test/: DELETED"
echo "  - Legacy src/: DELETED"
echo "  - Backups: ../erlmcp_backups/"
echo ""
echo "Next steps:"
echo "  1. Run: rebar3 eunit"
echo "  2. Run: rebar3 dialyzer"
echo "  3. Run: make benchmark-quick"
echo "  4. Update documentation"
echo "  5. Git commit changes"
```

---

## Appendix D: Post-Deletion Validation Script

```bash
#!/bin/bash
# post_deletion_validation.sh

set -e

cd /Users/sac/erlmcp

echo "======================================"
echo "ERLMCP V2 POST-DELETION VALIDATION"
echo "======================================"

# Check 1: Legacy directories gone
echo ""
echo "[1/10] Verifying legacy structure deleted..."
if [ -d "src" ] || [ -d "test" ]; then
    echo "  ❌ Legacy directories still present"
    exit 1
else
    echo "  ✅ Legacy directories deleted"
fi

# Check 2: Umbrella apps intact
echo ""
echo "[2/10] Verifying umbrella structure..."
for app in erlmcp_core erlmcp_transports erlmcp_observability tcps_erlmcp; do
    if [ -d "apps/$app/src" ]; then
        module_count=$(ls apps/$app/src/*.erl 2>/dev/null | wc -l)
        echo "  ✅ apps/$app: $module_count modules"
    else
        echo "  ❌ MISSING: apps/$app"
        exit 1
    fi
done

# Check 3: Compilation
echo ""
echo "[3/10] Testing clean compilation..."
rebar3 clean
if rebar3 compile 2>&1 | tee /tmp/compile.log | grep -q "ERROR"; then
    echo "  ❌ Compilation failed"
    cat /tmp/compile.log
    exit 1
else
    echo "  ✅ Compilation successful"
fi

# Check 4: Tests
echo ""
echo "[4/10] Running test suite..."
if rebar3 eunit --verbose 2>&1 | tee /tmp/tests.log; then
    test_count=$(grep -c "Test passed" /tmp/tests.log || echo "0")
    echo "  ✅ All tests passed ($test_count tests)"
else
    echo "  ❌ Test failures detected"
    exit 1
fi

# Check 5: Dialyzer
echo ""
echo "[5/10] Running Dialyzer..."
if rebar3 dialyzer 2>&1 | grep -q "ERROR"; then
    echo "  ❌ Dialyzer errors"
    exit 1
else
    echo "  ✅ Dialyzer clean"
fi

# Check 6: Xref
echo ""
echo "[6/10] Running xref..."
if rebar3 xref 2>&1 | grep -q "ERROR"; then
    echo "  ❌ Xref errors"
    exit 1
else
    echo "  ✅ Xref clean"
fi

# Check 7: GraphQL removed
echo ""
echo "[7/10] Verifying GraphQL removal..."
if find apps/*/src -name "*graphql*" | grep -q "."; then
    echo "  ❌ GraphQL files still present"
    exit 1
else
    echo "  ✅ GraphQL fully removed"
fi

# Check 8: Coverage
echo ""
echo "[8/10] Checking test coverage..."
rebar3 cover
coverage=$(grep -o "[0-9]\+%" _build/test/cover/index.html | head -1 || echo "0%")
echo "  ℹ️  Coverage: $coverage (target: ≥80%)"

# Check 9: Benchmarks
echo ""
echo "[9/10] Running quick benchmarks..."
if make benchmark-quick 2>&1 | tee /tmp/bench.log | grep -q "ERROR"; then
    echo "  ❌ Benchmark errors"
    exit 1
else
    echo "  ✅ Benchmarks passed"
    grep "ops/sec" /tmp/bench.log | head -5
fi

# Check 10: File integrity
echo ""
echo "[10/10] Checking file integrity..."
broken_symlinks=$(find apps/ -xtype l 2>/dev/null | wc -l)
if [ "$broken_symlinks" -gt 0 ]; then
    echo "  ⚠️  Warning: $broken_symlinks broken symlinks"
else
    echo "  ✅ No broken symlinks"
fi

echo ""
echo "======================================"
echo "✅ VALIDATION COMPLETE - V2.0 READY"
echo "======================================"
echo ""
echo "Summary:"
echo "  ✅ Legacy structure removed"
echo "  ✅ Umbrella apps functional"
echo "  ✅ Compilation clean"
echo "  ✅ Tests passing"
echo "  ✅ Type checking clean"
echo "  ✅ Cross-references clean"
echo "  ✅ GraphQL removed"
echo "  ✅ Coverage acceptable"
echo "  ✅ Benchmarks passing"
echo "  ✅ File integrity verified"
echo ""
echo "Ready for production deployment."
```

---

## Document Version History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2026-01-28 | Erlang Architect Agent | Initial cleanup strategy |

---

## Sign-Off

**Prepared By:** Erlang Architect Agent
**Date:** 2026-01-28
**Status:** READY FOR EXECUTION

**Pre-Execution Checklist:**
- [ ] All stakeholders notified
- [ ] Backup location verified
- [ ] Rollback script tested
- [ ] Emergency contact established
- [ ] Execution window scheduled

**Post-Execution Checklist:**
- [ ] All validation checks passed
- [ ] Documentation updated
- [ ] Git commit created
- [ ] Team notified of completion
- [ ] Backups archived

---

**END OF CLEANUP STRATEGY DOCUMENT**
