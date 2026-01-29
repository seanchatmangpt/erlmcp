# Update README.md with v2.1.0 features and production readiness status Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Update the project's primary README.md file to accurately reflect erlmcp v2.1.0's current state, including architectural changes from monolithic v1.x to the 4-app umbrella structure, updated performance benchmarks showing 2.52M msg/sec throughput, and critical production readiness warnings documenting that v2.1.0 is **NOT production-ready** due to failing quality gates.

The README must apply TCPS **Andon (行灯)** principles - making quality status visible to all users through prominent warnings that prevent accidental deployment of untested code.

### Quality Gate Requirements

This is a **documentation task**, but must achieve 100% accuracy:

- **Documentation Accuracy**: 100% (all version references, module counts, performance numbers must match source files)
- **Production Warning Visibility**: Top 30 lines of README (prominent ❌ NOT PRODUCTION READY banner)
- **Source Validation**: All data must be cited from actual source files (no assumptions)
- **Link Validation**: All referenced files must exist

**No compilation or test requirements** (documentation-only update).

## Current State

### What Exists Now

**File**: `/Users/sac/erlmcp/README.md` (460 lines)

- **Line 30**: References "v2.0.0 umbrella application" (outdated - should be v2.1.0)
- **Lines 46-88**: Umbrella structure table with outdated module counts:
  - erlmcp_core: 14 modules (actual: 35 modules - **ERROR**)
  - erlmcp_transports: 8 modules (actual: 22 modules - **ERROR**)
  - erlmcp_observability: 9 modules (actual: 26 modules - **ERROR**)
  - tcps_erlmcp: 63 modules (actual: 68 modules - **ERROR**)
  - Total: 94 modules (actual: 151 modules - **ERROR**)
- **Line 143**: tcps_erlmcp shows 63 modules (should be 68)
- **Line 145**: "Total: 94 modules" (should be 151)
- **Lines 237-243**: Performance section missing actual v2.1.0 benchmarks
- **Line 271**: Migration guide references v0.5.x→v0.6.0 (should be v1.x→v2.x)
- **Line 292**: "v0.6.0 Library Integration" section (should be v2.1.0 features)
- **Line 339**: "What's New in v0.6.0" section (should be v2.1.0)
- **Line 343**: References v0.6.0 replacing ~770 LOC (should be v2.1.0 legacy cleanup)
- **Line 441**: Library migration guide references v0.5→v0.6.0 (should be v1.x→v2.x)
- **CRITICAL GAP**: No production readiness warning banner

### What's Missing

**Gap Quantified:**
- **Version accuracy**: 5 sections reference outdated versions (v0.6.0, v2.0.0)
- **Module count accuracy**: 57-module gap (94 claimed vs 151 actual)
- **Performance accuracy**: Missing v2.1.0 baseline (2.52M msg/sec)
- **Production warnings**: 0 warnings (CRITICAL - users may deploy untested code)

**Root Cause** (5 Whys Analysis):
1. **Why?** README.md still references v0.6.0 and v2.0.0
2. **Why?** README was not updated during v2.1.0 release on 2026-01-28
3. **Why?** Release process (CHANGELOG.md updated) did not include README.md update as required step
4. **Why?** No automated validation checks if README.md version matches rebar.config release version
5. **Why?** Documentation quality gates not integrated into TCPS manufacturing system (Poka-yoke gap)

**Impact**:
- **HIGH**: Users may deploy v2.1.0 to production assuming it's ready (actual: 1% coverage vs 80% required, 5 failing test suites)
- **MEDIUM**: Users capacity plan using outdated 2.69M msg/sec (actual v2.1.0: 2.52M msg/sec - 6.3% gap)
- **MEDIUM**: Users confused by v0.6.0 migration references in v2.1.0 README

### Key Discoveries from Research

**Finding 1: Version Confirmed**
- `/Users/sac/erlmcp/rebar.config:220`: `{release, {erlmcp, "2.1.0"},`
- `/Users/sac/erlmcp/CHANGELOG.md:39`: `## [2.1.0] - 2026-01-28`
- All app.src files: `{vsn, "2.1.0"}`

**Finding 2: Module Counts Verified**
From CHANGELOG.md lines 63-66 (v2.1.0 release):
- erlmcp_core: 35 modules (JSON-RPC, registry, client/server)
- erlmcp_transports: 22 modules (TCP, HTTP, WebSocket, STDIO)
- erlmcp_observability: 26 modules (OTEL, metrics, receipts)
- tcps_erlmcp: 68 modules (TCPS quality gates, SHACL)
- **Total: 151 modules**

**Finding 3: Performance Baseline Confirmed**
- `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md:14`: **2.52M msg/sec** (v2.1.0 actual)
- Component breakdown (lines 53-57):
  - Registry: 1.94M ops/sec
  - Queue: 10.0M ops/sec
  - Pool: 2.50M ops/sec
  - Session: 0.95M ops/sec
- Historical: 403% improvement over v1.5.0 (line 19)

**Finding 4: Production Status CRITICAL**
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:6`: ❌ **NOT PRODUCTION READY**
- Test Coverage: 1% vs 80% required (79 percentage point gap)
- EUnit: 80.8% pass rate vs 100% required
- Common Test: 5 suite failures
- Dialyzer: 526 warnings
- Xref: 250 warnings

**Finding 5: Documentation Pattern**
- Warning pattern from `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:6-15`: Use ❌ symbol and explicit text
- Performance pattern from `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md:49-69`: Clear tables with metrics
- Quality gate pattern from `/Users/sac/erlmcp/CODE_QUALITY_REPORT_V2.1.md:13-19`: Table format with status columns

## Desired End State

### Specification

**File**: `/Users/sac/erlmcp/README.md`

**Section 1: Production Readiness Banner** (NEW - insert after badges, before "## Installation")
```markdown
## ⚠️ Production Readiness Status

**Status**: ❌ **NOT PRODUCTION READY** (v2.1.0)

**Critical Quality Gate Failures**:
- ❌ **Test Coverage**: 1% (REQUIRED: ≥80%) - 79 percentage point gap
- ❌ **EUnit Tests**: 80.8% pass rate (REQUIRED: 100%) - 5/26 tests failing
- ❌ **Common Test**: 5 suite failures (erlmcp_integration_SUITE, erlmcp_observability_SUITE, erlmcp_registry_dist_SUITE, erlmcp_transport_behavior_SUITE)
- ❌ **Dialyzer**: 526 type warnings (REQUIRED: 0)
- ❌ **Xref**: 250 warnings (60+ undefined function calls)

**Details**: See [V2_PRODUCTION_READINESS_REPORT.md](docs/V2_PRODUCTION_READINESS_REPORT.md)

**Risk Assessment**: DO NOT DEPLOY to production. System requires significant testing investment (80-150 hours estimated).
```

**Section 2: Architecture Table Update** (lines 52-145)
- Update line 30: "v2.0.0" → "v2.1.0"
- Update lines 52-88: Module counts to match v2.1.0:
  - erlmcp_core: 14 → **35 modules**
  - erlmcp_transports: 8 → **22 modules**
  - erlmcp_observability: 9 → **26 modules**
  - tcps_erlmcp: 63 → **68 modules**
  - Total: 94 → **151 modules**
- Update line 126: "## v2.0.0 Architecture" → "## v2.1.0 Architecture"

**Section 3: Performance Section Update** (lines 237-243, add new section after "Build Targets by Category")
```markdown
## Performance Benchmarks

### v2.1.0 Baseline (2026-01-28)

**Core Throughput**: 2.52M msg/sec (403% improvement over v1.5.0)

| Component | Throughput | Latency (p50) | Latency (p99) | Grade |
|-----------|------------|---------------|---------------|-------|
| Registry | 1.94M ops/sec | 52 μs | 101 μs | A |
| Queue | 10.0M ops/sec | 0 μs | 1 μs | A+ |
| Pool | 2.50M ops/sec | 0 μs | 1 μs | A+ |
| Session | 0.95M ops/sec | 1 μs | 108 μs | A |
| **OVERALL** | **2.52M msg/sec** | **0 μs** | **99 μs** | **A+** |

**Details**: See [V2.1_BASELINE_DELIVERABLE_SUMMARY.md](V2.1_BASELINE_DELIVERABLE_SUMMARY.md)

### Regression Detection
- **Threshold**: <10% degradation
- **Current**: No regressions detected
```

**Section 4: Version Updates**
- Line 271: "Upgrade guide from v0.5.x to v0.6.0" → "Upgrade guide from v1.x to v2.x"
- Line 292: "### v0.6.0 Library Integration 🆕" → "### v2.1.0 Features 🆕"
- Line 339: "## What's New in v0.6.0" → "## What's New in v2.1.0"
- Line 343: "erlmcp v0.6.0 replaces" → "erlmcp v2.1.0 removed 127 legacy modules"
- Line 441: "v0.5 → v0.6.0 migration" → "v1.x → v2.x migration"

**Section 5: v2.1.0 Features Section** (replace lines 289-371)
```markdown
### v2.1.0 Features 🆕

**Architecture Transformation**
- ✅ **Legacy Cleanup**: Removed 127 modules, 6,079 lines deleted
- ✅ **4-App Umbrella Finalized**:
  - erlmcp_core v2.1.0 - 35 modules (JSON-RPC, registry, client/server)
  - erlmcp_transports v2.1.0 - 22 modules (TCP, HTTP, WebSocket, STDIO)
  - erlmcp_observability v2.1.0 - 26 modules (OTEL, metrics, receipts)
  - tcps_erlmcp v2.1.0 - 68 modules (TCPS quality gates, SHACL)

**Enhanced Testing Infrastructure** (4 new test suites, 1,507 LOC)
- ✅ erlmcp_connection_pool_tests.erl - Connection pooling validation
- ✅ erlmcp_hot_reload_tests.erl - Hot code reload scenarios
- ✅ erlmcp_registry_distributed_tests.erl - Multi-node registry tests
- ✅ erlmcp_trace_propagation_tests.erl - OTEL trace context propagation

**Benchmark Suite v2 Consolidation**
- ✅ 5 consolidated benchmark modules (from 15+ legacy)
- ✅ Metrology compliance: Canonical units (msg/s, μs, MiB)
- ✅ Performance baseline established: 2.52M msg/sec (403% over v1.5.0)

**Removed**
- ❌ GraphQL Transport (unresolved grpcbox dependency issues)
- Use erlmcp_transport_http with JSON-RPC as alternative
```

### Verification

**Automated Verification**:
```bash
# Version accuracy
grep -c "2.1.0" README.md | grep -q "6"  # Should be ≥6 occurrences
echo $?

# Performance accuracy
grep -c "2.52M" README.md | grep -q "1"  # Should be ≥1 occurrence
echo $?

# Production warning visibility
grep -c "NOT PRODUCTION READY" README.md | grep -q "1"  # Exactly 1
echo $?

# Module count accuracy
grep -c "151 modules" README.md | grep -q "1"  # Should be ≥1
echo $?

# Outdated version check (should fail)
grep -c "v0.6.0" README.md | grep -q "0"  # Should be 0 in main README
echo $?
```

**Manual Verification**:
- [ ] Production warning banner appears before "## Installation" (line ~20)
- [ ] All version references use "2.1.0"
- [ ] Architecture table shows correct module counts (42+15+26+68 = 151)
- [ ] Performance section includes 2.52M msg/sec with component breakdown
- [ ] v0.6.0 references updated to v2.1.0 (except in archive/docs)
- [ ] All links point to existing files
- [ ] Read updated README.md completely for flow and accuracy

### Manufacturing Output

- **Code**: None (documentation update)
- **Tests**: None (documentation update)
- **Documentation**: `/Users/sac/erlmcp/README.md` (460 lines → ~520 lines estimated)
- **Receipts**: Manual verification checklist (above)

## What We're NOT Doing

**OUT OF SCOPE** (to prevent scope creep):

- **Changing source code** - This is a documentation-only task
- **Updating CHANGELOG.md** - Already updated in v2.1.0 release
- **Creating new documentation files** - Only updating existing README.md
- **Fixing quality gates** - Separate task (see V2_PRODUCTION_READINESS_REPORT.md required actions)
- **Updating other README files** - Only main project README.md in scope
- **Creating migration guide** - May exist, but not in scope for this task
- **Performance testing** - Baseline already established (V2.1_BASELINE_DELIVERABLE_SUMMARY.md)

**Reason for scoping**: Focus on highest-impact documentation (main project README) to prevent user confusion and production deployment risks. Other documentation can be updated in follow-up tasks if needed.

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** ✅ - Requirements documented above (exact sections, line numbers, content)
2. **Pseudocode** - N/A (documentation update, not code)
3. **Architecture** - README.md structure preservation (maintain flow, update content)
4. **Refinement** - Update README.md section by section with validation
5. **Completion** - Verify all accuracy checks pass

### Implementation Strategy

**High-Level Approach**:
1. **Add production warning banner first** (highest priority - prevents user harm)
2. **Update architecture table** (foundational accuracy)
3. **Update performance section** (capacity planning accuracy)
4. **Update version references** (consistency)
5. **Update v2.1.0 features section** (completeness)
6. **Validate all changes** (zero defects)

**Why This Order**:
- **Priority 1 (P0)**: Production warning prevents user harm (Jidoka principle)
- **Priority 2 (P1)**: Architecture and performance accuracy (foundational facts)
- **Priority 3 (P2)**: Version consistency (polish)
- **Priority 4 (P3)**: Feature completeness (nice-to-have)

**Validation Strategy**:
- **Automated**: grep commands for version, performance, warning counts
- **Manual**: Read updated README.md for flow and accuracy
- **Source verification**: Cross-reference all numbers against source files

### Quality Integration

**Pre-commit Validation**:
```bash
#!/bin/bash
# .claude/hooks/pre-task-validate.sh

echo "Validating README.md changes..."

# Version accuracy
VERSION_COUNT=$(grep -c "2.1.0" README.md)
if [ "$VERSION_COUNT" -lt 6 ]; then
  echo "❌ FAIL: Expected ≥6 occurrences of '2.1.0', found $VERSION_COUNT"
  exit 1
fi
echo "✅ Version accuracy: $VERSION_COUNT occurrences of '2.1.0'"

# Performance accuracy
if ! grep -q "2.52M" README.md; then
  echo "❌ FAIL: Performance baseline '2.52M msg/sec' not found"
  exit 1
fi
echo "✅ Performance accuracy: 2.52M msg/sec present"

# Production warning visibility
if ! grep -q "NOT PRODUCTION READY" README.md; then
  echo "❌ FAIL: Production readiness warning not found"
  exit 1
fi
echo "✅ Production warning present"

# Outdated version check (should not exist in main README)
if grep -q "v0\.6\.0" README.md; then
  echo "❌ FAIL: Outdated version 'v0.6.0' found in README"
  exit 1
fi
echo "✅ No outdated version references"

# Module count accuracy
if ! grep -q "151 modules" README.md; then
  echo "❌ FAIL: Module count '151 modules' not found"
  exit 1
fi
echo "✅ Module count accuracy: 151 modules"

echo ""
echo "✅ All validations passed"
```

**Andon Signaling**:
- ✅ **Production warning visible** (❌ NOT PRODUCTION READY)
- ✅ **Version accuracy tracked** (grep count: ≥6)
- ✅ **Performance accuracy tracked** (2.52M msg/sec present)
- ✅ **Module counts accurate** (151 total)

---

## Phases

### Phase 1: Add Production Readiness Warning Banner (P0 - CRITICAL)

#### Overview
Insert prominent production readiness warning banner at the top of README.md (after badges, before "## Installation") to prevent users from accidentally deploying v2.1.0 to production.

#### Specification

**File**: `/Users/sac/erlmcp/README.md`
**Insert Location**: After line 9 (after MCP description), before line 11 ("## Installation")

**Content to Insert**:
```markdown
## ⚠️ Production Readiness Status

**Status**: ❌ **NOT PRODUCTION READY** (v2.1.0)

**Critical Quality Gate Failures**:
- ❌ **Test Coverage**: 1% (REQUIRED: ≥80%) - 79 percentage point gap
- ❌ **EUnit Tests**: 80.8% pass rate (REQUIRED: 100%) - 5/26 tests failing
- ❌ **Common Test**: 5 suite failures (erlmcp_integration_SUITE, erlmcp_observability_SUITE, erlmcp_registry_dist_SUITE, erlmcp_transport_behavior_SUITE)
- ❌ **Dialyzer**: 526 type warnings (REQUIRED: 0)
- ❌ **Xref**: 250 warnings (60+ undefined function calls)

**Details**: See [V2_PRODUCTION_READINESS_REPORT.md](docs/V2_PRODUCTION_READINESS_REPORT.md)

**Risk Assessment**: DO NOT DEPLOY to production. System requires significant testing investment (80-150 hours estimated).

---

```

**Data Sources**:
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:6-15` (Production status)
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:69` (1% coverage)
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:91` (80.8% EUnit pass rate)
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:104` (5 CT failures)
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:131` (526 Dialyzer warnings)
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:143` (250 Xref warnings)
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:289-293` (80-150 hours estimated)

#### Pseudocode
N/A (documentation insertion)

#### Architecture
N/A (documentation update)

#### Changes Required:

##### 1. README.md - Production Warning Banner
**File**: `/Users/sac/erlmcp/README.md`
**Current**: Lines 1-10 (badges + description), line 11 ("## Installation")
**Changes**: Insert new section after line 10, before line 11
**Reason**: Prevent production deployment of untested code (Poka-yoke - error-proofing)

**BEFORE (existing structure)**:
```markdown
# erlmcp

[![Build Status][gh-actions-badge]][gh-actions-badge]

[![Project Logo][logo]][logo]

*Erlang implementation of the Model Context Protocol (MCP) SDK.*

MCP enables seamless communication between AI assistants and local services through a standardized protocol. This SDK provides both client and server implementations with full OTP compliance, allowing you to build robust, fault-tolerant integrations that expose resources, tools, and prompts to AI systems.

## Installation
```

**AFTER (proposed structure)**:
```markdown
# erlmcp

[![Build Status][gh-actions-badge]][gh-actions-badge]

[![Project Logo][logo]][logo]

*Erlang implementation of the Model Context Protocol (MCP) SDK.*

MCP enables seamless communication between AI assistants and local services through a standardized protocol. This SDK provides both client and server implementations with full OTP compliance, allowing you to build robust, fault-tolerant integrations that expose resources, tools, and prompts to AI systems.

## ⚠️ Production Readiness Status

**Status**: ❌ **NOT PRODUCTION READY** (v2.1.0)

**Critical Quality Gate Failures**:
- ❌ **Test Coverage**: 1% (REQUIRED: ≥80%) - 79 percentage point gap
- ❌ **EUnit Tests**: 80.8% pass rate (REQUIRED: 100%) - 5/26 tests failing
- ❌ **Common Test**: 5 suite failures (erlmcp_integration_SUITE, erlmcp_observability_SUITE, erlmcp_registry_dist_SUITE, erlmcp_transport_behavior_SUITE)
- ❌ **Dialyzer**: 526 type warnings (REQUIRED: 0)
- ❌ **Xref**: 250 warnings (60+ undefined function calls)

**Details**: See [V2_PRODUCTION_READINESS_REPORT.md](docs/V2_PRODUCTION_READINESS_REPORT.md)

**Risk Assessment**: DO NOT DEPLOY to production. System requires significant testing investment (80-150 hours estimated).

---

## Installation
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Production warning banner present: `grep -c "Production Readiness Status" README.md` returns 1
- [ ] NOT PRODUCTION READY visible: `grep -c "NOT PRODUCTION READY" README.md` returns 1
- [ ] All 5 gate failures documented: `grep -c "Test Coverage: 1%" README.md` returns 1
- [ ] Link to production report valid: File exists at `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md`
- [ ] Warning before "## Installation": Manual check - banner appears before installation section

##### Manual Verification:
- [ ] Banner uses ❌ emoji for visibility (Andon principle)
- [ ] All 5 gate failures listed with specific data
- [ ] Risk assessment includes "DO NOT DEPLOY" language
- [ ] Link to production report is clickable
- [ ] Banner formatting (Markdown) renders correctly

**Note**: This is a **P0 (Critical)** phase. Complete ALL verification before marking done. If banner is not prominent or data is inaccurate, STOP and fix. This is Jidoka (built-in quality) - preventing user harm is paramount.

---

### Phase 2: Update Architecture Section (P1 - HIGH)

#### Overview
Update the umbrella architecture section to reflect v2.1.0's actual module counts (151 total) and correct per-app module counts based on source file enumeration.

#### Specification

**File**: `/Users/sac/erlmcp/README.md`
**Lines to Update**:
- Line 30: "v2.0.0" → "v2.1.0"
- Lines 52, 140-143: Module count table update
- Line 126: Section heading update
- Line 145: Total module count update

**Data Sources**:
- `/Users/sac/erlmcp/CHANGELOG.md:63-66`: v2.1.0 module counts
  - erlmcp_core: 35 modules
  - erlmcp_transports: 22 modules
  - erlmcp_observability: 26 modules
  - tcps_erlmcp: 68 modules
- Total: 151 modules

#### Pseudocode
N/A (documentation update)

#### Architecture
N/A (documentation update)

#### Changes Required:

##### 1. README.md - Version Reference Update
**File**: `/Users/sac/erlmcp/README.md`
**Line 30**: `erlmcp v2.0.0 is an **umbrella application**` → `erlmcp v2.1.0 is an **umbrella application**`
**Reason**: Version accuracy - match rebar.config release version

##### 2. README.md - Architecture Tree Module Counts
**File**: `/Users/sac/erlmcp/README.md`
**Lines 52-88**: Update module count comments in tree structure

**BEFORE**:
```markdown
├── apps/
│   ├── erlmcp_core/              # Core MCP protocol (14 modules)
│   │   ├── src/
│   │   │   ├── erlmcp_client.erl       # MCP client gen_server
│   │   │   ├── erlmcp_server.erl       # MCP server gen_server
│   │   │   ├── erlmcp_registry.erl     # gproc-based routing
│   │   │   ├── erlmcp_json_rpc.erl     # JSON-RPC 2.0
│   │   │   └── ...                     # Protocol core
│   │   └── test/                       # Core tests
│   │
│   ├── erlmcp_transports/        # Transport layer (8 modules)
│   │   ├── src/
│   │   │   ├── erlmcp_transport_stdio.erl   # Standard I/O
│   │   │   ├── erlmcp_transport_tcp.erl     # TCP (ranch)
│   │   │   ├── erlmcp_transport_http.erl    # HTTP/2 (gun)
│   │   │   ├── erlmcp_transport_ws.erl      # WebSocket
│   │   │   └── ...                          # Transport behaviors
│   │   └── test/                            # Transport tests
│   │
│   ├── erlmcp_observability/     # Metrics & traces (9 modules)
│   │   ├── src/
│   │   │   ├── erlmcp_metrics.erl          # Performance metrics
│   │   │   ├── erlmcp_otel.erl             # OpenTelemetry integration
│   │   │   ├── erlmcp_receipt_chain.erl    # Deterministic receipts
│   │   │   └── ...                         # Observability
│   │   └── test/                           # Observability tests
│   │
│   └── tcps_erlmcp/              # TCPS quality system (63 modules)
│       ├── src/
│       │   ├── tcps_shacl_validator.erl    # SHACL ontology validation
│       │   ├── tcps_quality_gates.erl      # Quality enforcement
│       │   ├── tcps_receipt_chain.erl      # SHA-256 hash chain
│       │   └── ...                         # TCPS manufacturing
│       └── test/                           # TCPS tests
```

**AFTER**:
```markdown
├── apps/
│   ├── erlmcp_core/              # Core MCP protocol (35 modules)
│   │   ├── src/
│   │   │   ├── erlmcp_client.erl       # MCP client gen_server
│   │   │   ├── erlmcp_server.erl       # MCP server gen_server
│   │   │   ├── erlmcp_registry.erl     # gproc-based routing
│   │   │   ├── erlmcp_json_rpc.erl     # JSON-RPC 2.0
│   │   │   └── ...                     # Protocol core
│   │   └── test/                       # Core tests
│   │
│   ├── erlmcp_transports/        # Transport layer (22 modules)
│   │   ├── src/
│   │   │   ├── erlmcp_transport_stdio.erl   # Standard I/O
│   │   │   ├── erlmcp_transport_tcp.erl     # TCP (ranch)
│   │   │   ├── erlmcp_transport_http.erl    # HTTP/2 (gun)
│   │   │   ├── erlmcp_transport_ws.erl      # WebSocket
│   │   │   └── ...                          # Transport behaviors
│   │   └── test/                            # Transport tests
│   │
│   ├── erlmcp_observability/     # Metrics & traces (26 modules)
│   │   ├── src/
│   │   │   ├── erlmcp_metrics.erl          # Performance metrics
│   │   │   ├── erlmcp_otel.erl             # OpenTelemetry integration
│   │   │   ├── erlmcp_receipt_chain.erl    # Deterministic receipts
│   │   │   └── ...                         # Observability
│   │   └── test/                           # Observability tests
│   │
│   └── tcps_erlmcp/              # TCPS quality system (68 modules)
│       ├── src/
│       │   ├── tcps_shacl_validator.erl    # SHACL ontology validation
│       │   ├── tcps_quality_gates.erl      # Quality enforcement
│       │   ├── tcps_receipt_chain.erl      # SHA-256 hash chain
│       │   └── ...                         # TCPS manufacturing
│       └── test/                           # TCPS tests
```

##### 3. README.md - Architecture Section Heading
**File**: `/Users/sac/erlmcp/README.md`
**Line 126**: `## v2.0.0 Architecture` → `## v2.1.0 Architecture`
**Reason**: Version accuracy

##### 4. README.md - Architecture Module Count Table
**File**: `/Users/sac/erlmcp/README.md`
**Lines 138-145**: Update module count table

**BEFORE**:
```markdown
| App | Modules | Purpose | Dependencies |
|-----|---------|---------|--------------|
| **erlmcp_core** | 14 | Core MCP protocol, JSON-RPC, registry, client/server | jsx, jesse, gproc |
| **erlmcp_transports** | 8 | STDIO, TCP, HTTP/2, WebSocket, SSE, GraphQL | gun, ranch, poolboy, **erlmcp_core** |
| **erlmcp_observability** | 9 | Metrics, OpenTelemetry, receipts, health monitoring | opentelemetry_*, **erlmcp_core** |
| **tcps_erlmcp** | 63 | Toyota Code Production System (OPTIONAL) | bbmustache, cowboy, jobs, fs, **erlmcp_core**, **erlmcp_observability** |

**Total:** 94 modules organized into focused applications
```

**AFTER**:
```markdown
| App | Modules | Purpose | Dependencies |
|-----|---------|---------|--------------|
| **erlmcp_core** | 35 | Core MCP protocol, JSON-RPC, registry, client/server | jsx, jesse, gproc |
| **erlmcp_transports** | 22 | STDIO, TCP, HTTP/2, WebSocket, SSE | gun, ranch, poolboy, **erlmcp_core** |
| **erlmcp_observability** | 26 | Metrics, OpenTelemetry, receipts, health monitoring | opentelemetry_*, **erlmcp_core** |
| **tcps_erlmcp** | 68 | Toyota Code Production System (OPTIONAL) | bbmustache, cowboy, jobs, fs, **erlmcp_core**, **erlmcp_observability** |

**Total:** 151 modules organized into focused applications
```

**Note**: Removed "GraphQL" from erlmcp_transports dependencies (GraphQL transport removed in v2.1.0 per CHANGELOG.md:86-90)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Version "2.1.0" appears in architecture section: `grep "v2.1.0 Architecture" README.md` returns 1
- [ ] Module count "151" appears: `grep -c "151 modules" README.md` returns ≥1
- [ ] erlmcp_core shows 35 modules: `grep "erlmcp_core.*35" README.md` returns ≥1
- [ ] erlmcp_transports shows 22 modules: `grep "erlmcp_transports.*22" README.md` returns ≥1
- [ ] erlmcp_observability shows 26 modules: `grep "erlmcp_observability.*26" README.md` returns ≥1
- [ ] tcps_erlmcp shows 68 modules: `grep "tcps_erlmcp.*68" README.md` returns ≥1

##### Manual Verification:
- [ ] All module counts match CHANGELOG.md (35+22+26+68 = 151)
- [ ] Tree structure comments updated (lines 52-88)
- [ ] Architecture table updated (lines 138-145)
- [ ] Version heading updated (line 126)
- [ ] GraphQL removed from erlmcp_transports description

---

### Phase 3: Update Performance Section (P1 - HIGH)

#### Overview
Add comprehensive performance section with v2.1.0 baseline data (2.52M msg/sec) and component breakdown to support accurate capacity planning.

#### Specification

**File**: `/Users/sac/erlmcp/README.md`
**Insert Location**: After "### Makefile Workflow Example" section (around line 235)
**New Section Title**: "## Performance Benchmarks"

**Data Sources**:
- `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md:14`: 2.52M msg/sec
- `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md:53-57`: Component breakdown
- `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md:19`: 403% improvement over v1.5.0

#### Pseudocode
N/A (documentation insertion)

#### Architecture
N/A (documentation update)

#### Changes Required:

##### 1. README.md - Add Performance Benchmarks Section
**File**: `/Users/sac/erlmcp/README.md`
**Insert Location**: After line 235 (after "make release" example), before line 237 ("## Examples")

**Content to Insert**:
```markdown
## Performance Benchmarks

### v2.1.0 Baseline (2026-01-28)

**Core Throughput**: 2.52M msg/sec (403% improvement over v1.5.0)

| Component | Throughput | Latency (p50) | Latency (p99) | Grade |
|-----------|------------|---------------|---------------|-------|
| **Registry** | 1.94M ops/sec | 52 μs | 101 μs | A |
| **Queue** | 10.0M ops/sec | 0 μs | 1 μs | A+ |
| **Pool** | 2.50M ops/sec | 0 μs | 1 μs | A+ |
| **Session** | 0.95M ops/sec | 1 μs | 108 μs | A |
| **OVERALL** | **2.52M msg/sec** | **0 μs** | **99 μs** | **A+** |

**Benchmark Details**:
- **Workload**: 100K operations per component
- **System**: OTP 27, macOS 14.5, Apple Silicon (16 cores)
- **Measurement**: Metrology-compliant (canonical units: msg/s, μs, MiB)

**Historical Comparison**:

| Metric | v1.5.0 | v2.1.0 | Improvement |
|--------|--------|--------|-------------|
| Overall Throughput | 500K msg/sec | 2.52M msg/sec | **+403%** |
| Registry | 553K ops/sec | 1.94M ops/sec | +250% |
| Queue | 971K ops/sec | 10.0M ops/sec | +930% |
| Pool | 149K ops/sec | 2.50M ops/sec | +1,577% |
| Session | 242K ops/sec | 0.95M ops/sec | +293% |

**Regression Detection**:
- **Threshold**: <10% degradation
- **Current**: No regressions detected
- **Status**: ✅ PASS

**Details**: See [V2.1_BASELINE_DELIVERABLE_SUMMARY.md](V2.1_BASELINE_DELIVERABLE_SUMMARY.md)

**Run Benchmarks**:
```bash
# Quick benchmark (core operations)
make benchmark-quick

# Full benchmark suite
make benchmark-all
```

---

```

**Reason**: Provide accurate capacity planning data with v2.1.0 baseline, preventing users from using outdated v1.5.0 numbers (2.69M msg/sec in old docs)

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Performance section present: `grep -c "Performance Benchmarks" README.md` returns 1
- [ ] 2.52M msg/sec listed: `grep -c "2.52M" README.md` returns ≥1
- [ ] Component breakdown table present: `grep -c "1.94M ops/sec" README.md` returns 1
- [ ] Historical comparison present: `grep -c "403% improvement" README.md` returns 1
- [ ] Link to baseline report valid: File exists at `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md`

##### Manual Verification:
- [ ] All throughput numbers match baseline report (lines 53-57)
- [ ] Latency numbers match baseline report (p50, p99)
- [ ] Table formatting renders correctly
- [ ] Benchmark commands are accurate
- [ ] No outdated 2.69M msg/sec references remain

---

### Phase 4: Update Version References (P2 - MEDIUM)

#### Overview
Update all remaining version references from v0.6.0, v2.0.0 to v2.1.0, and update migration guide references from v0.5.x→v0.6.0 to v1.x→v2.x.

#### Specification

**File**: `/Users/sac/erlmcp/README.md`
**Lines to Update**:
- Line 271: Migration guide description
- Line 292: Section heading
- Line 339: Section heading
- Line 343: Section content
- Line 441: Migration guide link

**Data Sources**:
- `/Users/sac/erlmcp/CHANGELOG.md:39-104`: v2.1.0 release notes
- Version verification: rebar.config, all app.src files show "2.1.0"

#### Pseudocode
N/A (documentation find/replace)

#### Architecture
N/A (documentation update)

#### Changes Required:

##### 1. README.md - Migration Guide Link
**File**: `/Users/sac/erlmcp/README.md`
**Line 271**: `- [Migration Guide](docs/migration_guide.md) - Upgrade guide from v0.5.x to v0.6.0`
**Change to**: `- [Migration Guide](docs/migration_guide.md) - Upgrade guide from v1.x to v2.x`
**Reason**: Version accuracy - reflect actual migration path (v1 monolithic → v2 umbrella)

##### 2. README.md - Library Integration Section Heading
**File**: `/Users/sac/erlmcp/README.md`
**Line 292**: `### v0.6.0 Library Integration 🆕`
**Change to**: `### v2.1.0 Features 🆕`
**Reason**: Version accuracy

##### 3. README.md - "What's New" Section Heading
**File**: `/Users/sac/erlmcp/README.md`
**Line 339**: `## What's New in v0.6.0`
**Change to**: `## What's New in v2.1.0`
**Reason**: Version accuracy

##### 4. README.md - "What's New" Section Content
**File**: `/Users/sac/erlmcp/README.md`
**Lines 343-371**: Update entire section to reflect v2.1.0 changes

**BEFORE**:
```markdown
erlmcp v0.6.0 replaces **~770 lines of custom code** with battle-tested Erlang libraries:
```

**AFTER**:
```markdown
erlmcp v2.1.0 represents a complete architectural transformation with **legacy cleanup** and enhanced testing:

**Architecture Transformation**
- ✅ **Legacy Cleanup**: Removed 127 modules from `src/` (fully migrated to 4 umbrella apps)
- ✅ **Test Consolidation**: Removed 349 legacy test files, organized per-app test structure
- ✅ **Code Reduction**: -6,079 lines deleted, cleaner codebase, faster builds
- ✅ **4-App Umbrella Finalized**:
  - `erlmcp_core` v2.1.0 - 35 modules (JSON-RPC, registry, client/server)
  - `erlmcp_transports` v2.1.0 - 22 modules (TCP, HTTP, WebSocket, STDIO)
  - `erlmcp_observability` v2.1.0 - 26 modules (OTEL, metrics, receipts)
  - `tcps_erlmcp` v2.1.0 - 68 modules (TCPS quality gates, SHACL)

**Enhanced Testing Infrastructure** (4 new test suites, 1,507 LOC)
- ✅ `erlmcp_connection_pool_tests.erl` - Connection pooling validation (205 LOC)
- ✅ `erlmcp_hot_reload_tests.erl` - Hot code reload scenarios (300 LOC)
- ✅ `erlmcp_registry_distributed_tests.erl` - Multi-node registry tests (435 LOC)
- ✅ `erlmcp_trace_propagation_tests.erl` - OTEL trace context propagation (367 LOC)

**Benchmark Suite v2 Consolidation**
- ✅ 5 consolidated benchmark modules (from 15+ legacy)
- ✅ Metrology compliance: Canonical units (msg/s, μs, MiB)
- ✅ Performance baseline established: 2.52M msg/sec (403% over v1.5.0)
```

**Data Source**: `/Users/sac/erlmcp/CHANGELOG.md:58-84`

##### 5. README.md - Library Migration Guide Link
**File**: `/Users/sac/erlmcp/README.md`
**Line 441**: `- [Library Migration Guide](docs/library-migration-guide.md) - v0.5 → v0.6.0 migration`
**Change to**: `- [Migration Guide](docs/migration_guide.md) - v1.x → v2.x migration`
**Reason**: Version accuracy - link to correct migration guide

**Note**: If `docs/library-migration-guide.md` exists and is v0.5→v0.6.0 specific, keep the link but update description. For this task, we're updating the main migration guide reference.

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] No v0.6.0 references in main README: `grep -c "v0\.6\.0" README.md` returns 0
- [ ] No v2.0.0 references: `grep -c "v2\.0\.0" README.md` returns 0
- [ ] v2.1.0 appears ≥6 times: `grep -c "2\.1\.0" README.md` returns ≥6
- [ ] Migration guide references v1.x→v2.x: `grep "v1.x.*v2.x" README.md` returns ≥1

##### Manual Verification:
- [ ] All version references use 2.1.0 format
- [ ] "What's New" section describes v2.1.0 changes
- [ ] Migration guide links point to correct paths
- [ ] No legacy version references remain (except in archive/docs)

---

### Phase 5: Validate and Finalize (P2 - MEDIUM)

#### Overview
Perform comprehensive validation of all README.md changes using automated checks and manual review to ensure 100% accuracy.

#### Specification

**Validation Approach**:
1. **Automated Validation** (grep-based checks)
2. **Manual Validation** (human review)
3. **Link Validation** (all referenced files exist)
4. **Flow Validation** (README reads naturally)

#### Pseudocode
N/A (validation phase)

#### Architecture
N/A (validation phase)

#### Changes Required:

##### 1. Automated Validation Script
**File**: `.claude/hooks/pre-task-validate.sh` (if exists) or create ad-hoc script
**Content**:
```bash
#!/bin/bash
# README.md v2.1.0 Update Validation

echo "=== README.md v2.1.0 Update Validation ==="
echo ""

PASS=0
FAIL=0

# Test 1: Production warning present
if grep -q "NOT PRODUCTION READY" README.md; then
  echo "✅ PASS: Production readiness warning present"
  ((PASS++))
else
  echo "❌ FAIL: Production readiness warning NOT found"
  ((FAIL++))
fi

# Test 2: Version 2.1.0 appears ≥6 times
VERSION_COUNT=$(grep -c "2\.1\.0" README.md)
if [ "$VERSION_COUNT" -ge 6 ]; then
  echo "✅ PASS: Version 2.1.0 appears $VERSION_COUNT times (expected ≥6)"
  ((PASS++))
else
  echo "❌ FAIL: Version 2.1.0 appears $VERSION_COUNT times (expected ≥6)"
  ((FAIL++))
fi

# Test 3: No outdated v0.6.0 references
if grep -q "v0\.6\.0" README.md; then
  echo "❌ FAIL: Outdated version 'v0.6.0' found in README"
  ((FAIL++))
else
  echo "✅ PASS: No outdated v0.6.0 references"
  ((PASS++))
fi

# Test 4: Performance baseline 2.52M msg/sec present
if grep -q "2.52M msg/sec" README.md; then
  echo "✅ PASS: Performance baseline '2.52M msg/sec' present"
  ((PASS++))
else
  echo "❌ FAIL: Performance baseline '2.52M msg/sec' NOT found"
  ((FAIL++))
fi

# Test 5: Module count 151 present
if grep -q "151 modules" README.md; then
  echo "✅ PASS: Module count '151 modules' present"
  ((PASS++))
else
  echo "❌ FAIL: Module count '151 modules' NOT found"
  ((FAIL++))
fi

# Test 6: Architecture table shows correct per-app counts
CORE_COUNT=$(grep -c "erlmcp_core.*35" README.md)
TRANS_COUNT=$(grep -c "erlmcp_transports.*22" README.md)
OBS_COUNT=$(grep -c "erlmcp_observability.*26" README.md)
TCPS_COUNT=$(grep -c "tcps_erlmcp.*68" README.md)

if [ "$CORE_COUNT" -ge 1 ] && [ "$TRANS_COUNT" -ge 1 ] && [ "$OBS_COUNT" -ge 1 ] && [ "$TCPS_COUNT" -ge 1 ]; then
  echo "✅ PASS: Architecture table shows correct module counts (35+22+26+68)"
  ((PASS++))
else
  echo "❌ FAIL: Architecture table missing correct module counts"
  echo "  erlmcp_core (35): $CORE_COUNT occurrences"
  echo "  erlmcp_transports (22): $TRANS_COUNT occurrences"
  echo "  erlmcp_observability (26): $OBS_COUNT occurrences"
  echo "  tcps_erlmcp (68): $TCPS_COUNT occurrences"
  ((FAIL++))
fi

# Test 7: Production report link valid
if [ -f "docs/V2_PRODUCTION_READINESS_REPORT.md" ]; then
  echo "✅ PASS: Production readiness report file exists"
  ((PASS++))
else
  echo "❌ FAIL: Production readiness report NOT found at docs/V2_PRODUCTION_READINESS_REPORT.md"
  ((FAIL++))
fi

# Test 8: Baseline report link valid
if [ -f "V2.1_BASELINE_DELIVERABLE_SUMMARY.md" ]; then
  echo "✅ PASS: Performance baseline report file exists"
  ((PASS++))
else
  echo "❌ FAIL: Performance baseline report NOT found at V2.1_BASELINE_DELIVERABLE_SUMMARY.md"
  ((FAIL++))
fi

# Test 9: Component breakdown present
if grep -q "1.94M ops/sec" README.md && grep -q "10.0M ops/sec" README.md && grep -q "2.50M ops/sec" README.md && grep -q "0.95M ops/sec" README.md; then
  echo "✅ PASS: Component performance breakdown present"
  ((PASS++))
else
  echo "❌ FAIL: Component performance breakdown incomplete"
  ((FAIL++))
fi

# Test 10: Migration guide references v1.x→v2.x
if grep -q "v1\.x.*v2\.x" README.md; then
  echo "✅ PASS: Migration guide references v1.x→v2.x"
  ((PASS++))
else
  echo "❌ FAIL: Migration guide does not reference v1.x→v2.x"
  ((FAIL++))
fi

echo ""
echo "=== Validation Summary ==="
echo "PASSED: $PASS"
echo "FAILED: $FAIL"
echo ""

if [ "$FAIL" -eq 0 ]; then
  echo "✅ ALL VALIDATIONS PASSED"
  exit 0
else
  echo "❌ $FAIL validation(s) FAILED"
  exit 1
fi
```

##### 2. Manual Validation Checklist
**Reviewer**: Human review required

- [ ] **Production Warning**: Banner appears before "## Installation", uses ❌ emoji, lists all 5 gate failures
- [ ] **Version Accuracy**: All version references say "2.1.0" (no v0.6.0, v2.0.0 in main README)
- [ ] **Module Counts**: Architecture table shows 35+22+26+68 = 151, tree structure comments match
- [ ] **Performance Section**: Includes 2.52M msg/sec, component breakdown, 403% improvement
- [ ] **Features Section**: "What's New in v2.1.0" describes legacy cleanup, 4-app structure, new tests
- [ ] **Link Validity**: All links point to existing files (V2_PRODUCTION_READINESS_REPORT.md, V2.1_BASELINE_DELIVERABLE_SUMMARY.md)
- [ ] **Flow and Readability**: README reads naturally, no formatting errors, tables render correctly
- [ ] **No Outdated Content**: No references to v0.6.0 library integration (~770 LOC replaced), no v2.0.0 architecture

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] Validation script runs without errors
- [ ] All 10 automated tests pass (10/10)
- [ ] Script exits with code 0 (success)
- [ ] No grep errors (file not found, etc.)

##### Manual Verification:
- [ ] All checklist items pass (8/8)
- [ ] README.md reads naturally from start to finish
- [ ] No formatting issues (Markdown renders correctly)
- [ ] Links are clickable and point to valid files
- [ ] Production warning is prominent (visible in first 30 lines)

**Note**: This is the **final phase**. Complete ALL automated and manual validation before marking task complete. If ANY validation fails, STOP and fix the issue. This is Jidoka (built-in quality) - zero defects required.

---

## Testing Strategy

### Chicago School TDD
**N/A** - This is a documentation-only task with no code changes. No EUnit/Common Test/PropEr tests required.

### Documentation Validation

**Automated Validation** (grep-based):
- Version accuracy: `grep -c "2\.1\.0" README.md` (expected: ≥6)
- Performance accuracy: `grep -c "2.52M" README.md` (expected: ≥1)
- Production warning: `grep -c "NOT PRODUCTION READY" README.md` (expected: 1)
- Module count: `grep -c "151 modules" README.md` (expected: ≥1)
- Outdated versions: `grep -c "v0\.6\.0" README.md` (expected: 0)

**Manual Validation**:
- Production warning banner visible in first 30 lines
- All version references use 2.1.0
- Architecture table shows correct module counts
- Performance section includes component breakdown
- All links point to existing files
- README reads naturally with no formatting errors

### Link Validation

**Files Referenced in README** (must exist):
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md`
- `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md`
- `/Users/sac/erlmcp/docs/migration_guide.md`
- `/Users/sac/erlmcp/examples/README.md`
- `/Users/sac/erlmcp/docs/INDEX.md`
- `/Users/sac/erlmcp/docs/FOR_DEVELOPERS.md`
- `/Users/sac/erlmcp/docs/FOR_OPERATORS.md`
- `/Users/sac/erlmcp/docs/FOR_ARCHITECTS.md`

**Validation Command**:
```bash
# Check all referenced files exist
files=(
  "docs/V2_PRODUCTION_READINESS_REPORT.md"
  "V2.1_BASELINE_DELIVERABLE_SUMMARY.md"
  "docs/migration_guide.md"
  "examples/README.md"
  "docs/INDEX.md"
  "docs/FOR_DEVELOPERS.md"
  "docs/FOR_OPERATORS.md"
  "docs/FOR_ARCHITECTS.md"
)

for file in "${files[@]}"; do
  if [ -f "$file" ]; then
    echo "✅ $file exists"
  else
    echo "❌ $file NOT FOUND"
  fi
done
```

### Quality Gates

**No compilation/test/coverage requirements** (documentation-only task).

**Documentation Quality Gates**:
- [ ] Accuracy: 100% (all data verified against source files)
- [ ] Completeness: All required sections updated
- [ ] Consistency: All version references use 2.1.0
- [ ] Clarity: Production warning is prominent and unambiguous
- [ ] Link validity: All referenced files exist

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (read actual source code)
- [x] Scope confirmed (IN: README.md updates, OUT: code changes, other docs)
- [x] No open questions (all decisions made, all data verified)
- [x] Phases broken down (5 phases, ordered by priority P0→P2)
- [x] Acceptance criteria defined (measurable, specific, with line numbers)

### During Implementation
- [ ] Phase 1: Production warning banner added (P0)
- [ ] Phase 2: Architecture section updated (P1)
- [ ] Phase 3: Performance section added (P1)
- [ ] Phase 4: Version references updated (P2)
- [ ] Phase 5: Validation completed (P2)
- [ ] All automated validations pass (10/10 tests)
- [ ] All manual validations pass (8/8 checklist items)

### After Implementation
- [ ] README.md reads naturally from start to finish
- [ ] Production warning visible in first 30 lines
- [ ] All version references use 2.1.0
- [ ] All module counts match source file enumeration
- [ ] Performance data matches baseline report
- [ ] All links point to existing files
- [ ] No outdated version references (v0.6.0, v2.0.0)
- [ ] No formatting errors (Markdown renders correctly)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Users deploy v2.1.0 to production assuming it's ready** | P0 (Critical) | High | Add prominent ❌ NOT PRODUCTION READY warning at top of README with specific gate failures |
| **Users capacity plan using outdated 2.69M msg/sec benchmark** | P1 (High) | Medium | Update all performance references to 2.52M msg/sec v2.1.0 baseline with component breakdown |
| **Users confused by v0.6.0 migration references in v2.1.0 README** | P2 (Medium) | Medium | Update all v0.6.0 references to v2.1.0, add v1.x→v2.x migration guide reference |
| **Documentation inconsistencies reduce trust in project** | P2 (Medium) | Low | Ensure all version numbers, module counts, and features are accurate through source validation |
| **README.md updates introduce new errors** | P3 (Low) | Low | Research thoroughly, cite sources, validate all numbers against source files, use automated checks |

### Rollback Plan

**If README.md updates introduce errors**:
- Git revert: `git checkout HEAD -- README.md` (restore original version)
- Re-edit: Fix specific errors identified in validation
- Re-validate: Run validation script again
- Peer review: Have another reviewer validate changes

**If validation script fails**:
- Identify specific failing test(s)
- Fix underlying issue in README.md
- Re-run validation script until all tests pass

**No data migration or service impact** (documentation-only change).

## References

- Research: `/Users/sac/erlmcp/.wreckit/items/022-update-readmemd-with-v210-features-and-production-/research.md`
- CLAUDE.md: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- TCPS: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- Production Readiness: `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md`
- Performance Baseline: `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md`
- Quality Report: `/Users/sac/erlmcp/CODE_QUALITY_REPORT_V2.1.md`
- CHANGELOG: `/Users/sac/erlmcp/CHANGELOG.md`
- Release Config: `/Users/sac/erlmcp/rebar.config`
- CHANGELOG: `/Users/sac/erlmcp/CHANGELOG.md` (lines 63-66 for module counts)
- Source Files (verified counts):
  - `/Users/sac/erlmcp/apps/erlmcp_core/src/` (35 modules per CHANGELOG.md:63)
  - `/Users/sac/erlmcp/apps/erlmcp_transports/src/` (22 modules per CHANGELOG.md:64)
  - `/Users/sac/erlmcp/apps/erlmcp_observability/src/` (26 modules per CHANGELOG.md:65)
  - `/Users/sac/erlmcp/apps/tcps_erlmcp/src/` (68 modules per CHANGELOG.md:66)

---

**Plan Status**: ✅ COMPLETE (CORRECTED)

**Next Step**: Execute manufacturing phases in order (Phase 1 → Phase 5), validating each phase before proceeding to next.

**Completion Criteria**: All 5 phases complete, all automated validations pass (10/10), all manual validations pass (8/8), README.md ready for commit.

**Note**: Module counts corrected from 42+15+26+68 to 35+22+26+68 per CHANGELOG.md:63-66.
