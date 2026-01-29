# Research: Update README with v2.1.0 Changes

**Date**: 2026-01-29
**Item**: 046-update-readme-with-v210-changes
**Section**: docs
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
README still references v1.x features. Needs update for v2.1.0 architecture and current production readiness status.

**Motivation:** Documentation must reflect current architecture and production readiness status to set proper user expectations.

**Success criteria:**
- README.md updated
- v2.1.0 features documented
- Production readiness warnings added
- Performance benchmarks updated
- Test coverage status documented

**Technical constraints:**
- Update Architecture Section for 4-app umbrella structure
- Update Quick Start Section for rebar3 (vs rebar)
- Add production readiness warning
- Update Performance Section with v2.1.0 benchmarks (2.52M msg/sec)
- Document test coverage status

**Signals:** priority: low, urgency: P3 - NICE TO HAVE

### Quality Gate Status
- **Gate Type**: Documentation Accuracy
- **Current State**: README.md references v1.x monolithic architecture, outdated performance numbers (2.69M msg/sec from v0.7.0), missing v2.1.0 features
- **Target State**: README.md reflects v2.1.0 umbrella architecture, accurate benchmarks (2.52M msg/sec), production readiness warning, test coverage (88.5%)
- **Gap**: Documentation drift - 2 major versions behind (v1.x → v2.1.0), performance numbers off by 6.8% (2.69M vs 2.52M msg/sec), missing critical production readiness information

## Summary

**Manufacturing Objective**: Update README.md to accurately reflect erlmcp v2.1.0's current state, transitioning from monolithic v1.x architecture to the 4-app umbrella structure, correcting outdated performance benchmarks, and adding critical production readiness warnings to prevent user misalignment.

**Technical Approach**:
1. **Architecture Section**: Replace monolithic `src/` structure references with umbrella `apps/` structure (erlmcp_core: 35 modules, erlmcp_transports: 22 modules, erlmcp_observability: 26 modules, tcps_erlmcp: 68 modules = 151 total modules)
2. **Quick Start Section**: Update build commands from legacy `rebar` to `rebar3` with umbrella-aware targets
3. **Performance Section**: Correct baseline from v1's 2.69M msg/sec to v2.1.0's 2.52M msg/sec (6.3% regression, within 10% threshold)
4. **Production Readiness**: Add explicit warning that v2.1.0 is **NOT production-ready** due to failing quality gates (per item 022 analysis)
5. **Test Coverage**: Document current 88.5% test coverage and 91% type coverage (exceeds 80% target)

**TCPS Justification**:
- **Jidoka**: Documentation accuracy is a quality gate - outdated docs cause user confusion and support burden
- **Poka-yoke**: Adding production readiness warnings prevents users from deploying non-production code
- **Kaizen**: Continuous improvement of documentation ensures alignment with codebase evolution
- **Andon**: Visual warnings (production readiness status) make problems immediately visible

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/README.md` (460 lines) - Primary documentation file
  - `/Users/sac/erlmcp/CHANGELOG.md` (500 lines) - Contains v2.1.0 release details
  - `/Users/sac/erlmcp/RELEASE_NOTES_v2.1.0.md` (150+ lines) - Comprehensive v2.1.0 release notes
  - `/Users/sac/erlmcp/BENCHMARK_RESULTS_V2.md` (100+ lines) - v2.0.0 benchmark results
  - `/Users/sac/erlmcp/bench/results/v2_benchmark_report_20260128_115411.md` (270 lines) - Detailed v2.0.0 benchmark report
  - `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md` - v2.1.0 baseline showing 2.52M msg/sec
  - `/Users/sac/erlmcp/FINAL_DELIVERABLES_INDEX.md` - Test coverage: 88.5% code, 91% type
  - `/Users/sac/erlmcp/apps/*/src/*.app.src` - Version confirmation: all apps at 2.1.0

- **Patterns**:
  - **Umbrella Architecture**: 4 OTP applications (erlmcp_core, erlmcp_transports, erlmcp_observability, tcps_erlmcp)
  - **Build System**: rebar3 with umbrella project structure (rebar.config:220 confirms release version "2.1.0")
  - **Test Coverage**: 88.5% code coverage, 91% type coverage (exceeds 80% target per FINAL_DELIVERABLES_INDEX.md:228)
  - **Performance Baseline**: 2.52M msg/sec (V2.1_BASELINE_DELIVERABLE_SUMMARY.md:14,57)

- **Tests**:
  - 11 comprehensive test suites (per RELEASE_NOTES_v2.1.0.md:16)
  - 500+ tests passing (per FINAL_DELIVERABLES_INDEX.md:365)
  - 88.5% average test coverage (exceeds 80% target)
  - EUnit, Common Test, and PropEr test frameworks

- **Quality**:
  - **Compilation**: ✅ 151 modules compiled, 0 errors (RELEASE_NOTES_v2.1.0.md:118)
  - **Type Safety**: ✅ 91% type coverage (FINAL_DELIVERABLES_INDEX.md:228)
  - **Test Coverage**: ✅ 88.5% code coverage (FINAL_DELIVERABLES_INDEX.md:82)
  - **Performance**: ✅ 2.52M msg/sec throughput (V2.1_BASELINE_DELIVERABLE_SUMMARY.md:14)
  - **Production Readiness**: ⚠️ **NOT PRODUCTION-READY** - per item 022 analysis, v2.1.0 has failing quality gates

### Key Files
- `README.md:1-460` - Primary documentation (needs comprehensive update)
  - Line 126-179: v2.0.0 Architecture section (already partially updated for umbrella, but needs v2.1.0 refinements)
  - Line 30-88: Umbrella structure section (needs v2.1.0 module count updates)
  - Performance section: References outdated v0.7.0 baseline (2.69M msg/sec), needs v2.1.0 numbers (2.52M msg/sec)
  - Missing: Production readiness warning
  - Missing: Test coverage documentation (88.5%)

- `rebar.config:220` - Release version: "2.1.0" (source of truth for version)
- `apps/erlmcp_core/src/erlmcp_core.app.src:3` - Version: 2.1.0
- `apps/erlmcp_transports/src/erlmcp_transports.app.src:3` - Version: 2.1.0
- `apps/erlmcp_observability/src/erlmcp_observability.app.src:3` - Version: 2.1.0
- `apps/tcps_erlmcp/src/tcps_erlmcp.app.src:3` - Version: 2.1.0

- `CHANGELOG.md:39-140` - v2.1.0 release details (source for feature list)
- `RELEASE_NOTES_v2.1.0.md:1-150` - Comprehensive release notes (source for architecture changes)
- `V2.1_BASELINE_DELIVERABLE_SUMMARY.md:14,57,63` - Performance baseline: 2.52M msg/sec
- `FINAL_DELIVERABLES_INDEX.md:82,152,228` - Test coverage: 88.5% code, 91% type
- `.wreckit/items/022-update-readmemd-with-v210-features-and-production-/plan.md:8` - Production readiness warning requirement

### OTP Patterns Observed
- **Behavior**: Umbrella application with 4 independent OTP apps
- **Supervision**:
  - erlmcp_core_sup (one_for_one) - Registry, client/server supervision
  - erlmcp_transport_sup (one_for_one) - Dynamic transport instances
  - tcps_erlmcp_sup (one_for_one) - Quality gates and monitoring
- **Process Pattern**: Process-per-connection with registry-based routing (gproc)
- **Test Pattern**: Chicago School TDD - real processes, no mocks, comprehensive coverage (88.5%)

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - erlmcp_core (35 modules) - MCP protocol, JSON-RPC, registry, client/server
  - erlmcp_transports (22 modules) - STDIO, TCP, HTTP, WebSocket transports
  - erlmcp_observability (26 modules) - OpenTelemetry, metrics, receipts
  - tcps_erlmcp (68 modules) - Toyota Code Production System quality gates

- **External Libraries**:
  - jsx 3.1.0 (JSON encoding)
  - jesse 1.8.1 (JSON Schema validation)
  - gproc 0.9.0 (Process registry)
  - gun 2.0.1 (HTTP/2 client)
  - ranch 2.1.0 (TCP acceptor pool)
  - poolboy 1.5.2 (Worker pooling)
  - opentelemetry 1.3.0 (Observability)
  - bbmustache 1.12.2 (Template rendering)
  - cowboy 2.10.0 (HTTP server)
  - fs 0.9.2 (File system monitoring)

- **OTP Applications**: kernel, stdlib, sasl, mnesia, crypto, ssl, inets

### TCPS Quality Gates to Pass
- [ ] **Documentation Accuracy**: README.md reflects v2.1.0 state (0 discrepancies)
- [ ] **Architecture Clarity**: Umbrella structure clearly documented with accurate module counts
- [ ] **Performance Honesty**: Benchmarks reflect actual v2.1.0 baseline (2.52M msg/sec, not 2.69M)
- [ ] **Production Warnings**: Explicit statement of production readiness status
- [ ] **Test Coverage**: Document current 88.5% test coverage and 91% type coverage

### Patterns to Follow
- **Documentation Pattern**: Reference CHANGELOG.md and RELEASE_NOTES_v2.1.0.md for authoritative feature list
- **Performance Documentation**: Use V2.1_BASELINE_DELIVERABLE_SUMMARY.md as source of truth for benchmarks
- **Quality Pattern**: Follow TCPS documentation standards (CLAUDE.md:74-85 for quality gates)

## Root Cause Analysis (5 Whys)

**Problem**: README.md documentation drift from v2.1.0 reality

1. **Why?** README.md was updated for v2.0.0 umbrella architecture but not refreshed for v2.1.0
2. **Why?** v2.1.0 release (2026-01-28) included comprehensive CHANGELOG.md and RELEASE_NOTES_v2.1.0.md but README.md update was not part of release checklist
3. **Why?** Release process (CHANGELOG.md:398-427) does not mandate README.md synchronization
4. **Why?** Documentation updates treated as secondary to code changes, not part of quality gates
5. **ROOT CAUSE**: Missing Poka-yoke - No automated validation that README.md reflects current version reality (version numbers, module counts, performance baselines, production readiness status)

**Solution**:
- **Immediate**: Update README.md to v2.1.0 state with accurate metrics
- **Prevention**: Add documentation accuracy check to TCPS quality gates (validate README.md version matches rebar.config, module counts accurate, performance numbers current)
- **Process**: Include README.md update in release checklist (CHANGELOG.md:398-427)

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Performance Misrepresentation** | P1 (High) | Users capacity plan with 2.69M msg/sec (v0.7.0) but actual is 2.52M msg/sec (v2.1.0) = 6.8% gap | Update README.md with accurate v2.1.0 baseline (2.52M msg/sec), document regression from v1 baseline |
| **Production Deployment of Non-Production Code** | P0 (Critical) | Users deploy v2.1.0 assuming production-ready based on README.md, but quality gates failing (per item 022) | Add explicit "⚠️ NOT PRODUCTION-READY" warning with reasons and timeline |
| **Architecture Confusion** | P2 (Medium) | Users reference monolithic v1.x structure instead of v2.1.0 umbrella apps, causing integration issues | Update Architecture section with current 4-app structure, accurate module counts (35+22+26+68=151) |
| **Build System Confusion** | P3 (Low) | Users attempt legacy `rebar` commands instead of `rebar3` for umbrella | Update Quick Start with rebar3 commands, remove rebar references |
| **Missing Test Coverage Context** | P2 (Medium) | Users unaware of 88.5% test coverage (exceeds 80% target), reduces confidence in code quality | Document test coverage status prominently, link to coverage reports |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production deployment - Users deploy non-production code causing outages
- **P1 (High):** Major capacity planning errors - 6.8% performance gap causes production issues
- **P2 (Medium):** Architecture confusion - Integration failures, support burden
- **P3 (Low):** Minor build confusion - User frustration, quick workaround

## Recommended Manufacturing Approach

**TCPS Methodology:**

1. **Specification** - Requirements defined (item.json success criteria):
   - Update Architecture Section for 4-app umbrella structure
   - Update Quick Start Section for rebar3 (vs rebar)
   - Add production readiness warning
   - Update Performance Section with v2.1.0 benchmarks (2.52M msg/sec)
   - Document test coverage status (88.5%)

2. **Pseudocode** - Documentation update plan:
   ```
   SECTION 1: Update version references
   - v1.x → v2.1.0
   - Monolithic src/ → Umbrella apps/

   SECTION 2: Architecture Section (line 126-236)
   - Confirm 4-app structure (already correct in README)
   - Update module counts:
     * erlmcp_core: 14 → 35 modules
     * erlmcp_transports: 8 → 22 modules
     * erlmcp_observability: 9 → 26 modules
     * tcps_erlmcp: 63 → 68 modules
     * Total: 94 → 151 modules

   SECTION 3: Quick Start Section (line 26-125)
   - Verify rebar3 commands (already correct)
   - Add umbrella-specific commands

   SECTION 4: Performance Section
   - Replace 2.69M msg/sec (v0.7.0 baseline) with 2.52M msg/sec (v2.1.0 baseline)
   - Add context: 6.3% regression from v1, within 10% threshold
   - Reference: V2.1_BASELINE_DELIVERABLE_SUMMARY.md:14,57

   SECTION 5: Production Readiness Warning (NEW)
   - Add prominent warning after Quick Start
   - "⚠️ PRODUCTION READINESS: v2.1.0 is NOT production-ready"
   - List failing quality gates (per item 022 analysis)
   - Estimated timeline: v2.2.0 for production readiness

   SECTION 6: Test Coverage Documentation (NEW)
   - Document 88.5% code coverage, 91% type coverage
   - Reference: FINAL_DELIVERABLES_INDEX.md:82,228
   ```

3. **Architecture** - Documentation structure:
   - No code changes required
   - Pure documentation update
   - Source files: CHANGELOG.md, RELEASE_NOTES_v2.1.0.md, V2.1_BASELINE_DELIVERABLE_SUMMARY.md, FINAL_DELIVERABLES_INDEX.md

4. **Refinement** - Chicago School TDD approach:
   - N/A for documentation (no tests needed)
   - Manual validation: README.md accuracy verification

5. **Completion** - Quality validation:
   - Verify README.md version matches rebar.config:220 (2.1.0)
   - Verify module counts sum to 151 (35+22+26+68)
   - Verify performance baseline 2.52M msg/sec documented
   - Verify production readiness warning present
   - Verify test coverage 88.5% documented

**Implementation Strategy:**

1. **Read current README.md** completely (already done)
2. **Identify sections requiring updates** (lines 1-460)
3. **Draft updates** with source file references:
   - Architecture: RELEASE_NOTES_v2.1.0.md:66-86
   - Performance: V2.1_BASELINE_DELIVERABLE_SUMMARY.md:14,57,63
   - Test Coverage: FINAL_DELIVERABLES_INDEX.md:82,228
   - Production Readiness: item 022 plan analysis
4. **Apply updates** to README.md
5. **Validate** all success criteria met

**Quality Validation:**

- **Automated**:
  - grep version numbers match rebar.config
  - Verify module counts accurate

- **Manual**:
  - Read updated README.md for clarity
  - Verify all v1.x references removed
  - Confirm production warning visible

- **Metrics**:
  - 0 v1.x references remaining
  - Version 2.1.0 documented in multiple sections
  - Performance baseline accuracy: 2.52M msg/sec
  - Module count accuracy: 151 total
  - Production readiness warning present
  - Test coverage documented: 88.5%

## Open Questions
**NONE** - All research complete:

- ✅ v2.1.0 version confirmed (rebar.config:220, all app.src files)
- ✅ Umbrella structure documented (README.md:48-88 already has structure, needs module count updates)
- ✅ Performance baseline confirmed (2.52M msg/sec from V2.1_BASELINE_DELIVERABLE_SUMMARY.md)
- ✅ Test coverage confirmed (88.5% from FINAL_DELIVERABLES_INDEX.md)
- ✅ Production readiness status confirmed (NOT production-ready per item 022)
- ✅ Build system confirmed (rebar3, not rebar)
- ✅ All source files identified and read

## Manufacturing Checklist
- [x] Root cause identified (missing Poka-yoke for documentation accuracy)
- [x] Quality gates defined (version accuracy, module counts, performance numbers, production warnings, test coverage)
- [x] OTP patterns understood (umbrella architecture, 4 apps, 151 modules)
- [x] Test strategy clear (manual validation, no code tests needed)
- [x] Risk assessment complete (5 risks identified, P0-P3 severity)
- [x] No open questions (all research complete)
- [x] Success criteria defined (5 criteria from item.json)
- [x] Technical constraints documented (6 constraints from item.json)
- [x] Source files identified (CHANGELOG.md, RELEASE_NOTES_v2.1.0.md, V2.1_BASELINE_DELIVERABLE_SUMMARY.md, FINAL_DELIVERABLES_INDEX.md)
- [x] Update plan complete (6 sections identified)
