# Research: Update README.md with v2.1.0 features and production readiness status

**Date**: 2026-01-29
**Item**: 022-update-readmemd-with-v210-features-and-production-
**Section**: docs
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
README is outdated - references v1.x instead of v2.1.0

**Motivation:** Users need accurate documentation for v2.1.0. Must warn about production readiness status.

**Success criteria:**
- README.md updated
- v2.1.0 features documented
- Production readiness warnings added
- Performance benchmarks updated (2.52M msg/sec)
- Test coverage status documented

**Technical constraints:**
- Architecture section: 4-app umbrella structure
- Quick Start: rebar3 (vs rebar)
- Performance section: v2.1.0 benchmarks
- Testing section: coverage reports

**Signals:** priority: low, urgency: P3 - NICE TO HAVE

### Quality Gate Status
- **Gate Type**: Documentation Accuracy
- **Current State**: README.md references v0.6.0 (lines 292, 339, 343, 441), missing v2.1.0 features, no production readiness warnings, outdated performance numbers (2.69M msg/sec from v1.5.0 baseline vs actual v2.1.0 baseline of 2.52M msg/sec)
- **Target State**: Complete v2.1.0 documentation with production readiness warnings and accurate benchmarks
- **Gap**:
  - Version references: 4 sections still reference v0.6.0 (should be v2.1.0)
  - Performance: 2.69M msg/sec (v1.5.0) → 2.52M msg/sec (v2.1.0 actual)
  - Architecture: Missing v2.1.0 4-app umbrella details (35+22+26+68 = 151 modules)
  - Production status: Missing critical warnings (NOT PRODUCTION READY per V2_PRODUCTION_READINESS_REPORT.md)

## Summary

This task updates the project's primary README.md file to accurately reflect erlmcp v2.1.0's current state, including architectural changes from monolithic v1.x to the 4-app umbrella structure, updated performance benchmarks showing 2.52M msg/sec throughput, and critical production readiness warnings documenting that v2.1.0 is **NOT production-ready** due to failing quality gates (1% coverage vs 80% required, 80.8% test pass rate vs 100% required, 526 dialyzer warnings, 250 xref warnings).

The manufacturing approach applies TCPS **Andon (行灯)** principles - making quality status visible to all users through prominent warnings. The README must document:
1. **Jidoka (自働化)**: Clear production readiness status with stop-the-line quality gates
2. **Poka-yoke (ポカヨケ)**: Error-proofing by preventing users from deploying untested code
3. **Andon (行灯)**: Visual signaling of quality gate status (❌ NOT PRODUCTION READY)

The technical approach involves updating specific sections while preserving the document's structure:
- Architecture section (lines 46-88): Update with v2.1.0 4-app umbrella details (151 total modules)
- Performance section (lines 237-243): Replace v1.5.0 2.69M msg/sec with v2.1.0 2.52M msg/sec baseline
- Migration guide references (lines 271, 292, 339, 343, 441): Update from v0.5.x→v0.6.0 to v1.x→v2.x migration
- Add production readiness section with clear warnings based on V2_PRODUCTION_READINESS_REPORT.md
- Document test coverage status (1% measured vs 80% required)

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/README.md` (460 lines) - Primary project documentation
  - `/Users/sac/erlmcp/rebar.config` (278 lines) - Release version: 2.1.0 (line 220)
  - `/Users/sac/erlmcp/CHANGELOG.md` - Version 2.1.0 release notes (lines 39-100)
  - `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md` - Production readiness evaluation (NOT READY)
  - `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md` - Performance baseline (2.52M msg/sec)
  - `/Users/sac/erlmcp/CODE_QUALITY_REPORT_V2.1.md` - Quality gate status

- **Patterns**:
  - README.md follows standard Markdown structure with badges, installation, quick start, architecture, features, examples, documentation, and development sections
  - Uses badge system for build status at top
  - Code examples in Erlang syntax
  - Tables for architecture overview

- **Tests**:
  - Current coverage: 1% (from V2_PRODUCTION_READINESS_REPORT.md line 69)
  - EUnit pass rate: 80.8% (21/26 tests, line 91)
  - Common Test: 5 suite failures (line 104)

- **Quality**:
  - Compilation: PASS (151 modules, 0 errors)
  - Coverage: FAIL (1% vs 80% required) - Critical gap of 79 percentage points
  - EUnit: FAIL (80.8% vs 100% required)
  - Common Test: FAIL (5 suites failing)
  - Dialyzer: FAIL (526 warnings vs 0 required)
  - Xref: FAIL (250 warnings vs 0 required)
  - Production readiness: ❌ NOT READY

### Key Files

- `/Users/sac/erlmcp/README.md:1-460` - Primary documentation requiring updates
  - Line 30: Umbrella structure mentioned
  - Lines 46-88: Architecture section - needs v2.1.0 4-app details
  - Lines 126-179: v2.0.0 Architecture section - needs update to v2.1.0
  - Lines 237-243: SLO and Performance Targets - needs 2.52M msg/sec
  - Lines 271, 292, 339, 343, 441: v0.6.0 references - need v2.1.0
  - Line 143: tcps_erlmcp modules count shown as 63 (actual: 68 per tcps_erlmcp.app.src)

- `/Users/sac/erlmcp/rebar.config:220` - Release version {erlmcp, "2.1.0"}
  - Defines release version as 2.1.0
  - Includes all 4 apps: erlmcp_core, erlmcp_transports, erlmcp_observability, tcps_erlmcp

- `/Users/sac/erlmcp/apps/tcps_erlmcp/src/tcps_erlmcp.app.src:3` - Version: "2.1.0"
  - TCPS app version 2.1.0
  - Lists 68 modules (not 63 as stated in README.md)

- `/Users/sac/erlmcp/CHANGELOG.md:39-100` - Version 2.1.0 release notes
  - Release date: 2026-01-28
  - Legacy cleanup: Removed 127 modules, 6,079 lines
  - 4-app umbrella finalized with module counts
  - Test infrastructure: 4 new test suites (1,507 LOC)
  - Benchmark baseline established

- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:1-150` - Production readiness evaluation
  - Status: ❌ NOT PRODUCTION READY (line 6)
  - Test coverage: 1% vs 80% required (line 69)
  - EUnit: 80.8% pass rate vs 100% (line 91)
  - Common Test: 5 suite failures (line 104)
  - Dialyzer: 526 warnings (line 131)
  - Xref: 250 warnings (line 143)
  - Passed: Compilation, Performance, Version consistency

- `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md:14-69` - Performance baseline
  - Core throughput: 2.52M msg/sec (line 14)
  - Historical improvement: 403% over v1.5.0 (line 19)
  - Component breakdown: Registry 1.94M, Queue 10.0M, Pool 2.50M, Session 0.95M (lines 53-57)
  - Overall grade: A+ (line 57)

- `/Users/sac/erlmcp/CODE_QUALITY_REPORT_V2.1.md:1-100` - Quality gate report
  - Overall score: 85% (line 13)
  - Compilation: PASS (151 modules, line 38)
  - Critical tests: PASS (erlmcp_batch_tests 14/14, line 17)
  - Dialyzer: 526 warnings documented (line 18)
  - Xref: 250 warnings acceptable (line 18)

### OTP Patterns Observed
- **Behavior**: Documentation update task (no code changes)
- **Supervision**: N/A (documentation only)
- **Process Pattern**: N/A (documentation only)
- **Test Pattern**: Documentation accuracy validation through research

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - README.md structure and sections
  - CHANGELOG.md for v2.1.0 release details
  - V2_PRODUCTION_READINESS_REPORT.md for production status
  - V2.1_BASELINE_DELIVERABLE_SUMMARY.md for performance data
  - CODE_QUALITY_REPORT_V2.1.md for quality gate status

- **External Libraries**: None (documentation only)
- **OTP Applications**: erlmcp (umbrella), erlmcp_core, erlmcp_transports, erlmcp_observability, tcps_erlmcp

### TCPS Quality Gates to Pass
This is a documentation task, but must accurately reflect actual quality gate status:

- [ ] **Documentation Accuracy**: README.md must match actual v2.1.0 state
  - Version: 2.1.0 (not v0.6.0)
  - Architecture: 4-app umbrella (35+22+26+68 = 151 modules, not 63 for tcps)
  - Performance: 2.52M msg/sec (not 2.69M msg/sec from v1.5.0)
  - Production status: ❌ NOT READY (must be prominently displayed)

- [ ] **Andon (行灯) - Visible Signaling**: Production readiness warnings must be:
  - Prominent (near top of README)
  - Clear (use ❌ symbol and explicit text)
  - Specific (list failing gates with data)
  - Actionable (provide path to production readiness)

- [ ] **Poka-yoke (ポカヨケ) - Error-Proofing**: Prevent users from:
  - Assuming production readiness when it's not
  - Using outdated performance numbers for capacity planning
  - Deploying without understanding quality gate failures

### Patterns to Follow
- **Documentation Pattern**: `/Users/sac/erlmcp/README.md` - Existing structure and tone
- **Warning Pattern**: `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md:6-15` - Clear production status with emoji
- **Performance Documentation**: `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md:49-69` - Clear tables with metrics
- **Quality Gate Pattern**: `/Users/sac/erlmcp/CODE_QUALITY_REPORT_V2.1.md:13-19` - Table format with status columns

## Root Cause Analysis (5 Whys)

**Problem**: README.md is outdated and misleads users about v2.1.0 capabilities

1. **Why?** README.md still references v0.6.0 features and migration paths from v0.5.x
2. **Why?** README was not updated during v2.1.0 release on 2026-01-28
3. **Why?** Release process (CHANGELOG.md updated) did not include README.md update as required step
4. **Why?** No automated validation checks if README.md version matches rebar.config release version
5. **Why?** Documentation quality gates not integrated into TCPS manufacturing system (Poka-yoke gap)

**Solution**:
- **Immediate**: Update README.md to match v2.1.0 reality with production warnings
- **Systemic**: Add documentation accuracy checks to TCPS quality gates (future enhancement)
- **Preventive**: Create README.md validation script that checks:
  - Version consistency with rebar.config
  - Performance numbers match latest baseline
  - Production readiness status reflects actual gate results

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| Users deploy v2.1.0 to production assuming it's ready | P0 (Critical) | Production outages due to 1% coverage, 5 failing test suites, 526 dialyzer warnings | Add prominent ❌ NOT PRODUCTION READY warning at top of README with specific gate failures |
| Users capacity plan using outdated 2.69M msg/sec benchmark | P1 (High) | Under-provisioned infrastructure (2.69M vs actual 2.52M = 6.3% gap) | Update all performance references to 2.52M msg/sec v2.1.0 baseline |
| Users confused by v0.6.0 migration references in v2.1.0 README | P2 (Medium) | Wasted time following outdated migration paths | Update all v0.6.0 references to v2.1.0, add v1.x→v2.x migration guide reference |
| Documentation inconsistencies reduce trust in project | P2 (Medium) | Users abandon project due to poor documentation | Ensure all version numbers, module counts, and features are accurate |
| README.md updates introduce new errors | P3 (Low) | Misinformation spread to users | Research thoroughly, cite sources, validate all numbers against source files |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria (item.json)
2. **Pseudocode** - N/A (documentation update, not code)
3. **Architecture** - Identify sections to update, preserve structure
4. **Refinement** - Update README.md section by section with validation
5. **Completion** - Verify all accuracy checks pass

**Implementation Strategy:**

**Phase 1: Analysis (COMPLETE)**
- ✅ Read existing README.md completely
- ✅ Identify all sections requiring updates
- ✅ Source accurate data from:
  - rebar.config (version 2.1.0)
  - CHANGELOG.md (release details)
  - V2_PRODUCTION_READINESS_REPORT.md (quality status)
  - V2.1_BASELINE_DELIVERABLE_SUMMARY.md (performance)
  - CODE_QUALITY_REPORT_V2.1.md (gates)

**Phase 2: Update Planning (RECOMMENDED)**
Update README.md in order of priority:

1. **Production Readiness Warning** (CRITICAL - P0)
   - Add banner at top (after badges, before installation)
   - Use ❌ NOT PRODUCTION READY heading
   - List 5 critical failures with data:
     - Test Coverage: 1% vs 80% required
     - EUnit: 80.8% vs 100% required
     - Common Test: 5 suite failures
     - Dialyzer: 526 warnings
     - Xref: 250 warnings
   - Provide link to V2_PRODUCTION_READINESS_REPORT.md

2. **Version Updates** (HIGH - P1)
   - Line 143: Change tcps_erlmcp from 63 → 68 modules
   - Lines 292, 339, 343, 441: Change v0.6.0 → v2.1.0
   - Line 271: Update migration guide reference from v0.5.x→v0.6.0 to v1.x→v2.x
   - Verify all version references use 2.1.0

3. **Architecture Section** (HIGH - P1)
   - Lines 46-88: Update umbrella structure table:
     - erlmcp_core: 14 → 35 modules
     - erlmcp_transports: 8 → 22 modules
     - erlmcp_observability: 9 → 26 modules
     - tcps_erlmcp: 63 → 68 modules
     - Total: 94 → 151 modules
   - Update line 145: "Total: 94 modules" → "Total: 151 modules"
   - Lines 147-179: Verify v2.0.0 Architecture section reflects v2.1.0

4. **Performance Section** (MEDIUM - P2)
   - Lines 237-243: Update performance numbers:
     - Replace 2.69M msg/sec → 2.52M msg/sec (v2.1.0 baseline)
     - Add component breakdown table:
       - Registry: 1.94M ops/sec
       - Queue: 10.0M ops/sec
       - Pool: 2.50M ops/sec
       - Session: 0.95M ops/sec
     - Note 403% improvement over v1.5.0
   - Update CLAUDE.md references if mentioned

5. **Features Section** (MEDIUM - P2)
   - Lines 289-298: v0.6.0 Library Integration → v2.1.0 Feature Summary
   - Add v2.1.0 features from CHANGELOG.md:
     - Legacy cleanup: 127 modules removed, 6,079 lines deleted
     - 4 new test suites (1,507 LOC)
     - Benchmark consolidation (5 modules from 15+)
   - Lines 339-371: Update "What's New" section to v2.1.0

6. **Testing Section** (LOW - P3)
   - Add testing section after Development (around line 310)
   - Document current test status:
     - Coverage: 1% (NOT production-ready)
     - EUnit: 80.8% pass rate
     - Common Test: 5 suites failing
     - Link to test reports
   - Include path to improvement: "Run `make test` to verify"

**Phase 3: Validation (REQUIRED)**
After updating README.md:
- [ ] Verify all version numbers are 2.1.0
- [ ] Count modules: 35+22+26+68 = 151
- [ ] Check performance: 2.52M msg/sec (not 2.69M)
- [ ] Confirm production warning is prominent and clear
- [ ] Validate all links point to existing files
- [ ] Read updated README.md completely for flow and accuracy

**Quality Validation:**
- **Automated**:
  - `grep -c "2.1.0" README.md` should be >5
  - `grep -c "2.52M" README.md` should be >1
  - `grep -c "NOT PRODUCTION READY" README.md` should be 1
- **Manual**:
  - Compare updated sections against source documents
  - Verify module counts match app.src files
  - Check performance numbers match baseline report
- **Metrics**:
  - Version accuracy: 100% (all references to 2.1.0)
  - Performance accuracy: 100% (2.52M msg/sec)
  - Production warning visibility: Top 30 lines of README

## Open Questions
**NONE** - All research complete. Have identified:
- Exact sections requiring updates (with line numbers)
- Source data for all updates (with file paths)
- Production readiness status (with gate failures)
- Performance baseline (with component breakdown)
- Version information (from rebar.config and app.src files)
- Module counts (from actual apps structure)
- Migration paths (from CHANGELOG.md)

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Documentation drift during release process
- [x] Quality gates defined (specific thresholds) - Version accuracy, performance accuracy, production warning visibility
- [x] OTP patterns understood (behaviors, supervision) - N/A (documentation task)
- [x] Test strategy clear (Chicago School TDD) - Research complete, no code tests needed
- [x] Risk assessment complete (severity P0-P3) - 5 risks identified with mitigations
- [x] No open questions (all research complete) - All source data verified with line numbers

---

## Appendix: Source Data Validation

### Version Information
- **rebar.config:220**: `{release, {erlmcp, "2.1.0"},` - CONFIRMED
- **CHANGELOG.md:39**: `## [2.1.0] - 2026-01-28` - CONFIRMED
- **tcps_erlmcp.app.src:3**: `{vsn, "2.1.0"}` - CONFIRMED

### Module Counts (from CHANGELOG.md:62-66 and apps structure)
- erlmcp_core: 35 modules (line 63) - Confirmed in apps/erlmcp_core/src/
- erlmcp_transports: 22 modules (line 64) - Confirmed in apps/erlmcp_transports/src/
- erlmcp_observability: 26 modules (line 65) - Confirmed in apps/erlmcp_observability/src/
- tcps_erlmcp: 68 modules (line 66) - Confirmed in tcps_erlmcp.app.src (README shows 63 - ERROR)
- **Total: 151 modules** (README shows 94 - ERROR)

### Performance Baseline (from V2.1_BASELINE_DELIVERABLE_SUMMARY.md)
- **Overall: 2.52M msg/sec** (line 14) - v2.1.0 actual
- Component breakdown (lines 53-57):
  - Registry: 1.94M ops/sec
  - Queue: 10.0M ops/sec
  - Pool: 2.50M ops/sec
  - Session: 0.95M ops/sec
- Historical: 403% improvement over v1.5.0 (line 19)
- **README currently shows: 2.69M msg/sec (v1.5.0 baseline from CLAUDE.md:22,156)** - OUTDATED

### Production Readiness (from V2_PRODUCTION_READINESS_REPORT.md)
- **Status: ❌ NOT PRODUCTION READY** (line 6)
- Test Coverage: 1% (line 69) - Target: 80% - Gap: 79 percentage points
- EUnit: 80.8% pass rate (line 91) - Target: 100%
- Common Test: 5 suite failures (line 104)
- Dialyzer: 526 warnings (line 131)
- Xref: 250 warnings (line 143)
- **README currently has: No production readiness warning** - CRITICAL GAP

### Quality Gate Status (from CODE_QUALITY_REPORT_V2.1.md)
- Overall Score: 85% (line 13)
- Compilation: ✅ PASS (151 modules, line 38)
- Critical Tests: ✅ PASS (erlmcp_batch_tests 14/14, line 17)
- Dialyzer: ⚠️ 526 warnings documented (line 18)
- Xref: ⚠️ 250 warnings acceptable (line 18)
- Benchmarks: ⚠️ DEFERRED (line 19)

**Conclusion**: README.md requires significant updates to reflect v2.1.0 reality, with critical production readiness warnings being highest priority (P0).
