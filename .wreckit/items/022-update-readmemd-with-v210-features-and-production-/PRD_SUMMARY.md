# PRD Summary - Update README.md with v2.1.0 features and production readiness status

**Date Created**: 2026-01-29
**Item ID**: 022-update-readmemd-with-v210-features-and-production-
**Branch**: wreckit/022-update-readmemd-with-v210-features-and-production-
**Base Branch**: main

---

## User Stories Summary

### US-001: Add production readiness warning banner to README.md (P0 - CRITICAL)
**Priority**: 1 (Critical)
**Status**: pending
**Estimated Complexity**: 1 hour

**Objective**: Insert prominent production readiness warning banner at the top of README.md (after badges, before "## Installation") to prevent users from accidentally deploying v2.1.0 to production.

**Key Acceptance Criteria**:
- Banner inserted after line 9, before "## Installation"
- Banner includes ❌ NOT PRODUCTION READY status with explicit text
- Banner lists all 5 critical quality gate failures with specific data (1% coverage, 80.8% EUnit, 5 CT failures, 526 Dialyzer warnings, 250 Xref warnings)
- Banner includes link to docs/V2_PRODUCTION_READINESS_REPORT.md
- Banner includes "DO NOT DEPLOY to production" risk assessment
- Automated validation: `grep -c 'NOT PRODUCTION READY' README.md` returns exactly 1
- Manual validation: Banner appears in first 30 lines of README.md

**TCPS Principle**: Andon (行灯) - making quality status visible to all users

---

### US-002: Update architecture section with v2.1.0 module counts (P1 - HIGH)
**Priority**: 1 (High)
**Status**: pending
**Estimated Complexity**: 2 hours

**Objective**: Update the umbrella architecture section to reflect v2.1.0's actual module counts (151 total) and correct per-app module counts based on source file enumeration.

**Key Acceptance Criteria**:
- Line 30 updated: "v2.0.0" → "v2.1.0"
- Architecture tree structure comments updated with correct module counts:
  - erlmcp_core: 14 → 42 modules
  - erlmcp_transports: 8 → 15 modules
  - erlmcp_observability: 9 → 26 modules
  - tcps_erlmcp: 63 → 68 modules
- Architecture table updated with correct counts and total shows "151 modules"
- GraphQL removed from erlmcp_transports dependencies
- Automated validation: `grep -c '151 modules' README.md` returns ≥1
- Module counts verified against actual source file enumeration

**Gap**: 57-module discrepancy (94 claimed vs 151 actual)

---

### US-003: Add performance benchmarks section with v2.1.0 baseline data (P1 - HIGH)
**Priority**: 1 (High)
**Status**: pending
**Estimated Complexity**: 2 hours

**Objective**: Add comprehensive performance section with v2.1.0 baseline data (2.52M msg/sec) and component breakdown to support accurate capacity planning.

**Key Acceptance Criteria**:
- New section "## Performance Benchmarks" added after line 235
- Section includes "### v2.1.0 Baseline (2026-01-28)" subheading
- Core throughput listed: **2.52M msg/sec** (403% improvement over v1.5.0)
- Component breakdown table included with exact metrics from baseline report
- Historical comparison table included (v1.5.0 → v2.1.0)
- Regression detection section included
- Link to baseline report: V2.1_BASELINE_DELIVERABLE_SUMMARY.md
- Benchmark commands included: `make benchmark-quick`, `make benchmark-all`
- Automated validation: `grep -c '2.52M msg/sec' README.md` returns ≥1

**Risk**: Users capacity plan using outdated 2.69M msg/sec benchmark (6.3% gap from actual 2.52M msg/sec)

---

### US-004: Update version references from v0.6.0/v2.0.0 to v2.1.0 (P2 - MEDIUM)
**Priority**: 2 (Medium)
**Status**: pending
**Estimated Complexity**: 1.5 hours

**Objective**: Update all remaining version references from v0.6.0, v2.0.0 to v2.1.0, and update migration guide references from v0.5.x→v0.6.0 to v1.x→v2.x.

**Key Acceptance Criteria**:
- Line 271: Migration guide description updated to "v1.x to v2.x"
- Line 292: Section heading updated to "### v2.1.0 Features 🆕"
- Line 339: Section heading updated to "## What's New in v2.1.0"
- Lines 343-371: "What's New" section content updated to describe v2.1.0 changes
- Line 441: Migration guide link updated to "v1.x → v2.x migration"
- Automated validation: `grep -c 'v0\\.6\\.0' README.md` returns 0
- Automated validation: `grep -c '2\\.1\\.0' README.md` returns ≥6

**Gap**: 5 sections reference outdated versions (v0.6.0, v2.0.0)

---

### US-005: Validate all README.md changes with automated and manual checks (P2 - MEDIUM)
**Priority**: 2 (Medium)
**Status**: pending
**Estimated Complexity**: 1 hour

**Objective**: Perform comprehensive validation of all README.md changes using automated checks and manual review to ensure 100% accuracy.

**Key Acceptance Criteria**:
- Automated validation script created and passes all 10 tests:
  1. Production warning present
  2. Version 2.1.0 appears ≥6 times
  3. No outdated v0.6.0 references
  4. Performance baseline present
  5. Module count 151 present
  6. Architecture table correct
  7. Production report link valid
  8. Baseline report link valid
  9. Component breakdown present
  10. Migration guide references v1.x→v2.x
- Manual validation checklist passes all 8 items:
  1. Production warning banner appears before "## Installation"
  2. All version references say "2.1.0"
  3. Architecture table shows 42+15+26+68 = 151 modules
  4. Performance section includes 2.52M msg/sec with component breakdown
  5. "What's New" section describes v2.1.0 changes
  6. All links point to existing files
  7. README reads naturally with no formatting errors
  8. No outdated content
- README.md reads naturally from start to finish (human review)
- No formatting issues (tables render correctly, links clickable)

**TCPS Principle**: Poka-yoke (ポカヨケ) - error-proofing through validation

---

## Execution Order

**TCPS Priority Sequence**:

1. **US-001 (P0)** - Add production warning banner (CRITICAL - prevents user harm)
2. **US-002 (P1)** - Update architecture section (HIGH - foundational accuracy)
3. **US-003 (P1)** - Add performance benchmarks (HIGH - capacity planning)
4. **US-004 (P2)** - Update version references (MEDIUM - consistency)
5. **US-005 (P2)** - Validate all changes (MEDIUM - quality gate)

**Total Estimated Time**: 7.5 hours

---

## Quality Gates

**Documentation Quality Gates** (all must pass):

- ✅ **Accuracy**: 100% (all data verified against source files)
- ✅ **Completeness**: All required sections updated
- ✅ **Consistency**: All version references use 2.1.0
- ✅ **Clarity**: Production warning is prominent and unambiguous
- ✅ **Link validity**: All referenced files exist

**Automated Validation Commands**:
```bash
# Version accuracy
grep -c "2.1.0" README.md | grep -q "6"

# Performance accuracy
grep -c "2.52M" README.md | grep -q "1"

# Production warning visibility
grep -c "NOT PRODUCTION READY" README.md | grep -q "1"

# Module count accuracy
grep -c "151 modules" README.md | grep -q "1"

# Outdated version check (should fail)
grep -c "v0.6.0" README.md | grep -q "0"
```

---

## Manufacturing Output

**Files Modified**:
- `/Users/sac/erlmcp/README.md` (460 lines → ~520 lines estimated)

**Files Created**:
- `.claude/hooks/pre-task-validate.sh` (validation script)

**Files Referenced** (must exist):
- `/Users/sac/erlmcp/docs/V2_PRODUCTION_READINESS_REPORT.md`
- `/Users/sac/erlmcp/V2.1_BASELINE_DELIVERABLE_SUMMARY.md`
- `/Users/sac/erlmcp/CHANGELOG.md`
- `/Users/sac/erlmcp/rebar.config`

---

## Completion Criteria

**Phase Completion**:
- [x] Research phase complete (all source files read and validated)
- [x] Planning phase complete (plan.md created with 5 phases)
- [x] PRD created (5 user stories with measurable acceptance criteria)
- [ ] Implementation phase pending (execute 5 user stories in order)
- [ ] Validation phase pending (automated + manual checks)
- [ ] Documentation complete (commit README.md changes)

**Final Sign-Off**:
- All 5 user stories complete
- All automated validations pass (10/10 tests)
- All manual validations pass (8/8 checklist items)
- README.md ready for commit to branch `wreckit/022-update-readmemd-with-v210-features-and-production-`

---

**Plan Status**: ✅ COMPLETE
**PRD Status**: ✅ COMPLETE
**Ready for Implementation**: ✅ YES

**Next Step**: Execute US-001 (Add production readiness warning banner)
