# Item 021: Remove 27+ Unused Local Functions - Summary

**Status**: Planning Complete ✅
**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery

## Planning Deliverables

### ✅ plan.md Created
**Location**: `/Users/sac/erlmcp/.wreckit/items/021-remove-27-unused-local-functions-dead-code/plan.md`

**Structure**:
- Manufacturing Objective (0 unused function warnings, ≥500 LOC removed)
- Current State (26-28 unused functions across 4 apps)
- Root Cause (5 Whys: global nowarn_unused_function suppression)
- Desired End State (specification, verification, manufacturing output)
- OUT OF SCOPE items (prevent scope creep)
- Manufacturing Approach (4 phases, 7 hours total)
- **Phase 1**: Inventory (1 hour) - Run Xref, create spreadsheet
- **Phase 2**: Categorization (2 hours) - Audit each function, apply decision matrix
- **Phase 3**: Cleanup (3 hours) - 4 batches: REMOVE, DOCUMENT, MOVE, SUPPRESS
- **Phase 4**: Verification (1 hour) - All quality gates passing
- Testing Strategy (Chicago School TDD, regression testing only)
- Manufacturing Checklist (before/during/after)
- Risk Management (6 risks identified, P1-P3 severity)
- Rollback Plan (git revert per batch)
- References (research, TCPS, OTP patterns)
- Appendix (decision matrix, examples)

### ✅ PRD Created
**Location**: Saved via `mcp__wreckit__save_prd` tool

**User Stories**: 10 stories, prioritized P1 (critical) and P2 (medium)

**Priority 1 (Critical - MUST complete)**:
1. **US-001**: Run Xref analysis and create inventory of 26-28 unused local functions
2. **US-002**: Categorize all 26-28 unused functions using decision matrix
3. **US-003**: Batch 1: Remove dead code (all functions with REMOVE decision)
4. **US-004**: Batch 2: Document public API functions (all functions with DOCUMENT decision)
5. **US-007**: Verify zero unused function warnings and generate final report

**Priority 2 (Medium - complete if time permits)**:
6. **US-005**: Batch 3: Move test helpers to test files
7. **US-006**: Batch 4: Suppress legitimate unused functions
8. **US-008**: Verify erlmcp_transport_sse retry functions
9. **US-009**: Verify tcps_receipt_verifier is_atom_stage/1 function
10. **US-010**: Add roadmap comment for erlmcp_server audio content functions

**Acceptance Criteria**: All stories have measurable, testable acceptance criteria
- Specific commands: `rebar3 xref`, `rebar3 compile`, `rebar3 eunit`
- Specific files: `docs/XREF_INVENTORY.md`, `docs/XREF_CATEGORIZATION_REPORT.md`
- Specific thresholds: 0 unused warnings, 100% test pass rate, ≥80% coverage

## Validation Checklist

### ✅ Research Verified
- [x] Read actual source code for all 5 known unused functions
- [x] Verified erlmcp_security_headers:add_to_response/1 is NOT exported (lines 19-25 export list)
- [x] Verified erlmcp_server audio functions already suppressed (lines 9-14)
- [x] Verified erlmcp_transport_sse retry functions marked @private (lines 470-492)
- [x] Verified tcps_receipt_verifier:is_atom_stage/1 not exported (line 765)
- [x] Verified tcps_work_order:atom_to_binary/1 is duplicate wrapper (lines 2110-2113)

### ✅ Scope Confirmed
**IN SCOPE**:
- Audit 26-28 unused local functions from Xref
- Remove dead code (not exported, no callers)
- Document public API (exported but unused internally)
- Add to xref_ignores with comments
- Move test helpers to test files
- Suppress legitimate unused (future features)
- Verify 0 unused function warnings

**OUT OF SCOPE**:
- Removing global nowarn_unused_function directive (Item 022)
- Fixing undefined function calls (Item 020)
- Adding tests for removed functions (dead code has no tests)
- Refactoring Xref configuration structure
- Performance optimization (not the goal)

### ✅ No Open Questions
All 7 questions from research answered:
1. **Should we remove global nowarn_unused_function?** NO - deferred to Item 022
2. **How to distinguish public API from dead code?** Use decision matrix (exported? callers?)
3. **What if removing function breaks tests?** It's NOT dead code - move to test file
4. **Should we add -compile directives for public API?** NO - use xref_ignores instead
5. **Estimated time?** 7 hours (1h + 2h + 3h + 1h)
6. **One PR or multiple?** One PR with 4 commits (batches)
7. **What if Xref false positives?** Manual verification before removal

### ✅ Phases Broken Down (≤4 hours each)
- **Phase 1**: Inventory (1 hour) ✅
- **Phase 2**: Categorization (2 hours) ✅
- **Phase 3**: Cleanup (3 hours) ✅
- **Phase 4**: Verification (1 hour) ✅
- **Total**: 7 hours ✅

### ✅ Acceptance Criteria Defined
**Measurable**:
- Xref: 0 unused function warnings (from 26-28)
- Tests: 100% pass rate (EUnit + Common Test)
- Compilation: 0 errors, 0 warnings
- Coverage: ≥80%
- Dialyzer: 0 warnings
- LOC removed: ≥500

**Specific**:
- Commands: `rebar3 xref`, `rebar3 compile`, `rebar3 eunit`, `rebar3 ct`, `rebar3 cover`, `rebar3 dialyzer`
- Files: `docs/XREF_INVENTORY.md`, `docs/XREF_CATEGORIZATION_REPORT.md`, `docs/XREF_CLEANUP_REPORT.md`
- Metrics: Functions removed, documented, moved, suppressed; Lines removed; Warnings before/after

## Quality Gates

**ALL MUST PASS**:
- [ ] Compilation: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] EUnit: `rebar3 eunit` - 100% pass rate
- [ ] Common Test: `rebar3 ct` - 100% pass rate
- [ ] Coverage: `rebar3 cover` - ≥80%
- [ ] Dialyzer: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 unused function warnings (PRIMARY GOAL)

**Enforcement**:
- Run after Phase 3 (cleanup)
- Run after Phase 4 (verification)
- If ANY gate fails, STOP and fix - Jidoka (built-in quality)

## TCPS Principles Applied

✅ **Standard Work** (標準作業): Every step documented in plan.md
- Phase 1: Run Xref → Extract → Create inventory
- Phase 2: Check exported → Search callers → Apply decision matrix
- Phase 3: Execute in 4 batches (REMOVE, DOCUMENT, MOVE, SUPPRESS)
- Phase 4: Run all quality gates → Generate report

✅ **Andon** (行灯): Progress visible, problems signaled
- Phase 1: `/tmp/xref_unused.txt` (baseline)
- Phase 2: `docs/XREF_INVENTORY.md` (inventory table)
- Phase 3: Git commits (4 batches, easy rollback)
- Phase 4: `/tmp/xref_final.txt` (validation)
- Quality gates: Red flag = stop and fix

✅ **Heijunka** (平準化): Small phases, independently verifiable
- Phase 1: 1 hour (analysis only)
- Phase 2: 2 hours (analysis only)
- Phase 3: 3 hours (4 batches, each committed separately)
- Phase 4: 1 hour (verification)

✅ **Poka-yoke** (ポカヨケ): Quality gates built into process
- Decision matrix prevents wrong decisions
- P1 risk check: NO exported functions removed
- Test regression check: If tests fail, function was NOT dead code
- Xref validation: 0 warnings (primary goal)

✅ **Jidoka** (自働化): Failures stop production
- If ANY quality gate fails, STOP and fix
- No "good enough" - perfect or nothing
- Zero defects: 99.99966% defect-free delivery

## Next Steps

**Execute Manufacturing Plan**:

1. **Phase 1** (US-001): Run Xref, create inventory
   - Command: `rebar3 xref 2>&1 | tee /tmp/xref_unused.txt`
   - Output: `docs/XREF_INVENTORY.md`

2. **Phase 2** (US-002, US-008, US-009): Categorize functions
   - Apply decision matrix
   - Output: Completed inventory, `docs/XREF_CATEGORIZATION_REPORT.md`

3. **Phase 3** (US-003, US-004, US-005, US-006, US-010): Cleanup
   - Batch 1: Remove dead code (US-003)
   - Batch 2: Document public API (US-004, US-010)
   - Batch 3: Move test helpers (US-005)
   - Batch 4: Suppress legitimate unused (US-006)

4. **Phase 4** (US-007): Verification
   - Run all quality gates
   - Generate `docs/XREF_CLEANUP_REPORT.md`
   - Verify 0 unused function warnings

**Estimated Timeline**: 7 hours total

## Sign-off

**Planning Complete**: 2025-01-29
**Plan Created**: `/Users/sac/erlmcp/.wreckit/items/021-remove-27-unused-local-functions-dead-code/plan.md`
**PRD Created**: 10 user stories via `mcp__wreckit__save_prd` tool
**Quality Gates Defined**: All 6 gates (compile, tests, coverage, dialyzer, xref, performance)
**Risk Assessment**: 6 risks identified, mitigations documented
**Rollback Plan**: Git revert per batch (easy rollback)

**Ready for Manufacturing**: ✅ YES

---

**TCPS Manufacturing**: Zero Defects, Every Time.
