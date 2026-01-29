# Planning Complete - Item 021

**Date**: 2025-01-29
**Status**: READY FOR IMPLEMENTATION
**TCPS Compliance**: Lean Six Sigma Level Strictness

## Summary

Manufacturing plan and PRD created for **Item 021: Remove 27+ unused local functions (dead code)**.

## Deliverables

### 1. Manufacturing Plan (`plan.md`)

**Structure**:
- Manufacturing Objective: Eliminate 26-28 unused local function warnings
- Quality Gate Requirements: 7 gates defined (compile, EUnit, CT, coverage, Dialyzer, Xref)
- Current State: 5 known unused functions documented, root cause identified
- Desired End State: 0 unused function warnings, ≥500 LOC removed
- Manufacturing Approach: 4 phases (Inventory → Categorization → Cleanup → Verification)
- Testing Strategy: Chicago School TDD (existing tests must pass)
- Risk Management: 6 risks identified with mitigations
- References: All research and configuration files linked

**Key Decisions**:
- **DO NOT remove global `nowarn_unused_function`** (deferred to Item 022)
- **DO NOT fix undefined function calls** (Item 020's responsibility)
- **DO categorize all 26-28 functions** before removing any (prevents API breakage)

### 2. Product Requirements Document (PRD)

**User Stories Created**: 7 stories

| Story ID | Title | Priority | Phase | Estimated |
|----------|-------|----------|-------|-----------|
| US-001 | Run Xref analysis and create inventory | P1 | Phase 1 | 1h |
| US-002 | Categorize all unused functions | P1 | Phase 2 | 2h |
| US-003 | Remove dead code functions | P2 | Phase 3 (Batch 1) | 1h |
| US-004 | Document public API functions | P2 | Phase 3 (Batch 2) | 1h |
| US-005 | Move test helper functions | P3 | Phase 3 (Batch 3) | 0.5h |
| US-006 | Suppress legitimate unused functions | P3 | Phase 3 (Batch 4) | 0.5h |
| US-007 | Verify zero warnings and final report | P1 | Phase 4 | 1h |

**Total Estimated Time**: 7 hours

## Quality Gates

All user stories MUST pass:
- **Compilation**: 0 errors
- **EUnit**: 100% pass rate
- **Common Test**: 100% pass rate
- **Coverage**: ≥80%
- **Dialyzer**: 0 warnings
- **Xref**: 0 unused function warnings (PRIMARY GOAL)

## Risk Assessment

**P1 (High) Risks**: 1 identified
- Removing unused function breaks external API
- **Mitigation**: Verify NO exported functions removed, check examples/ for usage

**P2 (Medium) Risks**: 2 identified
- Dead code removal eliminates needed functionality (dynamic calls)
- Test helpers moved to test files break test isolation
- **Mitigation**: Search for dynamic call sites, run tests after each batch

**P3 (Low) Risks**: 3 identified
- Xref false positives, public API not documented
- **Mitigation**: Manual verification, add @doc comments

## TCPS Principles Applied

✅ **Standard Work** (標準作業): Every step documented in plan.md
✅ **Andon** (行灯): Progress visible via inventory and reports
✅ **Heijunka** (平準化): 4 small phases (≤4 hours each)
✅ **Poka-yoke** (ポカヨケ): Quality gates at every phase
✅ **Jidoka** (自働化): Failures stop production

## Next Steps

1. **Start Implementation**: Execute US-001 (Run Xref analysis)
2. **Follow Plan**: Work through phases sequentially (1 → 2 → 3 → 4)
3. **Quality Gates**: Run all gates after each phase
4. **Documentation**: Create inventory, categorization report, cleanup report
5. **Completion**: Verify 0 unused function warnings, generate final report

## Files Created/Modified

- `/Users/sac/erlmcp/.wreckit/items/021-remove-27-unused-local-functions-dead-code/plan.md` (updated)
- `/Users/sac/erlmcp/.wreckit/items/021-remove-27-unused-local-functions-dead-code/PLANNING_COMPLETE.md` (this file)
- PRD saved via `mcp__wreckit__save_prd` tool

## Verification Checklist

Before starting implementation:
- [x] Research verified (read actual source code)
- [x] Scope confirmed (IN/OUT documented)
- [x] No open questions (all 7 questions answered in plan.md)
- [x] Phases broken down (4 phases, ≤4 hours each)
- [x] Acceptance criteria defined (measurable, specific)
- [x] Risk assessment complete (6 risks, mitigations documented)
- [x] Quality gates defined (7 gates, all mandatory)
- [x] User stories created (7 stories, all with acceptance criteria)
- [x] PRD saved to wreckit system

## Sign-off

**Planning Status**: ✅ COMPLETE
**Ready for Implementation**: YES
**Estimated Duration**: 7 hours
**TCPS Compliance**: LEAN SIX SIGMA LEVEL STRICTNESS
**Zero Defects**: 99.99966% defect-free delivery

---

**Manufacturing Plan Created By**: Claude (Agent SDK)
**Date**: 2025-01-29
**Next Review**: After Phase 1 (Inventory) completion
