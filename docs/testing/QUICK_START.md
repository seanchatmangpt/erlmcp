# Test Coverage Strategy - Quick Start Guide

**Created**: 2026-01-30
**Status**: Implementation Ready

---

## Executive Summary

**Current State:**
- 89 test files exist (73 EUnit + 16 CT)
- Coverage: ~0% (tests don't start application properly)
- 24 broken test files need triage
- 47 redundant test artifacts need cleanup

**Target State:**
- 80%+ overall coverage (enforced by quality gates)
- 85%+ core module coverage
- 120+ test files (add 10 property tests)
- Clean directory structure
- Automated quality gates (pre-commit + CI/CD)

---

## Critical Issues Found

### 1. Coverage Reporting Shows 0% (Tests Exist!)

**Problem**: 89 test files exist but coverage reports show 0%

**Root Cause**: Tests don't call `application:ensure_all_started/1`

**Fix**: Add to all test setups:
```erlang
setup_application() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    application:ensure_all_started(erlmcp_observability).

cleanup_application(_) ->
    application:stop(erlmcp_observability),
    application:stop(erlmcp_transports),
    application:stop(erlmcp_core).
```

### 2. Broken Test Files

**Found**: 24 .broken files

**Location**:
- 11 in `apps/` (mix of src and test)
- 13 in `test.bak/`

**Priority Fix Order**:
1. `erlmcp_integration_SUITE.erl.broken` (P0) - Critical E2E tests
2. `mcp_compliance_SUITE.erl.broken` (P0) - Protocol compliance
3. `erlmcp_error_handling_tests.erl.broken` (P1) - Error handling
4. `erlmcp_jsonrpc_compliance_tests.erl.broken` (P1) - JSON-RPC validation
5. `erlmcp_server_capabilities_SUITE.erl.broken` (P1) - Capability negotiation

### 3. Missing Property-Based Tests

**Current**: 1 Proper test file
**Target**: 10+ Proper test files

**Critical Gap**: No property tests for:
- Capability negotiation roundtrips
- Session state machine properties
- Tool/resource invocation invariants
- Transport message ordering
- Batch request/response correlation

### 4. Scattered Test Artifacts

**Found**: 47+ files scattered across:
- `test/*.md` (15 docs)
- `test.bak/*.broken` (13 broken tests)
- `test/*.sh` (4 scripts)
- `test/*.escript` (2 scripts)
- `test_destructive/` (2 files)
- `tests/` (2 SUITE files)
- `attic/legacy_untrusted/` (3 old benchmarks)

**Action**: Consolidate to clean structure (see implementation plan)

---

## 11-Week Implementation Roadmap

### Phase 1: Fix Infrastructure (Week 1-2)

**Tasks**:
1. Triage 24 .broken files (2 hours)
2. Add `application:ensure_all_started/1` to all 89 test files (6 hours)
3. Fix 8 P0/P1 broken test files (12 hours)
4. Verify coverage >0% (2 hours)

**Deliverables**:
- ✅ 8 broken test files fixed
- ✅ All 89 test files have proper application setup
- ✅ Coverage reports show >0% baseline
- ✅ Quality gates script runs successfully

**Effort**: 22 hours total

---

### Phase 2: Achieve 50% Coverage (Week 3-4)

**Focus**: Core modules (Priority 1)

**Modules**:
- `erlmcp_client.erl` (add edge cases, error paths)
- `erlmcp_server.erl` (tool/resource handlers, errors)
- `erlmcp_registry.erl` (concurrent registration, failures)
- `erlmcp_json_rpc.erl` (malformed JSON, edge cases)
- `erlmcp_capabilities.erl` (NEW - capability negotiation tests)
- `erlmcp_session_manager.erl` (session lifecycle, cleanup)

**Deliverables**:
- ✅ 50% overall coverage achieved
- ✅ Core modules at 60%+ coverage
- ✅ All error paths tested
- ✅ Boundary conditions covered

**Effort**: 31 hours total

---

### Phase 3: Achieve 80%+ Coverage (Week 5-8)

**Week 5-6: Transport Tests**
- Add unit tests for stdio, tcp, http, websocket (26 hours)

**Week 7: Property-Based Tests**
- Add 10 Proper test files for protocol invariants (40 hours)

**Week 8: Feature Tests**
- Complete tests for resource, tool, prompt, completion, sampling (20 hours)

**Deliverables**:
- ✅ 80%+ overall coverage achieved
- ✅ Core modules at 85%+ coverage
- ✅ Transport modules at 80%+ coverage
- ✅ 10 property test files added
- ✅ All feature modules tested

**Effort**: 86 hours total

---

### Phase 4: Consolidate (Week 9-10)

**Tasks**:
1. Relocate/delete 47 redundant files (8 hours)
2. Reorganize directory structure (4 hours)
3. Create test organization docs (4 hours)
4. Verify tests still pass (2 hours)

**Deliverables**:
- ✅ Clean directory structure
- ✅ Test organization documentation
- ✅ All tests passing after reorganization

**Effort**: 18 hours total

---

### Phase 5: Automate (Week 11)

**Tasks**:
1. Create pre-commit hook (4 hours)
2. Create post-task validation hook (4 hours)
3. Set up CI/CD pipeline (8 hours)

**Deliverables**:
- ✅ Pre-commit hook enforces quality gates
- ✅ Post-task hook validates agent work
- ✅ CI/CD pipeline runs on every push
- ✅ Coverage reports uploaded to Codecov

**Effort**: 16 hours total

---

## Total Effort: 173 hours (4.3 weeks at 40 hours/week)

---

## Coverage Heat Map (Priority Order)

### Priority 1: Core Modules (Target: 85%+)
- `erlmcp_client.erl` - Current: 0% | Target: 90%
- `erlmcp_server.erl` - Current: 0% | Target: 90%
- `erlmcp_registry.erl` - Current: 0% | Target: 85%
- `erlmcp_json_rpc.erl` - Current: 0% | Target: 95%
- `erlmcp_capabilities.erl` - Current: 0% | Target: 85%
- `erlmcp_session_manager.erl` - Current: 0% | Target: 85%

### Priority 2: Transport Modules (Target: 80%+)
- `erlmcp_transport_stdio.erl` - Current: 0% | Target: 80%
- `erlmcp_transport_tcp.erl` - Current: 0% | Target: 80%
- `erlmcp_transport_http.erl` - Current: 0% | Target: 80%
- `erlmcp_transport_websocket.erl` - Current: 0% | Target: 80%

### Priority 3: Feature Modules (Target: 80%+)
- `erlmcp_resource.erl` - Current: 0% | Target: 85%
- `erlmcp_tool.erl` - Current: 0% | Target: 85%
- `erlmcp_prompt_template.erl` - Current: 0% | Target: 80%
- `erlmcp_completion.erl` - Current: 0% | Target: 80%
- `erlmcp_sampling.erl` - Current: 0% | Target: 80%

### Priority 4: Support Modules (Target: 75%+)
- All other modules in erlmcp_core

---

## Key Commands

```bash
# Run all tests with coverage
rebar3 do eunit, ct, cover --verbose

# View coverage report
open _build/test/cover/index.html  # macOS
xdg-open _build/test/cover/index.html  # Linux

# Run quality gates (pre-commit hook)
.git/hooks/pre-commit

# Run post-task validation (Claude agent)
.claude/hooks/post-task-validate.sh

# Fix a specific broken test
mv test.bak/mcp_compliance_SUITE.erl.broken apps/erlmcp_core/test/mcp_compliance_SUITE.erl
# Then fix compilation errors

# Add application setup to a test file
# See README.md for template
```

---

## Quality Gates (Enforced Before Commit)

**Blocking Gates** (MUST pass):
- ✅ Compilation: 0 errors
- ✅ Tests: 100% pass rate (0 failures)
- ✅ Coverage: ≥80% overall, ≥85% core modules

**Warning Gates** (reported but not blocking):
- ⚠️ Dialyzer: Type warnings
- ⚠️ Xref: Undefined function calls

---

## Next Steps

1. **Week 1**: Fix broken tests infrastructure
   - Add `application:ensure_all_started/1` to all test files
   - Fix 8 P0/P1 broken tests
   - Verify coverage >0%

2. **Week 2-4**: Achieve 50% coverage
   - Add tests for core modules (Priority 1)
   - Focus on error handling and edge cases

3. **Week 5-8**: Achieve 80%+ coverage
   - Add transport tests
   - Add 10 property-based tests
   - Complete feature module tests

4. **Week 9-10**: Consolidate and cleanup
   - Reorganize directory structure
   - Delete 47 redundant files

5. **Week 11**: Automate quality gates
   - Pre-commit hook
   - Post-task validation
   - CI/CD pipeline

---

## Documentation

**Comprehensive Strategy** (1580 lines):
- [test_strategy_plan.md](test_strategy_plan.md) - Full plan with 5 C4 PlantUML diagrams

**Quick Reference** (230 lines):
- [README.md](README.md) - Test organization and patterns

**This Guide** (you are here):
- [QUICK_START.md](QUICK_START.md) - Summary and roadmap

---

## Contact

For questions or issues:
- See [test_strategy_plan.md](test_strategy_plan.md) for detailed implementation
- See [README.md](README.md) for test patterns and examples
- See [../../CLAUDE.md](../../CLAUDE.md) for project guidelines

---

**Last Updated**: 2026-01-30
**Version**: 1.0.0
