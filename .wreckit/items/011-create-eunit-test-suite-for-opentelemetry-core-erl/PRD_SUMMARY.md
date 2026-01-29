# PRD Summary - EUnit Test Suite for OpenTelemetry Core

**Item ID**: 011-create-eunit-test-suite-for-opentelemetry-core-erl
**Branch**: wreckit/011-create-eunit-test-suite-for-opentelemetry-core-erl
**Total Stories**: 11 user stories
**Priority Distribution**: 5 Critical (P1), 6 High/Medium (P2/P3)

## User Stories Breakdown

### Priority 1 (Critical) - 5 Stories

1. **US-011-001**: Implement W3C Trace Context compliance tests (CRITICAL)
   - 10 W3C traceparent format validation tests
   - Tests for valid/invalid format detection, round-trip preservation
   - MANDATORY foundation - all other phases depend on this

2. **US-011-002**: Implement core API testing
   - 10 core API tests (init, span lifecycle, context management)
   - Tests for ID generation and uniqueness validation

3. **US-011-011**: Verify overall coverage ≥80% and all quality gates pass
   - FINAL verification story
   - All quality gates must pass: compile, eunit, cover, dialyzer, xref

### Priority 2 (High) - 4 Stories

4. **US-011-003**: Implement span hierarchy and attributes testing (5 tests)
5. **US-011-004**: Implement error recording and status testing (6 tests)
6. **US-011-005**: Implement baggage and correlation testing (5 tests)
7. **US-011-006**: Implement context propagation testing (3 tests)

### Priority 3 (Medium) - 3 Stories

8. **US-011-007**: Implement event tracking testing (4 tests)
9. **US-011-008**: Implement sampling strategies testing (6 tests)
10. **US-011-009**: Implement RPC integration testing (5 tests)
11. **US-011-010**: Implement edge cases and error handling testing (6 tests)

## Total Test Count

**59 tests across 10 test groups:**
- W3C Trace Context: 10 tests
- Core API: 10 tests
- Span Hierarchy: 5 tests
- Error Recording: 6 tests
- Baggage: 5 tests
- Context Propagation: 3 tests
- Event Tracking: 4 tests
- Sampling: 6 tests
- RPC Integration: 5 tests
- Edge Cases: 6 tests

## Quality Gates (ALL MANDATORY)

Every story must pass:
1. **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
2. **EUnit**: `rebar3 eunit --module=erlmcp_otel_tests` - 100% pass rate
3. **Coverage**: `rebar3 cover -v apps/erlmcp_observability` - ≥80%
4. **Dialyzer**: `rebar3 dialyzer -r apps/erlmcp_observability` - 0 warnings
5. **Xref**: `rebar3 xref` - 0 undefined function calls

## TCPS Compliance

- **Standard Work**: Every step documented with measurable acceptance criteria
- **Heijunka**: 10 small phases (≤4 hours each), independently verifiable
- **Poka-yoke**: Quality gates built into every story, failures stop the line
- **Andon**: Progress visible via test results, coverage reports
- **Jidoka**: Zero defects - 100% test pass rate required

## Manufacturing Output

- **Test File**: `apps/erlmcp_observability/test/erlmcp_otel_tests.erl` (expanded to ~800-1000 lines)
- **Coverage**: ≥80% for erlmcp_otel.erl (1,005 lines)
- **W3C Compliance**: 100% traceparent format validation
- **Documentation**: Coverage report (HTML), test execution logs

## Dependencies

- **Phase 1 (US-011-001)** must complete first - W3C compliance is foundation
- Phases 2-10 can proceed in parallel after Phase 1
- **US-011-011** is final verification - all other stories must complete first

## Success Criteria

✓ All 59 tests passing (100% pass rate)
✓ Coverage ≥80% (verified in cover report)
✓ W3C traceparent format validated (10 explicit tests)
✓ All 31 exported functions covered
✓ Dialyzer 0 warnings
✓ Xref 0 undefined function calls
✓ Code review: OTP patterns verified
