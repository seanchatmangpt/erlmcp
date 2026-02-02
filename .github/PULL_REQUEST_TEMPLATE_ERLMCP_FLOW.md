---
name: erlmcp-flow Pull Request
about: PR template for erlmcp-flow agent coordination transport
---

# erlmcp-flow Pull Request

## Summary
<!-- Describe the changes in 1-2 sentences -->

This PR implements _________ for erlmcp-flow to enable _________.

## Type of Change
- [ ] New module (registry, router, transport, bridge, etc.)
- [ ] Bug fix (non-breaking change which fixes an issue)
- [ ] New feature (non-breaking change which adds functionality)
- [ ] Performance improvement
- [ ] Code refactoring
- [ ] Test improvement
- [ ] Documentation update

## erlmcp-flow Component
<!-- Which erlmcp-flow component does this PR affect? -->

- [ ] **Registry** (`erlmcp_flow_registry.erl`) - O(log N) agent lookup
- [ ] **Router** (`erlmcp_flow_router.erl`) - Direct/broadcast/gossip messaging
- [ ] **Transport** (`erlmcp_flow_transport.erl`) - Transport behavior implementation
- [ ] **Agent** (`erlmcp_flow_agent.erl`) - Agent gen_server
- [ ] **Bridges** (`erlmcp_flow_*_bridge.erl`) - stdio/TCP/HTTP integration
- [ ] **Flow Control** (`erlmcp_flow_backpressure.erl`) - Token bucket backpressure
- [ ] **Serializer** (`erlmcp_flow_serializer.erl`) - Message serialization
- [ ] **Supervisor** (`erlmcp_flow_sup.erl`, `erlmcp_flow_agent_sup.erl`) - OTP supervision
- [ ] **Benchmarks** (`erlmcp_flow_bench.erl`) - Performance validation
- [ ] **Documentation** - Architecture, examples, guides

## Related Issues
Fixes #
Relates to #
Blocks #
Part of EPIC #

## Implementation Phase
<!-- Which implementation phase does this PR belong to? (see erlmcp-flow-implementation-plan.md) -->

- [ ] **Phase 1**: Registry (8h) - O(log N) lookup with gproc
- [ ] **Phase 2**: Router (12h) - Direct/broadcast/gossip patterns
- [ ] **Phase 3**: Transport (10h) - Behavior compliance
- [ ] **Phase 4**: Bridges (8h) - stdio/TCP/HTTP integration
- [ ] **Phase 5**: Flow Control (6h) - Backpressure mechanisms
- [ ] **Phase 6**: Benchmarks (4h) - Performance validation

## Changes Made

### Core Implementation
<!-- List main code changes -->

**Modules Added/Modified**:
- `apps/erlmcp_flow/src/erlmcp_flow_*.erl`:
  -
  -

**API Changes**:
- New functions:
  -
  -
- Modified functions:
  -
  -

### Tests (Chicago TDD)
<!-- Real processes, no mocks -->

**EUnit Tests** (`apps/erlmcp_flow/test/*_tests.erl`):
- [ ] Basic functionality tests
- [ ] Edge case tests
- [ ] Error handling tests
- [ ] Crash recovery tests
- [ ] Performance tests

**Common Test** (`apps/erlmcp_flow/test/*_SUITE.erl`):
- [ ] Integration tests with other erlmcp_flow modules
- [ ] Integration tests with erlmcp core
- [ ] Transport integration tests
- [ ] End-to-end workflow tests

**Test Coverage**:
- Total: _____% (target: ≥82%)
- New modules: _____% (target: ≥85%)

### Documentation
- [ ] Function specs with types
- [ ] Module documentation
- [ ] Architecture doc updated
- [ ] Examples added/updated
- [ ] CHANGELOG.md updated

## Quality Gates

### Compilation
- [ ] ✅ 0 errors
- [ ] ✅ 0 warnings (or justified)
- [ ] ✅ All erlmcp_flow modules compile

```bash
cd apps/erlmcp_flow
TERM=dumb rebar3 compile
# Result: ✅ 0 errors
```

### Tests
- [ ] ✅ EUnit: 100% pass rate
- [ ] ✅ Common Test: 100% pass rate
- [ ] ✅ Coverage: ≥82% (actual: _____%)
- [ ] ✅ Chicago TDD: Real processes, no mocks

```bash
rebar3 eunit --app erlmcp_flow
# Result: ✅ All passed

rebar3 ct --dir apps/erlmcp_flow/test
# Result: ✅ All passed

rebar3 cover
# Result: ✅ Coverage: _____%
```

### Type Safety & Static Analysis
- [ ] ✅ Dialyzer: 0 errors
- [ ] ✅ Xref: 0 undefined functions
- [ ] ✅ Specs: Complete on public APIs
- [ ] ✅ Type definitions in erlmcp_flow.hrl

```bash
cd apps/erlmcp_flow
rebar3 dialyzer
# Result: ✅ 0 errors

rebar3 xref
# Result: ✅ 0 undefined functions
```

### Code Quality
- [ ] ✅ Formatted with `rebar3 format`
- [ ] ✅ No hardcoded values (use application env)
- [ ] ✅ OTP patterns: gen_server, supervision, let-it-crash
- [ ] ✅ Proper error handling (crash vs return error)

```bash
cd apps/erlmcp_flow
rebar3 format --check
# Result: ✅ Formatted
```

### Performance (if applicable)
- [ ] ✅ Benchmarks run
- [ ] ✅ Performance targets met
- [ ] ✅ No regression >10%

**Benchmark Results**:

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Agent lookup (p50) | <10μs | _____μs | ✅/❌ |
| Agent lookup (p95) | <50μs | _____μs | ✅/❌ |
| Agent lookup (p99) | <100μs | _____μs | ✅/❌ |
| Direct messaging | >100K msg/sec | _____K msg/sec | ✅/❌ |
| Broadcast (60 agents) | <10ms avg | _____ms | ✅/❌ |
| Registry throughput | >500K lookups/sec | _____K lookups/sec | ✅/❌ |

```bash
rebar3 eunit --module erlmcp_flow_bench
# See benchmark results above
```

## Test Plan

### Unit Tests (Chicago TDD)
- [ ] All existing tests pass
- [ ] New tests added for new functionality
- [ ] Edge cases covered
- [ ] Error conditions tested
- [ ] Crash recovery tested

**Test Modules**:
- `apps/erlmcp_flow/test/erlmcp_flow_*_tests.erl`

### Integration Tests
- [ ] Integration with erlmcp_flow registry
- [ ] Integration with erlmcp_flow router
- [ ] Integration with erlmcp core (if applicable)
- [ ] Transport bridge integration (if applicable)

**Test Suites**:
- `apps/erlmcp_flow/test/erlmcp_flow_*_SUITE.erl`

### Manual Testing
- [ ] Tested in local environment
- [ ] Tested with multiple agents (if applicable)
- [ ] Tested with different messaging patterns (if applicable)
- [ ] Tested crash recovery scenarios

**Manual Verification Checklist**:
1.
2.
3.

### Performance Testing
- [ ] Benchmarks run on dedicated hardware
- [ ] Performance targets validated
- [ ] Regression check passed (<10%)

## OTP Compliance

### gen_server Compliance
- [ ] ✅ `init/1` never blocks (async cast if needed)
- [ ] ✅ `handle_call/3` has proper timeout handling
- [ ] ✅ `handle_cast/2` returns quickly
- [ ] ✅ `handle_info/2` handles all expected messages
- [ ] ✅ `terminate/2` cleans up resources
- [ ] ✅ `code_change/3` implemented for hot code reload

### Supervision Compliance
- [ ] ✅ All processes supervised (no unsupervised spawn)
- [ ] ✅ Supervision strategy appropriate (one_for_all, one_for_one, simple_one_for_one)
- [ ] ✅ Restart strategy appropriate (permanent, transient, temporary)
- [ ] ✅ Child spec properly defined
- [ ] ✅ Let-it-crash philosophy followed

### Process Isolation
- [ ] ✅ Process-per-agent (if applicable)
- [ ] ✅ No shared mutable state
- [ ] ✅ Message passing for communication
- [ ] ✅ Crash isolation (supervisor prevents cascade failures)

## Breaking Changes

<!-- List any breaking changes -->

### API Changes
- None

### Configuration Changes
- None

### Migration Required
- None

## Architecture Impact

### Integration with erlmcp Core
- [ ] No breaking changes to erlmcp core
- [ ] Uses `erlmcp_registry` for lookups (if needed)
- [ ] Uses `erlmcp_json_rpc` for serialization (if needed)
- [ ] Reports metrics via `erlmcp_metrics` (if applicable)

### Integration with erlmcp Transports
- [ ] stdio integration tested
- [ ] TCP integration tested
- [ ] HTTP integration tested

### Performance Impact
- [ ] No performance degradation to existing erlmcp components
- [ ] erlmcp_flow meets performance targets
- [ ] Resource usage acceptable (memory, CPU)

## Documentation

### Code Documentation
- [ ] ✅ All public functions have specs
- [ ] ✅ Complex logic documented
- [ ] ✅ Module overview documentation
- [ ] ✅ Type definitions in erlmcp_flow.hrl

### Architecture Documentation
- [ ] Architecture doc updated (`docs/erlmcp-flow-architecture.md`)
- [ ] Examples updated (`docs/erlmcp-flow-examples.md`)
- [ ] README updated (`docs/ERLMCP-FLOW-README.md`)

### CHANGELOG.md
- [ ] ✅ Entry added with version, date, changes

**CHANGELOG Entry**:
```markdown
## [Unreleased] - 2026-02-02

### Added
- erlmcp_flow_registry: O(log N) agent lookup with gproc

### Changed
- None

### Fixed
- None
```

## Reviewer Checklist

### Code Quality
- [ ] OTP patterns correctly applied
- [ ] No mocks (Chicago TDD with real processes)
- [ ] Specs and types complete
- [ ] Error handling appropriate (crash vs return error)
- [ ] No hardcoded values

### Tests
- [ ] Test coverage ≥82%
- [ ] Tests are deterministic (no sleeps, no race conditions)
- [ ] Edge cases covered
- [ ] Integration tests present

### Performance
- [ ] Benchmarks meet targets
- [ ] No obvious anti-patterns
- [ ] Memory leak checked (long-running test)

### Documentation
- [ ] Public functions documented
- [ ] Architecture docs updated if needed
- [ ] CHANGELOG.md updated

### CI/CD
- [ ] All quality gates passed
- [ ] No new warnings

## Additional Notes
<!-- Any additional information for reviewers -->

**Implementation Notes**:
-
-

**Known Limitations**:
-
-

**Future Enhancements**:
-
-

## Reviewers
<!-- Tag reviewers -->

@erlang-otp-developer @erlang-transport-builder @code-reviewer

## Pre-Merge Checklist

Before requesting merge, ensure:

- [ ] ✅ All quality gates passed in CI
- [ ] ✅ 2+ approvals received
- [ ] ✅ Branch up-to-date with main
- [ ] ✅ No merge conflicts
- [ ] ✅ Documentation complete
- [ ] ✅ CHANGELOG.md updated

---

## Quality Gate Results

**Local Validation**:

```bash
# Compilation
cd apps/erlmcp_flow
TERM=dumb rebar3 compile
# Result: ✅ 0 errors

# Tests
rebar3 eunit --app erlmcp_flow
# Result: ✅ All passed

# Coverage
rebar3 cover
# Result: ✅ Coverage: _____%

# Dialyzer
rebar3 dialyzer
# Result: ✅ 0 errors

# Xref
rebar3 xref
# Result: ✅ 0 undefined functions

# Format
rebar3 format --check
# Result: ✅ Formatted

# Benchmarks (if applicable)
rebar3 eunit --module erlmcp_flow_bench
# Result: ✅ Targets met
```

**CI Validation**:

See GitHub Actions workflow results in PR checks.

---

**Session Link**: https://claude.ai/code/session_{session_id}

**Implementation Time**: _____ hours (Phase ___ / 48h total)
