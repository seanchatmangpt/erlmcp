# ErlMCP Test Suite Organization

**Quick Reference Guide**

## Directory Structure

```
apps/*/test/      - Unit tests (EUnit) and integration tests (CT) per application
test/             - Legacy top-level integration tests (being phased out)
test_destructive/ - Destructive tests (memory exhaustion, chaos)
docs/testing/     - Test documentation and coverage reports
scripts/test/     - Test automation scripts
```

## Test Types

### EUnit Tests (`*_tests.erl`)
- **Purpose**: Unit tests following Chicago School TDD
- **Location**: `apps/*/test/*_tests.erl`
- **Pattern**: One test file per module (`<module>_tests.erl`)
- **Run**: `rebar3 eunit --module=<module>_tests`
- **Count**: 73 test files

### Common Test Suites (`*_SUITE.erl`)
- **Purpose**: Integration tests with multi-process scenarios
- **Location**: `apps/*/test/*_SUITE.erl`
- **Run**: `rebar3 ct --suite=<suite>`
- **Count**: 16 test suites

### Property Tests (`*_proper_tests.erl`)
- **Purpose**: Property-based testing with PropEr
- **Location**: `apps/*/test/*_proper_tests.erl`
- **Run**: `rebar3 proper --module=<module>_proper_tests`
- **Count**: 1 test file (needs expansion to 10+)

### Benchmark Tests
- **Purpose**: Performance benchmarks
- **Location**: `bench/erlmcp_bench_*.erl`
- **Run**: `./scripts/bench/run_all_benchmarks.sh`
- **Count**: 5 benchmark modules (core_ops, network_real, stress, chaos, integration)

## Quick Commands

```bash
# Run all tests
rebar3 do eunit, ct

# Run with coverage
rebar3 do eunit, ct, cover --verbose

# View coverage report (after running cover)
open _build/test/cover/index.html  # macOS
xdg-open _build/test/cover/index.html  # Linux

# Run specific test module
rebar3 eunit --module=erlmcp_client_tests

# Run specific CT suite
rebar3 ct --suite=erlmcp_integration_SUITE

# Run property tests
rebar3 proper --module=erlmcp_json_rpc_proper_tests

# Quality gates (pre-commit hook)
.git/hooks/pre-commit

# Post-task validation (Claude agent)
.claude/hooks/post-task-validate.sh
```

## Coverage Targets

| Module Type | Target Coverage | Priority |
|-------------|----------------|----------|
| **Core modules** (client, server, registry, JSON-RPC) | 85%+ | P1 |
| **Transport modules** (stdio, tcp, http, websocket) | 80%+ | P2 |
| **Feature modules** (resource, tool, prompt) | 80%+ | P3 |
| **Support modules** (auth, cache, pagination) | 75%+ | P4 |
| **Overall** | 80%+ | **Enforced** |

## Test Patterns (Chicago School TDD)

### EUnit Test Template

```erlang
-module(my_module_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Setup/Teardown (Chicago School: use real application)

my_test_() ->
    {setup,
     fun setup_application/0,
     fun cleanup_application/1,
     fun(_) ->
         [
          ?_test(test_basic_operation()),
          ?_test(test_error_handling()),
          ?_test(test_edge_cases())
         ]
     end}.

setup_application() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),
    ok.

cleanup_application(_) ->
    application:stop(erlmcp_transports),
    application:stop(erlmcp_core),
    ok.

%%% Test Functions (Chicago School: verify state, not interactions)

test_basic_operation() ->
    %% Setup: Start real gen_server
    {ok, Pid} = my_module:start_link(),

    %% Exercise: Call real API
    ok = my_module:do_something(Pid, args),

    %% Verify: Check observable state (state-based verification)
    {ok, State} = my_module:get_state(Pid),
    ?assertEqual(expected_state, State),

    %% Teardown
    ok = my_module:stop(Pid).
```

### Common Test Suite Template

```erlang
-module(my_module_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [test_case_1, test_case_2].

init_per_suite(Config) ->
    %% Start real application (Chicago School: real system)
    application:ensure_all_started(erlmcp_core),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_core),
    ok.

test_case_1(_Config) ->
    %% Integration test with real processes
    {ok, Pid1} = module_a:start_link(),
    {ok, Pid2} = module_b:start_link(),

    %% Test interaction
    ok = module_a:send_to(Pid1, Pid2, message),

    %% Verify state (Chicago School)
    {ok, Messages} = module_b:get_messages(Pid2),
    [message] = Messages.
```

## Quality Gates (Enforced by Git Hooks)

**Pre-Commit Hook** (`.git/hooks/pre-commit`):
- ✅ Compilation MUST succeed (0 errors)
- ✅ Tests MUST pass (100% pass rate)
- ✅ Coverage MUST be ≥80% overall, ≥85% core
- ⚠️ Dialyzer warnings reported (not blocking)
- ⚠️ Xref warnings reported (not blocking)

**Post-Task Hook** (`.claude/hooks/post-task-validate.sh`):
- Validates agent work after task completion
- Generates quality gate reports in `test_results/quality_gates/`
- Blocks merge if quality gates fail

## Current Status

**Test Files:**
- 73 EUnit test files (unit tests)
- 16 Common Test suites (integration tests)
- 1 PropEr test file (property-based tests)
- 5 benchmark modules

**Coverage:**
- Current: ~0% (tests exist but not contributing to coverage)
- Target: 80%+ overall, 85%+ core modules
- Issue: Missing `application:ensure_all_started/1` in test setups

**Broken Files:**
- 24 .broken files (11 in apps/, 13 in test.bak/)
- Priority: Fix 8 P0/P1 broken files in Phase 1

## Implementation Roadmap

**Phase 1 (Week 1-2)**: Fix broken tests, add application setup
**Phase 2 (Week 3-4)**: Achieve 50% coverage
**Phase 3 (Week 5-8)**: Achieve 80%+ coverage, add property tests
**Phase 4 (Week 9-10)**: Consolidate and cleanup test structure
**Phase 5 (Week 11)**: Automated quality gates and CI/CD

See [test_strategy_plan.md](test_strategy_plan.md) for full details.

## Chicago School TDD Principles

**DO:**
- ✅ Use real gen_servers, real processes, real supervision
- ✅ Verify observable state changes (API results, message receipts)
- ✅ Test behaviors and outputs, not internal method calls
- ✅ Integrate components together whenever practical

**DON'T:**
- ❌ No mock objects (meck, mocking frameworks)
- ❌ No interaction verification (which methods were called)
- ❌ No stubbing of collaborators (use real implementations)

## Resources

**Documentation:**
- [test_strategy_plan.md](test_strategy_plan.md) - Comprehensive strategy with C4 diagrams
- [../otp-patterns.md](../otp-patterns.md) - OTP patterns and best practices
- [../../CLAUDE.md](../../CLAUDE.md) - Development guide

**External:**
- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [EUnit User's Guide](https://www.erlang.org/doc/apps/eunit/chapter.html)
- [Common Test User's Guide](https://www.erlang.org/doc/apps/common_test/basics_chapter.html)
- [PropEr Documentation](https://proper-testing.github.io/)

---

**Last Updated**: 2026-01-30
