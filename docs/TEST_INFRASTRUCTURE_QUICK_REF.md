# erlmcp Test Infrastructure - Quick Reference Card

**Last Updated**: 2026-01-29
**Status**: ✅ OPERATIONAL (8.6/10)

---

## 🚀 Quick Commands

### Run Tests
```bash
make test                    # All tests (eunit + ct)
make eunit                   # EUnit only
make ct                      # Common Test only
make coverage                # Coverage report
make check                   # Compile + test
make validate                # ALL quality gates (BLOCKING)
```

### Module-Specific
```bash
rebar3 eunit --module=erlmcp_auth_tests
rebar3 ct --suite=erlmcp_integration_SUITE
cd apps/erlmcp_core && rebar3 eunit
```

### Quality Enforcement
```bash
make validate-compile        # Compilation check
make validate-test           # Test check
make validate-coverage       # Coverage ≥80%
make validate-quality        # Dialyzer + Xref
make test-strict             # Block on test failures
make coverage-strict         # Block if <80%
```

---

## 📊 Test Statistics

| Metric | Count | Status |
|--------|-------|--------|
| **EUnit Tests** | 58 files | ✅ |
| **CT Suites** | 7 files | ✅ |
| **Total Tests** | 65 files | ✅ |
| **CI Workflows** | 17 | ✅ |
| **Quality Gates** | 6 blocking | ✅ |
| **Coverage** | ~80% | ✅ |

---

## 🎯 Quality Gates

| Gate | Threshold | Blocking | Command |
|------|-----------|----------|---------|
| **1. Compilation** | 0 errors | ✅ Yes | `make validate-compile` |
| **2. Xref** | 0 undefined | ✅ Yes | `make validate-quality` |
| **3. Dialyzer** | 0 type errors | ✅ Yes | `make validate-quality` |
| **4. Unit Tests** | ≥90% pass | ✅ Yes | `make validate-test` |
| **5. Coverage** | ≥80% | ✅ Yes | `make validate-coverage` |
| **6. Quality CT** | 4 suites | ✅ Yes | `make validate-test` |
| **7. General CT** | Best effort | ❌ No | `make ct` |

---

## 📁 Test Structure

```
erlmcp/
├── test/                          # Root (mixed - needs cleanup)
│   ├── *_tests.erl                # EUnit tests (4 files)
│   └── gcp_simulator_*.erl        # Integration tests
│
├── apps/erlmcp_core/test/         # Core tests (48 files)
│   ├── erlmcp_auth_tests.erl      # ✅ 8/8 tests pass
│   ├── erlmcp_client_tests.erl
│   ├── erlmcp_server_tests.erl
│   └── ...
│
├── apps/erlmcp_transports/test/   # Transport tests (8 files)
│   ├── erlmcp_transport_stdio_tests.erl
│   ├── erlmcp_transport_tcp_tests.erl
│   └── *_SUITE.erl
│
└── apps/erlmcp_observability/test/# Observability tests (5 files)
    ├── erlmcp_metrics_tests.erl
    └── *_SUITE.erl
```

---

## 🔧 Dependencies

| Library | Version | Purpose | Status |
|---------|---------|---------|--------|
| **proper** | 1.4.0 | Property-based testing | ✅ Installed |
| **meck** | 0.9.2 | Mocking library | ✅ Installed |
| **coveralls** | 2.2.0 | Coverage reporting | ✅ Installed |
| **rebar3_proper** | 0.12.1 | Proper integration | ✅ Installed |

---

## 🎣 Git Hooks

### Pre-commit
```bash
.git/hooks/pre-commit
→ Runs: tools/claude-md-enforcer.sh
→ Checks: CLAUDE.md quality rules
→ Blocks: If rules violated
```

### Pre-push
```bash
.git/hooks/pre-push
→ Runs: tools/quality-gate.sh + benchmarks
→ Checks: Full quality + performance
→ Blocks: If regression >10%
```

### Bypass (Not Recommended)
```bash
git commit --no-verify    # Bypass pre-commit
git push --no-verify      # Bypass pre-push
```

---

## 🧪 Test Writing Quick Start

### EUnit Template
```erlang
-module(my_module_tests).
-include_lib("eunit/include/eunit.hrl").

my_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Test description", fun test_case/0}
     ]}.

setup() ->
    % Start real processes (Chicago School TDD)
    {ok, Pid} = my_module:start_link(),
    Pid.

cleanup(Pid) ->
    % Stop processes
    my_module:stop(Pid).

test_case() ->
    % Exercise
    ok = my_module:do_something(),
    % Verify (state-based, not interaction)
    ?assertEqual(expected, my_module:get_state()).
```

### Common Test Template
```erlang
-module(my_module_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [test_case_1, test_case_2].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp).

test_case_1(_Config) ->
    % Integration test with real processes
    {ok, Pid} = my_module:start_link(),
    ok = my_module:do_something(Pid),
    ?assertEqual(expected, my_module:get_state(Pid)).
```

### Proper Template
```erlang
prop_roundtrip() ->
    ?FORALL(Input, input_generator(),
        begin
            Encoded = my_module:encode(Input),
            {ok, Decoded} = my_module:decode(Encoded),
            Decoded =:= Input
        end).

input_generator() ->
    proper_types:binary().
```

---

## ⚠️ Known Issues

### Priority: HIGH
1. **Test Organization**
   - Mixed root test directory
   - Many `.skip` files need cleanup
   - Empty `test/chaos/` and `test/malformed/`

2. **Test Data Management**
   - No centralized fixtures
   - Hard-coded test data
   - No test data generators

### Priority: MEDIUM
3. **Compiler Warnings**
   - `compiler_warnings_as_errors = false`
   - Should be `true` for production

4. **Documentation**
   - No test writing guide
   - No testing standards doc

---

## 🔨 Immediate Actions

### 1. Clean Test Directories
```bash
find test -name "*.skip" -delete
mkdir -p test/unit test/integration test/stress
mv test/*_tests.erl test/unit/ 2>/dev/null || true
mv test/*_SUITE.erl test/integration/ 2>/dev/null || true
```

### 2. Create Test Fixtures
```bash
mkdir -p test/fixtures/data
# Create test/fixtures/test_data.hrl with common test data
```

### 3. Enable Compiler Warnings
```erlang
% In rebar.config test profile:
{compiler_warnings_as_errors, true}.
```

---

## 📚 Resources

### Documentation
- **Full Report**: `docs/TEST_INFRASTRUCTURE_VALIDATION_REPORT.md`
- **CLAUDE.md**: Project quality rules
- **Makefile**: `make help` for all commands

### Tools
- **rebar3**: https://www.rebar3.org/
- **Proper**: https://proper.softlab.ntua.gr/
- **Meck**: https://github.com/eproxus/meck

### Test Patterns
- **Chicago School TDD**: State-based, real collaborators
- **EUnit**: Unit tests, fast feedback
- **Common Test**: Integration, multi-process
- **Proper**: Property-based, invariants

---

## ✅ Verification Checklist

Before committing:
- [ ] All tests pass: `make test`
- [ ] Coverage ≥80%: `make validate-coverage`
- [ ] No compiler warnings: `make validate-compile`
- [ ] Dialyzer clean: `make validate-quality`
- [ ] Quality gates pass: `make validate`

Before releasing:
- [ ] All validation checks pass
- [ ] Performance benchmarks run
- [ ] Documentation updated
- [ ] CHANGELOG updated

---

**Status**: ✅ OPERATIONAL
**Score**: 8.6/10 - STRONG
**Next Review**: 2026-02-28

For detailed analysis, see: `docs/TEST_INFRASTRUCTURE_VALIDATION_REPORT.md`
