# erlmcp-flow Code Quality Standards

**Version:** 1.0.0
**Date:** 2026-02-01
**Status:** MANDATORY

## Executive Summary

This document defines the formal code quality standards for erlmcp-flow, integrating:
- **Chicago School TDD** (test-first, real processes, no mocks)
- **OTP Design Patterns** (gen_server, supervision, behaviors)
- **Joe Armstrong Principles** (let-it-crash, isolation, observable behavior)
- **TCPS Manufacturing** (Jidoka, Poka-Yoke, Andon)

---

## 1. Chicago School TDD Standards

### 1.1 Core Principles

**Definition**: Chicago School TDD emphasizes testing through real collaborators and observable behavior, rejecting mocks and implementation-driven tests.

| Principle | Rule | Enforcement |
|-----------|------|-------------|
| Real Processes | ∀test. uses_real_erlmcp_processes = true | CI: Anti-pattern scan |
| No Mocks | Mocks ∪ Fakes ∪ Placeholders ∪ Stubs = ∅ | CI: Mock detection |
| Observable Behavior | Test ⊨ Observable(Behavior) ⊬ Implementation | CI: sys:get_status detection |
| State-Based Assertions | Assert(State) ⊬ Assert(Interactions) | Manual code review |

### 1.2 Anti-Patterns (BLOCKING)

```erlang
% ❌ VIOLATION: Dummy process pattern
spawn(fun() -> receive stop -> ok end end)

% ✅ CORRECT: Real erlmcp process
{ok, Pid} = erlmcp_server:start_link(Opts)

% ❌ VIOLATION: State inspection
{status, _, _, [_, _, _, _, Misc]} = sys:get_status(Pid),
State = proplists:get_value(state, lists:last(Misc))

% ✅ CORRECT: Observable behavior
Response = erlmcp_client:call(Pid, Method, Params),
?assertEqual(expected_response, Response)

% ❌ VIOLATION: Mock framework
meck:new(erlmcp_registry),
meck:expect(erlmcp_registry, register, fun(_, _) -> ok end)

% ✅ CORRECT: Real registry
{ok, _RegPid} = erlmcp_registry:start_link(),
ok = erlmcp_registry:register(Name, Pid)

% ❌ VIOLATION: Test-specific state record
-record(state, {field1, field2}).  % In test file

% ✅ CORRECT: Use production record
-include("erlmcp_server.hrl").  % Production record
```

### 1.3 File Size Limits

- **Maximum:** 500 lines per test file
- **Rationale:** Large test files indicate tight coupling
- **Enforcement:** CI workflow (automatic rejection)

---

## 2. OTP Design Patterns

### 2.1 gen_server Requirements

```erlang
% ✅ MANDATORY: All 6 callbacks implemented
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

% ✅ MANDATORY: init/1 never blocks
init(Opts) ->
    % CORRECT: Async initialization
    self() ! initialize_async,
    {ok, #state{opts = Opts}}.

% ❌ VIOLATION: Blocking init/1
init(Opts) ->
    % WRONG: Blocking database connection
    {ok, Conn} = connect_to_database(Opts),  % Blocks!
    {ok, #state{conn = Conn}}.
```

### 2.2 Supervision Trees (3-Tier)

```erlang
% TIER 1: Root supervisor (one_for_all)
erlmcp_sup → {erlmcp_core_sup, erlmcp_registry}

% TIER 2: App supervisors (simple_one_for_one)
{server_sup, client_sup, session_sup} → isolated per-connection

% TIER 3: Isolated observability
erlmcp_observability_sup → {metrics, dashboard, tracing}

% ✅ MANDATORY: All processes supervised
{ok, Pid} = supervisor:start_child(MySup, ChildSpec).

% ❌ VIOLATION: Unsupervised spawn
Pid = spawn(fun() -> worker_loop() end).  % WRONG!
```

### 2.3 Behavior Modules

```erlang
% ✅ MANDATORY: Transport behavior implementation
-behaviour(erlmcp_transport).

-export([init/2, send/2, close/1]).

init(Type, Opts) ->
    % Initialize transport-specific state
    {ok, #state{type = Type, opts = Opts}}.

send(Data, State) ->
    % Send data through transport
    {ok, State}.

close(State) ->
    % Cleanup transport resources
    ok.
```

### 2.4 Process Isolation

| Pattern | Requirement | Example |
|---------|-------------|---------|
| Process-per-Connection | ∀c ∈ Conn. ∃!p ∈ GenServer. handles(p,c) | erlmcp_server per client |
| Request-ID Correlation | ∀req. ∃!id ∈ UUID. State.pending[id] = req | JSON-RPC 2.0 ID tracking |
| Let-It-Crash | failure(Child) → restart(Child) ⊬ failure(Sibling) | Supervision strategies |
| Monitoring | Use monitors for cleanup, not links | erlang:monitor(process, Pid) |

---

## 3. Joe Armstrong Principles

### 3.1 Let-It-Crash Philosophy

```erlang
% ✅ CORRECT: Let supervisor handle failures
handle_call({process, Data}, _From, State) ->
    % No defensive error handling
    Result = process_data(Data),  % Let it crash if invalid
    {reply, Result, State}.

% ❌ VIOLATION: Defensive programming
handle_call({process, Data}, _From, State) ->
    try
        Result = process_data(Data),
        {reply, {ok, Result}, State}
    catch
        error:Reason ->
            % WRONG: Swallowing errors
            {reply, {error, Reason}, State}
    end.

% ✅ CORRECT: Supervisor handles restart
ChildSpec = #{
    id => worker,
    start => {worker, start_link, [Opts]},
    restart => permanent,  % Always restart
    shutdown => 5000,
    type => worker,
    modules => [worker]
}.
```

### 3.2 Observable Behavior (Black-Box Testing)

```erlang
% ✅ CORRECT: Test observable behavior
test_server_echo() ->
    {ok, Pid} = erlmcp_server:start_link([{transport, stdio}]),
    Request = #{method => <<"echo">>, params => #{text => <<"hello">>}},
    Response = erlmcp_client:call(Pid, Request),
    ?assertEqual(#{result => #{text => <<"hello">>}}, Response).

% ❌ VIOLATION: Test implementation details
test_server_internal_state() ->
    {ok, Pid} = erlmcp_server:start_link([]),
    {status, _, _, [_, _, _, _, Misc]} = sys:get_status(Pid),  % WRONG!
    State = proplists:get_value(state, lists:last(Misc)),
    ?assertMatch(#state{initialized = true}, State).  % Testing internals
```

### 3.3 Armstrong-AGI Principle

**Build systems where incorrect behavior cannot exist.**

- Supervision → crash isolation impossible to violate
- Behaviors → type system enforces compliance
- Gates → violations impossible to commit
- Black-box → implementation hidden
- Chaos → resilience verified

---

## 4. Quality Checklist

### 4.1 Compilation Gate

```bash
# GATE 1: Zero compilation errors
TERM=dumb rebar3 compile

# Success criteria:
# - Exit code: 0
# - Errors: 0
# - Warnings: acceptable (non-blocking)

# Enforcement: CI (BLOCKING), pre-commit hook
```

### 4.2 Test Gate

```bash
# GATE 2: Test suite (EUnit + Common Test)
rebar3 eunit
rebar3 ct --dir=test/integration

# Success criteria:
# - Exit code: 0
# - Pass rate: ≥90%
# - Failures: 0 (for critical paths)

# Enforcement: CI (BLOCKING), make test-strict
```

### 4.3 Coverage Gate

```bash
# GATE 3: Code coverage
rebar3 cover --verbose
./scripts/check_coverage_threshold.sh 80

# Success criteria:
# - Overall coverage: ≥80%
# - Core modules: ≥85%
# - Public APIs: 100%

# Enforcement: CI (BLOCKING), make coverage-strict
```

### 4.4 Dialyzer Gate

```bash
# GATE 4: Type checking
rebar3 dialyzer

# Success criteria:
# - Exit code: 0
# - Type errors: 0
# - Warnings: 0

# Enforcement: CI (BLOCKING), make validate-quality
```

### 4.5 Xref Gate

```bash
# GATE 5: Cross-reference analysis
rebar3 xref

# Success criteria:
# - Exit code: 0
# - Undefined function calls: 0
# - Unused functions: acceptable (non-blocking)

# Enforcement: CI (BLOCKING), make validate-quality
```

### 4.6 Format Gate

```bash
# GATE 6: Code formatting
rebar3 format --verify

# Success criteria:
# - Exit code: 0
# - All files formatted according to .formatter.exs

# Enforcement: CI (non-blocking), pre-commit hook
```

### 4.7 Performance Gate

```bash
# GATE 7: Performance regression
./scripts/bench/check_regression.sh

# Success criteria:
# - Regression: <10%
# - Baseline comparisons pass

# Enforcement: CI (BLOCKING for releases), make validate-bench
```

---

## 5. CI/CD Integration

### 5.1 GitHub Actions Workflow

**File:** `.github/workflows/quality-gate.yml`

```yaml
name: Quality Gate

on:
  push:
    branches: [main, 'release/**', 'feature/**']
  pull_request:
    branches: [main, 'release/**']

jobs:
  comprehensive-quality-check:
    runs-on: ubuntu-22.04
    timeout-minutes: 45

    steps:
    - uses: actions/checkout@v4

    - name: Setup Erlang/OTP 28+
      uses: erlef/setup-beam@v1
      with:
        otp-version: 28
        rebar3-version: '3.25'

    # GATE 1: Compilation (BLOCKING)
    - name: Gate 1 - Compilation
      run: |
        TERM=dumb rebar3 compile 2>&1 | tee compile.log
        if [ ${PIPESTATUS[0]} -ne 0 ]; then
          echo "::error::Compilation FAILED - MERGE BLOCKED"
          exit 1
        fi

    # GATE 2: Xref (BLOCKING)
    - name: Gate 2 - Xref
      run: |
        rebar3 xref 2>&1 | tee xref.log
        if [ ${PIPESTATUS[0]} -ne 0 ]; then
          echo "::error::Xref FAILED - MERGE BLOCKED"
          exit 1
        fi

    # GATE 3: Dialyzer (BLOCKING)
    - name: Gate 3 - Dialyzer
      run: |
        rebar3 dialyzer 2>&1 | tee dialyzer.log
        if [ ${PIPESTATUS[0]} -ne 0 ]; then
          echo "::error::Dialyzer FAILED - MERGE BLOCKED"
          exit 1
        fi

    # GATE 4: Unit Tests (BLOCKING)
    - name: Gate 4 - Unit Tests
      run: |
        rebar3 as test do compile, eunit --cover
        if [ $? -ne 0 ]; then
          echo "::error::Unit tests FAILED - MERGE BLOCKED"
          exit 1
        fi

    # GATE 5: Coverage (BLOCKING)
    - name: Gate 5 - Coverage
      run: |
        rebar3 cover --verbose
        ./scripts/check_coverage_threshold.sh 80
        if [ $? -ne 0 ]; then
          echo "::error::Coverage <80% - MERGE BLOCKED"
          exit 1
        fi

    # GATE 6: Chicago TDD Compliance
    - name: Gate 6 - Chicago TDD
      run: |
        ./.github/scripts/chicago-tdd-scan.sh
        if [ $? -ne 0 ]; then
          echo "::error::Chicago TDD violations - MERGE BLOCKED"
          exit 1
        fi
```

### 5.2 Pre-Commit Hooks

**File:** `.git/hooks/pre-commit`

```bash
#!/bin/bash
# erlmcp quality gate pre-commit hook

echo "🚦 Running quality gates..."

# Gate 1: Compilation
echo "Gate 1: Compilation..."
if ! TERM=dumb rebar3 compile > /dev/null 2>&1; then
    echo "❌ Compilation failed - commit blocked"
    exit 1
fi

# Gate 2: Tests (fast smoke tests only)
echo "Gate 2: Smoke tests..."
if ! rebar3 eunit --module=erlmcp_json_rpc_tests > /dev/null 2>&1; then
    echo "❌ Smoke tests failed - commit blocked"
    exit 1
fi

# Gate 3: Format check
echo "Gate 3: Format check..."
if ! rebar3 format --verify > /dev/null 2>&1; then
    echo "⚠️  Format violations detected - auto-formatting..."
    rebar3 format
    git add -u  # Stage formatted files
fi

# Gate 4: Chicago TDD anti-patterns
echo "Gate 4: Chicago TDD scan..."
if grep -r "meck:new\|sys:get_status\|spawn(fun() -> receive" apps/*/test/ > /dev/null 2>&1; then
    echo "❌ Chicago TDD violations detected - commit blocked"
    echo "Run: make help | grep chicago"
    exit 1
fi

echo "✅ All pre-commit gates passed"
exit 0
```

### 5.3 Makefile Integration

```makefile
# Quality gates (BLOCKING)
validate: validate-compile validate-test validate-coverage validate-quality
	@echo "✅ ALL QUALITY GATES PASSED"

validate-compile:
	@echo "🔨 Gate: Compilation"
	@TERM=dumb rebar3 compile || exit 1

validate-test:
	@echo "🧪 Gate: Tests"
	@rebar3 eunit || exit 1
	@rebar3 ct || exit 1

validate-coverage:
	@echo "📊 Gate: Coverage ≥80%"
	@rebar3 cover --verbose
	@./scripts/check_coverage_threshold.sh 80 || exit 1

validate-quality:
	@echo "🔍 Gate: Dialyzer + Xref"
	@rebar3 dialyzer || exit 1
	@rebar3 xref || exit 1

validate-bench:
	@echo "⚡ Gate: Performance"
	@./scripts/bench/check_regression.sh || exit 1

# Chicago TDD validation
validate-chicago-tdd:
	@echo "📚 Gate: Chicago School TDD"
	@./.github/scripts/chicago-tdd-scan.sh || exit 1
```

---

## 6. Quality Metrics

### 6.1 Mandatory Thresholds

| Metric | Threshold | Gate | Enforcement |
|--------|-----------|------|-------------|
| Compilation errors | 0 | BLOCKING | CI, pre-commit |
| Test failures | 0 | BLOCKING | CI |
| Test pass rate | ≥90% | BLOCKING | CI |
| Code coverage | ≥80% | BLOCKING | CI, pre-commit |
| Core module coverage | ≥85% | BLOCKING | CI |
| Public API coverage | 100% | BLOCKING | CI |
| Dialyzer warnings | 0 | BLOCKING | CI |
| Xref undefined calls | 0 | BLOCKING | CI |
| Performance regression | <10% | BLOCKING (releases) | CI |
| File size | <500 lines | BLOCKING | CI |

### 6.2 Code Review Checklist

**Before approving any PR:**

- [ ] All 6 gen_server callbacks implemented
- [ ] Supervision tree properly configured
- [ ] No unsupervised spawn calls
- [ ] init/1 never blocks
- [ ] Monitors used instead of links for cleanup
- [ ] No mocks/fakes/stubs in tests
- [ ] Tests use real erlmcp processes
- [ ] No sys:get_status in tests
- [ ] Test files <500 lines
- [ ] All public functions have type specs
- [ ] Coverage ≥80%
- [ ] No dialyzer warnings
- [ ] No undefined function calls
- [ ] Format verification passes

---

## 7. Enforcement Mechanisms

### 7.1 Automated (CI/CD)

```bash
# GitHub Actions (BLOCKING)
- Compilation gate
- Xref gate
- Dialyzer gate
- Test gate (≥90% pass rate)
- Coverage gate (≥80%)
- Chicago TDD anti-pattern scan
- Format verification

# Pre-commit hooks (BLOCKING)
- Compilation
- Smoke tests
- Format auto-fix
- Chicago TDD anti-pattern scan
```

### 7.2 Manual (Code Review)

```markdown
## Code Review Checklist

### OTP Patterns
- [ ] gen_server: All 6 callbacks
- [ ] Supervision: Proper restart strategies
- [ ] Monitoring: Monitors for cleanup
- [ ] Error handling: All code paths covered
- [ ] Type specs: All public functions

### Chicago TDD
- [ ] Real collaborators (no mocks)
- [ ] State-based assertions
- [ ] Integration tests where practical
- [ ] Observable behavior testing
- [ ] File size <500 lines

### Documentation
- [ ] Function specs (-spec)
- [ ] Module docstrings
- [ ] README updated (if public API changes)
- [ ] CHANGELOG.md entry
```

### 7.3 TCPS Quality System

```bash
# Jidoka (built-in quality)
make jidoka  # 8 quality gates, stop-the-line on failure

# Poka-Yoke (error-proofing)
make poka-yoke  # 8 error-proofing checks

# Andon (visual alert system)
make andon  # Show quality status dashboard
make andon-watch  # Real-time monitoring

# Quality receipt (release certification)
make release-validate  # Generate quality receipt
```

---

## 8. Violation Examples & Fixes

### 8.1 Mock Usage Violation

```erlang
% ❌ VIOLATION
test_with_mock() ->
    meck:new(erlmcp_registry),
    meck:expect(erlmcp_registry, lookup, fun(_) -> {ok, self()} end),
    % ... test code
    meck:unload(erlmcp_registry).

% ✅ FIX: Use real registry
test_with_real_registry() ->
    {ok, RegPid} = erlmcp_registry:start_link(),
    ok = erlmcp_registry:register(test_name, self()),
    {ok, Pid} = erlmcp_registry:lookup(test_name),
    ?assertEqual(self(), Pid),
    erlmcp_registry:stop(RegPid).
```

### 8.2 Dummy Process Violation

```erlang
% ❌ VIOLATION
test_with_dummy() ->
    DummyPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    % ... test code
    DummyPid ! stop.

% ✅ FIX: Use real gen_server
test_with_real_server() ->
    {ok, Pid} = erlmcp_server:start_link([{transport, stdio}]),
    % ... test code
    erlmcp_server:stop(Pid).
```

### 8.3 State Inspection Violation

```erlang
% ❌ VIOLATION
test_state_inspection() ->
    {ok, Pid} = erlmcp_server:start_link([]),
    {status, _, _, [_, _, _, _, Misc]} = sys:get_status(Pid),
    State = proplists:get_value(state, lists:last(Misc)),
    ?assertMatch(#state{initialized = true}, State).

% ✅ FIX: Test observable behavior
test_observable_behavior() ->
    {ok, Pid} = erlmcp_server:start_link([]),
    % Verify initialization through observable behavior
    Response = erlmcp_client:call(Pid, initialize, #{}),
    ?assertEqual(ok, Response).
```

---

## 9. References

### 9.1 Internal Documentation

- `CLAUDE.md` - Formal specification
- `docs/architecture/OTP_PATTERNS.md` - OTP design patterns
- `docs/testing/CHICAGO_TDD.md` - Chicago School TDD guide
- `.claude/TCPS_SYSTEM_COMPLETE.md` - TCPS manufacturing system

### 9.2 External Resources

- [Growing Object-Oriented Software, Guided by Tests](http://www.growing-object-oriented-software.com/) - Freeman & Pryce
- [Erlang OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [Joe Armstrong's Thesis](https://erlang.org/download/armstrong_thesis_2003.pdf) - Making reliable distributed systems

---

## 10. Appendix: Complete CI/CD Pipeline

### 10.1 Full GitHub Actions Workflow

```yaml
name: Comprehensive Quality Pipeline

on: [push, pull_request]

jobs:
  quality-gates:
    runs-on: ubuntu-22.04
    steps:
    - uses: actions/checkout@v4
    - uses: erlef/setup-beam@v1
      with:
        otp-version: 28
        rebar3-version: '3.25'

    # Parallel execution of independent gates
    - name: Parallel Gate Execution
      run: |
        make validate-compile &
        make validate-test &
        make validate-coverage &
        make validate-quality &
        wait

        # All gates must pass
        if [ $? -ne 0 ]; then
          echo "::error::Quality gates FAILED"
          exit 1
        fi

    # Chicago TDD compliance
    - name: Chicago TDD Compliance
      run: make validate-chicago-tdd

    # Performance regression (releases only)
    - name: Performance Gate
      if: startsWith(github.ref, 'refs/tags/v')
      run: make validate-bench

    # Generate quality receipt
    - name: Quality Receipt
      if: success()
      run: make release-validate
```

### 10.2 Quality Metrics Dashboard

```bash
# Real-time quality metrics
make metrics-snapshot    # Capture current snapshot
make metrics-trend       # 30-day trend analysis
make metrics-report      # Comprehensive report

# CI integration
make metrics-ci          # Snapshot + regression check
```

---

**Document Version:** 1.0.0
**Last Updated:** 2026-02-01
**Maintained By:** erlmcp-flow Core Team
**Review Cycle:** Quarterly
