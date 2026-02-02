# Quality Standards Quick Reference

**Version:** 1.0.0
**For:** erlmcp-flow developers
**Last Updated:** 2026-02-01

## TL;DR - Quality Gates Checklist

Before committing code, ensure:

```bash
✅ make compile           # 0 errors
✅ make test             # 0 failures
✅ make coverage         # ≥80%
✅ make dialyzer         # 0 warnings
✅ make xref             # 0 undefined calls
✅ make format --verify  # All files formatted
```

---

## Quick Commands

### Daily Development

```bash
make doctor              # Check environment health
make quick               # Fast check: compile + smoke tests (<5min)
make verify              # Full validation before PR (<15min)
make ci-local            # Reproduce exact CI workflow
```

### Quality Gates

```bash
make validate            # Run ALL quality gates (BLOCKING)
make validate-compile    # Compilation only
make validate-test       # Tests only
make validate-coverage   # Coverage ≥80%
make validate-quality    # Dialyzer + Xref
make validate-bench      # Performance regression
```

### Installation

```bash
# Install pre-commit hooks (run once)
./scripts/install-quality-hooks.sh

# Verify hooks installed
ls -la .git/hooks/pre-commit
ls -la .git/hooks/pre-push
```

---

## Quality Standards Summary

### 1. Chicago School TDD

| ✅ Required | ❌ Prohibited |
|-------------|---------------|
| Real erlmcp processes | Mocks (meck) |
| State-based assertions | sys:get_status |
| Observable behavior testing | Dummy spawn processes |
| Integration tests | Test-specific state records |
| Test files <500 lines | Fakes/stubs/placeholders |

**Example:**

```erlang
% ❌ WRONG: Mock
meck:new(erlmcp_registry),
meck:expect(erlmcp_registry, lookup, fun(_) -> {ok, self()} end)

% ✅ CORRECT: Real process
{ok, RegPid} = erlmcp_registry:start_link(),
ok = erlmcp_registry:register(test_name, self()),
{ok, Pid} = erlmcp_registry:lookup(test_name)
```

### 2. OTP Patterns

| Pattern | Requirement |
|---------|-------------|
| gen_server | All 6 callbacks implemented |
| Supervision | All processes supervised |
| init/1 | Never blocks (use async cast) |
| Monitoring | Use monitors for cleanup, not links |
| Behaviors | Implement full behavior contract |

**Example:**

```erlang
% ✅ CORRECT: Non-blocking init
init(Opts) ->
    self() ! initialize_async,
    {ok, #state{opts = Opts}}.

% ❌ WRONG: Blocking init
init(Opts) ->
    {ok, Conn} = connect_to_database(Opts),  % Blocks!
    {ok, #state{conn = Conn}}.
```

### 3. Coverage Requirements

| Module Type | Coverage | Gate |
|-------------|----------|------|
| Overall | ≥80% | BLOCKING |
| Core modules | ≥85% | BLOCKING |
| Public APIs | 100% | BLOCKING |

### 4. Type Specifications

```erlang
% ✅ REQUIRED: All public functions must have specs
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).
```

---

## CI/CD Integration

### GitHub Actions Workflow

```yaml
# .github/workflows/quality-gate.yml
jobs:
  quality-gates:
    runs-on: ubuntu-22.04
    steps:
    - uses: actions/checkout@v4
    - uses: erlef/setup-beam@v1
      with:
        otp-version: 28

    # 6 BLOCKING gates
    - run: TERM=dumb rebar3 compile
    - run: rebar3 xref
    - run: rebar3 dialyzer
    - run: rebar3 eunit --cover
    - run: rebar3 cover && ./scripts/check_coverage_threshold.sh 80
    - run: ./.github/scripts/chicago-tdd-scan.sh
```

### Pre-Commit Hooks

Installed via: `./scripts/install-quality-hooks.sh`

**Hooks:**
1. `pre-commit` - Compilation, smoke tests, format, Chicago TDD
2. `commit-msg` - Message format validation
3. `pre-push` - Full tests, coverage, dialyzer

---

## Anti-Pattern Detection

### Automated Scans (CI)

```bash
# Run Chicago TDD scanner
./.github/scripts/chicago-tdd-scan.sh

# Detects:
# - Dummy process patterns
# - sys:get_status usage
# - Record duplication
# - Mock framework usage
# - Stub/fake modules
```

### Manual Detection

```bash
# Check for mocks
grep -r "meck:new\|meck:expect" apps/*/test/

# Check for state inspection
grep -r "sys:get_status\|sys:get_state" apps/*/test/

# Check for dummy processes
grep -r -E "spawn\(fun\(\) -> receive" apps/*/test/
```

---

## Code Review Checklist

**Before approving any PR:**

### OTP Compliance
- [ ] All 6 gen_server callbacks implemented
- [ ] Supervision tree properly configured
- [ ] No unsupervised spawn calls
- [ ] init/1 never blocks
- [ ] Monitors used for cleanup

### Chicago TDD Compliance
- [ ] No mocks/fakes/stubs
- [ ] Tests use real erlmcp processes
- [ ] No sys:get_status in tests
- [ ] Test files <500 lines
- [ ] Observable behavior testing

### Quality Metrics
- [ ] Coverage ≥80%
- [ ] All public functions have type specs
- [ ] No dialyzer warnings
- [ ] No undefined function calls
- [ ] Format verification passes

### Documentation
- [ ] Function specs (-spec)
- [ ] Module docstrings
- [ ] README updated (if API changes)
- [ ] CHANGELOG.md entry

---

## Common Violations & Fixes

### Violation 1: Mock Usage

```erlang
% ❌ VIOLATION
test_with_mock() ->
    meck:new(erlmcp_registry),
    meck:expect(erlmcp_registry, lookup, fun(_) -> {ok, self()} end),
    % ... test code
    meck:unload(erlmcp_registry).

% ✅ FIX
test_with_real_registry() ->
    {ok, RegPid} = erlmcp_registry:start_link(),
    ok = erlmcp_registry:register(test_name, self()),
    {ok, Pid} = erlmcp_registry:lookup(test_name),
    ?assertEqual(self(), Pid),
    erlmcp_registry:stop(RegPid).
```

### Violation 2: State Inspection

```erlang
% ❌ VIOLATION
test_state_inspection() ->
    {ok, Pid} = erlmcp_server:start_link([]),
    {status, _, _, [_, _, _, _, Misc]} = sys:get_status(Pid),
    State = proplists:get_value(state, lists:last(Misc)),
    ?assertMatch(#state{initialized = true}, State).

% ✅ FIX
test_observable_behavior() ->
    {ok, Pid} = erlmcp_server:start_link([]),
    Response = erlmcp_client:call(Pid, initialize, #{}),
    ?assertEqual(ok, Response).
```

### Violation 3: Blocking init/1

```erlang
% ❌ VIOLATION
init(Opts) ->
    {ok, Conn} = connect_database(),  % Blocks!
    {ok, #state{conn = Conn}}.

% ✅ FIX
init(Opts) ->
    self() ! connect_async,
    {ok, #state{opts = Opts}}.

handle_info(connect_async, State) ->
    {ok, Conn} = connect_database(),
    {noreply, State#state{conn = Conn}}.
```

---

## Troubleshooting

### "Compilation failed" (Gate 1)

```bash
# Check errors
TERM=dumb rebar3 compile

# Clean and rebuild
make clean
make compile
```

### "Tests failed" (Gate 2)

```bash
# Run tests with verbose output
rebar3 eunit --verbose

# Run specific module
rebar3 eunit --module=my_module_tests
```

### "Coverage <80%" (Gate 3)

```bash
# Generate coverage report
rebar3 cover --verbose

# View HTML report
open _build/test/cover/index.html

# Check which modules are low
grep -E "[0-9]+%" _build/test/cover/cover.log | sort -t'|' -k3 -n
```

### "Dialyzer warnings" (Gate 4)

```bash
# Run dialyzer with verbose output
rebar3 dialyzer --verbose

# Check specific module
rebar3 dialyzer --module=my_module
```

### "Chicago TDD violations" (Gate 6)

```bash
# Run scanner
./.github/scripts/chicago-tdd-scan.sh

# Fix specific violations
# See docs/ERLMCP_FLOW_QUALITY_STANDARDS.md Section 8
```

---

## Bypassing Quality Gates (NOT RECOMMENDED)

**NEVER bypass on main/release branches!**

```bash
# Bypass pre-commit hook (local only)
git commit --no-verify

# Bypass pre-push hook (local only)
git push --no-verify
```

**Consequences:**
- CI will still block the PR
- Code review will reject
- Quality metrics will fail
- Technical debt accumulates

---

## TCPS Quality System

### Jidoka (Built-In Quality)

```bash
make jidoka              # 8 quality gates, stop-the-line on failure
make poka-yoke           # 8 error-proofing checks
make andon               # Show quality status dashboard
make andon-watch         # Real-time monitoring
make release-validate    # Generate quality receipt
```

### Quality Receipt

Generated on release:

```bash
make release-validate

# Produces:
# - Compilation report
# - Test results
# - Coverage metrics
# - Dialyzer/Xref results
# - Performance benchmarks
# - Chicago TDD compliance
# - Certification timestamp
```

---

## Resources

### Documentation
- `docs/ERLMCP_FLOW_QUALITY_STANDARDS.md` - Full specification
- `CLAUDE.md` - Project standards
- `docs/architecture/OTP_PATTERNS.md` - OTP patterns
- `docs/testing/CHICAGO_TDD.md` - Chicago TDD guide

### Scripts
- `.github/scripts/chicago-tdd-scan.sh` - Anti-pattern scanner
- `scripts/install-quality-hooks.sh` - Hook installer
- `scripts/check_coverage_threshold.sh` - Coverage validator

### Workflows
- `.github/workflows/quality-gate.yml` - Main quality gate
- `.github/workflows/chicago-school-tdd.yml` - TDD compliance

---

## Quick Reference Cards

### Developer Workflow

```
1. make doctor          # Check environment
2. Edit code
3. make quick           # Pre-commit check
4. git commit           # Pre-commit hook runs
5. make verify          # Pre-PR check
6. git push             # Pre-push hook runs
7. CI runs             # GitHub Actions quality gates
```

### Quality Metrics Targets

| Metric | Target | Gate |
|--------|--------|------|
| Compilation errors | 0 | BLOCKING |
| Test failures | 0 | BLOCKING |
| Test pass rate | ≥90% | BLOCKING |
| Code coverage | ≥80% | BLOCKING |
| Dialyzer warnings | 0 | BLOCKING |
| Xref undefined | 0 | BLOCKING |
| Performance regression | <10% | BLOCKING (releases) |
| File size | <500 lines | BLOCKING |

### Chicago TDD Rules

```
✅ DO:
- Use real erlmcp processes
- Test observable behavior
- Write state-based assertions
- Keep test files <500 lines
- Use integration tests

❌ DON'T:
- Use mocks/fakes/stubs
- Inspect internal state
- Create dummy processes
- Duplicate production records
- Test implementation details
```

---

**For detailed information, see:** `docs/ERLMCP_FLOW_QUALITY_STANDARDS.md`
**Questions?** Open an issue or consult the team in #erlmcp-flow
