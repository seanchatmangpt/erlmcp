# TCPS Jidoka Automation (自働化)

## Philosophy: Automation with Human Touch

**Jidoka (自働化)** - Literal meaning: "automation with a human touch"

Jidoka is one of the two pillars of the Toyota Production System (the other being Just-In-Time). It embodies the principle of building quality into the process, not inspecting it in afterward.

### Core Principles

1. **Built-in Quality** - Quality cannot be inspected into a product; it must be built into the production process
2. **Stop-the-Line Authority** - Any worker has the authority and responsibility to stop production when a defect is detected
3. **Immediate Visibility** - Problems must be made visible immediately through the Andon system
4. **Root Cause Resolution** - Fix the root cause, don't work around problems
5. **Mistake-Proofing (Poka-Yoke)** - Design processes to prevent errors before they occur

## TCPS Quality Gate System

erlmcp implements 8 mandatory quality gates that mirror manufacturing quality control stations:

### Gate 1: Schema Validation (Compile)

**Purpose:** Ensure code structure is valid before proceeding

**Checks:**
- All modules compile successfully
- No syntax errors
- Warnings logged (non-blocking)

**Andon Trigger:**
- Compilation failures
- Critical compiler errors

**Resolution:**
```bash
# Fix compilation errors
rebar3 compile

# Review error messages
cat /tmp/compile_output.txt

# Re-run gate
make jidoka
```

---

### Gate 2: Authorization (Type Checking)

**Purpose:** Verify type correctness and prevent runtime type errors

**Checks:**
- Dialyzer type analysis passes
- No type specification violations
- Function contracts verified

**Andon Trigger:**
- Type mismatches
- Invalid function specifications
- Contract violations

**Resolution:**
```bash
# Run dialyzer with verbose output
rebar3 dialyzer

# Add type specifications
-spec function_name(arg1_type()) -> return_type().

# Re-run gate
make jidoka
```

---

### Gate 3: Rate Limiting (Performance)

**Purpose:** Detect performance anti-patterns early

**Checks:**
- No `lists:append/2` (use `++` or `lists:reverse/1`)
- No `length/1` in guards (use pattern matching)
- No obvious inefficiencies

**Andon Trigger:**
- More than 3 anti-patterns detected
- Known performance bottlenecks

**Resolution:**
```erlang
% BAD: lists:append in loop
process_list([H|T], Acc) ->
    process_list(T, lists:append(Acc, [H])).  % O(n) each iteration = O(n²)

% GOOD: Accumulate and reverse
process_list([H|T], Acc) ->
    process_list(T, [H|Acc]);  % O(1) each iteration = O(n)
process_list([], Acc) ->
    lists:reverse(Acc).        % O(n) once
```

---

### Gate 4: Resource Availability (Dependencies)

**Purpose:** Ensure all required resources are present

**Checks:**
- `rebar.lock` exists (dependencies locked)
- Critical dependencies present: jsx, jesse, gproc, gun, ranch, poolboy
- All dependencies compiled

**Andon Trigger:**
- Missing dependencies
- Unlocked dependencies (reproducibility risk)

**Resolution:**
```bash
# Fetch dependencies
rebar3 get-deps

# Lock dependencies
rebar3 lock

# Re-run gate
make jidoka
```

---

### Gate 5: Performance Envelope (Benchmarks)

**Purpose:** Verify performance characteristics remain acceptable

**Checks:**
- No obvious performance regressions
- Optional: Run benchmark suite (controlled by `JIDOKA_RUN_BENCHMARKS=1`)

**Andon Trigger:**
- Significant performance degradation (>10%)
- Benchmark failures

**Resolution:**
```bash
# Run benchmarks manually
JIDOKA_RUN_BENCHMARKS=1 make jidoka

# Or run specific benchmarks
make benchmark-quick

# Investigate regressions
make benchmark-stress
```

---

### Gate 6: Security Scan (No Secrets)

**Purpose:** Prevent security vulnerabilities from entering codebase

**Checks:**
- No hardcoded passwords, API keys, secrets
- No `TODO SECURITY` markers
- No suspicious credential patterns

**Andon Trigger:**
- Hardcoded secrets detected
- Security TODO items in production code

**Resolution:**
```erlang
% BAD: Hardcoded secret
-define(API_KEY, "sk-1234567890abcdef").

% GOOD: Environment variable or config
get_api_key() ->
    os:getenv("API_KEY", "").

% Or from application config
{ok, ApiKey} = application:get_env(myapp, api_key).
```

---

### Gate 7: Compliance (Coverage)

**Purpose:** Ensure all tests pass and adequate coverage exists

**Checks:**
- All EUnit tests pass (0 failures)
- Optional: Coverage ≥80% (future enhancement)

**Andon Trigger:**
- Any test failures
- Coverage below threshold

**Resolution:**
```bash
# Run tests with verbose output
rebar3 eunit --verbose

# Run specific module tests
rebar3 eunit --module=MODULE_tests

# Check coverage
rebar3 cover
```

---

### Gate 8: Receipt Generation (Evidence)

**Purpose:** Create immutable evidence chain of quality verification

**Checks:**
- Generate SHA-256 hash of receipt
- Link to previous receipt (blockchain-style)
- Record all gate results

**Output:**
```
.tcps/receipts/jidoka_success_1738023456_1738023789.txt
```

**Receipt Contents:**
- Work order ID
- Timestamp (ISO 8601 UTC)
- All 8 gate results
- Receipt hash (SHA-256)
- Previous receipt hash (chain link)
- Chain validation status

---

## Andon System (行灯 - Stop The Line)

The Andon system provides visible signaling when quality issues are detected.

### Andon Symbols

- 🟢 **Green** - All systems operational
- 🟡 **Yellow** - Warning, attention needed
- 🔴 **Red** - Line stopped, immediate action required

### Andon Operations

#### Check Status

```bash
make andon
# or
./tools/tcps/andon_cord.sh status
```

**Output:**
```
╔═══════════════════════════════════════════════════════════╗
║  TCPS Andon Board (行灯板)                                ║
║  Stop-The-Line Status                                     ║
╚═══════════════════════════════════════════════════════════╝

Total Andon Events: 12
Failure Events:     3

╔═══════════════════════════════════════════════════════════╗
║  🔴 ACTIVE ISSUES - LINE STOPPED                          ║
╚═══════════════════════════════════════════════════════════╝

Recent Failures (last 5):
  2026-01-28T10:30:00Z
  Gate:   Gate 7: Compliance
  Reason: Test failures detected
```

#### Pull Andon Cord (Manual)

```bash
./tools/tcps/andon_cord.sh pull "Reason for stopping"
```

Use this when you detect a problem that requires immediate attention:
- Integration test failures
- Production deployment issues
- Critical bugs discovered

#### Clear Andon

```bash
make andon-clear
# or
./tools/tcps/andon_cord.sh clear
```

**Only clear Andon after:**
1. Root cause identified
2. Fix implemented
3. All quality gates pass
4. Receipt generated

#### Watch Mode (Real-time Monitoring)

```bash
make andon-watch
# or
./tools/tcps/andon_cord.sh watch
```

Refreshes every 5 seconds. Use during CI/CD or continuous development.

---

## Poka-Yoke (ポカヨケ - Mistake-Proofing)

**Poka-Yoke** means "mistake-proofing" - designing processes to prevent errors before they occur.

### 8 Poka-Yoke Checks

#### 1. No .broken Files

Prevents broken code artifacts from persisting.

```bash
# Check
find . -name "*.broken" -type f
```

#### 2. No TODO/FIXME in Committed Code

Ensures all work is complete before commit.

```bash
# Check staged files
git diff --cached --name-only | xargs grep -n "TODO\|FIXME"
```

**Guidance:**
- Resolve TODOs before committing
- Convert persistent TODOs to GitHub issues
- Use FIXME only for urgent items

#### 3. No Hardcoded Secrets

Prevents credential leakage.

```bash
# Patterns checked
password\s*=\s*"[^"]+"
api_key\s*=\s*"[^"]+"
secret\s*=\s*"[^"]+"
token\s*=\s*"[^"]+"
```

**Resolution:**
- Use environment variables
- Use application configuration
- Use secret management systems (Vault, AWS Secrets Manager)

#### 4. No Unused Variables/Functions

Detects dead code via xref.

```bash
rebar3 xref
```

**Resolution:**
- Remove unused exports
- Add `-compile(export_all).` only in tests
- Use `_Var` for intentionally unused variables

#### 5. Proper Module Structure

Ensures module name matches filename.

```erlang
% File: erlmcp_client.erl
-module(erlmcp_client).  % ✓ Matches filename

% File: client.erl
-module(erlmcp_client).  % ✗ Mismatch (should be -module(client))
```

#### 6. Test Coverage

Verifies test file exists for each module.

**Pattern:**
```
apps/erlmcp_core/src/erlmcp_client.erl
apps/erlmcp_core/test/erlmcp_client_tests.erl  ✓

apps/erlmcp_core/src/erlmcp_server.erl
apps/erlmcp_core/test/erlmcp_server_tests.erl  ✓
```

**Exceptions:**
- `*_sup.erl` - Supervisors (behavior verified by OTP)
- `*_app.erl` - Applications (behavior verified by OTP)

#### 7. No Debug Output

Prevents debug statements in production code.

```erlang
% BAD: Debug output in production
handle_call(Request, From, State) ->
    io:format("DEBUG: Got request ~p~n", [Request]),  % ✗
    {reply, ok, State}.

% GOOD: Proper logging
handle_call(Request, From, State) ->
    logger:debug("Received request", #{request => Request}),  % ✓
    {reply, ok, State}.
```

#### 8. Proper Supervision

Ensures processes are supervised, not orphaned.

```erlang
% BAD: Unsupervised spawn
start_worker() ->
    spawn(fun() -> worker_loop() end).  % ✗ No supervision

% GOOD: Supervised child
start_worker() ->
    supervisor:start_child(worker_sup, []).  % ✓ Under supervision
```

---

## Integration with Development Workflow

### Daily Development

```bash
# 1. Write code
vim apps/erlmcp_core/src/erlmcp_client.erl

# 2. Write tests (TDD)
vim apps/erlmcp_core/test/erlmcp_client_tests.erl

# 3. Run poka-yoke before commit
make poka-yoke

# 4. Run full quality gates
make jidoka

# 5. Check Andon status
make andon
```

### Pre-Commit Hook (Recommended)

Create `.git/hooks/pre-commit`:

```bash
#!/bin/bash
set -e

echo "Running TCPS Poka-Yoke checks..."
make poka-yoke

echo "Running TCPS Jidoka quality gates..."
make jidoka

echo "✓ All quality gates passed - commit allowed"
```

### CI/CD Integration

Add to your CI pipeline:

```yaml
# .github/workflows/ci.yml
jobs:
  quality-gates:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Install Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: '25.0'
          rebar3-version: '3.22'

      - name: Compile dependencies
        run: rebar3 compile

      - name: Run TCPS Poka-Yoke
        run: make poka-yoke

      - name: Run TCPS Jidoka Quality Gates
        run: make jidoka
        env:
          JIDOKA_RUN_BENCHMARKS: 0  # Skip benchmarks in CI (too slow)

      - name: Check Andon Status
        if: failure()
        run: make andon
```

---

## Makefile Targets

### Combined Targets

```bash
make tcps-quality-gates  # Run poka-yoke + jidoka (full quality system)
```

### Individual Targets

```bash
make jidoka              # Run 8 quality gates
make poka-yoke           # Run 8 error-proofing checks
make andon               # Show Andon board status
make andon-clear         # Clear Andon (after fixing issues)
make andon-watch         # Real-time monitoring
```

---

## Receipt Chain

Every successful quality gate run generates an immutable receipt with SHA-256 hash chain.

### Receipt Structure

```
TCPS Jidoka Success Receipt (自働化成功レシート)
================================================

Work Order:    1738023456
Timestamp:     2026-01-28T10:30:00Z
Quality Gates: 8/8 PASSED

Gate Results:
  [✓] Gate 1: Schema Validation (Compile)
  [✓] Gate 2: Authorization (Type Checking)
  [✓] Gate 3: Rate Limiting (Performance)
  [✓] Gate 4: Resource Availability (Dependencies)
  [✓] Gate 5: Performance Envelope (Benchmarks)
  [✓] Gate 6: Security Scan (No Secrets)
  [✓] Gate 7: Compliance (Coverage)
  [✓] Gate 8: Receipt Generation (Evidence)

Jidoka Principle Verified:
✓ Quality built into the process
✓ Zero defects at each stage
✓ Andon system operational
✓ Stop-the-line authority working

Receipt Hash: 1a2b3c4d5e6f7890abcdef1234567890abcdef1234567890abcdef1234567890
Previous Receipt: 9876543210fedcba0987654321fedcba0987654321fedcba0987654321fedcba

Chain Status: VALID
```

### Receipt Chain Verification

```bash
# List all receipts
ls -lt .tcps/receipts/

# Verify chain integrity
for receipt in .tcps/receipts/jidoka_success_*.txt; do
    hash=$(sha256sum "$receipt" | cut -d' ' -f1)
    echo "$receipt: $hash"
done
```

---

## Troubleshooting

### Gate Failures

**Q: Gate 1 (Compile) failed - what do I do?**

A: Check compiler output:
```bash
cat /tmp/compile_output.txt
rebar3 compile
```

**Q: Gate 7 (Tests) failed - how to debug?**

A: Run tests with verbose output:
```bash
rebar3 eunit --verbose
rebar3 eunit --module=FAILING_MODULE_tests
```

### Andon Issues

**Q: Andon shows old failures that are fixed?**

A: Clear Andon after confirming fixes:
```bash
make andon-clear
```

**Q: How to view Andon history?**

A: Check archived logs:
```bash
ls -lt .tcps/andon_log.txt*
cat .tcps/andon_log.txt.20260128_103000
```

### Poka-Yoke Warnings

**Q: Poka-Yoke shows warnings but no errors - can I proceed?**

A: Yes. Warnings are non-blocking but should be addressed:
- TODO/FIXME → Convert to issues or resolve
- Debug output → Replace with proper logging
- Missing tests → Add test files

---

## Best Practices

### 1. Run Quality Gates Frequently

```bash
# After every significant change
make poka-yoke
make jidoka

# Check status
make andon
```

### 2. Never Bypass Quality Gates

If a gate fails:
1. ✓ Fix the root cause
2. ✗ DON'T skip the gate
3. ✗ DON'T disable the check

### 3. Use Andon Early

Pull the Andon cord as soon as you detect a problem:
```bash
./tools/tcps/andon_cord.sh pull "Integration test flaky - needs investigation"
```

### 4. Maintain Receipt Chain

The receipt chain provides audit trail:
- Keep receipts in version control
- Archive old receipts periodically
- Verify chain integrity before releases

### 5. Monitor Andon During CI/CD

Add Andon status to build notifications:
```bash
if make jidoka; then
    echo "✓ Quality gates passed"
else
    make andon  # Show failure details
    exit 1
fi
```

---

## Philosophy in Practice

### The Toyota Way

**Jidoka Example:**

Traditional approach:
```
Write code → Commit → CI fails → Debug → Fix → Re-commit
                ↑                    ↓
                └────── Waste ───────┘
```

Jidoka approach:
```
Write code → Run poka-yoke → Fix issues → Run jidoka → Commit ✓
             ↑ Immediate feedback, no waste            ↓
             └─────── Quality built in ───────────────┘
```

### Stop-the-Line Authority

In manufacturing, any worker can pull the Andon cord to stop the entire production line.

In erlmcp, any developer can (and should):
1. Run `make jidoka` before committing
2. Pull Andon cord when issues detected
3. Fix root cause (not symptoms)
4. Resume only after quality verified

### Continuous Improvement (Kaizen)

After each Andon event:
1. Document what went wrong
2. Perform 5 Whys analysis (see `.claude/commands/5-whys-analyze.sh`)
3. Update poka-yoke checks to prevent recurrence
4. Share learnings with team

---

## Reference

### Japanese Terminology

- **自働化 (Jidoka)** - Automation with human touch; built-in quality
- **行灯 (Andon)** - Lantern/light; visual signaling system
- **ポカヨケ (Poka-Yoke)** - Mistake-proofing; error prevention
- **改善 (Kaizen)** - Continuous improvement
- **レシート (Reshīto)** - Receipt; evidence/proof

### Related TCPS Commands

```bash
/tcps-jidoka            # Run quality gates
/tcps-andon status      # Check line status
/poka-yoke-validate     # Error-proofing checks
/5-whys-analyze         # Root cause analysis
/tcps-receipt verify    # Verify receipt chain
```

### Files

- `tools/tcps/jidoka_quality_gate.sh` - Main quality gate script
- `tools/tcps/andon_cord.sh` - Andon system controller
- `tools/tcps/poka_yoke_validator.sh` - Error-proofing validator
- `.tcps/receipts/` - Receipt chain storage
- `.tcps/andon_log.txt` - Andon event log

---

## Conclusion

The TCPS Jidoka system brings manufacturing-grade quality principles to software development:

✓ **Quality built in** - Not inspected in afterward
✓ **Stop-the-line authority** - Immediate problem resolution
✓ **Mistake-proofing** - Prevent errors before they occur
✓ **Visual management** - Andon makes problems visible
✓ **Evidence chain** - Immutable receipts prove quality

**Remember:** The goal is zero defects, not fast defects.

---

*Document Version: 1.0*
*Last Updated: 2026-01-28*
*erlmcp v2.0.0*
