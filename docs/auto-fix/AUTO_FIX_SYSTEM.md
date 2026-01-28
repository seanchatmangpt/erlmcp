# Auto-Fix System - Jidoka (自働化)

## Overview

The erlmcp Auto-Fix System is an automated quality enforcement system based on the Toyota concept of **Jidoka (自働化)** - automation with human intelligence. The system monitors quality gates, automatically attempts to fix failures, and escalates to human intervention when automatic fixes fail.

## Philosophy: Jidoka (自働化)

**Jidoka** means "automation with a human touch" or "autonomation". Key principles:

1. **Built-in Quality** - Quality checks are integrated into the development process
2. **Stop-the-Line Authority** - The system can halt progress when quality issues are detected
3. **Immediate Problem Detection** - Issues are caught and addressed immediately
4. **Escalation Path** - Clear path from auto-fix to human intervention
5. **Continuous Improvement** - Learn from failures to improve auto-fix capabilities

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Gate Failure Dispatcher                   │
│  (Monitors quality gates, dispatches fix agents, escalates)  │
└────────────┬────────────────────────────────────────────────┘
             │
             ├─────────────┬──────────────┬──────────────┐
             ▼             ▼              ▼              ▼
    ┌────────────┐  ┌─────────────┐  ┌────────────┐  ┌─────────┐
    │  Syntax    │  │    Type     │  │   Test     │  │  Perf   │
    │ Fix Agent  │  │  Fix Agent  │  │ Fix Agent  │  │  Agent  │
    └────────────┘  └─────────────┘  └────────────┘  └─────────┘
             │             │              │              │
             └─────────────┴──────────────┴──────────────┘
                            ▼
                  ┌──────────────────┐
                  │   Orchestrator   │
                  │ (Coordinates fix │
                  │  cycles, retries)│
                  └──────────────────┘
                            │
                ┌───────────┴────────────┐
                ▼                        ▼
        ┌─────────────┐          ┌─────────────┐
        │  All Gates  │          │  Escalation │
        │   Passed    │          │  to Human   │
        └─────────────┘          └─────────────┘
```

## Components

### 1. Gate Failure Dispatcher

**File:** `tools/auto-fix/gate-failure-dispatcher.sh`

**Responsibilities:**
- Monitor quality gate results
- Track fix attempt counts
- Dispatch appropriate fix agents
- Escalate after max attempts (default: 3)
- Maintain state across runs

**Usage:**
```bash
# Run all quality gates with auto-fix
./tools/auto-fix/gate-failure-dispatcher.sh run-all

# Monitor single gate
./tools/auto-fix/gate-failure-dispatcher.sh monitor compilation "rebar3 compile"

# Check status
./tools/auto-fix/gate-failure-dispatcher.sh status

# Reset attempt counters
./tools/auto-fix/gate-failure-dispatcher.sh reset
```

**State Tracking:**
- Stored in: `logs/auto-fix/dispatcher-state.json`
- Tracks: total failures, fixes, escalations, active attempts per gate
- Reset: `rm logs/auto-fix/dispatcher-state.json`

### 2. Syntax Fix Agent

**File:** `tools/auto-fix/syntax-fix-agent.sh`

**Capabilities:**
- Fix unused variables (prefix with `_`)
- Add missing export declarations
- Fix common typos
- Add missing punctuation (semicolons, commas)

**Limitations:**
- Cannot fix complex syntax errors
- Cannot infer correct function logic
- Cannot resolve ambiguous syntax

**Example Fixes:**
```erlang
%% Before: unused variable warning
my_function(Arg1, Arg2) -> Arg1.

%% After: auto-fixed
my_function(Arg1, _Arg2) -> Arg1.
```

### 3. Test Fix Agent

**File:** `tools/auto-fix/test-fix-agent.sh`

**Capabilities:**
- Update assertion expectations for simple mismatches
- Add missing setup/teardown fixtures
- Fix mock cleanup issues (meck:unload)
- Detect common test patterns (badmatch, timeout, etc.)

**Limitations:**
- Cannot fix logic errors in tests
- Cannot determine correct test expectations
- Cannot fix race conditions
- Cannot optimize slow tests

**Pattern Detection:**
- `PATTERN_ASSERTION_MISMATCH` - Expected vs actual mismatch
- `PATTERN_SETUP_TEARDOWN` - Missing fixture
- `PATTERN_UNDEFINED_FUNCTION` - Missing dependency
- `PATTERN_TIMEOUT` - Test timeout
- `PATTERN_BADMATCH` - Pattern match error

### 4. Type Fix Agent

**File:** `tools/auto-fix/type-fix-agent.sh`

**Capabilities:**
- Add missing `-spec` declarations
- Generate basic type specs from function signatures
- Update invalid type specifications
- Suggest proper types for common patterns

**Limitations:**
- Cannot infer complex type relationships
- Cannot fix fundamental type mismatches
- Cannot determine precise types without analysis
- Generates conservative `term()` types by default

**Example Fixes:**
```erlang
%% Before: missing spec warning
get_value(Key) -> maps:get(Key, my_map()).

%% After: auto-fixed
-spec get_value(term()) -> term().
get_value(Key) -> maps:get(Key, my_map()).
```

### 5. Orchestrator

**File:** `tools/auto-fix/orchestrator.sh`

**Responsibilities:**
- Coordinate multiple fix agents
- Run iterative fix cycles
- Validate fixes after each iteration
- Escalate after max iterations (default: 5)
- Generate comprehensive escalation reports

**Quality Gate Order:**
1. **Compilation** - Must pass before other gates
2. **Dialyzer** - Type checking
3. **XRef** - Cross-reference analysis
4. **Tests** - EUnit test suite
5. **Coverage** - Test coverage threshold

**Usage:**
```bash
# Run orchestration with default 5 iterations
./tools/auto-fix/orchestrator.sh orchestrate

# Run with custom iterations
./tools/auto-fix/orchestrator.sh orchestrate 10

# Validate all fixes with clean build
./tools/auto-fix/orchestrator.sh validate

# Interactive mode
./tools/auto-fix/orchestrator.sh interactive
```

## Quality Gates

### Gate 1: Compilation
**Command:** `TERM=dumb rebar3 compile`
**Fix Agent:** `syntax-fix-agent.sh`
**Common Issues:**
- Unused variables
- Missing exports
- Syntax errors
- Module not found

### Gate 2: Dialyzer
**Command:** `rebar3 dialyzer`
**Fix Agent:** `type-fix-agent.sh`
**Common Issues:**
- Missing `-spec` declarations
- Type mismatches
- Invalid type specs
- Function has no local return

### Gate 3: XRef
**Command:** `rebar3 xref`
**Fix Agent:** None (manual review required)
**Common Issues:**
- Undefined functions
- Unused exports
- Circular dependencies

### Gate 4: Tests
**Command:** `rebar3 eunit`
**Fix Agent:** `test-fix-agent.sh`
**Common Issues:**
- Assertion failures
- Setup/teardown errors
- Mock configuration
- Test timeouts

### Gate 5: Coverage
**Command:** `rebar3 cover --verbose`
**Fix Agent:** None (requires writing new tests)
**Common Issues:**
- Coverage below 80% threshold
- Uncovered code paths
- Missing edge case tests

## Workflow

### Automatic Fix Cycle

```
┌─────────────────┐
│  Quality Gate   │
│     Fails       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐      ┌──────────────────┐
│  Check Attempt  │─Yes─>│  Escalate to     │
│  Count > 3?     │      │  Human           │
└────────┬────────┘      └──────────────────┘
         │ No
         ▼
┌─────────────────┐
│  Dispatch Fix   │
│     Agent       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   Apply Fix     │
│   Attempt       │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Re-run Gate    │
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
  Pass      Fail
    │         │
    ▼         ▼
┌────────┐ ┌────────┐
│Success │ │ Retry  │
└────────┘ └────────┘
```

### Iteration Cycle

The orchestrator runs up to 5 iterations (configurable):

1. **Iteration 1-4:**
   - Run all quality gates in order
   - Dispatch fix agents for failures
   - Apply fixes and retry
   - Continue to next iteration if any gate fails

2. **Iteration 5 (Final):**
   - Last attempt at automatic fix
   - If any gate fails, escalate to human
   - Generate comprehensive escalation report

3. **Success:**
   - All gates pass
   - System reports success
   - Development can continue

4. **Escalation:**
   - Generate escalation report
   - Trigger Andon alert (行灯)
   - Stop the line (halt automated fixes)
   - Require human intervention

## Escalation

When automatic fixes fail after max attempts, the system escalates to human intervention.

### Escalation Report Contains:

1. **Summary:**
   - Timestamp
   - Number of iterations attempted
   - Failed gates

2. **Error Details:**
   - Full error output for each failed gate
   - Error file locations
   - Relevant log excerpts

3. **Next Steps:**
   - Manual fix procedures
   - Commands to run
   - How to reset auto-fix state

4. **Context:**
   - Recent changes
   - Related errors
   - Suggested root cause

### Andon Alert (行灯)

When escalation occurs, an **Andon Alert** is displayed:

```
╔════════════════════════════════════════════╗
║     ANDON ALERT (行灯) - LINE STOPPED      ║
╔════════════════════════════════════════════╗
Failed after 5 iterations
Report: logs/auto-fix/escalation-1234567890.txt
╚════════════════════════════════════════════╝
```

This is inspired by Toyota's Andon cord system, where any worker can stop the production line when quality issues are detected.

## Usage Patterns

### Development Workflow

```bash
# 1. Make code changes
vim src/my_module.erl

# 2. Run auto-fix orchestration
./tools/auto-fix/orchestrator.sh orchestrate

# 3a. If successful, continue development
# 3b. If escalated, review escalation report and fix manually

# 4. Reset state when issues resolved
./tools/auto-fix/gate-failure-dispatcher.sh reset
```

### CI/CD Integration

```bash
#!/bin/bash
# In your CI/CD pipeline

# Run orchestration
if ./tools/auto-fix/orchestrator.sh orchestrate 3; then
    echo "Quality gates passed"
    exit 0
else
    echo "Quality gates failed, check escalation reports"
    exit 1
fi
```

### Pre-Commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Quick auto-fix check
./tools/auto-fix/gate-failure-dispatcher.sh run-all || {
    echo "Auto-fix failed. Commit blocked."
    echo "Fix issues or use: git commit --no-verify"
    exit 1
}
```

## Configuration

### Max Attempts (per gate)
**File:** `tools/auto-fix/gate-failure-dispatcher.sh`
**Variable:** `MAX_FIX_ATTEMPTS=3`

Change to allow more fix attempts per gate before escalation.

### Max Iterations (orchestrator)
**File:** `tools/auto-fix/orchestrator.sh`
**Variable:** `MAX_ITERATIONS=5`

Change to allow more full iteration cycles before escalation.

### Log Directory
**Default:** `logs/auto-fix/`

All logs, state files, and escalation reports are stored here.

**Files:**
- `dispatcher.log` - Dispatcher activity log
- `syntax-fix.log` - Syntax fix agent log
- `test-fix.log` - Test fix agent log
- `type-fix.log` - Type fix agent log
- `orchestrator.log` - Orchestrator activity log
- `dispatcher-state.json` - Current state
- `escalation-*.txt` - Escalation reports
- `*-errors-*.txt` - Temporary error files

## Limitations

### What Auto-Fix Can Do:

✅ Fix unused variables
✅ Add missing exports
✅ Add basic type specs
✅ Update simple test assertions
✅ Add setup/teardown fixtures
✅ Fix mock cleanup
✅ Detect common error patterns

### What Auto-Fix Cannot Do:

❌ Fix logic errors
❌ Infer correct business logic
❌ Resolve complex type relationships
❌ Fix race conditions
❌ Optimize performance
❌ Write new tests
❌ Resolve circular dependencies
❌ Fix fundamental architectural issues

### When to Use Auto-Fix:

- During development for quick fixes
- In CI/CD for automated quality checks
- For common, repetitive fixes
- To catch simple mistakes early

### When NOT to Use Auto-Fix:

- For complex refactoring
- When architectural changes needed
- For performance optimization
- When business logic changes required
- During code review (use manual review)

## Override Procedures

### Disable Auto-Fix for Specific Gate

```bash
# Skip auto-fix, run gate manually
TERM=dumb rebar3 compile
# Fix manually
vim src/module.erl
```

### Reset All State

```bash
# Clear all auto-fix state
rm -rf logs/auto-fix/*.json
rm -rf logs/auto-fix/*-errors-*.txt

# Or use reset command
./tools/auto-fix/gate-failure-dispatcher.sh reset
```

### Force Manual Escalation

```bash
# Create manual escalation (for documentation)
cat > logs/auto-fix/manual-escalation.txt <<EOF
Manual Escalation
=================
Issue: [describe issue]
Root Cause: [describe]
Fix Applied: [describe]
EOF
```

### Skip Auto-Fix in CI

```bash
# Environment variable to disable auto-fix
export SKIP_AUTO_FIX=1

# Then run gates manually
make check
```

## Best Practices

1. **Run Locally First**
   - Test auto-fix locally before relying on it in CI
   - Understand what it can and cannot fix

2. **Review Auto-Fixes**
   - Always review auto-applied fixes
   - Ensure they're correct, not just compiling

3. **Use Iteration Limit Wisely**
   - Default 5 iterations is usually sufficient
   - More iterations may hide deeper issues

4. **Monitor Escalation Patterns**
   - Track which gates escalate most often
   - Improve fix agents based on patterns

5. **Keep Logs**
   - Preserve escalation reports for learning
   - Use them to improve auto-fix logic

6. **Reset State Regularly**
   - Reset after resolving escalations
   - Prevents stale state issues

7. **Don't Over-Rely**
   - Auto-fix is a helper, not a replacement for good code
   - Some issues require human intelligence

## Future Enhancements

Potential improvements to the auto-fix system:

1. **Machine Learning Integration**
   - Learn from successful manual fixes
   - Improve fix suggestions over time

2. **Coverage Fix Agent**
   - Generate basic test cases for uncovered code
   - Suggest test scenarios

3. **Performance Fix Agent**
   - Detect common performance anti-patterns
   - Suggest optimizations

4. **XRef Fix Agent**
   - Fix undefined function calls
   - Remove unused exports

5. **Integration with Version Control**
   - Auto-create fix commits
   - Tag auto-fixes for review

6. **Parallel Fix Execution**
   - Run fix agents in parallel
   - Faster fix cycles

7. **Fix Confidence Scoring**
   - Rate confidence in each fix
   - Only auto-apply high-confidence fixes

## Troubleshooting

### Auto-Fix Keeps Failing

**Problem:** Same gate fails repeatedly
**Solution:**
1. Check logs: `logs/auto-fix/<agent>-fix.log`
2. Review error file manually
3. The issue likely requires manual fix
4. Reset state and fix manually

### State File Corruption

**Problem:** JSON parsing errors
**Solution:**
```bash
rm logs/auto-fix/dispatcher-state.json
./tools/auto-fix/gate-failure-dispatcher.sh reset
```

### Fix Agent Not Found

**Problem:** "command not found" error
**Solution:**
```bash
chmod +x tools/auto-fix/*.sh
```

### Escalation Report Not Generated

**Problem:** No escalation file created
**Solution:**
1. Check log directory permissions: `ls -la logs/auto-fix/`
2. Create if missing: `mkdir -p logs/auto-fix`
3. Check disk space: `df -h`

## Support

For issues or questions:
1. Check logs in `logs/auto-fix/`
2. Review escalation reports
3. Consult this documentation
4. Open issue in erlmcp repository

## Related Documentation

- [TCPS System](../tcps/TCPS.md) - Toyota Code Production System
- [OTP Patterns](../otp-patterns.md) - Erlang/OTP best practices
- [Quality Gates](../quality-gates.md) - Quality gate definitions
- [Development Guide](../../CLAUDE.md) - erlmcp development guide

---

**Remember:** Auto-fix is automation with human intelligence. It catches simple mistakes quickly, but human judgment is still essential for quality code.
