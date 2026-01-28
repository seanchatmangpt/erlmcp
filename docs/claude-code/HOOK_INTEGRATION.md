# Claude Code Hook Integration Architecture

## Overview

The erlmcp project uses Claude Code hooks to automatically enforce quality rules defined in `CLAUDE.md`. This creates a "shift-left" quality approach where violations are caught immediately during development rather than later in CI/CD.

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                        CLAUDE.md                             │
│  (Single Source of Truth for Quality Rules)                 │
│                                                              │
│  - Compilation: 0 errors                                    │
│  - Tests: 100% pass rate                                    │
│  - Coverage: ≥80%                                           │
│  - Dialyzer: 0 warnings (goal)                             │
│  - Benchmarks: <10% regression                             │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  │ Parsed by
                  ▼
┌─────────────────────────────────────────────────────────────┐
│          tools/claude-md-enforcer.sh                        │
│  (Quality Rules Parser & Validator)                         │
│                                                              │
│  1. Parse CLAUDE.md rules                                   │
│  2. Run rebar3 compile                                      │
│  3. Run rebar3 eunit                                        │
│  4. Run rebar3 cover                                        │
│  5. Run rebar3 dialyzer                                     │
│  6. Run rebar3 xref                                         │
│  7. Report violations                                       │
│  8. EXIT 0 (pass) or EXIT 1 (fail)                         │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  │ Called by
                  │
        ┌─────────┼─────────┬──────────────┐
        │         │         │              │
        ▼         ▼         ▼              ▼
┌──────────┐ ┌────────┐ ┌─────────┐ ┌─────────────┐
│ Git      │ │ Claude │ │ CI/CD   │ │ Manual      │
│ Pre-     │ │ Code   │ │ GitHub  │ │ Validation  │
│ Commit   │ │ Post-  │ │ Actions │ │             │
│ Hook     │ │ Task   │ │         │ │ $ ./tools/  │
│          │ │ Hook   │ │         │ │   claude-   │
│          │ │        │ │         │ │   md-       │
│          │ │        │ │         │ │   enforcer  │
│          │ │        │ │         │ │   .sh       │
└──────────┘ └────────┘ └─────────┘ └─────────────┘
```

## Components

### 1. tools/claude-md-enforcer.sh

**Purpose:** Core validation engine that parses CLAUDE.md and enforces quality rules.

**Responsibilities:**
- Parse quality targets from CLAUDE.md line 74
- Run compilation checks (rebar3 compile)
- Run test suites (rebar3 eunit)
- Calculate test pass rates
- Measure code coverage (rebar3 cover)
- Check type safety (rebar3 dialyzer)
- Validate cross-references (rebar3 xref)
- Report violations with actionable errors
- Exit with status code (0=pass, 1=fail)

**Exit Codes:**
- `0`: All quality rules satisfied
- `1`: One or more rule violations detected

**Usage:**
```bash
# Run validator
./tools/claude-md-enforcer.sh

# Returns exit code 0 or 1
echo $?
```

**Output Format:**
```
[INFO] CLAUDE.md Quality Rules Enforcer
[INFO] Project: erlmcp
[INFO] Root: /Users/sac/erlmcp

[INFO] Parsing quality rules from CLAUDE.md...
[INFO] Quality targets extracted:
[INFO]   - Compilation errors: ≤0
[INFO]   - Test pass rate: ≥100%
[INFO]   - Coverage: ≥80%
[INFO]   - Performance regression: <10%

[INFO] Validating compilation...
[INFO] ✅ Compilation passed (0 errors)

[INFO] Validating tests...
[INFO] ✅ Tests passed: 42/42 (100%)

[INFO] Validating test coverage...
[INFO] ✅ Coverage: 85% (≥80%)

[INFO] ✅ All quality rules satisfied
```

### 2. .claude/hooks/post-task-validate.sh

**Purpose:** Claude Code hook that runs after any agent completes a task.

**Trigger:** Automatically after agent task completion

**Responsibilities:**
- Receive task metadata (ID, description)
- Call claude-md-enforcer.sh
- Block task completion if validation fails
- Provide resolution steps for violations

**Exit Codes:**
- `0`: Task validation passed, approve completion
- `1`: Validation failed, block completion

**Hook Lifecycle:**
```
Agent completes task
       ↓
post-task-validate.sh triggered
       ↓
Calls claude-md-enforcer.sh
       ↓
┌──────────────┬─────────────┐
│ EXIT 0       │ EXIT 1      │
│ (Pass)       │ (Fail)      │
├──────────────┼─────────────┤
│ ✅ Task      │ ❌ Task     │
│    approved  │    blocked  │
│              │             │
│ Continue     │ Show        │
│              │ violations  │
│              │             │
│              │ Require     │
│              │ fixes       │
└──────────────┴─────────────┘
```

**Usage:**
```bash
# Automatically called by Claude Code
# Manual test:
./.claude/hooks/post-task-validate.sh "task-123" "Implement feature X"
```

### 3. .claude/hooks/pre-commit-validate.sh

**Purpose:** Git pre-commit hook that validates code before commit.

**Trigger:** Automatically before `git commit`

**Responsibilities:**
- Check if merge commit (skip validation)
- Call claude-md-enforcer.sh
- Block commit if validation fails
- Provide resolution steps

**Exit Codes:**
- `0`: Commit approved
- `1`: Commit blocked

**Hook Lifecycle:**
```
Developer runs: git commit
       ↓
pre-commit-validate.sh triggered
       ↓
Check if merge commit? ──Yes──> Skip validation, allow commit
       │
       No
       ↓
Calls claude-md-enforcer.sh
       ↓
┌──────────────┬─────────────┐
│ EXIT 0       │ EXIT 1      │
│ (Pass)       │ (Fail)      │
├──────────────┼─────────────┤
│ ✅ Commit    │ ❌ Commit   │
│    proceeds  │    blocked  │
│              │             │
│ Create       │ Show        │
│ commit       │ violations  │
│              │             │
│              │ Require     │
│              │ fixes       │
└──────────────┴─────────────┘
```

**Bypass (Not Recommended):**
```bash
# Emergency bypass only
git commit --no-verify
```

### 4. tools/claude-md-sync.sh

**Purpose:** Synchronize CLAUDE.md rules to all enforcement points.

**Responsibilities:**
- Install git pre-commit hook
- Verify Claude Code hooks exist
- Check CI/CD workflows for enforcer integration
- Verify Makefile targets
- Ensure all scripts are executable
- Report synchronization status

**Usage:**
```bash
# Run after updating CLAUDE.md rules
./tools/claude-md-sync.sh
```

**Output:**
```
[SYNC] CLAUDE.md Rules Synchronization
[SYNC] Project: erlmcp

[SYNC] Synchronizing to git pre-commit hooks...
[INFO] ✅ Git pre-commit hook installed at .git/hooks/pre-commit

[SYNC] Synchronizing to CI/CD workflows...
[INFO] ✅ Workflow already includes enforcer: ci.yml

[SYNC] Synchronizing to Makefile...
[INFO] ✅ Makefile already includes enforcer target

[SYNC] Verifying enforcement points...
[INFO] ✅ CLAUDE.md enforcer exists
[INFO] ✅ Post-task hook exists
[INFO] ✅ Pre-commit hook script exists
[INFO] ✅ Git pre-commit hook installed

[SYNC] ✅ Synchronization complete
```

## Rule Extraction Process

### CLAUDE.md Format

Quality rules are defined on line 74 of CLAUDE.md:

```markdown
**Targets:** 0 errors, 100% test pass, ≥80% coverage, <10% perf regression.
```

### Parser Logic

```bash
# Extract compilation error threshold
COMPILATION_ERRORS_MAX=0

# Extract test pass rate threshold
TEST_PASS_RATE_MIN=100

# Extract coverage threshold
COVERAGE_MIN=80

# Extract performance regression threshold
PERF_REGRESSION_MAX=10
```

### Validation Rules

| Rule | Source | Validation Command | Threshold | Blocking |
|------|--------|-------------------|-----------|----------|
| Compilation | Line 74 | `rebar3 compile` | 0 errors | Yes |
| Tests | Line 74 | `rebar3 eunit` | 100% pass | Yes |
| Coverage | Line 74 | `rebar3 cover` | ≥80% | Yes |
| Dialyzer | Line 80 | `rebar3 dialyzer` | 0 warnings (goal) | No |
| Xref | Line 80 | `rebar3 xref` | Clean | No |
| Benchmarks | Line 74 | `make benchmark-quick` | <10% regression | Conditional |

## Enforcement Points

### 1. Pre-Commit Hook

**Location:** `.git/hooks/pre-commit`

**When:** Before every `git commit`

**Scope:** All staged changes

**Behavior:**
- Blocking: Commit fails if validation fails
- Bypass: `git commit --no-verify` (not recommended)

### 2. Post-Task Hook

**Location:** `.claude/hooks/post-task-validate.sh`

**When:** After agent completes task

**Scope:** Changes made by agent

**Behavior:**
- Blocking: Task marked incomplete if validation fails
- Agent must fix violations before task completion

### 3. CI/CD Pipeline

**Location:** `.github/workflows/*.yml`

**When:** On push, pull request

**Scope:** All code in branch

**Behavior:**
- Blocking: PR cannot merge if validation fails
- Required status check

### 4. Manual Validation

**Location:** `./tools/claude-md-enforcer.sh`

**When:** Developer runs manually

**Scope:** Current codebase state

**Behavior:**
- Non-blocking: For testing and debugging
- Returns exit code for scripting

## Integration with Claude Code Agents

### Agent Workflow

1. Agent receives task
2. Agent implements solution
3. Agent completes task
4. **post-task-validate.sh runs automatically**
5. If validation fails:
   - Task marked incomplete
   - Violations shown to agent
   - Agent must fix issues
   - Repeat from step 3
6. If validation passes:
   - Task marked complete
   - Changes ready for commit

### Agent Best Practices

**Before marking task complete:**
- Run `rebar3 compile` and fix errors
- Run `rebar3 eunit` and fix failures
- Ensure `rebar3 cover` shows ≥80% coverage
- Run `rebar3 dialyzer` and address warnings
- Document any intentional bypasses

**If validation fails:**
- Read violation messages carefully
- Fix issues in code (don't bypass hooks)
- Re-run validation manually: `./tools/claude-md-enforcer.sh`
- Continue task work

## Troubleshooting

### Problem: Pre-commit hook not running

**Solution:**
```bash
# Re-install hooks
./tools/claude-md-sync.sh

# Verify installation
ls -la .git/hooks/pre-commit

# Test hook manually
./.claude/hooks/pre-commit-validate.sh
```

### Problem: Tests fail in hook but pass manually

**Cause:** Environment differences

**Solution:**
```bash
# Hook runs with clean environment
# Test with same environment:
env -i HOME="$HOME" PATH="$PATH" rebar3 eunit

# Check for ENV variables in tests
grep -r "os:getenv" test/
```

### Problem: Hook is too slow

**Solution:**
```bash
# Use faster validation during development
# Full validation in CI/CD

# Option 1: Skip dialyzer in hook (faster)
# Edit enforcer to make dialyzer optional

# Option 2: Parallel validation
# Run compile + tests in parallel
```

### Problem: Need to bypass hook urgently

**Solution:**
```bash
# Emergency bypass (commit message required)
git commit --no-verify -m "WIP: Fix in progress, bypassing hooks"

# Then immediately fix and commit properly
./tools/claude-md-enforcer.sh
git add .
git commit -m "Fix quality violations"
```

### Problem: Hook blocks merge commit

**Solution:**
```bash
# Merge commits automatically skip validation
# If blocked, check that MERGE_HEAD exists:
git rev-parse --verify MERGE_HEAD

# If missing, it's not a merge commit
# Run validation and fix issues
```

## Testing Hooks

### Test Pre-Commit Hook

```bash
# Make a small change
echo "% Test comment" >> src/erlmcp_client.erl

# Stage change
git add src/erlmcp_client.erl

# Try to commit (hook will run)
git commit -m "Test commit"

# If hook fails, validation is working
# If hook passes, change should compile/test
```

### Test Post-Task Hook

```bash
# Simulate agent task completion
./.claude/hooks/post-task-validate.sh "test-task" "Test task description"

# Check exit code
echo $?
# 0 = validation passed
# 1 = validation failed
```

### Test Enforcer Directly

```bash
# Run enforcer
./tools/claude-md-enforcer.sh

# Check detailed output
./tools/claude-md-enforcer.sh 2>&1 | tee validation.log

# Test with intentional failure
# (temporarily break a test, then run enforcer)
```

## Metrics and Monitoring

### Validation Metrics

Track these metrics over time:

- **Hook failure rate:** % of commits blocked by pre-commit hook
- **Validation time:** Time to run claude-md-enforcer.sh
- **Coverage trend:** Code coverage % over time
- **Test pass rate:** % of test runs that pass
- **Dialyzer warnings:** Number of type warnings

### Quality Trends

```bash
# Track coverage over time
echo "$(date +%Y-%m-%d),$(rebar3 cover | grep total | sed 's/.*total: \([0-9]*\)%.*/\1/')" >> metrics/coverage.csv

# Track test count
echo "$(date +%Y-%m-%d),$(rebar3 eunit | grep 'All .* tests' | sed 's/All \([0-9]*\) tests.*/\1/')" >> metrics/tests.csv

# Visualize with gnuplot or similar
```

## Maintenance

### Updating Rules

1. Edit CLAUDE.md quality targets (line 74)
2. Run synchronization: `./tools/claude-md-sync.sh`
3. Test with enforcer: `./tools/claude-md-enforcer.sh`
4. Commit changes: `git add CLAUDE.md tools/ .claude/`
5. Rules propagate to all developers on next pull

### Adding New Validation Rules

1. Update parser in `tools/claude-md-enforcer.sh`
2. Add validation function
3. Call from main()
4. Update CLAUDE.md to document new rule
5. Run sync: `./tools/claude-md-sync.sh`
6. Update this documentation

### Hook Versioning

Hooks are versioned with the project:

```bash
# Version is in CLAUDE.md
# Hooks reference CLAUDE.md as source of truth
# Sync tool ensures consistency

# To update hooks project-wide:
git pull origin main
./tools/claude-md-sync.sh
```

## References

- **CLAUDE.md:** Quality rules source of truth
- **tools/claude-md-enforcer.sh:** Validation engine
- **.claude/hooks/:** Claude Code hook scripts
- **docs/architecture.md:** Project architecture
- **docs/otp-patterns.md:** OTP development patterns

## Philosophy

> "The best time to catch a bug is before it's written. The second best time is in the IDE. The third best time is in a pre-commit hook."

This hook integration embodies the "shift-left" quality philosophy:

1. **Prevention:** Rules in CLAUDE.md prevent misunderstanding
2. **Early Detection:** Hooks catch violations immediately
3. **Fast Feedback:** Local validation (seconds, not minutes in CI)
4. **Consistency:** Same rules everywhere (local, CI, production)
5. **Automation:** No manual steps, no forgotten checks

The result is higher quality code with less effort and frustration.
