# Quality Enforcement Quick Start

## 5-Minute Setup Guide

Get automated quality gates running in 5 minutes. This guide covers installation, first validation, and fixing common issues.

## Prerequisites

Verify you have these installed:

```bash
# Check Erlang/OTP version (need 25+)
erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().'  -noshell

# Check rebar3 version (need 3.22+)
rebar3 version

# Check Git version (need 2.30+)
git --version
```

If missing, install from:
- Erlang: https://www.erlang.org/downloads
- rebar3: https://rebar3.org/docs/getting-started/
- Git: https://git-scm.com/downloads

## Step 1: Install Quality Gates (1 minute)

Run the one-command installer:

```bash
cd /path/to/erlmcp
./tools/setup-quality-gates.sh
```

This script will:
- ✅ Install Git hooks (pre-commit, commit-msg, pre-push)
- ✅ Configure quality thresholds
- ✅ Set up baseline metrics
- ✅ Validate installation
- ✅ Run first quality check

**Expected Output:**
```
==> Installing erlmcp Quality Gates...
✓ Git hooks installed
✓ Configuration validated
✓ Baseline metrics recorded
✓ Installation verified

==> Running first quality check...
✓ Compilation: PASS (3.2s)
✓ Dialyzer: PASS (8.1s)
✓ XRef: PASS (1.3s)
✓ Format: PASS (0.8s)
✓ Tests: PASS (12.4s, 87.3% coverage)

🎉 Quality gates installed successfully!

Next steps:
1. Make a small change to any .erl file
2. Try to commit it
3. Watch quality gates in action
```

## Step 2: Run First Validation (1 minute)

Test that quality gates work correctly:

```bash
# Make a test change
echo "%% Test comment" >> src/erlmcp.erl

# Try to commit (this will trigger quality gates)
git add src/erlmcp.erl
git commit -m "test: Validate quality gates"
```

**Expected Output:**
```
==> Running pre-commit quality gates...

[1/5] Compiling...
✓ Compilation successful (1.2s)

[2/5] Type checking (Dialyzer)...
✓ No type errors found (3.4s)

[3/5] Cross-reference check...
✓ No undefined functions (0.5s)

[4/5] Format validation...
✓ Code properly formatted (0.3s)

[5/5] Running tests...
✓ All tests passed (5.2s)
✓ Coverage: 87.3% (target: 80%)

🎉 All quality gates passed!
[main a1b2c3d] test: Validate quality gates
 1 file changed, 1 insertion(+)
```

## Step 3: Interpret Results (1 minute)

### Understanding Quality Gate Output

Quality gates show clear status for each check:

```
✓ = Passed (green in color terminals)
⚠ = Warning (yellow)
✗ = Failed (red)
```

### Common Status Messages

| Message | Meaning | Action Required |
|---------|---------|-----------------|
| `✓ Compilation successful` | Code compiles without errors | None |
| `⚠ 3 compiler warnings` | Code compiles but has warnings | Review and fix warnings |
| `✗ Compilation failed` | Code doesn't compile | Fix syntax/type errors |
| `✓ Coverage: 87.3%` | Test coverage above threshold | None |
| `⚠ Coverage: 78.1%` | Coverage below 80% threshold | Add more tests |
| `✗ 5 tests failed` | Test failures detected | Fix failing tests |

### Detailed Error Information

When a gate fails, you get detailed error info:

```
[2/5] Type checking (Dialyzer)...
✗ Type errors found (3.4s)

src/erlmcp_client.erl:123: Function call with opaque term
  The call erlmcp_client:handle_call(Request::term(), From::{pid(),term()}, State::state())
  breaks the opacity of state()

Fix: Add proper type specification or use accessor functions
See: docs/quality-enforcement/DIALYZER_ERRORS.md#opaque-terms
```

## Step 4: Fix Common Issues (2 minutes)

### Issue 1: Compilation Errors

**Error:**
```
✗ Compilation failed
src/my_module.erl:15: syntax error before: ')'
```

**Fix:**
```bash
# Open the file and fix the syntax error at line 15
vim src/my_module.erl +15

# Verify fix
rebar3 compile

# Try commit again
git commit -m "fix: Correct syntax error"
```

### Issue 2: Format Violations

**Error:**
```
⚠ Format violations found
src/my_module.erl needs formatting
```

**Fix:**
```bash
# Auto-format the file
rebar3 format

# Check the changes
git diff src/my_module.erl

# Add and commit
git add src/my_module.erl
git commit -m "style: Auto-format code"
```

### Issue 3: Test Failures

**Error:**
```
✗ 2 tests failed
  erlmcp_client_tests:call_tool_test/0
  erlmcp_server_tests:register_tool_test/0
```

**Fix:**
```bash
# Run tests with verbose output
rebar3 eunit --module=erlmcp_client_tests --verbose

# Fix the failing test
vim test/erlmcp_client_tests.erl

# Verify fix
rebar3 eunit --module=erlmcp_client_tests

# Commit when all pass
git commit -m "test: Fix call_tool_test expectations"
```

### Issue 4: Low Coverage

**Error:**
```
⚠ Coverage: 78.1% (target: 80%)
Uncovered modules:
  - erlmcp_new_feature.erl (45.2%)
  - erlmcp_helper.erl (67.8%)
```

**Fix:**
```bash
# Add tests for uncovered modules
vim test/erlmcp_new_feature_tests.erl

# Run coverage check
rebar3 cover --verbose

# Verify coverage improved
rebar3 eunit --cover
rebar3 cover --verbose

# Commit when above threshold
git commit -m "test: Increase coverage for new_feature"
```

### Issue 5: Dialyzer Warnings

**Error:**
```
✗ Dialyzer found 3 issues
src/erlmcp_client.erl:45: Function has no local return
src/erlmcp_server.erl:123: Unmatched return of error
```

**Fix:**
```bash
# Run Dialyzer to see full details
rebar3 dialyzer

# Common fixes:
# 1. Add missing return value handling
# 2. Add proper type specs
# 3. Fix incorrect pattern matches

# Example fix in src/erlmcp_client.erl:45
-spec my_function(term()) -> {ok, result()} | {error, reason()}.
my_function(Input) ->
    case process(Input) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}  % Handle error case
    end.

# Verify fix
rebar3 dialyzer

# Commit when clean
git commit -m "fix: Handle error returns in client"
```

## Emergency: Skip Quality Gates

**WARNING:** Only use in emergencies (production down, critical hotfix).

```bash
# Skip pre-commit hook (NOT RECOMMENDED)
git commit --no-verify -m "hotfix: Critical production fix"

# Document why you skipped in commit message
git commit --no-verify -m "hotfix: Critical production fix

QUALITY GATES SKIPPED: Production system down, need immediate deploy.
Will create follow-up PR with proper validation."

# Create follow-up issue immediately
gh issue create --title "Validate hotfix commit a1b2c3d" \
  --body "Quality gates were skipped for emergency hotfix. Must validate."
```

**After Emergency:**
1. Create new branch from hotfix
2. Run full quality validation
3. Fix any issues found
4. Create PR for review
5. Document lessons learned

## Verification Checklist

After setup, verify everything works:

- [ ] Git hooks are executable (`ls -la .git/hooks/pre-commit`)
- [ ] `git commit` triggers quality gates
- [ ] Compilation errors block commits
- [ ] Test failures block commits
- [ ] Coverage threshold enforced
- [ ] Format violations detected
- [ ] Dialyzer runs successfully
- [ ] Clear error messages displayed
- [ ] `--no-verify` emergency skip works

## Performance Tuning

### If Quality Gates Are Too Slow

Quality gates should complete in <15 seconds for small changes. If slower:

**1. Enable Parallel Execution:**
```bash
# Edit .git/hooks/pre-commit
export MAKEFLAGS="-j$(nproc)"  # Use all CPU cores
```

**2. Cache Dialyzer PLT:**
```bash
# Build PLT once (takes 5-10 minutes)
rebar3 dialyzer

# PLT is cached in ~/.cache/rebar3/
# Subsequent runs take only 2-3 seconds
```

**3. Incremental Testing:**
```bash
# Only run tests for changed modules
# Edit .git/hooks/pre-commit to add:
CHANGED_MODULES=$(git diff --cached --name-only | grep "\.erl$" | sed 's/src\///' | sed 's/\.erl$//')
for module in $CHANGED_MODULES; do
    rebar3 eunit --module=${module}_tests
done
```

**4. Skip Slow Checks Locally:**
```bash
# Configure per-developer settings
cat > .git/hooks/pre-commit.local <<EOF
# Skip Dialyzer locally (still runs in CI)
export SKIP_DIALYZER=1

# Skip coverage check locally
export SKIP_COVERAGE=1
EOF
```

## IDE Integration

### Visual Studio Code

Install the Erlang LS extension and configure:

```json
// .vscode/settings.json
{
  "erlang.formatOnSave": true,
  "erlang.linting.dialyzer.enable": true,
  "editor.codeActionsOnSave": {
    "source.fixAll": true
  }
}
```

### Emacs

Add to your `.emacs` or `init.el`:

```elisp
;; Run quality gates before save
(add-hook 'erlang-mode-hook
  (lambda ()
    (add-hook 'before-save-hook
      (lambda ()
        (shell-command "rebar3 format")
        (revert-buffer t t t)) nil t)))
```

### Vim/Neovim

Add to your `.vimrc`:

```vim
" Auto-format on save
autocmd BufWritePre *.erl silent! !rebar3 format %

" Run tests on save
autocmd BufWritePost *_tests.erl silent! !rebar3 eunit --module=%:t:r
```

## Troubleshooting

### Problem: Hooks Not Running

**Symptoms:** Commits succeed without quality gate output

**Solution:**
```bash
# Check hook permissions
ls -la .git/hooks/pre-commit

# If not executable, fix:
chmod +x .git/hooks/pre-commit
chmod +x .git/hooks/commit-msg
chmod +x .git/hooks/pre-push

# Verify hooks are installed
cat .git/hooks/pre-commit | head -5
```

### Problem: "Command Not Found" Errors

**Symptoms:** `rebar3: command not found`

**Solution:**
```bash
# Add rebar3 to PATH
export PATH="$HOME/.cache/rebar3/bin:$PATH"

# Or install system-wide
sudo cp ~/.cache/rebar3/bin/rebar3 /usr/local/bin/

# Verify
which rebar3
rebar3 version
```

### Problem: Dialyzer Takes Forever

**Symptoms:** Dialyzer runs for >5 minutes

**Solution:**
```bash
# Rebuild PLT (one-time, 10 minutes)
rm -rf ~/.cache/rebar3/rebar3_*_plt
rebar3 dialyzer

# Use parallel Dialyzer (faster)
# Edit rebar.config:
{dialyzer, [
    {warnings, [error_handling, race_conditions]},
    {get_warnings, true},
    {plt_apps, [kernel, stdlib]},  % Minimal PLT
    {plt_extra_apps, []},
    {plt_location, local},
    {base_plt_apps, [kernel, stdlib, crypto]},
    {base_plt_location, global}
]}.
```

### Problem: False Positive Warnings

**Symptoms:** Quality gates fail on correct code

**Solution:**
```bash
# Report false positives
gh issue create --title "Quality gate false positive" \
  --body "Gate: Dialyzer
File: src/my_module.erl:45
Issue: Claims unused function but it's exported and used
Expected: Should pass"

# Temporary workaround (add to commit message)
git commit -m "feat: Add new feature

Known issue: Dialyzer false positive on line 45.
Tracked in issue #123."
```

## Next Steps

Now that quality gates are installed:

1. **Read BENEFITS.md** - Understand the value proposition
2. **Review IMPLEMENTATION_PLAN.md** - See upcoming features
3. **Check COMPARISON.md** - See before/after case studies
4. **Configure CI/CD** - Extend gates to GitHub Actions
5. **Integrate TCPS** - Connect to manufacturing system

## Getting Help

- **Documentation:** `/docs/quality-enforcement/`
- **Issues:** https://github.com/your-org/erlmcp/issues
- **Team Chat:** #erlmcp-quality channel
- **Weekly Office Hours:** Fridays 2-3 PM

## Summary

You've successfully:
- ✅ Installed Git hooks for local validation
- ✅ Run your first quality check
- ✅ Learned to interpret results
- ✅ Fixed common issues
- ✅ Configured emergency skip mechanism

**Quality gates are now protecting your commits!**

Every commit is now automatically validated for:
- Compilation correctness
- Type safety (Dialyzer)
- Code formatting
- Test coverage
- Cross-reference integrity

Welcome to zero-defect development!
