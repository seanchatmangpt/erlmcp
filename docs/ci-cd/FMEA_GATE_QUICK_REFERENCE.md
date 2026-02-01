# FMEA Gate Quick Reference Card

**For Developers**: What to do when the FMEA gate fails on your PR.

---

## What is the FMEA Gate?

The FMEA (Failure Mode and Effects Analysis) gate is a security quality gate that blocks PRs when critical security tests fail. It protects against the top security failure modes in erlmcp.

**Threshold**: RPN ≥ 250 (Risk Priority Number = Severity × Occurrence × Detection)

---

## When You See This in Your PR:

```markdown
## Security FMEA Gate ❌

**Critical Failure Modes**: 5/6 passed (threshold: RPN ≥ 250)

### ❌ Failing Modes:
- **FM-05** (RPN 324): Tool call injection via transport/parser ambiguity

**Action**: Run tests locally and fix failures before merging
```

## What To Do:

### Step 1: Reproduce Locally

```bash
# Run all tests
rebar3 do eunit, ct

# Or run specific failing module
rebar3 eunit --module=erlmcp_message_parser_tests
```

### Step 2: Understand the Failure

Check the FM registry for context:
```bash
jq '.failure_modes[] | select(.id == "FM-05")' docs/fmea/fmea_security.json
```

Or read the GAP document referenced in the FM entry.

### Step 3: Fix the Issue

- If your code introduced the failure: fix the bug
- If the test is incorrect: update the test (with security review)
- If the FM is no longer relevant: update FMEA registry (with security review)

### Step 4: Verify Fix

```bash
# Run the specific test again
rebar3 eunit --module=erlmcp_message_parser_tests

# Verify all critical FMs pass
scripts/validation/generate_fmea_dashboard.sh --gate --threshold 250
```

### Step 5: Commit and Push

The gate will re-run on your next push. Once all critical FMs pass, the gate clears.

---

## Critical Failure Modes (RPN ≥ 250)

| FM ID | RPN | What It Protects | Test Suite |
|-------|-----|------------------|------------|
| FM-05 | 324 | Protocol parsing injection | erlmcp_message_parser_tests |
| FM-02 | 300 | Session hijacking | erlmcp_session_manager_tests |
| FM-03 | 280 | SSE stream data leak | erlmcp_sse_event_store_replay_tests |
| FM-10 | 280 | Task result isolation | erlmcp_tasks_edge_cases_tests |
| FM-08 | 270 | Log secret leakage | erlmcp_logging_tests |
| FM-04 | 250 | Auth bypass | erlmcp_auth_tests |

---

## Common Scenarios

### "My PR didn't touch security code, why did it fail?"

Security tests can fail due to:
- Refactoring broke a security property
- Dependency update introduced vulnerability
- Test flakiness (rare, but re-run to verify)

**Action**: Still fix it. Security regressions are critical regardless of intent.

### "The test is flaky, can I skip it?"

**No.** Security tests must be deterministic. If a test is flaky:
1. Reproduce the flakiness locally
2. Fix the test (not the gate)
3. Create an issue if you can't reproduce

**Never skip security gates.**

### "Can I lower the threshold to make my PR pass?"

**No.** The threshold (RPN ≥ 250) is set based on risk analysis. Lowering it requires:
- Security team approval
- FMEA registry update with justification
- Separate PR with review

### "My FM is marked CRITICAL but RPN < 250. Does it block?"

**No.** Only FMs with RPN ≥ 250 block the gate. However:
- You'll see warnings in the report
- Consider fixing anyway (defense in depth)
- CRITICAL priority means it's important, even if not gate-blocking

---

## Advanced Usage

### Run Gate Locally Before Pushing

```bash
# See full dashboard
scripts/validation/generate_fmea_dashboard.sh

# Run in gate mode (same as CI)
scripts/validation/generate_fmea_dashboard.sh --gate --threshold 250
```

### Check Specific FM Status

```bash
# Run only FM-05 tests
rebar3 eunit --module=erlmcp_message_parser_tests
rebar3 eunit --module=erlmcp_json_rpc_error_tests
rebar3 eunit --module=erlmcp_protocol_validator_tests
```

### View Historical Reports

```bash
# Download artifacts from failed CI run
gh run download <run-id> -n fmea_report

# View report
jq . reports/fmea_rpn_report.json
```

---

## Getting Help

1. **Read the full validation report**: `docs/ci-cd/FMEA_GATE_VALIDATION.md`
2. **Check the FM registry**: `docs/fmea/fmea_security.json`
3. **Read GAP docs**: `docs/GAP*_*.md` (linked in FM entries)
4. **Ask in security channel**: If unsure about security implications

---

## For Reviewers

When reviewing a PR with FMEA gate failures:

- ✅ **Do not approve** until gate passes
- ✅ Check if the FM failure is related to PR changes
- ✅ Verify the fix addresses root cause (not just making test pass)
- ✅ Consider if new FMs should be added to registry

**Exception**: Only security team can approve merges with failing gates (emergency hotfixes only).

---

## Configuration

**Workflow**: `.github/workflows/security-fmea-gate.yml`
**Script**: `scripts/validation/generate_fmea_dashboard.sh`
**Registry**: `docs/fmea/fmea_security.json`
**Threshold**: RPN ≥ 250 (line 64 in workflow)

**Maintainer**: erlang-github-ops agent
**Last Updated**: 2026-02-01

---

**Remember**: Security gates protect users. Fix failures, don't bypass them.
