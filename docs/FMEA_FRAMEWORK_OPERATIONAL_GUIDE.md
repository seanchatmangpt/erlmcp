# FMEA Framework Operational Guide

**Version**: 1.0
**Date**: 2026-02-01
**Status**: Operational
**Audience**: SRE, DevOps, Security Engineers, Development Team

---

## Executive Summary

This guide provides operational procedures for the **FMEA→GAP→SUITE→GATE** security economics framework in erlmcp. This framework replaces "best-effort" security with **architecturally enforced** security controls where entire classes of failures are impossible.

**What you'll learn**:
- How to run the FMEA dashboard
- How to add new failure modes
- How to interpret validation reports
- How to respond to gate failures
- Monthly review processes
- Incident escalation procedures

---

## Part I: Quick Start

### Running the FMEA Dashboard

#### Command-Line Dashboard

```bash
# Generate dashboard (report mode)
./scripts/validation/generate_fmea_dashboard.sh

# Output:
# - reports/fmea_rpn_report.json (machine-readable)
# - Console summary

# Generate dashboard with CI gate (blocking mode)
./scripts/validation/generate_fmea_dashboard.sh --gate --threshold 250

# Exit codes:
# 0 = All critical FMs passing
# 1 = One or more critical FMs failing (blocks merge)
```

#### Interactive HTML Dashboard

```bash
# Open in browser
open reports/fmea_interactive_dashboard.html

# Features:
# - Visual FM cards with RPN color coding
# - Click to expand details
# - Filter by status (closed/partial/critical)
# - Real-time gate decision
```

#### Expected Output

```
═══════════════════════════════════════════════════════════════
FMEA Dashboard Generator
═══════════════════════════════════════════════════════════════
Generated: 2026-02-01T12:00:00Z
FMEA Registry: docs/fmea/fmea_security.json
Threshold: RPN ≥ 250

Running FM Closure Tests
────────────────────────────────────────────────────────────────
  FM-05: Testing erlmcp_protocol_validator_tests... ✅ PASSED
  FM-02: Testing erlmcp_session_manager_tests... ✅ PASSED
  FM-03: Testing erlmcp_sse_event_store_replay_tests... ✅ PASSED
  FM-04: Testing erlmcp_auth_tests... ✅ PASSED
  FM-08: Testing erlmcp_logging_tests... ⚠️  CI test, skipping
  FM-10: Testing erlmcp_tasks_edge_cases_tests... ✅ PASSED
  FM-07: Testing erlmcp_uri_validator_tests... ✅ PASSED

────────────────────────────────────────────────────────────────
Summary
────────────────────────────────────────────────────────────────
  Critical FMs (RPN ≥ 250): 8 total
    ✅ Passed: 7
    ❌ Failed: 0

✅ FMEA GATE PASSED
All critical failure modes have passing tests.

Report generated:
  - reports/fmea_rpn_report.json
```

---

## Part II: Understanding the Framework

### The Four Stages

```
FMEA → GAP → SUITE → GATE
  ↓      ↓      ↓       ↓
 Risk  Close  Test   Block
```

#### 1. FMEA (Failure Mode & Effects Analysis)

**Input**: Security threat models
**Output**: `docs/fmea/fmea_security.json`

**12 Failure Modes** ranked by RPN (Risk Priority Number):

```
RPN = Severity (1-10) × Occurrence (1-10) × Detection (1-10)
Range: 1-1000
Critical threshold: ≥250
```

**Example FM Entry**:
```json
{
  "id": "FM-05",
  "title": "Tool call injection via transport/parser ambiguity",
  "rpn": 324,
  "priority": "CRITICAL",
  "primary_controls": [
    "apps/erlmcp_core/src/erlmcp_message_parser.erl"
  ],
  "test_paths": [
    "apps/erlmcp_validation/test/erlmcp_protocol_validator_tests.erl"
  ],
  "closure_criterion": "strict validation against schema at boundary"
}
```

#### 2. GAP (Gap Analysis)

**Input**: FMEA registry
**Output**: `docs/security/GAP_SECURITY_INDEX.md`

**Maps each FM to**:
- Implementation modules (where the control lives)
- Test modules (where closure is verified)
- Documentation (GAP_*.md files)
- Closure status (✅/⚠️/❌)

**Example GAP Entry**:
```markdown
### FM-05: Tool Call Injection

**Module Ownership**:
- erlmcp_message_parser.erl (parsing)
- erlmcp_json_rpc.erl (JSON-RPC structure)
- erlmcp_protocol_validator.erl (schema validation)

**Test Closure**:
- erlmcp_protocol_validator_tests.erl (632 lines)
- Coverage: schema validation, unknown field rejection, fuzz tests

**Status**: ✅ CLOSED
```

#### 3. SUITE (Test Suite)

**Input**: Test paths from FMEA registry
**Output**: Pass/fail for each FM

**Test Types**:
- **Unit tests**: Control module behavior (EUnit)
- **Integration tests**: End-to-end scenarios (CT)
- **Destructive tests**: DoS recovery (mailbox_bomb_SUITE)
- **Benchmarks**: Performance under attack (erlmcp_bench_*)

**Example Test Execution**:
```bash
# Run a single FM's tests
rebar3 eunit --module=erlmcp_protocol_validator_tests

# Run all FM tests
rebar3 do eunit, ct

# Run destructive tests (separate)
rebar3 ct --suite=test_destructive/mailbox_bomb_SUITE
```

#### 4. GATE (Quality Gate)

**Input**: Test results from SUITE stage
**Output**: PASS (allow merge) or FAIL (block merge)

**Gate Logic**:
```bash
for fm in CRITICAL_FMS; do
  if ! run_tests(fm); then
    echo "❌ $fm FAILED"
    exit 1  # Block merge
  fi
done

echo "✅ GATE PASSED"
exit 0  # Allow merge
```

**CI Integration**: `.github/workflows/security-fmea-gate.yml`

---

## Part III: Adding a New Failure Mode

### Step 1: Identify the Threat

**Questions to answer**:
1. What is the attack vector?
2. What is the impact if exploited?
3. How likely is exploitation?
4. Can we detect it currently?

**Example**:
- **Attack**: SQL injection in resource metadata
- **Impact**: Database compromise (S=10)
- **Likelihood**: Common web attack (O=6)
- **Detection**: Currently not validated (D=8)
- **RPN**: 10 × 6 × 8 = 480 (CRITICAL!)

### Step 2: Calculate RPN

**Severity (S)**: 1-10
- 9-10: Data exfiltration, auth bypass, RCE
- 7-8: Service unavailable
- 5-6: Service degraded
- 1-4: Minor impact

**Occurrence (O)**: 1-10
- 7-10: Common exploitation path
- 5-6: Occasional (requires conditions)
- 3-4: Rare (requires sophistication)
- 1-2: Very rare (requires multiple failures)

**Detection (D)**: 1-10
- 1-2: Easy to detect (immediate error)
- 3-4: Usually detected
- 5-6: May not detect immediately
- 7-8: Likely to escape
- 9-10: Cannot detect before impact

### Step 3: Add to FMEA Registry

**Edit**: `docs/fmea/fmea_security.json`

```json
{
  "id": "FM-13",
  "title": "SQL injection in resource metadata",
  "category": "Input Validation",
  "severity": 10,
  "occurrence": 6,
  "detection": 8,
  "rpn": 480,
  "priority": "CRITICAL",
  "threat_class": "injection",
  "effect": "database compromise via unvalidated metadata",
  "primary_controls": [
    "apps/erlmcp_core/src/erlmcp_resource_validator.erl"
  ],
  "test_paths": [
    "apps/erlmcp_core/test/erlmcp_resource_validator_tests.erl"
  ],
  "gap_ids": [
    "GAP#50"
  ],
  "closure_criterion": "parameterized queries enforced; all metadata inputs validated; SQL injection tests pass",
  "spec_reference": "MCP 2025-11-25: Resource Metadata, Section 5.3",
  "doc_reference": "docs/GAP50_SQL_INJECTION_PREVENTION.md"
}
```

### Step 4: Implement Controls

**Create control module** (if needed):

```erlang
%% apps/erlmcp_core/src/erlmcp_resource_validator.erl

-module(erlmcp_resource_validator).
-export([validate_metadata/1]).

%% Validate resource metadata to prevent injection
validate_metadata(Metadata) ->
    case is_valid_metadata(Metadata) of
        true ->
            SanitizedMetadata = sanitize_metadata(Metadata),
            {ok, SanitizedMetadata};
        false ->
            {error, invalid_metadata}
    end.

is_valid_metadata(Metadata) ->
    %% Schema validation
    %% No SQL keywords in values
    %% Length limits
    %% Character whitelist
    true.

sanitize_metadata(Metadata) ->
    %% Escape dangerous characters
    %% Enforce parameterized queries
    Metadata.
```

**Integration point**:

```erlang
%% In erlmcp_server.erl, resources/read handler
handle_resource_read(ResourceUri, Metadata) ->
    case erlmcp_resource_validator:validate_metadata(Metadata) of
        {ok, SafeMetadata} ->
            read_resource(ResourceUri, SafeMetadata);
        {error, invalid_metadata} ->
            {error, {invalid_request, <<"Invalid metadata format">>}}
    end.
```

### Step 5: Write Tests

**Create test module**: `apps/erlmcp_core/test/erlmcp_resource_validator_tests.erl`

```erlang
-module(erlmcp_resource_validator_tests).
-include_lib("eunit/include/eunit.hrl").

%% Positive tests
valid_metadata_test() ->
    Metadata = #{<<"author">> => <<"alice">>, <<"version">> => <<"1.0">>},
    ?assertMatch({ok, _}, erlmcp_resource_validator:validate_metadata(Metadata)).

%% Negative tests (SQL injection attempts)
sql_injection_blocked_test() ->
    Metadata = #{<<"author">> => <<"'; DROP TABLE users--">>},
    ?assertEqual({error, invalid_metadata},
                 erlmcp_resource_validator:validate_metadata(Metadata)).

sql_union_injection_blocked_test() ->
    Metadata = #{<<"author">> => <<"alice' UNION SELECT * FROM secrets--">>},
    ?assertEqual({error, invalid_metadata},
                 erlmcp_resource_validator:validate_metadata(Metadata)).

sql_time_based_injection_blocked_test() ->
    Metadata = #{<<"author">> => <<"alice'; WAITFOR DELAY '00:00:05'--">>},
    ?assertEqual({error, invalid_metadata},
                 erlmcp_resource_validator:validate_metadata(Metadata)).

%% Edge cases
empty_metadata_test() ->
    ?assertMatch({ok, _}, erlmcp_resource_validator:validate_metadata(#{})).

max_length_metadata_test() ->
    LongValue = binary:copy(<<"a">>, 10000),
    Metadata = #{<<"field">> => LongValue},
    ?assertEqual({error, invalid_metadata},
                 erlmcp_resource_validator:validate_metadata(Metadata)).
```

### Step 6: Update GAP Index

**Edit**: `docs/security/GAP_SECURITY_INDEX.md`

```markdown
### FM-13: SQL Injection in Resource Metadata

**RPN**: 480 | **Priority**: CRITICAL | **Status**: ✅ **CLOSED**

**Module Ownership**:
- apps/erlmcp_core/src/erlmcp_resource_validator.erl (primary control)
- Integrated into: erlmcp_server.erl (resources/read handler)

**Test Closure**:
- apps/erlmcp_core/test/erlmcp_resource_validator_tests.erl
  • Valid metadata tests (5)
  • SQL injection prevention (10)
  • Edge cases (5)
  = 20 tests total

**Closure Criterion**:
- [x] Parameterized queries enforced
- [x] All metadata inputs validated
- [x] SQL injection tests pass (20/20)

**GAP Reference**: GAP#50 → `docs/GAP50_SQL_INJECTION_PREVENTION.md`
```

### Step 7: Run Validation

```bash
# Run new FM tests
rebar3 eunit --module=erlmcp_resource_validator_tests

# Run full FMEA dashboard
./scripts/validation/generate_fmea_dashboard.sh

# Verify FM-13 appears in report
jq '.failure_modes[] | select(.id == "FM-13")' docs/fmea/fmea_security.json
```

### Step 8: Update Dashboard

The interactive dashboard auto-generates from `docs/fmea/fmea_security.json`. Re-run:

```bash
# Regenerate dashboard
./scripts/validation/generate_fmea_dashboard.sh

# View updated dashboard
open reports/fmea_interactive_dashboard.html
```

---

## Part IV: Interpreting FMEA Reports

### Machine-Readable Report Format

**File**: `reports/fmea_rpn_report.json`

```json
{
  "generated": "2026-02-01T12:00:00Z",
  "fmea_version": "1.0",
  "summary": {
    "total_fms": 12,
    "critical_count": 8,
    "critical_passed": 7,
    "critical_failed": 0,
    "threshold": 250
  },
  "gate_result": {
    "status": "PASSED",
    "failing_fms": []
  },
  "failure_modes": [
    {
      "id": "FM-05",
      "rpn": 324,
      "priority": "CRITICAL",
      "closure_status": "PASSED",
      "test_results": {
        "erlmcp_protocol_validator_tests": "✅ PASSED"
      }
    }
  ]
}
```

### Querying the Report

```bash
# List all CRITICAL priority FMs
jq '.failure_modes[] | select(.priority == "CRITICAL") | {id, title, rpn}' \
   docs/fmea/fmea_security.json

# Find FMs requiring a specific test
jq '.failure_modes[] | select(.test_paths[] | contains("erlmcp_auth")) | .id' \
   docs/fmea/fmea_security.json

# Find highest RPN FMs
jq '.failure_modes | sort_by(.rpn) | reverse | .[:5] | .[] | {id, title, rpn}' \
   docs/fmea/fmea_security.json

# Check gate status
jq '.gate_result.status' reports/fmea_rpn_report.json
```

### Understanding FM Status Indicators

| Indicator | Meaning | Action |
|-----------|---------|--------|
| ✅ CLOSED | All tests passing, FM fully mitigated | None (monitor) |
| ⚠️ PARTIAL | Tests exist, needs verification | Verify CI gates |
| ⚠️ VERIFY | Implementation done, CI gate unclear | Confirm CI workflow |
| ❌ BLOCKED | Tests failing or missing | Fix immediately |

### RPN Color Coding

| RPN Range | Color | Priority | Action |
|-----------|-------|----------|--------|
| 300-1000 | Red | CRITICAL | Immediate fix required |
| 250-299 | Orange | CRITICAL | Priority fix (within sprint) |
| 200-249 | Yellow | HIGH | Schedule fix (1-2 sprints) |
| 100-199 | Blue | MEDIUM | Backlog |
| < 100 | Green | LOW | Monitor |

---

## Part V: Responding to Gate Failures

### Scenario 1: CI Gate Blocks Merge

**Symptom**:
```bash
❌ FMEA GATE FAILED
Critical FMs without passing tests:
  - FM-05:324:Tool call injection via transport/parser ambiguity
```

**Diagnosis**:
```bash
# 1. Find the failing test
jq '.failure_modes[] | select(.id == "FM-05") | .test_paths[]' \
   docs/fmea/fmea_security.json
# Output: apps/erlmcp_validation/test/erlmcp_protocol_validator_tests.erl

# 2. Run the test locally
rebar3 eunit --module=erlmcp_protocol_validator_tests --verbose

# 3. Check for recent changes
git log --oneline -- apps/erlmcp_core/src/erlmcp_message_parser.erl
```

**Remediation**:
1. **Fix the test failure**: Address the root cause in control module
2. **Verify fix**: Run test suite locally
3. **Re-run gate**: Push commit, verify CI passes

**Example Fix**:
```erlang
%% Bug: Unknown fields not rejected
validate_message(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Msg) ->
    case maps:keys(Msg) of
        [<<"jsonrpc">>, <<"method">>, <<"params">>, <<"id">>] ->
            {ok, Msg};
        [<<"jsonrpc">>, <<"method">>, <<"id">>] ->
            {ok, Msg};
        _ ->
            {error, unknown_fields}  % ← FIX: Reject unknown fields
    end.
```

### Scenario 2: New FM Added, Gate Fails

**Symptom**:
```bash
❌ FMEA GATE FAILED
Critical FMs without passing tests:
  - FM-13:480:SQL injection in resource metadata
```

**Expected**: New FMs should NOT block merge until tests are implemented.

**Resolution**:
1. **Verify threshold**: Ensure new FM is marked with appropriate priority
2. **Implement tests**: Follow "Adding a New Failure Mode" guide
3. **Or**: Lower priority temporarily if not blocking release

**Adjust RPN** (if false alarm):
```json
{
  "id": "FM-13",
  "priority": "HIGH",  // Changed from CRITICAL
  "rpn": 240  // Below 250 threshold
}
```

### Scenario 3: Flaky Test

**Symptom**:
```bash
# Sometimes passes, sometimes fails
FM-03: Testing erlmcp_sse_event_store_replay_tests... ❌ FAILED
```

**Diagnosis**:
```bash
# Run test 10 times
for i in {1..10}; do
  rebar3 eunit --module=erlmcp_sse_event_store_replay_tests
done

# Check for race conditions
grep -E "timer:|receive after|spawn" \
  apps/erlmcp_core/test/erlmcp_sse_event_store_replay_tests.erl
```

**Common Causes**:
1. **Race condition**: Test doesn't wait for async operation
2. **Timing assumption**: Test assumes operation completes in X ms
3. **State pollution**: Previous test didn't clean up

**Fix**:
```erlang
%% Bad: Race condition
test_async_operation() ->
    spawn(fun() -> operation() end),
    ?assertEqual(expected, get_result()).  % ← May not be ready

%% Good: Synchronized wait
test_async_operation() ->
    Pid = spawn(fun() -> operation() end),
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, normal} -> ok
    after 5000 ->
        error(timeout)
    end,
    ?assertEqual(expected, get_result()).
```

### Scenario 4: Dependency CVE Detected (FM-12)

**Symptom**:
```bash
❌ FM-12: Dependency audit FAILED
Critical CVE detected: CVE-2025-1234 in jsx 3.1.0
```

**Immediate Action**:
1. **Check severity**: Is it exploitable in erlmcp's usage?
2. **Find patched version**: Check dependency's GitHub releases
3. **Update dependency**: Modify rebar.config
4. **Test**: Ensure no breaking changes
5. **Deploy**: Emergency patch if production is affected

**Example**:
```erlang
%% rebar.config
{deps, [
    {jsx, "3.1.1"}  % Updated from 3.1.0 to patch CVE-2025-1234
]}.
```

```bash
# Update dependencies
rebar3 upgrade jsx

# Run tests
rebar3 do eunit, ct

# Verify CVE is resolved
./scripts/release/scan_vulnerabilities.sh
```

---

## Part VI: Monthly Review Process

### FMEA Quarterly Review (Recommended Cadence)

**When**: First Monday of each quarter
**Duration**: 2 hours
**Participants**: Security lead, SRE lead, 2-3 senior engineers

#### Agenda

**1. Review FMEA Registry (30 min)**
- Are all 12 FMs still relevant?
- Any new threat models discovered?
- Update RPN scores based on production data

**2. Review Closure Status (30 min)**
- Check all critical FMs still have passing tests
- Review any ⚠️ VERIFY items
- Update GAP index

**3. Review Production Incidents (30 min)**
- Did any FM manifest in production?
- Were controls effective?
- Update RPN scores based on actual occurrence

**4. Identify New FMs (30 min)**
- Review security advisories (NIST NVD, GitHub)
- Review recent CVEs in dependencies
- Review penetration test results (if any)
- Add new FMs to backlog

#### Metrics to Review

```bash
# Generate quarterly report
./scripts/validation/generate_fmea_dashboard.sh > /tmp/fmea_report.txt

# Metrics:
# - Total FMs
# - Critical FMs (RPN ≥ 250)
# - Closure rate (✅ / total)
# - Average RPN (trend over time)
# - Gate pass rate (last 90 days)
```

#### Example Review Notes

```markdown
# FMEA Quarterly Review - Q1 2026

**Date**: 2026-04-01
**Attendees**: Alice (Security), Bob (SRE), Charlie (Backend)

## Summary
- Total FMs: 12 → 13 (added FM-13 SQL injection)
- Critical FMs: 8 → 9
- Closure rate: 83% → 92%
- Average RPN: 256 → 248 (improved)

## Changes
1. Added FM-13 (SQL injection) - RPN 480
2. Updated FM-09 RPN: 224 → 180 (improved detection via new monitoring)
3. Closed FM-08 CI verification (⚠️ → ✅)

## Action Items
- [ ] Implement FM-13 controls (Alice, due 2026-04-15)
- [ ] Complete FM-11 WebSocket compliance tests (Bob, due 2026-05-01)
- [ ] Update FMEA dashboard with new FMs (Charlie, due 2026-04-05)

## Next Review: 2026-07-01
```

---

## Part VII: Incident Escalation

### When to Escalate

**Escalate immediately if**:
1. **Critical FM manifests in production** (RPN ≥ 250)
2. **FMEA gate fails in production release branch**
3. **Dependency CVE with known exploit**
4. **Multiple FMs failing simultaneously**

### Escalation Path

**Level 1: Engineering Team** (Response: < 2 hours)
- Engineer notices gate failure
- Attempts to fix locally
- If fix not obvious, escalate to Level 2

**Level 2: Security Lead** (Response: < 1 hour)
- Reviews FMEA report
- Assesses impact
- Decides: fix, rollback, or emergency patch
- If production is affected, escalate to Level 3

**Level 3: Incident Commander** (Response: < 30 min)
- Declares incident
- Coordinates response team
- Authorizes emergency procedures
- Communicates to stakeholders

### Example Incident: FM-02 Session Hijack Detected

**Timeline**:

```
14:00 - Alert: Unusual session access patterns detected
14:05 - SRE investigates, suspects FM-02 (session fixation)
14:10 - Run FMEA dashboard, verify FM-02 controls
14:15 - Check logs: session IDs leaked in application logs (control failed!)
14:20 - Escalate to Security Lead
14:25 - Security Lead confirms: logging regression introduced in PR #1234
14:30 - Escalate to Incident Commander
14:35 - IC declares P1 incident, coordinates response
14:40 - Emergency fix: patch erlmcp_logging.erl to re-enable redaction
14:50 - Deploy patch to production
15:00 - Verify fix: no more session IDs in logs
15:30 - Rotate all session IDs (force re-auth)
16:00 - Incident resolved
```

**Root Cause**: PR #1234 disabled logging redaction for "debugging" but forgot to re-enable.

**Preventive Measures**:
1. Add FM-08 CI gate to block logging regressions
2. Add secret scanning pre-commit hook
3. Update code review checklist to verify logging changes

---

## Part VIII: Best Practices

### DO

✅ **Run FMEA dashboard before every release**
```bash
./scripts/validation/generate_fmea_dashboard.sh --gate --threshold 250
```

✅ **Update RPN scores based on production data**
- If FM-09 (DoS) never manifested: lower Occurrence score
- If FM-08 (logging secrets) happened once: raise Occurrence score

✅ **Document new threats immediately**
- Don't wait for quarterly review
- Add to FMEA registry as soon as discovered

✅ **Use FMEA reports in retrospectives**
- Did this incident match any FM?
- Should we add a new FM?

✅ **Keep tests green**
- Fix failing tests immediately
- Don't disable tests to "unblock" merge

### DON'T

❌ **Don't lower RPN to bypass gate**
- If FM is truly low-risk, document why
- Get security lead approval

❌ **Don't skip quarterly reviews**
- FMEA becomes stale without regular updates

❌ **Don't implement controls without tests**
- Untested controls = false sense of security

❌ **Don't ignore ⚠️ VERIFY items**
- These are technical debt that accumulates

❌ **Don't merge PRs with failing FMEA gate**
- Gate exists to prevent regressions
- Exception requires incident commander approval

---

## Part IX: Troubleshooting

### Problem: Dashboard script fails

```bash
$ ./scripts/validation/generate_fmea_dashboard.sh
./scripts/validation/generate_fmea_dashboard.sh: line 52: jq: command not found
```

**Solution**: Install `jq`
```bash
# macOS
brew install jq

# Linux
sudo apt-get install jq

# Verify
jq --version
```

### Problem: rebar3 not found

```bash
$ rebar3 eunit
bash: rebar3: command not found
```

**Solution**: Install rebar3
```bash
# Download
wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3

# Add to PATH
sudo mv rebar3 /usr/local/bin/

# Verify
rebar3 version
```

### Problem: Tests pass locally, fail in CI

**Diagnosis**:
```bash
# Check CI environment
cat .github/workflows/security-fmea-gate.yml

# Common issues:
# 1. Different Erlang/OTP version
# 2. Missing dependencies
# 3. Different TERM setting
```

**Solution**:
```yaml
# .github/workflows/security-fmea-gate.yml
jobs:
  fmea-gate:
    runs-on: ubuntu-latest
    env:
      TERM: dumb  # Fix terminal issues
    steps:
      - uses: erlang/setup-beam@v1
        with:
          otp-version: '28.3.1'  # Match local version
```

### Problem: HTML dashboard doesn't update

**Cause**: Dashboard is static HTML, needs regeneration.

**Solution**:
```bash
# Regenerate dashboard
./scripts/validation/generate_fmea_dashboard.sh

# Refresh browser
open reports/fmea_interactive_dashboard.html
```

---

## Part X: Reference

### File Locations

| File | Purpose |
|------|---------|
| `docs/fmea/fmea_security.json` | FMEA registry (source of truth) |
| `docs/security/GAP_SECURITY_INDEX.md` | FM→GAP→SUITE mapping |
| `scripts/validation/generate_fmea_dashboard.sh` | Dashboard generator |
| `reports/fmea_rpn_report.json` | Machine-readable report |
| `reports/fmea_interactive_dashboard.html` | Visual dashboard |
| `reports/FMEA_VALIDATION_EVIDENCE_BUNDLE.md` | Comprehensive evidence |

### Commands Reference

```bash
# Generate dashboard (report mode)
./scripts/validation/generate_fmea_dashboard.sh

# Generate dashboard (gate mode)
./scripts/validation/generate_fmea_dashboard.sh --gate --threshold 250

# Run all tests
rebar3 do eunit, ct

# Run specific FM test
rebar3 eunit --module=erlmcp_protocol_validator_tests

# Query FMEA registry
jq '.failure_modes[] | select(.id == "FM-05")' docs/fmea/fmea_security.json

# Check gate status
jq '.gate_result.status' reports/fmea_rpn_report.json

# View interactive dashboard
open reports/fmea_interactive_dashboard.html
```

### Contacts

| Role | Responsibility | Contact |
|------|----------------|---------|
| **Security Lead** | FMEA ownership, RPN updates | security@erlmcp.dev |
| **SRE Lead** | Gate operations, incident response | sre@erlmcp.dev |
| **Engineering Lead** | Control implementation | engineering@erlmcp.dev |
| **Incident Commander** | P1/P2 incident coordination | oncall@erlmcp.dev |

### Further Reading

- **FMEA Methodology**: `docs/FMEA_FAILURE_MODE_ANALYSIS.md`
- **Security Economics**: `docs/SECURITY_FMEA_ECONOMICS_FRAMEWORK.md`
- **Evidence Bundle**: `reports/FMEA_VALIDATION_EVIDENCE_BUNDLE.md`
- **GAP Index**: `docs/security/GAP_SECURITY_INDEX.md`
- **MCP Spec**: `docs/protocol.md`

---

## Appendix A: FMEA Cheat Sheet

### Quick FM Lookup

| FM ID | Title | RPN | Status |
|-------|-------|-----|--------|
| FM-01 | Origin bypass | 216 | ✅ |
| FM-02 | Session hijack | 300 | ✅ |
| FM-03 | SSE cross-client | 280 | ✅ |
| FM-04 | Auth bypass | 250 | ✅ |
| FM-05 | Tool injection | 324 | ✅ |
| FM-06 | Header downgrade | 240 | ✅ |
| FM-07 | Path traversal | 240 | ✅ |
| FM-08 | Logging secrets | 270 | ⚠️ |
| FM-09 | DoS exhaustion | 224 | ✅ |
| FM-10 | Task cross-bleed | 280 | ✅ |
| FM-11 | WS fragmentation | 216 | ⚠️ |
| FM-12 | Supply chain | 240 | ⚠️ |

### RPN Calculation Examples

**Example 1: SQL Injection**
- Severity: 10 (database compromise)
- Occurrence: 6 (common attack)
- Detection: 8 (no validation currently)
- **RPN = 10 × 6 × 8 = 480** (CRITICAL!)

**Example 2: Log File Rotation Failure**
- Severity: 4 (disk space issues)
- Occurrence: 3 (rare misconfiguration)
- Detection: 2 (easy to detect via monitoring)
- **RPN = 4 × 3 × 2 = 24** (LOW)

---

**Document Status**: FINAL
**Version**: 1.0
**Last Updated**: 2026-02-01
**Next Review**: 2026-04-01

---

**End of Operational Guide**
