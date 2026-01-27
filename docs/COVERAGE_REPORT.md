# TCPS Code Coverage Report
## Lean Six Sigma Quality Standard: 80%+ Coverage Target

**Report Generated:** 2026-01-26
**Build Profile:** test
**Coverage Tool:** rebar3 cover + Erlang OTP cover module

---

## Executive Summary

| Metric | Value | Status | Target |
|--------|-------|--------|--------|
| **Overall Coverage** | **1%** | ❌ **CRITICAL** | ≥80% |
| **Total Modules** | 53 | - | - |
| **Modules ≥80%** | 0 | ❌ | 53 (100%) |
| **Modules ≥90%** | 0 | ❌ | 4 critical |
| **Modules <80%** | 53 | ❌ **CRITICAL** | 0 |
| **Critical Modules ≥90%** | 0/4 | ❌ | 4/4 (100%) |

### Quality Gate Status: ❌ **FAILED**

**CRITICAL ISSUE:** Overall coverage is 1%, far below the 80% Lean Six Sigma standard. Immediate action required to bring all modules to production quality.

---

## Per-Module Coverage Analysis

### Critical Modules (Must be ≥90%)

These modules implement core TCPS quality gates and safety mechanisms:

| Module | Lines | Covered | Coverage | Status | Gap to 90% |
|--------|-------|---------|----------|--------|------------|
| `tcps_andon` | ~400 | ~12 | **3%** | ❌ | **-87%** |
| `tcps_root_cause` | ~300 | 0 | **0%** | ❌ | **-90%** |
| `tcps_receipt_verifier` | ~250 | 0 | **0%** | ❌ | **-90%** |
| `tcps_rebar3_quality` | ~350 | 0 | **0%** | ❌ | **-90%** |

**Critical Priority:** These modules control production quality gates and must achieve ≥90% coverage immediately.

### Core TCPS Modules (Must be ≥80%)

| Module | Coverage | Status | Priority | Gap to 80% |
|--------|----------|--------|----------|------------|
| `tcps_kanban` | 0% | ❌ | HIGH | -80% |
| `tcps_kaizen` | 0% | ❌ | HIGH | -80% |
| `tcps_work_order` | N/A* | ❌ | HIGH | -80% |
| `tcps_persistence` | 0% | ❌ | HIGH | -80% |
| `tcps_deterministic` | 1% | ❌ | HIGH | -79% |
| `tcps_health` | 0% | ❌ | MEDIUM | -80% |
| `tcps_dashboard` | 0% | ❌ | MEDIUM | -80% |
| `tcps_dashboard_handler` | 0% | ❌ | MEDIUM | -80% |
| `tcps_dashboard_sse_handler` | 0% | ❌ | MEDIUM | -80% |
| `tcps_metrics_aggregator` | 0% | ❌ | MEDIUM | -80% |
| `tcps_metrics_cache` | 0% | ❌ | MEDIUM | -80% |
| `tcps_ontology_index` | 0% | ❌ | MEDIUM | -80% |
| `tcps_query_cache` | 0% | ❌ | MEDIUM | -80% |
| `tcps_rdf_incremental` | 0% | ❌ | MEDIUM | -80% |
| `tcps_sse_manager` | 0% | ❌ | MEDIUM | -80% |
| `tcps_rebar3_andon` | 0% | ❌ | HIGH | -80% |
| `tcps_rebar3_receipt` | 0% | ❌ | HIGH | -80% |
| `tcps_rebar3_shacl` | 0% | ❌ | HIGH | -80% |

*Note: `tcps_work_order.beam` shows "no abstract code" - requires recompilation with debug_info.

### ErlMCP Core Transport Modules (Must be ≥80%)

| Module | Coverage | Status | Priority | Gap to 80% |
|--------|----------|--------|----------|------------|
| `erlmcp` | 0% | ❌ | HIGH | -80% |
| `erlmcp_app` | 0% | ❌ | HIGH | -80% |
| `erlmcp_client` | 0% | ❌ | HIGH | -80% |
| `erlmcp_json_rpc` | 0% | ❌ | HIGH | -80% |
| `erlmcp_registry` | **72%** | ⚠️ | HIGH | -8% |
| `erlmcp_server` | 0% | ❌ | HIGH | -80% |
| `erlmcp_server_new` | 0% | ❌ | HIGH | -80% |
| `erlmcp_server_sup` | 0% | ❌ | HIGH | -80% |
| `erlmcp_stdio` | 0% | ❌ | HIGH | -80% |
| `erlmcp_stdio_server` | 0% | ❌ | HIGH | -80% |
| `erlmcp_sup` | 0% | ❌ | HIGH | -80% |
| `erlmcp_transport` | 0% | ❌ | HIGH | -80% |
| `erlmcp_transport_http` | 0% | ❌ | HIGH | -80% |
| `erlmcp_transport_stdio` | 0% | ❌ | HIGH | -80% |
| `erlmcp_transport_stdio_new` | 0% | ❌ | HIGH | -80% |
| `erlmcp_transport_sup` | 0% | ❌ | HIGH | -80% |
| `erlmcp_transport_tcp` | 0% | ❌ | HIGH | -80% |
| `erlmcp_version` | 0% | ❌ | LOW | -80% |

**Notable:** `erlmcp_registry` at 72% is closest to threshold - needs only 8% more coverage.

### CLI Modules (Acceptable <80%)

CLI modules are interactive and difficult to test automatically. Coverage <80% is acceptable for these:

| Module | Coverage | Status | Notes |
|--------|----------|--------|-------|
| `tcps_cli_andon` | 0% | ℹ️ | Interactive CLI - manual testing |
| `tcps_cli_config` | 0% | ℹ️ | Configuration CLI |
| `tcps_cli_examples` | 0% | ℹ️ | Example generator |
| `tcps_cli_format` | 0% | ℹ️ | Output formatting |
| `tcps_cli_kaizen` | 0% | ℹ️ | Interactive Kaizen CLI |
| `tcps_cli_kanban` | 0% | ℹ️ | Interactive Kanban CLI |
| `tcps_cli_quality` | 0% | ℹ️ | Quality report CLI |
| `tcps_cli_receipt` | 0% | ℹ️ | Receipt viewer CLI |
| `tcps_cli_root_cause` | 0% | ℹ️ | 5 Whys CLI |
| `tcps_cli_tpm` | 0% | ℹ️ | TPM metrics CLI |
| `tcps_cli_work_order` | 0% | ℹ️ | Work order CLI |

### Utility & Support Modules

| Module | Coverage | Status |
|--------|----------|--------|
| `rdf_utils` | 0% | ❌ |
| `rebar3_tcps_plugin` | 0% | ℹ️ |

---

## Coverage Gaps Analysis

### High-Priority Uncovered Code Paths

Based on module analysis, the following critical code paths lack test coverage:

#### 1. **tcps_andon.erl** (3% coverage)
**Uncovered Critical Paths:**
- Andon cord pull simulation (stop-the-line)
- Escalation workflows when quality gates fail
- Email/notification sending on Andon events
- Multi-level escalation (team → supervisor → management)
- Andon resolution and acknowledgment flows

**Recommended Tests:**
```erlang
% test/tcps_andon_tests.erl - Additional tests needed
- test_andon_pull_creates_event/0
- test_andon_escalation_levels/0
- test_andon_email_notification/0
- test_andon_resolution_workflow/0
- test_andon_event_persistence/0
- test_andon_concurrent_pulls/0
```

#### 2. **tcps_root_cause.erl** (0% coverage)
**Uncovered Critical Paths:**
- 5 Whys analysis creation
- Root cause determination logic
- Countermeasure tracking
- Effectiveness validation

**Recommended Tests:**
```erlang
% test/tcps_root_cause_tests.erl - Tests exist but not running
- Verify test compilation errors are fixed
- Add edge case testing (empty inputs, invalid data)
- Add concurrent analysis testing
- Add persistence testing
```

#### 3. **tcps_receipt_verifier.erl** (0% coverage)
**Uncovered Critical Paths:**
- Receipt signature verification
- Merkle tree validation
- Deterministic build verification
- Tamper detection

**Recommended Tests:**
```erlang
% test/tcps_receipt_verifier_tests.erl - New tests needed
- test_verify_valid_receipt/0
- test_detect_tampered_receipt/0
- test_verify_merkle_tree/0
- test_verify_build_determinism/0
- test_verify_signature_chain/0
```

#### 4. **tcps_rebar3_quality.erl** (0% coverage)
**Uncovered Critical Paths:**
- Quality gate checks (pass rate, coverage)
- Build blocking on quality failures
- Quality metrics aggregation
- Threshold validation

**Recommended Tests:**
```erlang
% test/tcps_rebar3_quality_tests.erl - Provider integration tests
- test_quality_gates_pass_with_good_metrics/0
- test_quality_gates_fail_below_threshold/0
- test_quality_gates_check_coverage/0
- test_quality_gates_check_pass_rate/0
```

#### 5. **erlmcp_registry.erl** (72% coverage - closest to threshold)
**Uncovered Paths (28% gap):**
- Concurrent registration edge cases
- Process monitoring cleanup edge cases
- Transport unbinding edge cases

**Recommended Tests:**
```erlang
% test/erlmcp_registry_tests.erl - Fix compilation errors
- Fix erlmcp_registry_gproc_tests.erl guard expression errors
- Add stress testing for concurrent operations
- Add failure mode testing (process crashes during registration)
```

#### 6. **Core ErlMCP Modules** (0% coverage)
**Critical Missing Tests:**
- `erlmcp_client` - Client connection, message sending
- `erlmcp_server` - Server lifecycle, request handling
- `erlmcp_json_rpc` - JSON-RPC parsing, validation
- `erlmcp_transport_*` - All transport layers

**Recommended Tests:**
```erlang
% test/erlmcp_client_tests.erl
% test/erlmcp_server_tests.erl (may exist but not running)
% test/erlmcp_transport_tests.erl
- Full end-to-end integration tests
- Protocol compliance tests
- Error handling tests
```

---

## Compilation Issues Blocking Coverage

### 1. Test Files with Compilation Errors

These test files fail to compile, preventing coverage measurement:

| Test File | Error | Fix Required |
|-----------|-------|--------------|
| `erlmcp_registry_gproc_tests.erl` | Illegal guard expression: `is_process_alive/1` in guard | Move to function body |
| `tcps_ontology_benchmark.erl` | Illegal guard: `rand:uniform/1` in guard | Move to function body or use case |

**Action:** Fix guard expressions in test files to enable test execution.

### 2. Source Files Without Abstract Code

| Source File | Issue | Fix |
|-------------|-------|-----|
| `erlmcp_templates.beam` | No abstract code | Recompile with `debug_info` |
| `tcps_work_order.beam` | No abstract code | Recompile with `debug_info` |

**Action:** Ensure all source files compiled with `{erl_opts, [debug_info]}` in test profile.

---

## Test Suite Status

### Existing Tests

| Test Suite | Status | Coverage Contribution |
|------------|--------|----------------------|
| EUnit tests | ✅ Running | Minimal (1% overall) |
| Common Test (CT) integration | ⚠️ Partial | Some modules covered |
| Property-based (PropER) | ❓ Unknown | Not evaluated |

### Test Execution Summary

```
Total Test Files: ~30
Passing Tests: ~15
Failing Tests: ~2 (compilation errors)
Skipped Tests: ~13 (not being executed)
```

**Issue:** Many test files exist but are not being executed during coverage runs.

---

## Action Plan to Achieve 80%+ Coverage

### Phase 1: Fix Test Infrastructure (Week 1)

**Priority: CRITICAL**

1. **Fix Compilation Errors**
   - [ ] Fix `erlmcp_registry_gproc_tests.erl` guard expressions
   - [ ] Fix `tcps_ontology_benchmark.erl` guard expressions
   - [ ] Ensure all source modules compiled with `debug_info`
   - [ ] Verify all test modules compile successfully

2. **Enable All Existing Tests**
   - [ ] Investigate why existing tests aren't running
   - [ ] Fix test discovery issues (eunit vs CT)
   - [ ] Ensure all test_* functions are being executed
   - [ ] Run full test suite: `rebar3 as test do eunit, ct`

**Expected Coverage Gain:** 1% → 15-25%

### Phase 2: Cover Critical Modules (Week 2)

**Priority: HIGH**

3. **tcps_andon.erl: 3% → 90%** (Gap: 87%)
   - [ ] Add 15+ test cases for Andon events
   - [ ] Test escalation workflows
   - [ ] Test notification system
   - [ ] Test concurrent Andon pulls
   - [ ] Target: 150+ lines of test code

4. **tcps_root_cause.erl: 0% → 90%** (Gap: 90%)
   - [ ] Fix existing test compilation
   - [ ] Add 20+ test cases for 5 Whys analysis
   - [ ] Test root cause determination
   - [ ] Test countermeasure tracking
   - [ ] Target: 200+ lines of test code

5. **tcps_receipt_verifier.erl: 0% → 90%** (Gap: 90%)
   - [ ] Create new test file
   - [ ] Add 15+ test cases for verification
   - [ ] Test signature validation
   - [ ] Test tamper detection
   - [ ] Test Merkle tree validation
   - [ ] Target: 180+ lines of test code

6. **tcps_rebar3_quality.erl: 0% → 90%** (Gap: 90%)
   - [ ] Add 12+ provider integration tests
   - [ ] Test quality gate pass/fail scenarios
   - [ ] Test threshold validation
   - [ ] Test metrics aggregation
   - [ ] Target: 150+ lines of test code

**Expected Coverage Gain:** 15-25% → 40-50%

### Phase 3: Cover Core TCPS Modules (Week 3)

**Priority: HIGH**

7. **TCPS Core Modules: 0% → 80%** (13 modules)
   - tcps_kanban
   - tcps_kaizen
   - tcps_persistence
   - tcps_deterministic
   - tcps_work_order
   - tcps_rebar3_andon
   - tcps_rebar3_receipt
   - tcps_rebar3_shacl
   - tcps_health
   - tcps_dashboard
   - tcps_metrics_*
   - tcps_query_cache
   - tcps_ontology_index

   - [ ] Create/enhance test files for each module
   - [ ] Target: 10-15 test cases per module
   - [ ] Focus on critical code paths
   - [ ] Add integration tests

**Expected Coverage Gain:** 40-50% → 65-75%

### Phase 4: Cover ErlMCP Transport Layer (Week 4)

**Priority: HIGH**

8. **ErlMCP Core: 0-72% → 80%+** (18 modules)
   - erlmcp_registry (72% → 80%: add 8%)
   - erlmcp_client (0% → 80%)
   - erlmcp_server (0% → 80%)
   - erlmcp_json_rpc (0% → 80%)
   - erlmcp_transport_* (all 0% → 80%)

   - [ ] Fix existing test files
   - [ ] Add end-to-end integration tests
   - [ ] Add protocol compliance tests
   - [ ] Add failure mode tests
   - [ ] Target: 250+ lines of new test code

**Expected Coverage Gain:** 65-75% → 80%+

---

## HTML Coverage Reports

**Location:** `_build/test/cover/index.html`

**View Command:**
```bash
open _build/test/cover/index.html
```

**Per-Module Reports:**
- Detailed line-by-line coverage highlighting
- Green = covered lines
- Red = uncovered lines
- Directory: `_build/test/cover/aggregate/`

**Report Contents:**
- Module coverage percentages
- Line-by-line coverage visualization
- Function coverage analysis
- Branch coverage (where applicable)

---

## CI/CD Integration

### GitHub Actions Coverage Enforcement

**File:** `.github/workflows/ci.yml`

**Proposed Coverage Check:**
```yaml
- name: Run Tests with Coverage
  run: |
    rebar3 as test do compile, eunit --cover, ct --dir=test/integration --cover, cover --verbose

- name: Check Coverage Threshold
  run: |
    ./scripts/check_coverage_threshold.sh 80

- name: Upload Coverage Report
  if: always()
  uses: actions/upload-artifact@v3
  with:
    name: coverage-report
    path: _build/test/cover/

- name: Publish Coverage Summary
  run: |
    echo "## Coverage Report" >> $GITHUB_STEP_SUMMARY
    echo "" >> $GITHUB_STEP_SUMMARY
    grep "total" _build/test/cover/cover.log >> $GITHUB_STEP_SUMMARY
```

**Enforcement Policy:**
- [ ] Block PR merges if coverage <80%
- [ ] Require manual override for coverage failures
- [ ] Generate coverage report artifacts for every build
- [ ] Add coverage badge to README.md

---

## Summary Statistics

### Current State (2026-01-26)
```
Total Modules: 53
Coverage: 1% (CRITICAL - 79% below target)
Modules ≥80%: 0/53 (0%)
Critical Modules ≥90%: 0/4 (0%)
```

### Target State (Lean Six Sigma)
```
Total Modules: 53
Coverage: ≥80% (target)
Modules ≥80%: 53/53 (100%)
Critical Modules ≥90%: 4/4 (100%)
```

### Effort Required
```
Estimated Test Code: 2,000-3,000 lines
Estimated Time: 4 weeks (1 developer)
Test Files to Create/Fix: ~25
Test Cases to Add: ~250+
```

---

## Recommendations

### Immediate Actions (This Week)
1. ✅ **Fix test compilation errors** - Blocking all progress
2. ✅ **Run coverage generation script** - `./scripts/generate_coverage.sh`
3. ✅ **Identify why existing tests aren't running** - Many tests exist but don't execute
4. ✅ **Fix erlmcp_registry to 80%** - Only needs 8% more (easiest win)

### Short-Term Actions (Next 2 Weeks)
5. ⚠️ **Cover critical modules to 90%** - tcps_andon, tcps_root_cause, tcps_receipt_verifier, tcps_rebar3_quality
6. ⚠️ **Cover core TCPS modules to 80%** - 13 modules
7. ⚠️ **Add CI/CD coverage enforcement** - Prevent regressions

### Long-Term Actions (Next 4 Weeks)
8. ⚠️ **Cover all ErlMCP transport modules to 80%** - 18 modules
9. ⚠️ **Achieve overall 80%+ coverage** - Lean Six Sigma compliance
10. ⚠️ **Set up continuous coverage monitoring** - Prevent future degradation

---

## References

- **Coverage HTML Reports:** `_build/test/cover/index.html`
- **Coverage Log:** `_build/test/cover/cover.log`
- **Test Suites:** `test/` and `test/integration/`
- **Coverage Scripts:** `scripts/generate_coverage.sh`, `scripts/check_coverage_threshold.sh`
- **Lean Six Sigma Standards:** 80%+ code coverage, 99.99966% defect-free

---

**Generated by:** TCPS Coverage Analysis Agent
**Compliance:** Lean Six Sigma Level Strictness (Zero-Defect Quality)
**Next Review:** After Phase 1 completion (Week 1)
