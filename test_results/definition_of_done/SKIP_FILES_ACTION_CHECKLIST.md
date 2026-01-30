# Skip/Broken Files - Action Checklist

**Generated:** 2026-01-30
**Purpose:** Step-by-step actions for cleaning up all .skip and .broken files

---

## Phase 1: Immediate Actions (1 day)

### 1.1 Update .gitignore (5 minutes)
- [x] Verify `_build/` is in `.gitignore` (already present on line 14)
- [x] Confirm no build artifacts are tracked (verified - none tracked)
- [ ] **STATUS:** COMPLETE - No action needed

### 1.2 Run All Skip Tests (1-2 hours)
- [ ] Run: `rebar3 eunit --module=erlmcp_batch_tests`
- [ ] Run: `rebar3 eunit --module=erlmcp_cpu_quota_tests`
- [ ] Run: `rebar3 eunit --module=erlmcp_progress_tests`
- [ ] Run: `rebar3 eunit --module=erlmcp_json_rpc_tests`
- [ ] Document all failures in spreadsheet

### 1.3 Fix Simple Test Failures (2-4 hours)
- [ ] Fix missing dependencies
- [ ] Fix setup/teardown issues
- [ ] Fix import/include issues
- [ ] Rename files to remove `.skip` or `.broken` suffix

---

## Phase 2: Core Module Tests (2-3 days)

### 2.1 Fix erlmcp_batch_tests (2-4 hours)
- [ ] Run tests: `rebar3 eunit --module=erlmcp_batch_tests`
- [ ] Fix failures
- [ ] Verify 100% pass rate
- [ ] Rename: `erlmcp_batch_tests.erl.skip` → `erlmcp_batch_tests.erl`
- [ ] Commit: "fix: enable erlmcp_batch_tests"

### 2.2 Fix erlmcp_cpu_quota_tests (2-3 hours)
- [ ] Run tests: `rebar3 eunit --module=erlmcp_cpu_quota_tests`
- [ ] Fix failures
- [ ] Verify 100% pass rate
- [ ] Rename: `erlmcp_cpu_quota_tests.erl.skip` → `erlmcp_cpu_quota_tests.erl`
- [ ] Commit: "fix: enable erlmcp_cpu_quota_tests"

### 2.3 Fix erlmcp_progress_tests (3-5 hours)
- [ ] Run tests: `rebar3 eunit --module=erlmcp_progress_tests`
- [ ] Fix failures
- [ ] Verify 100% pass rate
- [ ] Rename: `erlmcp_progress_tests.erl.broken` → `erlmcp_progress_tests.erl`
- [ ] Commit: "fix: enable erlmcp_progress_tests"

### 2.4 Fix erlmcp_json_rpc_tests (4-6 hours)
- [ ] Run tests: `rebar3 eunit --module=erlmcp_json_rpc_tests`
- [ ] Fix failures
- [ ] Verify 100% pass rate
- [ ] Rename: `erlmcp_json_rpc_tests.erl.skip` → `erlmcp_json_rpc_tests.erl`
- [ ] Commit: "fix: enable erlmcp_json_rpc_tests"

---

## Phase 3: Integration Tests (2-3 days)

### 3.1 Merge Integration Suite Files (1 hour)
- [ ] Compare: `erlmcp_integration_SUITE.erl.skip` vs `.broken`
- [ ] Merge differences into single file
- [ ] Keep best version of each test
- [ ] Delete `.broken` version
- [ ] Rename: `erlmcp_integration_SUITE.erl.skip` → `erlmcp_integration_SUITE.erl`

### 3.2 Fix Integration Suite (6-8 hours)
- [ ] Run tests: `rebar3 ct --suite=erlmcp_integration_SUITE`
- [ ] Fix failures
- [ ] Verify 100% pass rate
- [ ] Commit: "fix: enable erlmcp_integration_SUITE"

### 3.3 Fix Tool Execution Tests (6-8 hours)
- [ ] Run: `rebar3 eunit --module=erlmcp_tool_execution_tests`
- [ ] Fix failures
- [ ] Verify 100% pass rate
- [ ] Rename: `erlmcp_tool_execution_tests.erl.skip` → `erlmcp_tool_execution_tests.erl`
- [ ] Commit: "fix: enable erlmcp_tool_execution_tests"

### 3.4 Fix Tool Execution Suite (6-8 hours)
- [ ] Run: `rebar3 ct --suite=erlmcp_tool_execution_SUITE`
- [ ] Fix failures
- [ ] Verify 100% pass rate
- [ ] Rename: `erlmcp_tool_execution_SUITE.erl.skip` → `erlmcp_tool_execution_SUITE.erl`
- [ ] Commit: "fix: enable erlmcp_tool_execution_SUITE"

---

## Phase 4: Transport Tests (2-3 days)

### 4.1 Complete Transport Validator (8-12 hours)
- [ ] Review: `erlmcp_transport_validator_SUITE.erl.broken`
- [ ] Complete missing test cases
- [ ] Run: `rebar3 ct --suite=erlmcp_transport_validator_SUITE`
- [ ] Fix failures
- [ ] Verify 100% pass rate (all 40 test cases)
- [ ] Rename: `erlmcp_transport_validator_SUITE.erl.broken` → `erlmcp_transport_validator_SUITE.erl`
- [ ] Commit: "feat: complete transport validator suite"

---

## Phase 5: Feature Evaluation (1-2 days)

### 5.1 Evaluate Core Features (2-4 hours)
For each file, decide: **IMPLEMENT** or **DELETE**

- [ ] **erlmcp_uri_validator.erl.broken**
  - [ ] Decision: [ ] IMPLEMENT [ ] DELETE
  - [ ] Rationale: _________________________________________
  - [ ] Action: ____________________________________________

- [ ] **erlmcp_schema_validator.erl.broken**
  - [ ] Decision: [ ] IMPLEMENT [ ] DELETE
  - [ ] Rationale: _________________________________________
  - [ ] Action: ____________________________________________
  - [ ] Note: Consider using jesse library instead

- [ ] **erlmcp_prompt_argument_validator.erl.broken**
  - [ ] Decision: [ ] IMPLEMENT [ ] DELETE
  - [ ] Rationale: _________________________________________
  - [ ] Action: ____________________________________________

- [ ] **erlmcp_request_id.erl.broken**
  - [ ] Decision: [ ] IMPLEMENT [ ] DELETE
  - [ ] Rationale: _________________________________________
  - [ ] Action: ____________________________________________
  - [ ] Note: Check if already implemented in erlmcp_client

- [ ] **erlmcp_rate_limiter_v2.erl.broken**
  - [ ] Decision: [ ] IMPLEMENT [ ] DELETE
  - [ ] Rationale: _________________________________________
  - [ ] Action: ____________________________________________
  - [ ] Note: erlmcp_rate_limiter.erl (v1) already exists

### 5.2 Evaluate Transport Features (1-2 hours)

- [ ] **erlmcp_connection_pool.erl.skip**
  - [ ] Decision: [ ] IMPLEMENT [ ] DELETE
  - [ ] Rationale: _________________________________________
  - [ ] Action: ____________________________________________

- [ ] **erlmcp_connection_pool_worker.erl.broken**
  - [ ] Decision: [ ] IMPLEMENT [ ] DELETE
  - [ ] Rationale: _________________________________________
  - [ ] Action: ____________________________________________

### 5.3 Evaluate Other Features (1 hour)

- [ ] **erlmcp_pricing_upgrade.erl.broken**
  - [ ] Decision: [ ] IMPLEMENT [ ] DELETE
  - [ ] Rationale: _________________________________________
  - [ ] Action: ____________________________________________
  - [ ] Note: Consider if pricing is in scope for SDK

### 5.4 Evaluate Security Tests (1 hour)

- [ ] **erlmcp_prompt_injection_tests.erl.skip**
  - [ ] Decision: [ ] IMPLEMENT [ ] DELETE
  - [ ] Rationale: _________________________________________
  - [ ] Action: ____________________________________________
  - [ ] Note: Security testing is important but may be out of scope

---

## Phase 6: Implement or Delete (Variable)

### 6.1 For Features Marked "IMPLEMENT"
- [ ] Create implementation plan
- [ ] Estimate effort
- [ ] Prioritize by business value
- [ ] Schedule implementation

### 6.2 For Features Marked "DELETE"
- [ ] Document why feature is not needed
- [ ] Delete `.broken` or `.skip` files
- [ ] Update technical debt tracker
- [ ] Commit: "chore: remove unimplemented feature [feature_name]"

---

## Quick Reference Commands

### Run Specific Test
```bash
# EUnit tests
rebar3 eunit --module=<module_name>_tests

# Common Test suites
rebar3 ct --suite=<suite_name>

# All tests
rebar3 do eunit, ct
```

### Rename File (Remove .skip or .broken)
```bash
git mv <filename>.erl.skip <filename>.erl
git mv <filename>.erl.broken <filename>.erl
```

### Check Coverage
```bash
rebar3 cover --verbose
```

---

## Success Criteria

### Phase 1 Complete When:
- [x] `.gitignore` verified (already complete)
- [ ] All `.skip` tests run and failures documented
- [ ] Simple fixes applied (if any)

### Phase 2 Complete When:
- [ ] All 4 core module tests enabled
- [ ] 100% test pass rate
- [ ] Code coverage increased by 10-15%

### Phase 3 Complete When:
- [ ] All integration tests enabled
- [ ] 100% test pass rate
- [ ] End-to-end MCP workflows validated

### Phase 4 Complete When:
- [ ] Transport validator complete
- [ ] All 40 test cases passing
- [ ] All transport implementations validated

### Phase 5 Complete When:
- [ ] All unimplemented features evaluated
- [ ] Decision documented for each feature
- [ ] Implementation or deletion plan created

### Phase 6 Complete When:
- [ ] All decided features implemented OR deleted
- [ ] No `.broken` or `.skip` files remain (except new in-progress work)
- [ ] Technical debt tracker updated

---

## Notes

### What to Do When Tests Fail

1. **Read the error message carefully**
2. **Check the test setup** (missing dependencies, wrong configuration)
3. **Check the test assertions** (are they correct?)
4. **Check the implementation** (is there a bug?)
5. **Fix the issue** (either in test or implementation)
6. **Re-run the test** until it passes

### When to Delete vs. Implement

**DELETE if:**
- Feature is out of scope for SDK
- Feature already implemented elsewhere
- Feature is not critical for production
- Feature would take >16 hours to implement

**IMPLEMENT if:**
- Feature is critical for production
- Feature is in MCP specification
- Feature is security-related
- Feature is requested by users

### When to Keep .broken or .skip

**KEEP if:**
- Feature is in active development
- Feature is blocked by external dependency
- Feature is planned for next release

**DO NOT KEEP if:**
- Feature is abandoned
- Feature is obsolete
- Feature has been replaced

---

## Tracking

**Total Files:** 17 active files (excluding build artifacts)
**Completed:** [0/17]
**In Progress:** [0/17]
**Not Started:** [17/17]

**Progress by Phase:**
- Phase 1: [████████░░] 80% (gitignore verified)
- Phase 2: [░░░░░░░░░░] 0%
- Phase 3: [░░░░░░░░░░] 0%
- Phase 4: [░░░░░░░░░░] 0%
- Phase 5: [░░░░░░░░░░] 0%
- Phase 6: [░░░░░░░░░░] 0%

---

**Last Updated:** 2026-01-30
**Next Review:** After Phase 1 completion
