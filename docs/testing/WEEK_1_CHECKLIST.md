# Week 1 Test Implementation Checklist

**Sprint 1: Core Infrastructure Testing**
**Duration:** Week 1 (5 working days)
**Team:** 2 test engineers
**Goal:** 5 critical modules tested with 85%+ coverage

---

## Day 1: Setup & erlmcp_subscription

### Morning: Environment Setup (Engineer 1 + 2)

- [ ] Clone erlmcp repository
- [ ] Run `rebar3 compile` to verify build
- [ ] Run existing tests: `rebar3 eunit` and `rebar3 ct`
- [ ] Generate baseline coverage: `rebar3 cover --verbose`
- [ ] Review Chicago School TDD principles (CLAUDE.md)
- [ ] Set up test file templates

**Commands:**
```bash
cd /Users/sac/erlmcp
TERM=dumb rebar3 compile
rebar3 eunit
rebar3 ct
rebar3 cover --verbose
open _build/test/cover/index.html
```

### Afternoon: erlmcp_subscription_tests.erl (Engineer 2)

**Module:** `apps/erlmcp_core/src/erlmcp_subscription.erl` (51 LOC, stubs only)
**Estimated:** 2 hours
**Coverage target:** 85%+

- [ ] Create `apps/erlmcp_core/test/erlmcp_subscription_tests.erl`
- [ ] Implement basic_test_/0 setup/teardown (real process)
- [ ] Test: subscribe/2 success
- [ ] Test: unsubscribe/2 success
- [ ] Test: list_subscribers/1 empty list
- [ ] Test: notify/2 message delivery (currently stub, document TODO)
- [ ] Test: concurrent subscriptions (10 processes)
- [ ] Test: duplicate subscription handling
- [ ] Run: `rebar3 eunit --module=erlmcp_subscription_tests`
- [ ] Verify: 100% pass rate, 85%+ coverage
- [ ] Commit: "Add erlmcp_subscription_tests.erl with Chicago School TDD"

**Expected output:**
```
erlmcp_subscription_tests:
  subscribe_test ...................... [ok]
  unsubscribe_test .................... [ok]
  list_subscribers_test ............... [ok]
  notify_test ......................... [ok]
  concurrent_subscribe_test ........... [ok]
  All 5 tests passed.
```

---

## Day 2: erlmcp_secrets (Engineer 2) + erlmcp_split_brain_detector (Engineer 1)

### erlmcp_secrets_tests.erl (Engineer 2)

**Module:** `apps/erlmcp_core/src/erlmcp_secrets.erl` (~100 LOC)
**Estimated:** 4 hours
**Coverage target:** 85%+

- [ ] Read module: understand API (store, retrieve, rotate, delete)
- [ ] Create `apps/erlmcp_core/test/erlmcp_secrets_tests.erl`
- [ ] Test: store secret (encrypted storage)
- [ ] Test: retrieve secret (decryption)
- [ ] Test: rotate secret (old secret invalidated)
- [ ] Test: delete secret (secure deletion verified)
- [ ] Test: retrieve nonexistent secret (error handling)
- [ ] Test: concurrent secret access (10 processes)
- [ ] Test: environment variable fallback (if applicable)
- [ ] Test: encryption roundtrip (store → retrieve → match)
- [ ] Run: `rebar3 eunit --module=erlmcp_secrets_tests`
- [ ] Verify: 100% pass rate, 85%+ coverage

### erlmcp_split_brain_detector_tests.erl (Engineer 1)

**Module:** `apps/erlmcp_core/src/erlmcp_split_brain_detector.erl` (221 LOC)
**Estimated:** 6 hours
**Coverage target:** 85%+

- [ ] Read module: understand partition detection strategies
- [ ] Create `apps/erlmcp_core/test/erlmcp_split_brain_detector_tests.erl`
- [ ] Test: start_link/0 with default strategy
- [ ] Test: get_partition_status/0 (no partition detected)
- [ ] Test: winner_takes_all strategy (majority wins)
- [ ] Test: oldest_node strategy (oldest node wins)
- [ ] Test: configured_master strategy (master node wins)
- [ ] Test: periodic partition check (timer-based)
- [ ] Test: force_resolution/0 manual trigger
- [ ] Test: partition detected event logging
- [ ] Test: partition resolved event logging
- [ ] Run: `rebar3 eunit --module=erlmcp_split_brain_detector_tests`
- [ ] Verify: 100% pass rate, 85%+ coverage

---

## Day 3: erlmcp_message_handler (Engineer 2) + erlmcp_hooks Part 1 (Engineer 1)

### erlmcp_message_handler_tests.erl (Engineer 2)

**Module:** `apps/erlmcp_core/src/erlmcp_message_handler.erl` (~150 LOC)
**Estimated:** 5 hours
**Coverage target:** 85%+

- [ ] Read module: understand message routing, protocol parsing
- [ ] Create `apps/erlmcp_core/test/erlmcp_message_handler_tests.erl`
- [ ] Test: parse valid JSON-RPC message
- [ ] Test: parse invalid JSON (error handling)
- [ ] Test: route request to handler
- [ ] Test: route notification to handler
- [ ] Test: handle batch messages
- [ ] Test: error response generation
- [ ] Test: timeout handling
- [ ] Test: concurrent message handling (20 processes)
- [ ] Run: `rebar3 eunit --module=erlmcp_message_handler_tests`
- [ ] Verify: 100% pass rate, 85%+ coverage

### erlmcp_hooks_tests.erl Part 1 (Engineer 1)

**Module:** `apps/erlmcp_core/src/erlmcp_hooks.erl` (597 LOC)
**Estimated:** 8 hours TOTAL (Day 3: 4h, Day 4: 4h)
**Coverage target:** 90%+ (critical module)

**Day 3 focus: pre_task, post_task (basic)**

- [ ] Read module: understand TCPS integration, quality gates
- [ ] Create `apps/erlmcp_core/test/erlmcp_hooks_tests.erl`
- [ ] Test: start_link/0 gen_server initialization
- [ ] Test: pre_task/2 with valid description
- [ ] Test: pre_task/2 with empty description (fails)
- [ ] Test: post_task/1 with quality_gates_enabled = false (passes)
- [ ] Test: post_task/1 with TCPS unavailable (basic_validation fallback)
- [ ] Run: `rebar3 eunit --module=erlmcp_hooks_tests` (partial)

---

## Day 4: erlmcp_hooks Part 2 (Engineer 1 + Engineer 2 pair programming)

**Day 4 focus: post_task TCPS integration, pre/post-edit, session lifecycle**

- [ ] Test: post_task/2 with TCPS quality gates (all pass)
- [ ] Test: post_task/2 with TCPS quality gates (compilation fails)
- [ ] Test: post_task/2 with TCPS quality gates (tests fail)
- [ ] Test: post_task/2 receipt generation verified
- [ ] Test: pre_edit/2 valid Erlang file (.erl)
- [ ] Test: pre_edit/2 valid Rust file (.rs)
- [ ] Test: pre_edit/2 invalid file type (rejected)
- [ ] Test: post_edit/2 auto-format Erlang (rebar3 fmt called)
- [ ] Test: post_edit/2 auto-format Rust (rustfmt called)
- [ ] Test: session_start/1 TCPS services started
- [ ] Test: session_end/1 metrics exported
- [ ] Run: `rebar3 eunit --module=erlmcp_hooks_tests`
- [ ] Verify: 100% pass rate, 90%+ coverage

**Pair programming rationale:** erlmcp_hooks is the most complex module (597 LOC) and integrates with TCPS quality gates. Two engineers ensure comprehensive test coverage and catch edge cases.

---

## Day 5: Integration, Coverage, Review

### Morning: Integration Testing (Both Engineers)

- [ ] Run ALL Week 1 tests together:
  ```bash
  rebar3 eunit --module=erlmcp_subscription_tests
  rebar3 eunit --module=erlmcp_secrets_tests
  rebar3 eunit --module=erlmcp_split_brain_detector_tests
  rebar3 eunit --module=erlmcp_message_handler_tests
  rebar3 eunit --module=erlmcp_hooks_tests
  ```

- [ ] Run full EUnit suite: `rebar3 eunit`
- [ ] Run full Common Test suite: `rebar3 ct`
- [ ] Generate coverage report: `rebar3 cover --verbose`
- [ ] Open coverage HTML: `open _build/test/cover/index.html`

### Afternoon: Coverage Verification & Cleanup

- [ ] Verify coverage for each module:
  - [ ] erlmcp_subscription: 85%+ ✅
  - [ ] erlmcp_secrets: 85%+ ✅
  - [ ] erlmcp_split_brain_detector: 85%+ ✅
  - [ ] erlmcp_message_handler: 85%+ ✅
  - [ ] erlmcp_hooks: 90%+ ✅

- [ ] Chicago School TDD compliance review:
  - [ ] No mocking detected (manual code review)
  - [ ] Real gen_servers used in all tests
  - [ ] State-based assertions (not interaction verification)
  - [ ] Integration tests where applicable

- [ ] Code quality checks:
  ```bash
  TERM=dumb rebar3 compile  # Must pass with 0 errors
  rebar3 dialyzer            # Must pass (optional for tests)
  rebar3 xref                # Must pass
  ```

- [ ] Git commits:
  ```bash
  git add apps/erlmcp_core/test/*_tests.erl
  git commit -m "Week 1: Add tests for 5 core modules (85%+ coverage)

  Modules tested:
  - erlmcp_subscription (subscribe/notify/list)
  - erlmcp_secrets (encryption, rotation, secure deletion)
  - erlmcp_split_brain_detector (partition strategies)
  - erlmcp_message_handler (routing, parsing)
  - erlmcp_hooks (quality gates, TCPS integration)

  Chicago School TDD:
  - Real gen_servers, no mocking
  - State-based verification
  - Integration testing

  Coverage: 85%+ for all modules (90% for erlmcp_hooks)
  Tests: 40+ scenarios, 100% pass rate

  Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"
  ```

---

## Quality Gate Report (End of Week 1)

**Generate this report on Day 5:**

```markdown
# Week 1 Test Implementation Report

**Sprint:** Core Infrastructure Testing
**Duration:** 5 days
**Team:** 2 test engineers
**Completed:** 2026-01-XX

## Summary

✅ **Modules tested:** 5/5 (100% completion)
✅ **Tests created:** 40+ test scenarios
✅ **Coverage:** 85%+ average (90% for erlmcp_hooks)
✅ **Pass rate:** 100% (0 failures)
✅ **Chicago School TDD:** Verified (no mocking, real collaborators)

## Coverage by Module

| Module | LOC | Tests | Coverage | Status |
|--------|-----|-------|----------|--------|
| erlmcp_subscription | 51 | 5 | 87% | ✅ PASS |
| erlmcp_secrets | ~100 | 8 | 89% | ✅ PASS |
| erlmcp_split_brain_detector | 221 | 9 | 86% | ✅ PASS |
| erlmcp_message_handler | ~150 | 8 | 85% | ✅ PASS |
| erlmcp_hooks | 597 | 11 | 91% | ✅ PASS |
| **TOTAL** | **1,119** | **41** | **88%** | ✅ |

## Quality Metrics

✅ **Compilation:** `rebar3 compile` - 0 errors, 0 warnings
✅ **Tests:** `rebar3 eunit` - 41/41 passed (100%)
✅ **Coverage:** `rebar3 cover` - 88% average (target: 85%)
✅ **Dialyzer:** `rebar3 dialyzer` - 0 warnings (optional for tests)
✅ **Xref:** `rebar3 xref` - 0 undefined function calls

## Chicago School TDD Compliance

✅ **Real collaborators:** All tests use real gen_servers, no mocks
✅ **State-based verification:** All assertions check observable state
✅ **No interaction verification:** No meck, no method call tracking
✅ **Integration testing:** erlmcp_hooks tests TCPS integration with real services

## Next Steps

**Week 2 Sprint:** Pricing & SLA Testing
- erlmcp_pricing_plan (479 LOC, 8h)
- erlmcp_pricing_receipt (742 LOC, 10h)
- tcps_poka_yoke (528 LOC, 8h)
- erlmcp_sla_monitor (414 LOC, 8h)
- erlmcp_sla_envelope (45 LOC, 2h)
- tcps_poka_yoke_validator (58 LOC, 2h)

**Target:** 6 modules, 38 hours, 85%+ coverage
```

---

## Troubleshooting Guide

### Issue: Test fails with "process not registered"

**Cause:** Gen_server not started before test execution.

**Fix:**
```erlang
% Ensure setup function starts gen_server
{setup,
 fun() ->
     {ok, Pid} = erlmcp_hooks:start_link(),
     Pid
 end,
 fun(Pid) ->
     gen_server:stop(Pid)  % Cleanup
 end,
 fun(Pid) -> ... end
}.
```

---

### Issue: Coverage below 85%

**Cause:** Uncovered error paths, private functions not exercised.

**Fix:**
1. Run `rebar3 cover --verbose` to see uncovered lines
2. Open `_build/test/cover/<module>.COVER.html` in browser
3. Add tests for red (uncovered) lines
4. Focus on error paths: invalid input, timeouts, crashes

---

### Issue: Dialyzer warnings in test code

**Cause:** Missing type specs, incorrect return types.

**Fix:**
```erlang
% Add -spec for test helper functions
-spec setup_server() -> pid().
setup_server() ->
    {ok, Pid} = erlmcp_hooks:start_link(),
    Pid.
```

**Note:** Dialyzer for tests is optional but recommended.

---

### Issue: TCPS quality gates not available

**Cause:** tcps_erlmcp application not started.

**Fix:**
```erlang
% In test setup
erlmcp_hooks_test_() ->
    {setup,
     fun() ->
         application:ensure_all_started(tcps_erlmcp),
         {ok, Pid} = erlmcp_hooks:start_link(),
         Pid
     end,
     ...
    }.
```

---

## Reference Files

**Read these before starting:**
- `/Users/sac/erlmcp/CLAUDE.md` - Project quality standards
- `/Users/sac/erlmcp/docs/testing/TEST_COVERAGE_PLAN.md` - Detailed plan
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_client_tests.erl` - Example test file
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_registry_tests.erl` - Another example

**Commands reference:**
```bash
# Compile
TERM=dumb rebar3 compile

# Run specific test module
rebar3 eunit --module=erlmcp_hooks_tests

# Run all EUnit tests
rebar3 eunit

# Run all Common Test suites
rebar3 ct

# Generate coverage
rebar3 cover --verbose

# View coverage HTML
open _build/test/cover/index.html

# Dialyzer
rebar3 dialyzer

# Xref
rebar3 xref
```

---

## Daily Standup Template

**Format for daily updates:**

```
Day X Standup:

Engineer 1 (erlmcp_hooks, erlmcp_split_brain_detector):
- Yesterday: [completed tasks]
- Today: [planned tasks]
- Blockers: [issues, if any]
- Coverage: [current coverage %]

Engineer 2 (erlmcp_subscription, erlmcp_secrets, erlmcp_message_handler):
- Yesterday: [completed tasks]
- Today: [planned tasks]
- Blockers: [issues, if any]
- Coverage: [current coverage %]
```

---

## Success Criteria (Week 1)

**ALL must be ✅ before declaring Week 1 complete:**

- [ ] 5 modules tested: subscription, secrets, split_brain_detector, message_handler, hooks
- [ ] 40+ test scenarios implemented
- [ ] 100% test pass rate (0 failures)
- [ ] 85%+ coverage for each module (90% for erlmcp_hooks)
- [ ] Chicago School TDD verified (no mocking)
- [ ] Compilation clean (0 errors)
- [ ] All tests committed to git
- [ ] Week 1 report generated

**If any criteria fails, DO NOT proceed to Week 2 until resolved.**

---

**Last Updated:** 2026-01-28
**Generated by:** erlang-test-engineer agent
**Methodology:** Chicago School TDD, sprint planning
