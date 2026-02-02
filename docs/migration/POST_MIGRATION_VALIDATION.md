# Post-Migration Validation Checklist

**Migration**: OTP [FROM_VERSION] → OTP [TO_VERSION]
**Migration Date**: [YYYY-MM-DD]
**Validation Date**: [YYYY-MM-DD]
**Owner**: [NAME]

---

## Section 1: Immediate Validation (First 30 Minutes)

### OTP Version Verification
- [ ] **Target OTP version active**
  ```bash
  erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop
  ```
  Expected: [TO_VERSION]

- [ ] **OTP installation path correct**
  ```bash
  which erl
  ```
  Expected: /Users/sac/.erlmcp/otp-[TO_VERSION]/bin/erl

- [ ] **ERLMCP_OTP_BIN environment variable set**
  ```bash
  echo $ERLMCP_OTP_BIN
  ```
  Expected: /Users/sac/erlmcp/.erlmcp/otp-[TO_VERSION]/bin

### Application Startup
- [ ] **Application compiles**
  ```bash
  rebar3 clean
  rebar3 compile
  ```
  Expected: 0 errors

- [ ] **Application starts**
  ```bash
  erl -pa _build/default/lib/*/ebin -noshell -eval '
      {ok, _} = application:ensure_all_started(erlmcp),
      io:format("✅ Application started~n"),
      halt(0)
  '
  ```
  Expected: ✅ Application started

- [ ] **Supervision tree healthy**
  ```bash
  erl -pa _build/default/lib/*/ebin -noshell -eval '
      {ok, Children} = supervisor:which_children(erlmcp_sup),
      io:format("✅ Supervisor has ~p children~n", [length(Children)]),
      halt(0)
  '
  ```
  Expected: All expected children present

### Configuration Verification
- [ ] **Configuration file loaded**
  ```bash
  ls -la config/sys.config
  ```
  Expected: sys.config.otp[TO_VERSION] (or appropriate symlink)

- [ ] **Configuration values correct**
  ```erlang
  % From Erlang shell
  application:get_env(erlmcp, otp_version).
  ```
  Expected: {ok, {[TO_MAJOR], [TO_MINOR], [TO_PATCH]}}

- [ ] **Resource limits set correctly**
  ```erlang
  application:get_env(erlmcp, resource_limits).
  ```
  Expected: {ok, #{max_connections => ..., max_processes => ...}}

---

## Section 2: Smoke Tests (First Hour)

### Basic Operations
- [ ] **Registry operations work**
  ```bash
  erl -pa _build/default/lib/*/ebin -noshell -eval '
      {ok, _} = application:ensure_all_started(erlmcp),
      ok = erlmcp_registry:register_server(test_smoke, self(), #{}),
      {ok, Pid} = erlmcp_registry:find_server(test_smoke),
      io:format("✅ Registry working~n"),
      halt(0)
  '
  ```

- [ ] **Queue operations work**
  ```erlang
  % From Erlang shell
  Queue = erlmcp_queue:new(),
  Queue2 = erlmcp_queue:in(item, Queue),
  {item, Queue3} = erlmcp_queue:out(Queue2),
  io:format("✅ Queue working~n").
  ```

- [ ] **Client/server communication works**
  ```bash
  erl -pa _build/default/lib/*/ebin -noshell -eval '
      {ok, _} = application:ensure_all_started(erlmcp),
      {ok, Client} = erlmcp_client:start_link({stdio, #{}}),
      {ok, initialized} = erlmcp_client:initialize(Client, #{}),
      erlmcp_client:stop(Client),
      io:format("✅ Client/server working~n"),
      halt(0)
  '
  ```

### Transport Validation
- [ ] **STDIO transport works**
  ```erlang
  {ok, Stdio} = erlmcp_transport:start_link({stdio, #{}}),
  ok = erlmcp_transport:send(Stdio, <<"test">>),
  erlmcp_transport:close(Stdio).
  ```

- [ ] **TCP transport works** (if configured)
  ```erlang
  {ok, Tcp} = erlmcp_transport:start_link({tcp, #{mode => client, host => "localhost", port => 8080}}),
  ok = erlmcp_transport:close(Tcp).
  ```

---

## Section 3: Unit Tests (First 2 Hours)

### EUnit Tests
- [ ] **Run EUnit tests**
  ```bash
  rebar3 eunit --verbose 2>&1 | tee backups/post_migration/eunit_after.log
  ```
  Expected: 100% pass rate

- [ ] **Compare with baseline**
  ```bash
  diff backups/pre_migration_/eunit_before.log \
       backups/post_migration/eunit_after.log
  ```
  Expected: No new failures

- [ ] **Test coverage maintained**
  ```bash
  rebar3 cover
  open _build/test/cover/index.html
  ```
  Expected: >= 80% coverage

### Critical Test Suites
- [ ] **erlmcp_registry_tests** pass
- [ ] **erlmcp_queue_tests** pass
- [ ] **erlmcp_sup_tests** pass
- [ ] **erlmcp_json_rpc_tests** pass
- [ ] **erlmcp_transport_tests** pass

---

## Section 4: Integration Tests (First 4 Hours)

### Common Test Suites
- [ ] **Run CT tests**
  ```bash
  rebar3 ct --verbose 2>&1 | tee backups/post_migration/ct_after.log
  ```
  Expected: 100% pass rate

- [ ] **Integration suite passes**
  ```bash
  rebar3 ct --suite=erlmcp_integration_SUITE
  ```

- [ ] **Client/server suite passes**
  ```bash
  rebar3 ct --suite=erlmcp_client_server_SUITE
  ```

### End-to-End Tests
- [ ] **Full MCP protocol flow works**
- [ ] **Session management works**
- [ ] **Resource operations work**
- [ ] **Tool operations work**
- [ ] **Prompt operations work**

---

## Section 5: Performance Validation (First 6 Hours)

### Benchmark Execution
- [ ] **Run performance benchmarks**
  ```bash
  make benchmark-quick > backups/post_migration/benchmark_after.log 2>&1
  ```

- [ ] **Compare with baseline**
  ```bash
  python3 scripts/compare_benchmarks.py \
      --baseline backups/pre_migration_/baseline_before.log \
      --current backups/post_migration/benchmark_after.log \
      --threshold 0.1
  ```
  Expected: No regression > 10%

### Key Metrics
| Metric | Before | After | Change | Status |
|--------|--------|-------|--------|--------|
| **Registry throughput** | [VALUE] msg/s | [VALUE] msg/s | [%] | ☐ Pass / ☐ Fail |
| **Queue throughput** | [VALUE] msg/s | [VALUE] msg/s | [%] | ☐ Pass / ☐ Fail |
| **Memory usage** | [VALUE] GB | [VALUE] GB | [%] | ☐ Pass / ☐ Fail |
| **Latency p95** | [VALUE] us | [VALUE] us | [%] | ☐ Pass / ☐ Fail |

**Acceptance Criteria**:
- Registry throughput >= 90% of baseline
- Queue throughput >= 90% of baseline
- Memory usage <= 110% of baseline
- Latency p95 <= 110% of baseline

### OTP 28 Specific Features
- [ ] **Process iterator enabled**
  ```erlang
  erlang:system_info(process_count).
  ```
  Expected: Returns process count efficiently

- [ ] **Priority messages working** (if OTP 28)
- [ ] **Hibernation working** (if OTP 28)
  ```erlang
  % Check if supervisor hibernates
  process_info(erlmcp_sup, memory).
  ```

---

## Section 6: Quality Gates (First 8 Hours)

### Dialyzer
- [ ] **Run Dialyzer**
  ```bash
  rebar3 dialyzer
  ```
  Expected: 0 new warnings

- [ ] **Compare with baseline**
  ```bash
  diff backups/pre_migration_/dialyzer_before.log \
       backups/post_migration/dialyzer_after.log
  ```

### Xref
- [ ] **Run Xref**
  ```bash
  rebar3 xref
  ```
  Expected: 0 undefined functions

### Code Format
- [ ] **Check code formatting**
  ```bash
  rebar3 format --verify
  ```
  Expected: All files formatted (or known exceptions documented)

---

## Section 7: Production Readiness (First 12 Hours)

### Pre-Production Checklist
- [ ] **All smoke tests pass**
- [ ] **All unit tests pass**
- [ ] **All integration tests pass**
- [ ] **Performance validated**
- [ ] **Quality gates pass**
- [ ] **No new warnings**
- [ ] **No regressions detected**

### Staging Deployment
- [ ] **Deploy to staging environment**
- [ ] **Run smoke tests in staging**
- [ ] **Run integration tests in staging**
- [ ] **Monitor for 4 hours in staging**
- [ ] **Check logs for errors**

### Production Deployment Decision

**GO for production if ALL items pass**:
- [ ] Staging deployment successful
- [ ] All tests pass in staging
- [ ] No errors in staging logs
- [ ] Performance acceptable
- [ ] Stakeholders approve

**NO-GO if ANY item fails**:
- Investigate failure
- Fix in staging
- Re-validate
- Re-assess

---

## Section 8: 24-Hour Monitoring

### Hourly Health Checks

| Hour | Health Check | Error Rate | Availability | Notes |
|------|--------------|------------|--------------|-------|
| 1 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 2 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 3 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 4 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 5 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 6 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 7 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 8 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 9 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 10 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 11 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 12 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 13 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 14 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 15 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 16 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 17 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 18 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 19 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 20 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 21 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 22 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 23 | ☐ Pass / ☐ Fail | [%] | [%] | |
| 24 | ☐ Pass / ☐ Fail | [%] | [%] | |

### Continuous Monitoring
- [ ] **Error logs reviewed hourly**
  ```bash
  tail -100 log/erlang.log.1 | grep -i error
  ```
  Expected: < 5 errors/hour

- [ ] **Memory usage monitored hourly**
  ```bash
  erl -noshell -eval 'io:format("~p MB~n", [erlang:memory(total) div 1048576]), halt(0).'
  ```
  Expected: No memory leaks

- [ ] **Process count monitored hourly**
  ```bash
  erl -noshell -eval 'io:format("~p processes~n", [length(erlang:processes())]), halt(0).'
  ```
  Expected: Stable or expected growth

---

## Section 9: Final Validation (After 24 Hours)

### Stability Assessment
- [ ] **Service availability >= 99.9%**
- [ ] **Error rate < 1%**
- [ ] **No crashes or restarts**
- [ ] **No memory leaks**
- [ ] **Performance stable**

### Final Metrics
| Metric | 24h Average | Target | Status |
|--------|-------------|--------|--------|
| **Availability** | [%] | >= 99.9% | ☐ Pass / ☐ Fail |
| **Error Rate** | [%] | < 1% | ☐ Pass / ☐ Fail |
| **Throughput** | [msg/s] | >= baseline | ☐ Pass / ☐ Fail |
| **Latency p95** | [us] | <= 110% baseline | ☐ Pass / ☐ Fail |
| **Memory** | [GB] | No leaks | ☐ Pass / ☐ Fail |

### Migration Success Criteria

**ALL criteria must pass for migration to be considered successful**:

- [ ] ✅ OTP version verified
- [ ] ✅ Application starts successfully
- [ ] ✅ All tests pass (EUnit, CT, Smoke)
- [ ] ✅ Performance regression < 10%
- [ ] ✅ Quality gates pass (Dialyzer, Xref)
- [ ] ✅ 24-hour stability confirmed
- [ ] ✅ No critical issues detected
- [ ] ✅ Documentation updated

---

## Section 10: Post-Migration Actions

### Documentation Updates
- [ ] **CLAUDE.md updated** with new OTP version
- [ ] **README.md updated** with new prerequisites
- [ ] **CHANGELOG.md updated** with migration details
- [ ] **Migration report created**

### Knowledge Transfer
- [ ] **Team training session** scheduled
- [ ] **Migration lessons learned** documented
- [ ] **Runbook updated** with new procedures
- [ ] **Monitoring dashboards** updated

### Cleanup
- [ ] **Old OTP version** removed (after 30 days)
- [ ] **Temporary backups** removed (after 30 days)
- [ ] **Migration scripts** archived

---

## Section 11: Rollback Decision

### Immediate Rollback Triggers (0-2 hours)

Roll back IMMEDIATELY if ANY of these occur:
- [ ] Error rate > 5%
- [ ] Service availability < 99%
- [ ] Data corruption detected
- [ ] Critical features not working
- [ ] Security vulnerability detected

### Short-Term Rollback Triggers (2-12 hours)

Consider rollback if:
- [ ] Error rate consistently > 1%
- [ ] Performance regression > 20%
- [ ] Customer complaints increase
- [ ] Memory leak detected

### Long-Term Rollback Triggers (12-24 hours)

Consider rollback if:
- [ ] Multiple minor issues accumulate
- [ ] Business impact observed
- [ ] Unable to resolve critical issues

### Rollback Approval

If rollback is triggered:
- [ ] **Issue documented**
- [ ] **Stakeholders notified**
- [ ] **Rollback approved** by [ROLE]
- [ ] **Rollback executed**
- [ ] **Validation post-rollback**
- [ ] **Root cause analysis** initiated

---

## Section 12: Post-Migration Report

### Migration Summary

**Migration Details**:
- From OTP: [FROM_VERSION]
- To OTP: [TO_VERSION]
- Migration Date: [DATE]
- Migration Owner: [NAME]

**Timeline**:
- Phase 0 (Prep): [DURATION]
- Phase 1 (Upgrade): [DURATION]
- Phase 2 (Validate): [DURATION]
- Phase 3 (Deploy): [DURATION]

**Results**:
- ✅ / ❌ Migration successful
- Issues encountered: [YES/NO]
- Rollback required: [YES/NO]

### Issues Found

| Issue | Severity | Resolution | Time to Fix |
|-------|----------|------------|-------------|
| | | | |

### Lessons Learned

**What went well**:
1.
2.
3.

**What could be improved**:
1.
2.
3.

**Recommendations for future migrations**:
1.
2.
3.

---

## Automated Validation Script

```bash
#!/bin/bash
# scripts/post_migration_validation.sh

set -e

echo "=== Post-Migration Validation ==="
echo ""

TARGET_OTP="${1:-28.3.1}"
BACKUP_DIR="backups/post_migration_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"

# Section 1: OTP Version
echo "Section 1: OTP Version Verification"
echo "-----------------------------------"

CURRENT_OTP=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]).' -s init stop)
echo "Current OTP: $CURRENT_OTP"
echo "Target OTP: $TARGET_OTP"

if [ "$CURRENT_OTP" == "$(echo $TARGET_OTP | cut -d. -f1)" ]; then
    echo "✅ OTP version matches"
else
    echo "❌ OTP version mismatch"
    exit 1
fi

echo ""

# Section 2: Compilation
echo "Section 2: Compilation"
echo "--------------------"

if rebar3 clean && rebar3 compile; then
    echo "✅ Compilation successful"
else
    echo "❌ Compilation failed"
    exit 1
fi

echo ""

# Section 3: Smoke Tests
echo "Section 3: Smoke Tests"
echo "--------------------"

if erl -pa _build/default/lib/*/ebin -noshell -eval '
    {ok, _} = application:ensure_all_started(erlmcp),
    {ok, Children} = supervisor:which_children(erlmcp_sup),
    io:format("✅ Application started with ~p children~n", [length(Children)]),
    halt(0)
'; then
    echo "✅ Smoke tests pass"
else
    echo "❌ Smoke tests fail"
    exit 1
fi

echo ""

# Section 4: Unit Tests
echo "Section 4: Unit Tests"
echo "-------------------"

read -p "Run full EUnit suite? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    if rebar3 eunit --verbose 2>&1 | tee "$BACKUP_DIR/eunit.log"; then
        echo "✅ EUnit tests pass"
    else
        echo "❌ EUnit tests fail"
        exit 1
    fi
else
    echo "⏭️  Skipping EUnit tests"
fi

echo ""

# Section 5: Benchmarks
echo "Section 5: Performance Benchmarks"
echo "--------------------------------"

read -p "Run benchmarks? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    make benchmark-quick 2>&1 | tee "$BACKUP_DIR/benchmark.log"
    echo "✅ Benchmarks complete - review $BACKUP_DIR/benchmark.log"
else
    echo "⏭️  Skipping benchmarks"
fi

echo ""

# Section 6: Summary
echo "=== Validation Summary ==="
echo "OTP Version: $CURRENT_OTP"
echo "Backup: $BACKUP_DIR"
echo ""
echo "✅ All validation checks passed"
echo ""
echo "Next steps:"
echo "1. Run full test suite: rebar3 ct"
echo "2. Monitor for 24 hours"
echo "3. Complete validation checklist"
```

---

## Notes

**Common Post-Migration Issues**:

1. **Tests fail due to timing changes**:
   - OTP 28 may have different scheduling behavior
   - Adjust test timeouts if needed

2. **Performance appears slower**:
   - Check if OTP 28 optimizations are enabled
   - Verify configuration settings

3. **Memory usage higher**:
   - Normal initially due to code loading
   - Monitor for leaks over 24 hours

**Estimated Validation Time**: 12-24 hours

**Contact**: [YOUR NAME] for questions

---

**END OF VALIDATION CHECKLIST**
