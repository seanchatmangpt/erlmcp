# Upgrade Best Practices

**Version**: 2.1.0
**Last Updated**: 2026-02-01
**Target**: Erlang/OTP 28.3.1

---

## Table of Contents

1. [Overview](#overview)
2. [Pre-Upgrade Preparation](#pre-upgrade-preparation)
3. [Testing Strategies](#testing-strategies)
4. [Production Upgrade Strategies](#production-upgrade-strategies)
5. [Monitoring During Upgrade](#monitoring-during-upgrade)
6. [Post-Upgrade Validation](#post-upgrade-validation)
7. [Continuous Upgrade Practices](#continuous-upgrade-practices)
8. [Team Coordination](#team-coordination)

---

## Overview

Upgrading Erlang/OTP requires careful planning and execution. This guide provides best practices for upgrading erlmcp from earlier OTP versions to 28.3.1.

### Key Principles

1. **Test Thoroughly**: Never skip testing
2. **Monitor Closely**: Watch metrics during upgrade
3. **Rollback Ready**: Always have rollback plan
4. **Document Everything**: Record all changes
5. **Communicate**: Keep team informed

---

## Pre-Upgrade Preparation

### 1. Current System Assessment

**Inventory Your System**:
```bash
# Check current OTP version
erl -version

# Check erlmcp version
git describe --tags

# Document current configuration
cat vm.args
cat config/sys.config

# Document performance baseline
make benchmark-full > baseline.txt

# Check dependencies
rebar3 tree | grep -E "[0-9]+\.[0-9]+\.[0-9]+"
```

**Create Inventory Document**:

| Component | Version | Notes |
|-----------|---------|-------|
| Erlang/OTP | 27.3 | Current production |
| erlmcp | 2.0.5 | Stable |
| OS | Ubuntu 22.04 | LTS |
| Dependencies | [List] | Check compatibility |
| Custom NIFs | None | N/A |

### 2. Compatibility Check

**Verify Dependencies Support OTP 28**:
```bash
# Check all dependencies
rebar3 tree | awk '{print $2}' | sort -u | while read dep; do
    echo "Checking $dep..."
    # Check Hex.pm for OTP 28 support
done
```

**Review Breaking Changes**:
- Read OTP 28 release notes
- Read erlmcp 2.1.0 changelog
- Review API compatibility docs

### 3. Create Testing Plan

**Define Test Scenarios**:
- [ ] Unit tests (100% pass rate)
- [ ] Integration tests (all suites)
- [ ] Performance tests (<10% regression)
- [ ] Load tests (2x production load)
- [ ] Upgrade tests (26→27, 27→28, 26→28)
- [ ] Rollback tests

**Set Up Test Environment**:
```bash
# Clone production
git clone -b production . test-env

# Install OTP 28 in test
cd test-env
./scripts/install-otp.sh 28.3.1

# Run full test suite
make check
```

### 4. Backup Strategy

**Create Comprehensive Backups**:
```bash
# 1. Code backup
git tag backup-before-otp28-upgrade-$(date +%Y%m%d)

# 2. Configuration backup
mkdir -p backups/$(date +%Y%m%d)
cp vm.args backups/$(date +%Y%m%d)/
cp config/sys.config backups/$(date +%Y%m%d)/
cp config/vm.args backups/$(date +%Y%m%d)/

# 3. Data backup
cp -r data/ backups/$(date +%Y%m%d)/data/

# 4. Dependency cache backup
cp -r _build backups/$(date +%Y%m%d)/build/

# 5. Document backup locations
echo "Backup location: backups/$(date +%Y%m%d)" > BACKUP_INFO.txt
```

### 5. Rollback Plan

**Document Rollback Steps**:
```bash
# 1. Stop current system
make stop

# 2. Switch OTP version
kerl deactivate
. ~/.kerl/27.3/activate

# 3. Restore previous code
git checkout v2.0.x

# 4. Rebuild
make clean compile

# 5. Restore data (if needed)
cp -r backups/20250131/data/ data/

# 6. Start
make start

# 7. Verify
make health
```

**Rollback Criteria**:
- Critical bugs (crashes, data corruption)
- Performance regression >20%
- Failed validation tests
- Customer-impacting issues

---

## Testing Strategies

### 1. Unit Testing

**Ensure 100% Pass Rate**:
```bash
# Run all unit tests
rebar3 eunit

# Check coverage
rebar3 cover
# Ensure >= 80% coverage
```

**Version-Specific Tests**:
```erlang
% erlmcp_otp28_tests.erl
-module(erlmcp_otp28_tests).

-include_lib("eunit/include/eunit.hrl").

otp_28_feature_test() ->
    case erlang:system_info(otp_release) of
        "28" ->
            % Test priority messages
            ?assert(test_priority_messages()),
            ?assert(test_hibernate_callback()),
            ?assert(test_pcre2_regex());
        _ ->
            % Skip or mark as passed
            ?skip(not_otp_28)
    end.
```

### 2. Integration Testing

**Full CT Suite**:
```bash
# Run all integration tests
rebar3 ct

# Specific suites
rebar3 ct --suite=erlmcp_integration_SUITE
rebar3 ct --suite=erlmcp_upgrade_SUITE
```

**Upgrade Path Tests**:
```bash
# OTP 26 → 27
rebar3 ct --suite=erlmcp_otp26_to_27_upgrade_SUITE

# OTP 27 → 28
rebar3 ct --suite=erlmcp_otp27_to_28_upgrade_SUITE

# Direct 26 → 28
rebar3 ct --suite=erlmcp_otp26_to_28_upgrade_SUITE
```

### 3. Performance Testing

**Baseline Comparison**:
```bash
# Before upgrade (OTP 27.3)
make benchmark-full > baseline_otp27.txt

# After upgrade (OTP 28.3.1)
make benchmark-full > baseline_otp28.txt

# Compare
diff baseline_otp27.txt baseline_otp28.txt
```

**Key Metrics**:
| Metric | OTP 27 | OTP 28 | Regression |
|--------|--------|--------|------------|
| Registry throughput | 485K | 553K | +14% ✓ |
| Queue throughput | 847K | 971K | +14.6% ✓ |
| Memory per conn | 12KB | 9KB | -25% ✓ |
| Latency P99 | 50ms | 42ms | -16% ✓ |

**Acceptable Thresholds**:
- Throughput: <10% regression acceptable
- Latency: <15% increase acceptable
- Memory: <20% increase acceptable

### 4. Load Testing

**Sustained Load Test**:
```bash
# 1x production load for 1 hour
rebar3 ct --suite=erlmcp_load_test_SUITE --duration=3600

# 2x production load for 30 minutes
rebar3 ct --suite=erlmcp_load_test_SUITE --duration=1800 --multiplier=2

# 10x spike for 5 minutes
rebar3 ct --suite=erlmcp_spike_test_SUITE --duration=300 --multiplier=10
```

**Metrics to Watch**:
- Error rate: Must be 0%
- Memory growth: <5% per hour
- GC pauses: <100ms
- Scheduler utilization: <80%

### 5. Chaos Testing

**Failure Scenarios**:
```bash
# Network failures
rebar3 ct --suite=erlmcp_chaos_net_SUITE

# Process crashes
rebar3 ct --suite=erlmcp_chaos_crash_SUITE

# High load
rebar3 ct --suite=erlmcp_chaos_load_SUITE

# Resource exhaustion
rebar3 ct --suite=erlmcp_chaos_resource_SUITE
```

---

## Production Upgrade Strategies

### Strategy 1: Blue-Green Deployment

**Best For**: Zero-downtime requirements

**Steps**:
1. **Set up Green Environment**:
   - Install OTP 28.3.1
   - Deploy erlmcp 2.1.0
   - Run full validation
   - Warm up caches

2. **Switch Traffic**:
   - Update load balancer
   - Monitor for 10 minutes
   - Verify all systems

3. **Decommission Blue**:
   - Keep for 24 hours
   - Monitor for issues
   - Decommission if stable

**Time**: 30-60 minutes
**Risk**: Low (can rollback instantly)

### Strategy 2: Rolling Upgrade

**Best For**: Multi-node clusters

**Steps**:
1. **Upgrade One Node**:
   - Stop node
   - Upgrade OTP
   - Deploy erlmcp
   - Start node
   - Validate

2. **Repeat**:
   - Upgrade 10% of nodes
   - Monitor 30 minutes
   - Continue batch by batch

3. **Final Validation**:
   - All nodes on OTP 28
   - Full cluster test

**Time**: 2-4 hours (depending on cluster size)
**Risk**: Medium (partial upgrade state)

### Strategy 3: Canary Deployment

**Best For**: Conservative approach

**Steps**:
1. **Deploy Canary**:
   - 5% of traffic to new version
   - Monitor for 1 hour

2. **Expand**:
   - 10% traffic
   - Monitor 1 hour

3. **Full Rollout**:
   - 25% → 50% → 100%
   - Monitor at each step

**Time**: 4-6 hours
**Risk**: Very Low (gradual exposure)

---

## Monitoring During Upgrade

### Key Metrics to Watch

**1. System Health**:
```bash
# Health check
make health

# Expected output:
✓ OTP version: 28.3.1
✓ erlmcp version: 2.1.0
✓ All applications started
✓ All supervisors running
✓ No error logs
```

**2. Performance Metrics**:
```bash
# Throughput
erlmcp_metrics:throughput()

% Expected: >500K msg/s (registry)

# Latency
erlmcp_metrics:latency(p99)

% Expected: <50ms

# Memory
erlang:memory(total)

% Expected: <1GB growth
```

**3. Error Rates**:
```bash
# Check error logs
tail -f log/erlang.log | grep ERROR

# Should be: No errors

# Check crash dumps
ls -la log/crash.dump*

% Expected: 0 files
```

### Real-Time Dashboards

**Start Monitoring**:
```bash
# 1. Metrics dashboard
make dashboard
% Access at http://localhost:4000

# 2. OTEL tracing
opentelemetry:initialize()
% View at http://localhost:16686 (Jaeger)

# 3. Observer
erl -s observer start
```

### Alerts Setup

**Configure Alerts**:
```erlang
% In erlmcp_alerts.erl
alert_upgrade() ->
    % CPU >90% for 5 minutes
    alert_if(cpu_avg, >, 90, 300),

    % Memory >2GB
    alert_if(memory_total, >, 2000000000, 60),

    % Error rate >1%
    alert_if(error_rate, >, 1.0, 60),

    % Latency P99 >100ms
    alert_if(latency_p99, >, 100, 300).
```

---

## Post-Upgrade Validation

### 1. Functional Validation

**Smoke Tests**:
```bash
# Quick smoke test
make smoke

% Tests:
% - Start/stop
% - Basic operations
% - Session management
% - Tool calls
% - Resources
% - Prompts
```

**Full Feature Test**:
```bash
# All features
rebar3 ct --suite=erlmcp_feature_SUITE

% Includes:
% - All MCP protocol features
% - All transports
% - All session backends
% - All auth methods
```

### 2. Performance Validation

**Compare to Baseline**:
```bash
# Run benchmarks
make benchmark-full

# Compare to pre-upgrade baseline
diff baseline_otp27.txt baseline_otp28.txt
```

**Acceptable Regressions**:
| Metric | Threshold |
|--------|-----------|
| Throughput | <10% decrease |
| Latency P50 | <15% increase |
| Latency P99 | <20% increase |
| Memory | <20% increase |

### 3. Stability Validation

**24-Hour Soak Test**:
```bash
# Run for 24 hours
rebar3 ct --suite=erlmcp_soak_test_SUITE --duration=86400

% Monitor:
% - No crashes
% - No memory leaks
% - Stable performance
% - No error growth
```

### 4. Validation Checklist

- [ ] All unit tests passing
- [ ] All integration tests passing
- [ ] Performance regression <10%
- [ ] No errors in logs
- [ ] All applications started
- [ ] All supervisors running
- [ ] No crash dumps
- [ ] Memory stable
- [ ] CPU normal
- [ ] Network traffic normal
- [ ] Customer functionality working

---

## Continuous Upgrade Practices

### 1. Stay Current

**Upgrade Promptly**:
- Upgrade to minor versions within 3 months
- Upgrade to major versions within 6 months
- Never skip more than 2 major versions

**Track Releases**:
- Subscribe to Erlang/OTP announcements
- Watch erlmcp releases
- Monitor dependency updates

### 2. Automated Testing

**CI/CD Integration**:
```yaml
# .github/workflows/otp-upgrade-test.yml
name: OTP Upgrade Test

on:
  push:
    paths:
      - 'rebar.config'
      - 'apps/**/*.erl'

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: [27.3, 28.3.1]
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
      - run: make check
      - run: make benchmark-quick
```

### 3. Documentation

**Document Upgrades**:
```markdown
# Upgrade Log

## 2025-02-01: OTP 27.3 → 28.3.1

### Changes
- Upgraded OTP
- Added priority messages
- Enabled hibernation
- Updated dependencies

### Issues
- Issue #1: PCRE2 recompile required
- Resolution: Clean rebuild

### Performance
- Registry: +14%
- Queue: +14.6%
- Memory: -25%

### Rollback
- Not needed
```

---

## Team Coordination

### 1. Pre-Upgrade Communication

**Notify Stakeholders**:
- Engineering team: 2 weeks before
- Operations team: 1 week before
- Customers: 48 hours before
- Support team: 24 hours before

**Communication Template**:
```
Subject: Scheduled Upgrade - Erlang/OTP 28.3.1

When: [Date/Time]
Duration: 2 hours
Impact: Potential 5-minute downtime
Reason: Performance improvements, new features
```

### 2. During Upgrade

**Roles and Responsibilities**:

| Role | Responsibilities |
|------|------------------|
| Upgrade Lead | Execute upgrade, monitor progress |
| Ops Engineer | Watch metrics, handle alerts |
| QA Engineer | Run validation tests |
| Support Lead | Handle customer issues |
| Communication | Update status page, notify team |

**Status Updates**:
- Every 15 minutes during upgrade
- Immediate if issues arise
- Final when complete

### 3. Post-Upgrade

**Retrospective**:
```markdown
# Upgrade Retrospective

## What Went Well
- Thorough testing prevented issues
- Monitoring caught potential problems early
- Team communication was effective

## What Could Improve
- Need more chaos testing
- Improve documentation
- Faster rollback procedure

## Action Items
- [ ] Add chaos tests to suite
- [ ] Update documentation
- [ ] Automate rollback script
```

---

## Quick Reference

### Pre-Upgrade Checklist

- [ ] Inventory system
- [ ] Check compatibility
- [ ] Create testing plan
- [ ] Create backups
- [ ] Document rollback
- [ ] Notify team
- [ ] Schedule maintenance window

### Upgrade Day Checklist

- [ ] Stop system
- [ ] Upgrade OTP
- [ ] Deploy erlmcp
- [ ] Start system
- [ ] Run smoke tests
- [ ] Monitor metrics
- [ ] Run full tests
- [ ] Validate performance
- [ ] Notify team (complete)

### Post-Upgrade Checklist

- [ ] All tests passing
- [ ] Performance acceptable
- [ ] No errors in logs
- [ ] Team notified
- [ ] Documentation updated
- [ ] Retrospective scheduled

---

## Support

**Questions**: https://github.com/seanchatmangpt/erlmcp/discussions
**Issues**: https://github.com/seanchatmangpt/erlmcp/issues
**Email**: support@erlmcp.org

---

**Document Version**: 1.0
**Author**: SPARC Orchestrator
**Review Date**: 2026-03-01
