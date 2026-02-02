# OTP Version Migration Guide

**Version**: 2.1.0
**Last Updated**: 2026-02-01
**Target**: Erlang/OTP 26.x → 27.x → 28.3.1

---

## Table of Contents

1. [Overview](#overview)
2. [Version Support Matrix](#version-support-matrix)
3. [Migration Paths](#migration-paths)
4. [OTP 26 → 27 Migration](#otp-26--27-migration)
5. [OTP 27 → 28 Migration](#otp-27--28-migration)
6. [Direct 26 → 28 Migration](#direct-26--28-migration)
7. [Breaking Changes](#breaking-changes)
8. [Deprecation Timeline](#deprecation-timeline)
9. [Rollback Procedures](#rollback-procedures)
10. [Testing Requirements](#testing-requirements)

---

## Overview

erlmcp **2.1.0 requires Erlang/OTP 28.3.1** (strict minimum). This guide covers migration from earlier versions (26.x, 27.x).

### Why Upgrade?

**OTP 28 Features**:
- Priority messages (critical for MCP protocol)
- `hibernate/0` callback (memory optimization)
- PCRE2 regex engine (3-4x faster)
- Enhanced process iteration
- Improved scheduler efficiency
- Better JSON performance
- Enhanced tracing/debugging

**OTP 27 Features**:
- `runtime_dependencies` enforcement
- Native `json` module
- Enhanced monitoring tools
- Improved SSL/TLS

**OTP 26 Features**:
- Concurrent application startup
- Persistent term configuration
- `prep_stop/1` callback
- Maybe expressions (default)

---

## Version Support Matrix

| erlmcp Version | Minimum OTP | Recommended OTP | EOL Date |
|----------------|-------------|-----------------|----------|
| 2.1.0 | 28.0 | 28.3.1 | TBD |
| 2.0.x | 27.0 | 27.3 | 2026-06-01 |
| 1.5.x | 26.0 | 26.2.5 | 2025-12-01 |

**Support Policy**: Current - 1 (N-1 support). Example: When 2.1.0 is current, 2.0.x is supported, 1.5.x is EOL.

---

## Migration Paths

### Supported Paths

```
OTP 26.2.5 ──┐
              ├─→ OTP 27.3 ──→ OTP 28.3.1
OTP 27.0 ─────┘                          │
                                          └─→ erlmcp 2.1.0
```

### Recommended Strategy

**For Production Systems**:
1. Test on OTP 27.3 first (intermediate step)
2. Upgrade to OTP 28.3.1 in staging
3. Run full test suite
4. Deploy to production with monitoring

**For New Development**:
1. Direct install OTP 28.3.1
2. Verify with `make check`
3. Develop against 28.3.1 only

---

## OTP 26 → 27 Migration

### Step 1: Pre-Migration Checklist

- [ ] All tests passing on OTP 26.2.5
- [ ] Backup `rebar3` local cache (`~/.cache/rebar3`)
- [ ] Document current performance baselines
- [ ] Verify dependencies support OTP 27

### Step 2: Install OTP 27.3

**Using kerl**:
```bash
kerl build 27.3 27.3
kerl install 27.3 ~/.kerl/27.3
. ~/.kerl/27.3/activate
erl -version  # Verify: OTP 27
```

**Custom Install**:
```bash
./scripts/install-otp.sh 27.3
export PATH="/usr/local/otp-27.3/bin:$PATH"
```

### Step 3: Update Dependencies

```bash
# Clear cache
rm -rf _build/default/lib
rm -rf ~/.cache/rebar3

# Fetch OTP 27-optimized dependencies
rebar3 compile
```

### Step 4: Code Changes

**Maybe Expressions** (now default):
```erlang
% OLD (OTP 25-)
case find_user(Id) of
    {ok, User} -> {ok, User};
    error -> {error, not_found}
end.

% NEW (OTP 26+, default in 27)
try find_user(Id) of
    User -> {ok, User}
catch
    error:_ -> {error, not_found}
end.
```

**Persistent Terms** (for configuration):
```erlang
% OLD (ETS)
application:set_env(erlmcp, config, Config).

% NEW (Persistent term - faster)
persistent_term:put({erlmcp, config}, Config).
```

### Step 5: Test

```bash
# Full validation
make check

# Performance comparison
make benchmark-quick
```

### Step 6: Deploy

1. Deploy to staging
2. Monitor for 24 hours
3. Deploy to production (10% rollout)
4. Monitor metrics
5. Full rollout

---

## OTP 27 → 28 Migration

### Step 1: Pre-Migration Checklist

- [ ] All tests passing on OTP 27.3
- [ ] Review OTP 28 release notes
- [ ] Check Native Implemented Functions (NIFs) compatibility
- [ ] Backup current VM args (`vm.args`)

### Step 2: Install OTP 28.3.1

```bash
kerl build 28.3.1 28.3.1
kerl install 28.3.1 ~/.kerl/28.3.1
. ~/.kerl/28.3.1/activate
erl -version  # Verify: OTP 28
```

### Step 3: Update `rebar.config`

```erlang
% Update minimum version
{minimum_otp_vsn, "28"}.

% Add platform-specific defines
{erl_opts,
 [{platform_define, "^2[6-7]", 'OTP_LEGACY'},
  {platform_define, "^2[8-9]|^[3-9]", 'OTP_MODERN'}]}.
```

### Step 4: Code Changes

**Priority Messages** (new feature):
```erlang
% OTP 28+ ONLY
erlang:send(Pid, Message, [priority]),
erlang:send(Dest, Message, [priority, {noconnect, false}]),
```

**Hibernate Callback** (memory optimization):
```erlang
% Add to gen_server
handle_cast(request, State) ->
    {noreply, State, hibernate}.  % Force hibernation
```

**PCRE2 Regex** (faster):
```erlang
% PCRE2 is now default - no code changes needed
% 3-4x faster than PCRE1 (OTP 27-)
```

### Step 5: Update `vm.args`

```erlang
% OTP 28+ scheduler improvements
+SDio 128          % Dirty I/O schedulers (default in OTP 28)
+SDcpu 128         % Dirty CPU schedulers
+JPperf true       % JIT performance monitoring (new)
+PI 7000           % Process info interval (default changed)
```

### Step 6: Test

```bash
# Full validation
make check

# Test new features
rebar3 eunit --module=erlmcp_priority_tests
rebar3 ct --suite=erlmcp_otp28_compat_SUITE
```

### Step 7: Performance Validation

```bash
# Before/after comparison
make benchmark-full

# Key metrics to check:
# - Message throughput (should improve 5-10%)
# - Memory usage (should decrease 10-15%)
# - Scheduler utilization (should be more balanced)
```

---

## Direct 26 → 28 Migration

**WARNING**: Direct upgrade is high-risk. Only use if:
- No production dependencies on OTP 27
- Can afford extended downtime
- Full regression testing possible

### Accelerated Path

1. **Install OTP 28.3.1** (skip 27)
2. **Apply all code changes** from both 26→27 and 27→28
3. **Update dependencies** to versions supporting OTP 28
4. **Run extended tests** (3x normal suite)

### Risk Mitigation

- Keep OTP 26 environment available for rollback
- Run parallel testing on both versions
- Document all breaking changes encountered
- Have emergency rollback procedure ready

---

## Breaking Changes

### OTP 26 → 27

| Change | Impact | Migration Required |
|--------|--------|-------------------|
| Maybe expressions default | Build failures if `maybe` used as variable | Rename `maybe` variables |
| SSL defaults | `verify_peer` now default | Update SSL config |
| ETS performance | New counters API | Update ETS usage |

### OTP 27 → 28

| Change | Impact | Migration Required |
|--------|--------|-------------------|
| `hibernate/0` callback | Requires callback update | Add hibernate support |
| PCRE2 regex | Binary format incompatibility | Recompile all regex patterns |
| Scheduler defaults | Dirty scheduler changes | Update `vm.args` |
| Process iterator API | Enhanced interface | Update process iteration code |

### erlmcp-Specific Changes

**2.0.x → 2.1.0**:
- Requires OTP 28+ (strict)
- Priority messages in transport layer
- Hibernate support in session backend
- Enhanced JSON-RPC parsing

---

## Deprecation Timeline

### OTP 26 Support
- **EOL**: 2025-12-01
- **Maintenance Mode**: 2025-06-01 to 2025-12-01
- **Status**: Security updates only

### OTP 27 Support
- **EOL**: 2026-06-01
- **Maintenance Mode**: 2025-12-01 to 2026-06-01
- **Status**: Bug fixes + security

### OTP 28 Support
- **EOL**: TBD (estimated 2027-12-01)
- **Status**: Full support

---

## Rollback Procedures

### Emergency Rollback (OTP 28 → 27)

**Trigger**: Critical bug, performance regression, data corruption

**Steps**:
1. Stop erlmcp: `make stop`
2. Switch OTP version:
   ```bash
   kerl deactivate  # Exit OTP 28
   . ~/.kerl/27.3/activate  # Activate OTP 27
   ```
3. Restore code: `git checkout v2.0.x`
4. Rebuild: `make clean compile`
5. Start: `make start`

**Time**: < 5 minutes

### Planned Rollback

**Trigger**: Scheduled maintenance, testing failed

**Steps**:
1. Deploy previous version to staging
2. Run full regression test
3. Switch load balancer to previous version
4. Monitor for 1 hour
5. Decommission new version

**Time**: 30-60 minutes

---

## Testing Requirements

### Unit Tests

**Requirement**: 100% pass rate on all modules

```bash
rebar3 eunit --module=$MODULE
```

### Integration Tests

**Requirement**: All CT suites passing

```bash
rebar3 ct --suite=erlmcp_integration_SUITE
```

### Upgrade Tests

**Requirement**: Specific tests for migration paths

```bash
# OTP 26 → 27
rebar3 ct --suite=erlmcp_otp26_to_27_upgrade_SUITE

# OTP 27 → 28
rebar3 ct --suite=erlmcp_otp27_to_28_upgrade_SUITE

# Direct 26 → 28
rebar3 ct --suite=erlmcp_otp26_to_28_upgrade_SUITE
```

### Performance Tests

**Requirement**: < 10% regression vs baseline

```bash
make benchmark-full
```

**Key Metrics**:
- Registry throughput: 553K msg/s minimum
- Queue throughput: 971K msg/s minimum
- Connection capacity: 40K/node minimum
- Memory per connection: < 10KB

### Load Tests

**Requirement**: Sustain 2x production load for 1 hour

```bash
rebar3 ct --suite=erlmcp_load_test_SUITE
```

---

## Quick Reference

### Verify Current OTP Version

```bash
erl -version
% Or from Erlang shell:
erlang:system_info(otp_release).
```

### Check erlmcp Version

```bash
grep "vsn" apps/erlmcp_core/src/erlmcp.app.src
```

### Full Validation

```bash
make check  # Compile + tests + dialyzer + xref
```

### Performance Baseline

```bash
make benchmark-quick  # 5-minute regression check
```

---

## Support

**Issues**: https://github.com/seanchatmangpt/erlmcp/issues
**Discussions**: https://github.com/seanchatmangpt/erlmcp/discussions
**Email**: support@erlmcp.org

---

**Document Version**: 1.0
**Author**: SPARC Orchestrator
**Review Date**: 2026-03-01
