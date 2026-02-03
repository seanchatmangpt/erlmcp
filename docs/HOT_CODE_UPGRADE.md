# erlmcp Hot Code Upgrade Guide v3.0.0

## Table of Contents
1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Upgrade Procedures](#upgrade-procedures)
4. [Rollback Procedures](#rollback-procedures)
5. [Troubleshooting](#troubleshooting)
6. [Best Practices](#best-practices)
7. [Runtime Considerations](#runtime-considerations)

## Overview

### What is Hot Code Upgrade?

Hot code upgrade (also called "live upgrade" or "zero-downtime upgrade") is the ability to upgrade running Erlang/OTP systems without stopping the node or disrupting service. This is a core feature of Erlang/OTP that enables continuous deployment in production systems.

### Key Benefits

- **Zero Downtime**: No service interruption during upgrades
- **State Preservation**: In-memory state is transformed and preserved
- **Gradual Rollout**: Upgrade multiple nodes one at a time
- **Instant Rollback**: Downgrade if issues are detected
- **Connection Continuity**: Active connections remain alive

### How It Works

```
┌──────────────────────────────────────────────────────────────┐
│                    Upgrade Process Flow                      │
├──────────────────────────────────────────────────────────────┤
│                                                              │
│  1. Pre-Flight Checks    ──►  Verify system health          │
│                                                              │
│  2. Prepare Upgrade      ──►  Suspend supervisors            │
│                                                              │
│  3. Backup State         ──►  Snapshot ETS/Mnesia tables     │
│                                                              │
│  4. Load New Code        ──►  Load new beam files            │
│                                                              │
│  5. Transform State      ──►  Execute code_change/3          │
│                                                              │
│  6. Migrate Data         ──►  Transform ETS/Mnesia data      │
│                                                              │
│  7. Resume Operations    ──►  Resume supervisors             │
│                                                              │
│  8. Verify Upgrade       ──►  Health checks                  │
│                                                              │
└──────────────────────────────────────────────────────────────┘
```

## Prerequisites

### System Requirements

- **Erlang/OTP**: Version 28+ (STRICT requirement)
- **Disk Space**: Minimum 100MB free for upgrade artifacts
- **Memory**: Sufficient headroom (< 90% usage)
- **Process Count**: < 90% of process limit
- **Database**: ETS/Mnesia tables accessible

### Application Requirements

- **Running Node**: erlmcp node must be running
- **No Pending Operations**: All in-flight requests completed
- **Health Status**: System in `healthy` state
- **Dependencies**: All required applications started

### Check Prerequisites

```bash
# Check node status
erlmcpctl status

# Check system health
erlmcpctl health

# Check version
erlmctl version

# Verify prerequisites
./scripts/upgrade.sh 3.0.0 --dry-run
```

## Upgrade Procedures

### Manual Upgrade Using Scripts

#### Step 1: Pre-Flight Checks

```bash
# Run pre-flight checks
./scripts/upgrade.sh 3.0.0 --dry-run

# Expected output:
# [INFO] Running pre-flight checks...
# [INFO] Node erlmcp@127.0.0.1 is running
# [INFO] Checking application state...
# [INFO] Pre-flight checks passed
```

#### Step 2: Execute Upgrade

```bash
# Full upgrade (5 minute timeout)
./scripts/upgrade.sh 3.0.0

# With custom timeout (10 minutes)
./scripts/upgrade.sh 3.0.0 --timeout=600

# Force upgrade (skip checks)
./scripts/upgrade.sh 3.0.0 --force
```

#### Step 3: Verify Upgrade

```bash
# Run post-upgrade verification
./scripts/upgrade.sh 3.0.0 --verify-only

# Expected output:
# [INFO] Verifying upgrade to 3.0.0...
# [INFO] Upgrade verification passed
# [INFO] All checks passed
```

### Programmatic Upgrade (Erlang Shell)

```erlang
% Connect to the running node
erl -name upgrader@127.0.0.1 -setcookie erlmcp -remsh erlmcp@127.0.0.1

% Step 1: Prepare upgrade
{ok, Info} = rpc:call(erlmcp@127.0.0.1, erlmcp_upgrade_coordinator, prepare_upgrade, [<<"3.0.0">>]).

% Step 2: Execute upgrade
Modules = [erlmcp_app, erlmcp_sup, erlmcp_core_sup, erlmcp_server_sup,
           erlmcp_registry, erlmcp_server, erlmcp_client],
{ok, Result} = rpc:call(erlmcp@127.0.0.1, erlmcp_upgrade_coordinator, execute_upgrade, [<<"3.0.0">>, Modules]).

% Step 3: Verify upgrade
{ok, Verification} = rpc:call(erlmcp@127.0.0.1, erlmcp_upgrade_coordinator, verify_upgrade, [<<"3.0.0">>]).
```

### Release Upgrade (relup)

```bash
# Build release with relup
rebar3 release -r 3.0.0

# Install relup
rebar3 relup -m <old_release_dir> -u <new_release_dir>

# Apply upgrade
erlmcp upgrade <release_package>

# Verify
erlmcpctl version
```

## Rollback Procedures

### Automatic Rollback on Failure

The upgrade coordinator automatically rolls back if critical failures occur:

```
Upgrade Failed at Phase: transform_state
├─ Error: {transformation_failed, ...}
├─ Action: Automatic rollback initiated
└─ Status: Rolling back to 2.1.0
```

### Manual Downgrade

```bash
# Immediate downgrade to previous version
./scripts/downgrade.sh 2.1.0

# Downgrade with timeout
./scripts/downgrade.sh 2.1.0 --timeout=600

# Force downgrade (skip checks)
./scripts/downgrade.sh 2.1.0 --force
```

### Programmatic Downgrade

```erlang
% Prepare downgrade
{ok, Info} = rpc:call(erlmcp@127.0.0.1, erlmcp_upgrade_coordinator, prepare_downgrade, [<<"2.1.0">>]).

% Execute downgrade
{ok, Result} = rpc:call(erlmcp@127.0.0.1, erlmcp_upgrade_coordinator, execute_downgrade, [<<"2.1.0">>, Modules]).

% Verify downgrade
{ok, Verification} = rpc:call(erlmcp@127.0.0.1, erlmcp_upgrade_coordinator, verify_downgrade, [<<"2.1.0">>]).
```

## Troubleshooting

### Common Issues

#### Issue: Pre-Flight Checks Fail

**Symptom:**
```
[ERROR] Pre-flight checks failed: {system_not_healthy, degraded}
```

**Solution:**
1. Check system health: `erlmcpctl health`
2. Resolve health issues (memory, CPU, disk)
3. Retry upgrade when system is healthy

#### Issue: Upgrade Stuck at "Suspend Supervisors"

**Symptom:**
```
[INFO] Suspending supervisors...
[WARN] Timeout waiting for supervisor suspend
```

**Solution:**
1. Check supervisor status: `erlmcpctl supervisors`
2. Ensure no blocked gen_servers
3. Increase timeout: `--timeout=600`
4. Last resort: Restart node (data loss possible)

#### Issue: State Transformation Failed

**Symptom:**
```
[ERROR] {transformation_failed, {invalid_state, ...}}
```

**Solution:**
1. Check error logs for details
2. Verify appup file instructions
3. Test upgrade in staging environment
4. Manual rollback: `./scripts/downgrade.sh 2.1.0`

#### Issue: ETS Table Migration Failed

**Symptom:**
```
[ERROR] {ets_migration_failed, [...]}
```

**Solution:**
1. Check ETS table status: `ets:info(Table)`
2. Verify table backup exists
3. Manual restore from backup
4. Contact support

### Emergency Procedures

#### Emergency Rollback

```bash
# Immediate rollback (unsafe)
./scripts/downgrade.sh 2.1.0 --force

# Or via Erlang shell
erl -name emergency@127.0.0.1 -setcookie erlmcp -remsh erlmcp@127.0.0.1
% Execute emergency downgrade
```

#### Node Recovery (Last Resort)

```bash
# If rollback fails, restart node
erlmcpctl stop
erlmcpctl start

# Restore from backup if needed
erlmcpctl restore /backup/path
```

## Best Practices

### Before Upgrade

1. **Test in Staging**: Always test upgrades in staging first
2. **Backup Data**: Create full system backup
3. **Monitor System**: Have monitoring ready
4. **Plan Rollback**: Know rollback procedure
5. **Notify Users**: Inform users of planned upgrade

### During Upgrade

1. **Monitor Logs**: Watch upgrade progress in real-time
2. **Check Metrics**: Monitor performance metrics
3. **Verify Phases**: Ensure each phase completes
4. **Be Patient**: Don't interrupt upgrade process
5. **Document Issues**: Record any problems encountered

### After Upgrade

1. **Run Smoke Tests**: Verify basic functionality
2. **Monitor Performance**: Watch for performance issues
3. **Check Logs**: Review error logs
4. **Verify Data**: Ensure data integrity
5. **Update Documentation**: Document upgrade results

### Upgrade Checklist

```
Pre-Upgrade:
☐ Backup system state
☐ Test upgrade in staging
☐ Notify stakeholders
☐ Prepare rollback plan
☐ Monitor system health

During Upgrade:
☐ Watch upgrade logs
☐ Monitor system metrics
☐ Verify each phase
☐ Don't interrupt process

Post-Upgrade:
☐ Run smoke tests
☐ Verify application state
☐ Check error logs
☐ Monitor performance
☐ Document results
```

## Runtime Considerations

### Performance Impact

- **Upgrade Duration**: Typically 30-120 seconds
- **CPU Spike**: Temporary increase during code load
- **Memory Usage**: Brief increase during state transformation
- **Throughput**: Minimal impact on request processing

### Connection Handling

- **Active Connections**: Preserved during upgrade
- **New Connections**: Briefly paused during supervisor suspend
- **In-Flight Requests**: Completed before upgrade proceeds
- **Session State**: Transformed and preserved

### Data Consistency

- **ETS Tables**: Atomic transformation
- **Mnesia**: Transaction-safe migration
- **Registry State**: Version-aware transformation
- **Process State**: Converted via code_change/3

### Monitoring During Upgrade

Monitor these metrics during upgrade:

```erlang
% Check upgrade progress
{ok, Progress} = erlmcp_upgrade_coordinator:get_upgrade_status().

% Monitor system health
Health = erlmcp_upgrade_monitor:get_system_health().

% Check upgrade phases
{ok, Progress} = erlmcp_upgrade_monitor:get_upgrade_progress().
```

### OTEL Spans

Upgrade phases are traced with OpenTelemetry:

```
erlmcp_upgrade (span)
├─ prepare_upgrade
├─ suspend_supervisors
├─ backup_state
├─ load_modules
├─ transform_state
├─ migrate_data
├─ resume_supervisors
└─ verify_upgrade
```

View traces in your OTEL dashboard.

## Version Compatibility Matrix

| From Version | To Version | Supported | Notes |
|--------------|------------|-----------|-------|
| 2.1.0        | 3.0.0      | ✅ Yes    | Direct upgrade supported |
| 2.0.0        | 3.0.0      | ❌ No     | Upgrade to 2.1.0 first |
| 1.x          | 3.0.0      | ❌ No     | Major version change |

## Support

For issues or questions:

- **Documentation**: See /docs directory
- **Issue Tracker**: https://github.com/banyan-platform/erlmcp/issues
- **Email**: support@banyan-platform.com

## Appendix

### File Structure

```
erlmcp/
├── apps/
│   ├── erlmcp_core/
│   │   └── src/
│   │       ├── erlmcp_core.appup      # Upgrade instructions
│   │       ├── erlmcp_upgrade_coordinator.erl
│   │       ├── erlmcp_state_migration.erl
│   │       └── erlmcp_protocol_versioning.erl
│   ├── erlmcp_transports/
│   │   └── src/
│   │       └── erlmcp_transports.appup
│   ├── erlmcp_observability/
│   │   └── src/
│   │       └── erlmcp_observability.appup
│   └── erlmcp_validation/
│       └── src/
│           └── erlmcp_validation.appup
├── scripts/
│   ├── upgrade.sh                     # Upgrade script
│   └── downgrade.sh                   # Downgrade script
└── docs/
    ├── HOT_CODE_UPGRADE.md            # This document
    ├── UPGRADE_RUNBOOK.md             # Troubleshooting guide
    └── CHANGELOG.md                   # Version history
```

### Environment Variables

- `ERLMCP_NODE`: Node name (default: `erlmcp@127.0.0.1`)
- `ERLMCP_COOKIE`: Erlang cookie (default: `erlmcp`)
- `ERLMCP_UPGRADE_TIMEOUT`: Upgrade timeout in seconds (default: 300)
- `CT_SKIP_UPGRADE_TEST`: Skip upgrade tests in CI (default: `false`)

### References

- [OTP Design Principles - Appup](http://erlang.org/doc/design_principles/appup.html)
- [Release Handling](http://erlang.org/doc/design_principles/release_handling.html)
- [System Principles](http://erlang.org/doc/system_principles/system_principles.html)
