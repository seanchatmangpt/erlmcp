# erlmcp Upgrade Guide

Safe, well-tested version upgrades with automated health verification.

## Quick Start

### Generate Upgrade Plan

Before upgrading, review the changes:

```bash
erlmcp upgrade plan 1.3.0 1.4.0
```

Output:
```
UPGRADE PLAN
============

CONFIG:
  - client_defaults.max_pending_requests
      Description: Increased from 100 to 250 for higher concurrency
  - server_defaults.max_subscriptions_per_resource
      Description: Increased from 500 to 1000 for scalability

BEHAVIOR:
  - circuit_breaker
      Description: Automatic circuit breaker enabled to prevent retry amplification
  - upgrade_plan
      Description: New upgrade plan/verify CLI commands for safe version transitions
  - ...
```

### Verify Upgrade Health

After upgrading, run verification:

```bash
erlmcp upgrade verify
```

Output:
```
UPGRADE VERIFICATION
====================

  [OK] endpoints_responsive
  [OK] registry_shards_healthy
  [OK] receipt_emission_working
  [OK] queue_bounds_respected
  [OK] transport_stable
  [OK] configuration_loaded
  [OK] supervision_tree_healthy
  [OK] version_consistent

Upgrade verification PASSED
```

## Upgrade Paths

### v0.7.0 → v1.0.0

**Key Changes:**
- HTTP max_connections increased: 50 → 100
- Registry sharding enabled by default for 100K+ connections
- Memory optimization with buffer pool and session compression
- TCP transport switched to ranch 2.1.0

**Migration Steps:**
1. Review plan: `erlmcp upgrade plan 0.7.0 1.0.0`
2. Backup configuration
3. Update application version in `src/erlmcp.app.src`
4. Run: `rebar3 compile && rebar3 release`
5. Verify: `erlmcp upgrade verify`

### v1.0.0 → v1.3.0

**Key Changes:**
- Max progress tokens increased: 5000 → 10000
- Completion API moved from experimental to stable
- WebSocket transport moved from experimental to stable
- Backpressure handling and connection limiting enabled

**Migration Steps:**
1. Review plan: `erlmcp upgrade plan 1.0.0 1.3.0`
2. Update sys.config if using custom queue settings
3. Rebuild: `rebar3 compile`
4. Deploy and verify: `erlmcp upgrade verify`

### v1.3.0 → v1.4.0

**Key Changes:**
- Max pending requests increased: 100 → 250
- Circuit breaker enabled (prevents retry amplification)
- Enhanced OTEL instrumentation
- Improved stdio I/O handling
- New upgrade_plan/verify commands

**Migration Steps:**

1. **Generate upgrade plan** to understand all changes:
   ```bash
   erlmcp upgrade plan 1.3.0 1.4.0
   ```

2. **Backup current state**:
   ```bash
   cp -r config config.backup.v1.3.0
   cp -r sys.config sys.config.backup.v1.3.0
   ```

3. **Update application version**:
   ```bash
   # Edit src/erlmcp.app.src
   {vsn, "1.4.0"}
   ```

4. **Review configuration changes**:
   - Higher concurrency defaults are backward compatible
   - Circuit breaker is opt-in in config (see below)

5. **Compile and release**:
   ```bash
   rebar3 clean
   rebar3 compile
   rebar3 release
   ```

6. **Deploy new version** to your environment

7. **Verify post-upgrade health**:
   ```bash
   erlmcp upgrade verify
   ```

8. **Monitor for issues** in first 24 hours:
   - Watch OTEL metrics dashboard
   - Check application logs
   - Verify message throughput

## Configuration Changes by Version

### erlmcp.app.src

Track these environment variables across versions:

```erlang
{env, [
    {client_defaults, #{
        timeout => 5000,
        strict_mode => false,
        max_pending_requests => 250         % v1.4.0+: was 100
    }},
    {server_defaults, #{
        max_subscriptions_per_resource => 1000,  % v1.4.0+: was 500
        max_progress_tokens => 10000       % v1.0.0+: was 5000
    }},
    {transport_defaults, #{
        tcp => #{
            connect_timeout => 5000,
            keepalive => true,
            nodelay => true
        },
        http => #{
            connect_timeout => 5000,
            request_timeout => 30000,
            max_connections => 100          % v1.0.0+: was 50
        }
    }}
]}
```

### Circuit Breaker (v1.4.0+)

Enable circuit breaker to prevent retry amplification:

```erlang
{circuit_breaker, #{
    enabled => true,
    failure_threshold => 5,
    recovery_timeout => 30000,
    half_open_requests => 3
}}
```

## Health Checks

The upgrade verify command runs 8 critical checks:

| Check | Purpose | Failure Impact |
|-------|---------|-----------------|
| endpoints_responsive | Main supervisor responding | Critical |
| registry_shards_healthy | Message routing working | Critical |
| receipt_emission_working | Optional feature | Warning |
| queue_bounds_respected | Memory limits enforced | Critical |
| transport_stable | TCP/HTTP/Stdio working | Critical |
| configuration_loaded | Config keys present | Critical |
| supervision_tree_healthy | All child processes alive | Critical |
| version_consistent | Version format valid | Warning |

**Passing threshold:** At least 6/8 critical checks must pass.

## Troubleshooting

### "endpoint_responsive check failed"

```bash
# Verify erlmcp application is running
erl -pa _build/default/lib/*/ebin

> application:ensure_all_started(erlmcp).
> whereis(erlmcp_sup).
<0.ABC.0>
```

If supervisor not running, check logs for startup errors.

### "registry_shards_healthy check failed"

```bash
# Verify registry is operational
> erlmcp_registry:list_servers().
[]

# If empty list, registry is working but no servers registered
```

### "queue_bounds_respected check failed"

```bash
# Check current queue configuration
> application:get_env(erlmcp, client_defaults).
{ok, #{max_pending_requests => 250}}

# If not set, defaults are 100/1000 depending on version
```

### "transport_stable check failed"

```bash
# Verify transport modules loaded
> code:ensure_loaded(erlmcp_transport_tcp).
{module, erlmcp_transport_tcp}
```

## Rollback Procedure

If upgrade verification fails:

1. **Stop the application**:
   ```bash
   erlmcp stop
   ```

2. **Restore previous version**:
   ```bash
   cd /path/to/erlmcp
   git checkout vX.Y.Z  # or restore from backup
   ```

3. **Recompile**:
   ```bash
   rebar3 clean
   rebar3 compile
   rebar3 release
   ```

4. **Restart**:
   ```bash
   erlmcp start
   ```

5. **Verify rollback**:
   ```bash
   erlmcp status
   ```

## Deterministic Testing

Upgrade plans are fully deterministic - same input always produces identical output.

Verify this yourself:

```bash
# Run upgrade plan 5 times
for i in {1..5}; do
  erlmcp upgrade plan 1.3.0 1.4.0 > plan_run_$i.txt
done

# All files should be identical
diff plan_run_1.txt plan_run_2.txt  # No output = identical
diff plan_run_2.txt plan_run_3.txt  # No output = identical
```

## Version Constraints

### Supported Upgrade Paths

✅ **Forward upgrades** (increasing version):
```
0.7.0 → 1.0.0 ✓
1.0.0 → 1.3.0 ✓
1.3.0 → 1.4.0 ✓
0.7.0 → 1.4.0 ✓ (skips intermediate versions)
```

❌ **Not supported**:
```
1.4.0 → 1.3.0 ✗ (downgrade)
1.4.0 → 1.4.0 ✗ (same version - no migration needed)
invalid → 1.4.0 ✗ (invalid format)
```

### Version Format

Must be semantic versioning: `MAJOR.MINOR.PATCH`

Valid: `0.7.0`, `1.0.0`, `1.3.0`, `1.4.0`, `2.0.0`

Invalid: `1.0`, `1`, `v1.0.0`, `1.0.0.0`

## Testing Upgrade in Dev

### Simulate Full Upgrade Cycle

```bash
# 1. Check current version
rebar3 shell
> application:info(erlmcp).

# 2. Generate plan
erlmcp upgrade plan 1.3.0 1.4.0

# 3. Make config changes
# (edit sys.config)

# 4. Recompile
rebar3 compile

# 5. Verify health
erlmcp upgrade verify
```

### Run Upgrade Test Suite

```bash
# Run all upgrade tests
rebar3 ct --suite=erlmcp_upgrade_SUITE

# Run specific test
rebar3 ct --suite=erlmcp_upgrade_SUITE --case=test_plan_v1_3_to_v1_4

# Run with repeats for determinism verification
rebar3 ct --suite=erlmcp_upgrade_SUITE --repeat=5
```

## Monitoring Post-Upgrade

After deployment, monitor these metrics:

### Critical Metrics

- **Message throughput** (operations/sec) - should remain stable
- **Queue depth** (messages) - should stay within configured bounds
- **Memory usage** (MB) - should not grow linearly
- **Error rate** (errors/min) - should not increase

### OTEL Spans

Look for spans tagged with:
- `otel.event=upgrade_started`
- `otel.event=health_check`
- `otel.event=version_transition`

### Log Markers

Search logs for:
```
[upgrade] Upgrade from X.Y.Z to A.B.C started
[upgrade] Health check: endpoints_responsive = ok
[upgrade] All health checks passed
```

## FAQ

**Q: Can I skip versions (e.g., 1.0 → 1.4)?**
A: Yes, `erlmcp upgrade plan 1.0.0 1.4.0` will generate the complete plan across all intermediate changes.

**Q: Is downgrading supported?**
A: No. Downgrading is not supported. Use git rollback or restore from backup.

**Q: How long does verification take?**
A: Typically 2-5 seconds depending on system load.

**Q: Can verification fail on healthy systems?**
A: Rarely. Most failures indicate configuration issues. Run `erlmcp status` to debug.

**Q: Do I need to stop the app before upgrading?**
A: Yes. Apply the new version while the app is stopped, then restart.

**Q: What if verification fails?**
A: See Troubleshooting section. Most issues are resolvable without rollback.
