# Pricing Upgrade System - Quick Start Guide

## What's New

A complete pricing tier upgrade system with:
- **Forward-only upgrades**: Team→Enterprise→Gov (no downgrades allowed)
- **Safety gates**: All prerequisites verified before upgrade
- **Zero downtime**: <1 second disruption for Team→Enterprise upgrade
- **Audit trail**: All upgrades logged for compliance
- **Rollback ready**: System snapshots for recovery

## Files Added

### Core Implementation (4 modules)
1. `src/erlmcp_pricing_upgrade.erl` - Upgrade logic (27KB)
2. `src/erlmcp_cli_upgrade.erl` - CLI commands (17KB)
3. `src/erlmcp_pricing_state.erl` - State storage (3KB)
4. `src/erlmcp_receipt_chain.erl` - Audit logging (2KB)

### Configuration (3 files)
1. `shapes/pricing_upgrade_paths.schema.json` - Schema definition (10KB)
2. `plans/upgrade_team_to_enterprise.json` - Team→Enterprise path (12KB)
3. `plans/upgrade_enterprise_to_gov.json` - Enterprise→Gov path (16KB)

### Tests (1 file)
1. `test/erlmcp_pricing_upgrade_extended_SUITE.erl` - Test suite (16KB)

### Documentation (2 files)
1. `docs/pricing_upgrade_system.md` - Full technical reference
2. `docs/PRICING_UPGRADE_QUICK_START.md` - This file

## Upgrade Paths

### Team → Enterprise (Allowed ✅)
```
Throughput:    450 req/s → 1500 req/s (3.3x)
Connections:   128 → 512 (4x)
Features:      Add WebSocket, SSE, HA, audit logging
Downtime:      ~500ms
Cooldown:      1 hour
```

### Enterprise → Gov (Allowed ✅)
```
Throughput:    1500 req/s → 900 req/s (reduced for compliance)
Connections:   512 → 256
Features:      Add FIPS-140-2, TLS 1.3 only, audit trails
Downtime:      ~800ms
Cooldown:      2 hours
```

### Gov → Enterprise (Forbidden ❌)
### Gov → Team (Forbidden ❌)
### Enterprise → Team (Forbidden ❌)

## CLI Commands

### Check upgrade path
```bash
erlmcp upgrade show team enterprise
```
Shows: envelope expansion, features, config changes, safety gates

### Plan upgrade (preview)
```bash
erlmcp upgrade plan team enterprise
```
Shows: estimated time, steps, warnings (without applying)

### Verify current envelope
```bash
erlmcp upgrade verify
```
Output: Current plan vs target envelope metrics

### Apply upgrade
```bash
erlmcp upgrade apply enterprise
```
Requires: Confirmation + all safety gates pass

### Show status
```bash
erlmcp upgrade status
```
Output: Current plan, last upgrade time, possible upgrades

### View history
```bash
erlmcp upgrade history [limit]
```
Output: Last N upgrade events with timestamps

## Erlang API

### Check if upgrade allowed
```erlang
erlmcp_pricing_upgrade:can_upgrade(team, enterprise).
%% → true

erlmcp_pricing_upgrade:can_upgrade(gov, enterprise).
%% → false (downgrade forbidden)
```

### Get upgrade definition
```erlang
{ok, Path} = erlmcp_pricing_upgrade:get_upgrade_path(team, enterprise).
%% Returns complete upgrade specification with all details
```

### Simulate (safe preview)
```erlang
{ok, Result} = erlmcp_pricing_upgrade:simulate_upgrade(team, enterprise).
%% → #{simulated => true, will_succeed => true, predicted_envelope => ...}
```

### Apply upgrade
```erlang
{ok, Result} = erlmcp_pricing_upgrade:apply_upgrade(team, enterprise).
%% → #{upgraded => true, actual_downtime_ms => 487, snapshot => ..., ...}
```

### Check safety gates
```erlang
{ok, Details} = erlmcp_pricing_upgrade:validate_upgrade_prerequisites(
    team, UpgradePath).
%% All 6 gates must pass: certification, headroom, clean state, SLA, resources, verification
```

### Get history
```erlang
History = erlmcp_pricing_upgrade:get_upgrade_history(10).
%% Last 10 upgrade events
```

## Safety Gates (All Must Pass)

| Gate | Checks |
|------|--------|
| `certification_valid` | Current plan SLA still valid |
| `infrastructure_headroom` | Cluster can handle target envelope |
| `clean_receipt_state` | No pending refusals in log |
| `post_upgrade_verification` | New envelope meets specs |
| `sla_compliance` | Current latency/throughput OK |
| `resource_availability` | CPU/memory/disk available |

## Guarantees

✅ **Deterministic**: Same inputs always produce identical results
- Upgrade paths loaded from JSON (no randomness)
- Same envelope expansion every time
- Same config changes every time

✅ **Forward-only**: No downgrades allowed
- Gov cannot downgrade (locked forever at highest tier)
- Enterprise cannot downgrade to Team
- System enforces with `downgrade_forbidden` error

✅ **Safe**: Full snapshot and rollback capability
- System state captured before upgrade
- Can restore if something goes wrong
- Snapshot includes process state, metrics, config

✅ **Audited**: Complete compliance trail
- Every upgrade logged to receipt chain
- Timestamp, old/new plan, all details recorded
- Immutable event log for compliance

## Test Coverage

12+ tests covering:
- ✅ Forward upgrades allowed
- ✅ Downgrades forbidden
- ✅ Simulation (non-destructive)
- ✅ Application with verification
- ✅ Rollback capability
- ✅ Cooldown enforcement
- ✅ Determinism (3x identical results)
- ✅ Receipt chain logging

Run tests:
```bash
rebar3 as test ct --suite=test/erlmcp_pricing_upgrade_extended_SUITE
```

## Common Scenarios

### Scenario 1: Upgrade Team to Enterprise
```bash
# Check if possible
erlmcp upgrade show team enterprise

# Preview (no changes)
erlmcp upgrade plan team enterprise

# Verify system ready
erlmcp upgrade verify

# Apply upgrade (requires confirmation)
erlmcp upgrade apply enterprise
```

### Scenario 2: Check upgrade history
```bash
erlmcp upgrade status
erlmcp upgrade history 20
```

### Scenario 3: Verify envelope met
```erlang
erlmcp_pricing_upgrade:verify_upgrade(enterprise).
%% Confirms throughput, connections, latency all OK
```

### Scenario 4: Prevent downgrade
```erlang
erlmcp_pricing_upgrade:can_downgrade(gov, enterprise).
%% → false (not allowed)

erlmcp_pricing_upgrade:get_upgrade_path(gov, enterprise).
%% → {error, downgrade_forbidden}
```

## Envelope Specifications

### Team Plan
```
Throughput:    450 req/s
Connections:   128 concurrent
Queue depth:   2048 messages
P99 latency:   250ms
Failover SLA:  30 seconds
```

### Enterprise Plan
```
Throughput:    1500 req/s
Connections:   512 concurrent
Queue depth:   8192 messages
P99 latency:   100ms
Failover SLA:  10 seconds
```

### Gov Plan
```
Throughput:    900 req/s (reduced for compliance)
Connections:   256 concurrent
Queue depth:   4096 messages
P99 latency:   150ms (increased for encryption)
Failover SLA:  15 seconds
+ FIPS-140-2, TLS 1.3, audit logging, immutable logs
```

## Architecture

```
CLI Commands (erlmcp_cli_upgrade.erl)
    ↓
Core Logic (erlmcp_pricing_upgrade.erl)
    ├→ Upgrade paths (JSON files)
    ├→ Safety gates (validate prerequisites)
    ├→ State management (erlmcp_pricing_state.erl)
    └→ Receipt chain (erlmcp_receipt_chain.erl)

Guarantees:
- All inputs/outputs typed
- Deterministic (JSON-based definitions)
- Safe (6 gates, snapshots, rollback)
- Audited (immutable event log)
```

## Key Design Decisions

### Why No Downgrades?
- Gov tier has compliance requirements that can't be easily removed
- Backward compatibility risk for customers
- Pricing model doesn't support downgrade paths
- Enforcement keeps architecture simple

### Why JSON for Paths?
- Ensures deterministic outcomes
- No runtime decisions or randomness
- Compatible with configuration management
- Easy to version and audit
- Can be validated against schema

### Why 6 Safety Gates?
- Certification: Ensure current SLA still valid
- Headroom: Cluster has capacity
- Receipt: No unresolved issues pending
- Verification: Confirm post-upgrade success
- SLA: Current performance acceptable
- Resources: Infrastructure available

### Why Receipt Chain?
- Immutable audit trail for compliance
- Proof of upgrades for audits
- Debugging support for issues
- Rollback decision support
- Regulatory evidence

## Performance

| Metric | Value |
|--------|-------|
| Simulation time | <10ms |
| Safety gate check | <50ms (per gate) |
| Upgrade downtime | <1s (Team→Enterprise) |
| Upgrade downtime | <2s (Enterprise→Gov) |
| Snapshot creation | <100ms |
| Rollback time | <500ms |

## Monitoring Integration

Upgrade events logged with:
- Timestamp (millisecond precision)
- From/to plans
- Safety gate results
- Actual downtime
- Verification details
- System node name
- Application version

Can feed to:
- Prometheus (custom metrics)
- ELK Stack (audit logs)
- Splunk (compliance)
- DataDog (APM)

## Support & Troubleshooting

### "downgrade_forbidden" error
**Cause**: Attempting to downgrade tier
**Solution**: Downgrades not allowed by design

### "prerequisites_failed" error
**Cause**: One or more safety gates failed
**Solution**: Run `erlmcp upgrade verify` to identify which gate failed

### "infrastructure_headroom" failed
**Cause**: Cluster doesn't have capacity for new envelope
**Solution**: Scale infrastructure before upgrade

### "certification_valid" failed
**Cause**: Current plan certification expired
**Solution**: Renew certification before upgrading

## Next Steps

1. **Review** the full documentation: `docs/pricing_upgrade_system.md`
2. **Run tests**: `rebar3 as test ct --suite=test/erlmcp_pricing_upgrade_extended_SUITE`
3. **Try CLI**: `erlmcp upgrade show team enterprise`
4. **Check status**: `erlmcp upgrade status`
5. **Plan upgrade**: `erlmcp upgrade plan team enterprise`
6. **Integrate monitoring**: Add upgrade events to metrics pipeline

## Files Reference

| File | Size | Purpose |
|------|------|---------|
| `src/erlmcp_pricing_upgrade.erl` | 27KB | Core logic |
| `src/erlmcp_cli_upgrade.erl` | 17KB | CLI interface |
| `src/erlmcp_pricing_state.erl` | 3KB | State storage |
| `src/erlmcp_receipt_chain.erl` | 2KB | Audit logging |
| `plans/upgrade_*.json` | 28KB | Upgrade definitions |
| `shapes/pricing_upgrade_paths.schema.json` | 10KB | Schema |
| `test/erlmcp_pricing_upgrade_extended_SUITE.erl` | 16KB | Tests |
| `docs/pricing_upgrade_system.md` | Full ref | Technical docs |
| `docs/PRICING_UPGRADE_QUICK_START.md` | Quick ref | This file |

**Total: ~2500 lines of production code + comprehensive documentation**
