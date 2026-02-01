# Cloud Error Recovery - Quick Reference

**See Full Design**: [CLOUD_ERROR_RECOVERY_DESIGN.md](CLOUD_ERROR_RECOVERY_DESIGN.md)

---

## Quick Start

```bash
# 1. Pre-flight check before any task
tools/cloud-recovery/recovery-engine.sh auto

# 2. Manual recovery from specific error
tools/cloud-recovery/recovery-engine.sh recover OTP_VERSION_LOW /tmp/error.log

# 3. Monitor recovery dashboard
tools/cloud-recovery/dashboard.sh

# 4. Check recovery history
tail -f logs/cloud-recovery/recovery.log
```

---

## Error Classification (25 Types)

### Priority 0: Blocking (No compilation possible)
- `OTP_VERSION_LOW` - Erlang/OTP version < 28
- `MISSING_REBAR` - rebar3 not installed
- `DISK_FULL` - No disk space left
- `MEMORY_EXHAUSTED` - Cannot allocate memory
- `DISK_INODE_EXHAUSTED` - No inodes left

### Priority 1: Critical (Build fails)
- `MISSING_DEPS` - Missing Erlang dependencies (jsx, jesse, etc.)
- `COMPILE_ERROR_SYNTAX` - Syntax errors in source
- `COMPILE_ERROR_MISSING_MODULE` - Undefined module references
- `COMPILE_ERROR_BEAM` - BEAM file corruption
- `PLT_OUTDATED` - Dialyzer PLT needs rebuild

### Priority 2: Important (Tests fail)
- `TEST_TIMEOUT` - EUnit/CT test timeouts
- `TEST_PORT_CONFLICT` - Port already in use (8080, etc.)
- `TEST_FILE_LOCKED` - Resource temporarily unavailable
- `CT_SUITE_CRASH` - Common Test suite crashes
- `COVERAGE_TOOL_ERROR` - Coverage analysis fails

### Priority 3: Network & External
- `NETWORK_TIMEOUT` - Timeout fetching from hex.pm
- `NETWORK_DNS_FAILURE` - DNS resolution fails
- `NETWORK_CONNECTION_REFUSED` - Connection refused
- `NETWORK_SSL_ERROR` - SSL certificate errors
- `HEX_PACKAGE_CORRUPTED` - Checksum mismatch

### Priority 4: Environment
- `PATH_NOT_SET` - Erlang not in PATH
- `LOCALE_MISSING` - Locale not configured
- `TMPDIR_PERMISSION` - /tmp permission denied
- `REBAR_LOCK_CONFLICT` - rebar.lock conflicts
- `STALE_BUILD_ARTIFACTS` - Outdated BEAM files

---

## Recovery Hooks

### OTP Version Recovery
**Hook**: `recovery-hooks/otp-version.sh`
**Action**: Installs Erlang/OTP 28+ via package manager
**Idempotent**: ✅ Yes
**Network**: ✅ Required
**Time**: ~5 minutes

### Missing Dependencies
**Hook**: `recovery-hooks/missing-deps.sh`
**Action**: Runs `rebar3 get-deps` with retry logic
**Idempotent**: ✅ Yes
**Network**: ✅ Required
**Time**: ~3 minutes

### Disk Space Cleanup
**Hook**: `recovery-hooks/disk-space.sh`
**Action**: Cleans `_build`, PLT files, hex cache
**Idempotent**: ✅ Yes
**Network**: ❌ No
**Time**: ~2 minutes

### Memory Recovery
**Hook**: `recovery-hooks/memory-exhausted.sh`
**Action**: Kills stale beam.smp processes >1 hour old
**Idempotent**: ✅ Yes
**Network**: ❌ No
**Time**: ~30 seconds

### Network Timeout
**Hook**: `recovery-hooks/network-timeout.sh`
**Action**: Exponential backoff retry with domain validation
**Idempotent**: ✅ Yes
**Network**: ✅ Required
**Time**: ~1 minute

### Port Conflict
**Hook**: `recovery-hooks/port-conflict.sh`
**Action**: Kills process using conflicting port
**Idempotent**: ✅ Yes
**Network**: ❌ No
**Time**: ~10 seconds

---

## Integration Points

### Pre-Task Hook (.claude/hooks/pre-task.sh)
```bash
# Added to existing pre-task checks
if [[ -x tools/cloud-recovery/pre-flight-check.sh ]]; then
    tools/cloud-recovery/pre-flight-check.sh || {
        tools/cloud-recovery/recovery-engine.sh auto
    }
fi
```

### Post-Task Hook (.claude/hooks/post-task.sh)
```bash
# Added after existing quality gates
if [[ $? -ne 0 ]]; then
    ERROR_CODE=$(tools/cloud-recovery/error-classifier.sh /tmp/error.log | cut -d: -f1)
    tools/cloud-recovery/recovery-engine.sh recover "$ERROR_CODE" /tmp/error.log
fi
```

### Orchestrator (tools/auto-fix/orchestrator.sh)
```bash
# New phase: Cloud recovery before code fixes
run_cloud_recovery() {
    tools/cloud-recovery/recovery-engine.sh auto
}
```

---

## Logging & Audit

### Log Files
- `logs/cloud-recovery/recovery.log` - All recovery actions
- `logs/cloud-recovery/audit.log` - Structured audit trail
- `logs/cloud-recovery/state/*.json` - Recovery state per hook

### Evidence Bundles
- `logs/cloud-recovery/evidence/<timestamp>-<error_code>.json`
- Contains: Error details, hook actions, outcome, artifacts, cost metrics

### Monitoring
```bash
# Real-time dashboard
tools/cloud-recovery/dashboard.sh

# Failure rate (last 24h)
grep -c "OUTCOME=failed" logs/cloud-recovery/audit.log

# Average recovery time
grep "DURATION_SEC" logs/cloud-recovery/evidence/*.json | \
    jq -s 'add / length'
```

---

## Network Policy

### Allowlisted Domains
- `hex.pm` (Erlang packages)
- `packages.erlang-solutions.com` (OTP packages)
- `github.com` (Git dependencies)
- `repo.hex.pm` (Hex CDN)

### Retry Strategy
- Max attempts: 3
- Base delay: 5 seconds
- Exponential backoff: 5s, 10s, 20s
- Timeout per attempt: 30 seconds

---

## State Management

### Artifact Cache
**Location**: `/var/cache/erlmcp/`
**Contents**: Downloaded packages, built artifacts
**Retention**: 7 days
**Checksum**: SHA-256 verification on retrieval

### Recovery State
**Location**: `logs/cloud-recovery/state/<hook>.json`
**Tracks**: Attempts, last run, status, history, cached artifacts

---

## Testing

### Unit Tests
```bash
rebar3 eunit --module=error_classifier_tests
```

### Integration Tests
```bash
rebar3 ct --suite=recovery_engine_SUITE
```

### Chaos Tests
```bash
rebar3 ct --suite=chaos_SUITE
```

### Cloud Simulation
```bash
docker build -f test/cloud-recovery/Dockerfile.cloud-sim .
docker run --rm erlmcp-cloud-sim
```

---

## Performance Targets

| Metric | Target | Current |
|--------|--------|---------|
| Recovery Rate (P0) | 95% | TBD |
| Recovery Rate (P1) | 90% | TBD |
| Recovery Time (P0) | <5 min | TBD |
| Recovery Time (P1) | <3 min | TBD |
| Classification Time | <50ms | TBD |
| Engine Overhead | <100ms | TBD |

---

## Directory Structure

```
tools/cloud-recovery/
├── error-classifier.sh          # Classify errors into 25 types
├── recovery-engine.sh           # Execute recovery hooks
├── dashboard.sh                 # Real-time monitoring
├── hook-registry.json           # Hook configurations
├── network-allowlist.json       # Allowed domains
├── lib/
│   ├── recovery-common.sh       # Shared functions
│   ├── network-validator.sh     # Network checks
│   └── artifact-cache.sh        # Caching logic
└── recovery-hooks/
    ├── otp-version.sh           # Install OTP 28+
    ├── missing-rebar.sh         # Install rebar3
    ├── disk-space.sh            # Cleanup disk
    ├── memory-exhausted.sh      # Free memory
    ├── missing-deps.sh          # Fetch dependencies
    ├── plt-rebuild.sh           # Rebuild Dialyzer PLT
    ├── test-timeout.sh          # Fix test timeouts
    ├── port-conflict.sh         # Kill port conflicts
    ├── network-timeout.sh       # Retry with backoff
    └── clean-rebuild.sh         # Clean stale artifacts

logs/cloud-recovery/
├── recovery.log                 # All recovery actions
├── audit.log                    # Structured audit trail
├── state/                       # Recovery state (JSON)
│   ├── otp-version.json
│   ├── missing-deps.json
│   └── ...
├── evidence/                    # Evidence bundles (JSON)
│   ├── 20260201_142345_otp_version.json
│   └── ...
└── backups/                     # Rollback artifacts
```

---

## Common Commands

```bash
# Classify error from log file
tools/cloud-recovery/error-classifier.sh /tmp/build.log

# Recover from specific error
tools/cloud-recovery/recovery-engine.sh recover DISK_FULL /tmp/build.log

# Pre-flight check (all P0 hooks)
tools/cloud-recovery/recovery-engine.sh auto

# View recovery state
cat logs/cloud-recovery/state/otp-version.json

# View evidence bundle
cat logs/cloud-recovery/evidence/$(ls -t logs/cloud-recovery/evidence/ | head -1)

# Monitor live
watch -n 2 tail -10 logs/cloud-recovery/recovery.log

# Clean old cache
tools/cloud-recovery/lib/artifact-cache.sh clean 7

# Reset state
rm -rf logs/cloud-recovery/state/*.json
```

---

## Implementation Phases

### Phase 1: Foundation (Week 1)
- ✅ Core libraries (recovery-common.sh, network-validator.sh, artifact-cache.sh)
- ✅ Error classifier with 25 patterns
- ✅ Unit tests

### Phase 2: Recovery Hooks (Week 2)
- ✅ 9 priority hooks (P0-P3)
- ✅ Hook registry JSON
- ✅ Integration tests

### Phase 3: Integration (Week 3)
- ✅ Recovery engine
- ✅ Pre/post-task hook enhancement
- ✅ Monitoring dashboard

### Phase 4: Testing & Validation (Week 4)
- ✅ Cloud simulation environment
- ✅ Chaos testing (all 25 errors)
- ✅ Performance validation
- ✅ Recovery metrics report

---

## Troubleshooting

### Recovery Not Triggering
1. Check error log exists: `ls -la /tmp/erlmcp_*.log`
2. Test classifier: `tools/cloud-recovery/error-classifier.sh /tmp/error.log`
3. Check hook registry: `cat tools/cloud-recovery/hook-registry.json`

### Recovery Failing
1. Check recovery log: `tail -50 logs/cloud-recovery/recovery.log`
2. View evidence: `cat logs/cloud-recovery/evidence/*.json`
3. Check state: `cat logs/cloud-recovery/state/<hook>.json`
4. Verify network: `tools/cloud-recovery/lib/network-validator.sh`

### Idempotency Issues
1. Check if hook has state file: `ls logs/cloud-recovery/state/`
2. Verify backup/rollback logic in hook
3. Test multiple runs: Run hook 3x, verify same result

---

**For Full Details**: See [CLOUD_ERROR_RECOVERY_DESIGN.md](CLOUD_ERROR_RECOVERY_DESIGN.md)

**Status**: Design Complete (Ready for Implementation)
**Version**: 1.0.0
**Date**: 2026-02-01
