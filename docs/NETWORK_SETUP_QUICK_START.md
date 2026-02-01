# Network-Aware Setup Quick Start

**Quick reference for using the network-aware dependency installation system**

## For Users

### First-Time Setup (Cloud VM)

```bash
# The SessionStart hook will run automatically in Claude Code
# It will:
# 1. Detect cloud environment
# 2. Install OTP 28 from GitHub (or cache)
# 3. Install rebar3
# 4. Fetch hex.pm dependencies

# Manual trigger (if needed):
bash .claude/hooks/SessionStart.sh
```

**Expected Time**:
- First install: ~3 minutes (downloading OTP 28)
- Subsequent installs: ~5 seconds (using cache)

### Check Environment

```bash
# Full environment report
./.claude/scripts/detect_environment.sh report

# Quick checks
./.claude/scripts/detect_environment.sh network    # cloud/local/offline
./.claude/scripts/detect_environment.sh erlang     # Check OTP version
```

### Manage Cache

```bash
# View cache statistics
./.claude/scripts/cache_manager.sh stats

# Clean old files (30+ days)
./.claude/scripts/cache_manager.sh clean 30

# Clear all cache
./.claude/scripts/cache_manager.sh clear
```

### Verify Network Compliance

```bash
# Check all URLs use allowlisted domains
./scripts/check_network_compliance.sh
```

## For Developers

### Adding a New Dependency

1. **Add to rebar.config**:
```erlang
{deps, [
    {new_package, "1.0.0"}  % From hex.pm - OK
]}.
```

2. **Verify network compliance**:
```bash
./scripts/check_network_compliance.sh
```

3. **Test in restricted network**:
```bash
docker build -f docker/test-cloud-vm.Dockerfile -t test .
docker run -it test
```

### Allowlisted Domains

**Current allowlist** (see `scripts/check_network_compliance.sh`):
- github.com
- raw.githubusercontent.com
- api.github.com
- hex.pm
- repo.hex.pm
- builds.hex.pm
- cdn.jsdelivr.net

**To add a new domain**:
1. Verify necessity and trustworthiness
2. Update `ALLOWLIST` in `scripts/check_network_compliance.sh`
3. Document in `NETWORK_AWARE_SETUP_DESIGN.md`
4. Add CI check in `.github/workflows/network-compliance.yml`

### Creating Pre-built Artifacts

**For OTP**:
```bash
# Manual build
./scripts/build_otp_artifact.sh 28.0

# Upload to GitHub Release
gh release create OTP-28.0 /tmp/otp-28.0-Linux-x86_64.tar.gz
```

**For Dependencies**:
```bash
# Create dependency cache
./.claude/scripts/cache_manager.sh cache-deps 2.1.0

# Upload to GitHub Release
gh release upload v2.1.0 ~/.erlmcp/cache/deps-2.1.0.tar.gz
```

## Troubleshooting

### Problem: SessionStart fails with "OTP download failed"

**Diagnosis**:
```bash
# Check network
curl -I https://github.com

# Check logs
tail -50 ~/.erlmcp/logs/session-*.log
```

**Solution**:
```bash
# Clear corrupted cache
./.claude/scripts/cache_manager.sh clear

# Re-run setup
bash .claude/hooks/SessionStart.sh
```

### Problem: `erl` command not found after install

**Diagnosis**:
```bash
# Check installation
ls ~/.erlmcp/otp/28.0/bin/erl

# Check PATH
echo $PATH
```

**Solution**:
```bash
# Add to PATH
export PATH="$HOME/.erlmcp/otp/28.0/bin:$PATH"

# Make permanent (add to ~/.bashrc)
echo 'export PATH="$HOME/.erlmcp/otp/28.0/bin:$PATH"' >> ~/.bashrc
```

### Problem: Dependency fetch fails offline

**Solution**:
```bash
# Use locked dependencies only
rebar3 get-deps --only locked

# Or restore from cache
tar -xzf ~/.erlmcp/cache/deps-2.1.0.tar.gz -C ~
```

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│ SessionStart Hook                                           │
│  ├─ Detect Environment (cloud/local/offline)                │
│  ├─ Install OTP 28                                          │
│  │   ├─ Tier 1: Already installed                          │
│  │   ├─ Tier 2: Restore from cache (~5s)                   │
│  │   ├─ Tier 3: Download from GitHub (~25s)                │
│  │   └─ Tier 4: Build from source (~10min, fallback)       │
│  ├─ Install rebar3                                          │
│  └─ Install hex dependencies                                │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Cache Manager                                               │
│  ├─ ~/.erlmcp/cache/                                        │
│  │   ├─ otp-28.0.tar.gz (150 MB)                           │
│  │   ├─ rebar3-3.25.0 (1 MB)                               │
│  │   ├─ deps-2.1.0.tar.gz (50 MB)                          │
│  │   └─ checksums.txt                                      │
│  └─ Operations: stats, clean, clear, verify                │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ Network Compliance Checker                                  │
│  ├─ Scan rebar.config for git dependencies                 │
│  ├─ Scan scripts for hardcoded URLs                        │
│  ├─ Verify all URLs use allowlisted domains                │
│  └─ Block commits with violations (pre-commit hook)        │
└─────────────────────────────────────────────────────────────┘
```

## Key Files

| File | Purpose |
|------|---------|
| `NETWORK_AWARE_SETUP_DESIGN.md` | Full design specification (682 lines) |
| `.claude/scripts/install_otp.sh` | OTP installation with fallback |
| `.claude/scripts/cache_manager.sh` | Cache management utilities |
| `.claude/scripts/detect_environment.sh` | Environment detection |
| `scripts/check_network_compliance.sh` | Network compliance checker |

## Performance Metrics

| Scenario | Time | Network | Cache Hit |
|----------|------|---------|-----------|
| First install (cloud, no cache) | 3m 12s | GitHub | 0% |
| Second install (cloud, cached) | 4.2s | None | 100% |
| Local install (asdf) | 2m 45s | asdf CDN | 0% |
| Offline install (cached) | 3.8s | None | 100% |

## Next Steps

1. **For first-time users**: Run `bash .claude/hooks/SessionStart.sh`
2. **For developers**: Review `NETWORK_AWARE_SETUP_DESIGN.md`
3. **For CI/CD**: Add network compliance check to workflows
4. **For offline users**: Download offline package from GitHub Releases

## References

- Full Design: `/home/user/erlmcp/NETWORK_AWARE_SETUP_DESIGN.md`
- Environment Detection: `.claude/scripts/detect_environment.sh`
- Cache Management: `.claude/scripts/cache_manager.sh`
- Network Compliance: `scripts/check_network_compliance.sh`

---

**Last Updated**: 2026-02-01
**Version**: 1.0.0
