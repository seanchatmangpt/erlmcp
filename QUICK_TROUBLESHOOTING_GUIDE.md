# erlmcp rebar3/hex Quick Troubleshooting Guide

## Problem Diagnosis Flowchart

```
Does 'rebar3 version' work?
├─ NO → Issue: OTP not installed
│       └─ Solution: Run SessionStart.sh OR rebuild OTP
│
└─ YES → Does 'rebar3 get-deps' work?
         ├─ NO (hex.pm download fails) → Issue: hex.pm unreachable
         │       └─ Solution: Use git-fallback (RECOMMENDED)
         │       └─ OR: Use hex mirrors
         │       └─ OR: Manual downloads
         │
         └─ YES → Your build should work! Try:
                  TERM=dumb rebar3 compile
```

---

## Quick Fixes

### Fix 1: rebar3 not found

**Symptom**: `rebar3: command not found`

**Fix**:
```bash
source .erlmcp/env.sh
echo $PATH  # Should show .erlmcp/otp-28.3.1/bin
./rebar3 version
```

If still fails, OTP is broken:
```bash
rm -rf .erlmcp/otp-28.3.1 .erlmcp/env.sh .erlmcp/cache/sessionstart.lock
./.claude/hooks/SessionStart.sh
```

### Fix 2: hex.pm download fails

**Symptom**: `Package not found in any repo: [PACKAGE]`

**Fix 1 - Use Git Dependencies** (RECOMMENDED):
```bash
# Backup
cp rebar.config rebar.config.hex.bak

# Switch to git
./scripts/git-fallback-test.sh
cp rebar.config.git rebar.config
rm -rf _build

# Test
./rebar3 get-deps
./rebar3 compile
```

**Fix 2 - Try Mirrors**:
```bash
./scripts/hex-mirror-alternatives.sh
# Use the fastest mirror reported
export HEX_MIRROR=https://mirrors.aliyun.com/hex
./rebar3 get-deps
```

**Fix 3 - Diagnose Network Issues**:
```bash
# Run full diagnostic
./scripts/diagnose-package-issues.sh --full

# Check connectivity
curl -v https://repo.hex.pm

# Check DNS
nslookup hex.pm
```

### Fix 3: Compilation fails

**Symptom**: `rebar3 compile` fails with various errors

**Steps**:
```bash
# 1. Verify environment
source .erlmcp/env.sh
echo $ERLMCP_OTP_BIN_PATH

# 2. Clear build cache
rm -rf _build

# 3. Check dependencies
./rebar3 get-deps

# 4. Compile with verbose output
DEBUG=1 ./rebar3 compile

# 5. If still fails, check OTP
erl -version
erlc --version
```

### Fix 4: Proxy Issues

**Symptom**: Network timeouts, SSL certificate errors

**Solution**:
```bash
# Check proxy settings
env | grep -i proxy

# Try alternative mirror
export HEX_MIRROR=https://mirrors.aliyun.com/hex

# Or use git fallback
cp rebar.config.git rebar.config
./rebar3 get-deps
```

---

## Tool Quick Reference

### Master Diagnostic

```bash
# Quick diagnostic (30s)
./scripts/diagnose-package-issues.sh --quick

# Full diagnostic (5 min)
./scripts/diagnose-package-issues.sh --full

# Auto-fix
./scripts/diagnose-package-issues.sh --fix
```

### Git Fallback

```bash
# Test git fallback (no cloning)
./scripts/git-fallback-test.sh --dry-run

# Generate git-based rebar.config
./scripts/git-fallback-test.sh
cp rebar.config.git rebar.config
```

### Hex Mirrors

```bash
# Test all mirrors
./scripts/hex-mirror-alternatives.sh

# Test specific mirror
./scripts/hex-mirror-alternatives.sh jsx 3.1.0
```

### Manual Download

```bash
# Download specific package
./scripts/manual-download.sh jsx 3.1.0

# Download all packages
./scripts/manual-download.sh --all
```

### Offline Bundle

```bash
# Create offline repository
./scripts/offline-package-test.sh --create-repo

# Create portable bundle
./scripts/offline-package-test.sh --bundle
```

### Alternative OTP

```bash
# List available versions
./scripts/kerl-otp-manager.sh --list

# Install version
./scripts/kerl-otp-manager.sh --install 28.3.1

# Activate
source ~/.kerl/installations/28.3.1/activate

# Test
./scripts/kerl-otp-manager.sh --test 28.3.1
```

---

## Verification Commands

```bash
# Check OTP installation
source .erlmcp/env.sh
erl -noshell -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().'
# Expected: 28

# Check rebar3
./rebar3 version
# Expected: rebar 3.26.0 on Erlang/OTP 28

# Check path
which erl erlc escript rebar3
# All should be found

# Test hex.pm access
curl -I https://repo.hex.pm
# Should get 200 OK

# Test git access
git ls-remote https://github.com/ninenines/gun.git
# Should show git refs

# Verify env file
cat .erlmcp/env.sh
# Should have /home/user/erlmcp paths
```

---

## Common Errors & Solutions

| Error | Cause | Solution |
|-------|-------|----------|
| `rebar3: command not found` | OTP not in PATH | `source .erlmcp/env.sh` |
| `Package not found: [PKG]` | hex.pm unreachable | Use `git-fallback` or mirrors |
| `SSL_ERROR` | Proxy/cert issue | Try git fallback or mirror |
| `Command failed: erl` | Broken OTP install | Rebuild OTP |
| `ENOTDIR: not a directory` | Symlinks broken | Remove `.erlmcp/otp-*` |
| `Permission denied: rebar3` | Not executable | `chmod +x rebar3` |
| `git clone timeout` | Network slow | Use git with `--depth 1` |

---

## When hex.pm Works vs Doesn't

### ✓ When hex.pm Works
- Fast network connection to hex.pm
- No proxy or compatible proxy
- Valid SSL certificates
- hex.pm servers operational

### ✗ When hex.pm Doesn't Work
- Network blocked hex.pm
- Proxy incompatible with HTTP client
- Corporate firewall
- Geographic blocking
- hex.pm maintenance/downtime

---

## Recommended Order of Attempts

### Scenario 1: Fresh setup

```bash
# 1. Set environment
source .erlmcp/env.sh

# 2. Verify OTP
erl -version

# 3. Install/verify rebar3
./rebar3 version

# 4. Try hex.pm
./rebar3 get-deps
# If works → Success!
# If fails → Go to Scenario 2
```

### Scenario 2: hex.pm failing

```bash
# 1. Try diagnostic
./scripts/diagnose-package-issues.sh --quick

# 2. Try git fallback (RECOMMENDED)
./scripts/git-fallback-test.sh
cp rebar.config.git rebar.config
./rebar3 get-deps
# If works → Continue with git!

# 3. If git fails, try mirrors
./scripts/hex-mirror-alternatives.sh
# Use fastest mirror
export HEX_MIRROR=https://[fastest]
./rebar3 get-deps

# 4. Last resort: manual download
./scripts/manual-download.sh --all
```

### Scenario 3: Everything broken

```bash
# 1. Run full diagnostic
./scripts/diagnose-package-issues.sh --full

# 2. Remove broken artifacts
rm -rf .erlmcp/otp-* .erlmcp/env.sh _build

# 3. Rebuild environment
./.claude/hooks/SessionStart.sh

# 4. Install rebar3
curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o rebar3
chmod +x rebar3

# 5. Try git fallback
./scripts/git-fallback-test.sh
cp rebar.config.git rebar.config
./rebar3 get-deps
```

---

## Environment Variables Reference

```bash
# Source this before building
source .erlmcp/env.sh

# Key variables set:
ERLMCP_OTP_VERSION="28.3.1"           # OTP version
ERLMCP_OTP_BIN_PATH="/home/user/..."  # OTP bin directory
ERLMCP_CACHE="/home/user/.erlmcp/cache/"  # Cache location
ERLMCP_PROFILE="cloud"                # Running in cloud
CLAUDE_CODE_REMOTE="true"             # Remote Claude Code
```

---

## Performance Tips

1. **Parallel Compilation**: Uses all CPU cores by default
2. **Caching**: rebar3 caches dependencies, don't delete
3. **Git Cloning**: Faster with `--depth 1` (shallow clone)
4. **Mirrors**: Use geographic-local mirror for speed
5. **Offline Mode**: Build offline bundles for CI/CD

---

## Debugging

### Enable Verbose Output

```bash
DEBUG=1 ./rebar3 compile
REBAR_LOG_LEVEL=debug ./rebar3 get-deps
VERBOSE=1 ./scripts/diagnose-package-issues.sh --full
```

### Check Logs

```bash
# See diagnostic logs
cat log/diagnostics/diagnostic-report-*.txt

# See rebar3 logs
cat _build/*/log/*.log

# See script logs
ls -la log/
```

### Network Debugging

```bash
# Test connectivity
ping -c 3 repo.hex.pm
curl -v https://repo.hex.pm

# Check DNS
nslookup hex.pm
dig hex.pm

# Check proxy
echo $HTTP_PROXY
echo $HTTPS_PROXY
```

---

## When to Use Each Solution

| Solution | When | Why |
|----------|------|-----|
| **hex.pm** | hex.pm accessible | Primary, standard approach |
| **git-fallback** | hex.pm blocked | All packages on GitHub |
| **mirrors** | hex.pm slow | Faster CDN alternatives |
| **manual-download** | Network issues | Full control, offline capable |
| **offline-bundle** | CI/CD, repeatable | Cache dependencies |
| **kerl** | Multiple versions | Alternative OTP management |

---

## Getting Help

### Check Diagnostic Output

```bash
./scripts/diagnose-package-issues.sh --full
# Generates: log/diagnostics/diagnostic-report-YYYYMMDD_HHMMSS.txt
```

### Read Documentation

- Full guide: `docs/PACKAGE_MANAGER_DIAGNOSTICS.md`
- Root cause: `REBAR3_HEX_FAILURE_ROOT_CAUSE_AND_SOLUTIONS.md`
- Script ref: `scripts/README-DIAGNOSTICS.md`
- Quick ref: `scripts/QUICK_REFERENCE.txt`

### Manual Investigation

```bash
# What packages are needed?
grep "^\s*{" rebar.config | grep -v "^\s*{"

# What's in cache?
ls -la ~/.cache/rebar3/hex/default/

# What's in build?
ls -la _build/default/lib/

# Check git repo
git remote -v
```

---

## Success Indicators

✓ All of these should work:
```bash
source .erlmcp/env.sh          # Environment loads
erl -version                   # OTP works
./rebar3 version               # rebar3 works
./rebar3 get-deps              # Dependencies fetch
TERM=dumb ./rebar3 compile     # Build compiles
make check                      # All tests pass
```

---

**Last Updated**: 2026-02-02
**Status**: Production Ready
