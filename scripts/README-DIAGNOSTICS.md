# Package Manager Diagnostic Scripts

## Overview

This directory contains diagnostic and troubleshooting scripts for rebar3/hex package manager issues.

## Scripts

### Master Diagnostic

```bash
./diagnose-package-issues.sh [--quick|--full|--fix]
```

Comprehensive diagnostic tool that runs all tests and provides actionable recommendations.

### Individual Diagnostic Tools

| Script | Purpose | Usage |
|--------|---------|-------|
| `hex-mirror-alternatives.sh` | Test alternative hex mirrors | `./hex-mirror-alternatives.sh [package] [version]` |
| `git-fallback-test.sh` | Test git-based dependencies | `./git-fallback-test.sh [--dry-run]` |
| `manual-download.sh` | Manual package download | `./manual-download.sh [--package PKG --version VER]` |
| `offline-package-test.sh` | Offline installation | `./offline-package-test.sh [--create-repo\|--test-offline]` |
| `kerl-otp-manager.sh` | OTP version management | `./kerl-otp-manager.sh [--install VER]` |

## Quick Start

### Problem: Cannot download hex packages

```bash
# Step 1: Diagnose
./diagnose-package-issues.sh --quick

# Step 2: Try automatic fix
./diagnose-package-issues.sh --fix
```

### Problem: Need offline build

```bash
# Create offline repository
./offline-package-test.sh --create-repo

# Test offline build
./offline-package-test.sh --test-offline
```

### Problem: Slow downloads

```bash
# Find fastest mirror
./hex-mirror-alternatives.sh

# Set environment variable for fastest mirror
export HEX_MIRROR=https://mirrors.aliyun.com/hex
```

## Features

### 1. Hex Mirror Testing
- Tests 4+ alternative mirrors
- Measures connection speed
- Verifies API availability
- Downloads test packages
- Generates optimized config

### 2. Git Fallback
- Converts hex deps to git repos
- Maps 15+ popular packages
- Tests git accessibility
- Generates hybrid configs
- Dry-run mode available

### 3. Manual Download
- Direct hex.pm downloads
- SHA256 checksum verification
- Automatic cache installation
- Batch processing
- Metadata inspection

### 4. Offline Mode
- Local repository creation
- Package bundling
- Portable tarballs
- Index generation
- Offline build testing

### 5. OTP Management
- Kerl installation
- Multiple OTP versions
- Source builds
- Version activation
- Compatibility testing

## Output Files

### Generated Configurations

| File | Description |
|------|-------------|
| `rebar.config.mirrors` | Alternative hex mirrors |
| `rebar.config.git` | Full git dependencies |
| `rebar.config.mixed` | Hybrid hex+git |
| `hex-packages-bundle-*.tar.gz` | Offline package bundle |
| `activate-otp-*.sh` | OTP activation script |

### Log Directories

| Directory | Contents |
|-----------|----------|
| `log/diagnostics/` | Diagnostic reports |
| `log/hex-mirror-test/` | Mirror test results |
| `log/git-fallback-test/` | Git test logs |
| `log/manual-download/` | Download logs |
| `log/offline-test/` | Offline build logs |
| `log/kerl-manager/` | Kerl management logs |

## Common Workflows

### Development Setup

```bash
# First-time setup with diagnostics
./diagnose-package-issues.sh --full

# If issues found, auto-fix
./diagnose-package-issues.sh --fix

# Verify
TERM=dumb rebar3 compile
```

### CI/CD Setup

```bash
# Create package cache (run once)
./offline-package-test.sh --create-repo
./offline-package-test.sh --bundle

# In CI pipeline, extract bundle
tar -xzf hex-packages-bundle.tar.gz -C ~/.cache/rebar3/hex/default/

# Build with offline cache
HEX_OFFLINE=true TERM=dumb rebar3 compile
```

### Restricted Network

```bash
# Test mirrors
./hex-mirror-alternatives.sh

# If mirrors fail, use git
./git-fallback-test.sh
cp rebar.config.git rebar.config

# If git fails, manual download
./manual-download.sh
```

## Environment Variables

```bash
# Use alternative mirror
export HEX_MIRROR=https://mirrors.aliyun.com/hex

# Enable offline mode
export HEX_OFFLINE=true

# Custom cache directory
export HEX_HOME=~/.cache/rebar3/hex/default
export REBAR_CACHE_DIR=~/.cache/rebar3

# Proxy settings
export HTTP_PROXY=http://proxy:8080
export HTTPS_PROXY=https://proxy:8080
```

## Diagnostic Report Example

```
=========================================
DIAGNOSTIC SUMMARY
=========================================

System Information:
  OS: Linux 4.4.0
  Date: 2026-02-01
  User: user
  Erlang: Erlang/OTP 28 [erts-15.3.1]
  Rebar3: rebar 3.24.0

Test Results:
  Total checks: 15
  ✓ Passed: 12
  ✗ Failed: 3

=========================================
RECOMMENDATIONS
=========================================

Issue: Compilation failures detected

Recommended actions:
  1. Try alternative mirrors:
     ./scripts/hex-mirror-alternatives.sh

  2. Use git-based dependencies:
     ./scripts/git-fallback-test.sh
     cp rebar.config.git rebar.config

  3. Manual package download:
     ./scripts/manual-download.sh
```

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Failure or issues detected |

## Dependencies

### Required
- bash (4.0+)
- curl
- tar
- grep/sed/awk

### Optional
- git (for git-fallback-test.sh)
- sha256sum or shasum (for checksum verification)
- rebar3 (for testing)
- erl (for OTP testing)

## Safety

All scripts are:
- Idempotent (safe to run multiple times)
- Non-destructive (create backups)
- Dry-run capable (where applicable)
- Verbose with logging

Backups created in:
- `.rebar-backup/` - rebar.config backups
- `.hex-mirror-cache/` - Test downloads
- `.offline-hex-repo/` - Local repository

## Performance

| Script | Duration | Network | Disk |
|--------|----------|---------|------|
| diagnose-package-issues.sh --quick | 30s | Yes | 1MB |
| diagnose-package-issues.sh --full | 5min | Yes | 10MB |
| hex-mirror-alternatives.sh | 1min | Yes | 5MB |
| git-fallback-test.sh --dry-run | 30s | Yes | <1MB |
| git-fallback-test.sh | 10min | Yes | 50MB |
| manual-download.sh | 5min | Yes | 20MB |
| offline-package-test.sh | 3min | Yes | 30MB |
| kerl-otp-manager.sh --install | 20min | Yes | 500MB |

## Documentation

See `docs/PACKAGE_MANAGER_DIAGNOSTICS.md` for comprehensive guide.

## Examples

### Example 1: Fresh install on slow network

```bash
$ ./diagnose-package-issues.sh --full
[...diagnostics...]

Recommendation: Try alternative mirrors

$ ./hex-mirror-alternatives.sh
Testing mirrors...
aliyun: 89ms (fastest)
hex.pm: 245ms

$ export HEX_MIRROR=https://mirrors.aliyun.com/hex
$ TERM=dumb rebar3 compile
✓ Success (3x faster)
```

### Example 2: Offline build for airgap environment

```bash
# On internet machine:
$ ./offline-package-test.sh --create-repo
✓ Created repository with 23 packages

$ ./offline-package-test.sh --bundle
✓ Bundle created: hex-packages-bundle-20260201.tar.gz (15MB)

# Transfer to airgap machine:
$ scp hex-packages-bundle-20260201.tar.gz airgap:~/

# On airgap machine:
$ tar -xzf hex-packages-bundle-20260201.tar.gz -C ~/.cache/rebar3/hex/default/
$ HEX_OFFLINE=true TERM=dumb rebar3 compile
✓ Offline build successful
```

### Example 3: Multiple OTP versions

```bash
$ ./kerl-otp-manager.sh --list
Available releases:
[...list...]

$ ./kerl-otp-manager.sh --install 28.3.1
Building OTP 28.3.1...
[...20 minutes...]
✓ Installation completed

$ source ~/.kerl/installations/28.3.1/activate
Activated OTP 28.3.1

$ ./kerl-otp-manager.sh --test 28.3.1
✓ rebar3 compile successful with OTP 28.3.1
```

## Troubleshooting

### Script won't run

```bash
# Make executable
chmod +x ./scripts/*.sh

# Check bash version
bash --version  # Need 4.0+
```

### Curl SSL errors

```bash
# Use insecure mode (testing only)
curl -k https://hex.pm

# Or install certificates
# Ubuntu/Debian: apt-get install ca-certificates
# macOS: brew install curl
```

### Permission denied

```bash
# Scripts write to:
mkdir -p log/{diagnostics,hex-mirror-test,git-fallback-test,manual-download,offline-test,kerl-manager}
chmod 755 log/
```

## Contributing

When adding new diagnostic scripts:

1. Follow naming convention: `lowercase-with-dashes.sh`
2. Include help text and usage
3. Add to this README
4. Update PACKAGE_MANAGER_DIAGNOSTICS.md
5. Use color codes consistently
6. Log to `log/{script-name}/`
7. Create backups before modifications

## License

Part of erlmcp project. See LICENSE.

---

For detailed documentation, see `docs/PACKAGE_MANAGER_DIAGNOSTICS.md`
