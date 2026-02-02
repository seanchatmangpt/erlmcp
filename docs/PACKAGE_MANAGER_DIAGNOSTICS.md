# Package Manager Diagnostics Guide

## Overview

This guide covers diagnostic tools for debugging rebar3/hex package download issues in the erlmcp project.

## Quick Start

```bash
# Run quick diagnostic
./scripts/diagnose-package-issues.sh --quick

# Run full diagnostic with all tests
./scripts/diagnose-package-issues.sh --full

# Attempt automatic fixes
./scripts/diagnose-package-issues.sh --fix
```

## Diagnostic Scripts

### 1. Master Diagnostic Tool

**Script:** `diagnose-package-issues.sh`

**Purpose:** Comprehensive diagnostic that runs all tests and provides recommendations.

**Usage:**
```bash
./scripts/diagnose-package-issues.sh [--quick|--full|--fix]
```

**Modes:**
- `--quick`: Fast diagnostic (baseline + network + cache)
- `--full`: Complete diagnostic (all tests)
- `--fix`: Automatic repair attempt

**Output:** Detailed report saved to `log/diagnostics/diagnostic-report-YYYYMMDD_HHMMSS.txt`

### 2. Hex Mirror Alternatives

**Script:** `hex-mirror-alternatives.sh`

**Purpose:** Test multiple hex.pm mirrors for availability and speed.

**Mirrors tested:**
- Primary: https://hex.pm
- Aliyun: https://mirrors.aliyun.com/hex/
- Fly CDN: https://hexpm-cdn.fly.dev
- Erlang Package: https://hex-cdn.erlang-package.com

**Usage:**
```bash
# Test all mirrors
./scripts/hex-mirror-alternatives.sh

# Test specific package
./scripts/hex-mirror-alternatives.sh jsx 3.1.0
```

**Output:**
- Connection speed tests
- API availability checks
- Package download verification
- Generated config: `rebar.config.mirrors`

**Example output:**
```
Mirror               Connectivity    API        Download
------               ------------    ---        --------
hex.pm              245ms           OK         OK (152KB in 1.2s)
aliyun              89ms            OK         OK (152KB in 0.3s)
fly-cdn             312ms           FAILED     SKIPPED
erlang-pkg          FAILED          SKIPPED    SKIPPED
```

### 3. Git Fallback Test

**Script:** `git-fallback-test.sh`

**Purpose:** Convert hex dependencies to git repositories as fallback.

**Usage:**
```bash
# Dry run (no cloning)
./scripts/git-fallback-test.sh --dry-run

# Full test with cloning
./scripts/git-fallback-test.sh
```

**Features:**
- Maps hex packages to GitHub/GitLab repos
- Tests git repository accessibility
- Verifies package versions/tags
- Generates git-based rebar.config
- Creates mixed hex+git configuration

**Generated configs:**
- `rebar.config.git` - Full git mode
- `rebar.config.mixed` - Hybrid hex+git

**Package mappings:**
| Hex Package | Git Repository |
|-------------|----------------|
| jsx | https://github.com/talentdeficit/jsx.git |
| jesse | https://github.com/for-GET/jesse.git |
| gproc | https://github.com/uwiger/gproc.git |
| gun | https://github.com/ninenines/gun.git |
| ranch | https://github.com/ninenines/ranch.git |
| cowboy | https://github.com/ninenines/cowboy.git |

### 4. Manual Package Download

**Script:** `manual-download.sh`

**Purpose:** Download packages directly from hex.pm and install to local cache.

**Usage:**
```bash
# Download all packages from rebar.lock
./scripts/manual-download.sh

# Download specific package
./scripts/manual-download.sh --package jsx --version 3.1.0
```

**Features:**
- Fetches package metadata from hex API
- Downloads tarballs from repo.hex.pm
- Verifies SHA256 checksums
- Extracts and inspects package contents
- Installs to hex cache (~/.cache/rebar3/hex/default)
- Tests rebar3 recognition

**Process:**
1. Fetch metadata: `GET https://hex.pm/api/packages/{package}`
2. Extract checksum from metadata
3. Download tarball: `GET https://repo.hex.pm/tarballs/{package}-{version}.tar`
4. Verify checksum: `sha256sum`
5. Install to cache: `~/.cache/rebar3/hex/default/tarballs/`

### 5. Offline Package Test

**Script:** `offline-package-test.sh`

**Purpose:** Create local hex repository for offline installation.

**Usage:**
```bash
# Create offline repository
./scripts/offline-package-test.sh --create-repo

# Test offline build
./scripts/offline-package-test.sh --test-offline

# Show repository status
./scripts/offline-package-test.sh --status

# Create portable bundle
./scripts/offline-package-test.sh --bundle
```

**Features:**
- Creates local hex repository structure
- Copies packages from hex cache
- Downloads missing packages
- Generates repository index
- Configures rebar3 for offline mode
- Creates portable tarball bundle

**Directory structure:**
```
.offline-hex-repo/
├── packages/          # Package index
├── tarballs/          # Package tarballs
└── packages.idx       # Repository index
```

**Offline configuration:**
```erlang
% ~/.config/rebar3/hex.config
{offline, true}.
{repos, [
    #{name => <<"hexpm">>,
      url => <<"file:///path/to/.offline-hex-repo">>,
      public_key => undefined}
]}.
```

### 6. Kerl OTP Manager

**Script:** `kerl-otp-manager.sh`

**Purpose:** Alternative OTP installation and management using Kerl.

**Usage:**
```bash
# Install kerl
./scripts/kerl-otp-manager.sh --status

# List available OTP releases
./scripts/kerl-otp-manager.sh --list

# Install OTP version
./scripts/kerl-otp-manager.sh --install 28.3.1

# Activate OTP version
./scripts/kerl-otp-manager.sh --activate 28.3.1

# Test with rebar3
./scripts/kerl-otp-manager.sh --test 28.3.1

# Compare installations
./scripts/kerl-otp-manager.sh --compare

# Uninstall
./scripts/kerl-otp-manager.sh --uninstall 28.3.1
```

**Features:**
- Downloads and installs kerl
- Builds OTP from source
- Manages multiple OTP versions
- Creates activation scripts
- Tests compatibility with rebar3
- Compares kerl vs system OTP

**Kerl configuration:**
```bash
# ~/.kerlrc
KERL_CONFIGURE_OPTIONS="--enable-hipe --enable-smp-support --enable-threads"
KERL_BUILD_BACKEND=git
KERL_BUILD_DOCS=yes
MAKEFLAGS="-j$(nproc)"
```

**Activation:**
```bash
# Method 1: Direct activation
source ~/.kerl/installations/28.3.1/activate

# Method 2: Convenience script
source ./activate-otp-28.3.1.sh
```

## Common Issues and Solutions

### Issue 1: Hex download timeout

**Symptoms:**
```
===> Verifying dependencies...
===> Fetching jsx (3.1.0)
===> Failed to fetch package jsx 3.1.0
```

**Solutions:**

1. **Try alternative mirrors:**
   ```bash
   ./scripts/hex-mirror-alternatives.sh
   export HEX_MIRROR=https://mirrors.aliyun.com/hex
   ```

2. **Use git dependencies:**
   ```bash
   ./scripts/git-fallback-test.sh
   cp rebar.config.git rebar.config
   rm -rf _build
   rebar3 compile
   ```

3. **Manual download:**
   ```bash
   ./scripts/manual-download.sh
   ```

### Issue 2: Network connectivity problems

**Symptoms:**
```
===> Failed to connect to hex.pm
Connection refused
```

**Solutions:**

1. **Check proxy settings:**
   ```bash
   echo $HTTP_PROXY $HTTPS_PROXY
   export HTTP_PROXY=http://proxy:8080
   export HTTPS_PROXY=http://proxy:8080
   ```

2. **Use offline mode:**
   ```bash
   ./scripts/offline-package-test.sh --create-repo
   ./scripts/offline-package-test.sh --test-offline
   ```

3. **Create portable bundle:**
   ```bash
   # On machine with internet
   ./scripts/offline-package-test.sh --bundle
   
   # Transfer hex-packages-bundle-YYYYMMDD.tar.gz to offline machine
   tar -xzf hex-packages-bundle-YYYYMMDD.tar.gz -C ~/.cache/rebar3/hex/default/
   ```

### Issue 3: Package checksum mismatch

**Symptoms:**
```
===> Package jsx-3.1.0 checksum mismatch
Expected: ABC123...
Actual: DEF456...
```

**Solutions:**

1. **Clear hex cache:**
   ```bash
   rm -rf ~/.cache/rebar3/hex
   rebar3 get-deps
   ```

2. **Download fresh copy:**
   ```bash
   ./scripts/manual-download.sh --package jsx --version 3.1.0
   ```

3. **Use git version:**
   ```bash
   ./scripts/git-fallback-test.sh
   ```

### Issue 4: OTP version incompatibility

**Symptoms:**
```
===> OTP version 27 required, found 26
```

**Solutions:**

1. **Install OTP 28 with kerl:**
   ```bash
   ./scripts/kerl-otp-manager.sh --install 28.3.1
   source ~/.kerl/installations/28.3.1/activate
   ```

2. **Verify installation:**
   ```bash
   erl -version
   ./scripts/kerl-otp-manager.sh --test 28.3.1
   ```

## Workflow Examples

### Scenario 1: First-time setup on restricted network

```bash
# Step 1: Diagnose
./scripts/diagnose-package-issues.sh --full

# Step 2: Try mirrors
./scripts/hex-mirror-alternatives.sh

# Step 3: If mirrors fail, use git
./scripts/git-fallback-test.sh
cp rebar.config.git rebar.config

# Step 4: If git fails, manual download
./scripts/manual-download.sh

# Step 5: Verify
TERM=dumb rebar3 compile
```

### Scenario 2: Offline build preparation

```bash
# On internet-connected machine:
./scripts/offline-package-test.sh --create-repo
./scripts/offline-package-test.sh --bundle

# Transfer hex-packages-bundle-YYYYMMDD.tar.gz

# On offline machine:
tar -xzf hex-packages-bundle-YYYYMMDD.tar.gz -C ~/.cache/rebar3/hex/default/
./scripts/offline-package-test.sh --test-offline
```

### Scenario 3: CI/CD optimization

```bash
# Cache packages for CI
./scripts/offline-package-test.sh --create-repo
./scripts/offline-package-test.sh --bundle

# In CI script:
- name: Extract package cache
  run: tar -xzf hex-packages-bundle.tar.gz -C ~/.cache/rebar3/hex/default/

- name: Build
  run: TERM=dumb rebar3 compile
  env:
    HEX_OFFLINE: true
```

## Performance Comparison

| Method | First Build | Rebuild | Network | Reliability |
|--------|-------------|---------|---------|-------------|
| Hex (default) | 2-3 min | 30s | Required | Medium |
| Git deps | 3-5 min | 45s | Required | High |
| Offline cache | 1-2 min | 30s | Not required | Very High |
| Manual download | 5-10 min | 30s | Initial only | High |

## Directory Structure

```
scripts/
├── diagnose-package-issues.sh      # Master diagnostic
├── hex-mirror-alternatives.sh      # Mirror testing
├── git-fallback-test.sh            # Git fallback
├── manual-download.sh              # Manual download
├── offline-package-test.sh         # Offline mode
└── kerl-otp-manager.sh             # OTP management

log/
├── diagnostics/                     # Diagnostic reports
├── hex-mirror-test/                 # Mirror test results
├── git-fallback-test/               # Git test logs
├── manual-download/                 # Download logs
├── offline-test/                    # Offline test logs
└── kerl-manager/                    # Kerl logs

.offline-hex-repo/                   # Local repository
├── packages/
├── tarballs/
└── packages.idx

.kerl/                               # Kerl installations
├── builds/
├── installations/
└── kerl
```

## Environment Variables

| Variable | Purpose | Example |
|----------|---------|---------|
| `HEX_MIRROR` | Alternative hex mirror | `https://mirrors.aliyun.com/hex` |
| `HEX_OFFLINE` | Enable offline mode | `true` |
| `HEX_HOME` | Hex cache directory | `~/.cache/rebar3/hex/default` |
| `REBAR_CACHE_DIR` | Rebar3 cache | `~/.cache/rebar3` |
| `HTTP_PROXY` | HTTP proxy | `http://proxy:8080` |
| `HTTPS_PROXY` | HTTPS proxy | `https://proxy:8080` |

## Troubleshooting Checklist

- [ ] Run diagnostic: `./scripts/diagnose-package-issues.sh --full`
- [ ] Check network: `curl -I https://hex.pm`
- [ ] Check hex cache: `ls ~/.cache/rebar3/hex/default/tarballs/`
- [ ] Check OTP version: `erl -version`
- [ ] Check rebar3 version: `rebar3 version`
- [ ] Try alternative mirror: `./scripts/hex-mirror-alternatives.sh`
- [ ] Try git fallback: `./scripts/git-fallback-test.sh`
- [ ] Try manual download: `./scripts/manual-download.sh`
- [ ] Check logs: `ls log/diagnostics/`

## Support

For additional help:

1. Review diagnostic report in `log/diagnostics/`
2. Check individual script logs
3. Run `./scripts/diagnose-package-issues.sh --fix` for automatic repair
4. Consult erlmcp documentation: `docs/`

## References

- Hex.pm API: https://hex.pm/docs/api
- Kerl: https://github.com/kerl/kerl
- Rebar3: https://rebar3.org/docs/
- Erlang/OTP: https://erlang.org/doc/

---

Generated by erlmcp package manager diagnostic tools.
