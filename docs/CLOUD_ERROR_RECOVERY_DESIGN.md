# Cloud Error Recovery System Design

**Version**: 1.0.0
**Status**: Design
**Target**: erlmcp v2.1.0+
**Author**: Erlang Test Engineer
**Date**: 2026-02-01

---

## Executive Summary

This document specifies a **hook-based error recovery system** optimized for cloud VMs where manual intervention is expensive. The system automatically fixes common infrastructure and environment failures without breaking OTP isolation guarantees.

**Mission**: Reduce cloud VM downtime from hours to seconds by automating recovery from 25+ common failure modes.

**Key Metrics**:
- Target recovery rate: 90% of common failures
- Recovery time: <2 minutes per failure
- Zero manual intervention for P0/P1 failures
- 100% idempotent operations
- Full audit trail for compliance

---

## Table of Contents

1. [Architecture](#architecture)
2. [Error Classification System](#error-classification-system)
3. [Recovery Hooks by Error Type](#recovery-hooks-by-error-type)
4. [Idempotent Recovery Scripts](#idempotent-recovery-scripts)
5. [Integration with Hooks](#integration-with-hooks)
6. [Logging & Auditability](#logging--auditability)
7. [Network-Aware Recovery](#network-aware-recovery)
8. [State Management](#state-management)
9. [Implementation Guide](#implementation-guide)
10. [Testing Strategy](#testing-strategy)
11. [Cloud Simulation Environment](#cloud-simulation-environment)

---

## Architecture

### System Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Cloud Error Recovery                      │
│                         (CERF)                               │
└─────────────────────────────────────────────────────────────┘
                              │
            ┌─────────────────┴─────────────────┐
            │                                   │
  ┌─────────▼──────────┐            ┌──────────▼─────────┐
  │  Error Classifier  │            │  Recovery Engine   │
  │  (Parser & Triage) │            │  (Hook Executor)   │
  └─────────┬──────────┘            └──────────┬─────────┘
            │                                   │
            │ 25+ Error Patterns                │ Idempotent Hooks
            │                                   │
  ┌─────────▼──────────────────────────────────▼─────────┐
  │              Recovery Hook Registry                   │
  │  {OTP_VERSION → hook1, DISK_SPACE → hook2, ...}      │
  └─────────┬──────────────────────────────────┬─────────┘
            │                                   │
  ┌─────────▼─────────┐            ┌──────────▼─────────┐
  │  State Manager    │            │  Audit Logger      │
  │  (Retry Control)  │            │  (Evidence Chain)  │
  └───────────────────┘            └────────────────────┘
```

### Design Principles

1. **Idempotency First**: Every recovery operation must be safe to run multiple times
2. **Isolation Preservation**: No cross-process state mutation (OTP supervision intact)
3. **Fail-Fast with Evidence**: If recovery fails, collect evidence and escalate immediately
4. **Network Awareness**: Respect allowlisted domains, handle timeouts gracefully
5. **State Preservation**: Cache artifacts, preserve build state across retries
6. **Audit Everything**: Every recovery attempt logged with timestamp, error, action, result

### Integration with Existing erlmcp Systems

```
Existing Auto-Fix System (Code-Level)
  ├── syntax-fix-agent.sh       ─┐
  ├── type-fix-agent.sh          ├─► Code errors (compilation, types, tests)
  ├── test-fix-agent.sh         ─┘
  └── gate-failure-dispatcher.sh

New Cloud Recovery System (Infrastructure-Level)
  ├── error-classifier.sh        ─┐
  ├── recovery-engine.sh          ├─► Infrastructure errors (OTP, deps, network, disk)
  ├── recovery-hooks/             │
  │   ├── otp-version.sh          │
  │   ├── missing-deps.sh         │
  │   ├── network-error.sh        │
  │   ├── disk-space.sh           │
  │   └── ...                    ─┘
  └── cloud-dispatcher.sh

Coordinated by:
  └── tools/auto-fix/orchestrator.sh (enhanced)
```

---

## Error Classification System

### Error Taxonomy (25 Categories)

#### Priority 0: Blocking (No compilation possible)

| Error Code | Pattern | Severity | Recovery Time | Retry |
|------------|---------|----------|---------------|-------|
| `OTP_VERSION_LOW` | `minimum_otp_vsn.*required: 28` | P0 | 5 min | Yes |
| `MISSING_REBAR` | `command not found: rebar3` | P0 | 2 min | Yes |
| `DISK_FULL` | `No space left on device` | P0 | 3 min | Yes |
| `MEMORY_EXHAUSTED` | `Cannot allocate memory` | P0 | 1 min | Yes |
| `DISK_INODE_EXHAUSTED` | `No space.*inodes` | P0 | 3 min | Yes |

#### Priority 1: Critical (Build fails)

| Error Code | Pattern | Severity | Recovery Time | Retry |
|------------|---------|----------|---------------|-------|
| `MISSING_DEPS` | `{error.*{not_found.*jsx}}` | P1 | 3 min | Yes |
| `COMPILE_ERROR_SYNTAX` | `syntax error before:` | P1 | 1 min | Yes |
| `COMPILE_ERROR_MISSING_MODULE` | `undefined module.*` | P1 | 2 min | Yes |
| `COMPILE_ERROR_BEAM` | `beam_lib.*error` | P1 | 2 min | Yes |
| `PLT_OUTDATED` | `dialyzer.*rebuild.*plt` | P1 | 5 min | Yes |

#### Priority 2: Important (Tests fail)

| Error Code | Pattern | Severity | Recovery Time | Retry |
|------------|---------|----------|---------------|-------|
| `TEST_TIMEOUT` | `{test_timeout.*eunit}` | P2 | 1 min | Yes |
| `TEST_PORT_CONFLICT` | `eaddrinuse.*8080` | P2 | 30 sec | Yes |
| `TEST_FILE_LOCKED` | `Resource temporarily unavailable` | P2 | 30 sec | Yes |
| `CT_SUITE_CRASH` | `Common Test.*suite crashed` | P2 | 2 min | Yes |
| `COVERAGE_TOOL_ERROR` | `cover.*analyze.*failed` | P2 | 1 min | Yes |

#### Priority 3: Network & External

| Error Code | Pattern | Severity | Recovery Time | Retry |
|------------|---------|----------|---------------|-------|
| `NETWORK_TIMEOUT` | `{error.*timeout.*hex.pm}` | P3 | 1 min | Yes (3×) |
| `NETWORK_DNS_FAILURE` | `nxdomain.*hex.pm` | P3 | 30 sec | Yes (3×) |
| `NETWORK_CONNECTION_REFUSED` | `econnrefused` | P3 | 1 min | Yes (3×) |
| `NETWORK_SSL_ERROR` | `{error.*ssl.*certificate}` | P3 | 2 min | Yes (2×) |
| `HEX_PACKAGE_CORRUPTED` | `checksum mismatch` | P3 | 1 min | Yes (2×) |

#### Priority 4: Environment

| Error Code | Pattern | Severity | Recovery Time | Retry |
|------------|---------|----------|---------------|-------|
| `PATH_NOT_SET` | `erl.*command not found` | P4 | 30 sec | Yes |
| `LOCALE_MISSING` | `locale.*not found` | P4 | 1 min | Yes |
| `TMPDIR_PERMISSION` | `Permission denied.*tmp` | P4 | 30 sec | Yes |
| `REBAR_LOCK_CONFLICT` | `rebar.lock.*conflict` | P4 | 1 min | Yes |
| `STALE_BUILD_ARTIFACTS` | `outdated.*beam.*files` | P4 | 2 min | Yes |

### Error Classifier Implementation

**File**: `tools/cloud-recovery/error-classifier.sh`

```bash
#!/usr/bin/env bash
# Error Classifier - Parse stderr/stdout and classify errors
# Returns: ERROR_CODE or UNKNOWN

set -euo pipefail

classify_error() {
    local log_file="$1"
    local error_code="UNKNOWN"
    local confidence=0

    # Priority 0: Blocking errors (check first)
    if grep -q "minimum_otp_vsn.*required.*28" "$log_file"; then
        error_code="OTP_VERSION_LOW"
        confidence=100
    elif grep -q "command not found: rebar3" "$log_file"; then
        error_code="MISSING_REBAR"
        confidence=100
    elif grep -q "No space left on device" "$log_file"; then
        error_code="DISK_FULL"
        confidence=100
    elif grep -q "Cannot allocate memory" "$log_file"; then
        error_code="MEMORY_EXHAUSTED"
        confidence=100
    elif grep -q "No space.*inodes" "$log_file"; then
        error_code="DISK_INODE_EXHAUSTED"
        confidence=100

    # Priority 1: Build errors
    elif grep -Eq "{error.*{not_found.*(jsx|jesse|gproc|gun|ranch|poolboy|cowboy)}}" "$log_file"; then
        error_code="MISSING_DEPS"
        confidence=95
    elif grep -q "syntax error before:" "$log_file"; then
        error_code="COMPILE_ERROR_SYNTAX"
        confidence=90
    elif grep -q "undefined module" "$log_file"; then
        error_code="COMPILE_ERROR_MISSING_MODULE"
        confidence=90
    elif grep -q "beam_lib.*error" "$log_file"; then
        error_code="COMPILE_ERROR_BEAM"
        confidence=90
    elif grep -q "dialyzer.*rebuild.*plt" "$log_file"; then
        error_code="PLT_OUTDATED"
        confidence=95

    # Priority 2: Test errors
    elif grep -q "{test_timeout" "$log_file"; then
        error_code="TEST_TIMEOUT"
        confidence=85
    elif grep -q "eaddrinuse.*[0-9]\{4,5\}" "$log_file"; then
        error_code="TEST_PORT_CONFLICT"
        confidence=90
    elif grep -q "Resource temporarily unavailable" "$log_file"; then
        error_code="TEST_FILE_LOCKED"
        confidence=80
    elif grep -q "Common Test.*suite crashed" "$log_file"; then
        error_code="CT_SUITE_CRASH"
        confidence=85
    elif grep -q "cover.*analyze.*failed" "$log_file"; then
        error_code="COVERAGE_TOOL_ERROR"
        confidence=85

    # Priority 3: Network errors
    elif grep -q "{error.*timeout.*hex.pm}" "$log_file"; then
        error_code="NETWORK_TIMEOUT"
        confidence=90
    elif grep -q "nxdomain.*hex.pm" "$log_file"; then
        error_code="NETWORK_DNS_FAILURE"
        confidence=90
    elif grep -q "econnrefused" "$log_file"; then
        error_code="NETWORK_CONNECTION_REFUSED"
        confidence=85
    elif grep -q "{error.*ssl.*certificate}" "$log_file"; then
        error_code="NETWORK_SSL_ERROR"
        confidence=90
    elif grep -q "checksum mismatch" "$log_file"; then
        error_code="HEX_PACKAGE_CORRUPTED"
        confidence=90

    # Priority 4: Environment errors
    elif grep -q "erl.*command not found" "$log_file"; then
        error_code="PATH_NOT_SET"
        confidence=95
    elif grep -q "locale.*not found" "$log_file"; then
        error_code="LOCALE_MISSING"
        confidence=90
    elif grep -q "Permission denied.*tmp" "$log_file"; then
        error_code="TMPDIR_PERMISSION"
        confidence=90
    elif grep -q "rebar.lock.*conflict" "$log_file"; then
        error_code="REBAR_LOCK_CONFLICT"
        confidence=85
    elif grep -q "outdated.*beam.*files" "$log_file"; then
        error_code="STALE_BUILD_ARTIFACTS"
        confidence=80
    fi

    echo "$error_code:$confidence"
}

# Main
if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <log_file>" >&2
    exit 1
fi

classify_error "$1"
```

**Output Format**: `ERROR_CODE:CONFIDENCE` (e.g., `OTP_VERSION_LOW:100`)

---

## Recovery Hooks by Error Type

### Hook Registry

**File**: `tools/cloud-recovery/hook-registry.json`

```json
{
  "hooks": {
    "OTP_VERSION_LOW": {
      "script": "recovery-hooks/otp-version.sh",
      "max_retries": 1,
      "timeout_sec": 300,
      "priority": 0,
      "requires_sudo": true,
      "requires_network": true
    },
    "MISSING_REBAR": {
      "script": "recovery-hooks/missing-rebar.sh",
      "max_retries": 2,
      "timeout_sec": 120,
      "priority": 0,
      "requires_sudo": false,
      "requires_network": true
    },
    "DISK_FULL": {
      "script": "recovery-hooks/disk-space.sh",
      "max_retries": 1,
      "timeout_sec": 180,
      "priority": 0,
      "requires_sudo": false,
      "requires_network": false
    },
    "MEMORY_EXHAUSTED": {
      "script": "recovery-hooks/memory-exhausted.sh",
      "max_retries": 1,
      "timeout_sec": 60,
      "priority": 0,
      "requires_sudo": false,
      "requires_network": false
    },
    "MISSING_DEPS": {
      "script": "recovery-hooks/missing-deps.sh",
      "max_retries": 3,
      "timeout_sec": 180,
      "priority": 1,
      "requires_sudo": false,
      "requires_network": true
    },
    "COMPILE_ERROR_SYNTAX": {
      "script": "../auto-fix/syntax-fix-agent.sh",
      "max_retries": 3,
      "timeout_sec": 60,
      "priority": 1,
      "requires_sudo": false,
      "requires_network": false
    },
    "PLT_OUTDATED": {
      "script": "recovery-hooks/plt-rebuild.sh",
      "max_retries": 1,
      "timeout_sec": 300,
      "priority": 1,
      "requires_sudo": false,
      "requires_network": false
    },
    "TEST_TIMEOUT": {
      "script": "recovery-hooks/test-timeout.sh",
      "max_retries": 2,
      "timeout_sec": 60,
      "priority": 2,
      "requires_sudo": false,
      "requires_network": false
    },
    "TEST_PORT_CONFLICT": {
      "script": "recovery-hooks/port-conflict.sh",
      "max_retries": 3,
      "timeout_sec": 30,
      "priority": 2,
      "requires_sudo": false,
      "requires_network": false
    },
    "NETWORK_TIMEOUT": {
      "script": "recovery-hooks/network-timeout.sh",
      "max_retries": 3,
      "timeout_sec": 60,
      "priority": 3,
      "requires_sudo": false,
      "requires_network": true
    },
    "STALE_BUILD_ARTIFACTS": {
      "script": "recovery-hooks/clean-rebuild.sh",
      "max_retries": 1,
      "timeout_sec": 120,
      "priority": 4,
      "requires_sudo": false,
      "requires_network": false
    }
  }
}
```

### Recovery Hook: OTP Version

**File**: `tools/cloud-recovery/recovery-hooks/otp-version.sh`

```bash
#!/usr/bin/env bash
# Recovery Hook: OTP Version Low
# Upgrades Erlang/OTP to version 28+ (IDEMPOTENT)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/recovery-common.sh"

hook_name="otp-version"
log_recovery_start "$hook_name"

# Check if OTP 28+ already installed (idempotency)
if erl -eval 'halt(0)' -noshell 2>/dev/null; then
    OTP_VERSION=$(erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:format("~s", [Version]), halt().' -noshell 2>/dev/null || echo "0")
    MAJOR_VERSION=$(echo "$OTP_VERSION" | cut -d. -f1)

    if [[ "$MAJOR_VERSION" -ge 28 ]]; then
        log_recovery_skip "$hook_name" "OTP $OTP_VERSION already installed (≥28)"
        exit 0
    fi
fi

log_recovery_action "$hook_name" "Installing Erlang/OTP 28+"

# Detect OS
if [[ -f /etc/os-release ]]; then
    source /etc/os-release
    OS_ID="$ID"
else
    log_recovery_error "$hook_name" "Cannot detect OS"
    exit 1
fi

# Install based on OS (idempotent - package manager handles this)
case "$OS_ID" in
    ubuntu|debian)
        log_recovery_action "$hook_name" "Installing via apt (Ubuntu/Debian)"

        # Add Erlang Solutions repository (idempotent)
        if [[ ! -f /etc/apt/sources.list.d/erlang-solutions.list ]]; then
            wget -O- https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc | sudo apt-key add -
            echo "deb https://packages.erlang-solutions.com/ubuntu focal contrib" | sudo tee /etc/apt/sources.list.d/erlang-solutions.list
            sudo apt-get update
        fi

        # Install Erlang (apt is idempotent)
        sudo apt-get install -y esl-erlang=1:28.* || {
            log_recovery_error "$hook_name" "apt-get install failed"
            exit 1
        }
        ;;

    centos|rhel|fedora)
        log_recovery_action "$hook_name" "Installing via yum (RHEL/CentOS)"

        # Add Erlang Solutions repository
        if [[ ! -f /etc/yum.repos.d/erlang-solutions.repo ]]; then
            sudo wget -O /etc/yum.repos.d/erlang-solutions.repo \
                https://packages.erlang-solutions.com/rpm/erlang_solutions.repo
        fi

        # Install Erlang
        sudo yum install -y erlang-28.* || {
            log_recovery_error "$hook_name" "yum install failed"
            exit 1
        }
        ;;

    alpine)
        log_recovery_action "$hook_name" "Installing via apk (Alpine)"
        sudo apk add --no-cache erlang~=28 erlang-dev~=28 || {
            log_recovery_error "$hook_name" "apk add failed"
            exit 1
        }
        ;;

    *)
        log_recovery_error "$hook_name" "Unsupported OS: $OS_ID"
        exit 1
        ;;
esac

# Verify installation
if ! erl -eval 'halt(0)' -noshell 2>/dev/null; then
    log_recovery_error "$hook_name" "Erlang installation verification failed"
    exit 1
fi

NEW_VERSION=$(erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:format("~s", [Version]), halt().' -noshell)
log_recovery_success "$hook_name" "Installed Erlang/OTP $NEW_VERSION"
exit 0
```

### Recovery Hook: Missing Dependencies

**File**: `tools/cloud-recovery/recovery-hooks/missing-deps.sh`

```bash
#!/usr/bin/env bash
# Recovery Hook: Missing Dependencies
# Fetches missing rebar3 dependencies (IDEMPOTENT)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/recovery-common.sh"

hook_name="missing-deps"
log_recovery_start "$hook_name"

# Check if rebar3 exists
if ! command -v rebar3 &>/dev/null; then
    log_recovery_error "$hook_name" "rebar3 not found (use missing-rebar hook first)"
    exit 1
fi

# Clean lock file if corrupted (idempotent check)
if [[ -f rebar.lock ]]; then
    if ! rebar3 as test tree &>/dev/null; then
        log_recovery_action "$hook_name" "Removing corrupted rebar.lock"
        mv rebar.lock "rebar.lock.backup.$(date +%s)" || true
    fi
fi

# Fetch dependencies with retry (network-aware)
max_attempts=3
attempt=1

while [[ $attempt -le $max_attempts ]]; do
    log_recovery_action "$hook_name" "Fetching dependencies (attempt $attempt/$max_attempts)"

    if timeout 120 rebar3 get-deps 2>&1 | tee /tmp/erlmcp_deps_fetch.log; then
        log_recovery_success "$hook_name" "Dependencies fetched successfully"
        exit 0
    fi

    # Check for network errors
    if grep -q "timeout\|econnrefused\|nxdomain" /tmp/erlmcp_deps_fetch.log; then
        log_recovery_action "$hook_name" "Network error detected, waiting 10s before retry"
        sleep 10
    else
        log_recovery_error "$hook_name" "Non-network error, aborting"
        cat /tmp/erlmcp_deps_fetch.log >&2
        exit 1
    fi

    ((attempt++))
done

log_recovery_error "$hook_name" "Failed after $max_attempts attempts"
exit 1
```

### Recovery Hook: Disk Space

**File**: `tools/cloud-recovery/recovery-hooks/disk-space.sh`

```bash
#!/usr/bin/env bash
# Recovery Hook: Disk Full
# Cleans up build artifacts and temporary files (IDEMPOTENT)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/recovery-common.sh"

hook_name="disk-space"
log_recovery_start "$hook_name"

# Check current disk usage
df_output=$(df -h . | tail -1)
usage_percent=$(echo "$df_output" | awk '{print $5}' | sed 's/%//')

log_recovery_action "$hook_name" "Current disk usage: ${usage_percent}%"

if [[ $usage_percent -lt 90 ]]; then
    log_recovery_skip "$hook_name" "Disk usage below 90% threshold"
    exit 0
fi

# Cleanup strategy (safest to most aggressive)
freed_space=0

# 1. Clean rebar3 build artifacts (safest, idempotent)
log_recovery_action "$hook_name" "Cleaning rebar3 build artifacts"
if [[ -d _build ]]; then
    before=$(du -sm _build 2>/dev/null | awk '{print $1}' || echo 0)
    rebar3 clean || true
    after=$(du -sm _build 2>/dev/null | awk '{print $1}' || echo 0)
    freed=$((before - after))
    freed_space=$((freed_space + freed))
    log_recovery_action "$hook_name" "Freed ${freed}MB from _build"
fi

# Check if enough space freed
df_output=$(df -h . | tail -1)
usage_percent=$(echo "$df_output" | awk '{print $5}' | sed 's/%//')
if [[ $usage_percent -lt 85 ]]; then
    log_recovery_success "$hook_name" "Freed ${freed_space}MB, disk usage now ${usage_percent}%"
    exit 0
fi

# 2. Clean dialyzer PLT files (can rebuild)
log_recovery_action "$hook_name" "Removing dialyzer PLT files"
if [[ -d $HOME/.cache/rebar3/rebar3_*_plt ]]; then
    before=$(du -sm $HOME/.cache/rebar3 2>/dev/null | awk '{print $1}' || echo 0)
    rm -rf "$HOME/.cache/rebar3/rebar3_"*"_plt" || true
    after=$(du -sm $HOME/.cache/rebar3 2>/dev/null | awk '{print $1}' || echo 0)
    freed=$((before - after))
    freed_space=$((freed_space + freed))
    log_recovery_action "$hook_name" "Freed ${freed}MB from PLT files"
fi

# Check again
df_output=$(df -h . | tail -1)
usage_percent=$(echo "$df_output" | awk '{print $5}' | sed 's/%//')
if [[ $usage_percent -lt 85 ]]; then
    log_recovery_success "$hook_name" "Freed ${freed_space}MB, disk usage now ${usage_percent}%"
    exit 0
fi

# 3. Clean hex cache (last resort, can re-download)
log_recovery_action "$hook_name" "Cleaning hex package cache"
if [[ -d $HOME/.cache/rebar3/hex ]]; then
    before=$(du -sm $HOME/.cache/rebar3/hex 2>/dev/null | awk '{print $1}' || echo 0)
    rm -rf "$HOME/.cache/rebar3/hex/"* || true
    after=$(du -sm $HOME/.cache/rebar3/hex 2>/dev/null | awk '{print $1}' || echo 0)
    freed=$((before - after))
    freed_space=$((freed_space + freed))
    log_recovery_action "$hook_name" "Freed ${freed}MB from hex cache"
fi

# Final check
df_output=$(df -h . | tail -1)
usage_percent=$(echo "$df_output" | awk '{print $5}' | sed 's/%//')

if [[ $usage_percent -lt 85 ]]; then
    log_recovery_success "$hook_name" "Freed ${freed_space}MB, disk usage now ${usage_percent}%"
    exit 0
else
    log_recovery_error "$hook_name" "Only freed ${freed_space}MB, usage still ${usage_percent}%"
    exit 1
fi
```

### Recovery Hook: Memory Exhausted

**File**: `tools/cloud-recovery/recovery-hooks/memory-exhausted.sh`

```bash
#!/usr/bin/env bash
# Recovery Hook: Memory Exhausted
# Kills stale Erlang processes and frees memory (IDEMPOTENT)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/recovery-common.sh"

hook_name="memory-exhausted"
log_recovery_start "$hook_name"

# Check current memory usage
mem_available=$(free -m | awk 'NR==2 {print $7}')
log_recovery_action "$hook_name" "Available memory: ${mem_available}MB"

if [[ $mem_available -gt 512 ]]; then
    log_recovery_skip "$hook_name" "Sufficient memory available (>512MB)"
    exit 0
fi

# Kill stale beam.smp processes (excluding current shell)
log_recovery_action "$hook_name" "Killing stale Erlang processes"
killed_count=0

# Find beam.smp processes older than 1 hour
for pid in $(pgrep -f beam.smp); do
    # Skip if process is less than 1 hour old
    start_time=$(ps -p "$pid" -o etimes= 2>/dev/null || echo 0)
    if [[ $start_time -gt 3600 ]]; then
        log_recovery_action "$hook_name" "Killing beam.smp PID $pid (running ${start_time}s)"
        kill -9 "$pid" 2>/dev/null || true
        ((killed_count++))
    fi
done

if [[ $killed_count -eq 0 ]]; then
    log_recovery_action "$hook_name" "No stale processes found"
fi

# Wait for memory to be freed
sleep 2

# Check memory again
mem_available=$(free -m | awk 'NR==2 {print $7}')

if [[ $mem_available -gt 256 ]]; then
    log_recovery_success "$hook_name" "Freed memory, now ${mem_available}MB available"
    exit 0
else
    log_recovery_error "$hook_name" "Insufficient memory after cleanup (${mem_available}MB)"
    exit 1
fi
```

### Recovery Hook: Network Timeout

**File**: `tools/cloud-recovery/recovery-hooks/network-timeout.sh`

```bash
#!/usr/bin/env bash
# Recovery Hook: Network Timeout
# Implements exponential backoff retry with network validation (IDEMPOTENT)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/recovery-common.sh"

hook_name="network-timeout"
log_recovery_start "$hook_name"

# Allowlisted domains for erlmcp
ALLOWED_DOMAINS=(
    "hex.pm"
    "packages.erlang-solutions.com"
    "github.com"
    "repo.hex.pm"
)

# Test network connectivity
test_network() {
    local domain="$1"
    local timeout=5

    if timeout "$timeout" curl -sSf "https://$domain" &>/dev/null; then
        return 0
    else
        return 1
    fi
}

# Check each allowlisted domain
log_recovery_action "$hook_name" "Testing network connectivity"
reachable_count=0

for domain in "${ALLOWED_DOMAINS[@]}"; do
    if test_network "$domain"; then
        log_recovery_action "$hook_name" "✓ $domain reachable"
        ((reachable_count++))
    else
        log_recovery_action "$hook_name" "✗ $domain unreachable"
    fi
done

if [[ $reachable_count -eq 0 ]]; then
    log_recovery_error "$hook_name" "No allowlisted domains reachable"
    exit 1
fi

if [[ $reachable_count -eq ${#ALLOWED_DOMAINS[@]} ]]; then
    log_recovery_success "$hook_name" "All domains reachable, network OK"
    exit 0
fi

# Partial connectivity - retry with exponential backoff
log_recovery_action "$hook_name" "Partial connectivity, implementing retry strategy"

max_attempts=3
base_delay=5

for attempt in $(seq 1 $max_attempts); do
    delay=$((base_delay * 2**(attempt-1)))
    log_recovery_action "$hook_name" "Waiting ${delay}s before retry (attempt $attempt/$max_attempts)"
    sleep "$delay"

    # Re-test
    reachable_count=0
    for domain in "${ALLOWED_DOMAINS[@]}"; do
        if test_network "$domain"; then
            ((reachable_count++))
        fi
    done

    if [[ $reachable_count -eq ${#ALLOWED_DOMAINS[@]} ]]; then
        log_recovery_success "$hook_name" "Network recovered after ${attempt} retries"
        exit 0
    fi
done

log_recovery_error "$hook_name" "Network still unstable after $max_attempts retries"
exit 1
```

### Recovery Hook: Test Port Conflict

**File**: `tools/cloud-recovery/recovery-hooks/port-conflict.sh`

```bash
#!/usr/bin/env bash
# Recovery Hook: Test Port Conflict
# Finds and kills processes using test ports (IDEMPOTENT)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/recovery-common.sh"

hook_name="port-conflict"
log_recovery_start "$hook_name"

# Extract port number from error log
ERROR_LOG="${1:-/tmp/erlmcp_test_error.log}"
if [[ ! -f "$ERROR_LOG" ]]; then
    log_recovery_error "$hook_name" "Error log not found: $ERROR_LOG"
    exit 1
fi

PORT=$(grep -oP 'eaddrinuse.*\K[0-9]{4,5}' "$ERROR_LOG" | head -1)

if [[ -z "$PORT" ]]; then
    log_recovery_error "$hook_name" "Could not extract port number from error log"
    exit 1
fi

log_recovery_action "$hook_name" "Detected port conflict: $PORT"

# Find process using port
if command -v lsof &>/dev/null; then
    PID=$(lsof -ti tcp:"$PORT" 2>/dev/null || true)
elif command -v ss &>/dev/null; then
    PID=$(ss -tlnp | grep ":$PORT " | grep -oP 'pid=\K[0-9]+' || true)
else
    log_recovery_error "$hook_name" "Neither lsof nor ss available"
    exit 1
fi

if [[ -z "$PID" ]]; then
    log_recovery_skip "$hook_name" "Port $PORT is now free"
    exit 0
fi

# Kill process
log_recovery_action "$hook_name" "Killing process $PID using port $PORT"
kill -9 "$PID" 2>/dev/null || {
    log_recovery_error "$hook_name" "Failed to kill process $PID"
    exit 1
}

# Wait for port to be released
sleep 1

# Verify port is free
if command -v lsof &>/dev/null; then
    if lsof -ti tcp:"$PORT" &>/dev/null; then
        log_recovery_error "$hook_name" "Port $PORT still in use after kill"
        exit 1
    fi
fi

log_recovery_success "$hook_name" "Port $PORT freed"
exit 0
```

---

## Idempotent Recovery Scripts

### Common Recovery Library

**File**: `tools/cloud-recovery/lib/recovery-common.sh`

```bash
#!/usr/bin/env bash
# Common recovery library - shared functions for all hooks

# Logging functions
LOG_DIR="${LOG_DIR:-logs/cloud-recovery}"
mkdir -p "$LOG_DIR"

log_recovery_start() {
    local hook="$1"
    echo "[$(date -Iseconds)] START $hook" | tee -a "$LOG_DIR/recovery.log"
}

log_recovery_action() {
    local hook="$1"
    local message="$2"
    echo "[$(date -Iseconds)] ACTION $hook: $message" | tee -a "$LOG_DIR/recovery.log"
}

log_recovery_skip() {
    local hook="$1"
    local reason="$2"
    echo "[$(date -Iseconds)] SKIP $hook: $reason" | tee -a "$LOG_DIR/recovery.log"
}

log_recovery_success() {
    local hook="$1"
    local message="$2"
    echo "[$(date -Iseconds)] SUCCESS $hook: $message" | tee -a "$LOG_DIR/recovery.log"
}

log_recovery_error() {
    local hook="$1"
    local message="$2"
    echo "[$(date -Iseconds)] ERROR $hook: $message" | tee -a "$LOG_DIR/recovery.log" >&2
}

# State management functions
get_recovery_state() {
    local hook="$1"
    local state_file="$LOG_DIR/state/${hook}.json"

    if [[ -f "$state_file" ]]; then
        cat "$state_file"
    else
        echo '{"attempts": 0, "last_run": null, "status": "never_run"}'
    fi
}

set_recovery_state() {
    local hook="$1"
    local status="$2"
    local state_file="$LOG_DIR/state/${hook}.json"

    mkdir -p "$LOG_DIR/state"

    local attempts=$(get_recovery_state "$hook" | jq -r '.attempts // 0')
    ((attempts++))

    cat > "$state_file" <<EOF
{
  "attempts": $attempts,
  "last_run": "$(date -Iseconds)",
  "status": "$status"
}
EOF
}

# Rollback function (for atomic operations)
rollback_recovery() {
    local hook="$1"
    local backup_dir="$LOG_DIR/backups/${hook}"

    if [[ -d "$backup_dir" ]]; then
        log_recovery_action "$hook" "Rolling back changes"
        # Restore backups (implementation depends on hook)
        # Example: cp -r "$backup_dir"/* /target/location/
    fi
}

# Network check
check_network_required() {
    local hook="$1"

    # Read from hook registry
    local requires_network=$(jq -r ".hooks.\"$hook\".requires_network // false" \
        "$(dirname "${BASH_SOURCE[0]}")/../hook-registry.json")

    if [[ "$requires_network" == "true" ]]; then
        if ! ping -c 1 -W 2 8.8.8.8 &>/dev/null; then
            log_recovery_error "$hook" "Network required but unavailable"
            return 1
        fi
    fi

    return 0
}
```

### Idempotency Patterns

#### Pattern 1: Check-Then-Act

```bash
# Example: Only install if not already installed
if ! command -v rebar3 &>/dev/null; then
    # Install rebar3
    install_rebar3
fi
```

#### Pattern 2: Atomic Operations with Rollback

```bash
# Backup before modification
backup_file="/tmp/backup.$(date +%s)"
cp target_file "$backup_file"

# Attempt operation
if ! modify_target_file; then
    # Rollback on failure
    cp "$backup_file" target_file
    exit 1
fi

# Cleanup backup on success
rm "$backup_file"
```

#### Pattern 3: State File Tracking

```bash
# Check if operation already completed
state_file="$LOG_DIR/state/operation.complete"
if [[ -f "$state_file" ]]; then
    echo "Operation already completed on $(cat "$state_file")"
    exit 0
fi

# Perform operation
perform_operation

# Mark complete
date -Iseconds > "$state_file"
```

#### Pattern 4: Package Manager Idempotency

```bash
# Package managers are naturally idempotent
apt-get install -y erlang-28  # Won't reinstall if already present
yum install -y erlang-28       # Same behavior
```

---

## Integration with Hooks

### Pre-Task Hook Integration

**File**: `.claude/hooks/pre-task.sh` (enhanced)

```bash
#!/bin/bash
set -e
cd "$CLAUDE_PROJECT_DIR" || exit 2

# Existing pre-task checks...
# (compilation, tests, xref)

# NEW: Cloud recovery pre-check
if [[ -x tools/cloud-recovery/pre-flight-check.sh ]]; then
    echo -e "${YELLOW}→ Cloud pre-flight checks${NC}"
    if ! tools/cloud-recovery/pre-flight-check.sh; then
        echo -e "${RED}✗ Pre-flight failed, attempting recovery${NC}"

        # Attempt automatic recovery
        if tools/cloud-recovery/recovery-engine.sh auto; then
            echo -e "${GREEN}✓ Recovery successful, continuing${NC}"
        else
            echo -e "${RED}✗ Recovery failed, manual intervention required${NC}"
            exit 2
        fi
    fi
fi

# Continue with existing quality gates...
```

### Post-Task Hook Integration

**File**: `.claude/hooks/post-task.sh` (enhanced)

```bash
#!/bin/bash
set -e
cd "$CLAUDE_PROJECT_DIR" || exit 2

# Existing post-task analysis...

# NEW: Cloud recovery post-task error detection
if [[ $? -ne 0 ]]; then
    ERROR_LOG="/tmp/erlmcp_last_error.log"

    # Capture last 50 lines of stderr/stdout
    tail -50 "$REPORT_DIR/eunit_$TIMESTAMP.log" > "$ERROR_LOG" 2>&1

    # Classify error
    ERROR_RESULT=$(tools/cloud-recovery/error-classifier.sh "$ERROR_LOG")
    ERROR_CODE=$(echo "$ERROR_RESULT" | cut -d: -f1)
    CONFIDENCE=$(echo "$ERROR_RESULT" | cut -d: -f2)

    echo -e "${YELLOW}Detected error: $ERROR_CODE (confidence: $CONFIDENCE%)${NC}"

    # Attempt recovery if confidence > 80%
    if [[ $CONFIDENCE -gt 80 ]]; then
        echo -e "${YELLOW}Attempting automatic recovery...${NC}"

        if tools/cloud-recovery/recovery-engine.sh recover "$ERROR_CODE" "$ERROR_LOG"; then
            echo -e "${GREEN}✓ Recovery successful, retrying task${NC}"

            # Retry original task
            exec "$0"  # Re-run post-task hook
        else
            echo -e "${RED}✗ Recovery failed${NC}"
            exit 1
        fi
    fi
fi
```

### Recovery Engine

**File**: `tools/cloud-recovery/recovery-engine.sh`

```bash
#!/usr/bin/env bash
# Recovery Engine - Executes recovery hooks based on error classification

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/recovery-common.sh"

HOOK_REGISTRY="$SCRIPT_DIR/hook-registry.json"

# Load hook configuration
load_hook_config() {
    local error_code="$1"

    if ! jq -e ".hooks.\"$error_code\"" "$HOOK_REGISTRY" &>/dev/null; then
        echo "ERROR: No hook registered for $error_code" >&2
        return 1
    fi

    jq -r ".hooks.\"$error_code\"" "$HOOK_REGISTRY"
}

# Execute recovery hook with timeout and retry
execute_hook() {
    local error_code="$1"
    local error_log="$2"

    local config=$(load_hook_config "$error_code")
    local script=$(echo "$config" | jq -r '.script')
    local max_retries=$(echo "$config" | jq -r '.max_retries')
    local timeout_sec=$(echo "$config" | jq -r '.timeout_sec')
    local priority=$(echo "$config" | jq -r '.priority')

    log_recovery_action "engine" "Executing hook for $error_code (priority: P$priority)"

    local attempt=1
    while [[ $attempt -le $max_retries ]]; do
        log_recovery_action "engine" "Attempt $attempt/$max_retries for $error_code"

        # Execute with timeout
        if timeout "$timeout_sec" "$SCRIPT_DIR/$script" "$error_log" 2>&1 | tee -a "$LOG_DIR/recovery.log"; then
            log_recovery_success "engine" "Hook succeeded for $error_code"
            set_recovery_state "$error_code" "success"
            return 0
        fi

        log_recovery_action "engine" "Hook failed, retry $attempt/$max_retries"
        ((attempt++))

        # Exponential backoff between retries
        if [[ $attempt -le $max_retries ]]; then
            sleep $((2 ** attempt))
        fi
    done

    log_recovery_error "engine" "Hook failed after $max_retries attempts for $error_code"
    set_recovery_state "$error_code" "failed"
    return 1
}

# Main command dispatcher
case "${1:-}" in
    auto)
        # Pre-flight checks (run all P0 hooks)
        log_recovery_start "engine"
        log_recovery_action "engine" "Running pre-flight checks"

        # Check OTP version
        if ! erl -eval 'halt(0)' -noshell 2>/dev/null; then
            execute_hook "MISSING_OTP" "/dev/null"
        fi

        # Check rebar3
        if ! command -v rebar3 &>/dev/null; then
            execute_hook "MISSING_REBAR" "/dev/null"
        fi

        # Check disk space
        usage=$(df . | tail -1 | awk '{print $5}' | sed 's/%//')
        if [[ $usage -gt 90 ]]; then
            execute_hook "DISK_FULL" "/dev/null"
        fi

        log_recovery_success "engine" "Pre-flight checks complete"
        ;;

    recover)
        # Recover from specific error
        error_code="${2:-}"
        error_log="${3:-/tmp/erlmcp_last_error.log}"

        if [[ -z "$error_code" ]]; then
            echo "Usage: $0 recover <ERROR_CODE> [error_log]" >&2
            exit 1
        fi

        execute_hook "$error_code" "$error_log"
        ;;

    *)
        echo "Usage: $0 {auto|recover <ERROR_CODE> [error_log]}" >&2
        exit 1
        ;;
esac
```

---

## Logging & Auditability

### Audit Log Format

**File**: `logs/cloud-recovery/audit.log`

```
[2026-02-01T14:23:45+00:00] EVENT=recovery_start HOOK=otp-version ERROR=OTP_VERSION_LOW TRIGGERED_BY=post-task.sh
[2026-02-01T14:23:46+00:00] ACTION=check_current_version HOOK=otp-version RESULT=version_22_detected
[2026-02-01T14:23:47+00:00] ACTION=detect_os HOOK=otp-version RESULT=ubuntu_20.04
[2026-02-01T14:24:12+00:00] ACTION=install_erlang HOOK=otp-version RESULT=success VERSION=28.0.1
[2026-02-01T14:24:15+00:00] ACTION=verify_installation HOOK=otp-version RESULT=success VERSION=28.0.1
[2026-02-01T14:24:15+00:00] EVENT=recovery_complete HOOK=otp-version DURATION_SEC=30 OUTCOME=success
```

### Evidence Chain

Each recovery attempt generates an evidence bundle:

**File**: `logs/cloud-recovery/evidence/<timestamp>-<error_code>.json`

```json
{
  "recovery_id": "rec_20260201_142345_otp_version",
  "timestamp_start": "2026-02-01T14:23:45+00:00",
  "timestamp_end": "2026-02-01T14:24:15+00:00",
  "duration_sec": 30,
  "error": {
    "code": "OTP_VERSION_LOW",
    "confidence": 100,
    "original_message": "minimum_otp_vsn required: 28, current: 22",
    "source_log": "/tmp/erlmcp_compile.log"
  },
  "hook": {
    "name": "otp-version",
    "script": "recovery-hooks/otp-version.sh",
    "priority": 0,
    "max_retries": 1,
    "actual_attempts": 1
  },
  "actions": [
    {
      "timestamp": "2026-02-01T14:23:46+00:00",
      "action": "check_current_version",
      "result": "version_22_detected",
      "details": {"current_version": "22.3.4"}
    },
    {
      "timestamp": "2026-02-01T14:23:47+00:00",
      "action": "detect_os",
      "result": "ubuntu_20.04",
      "details": {"os_id": "ubuntu", "os_version": "20.04"}
    },
    {
      "timestamp": "2026-02-01T14:24:12+00:00",
      "action": "install_erlang",
      "result": "success",
      "details": {"package": "esl-erlang=1:28.0.1", "method": "apt-get"}
    },
    {
      "timestamp": "2026-02-01T14:24:15+00:00",
      "action": "verify_installation",
      "result": "success",
      "details": {"verified_version": "28.0.1"}
    }
  ],
  "outcome": "success",
  "rollback_performed": false,
  "manual_intervention_required": false,
  "artifacts": {
    "backup_files": [],
    "log_files": [
      "logs/cloud-recovery/recovery.log",
      "logs/cloud-recovery/otp-version.log"
    ]
  },
  "cost": {
    "network_bytes_downloaded": 52428800,
    "disk_space_used_mb": 250,
    "cpu_seconds": 25
  }
}
```

### Monitoring Dashboard

**File**: `tools/cloud-recovery/dashboard.sh`

```bash
#!/usr/bin/env bash
# Cloud Recovery Dashboard - Real-time monitoring

watch -n 5 '
echo "=== Cloud Error Recovery Dashboard ==="
echo ""
echo "Recent Recoveries (last 10):"
tail -10 logs/cloud-recovery/recovery.log | grep SUCCESS
echo ""
echo "Active Recovery Attempts:"
tail -20 logs/cloud-recovery/recovery.log | grep "START\|ACTION" | tail -5
echo ""
echo "Failure Rate (last 24h):"
total=$(grep -c "EVENT=recovery_start" logs/cloud-recovery/audit.log || echo 0)
failed=$(grep -c "OUTCOME=failed" logs/cloud-recovery/audit.log || echo 0)
if [[ $total -gt 0 ]]; then
    rate=$((failed * 100 / total))
    echo "  Total: $total, Failed: $failed, Rate: ${rate}%"
else
    echo "  No recoveries in last 24h"
fi
'
```

---

## Network-Aware Recovery

### Network Allowlist

**File**: `tools/cloud-recovery/network-allowlist.json`

```json
{
  "allowlisted_domains": [
    {
      "domain": "hex.pm",
      "purpose": "Erlang package repository",
      "protocols": ["https"],
      "ports": [443],
      "required_for": ["MISSING_DEPS", "HEX_PACKAGE_CORRUPTED"]
    },
    {
      "domain": "packages.erlang-solutions.com",
      "purpose": "Erlang/OTP packages",
      "protocols": ["https", "http"],
      "ports": [443, 80],
      "required_for": ["OTP_VERSION_LOW"]
    },
    {
      "domain": "github.com",
      "purpose": "Git dependencies",
      "protocols": ["https", "git"],
      "ports": [443, 9418],
      "required_for": ["MISSING_DEPS"]
    },
    {
      "domain": "repo.hex.pm",
      "purpose": "Hex CDN",
      "protocols": ["https"],
      "ports": [443],
      "required_for": ["MISSING_DEPS"]
    }
  ],
  "network_policies": {
    "max_retry_attempts": 3,
    "timeout_sec": 30,
    "exponential_backoff": true,
    "base_delay_sec": 5
  }
}
```

### Network Validation

**File**: `tools/cloud-recovery/lib/network-validator.sh`

```bash
#!/usr/bin/env bash
# Network validator - check connectivity before network operations

source "$(dirname "${BASH_SOURCE[0]}")/recovery-common.sh"

ALLOWLIST_FILE="$(dirname "${BASH_SOURCE[0]}")/../network-allowlist.json"

validate_domain_access() {
    local domain="$1"
    local timeout="${2:-10}"

    # Check DNS resolution
    if ! host "$domain" &>/dev/null; then
        log_recovery_error "network" "DNS resolution failed for $domain"
        return 1
    fi

    # Check HTTPS connectivity
    if ! timeout "$timeout" curl -sSf "https://$domain" &>/dev/null; then
        log_recovery_error "network" "HTTPS connection failed for $domain"
        return 1
    fi

    log_recovery_action "network" "Validated access to $domain"
    return 0
}

validate_all_required_domains() {
    local error_code="$1"

    # Get required domains for this error code
    local domains=$(jq -r ".allowlisted_domains[] | select(.required_for[] == \"$error_code\") | .domain" "$ALLOWLIST_FILE")

    if [[ -z "$domains" ]]; then
        log_recovery_action "network" "No network requirements for $error_code"
        return 0
    fi

    local all_valid=true
    for domain in $domains; do
        if ! validate_domain_access "$domain"; then
            all_valid=false
        fi
    done

    if [[ "$all_valid" == "true" ]]; then
        return 0
    else
        return 1
    fi
}
```

---

## State Management

### Recovery State Schema

**File**: `logs/cloud-recovery/state/<hook>.json`

```json
{
  "hook": "otp-version",
  "attempts": 2,
  "last_run": "2026-02-01T14:24:15+00:00",
  "status": "success",
  "history": [
    {
      "timestamp": "2026-02-01T10:15:30+00:00",
      "outcome": "failed",
      "error": "network_timeout",
      "duration_sec": 120
    },
    {
      "timestamp": "2026-02-01T14:24:15+00:00",
      "outcome": "success",
      "duration_sec": 30
    }
  ],
  "artifacts_cached": {
    "erlang_28_deb": {
      "path": "/var/cache/erlmcp/esl-erlang_28.0.1_amd64.deb",
      "size_bytes": 52428800,
      "checksum_sha256": "abc123...",
      "cached_at": "2026-02-01T14:24:10+00:00"
    }
  },
  "config_overrides": {}
}
```

### Artifact Cache Management

**File**: `tools/cloud-recovery/lib/artifact-cache.sh`

```bash
#!/usr/bin/env bash
# Artifact cache - preserve downloaded files between recovery attempts

CACHE_DIR="${CACHE_DIR:-/var/cache/erlmcp}"
mkdir -p "$CACHE_DIR"

cache_artifact() {
    local source_file="$1"
    local artifact_name="$2"

    if [[ ! -f "$source_file" ]]; then
        return 1
    fi

    local cache_file="$CACHE_DIR/$artifact_name"
    cp "$source_file" "$cache_file"

    # Store metadata
    local checksum=$(sha256sum "$cache_file" | awk '{print $1}')
    local size=$(stat -f%z "$cache_file" 2>/dev/null || stat -c%s "$cache_file")

    cat > "$cache_file.meta.json" <<EOF
{
  "artifact_name": "$artifact_name",
  "path": "$cache_file",
  "size_bytes": $size,
  "checksum_sha256": "$checksum",
  "cached_at": "$(date -Iseconds)"
}
EOF

    log_recovery_action "cache" "Cached artifact: $artifact_name ($size bytes)"
}

retrieve_artifact() {
    local artifact_name="$1"
    local dest_file="$2"

    local cache_file="$CACHE_DIR/$artifact_name"

    if [[ ! -f "$cache_file" ]]; then
        return 1
    fi

    # Verify checksum
    local stored_checksum=$(jq -r '.checksum_sha256' "$cache_file.meta.json")
    local actual_checksum=$(sha256sum "$cache_file" | awk '{print $1}')

    if [[ "$stored_checksum" != "$actual_checksum" ]]; then
        log_recovery_error "cache" "Checksum mismatch for $artifact_name"
        rm "$cache_file" "$cache_file.meta.json"
        return 1
    fi

    cp "$cache_file" "$dest_file"
    log_recovery_action "cache" "Retrieved artifact from cache: $artifact_name"
    return 0
}

clean_old_cache() {
    local max_age_days="${1:-7}"

    find "$CACHE_DIR" -type f -mtime +"$max_age_days" -delete
    log_recovery_action "cache" "Cleaned cache older than $max_age_days days"
}
```

---

## Implementation Guide

### Phase 1: Foundation (Week 1)

#### Tasks
1. Create directory structure
   ```bash
   mkdir -p tools/cloud-recovery/{lib,recovery-hooks}
   mkdir -p logs/cloud-recovery/{state,evidence,backups}
   ```

2. Implement core libraries
   - `lib/recovery-common.sh` (logging, state management)
   - `lib/network-validator.sh` (network checks)
   - `lib/artifact-cache.sh` (caching)

3. Create error classifier
   - `error-classifier.sh` (25 error patterns)

4. Test with mock errors

#### Deliverables
- ✅ Core libraries (3 files)
- ✅ Error classifier with 25 patterns
- ✅ Unit tests for classifier

### Phase 2: Recovery Hooks (Week 2)

#### Tasks
1. Implement P0 hooks (blocking errors)
   - `recovery-hooks/otp-version.sh`
   - `recovery-hooks/missing-rebar.sh`
   - `recovery-hooks/disk-space.sh`
   - `recovery-hooks/memory-exhausted.sh`

2. Implement P1 hooks (build errors)
   - `recovery-hooks/missing-deps.sh`
   - `recovery-hooks/plt-rebuild.sh`

3. Implement P2 hooks (test errors)
   - `recovery-hooks/test-timeout.sh`
   - `recovery-hooks/port-conflict.sh`

4. Implement P3 hooks (network errors)
   - `recovery-hooks/network-timeout.sh`

#### Deliverables
- ✅ 9 recovery hooks (all idempotent)
- ✅ Hook registry JSON
- ✅ Integration tests for each hook

### Phase 3: Integration (Week 3)

#### Tasks
1. Implement recovery engine
   - `recovery-engine.sh` (hook executor)

2. Integrate with existing hooks
   - Enhance `.claude/hooks/pre-task.sh`
   - Enhance `.claude/hooks/post-task.sh`

3. Implement orchestrator integration
   - Update `tools/auto-fix/orchestrator.sh`

4. Add monitoring dashboard
   - `dashboard.sh` (real-time monitoring)

#### Deliverables
- ✅ Recovery engine
- ✅ Enhanced pre/post-task hooks
- ✅ Monitoring dashboard

### Phase 4: Testing & Validation (Week 4)

#### Tasks
1. Create cloud simulation environment
2. Run chaos tests (inject all 25 error types)
3. Measure recovery rate, time, success rate
4. Generate evidence bundles for audit
5. Performance testing (recovery overhead)

#### Deliverables
- ✅ Cloud simulation environment
- ✅ Chaos test suite (25 scenarios)
- ✅ Recovery metrics report
- ✅ Performance validation

---

## Testing Strategy

### Unit Tests

**File**: `test/cloud-recovery/error_classifier_tests.erl`

```erlang
-module(error_classifier_tests).
-include_lib("eunit/include/eunit.hrl").

classify_otp_version_low_test() ->
    LogFile = "/tmp/test_otp_version.log",
    file:write_file(LogFile, "minimum_otp_vsn required: 28, current: 22"),

    Result = os:cmd("tools/cloud-recovery/error-classifier.sh " ++ LogFile),

    ?assertMatch("OTP_VERSION_LOW:100\n", Result),
    file:delete(LogFile).

classify_missing_deps_test() ->
    LogFile = "/tmp/test_missing_deps.log",
    file:write_file(LogFile, "{error,{not_found,jsx}}"),

    Result = os:cmd("tools/cloud-recovery/error-classifier.sh " ++ LogFile),

    ?assertMatch("MISSING_DEPS:95\n", Result),
    file:delete(LogFile).

classify_disk_full_test() ->
    LogFile = "/tmp/test_disk_full.log",
    file:write_file(LogFile, "cp: cannot create file: No space left on device"),

    Result = os:cmd("tools/cloud-recovery/error-classifier.sh " ++ LogFile),

    ?assertMatch("DISK_FULL:100\n", Result),
    file:delete(LogFile).
```

### Integration Tests

**File**: `test/cloud-recovery/recovery_engine_SUITE.erl`

```erlang
-module(recovery_engine_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [
    test_otp_version_recovery,
    test_missing_deps_recovery,
    test_disk_space_recovery,
    test_network_timeout_recovery
].

init_per_suite(Config) ->
    % Setup mock environment
    os:cmd("mkdir -p /tmp/erlmcp_test_recovery"),
    Config.

end_per_suite(_Config) ->
    % Cleanup
    os:cmd("rm -rf /tmp/erlmcp_test_recovery"),
    ok.

test_otp_version_recovery(_Config) ->
    % Inject OTP version error
    ErrorLog = "/tmp/test_otp_error.log",
    file:write_file(ErrorLog, "minimum_otp_vsn required: 28"),

    % Classify
    ClassifyResult = os:cmd("tools/cloud-recovery/error-classifier.sh " ++ ErrorLog),
    <<"OTP_VERSION_LOW:100\n">> = list_to_binary(ClassifyResult),

    % Execute recovery (mock - don't actually install OTP in tests)
    % In real test, would verify hook is idempotent and safe

    ok.
```

### Chaos Tests

**File**: `test/cloud-recovery/chaos_SUITE.erl`

```erlang
-module(chaos_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [
    test_inject_all_error_types,
    test_recovery_rate,
    test_recovery_time,
    test_idempotency
].

test_inject_all_error_types(_Config) ->
    ErrorTypes = [
        "OTP_VERSION_LOW",
        "MISSING_REBAR",
        "DISK_FULL",
        "MEMORY_EXHAUSTED",
        "MISSING_DEPS",
        "COMPILE_ERROR_SYNTAX",
        "TEST_TIMEOUT",
        "NETWORK_TIMEOUT"
        % ... all 25 types
    ],

    Results = lists:map(fun(ErrorType) ->
        inject_error_and_recover(ErrorType)
    end, ErrorTypes),

    Successes = length([R || R <- Results, R =:= success]),
    Total = length(ErrorTypes),
    SuccessRate = Successes / Total,

    % Target: 90% recovery rate
    ?assert(SuccessRate >= 0.9).

test_idempotency(_Config) ->
    % Run each hook 3 times, verify same result
    Hooks = [
        "otp-version.sh",
        "disk-space.sh",
        "missing-deps.sh"
    ],

    lists:foreach(fun(Hook) ->
        Result1 = run_hook(Hook),
        Result2 = run_hook(Hook),
        Result3 = run_hook(Hook),

        ?assertEqual(Result1, Result2),
        ?assertEqual(Result2, Result3)
    end, Hooks).
```

---

## Cloud Simulation Environment

### Docker-Based Simulation

**File**: `test/cloud-recovery/Dockerfile.cloud-sim`

```dockerfile
FROM ubuntu:20.04

# Simulate cloud VM with minimal OTP
RUN apt-get update && \
    apt-get install -y wget curl gnupg2 lsb-release && \
    # Install OLD Erlang (OTP 22) to trigger upgrade
    apt-get install -y erlang=1:22.* || true

# Install rebar3
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && \
    chmod +x rebar3 && \
    mv rebar3 /usr/local/bin/

# Simulate limited disk space
RUN dd if=/dev/zero of=/tmp/filler bs=1M count=100

# Copy erlmcp
WORKDIR /workspace
COPY . .

# Run recovery tests
CMD ["bash", "test/cloud-recovery/run-simulation.sh"]
```

### Simulation Script

**File**: `test/cloud-recovery/run-simulation.sh`

```bash
#!/usr/bin/env bash
# Cloud recovery simulation - inject failures and verify recovery

set -euo pipefail

echo "=== Cloud Recovery Simulation ==="
echo ""

# Scenario 1: OTP Version Too Low
echo "Scenario 1: OTP Version Too Low"
TERM=dumb rebar3 compile 2>&1 | tee /tmp/compile.log || {
    # Should detect OTP_VERSION_LOW error
    ERROR_CODE=$(tools/cloud-recovery/error-classifier.sh /tmp/compile.log | cut -d: -f1)

    if [[ "$ERROR_CODE" == "OTP_VERSION_LOW" ]]; then
        echo "✓ Correctly identified OTP_VERSION_LOW"

        # Attempt recovery
        if tools/cloud-recovery/recovery-engine.sh recover OTP_VERSION_LOW /tmp/compile.log; then
            echo "✓ Recovery successful"
        else
            echo "✗ Recovery failed"
            exit 1
        fi
    fi
}
echo ""

# Scenario 2: Disk Full
echo "Scenario 2: Disk Full"
# Fill disk to 95%
dd if=/dev/zero of=/tmp/filler2 bs=1M count=500 2>/dev/null || {
    ERROR_CODE=$(tools/cloud-recovery/error-classifier.sh /tmp/dd.log | cut -d: -f1)

    if [[ "$ERROR_CODE" == "DISK_FULL" ]]; then
        echo "✓ Correctly identified DISK_FULL"

        if tools/cloud-recovery/recovery-engine.sh recover DISK_FULL /tmp/dd.log; then
            echo "✓ Recovery successful"
        fi
    fi
}
echo ""

# Scenario 3: Network Timeout (simulate with iptables)
echo "Scenario 3: Network Timeout"
# Block hex.pm temporarily
sudo iptables -A OUTPUT -d hex.pm -j DROP
rebar3 get-deps 2>&1 | tee /tmp/deps.log || {
    ERROR_CODE=$(tools/cloud-recovery/error-classifier.sh /tmp/deps.log | cut -d: -f1)

    if [[ "$ERROR_CODE" == "NETWORK_TIMEOUT" ]]; then
        echo "✓ Correctly identified NETWORK_TIMEOUT"

        # Restore network
        sudo iptables -D OUTPUT -d hex.pm -j DROP

        if tools/cloud-recovery/recovery-engine.sh recover NETWORK_TIMEOUT /tmp/deps.log; then
            echo "✓ Recovery successful"
        fi
    fi
}
echo ""

echo "=== Simulation Complete ==="
```

---

## Metrics & KPIs

### Recovery Rate Target

| Priority | Target Recovery Rate | Max Recovery Time |
|----------|----------------------|-------------------|
| P0 | 95% | 5 minutes |
| P1 | 90% | 3 minutes |
| P2 | 85% | 2 minutes |
| P3 | 80% | 1 minute |
| P4 | 75% | 1 minute |

### Performance Overhead

- Recovery engine invocation: <100ms
- Error classification: <50ms
- Audit log write: <10ms
- State persistence: <20ms

### Cost Metrics

- Network bandwidth: Track bytes downloaded per recovery
- Disk space: Track artifacts cached
- CPU time: Track recovery hook execution time

---

## Appendix A: Error Pattern Reference

| Error Code | Regex Pattern | Example |
|------------|---------------|---------|
| `OTP_VERSION_LOW` | `minimum_otp_vsn.*required.*28` | `minimum_otp_vsn required: 28, current: 22` |
| `MISSING_REBAR` | `command not found: rebar3` | `bash: rebar3: command not found` |
| `DISK_FULL` | `No space left on device` | `cp: cannot create file: No space left on device` |
| `MEMORY_EXHAUSTED` | `Cannot allocate memory` | `erl: Cannot allocate memory` |
| `MISSING_DEPS` | `{error.*{not_found.*jsx}}` | `{error,{not_found,jsx}}` |
| `COMPILE_ERROR_SYNTAX` | `syntax error before:` | `syntax error before: '}'` |
| `TEST_TIMEOUT` | `{test_timeout.*eunit}` | `{test_timeout,erlmcp_server_tests}` |
| `NETWORK_TIMEOUT` | `{error.*timeout.*hex.pm}` | `{error,{timeout,hex.pm}}` |

(Full table includes all 25 patterns)

---

## Appendix B: Hook Template

**File**: `tools/cloud-recovery/recovery-hooks/TEMPLATE.sh`

```bash
#!/usr/bin/env bash
# Recovery Hook Template
# Description: [What this hook recovers from]
# Idempotent: Yes
# Network Required: [Yes/No]

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/../lib/recovery-common.sh"

hook_name="HOOK_NAME"
log_recovery_start "$hook_name"

# Step 1: Check if recovery needed (idempotency)
if [[ CONDITION_ALREADY_FIXED ]]; then
    log_recovery_skip "$hook_name" "Already in desired state"
    exit 0
fi

# Step 2: Perform recovery action
log_recovery_action "$hook_name" "Performing recovery action"

# [Your recovery logic here]
# Must be idempotent - safe to run multiple times

# Step 3: Verify recovery succeeded
if [[ VERIFICATION_CHECK ]]; then
    log_recovery_success "$hook_name" "Recovery completed successfully"
    set_recovery_state "$hook_name" "success"
    exit 0
else
    log_recovery_error "$hook_name" "Recovery verification failed"
    set_recovery_state "$hook_name" "failed"
    exit 1
fi
```

---

## Appendix C: Glossary

| Term | Definition |
|------|------------|
| **Idempotent** | Operation that can be applied multiple times with the same result |
| **Hook** | Script executed in response to an error |
| **Recovery Engine** | Orchestrator that dispatches hooks based on error classification |
| **Evidence Chain** | Audit trail of all recovery attempts with full context |
| **Artifact Cache** | Persistent storage for downloaded files to avoid re-downloading |
| **Priority** | Error urgency (P0=blocking, P1=critical, P2=important, P3=network, P4=environment) |

---

## Summary

This Cloud Error Recovery Design provides erlmcp with:

1. **Automated Recovery**: 90% of common cloud VM failures fixed without manual intervention
2. **Idempotent Operations**: Safe to retry, no side effects on re-run
3. **Full Auditability**: Every recovery attempt logged with evidence
4. **Network Awareness**: Respects allowlisted domains, handles timeouts gracefully
5. **State Preservation**: Caches artifacts, preserves build state across retries
6. **OTP Compliance**: No isolation violations, works with supervision trees

**Next Steps**: Implement Phase 1 (Foundation) and begin testing with mock errors.

---

**End of Document**
