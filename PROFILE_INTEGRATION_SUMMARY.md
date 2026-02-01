# Phase 2: ERLMCP_PROFILE Integration - Implementation Summary

## Overview

Successfully integrated `ERLMCP_PROFILE` environment variable into all build, test, and benchmark scripts. This phase completes the config unification by ensuring all scripts respect the profile system while maintaining backward compatibility.

## Implementation Date

2026-02-01

## Scripts Updated

### 1. Core Validation Script

**File:** `/home/user/erlmcp/scripts/validate_profile.sh` (NEW)
- **Purpose:** Central validation script for all profile-aware scripts
- **Valid Profiles:** dev, test, staging, prod
- **Exit Codes:** 0 (valid), 1 (invalid)
- **Features:**
  - Validates profile name against whitelist
  - Checks for profile-specific config files (warns if missing)
  - Used by all other scripts for consistent validation

### 2. Build Script

**File:** `/home/user/erlmcp/scripts/build_and_test.sh`
- **Default Profile:** `dev` (development builds)
- **Integration Points:**
  - Profile validation at script start
  - Profile displayed in status output
  - Help text documents ERLMCP_PROFILE env var
  - Graceful fallback to 'dev' on invalid profile
- **Usage:**
  ```bash
  # Use default (dev)
  ./scripts/build_and_test.sh

  # Override profile
  ERLMCP_PROFILE=test ./scripts/build_and_test.sh
  ```

### 3. CI Validation Script

**File:** `/home/user/erlmcp/scripts/validation/run-ci.sh`
- **Default Profile:** `test` (mapped from VALIDATION_LEVEL)
- **Backward Compatibility:** Maps existing VALIDATION_LEVEL to ERLMCP_PROFILE
  - `strict` → `test`
  - `quick/standard/full` → `test`
- **Integration Points:**
  - Profile validation after VALIDATION_LEVEL mapping
  - Profile displayed in pre-flight checks
  - Maintains all existing VALIDATION_LEVEL behavior
- **Usage:**
  ```bash
  # Backward compatible (uses VALIDATION_LEVEL)
  VALIDATION_LEVEL=strict ./scripts/validation/run-ci.sh

  # New way (explicit profile)
  ERLMCP_PROFILE=test ./scripts/validation/run-ci.sh

  # Override mapping
  VALIDATION_LEVEL=strict ERLMCP_PROFILE=staging ./scripts/validation/run-ci.sh
  ```

### 4. Test Runner

**File:** `/home/user/erlmcp/tools/test-runner.sh`
- **Default Profile:** `test` (safer for testing)
- **Rationale:** Test profile ensures strict mode, no secrets, fast fail
- **Integration Points:**
  - Profile validation at script start
  - Profile displayed in test runner header
  - Graceful fallback to 'test' on invalid profile
- **Usage:**
  ```bash
  # Use default (test)
  ./tools/test-runner.sh

  # Override for development
  ERLMCP_PROFILE=dev ./tools/test-runner.sh
  ```

### 5. Benchmark Scripts

All benchmark scripts now use `staging` profile by default (production-like with logging).

#### Quick Benchmark Scripts

**Files:**
- `/home/user/erlmcp/scripts/bench/quick.sh`
- `/home/user/erlmcp/scripts/bench/quick_bench.sh`
- `/home/user/erlmcp/scripts/bench/run_quick_benchmarks.sh`

**Default Profile:** `staging`
**Rationale:** Production-like performance with debugging enabled
**Override:** `ERLMCP_PROFILE=prod` for extreme production scenarios

#### Full Benchmark Suite

**File:** `/home/user/erlmcp/scripts/bench/run_all_benchmarks.sh`
**Default Profile:** `staging`
**Integration Points:**
- Profile validation after SCRIPT_DIR setup
- Profile displayed in banner
- Help documentation updated
**Usage:**
  ```bash
  # Standard staging profile
  ./scripts/bench/run_all_benchmarks.sh

  # Production profile for release benchmarks
  ERLMCP_PROFILE=prod ./scripts/bench/run_all_benchmarks.sh full
  ```

## Profile Defaults by Script Type

| Script Type | Default Profile | Rationale |
|-------------|-----------------|-----------|
| Build (`build_and_test.sh`) | `dev` | Development workflow, loose logging |
| Test (`test-runner.sh`) | `test` | Strict mode, no secrets, CI-optimized |
| CI (`run-ci.sh`) | `test` | Mapped from VALIDATION_LEVEL, strict |
| Benchmark (`bench/*.sh`) | `staging` | Production-like with debugging |

## Non-Breaking Guarantees

### 1. Graceful Fallback
All scripts fall back to their default profile if:
- Invalid profile specified
- validate_profile.sh not found
- Profile config file missing (warns only)

### 2. Backward Compatibility
- `run-ci.sh` maintains VALIDATION_LEVEL support
- All existing env vars still work
- No scripts require ERLMCP_PROFILE to be set

### 3. No Errors on Missing Config
Scripts warn but continue if profile-specific config files are missing, allowing:
- Incremental migration
- Testing before config files are created
- Development without all profiles configured

## Validation Testing

All scripts tested with:
- ✅ Valid profiles (dev, test, staging, prod)
- ✅ Invalid profile (graceful fallback)
- ✅ Missing ERLMCP_PROFILE (uses defaults)
- ✅ Profile displayed in output
- ✅ Validation script path resolution

### Test Results

```bash
# validate_profile.sh
$ ./scripts/validate_profile.sh dev
✓ Profile 'dev' is valid

$ ./scripts/validate_profile.sh invalid
ERROR: Invalid profile 'invalid'
Valid profiles: dev test staging prod
Exit code: 1

# build_and_test.sh
$ ERLMCP_PROFILE=test ./scripts/build_and_test.sh --help
✓ Profile 'test' is valid
[Shows help with profile documentation]

# run-ci.sh backward compatibility
$ VALIDATION_LEVEL=strict ./scripts/validation/run-ci.sh
ℹ ERLMCP_PROFILE: test
ℹ VALIDATION_LEVEL: strict

# test-runner.sh
$ ./tools/test-runner.sh
✓ Profile 'test' is valid
========================================
ErlMCP Test Runner
========================================
Profile: test

# quick.sh
$ ERLMCP_PROFILE=staging ./scripts/bench/quick.sh
✓ Profile 'staging' is valid
════════════════════════════════════════
  ERLMCP QUICK PERFORMANCE CHECK
════════════════════════════════════════
Target: Sub-2-minute sanity check for local development
Profile: staging
```

## Integration Pattern

All scripts follow this pattern:

```bash
#!/usr/bin/env bash
# ... header ...
# Environment:
#   ERLMCP_PROFILE  Profile to use (dev|test|staging|prod), defaults to 'X'

set -euo pipefail

# ==============================================================================
# Profile Configuration
# ==============================================================================

# Set default profile for this script type
ERLMCP_PROFILE="${ERLMCP_PROFILE:-default_profile}"

# Get paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"  # Adjust as needed
VALIDATE_SCRIPT="$PROJECT_ROOT/scripts/validate_profile.sh"

# Validate profile (with graceful fallback)
if [ -f "$VALIDATE_SCRIPT" ]; then
    if ! "$VALIDATE_SCRIPT" "$ERLMCP_PROFILE" 2>/dev/null; then
        echo "WARNING: Invalid profile '$ERLMCP_PROFILE', falling back to 'default_profile'"
        ERLMCP_PROFILE=default_profile
    fi
else
    echo "WARNING: validate_profile.sh not found, using profile: $ERLMCP_PROFILE"
fi

export ERLMCP_PROFILE

# ... rest of script ...
# Display profile in banner/output
echo "Profile: $ERLMCP_PROFILE"
```

## Files Modified

1. `/home/user/erlmcp/scripts/validate_profile.sh` (NEW - 74 lines)
2. `/home/user/erlmcp/scripts/build_and_test.sh` (updated)
3. `/home/user/erlmcp/scripts/validation/run-ci.sh` (updated)
4. `/home/user/erlmcp/tools/test-runner.sh` (updated)
5. `/home/user/erlmcp/scripts/bench/quick.sh` (updated)
6. `/home/user/erlmcp/scripts/bench/quick_bench.sh` (updated)
7. `/home/user/erlmcp/scripts/bench/run_quick_benchmarks.sh` (updated)
8. `/home/user/erlmcp/scripts/bench/run_all_benchmarks.sh` (updated)

**Total:** 1 new file, 7 updated files

## Quality Gates

✅ **Compilation:** Not required (shell scripts)
✅ **Testing:** Manual validation of all scripts
✅ **Coverage:** All critical build/test/bench scripts covered
✅ **Backward Compatibility:** VALIDATION_LEVEL mapping preserved
✅ **Documentation:** All scripts document ERLMCP_PROFILE in help/comments

## Next Steps (Phase 3)

Future enhancements could include:
1. Update Makefile targets to pass profile to scripts
2. Add profile-specific rebar3 commands (e.g., `rebar3 as test compile`)
3. Create profile-specific release configurations
4. Add profile validation to pre-commit hooks
5. Document profile system in main README.md

## Armstrong-AGI Compliance

**Principle:** Don't promise correct behavior. Make incorrect behavior impossible.

**Application:**
- ✅ Invalid profiles → impossible (validated, graceful fallback)
- ✅ Missing validation script → impossible failure (warn and continue)
- ✅ Breaking changes → impossible (backward compatibility maintained)
- ✅ Undocumented behavior → impossible (all scripts document profile usage)

## TPS Alignment

### Poka-Yoke (Mistake-Proofing)
- Invalid profiles rejected by validation script
- Graceful fallbacks prevent script failures
- Clear error messages guide users

### Jidoka (Built-in Quality)
- validate_profile.sh is the single source of truth
- All scripts use identical validation pattern
- Impossible to use invalid profile

### Kaizen (Continuous Improvement)
- Profile system enables easier configuration management
- Backward compatibility maintains existing workflows
- Foundation for future profile-based optimizations

---

**Status:** ✅ Phase 2 Complete - All scripts integrated with ERLMCP_PROFILE
**Date:** 2026-02-01
**Quality Gates:** All passed
