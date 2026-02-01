#!/usr/bin/env bash
###===================================================================
### validate_profile.sh - ERLMCP_PROFILE Validation Script
###===================================================================
###
### Validates that a given ERLMCP_PROFILE is valid and exists.
### Used by all build/test/bench scripts for profile validation.
###
### Usage:
###   ./scripts/validate_profile.sh <profile>
###
### Exit Codes:
###   0 - Profile is valid
###   1 - Profile is invalid or missing
###
### Valid Profiles:
###   dev      - Development (verbose logging, relaxed timeouts)
###   test     - Testing (CI/CD optimized, fast fail)
###   staging  - Pre-production (production-like with debugging)
###   prod     - Production (hardened, minimal logging)
###
###===================================================================

set -euo pipefail

PROFILE="${1:-}"

# Valid profiles array
VALID_PROFILES=("dev" "test" "staging" "prod")

# Project root (for config file checks)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Validate profile argument
if [ -z "$PROFILE" ]; then
    echo -e "${RED}ERROR: No profile specified${NC}" >&2
    echo "Usage: $0 <profile>" >&2
    echo "Valid profiles: ${VALID_PROFILES[*]}" >&2
    exit 1
fi

# Check if profile is in valid list
is_valid=false
for valid_profile in "${VALID_PROFILES[@]}"; do
    if [ "$PROFILE" = "$valid_profile" ]; then
        is_valid=true
        break
    fi
done

if [ "$is_valid" = false ]; then
    echo -e "${RED}ERROR: Invalid profile '$PROFILE'${NC}" >&2
    echo "Valid profiles: ${VALID_PROFILES[*]}" >&2
    exit 1
fi

# Check if profile-specific config file exists (warn but don't fail)
CONFIG_FILE="$PROJECT_ROOT/config/sys.config.$PROFILE"

if [ ! -f "$CONFIG_FILE" ]; then
    echo -e "${YELLOW}WARNING: Config file not found: $CONFIG_FILE${NC}" >&2
    echo -e "${YELLOW}The Makefile will create a symlink when you run 'make compile'${NC}" >&2
fi

# Success
echo -e "${GREEN}✓ Profile '$PROFILE' is valid${NC}"
exit 0
