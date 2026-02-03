#!/usr/bin/env bash
# =============================================================================
# P0-002: Validate No Hardcoded Cluster Cookies
# =============================================================================
# This script validates that no default cookies are present in any
# docker-compose.yml or vm.args files.
#
# Exit codes:
#   0 - No hardcoded cookies found (PASS)
#   1 - Default cookies found (FAIL)
#   2 - Usage error
#
# Usage: ./scripts/security/validate-no-default-cookies.sh
# =============================================================================

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

FAILURES=0
PASS=0
SKIPPED=0

echo "=== P0-002: Validating no hardcoded cluster cookies ==="
echo

# Function to check file for default cookies
check_file_for_default_cookie() {
    local file="$1"
    local pattern="$2"

    if [ ! -f "$file" ]; then
        echo -e "${YELLOW}⚠️  SKIP: File not found: $file${NC}"
        ((SKIPPED++))
        return 0
    fi

    if grep -qE "$pattern" "$file"; then
        echo -e "${RED}❌ FAIL: Default cookie found in $file${NC}"
        grep -E "$pattern" "$file" | sed 's/^/    /'
        ((FAILURES++))
        return 1
    else
        echo -e "${GREEN}✅ PASS: $file has no default cookies${NC}"
        ((PASS++))
        return 0
    fi
}

# Check all docker-compose files
echo "Checking docker-compose files..."
check_file_for_default_cookie "docker-compose.yml" "ERLANG_COOKIE:\s*\$\{[^}]*:-[^}]*cookie"
check_file_for_default_cookie "docker-compose.cluster.yml" "ERLANG_COOKIE:\s*\$\{[^}]*:-[^}]*cookie"
check_file_for_default_cookie "docker-compose.colima.yml" "ERLANG_COOKIE:\s*\$\{[^}]*:-[^}]*cookie"
check_file_for_default_cookie "docker-compose.prod.yml" "ERLANG_COOKIE:\s*\$\{[^}]*:-[^}]*cookie"

# Check subdirectories
for file in docker/*/docker-compose*.yml; do
    [ -f "$file" ] && check_file_for_default_cookie "$file" "ERLANG_COOKIE:\s*\$\{[^}]*:-[^}]*cookie"
done

for file in docker-swarm/*.yml; do
    [ -f "$file" ] && check_file_for_default_cookie "$file" "ERLANG_COOKIE:\s*\$\{[^}]*:-[^}]*cookie"
done

echo
echo "Checking vm.args files..."
check_file_for_default_cookie "vm.args" "^-setcookie\s+[a-zA-Z]"

# Check config subdirectories
if [ -d "config" ]; then
    for file in config/*/vm.args*; do
        [ -f "$file" ] && check_file_for_default_cookie "$file" "^-setcookie\s+[a-zA-Z]"
    done
fi

if [ -d "rel" ]; then
    for file in rel/*.vm.args; do
        [ -f "$file" ] && check_file_for_default_cookie "$file" "^-setcookie\s+[a-zA-Z]"
    done
fi

echo
echo "Checking .env files for default values..."
# Skip .env.prod.template since it's meant to be a template
if [ -f ".env" ]; then
    check_file_for_default_cookie ".env" "ERLANG_COOKIE=(erlmcp_prod_cookie|erlmcp_dev_cookie|erlmcp_secret_cookie|changeme|default|test)"
fi

# Check specific env files
for file in .env.prod .env.dev .env.staging; do
    [ -f "$file" ] && check_file_for_default_cookie "$file" "ERLANG_COOKIE=(erlmcp_prod_cookie|erlmcp_dev_cookie|erlmcp_secret_cookie|changeme|default|test)"
done

echo
echo "=== Summary ==="
echo "Passed: $PASS"
echo "Failed: $FAILURES"
echo "Skipped: $SKIPPED"

if [ $FAILURES -eq 0 ]; then
    echo -e "${GREEN}✅ PASS: No hardcoded cookies found${NC}"
    exit 0
else
    echo -e "${RED}❌ FAIL: $FAILURES file(s) with hardcoded cookies found${NC}"
    echo
    echo "To fix:"
    echo "  1. Remove default values from docker-compose.yml:"
    echo "     Change: ERLANG_COOKIE: \${ERLANG_COOKIE:-default_value}"
    echo "     To:     ERLANG_COOKIE: \${ERLANG_COOKIE}"
    echo
    echo "  2. Remove hardcoded cookies from vm.args:"
    echo "     Comment out or remove: -setcookie hardcoded_value"
    echo
    echo "  3. Set ERLANG_COOKIE via environment or Docker secrets"
    exit 1
fi
