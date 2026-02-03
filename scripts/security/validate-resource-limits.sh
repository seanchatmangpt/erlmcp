#!/usr/bin/env bash
# =============================================================================
# P0-008: Validate Container Resource Limits
# =============================================================================
# This script validates that all services in docker-compose files
# have resource limits defined.
#
# Exit codes:
#   0 - All services have resource limits (PASS)
#   1 - Services missing resource limits (FAIL)
#   2 - Usage error
#
# Usage: ./scripts/security/validate-resource-limits.sh
# =============================================================================

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

FAILURES=0
SERVICES_CHECKED=0
SERVICES_PASSED=0

echo "=== P0-008: Validating container resource limits ==="
echo

# Check if yq is available
if ! command -v yq &> /dev/null; then
    echo -e "${YELLOW}⚠️  WARNING: yq not found. Using grep-based fallback (less precise).${NC}"
    echo "             Install yq for accurate validation: https://github.com/mikefarah/yq"
    echo
    USE_YQ=false
else
    USE_YQ=true
fi

# Function to check a single compose file
check_compose_file() {
    local file="$1"

    if [ ! -f "$file" ]; then
        return 0
    fi

    echo "Checking: $file"

    if [ "$USE_YQ" = true ]; then
        # Use yq for accurate parsing
        local services
        services=$(yq eval '.services | keys | .[]' "$file" 2>/dev/null) || return 0

        for service in $services; do
            ((SERVICES_CHECKED++))

            local has_limits
            has_limits=$(yq eval ".services.$service.deploy.resources.limits" "$file" 2>/dev/null || echo "null")

            if [ "$has_limits" = "null" ] || [ -z "$has_limits" ]; then
                echo -e "  ${RED}❌ Service '$service' missing resource limits${NC}"
                ((FAILURES++))
            else
                echo -e "  ${GREEN}✅ Service '$service' has resource limits${NC}"
                ((SERVICES_PASSED++))
            fi
        done
    else
        # Fallback: grep-based check
        if grep -q "deploy:" "$file" && grep -q "resources:" "$file" && grep -A 5 "resources:" "$file" | grep -q "limits:"; then
            # Count services with deploy.resources
            local services_with_limits
            services_with_limits=$(grep -c "deploy:" "$file" 2>/dev/null || echo "0")

            if [ "$services_with_limits" -gt 0 ]; then
                echo -e "  ${GREEN}✅ File has resource limits defined ($services_with_limits service(s))${NC}"
                ((SERVICES_PASSED += services_with_limits))
                ((SERVICES_CHECKED += services_with_limits))
            else
                echo -e "  ${RED}❌ File missing resource limits${NC}"
                ((FAILURES++))
            fi
        else
            echo -e "  ${RED}❌ File missing deploy.resources section${NC}"
            ((FAILURES++))
        fi
    fi

    echo
}

# Check all docker-compose files
echo "Checking docker-compose files..."
shopt -s nullglob
for file in docker-compose.yml docker-compose*.yml; do
    check_compose_file "$file"
done
for file in docker/*/docker-compose*.yml; do
    check_compose_file "$file"
done
for file in docker-swarm/*.yml; do
    check_compose_file "$file"
done
shopt -u nullglob

echo "=== Summary ==="
echo "Services checked: $SERVICES_CHECKED"
echo "Services passed: $SERVICES_PASSED"
echo "Failures: $FAILURES"

if [ $FAILURES -eq 0 ]; then
    echo -e "${GREEN}✅ PASS: All services have resource limits${NC}"
    exit 0
else
    echo -e "${RED}❌ FAIL: $FAILURES service(s) missing resource limits${NC}"
    echo
    echo "To fix, add resource limits to each service:"
    echo
    cat <<'EOF'
    deploy:
      resources:
        limits:
          cpus: '${SERVICE_NAME}_CPU_LIMIT:-2.0'
          memory: '${SERVICE_NAME}_MEMORY_LIMIT:-4G}'
          pids: ${SERVICE_NAME}_PIDS_LIMIT:-4096}
        reservations:
          cpus: '${SERVICE_NAME}_CPU_RESERVATION:-1.0}'
          memory: '${SERVICE_NAME}_MEMORY_RESERVATION:-2G}'
EOF
    exit 1
fi
