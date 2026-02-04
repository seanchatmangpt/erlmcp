#!/bin/bash
# healthcheck.sh - Monitor erlmcp health for production deployment
set -euo pipefail

# Configuration
HEALTH_URL="${HEALTH_URL:-http://localhost:8080/health}"
TIMEOUT="${TIMEOUT:-10}"
RETRY_COUNT="${RETRY_COUNT:-3}"
RETRY_DELAY="${RETRY_DELAY:-5}"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo "🔍 erlmcp Health Check"
echo "======================"

# Function to check health
check_health() {
    if curl -f --max-time "$TIMEOUT" "$HEALTH_URL" >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Attempt health checks
for i in $(seq 1 $RETRY_COUNT); do
    if check_health; then
        echo -e "${GREEN}✅ erlmcp is healthy${NC}"
        exit 0
    else
        echo -e "${YELLOW}⚠ Attempt $i/$RETRY_COUNT failed${NC}"
        if [ $i -lt $RETRY_COUNT ]; then
            echo "Waiting $RETRY_DELAY seconds before retry..."
            sleep $RETRY_DELAY
        fi
    fi
done

# All attempts failed
echo -e "${RED}❌ erlmcp is unhealthy after $RETRY_COUNT attempts${NC}"
echo "Health URL: $HEALTH_URL"
exit 1