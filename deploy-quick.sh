#!/bin/bash
# deploy-quick.sh - Fast production deployment for erlmcp v3.0.0
set -euo pipefail

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${GREEN}🚀 Starting erlmcp production deployment...${NC}"
echo "============================================"

# Set production cookie
export ERLANG_COOKIE=erlmcp_prod
export ERLMCP_ENV=production

# Step 1: Build production release
echo -e "${YELLOW}1/4 Building production release...${NC}"
docker compose run --rm -e ERLANG_COOKIE=erlmcp_prod erlmcp-build make release
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Production release built successfully${NC}"
else
    echo -e "${RED}✗ Failed to build production release${NC}"
    exit 1
fi

# Step 2: Build Docker image
echo -e "${YELLOW}2/4 Building Docker image...${NC}"
docker build -t erlmcp:3.0.0-prod .
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ Docker image built successfully${NC}"
else
    echo -e "${RED}✗ Failed to build Docker image${NC}"
    exit 1
fi

# Step 3: Run smoke tests
echo -e "${YELLOW}3/4 Running smoke tests...${NC}"
if [ -f scripts/release/smoke-test.sh ]; then
    chmod +x scripts/release/smoke-test.sh
    ./scripts/release/smoke-test.sh erlmcp:3.0.0-prod
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ Smoke tests passed${NC}"
    else
        echo -e "${RED}✗ Smoke tests failed${NC}"
        exit 1
    fi
else
    echo -e "${YELLOW}⚠ Smoke test script not found, skipping...${NC}"
fi

# Step 4: Deploy to production
echo -e "${YELLOW}4/4 Deploying to production...${NC}"
docker-compose up -d
sleep 10

# Verify deployment
echo -e "${YELLOW}Verifying deployment...${NC}"
if curl -f http://localhost:8080/health >/dev/null 2>&1; then
    echo -e "${GREEN}✅ Deployment complete - erlmcp is running${NC}"
else
    echo -e "${RED}❌ Deployment failed - service not responding${NC}"
    exit 1
fi

echo "============================================"
echo -e "${GREEN}🎉 erlmcp v3.0.0 deployed successfully!${NC}"
echo ""
echo "Next steps:"
echo "  1. Check monitoring dashboard at http://localhost:3000"
echo "  2. Review logs with: docker-compose logs -f erlmcp"
echo "  3. Run tests with: docker compose run --rm erlmcp-ct"
echo ""