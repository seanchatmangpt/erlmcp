#!/bin/bash
set -euo pipefail

PURPLE='\033[0;35m'
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${PURPLE}🎯 Quality Gates Demo${NC}\n"

echo -e "${BLUE}Zero Tolerance Rules${NC}"
echo "  ❌ Compilation errors - BLOCKING"
echo "  ❌ Test failures (>5) - BLOCKING"
echo "  ❌ Coverage <80% - BLOCKING"
echo "  ❌ Dialyzer errors - BLOCKING"
echo "  ❌ XREF >10 warnings - BLOCKING"

echo -e "\n${BLUE}Reporting Rules${NC}"
echo "  ⚠️  Dialyzer warnings - REVIEW"
echo "  ⚠️  XREF warnings - REVIEW"
echo "  ⚠️  Performance regression - REVIEW"

echo -e "\n${BLUE}Quality Gate Commands${NC}"
echo "  ./tools/quality-gate-enforcer.sh  # Full check"
echo "  ./tools/claude-md-enforcer.sh     # CLAUDE.md validation"
echo "  rebar3 compile                    # Quick compile"
echo "  rebar3 eunit                      # Run tests"
echo "  rebar3 cover                      # Coverage"

echo -e "\n${BLUE}Pre-commit Integration${NC}"
echo "  ✅ Pre-task.sh runs before Bash commands"
echo "  ✅ Post-task.sh runs after Claude stops"
echo "  ✅ Blocks on critical violations"

echo -e "\n${BLUE}CI/CD Pipeline${NC}"
echo "  • Compile (0 errors required)"
echo "  • Tests (100% pass required)"
echo "  • Coverage (≥80% required)"
echo "  • Type safety (0 errors)"
echo "  • Code quality checks"

echo -e "\n${BLUE}Quality Metrics${NC}"
echo "  📊 Real-time validation"
echo "  📈 Comprehensive reporting"
echo "  🔒 Manufacturing-grade delivery"
echo "  🚀 Production-ready code"

echo -e "\n${GREEN}✅ Quality gate system ready${NC}"
exit 0
