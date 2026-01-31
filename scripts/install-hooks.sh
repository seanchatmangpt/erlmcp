#!/bin/bash
#
# Install erlmcp pre-commit hooks
# Sets up quality gate enforcement on git commits
#

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=========================================="
echo "  ERLMCP PRE-COMMIT HOOK INSTALLER"
echo "=========================================="
echo ""

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Hook path
QUALITY_GATES_HOOK="$PROJECT_ROOT/.git/hooks/erlmcp-quality-gates"
PRE_COMMIT_FILE="$PROJECT_ROOT/.git/hooks/pre-commit"

# Ensure the quality gates hook is executable
echo "Setting up quality gates hook..."
chmod +x "$QUALITY_GATES_HOOK" 2>/dev/null || true

# Backup existing pre-commit if it exists
if [ -f "$PRE_COMMIT_FILE" ] && [ ! -h "$PRE_COMMIT_FILE" ]; then
    BACKUP_FILE="$PRE_COMMIT_FILE.backup.$(date +%Y%m%d_%H%M%S)"
    echo "Backing up existing pre-commit to:"
    echo "  $BACKUP_FILE"
    cp "$PRE_COMMIT_FILE" "$BACKUP_FILE"
fi

# Create new pre-commit that calls quality gates
cat > "$PRE_COMMIT_FILE" << 'INNEREOF'
#!/bin/bash
#
# Pre-commit wrapper for erlmcp quality gates
#

echo "Running pre-commit quality gates..."
bash .git/hooks/erlmcp-quality-gates
EXIT_CODE=$?

if [ $EXIT_CODE -ne 0 ]; then
    echo ""
    echo "Quality gates failed. Commit blocked."
    echo "To bypass (not recommended): git commit --no-verify"
fi

exit $EXIT_CODE
INNEREOF

chmod +x "$PRE_COMMIT_FILE"

echo ""
echo "=========================================="
echo "  INSTALLATION COMPLETE"
echo "=========================================="
echo ""
echo -e "${GREEN}✓ Pre-commit hooks installed${NC}"
echo ""
echo "Installed hooks:"
echo "  - .git/hooks/erlmcp-quality-gates (quality gate implementation)"
echo "  - .git/hooks/pre-commit (wrapper)"
echo ""
echo "Quality gates enforced on every commit:"
echo "  ✓ Compilation must succeed (0 errors) [BLOCKING]"
echo "  ✓ EUnit tests must pass (0 failures) [BLOCKING]"
echo "  ✓ Coverage must be >= 80% [BLOCKING]"
echo "  ✓ Dialyzer warnings checked (non-blocking)"
echo "  ✓ Xref must be clean (non-blocking)"
echo ""
echo "To bypass (not recommended):"
echo "  git commit --no-verify -am \"message\""
echo ""
echo "Test the hook:"
echo "  ./scripts/test-commit-hook.sh"
echo ""

exit 0
