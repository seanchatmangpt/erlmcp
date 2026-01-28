#!/usr/bin/env bash
# ============================================================================
# ERLMCP HOOK INSTALLER
# ============================================================================
# Installs git hooks for quality gate enforcement.
#
# Usage:
#   ./tools/install-hooks.sh
# ============================================================================

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo ""
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo -e "${BLUE}   ERLMCP Git Hooks Installer                          ${NC}"
echo -e "${BLUE}═══════════════════════════════════════════════════════${NC}"
echo ""

# Get repository root
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
HOOKS_DIR="$REPO_ROOT/.git/hooks"
TOOLS_DIR="$REPO_ROOT/tools"

# Check if we're in a git repository
if [ ! -d "$HOOKS_DIR" ]; then
    echo -e "${YELLOW}Warning: .git/hooks directory not found${NC}"
    echo "Are you in a git repository?"
    exit 1
fi

# Check if quality-gate.sh exists
if [ ! -f "$TOOLS_DIR/quality-gate.sh" ]; then
    echo -e "${YELLOW}Warning: $TOOLS_DIR/quality-gate.sh not found${NC}"
    echo "Please ensure quality-gate.sh exists before installing hooks."
    exit 1
fi

# Make quality-gate.sh executable
chmod +x "$TOOLS_DIR/quality-gate.sh"
echo -e "${GREEN}✓${NC} Made quality-gate.sh executable"

# Install pre-commit hook
if [ -f "$HOOKS_DIR/pre-commit" ]; then
    # Backup existing hook
    BACKUP="$HOOKS_DIR/pre-commit.backup.$(date +%Y%m%d_%H%M%S)"
    cp "$HOOKS_DIR/pre-commit" "$BACKUP"
    echo -e "${YELLOW}⚠${NC} Backed up existing pre-commit hook to: $BACKUP"
fi

# The pre-commit hook should already exist, just make it executable
if [ -f "$HOOKS_DIR/pre-commit" ]; then
    chmod +x "$HOOKS_DIR/pre-commit"
    echo -e "${GREEN}✓${NC} Installed pre-commit hook"
else
    echo -e "${YELLOW}⚠${NC} pre-commit hook not found in $HOOKS_DIR"
fi

# Install pre-push hook
if [ -f "$HOOKS_DIR/pre-push" ]; then
    # Backup existing hook
    BACKUP="$HOOKS_DIR/pre-push.backup.$(date +%Y%m%d_%H%M%S)"
    cp "$HOOKS_DIR/pre-push" "$BACKUP"
    echo -e "${YELLOW}⚠${NC} Backed up existing pre-push hook to: $BACKUP"
fi

# The pre-push hook should already exist, just make it executable
if [ -f "$HOOKS_DIR/pre-push" ]; then
    chmod +x "$HOOKS_DIR/pre-push"
    echo -e "${GREEN}✓${NC} Installed pre-push hook"
else
    echo -e "${YELLOW}⚠${NC} pre-push hook not found in $HOOKS_DIR"
fi

echo ""
echo -e "${GREEN}═══════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}   Installation Complete                               ${NC}"
echo -e "${GREEN}═══════════════════════════════════════════════════════${NC}"
echo ""
echo "Installed hooks:"
echo "  - pre-commit: Quality gate (compile, tests, dialyzer, xref)"
echo "  - pre-push:   Performance regression check"
echo ""
echo "To test hooks manually:"
echo "  ./tools/quality-gate.sh"
echo ""
echo "To bypass hooks (NOT recommended):"
echo "  git commit --no-verify"
echo "  git push --no-verify"
echo ""
echo "For more information:"
echo "  cat docs/quality-gates/PRE_COMMIT_HOOKS.md"
echo ""
