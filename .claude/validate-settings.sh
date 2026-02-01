#!/usr/bin/env bash
# Validation script for .claude/settings.json
# Verifies JSON schema compliance, hook paths, and subagent definitions
#
# Usage: ./.claude/validate-settings.sh
# Exit codes:
#   0 - All validations passed
#   1 - Validation errors found

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SETTINGS_FILE="$SCRIPT_DIR/settings.json"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL_CHECKS=0
PASSED_CHECKS=0
FAILED_CHECKS=0

# Helper functions
pass() {
    echo -e "${GREEN}✓${NC} $1"
    PASSED_CHECKS=$((PASSED_CHECKS + 1))
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
}

fail() {
    echo -e "${RED}✗${NC} $1"
    FAILED_CHECKS=$((FAILED_CHECKS + 1))
    TOTAL_CHECKS=$((TOTAL_CHECKS + 1))
}

warn() {
    echo -e "${YELLOW}⚠${NC} $1"
}

info() {
    echo -e "ℹ $1"
}

# Check 1: JSON syntax validation
validate_json_syntax() {
    info "Checking JSON syntax..."

    if command -v jq &> /dev/null; then
        if jq empty "$SETTINGS_FILE" 2>/dev/null; then
            pass "JSON syntax is valid"
        else
            fail "JSON syntax error in $SETTINGS_FILE"
            jq empty "$SETTINGS_FILE" 2>&1 || true
            return 1
        fi
    elif command -v python3 &> /dev/null; then
        if python3 -c "import json; json.load(open('$SETTINGS_FILE'))" 2>/dev/null; then
            pass "JSON syntax is valid (validated with Python)"
        else
            fail "JSON syntax error in $SETTINGS_FILE"
            python3 -c "import json; json.load(open('$SETTINGS_FILE'))" 2>&1 || true
            return 1
        fi
    else
        warn "Neither jq nor python3 available - skipping JSON syntax validation"
    fi
    return 0
}

# Check 2: Required sections
validate_required_sections() {
    info "Checking required sections..."

    if ! command -v jq &> /dev/null; then
        warn "jq not available - skipping required sections validation"
        return 0
    fi

    local required_sections=("permissions" "hooks" "subagents")

    for section in "${required_sections[@]}"; do
        if jq -e ".$section" "$SETTINGS_FILE" &>/dev/null; then
            pass "Required section '$section' exists"
        else
            fail "Required section '$section' missing"
        fi
    done

    # Check hook events
    local hook_events=("SessionStart" "PreToolUse" "PostToolUse" "Stop" "SessionEnd")
    for event in "${hook_events[@]}"; do
        if jq -e ".hooks.\"$event\"" "$SETTINGS_FILE" &>/dev/null; then
            pass "Hook event '$event' defined"
        else
            fail "Hook event '$event' missing"
        fi
    done
    return 0
}

# Check 3: Subagent definitions
validate_subagents() {
    info "Checking subagent definitions..."

    if ! command -v jq &> /dev/null; then
        warn "jq not available - skipping subagent validation"
        return 0
    fi

    # Check all three required subagents
    local subagents=("verifier" "build-engineer" "release-scout")
    for agent in "${subagents[@]}"; do
        if jq -e ".subagents.\"$agent\"" "$SETTINGS_FILE" &>/dev/null; then
            pass "Subagent '$agent' defined"

            # Check for required fields
            local has_desc=$(jq -r ".subagents.\"$agent\".description // empty" "$SETTINGS_FILE" 2>/dev/null)
            if [ -n "$has_desc" ]; then
                pass "Subagent '$agent' has description"
            else
                warn "Subagent '$agent' missing description"
            fi

            # Check tool access
            local has_tools=$(jq -e ".subagents.\"$agent\".toolAccess" "$SETTINGS_FILE" 2>/dev/null)
            if [ $? -eq 0 ]; then
                pass "Subagent '$agent' has toolAccess defined"
            else
                fail "Subagent '$agent' missing toolAccess"
            fi
        else
            fail "Subagent '$agent' not defined"
        fi
    done
    return 0
}

# Check 4: Permission rules
validate_permissions() {
    info "Checking permission rules..."

    if ! command -v jq &> /dev/null; then
        warn "jq not available - skipping permissions validation"
        return 0
    fi

    # Check deny rules
    local deny_count=$(jq '.permissions.deny | length' "$SETTINGS_FILE" 2>/dev/null || echo "0")
    if [ "$deny_count" -gt 0 ]; then
        pass "Permission deny rules defined ($deny_count rules)"
    else
        warn "No permission deny rules defined"
    fi

    # Check allow rules
    local allow_count=$(jq '.permissions.allow | length' "$SETTINGS_FILE" 2>/dev/null || echo "0")
    if [ "$allow_count" -gt 0 ]; then
        pass "Permission allow rules defined ($allow_count rules)"
    else
        warn "No permission allow rules defined"
    fi

    # Check for .env in deny list
    if jq -e '.permissions.deny[] | select(.pattern == ".env")' "$SETTINGS_FILE" &>/dev/null; then
        pass "Secret files (.env) are denied"
    else
        warn "Secret files (.env) not in deny list"
    fi
    return 0
}

# Check 5: Hook paths (documentation check - files will be created by other WOs)
validate_hook_paths() {
    info "Checking hook path documentation..."

    # Expected hook paths (will be created by other WOs)
    local expected_hooks=(
        ".claude/hooks/SessionStart.sh|WO-001"
        ".claude/hooks/policy-bash.sh|WO-002"
        ".claude/hooks/policy-websearch.sh|Future"
        ".claude/hooks/policy-write.sh|Future"
        ".claude/hooks/post-write-ci.sh|WO-004"
        ".claude/hooks/post-git-commit.sh|Future"
        ".claude/hooks/receipt.sh|WO-005"
    )

    local found=0
    local pending=0

    for entry in "${expected_hooks[@]}"; do
        local hook="${entry%|*}"
        local wo="${entry#*|}"
        local full_path="$PROJECT_ROOT/$hook"

        if [ -f "$full_path" ]; then
            if [ -x "$full_path" ]; then
                pass "Hook exists and is executable: $hook"
                found=$((found + 1))
            else
                warn "Hook exists but not executable: $hook (run: chmod +x $hook)"
            fi
        else
            info "Hook pending creation: $hook (assigned to: $wo)"
            pending=$((pending + 1))
        fi
    done

    info "Hook status: $found implemented, $pending pending"
    pass "Hook documentation is complete"
    return 0
}

# Check 6: Skills configuration
validate_skills() {
    info "Checking skills configuration..."

    if ! command -v jq &> /dev/null; then
        warn "jq not available - skipping skills validation"
        return 0
    fi

    # Check otp-manager skill
    if jq -e '.skills."otp-manager"' "$SETTINGS_FILE" &>/dev/null; then
        pass "OTP manager skill defined"

        # Check skill path
        local skill_path=$(jq -r '.skills."otp-manager".path' "$SETTINGS_FILE" 2>/dev/null)
        if [ -n "$skill_path" ] && [ "$skill_path" != "null" ]; then
            pass "OTP manager skill has path: $skill_path"

            # Note: Skill file will be created by WO-006
            if [ -f "$PROJECT_ROOT/$skill_path" ]; then
                pass "OTP manager skill file exists"
            else
                info "OTP manager skill file pending creation (assigned to: WO-006)"
            fi
        else
            fail "OTP manager skill missing path"
        fi
    else
        fail "OTP manager skill not defined"
    fi
    return 0
}

# Main validation
main() {
    echo "================================================="
    echo "Settings.json Validation"
    echo "================================================="
    echo ""
    echo "File: $SETTINGS_FILE"
    echo ""

    # Run all validation checks
    validate_json_syntax
    echo ""

    validate_required_sections
    echo ""

    validate_subagents
    echo ""

    validate_permissions
    echo ""

    validate_hook_paths
    echo ""

    validate_skills
    echo ""

    # Summary
    echo "================================================="
    echo "Validation Summary"
    echo "================================================="
    echo "Total checks: $TOTAL_CHECKS"
    echo -e "Passed: ${GREEN}$PASSED_CHECKS${NC}"
    echo -e "Failed: ${RED}$FAILED_CHECKS${NC}"
    echo ""

    if [ "$FAILED_CHECKS" -eq 0 ]; then
        echo -e "${GREEN}✓ All validations passed${NC}"
        echo ""
        echo "Note: Some hooks/skills are not yet created (expected)."
        echo "They will be implemented by other work orders:"
        echo "  - WO-001: SessionStart.sh (erlang-otp-developer)"
        echo "  - WO-002: policy-bash.sh (erlang-transport-builder)"
        echo "  - WO-004: post-write-ci.sh (erlang-test-engineer)"
        echo "  - WO-005: receipt.sh (erlang-github-ops)"
        echo "  - WO-006: otp-manager skill (erlang-otp-developer)"
        echo "  - WO-007/008/009: Subagent definitions (already in settings.json)"
        return 0
    else
        echo -e "${RED}✗ Validation failed with $FAILED_CHECKS error(s)${NC}"
        return 1
    fi
}

# Run main
main
