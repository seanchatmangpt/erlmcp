#!/usr/bin/env bash
# Test suite for policy-bash.sh
# WO-002: Policy-Bash Hook Implementation
# Coverage: 25+ test cases

set -euo pipefail

HOOK_SCRIPT="/home/user/erlmcp/.claude/hooks/policy-bash.sh"
TESTS_PASSED=0
TESTS_FAILED=0
TOTAL_TESTS=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test helper: run hook with JSON input and check decision
test_hook() {
    local test_name="$1"
    local input_json="$2"
    local expected_decision="$3"
    local description="$4"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    # Run hook and capture output
    local output
    output=$(echo "$input_json" | "$HOOK_SCRIPT" 2>&1)

    # Extract decision (with or without jq)
    local actual_decision
    if command -v jq &> /dev/null; then
        actual_decision=$(echo "$output" | jq -r '.permissionDecision // "ERROR"')
    else
        actual_decision=$(echo "$output" | grep -oE '"permissionDecision"[[:space:]]*:[[:space:]]*"[^"]*"' | sed 's/.*"\([^"]*\)"/\1/')
    fi

    # Check result
    if [ "$actual_decision" = "$expected_decision" ]; then
        echo -e "${GREEN}✓${NC} Test $TOTAL_TESTS: $test_name - $description"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}✗${NC} Test $TOTAL_TESTS: $test_name - $description"
        echo "  Expected: $expected_decision"
        echo "  Got: $actual_decision"
        echo "  Output: $output"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

echo "========================================"
echo "Policy-Bash Hook Test Suite"
echo "========================================"
echo ""

# ========================================
# Test Group 1: Allowlisted Build Commands
# ========================================
echo "Test Group 1: Allowlisted Build Commands"
echo "----------------------------------------"

test_hook \
    "allow_erl_version" \
    '{"tool": "Bash", "command": "erl -version"}' \
    "allow" \
    "erl command should be allowed"

test_hook \
    "allow_erl_noshell" \
    '{"tool": "Bash", "command": "erl -noshell -eval \"io:format(\\\"test\\\")\" -s init stop"}' \
    "allow" \
    "erl -noshell should be allowed"

test_hook \
    "allow_rebar3_compile" \
    '{"tool": "Bash", "command": "rebar3 compile"}' \
    "allow" \
    "rebar3 compile should be allowed"

test_hook \
    "allow_rebar3_eunit" \
    '{"tool": "Bash", "command": "rebar3 eunit --module=erlmcp_client_tests"}' \
    "allow" \
    "rebar3 eunit should be allowed"

test_hook \
    "allow_make_check" \
    '{"tool": "Bash", "command": "make check"}' \
    "allow" \
    "make command should be allowed"

test_hook \
    "allow_git_status" \
    '{"tool": "Bash", "command": "git status"}' \
    "allow" \
    "git status should be allowed"

test_hook \
    "allow_term_rebar3" \
    '{"tool": "Bash", "command": "TERM=dumb rebar3 compile"}' \
    "allow" \
    "TERM= prefix should be allowed"

# ========================================
# Test Group 2: Network - Allowlisted Domains
# ========================================
echo ""
echo "Test Group 2: Network - Allowlisted Domains"
echo "-------------------------------------------"

test_hook \
    "allow_wget_github" \
    '{"tool": "Bash", "command": "wget https://github.com/erlang/otp/releases/download/OTP-28.3.1/otp_src_28.3.1.tar.gz"}' \
    "allow" \
    "wget from github.com should be allowed"

test_hook \
    "allow_curl_hex" \
    '{"tool": "Bash", "command": "curl https://hex.pm/api/packages/cowboy"}' \
    "allow" \
    "curl to hex.pm should be allowed"

test_hook \
    "allow_wget_erlang_solutions" \
    '{"tool": "Bash", "command": "wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb"}' \
    "allow" \
    "wget from erlang-solutions.com should be allowed"

test_hook \
    "allow_git_clone_github" \
    '{"tool": "Bash", "command": "git clone https://github.com/erlang/otp.git"}' \
    "allow" \
    "git clone from github.com should be allowed"

test_hook \
    "allow_git_push_github" \
    '{"tool": "Bash", "command": "git push origin main"}' \
    "allow" \
    "git push should be allowed (assumes github.com origin)"

# ========================================
# Test Group 3: Network - Unknown Domains (Ask)
# ========================================
echo ""
echo "Test Group 3: Network - Unknown Domains (Ask)"
echo "---------------------------------------------"

test_hook \
    "ask_wget_unknown" \
    '{"tool": "Bash", "command": "wget https://example.com/file.tar.gz"}' \
    "ask" \
    "wget from unknown domain should ask"

test_hook \
    "ask_curl_unknown" \
    '{"tool": "Bash", "command": "curl https://api.example.com/data"}' \
    "ask" \
    "curl to unknown domain should ask"

test_hook \
    "ask_git_clone_unknown" \
    '{"tool": "Bash", "command": "git clone https://gitlab.com/user/repo.git"}' \
    "ask" \
    "git clone from unknown domain should ask"

# ========================================
# Test Group 4: Dangerous Commands (Deny)
# ========================================
echo ""
echo "Test Group 4: Dangerous Commands (Deny)"
echo "---------------------------------------"

test_hook \
    "deny_sudo" \
    '{"tool": "Bash", "command": "sudo apt-get install erlang"}' \
    "deny" \
    "sudo should be denied"

test_hook \
    "deny_rm_rf" \
    '{"tool": "Bash", "command": "rm -rf /tmp/test"}' \
    "deny" \
    "rm -rf should be denied"

test_hook \
    "deny_rm_fr" \
    '{"tool": "Bash", "command": "rm -fr /tmp/test"}' \
    "deny" \
    "rm -fr should be denied"

test_hook \
    "deny_rm_rf_nospace" \
    '{"tool": "Bash", "command": "rm -rf/tmp/test"}' \
    "deny" \
    "rm -rf (no space) should be denied"

test_hook \
    "deny_dd_destructive" \
    '{"tool": "Bash", "command": "dd if=/dev/zero of=/dev/sda"}' \
    "deny" \
    "dd to /dev/sda should be denied"

# ========================================
# Test Group 5: Non-Deterministic Commands (Deny)
# ========================================
echo ""
echo "Test Group 5: Non-Deterministic Commands (Deny)"
echo "-----------------------------------------------"

test_hook \
    "deny_date" \
    '{"tool": "Bash", "command": "date +%s"}' \
    "deny" \
    "date should be denied (non-deterministic)"

test_hook \
    "deny_uname" \
    '{"tool": "Bash", "command": "uname -a"}' \
    "deny" \
    "uname should be denied (non-deterministic)"

test_hook \
    "deny_whoami" \
    '{"tool": "Bash", "command": "whoami"}' \
    "deny" \
    "whoami should be denied (non-deterministic)"

test_hook \
    "deny_hostname" \
    '{"tool": "Bash", "command": "hostname"}' \
    "deny" \
    "hostname should be denied (non-deterministic)"

# ========================================
# Test Group 6: Edge Cases
# ========================================
echo ""
echo "Test Group 6: Edge Cases"
echo "-----------------------"

test_hook \
    "allow_non_bash_tool" \
    '{"tool": "Read", "input": {"file_path": "/home/user/erlmcp/README.md"}}' \
    "allow" \
    "Non-Bash tool should be allowed"

test_hook \
    "deny_empty_command" \
    '{"tool": "Bash", "command": ""}' \
    "deny" \
    "Empty command should be denied"

test_hook \
    "deny_empty_input" \
    '' \
    "deny" \
    "Empty input should be denied"

test_hook \
    "ask_unknown_command" \
    '{"tool": "Bash", "command": "some_unknown_binary --flag"}' \
    "ask" \
    "Unknown command should ask"

# ========================================
# Test Group 7: Input Variations (JSON Format)
# ========================================
echo ""
echo "Test Group 7: Input Variations (JSON Format)"
echo "--------------------------------------------"

test_hook \
    "allow_nested_input_command" \
    '{"tool": "Bash", "input": {"command": "erl -version"}}' \
    "allow" \
    "Nested input.command format should work"

test_hook \
    "allow_name_field" \
    '{"name": "Bash", "command": "rebar3 compile"}' \
    "allow" \
    "name field instead of tool should work"

# ========================================
# Summary
# ========================================
echo ""
echo "========================================"
echo "Test Summary"
echo "========================================"
echo "Total tests: $TOTAL_TESTS"
echo -e "Passed: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Failed: ${RED}$TESTS_FAILED${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "\n${RED}Some tests failed.${NC}"
    exit 1
fi
