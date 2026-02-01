#!/usr/bin/env bash
# receipt_hook_tests.sh - Test suite for .claude/hooks/receipt.sh
#
# Specification: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-005
# Purpose: Verify receipt hook generates valid JSON with correct metadata
# Coverage: JSON validity, structure, timestamp, cost, metadata, quality gates

set -euo pipefail

# Test configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RECEIPT_HOOK="$PROJECT_ROOT/.claude/hooks/receipt.sh"
RECEIPT_DIR="$PROJECT_ROOT/.erlmcp/receipts"
TEST_DIR="$PROJECT_ROOT/.erlmcp/test-receipts"
BUILD_LOG="$PROJECT_ROOT/.erlmcp/build.log"
TEST_LOG="$PROJECT_ROOT/.erlmcp/test.log"

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Helper functions
log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

assert_equals() {
    local expected="$1"
    local actual="$2"
    local message="${3:-Assertion failed}"

    if [ "$expected" = "$actual" ]; then
        return 0
    else
        log_error "$message: expected='$expected' actual='$actual'"
        return 1
    fi
}

assert_not_equals() {
    local expected="$1"
    local actual="$2"
    local message="${3:-Assertion failed}"

    if [ "$expected" != "$actual" ]; then
        return 0
    else
        log_error "$message: expected!='$expected' actual='$actual'"
        return 1
    fi
}

assert_file_exists() {
    local file="$1"
    local message="${2:-File does not exist}"

    if [ -f "$file" ]; then
        return 0
    else
        log_error "$message: $file"
        return 1
    fi
}

assert_contains() {
    local haystack="$1"
    local needle="$2"
    local message="${3:-String not found}"

    if echo "$haystack" | grep -q "$needle"; then
        return 0
    else
        log_error "$message: '$needle' not in '$haystack'"
        return 1
    fi
}

run_test() {
    local test_name="$1"
    local test_func="$2"

    TESTS_RUN=$((TESTS_RUN + 1))
    log_info "Running test: $test_name"

    if $test_func; then
        TESTS_PASSED=$((TESTS_PASSED + 1))
        log_info "✓ PASS: $test_name"
        return 0
    else
        TESTS_FAILED=$((TESTS_FAILED + 1))
        log_error "✗ FAIL: $test_name"
        return 1
    fi
}

# Setup test environment
setup() {
    log_info "Setting up test environment"

    # Create test receipt directory
    mkdir -p "$TEST_DIR"
    mkdir -p "$PROJECT_ROOT/.erlmcp/transcripts"

    # Backup existing receipt directory
    if [ -d "$RECEIPT_DIR" ]; then
        mv "$RECEIPT_DIR" "$RECEIPT_DIR.backup"
    fi

    # Point receipt dir to test dir
    export RECEIPT_DIR="$TEST_DIR"

    # Clean up old test files
    rm -f "$BUILD_LOG" "$TEST_LOG"
}

# Cleanup test environment
cleanup() {
    log_info "Cleaning up test environment"

    # Restore receipt directory
    if [ -d "$RECEIPT_DIR.backup" ]; then
        rm -rf "$RECEIPT_DIR"
        mv "$RECEIPT_DIR.backup" "$RECEIPT_DIR"
    fi

    # Clean up test files
    rm -f "$BUILD_LOG" "$TEST_LOG"
    rm -rf "$TEST_DIR"
}

# Test 1: Script exists and is executable
test_script_exists() {
    assert_file_exists "$RECEIPT_HOOK" "Receipt hook not found"
    [ -x "$RECEIPT_HOOK" ] || {
        log_error "Receipt hook is not executable"
        return 1
    }
    return 0
}

# Test 2: Generates valid JSON
test_json_validity() {
    # Set up environment
    export SESSION_ID="test-session-001"

    # Run receipt hook
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1 || {
        log_error "Receipt hook failed to run"
        return 1
    }

    # Find the generated receipt
    RECEIPT_FILE=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" -type f | sort | tail -1)

    assert_file_exists "$RECEIPT_FILE" "Receipt file not created"

    # Validate JSON with jq
    if command -v jq &> /dev/null; then
        jq empty "$RECEIPT_FILE" 2>/dev/null || {
            log_error "Receipt JSON is invalid"
            cat "$RECEIPT_FILE"
            return 1
        }
    else
        log_warn "jq not available, skipping JSON validation"
    fi

    return 0
}

# Test 3: Contains all required fields
test_receipt_structure() {
    # Set up environment
    export SESSION_ID="test-session-002"

    # Run receipt hook
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1

    # Find the generated receipt
    RECEIPT_FILE=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" -type f | sort | tail -1)

    if ! command -v jq &> /dev/null; then
        log_warn "jq not available, skipping structure test"
        return 0
    fi

    # Check required fields
    local required_fields=(
        "session_id"
        "timestamp"
        "otp_version"
        "erlmcp_version"
        "build_hash"
        "quality_gates"
        "cost_estimate"
        "time_seconds"
    )

    for field in "${required_fields[@]}"; do
        VALUE=$(jq -r ".$field" "$RECEIPT_FILE" 2>/dev/null)
        if [ -z "$VALUE" ] || [ "$VALUE" = "null" ]; then
            log_error "Missing required field: $field"
            return 1
        fi
    done

    # Check quality_gates subfields
    local gate_fields=("compile" "eunit" "ct" "coverage")
    for gate in "${gate_fields[@]}"; do
        VALUE=$(jq -r ".quality_gates.$gate" "$RECEIPT_FILE" 2>/dev/null)
        if [ -z "$VALUE" ] || [ "$VALUE" = "null" ]; then
            log_error "Missing quality gate field: $gate"
            return 1
        fi
    done

    return 0
}

# Test 4: Timestamp format is ISO-8601
test_timestamp_format() {
    # Set up environment
    export SESSION_ID="test-session-003"

    # Run receipt hook
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1

    # Find the generated receipt
    RECEIPT_FILE=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" -type f | sort | tail -1)

    if ! command -v jq &> /dev/null; then
        log_warn "jq not available, skipping timestamp test"
        return 0
    fi

    TIMESTAMP=$(jq -r '.timestamp' "$RECEIPT_FILE")

    # Check ISO-8601 format (YYYY-MM-DDTHH:MM:SS+TZ or YYYY-MM-DDTHH:MM:SSZ)
    if echo "$TIMESTAMP" | grep -qE '^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}'; then
        return 0
    else
        log_error "Timestamp not in ISO-8601 format: $TIMESTAMP"
        return 1
    fi
}

# Test 5: Cost estimate is reasonable
test_cost_calculation() {
    # Set up environment
    export SESSION_ID="test-session-004"

    # Run receipt hook
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1

    # Find the generated receipt
    RECEIPT_FILE=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" -type f | sort | tail -1)

    if ! command -v jq &> /dev/null; then
        log_warn "jq not available, skipping cost test"
        return 0
    fi

    COST=$(jq -r '.cost_estimate' "$RECEIPT_FILE")

    # Cost should be in format $0.XXXX
    if echo "$COST" | grep -qE '^\$0\.[0-9]+$'; then
        # Extract numeric value
        COST_NUM=$(echo "$COST" | sed 's/\$//')

        # Cost should be reasonable (between $0.0001 and $0.5000 for typical sessions)
        if awk "BEGIN {exit !($COST_NUM >= 0.0001 && $COST_NUM <= 0.5000)}"; then
            return 0
        else
            log_error "Cost estimate out of reasonable range: $COST"
            return 1
        fi
    else
        log_error "Cost estimate not in expected format: $COST"
        return 1
    fi
}

# Test 6: Metadata accuracy
test_metadata_accuracy() {
    # Set up environment
    export SESSION_ID="test-session-005"

    # Run receipt hook
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1

    # Find the generated receipt
    RECEIPT_FILE=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" -type f | sort | tail -1)

    if ! command -v jq &> /dev/null; then
        log_warn "jq not available, skipping metadata test"
        return 0
    fi

    # Check session_id
    SESSION_ID_VAL=$(jq -r '.session_id' "$RECEIPT_FILE")
    assert_equals "test-session-005" "$SESSION_ID_VAL" "Session ID mismatch"

    # Check build_hash is valid SHA (40 hex chars or 'unknown')
    BUILD_HASH=$(jq -r '.build_hash' "$RECEIPT_FILE")
    if [ "$BUILD_HASH" != "unknown" ]; then
        if ! echo "$BUILD_HASH" | grep -qE '^[0-9a-f]{40}$'; then
            log_error "Build hash not a valid SHA-1: $BUILD_HASH"
            return 1
        fi
    fi

    # Check erlmcp_version matches app.src
    ERLMCP_VERSION=$(jq -r '.erlmcp_version' "$RECEIPT_FILE")
    if [ -f "$PROJECT_ROOT/apps/erlmcp_core/src/erlmcp_core.app.src" ]; then
        EXPECTED_VERSION=$(grep -oP '(?<=vsn, ")[^"]+' "$PROJECT_ROOT/apps/erlmcp_core/src/erlmcp_core.app.src")
        if [ "$ERLMCP_VERSION" != "unknown" ]; then
            assert_equals "$EXPECTED_VERSION" "$ERLMCP_VERSION" "erlmcp version mismatch"
        fi
    fi

    return 0
}

# Test 7: Quality gate parsing (pass scenario)
test_quality_gates_pass() {
    # Set up environment
    export SESSION_ID="test-session-006"

    # Create mock build log (successful)
    cat > "$BUILD_LOG" <<EOF
Compiling erlmcp_core
Compiling erlmcp_client
Compiling erlmcp_server
Build successful
EOF

    # Create mock test log (successful)
    cat > "$TEST_LOG" <<EOF
Running EUnit tests...
All 42 tests passed
Running CT tests...
All tests successful
Coverage: 85%
EOF

    # Run receipt hook
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1

    # Find the generated receipt
    RECEIPT_FILE=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" -type f | sort | tail -1)

    if ! command -v jq &> /dev/null; then
        log_warn "jq not available, skipping quality gates test"
        return 0
    fi

    # Check quality gates
    COMPILE=$(jq -r '.quality_gates.compile' "$RECEIPT_FILE")
    EUNIT=$(jq -r '.quality_gates.eunit' "$RECEIPT_FILE")
    CT=$(jq -r '.quality_gates.ct' "$RECEIPT_FILE")
    COVERAGE=$(jq -r '.quality_gates.coverage' "$RECEIPT_FILE")

    assert_equals "pass" "$COMPILE" "Compile gate should be pass"
    assert_equals "pass" "$EUNIT" "EUnit gate should be pass"
    assert_equals "pass" "$CT" "CT gate should be pass"
    assert_equals "pass" "$COVERAGE" "Coverage gate should be pass"

    return 0
}

# Test 8: Quality gate parsing (fail scenario)
test_quality_gates_fail() {
    # Set up environment
    export SESSION_ID="test-session-007"

    # Create mock build log (failed)
    cat > "$BUILD_LOG" <<EOF
Compiling erlmcp_core
ERROR: Syntax error in erlmcp_client.erl
Build failed
EOF

    # Create mock test log (failed)
    cat > "$TEST_LOG" <<EOF
Running EUnit tests...
Failed: 5
Coverage: 42%
EOF

    # Run receipt hook
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1

    # Find the generated receipt
    RECEIPT_FILE=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" -type f | sort | tail -1)

    if ! command -v jq &> /dev/null; then
        log_warn "jq not available, skipping quality gates test"
        return 0
    fi

    # Check quality gates
    COMPILE=$(jq -r '.quality_gates.compile' "$RECEIPT_FILE")
    EUNIT=$(jq -r '.quality_gates.eunit' "$RECEIPT_FILE")
    COVERAGE=$(jq -r '.quality_gates.coverage' "$RECEIPT_FILE")

    assert_equals "fail" "$COMPILE" "Compile gate should be fail"
    assert_equals "fail" "$EUNIT" "EUnit gate should be fail"
    assert_equals "fail" "$COVERAGE" "Coverage gate should be fail"

    return 0
}

# Test 9: Transcript archival
test_transcript_archival() {
    # Set up environment
    export SESSION_ID="test-session-008"

    # Create mock transcript
    MOCK_TRANSCRIPT="$PROJECT_ROOT/.erlmcp/mock-transcript.log"
    echo "Test transcript content" > "$MOCK_TRANSCRIPT"
    export TRANSCRIPT_PATH="$MOCK_TRANSCRIPT"

    # Run receipt hook
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1

    # Check if transcript was archived
    ARCHIVED_TRANSCRIPT=$(find "$PROJECT_ROOT/.erlmcp/transcripts" -name "session_${SESSION_ID}_*.log" -type f | head -1)

    if [ -z "$ARCHIVED_TRANSCRIPT" ]; then
        log_error "Transcript was not archived"
        return 1
    fi

    # Verify content matches
    if ! diff -q "$MOCK_TRANSCRIPT" "$ARCHIVED_TRANSCRIPT" >/dev/null 2>&1; then
        log_error "Archived transcript content does not match original"
        return 1
    fi

    # Clean up
    rm -f "$MOCK_TRANSCRIPT" "$ARCHIVED_TRANSCRIPT"

    return 0
}

# Test 10: Idempotent execution
test_idempotent() {
    # Set up environment
    export SESSION_ID="test-session-009"

    # Run receipt hook twice
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1
    sleep 1  # Ensure different timestamp
    cd "$PROJECT_ROOT" && "$RECEIPT_HOOK" >/dev/null 2>&1

    # Count receipts
    RECEIPT_COUNT=$(find "$PROJECT_ROOT/.erlmcp/receipts" -name "*.json" -type f | wc -l)

    if [ "$RECEIPT_COUNT" -lt 2 ]; then
        log_error "Idempotent test failed: expected at least 2 receipts, got $RECEIPT_COUNT"
        return 1
    fi

    return 0
}

# Main test runner
main() {
    log_info "Starting receipt hook test suite"
    log_info "Receipt hook: $RECEIPT_HOOK"

    # Setup
    setup

    # Run tests
    run_test "Script exists and is executable" test_script_exists
    run_test "Generates valid JSON" test_json_validity
    run_test "Contains all required fields" test_receipt_structure
    run_test "Timestamp format is ISO-8601" test_timestamp_format
    run_test "Cost estimate is reasonable" test_cost_calculation
    run_test "Metadata accuracy" test_metadata_accuracy
    run_test "Quality gates (pass scenario)" test_quality_gates_pass
    run_test "Quality gates (fail scenario)" test_quality_gates_fail
    run_test "Transcript archival" test_transcript_archival
    run_test "Idempotent execution" test_idempotent

    # Cleanup
    cleanup

    # Summary
    echo ""
    log_info "========================================="
    log_info "Test Summary"
    log_info "========================================="
    log_info "Tests run:    $TESTS_RUN"
    log_info "Tests passed: $TESTS_PASSED"
    log_info "Tests failed: $TESTS_FAILED"
    log_info "========================================="

    if [ "$TESTS_FAILED" -eq 0 ]; then
        log_info "✓ All tests passed!"
        exit 0
    else
        log_error "✗ Some tests failed"
        exit 1
    fi
}

# Run main if executed directly
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    main "$@"
fi
