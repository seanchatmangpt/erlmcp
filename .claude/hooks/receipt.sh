#!/usr/bin/env bash
# receipt.sh - SessionEnd audit trail hook for erlmcp
#
# Specification: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-005
# Purpose: Generate JSON receipt with session metadata, quality gates, cost estimate
# Trigger: SessionEnd event (automatic at end of cloud session)
# Output: .erlmcp/receipts/<timestamp>.json
#
# JSON Schema:
# {
#   "session_id": "session_...",           // From CLAUDE_SESSION_ID env
#   "timestamp": "2026-02-01T12:34:56Z",  // ISO-8601
#   "otp_version": "28.3.1",              // From erl -noshell
#   "erlmcp_version": "3.0.0",            // From app.src
#   "build_hash": "abc123def456",         // From git rev-parse HEAD
#   "quality_gates": {
#     "compile": "pass|fail|unknown",
#     "eunit": "pass|fail|unknown",
#     "ct": "pass|fail|unknown",
#     "coverage": "pass|fail|unknown"
#   },
#   "cost_estimate": "$0.XX",             // Based on session duration
#   "time_seconds": 480                   // Session duration
# }

set -euo pipefail

# Constants
RECEIPT_DIR=".erlmcp/receipts"
TRANSCRIPT_DIR=".erlmcp/transcripts"
BUILD_LOG=".erlmcp/build.log"
TEST_LOG=".erlmcp/test.log"
APP_SRC="apps/erlmcp_core/src/erlmcp_core.app.src"

# Ensure we're in the project root
if [ ! -f "$APP_SRC" ]; then
    echo "ERROR: Not in erlmcp project root (missing $APP_SRC)" >&2
    exit 1
fi

# Create receipt directory
mkdir -p "$RECEIPT_DIR"
mkdir -p "$TRANSCRIPT_DIR"

# 1. Capture metadata
SESSION_ID="${CLAUDE_SESSION_ID:-${SESSION_ID:-unknown}}"
TIMESTAMP=$(date -Iseconds 2>/dev/null || date +%Y-%m-%dT%H:%M:%S%z)
RECEIPT_TS=$(date +%s 2>/dev/null || echo "$(date +%Y%m%d%H%M%S)")
RECEIPT_FILE="$RECEIPT_DIR/${RECEIPT_TS}.json"

# OTP version - source env file to get OTP 28
OTP_VERSION="unknown"
ERLMCP_ROOT="$(cd "${ERLMCP_ROOT:-.}" && pwd)"
ENV_FILE="${ERLMCP_ROOT}/.erlmcp/env.sh"
OTP_BIN_PATH=""
if [ -f "$ENV_FILE" ]; then
    source "$ENV_FILE"
    # Use explicit OTP bin path from env file
    OTP_BIN_PATH="${ERLMCP_OTP_BIN_PATH:-}"
fi
# Check OTP version using explicit path OR system erl
if [ -n "$OTP_BIN_PATH" ] && [ -x "$OTP_BIN_PATH/erl" ]; then
    OTP_VERSION=$("$OTP_BIN_PATH/erl" -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null || echo "unknown")
elif command -v erl &> /dev/null; then
    OTP_VERSION=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null || echo "unknown")
fi

# erlmcp version (extract from app.src)
ERLMCP_VERSION="unknown"
if [ -f "$APP_SRC" ]; then
    ERLMCP_VERSION=$(grep -oP '(?<=vsn, ")[^"]+' "$APP_SRC" 2>/dev/null || echo "unknown")
fi

# Build hash
BUILD_HASH="unknown"
if git rev-parse --git-dir &> /dev/null; then
    BUILD_HASH=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
fi

# 2. Quality gates (parse logs)
COMPILE_STATUS="unknown"
EUNIT_STATUS="unknown"
CT_STATUS="unknown"
COVERAGE_STATUS="unknown"

# Parse build log for compilation status
if [ -f "$BUILD_LOG" ]; then
    if grep -q "Compiling" "$BUILD_LOG" 2>/dev/null; then
        if grep -qi "error" "$BUILD_LOG" 2>/dev/null; then
            COMPILE_STATUS="fail"
        else
            COMPILE_STATUS="pass"
        fi
    fi
fi

# Parse test log for EUnit status
if [ -f "$TEST_LOG" ]; then
    # EUnit patterns: "All N tests passed" or "Failed: N"
    if grep -q "All.*tests passed" "$TEST_LOG" 2>/dev/null; then
        EUNIT_STATUS="pass"
    elif grep -qi "Failed:.*[1-9]" "$TEST_LOG" 2>/dev/null; then
        EUNIT_STATUS="fail"
    elif grep -q "Test passed" "$TEST_LOG" 2>/dev/null; then
        EUNIT_STATUS="pass"
    fi

    # CT patterns: "All tests successful" or "FAILED"
    if grep -q "All.*tests.*successful" "$TEST_LOG" 2>/dev/null; then
        CT_STATUS="pass"
    elif grep -qi "FAILED" "$TEST_LOG" 2>/dev/null; then
        CT_STATUS="fail"
    fi

    # Coverage patterns: "Coverage: XX%" where XX >= 80
    if grep -oP 'Coverage:\s*\K\d+' "$TEST_LOG" 2>/dev/null | awk '{if ($1 >= 80) exit 0; else exit 1}'; then
        COVERAGE_STATUS="pass"
    elif grep -q "Coverage:" "$TEST_LOG" 2>/dev/null; then
        COVERAGE_STATUS="fail"
    fi
fi

# Alternative: Check rebar3 output files directly
if [ -d "_build/test/cover" ]; then
    # If coverage directory exists and has reports, assume coverage ran
    if [ -n "$(find _build/test/cover -name '*.html' 2>/dev/null)" ]; then
        # Extract coverage percentage from index.html if available
        if [ -f "_build/test/cover/index.html" ]; then
            COV_PCT=$(grep -oP 'total.*?\K\d+(?=%)' _build/test/cover/index.html 2>/dev/null | head -1)
            if [ -n "$COV_PCT" ] && [ "$COV_PCT" -ge 80 ] 2>/dev/null; then
                COVERAGE_STATUS="pass"
            elif [ -n "$COV_PCT" ]; then
                COVERAGE_STATUS="fail"
            fi
        fi
    fi
fi

# 3. Cost estimate and time calculation
# Heuristic: Estimate session duration based on file timestamps
SESSION_START_TIME=""
SESSION_END_TIME=$(date +%s)
SESSION_DURATION=0

# Try to find session start time from .erlmcp/cache or first log entry
if [ -f "$BUILD_LOG" ]; then
    # Get timestamp of first line in build log as proxy for session start
    SESSION_START_TIME=$(stat -c %Y "$BUILD_LOG" 2>/dev/null || stat -f %m "$BUILD_LOG" 2>/dev/null || echo "$SESSION_END_TIME")
    SESSION_DURATION=$((SESSION_END_TIME - SESSION_START_TIME))
fi

# If no logs, estimate a typical session (e.g., 300 seconds = 5 minutes)
if [ "$SESSION_DURATION" -le 0 ]; then
    SESSION_DURATION=300
fi

# Cost calculation: $0.10/hour = $0.00002778/second
COST_PER_SECOND="0.00002778"
COST_RAW=$(awk "BEGIN {printf \"%.4f\", $SESSION_DURATION * $COST_PER_SECOND}")
COST_ESTIMATE="\$${COST_RAW}"

# 4. Generate JSON receipt
cat > "$RECEIPT_FILE" <<EOF
{
  "session_id": "${SESSION_ID}",
  "timestamp": "${TIMESTAMP}",
  "otp_version": "${OTP_VERSION}",
  "erlmcp_version": "${ERLMCP_VERSION}",
  "build_hash": "${BUILD_HASH}",
  "quality_gates": {
    "compile": "${COMPILE_STATUS}",
    "eunit": "${EUNIT_STATUS}",
    "ct": "${CT_STATUS}",
    "coverage": "${COVERAGE_STATUS}"
  },
  "cost_estimate": "${COST_ESTIMATE}",
  "time_seconds": ${SESSION_DURATION}
}
EOF

# 5. Archive transcript (if available)
TRANSCRIPT_PATH="${TRANSCRIPT_PATH:-}"
if [ -n "$TRANSCRIPT_PATH" ] && [ -f "$TRANSCRIPT_PATH" ]; then
    TRANSCRIPT_ARCHIVE="${TRANSCRIPT_DIR}/session_${SESSION_ID}_${RECEIPT_TS}.log"
    cp "$TRANSCRIPT_PATH" "$TRANSCRIPT_ARCHIVE" 2>/dev/null || true
    echo "Transcript archived to: $TRANSCRIPT_ARCHIVE" >&2
fi

# 6. Output summary
echo "Receipt generated: $RECEIPT_FILE" >&2
echo "Session ID: $SESSION_ID" >&2
echo "Duration: ${SESSION_DURATION}s" >&2
echo "Cost estimate: $COST_ESTIMATE" >&2
echo "Quality gates: compile=$COMPILE_STATUS eunit=$EUNIT_STATUS ct=$CT_STATUS coverage=$COVERAGE_STATUS" >&2

# Validate JSON (if jq is available)
if command -v jq &> /dev/null; then
    if jq empty "$RECEIPT_FILE" 2>/dev/null; then
        echo "Receipt JSON valid" >&2
    else
        echo "WARNING: Receipt JSON is invalid!" >&2
        exit 1
    fi
fi

exit 0
