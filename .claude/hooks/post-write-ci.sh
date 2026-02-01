#!/usr/bin/env bash
#
# post-write-ci.sh - Async CI triggering hook
#
# Trigger: PostToolUse event (Write or Edit tool on .erl/.hrl files)
# Behavior: Non-blocking async execution of compile + tests
# Logs: .erlmcp/build.log and .erlmcp/test.log (rotated at 10MB)
#
# Reference: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-004
# Reference: CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md:226-254

set -euo pipefail

# ============================================================================
# Configuration
# ============================================================================

PROJECT_DIR="${ERLMCP_ROOT:-${CLAUDE_PROJECT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}}"
LOG_DIR="$PROJECT_DIR/.erlmcp"
BUILD_LOG="$LOG_DIR/build.log"
TEST_LOG="$LOG_DIR/test.log"
MAX_LOG_SIZE=$((10 * 1024 * 1024))  # 10MB
KEEP_ROTATIONS=5
TIMEOUT_SECONDS=120

# ============================================================================
# Log Rotation
# ============================================================================

rotate_log() {
    local logfile="$1"

    # Check if log exists and exceeds max size
    if [[ -f "$logfile" ]] && [[ $(stat -c%s "$logfile" 2>/dev/null || echo 0) -gt $MAX_LOG_SIZE ]]; then
        # Rotate existing backups
        for i in $(seq $((KEEP_ROTATIONS - 1)) -1 1); do
            if [[ -f "${logfile}.$i" ]]; then
                mv "${logfile}.$i" "${logfile}.$((i + 1))"
            fi
        done

        # Move current log to .1
        mv "$logfile" "${logfile}.1"

        # Remove oldest if exceeds keep limit
        if [[ -f "${logfile}.$((KEEP_ROTATIONS + 1))" ]]; then
            rm -f "${logfile}.$((KEEP_ROTATIONS + 1))"
        fi
    fi
}

# ============================================================================
# Event Handler (Main Entry Point)
# ============================================================================

# Parse PostToolUse event from environment variables
# Expected: TOOL (Write/Edit), SUBJECT (file path)
TOOL="${TOOL:-}"
SUBJECT="${SUBJECT:-}"

# Only process Write and Edit operations
if [[ "$TOOL" == "Write" || "$TOOL" == "Edit" ]]; then
    # Check if file is Erlang source (.erl, .hrl, .app.src)
    if [[ "$SUBJECT" =~ \.(erl|hrl|app\.src)$ ]]; then
        # Spawn async background process (non-blocking)
        # Create inline script with all variables embedded
        (
            # Inherit configuration from parent
            export PROJECT_DIR="$PROJECT_DIR"
            export LOG_DIR="$LOG_DIR"
            export BUILD_LOG="$BUILD_LOG"
            export TEST_LOG="$TEST_LOG"
            export MAX_LOG_SIZE="$MAX_LOG_SIZE"
            export KEEP_ROTATIONS="$KEEP_ROTATIONS"
            export TIMEOUT_SECONDS="$TIMEOUT_SECONDS"
            export MODIFIED_FILE="$SUBJECT"

            # Background CI execution
            {
                # Rotate logs function
                rotate_log_inline() {
                    local logfile="$1"
                    if [[ -f "$logfile" ]] && [[ $(stat -c%s "$logfile" 2>/dev/null || echo 0) -gt $MAX_LOG_SIZE ]]; then
                        for i in $(seq $((KEEP_ROTATIONS - 1)) -1 1); do
                            if [[ -f "${logfile}.$i" ]]; then
                                mv "${logfile}.$i" "${logfile}.$((i + 1))"
                            fi
                        done
                        mv "$logfile" "${logfile}.1"
                        if [[ -f "${logfile}.$((KEEP_ROTATIONS + 1))" ]]; then
                            rm -f "${logfile}.$((KEEP_ROTATIONS + 1))"
                        fi
                    fi
                }

                # Rotate logs before writing
                rotate_log_inline "$BUILD_LOG"
                rotate_log_inline "$TEST_LOG"

                # Ensure log directory exists
                mkdir -p "$LOG_DIR"

                # Extract module name from file path
                module_name=$(basename "$MODIFIED_FILE" | sed 's/\.\(erl\|hrl\)$//')

                # Timestamp
                timestamp=$(date '+%Y-%m-%d %H:%M:%S')

                # Log start
                {
                    echo "========================================================================"
                    echo "[$timestamp] CI triggered by: $MODIFIED_FILE"
                    echo "========================================================================"
                } >> "$BUILD_LOG"

                # Run compilation
                echo "[$timestamp] Running: rebar3 compile" >> "$BUILD_LOG"
                cd "$PROJECT_DIR" || exit 2

                # Source SessionStart environment for OTP 28
                local env_file="${PROJECT_DIR}/.erlmcp/env.sh"
                if [[ -f "$env_file" ]]; then
                    source "$env_file"
                fi

                if TERM=dumb timeout "$TIMEOUT_SECONDS" rebar3 compile >> "$BUILD_LOG" 2>&1; then
                    compile_status="SUCCESS"
                    echo "[$timestamp] Compile: SUCCESS" >> "$BUILD_LOG"

                    # Run tests if compilation succeeded
                    {
                        echo "========================================================================"
                        echo "[$timestamp] Test run triggered by: $MODIFIED_FILE"
                        echo "========================================================================"
                    } >> "$TEST_LOG"

                    # Determine test module name (convention: <module>_tests)
                    test_module="${module_name}_tests"

                    echo "[$timestamp] Running: rebar3 eunit --module=$test_module" >> "$TEST_LOG"

                    # Run tests (allow failure, don't block)
                    if timeout "$TIMEOUT_SECONDS" rebar3 eunit --module="$test_module" >> "$TEST_LOG" 2>&1; then
                        test_status="SUCCESS"
                        echo "[$timestamp] Tests: SUCCESS" >> "$TEST_LOG"
                    else
                        test_status="FAILED"
                        echo "[$timestamp] Tests: FAILED (or no tests found)" >> "$TEST_LOG"
                    fi
                else
                    compile_status="FAILED"
                    echo "[$timestamp] Compile: FAILED" >> "$BUILD_LOG"
                    test_status="SKIPPED"
                fi

                # Summary to build log
                {
                    echo "========================================================================"
                    echo "[$timestamp] CI Summary"
                    echo "  File: $MODIFIED_FILE"
                    echo "  Compile: $compile_status"
                    echo "  Tests: $test_status"
                    echo "========================================================================"
                    echo ""
                } >> "$BUILD_LOG"

            } &  # Run in background

            # Exit immediately
            exit 0

        ) >/dev/null 2>&1 &

        # Disown the background process
        disown 2>/dev/null || true

        # Return immediately (hook completes instantly)
        exit 0
    fi
fi

# If we reach here, event was not for Erlang files - exit cleanly
exit 0
