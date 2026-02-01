#!/usr/bin/env bash
# Claude Code Post-Task Validation Hook
# Runs after any agent completes a task
# Executes Erlang-based TCPS quality gates validation
#
# Hook Trigger: After task completion
# Exit 0: Task validation passed, continue
# Exit 1: Validation failed, block task completion

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Hook metadata
HOOK_NAME="post-task-validate"
TASK_ID="${1:-$(date +%s)}"
TASK_DESCRIPTION="${2:-No description}"

log_hook() {
    echo -e "${BLUE}[HOOK:$HOOK_NAME]${NC} $*"
}

log_info() {
    echo -e "${GREEN}[INFO]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Main validation
main() {
    log_hook "Post-Task Validation Hook (Erlang-based)"
    log_hook "Task ID: $TASK_ID"
    log_hook "Description: $TASK_DESCRIPTION"
    echo ""

    cd "$PROJECT_ROOT"

    # Source SessionStart environment for OTP 28
    local env_file="${PROJECT_ROOT}/.erlmcp/env.sh"
    if [[ -f "$env_file" ]]; then
        source "$env_file"
        log_hook "Sourced environment from: $env_file"
    fi

    # Run Erlang-based quality gates validation
    log_hook "Running TCPS quality gates via erlmcp_hooks..."
    echo ""

    # Execute Erlang hook module (standalone - no app startup required)
    erl -noshell -pa _build/default/lib/*/ebin -eval "
        % Try to start tcps_erlmcp (quality gates)
        io:format(\"Attempting to start TCPS quality gates...~n\"),
        case application:ensure_all_started(tcps_erlmcp) of
            {ok, _Apps} ->
                io:format(\"TCPS services started~n~n\"),
                TcpsAvailable = true;
            {error, _StartErr} ->
                io:format(\"TCPS services not available, using basic validation~n~n\"),
                TcpsAvailable = false
        end,

        % Run quality gates validation
        TaskId = <<\"$TASK_ID\">>,

        case TcpsAvailable of
            true ->
                % Full TCPS quality gates
                io:format(\"Running TCPS quality gates for task ~s~n\", [TaskId]),
                io:format(\"==================================================~n\"),
                case tcps_quality_gates:check_all_gates(TaskId) of
                    {ok, Receipts} ->
                        io:format(\"~n✅ ALL QUALITY GATES PASSED~n\"),
                        io:format(\"  Gates passed: ~p~n\", [length(Receipts)]),
                        io:format(\"==================================================~n\"),
                        halt(0);
                    {failed_at, Gate, Violations} ->
                        io:format(\"~n❌ QUALITY GATE FAILED: ~p~n\", [Gate]),
                        io:format(\"  Violations: ~p~n\", [length(Violations)]),
                        io:format(\"~nFirst violation:~n~p~n\", [hd(Violations)]),
                        io:format(\"==================================================~n\"),
                        halt(1)
                end;
            false ->
                % Basic validation (compile + test)
                io:format(\"Running basic validation...~n\"),
                io:format(\"==================================================~n\"),

                % Check compilation
                CompileOutput = os:cmd(\"TERM=dumb rebar3 compile 2>&1\"),
                CompileOk = string:find(CompileOutput, \"error\") == nomatch,

                % Check tests (EUnit + CT)
                EunitOutput = os:cmd(\"rebar3 eunit 2>&1\"),
                CtOutput = os:cmd(\"rebar3 ct 2>&1\"),

                EunitOk = string:find(EunitOutput, \"Failed: 0\") =/= nomatch orelse
                          string:find(EunitOutput, \"Test passed\") =/= nomatch,
                CtOk = string:find(CtOutput, \"FAILED\") == nomatch,

                case CompileOk andalso EunitOk andalso CtOk of
                    true ->
                        io:format(\"~n✅ BASIC VALIDATION PASSED~n\"),
                        io:format(\"  Compilation: OK~n\"),
                        io:format(\"  EUnit: OK~n\"),
                        io:format(\"  Common Test: OK~n\"),
                        io:format(\"==================================================~n\"),
                        halt(0);
                    false ->
                        io:format(\"~n❌ BASIC VALIDATION FAILED~n\"),
                        if
                            not CompileOk -> io:format(\"  Compilation: FAILED~n\");
                            true -> ok
                        end,
                        if
                            not EunitOk -> io:format(\"  EUnit: FAILED~n\");
                            true -> ok
                        end,
                        if
                            not CtOk -> io:format(\"  Common Test: FAILED~n\");
                            true -> ok
                        end,
                        io:format(\"==================================================~n\"),
                        halt(1)
                end
        end.
    " 2>&1

    EXIT_CODE=$?

    echo ""
    if [ $EXIT_CODE -eq 0 ]; then
        log_hook "✅ Post-task validation PASSED"
        log_hook "Task completion approved"
        exit 0
    else
        log_hook "❌ Post-task validation FAILED"
        log_hook "Task completion BLOCKED"
        echo ""
        log_error "RESOLUTION REQUIRED:"
        log_error "1. Review violations reported above"
        log_error "2. Fix compilation errors, test failures, or coverage gaps"
        log_error "3. Re-run: make validate"
        log_error "4. Task will remain incomplete until validation passes"
        echo ""
        exit 1
    fi
}

main "$@"
