#!/bin/bash

###################################################################
# Comprehensive Integration Test Execution Script
# 
# This script orchestrates the execution of all integration tests
# across multiple dimensions:
# 
# 1. Complete system integration tests
# 2. Multi-transport coordination tests  
# 3. Advanced load and stress tests
# 4. Configuration-driven scenarios
# 5. Real-world workflow simulations
# 6. Performance regression detection
#
# Usage:
#   ./run_comprehensive_integration_tests.sh [MODE] [OPTIONS]
#
# Modes:
#   --comprehensive  : Run all integration tests (default)
#   --quick         : Run essential validation tests only
#   --load-stress   : Run load and stress tests only
#   --multi-transport: Run multi-transport coordination tests
#   --orchestrated  : Use test orchestrator for coordination
#
# Options:
#   --parallel      : Enable parallel test execution
#   --coverage      : Generate coverage reports
#   --performance   : Enable performance monitoring
#   --agents        : Use coordinated test agents
#   --continuous    : Continuous integration mode
#   --debug         : Enable debug logging
#
###################################################################

set -e

# Script configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
RESULTS_DIR="$PROJECT_ROOT/_test_results"
COVERAGE_DIR="$PROJECT_ROOT/_coverage"
LOG_FILE="$RESULTS_DIR/integration_test_execution.log"

# Test execution modes
MODE="comprehensive"
PARALLEL_EXECUTION=false
COVERAGE_ENABLED=false
PERFORMANCE_MONITORING=false
AGENT_COORDINATION=false
CONTINUOUS_MODE=false
DEBUG_MODE=false

# Test suite definitions
COMPREHENSIVE_SUITES=(
    "erlmcp_comprehensive_integration_SUITE"
    "erlmcp_multi_transport_coordination_SUITE" 
    "erlmcp_advanced_load_stress_SUITE"
)

QUICK_VALIDATION_SUITES=(
    "erlmcp_integration_SUITE"
    "erlmcp_transport_behavior_SUITE"
)

LOAD_STRESS_SUITES=(
    "erlmcp_advanced_load_stress_SUITE"
    "erlmcp_load_SUITE"
)

MULTI_TRANSPORT_SUITES=(
    "erlmcp_multi_transport_coordination_SUITE"
    "erlmcp_transport_behavior_SUITE"
)

###################################################################
# Utility Functions
###################################################################

log() {
    local level="$1"
    shift
    local message="$*"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

log_info() {
    log "INFO" "$@"
}

log_error() {
    log "ERROR" "$@"
}

log_debug() {
    if [ "$DEBUG_MODE" = true ]; then
        log "DEBUG" "$@"
    fi
}

print_banner() {
    echo ""
    echo "=============================================================="
    echo "    ERLMCP COMPREHENSIVE INTEGRATION TEST ORCHESTRATION"
    echo "=============================================================="
    echo "Mode: $MODE"
    echo "Parallel Execution: $PARALLEL_EXECUTION"
    echo "Coverage Analysis: $COVERAGE_ENABLED" 
    echo "Performance Monitoring: $PERFORMANCE_MONITORING"
    echo "Agent Coordination: $AGENT_COORDINATION"
    echo "=============================================================="
    echo ""
}

setup_test_environment() {
    log_info "Setting up comprehensive test environment"
    
    # Create results directories
    mkdir -p "$RESULTS_DIR"
    mkdir -p "$COVERAGE_DIR"
    
    # Initialize log file
    echo "=== ErlMCP Comprehensive Integration Test Execution Log ===" > "$LOG_FILE"
    echo "Started at: $(date)" >> "$LOG_FILE"
    echo "" >> "$LOG_FILE"
    
    # Ensure project is compiled
    log_info "Compiling project"
    cd "$PROJECT_ROOT"
    if ! rebar3 compile; then
        log_error "Project compilation failed"
        exit 1
    fi
    
    # Compile test suites
    log_info "Compiling test suites"
    if ! rebar3 ct --compile_only; then
        log_error "Test suite compilation failed"
        exit 1
    fi
    
    log_info "Test environment setup completed"
}

initialize_coordination_agents() {
    if [ "$AGENT_COORDINATION" = true ]; then
        log_info "Initializing test coordination agents"
        
        # Use Claude Flow coordination hooks if available
        if command -v npx >/dev/null 2>&1; then
            log_debug "Starting coordination hooks"
            npx claude-flow@alpha hooks pre-task --description "comprehensive-integration-testing" || true
        fi
        
        # Initialize swarm coordination if available
        if [ -f "$PROJECT_ROOT/claude-flow" ]; then
            log_debug "Initializing test swarm coordination"
            "$PROJECT_ROOT/claude-flow" swarm init hierarchical --max-agents 8 || true
        fi
        
        log_info "Agent coordination initialized"
    fi
}

execute_test_suites() {
    local -n suites_ref=$1
    local execution_mode="$2"
    
    log_info "Executing ${#suites_ref[@]} test suites in $execution_mode mode"
    
    local ct_options=""
    
    # Configure Common Test options
    if [ "$PARALLEL_EXECUTION" = true ]; then
        ct_options="$ct_options --multiply_timetraps 2"
    fi
    
    if [ "$COVERAGE_ENABLED" = true ]; then
        ct_options="$ct_options --cover --cover_export_name integration_tests"
    fi
    
    if [ "$DEBUG_MODE" = true ]; then
        ct_options="$ct_options --verbosity 100"
    fi
    
    # Execute test suites
    local all_passed=true
    local suite_results=()
    
    for suite in "${suites_ref[@]}"; do
        log_info "Executing test suite: $suite"
        
        local suite_start_time=$(date +%s)
        
        if [ "$PARALLEL_EXECUTION" = true ] && [ "$execution_mode" != "load_stress" ]; then
            # Run in background for parallel execution (except load tests)
            rebar3 ct --suite "test/${suite}.erl" $ct_options \
                --logdir "$RESULTS_DIR/$suite" \
                > "$RESULTS_DIR/${suite}_output.log" 2>&1 &
            local suite_pid=$!
            suite_results+=("$suite:$suite_pid:$suite_start_time")
            log_debug "Started $suite in background (PID: $suite_pid)"
        else
            # Sequential execution
            if rebar3 ct --suite "test/${suite}.erl" $ct_options \
                --logdir "$RESULTS_DIR/$suite" \
                > "$RESULTS_DIR/${suite}_output.log" 2>&1; then
                local suite_end_time=$(date +%s)
                local suite_duration=$((suite_end_time - suite_start_time))
                log_info "✓ $suite completed successfully in ${suite_duration}s"
            else
                local suite_end_time=$(date +%s)
                local suite_duration=$((suite_end_time - suite_start_time))
                log_error "✗ $suite failed after ${suite_duration}s"
                all_passed=false
            fi
        fi
    done
    
    # Wait for parallel executions to complete
    if [ "$PARALLEL_EXECUTION" = true ] && [ ${#suite_results[@]} -gt 0 ]; then
        log_info "Waiting for ${#suite_results[@]} parallel test suites to complete"
        
        for suite_result in "${suite_results[@]}"; do
            IFS=':' read -r suite_name suite_pid suite_start_time <<< "$suite_result"
            
            if wait $suite_pid; then
                local suite_end_time=$(date +%s)
                local suite_duration=$((suite_end_time - suite_start_time))
                log_info "✓ $suite_name completed successfully in ${suite_duration}s"
            else
                local suite_end_time=$(date +%s)
                local suite_duration=$((suite_end_time - suite_start_time))
                log_error "✗ $suite_name failed after ${suite_duration}s"
                all_passed=false
            fi
        done
    fi
    
    return $([ "$all_passed" = true ] && echo 0 || echo 1)
}

run_orchestrated_tests() {
    log_info "Running orchestrated integration tests using test coordinator"
    
    cd "$PROJECT_ROOT"
    
    # Compile the orchestrator
    erlc -I include -I _build/default/lib/*/include -o test test/erlmcp_integration_test_orchestrator.erl
    
    # Execute orchestrated tests based on mode
    local orchestrator_command=""
    
    case "$MODE" in
        "comprehensive")
            orchestrator_command="erlmcp_integration_test_orchestrator:run_comprehensive_tests()"
            ;;
        "quick")
            orchestrator_command="erlmcp_integration_test_orchestrator:run_quick_validation()"
            ;;
        "load-stress")
            orchestrator_command="erlmcp_integration_test_orchestrator:run_load_stress_tests()"
            ;;
        "multi-transport")
            orchestrator_command="erlmcp_integration_test_orchestrator:run_configuration_scenarios()"
            ;;
    esac
    
    log_info "Executing orchestrated command: $orchestrator_command"
    
    # Run the orchestrated tests
    erl -pa _build/default/lib/*/ebin -pa test -noshell -eval "
try
    application:start(sasl),
    application:start(jsx),
    application:start(erlmcp),
    Result = $orchestrator_command,
    io:format(\"~nOrchestrated test result: ~p~n\", [Result]),
    case Result of
        {ok, _} -> halt(0);
        _ -> halt(1)
    end
catch
    Class:Error:Stack ->
        io:format(\"Orchestrated test error: ~p:~p~n~p~n\", [Class, Error, Stack]),
        halt(1)
end."
}

analyze_test_results() {
    log_info "Analyzing comprehensive test results"
    
    local total_suites=0
    local passed_suites=0
    local failed_suites=0
    
    # Count test results
    for result_dir in "$RESULTS_DIR"/*; do
        if [ -d "$result_dir" ]; then
            total_suites=$((total_suites + 1))
            
            # Check for test success indicators
            if find "$result_dir" -name "*.html" -exec grep -l "All .* tests passed" {} \; | head -1 > /dev/null; then
                passed_suites=$((passed_suites + 1))
            else
                failed_suites=$((failed_suites + 1))
            fi
        fi
    done
    
    # Generate summary
    log_info "=== TEST EXECUTION SUMMARY ==="
    log_info "Total test suites executed: $total_suites"
    log_info "Passed test suites: $passed_suites" 
    log_info "Failed test suites: $failed_suites"
    
    if [ $failed_suites -eq 0 ]; then
        log_info "🎉 ALL INTEGRATION TESTS PASSED!"
        return 0
    else
        log_error "❌ $failed_suites test suite(s) failed"
        return 1
    fi
}

generate_coverage_report() {
    if [ "$COVERAGE_ENABLED" = true ]; then
        log_info "Generating coverage report"
        
        cd "$PROJECT_ROOT"
        
        # Generate coverage HTML report
        rebar3 cover --verbose 2>&1 | tee "$RESULTS_DIR/coverage_generation.log"
        
        # Move coverage files to results directory
        if [ -d "_build/test/cover" ]; then
            cp -r _build/test/cover/* "$COVERAGE_DIR/" 2>/dev/null || true
        fi
        
        log_info "Coverage report generated in $COVERAGE_DIR"
    fi
}

performance_monitoring() {
    if [ "$PERFORMANCE_MONITORING" = true ]; then
        log_info "Collecting performance metrics"
        
        # Collect system resource usage during tests
        {
            echo "=== System Performance During Integration Tests ==="
            echo "Memory Usage:"
            free -h 2>/dev/null || echo "Memory info not available"
            echo ""
            echo "Disk Usage:"
            df -h "$PROJECT_ROOT" 2>/dev/null || echo "Disk info not available"
            echo ""
            echo "Load Average:"
            uptime 2>/dev/null || echo "Load average not available"
            echo ""
        } > "$RESULTS_DIR/performance_metrics.txt"
    fi
}

cleanup_coordination_agents() {
    if [ "$AGENT_COORDINATION" = true ]; then
        log_info "Cleaning up coordination agents"
        
        # Cleanup Claude Flow hooks if available
        if command -v npx >/dev/null 2>&1; then
            npx claude-flow@alpha hooks post-task --task-id "comprehensive-integration-testing" || true
        fi
        
        log_info "Agent coordination cleaned up"
    fi
}

###################################################################
# Main Execution Logic
###################################################################

parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --comprehensive)
                MODE="comprehensive"
                shift
                ;;
            --quick)
                MODE="quick"
                shift
                ;;
            --load-stress)
                MODE="load-stress"
                shift
                ;;
            --multi-transport)
                MODE="multi-transport"
                shift
                ;;
            --orchestrated)
                MODE="orchestrated"
                shift
                ;;
            --parallel)
                PARALLEL_EXECUTION=true
                shift
                ;;
            --coverage)
                COVERAGE_ENABLED=true
                shift
                ;;
            --performance)
                PERFORMANCE_MONITORING=true
                shift
                ;;
            --agents)
                AGENT_COORDINATION=true
                shift
                ;;
            --continuous)
                CONTINUOUS_MODE=true
                shift
                ;;
            --debug)
                DEBUG_MODE=true
                shift
                ;;
            --help|-h)
                echo "Usage: $0 [MODE] [OPTIONS]"
                echo ""
                echo "Modes:"
                echo "  --comprehensive   Run all integration tests (default)"
                echo "  --quick          Run essential validation tests only"
                echo "  --load-stress    Run load and stress tests only" 
                echo "  --multi-transport Run multi-transport coordination tests"
                echo "  --orchestrated   Use test orchestrator for coordination"
                echo ""
                echo "Options:"
                echo "  --parallel       Enable parallel test execution"
                echo "  --coverage       Generate coverage reports"
                echo "  --performance    Enable performance monitoring" 
                echo "  --agents         Use coordinated test agents"
                echo "  --continuous     Continuous integration mode"
                echo "  --debug          Enable debug logging"
                exit 0
                ;;
            *)
                log_error "Unknown argument: $1"
                exit 1
                ;;
        esac
    done
}

main() {
    parse_arguments "$@"
    print_banner
    
    local start_time=$(date +%s)
    local exit_code=0
    
    # Setup
    setup_test_environment
    initialize_coordination_agents
    
    # Execute tests based on mode
    case "$MODE" in
        "comprehensive")
            log_info "Running comprehensive integration tests"
            execute_test_suites COMPREHENSIVE_SUITES "comprehensive" || exit_code=1
            ;;
        "quick")
            log_info "Running quick validation tests"
            execute_test_suites QUICK_VALIDATION_SUITES "quick" || exit_code=1
            ;;
        "load-stress")
            log_info "Running load and stress tests"
            execute_test_suites LOAD_STRESS_SUITES "load_stress" || exit_code=1
            ;;
        "multi-transport")
            log_info "Running multi-transport coordination tests"
            execute_test_suites MULTI_TRANSPORT_SUITES "multi_transport" || exit_code=1
            ;;
        "orchestrated")
            log_info "Running orchestrated integration tests"
            run_orchestrated_tests || exit_code=1
            ;;
        *)
            log_error "Unknown test mode: $MODE"
            exit_code=1
            ;;
    esac
    
    # Analysis and reporting
    analyze_test_results || exit_code=1
    generate_coverage_report
    performance_monitoring
    
    # Cleanup
    cleanup_coordination_agents
    
    local end_time=$(date +%s)
    local total_duration=$((end_time - start_time))
    
    log_info "=== COMPREHENSIVE INTEGRATION TESTING COMPLETED ==="
    log_info "Total execution time: ${total_duration}s"
    log_info "Results directory: $RESULTS_DIR"
    log_info "Log file: $LOG_FILE"
    
    if [ $exit_code -eq 0 ]; then
        log_info "🎉 All integration tests completed successfully!"
    else
        log_error "❌ Integration testing completed with failures"
    fi
    
    exit $exit_code
}

# Execute main function with all arguments
main "$@"