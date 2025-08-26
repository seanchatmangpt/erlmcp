#!/bin/bash

# ErlMCP Load Generation Test Runner
# Comprehensive load testing and performance validation script

set -euo pipefail

# Configuration
TEST_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$TEST_DIR")"
LOG_DIR="${TEST_DIR}/load_test_logs"
RESULTS_DIR="${TEST_DIR}/load_test_results"

# Test configuration
DEFAULT_DURATION=30
DEFAULT_RATE=50
TRACE_ENABLED=true
PARALLEL_TESTS=false

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Create necessary directories
mkdir -p "$LOG_DIR" "$RESULTS_DIR"

print_header() {
    echo -e "${BLUE}================================${NC}"
    echo -e "${BLUE}  ErlMCP Load Generation Tests  ${NC}"
    echo -e "${BLUE}================================${NC}"
    echo ""
}

print_section() {
    echo -e "${YELLOW}--- $1 ---${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# Usage information
usage() {
    cat << EOF
Usage: $0 [OPTIONS] [TEST_SUITE]

Load testing options:
  -d, --duration SECONDS    Test duration in seconds (default: $DEFAULT_DURATION)
  -r, --rate RPS           Request rate per second (default: $DEFAULT_RATE)
  -p, --parallel           Run tests in parallel
  -t, --trace              Enable request tracing (default: $TRACE_ENABLED)
  -o, --output DIR         Output directory (default: $RESULTS_DIR)
  --no-trace               Disable request tracing
  -h, --help               Show this help

Test suites:
  unit                     Unit tests for load generator
  scenarios                Predefined scenario tests
  limits                   System limits validation
  protocols                Protocol comparison
  mcp-stress               MCP server stress testing
  capacity                 Capacity planning tests
  all                      Run all test suites (default)

Examples:
  $0                                    # Run all tests with defaults
  $0 -d 60 -r 100 scenarios           # Run scenarios for 60s at 100 req/sec
  $0 --parallel protocols              # Run protocol tests in parallel
  $0 limits --no-trace                 # Run limits tests without tracing

EOF
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -d|--duration)
                DEFAULT_DURATION="$2"
                shift 2
                ;;
            -r|--rate)
                DEFAULT_RATE="$2"
                shift 2
                ;;
            -p|--parallel)
                PARALLEL_TESTS=true
                shift
                ;;
            -t|--trace)
                TRACE_ENABLED=true
                shift
                ;;
            --no-trace)
                TRACE_ENABLED=false
                shift
                ;;
            -o|--output)
                RESULTS_DIR="$2"
                mkdir -p "$RESULTS_DIR"
                shift 2
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            -*)
                echo "Unknown option: $1"
                usage
                exit 1
                ;;
            *)
                TEST_SUITE="$1"
                shift
                ;;
        esac
    done
}

# Check dependencies
check_dependencies() {
    print_section "Checking Dependencies"
    
    # Check Erlang/OTP
    if ! command -v erl &> /dev/null; then
        print_error "Erlang/OTP not found. Please install Erlang/OTP."
        exit 1
    fi
    print_success "Erlang/OTP found: $(erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell)"
    
    # Check required Erlang applications
    erl -noshell -eval "
        Apps = [jsx, opentelemetry],
        Missing = lists:filter(fun(App) ->
            case application:load(App) of
                ok -> false;
                {error, _} -> true
            end
        end, Apps),
        case Missing of
            [] -> io:format('All required applications available~n');
            _ -> io:format('Missing applications: ~p~n', [Missing]), halt(1)
        end,
        halt(0).
    " 2>/dev/null || {
        print_error "Required Erlang applications not found. Run: rebar3 deps"
        exit 1
    }
    print_success "All required Erlang applications available"
    
    # Check if load generator module compiles
    cd "$PROJECT_ROOT"
    if erl -noshell -pa test -eval "
        case compile:file('test/erlmcp_load_generator.erl', [return]) of
            {ok, _} -> halt(0);
            {error, Errors} -> io:format('Compilation errors: ~p~n', [Errors]), halt(1)
        end.
    " 2>/dev/null; then
        print_success "Load generator module compiles successfully"
    else
        print_error "Load generator module compilation failed"
        exit 1
    fi
}

# Start Erlang distributed node for testing
start_test_node() {
    print_section "Starting Test Node"
    
    NODE_NAME="erlmcp_load_test_$$"
    COOKIE="erlmcp_load_test_cookie"
    
    # Start distributed node in background
    erl -sname "$NODE_NAME" -setcookie "$COOKIE" -pa test -noshell -detached \
        -eval "
            application:ensure_all_started(jsx),
            application:ensure_all_started(opentelemetry),
            otel_tracer_provider:setup(),
            {ok, _} = erlmcp_load_generator:start_link(),
            receive stop -> ok end.
        " &
    
    TEST_NODE_PID=$!
    sleep 2
    
    # Verify node is running
    if kill -0 $TEST_NODE_PID 2>/dev/null; then
        print_success "Test node started (PID: $TEST_NODE_PID)"
        echo "$TEST_NODE_PID" > "$LOG_DIR/test_node.pid"
        echo "$NODE_NAME" > "$LOG_DIR/test_node.name"
    else
        print_error "Failed to start test node"
        exit 1
    fi
}

# Stop test node
stop_test_node() {
    if [[ -f "$LOG_DIR/test_node.pid" ]]; then
        TEST_NODE_PID=$(cat "$LOG_DIR/test_node.pid")
        if kill -0 "$TEST_NODE_PID" 2>/dev/null; then
            print_info "Stopping test node (PID: $TEST_NODE_PID)"
            kill "$TEST_NODE_PID" 2>/dev/null || true
            wait "$TEST_NODE_PID" 2>/dev/null || true
        fi
        rm -f "$LOG_DIR/test_node.pid" "$LOG_DIR/test_node.name"
    fi
}

# Cleanup on exit
cleanup() {
    print_info "Cleaning up..."
    stop_test_node
}

trap cleanup EXIT

# Run unit tests
run_unit_tests() {
    print_section "Running Unit Tests"
    
    cd "$PROJECT_ROOT"
    
    # Compile test modules
    print_info "Compiling test modules..."
    if ! erl -noshell -pa test -eval "
        Modules = [erlmcp_load_generator, erlmcp_load_generator_tests, erlmcp_load_scenarios],
        Results = lists:map(fun(Mod) ->
            File = atom_to_list(Mod) ++ \".erl\",
            case compile:file(\"test/\" ++ File, [return, {outdir, \"test\"}]) of
                {ok, _} -> {Mod, ok};
                Error -> {Mod, Error}
            end
        end, Modules),
        Errors = [R || R = {_, {error, _}} <- Results],
        case Errors of
            [] -> halt(0);
            _ -> io:format('Compilation errors: ~p~n', [Errors]), halt(1)
        end.
    "; then
        print_error "Module compilation failed"
        return 1
    fi
    print_success "All test modules compiled successfully"
    
    # Run Common Test
    print_info "Running Common Test suite..."
    ct_run \
        -pa test \
        -dir test \
        -suite erlmcp_load_generator_tests \
        -logdir "$LOG_DIR/ct_logs" \
        -cover test/cover.spec 2>/dev/null || {
        
        # Fallback to manual test execution if ct_run fails
        print_info "ct_run not available, running tests manually..."
        
        TEST_RESULTS=$(erl -noshell -pa test -eval "
            application:ensure_all_started(jsx),
            application:ensure_all_started(opentelemetry),
            otel_tracer_provider:setup(),
            {ok, _} = erlmcp_load_generator:start_link(),
            
            TestCases = [
                test_constant_load_generation,
                test_burst_load_generation,
                test_ramp_up_load_generation,
                test_message_size_patterns,
                test_metrics_collection
            ],
            
            Results = lists:map(fun(TestCase) ->
                try
                    erlmcp_load_generator_tests:TestCase([]),
                    {TestCase, passed}
                catch
                    Error:Reason ->
                        {TestCase, {failed, Error, Reason}}
                end
            end, TestCases),
            
            Passed = length([R || {_, passed} <- Results]),
            Total = length(Results),
            
            io:format('Test Results: ~p/~p passed~n', [Passed, Total]),
            lists:foreach(fun({TC, Result}) ->
                io:format('  ~p: ~p~n', [TC, Result])
            end, Results),
            
            gen_server:stop(erlmcp_load_generator),
            halt(case Passed of Total -> 0; _ -> 1 end).
        " 2>&1)
        
        echo "$TEST_RESULTS" | tee "$LOG_DIR/unit_tests.log"
        
        if echo "$TEST_RESULTS" | grep -q "halt(0)"; then
            print_success "Unit tests passed"
            return 0
        else
            print_error "Some unit tests failed"
            return 1
        fi
    }
    
    print_success "Unit tests completed"
}

# Run scenario tests
run_scenario_tests() {
    print_section "Running Scenario Tests"
    
    local scenarios=(
        "api_baseline"
        "burst_traffic"
        "gradual_ramp" 
        "mobile_patterns"
        "websocket_streaming"
        "mcp_protocol_test"
        "mixed_workload"
    )
    
    local results_file="$RESULTS_DIR/scenario_results.json"
    echo "[]" > "$results_file"
    
    for scenario in "${scenarios[@]}"; do
        print_info "Running scenario: $scenario"
        
        local start_time=$(date +%s)
        local scenario_result
        
        scenario_result=$(erl -noshell -pa test -eval "
            application:ensure_all_started(jsx),
            application:ensure_all_started(opentelemetry),
            otel_tracer_provider:setup(),
            {ok, _} = erlmcp_load_generator:start_link(),
            
            Overrides = #{
                duration => ${DEFAULT_DURATION}000,
                trace_every_request => $TRACE_ENABLED
            },
            
            case erlmcp_load_scenarios:run_scenario($scenario, Overrides) of
                {ok, Result} ->
                    io:format('SCENARIO_RESULT: ~s~n', [jsx:encode(Result)]),
                    halt(0);
                {error, Reason} ->
                    io:format('SCENARIO_ERROR: ~p~n', [Reason]),
                    halt(1)
            end.
        " 2>&1)
        
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))
        
        if echo "$scenario_result" | grep -q "SCENARIO_RESULT:"; then
            local json_result
            json_result=$(echo "$scenario_result" | grep "SCENARIO_RESULT:" | sed 's/SCENARIO_RESULT: //')
            
            # Add timing information
            json_result=$(echo "$json_result" | jq ". + {test_duration_seconds: $duration}")
            
            # Append to results
            jq ". += [$json_result]" "$results_file" > "${results_file}.tmp" && mv "${results_file}.tmp" "$results_file"
            
            print_success "Scenario $scenario completed in ${duration}s"
        else
            print_error "Scenario $scenario failed"
            echo "$scenario_result" | head -10
        fi
    done
    
    # Generate summary report
    local summary
    summary=$(jq -r '
        "=== Scenario Test Summary ===\n" +
        "Total scenarios: \(length)\n" +
        "Average throughput: \(map(.throughput_rps) | add / length | round) req/sec\n" +
        "Average success rate: \((map(.success_rate) | add / length) * 100 | round)%\n" +
        "Average latency: \(map(.avg_latency_us) | add / length / 1000 | round)ms"
    ' "$results_file")
    
    echo -e "$summary"
    echo -e "$summary" > "$RESULTS_DIR/scenario_summary.txt"
    
    print_success "Scenario tests completed. Results saved to $results_file"
}

# Run system limits validation
run_limits_tests() {
    print_section "Running System Limits Tests"
    
    print_info "Validating system performance limits..."
    
    local limits_result
    limits_result=$(erl -noshell -pa test -eval "
        application:ensure_all_started(jsx),
        application:ensure_all_started(opentelemetry),
        otel_tracer_provider:setup(),
        {ok, _} = erlmcp_load_generator:start_link(),
        
        case erlmcp_load_scenarios:validate_system_limits() of
            {ok, Results} ->
                io:format('LIMITS_RESULT: ~s~n', [jsx:encode(Results)]),
                halt(0);
            {error, Reason} ->
                io:format('LIMITS_ERROR: ~p~n', [Reason]),
                halt(1)
        end.
    " 2>&1)
    
    if echo "$limits_result" | grep -q "LIMITS_RESULT:"; then
        local json_result
        json_result=$(echo "$limits_result" | grep "LIMITS_RESULT:" | sed 's/LIMITS_RESULT: //')
        echo "$json_result" | jq '.' > "$RESULTS_DIR/limits_test.json"
        
        # Extract key metrics
        local max_stable_rate
        local recommended_rate
        max_stable_rate=$(echo "$json_result" | jq -r '.max_stable_rate')
        recommended_rate=$(echo "$json_result" | jq -r '.recommended_rate')
        
        print_success "System limits test completed"
        print_info "Maximum stable rate: $max_stable_rate req/sec"
        print_info "Recommended rate: $recommended_rate req/sec"
        
        # Generate limits report
        cat > "$RESULTS_DIR/limits_report.txt" << EOF
=== System Performance Limits ===

Maximum stable rate: $max_stable_rate req/sec
Recommended rate: $recommended_rate req/sec

$(echo "$json_result" | jq -r '.system_capacity | to_entries[] | "  \(.key): \(.value)"')

Test Results:
$(echo "$json_result" | jq -r '.test_results[] | "  \(.test_name): \(.actual_throughput) req/sec (success: \(.success_rate * 100 | round)%)"')
EOF
        
    else
        print_error "System limits test failed"
        echo "$limits_result" | head -10
        return 1
    fi
}

# Run protocol comparison tests
run_protocol_tests() {
    print_section "Running Protocol Comparison Tests"
    
    print_info "Benchmarking different protocols..."
    
    local protocol_result
    protocol_result=$(erl -noshell -pa test -eval "
        application:ensure_all_started(jsx),
        application:ensure_all_started(opentelemetry),
        otel_tracer_provider:setup(),
        {ok, _} = erlmcp_load_generator:start_link(),
        
        case erlmcp_load_scenarios:benchmark_protocol_comparison() of
            {ok, Results} ->
                io:format('PROTOCOL_RESULT: ~s~n', [jsx:encode(Results)]),
                halt(0);
            {error, Reason} ->
                io:format('PROTOCOL_ERROR: ~p~n', [Reason]),
                halt(1)
        end.
    " 2>&1)
    
    if echo "$protocol_result" | grep -q "PROTOCOL_RESULT:"; then
        local json_result
        json_result=$(echo "$protocol_result" | grep "PROTOCOL_RESULT:" | sed 's/PROTOCOL_RESULT: //')
        echo "$json_result" | jq '.' > "$RESULTS_DIR/protocol_comparison.json"
        
        # Generate protocol comparison report
        echo "$json_result" | jq -r '
            "=== Protocol Comparison Results ===\n" +
            (.ranked_results | to_entries[] | 
                "  \(.value.protocol): \(.value.throughput | round) req/sec, " +
                "\(.value.success_rate * 100 | round)% success, " +
                "\(.value.avg_latency / 1000 | round)ms avg latency") +
            "\n\n=== Recommendations ===\n" +
            (.recommendations[] | "  \(.category): \(.protocol) - \(.reason)")
        ' > "$RESULTS_DIR/protocol_report.txt"
        
        cat "$RESULTS_DIR/protocol_report.txt"
        
        print_success "Protocol comparison completed"
    else
        print_error "Protocol comparison failed"
        echo "$protocol_result" | head -10
        return 1
    fi
}

# Run MCP stress tests
run_mcp_stress_tests() {
    print_section "Running MCP Server Stress Tests"
    
    print_info "Stress testing MCP server..."
    
    local mcp_config="{host => \"localhost\", port => 3000}"
    
    local mcp_result
    mcp_result=$(erl -noshell -pa test -eval "
        application:ensure_all_started(jsx),
        application:ensure_all_started(opentelemetry),
        otel_tracer_provider:setup(),
        {ok, _} = erlmcp_load_generator:start_link(),
        
        ServerConfig = $mcp_config,
        
        case erlmcp_load_scenarios:stress_test_mcp_server(ServerConfig) of
            {ok, Results} ->
                io:format('MCP_RESULT: ~s~n', [jsx:encode(Results)]),
                halt(0);
            {error, Reason} ->
                io:format('MCP_ERROR: ~p~n', [Reason]),
                halt(1)
        end.
    " 2>&1)
    
    if echo "$mcp_result" | grep -q "MCP_RESULT:"; then
        local json_result
        json_result=$(echo "$mcp_result" | grep "MCP_RESULT:" | sed 's/MCP_RESULT: //')
        echo "$json_result" | jq '.' > "$RESULTS_DIR/mcp_stress_test.json"
        
        # Generate MCP stress test report
        echo "$json_result" | jq -r '
            "=== MCP Server Stress Test Results ===\n" +
            "Overall Health: \(.server_health.overall_health)\n" +
            "Health Ratio: \(.server_health.health_ratio * 100 | round)%\n\n" +
            "Phase Results:\n" +
            (.phase_results[] | 
                "  \(.phase_name): \(.throughput_rps | round) req/sec, " +
                "\(.success_rate * 100 | round)% success")
        ' > "$RESULTS_DIR/mcp_stress_report.txt"
        
        cat "$RESULTS_DIR/mcp_stress_report.txt"
        
        print_success "MCP stress test completed"
    else
        print_info "MCP stress test skipped (server may not be available)"
        echo "$mcp_result" | head -5
    fi
}

# Run capacity planning tests
run_capacity_tests() {
    print_section "Running Capacity Planning Tests"
    
    print_info "Performing capacity planning analysis..."
    
    local requirements="{expected_users => 1000, peak_multiplier => 3, avg_requests_per_user => 10}"
    
    local capacity_result
    capacity_result=$(erl -noshell -pa test -eval "
        application:ensure_all_started(jsx),
        application:ensure_all_started(opentelemetry),
        otel_tracer_provider:setup(),
        {ok, _} = erlmcp_load_generator:start_link(),
        
        Requirements = $requirements,
        
        case erlmcp_load_scenarios:capacity_planning_test(Requirements) of
            {ok, Results} ->
                io:format('CAPACITY_RESULT: ~s~n', [jsx:encode(Results)]),
                halt(0);
            {error, Reason} ->
                io:format('CAPACITY_ERROR: ~p~n', [Reason]),
                halt(1)
        end.
    " 2>&1)
    
    if echo "$capacity_result" | grep -q "CAPACITY_RESULT:"; then
        local json_result
        json_result=$(echo "$capacity_result" | grep "CAPACITY_RESULT:" | sed 's/CAPACITY_RESULT: //')
        echo "$json_result" | jq '.' > "$RESULTS_DIR/capacity_planning.json"
        
        # Generate capacity planning report
        echo "$json_result" | jq -r '
            "=== Capacity Planning Results ===\n" +
            "Current Requirements:\n" +
            "  Expected Users: \(.capacity_recommendations.current_requirements.expected_users)\n" +
            "  Meets Requirements: \(.capacity_recommendations.current_requirements.meets_requirements)\n\n" +
            "Deployment Recommendations:\n" +
            "  Max Users: \(.capacity_recommendations.deployment_recommendation.recommended_max_users)\n" +
            "  Scaling Factor: \(.capacity_recommendations.deployment_recommendation.scaling_factor)\n\n" +
            "Infrastructure Sizing:\n" +
            (.deployment_sizing | to_entries[] | "  \(.key): \(.value)")
        ' > "$RESULTS_DIR/capacity_report.txt"
        
        cat "$RESULTS_DIR/capacity_report.txt"
        
        print_success "Capacity planning test completed"
    else
        print_error "Capacity planning test failed"
        echo "$capacity_result" | head -10
        return 1
    fi
}

# Generate comprehensive test report
generate_final_report() {
    print_section "Generating Final Report"
    
    local report_file="$RESULTS_DIR/load_test_report.html"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    cat > "$report_file" << EOF
<!DOCTYPE html>
<html>
<head>
    <title>ErlMCP Load Test Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .header { background: #2c3e50; color: white; padding: 20px; }
        .section { margin: 20px 0; padding: 15px; border: 1px solid #ddd; }
        .metric { display: inline-block; margin: 10px; padding: 10px; background: #f8f9fa; }
        .success { color: #27ae60; }
        .warning { color: #f39c12; }
        .error { color: #e74c3c; }
        pre { background: #f8f9fa; padding: 10px; overflow-x: auto; }
    </style>
</head>
<body>
    <div class="header">
        <h1>ErlMCP Load Test Report</h1>
        <p>Generated: $timestamp</p>
    </div>
    
    <div class="section">
        <h2>Test Configuration</h2>
        <div class="metric">Duration: ${DEFAULT_DURATION}s</div>
        <div class="metric">Rate: ${DEFAULT_RATE} req/sec</div>
        <div class="metric">Tracing: $TRACE_ENABLED</div>
        <div class="metric">Parallel: $PARALLEL_TESTS</div>
    </div>
EOF
    
    # Add results from each test suite if available
    if [[ -f "$RESULTS_DIR/scenario_results.json" ]]; then
        echo '<div class="section"><h2>Scenario Test Results</h2>' >> "$report_file"
        echo '<pre>' >> "$report_file"
        cat "$RESULTS_DIR/scenario_summary.txt" >> "$report_file" 2>/dev/null || echo "No scenario summary available" >> "$report_file"
        echo '</pre></div>' >> "$report_file"
    fi
    
    if [[ -f "$RESULTS_DIR/limits_test.json" ]]; then
        echo '<div class="section"><h2>System Limits</h2>' >> "$report_file"
        echo '<pre>' >> "$report_file"
        cat "$RESULTS_DIR/limits_report.txt" >> "$report_file" 2>/dev/null || echo "No limits report available" >> "$report_file"
        echo '</pre></div>' >> "$report_file"
    fi
    
    if [[ -f "$RESULTS_DIR/protocol_comparison.json" ]]; then
        echo '<div class="section"><h2>Protocol Comparison</h2>' >> "$report_file"
        echo '<pre>' >> "$report_file"
        cat "$RESULTS_DIR/protocol_report.txt" >> "$report_file" 2>/dev/null || echo "No protocol report available" >> "$report_file"
        echo '</pre></div>' >> "$report_file"
    fi
    
    cat >> "$report_file" << EOF
    
    <div class="section">
        <h2>Raw Results</h2>
        <p>All raw test data is available in the results directory: $RESULTS_DIR</p>
        <ul>
EOF
    
    for file in "$RESULTS_DIR"/*.json; do
        if [[ -f "$file" ]]; then
            local filename=$(basename "$file")
            echo "            <li><a href=\"$filename\">$filename</a></li>" >> "$report_file"
        fi
    done
    
    cat >> "$report_file" << EOF
        </ul>
    </div>
    
</body>
</html>
EOF
    
    print_success "Final report generated: $report_file"
    
    # Also create a text summary
    local summary_file="$RESULTS_DIR/test_summary.txt"
    {
        echo "ErlMCP Load Test Summary"
        echo "======================="
        echo "Generated: $timestamp"
        echo "Configuration: ${DEFAULT_DURATION}s duration, ${DEFAULT_RATE} req/sec rate"
        echo ""
        
        if [[ -f "$RESULTS_DIR/scenario_summary.txt" ]]; then
            echo "Scenario Tests:"
            cat "$RESULTS_DIR/scenario_summary.txt"
            echo ""
        fi
        
        if [[ -f "$RESULTS_DIR/limits_report.txt" ]]; then
            echo "System Limits:"
            head -10 "$RESULTS_DIR/limits_report.txt"
            echo ""
        fi
        
        if [[ -f "$RESULTS_DIR/protocol_report.txt" ]]; then
            echo "Protocol Comparison:"
            head -10 "$RESULTS_DIR/protocol_report.txt"
            echo ""
        fi
        
        echo "All detailed results available in: $RESULTS_DIR"
    } > "$summary_file"
    
    print_info "Text summary: $summary_file"
}

# Main execution function
main() {
    local test_suite="${TEST_SUITE:-all}"
    
    print_header
    
    print_info "Test suite: $test_suite"
    print_info "Duration: ${DEFAULT_DURATION}s"
    print_info "Rate: ${DEFAULT_RATE} req/sec"
    print_info "Tracing: $TRACE_ENABLED"
    print_info "Parallel: $PARALLEL_TESTS"
    print_info "Results dir: $RESULTS_DIR"
    echo ""
    
    # Check dependencies first
    check_dependencies
    
    # Start test infrastructure
    start_test_node
    
    local overall_success=true
    
    case "$test_suite" in
        unit)
            run_unit_tests || overall_success=false
            ;;
        scenarios)
            run_scenario_tests || overall_success=false
            ;;
        limits)
            run_limits_tests || overall_success=false
            ;;
        protocols)
            run_protocol_tests || overall_success=false
            ;;
        mcp-stress)
            run_mcp_stress_tests || overall_success=false
            ;;
        capacity)
            run_capacity_tests || overall_success=false
            ;;
        all)
            run_unit_tests || overall_success=false
            run_scenario_tests || overall_success=false
            run_limits_tests || overall_success=false
            run_protocol_tests || overall_success=false
            run_mcp_stress_tests || true  # Don't fail overall if MCP server not available
            run_capacity_tests || overall_success=false
            ;;
        *)
            print_error "Unknown test suite: $test_suite"
            usage
            exit 1
            ;;
    esac
    
    # Generate final report
    generate_final_report
    
    print_section "Test Summary"
    
    if $overall_success; then
        print_success "All tests completed successfully!"
        echo ""
        print_info "Results available in: $RESULTS_DIR"
        exit 0
    else
        print_error "Some tests failed. Check logs for details."
        echo ""
        print_info "Logs available in: $LOG_DIR"
        print_info "Results available in: $RESULTS_DIR"
        exit 1
    fi
}

# Parse arguments and run main function
TEST_SUITE=""
parse_args "$@"
main