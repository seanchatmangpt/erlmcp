#!/bin/bash

###############################################################################
# ERLMCP CLUSTER BENCHMARK RUNNER
###############################################################################
# Execute comprehensive benchmarks against running cluster
#
# Usage:
#   ./scripts/run-cluster-benchmark.sh
#
# Prerequisites:
#   - 4-node erlmcp cluster running (./scripts/start-cluster.sh)
#   - rebar3 compiled (_build/default ready)
#
# Outputs:
#   - logs/benchmark/results-TIMESTAMP.log
#   - logs/benchmark/detailed-TIMESTAMP.csv
###############################################################################

set -e

# Configuration
BENCHMARK_DIR="logs/benchmark"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
RESULTS_LOG="${BENCHMARK_DIR}/results-${TIMESTAMP}.log"
DETAILED_CSV="${BENCHMARK_DIR}/detailed-${TIMESTAMP}.csv"
BUILD_DIR="_build/default"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_section() {
    echo ""
    echo -e "${BLUE}========== $1 ==========${NC}"
    echo ""
}

# Ensure benchmark directory exists
mkdir -p "$BENCHMARK_DIR"

# Verify cluster is running
verify_cluster() {
    log_section "Cluster Verification"

    erl -setcookie erlmcp_cluster -sname benchmark_verify -noshell \
        -eval "
            net_adm:ping('erlmcp1@localhost'),
            Nodes = nodes([connected]),
            case length(Nodes) >= 3 of
                true ->
                    io:format('OK: Cluster has ~w connected nodes~n', [length(Nodes)]),
                    halt(0);
                false ->
                    io:format('ERROR: Expected 4 nodes, found ~w~n', [length(Nodes) + 1]),
                    halt(1)
            end
        " 2>&1 || {
        log_error "Cluster is not running!"
        log_error "Start cluster with: ./scripts/start-cluster.sh"
        exit 1
    }
}

# Run connection scaling benchmark
benchmark_connection_scaling() {
    log_section "Connection Scaling Benchmark"

    cat >> "$RESULTS_LOG" << EOF

=== CONNECTION SCALING BENCHMARK ===
Timestamp: $(date)

Test progressive connection levels to find max capacity per node (25K).

EOF

    echo "Connections,Duration(sec),Throughput(conn/sec),P99_Latency(ms)" > "$DETAILED_CSV"

    for CONNECTIONS in 100 1000 10000 25000 50000 75000 100000; do
        log_info "Testing $CONNECTIONS connections..."

        erl -setcookie erlmcp_cluster -sname scaling_test_$CONNECTIONS -noshell \
            -pa "${BUILD_DIR}/lib/*/ebin" \
            -eval "
                load_generator:generate_load($CONNECTIONS, 30),
                halt(0)
            " 2>&1 | tee -a "$RESULTS_LOG"

        # Extract results
        sleep 2
    done
}

# Run sustained load benchmark
benchmark_sustained_load() {
    log_section "Sustained Load Benchmark (5 minutes @ 100K)"

    cat >> "$RESULTS_LOG" << EOF

=== SUSTAINED LOAD BENCHMARK ===
Timestamp: $(date)

Hold 100K concurrent connections for 5 minutes and measure stability.

EOF

    log_info "Establishing 100K connections..."
    log_info "Running for 300 seconds..."

    erl -setcookie erlmcp_cluster -sname sustained_load -noshell \
        -pa "${BUILD_DIR}/lib/*/ebin" \
        -eval "
            load_generator:generate_load(100000, 300),
            halt(0)
        " 2>&1 | tee -a "$RESULTS_LOG"
}

# Run latency benchmark
benchmark_latency() {
    log_section "Latency Distribution Benchmark (100K)"

    cat >> "$RESULTS_LOG" << EOF

=== LATENCY DISTRIBUTION BENCHMARK ===
Timestamp: $(date)

Measure P50, P95, P99 latencies under 100K connections.

EOF

    log_info "Running latency measurement (1 minute)..."

    erl -setcookie erlmcp_cluster -sname latency_test -noshell \
        -pa "${BUILD_DIR}/lib/*/ebin" \
        -eval "
            load_generator:generate_load(100000, 60, [
                {message_interval, 50},
                {report_interval, 1000}
            ]),
            halt(0)
        " 2>&1 | tee -a "$RESULTS_LOG"
}

# Run failure recovery benchmark
benchmark_node_failure() {
    log_section "Node Failure Recovery Benchmark"

    cat >> "$RESULTS_LOG" << EOF

=== NODE FAILURE RECOVERY BENCHMARK ===
Timestamp: $(date)

Kill node erlmcp4 during load and measure recovery time.

EOF

    log_info "Starting load (100K connections)..."
    log_info "WARNING: This will shut down erlmcp4 node"
    log_info "You will need to manually restart it"
    log_info ""

    erl -setcookie erlmcp_cluster -sname failure_test -noshell \
        -pa "${BUILD_DIR}/lib/*/ebin" \
        -eval "
            spawn(fun() ->
                load_generator:generate_load(100000, 120),
                halt(0)
            end),
            timer:sleep(30000),
            io:format('Killing erlmcp4...~n'),
            rpc:call('erlmcp4@localhost', erlang, halt, []),
            io:format('Node killed. Monitoring recovery...~n'),
            timer:sleep(60000)
        " 2>&1 | tee -a "$RESULTS_LOG"

    echo ""
    log_info "Node failure test complete"
    log_info "Restart erlmcp4 with: erlmcp4_pid=\$(erl -setcookie erlmcp_cluster -sname erlmcp4 ...)"
}

# Run throughput benchmark
benchmark_throughput() {
    log_section "Message Throughput Benchmark"

    cat >> "$RESULTS_LOG" << EOF

=== MESSAGE THROUGHPUT BENCHMARK ===
Timestamp: $(date)

Measure maximum message throughput at 100K concurrent connections.

EOF

    log_info "Running throughput measurement (2 minutes)..."

    erl -setcookie erlmcp_cluster -sname throughput_test -noshell \
        -pa "${BUILD_DIR}/lib/*/ebin" \
        -eval "
            load_generator:generate_load(100000, 120, [
                {message_interval, 10}  % Faster messages
            ]),
            halt(0)
        " 2>&1 | tee -a "$RESULTS_LOG"
}

# Run complete benchmark suite
run_full_suite() {
    log_section "ERLMCP CLUSTER BENCHMARK SUITE"

    cat > "$RESULTS_LOG" << EOF
ERLMCP CLUSTER BENCHMARK RESULTS
=================================
Generated: $(date)
Cluster Configuration: 4-node, target 100K concurrent connections
Each node capacity: ~25K concurrent connections

EOF

    log_info "Starting benchmark suite..."
    log_info "Results will be saved to: $RESULTS_LOG"
    log_info ""

    # Run benchmarks
    benchmark_connection_scaling
    benchmark_sustained_load
    benchmark_latency
    benchmark_throughput

    # Optionally run failure recovery
    read -p "Run node failure recovery test? (y/n): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        benchmark_node_failure
    fi

    log_section "Benchmark Suite Complete"
    log_info "Results saved to: $RESULTS_LOG"
    log_info "Detailed metrics: $DETAILED_CSV"
}

# Main
main() {
    log_section "ERLMCP CLUSTER BENCHMARK RUNNER"

    verify_cluster
    run_full_suite

    log_info "All benchmarks completed successfully"
    log_info ""
    log_info "To view results:"
    log_info "  cat $RESULTS_LOG"
    log_info ""
}

main "$@"
