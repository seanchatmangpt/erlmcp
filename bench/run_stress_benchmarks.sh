#!/usr/bin/env bash
# Run stress/sustained load benchmarks for erlmcp

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS_DIR="$SCRIPT_DIR/results"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

usage() {
    echo "Usage: $0 [OPTIONS] [WORKLOAD]"
    echo ""
    echo "Run stress/sustained load benchmarks for erlmcp"
    echo ""
    echo "WORKLOADS:"
    echo "  quick       - 30 second stress test (100k ops/s, 100 workers)"
    echo "  standard    - 5 minute stress test (100k ops/s, 100 workers)"
    echo "  endurance   - 1 hour stress test (50k ops/s, 50 workers)"
    echo "  production  - 24 hour stress test (10k ops/s, 10 workers)"
    echo "  all         - Run all workloads sequentially (default)"
    echo ""
    echo "OPTIONS:"
    echo "  -h, --help     Show this help message"
    echo "  -o, --output   Output directory for results (default: ./results)"
    echo "  -c, --custom   Custom workload JSON file"
    echo "  -v, --verbose  Verbose output"
    echo ""
    echo "EXAMPLES:"
    echo "  $0 quick              # Run quick 30s test"
    echo "  $0 standard           # Run standard 5min test"
    echo "  $0 -o /tmp/results    # Custom output directory"
    exit 1
}

# Parse arguments
WORKLOAD="all"
OUTPUT_DIR="$RESULTS_DIR"
CUSTOM_WORKLOAD=""
VERBOSE=0

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -o|--output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        -c|--custom)
            CUSTOM_WORKLOAD="$2"
            shift 2
            ;;
        -v|--verbose)
            VERBOSE=1
            shift
            ;;
        quick|standard|endurance|production|all)
            WORKLOAD="$1"
            shift
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            usage
            ;;
    esac
done

# Create output directory
mkdir -p "$OUTPUT_DIR"

echo -e "${BLUE}=== erlmcp Stress Benchmark Suite ===${NC}"
echo ""
echo "Workload: $WORKLOAD"
echo "Output: $OUTPUT_DIR"
echo ""

# Compile bench module if needed
echo -e "${YELLOW}Compiling erlmcp_bench_stress module...${NC}"
cd "$PROJECT_ROOT"
erlc -I include -o /tmp bench/erlmcp_bench_stress.erl
if [ $? -ne 0 ]; then
    echo -e "${RED}Compilation failed!${NC}"
    exit 1
fi
echo -e "${GREEN}Compilation successful${NC}"
echo ""

# Prepare Erlang script
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
OUTPUT_FILE="$OUTPUT_DIR/stress_${WORKLOAD}_${TIMESTAMP}.json"

RUN_SCRIPT=$(cat <<'ERLANG'
io:format("~n=== Starting Stress Benchmark: %s ===~n~n", [Workload]),

Result = case Workload of
    "quick" -> erlmcp_bench_stress:quick_stress();
    "standard" -> erlmcp_bench_stress:standard_stress();
    "endurance" -> erlmcp_bench_stress:endurance_stress();
    "production" ->
        Workloads = erlmcp_bench_stress:workloads(),
        erlmcp_bench_stress:run_workload(lists:nth(4, Workloads));
    "all" -> erlmcp_bench_stress:run_all()
end,

%% Save to file
Json = jsx:encode(Result, [space, indent]),
file:write_file(OutputFile, Json),

%% Print summary
io:format("~n=== Benchmark Complete ===~n~n"),
case is_list(Result) of
    true ->
        %% Multiple results
        io:format("Ran ~p workloads~n", [length(Result)]),
        lists:foreach(fun(R) ->
            Id = maps:get(<<"workload_id">>, R),
            Ops = maps:get(<<"actual_ops_total">>, R),
            Tput = maps:get(<<"actual_throughput_avg">>, R),
            io:format("  ~s: ~p ops, ~.1f ops/s~n", [Id, Ops, Tput])
        end, Result);
    false ->
        %% Single result
        Id = maps:get(<<"workload_id">>, Result),
        Ops = maps:get(<<"actual_ops_total">>, Result),
        Tput = maps:get(<<"actual_throughput_avg">>, Result),
        MemLeak = maps:get(<<"memory_leak_detected">>, Result),
        Degrad = maps:get(<<"degradation_detected">>, Result),
        
        io:format("Workload: ~s~n", [Id]),
        io:format("Total Operations: ~p~n", [Ops]),
        io:format("Avg Throughput: ~.1f ops/s~n", [Tput]),
        io:format("Memory Leak: ~p~n", [MemLeak]),
        io:format("Degradation: ~p~n", [Degrad])
end,

io:format("~nResults saved to: ~s~n~n", [OutputFile]),
halt(0).
ERLANG
)

# Run benchmark
echo -e "${YELLOW}Running benchmark...${NC}"
echo ""

erl -pa /tmp -pa _build/default/lib/*/ebin -noshell \
    -eval "Workload = \"$WORKLOAD\", OutputFile = \"$OUTPUT_FILE\"." \
    -eval "$RUN_SCRIPT" 2>&1

EXIT_CODE=$?

if [ $EXIT_CODE -eq 0 ]; then
    echo -e "${GREEN}Benchmark completed successfully!${NC}"
    echo ""
    echo "Results: $OUTPUT_FILE"
    
    # Show summary if jq is available
    if command -v jq &> /dev/null; then
        echo ""
        echo -e "${BLUE}Summary:${NC}"
        jq -r '
            if type == "array" then
                .[] | "  \(.workload_id): \(.actual_ops_total) ops @ \(.actual_throughput_avg) ops/s"
            else
                "  \(.workload_id): \(.actual_ops_total) ops @ \(.actual_throughput_avg) ops/s"
            end
        ' "$OUTPUT_FILE"
    fi
else
    echo -e "${RED}Benchmark failed with exit code $EXIT_CODE${NC}"
    exit $EXIT_CODE
fi
