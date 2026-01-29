#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "===================================================================="
echo "ETS TABLE OVERFLOW DESTRUCTIVE STRESS TEST"
echo "===================================================================="
echo ""
echo "This test will fill ETS tables until they crash or corrupt data."
echo "WARNING: This may consume significant memory and CPU."
echo ""
read -p "Continue? (y/N) " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Aborted."
    exit 0
fi

# Ensure compiled
cd "$PROJECT_DIR"
if [ ! -f ebin/erlmcp_bench_ets_overflow.beam ]; then
    echo "Compiling ETS overflow benchmark..."
    erlc -I include -o ebin bench/erlmcp_bench_ets_overflow.erl
fi

# Create results directory
mkdir -p bench/results/ets_overflow
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
RESULT_FILE="bench/results/ets_overflow/overflow_test_${TIMESTAMP}.txt"
JSON_FILE="bench/results/ets_overflow/overflow_test_${TIMESTAMP}.json"

echo "Starting ETS overflow test..."
echo "Results will be saved to: $RESULT_FILE"
echo ""

# Run test for each table type
for TABLE_TYPE in set ordered_set bag; do
    echo ""
    echo "===================================================================="
    echo "Testing: $TABLE_TYPE"
    echo "===================================================================="
    
    # Run Erlang node
    erl -noshell \
        -pa ebin \
        -eval " \
            case erlmcp_bench_ets_overflow:run($TABLE_TYPE) of \
                Result -> \
                    io:format(\"~n~nRESULT:~n~p~n~n\", [Result]), \
                    jsx:encode_file(Result, <<\"$JSON_FILE\">>), \
                    init:stop() \
            end \
        " \
        2>&1 | tee -a "$RESULT_FILE"
    
    echo ""
    echo "Completed $TABLE_TYPE test"
    echo "Sleeping 5 seconds before next test..."
    sleep 5
done

echo ""
echo "===================================================================="
echo "ALL TESTS COMPLETED"
echo "===================================================================="
echo "Results saved to: $RESULT_FILE"
echo "JSON results: $JSON_FILE"
echo ""
