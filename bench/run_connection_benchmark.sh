#!/bin/bash
# Concurrent Connections Benchmark Script
# Tests erlmcp concurrent connection capability

echo "======================================================================"
echo "   erlmcp Concurrent Connections Benchmark"
echo "   Target: 40,000-50,000 concurrent connections per node"
echo "======================================================================"
echo ""

# Check system limits
echo "System Limits:"
echo "  Process limit (Erlang): $(erl -noshell -eval 'io:format("~p", [erlang:system_info(process_limit)]), halt().')"
echo "  Port limit (Erlang): $(erl -noshell -eval 'io:format("~p", [erlang:system_info(port_limit)]), halt().')"
echo "  Schedulers: $(erl -noshell -eval 'io:format("~p", [erlang:system_info(schedulers)]), halt().')"
echo ""

# Check OS limits
echo "OS Limits:"
case "$(uname)" in
    Darwin)
        echo "  Platform: macOS"
        echo "  kern.maxproc: $(sysctl -n kern.maxproc)"
        echo "  kern.maxprocperuid: $(sysctl -n kern.maxprocperuid)"
        echo "  kern.maxfiles: $(sysctl -n kern.maxfiles)"
        echo "  kern.maxfilesperproc: $(sysctl -n kern.maxfilesperproc)"
        ;;
    Linux)
        echo "  Platform: Linux"
        echo "  ulimit -u (max processes): $(ulimit -u)"
        echo "  ulimit -n (max open files): $(ulimit -n)"
        ;;
esac
echo ""

# Run benchmark
echo "Running benchmark..."
echo ""

erl -noshell -pa /Users/sac/erlmcp/bench -eval "
    Target = 10000,
    Result = erlmcp_bench_concurrent_connections:run(Target),
    halt()
."

echo ""
echo "======================================================================"
echo "   For full results, see: CONCURRENT_CONNECTIONS_REPORT.md"
echo "======================================================================"
