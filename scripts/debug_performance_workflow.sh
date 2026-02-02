#!/bin/bash

# Performance Debugging and Optimization Workflow Script
# This script demonstrates the complete CLI workflow for debugging and performance

set -e

echo "🚀 ERLMCP Performance Debugging Workflow"
echo "========================================"

# 1. COMPILATION CHECK
echo "1. Compilation Check"
echo "-------------------"
rebar3 compile
if [ $? -eq 0 ]; then
    echo "✅ Compilation successful"
else
    echo "❌ Compilation failed - check errors above"
    exit 1
fi

# 2. TEST VALIDATION
echo -e "\n2. Test Validation"
echo "------------------"
rebar3 eunit --verbose
rebar3 ct --suite=test/* | tee test_results.txt

if grep -q "failed" test_results.txt; then
    echo "❌ Some tests failed - check test_results.txt"
    exit 1
else
    echo "✅ All tests passing"
fi

# 3. PERFORMANCE BASELINE
echo -e "\n3. Performance Baseline"
echo "------------------------"
echo "Running quick benchmark to establish baseline..."
make benchmark-quick | tee baseline.txt

# Extract key metrics
throughput=$(grep "throughput_msg_per_s" baseline.txt | head -1 | awk '{print $2}')
memory=$(grep "memory_rss_mib_per_node" baseline.txt | head -1 | awk '{print $2}')

echo "📊 Baseline Metrics:"
echo "   Throughput: ${throughput:-unknown} msg/s"
echo "   Memory: ${memory:-unknown} MiB"

# 4. DETAILED BENCHMARKS
echo -e "\n4. Detailed Benchmark Categories"
echo "---------------------------------"

echo "🔬 Core Operations Benchmark"
./bench/erlmcp_bench_core_ops.erl core_ops_100k

echo "🌐 Network Benchmark"
./bench/erlmcp_bench_network_real.erl tcp_sustained_10k

echo "💪 Stress Benchmark"
./bench/erlmcp_bench_stress.erl stress_5min_100k_ops

# 5. PROFILING
echo -e "\n5. Performance Profiling"
echo "-------------------------"
echo "Running fprof on JSON encoding..."

# Create test data
erl -eval "
    File = 'test_data.json',
    Data = [{atom_to_binary(K, utf8), V} || {K,V} <- lists:seq(1,1000)],
    ok = file:write_file(File, jsx:encode(Data)),
    io:format('Created test data: ~p~n', [File]),
    halt()." -noshell

# Profile with fprof
fprof:trace([start, {procs, [self()]}]),
{ok, _} = file:read_file("test_data.json"),
fprof:profile(),
fprof:analyse([{dest, "encoding_profile.txt"}, {sort, self}]),
fprof:stop(),

echo "Profile saved to encoding_profile.txt"

# 6. MEMORY ANALYSIS
echo -e "\n6. Memory Leak Detection"
echo "------------------------"
echo "Running memory leak test for 30 seconds..."
timeout 30s erl -eval "
    start() ->
        process_flag(trap_exit, true),
        Pid = spawn_link(fun() -> memory_test_loop(0) end),
        receive
            {'EXIT', Pid, Reason} ->
                io:format('Process exited: ~p~n', [Reason])
        after 30000 ->
            exit(Pid, normal),
            io:format('Memory test completed~n')
        end.

    memory_test_loop(N) ->
        Memory = process_info(self(), memory),
        io:format('Iteration ~p, Memory: ~p~n', [N, Memory]),
        % Create some data to simulate workload
        _ = [spawn(fun() -> timer:sleep(100) end) || _ <- lists:seq(1,100)],
        timer:sleep(1000),
        memory_test_loop(N+1).

    start().
" -noshell

# 7. BOTTLENECK ANALYSIS
echo -e "\n7. Bottleneck Analysis"
echo "-----------------------"
echo "Analyzing system components..."

# Check process counts
echo "Process counts:"
erl -eval "io:format('Total: ~p~n', [length(processes())])." -noshell

# Check memory usage
echo "Memory usage:"
erl -eval "io:format('System: ~p bytes~n', [erlang:memory(total)])." -noshell

# Check registry performance if available
if grep -q "registry" baseline.txt; then
    echo "Registry performance:"
    grep "registry" baseline.txt
fi

# 8. PERFORMANCE REPORT
echo -e "\n8. Performance Report Generation"
echo "--------------------------------"
echo "Generating performance report..."

cat > performance_report.md << REPORT_EOF
# ERLMCP Performance Report

## Test Date: $(date)

### Summary
- ✅ Compilation: Success
- ✅ Tests: All passing
- ⚠️ Performance: Needs review

### Baseline Metrics
- Throughput: ${throughput:-unknown} msg/s
- Memory Usage: ${memory:-unknown} MiB

### Recommendations
1. **JSON Encoding**: Consider switching from jsx to jiffy for 2-3x improvement
2. **Registry**: Current performance at $(grep -o '[0-9]*' baseline.txt | head -1 | awk '{print $1/1000}')K msg/s
3. **Memory**: Monitor for leaks during sustained load

### Next Steps
1. Run profile analysis on encoding_profile.txt
2. Consider implementing connection pooling
3. Add monitoring for production deployment

REPORT_EOF

echo "✅ Report saved to performance_report.md"

# 9. CLEANUP
echo -e "\n9. Cleanup"
echo "----------"
rm -f test_data.json encoding_profile.txt test_results.txt

echo "🎉 Performance debugging workflow completed!"
echo "📊 Check performance_report.md for full results"
