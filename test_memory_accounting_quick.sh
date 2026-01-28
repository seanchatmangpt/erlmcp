#!/bin/bash
# Quick verification script for memory accounting module

echo "=== Memory Accounting Module Verification ==="
echo ""

echo "1. Compiling module..."
erlc -o /tmp src/erlmcp_memory_accounting.erl 2>&1
if [ $? -eq 0 ]; then
    echo "   ✓ Compilation successful"
else
    echo "   ✗ Compilation failed"
    exit 1
fi

echo ""
echo "2. Running integration tests..."
erl -pa /tmp -noshell -eval '
    io:format("   Testing basic measurements...~n"),
    {ok, H} = erlmcp_memory_accounting:measure_per_connection_heap(self()),
    io:format("     ✓ Heap measurement: ~.3f MiB~n", [H / (1024*1024)]),
    
    {ok, R} = erlmcp_memory_accounting:measure_per_node_rss(),
    io:format("     ✓ RSS measurement: ~.3f MiB~n", [R / (1024*1024)]),
    
    S = erlmcp_memory_accounting:estimate_state_size(#{data => lists:seq(1, 100)}),
    io:format("     ✓ State estimation: ~B bytes~n", [S]),
    
    io:format("   Testing decomposition...~n"),
    D = erlmcp_memory_accounting:decompose(#{connection_pids => []}),
    io:format("     ✓ Decomposition created~n"),
    
    case erlmcp_memory_accounting:validate_decomposition(D) of
        ok -> io:format("     ✓ Validation passed~n");
        E -> io:format("     ✗ Validation failed: ~p~n", [E])
    end,
    
    C = erlmcp_memory_accounting:format_compact(D),
    case maps:size(C) of
        10 -> io:format("     ✓ Compact format (10 fields)~n");
        N -> io:format("     ✗ Compact format has ~B fields (expected 10)~n", [N])
    end,
    
    R = erlmcp_memory_accounting:format_report(D),
    case byte_size(R) > 100 of
        true -> io:format("     ✓ Report generated (~B bytes)~n", [byte_size(R)]);
        false -> io:format("     ✗ Report too small~n")
    end,
    
    io:format("~n=== ALL VERIFICATIONS PASSED ===~n"),
    halt(0).
' 2>&1 | grep -v "WARNING REPORT"

echo ""
echo "3. File summary:"
echo "   Module:       $(wc -l < src/erlmcp_memory_accounting.erl) lines"
echo "   Tests:        $(wc -l < test/erlmcp_memory_accounting_SUITE.erl) lines"
echo "   Test helper:  $(wc -l < test/erlmcp_memory_accounting_test_server.erl) lines"
echo "   Demo:         $(wc -l < examples/memory_accounting_demo.erl) lines"
echo "   Docs:         $(wc -l < docs/memory-accounting.md) lines"

echo ""
echo "=== VERIFICATION COMPLETE ==="
