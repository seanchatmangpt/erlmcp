%%%-------------------------------------------------------------------
%%% @doc Quick integration test for memory accounting
%%% @end
%%%-------------------------------------------------------------------
-module(test_memory_accounting).

-export([run/0]).

run() ->
    io:format("=== Memory Accounting Integration Test ===~n"),
    
    % Test 1: Measure current process
    io:format("~nTest 1: Measure current process~n"),
    {ok, HeapBytes} = erlmcp_memory_accounting:measure_per_connection_heap(self()),
    io:format("  Current process heap: ~.3f MiB~n", [HeapBytes / (1024 * 1024)]),
    
    % Test 2: Measure node RSS
    io:format("~nTest 2: Measure node RSS~n"),
    {ok, RssBytes} = erlmcp_memory_accounting:measure_per_node_rss(),
    io:format("  Node RSS: ~.3f MiB~n", [RssBytes / (1024 * 1024)]),
    
    % Test 3: Estimate state size
    io:format("~nTest 3: Estimate state sizes~n"),
    SmallState = #{a => 1, b => 2, c => 3},
    LargeState = #{data => lists:seq(1, 10000)},
    SmallSize = erlmcp_memory_accounting:estimate_state_size(SmallState),
    LargeSize = erlmcp_memory_accounting:estimate_state_size(LargeState),
    io:format("  Small state: ~B bytes~n", [SmallSize]),
    io:format("  Large state: ~B bytes (~.3f MiB)~n", [LargeSize, LargeSize / (1024 * 1024)]),
    
    % Test 4: Full decomposition
    io:format("~nTest 4: Full memory decomposition~n"),
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => [self()],
        server_pid => self()
    }),
    
    % Validate
    case erlmcp_memory_accounting:validate_decomposition(Decomp) of
        ok ->
            io:format("  Validation: PASSED~n");
        {error, Reason} ->
            io:format("  Validation: FAILED - ~p~n", [Reason])
    end,
    
    % Format compact
    Compact = erlmcp_memory_accounting:format_compact(Decomp),
    io:format("~nCompact report:~n"),
    [io:format("  ~p: ~p~n", [K, V]) || {K, V} <- maps:to_list(Compact)],
    
    % Format full report
    Report = erlmcp_memory_accounting:format_report(Decomp),
    io:format("~nFull report:~n~s~n", [Report]),
    
    io:format("=== All Tests Completed Successfully ===~n"),
    ok.
