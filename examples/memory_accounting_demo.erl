%%%-------------------------------------------------------------------
%%% @doc
%%% Memory Accounting Demo - Shows proper memory reporting
%%%
%%% Demonstrates how to use erlmcp_memory_accounting to eliminate
%%% ambiguous "MiB/conn" reports.
%%% @end
%%%-------------------------------------------------------------------
-module(memory_accounting_demo).

-export([run/0, benchmark_100_connections/0, benchmark_10k_connections/0]).

run() ->
    io:format("~n=== Memory Accounting Demo ===~n~n"),
    
    %% Demo 1: Basic measurement
    demo_basic_measurement(),
    
    %% Demo 2: Small scale (100 connections)
    benchmark_100_connections(),
    
    %% Demo 3: Medium scale (10K connections) - comment out for quick demo
    %% benchmark_10k_connections(),
    
    io:format("~n=== Demo Complete ===~n"),
    ok.

%%====================================================================
%% Demo Functions
%%====================================================================

demo_basic_measurement() ->
    io:format("--- Demo 1: Basic Measurement ---~n"),
    
    %% Get current node baseline
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => []
    }),
    
    %% Validate
    ok = erlmcp_memory_accounting:validate_decomposition(Decomp),
    
    %% Print compact format
    Compact = erlmcp_memory_accounting:format_compact(Decomp),
    io:format("Node baseline (0 connections):~n"),
    io:format("  Base overhead: ~.2f MiB~n", 
              [maps:get(per_node_base_overhead_mib, Compact)]),
    io:format("  Total RSS:     ~.2f MiB~n", 
              [maps:get(per_node_total_rss_mib, Compact)]),
    io:format("~n"),
    ok.

benchmark_100_connections() ->
    io:format("--- Demo 2: 100 Connection Benchmark ---~n"),
    
    %% Start erlmcp application if needed
    application:ensure_all_started(erlmcp),
    
    %% Create 100 test processes (simulating connections)
    io:format("Creating 100 test processes...~n"),
    Pids = [spawn(fun connection_worker/0) || _ <- lists:seq(1, 100)],
    
    %% Wait for processes to stabilize
    timer:sleep(100),
    
    %% Measure memory decomposition
    io:format("Measuring memory decomposition...~n"),
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => Pids
    }),
    
    %% Validate
    case erlmcp_memory_accounting:validate_decomposition(Decomp) of
        ok ->
            io:format("Validation: PASSED~n");
        {error, Reason} ->
            io:format("Validation: FAILED - ~p~n", [Reason])
    end,
    
    %% Display results
    Compact = erlmcp_memory_accounting:format_compact(Decomp),
    io:format("~nMemory Breakdown (100 connections):~n"),
    io:format("  Per-connection:~n"),
    io:format("    Heap:        ~.3f MiB~n", [maps:get(per_connection_heap_mib, Compact)]),
    io:format("    State:       ~.3f MiB~n", [maps:get(per_connection_state_mib, Compact)]),
    io:format("    Total:       ~.3f MiB~n", [maps:get(per_connection_total_mib, Compact)]),
    io:format("~n"),
    io:format("  Per-node:~n"),
    io:format("    Overhead:    ~.2f MiB~n", [maps:get(per_node_base_overhead_mib, Compact)]),
    io:format("    Total RSS:   ~.2f MiB~n", [maps:get(per_node_total_rss_mib, Compact)]),
    io:format("    Connections: ~B~n", [maps:get(connections, Compact)]),
    io:format("~n"),
    
    %% Calculate efficiency
    TotalPerConn = maps:get(per_connection_total_mib, Compact),
    Overhead = maps:get(per_node_base_overhead_mib, Compact),
    TotalRss = maps:get(per_node_total_rss_mib, Compact),
    Efficiency = (100 * TotalPerConn) / (TotalRss / 100),
    
    io:format("  Efficiency:~n"),
    io:format("    Connection memory:   ~.2f MiB (~.1f%% of total)~n", 
              [100 * TotalPerConn, Efficiency]),
    io:format("    Infrastructure:      ~.2f MiB (~.1f%% of total)~n",
              [Overhead, 100 - Efficiency]),
    io:format("~n"),
    
    %% Show full report
    Report = erlmcp_memory_accounting:format_report(Decomp),
    io:format("Full decomposition report:~n~s~n", [Report]),
    
    %% Cleanup
    [exit(P, kill) || P <- Pids],
    ok.

benchmark_10k_connections() ->
    io:format("--- Demo 3: 10K Connection Benchmark ---~n"),
    io:format("Creating 10,000 test processes (this may take a moment)...~n"),
    
    %% Create in batches to avoid overwhelming scheduler
    BatchSize = 1000,
    Pids = lists:flatten([
        begin
            Batch = [spawn(fun connection_worker/0) || _ <- lists:seq(1, BatchSize)],
            io:format("  Created batch ~B/10 (~B connections)~n", 
                     [I, I * BatchSize]),
            Batch
        end || I <- lists:seq(1, 10)
    ]),
    
    %% Wait for processes to stabilize
    timer:sleep(500),
    
    %% Measure
    io:format("Measuring memory decomposition...~n"),
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => Pids
    }),
    
    %% Validate
    ok = erlmcp_memory_accounting:validate_decomposition(Decomp),
    
    %% Display compact results
    Compact = erlmcp_memory_accounting:format_compact(Decomp),
    io:format("~nMemory Breakdown (10K connections):~n"),
    io:format("  Per-connection total:  ~.3f MiB~n", 
              [maps:get(per_connection_total_mib, Compact)]),
    io:format("  Node total RSS:        ~.2f MiB~n", 
              [maps:get(per_node_total_rss_mib, Compact)]),
    io:format("  Expected at 100K:      ~.2f GiB~n",
              [maps:get(per_node_total_rss_mib, Compact) * 10 / 1024]),
    io:format("~n"),
    
    %% Cleanup
    io:format("Cleaning up...~n"),
    [exit(P, kill) || P <- Pids],
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Simple worker process that holds state
connection_worker() ->
    %% Simulate connection state
    State = #{
        id => make_ref(),
        buffer => <<0:8192>>,  % 1KB buffer
        queue => queue:new(),
        metadata => #{timestamp => erlang:system_time(millisecond)}
    },
    connection_loop(State).

connection_loop(State) ->
    receive
        {update, NewData} ->
            connection_loop(State#{data => NewData});
        stop ->
            ok
    after 300000 ->
        connection_loop(State)
    end.
