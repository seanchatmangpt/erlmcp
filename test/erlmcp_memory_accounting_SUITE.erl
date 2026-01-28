%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test Suite for erlmcp_memory_accounting
%%%
%%% Tests all memory decomposition and validation functions
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_accounting_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        test_measure_per_connection_heap,
        test_measure_per_connection_state,
        test_measure_per_node_rss,
        test_measure_per_node_base_overhead,
        test_measure_cluster_total,
        test_decompose_single_connection,
        test_decompose_multiple_connections,
        test_validate_decomposition_valid,
        test_validate_decomposition_invalid,
        test_format_report,
        test_format_compact,
        test_aggregate_measurements,
        test_get_connection_process_info,
        test_estimate_state_size,
        test_zero_connections,
        test_large_scale_decomposition,
        test_decomposition_with_dead_processes,
        test_conformance_no_ambiguous_reports
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_measure_per_connection_heap(_Config) ->
    % Spawn a test process
    Pid = spawn(fun() -> timer:sleep(10000) end),
    
    % Measure heap
    {ok, HeapBytes} = erlmcp_memory_accounting:measure_per_connection_heap(Pid),
    
    % Verify result
    ?assert(is_integer(HeapBytes)),
    ?assert(HeapBytes > 0),
    
    % Clean up
    exit(Pid, kill),
    ok.

test_measure_per_connection_state(_Config) ->
    % Start a gen_server with known state
    {ok, Pid} = erlmcp_memory_accounting_test_server:start_link(#{initial_data => lists:seq(1, 1000)}),
    
    % Measure state
    {ok, StateBytes} = erlmcp_memory_accounting:measure_per_connection_state(Pid),
    
    % Verify result
    ?assert(is_integer(StateBytes)),
    ?assert(StateBytes > 0),
    
    % Clean up
    gen_server:stop(Pid),
    ok.

test_measure_per_node_rss(_Config) ->
    % Measure node RSS
    {ok, RssBytes} = erlmcp_memory_accounting:measure_per_node_rss(),
    
    % Verify result
    ?assert(is_integer(RssBytes)),
    ?assert(RssBytes > 1024 * 1024),  % At least 1 MiB
    
    ct:pal("Node RSS: ~.2f MiB", [RssBytes / (1024 * 1024)]),
    ok.

test_measure_per_node_base_overhead(_Config) ->
    % Measure base overhead
    {ok, OverheadBytes} = erlmcp_memory_accounting:measure_per_node_base_overhead(#{
        server_pid => self(),
        registry_pid => undefined,
        supervisor_pids => []
    }),
    
    % Verify result
    ?assert(is_integer(OverheadBytes)),
    ?assert(OverheadBytes > 0),
    
    ct:pal("Base overhead: ~.2f MiB", [OverheadBytes / (1024 * 1024)]),
    ok.

test_measure_cluster_total(_Config) ->
    % Measure cluster total (single node)
    {ok, ClusterBytes} = erlmcp_memory_accounting:measure_cluster_total([node()]),
    
    % Verify result
    ?assert(is_integer(ClusterBytes)),
    ?assert(ClusterBytes > 0),
    
    ct:pal("Cluster total: ~.2f MiB", [ClusterBytes / (1024 * 1024)]),
    ok.

test_decompose_single_connection(_Config) ->
    % Create a test connection
    {ok, Pid} = erlmcp_memory_accounting_test_server:start_link(#{data => <<"test">>}),
    
    % Decompose memory
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => [Pid],
        server_pid => self(),
        registry_pid => undefined,
        supervisor_pids => []
    }),
    
    % Verify decomposition structure
    ?assert(is_tuple(Decomp)),
    ?assert(element(1, Decomp) =:= memory_decomposition),
    
    % Extract fields (record access without include)
    PerConnHeapMiB = element(2, Decomp),
    PerConnStateMiB = element(3, Decomp),
    PerConnTotalMiB = element(4, Decomp),
    Connections = element(8, Decomp),
    ClusterNodes = element(9, Decomp),
    
    ?assert(PerConnHeapMiB >= 0),
    ?assert(PerConnStateMiB >= 0),
    ?assert(PerConnTotalMiB >= 0),
    ?assert(Connections =:= 1),
    ?assert(ClusterNodes =:= 1),
    
    % Validate
    ok = erlmcp_memory_accounting:validate_decomposition(Decomp),
    
    % Clean up
    gen_server:stop(Pid),
    ok.

test_decompose_multiple_connections(_Config) ->
    % Create multiple test connections
    Pids = [begin
        {ok, P} = erlmcp_memory_accounting_test_server:start_link(#{data => I}),
        P
    end || I <- lists:seq(1, 10)],
    
    % Decompose memory
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => Pids,
        server_pid => self(),
        registry_pid => undefined,
        supervisor_pids => []
    }),
    
    % Verify
    Connections = element(8, Decomp),
    PerConnTotalMiB = element(4, Decomp),
    ?assert(Connections =:= 10),
    ?assert(PerConnTotalMiB > 0),
    
    % Validate
    ok = erlmcp_memory_accounting:validate_decomposition(Decomp),
    
    % Print report
    Report = erlmcp_memory_accounting:format_report(Decomp),
    ct:pal("Decomposition report:~n~s", [Report]),
    
    % Clean up
    [gen_server:stop(P) || P <- Pids],
    ok.

test_validate_decomposition_valid(_Config) ->
    % Create valid decomposition tuple manually
    Decomp = {memory_decomposition,
              0.050,   % per_connection_heap_mib
              0.012,   % per_connection_state_mib
              0.062,   % per_connection_total_mib
              150.0,   % per_node_base_overhead_mib
              200.0,   % per_node_total_rss_mib
              1000,    % per_node_process_count
              100,     % connections
              1,       % cluster_nodes
              200.0,   % cluster_total_rss_mib
              true,    % scope_per_node
              false,   % scope_per_cluster
              erlang:system_time(millisecond),  % timestamp
              node(),  % node
              []       % measured_pids
    },
    
    % Should pass validation
    Result = erlmcp_memory_accounting:validate_decomposition(Decomp),
    ?assertEqual(ok, Result),
    ok.

test_validate_decomposition_invalid(_Config) ->
    % Create invalid decomposition (heap + state != total)
    Decomp = {memory_decomposition,
              0.050,   % per_connection_heap_mib
              0.012,   % per_connection_state_mib
              1.000,   % per_connection_total_mib - WRONG!
              150.0,   % per_node_base_overhead_mib
              200.0,   % per_node_total_rss_mib
              1000,    % per_node_process_count
              100,     % connections
              1,       % cluster_nodes
              200.0,   % cluster_total_rss_mib
              true,    % scope_per_node
              false,   % scope_per_cluster
              erlang:system_time(millisecond),
              node(),
              []
    },
    
    % Should fail validation
    Result = erlmcp_memory_accounting:validate_decomposition(Decomp),
    ?assertMatch({error, {invalid_per_connection_sum, _}}, Result),
    ok.

test_format_report(_Config) ->
    % Create decomposition
    {ok, Pid} = erlmcp_memory_accounting_test_server:start_link(#{}),
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => [Pid],
        server_pid => self()
    }),
    
    % Format report
    Report = erlmcp_memory_accounting:format_report(Decomp),
    
    % Verify report structure
    ?assert(is_binary(Report)),
    ?assert(byte_size(Report) > 100),
    ?assert(binary:match(Report, <<"MEMORY DECOMPOSITION REPORT">>) =/= nomatch),
    ?assert(binary:match(Report, <<"Per-Connection Memory">>) =/= nomatch),
    ?assert(binary:match(Report, <<"Per-Node Memory">>) =/= nomatch),
    
    ct:pal("Report:~n~s", [Report]),
    
    gen_server:stop(Pid),
    ok.

test_format_compact(_Config) ->
    % Create decomposition
    {ok, Pid} = erlmcp_memory_accounting_test_server:start_link(#{}),
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => [Pid]
    }),
    
    % Format compact
    Compact = erlmcp_memory_accounting:format_compact(Decomp),
    
    % Verify compact structure
    ?assert(is_map(Compact)),
    ?assert(maps:is_key(per_connection_heap_mib, Compact)),
    ?assert(maps:is_key(per_connection_state_mib, Compact)),
    ?assert(maps:is_key(per_connection_total_mib, Compact)),
    ?assert(maps:is_key(per_node_base_overhead_mib, Compact)),
    ?assert(maps:is_key(per_node_total_rss_mib, Compact)),
    
    ct:pal("Compact: ~p", [Compact]),
    
    gen_server:stop(Pid),
    ok.

test_aggregate_measurements(_Config) ->
    % Create multiple decompositions
    Pids = [begin
        {ok, P} = erlmcp_memory_accounting_test_server:start_link(#{size => I * 100}),
        P
    end || I <- lists:seq(1, 5)],
    
    Decomps = [erlmcp_memory_accounting:decompose(#{
        connection_pids => [P]
    }) || P <- Pids],
    
    % Aggregate
    Agg = erlmcp_memory_accounting:aggregate_measurements(Decomps),
    
    % Verify
    ?assert(is_map(Agg)),
    ?assertEqual(5, maps:get(count, Agg)),
    ?assert(is_map(maps:get(heap_mib, Agg))),
    ?assert(is_map(maps:get(state_mib, Agg))),
    ?assert(is_map(maps:get(total_mib, Agg))),
    
    ct:pal("Aggregated: ~p", [Agg]),
    
    [gen_server:stop(P) || P <- Pids],
    ok.

test_get_connection_process_info(_Config) ->
    Pid = spawn(fun() -> timer:sleep(10000) end),
    
    Info = erlmcp_memory_accounting:get_connection_process_info(Pid),
    
    ?assert(is_map(Info)),
    ?assert(maps:is_key(memory, Info)),
    ?assert(maps:is_key(heap_size, Info)),
    ?assert(maps:is_key(total_heap_size, Info)),
    
    ct:pal("Process info: ~p", [Info]),
    
    exit(Pid, kill),
    ok.

test_estimate_state_size(_Config) ->
    % Test various data structures
    SmallState = #{a => 1, b => 2},
    LargeState = #{data => lists:seq(1, 10000)},
    
    SmallSize = erlmcp_memory_accounting:estimate_state_size(SmallState),
    LargeSize = erlmcp_memory_accounting:estimate_state_size(LargeState),
    
    ?assert(is_integer(SmallSize)),
    ?assert(is_integer(LargeSize)),
    ?assert(LargeSize > SmallSize),
    
    ct:pal("Small: ~B bytes, Large: ~B bytes", [SmallSize, LargeSize]),
    ok.

test_zero_connections(_Config) ->
    % Test with zero connections
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => []
    }),
    
    % Extract fields
    PerConnHeapMiB = element(2, Decomp),
    PerConnStateMiB = element(3, Decomp),
    PerNodeBaseOverheadMiB = element(5, Decomp),
    
    % Should still have node overhead
    ?assert(PerConnHeapMiB =:= 0.0),
    ?assert(PerConnStateMiB =:= 0.0),
    ?assert(PerNodeBaseOverheadMiB > 0),
    
    ok.

test_large_scale_decomposition(_Config) ->
    % Create 100 connections
    ct:pal("Creating 100 test connections..."),
    Pids = [begin
        {ok, P} = erlmcp_memory_accounting_test_server:start_link(#{id => I}),
        P
    end || I <- lists:seq(1, 100)],
    
    % Decompose
    ct:pal("Decomposing memory..."),
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => Pids
    }),
    
    % Validate
    ok = erlmcp_memory_accounting:validate_decomposition(Decomp),
    
    % Log results
    Compact = erlmcp_memory_accounting:format_compact(Decomp),
    ct:pal("100 connections memory breakdown:~n~p", [Compact]),
    
    % Verify reasonable values
    PerConnTotalMiB = element(4, Decomp),
    ?assert(PerConnTotalMiB < 10.0),
    
    % Clean up
    [gen_server:stop(P) || P <- Pids],
    ok.

test_decomposition_with_dead_processes(_Config) ->
    % Create connections and kill some
    Pids = [begin
        {ok, P} = erlmcp_memory_accounting_test_server:start_link(#{}),
        P
    end || _ <- lists:seq(1, 5)],
    
    % Kill half
    [exit(P, kill) || P <- lists:sublist(Pids, 3)],
    timer:sleep(100),
    
    % Should handle dead processes gracefully
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => Pids
    }),
    
    % Should still produce valid decomposition (with fewer measured connections)
    ?assert(is_tuple(Decomp)),
    ?assert(element(1, Decomp) =:= memory_decomposition),
    
    % Clean up living processes
    [catch gen_server:stop(P) || P <- Pids],
    ok.

test_conformance_no_ambiguous_reports(_Config) ->
    % This test enforces the rule: NO "MiB/conn" without decomposition
    {ok, Pid} = erlmcp_memory_accounting_test_server:start_link(#{}),
    
    % Get decomposition
    Decomp = erlmcp_memory_accounting:decompose(#{
        connection_pids => [Pid]
    }),
    
    % MUST validate before reporting
    ok = erlmcp_memory_accounting:validate_decomposition(Decomp),
    
    % Report MUST include all components
    Compact = erlmcp_memory_accounting:format_compact(Decomp),
    
    % Verify all required fields present
    RequiredFields = [
        per_connection_heap_mib,
        per_connection_state_mib,
        per_connection_total_mib,
        per_node_base_overhead_mib,
        per_node_total_rss_mib,
        connections,
        cluster_nodes,
        cluster_total_rss_mib,
        scope_per_node,
        scope_per_cluster
    ],
    
    [?assert(maps:is_key(Field, Compact), 
             io_lib:format("Missing required field: ~p", [Field])) 
     || Field <- RequiredFields],
    
    ct:pal("Conformance test passed. All components present:~n~p", [Compact]),
    
    gen_server:stop(Pid),
    ok.
