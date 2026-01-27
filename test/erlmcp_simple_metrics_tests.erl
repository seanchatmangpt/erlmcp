-module(erlmcp_simple_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup/Cleanup
%%====================================================================

setup() ->
    erlmcp_simple_metrics:start(),
    ok.

cleanup(_) ->
    erlmcp_simple_metrics:stop(),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

basic_functionality_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        ?_test(test_start_stop()),
        ?_test(test_counter_increment()),
        ?_test(test_latency_recording()),
        ?_test(test_convenience_functions()),
        ?_test(test_stats_retrieval()),
        ?_test(test_reset_functionality())
    ]}.

test_start_stop() ->
    % Already started in setup
    ?assertEqual({error, already_exists}, erlmcp_simple_metrics:start()),
    ?assertEqual(ok, erlmcp_simple_metrics:stop()),
    ?assertEqual(ok, erlmcp_simple_metrics:start()).

test_counter_increment() ->
    erlmcp_simple_metrics:reset(),
    
    % Test basic increment
    ?assertEqual(ok, erlmcp_simple_metrics:increment(test_counter, 1)),
    ?assertEqual(ok, erlmcp_simple_metrics:increment(test_counter, 5)),
    
    Stats = erlmcp_simple_metrics:get_stats(),
    Counters = maps:get(counters, Stats),
    ?assertEqual(6, maps:get(test_counter, Counters)).

test_latency_recording() ->
    erlmcp_simple_metrics:reset(),
    
    % Record some latencies
    ?assertEqual(ok, erlmcp_simple_metrics:record_latency(test_op, 10.0)),
    ?assertEqual(ok, erlmcp_simple_metrics:record_latency(test_op, 20.0)),
    ?assertEqual(ok, erlmcp_simple_metrics:record_latency(test_op, 30.0)),
    
    Stats = erlmcp_simple_metrics:get_stats(),
    Latencies = maps:get(latencies, Stats),
    TestOpStats = maps:get(test_op, Latencies),
    
    ?assertEqual(3, maps:get(count, TestOpStats)),
    ?assertEqual(10.0, maps:get(min_ms, TestOpStats)),
    ?assertEqual(30.0, maps:get(max_ms, TestOpStats)),
    ?assertEqual(20.0, maps:get(avg_ms, TestOpStats)).

test_convenience_functions() ->
    erlmcp_simple_metrics:reset(),
    
    % Test convenience functions
    ?assertEqual(ok, erlmcp_simple_metrics:request()),
    ?assertEqual(ok, erlmcp_simple_metrics:request()),
    ?assertEqual(ok, erlmcp_simple_metrics:error()),
    ?assertEqual(ok, erlmcp_simple_metrics:success()),
    
    Stats = erlmcp_simple_metrics:get_stats(),
    Counters = maps:get(counters, Stats),
    
    ?assertEqual(2, maps:get(request_count, Counters)),
    ?assertEqual(1, maps:get(error_count, Counters)),
    ?assertEqual(1, maps:get(success_count, Counters)).

test_stats_retrieval() ->
    erlmcp_simple_metrics:reset(),
    
    % Add some data
    erlmcp_simple_metrics:increment(test_counter, 42),
    erlmcp_simple_metrics:record_latency(test_latency, 123.45),
    
    Stats = erlmcp_simple_metrics:get_stats(),
    
    % Check structure
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(counters, Stats)),
    ?assert(maps:is_key(latencies, Stats)),
    ?assert(maps:is_key(system, Stats)),
    ?assert(maps:is_key(timestamp, Stats)),
    
    % Check system info
    System = maps:get(system, Stats),
    ?assert(maps:is_key(uptime_ms, System)),
    ?assert(maps:is_key(memory_total, System)),
    ?assert(maps:is_key(process_count, System)).

test_reset_functionality() ->
    % Add some data
    erlmcp_simple_metrics:increment(test_counter, 100),
    erlmcp_simple_metrics:record_latency(test_op, 50.0),
    
    % Verify data exists
    Stats1 = erlmcp_simple_metrics:get_stats(),
    Counters1 = maps:get(counters, Stats1),
    ?assert(maps:size(Counters1) > 0),
    
    % Reset
    ?assertEqual(ok, erlmcp_simple_metrics:reset()),
    
    % Verify reset
    Stats2 = erlmcp_simple_metrics:get_stats(),
    Counters2 = maps:get(counters, Stats2),
    Latencies2 = maps:get(latencies, Stats2),
    
    % Should have basic counters initialized to 0
    ?assertEqual(0, maps:get(request_count, Counters2, 0)),
    ?assertEqual(0, maps:get(error_count, Counters2, 0)),
    ?assertEqual(0, maps:get(success_count, Counters2, 0)),
    
    % Latencies should be empty
    ?assertEqual(0, maps:size(Latencies2)).

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {timeout, 10, ?_test(test_concurrent_access())}
    ]}.

test_concurrent_access() ->
    erlmcp_simple_metrics:reset(),
    
    % Spawn multiple processes doing concurrent operations
    NumProcs = 100,
    OpsPerProc = 100,
    
    Parent = self(),
    Pids = [spawn_link(fun() ->
        [begin
            erlmcp_simple_metrics:increment(concurrent_test, 1),
            erlmcp_simple_metrics:record_latency(concurrent_latency, float(I))
        end || I <- lists:seq(1, OpsPerProc)],
        Parent ! {done, self()}
    end) || _ <- lists:seq(1, NumProcs)],
    
    % Wait for all processes to complete
    [receive {done, Pid} -> ok end || Pid <- Pids],
    
    % Check results
    Stats = erlmcp_simple_metrics:get_stats(),
    Counters = maps:get(counters, Stats),
    Latencies = maps:get(latencies, Stats),
    
    % Should have NumProcs * OpsPerProc increments
    ExpectedCount = NumProcs * OpsPerProc,
    ?assertEqual(ExpectedCount, maps:get(concurrent_test, Counters)),
    
    % Should have recorded latencies
    ConcurrentLatency = maps:get(concurrent_latency, Latencies),
    ?assertEqual(ExpectedCount, maps:get(count, ConcurrentLatency)).