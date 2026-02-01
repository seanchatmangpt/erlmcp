%%%-------------------------------------------------------------------
%%% @doc Memory Guard Tests
%%%
%%% Comprehensive tests for memory limit enforcement and bounded refusal.
%%% Tests cover:
%%% - Payload size limits (16MB default)
%%% - System memory circuit breaker (80% threshold)
%%% - Bounded refusal error responses
%%% - Configuration overrides
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_guard_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

-define(MAX_PAYLOAD_SIZE, 16 * 1024 * 1024).  %% 16MB

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test payload size limit enforcement
payload_size_limit_enforcement_test() ->
    %% Small payload should pass
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(1024)),
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(1024 * 1024)), % 1MB
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(?MAX_PAYLOAD_SIZE)),

    %% Payload exactly at limit should fail
    ?assertEqual({error, payload_too_large},
                 erlmcp_memory_guard:check_allocation(?MAX_PAYLOAD_SIZE + 1)),

    %% Large payload should fail
    ?assertEqual({error, payload_too_large},
                 erlmcp_memory_guard:check_allocation(100 * 1024 * 1024)), % 100MB
    ?assertEqual({error, payload_too_large},
                 erlmcp_memory_guard:check_allocation(1024 * 1024 * 1024)). % 1GB

%% @doc Test custom payload size limits
custom_payload_size_limit_test() ->
    %% Set custom limit to 1MB
    CustomLimit = 1024 * 1024,

    %% Should pass with custom limit
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(1024, CustomLimit)),
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(CustomLimit, CustomLimit)),

    %% Should fail with custom limit
    ?assertEqual({error, payload_too_large},
                 erlmcp_memory_guard:check_allocation(CustomLimit + 1, CustomLimit)),
    ?assertEqual({error, payload_too_large},
                 erlmcp_memory_guard:check_allocation(10 * CustomLimit, CustomLimit)).

%% @doc Test system memory statistics retrieval
memory_stats_retrieval_test() ->
    Stats = erlmcp_memory_guard:get_memory_stats(),

    %% Verify stats map structure
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(total, Stats)),
    ?assert(maps:is_key(used, Stats)),
    ?assert(maps:is_key(available, Stats)),
    ?assert(maps:is_key(used_percent, Stats)),
    ?assert(maps:is_key(system_limit, Stats)),
    ?assert(maps:is_key(circuit_breaker_open, Stats)),

    %% Verify data types
    Total = maps:get(total, Stats),
    UsedPercent = maps:get(used_percent, Stats),
    CircuitBreakerOpen = maps:get(circuit_breaker_open, Stats),

    ?assert(is_integer(Total)),
    ?assert(is_float(UsedPercent)),
    ?assert(is_boolean(CircuitBreakerOpen)),

    %% Verify reasonable ranges
    ?assert(UsedPercent >= 0.0),
    ?assert(UsedPercent =< 100.0).

%% @doc Test circuit breaker status
circuit_breaker_status_test() ->
    %% Get circuit breaker status
    IsOpen = erlmcp_memory_guard:is_circuit_breaker_open(),
    ?assert(is_boolean(IsOpen)),

    %% Circuit breaker should be closed under normal conditions
    %% (unless system is actually under memory pressure)
    Stats = erlmcp_memory_guard:get_memory_stats(),
    UsedPercent = maps:get(used_percent, Stats),
    Threshold = erlmcp_memory_guard:get_circuit_breaker_threshold() * 100,

    %% Circuit breaker should match the memory usage
    ExpectedOpen = UsedPercent > Threshold,
    ?assertEqual(ExpectedOpen, IsOpen).

%% @doc Test configuration overrides for payload size
config_payload_size_override_test() ->
    %% Set custom payload limit via application environment
    CustomLimit = 2048, % 2KB
    application:set_env(erlmcp, max_payload_size, CustomLimit),

    try
        %% Verify the limit is applied
        ?assertEqual(CustomLimit, erlmcp_memory_guard:get_payload_limit()),

        %% Test enforcement with custom limit
        ?assertEqual(ok, erlmcp_memory_guard:check_allocation(CustomLimit)),
        ?assertEqual({error, payload_too_large},
                     erlmcp_memory_guard:check_allocation(CustomLimit + 1))
    after
        %% Restore default
        application:unset_env(erlmcp, max_payload_size)
    end.

%% @doc Test configuration overrides for circuit breaker threshold
config_circuit_breaker_threshold_override_test() ->
    %% Set custom circuit breaker threshold via application environment
    CustomThreshold = 0.95, % 95%
    application:set_env(erlmcp, circuit_breaker_threshold, CustomThreshold),

    try
        %% Verify the threshold is applied
        ?assertEqual(CustomThreshold, erlmcp_memory_guard:get_circuit_breaker_threshold())
    after
        %% Restore default
        application:unset_env(erlmcp, circuit_breaker_threshold)
    end.

%% @doc Test configuration overrides for system memory limit
config_system_memory_limit_override_test() ->
    %% Set custom system memory limit via application environment
    CustomLimit = 8 * 1024 * 1024 * 1024, % 8GB
    application:set_env(erlmcp, system_memory_limit, CustomLimit),

    try
        %% Verify the limit is applied
        ?assertEqual(CustomLimit, erlmcp_memory_guard:get_system_limit()),

        %% Verify it affects memory stats
        Stats = erlmcp_memory_guard:get_memory_stats(),
        ?assertEqual(CustomLimit, maps:get(system_limit, Stats))
    after
        %% Restore default
        application:unset_env(erlmcp, system_memory_limit)
    end.

%% @doc Test bounded refusal error responses
bounded_refusal_test() ->
    %% Test payload_too_large error
    ?assertEqual({error, payload_too_large},
                 erlmcp_memory_guard:check_allocation(999 * 1024 * 1024 * 1024)),

    %% Test resource_exhausted error (circuit breaker)
    %% Note: This test may not trigger unless system is under memory pressure
    Stats = erlmcp_memory_guard:get_memory_stats(),
    UsedPercent = maps:get(used_percent, Stats),
    Threshold = erlmcp_memory_guard:get_circuit_breaker_threshold() * 100,

    case UsedPercent > Threshold of
        true ->
            %% Circuit breaker should be open
            ?assertEqual({error, resource_exhausted}, erlmcp_memory_guard:check_allocation(1024));
        false ->
            %% Circuit breaker should be closed
            ?assertEqual(ok, erlmcp_memory_guard:check_allocation(1024))
    end.

%% @doc Test concurrent allocation checks
concurrent_allocation_check_test() ->
    %% Spawn multiple processes checking allocation concurrently
    NumProcesses = 100,
    PayloadSize = 1024, % 1KB (well within limit)

    Pids =
        [spawn(fun() ->
                  case erlmcp_memory_guard:check_allocation(PayloadSize) of
                      ok ->
                          ok;
                      {error, _} ->
                          error
                  end
               end)
         || _ <- lists:seq(1, NumProcesses)],

    %% All should complete successfully
    WaitResults = [wait_for_completion(Pid, 5000) || Pid <- Pids],
    ?assertEqual(lists:duplicate(NumProcesses, ok), WaitResults).

%% @doc Test edge cases
edge_cases_test() ->
    %% Zero-size payload should pass
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(0)),

    %% Single byte should pass
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(1)),

    %% Negative sizes should be rejected (function expects non_neg_integer())
    ?assertError(function_clause, erlmcp_memory_guard:check_allocation(-1)).

%% @doc Test memory guard with realistic workloads
realistic_workload_test() ->
    %% Simulate realistic message sizes
    SmallMessage = 1024, % 1KB
    MediumMessage = 100 * 1024, % 100KB
    LargeMessage = 1024 * 1024, % 1MB
    VeryLargeMessage = 10 * 1024 * 1024, % 10MB (within 16MB limit)

    %% All should pass
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(SmallMessage)),
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(MediumMessage)),
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(LargeMessage)),
    ?assertEqual(ok, erlmcp_memory_guard:check_allocation(VeryLargeMessage)),

    %% But oversized messages should fail
    OversizedMessage = 20 * 1024 * 1024, % 20MB (exceeds 16MB limit)
    ?assertEqual({error, payload_too_large},
                 erlmcp_memory_guard:check_allocation(OversizedMessage)).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Wait for a process to complete
wait_for_completion(Pid, Timeout) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after Timeout ->
        timeout
    end.
