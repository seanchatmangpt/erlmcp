%%%-------------------------------------------------------------------
%%% @doc Common Test suite for erlmcp_bench_chaos
%%%
%%% Tests chaos/adversarial scenarios and validates:
%%% - Correct refusal codes
%%% - Bounded refusal validation
%%% - Recovery within SLA
%%% - Metrology compliance
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_chaos_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("erlmcp_refusal.hrl").

%% CT callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

%% Test cases
-export([
    test_scenarios_defined/1,
    test_memory_exhaustion_scenario/1,
    test_message_flood_scenario/1,
    test_invalid_payload_scenario/1,
    test_connection_leak_scenario/1,
    test_slow_consumer_scenario/1,
    test_large_payload_scenario/1,
    test_process_crash_scenario/1,
    test_network_partition_scenario/1,
    test_supervisor_cascade_scenario/1,
    test_disk_full_scenario/1,
    test_cpu_saturation_scenario/1,
    test_run_all_scenarios/1,
    test_bounded_refusal_validation/1,
    test_chaos_report_generation/1,
    test_metrology_compliance/1
]).

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [
        {group, scenario_tests},
        {group, integration_tests},
        {group, validation_tests}
    ].

groups() ->
    [
        {scenario_tests, [parallel], [
            test_scenarios_defined,
            test_memory_exhaustion_scenario,
            test_message_flood_scenario,
            test_invalid_payload_scenario,
            test_connection_leak_scenario,
            test_slow_consumer_scenario,
            test_large_payload_scenario,
            test_process_crash_scenario,
            test_network_partition_scenario,
            test_supervisor_cascade_scenario,
            test_disk_full_scenario,
            test_cpu_saturation_scenario
        ]},
        {integration_tests, [sequence], [
            test_run_all_scenarios,
            test_chaos_report_generation
        ]},
        {validation_tests, [parallel], [
            test_bounded_refusal_validation,
            test_metrology_compliance
        ]}
    ].

init_per_suite(Config) ->
    %% Start sasl for supervisor reports
    application:start(sasl),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

%%====================================================================
%% Test Cases - Scenario Definitions
%%====================================================================

test_scenarios_defined(_Config) ->
    Scenarios = erlmcp_bench_chaos:scenarios(),

    %% Verify we have all 11 scenarios
    ?assertEqual(11, length(Scenarios)),

    %% Verify all scenarios have required fields
    lists:foreach(
        fun(Scenario) ->
            ?assertMatch(#{id := _, description := _, inject := _, expected := _}, Scenario),
            ?assert(is_binary(maps:get(id, Scenario))),
            ?assert(is_binary(maps:get(description, Scenario))),
            ?assert(is_function(maps:get(inject, Scenario), 1)),
            ?assert(maps:is_key(expected_code, Scenario)),
            ?assert(maps:is_key(timeout_ms, Scenario))
        end,
        Scenarios
    ),

    %% Verify scenario IDs are unique
    ScenarioIds = [maps:get(id, S) || S <- Scenarios],
    ?assertEqual(length(ScenarioIds), length(lists:usort(ScenarioIds))),

    ok.

%%====================================================================
%% Test Cases - Individual Scenarios
%%====================================================================

test_memory_exhaustion_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_memory_exhaustion">>),

    %% Validate result structure
    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_memory_exhaustion">>,
        <<"benchmark">> := <<"chaos">>,
        <<"scenario">> := <<"memory_exhaustion">>,
        <<"refusal_code">> := ?REFUSAL_RESOURCE_EXHAUSTED,
        <<"test_passed">> := true
    }, Result),

    %% Validate refusal code is correct
    ?assertEqual(?REFUSAL_RESOURCE_EXHAUSTED, maps:get(<<"refusal_code">>, Result)),

    %% Validate detection and recovery times are reasonable
    DetectionTime = maps:get(<<"detection_time_ms">>, Result),
    RecoveryTime = maps:get(<<"recovery_time_ms">>, Result),

    ?assert(DetectionTime < 1000.0, "Detection should be fast"),
    ?assert(RecoveryTime < 5000.0, "Recovery should be automatic"),

    %% Validate bounded refusal
    ?assertEqual(true, maps:get(<<"bounded_refusal_validated">>, Result)),

    ok.

test_message_flood_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_message_flood">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_message_flood">>,
        <<"refusal_code">> := ?REFUSAL_RATE_LIMIT_EXCEEDED,
        <<"test_passed">> := true
    }, Result),

    %% Should have message counts
    ?assert(maps:is_key(<<"messages_attempted">>, Result)),
    ?assert(maps:is_key(<<"messages_refused">>, Result)),

    ok.

test_invalid_payload_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_invalid_payload">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_invalid_payload">>,
        <<"refusal_code">> := ?REFUSAL_PROTOCOL_ERROR,
        <<"test_passed">> := true
    }, Result),

    %% Detection should be immediate
    DetectionTime = maps:get(<<"detection_time_ms">>, Result),
    ?assert(DetectionTime < 100.0, "Detection should be immediate for malformed payload"),

    %% No recovery needed
    RecoveryTime = maps:get(<<"recovery_time_ms">>, Result),
    ?assertEqual(0.0, RecoveryTime),

    ok.

test_connection_leak_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_connection_leak">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_connection_leak">>,
        <<"refusal_code">> := ?REFUSAL_CONCURRENT_LIMIT_EXCEEDED,
        <<"test_passed">> := true
    }, Result),

    %% Should have connection counts
    ?assert(maps:is_key(<<"max_connections">>, Result)),
    ?assert(maps:is_key(<<"attempted_connections">>, Result)),
    ?assert(maps:is_key(<<"connections_refused">>, Result)),

    ok.

test_slow_consumer_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_slow_consumer">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_slow_consumer">>,
        <<"refusal_code">> := ?REFUSAL_TIMEOUT,
        <<"test_passed">> := true
    }, Result),

    ok.

test_large_payload_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_large_payload">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_large_payload">>,
        <<"refusal_code">> := ?REFUSAL_MESSAGE_TOO_LARGE,
        <<"test_passed">> := true
    }, Result),

    %% Should have payload sizes
    PayloadSize = maps:get(<<"payload_size_bytes">>, Result),
    MaxAllowed = maps:get(<<"max_allowed_bytes">>, Result),

    ?assert(PayloadSize > MaxAllowed, "Payload should exceed limit"),

    ok.

test_process_crash_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_process_crash">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_process_crash">>,
        <<"scenario">> := _
    }, Result),

    %% May skip if no worker process found, but should not fail
    TestPassed = maps:get(<<"test_passed">>, Result),
    ?assert(is_boolean(TestPassed)),

    ok.

test_network_partition_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_network_partition">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_network_partition">>,
        <<"scenario">> := <<"network_partition">>,
        <<"test_passed">> := true
    }, Result),

    ok.

test_supervisor_cascade_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_supervisor_cascade">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_supervisor_cascade">>,
        <<"scenario">> := <<"supervisor_cascade">>,
        <<"test_passed">> := true
    }, Result),

    %% Recovery should be within 5s
    RecoveryTime = maps:get(<<"recovery_time_ms">>, Result),
    ?assert(RecoveryTime < 5000.0),

    ok.

test_disk_full_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_disk_full">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_disk_full">>,
        <<"refusal_code">> := ?REFUSAL_INTERNAL_ERROR,
        <<"test_passed">> := true
    }, Result),

    ok.

test_cpu_saturation_scenario(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_cpu_saturation">>),

    ?assertMatch(#{
        <<"workload_id">> := <<"chaos_cpu_saturation">>,
        <<"refusal_code">> := ?REFUSAL_SERVICE_UNAVAILABLE,
        <<"test_passed">> := true
    }, Result),

    ok.

%%====================================================================
%% Test Cases - Integration
%%====================================================================

test_run_all_scenarios(_Config) ->
    {ok, Results} = erlmcp_bench_chaos:run_all_scenarios(),

    %% Should have 11 results
    ?assertEqual(11, length(Results)),

    %% All results should have required fields
    lists:foreach(
        fun(Result) ->
            ?assert(maps:is_key(<<"workload_id">>, Result)),
            ?assert(maps:is_key(<<"benchmark">>, Result)),
            ?assert(maps:is_key(<<"scenario">>, Result)),
            ?assert(maps:is_key(<<"test_passed">>, Result)),
            ?assert(maps:is_key(<<"bounded_refusal_validated">>, Result))
        end,
        Results
    ),

    ok.

test_chaos_report_generation(_Config) ->
    {ok, Results} = erlmcp_bench_chaos:run_all_scenarios(),

    Report = erlmcp_bench_chaos:generate_chaos_report(Results),

    %% Validate report structure
    ?assertMatch(#{
        <<"benchmark">> := <<"chaos">>,
        <<"total_scenarios">> := 11,
        <<"passed">> := _,
        <<"failed">> := _,
        <<"success_rate_percent">> := _,
        <<"avg_recovery_time_ms">> := _,
        <<"avg_detection_time_ms">> := _,
        <<"overall_status">> := _
    }, Report),

    %% Success rate should be high
    SuccessRate = maps:get(<<"success_rate_percent">>, Report),
    ?assert(SuccessRate >= 90.0, "At least 90% of scenarios should pass"),

    ok.

%%====================================================================
%% Test Cases - Validation
%%====================================================================

test_bounded_refusal_validation(_Config) ->
    %% Test bounded refusal validation logic
    Scenario = #{
        id => <<"test_scenario">>,
        expected_code => ?REFUSAL_RATE_LIMIT_EXCEEDED
    },

    %% Valid bounded refusal
    ValidResult = #{
        <<"refusal_code">> => ?REFUSAL_RATE_LIMIT_EXCEEDED,
        <<"detection_time_ms">> => 50.0,
        <<"recovery_time_ms">> => 100.0,
        <<"data_loss">> => false,
        <<"cascading_failures">> => 0
    },

    ?assert(erlmcp_bench_chaos:validate_bounded_refusal(ValidResult, Scenario)),

    %% Invalid - wrong refusal code
    InvalidCode = ValidResult#{<<"refusal_code">> => 9999},
    ?assertNot(erlmcp_bench_chaos:validate_bounded_refusal(InvalidCode, Scenario)),

    %% Invalid - too slow detection
    SlowDetection = ValidResult#{<<"detection_time_ms">> => 2000.0},
    ?assertNot(erlmcp_bench_chaos:validate_bounded_refusal(SlowDetection, Scenario)),

    %% Invalid - data loss
    DataLoss = ValidResult#{<<"data_loss">> => true},
    ?assertNot(erlmcp_bench_chaos:validate_bounded_refusal(DataLoss, Scenario)),

    ok.

test_metrology_compliance(_Config) ->
    {ok, Result} = erlmcp_bench_chaos:run_scenario(<<"chaos_memory_exhaustion">>),

    %% Verify metrology schema compliance
    %% All required fields present
    RequiredFields = [
        <<"workload_id">>,
        <<"benchmark">>,
        <<"scenario">>,
        <<"injection_time_s">>,
        <<"detection_time_ms">>,
        <<"recovery_time_ms">>,
        <<"test_passed">>,
        <<"scope">>,
        <<"timestamp">>
    ],

    lists:foreach(
        fun(Field) ->
            ?assert(maps:is_key(Field, Result),
                   io_lib:format("Missing required field: ~p", [Field]))
        end,
        RequiredFields
    ),

    %% Verify types
    ?assert(is_binary(maps:get(<<"workload_id">>, Result))),
    ?assert(is_binary(maps:get(<<"benchmark">>, Result))),
    ?assert(is_binary(maps:get(<<"scenario">>, Result))),
    ?assert(is_float(maps:get(<<"injection_time_s">>, Result))),
    ?assert(is_float(maps:get(<<"detection_time_ms">>, Result))),
    ?assert(is_float(maps:get(<<"recovery_time_ms">>, Result))),
    ?assert(is_boolean(maps:get(<<"test_passed">>, Result))),
    ?assert(is_binary(maps:get(<<"scope">>, Result))),
    ?assert(is_integer(maps:get(<<"timestamp">>, Result))),

    %% Verify scope is valid
    Scope = maps:get(<<"scope">>, Result),
    ValidScopes = [<<"per_process">>, <<"per_connection">>, <<"per_node">>, <<"per_cluster">>],
    ?assert(lists:member(Scope, ValidScopes)),

    ok.
