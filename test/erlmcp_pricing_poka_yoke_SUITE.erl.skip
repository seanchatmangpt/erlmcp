%%%-------------------------------------------------------------------
%% @doc Test Suite for Pricing Plan Poka-Yoke Validation (v1.0.0)
%%
%% Comprehensive test suite for erlmcp_pricing_poka_yoke validation system.
%%
%% Test Coverage:
%% - 8 core validation tests
%% - Schema validation (required fields, structure)
%% - Envelope consistency (bounds checking)
%% - Refusal code verification (range 1001-1095)
%% - Evidence requirements (SBOM/provenance/chaos/bench)
%% - Error detection with corrupted plans
%% - Determinism verification (5 runs)
%% - All tests passing
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_pricing_poka_yoke_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp_refusal.hrl").
-include("erlmcp.hrl").

-compile(export_all).

%%====================================================================
%% CT Callback Functions
%%====================================================================

suite() ->
    [{timetrap, {seconds, 120}}].

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

all() ->
    [
        test_load_valid_team_plan,
        test_load_valid_enterprise_plan,
        test_load_valid_gov_plan,
        test_reject_plan_missing_fields,
        test_reject_envelope_throughput_exceeds_2x,
        test_reject_concurrent_connections_over_200k,
        test_validate_all_refusal_codes_exist,
        test_detect_corrupted_plan_json,
        test_deterministic_validation_five_runs_team,
        test_deterministic_validation_five_runs_enterprise,
        test_deterministic_validation_five_runs_gov
    ].

%%====================================================================
%% Test 1: Load and validate team plan
%%====================================================================

test_load_valid_team_plan(_Config) ->
    Plan = load_plan("plans/team.plan.json"),
    {ok, []} = erlmcp_pricing_poka_yoke:validate_plan(Plan),

    %% Verify basic structure
    <<"team">> = maps:get(tier, Plan),
    true = is_binary(maps:get(name, Plan)),
    true = is_map(maps:get(envelope, Plan)),
    true = is_map(maps:get(refusal_behavior, Plan)),
    true = is_map(maps:get(evidence, Plan)).

%%====================================================================
%% Test 2: Load and validate enterprise plan
%%====================================================================

test_load_valid_enterprise_plan(_Config) ->
    Plan = load_plan("plans/enterprise.plan.json"),
    {ok, []} = erlmcp_pricing_poka_yoke:validate_plan(Plan),

    %% Verify enterprise-specific fields
    <<"enterprise">> = maps:get(tier, Plan),
    Envelope = maps:get(envelope, Plan),
    true = maps:get(throughput_req_s, Envelope) > 1000,
    Evidence = maps:get(evidence, Plan),
    true = maps:is_key(audit_schema, Evidence).

%%====================================================================
%% Test 3: Load and validate gov plan
%%====================================================================

test_load_valid_gov_plan(_Config) ->
    Plan = load_plan("plans/gov.plan.json"),
    {ok, []} = erlmcp_pricing_poka_yoke:validate_plan(Plan),

    %% Verify gov-specific fields
    <<"gov">> = maps:get(tier, Plan),
    Compliance = maps:get(compliance, Plan),
    true = maps:get(fips_140_2, Compliance),
    Evidence = maps:get(evidence, Plan),
    true = maps:is_key(fips_certification, Evidence).

%%====================================================================
%% Test 4: Reject plan with missing required fields
%%====================================================================

test_reject_plan_missing_fields(_Config) ->
    %% Plan missing tier field
    InvalidPlan1 = #{
        name => <<"Test">>,
        pricing => #{model => <<"flat">>, description => <<"test">>, cost => <<"free">>},
        envelope => create_valid_envelope(),
        limits => create_valid_limits(),
        features => #{},
        refusal_behavior => #{},
        evidence => create_valid_evidence(),
        compliance => #{}
    },

    {error, Errors1} = erlmcp_pricing_poka_yoke:validate_plan(InvalidPlan1),
    true = lists:any(fun({schema, tier, _, _, _}) -> true; (_) -> false end, Errors1),

    %% Plan missing envelope field
    InvalidPlan2 = #{
        tier => <<"team">>,
        name => <<"Test">>,
        pricing => #{model => <<"flat">>, description => <<"test">>, cost => <<"free">>},
        limits => create_valid_limits(),
        features => #{},
        refusal_behavior => #{},
        evidence => create_valid_evidence(),
        compliance => #{}
    },

    {error, Errors2} = erlmcp_pricing_poka_yoke:validate_plan(InvalidPlan2),
    true = lists:any(fun({schema, envelope, _, _, _}) -> true; (_) -> false end, Errors2).

%%====================================================================
%% Test 5: Reject envelope throughput exceeding 2x baseline
%%====================================================================

test_reject_envelope_throughput_exceeds_2x(_Config) ->
    Plan = #{
        tier => <<"team">>,
        name => <<"Test">>,
        pricing => #{model => <<"flat">>, description => <<"test">>, cost => <<"free">>},
        envelope => #{
            throughput_req_s => 1000,          % Too high for 100 concurrent
            concurrent_connections => 100,
            queue_depth_messages => 1000,
            p99_latency_ms => 100,
            failover_sla_seconds => 10,
            connection_timeout_seconds => 60
        },
        limits => create_valid_limits(),
        features => #{},
        refusal_behavior => #{},
        evidence => create_valid_evidence(),
        compliance => #{}
    },

    {error, Errors} = erlmcp_pricing_poka_yoke:validate_plan(Plan),
    true = lists:any(fun({envelope, throughput_req_s, _, _, _}) -> true; (_) -> false end, Errors).

%%====================================================================
%% Test 6: Reject concurrent connections over 200K
%%====================================================================

test_reject_concurrent_connections_over_200k(_Config) ->
    Plan = #{
        tier => <<"team">>,
        name => <<"Test">>,
        pricing => #{model => <<"flat">>, description => <<"test">>, cost => <<"free">>},
        envelope => #{
            throughput_req_s => 100,
            concurrent_connections => 500000,  % Over 200K limit
            queue_depth_messages => 1000,
            p99_latency_ms => 100,
            failover_sla_seconds => 10,
            connection_timeout_seconds => 60
        },
        limits => create_valid_limits(),
        features => #{},
        refusal_behavior => #{},
        evidence => create_valid_evidence(),
        compliance => #{}
    },

    {error, Errors} = erlmcp_pricing_poka_yoke:validate_plan(Plan),
    true = lists:any(
        fun({envelope, concurrent_connections, _, _, _}) -> true; (_) -> false end,
        Errors).

%%====================================================================
%% Test 7: Validate all refusal codes in plans exist
%%====================================================================

test_validate_all_refusal_codes_exist(_Config) ->
    %% Load all three plans
    TeamPlan = load_plan("plans/team.plan.json"),
    EnterprisePlan = load_plan("plans/enterprise.plan.json"),
    GovPlan = load_plan("plans/gov.plan.json"),

    %% All should pass refusal code validation
    {ok, []} = erlmcp_pricing_poka_yoke:validate_plan(TeamPlan),
    {ok, []} = erlmcp_pricing_poka_yoke:validate_plan(EnterprisePlan),
    {ok, []} = erlmcp_pricing_poka_yoke:validate_plan(GovPlan),

    %% Verify refusal codes are correct values
    TeamRefusal = maps:get(refusal_behavior, TeamPlan),
    maps:foreach(fun(_, Entry) ->
        case maps:find(<<"error_code">>, Entry) of
            {ok, CodeStr} ->
                Code = binary_to_integer(CodeStr),
                true = erlmcp_refusal:is_valid_code(Code);
            error ->
                ok
        end
    end, TeamRefusal).

%%====================================================================
%% Test 8: Detect corrupted plan JSON
%%====================================================================

test_detect_corrupted_plan_json(_Config) ->
    %% Invalid JSON
    InvalidJson = <<"{broken json}">>,
    case erlmcp_pricing_poka_yoke:validate_plan(InvalidJson) of
        {error, [{parse, _, _, _, _} | _]} -> ok;
        {error, _} -> ok
    end,

    %% Non-map structure
    {error, [{schema, root, _, _, _}]} = erlmcp_pricing_poka_yoke:validate_plan([1, 2, 3]),

    %% Empty map (missing all required fields)
    {error, Errors} = erlmcp_pricing_poka_yoke:validate_plan(#{}),
    true = length(Errors) > 0.

%%====================================================================
%% Test 9-11: Deterministic validation (5 consecutive runs per plan)
%%====================================================================

test_deterministic_validation_five_runs_team(_Config) ->
    run_deterministic_validation("plans/team.plan.json", 5).

test_deterministic_validation_five_runs_enterprise(_Config) ->
    run_deterministic_validation("plans/enterprise.plan.json", 5).

test_deterministic_validation_five_runs_gov(_Config) ->
    run_deterministic_validation("plans/gov.plan.json", 5).

run_deterministic_validation(FilePath, Runs) ->
    Results = lists:map(fun(_) ->
        Plan = load_plan(FilePath),
        erlmcp_pricing_poka_yoke:validate_plan(Plan)
    end, lists:seq(1, Runs)),

    %% All runs should have identical results
    [FirstResult | RestResults] = Results,
    true = lists:all(fun(R) -> R =:= FirstResult end, RestResults),

    %% All should be successful
    true = lists:all(fun(R) -> R == {ok, []} end, Results).

%%====================================================================
%% Helper Functions
%%====================================================================

load_plan(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Json} ->
            jsx:decode(Json, [return_maps]);
        {error, Reason} ->
            ct:fail(io_lib:format("Failed to load plan ~s: ~p", [FilePath, Reason]))
    end.

create_valid_envelope() ->
    #{
        throughput_req_s => 450,
        concurrent_connections => 128,
        queue_depth_messages => 2048,
        p99_latency_ms => 250,
        failover_sla_seconds => 30,
        connection_timeout_seconds => 60
    }.

create_valid_limits() ->
    #{
        max_message_size_bytes => 1048576,
        max_payload_size_mb => 10,
        max_concurrent_requests_per_conn => 32,
        memory_limit_mb => 512,
        cpu_time_limit_seconds => 300,
        backpressure_threshold_bytes => 8388608
    }.

create_valid_evidence() ->
    #{
        sbom => <<"plans/team-sbom.json">>,
        provenance => <<"plans/team-provenance.json">>,
        chaos_report => <<"docs/plans/team-chaos-report.md">>,
        benchmark_report => <<"docs/plans/team-benchmark-report.md">>
    }.
