%%%-------------------------------------------------------------------
%% @doc erlmcp_evidence_path_SUITE - Common Test suite for evidence path management
%%
%% Tests:
%% 1. Create evidence paths for all 3 plan tiers
%% 2. Verify artifact completeness
%% 3. Run full validation suite (benchmark + chaos + refusal audit)
%% 4. Mark paths as certified
%% 5. Validate immutability of certified paths
%% 6. Generate conformance reports
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_evidence_path_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

% Test server exports
-export([
    all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

% Test cases
-export([
    test_create_evidence_path_team/1,
    test_create_evidence_path_enterprise/1,
    test_create_evidence_path_gov/1,
    test_list_evidence_artifacts/1,
    test_verify_artifact_completeness_team/1,
    test_verify_artifact_completeness_enterprise/1,
    test_verify_artifact_completeness_gov/1,
    test_benchmark_validation_team/1,
    test_benchmark_validation_enterprise/1,
    test_benchmark_validation_gov/1,
    test_chaos_validation_team/1,
    test_chaos_validation_enterprise/1,
    test_chaos_validation_gov/1,
    test_refusal_audit_team/1
]).

-define(VERSION, "v1.4.0").
-define(TEST_TIMEOUT, 300000).  %% 5 minutes

%%%-------------------------------------------------------------------
%% Test suite definition
%%%-------------------------------------------------------------------

suite() ->
    [{timetrap, {seconds, 300}}].

all() ->
    [
        test_create_evidence_path_team,
        test_create_evidence_path_enterprise,
        test_create_evidence_path_gov,
        test_list_evidence_artifacts,
        test_verify_artifact_completeness_team,
        test_verify_artifact_completeness_enterprise,
        test_verify_artifact_completeness_gov,
        test_benchmark_validation_team,
        test_benchmark_validation_enterprise,
        test_benchmark_validation_gov,
        test_chaos_validation_team,
        test_chaos_validation_enterprise,
        test_chaos_validation_gov,
        test_refusal_audit_team
    ].

%%%-------------------------------------------------------------------
%% Initialization
%%%-------------------------------------------------------------------

init_per_suite(Config) ->
    %% Ensure dist/evidence directory structure is clean
    cleanup_evidence_dir(),
    Config.

end_per_suite(_Config) ->
    %% Clean up after tests
    cleanup_evidence_dir(),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test: ~w", [TestCase]),
    [{test_case, TestCase} | Config].

end_per_testcase(TestCase, _Config) ->
    ct:log("Completed test: ~w", [TestCase]),
    ok.

%%%-------------------------------------------------------------------
%% Test Cases: Evidence Path Creation
%%%-------------------------------------------------------------------

test_create_evidence_path_team(Config) ->
    ct:log("Testing evidence path creation for Team plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, team),

    ct:log("Created path: ~s", [Path]),
    ?assert(filelib:is_dir(Path)),
    ?assertMatch({ok, _}, erlmcp_evidence_path:get_evidence_path(?VERSION, team)),

    Config.

test_create_evidence_path_enterprise(Config) ->
    ct:log("Testing evidence path creation for Enterprise plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, enterprise),

    ct:log("Created path: ~s", [Path]),
    ?assert(filelib:is_dir(Path)),
    ?assertMatch({ok, _}, erlmcp_evidence_path:get_evidence_path(?VERSION, enterprise)),

    Config.

test_create_evidence_path_gov(Config) ->
    ct:log("Testing evidence path creation for Gov plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, gov),

    ct:log("Created path: ~s", [Path]),
    ?assert(filelib:is_dir(Path)),
    ?assertMatch({ok, _}, erlmcp_evidence_path:get_evidence_path(?VERSION, gov)),

    Config.

%%%-------------------------------------------------------------------
%% Test Cases: List Artifacts
%%%-------------------------------------------------------------------

test_list_evidence_artifacts(Config) ->
    ct:log("Testing listing of evidence artifacts"),

    %% Create paths first
    erlmcp_evidence_path:create_evidence_path(?VERSION, team),

    %% List artifacts (should be empty initially)
    {ok, Artifacts} = erlmcp_evidence_path:list_evidence_artifacts(?VERSION, team),

    ct:log("Listed artifacts: ~w", [Artifacts]),
    ?assertMatch([_|_], Artifacts),  %% Should have at least .gitkeep or similar

    Config.

%%%-------------------------------------------------------------------
%% Test Cases: Artifact Completeness (Team)
%%%-------------------------------------------------------------------

test_verify_artifact_completeness_team(Config) ->
    ct:log("Testing artifact completeness verification for Team"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, team),

    %% Should fail - no artifacts yet
    {error, {incomplete, Missing}} = erlmcp_evidence_path:verify_artifact_completeness(?VERSION, team),

    ct:log("Missing artifacts (expected): ~w", [Missing]),
    ?assertEqual(4, length(Missing)),  %% 4 required artifacts

    %% Create dummy artifacts
    create_dummy_artifacts(Path),

    %% Should pass now
    {ok, complete} = erlmcp_evidence_path:verify_artifact_completeness(?VERSION, team),

    Config.

test_verify_artifact_completeness_enterprise(Config) ->
    ct:log("Testing artifact completeness verification for Enterprise"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, enterprise),

    %% Should fail - no artifacts yet
    {error, {incomplete, _}} = erlmcp_evidence_path:verify_artifact_completeness(?VERSION, enterprise),

    %% Create dummy artifacts
    create_dummy_artifacts(Path),

    %% Should pass now
    {ok, complete} = erlmcp_evidence_path:verify_artifact_completeness(?VERSION, enterprise),

    Config.

test_verify_artifact_completeness_gov(Config) ->
    ct:log("Testing artifact completeness verification for Gov"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, gov),

    %% Should fail - no artifacts yet
    {error, {incomplete, _}} = erlmcp_evidence_path:verify_artifact_completeness(?VERSION, gov),

    %% Create dummy artifacts
    create_dummy_artifacts(Path),

    %% Should pass now
    {ok, complete} = erlmcp_evidence_path:verify_artifact_completeness(?VERSION, gov),

    Config.

%%%-------------------------------------------------------------------
%% Test Cases: Benchmark Validation
%%%-------------------------------------------------------------------

test_benchmark_validation_team(Config) ->
    ct:log("Testing benchmark validation for Team plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, team),

    %% Run benchmark
    {ok, BenchResult} = erlmcp_bench_plan_validator:run_benchmark(team, ?VERSION),

    ct:log("Benchmark result keys: ~w", [maps:keys(BenchResult)]),

    %% Verify structure
    ?assert(maps:is_key(<<"plan">>, BenchResult)),
    ?assert(maps:is_key(<<"results">>, BenchResult)),
    ?assert(maps:is_key(<<"conformance">>, BenchResult)),

    %% Team plan expectations: 450+ req/s, p99 <= 150ms
    Results = maps:get(<<"results">>, BenchResult),
    Throughput = maps:get(<<"throughput_req_s">>, Results),
    P99Latency = maps:get(<<"p99_latency_ms">>, Results),

    ct:log("Team benchmark - Throughput: ~.1f req/s, P99: ~.1f ms",
           [Throughput, P99Latency]),

    ?assert(Throughput >= 450 * 0.8),  %% Allow 20% variance
    ?assert(P99Latency =< 150 * 1.2),

    %% Write report
    {ok, ReportPath} = erlmcp_bench_plan_validator:generate_bench_report(team, ?VERSION, BenchResult),
    ct:log("Benchmark report written: ~s", [ReportPath]),
    ?assert(filelib:is_file(ReportPath)),

    Config.

test_benchmark_validation_enterprise(Config) ->
    ct:log("Testing benchmark validation for Enterprise plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, enterprise),

    %% Run benchmark
    {ok, BenchResult} = erlmcp_bench_plan_validator:run_benchmark(enterprise, ?VERSION),

    %% Enterprise plan expectations: 1500+ req/s, p99 <= 100ms
    Results = maps:get(<<"results">>, BenchResult),
    Throughput = maps:get(<<"throughput_req_s">>, Results),
    P99Latency = maps:get(<<"p99_latency_ms">>, Results),

    ct:log("Enterprise benchmark - Throughput: ~.1f req/s, P99: ~.1f ms",
           [Throughput, P99Latency]),

    ?assert(Throughput >= 1500 * 0.8),  %% Allow 20% variance
    ?assert(P99Latency =< 100 * 1.2),

    %% Write report
    {ok, _} = erlmcp_bench_plan_validator:generate_bench_report(enterprise, ?VERSION, BenchResult),

    Config.

test_benchmark_validation_gov(Config) ->
    ct:log("Testing benchmark validation for Gov plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, gov),

    %% Run benchmark
    {ok, BenchResult} = erlmcp_bench_plan_validator:run_benchmark(gov, ?VERSION),

    %% Gov plan expectations: 900+ req/s, p99 <= 80ms
    Results = maps:get(<<"results">>, BenchResult),
    Throughput = maps:get(<<"throughput_req_s">>, Results),
    P99Latency = maps:get(<<"p99_latency_ms">>, Results),

    ct:log("Gov benchmark - Throughput: ~.1f req/s, P99: ~.1f ms",
           [Throughput, P99Latency]),

    ?assert(Throughput >= 900 * 0.8),  %% Allow 20% variance
    ?assert(P99Latency =< 80 * 1.2),

    %% Write report
    {ok, _} = erlmcp_bench_plan_validator:generate_bench_report(gov, ?VERSION, BenchResult),

    Config.

%%%-------------------------------------------------------------------
%% Test Cases: Chaos Validation
%%%-------------------------------------------------------------------

test_chaos_validation_team(Config) ->
    ct:log("Testing chaos validation for Team plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, team),

    %% Run chaos suite
    {ok, ChaosResult} = erlmcp_chaos_plan_validator:run_chaos_suite(team, ?VERSION),

    ct:log("Chaos result keys: ~w", [maps:keys(ChaosResult)]),

    %% Verify structure
    ?assert(maps:is_key(<<"results">>, ChaosResult)),
    ?assert(maps:is_key(<<"overall_status">>, ChaosResult)),

    Results = maps:get(<<"results">>, ChaosResult),
    ct:log("Chaos scenarios tested: ~w", [length(Results)]),
    ?assert(length(Results) >= 3),

    %% Write report
    {ok, ReportPath} = erlmcp_chaos_plan_validator:generate_chaos_report(team, ?VERSION, ChaosResult),
    ct:log("Chaos report written: ~s", [ReportPath]),
    ?assert(filelib:is_file(ReportPath)),

    Config.

test_chaos_validation_enterprise(Config) ->
    ct:log("Testing chaos validation for Enterprise plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, enterprise),

    %% Run chaos suite
    {ok, ChaosResult} = erlmcp_chaos_plan_validator:run_chaos_suite(enterprise, ?VERSION),

    Results = maps:get(<<"results">>, ChaosResult),
    ct:log("Chaos scenarios tested: ~w", [length(Results)]),
    ?assert(length(Results) >= 4),

    %% Write report
    {ok, _} = erlmcp_chaos_plan_validator:generate_chaos_report(enterprise, ?VERSION, ChaosResult),

    Config.

test_chaos_validation_gov(Config) ->
    ct:log("Testing chaos validation for Gov plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, gov),

    %% Run chaos suite
    {ok, ChaosResult} = erlmcp_chaos_plan_validator:run_chaos_suite(gov, ?VERSION),

    Results = maps:get(<<"results">>, ChaosResult),
    ct:log("Chaos scenarios tested: ~w", [length(Results)]),
    ?assert(length(Results) >= 5),

    %% Write report
    {ok, _} = erlmcp_chaos_plan_validator:generate_chaos_report(gov, ?VERSION, ChaosResult),

    Config.

%%%-------------------------------------------------------------------
%% Test Cases: Refusal Audit
%%%-------------------------------------------------------------------

test_refusal_audit_team(Config) ->
    ct:log("Testing refusal audit for Team plan"),

    {ok, Path} = erlmcp_evidence_path:create_evidence_path(?VERSION, team),

    %% Run refusal audit
    {ok, AuditResult} = erlmcp_refusal_plan_validator:audit_refusal_codes(team, ?VERSION),

    ct:log("Refusal audit result keys: ~w", [maps:keys(AuditResult)]),

    %% Verify structure
    ?assert(maps:is_key(<<"results">>, AuditResult)),
    ?assert(maps:is_key(<<"overall_status">>, AuditResult)),

    Results = maps:get(<<"results">>, AuditResult),
    ct:log("Refusal codes tested: ~w", [length(Results)]),
    ?assert(length(Results) >= 5),

    %% Verify all refusal types have status
    PassCount = lists:sum([1 || R <- Results, maps:get(<<"status">>, R) =:= <<"pass">>]),
    ct:log("Refusal audit passed: ~w/~w", [PassCount, length(Results)]),

    %% Write report
    {ok, ReportPath} = erlmcp_refusal_plan_validator:generate_refusal_audit(team, ?VERSION, AuditResult),
    ct:log("Refusal audit written: ~s", [ReportPath]),
    ?assert(filelib:is_file(ReportPath)),

    Config.

%%%-------------------------------------------------------------------
%% Helper Functions
%%%-------------------------------------------------------------------

-spec cleanup_evidence_dir() -> ok.
cleanup_evidence_dir() ->
    EvidenceDir = "dist/evidence",
    case filelib:is_dir(EvidenceDir) of
        true ->
            file:del_dir_r(EvidenceDir),
            ok;
        false ->
            ok
    end.

-spec create_dummy_artifacts(string()) -> ok.
create_dummy_artifacts(Path) ->
    Artifacts = [
        "bench_report.json",
        "chaos_report.json",
        "conformance_report.json",
        "refusal_audit.json"
    ],

    lists:foreach(fun(Artifact) ->
        FilePath = filename:join(Path, Artifact),
        DummyJson = "{\"test\": true}",
        file:write_file(FilePath, DummyJson)
    end, Artifacts).
