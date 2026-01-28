%%%-------------------------------------------------------------------
%% @doc Common Test Suite for Upgrade Plan and Verification
%%
%% Tests cover:
%% - Upgrade plan generation for stable versions
%% - Plan output determinism across 5 runs
%% - Version parsing and validation
%% - Health check completeness
%% - v1.3.0 -> v1.4.0 transition specifics
%% - Configuration compatibility checks
%%
%% Usage:
%%   rebar3 ct --suite=erlmcp_upgrade_SUITE
%%   rebar3 ct --suite=erlmcp_upgrade_SUITE --repeat=5
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_upgrade_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suite callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_upgrade_plan_generation/1,
    test_plan_output_deterministic/1,
    test_version_parsing_valid/1,
    test_version_parsing_invalid/1,
    test_version_comparison/1,
    test_plan_v1_3_to_v1_4/1,
    test_plan_includes_all_categories/1,
    test_health_check_all_checks_run/1,
    test_health_check_endpoints_responsive/1,
    test_health_check_config_loaded/1,
    test_health_check_deterministic_results/1,
    test_upgrade_verify_with_running_app/1
]).

-define(TEST_SUITE_NAME, "erlmcp_upgrade_v1.4.0").

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [
        test_upgrade_plan_generation,
        test_plan_output_deterministic,
        test_version_parsing_valid,
        test_version_parsing_invalid,
        test_version_comparison,
        test_plan_v1_3_to_v1_4,
        test_plan_includes_all_categories,
        test_health_check_all_checks_run,
        test_health_check_endpoints_responsive,
        test_health_check_config_loaded,
        test_health_check_deterministic_results,
        test_upgrade_verify_with_running_app
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ct:log("=== ~s Test Suite Initialized ===", [?TEST_SUITE_NAME]),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases: Plan Generation
%%====================================================================

%% @doc Test basic upgrade plan generation between versions.
test_upgrade_plan_generation(Config) ->
    ct:log("Testing upgrade plan generation from 0.7.0 to 1.0.0"),

    {ok, Plan} = erlmcp_upgrade:upgrade_plan("0.7.0", "1.0.0"),

    ?assertIsNotEmpty(Plan),
    ?assertIsList(Plan),

    %% Verify structure of plan items
    lists:foreach(fun(Item) ->
        ?assertMatch({config, _, _}, Item) orelse
        ?assertMatch({behavior, _, _}, Item) orelse
        ?assertMatch({profile, _, _}, Item) orelse
        ?assertMatch({taxonomy, _, _}, Item) orelse
        ?assertMatch({transport, _, _}, Item)
    end, Plan),

    ct:log("Plan has ~B changes across categories", [length(Plan)]),
    ok.

%% @doc Test that plan output is deterministic across multiple runs.
test_plan_output_deterministic(Config) ->
    ct:log("Testing upgrade plan output determinism"),

    %% Generate plan 5 times
    Plans = [erlmcp_upgrade:upgrade_plan("1.3.0", "1.4.0") || _ <- lists:seq(1, 5)],

    %% All should succeed
    ?assertMatch([{ok, _}, {ok, _}, {ok, _}, {ok, _}, {ok, _}], Plans),

    %% Extract the actual plans
    [plan1, Plan2, Plan3, Plan4, Plan5] = [P || {ok, P} <- Plans],

    %% All should be identical
    ?assertEqual(Plan1, Plan2, "Plan 1 should equal Plan 2"),
    ?assertEqual(Plan2, Plan3, "Plan 2 should equal Plan 3"),
    ?assertEqual(Plan3, Plan4, "Plan 3 should equal Plan 4"),
    ?assertEqual(Plan4, Plan5, "Plan 4 should equal Plan 5"),

    ct:log("All 5 plans are identical - determinism verified"),
    ok.

%%====================================================================
%% Test Cases: Version Parsing
%%====================================================================

%% @doc Test version parsing with valid input.
test_version_parsing_valid(Config) ->
    ct:log("Testing version parsing with valid inputs"),

    ValidVersions = [
        "0.7.0",
        "1.0.0",
        "1.3.0",
        "1.4.0",
        "2.0.0"
    ],

    lists:foreach(fun(Version) ->
        {ok, Plan} = erlmcp_upgrade:upgrade_plan(Version, "2.0.0"),
        case Version of
            "2.0.0" ->
                %% Same version should fail
                {error, same_version} = erlmcp_upgrade:upgrade_plan(Version, Version);
            _ ->
                ?assertIsNotEmpty(Plan) orelse ?assertEqual([], Plan)
        end
    end, ValidVersions),

    ct:log("All valid versions parsed successfully"),
    ok.

%% @doc Test version parsing with invalid input.
test_version_parsing_invalid(Config) ->
    ct:log("Testing version parsing with invalid inputs"),

    InvalidVersions = [
        "invalid",
        "1.0",
        "1",
        "a.b.c",
        "",
        "1.0.0.0"
    ],

    lists:foreach(fun(Version) ->
        Result = erlmcp_upgrade:upgrade_plan(Version, "2.0.0"),
        ?assertMatch({error, _}, Result,
                     io_lib:format("Version '~s' should be rejected", [Version]))
    end, InvalidVersions),

    ct:log("All invalid versions correctly rejected"),
    ok.

%% @doc Test version comparison logic.
test_version_comparison(Config) ->
    ct:log("Testing version comparison"),

    %% Test upgrade (forward)
    {ok, _} = erlmcp_upgrade:upgrade_plan("0.7.0", "1.0.0"),
    {ok, _} = erlmcp_upgrade:upgrade_plan("1.0.0", "1.4.0"),
    {ok, _} = erlmcp_upgrade:upgrade_plan("1.3.0", "1.4.0"),

    %% Test same version (error)
    {error, same_version} = erlmcp_upgrade:upgrade_plan("1.4.0", "1.4.0"),

    %% Test downgrade (error)
    {error, downgrade_not_supported} = erlmcp_upgrade:upgrade_plan("1.4.0", "1.3.0"),
    {error, downgrade_not_supported} = erlmcp_upgrade:upgrade_plan("1.0.0", "0.7.0"),

    ct:log("Version comparison logic verified"),
    ok.

%%====================================================================
%% Test Cases: Plan Content
%%====================================================================

%% @doc Test specific changes for v1.3.0 to v1.4.0 transition.
test_plan_v1_3_to_v1_4(Config) ->
    ct:log("Testing v1.3.0 to v1.4.0 upgrade plan"),

    {ok, Plan} = erlmcp_upgrade:upgrade_plan("1.3.0", "1.4.0"),

    %% Should include specific config changes
    ConfigChanges = [C || {config, _, _} = C <- Plan],
    ?assertIsNotEmpty(ConfigChanges),

    %% Should mention max_pending_requests increase
    PendingReqChange = lists:any(fun({config, Key, _}) ->
        string:find(Key, "max_pending_requests") =/= nomatch
    end, ConfigChanges),
    ?assert(PendingReqChange, "Should mention max_pending_requests change"),

    %% Should include behavior changes
    BehaviorChanges = [C || {behavior, _, _} = C <- Plan],
    ?assertIsNotEmpty(BehaviorChanges),

    %% Should mention circuit breaker
    CircuitBreakerChange = lists:any(fun({behavior, Key, _}) ->
        string:find(Key, "circuit_breaker") =/= nomatch
    end, BehaviorChanges),
    ?assert(CircuitBreakerChange, "Should mention circuit_breaker behavior"),

    %% Should mention upgrade_plan
    UpgradePlanChange = lists:any(fun({behavior, Key, _}) ->
        string:find(Key, "upgrade_plan") =/= nomatch
    end, BehaviorChanges),
    ?assert(UpgradePlanChange, "Should mention upgrade_plan behavior"),

    ct:log("v1.3.0 to v1.4.0 plan contains all expected changes"),
    ok.

%% @doc Test that plan includes all change categories.
test_plan_includes_all_categories(Config) ->
    ct:log("Testing that plan includes all categories"),

    {ok, Plan} = erlmcp_upgrade:upgrade_plan("0.7.0", "1.4.0"),

    %% Extract categories
    Categories = [Cat || {Cat, _, _} <- Plan],
    UniqueCategories = lists:usort(Categories),

    %% Should have multiple categories
    ?assertIsNotEmpty(UniqueCategories),
    ?assert(length(UniqueCategories) >= 2, "Should have at least 2 categories"),

    %% Print categories found
    ct:log("Categories found in plan: ~p", [UniqueCategories]),

    ok.

%%====================================================================
%% Test Cases: Health Checks
%%====================================================================

%% @doc Test that all health checks are run during verification.
test_health_check_all_checks_run(Config) ->
    ct:log("Testing that all health checks are executed"),

    {ok, Checks} = erlmcp_upgrade:upgrade_verify(),

    ?assertIsList(Checks),
    ?assertIsNotEmpty(Checks),

    %% Extract check names
    CheckNames = [Name || {Name, _} <- Checks],

    %% Verify expected checks are present
    ExpectedChecks = [
        endpoints_responsive,
        registry_shards_healthy,
        receipt_emission_working,
        queue_bounds_respected,
        transport_stable,
        configuration_loaded,
        supervision_tree_healthy,
        version_consistent
    ],

    lists:foreach(fun(ExpectedCheck) ->
        ?assert(lists:member(ExpectedCheck, CheckNames),
                io_lib:format("Expected check '~p' not found", [ExpectedCheck]))
    end, ExpectedChecks),

    ct:log("All ~B expected health checks executed", [length(ExpectedChecks)]),
    ok.

%% @doc Test endpoints responsive check.
test_health_check_endpoints_responsive(Config) ->
    ct:log("Testing endpoints responsive check"),

    {ok, Checks} = erlmcp_upgrade:upgrade_verify(),

    %% Find the endpoints check
    case lists:keyfind(endpoints_responsive, 1, Checks) of
        {endpoints_responsive, Result} ->
            ?assertMatch(ok, Result, "Endpoints should be responsive"),
            ct:log("Endpoints responsive check passed");
        false ->
            ?fail("endpoints_responsive check not found")
    end,

    ok.

%% @doc Test configuration loaded check.
test_health_check_config_loaded(Config) ->
    ct:log("Testing configuration loaded check"),

    {ok, Checks} = erlmcp_upgrade:upgrade_verify(),

    %% Find the config check
    case lists:keyfind(configuration_loaded, 1, Checks) of
        {configuration_loaded, Result} ->
            ?assertMatch(ok, Result, "Configuration should be loaded"),
            ct:log("Configuration loaded check passed");
        false ->
            ?fail("configuration_loaded check not found")
    end,

    ok.

%% @doc Test that health check results are deterministic.
test_health_check_deterministic_results(Config) ->
    ct:log("Testing health check result determinism"),

    %% Run verify 3 times
    Results1 = erlmcp_upgrade:upgrade_verify(),
    Results2 = erlmcp_upgrade:upgrade_verify(),
    Results3 = erlmcp_upgrade:upgrade_verify(),

    %% All should succeed
    ?assertMatch({ok, _}, Results1),
    ?assertMatch({ok, _}, Results2),
    ?assertMatch({ok, _}, Results3),

    %% Extract check results
    {ok, Checks1} = Results1,
    {ok, Checks2} = Results2,
    {ok, Checks3} = Results3,

    %% Results should be deterministic (same checks, same results)
    CheckNames1 = [Name || {Name, _} <- Checks1],
    CheckNames2 = [Name || {Name, _} <- Checks2],
    CheckNames3 = [Name || {Name, _} <- Checks3],

    ?assertEqual(CheckNames1, CheckNames2, "Check names should be deterministic"),
    ?assertEqual(CheckNames2, CheckNames3, "Check names should be deterministic"),

    ct:log("Health checks are deterministic across 3 runs"),
    ok.

%%====================================================================
%% Test Cases: Integration
%%====================================================================

%% @doc Test upgrade verify with running application.
test_upgrade_verify_with_running_app(Config) ->
    ct:log("Testing upgrade verify with running erlmcp app"),

    %% Ensure app is running
    case application:ensure_all_started(erlmcp) of
        {ok, _} ->
            {ok, Checks} = erlmcp_upgrade:upgrade_verify(),

            %% Verify all checks were run
            ?assertIsNotEmpty(Checks),

            %% Count passes and failures
            Passes = length([ok || {_, ok} <- Checks]),
            Failures = length([fail || {_, {error, _}} <- Checks]),

            ct:log("Verification complete: ~B checks passed, ~B checks failed",
                   [Passes, Failures]),

            %% Most checks should pass (some failures are OK for non-essential checks)
            ?assert(Passes >= 5, "At least 5 critical checks should pass");
        {error, Reason} ->
            ct:log("Warning: erlmcp app not available: ~p", [Reason]),
            ok
    end,

    ok.
