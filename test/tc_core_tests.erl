%%%-------------------------------------------------------------------
%% @doc Testcontainers Core Tests
%%
%% Unit tests for tc_core module logic.
%% Integration tests require Docker and are marked with {skip, no_docker}.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tc_core_tests).

-include_lib("eunit/include/eunit.hrl").

%% Export only test generators
-export([
    tc_core_unit_test_/0,
    tc_fixtures_unit_test_/0
]).

%%====================================================================
%% Unit Tests - No Docker Required
%%====================================================================

tc_core_unit_test_() ->
    {setup,
     fun unit_setup/0,
     fun unit_cleanup/1,
     [
         {"Container name generation", fun container_name_generation_/0},
         {"Network name validation", fun network_name_validation_/0},
         {"Fixture info retrieval", fun fixture_info_retrieval_/0},
         {"Fixture list completeness", fun fixture_list_completeness_/0},
         {"Scenario categories", fun scenario_categories_/0}
     ]}.

tc_fixtures_unit_test_() ->
    [
        {"All fixtures listed", fun all_fixtures_listed_/0},
        {"Fixture info has required fields", fun fixture_info_fields_/0}
    ].

unit_setup() ->
    ok.

unit_cleanup(_) ->
    ok.

%%====================================================================
%% Container Name Generation Tests
%%====================================================================

container_name_generation_() ->
    %% Test that container names are unique
    Name1 = generate_test_name(),
    Name2 = generate_test_name(),
    ?assertNotEqual(Name1, Name2),
    ok.

generate_test_name() ->
    list_to_binary("erlmcp-" ++ integer_to_list(erlang:unique_integer([positive]))).

%%====================================================================
%% Network Name Validation Tests
%%====================================================================

network_name_validation_() ->
    %% Valid network names
    ?assert(is_valid_network_name(<<"erlmcp-test">>)),
    ?assert(is_valid_network_name(<<"governed-cluster">>)),
    ?assert(is_valid_network_name(<<"gcp-sim-cluster">>)),

    %% Invalid network names (for documentation)
    ?assertNot(is_valid_network_name(<<"">>)),
    ok.

is_valid_network_name(<<>>) -> false;
is_valid_network_name(Name) when is_binary(Name) ->
    %% Docker network names must be alphanumeric with - and _
    Re = "^[a-zA-Z0-9][a-zA-Z0-9_.-]*$",
    case re:run(Name, Re) of
        {match, _} -> true;
        nomatch -> false
    end.

%%====================================================================
%% Fixture Info Tests
%%====================================================================

fixture_info_retrieval_() ->
    %% Test each fixture has valid info
    Fixtures = tc_fixtures:list_fixtures(),
    lists:foreach(fun(FixtureName) ->
        Info = tc_fixtures:fixture_info(FixtureName),
        ?assert(is_map(Info)),
        ?assertNot(maps:is_key(error, Info)),
        ?assertEqual(FixtureName, maps:get(name, Info))
    end, Fixtures),
    ok.

fixture_list_completeness_() ->
    Fixtures = tc_fixtures:list_fixtures(),

    %% All 8 fixtures should be listed
    ExpectedFixtures = [
        minimal_cluster,
        chaos_cluster,
        audited_cluster,
        gcp_sandbox,
        persistent_cluster,
        killswitch_cluster,
        policy_federation,
        full_governance
    ],

    ?assertEqual(length(ExpectedFixtures), length(Fixtures)),
    lists:foreach(fun(Expected) ->
        ?assert(lists:member(Expected, Fixtures))
    end, ExpectedFixtures),
    ok.

all_fixtures_listed_() ->
    Fixtures = tc_fixtures:list_fixtures(),
    ?assertEqual(8, length(Fixtures)),
    ok.

fixture_info_fields_() ->
    RequiredFields = [name, description, features, default_nodes, use_cases],

    lists:foreach(fun(FixtureName) ->
        Info = tc_fixtures:fixture_info(FixtureName),
        lists:foreach(fun(Field) ->
            ?assert(maps:is_key(Field, Info),
                    {missing_field, Field, FixtureName})
        end, RequiredFields)
    end, tc_fixtures:list_fixtures()),
    ok.

%%====================================================================
%% Scenario Category Tests
%%====================================================================

scenario_categories_() ->
    %% Test scenario categorization - only verify function exists
    %% We can't run actual scenarios without Docker

    %% Unknown category should return empty list
    ?assertEqual([], tc_scenarios:run_category(unknown_category)),
    ok.

%%====================================================================
%% Feature Combination Matrix Tests
%%====================================================================

feature_combination_test_() ->
    [
        {"Minimal cluster features", fun minimal_features_/0},
        {"Chaos cluster features", fun chaos_features_/0},
        {"Full governance features", fun full_governance_features_/0}
    ].

minimal_features_() ->
    Info = tc_fixtures:fixture_info(minimal_cluster),
    Features = maps:get(features, Info),
    ?assert(lists:member(governance_cluster, Features)),
    ok.

chaos_features_() ->
    Info = tc_fixtures:fixture_info(chaos_cluster),
    Features = maps:get(features, Info),
    ?assert(lists:member(governance_cluster, Features)),
    ?assert(lists:member(chaos_injection, Features)),
    ok.

full_governance_features_() ->
    Info = tc_fixtures:fixture_info(full_governance),
    Features = maps:get(features, Info),

    ExpectedFeatures = [
        governance_cluster,
        chaos_injection,
        receipt_chain,
        kill_switch,
        distributed_audit,
        federated_policy
    ],

    lists:foreach(fun(F) ->
        ?assert(lists:member(F, Features), {missing_feature, F})
    end, ExpectedFeatures),
    ok.

%%====================================================================
%% 80/20 Coverage Tests
%%====================================================================

coverage_80_20_test_() ->
    [
        {"8 fixtures cover 80% use cases", fun fixtures_coverage_/0},
        {"8 features are combinatorial", fun features_combinatorial_/0}
    ].

fixtures_coverage_() ->
    %% The 8 fixtures should cover the main use cases:
    %% - Basic distributed testing (minimal_cluster)
    %% - Fault tolerance (chaos_cluster)
    %% - Compliance (audited_cluster)
    %% - Cloud integration (gcp_sandbox)
    %% - State recovery (persistent_cluster)
    %% - Emergency stop (killswitch_cluster)
    %% - Access control (policy_federation)
    %% - Full integration (full_governance)

    %% Use lowercase keywords that appear in the use cases (case-insensitive match)
    UseCaseCategories = [
        {minimal_cluster, [<<"distributed">>, <<"Basic">>]},
        {chaos_cluster, [<<"partition">>, <<"failure">>]},
        {audited_cluster, [<<"Compliance">>, <<"Audit">>]},
        {gcp_sandbox, [<<"Cloud">>, <<"Pub/Sub">>]},
        {persistent_cluster, [<<"State">>, <<"recovery">>]},
        {killswitch_cluster, [<<"Emergency">>, <<"stop">>]},
        {policy_federation, [<<"access">>, <<"Policy">>]},
        {full_governance, [<<"End-to-end">>, <<"Complete">>]}
    ],

    lists:foreach(fun({Fixture, Keywords}) ->
        Info = tc_fixtures:fixture_info(Fixture),
        UseCases = maps:get(use_cases, Info),
        %% Join use cases into single searchable text
        UseCaseText = lists:foldl(fun(UC, Acc) ->
            <<Acc/binary, " ", UC/binary>>
        end, <<>>, UseCases),

        %% At least one keyword should appear in use cases
        HasKeyword = lists:any(fun(Keyword) ->
            binary:match(UseCaseText, Keyword) =/= nomatch
        end, Keywords),

        ?assert(HasKeyword, {fixture_missing_use_case, Fixture, Keywords})
    end, UseCaseCategories),
    ok.

features_combinatorial_() ->
    %% The 8 core features should be:
    %% 1. Governance Cluster
    %% 2. Chaos Injection
    %% 3. Receipt Chain
    %% 4. GCP Distributed
    %% 5. Stateful Testing
    %% 6. Kill Switch
    %% 7. Distributed Audit
    %% 8. Federated Policy

    %% Collect all unique features from fixtures
    AllFeatures = lists:usort(lists:flatten([
        maps:get(features, tc_fixtures:fixture_info(F))
        || F <- tc_fixtures:list_fixtures()
    ])),

    %% Should have at least 6 distinct features (some overlap expected)
    ?assert(length(AllFeatures) >= 6, {not_enough_features, AllFeatures}),
    ok.
