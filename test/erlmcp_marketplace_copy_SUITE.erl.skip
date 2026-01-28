%%%-------------------------------------------------------------------
%% @doc
%% Common Test Suite for erlmcp_marketplace_copy
%%
%% Tests marketplace copy generation from plan specifications.
%%
%% Test categories:
%% - Load and generate: Load plans, generate listings, verify all fields present
%% - Markdown validity: No unclosed blocks, valid syntax, proper formatting
%% - Determinism: Generate same listing 5x, byte-identical output
%% - Cross-references: Refusal codes, SLA values, compliance fields verified
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_marketplace_copy_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1
]).

%% Test cases
-export([
    test_load_team_plan_and_generate_listing/1,
    test_load_enterprise_plan_and_generate_listing/1,
    test_load_gov_plan_and_generate_listing/1,
    test_verify_team_markdown_valid/1,
    test_verify_enterprise_markdown_valid/1,
    test_verify_gov_markdown_valid/1,
    test_verify_team_deterministic_generation/1,
    test_verify_enterprise_deterministic_generation/1,
    test_verify_gov_deterministic_generation/1,
    test_verify_team_refusal_codes_exist/1,
    test_verify_enterprise_sla_values_match/1,
    test_verify_gov_compliance_fields_present/1
]).

%% ===================================================================
%% Test Configuration
%% ===================================================================

suite() ->
    [
        {timetrap, {seconds, 10}},
        {require, [config, plan_specs]}
    ].

all() ->
    [
        test_load_team_plan_and_generate_listing,
        test_load_enterprise_plan_and_generate_listing,
        test_load_gov_plan_and_generate_listing,
        test_verify_team_markdown_valid,
        test_verify_enterprise_markdown_valid,
        test_verify_gov_markdown_valid,
        test_verify_team_deterministic_generation,
        test_verify_enterprise_deterministic_generation,
        test_verify_gov_deterministic_generation,
        test_verify_team_refusal_codes_exist,
        test_verify_enterprise_sla_values_match,
        test_verify_gov_compliance_fields_present
    ].

init_per_suite(Config) ->
    ct:log("Starting erlmcp_marketplace_copy test suite", []),
    %% Ensure templates directory exists
    case file:list_dir("templates") of
        {ok, _} -> ok;
        _ -> ct:fail("templates directory not found")
    end,
    Config.

end_per_suite(Config) ->
    ct:log("Completed erlmcp_marketplace_copy test suite", []),
    Config.

%% ===================================================================
%% Test Cases - Load and Generate
%% ===================================================================

test_load_team_plan_and_generate_listing(Config) ->
    case erlmcp_plan_loader:load_plan(team) of
        {ok, PlanSpec} ->
            ct:log("Team plan loaded successfully", []),
            case erlmcp_marketplace_copy:generate_team_listing(PlanSpec) of
                {ok, Markdown} ->
                    ct:log("Generated team listing: ~p bytes", [byte_size(Markdown)]),
                    %% Verify essential fields present
                    ?assertMatch(true, binary:match(Markdown, <<"450">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Markdown, <<"Team">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Markdown, <<"req/s">>) =/= nomatch),
                    ct:log("Team listing contains all expected fields", []);
                {error, Reason} ->
                    ct:fail({generation_failed, Reason})
            end;
        {error, Reason} ->
            ct:fail({plan_load_failed, Reason})
    end.

test_load_enterprise_plan_and_generate_listing(Config) ->
    case erlmcp_plan_loader:load_plan(enterprise) of
        {ok, PlanSpec} ->
            ct:log("Enterprise plan loaded successfully", []),
            case erlmcp_marketplace_copy:generate_enterprise_listing(PlanSpec) of
                {ok, Markdown} ->
                    ct:log("Generated enterprise listing: ~p bytes", [byte_size(Markdown)]),
                    %% Verify essential fields present
                    ?assertMatch(true, binary:match(Markdown, <<"1500">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Markdown, <<"Enterprise">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Markdown, <<"99.95">>) =/= nomatch),
                    ct:log("Enterprise listing contains all expected fields", []);
                {error, Reason} ->
                    ct:fail({generation_failed, Reason})
            end;
        {error, Reason} ->
            ct:fail({plan_load_failed, Reason})
    end.

test_load_gov_plan_and_generate_listing(Config) ->
    case erlmcp_plan_loader:load_plan(gov) of
        {ok, PlanSpec} ->
            ct:log("Government plan loaded successfully", []),
            case erlmcp_marketplace_copy:generate_gov_listing(PlanSpec) of
                {ok, Markdown} ->
                    ct:log("Generated government listing: ~p bytes", [byte_size(Markdown)]),
                    %% Verify essential fields present
                    ?assertMatch(true, binary:match(Markdown, <<"900">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Markdown, <<"Government">>) =/= nomatch),
                    ?assertMatch(true, binary:match(Markdown, <<"FIPS">>) =/= nomatch),
                    ct:log("Government listing contains all expected fields", []);
                {error, Reason} ->
                    ct:fail({generation_failed, Reason})
            end;
        {error, Reason} ->
            ct:fail({plan_load_failed, Reason})
    end.

%% ===================================================================
%% Test Cases - Markdown Validity
%% ===================================================================

test_verify_team_markdown_valid(Config) ->
    {ok, PlanSpec} = erlmcp_plan_loader:load_plan(team),
    {ok, Markdown} = erlmcp_marketplace_copy:generate_team_listing(PlanSpec),
    case erlmcp_marketplace_copy:validate_listing_markdown(Markdown) of
        {ok, validated} ->
            ct:log("Team markdown is valid", []);
        {error, Reason} ->
            ct:fail({markdown_validation_failed, Reason})
    end.

test_verify_enterprise_markdown_valid(Config) ->
    {ok, PlanSpec} = erlmcp_plan_loader:load_plan(enterprise),
    {ok, Markdown} = erlmcp_marketplace_copy:generate_enterprise_listing(PlanSpec),
    case erlmcp_marketplace_copy:validate_listing_markdown(Markdown) of
        {ok, validated} ->
            ct:log("Enterprise markdown is valid", []);
        {error, Reason} ->
            ct:fail({markdown_validation_failed, Reason})
    end.

test_verify_gov_markdown_valid(Config) ->
    {ok, PlanSpec} = erlmcp_plan_loader:load_plan(gov),
    {ok, Markdown} = erlmcp_marketplace_copy:generate_gov_listing(PlanSpec),
    case erlmcp_marketplace_copy:validate_listing_markdown(Markdown) of
        {ok, validated} ->
            ct:log("Government markdown is valid", []);
        {error, Reason} ->
            ct:fail({markdown_validation_failed, Reason})
    end.

%% ===================================================================
%% Test Cases - Determinism (5 generations, byte-identical)
%% ===================================================================

test_verify_team_deterministic_generation(Config) ->
    {ok, PlanSpec} = erlmcp_plan_loader:load_plan(team),
    %% Generate 5 times
    Results = [erlmcp_marketplace_copy:generate_team_listing(PlanSpec) || _ <- lists:seq(1, 5)],
    %% Extract markdown from all results
    Markdowns = [M || {ok, M} <- Results],
    %% Verify all 5 succeeded
    case length(Markdowns) =:= 5 of
        true ->
            %% Verify all byte-identical
            [First | Rest] = Markdowns,
            AllIdentical = lists:all(fun(M) -> M =:= First end, Rest),
            case AllIdentical of
                true ->
                    ct:log("Team listing generation is deterministic (5 generations byte-identical)", []);
                false ->
                    ct:fail(determinism_violation)
            end;
        false ->
            ct:fail(generation_failed)
    end.

test_verify_enterprise_deterministic_generation(Config) ->
    {ok, PlanSpec} = erlmcp_plan_loader:load_plan(enterprise),
    %% Generate 5 times
    Results = [erlmcp_marketplace_copy:generate_enterprise_listing(PlanSpec) || _ <- lists:seq(1, 5)],
    %% Extract markdown from all results
    Markdowns = [M || {ok, M} <- Results],
    %% Verify all 5 succeeded
    case length(Markdowns) =:= 5 of
        true ->
            %% Verify all byte-identical
            [First | Rest] = Markdowns,
            AllIdentical = lists:all(fun(M) -> M =:= First end, Rest),
            case AllIdentical of
                true ->
                    ct:log("Enterprise listing generation is deterministic (5 generations byte-identical)", []);
                false ->
                    ct:fail(determinism_violation)
            end;
        false ->
            ct:fail(generation_failed)
    end.

test_verify_gov_deterministic_generation(Config) ->
    {ok, PlanSpec} = erlmcp_plan_loader:load_plan(gov),
    %% Generate 5 times
    Results = [erlmcp_marketplace_copy:generate_gov_listing(PlanSpec) || _ <- lists:seq(1, 5)],
    %% Extract markdown from all results
    Markdowns = [M || {ok, M} <- Results],
    %% Verify all 5 succeeded
    case length(Markdowns) =:= 5 of
        true ->
            %% Verify all byte-identical
            [First | Rest] = Markdowns,
            AllIdentical = lists:all(fun(M) -> M =:= First end, Rest),
            case AllIdentical of
                true ->
                    ct:log("Government listing generation is deterministic (5 generations byte-identical)", []);
                false ->
                    ct:fail(determinism_violation)
            end;
        false ->
            ct:fail(generation_failed)
    end.

%% ===================================================================
%% Test Cases - Cross-References
%% ===================================================================

test_verify_team_refusal_codes_exist(Config) ->
    {ok, PlanSpec} = erlmcp_plan_loader:load_plan(team),
    {ok, Markdown} = erlmcp_marketplace_copy:generate_team_listing(PlanSpec),
    %% Verify refusal codes mentioned in plan appear in markdown
    ?assertMatch(true, binary:match(Markdown, <<"rate_limit_exceeded">>) =/= nomatch),
    ?assertMatch(true, binary:match(Markdown, <<"service_unavailable">>) =/= nomatch),
    ?assertMatch(true, binary:match(Markdown, <<"payload_too_large">>) =/= nomatch),
    ct:log("Team refusal codes verified in markdown", []).

test_verify_enterprise_sla_values_match(Config) ->
    {ok, PlanSpec} = erlmcp_plan_loader:load_plan(enterprise),
    {ok, Markdown} = erlmcp_marketplace_copy:generate_enterprise_listing(PlanSpec),
    %% Extract SLA from plan spec
    SLA = maps:get(<<"sla">>, PlanSpec, #{}),
    Availability = maps:get(<<"availability_percentage">>, SLA, undefined),
    %% Verify SLA value appears in markdown
    case Availability of
        undefined ->
            ct:log("No SLA availability in plan spec", []);
        Avail ->
            AvailStr = erlang:float_to_binary(Avail, [{decimals, 2}]),
            ?assertMatch(true, binary:match(Markdown, AvailStr) =/= nomatch),
            ct:log("Enterprise SLA values verified: ~s%", [AvailStr])
    end.

test_verify_gov_compliance_fields_present(Config) ->
    {ok, PlanSpec} = erlmcp_plan_loader:load_plan(gov),
    {ok, Markdown} = erlmcp_marketplace_copy:generate_gov_listing(PlanSpec),
    %% Verify compliance fields
    ?assertMatch(true, binary:match(Markdown, <<"FIPS">>) =/= nomatch),
    ?assertMatch(true, binary:match(Markdown, <<"Audit">>) =/= nomatch),
    ?assertMatch(true, binary:match(Markdown, <<"Encryption">>) =/= nomatch),
    ct:log("Government compliance fields verified in markdown", []).
