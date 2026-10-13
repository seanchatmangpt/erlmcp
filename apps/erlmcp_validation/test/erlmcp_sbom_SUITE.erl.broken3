%%%-------------------------------------------------------------------
%%% @doc Common Test Suite for SBOM Generation
%%%
%%% == End-to-End Integration Tests ==
%%%
%%% Tests complete SBOM generation workflows:
%%% - Dependency collection from rebar.lock
%%% - SPDX 2.3 format generation
%%% - Vulnerability scanning
%%% - CI/CD integration
%%%
%%% == Test Organization ==
%%'
%%% 1. **Dependency Collection Tests**
%%%    - Parse rebar.lock
%%%    - Extract exact versions
%%%    - Build dependency graph
%%%
%%% 2. **SBOM Generation Tests**
%%%    - Generate SPDX 2.3 SBOM
%%%    - Validate format
%%%    - Convert to JSON
%%%
%%% 3. **Vulnerability Scanning Tests**
%%%    - Scan dependencies
%%%    - Generate reports
%%%    - Check CVE database
%%%
%%% 4. **CI/CD Integration Tests**
%%%    - Release workflow
%%%    - Artifact upload
%%%    - Security gates
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sbom_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%% CT callbacks
-export([all/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([dependency_collection_from_lock/1,
         sbom_generation_spdx23/1,
         vulnerability_scanning_workflow/1,
         cicd_release_integration/1,
         complete_sbom_pipeline/1]).

%%%====================================================================
%%% CT Callbacks
%%%====================================================================

all() ->
    [dependency_collection_from_lock,
     sbom_generation_spdx23,
     vulnerability_scanning_workflow,
     cicd_release_integration,
     complete_sbom_pipeline].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_validation),
    {ok, _} = erlmcp_sbom:start_link(),
    Config.

end_per_suite(_Config) ->
    gen_server:stop(erlmcp_sbom),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Test dependency collection from rebar.lock
dependency_collection_from_lock(_Config) ->
    %% Collect dependencies
    Deps = erlmcp_dependency_collector:collect(),

    %% Verify dependencies collected
    ?assert(length(Deps) > 0),

    %% Check for known dependencies
    DepNames = [element(1, D) || D <- Deps, D =/= skip],
    ?assert(lists:member(<<"jsx">>, DepNames)),
    ?assert(lists:member(<<"jesse">>, DepNames)),
    ?assert(lists:member(<<"gproc">>, DepNames)),

    %% Verify version format
    lists:foreach(fun
        (skip) -> ok;
        ({_Name, Vsn, _Level}) ->
            ?assert(is_binary(Vsn)),
            ?assert(size(Vsn) > 0)
    end, Deps),

    %% Check for circular dependencies
    ?assertEqual(ok, erlmcp_dependency_collector:check_circular_deps()),

    ok.

%% @doc Test SBOM generation with SPDX 2.3 format
sbom_generation_spdx23(_Config) ->
    %% Generate SBOM
    {ok, SBOM} = erlmcp_sbom:generate(),

    %% Validate SPDX version
    ?assertEqual(<<"SPDX-2.3">>, maps:get(<<"spdxVersion">>, SBOM)),

    %% Validate data license
    ?assertEqual(<<"CC0-1.0">>, maps:get(<<"dataLicense">>, SBOM)),

    %% Verify required fields
    RequiredFields = [<<"spdxVersion">>, <<"dataLicense">>, <<"spdxId">>,
                      <<"name">>, <<"version">>],
    lists:foreach(fun(Field) ->
        ?assert(maps:is_key(Field, SBOM))
    end, RequiredFields),

    %% Validate SPDX format
    ok = erlmcp_sbom:validate_spdx(SBOM),

    %% Verify packages
    Packages = maps:get(packages, SBOM),
    ?assert(length(Packages) >= 10),

    %% Verify OTP package included
    OTPPackages = [P || P <- Packages, maps:get(<<"name">>, P) =:= <<"otp">>],
    ?assert(length(OTPPackages) > 0),

    %% Verify relationships
    Relationships = maps:get(relationships, SBOM),
    ?assert(length(Relationships) >= 10),

    %% Convert to JSON
    {ok, JSON} = erlmcp_sbom:to_spdx_json(SBOM),
    ?assert(is_binary(JSON)),

    %% Verify JSON validity
    Decoded = jsx:decode(JSON),
    ?assertEqual(<<"SPDX-2.3">>, maps:get(<<"spdxVersion">>, Decoded)),

    ok.

%% @doc Test vulnerability scanning workflow
vulnerability_scanning_workflow(_Config) ->
    %% Generate SBOM
    {ok, SBOM} = erlmcp_sbom:generate(),

    %% Scan for vulnerabilities
    {ok, Vulns} = erlmcp_sbom:scan_vulnerabilities(SBOM),
    ?assert(is_list(Vulns)),

    %% Generate severity report
    Report = erlmcp_vulnerability_scanner:severity_report(Vulns),
    ?assert(is_map(Report)),

    %% Verify report fields
    ?assert(is_integer(maps:get(critical, Report))),
    ?assert(is_integer(maps:get(high, Report))),
    ?assert(is_integer(maps:get(medium, Report))),
    ?assert(is_integer(maps:get(low, Report))),
    ?assert(is_integer(maps:get(total, Report))),

    %% Verify total matches vulnerabilities
    ?assertEqual(length(Vulns), maps:get(total, Report)),

    %% Filter by severity
    Critical = erlmcp_vulnerability_scanner:filter_by_severity(critical, Vulns),
    High = erlmcp_vulnerability_scanner:filter_by_severity(high, Vulns),

    ?assertEqual(maps:get(critical, Report), length(Critical)),
    ?assertEqual(maps:get(high, Report), length(High)),

    ok.

%% @doc Test CI/CD release integration
cicd_release_integration(_Config) ->
    %% Simulate release workflow
    Version = <<"2.1.0">>,

    %% 1. Generate SBOM for release
    {ok, SBOM} = erlmcp_sbom:generate(Version),
    ?assertEqual(Version, maps:get(<<"version">>, SBOM)),

    %% 2. Validate SBOM
    ok = erlmcp_sbom:validate_spdx(SBOM),

    %% 3. Convert to JSON for artifact upload
    {ok, JSON} = erlmcp_sbom:to_json(SBOM),
    ?assert(is_binary(JSON)),

    %% 4. Scan for vulnerabilities (security gate)
    {ok, Vulns} = erlmcp_sbom:scan_vulnerabilities(SBOM),

    %% 5. Check for critical vulnerabilities (block release)
    Report = erlmcp_vulnerability_scanner:severity_report(Vulns),
    CriticalCount = maps:get(critical, Report),
    ?assertEqual(0, CriticalCount, "Release blocked: critical vulnerabilities found"),

    %% 6. Generate CycloneDX format
    {ok, CycloneDX} = erlmcp_sbom:to_json(SBOM),
    ?assert(is_binary(CycloneDX)),

    %% 7. Verify both formats are valid JSON
    ?assertMatch({ok, _}, jsx:decode(JSON)),
    ?assertMatch({ok, _}, jsx:decode(CycloneDX)),

    ok.

%% @doc Test complete SBOM pipeline
complete_sbom_pipeline(_Config) ->
    %% Complete end-to-end SBOM generation and validation

    %% Step 1: Collect dependencies
    Deps = erlmcp_dependency_collector:collect(),
    ?assert(length(Deps) > 0),

    %% Step 2: Build dependency graph
    Graph = erlmcp_dependency_collector:get_dependency_graph(),
    ?assert(is_map(Graph)),
    ?assert(maps:size(Graph) > 0),

    %% Step 3: Generate SBOM
    {ok, SBOM} = erlmcp_sbom:generate(),
    ok = erlmcp_sbom:validate_spdx(SBOM),

    %% Step 4: Verify all dependencies in SBOM
    Packages = maps:get(packages, SBOM),
    PackageNames = [maps:get(<<"name">>, P) || P <- Packages],

    lists:foreach(fun
        (skip) -> ok;
        ({Name, _Vsn, _Level}) ->
            ?assert(lists:member(Name, PackageNames))
    end, Deps),

    %% Step 5: Scan for vulnerabilities
    {ok, Vulns} = erlmcp_sbom:scan_vulnerabilities(SBOM),
    Report = erlmcp_vulnerability_scanner:severity_report(Vulns),

    %% Step 6: Verify no critical vulnerabilities
    ?assertEqual(0, maps:get(critical, Report)),

    %% Step 7: Generate both SPDX and CycloneDX
    {ok, SPDXJson} = erlmcp_sbom:to_spdx_json(SBOM),
    {ok, CycloneDXJson} = erlmcp_sbom:to_json(SBOM),

    %% Step 8: Verify both are valid JSON
    ?assertMatch({ok, _}, jsx:decode(SPDXJson)),
    ?assertMatch({ok, _}, jsx:decode(CycloneDXJson)),

    %% Step 9: Verify OTP version included
    OTPVersion = erlang:system_info(otp_release),
    ?assert(length(OTPPackages) > 0),

    ok.
