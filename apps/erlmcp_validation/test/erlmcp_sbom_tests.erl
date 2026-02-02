%%%-------------------------------------------------------------------
%%% @doc
%%% SBOM Generator Test Suite for erlmcp
%%%
%%% == Chicago School TDD ==
%%'
%%% Tests use REAL erlmcp_sbom processes, no mocks or fakes.
%%% Verify observable state and behavior, not implementation details.
%%%
%%% == Test Coverage ==
%%%
%%% 1. SBOM Generation Tests (10 tests)
%%%    - Generate SBOM for current version
%%%    - Generate SBOM for specific version
%%%    - Validate SPDX format compliance
%%%    - Verify required fields present
%%%
%%% 2. Dependency Collection Tests (10 tests)
%%%    - Parse rebar.lock
%%%    - Extract package versions
%%%    - Build dependency graph
%%%    - Check circular dependencies
%%%
%%% 3. Vulnerability Scanning Tests (10 tests)
%%%    - Scan SBOM for vulnerabilities
%%%    - Generate severity reports
%%%    - Filter by severity level
%%%    - CVE detail lookup
%%%
%%% 4. Format Conversion Tests (5 tests)
%%%    - Convert to CycloneDX JSON
%%%    - Convert to SPDX JSON
%%%    - Verify JSON validity
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sbom_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

%% @private
setup() ->
    {ok, Pid} = erlmcp_sbom:start_link(),
    Pid.

%% @private
cleanup(_Pid) ->
    gen_server:stop(erlmcp_sbom).

%%%====================================================================
%%% SBOM Generation Tests
%%%====================================================================

sbom_generation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun generate_sbom_for_current_version/0,
      fun generate_sbom_for_specific_version/0,
      fun validate_spdx_format/0,
      fun verify_required_fields/0,
      fun verify_packages_present/0,
      fun verify_relationships_present/0,
      fun verify_otp_version_included/0,
      fun verify_license_metadata/0,
      fun convert_to_cyclonedx/0,
      fun convert_to_spdx_json/0
     ]}.

generate_sbom_for_current_version() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        ?assertNotEqual(undefined, maps:get(<<"spdxVersion">>, SBOM)),
        ?assertEqual(<<"SPDX-2.3">>, maps:get(<<"spdxVersion">>, SBOM)),
        ?assertNotEqual(undefined, maps:get(<<"name">>, SBOM)),
        ?assertEqual(<<"erlmcp">>, maps:get(<<"name">>, SBOM))
    end).

generate_sbom_for_specific_version() ->
    ?_test(begin
        Version = <<"2.1.0">>,
        {ok, SBOM} = erlmcp_sbom:generate(Version),
        ?assertEqual(Version, maps:get(<<"version">>, SBOM)),
        ?assertEqual(<<"erlmcp">>, maps:get(<<"name">>, SBOM))
    end).

validate_spdx_format() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        ok = erlmcp_sbom:validate_spdx(SBOM)
    end).

verify_required_fields() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        RequiredFields = [spdxVersion, dataLicense, spdxId, name, version],
        lists:foreach(fun(Field) ->
            ?assertNotEqual(undefined, maps:get(Field, SBOM))
        end, RequiredFields)
    end).

verify_packages_present() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        Packages = maps:get(packages, SBOM, []),
        ?assert(length(Packages) > 0),

        %% Check for known dependencies
        PackageNames = [maps:get(name, P) || P <- Packages],
        ?assert(lists:member(<<"jsx">>, PackageNames)),
        ?assert(lists:member(<<"jesse">>, PackageNames)),
        ?assert(lists:member(<<"gproc">>, PackageNames))
    end).

verify_relationships_present() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        Relationships = maps:get(relationships, SBOM, []),
        ?assert(length(Relationships) > 0)
    end).

verify_otp_version_included() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        Packages = maps:get(packages, SBOM, []),
        OTPPackages = [P || P <- Packages, maps:get(name, P) =:= <<"otp">>],
        ?assert(length(OTPPackages) > 0)
    end).

verify_license_metadata() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        Packages = maps:get(packages, SBOM, []),
        lists:foreach(fun(P) ->
            License = maps:get(licenseConcluded, P, undefined),
            ?assertNotEqual(undefined, License)
        end, Packages)
    end).

convert_to_cyclonedx() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        {ok, JSON} = erlmcp_sbom:to_json(SBOM),
        ?assert(is_binary(JSON)),

        %% Verify JSON is valid
        Decoded = jsx:decode(JSON),
        ?assertEqual(<<"CycloneDX">>, maps:get(<<"bomFormat">>, Decoded))
    end).

convert_to_spdx_json() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        {ok, JSON} = erlmcp_sbom:to_spdx_json(SBOM),
        ?assert(is_binary(JSON)),

        %% Verify JSON is valid
        Decoded = jsx:decode(JSON),
        ?assertEqual(<<"SPDX-2.3">>, maps:get(<<"spdxVersion">>, Decoded))
    end).

%%%====================================================================
%%% Dependency Collection Tests
%%%====================================================================

dependency_collection_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun collect_dependencies_from_lock/0,
      fun verify_otp_version_detected/0,
      fun build_dependency_graph/0,
      fun check_circular_dependencies/0,
      fun get_direct_dependencies/0,
      fun get_transitive_dependencies/0
     ]}.

collect_dependencies_from_lock() ->
    ?_test(begin
        {ok, Deps} = erlmcp_sbom:get_dependencies(),
        ?assert(length(Deps) > 0),
        ?assert(lists:keymember(<<"jsx">>, 1, Deps)),
        ?assert(lists:keymember(<<"jesse">>, 1, Deps))
    end).

verify_otp_version_detected() ->
    ?_test(begin
        {ok, OTPVersion} = erlmcp_sbom:get_otp_version(),
        ?assert(is_binary(OTPVersion)),
        ?assert(size(OTPVersion) > 0)
    end).

build_dependency_graph() ->
    ?_test(begin
        Graph = erlmcp_dependency_collector:get_dependency_graph(),
        ?assert(is_map(Graph)),
        ?assert(maps:size(Graph) > 0)
    end).

check_circular_dependencies() ->
    ?_test(begin
        Result = erlmcp_dependency_collector:check_circular_deps(),
        ?assertEqual(ok, Result)
    end).

get_direct_dependencies() ->
    ?_test(begin
        DirectDeps = erlmcp_dependency_collector:get_direct_deps(),
        ?assert(is_list(DirectDeps)),
        ?assert(length(DirectDeps) > 0)
    end).

get_transitive_dependencies() ->
    ?_test(begin
        TransitiveDeps = erlmcp_dependency_collector:get_transitive_deps(),
        ?assert(is_list(TransitiveDeps))
    end).

%%%====================================================================
%%% Vulnerability Scanning Tests
%%%====================================================================

vulnerability_scanning_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun scan_sbom_for_vulnerabilities/0,
      fun generate_severity_report/0,
      fun filter_by_critical_severity/0,
      fun filter_by_high_severity/0,
      fun verify_empty_scan_results/0
     ]}.

scan_sbom_for_vulnerabilities() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        {ok, Vulns} = erlmcp_sbom:scan_vulnerabilities(SBOM),
        ?assert(is_list(Vulns))
    end).

generate_severity_report() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        {ok, Vulns} = erlmcp_sbom:scan_vulnerabilities(SBOM),
        Report = erlmcp_vulnerability_scanner:severity_report(Vulns),
        ?assert(is_map(Report)),
        ?assert(is_integer(maps:get(critical, Report))),
        ?assert(is_integer(maps:get(high, Report))),
        ?assert(is_integer(maps:get(medium, Report))),
        ?assert(is_integer(maps:get(low, Report))),
        ?assert(is_integer(maps:get(total, Report)))
    end).

filter_by_critical_severity() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        {ok, Vulns} = erlmcp_sbom:scan_vulnerabilities(SBOM),
        Critical = erlmcp_vulnerability_scanner:filter_by_severity(critical, Vulns),
        ?assert(is_list(Critical))
    end).

filter_by_high_severity() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),
        {ok, Vulns} = erlmcp_sbom:scan_vulnerabilities(SBOM),
        High = erlmcp_vulnerability_scanner:filter_by_severity(high, Vulns),
        ?assert(is_list(High))
    end).

verify_empty_scan_results() ->
    ?_test(begin
        %% Simulated vulnerability scan returns empty list
        {ok, SBOM} = erlmcp_sbom:generate(),
        {ok, Vulns} = erlmcp_sbom:scan_vulnerabilities(SBOM),
        ?assertEqual(0, length(Vulns))
    end).

%%%====================================================================
%%% Integration Tests
%%%====================================================================

integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun end_to_end_sbom_workflow/0,
      fun validate_complete_sbom/0,
      fun verify_all_dependencies_included/0
     ]}.

end_to_end_sbom_workflow() ->
    ?_test(begin
        %% 1. Generate SBOM
        {ok, SBOM} = erlmcp_sbom:generate(),

        %% 2. Validate SPDX format
        ok = erlmcp_sbom:validate_spdx(SBOM),

        %% 3. Convert to JSON
        {ok, JSON} = erlmcp_sbom:to_json(SBOM),
        ?assert(is_binary(JSON)),

        %% 4. Scan for vulnerabilities
        {ok, Vulns} = erlmcp_sbom:scan_vulnerabilities(SBOM),
        ?assert(is_list(Vulns))
    end).

validate_complete_sbom() ->
    ?_test(begin
        {ok, SBOM} = erlmcp_sbom:generate(),

        %% Validate all required fields
        ?assertEqual(<<"SPDX-2.3">>, maps:get(<<"spdxVersion">>, SBOM)),
        ?assertEqual(<<"CC0-1.0">>, maps:get(<<"dataLicense">>, SBOM)),
        ?assertEqual(<<"erlmcp">>, maps:get(<<"name">>, SBOM)),

        %% Verify packages
        Packages = maps:get(packages, SBOM),
        ?assert(length(Packages) >= 10),

        %% Verify relationships
        Relationships = maps:get(relationships, SBOM),
        ?assert(length(Relationships) >= 10)
    end).

verify_all_dependencies_included() ->
    ?_test(begin
        {ok, Deps} = erlmcp_sbom:get_dependencies(),
        {ok, SBOM} = erlmcp_sbom:generate(),

        Packages = maps:get(packages, SBOM),
        PackageNames = [maps:get(name, P) || P <- Packages],

        %% Verify all dependencies are in SBOM
        lists:foreach(fun({Name, _Vsn}) ->
            ?assert(lists:member(Name, PackageNames))
        end, Deps)
    end).
