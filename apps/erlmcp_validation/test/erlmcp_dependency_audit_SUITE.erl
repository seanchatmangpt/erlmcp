%%%-------------------------------------------------------------------
%%% @doc
%%% Dependency Audit Test Suite (FM-12)
%%%
%%% Comprehensive tests for automated dependency vulnerability scanning:
%%% - CVE detection in rebar.lock dependencies
%%% - CVSS scoring and severity classification
%%% - CI/CD blocking on critical vulnerabilities
%%% - NVD integration (mocked for tests)
%%% - Dependency freshness tracking
%%%
%%% == Test Organization ==
%%%
%%% 1. **CVE Detection Tests (8 tests)**:
%%%    - Parse rebar.lock and extract dependencies
%%%    - Query NVD for known vulnerabilities
%%%    - Parse CVE data and extract CVSS scores
%%%    - Filter by severity (Critical, High, Medium, Low)
%%%    - Handle missing/malformed CVE data
%%%
%%% 2. **Severity Classification Tests (6 tests)**:
%%%    - CVSS ≥ 9.0 → Critical (block)
%%%    - CVSS 7.0-8.9 → High (block)
%%%    - CVSS 4.0-6.9 → Medium (warn)
%%%    - CVSS < 4.0 → Low (info)
%%%    - Multiple CVEs for same dependency
%%%    - CVE severity overrides
%%%
%%% 3. **Dependency Management Tests (6 tests)**:
%%%    - Dependency freshness check
%%%    - Outdated dependency detection
%%%    - Safe version recommendations
%%%    - Known vulnerable versions tracking
%%%    - Transitive dependency analysis
%%%    - Dependency update prioritization
%%%
%%% 4. **Report Generation Tests (4 tests)**:
%%%    - JSON report structure
%%%    - Human-readable summary
%%%    - CI/CD integration format
%%%    - Audit trail persistence
%%%
%%% == Chicago School TDD ==
%%% All tests use REAL erlmcp processes, no mocks except for NVD API.
%%% NVD API is mocked to avoid external dependencies in tests.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_dependency_audit_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% CT Callbacks
%%%====================================================================

all() ->
    [
     {group, cve_detection},
     {group, severity_classification},
     {group, dependency_management},
     {group, report_generation}
    ].

groups() ->
    [
     {cve_detection, [parallel], [
         test_parse_rebar_lock,
         test_extract_dependencies,
         test_query_nvd_for_cves,
         test_parse_cve_data,
         test_handle_missing_cve_data,
         test_handle_malformed_cve_data,
         test_multiple_cves_per_dependency,
         test_transitive_dependency_scanning
     ]},
     {severity_classification, [parallel], [
         test_cvss_critical_blocking,
         test_cvss_high_blocking,
         test_cvss_medium_warning,
         test_cvss_low_info,
         test_multiple_cves_highest_severity,
         test_severity_override
     ]},
     {dependency_management, [parallel], [
         test_dependency_freshness,
         test_outdated_detection,
         test_safe_version_recommendation,
         test_vulnerable_version_tracking,
         test_transitive_analysis,
         test_update_prioritization
     ]},
     {report_generation, [parallel], [
         test_json_report_structure,
         test_human_readable_summary,
         test_ci_integration_format,
         test_audit_trail_persistence
     ]}
    ].

init_per_suite(Config) ->
    %% Start erlmcp_core if needed
    application:ensure_all_started(erlmcp_core),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%====================================================================
%%% CVE Detection Tests (8 tests)
%%%====================================================================

%% @doc Test parsing rebar.lock file
test_parse_rebar_lock(_Config) ->
    %% Create test rebar.lock content
    LockContent = "{\"1.2.0\",\n"
                  "[{<<\"jose\">>,{pkg,<<\"jose\">>,<<\"1.11.1\">>},0},\n"
                  " {<<\"jsx\">>,{pkg,<<\"jsx\">>,<<\"3.1.0\">>},0}]}.\n",

    %% Write temporary lock file
    TempFile = "/tmp/test_rebar.lock",
    ok = file:write_file(TempFile, LockContent),

    %% Parse the lock file
    {ok, Parsed} = erlmcp_vulnerability_scanner:parse_lock_file(TempFile),

    %% Verify structure
    ?assert(is_list(Parsed)),
    ?assert(length(Parsed) >= 2),

    %% Cleanup
    file:delete(TempFile),
    ok.

%% @doc Test extracting dependency info from parsed lock
test_extract_dependencies(_Config) ->
    %% Sample parsed lock data
    Parsed = [
        {<<"jose">>, {pkg, <<"jose">>, <<"1.11.1">>}, 0},
        {<<"jsx">>, {pkg, <<"jsx">>, <<"3.1.0">>}, 0},
        {<<"gproc">>, {pkg, <<"gproc">>, <<"0.9.0">>}, 0}
    ],

    %% Extract dependencies
    Deps = erlmcp_vulnerability_scanner:extract_dependencies(Parsed),

    %% Verify extracted data
    ?assertEqual(3, length(Deps)),

    %% Check structure of first dependency
    [FirstDep | _] = Deps,
    ?assert(maps:is_key(name, FirstDep)),
    ?assert(maps:is_key(version, FirstDep)),
    ?assert(maps:is_key(package_type, FirstDep)),

    ok.

%% @doc Test querying NVD for CVEs (mocked)
test_query_nvd_for_cves(_Config) ->
    %% Mock NVD response for a vulnerable dependency
    Dependency = #{
        name => <<"jose">>,
        version => <<"1.11.1">>,
        package_type => pkg
    },

    %% Query NVD (using mock)
    {ok, CVEs} = erlmcp_vulnerability_scanner:query_nvd(Dependency, #{
        mock => true,
        mock_response => [
            #{
                cve_id => <<"CVE-2023-50967">>,
                cvss_score => 8.1,
                severity => high,
                description => <<"JWT signature bypass">>,
                affected_versions => [<<"1.11.1">>]
            }
        ]
    }),

    %% Verify CVE data
    ?assert(is_list(CVEs)),
    ?assertEqual(1, length(CVEs)),

    [CVE | _] = CVEs,
    ?assertEqual(<<"CVE-2023-50967">>, maps:get(cve_id, CVE)),
    ?assertEqual(8.1, maps:get(cvss_score, CVE)),
    ?assertEqual(high, maps:get(severity, CVE)),

    ok.

%% @doc Test parsing CVE data from NVD response
test_parse_cve_data(_Config) ->
    %% Sample NVD API response (simplified)
    NVDResponse = #{
        vulnerabilities => [
            #{
                cve => #{
                    id => <<"CVE-2023-50967">>,
                    metrics => #{
                        cvssMetricV31 => [#{
                            cvssData => #{
                                baseScore => 8.1,
                                baseSeverity => <<"HIGH">>
                            }
                        }]
                    },
                    descriptions => [#{
                        lang => <<"en">>,
                        value => <<"JWT signature verification bypass">>
                    }]
                }
            }
        ]
    },

    %% Parse CVE data
    Parsed = erlmcp_vulnerability_scanner:parse_nvd_response(NVDResponse),

    %% Verify parsed structure
    ?assert(is_list(Parsed)),
    ?assertEqual(1, length(Parsed)),

    [CVE | _] = Parsed,
    ?assertEqual(<<"CVE-2023-50967">>, maps:get(cve_id, CVE)),
    ?assertEqual(8.1, maps:get(cvss_score, CVE)),
    ?assertEqual(high, maps:get(severity, CVE)),

    ok.

%% @doc Test handling missing CVE data
test_handle_missing_cve_data(_Config) ->
    Dependency = #{
        name => <<"safe_package">>,
        version => <<"1.0.0">>,
        package_type => pkg
    },

    %% Query NVD for package with no CVEs
    {ok, CVEs} = erlmcp_vulnerability_scanner:query_nvd(Dependency, #{
        mock => true,
        mock_response => []
    }),

    %% Verify empty result
    ?assertEqual([], CVEs),

    ok.

%% @doc Test handling malformed CVE data
test_handle_malformed_cve_data(_Config) ->
    %% Malformed NVD response (missing required fields)
    MalformedResponse = #{
        vulnerabilities => [
            #{cve => #{id => <<"CVE-INVALID">>}}
            %% Missing metrics, descriptions, etc.
        ]
    },

    %% Parse should handle gracefully
    Parsed = erlmcp_vulnerability_scanner:parse_nvd_response(MalformedResponse),

    %% Should return empty or have default values
    case Parsed of
        [] -> ok;
        [CVE | _] ->
            %% Should have defaults for missing fields
            ?assert(maps:is_key(cve_id, CVE)),
            ?assert(maps:is_key(cvss_score, CVE)),
            ?assert(maps:get(cvss_score, CVE) =:= 0.0)
    end,

    ok.

%% @doc Test multiple CVEs for single dependency
test_multiple_cves_per_dependency(_Config) ->
    Dependency = #{
        name => <<"vulnerable_pkg">>,
        version => <<"1.0.0">>,
        package_type => pkg
    },

    %% Mock response with multiple CVEs
    {ok, CVEs} = erlmcp_vulnerability_scanner:query_nvd(Dependency, #{
        mock => true,
        mock_response => [
            #{cve_id => <<"CVE-2023-001">>, cvss_score => 7.5, severity => high},
            #{cve_id => <<"CVE-2023-002">>, cvss_score => 9.8, severity => critical},
            #{cve_id => <<"CVE-2023-003">>, cvss_score => 5.3, severity => medium}
        ]
    }),

    %% Verify all CVEs returned
    ?assertEqual(3, length(CVEs)),

    %% Find highest severity
    HighestSeverity = erlmcp_vulnerability_scanner:highest_severity(CVEs),
    ?assertEqual(critical, HighestSeverity),

    ok.

%% @doc Test transitive dependency scanning
test_transitive_dependency_scanning(_Config) ->
    %% Sample lock with transitive dependencies (depth > 0)
    Parsed = [
        {<<"direct_dep">>, {pkg, <<"direct_dep">>, <<"1.0.0">>}, 0},
        {<<"transitive_dep">>, {pkg, <<"transitive_dep">>, <<"2.0.0">>}, 1},
        {<<"nested_dep">>, {pkg, <<"nested_dep">>, <<"3.0.0">>}, 2}
    ],

    %% Extract including transitive
    AllDeps = erlmcp_vulnerability_scanner:extract_dependencies(Parsed),
    ?assertEqual(3, length(AllDeps)),

    %% Extract only direct
    DirectDeps = erlmcp_vulnerability_scanner:extract_dependencies(Parsed, #{
        direct_only => true
    }),
    ?assertEqual(1, length(DirectDeps)),

    ok.

%%%====================================================================
%%% Severity Classification Tests (6 tests)
%%%====================================================================

%% @doc Test CVSS ≥ 9.0 → Critical (blocking)
test_cvss_critical_blocking(_Config) ->
    CVE = #{
        cve_id => <<"CVE-CRITICAL">>,
        cvss_score => 9.8,
        severity => critical
    },

    %% Classify
    Classification = erlmcp_vulnerability_scanner:classify_cve(CVE),

    ?assertEqual(critical, maps:get(severity, Classification)),
    ?assertEqual(block, maps:get(action, Classification)),

    ok.

%% @doc Test CVSS 7.0-8.9 → High (blocking)
test_cvss_high_blocking(_Config) ->
    CVE = #{
        cve_id => <<"CVE-HIGH">>,
        cvss_score => 8.1,
        severity => high
    },

    Classification = erlmcp_vulnerability_scanner:classify_cve(CVE),

    ?assertEqual(high, maps:get(severity, Classification)),
    ?assertEqual(block, maps:get(action, Classification)),

    ok.

%% @doc Test CVSS 4.0-6.9 → Medium (warning)
test_cvss_medium_warning(_Config) ->
    CVE = #{
        cve_id => <<"CVE-MEDIUM">>,
        cvss_score => 5.3,
        severity => medium
    },

    Classification = erlmcp_vulnerability_scanner:classify_cve(CVE),

    ?assertEqual(medium, maps:get(severity, Classification)),
    ?assertEqual(warn, maps:get(action, Classification)),

    ok.

%% @doc Test CVSS < 4.0 → Low (info)
test_cvss_low_info(_Config) ->
    CVE = #{
        cve_id => <<"CVE-LOW">>,
        cvss_score => 3.1,
        severity => low
    },

    Classification = erlmcp_vulnerability_scanner:classify_cve(CVE),

    ?assertEqual(low, maps:get(severity, Classification)),
    ?assertEqual(info, maps:get(action, Classification)),

    ok.

%% @doc Test multiple CVEs - use highest severity
test_multiple_cves_highest_severity(_Config) ->
    CVEs = [
        #{cve_id => <<"CVE-001">>, cvss_score => 5.3, severity => medium},
        #{cve_id => <<"CVE-002">>, cvss_score => 9.1, severity => critical},
        #{cve_id => <<"CVE-003">>, cvss_score => 7.5, severity => high}
    ],

    %% Get overall classification
    Overall = erlmcp_vulnerability_scanner:classify_cves(CVEs),

    ?assertEqual(critical, maps:get(severity, Overall)),
    ?assertEqual(block, maps:get(action, Overall)),
    ?assertEqual(9.1, maps:get(max_cvss, Overall)),

    ok.

%% @doc Test severity override (manual exception)
test_severity_override(_Config) ->
    CVE = #{
        cve_id => <<"CVE-OVERRIDE">>,
        cvss_score => 9.0,
        severity => critical
    },

    %% Apply override (e.g., false positive)
    Override = #{
        cve_id => <<"CVE-OVERRIDE">>,
        override_severity => low,
        override_action => info,
        reason => <<"False positive - not applicable to our use case">>
    },

    Classification = erlmcp_vulnerability_scanner:classify_cve(CVE, #{
        overrides => [Override]
    }),

    ?assertEqual(low, maps:get(severity, Classification)),
    ?assertEqual(info, maps:get(action, Classification)),
    ?assert(maps:is_key(override_reason, Classification)),

    ok.

%%%====================================================================
%%% Dependency Management Tests (6 tests)
%%%====================================================================

%% @doc Test dependency freshness check
test_dependency_freshness(_Config) ->
    Dependency = #{
        name => <<"jose">>,
        version => <<"1.11.1">>,
        package_type => pkg
    },

    %% Check freshness (mock Hex.pm API)
    {ok, Freshness} = erlmcp_dependency_management:check_freshness(Dependency, #{
        mock => true,
        latest_version => <<"1.11.2">>,
        release_date => <<"2024-06-15">>
    }),

    ?assert(maps:is_key(is_latest, Freshness)),
    ?assertEqual(false, maps:get(is_latest, Freshness)),
    ?assertEqual(<<"1.11.2">>, maps:get(latest_version, Freshness)),

    ok.

%% @doc Test outdated dependency detection
test_outdated_detection(_Config) ->
    Dependencies = [
        #{name => <<"jose">>, version => <<"1.11.1">>},
        #{name => <<"jsx">>, version => <<"3.1.0">>},
        #{name => <<"old_pkg">>, version => <<"0.5.0">>}
    ],

    %% Detect outdated (mock latest versions)
    Outdated = erlmcp_dependency_management:detect_outdated(Dependencies, #{
        mock => true,
        latest_versions => #{
            <<"jose">> => <<"1.11.2">>,
            <<"jsx">> => <<"3.1.0">>,  % Up to date
            <<"old_pkg">> => <<"2.0.0">>  % Major outdated
        }
    }),

    %% Should detect 2 outdated packages
    ?assertEqual(2, length(Outdated)),

    ok.

%% @doc Test safe version recommendation
test_safe_version_recommendation(_Config) ->
    Dependency = #{
        name => <<"jose">>,
        version => <<"1.11.1">>,
        cves => [
            #{cve_id => <<"CVE-2023-50967">>, cvss_score => 8.1, severity => high}
        ]
    },

    %% Get safe version recommendation
    {ok, Recommendation} = erlmcp_dependency_management:recommend_safe_version(
        Dependency, #{
            mock => true,
            safe_versions => [<<"1.11.2">>, <<"1.11.3">>]
        }
    ),

    ?assert(maps:is_key(recommended_version, Recommendation)),
    ?assertEqual(<<"1.11.2">>, maps:get(recommended_version, Recommendation)),
    ?assert(maps:is_key(fixes_cves, Recommendation)),

    ok.

%% @doc Test vulnerable version tracking
test_vulnerable_version_tracking(_Config) ->
    %% Track known vulnerable versions
    VulnerableVersions = #{
        <<"jose">> => [
            #{version => <<"1.11.1">>, cve => <<"CVE-2023-50967">>},
            #{version => <<"1.11.0">>, cve => <<"CVE-2023-50967">>}
        ]
    },

    %% Check if version is vulnerable
    IsVulnerable = erlmcp_dependency_management:is_vulnerable_version(
        <<"jose">>, <<"1.11.1">>, VulnerableVersions
    ),

    ?assertEqual(true, IsVulnerable),

    %% Check safe version
    IsSafe = erlmcp_dependency_management:is_vulnerable_version(
        <<"jose">>, <<"1.11.2">>, VulnerableVersions
    ),

    ?assertEqual(false, IsSafe),

    ok.

%% @doc Test transitive dependency analysis
test_transitive_analysis(_Config) ->
    %% Dependency tree
    DependencyTree = #{
        <<"app">> => [
            #{name => <<"jose">>, version => <<"1.11.1">>, depth => 1},
            #{name => <<"jsx">>, version => <<"3.1.0">>, depth => 1}
        ],
        <<"jose">> => [
            #{name => <<"base64url">>, version => <<"1.0.1">>, depth => 2}
        ]
    },

    %% Analyze transitive dependencies
    Analysis = erlmcp_dependency_management:analyze_transitive(DependencyTree),

    ?assert(maps:is_key(total_dependencies, Analysis)),
    ?assert(maps:get(total_dependencies, Analysis) >= 3),
    ?assert(maps:is_key(max_depth, Analysis)),

    ok.

%% @doc Test dependency update prioritization
test_update_prioritization(_Config) ->
    Vulnerabilities = [
        #{name => <<"pkg1">>, cvss_score => 9.1, severity => critical},
        #{name => <<"pkg2">>, cvss_score => 5.3, severity => medium},
        #{name => <<"pkg3">>, cvss_score => 8.1, severity => high}
    ],

    %% Prioritize updates
    Prioritized = erlmcp_dependency_management:prioritize_updates(Vulnerabilities),

    %% Should be sorted by severity (critical > high > medium)
    [First | _] = Prioritized,
    ?assertEqual(critical, maps:get(severity, First)),
    ?assertEqual(9.1, maps:get(cvss_score, First)),

    ok.

%%%====================================================================
%%% Report Generation Tests (4 tests)
%%%====================================================================

%% @doc Test JSON report structure
test_json_report_structure(_Config) ->
    ScanResults = #{
        scanned_at => erlang:system_time(second),
        dependencies_scanned => 15,
        vulnerabilities_found => 2,
        critical => 1,
        high => 1,
        medium => 0,
        low => 0,
        vulnerabilities => [
            #{
                dependency => <<"jose">>,
                version => <<"1.11.1">>,
                cve_id => <<"CVE-2023-50967">>,
                cvss_score => 8.1,
                severity => high,
                action => block
            }
        ]
    },

    %% Generate JSON report
    {ok, JSONReport} = erlmcp_vulnerability_scanner:generate_json_report(ScanResults),

    %% Verify JSON structure
    ?assert(is_binary(JSONReport)),

    %% Parse back to verify
    {ok, Parsed} = jsx:decode(JSONReport, [return_maps]),
    ?assert(maps:is_key(<<"scanned_at">>, Parsed)),
    ?assert(maps:is_key(<<"vulnerabilities">>, Parsed)),

    ok.

%% @doc Test human-readable summary
test_human_readable_summary(_Config) ->
    ScanResults = #{
        dependencies_scanned => 15,
        vulnerabilities_found => 2,
        critical => 1,
        high => 1,
        medium => 0,
        low => 0
    },

    %% Generate summary
    Summary = erlmcp_vulnerability_scanner:generate_summary(ScanResults),

    %% Verify summary format
    ?assert(is_binary(Summary)),
    ?assert(byte_size(Summary) > 0),

    %% Should contain key information
    ?assert(binary:match(Summary, <<"15">>) =/= nomatch),  % Dependencies scanned
    ?assert(binary:match(Summary, <<"2">>) =/= nomatch),   % Vulnerabilities found

    ok.

%% @doc Test CI/CD integration format
test_ci_integration_format(_Config) ->
    ScanResults = #{
        vulnerabilities_found => 2,
        critical => 1,
        high => 1,
        exit_code => 1  % Non-zero for blocking
    },

    %% Generate CI format
    CIOutput = erlmcp_vulnerability_scanner:generate_ci_output(ScanResults),

    %% Verify CI format
    ?assert(maps:is_key(exit_code, CIOutput)),
    ?assertEqual(1, maps:get(exit_code, CIOutput)),
    ?assert(maps:is_key(summary, CIOutput)),

    ok.

%% @doc Test audit trail persistence
test_audit_trail_persistence(_Config) ->
    ScanResults = #{
        scanned_at => erlang:system_time(second),
        dependencies_scanned => 15,
        vulnerabilities_found => 2
    },

    %% Save audit trail
    AuditFile = "/tmp/cve_audit_" ++ integer_to_list(erlang:system_time(second)) ++ ".json",
    ok = erlmcp_vulnerability_scanner:save_audit_trail(ScanResults, AuditFile),

    %% Verify file exists
    ?assert(filelib:is_file(AuditFile)),

    %% Read back and verify
    {ok, Content} = file:read_file(AuditFile),
    {ok, Parsed} = jsx:decode(Content, [return_maps]),
    ?assertEqual(15, maps:get(<<"dependencies_scanned">>, Parsed)),

    %% Cleanup
    file:delete(AuditFile),

    ok.
