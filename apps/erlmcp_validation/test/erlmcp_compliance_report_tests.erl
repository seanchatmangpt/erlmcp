-module(erlmcp_compliance_report_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Data Setup
%%%====================================================================

%% Sample compliance data for testing
sample_compliance_data() ->
    #{
        spec_version => <<"2025-11-25">>,
        timestamp => <<"2026-01-30T12:00:00Z">>,
        test_results => [
            #{
                name => <<"initialize_must_be_first_test">>,
                requirement_id => <<"req-001">>,
                requirement_name => <<"initialize_required">>,
                status => <<"passed">>,
                timestamp => <<"2026-01-30T12:00:00Z">>,
                evidence => <<"Request before initialize returned error">>
            },
            #{
                name => <<"tools_list_returns_array_test">>,
                requirement_id => <<"req-002">>,
                requirement_name => <<"tools_list_array">>,
                status => <<"passed">>,
                timestamp => <<"2026-01-30T12:01:00Z">>,
                evidence => <<"tools/list returned array of 3 tools">>
            },
            #{
                name => <<"resources_subscribe_test">>,
                requirement_id => <<"req-003">>,
                requirement_name => <<"resources_subscribe">>,
                status => <<"failed">>,
                timestamp => <<"2026-01-30T12:02:00Z">>,
                evidence => <<"Subscribe failed with timeout">>
            },
            #{
                name => <<"stdio_newline_delimited_test">>,
                requirement_id => <<"req-004">>,
                requirement_name => <<"stdio_format">>,
                status => <<"passed">>,
                timestamp => <<"2026-01-30T12:03:00Z">>,
                evidence => <<"Messages properly delimited">>
            }
        ],
        spec_requirements => [
            #{
                id => <<"req-001">>,
                name => <<"initialize_required">>,
                section => <<"Lifecycle">>,
                description => <<"Client must send initialize before other requests">>
            },
            #{
                id => <<"req-002">>,
                name => <<"tools_list_array">>,
                section => <<"Tools">>,
                description => <<"tools/list must return array">>
            },
            #{
                id => <<"req-003">>,
                name => <<"resources_subscribe">>,
                section => <<"Resources">>,
                description => <<"Server must support resource subscriptions">>
            },
            #{
                id => <<"req-004">>,
                name => <<"stdio_format">>,
                section => <<"Transports">>,
                description => <<"stdio transport uses newline-delimited JSON">>
            },
            #{
                id => <<"req-005">>,
                name => <<"prompts_get">>,
                section => <<"Prompts">>,
                description => <<"prompts/get retrieves prompt by name">>
            }
        ]
    }.

%%%====================================================================
%%% Compliance Calculation Tests
%%%====================================================================

calculate_overall_compliance_test() ->
    Data = sample_compliance_data(),
    {ok, Compliance, Details} = erlmcp_compliance_report:calculate_compliance(Data),

    %% Verify compliance is a float between 0 and 100
    ?assert(is_float(Compliance)),
    ?assert(Compliance >= 0.0),
    ?assert(Compliance =< 100.0),

    %% Verify details structure
    ?assert(is_map(Details)),
    ?assert(maps:is_key(total_requirements, Details)),
    ?assert(maps:is_key(passed_tests, Details)),
    ?assert(maps:is_key(by_section, Details)),

    %% With 4 passed tests out of 5 requirements, should be 80%
    ?assertEqual(5, maps:get(total_requirements, Details)),
    ?assertEqual(4, maps:get(passed_tests, Details)),
    ?assertEqual(80.0, Compliance).

calculate_section_compliance_test() ->
    Data = sample_compliance_data(),
    {ok, _Compliance, Details} = erlmcp_compliance_report:calculate_compliance(Data),

    BySection = maps:get(by_section, Details),

    %% Verify we have section breakdowns
    ?assert(is_map(BySection)),
    ?assert(maps:size(BySection) > 0).

handle_empty_test_results_test() ->
    Data = #{
        spec_version => <<"2025-11-25">>,
        test_results => [],
        spec_requirements => [
            #{id => <<"req-001">>, name => <<"test_req">>, section => <<"Test">>}
        ]
    },

    {ok, Compliance, _Details} = erlmcp_compliance_report:calculate_compliance(Data),
    ?assertEqual(0.0, Compliance).

handle_missing_requirements_test() ->
    Data = #{
        spec_version => <<"2025-11-25">>,
        test_results => [
            #{name => <<"test">>, status => <<"passed">>,
              requirement_id => <<"req-001">>, requirement_name => <<"req">>}
        ],
        spec_requirements => []
    },

    {ok, Compliance, _Details} = erlmcp_compliance_report:calculate_compliance(Data),
    ?assertEqual(0.0, Compliance).

%%%====================================================================
%%% Report Generation Tests
%%%====================================================================

generate_markdown_report_test() ->
    Data = sample_compliance_data(),
    {ok, Markdown} = erlmcp_compliance_report:generate_report(markdown, Data),

    %% Verify it's binary
    ?assert(is_binary(Markdown)),

    %% Check for required sections
    ?assert(nomatch =/= binary:match(Markdown, <<"# MCP Specification Compliance Report">>)),
    ?assert(nomatch =/= binary:match(Markdown, <<"## Summary">>)),
    ?assert(nomatch =/= binary:match(Markdown, <<"## Detailed Evidence">>)),
    ?assert(nomatch =/= binary:match(Markdown, <<"## Gap Analysis">>)),
    ?assert(nomatch =/= binary:match(Markdown, <<"## Traceability Matrix">>)),

    %% Check for compliance percentage
    ?assert(nomatch =/= binary:match(Markdown, <<"80.00%">>)),

    %% Check for test evidence
    ?assert(nomatch =/= binary:match(Markdown, <<"initialize_must_be_first_test">>)).

generate_json_report_test() ->
    Data = sample_compliance_data(),
    {ok, Json} = erlmcp_compliance_report:generate_report(json, Data),

    %% Verify it's binary
    ?assert(is_binary(Json)),

    %% Parse JSON and verify structure
    {ok, Parsed} = jsx:decode(Json, [return_maps]),

    %% Verify top-level fields
    ?assert(maps:is_key(<<"spec_version">>, Parsed)),
    ?assert(maps:is_key(<<"timestamp">>, Parsed)),
    ?assert(maps:is_key(<<"overall">>, Parsed)),
    ?assert(maps:is_key(<<"by_section">>, Parsed)),
    ?assert(maps:is_key(<<"evidence">>, Parsed)),
    ?assert(maps:is_key(<<"gaps">>, Parsed)),
    ?assert(maps:is_key(<<"recommendations">>, Parsed)),
    ?assert(maps:is_key(<<"traceability">>, Parsed)),

    %% Verify compliance value
    Overall = maps:get(<<"overall">>, Parsed),
    ?assert(is_float(Overall)),
    ?assertEqual(80.0, Overall).

generate_html_report_test() ->
    Data = sample_compliance_data(),
    {ok, HTML} = erlmcp_compliance_report:generate_report(html, Data),

    %% Verify it's binary
    ?assert(is_binary(HTML)),

    %% Check for HTML structure
    ?assert(nomatch =/= binary:match(HTML, <<"<!DOCTYPE html>">>)),
    ?assert(nomatch =/= binary:match(HTML, <<"<html">>)),
    ?assert(nomatch =/= binary:match(HTML, <<"</html>">>)),
    ?assert(nomatch =/= binary:match(HTML, <<"<title>MCP Compliance Report</title>">>)).

%%%====================================================================
%%% Traceability Matrix Tests
%%%====================================================================

matrix_maps_requirements_to_tests_test() ->
    Data = sample_compliance_data(),
    Matrix = erlmcp_compliance_report:create_traceability_matrix(Data),

    %% Verify matrix structure
    ?assert(is_map(Matrix)),
    ?assert(maps:is_key(<<"req-001">>, Matrix)),
    ?assert(maps:is_key(<<"req-002">>, Matrix)),

    %% Check traceability data structure
    ReqData = maps:get(<<"req-001">>, Matrix),
    ?assert(maps:is_key(<<"requirement">>, ReqData)),
    ?assert(maps:is_key(<<"tests">>, ReqData)),
    ?assert(maps:is_key(<<"status">>, ReqData)).

matrix_includes_status_test() ->
    Data = sample_compliance_data(),
    Matrix = erlmcp_compliance_report:create_traceability_matrix(Data),

    %% Check status values are valid
    maps:foreach(fun(_ReqId, ReqData) ->
        Status = maps:get(<<"status">>, ReqData),
        ?assert(lists:member(Status, [<<"passed">>, <<"failed">>, <<"partial">>, <<"untested">>]))
    end, Matrix).

handles_untested_requirements_test() ->
    Data = sample_compliance_data(),
    Matrix = erlmcp_compliance_report:create_traceability_matrix(Data),

    %% req-005 has no tests
    ReqData = maps:get(<<"req-005">>, Matrix),
    ?assertEqual(<<"untested">>, maps:get(<<"status">>, ReqData)),
    ?assertEqual([], maps:get(<<"tests">>, ReqData)),
    ?assertEqual(<<"never">>, maps:get(<<"last_tested">>, ReqData)).

%%%====================================================================
%%% Gap Analysis Tests
%%%====================================================================

identifies_missing_tests_test() ->
    Data = sample_compliance_data(),
    Gaps = erlmcp_compliance_report:identify_gaps(Data),

    %% req-005 has no tests - should be identified as missing
    MissingGaps = [G || G <- Gaps, maps:get(status, G) =:= <<"missing">>],
    ?assert(length(MissingGaps) > 0).

identifies_failed_tests_test() ->
    Data = sample_compliance_data(),
    Gaps = erlmcp_compliance_report:identify_gaps(Data),

    %% req-003 has a failed test
    FailedGaps = [G || G <- Gaps, maps:get(status, G) =:= <<"failed">>],
    ?assert(length(FailedGaps) > 0).

classifies_gaps_by_severity_test() ->
    Data = sample_compliance_data(),
    Gaps = erlmcp_compliance_report:identify_gaps(Data),

    %% Check severity classifications
    lists:foreach(fun(Gap) ->
        Severity = maps:get(<<"severity">>, Gap),
        ?assert(lists:member(Severity, [critical, high, medium, low]))
    end, Gaps).

%%%====================================================================
%%% Report Summary Tests
%%%====================================================================

summary_includes_overall_compliance_test() ->
    Data = sample_compliance_data(),
    {ok, Json} = erlmcp_compliance_report:generate_report(json, Data),
    {ok, Report} = jsx:decode(Json, [return_maps]),

    Summary = erlmcp_compliance_report:get_report_summary(Report),

    ?assert(maps:is_key(<<"overall_compliance">>, Summary)),
    ?assertEqual(80.0, maps:get(<<"overall_compliance">>, Summary)).

summary_counts_sections_test() ->
    Data = sample_compliance_data(),
    {ok, Json} = erlmcp_compliance_report:generate_report(json, Data),
    {ok, Report} = jsx:decode(Json, [return_maps]),

    Summary = erlmcp_compliance_report:get_report_summary(Report),

    ?assert(maps:is_key(<<"total_sections">>, Summary)),
    ?assert(maps:get(<<"total_sections">>, Summary) > 0).

%%%====================================================================
%%% Integration Tests
%%%====================================================================

full_report_generation_workflow_test() ->
    Data = sample_compliance_data(),

    %% Generate all formats
    {ok, _Markdown} = erlmcp_compliance_report:generate_report(markdown, Data),
    {ok, Json} = erlmcp_compliance_report:generate_report(json, Data),
    {ok, _HTML} = erlmcp_compliance_report:generate_report(html, Data),

    %% Verify JSON can be parsed
    {ok, Parsed} = jsx:decode(Json, [return_maps]),

    %% Verify all expected data is present
    ?assert(maps:is_key(<<"overall">>, Parsed)),
    ?assert(maps:is_key(<<"gaps">>, Parsed)),
    ?assert(maps:is_key(<<"recommendations">>, Parsed)).

all_formats_consistent_test() ->
    Data = sample_compliance_data(),

    %% Generate all formats
    {ok, Markdown} = erlmcp_compliance_report:generate_report(markdown, Data),
    {ok, Json} = erlmcp_compliance_report:generate_report(json, Data),
    {ok, HTML} = erlmcp_compliance_report:generate_report(html, Data),

    %% Extract compliance from each format
    {ok, JsonParsed} = jsx:decode(Json, [return_maps]),
    JsonCompliance = maps:get(<<"overall">>, JsonParsed),

    %% All should show 80%
    ?assertEqual(80.0, JsonCompliance),
    ?assert(nomatch =/= binary:match(Markdown, <<"80.00%">>)),
    ?assert(nomatch =/= binary:match(HTML, <<"80.00%">>)).
