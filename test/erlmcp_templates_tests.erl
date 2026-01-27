%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for TCPS template rendering module.
%%%
%%% This suite tests all TCPS templates with comprehensive coverage of:
%%% - Valid contexts (happy path)
%%% - Invalid/missing context keys
%%% - Edge cases and data variations
%%% - Template output validation
%%% - Deterministic rendering
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_templates_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Generators
%%%===================================================================

erlmcp_templates_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"List templates", fun test_list_templates/0},
            {"Receipt template - valid context", fun test_receipt_valid/0},
            {"Receipt template - missing keys", fun test_receipt_missing_keys/0},
            {"Receipt template - optional fields", fun test_receipt_optional/0},
            {"Receipt template - deterministic", fun test_receipt_deterministic/0},
            {"Work order template - valid context", fun test_work_order_valid/0},
            {"Work order template - missing keys", fun test_work_order_missing_keys/0},
            {"Work order template - with dependencies", fun test_work_order_dependencies/0},
            {"Andon event template - valid context", fun test_andon_event_valid/0},
            {"Andon event template - with 5 whys", fun test_andon_event_five_whys/0},
            {"Andon event template - missing keys", fun test_andon_event_missing_keys/0},
            {"SKU listing template - valid context", fun test_sku_listing_valid/0},
            {"SKU listing template - missing keys", fun test_sku_listing_missing_keys/0},
            {"SKU listing template - with receipts", fun test_sku_listing_receipts/0},
            {"Standard work template - valid context", fun test_standard_work_valid/0},
            {"Standard work template - missing keys", fun test_standard_work_missing_keys/0},
            {"Standard work template - with failures", fun test_standard_work_failures/0},
            {"Kaizen report template - valid context", fun test_kaizen_report_valid/0},
            {"Kaizen report template - missing keys", fun test_kaizen_report_missing_keys/0},
            {"Kaizen report template - with metrics", fun test_kaizen_report_metrics/0}
        ]
    }.

%%%===================================================================
%%% Setup/Cleanup
%%%===================================================================

setup() ->
    % Ensure templates directory exists
    ok = filelib:ensure_dir("templates/tcps/"),
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_list_templates() ->
    Templates = erlmcp_templates:list_templates(),
    ?assertEqual(6, length(Templates)),
    ?assert(lists:member(receipt, Templates)),
    ?assert(lists:member(work_order, Templates)),
    ?assert(lists:member(andon_event, Templates)),
    ?assert(lists:member(sku_listing, Templates)),
    ?assert(lists:member(standard_work, Templates)),
    ?assert(lists:member(kaizen_report, Templates)).

test_receipt_valid() ->
    Context = #{
        stage_name => <<"build">>,
        timestamp => <<"2026-01-26T17:30:00Z">>,
        status => <<"passed">>,
        sku_id => <<"erlmcp-v1.0.0">>,
        evidence_data => #{
            compiler => <<"erlc 25.3">>,
            warnings => 0,
            modules => 42
        }
    },
    {ok, Output} = erlmcp_templates:render_receipt(Context),
    ?assert(is_binary(Output)),
    ?assert(byte_size(Output) > 0),
    % Validate JSON structure
    JSON = jsx:decode(Output, [return_maps]),
    ?assertEqual(<<"build">>, maps:get(<<"stage_name">>, JSON)),
    ?assertEqual(<<"passed">>, maps:get(<<"status">>, JSON)),
    ?assertEqual(<<"erlmcp-v1.0.0">>, maps:get(<<"sku_id">>, JSON)).

test_receipt_missing_keys() ->
    Context = #{
        stage_name => <<"build">>,
        status => <<"passed">>
        % Missing: timestamp, sku_id, evidence_data
    },
    {error, {missing_context_keys, Missing}} = erlmcp_templates:render_receipt(Context),
    ?assert(lists:member(timestamp, Missing)),
    ?assert(lists:member(sku_id, Missing)),
    ?assert(lists:member(evidence_data, Missing)).

test_receipt_optional() ->
    Context = #{
        stage_name => <<"test">>,
        timestamp => <<"2026-01-26T18:00:00Z">>,
        status => <<"passed">>,
        sku_id => <<"erlmcp-test-v1.0.0">>,
        evidence_data => #{tests_passed => 100},
        checksum => <<"abc123def456">>,
        duration_ms => 5000,
        work_order_id => <<"wo-2026-001">>,
        environment => <<"staging">>
    },
    {ok, Output} = erlmcp_templates:render_receipt(Context),
    JSON = jsx:decode(Output, [return_maps]),
    ?assertEqual(<<"wo-2026-001">>, maps:get(<<"work_order_id">>, JSON)),
    ?assertEqual(<<"staging">>, maps:get(<<"environment">>, JSON)),
    ?assertEqual(5000, maps:get(<<"duration_ms">>, JSON)),
    ?assertEqual(<<"abc123def456">>, maps:get(<<"checksum">>, JSON)).

test_receipt_deterministic() ->
    % Render same context twice, should produce identical output
    Context = #{
        stage_name => <<"build">>,
        timestamp => <<"2026-01-26T17:30:00Z">>,
        status => <<"passed">>,
        sku_id => <<"erlmcp-v1.0.0">>,
        evidence_data => #{compiler => <<"erlc 25.3">>}
    },
    {ok, Output1} = erlmcp_templates:render_receipt(Context),
    {ok, Output2} = erlmcp_templates:render_receipt(Context),
    ?assertEqual(Output1, Output2).

test_work_order_valid() ->
    Context = #{
        work_order_id => <<"wo-2026-001">>,
        demand_signal => <<"marketplace">>,
        bucket => <<"reliability">>,
        priority => <<"high">>,
        created_by => <<"mcp-orchestrator">>,
        created_at => <<"2026-01-26T17:30:00Z">>,
        sku_id => <<"erlmcp-http-transport-v1.0.0">>
    },
    {ok, Output} = erlmcp_templates:render_work_order(Context),
    ?assert(is_binary(Output)),
    ?assert(byte_size(Output) > 0),
    % Check for RDF triples
    ?assertMatch({match, _}, re:run(Output, "tcps:wo-2026-001")),
    ?assertMatch({match, _}, re:run(Output, "tcps:WorkOrder")),
    ?assertMatch({match, _}, re:run(Output, "marketplace")),
    ?assertMatch({match, _}, re:run(Output, "reliability")).

test_work_order_missing_keys() ->
    Context = #{
        work_order_id => <<"wo-2026-001">>,
        demand_signal => <<"marketplace">>
        % Missing: bucket, priority, created_by, created_at, sku_id
    },
    {error, {missing_context_keys, Missing}} = erlmcp_templates:render_work_order(Context),
    ?assert(lists:member(bucket, Missing)),
    ?assert(lists:member(priority, Missing)).

test_work_order_dependencies() ->
    Context = #{
        work_order_id => <<"wo-2026-002">>,
        demand_signal => <<"internal">>,
        bucket => <<"security">>,
        priority => <<"critical">>,
        created_by => <<"security-scanner">>,
        created_at => <<"2026-01-26T18:00:00Z">>,
        sku_id => <<"erlmcp-auth-v2.0.0">>,
        dependencies => [<<"wo-2026-001">>, <<"wo-2026-000">>]
    },
    {ok, Output} = erlmcp_templates:render_work_order(Context),
    ?assertMatch({match, _}, re:run(Output, "tcps:dependsOn tcps:wo-2026-001")),
    ?assertMatch({match, _}, re:run(Output, "tcps:dependsOn tcps:wo-2026-000")).

test_andon_event_valid() ->
    Context = #{
        andon_id => <<"andon-2026-001">>,
        failure_reason => <<"SHACL validation failed">>,
        affected_sku => <<"erlmcp-broken-v1.0.0">>,
        severity => <<"critical">>,
        timestamp => <<"2026-01-26T19:00:00Z">>,
        detected_by => <<"shacl-validator">>,
        stage_name => <<"validate">>,
        error_details => <<"Shape violation: missing required property">>,
        remediation_status => <<"pending">>,
        spec_delta_required => true,
        test_delta_required => true
    },
    {ok, Output} = erlmcp_templates:render_andon_event(Context),
    ?assert(is_binary(Output)),
    ?assertMatch({match, _}, re:run(Output, "tcps:AndonEvent")),
    ?assertMatch({match, _}, re:run(Output, "tcps:lineStatus \"stopped\"")),
    ?assertMatch({match, _}, re:run(Output, "tcps:Quarantine")).

test_andon_event_five_whys() ->
    Context = #{
        andon_id => <<"andon-2026-002">>,
        failure_reason => <<"Test failure in HTTP transport">>,
        affected_sku => <<"erlmcp-http-v1.0.0">>,
        severity => <<"major">>,
        timestamp => <<"2026-01-26T20:00:00Z">>,
        detected_by => <<"test-runner">>,
        stage_name => <<"test">>,
        error_details => <<"Connection timeout">>,
        remediation_status => <<"in_progress">>,
        five_whys => [
            #{why => <<"Why did the test fail?">>, answer => <<"Connection timeout">>},
            #{why => <<"Why did it timeout?">>, answer => <<"Server not responding">>},
            #{why => <<"Why not responding?">>, answer => <<"Port conflict">>},
            #{why => <<"Why port conflict?">>, answer => <<"Hardcoded port">>},
            #{why => <<"Why hardcoded?">>, answer => <<"No configuration">>}
        ]
    },
    {ok, Output} = erlmcp_templates:render_andon_event(Context),
    ?assertMatch({match, _}, re:run(Output, "tcps:WhyAnalysis")),
    ?assertMatch({match, _}, re:run(Output, "Connection timeout")),
    ?assertMatch({match, _}, re:run(Output, "No configuration")).

test_andon_event_missing_keys() ->
    Context = #{
        andon_id => <<"andon-2026-003">>,
        failure_reason => <<"Some failure">>
        % Missing many required keys
    },
    {error, {missing_context_keys, Missing}} = erlmcp_templates:render_andon_event(Context),
    ?assert(lists:member(affected_sku, Missing)),
    ?assert(lists:member(severity, Missing)).

test_sku_listing_valid() ->
    Context = #{
        sku_id => <<"erlmcp-http-v1.0.0">>,
        name => <<"HTTP Transport for erlmcp">>,
        version => <<"1.0.0">>,
        description => <<"HTTP/HTTPS transport implementation">>,
        category => <<"Transport">>,
        author => <<"erlmcp Team">>,
        license => <<"Apache-2.0">>,
        repository_url => <<"https://github.com/example/erlmcp">>,
        features => [
            <<"HTTP/1.1 and HTTP/2 support">>,
            <<"TLS encryption">>,
            <<"Connection pooling">>
        ]
    },
    {ok, Output} = erlmcp_templates:render_sku_listing(Context),
    ?assert(is_binary(Output)),
    ?assertMatch({match, _}, re:run(Output, "# HTTP Transport for erlmcp")),
    ?assertMatch({match, _}, re:run(Output, "\\*\\*Version\\*\\*: 1.0.0")),
    ?assertMatch({match, _}, re:run(Output, "Apache-2.0")).

test_sku_listing_missing_keys() ->
    Context = #{
        sku_id => <<"erlmcp-broken">>,
        name => <<"Broken SKU">>
        % Missing required keys
    },
    {error, {missing_context_keys, Missing}} = erlmcp_templates:render_sku_listing(Context),
    ?assert(lists:member(version, Missing)),
    ?assert(lists:member(description, Missing)).

test_sku_listing_receipts() ->
    Context = #{
        sku_id => <<"erlmcp-complete-v1.0.0">>,
        name => <<"Complete SKU">>,
        version => <<"1.0.0">>,
        description => <<"Full featured SKU">>,
        category => <<"Tool">>,
        author => <<"Team">>,
        license => <<"MIT">>,
        repository_url => <<"https://example.com">>,
        features => [<<"Feature 1">>],
        receipts => [
            #{stage_name => <<"build">>, status => <<"passed">>,
              timestamp => <<"2026-01-26T12:00:00Z">>,
              receipt_url => <<"receipts/build.json">>},
            #{stage_name => <<"test">>, status => <<"passed">>,
              timestamp => <<"2026-01-26T12:05:00Z">>,
              receipt_url => <<"receipts/test.json">>}
        ]
    },
    {ok, Output} = erlmcp_templates:render_sku_listing(Context),
    ?assertMatch({match, _}, re:run(Output, "Quality Assurance")),
    ?assertMatch({match, _}, re:run(Output, "build.*passed")).

test_standard_work_valid() ->
    Context = #{
        stage_name => <<"Build Stage">>,
        stage_id => <<"build-001">>,
        description => <<"Compile Erlang code">>,
        inputs => [#{name => <<"Source Code">>, type => <<"erl">>, source => <<"src/">>}],
        outputs => [#{name => <<"BEAM Files">>, type => <<"beam">>, destination => <<"ebin/">>}],
        steps => [
            #{step_name => <<"Compile">>, action => <<"Run rebar3 compile">>}
        ],
        slo_time_budget_ms => 30000,
        slo_success_rate => 0.99
    },
    {ok, Output} = erlmcp_templates:render_standard_work(Context),
    ?assert(is_binary(Output)),
    ?assertMatch({match, _}, re:run(Output, "# Standard Work: Build Stage")),
    ?assertMatch({match, _}, re:run(Output, "30000ms")).

test_standard_work_missing_keys() ->
    Context = #{
        stage_name => <<"Incomplete">>,
        stage_id => <<"incomplete-001">>
        % Missing required keys
    },
    {error, {missing_context_keys, Missing}} = erlmcp_templates:render_standard_work(Context),
    ?assert(lists:member(description, Missing)),
    ?assert(lists:member(inputs, Missing)).

test_standard_work_failures() ->
    Context = #{
        stage_name => <<"Test Stage">>,
        stage_id => <<"test-001">>,
        description => <<"Run test suite">>,
        inputs => [#{name => <<"Tests">>, type => <<"erl">>, source => <<"test/">>}],
        outputs => [#{name => <<"Results">>, type => <<"json">>, destination => <<"reports/">>}],
        steps => [#{step_name => <<"Run">>, action => <<"Execute tests">>}],
        slo_time_budget_ms => 60000,
        slo_success_rate => 0.95,
        failure_modes => [
            #{failure_mode => <<"Timeout">>, severity => <<"medium">>,
              root_cause => <<"Slow tests">>, detection_method => <<"Timer">>,
              mitigation => <<"Optimize tests">>}
        ]
    },
    {ok, Output} = erlmcp_templates:render_standard_work(Context),
    ?assertMatch({match, _}, re:run(Output, "Failure Modes")),
    ?assertMatch({match, _}, re:run(Output, "Timeout")).

test_kaizen_report_valid() ->
    Context = #{
        report_id => <<"kaizen-2026-q1">>,
        report_period_start => <<"2026-01-01">>,
        report_period_end => <<"2026-03-31">>,
        generated_at => <<"2026-04-01T00:00:00Z">>,
        generated_by => <<"kaizen-agent">>,
        metrics_source => <<"quality_metrics.rq">>
    },
    {ok, Output} = erlmcp_templates:render_kaizen_report(Context),
    ?assert(is_binary(Output)),
    ?assertMatch({match, _}, re:run(Output, "# Kaizen Report")),
    ?assertMatch({match, _}, re:run(Output, "2026-01-01 to 2026-03-31")).

test_kaizen_report_missing_keys() ->
    Context = #{
        report_id => <<"incomplete-report">>
        % Missing required keys
    },
    {error, {missing_context_keys, Missing}} = erlmcp_templates:render_kaizen_report(Context),
    ?assert(lists:member(report_period_start, Missing)),
    ?assert(lists:member(generated_at, Missing)).

test_kaizen_report_metrics() ->
    Context = #{
        report_id => <<"kaizen-2026-q1">>,
        report_period_start => <<"2026-01-01">>,
        report_period_end => <<"2026-03-31">>,
        generated_at => <<"2026-04-01T00:00:00Z">>,
        generated_by => <<"kaizen-agent">>,
        metrics_source => <<"quality_metrics.rq">>,
        lead_time_data => #{
            average_lead_time_ms => 25000,
            median_lead_time_ms => 22000,
            p95_lead_time_ms => 40000,
            p99_lead_time_ms => 55000
        },
        defect_rate_data => #{
            total_runs => 1000,
            failed_runs => 5,
            defect_rate_percent => 0.5,
            target_defect_rate_percent => 1.0,
            six_sigma_level => 4.5
        },
        opportunities => [
            #{title => <<"Automate X">>, category => <<"automation">>,
              priority => <<"high">>, effort_estimate => <<"medium">>,
              impact_estimate => <<"high">>, description => <<"Automate manual step X">>}
        ]
    },
    {ok, Output} = erlmcp_templates:render_kaizen_report(Context),
    ?assertMatch({match, _}, re:run(Output, "Lead Time Analysis")),
    ?assertMatch({match, _}, re:run(Output, "Defect Rate Analysis")),
    ?assertMatch({match, _}, re:run(Output, "25000ms")),
    ?assertMatch({match, _}, re:run(Output, "4.5")).
