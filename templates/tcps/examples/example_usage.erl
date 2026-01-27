%%%-------------------------------------------------------------------
%%% @doc
%%% TCPS Template Usage Examples
%%%
%%% This module demonstrates how to use the TCPS templates for generating
%%% various artifacts in the Toyota Code Production System workflow.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_usage).

-export([
    generate_build_receipt/0,
    generate_work_order/0,
    generate_andon_event/0,
    generate_sku_listing/0,
    generate_standard_work/0,
    generate_kaizen_report/0,
    generate_all_examples/0
]).

%%%===================================================================
%%% Example 1: Build Stage Receipt
%%%===================================================================

generate_build_receipt() ->
    Context = #{
        stage_name => <<"build">>,
        timestamp => iso8601_now(),
        status => <<"passed">>,
        sku_id => <<"erlmcp-v0.6.0">>,
        evidence_data => #{
            compiler => <<"erlc 25.3">>,
            warnings => 0,
            errors => 0,
            modules_compiled => 42,
            beam_files => 42,
            build_tool => <<"rebar3">>,
            build_profile => <<"prod">>
        },
        checksum => <<"a1b2c3d4e5f6...">>,
        duration_ms => 3456,
        work_order_id => <<"wo-2026-001">>,
        environment => <<"production">>
    },
    {ok, Receipt} = erlmcp_templates:render_receipt(Context),
    file:write_file("receipts/build/erlmcp-v0.6.0.json", Receipt),
    io:format("Build receipt generated~n"),
    Receipt.

%%%===================================================================
%%% Example 2: Work Order for HTTP Transport
%%%===================================================================

generate_work_order() ->
    Context = #{
        work_order_id => <<"wo-2026-http-001">>,
        demand_signal => <<"marketplace">>,
        bucket => <<"reliability">>,
        priority => <<"high">>,
        created_by => <<"mcp-orchestrator">>,
        created_at => iso8601_now(),
        sku_id => <<"erlmcp-http-transport-v1.0.0">>,
        target_completion => iso8601_future(7),  % 7 days from now
        description => <<"Implement HTTP/HTTPS transport for MCP protocol">>,
        dependencies => [<<"wo-2026-registry-001">>]
    },
    {ok, WorkOrder} = erlmcp_templates:render_work_order(Context),
    file:write_file("ontology/work_orders/wo-2026-http-001.ttl", WorkOrder),
    io:format("Work order generated~n"),
    WorkOrder.

%%%===================================================================
%%% Example 3: Andon Event with 5 Whys
%%%===================================================================

generate_andon_event() ->
    Context = #{
        andon_id => <<"andon-2026-001">>,
        failure_reason => <<"SHACL validation failed on transport ontology">>,
        affected_sku => <<"erlmcp-tcp-transport-v1.0.0">>,
        severity => <<"critical">>,
        timestamp => iso8601_now(),
        detected_by => <<"shacl-validator">>,
        stage_name => <<"validate">>,
        work_order_id => <<"wo-2026-tcp-001">>,
        error_details => <<"
Shape violation: tcps:Transport missing required property tcps:portRange.
Expected: xsd:integer range [1024-65535]
Actual: undefined
Location: ontology/transports.ttl line 42
">>,
        remediation_status => <<"in_progress">>,
        remediation_plan => <<"
1. Add tcps:portRange property to Transport shape
2. Set default value 8080
3. Add SHACL constraint for valid port range
4. Regenerate ontology and retest
">>,
        spec_delta_required => true,
        test_delta_required => true,
        five_whys => [
            #{
                why => <<"Why did SHACL validation fail?">>,
                answer => <<"Missing required property tcps:portRange on Transport">>
            },
            #{
                why => <<"Why was the property missing?">>,
                answer => <<"Template didn't include portRange in generation">>
            },
            #{
                why => <<"Why didn't the template include it?">>,
                answer => <<"SPARQL query didn't extract port configuration">>
            },
            #{
                why => <<"Why didn't the query extract port config?">>,
                answer => <<"Ontology schema missing portRange definition">>
            },
            #{
                why => <<"Why was schema definition missing?">>,
                answer => <<"Initial spec didn't consider configurable ports">>
            }
        ]
    },
    {ok, AndonEvent} = erlmcp_templates:render_andon_event(Context),
    file:write_file("receipts/andon/andon-2026-001.ttl", AndonEvent),
    io:format("Andon event generated - LINE STOPPED~n"),
    AndonEvent.

%%%===================================================================
%%% Example 4: SKU Marketplace Listing
%%%===================================================================

generate_sku_listing() ->
    Context = #{
        sku_id => <<"erlmcp-http-transport-v1.0.0">>,
        name => <<"HTTP/HTTPS Transport for erlmcp">>,
        version => <<"1.0.0">>,
        description => <<"Production-grade HTTP/HTTPS transport implementation for Model Context Protocol">>,
        long_description => <<"
## Overview

This SKU provides a robust HTTP/HTTPS transport layer for the erlmcp Model Context Protocol implementation. It supports both HTTP/1.1 and HTTP/2 with TLS encryption, connection pooling, and automatic retry logic.

## Key Capabilities

- Full HTTP/1.1 and HTTP/2 support via Gun
- TLS 1.2+ encryption with certificate validation
- Connection pooling with configurable limits
- Automatic reconnection on failures
- Backpressure handling
- Comprehensive telemetry and metrics

## Production Ready

All TCPS quality gates passed with evidence receipts.
">>,
        category => <<"Transport">>,
        author => <<"erlmcp Team">>,
        license => <<"Apache-2.0">>,
        repository_url => <<"https://github.com/example/erlmcp">>,
        documentation_url => <<"https://erlmcp.dev/docs/transports/http">>,
        features => [
            <<"HTTP/1.1 and HTTP/2 support">>,
            <<"TLS 1.2+ encryption">>,
            <<"Connection pooling with backpressure">>,
            <<"Automatic reconnection">>,
            <<"OpenTelemetry instrumentation">>,
            <<"SHACL-validated ontology">>,
            <<"100% test coverage">>
        ],
        requirements => [
            <<"Erlang/OTP 25 or later">>,
            <<"gun 2.0.1+">>,
            <<"ranch 2.1.0+">>
        ],
        install_command => <<"rebar3 deps add erlmcp_http_transport">>,
        receipts => [
            #{stage_name => <<"build">>, status => <<"passed">>,
              timestamp => <<"2026-01-26T12:00:00Z">>,
              receipt_url => <<"receipts/build/http-transport.json">>},
            #{stage_name => <<"test">>, status => <<"passed">>,
              timestamp => <<"2026-01-26T12:05:00Z">>,
              receipt_url => <<"receipts/test/http-transport.json">>},
            #{stage_name => <<"validate">>, status => <<"passed">>,
              timestamp => <<"2026-01-26T12:10:00Z">>,
              receipt_url => <<"receipts/validate/http-transport.json">>},
            #{stage_name => <<"release">>, status => <<"passed">>,
              timestamp => <<"2026-01-26T12:15:00Z">>,
              receipt_url => <<"receipts/release/http-transport.json">>}
        ],
        quality_metrics => #{
            test_coverage => 100,
            build_time_ms => 3456,
            shacl_status => <<"passed">>,
            type_coverage => 100,
            deterministic => true
        },
        tags => [<<"transport">>, <<"http">>, <<"https">>, <<"mcp">>, <<"tcps-certified">>],
        created_at => <<"2026-01-20T00:00:00Z">>,
        updated_at => iso8601_now()
    },
    {ok, Listing} = erlmcp_templates:render_sku_listing(Context),
    file:write_file("dist/marketplace/erlmcp-http-transport-v1.0.0.md", Listing),
    io:format("SKU listing generated~n"),
    Listing.

%%%===================================================================
%%% Example 5: Standard Work for Build Stage
%%%===================================================================

generate_standard_work() ->
    Context = #{
        stage_name => <<"Build Stage">>,
        stage_id => <<"build-prod">>,
        description => <<"Compile Erlang source code to BEAM bytecode with production optimizations">>,
        version => <<"1.2.0">>,
        owner => <<"Build Team">>,
        last_updated => iso8601_now(),
        inputs => [
            #{name => <<"Source Code">>, type => <<"*.erl">>,
              source => <<"src/">>,
              description => <<"Erlang source modules">>,
              validation => <<"Syntax check with erlc -M">>},
            #{name => <<"Headers">>, type => <<"*.hrl">>,
              source => <<"include/">>,
              description => <<"Erlang header files">>,
              validation => <<"Referenced in source">>},
            #{name => <<"Dependencies">>, type => <<"rebar.lock">>,
              source => <<"./">>,
              description => <<"Locked dependency versions">>,
              validation => <<"Hash verification">>}
        ],
        outputs => [
            #{name => <<"BEAM Files">>, type => <<"*.beam">>,
              destination => <<"ebin/">>,
              description => <<"Compiled bytecode">>,
              format => <<"Erlang BEAM format">>},
            #{name => <<"App File">>, type => <<"*.app">>,
              destination => <<"ebin/">>,
              description => <<"Application metadata">>,
              format => <<"Erlang term format">>},
            #{name => <<"Build Receipt">>, type => <<"*.json">>,
              destination => <<"receipts/build/">>,
              description => <<"Build evidence">>,
              format => <<"TCPS receipt JSON">>}
        ],
        steps => [
            #{step_name => <<"Verify Dependencies">>,
              action => <<"Run 'rebar3 get-deps' to fetch dependencies">>,
              validation => <<"All deps in _build/default/lib">>,
              expected_time_ms => 2000,
              failure_action => <<"Raise Andon if any dep fails to fetch">>},
            #{step_name => <<"Compile Sources">>,
              action => <<"Run 'rebar3 as prod compile' with optimizations">>,
              validation => <<"0 errors, all BEAM files generated">>,
              expected_time_ms => 5000,
              failure_action => <<"Stop on first error, emit Andon event">>},
            #{step_name => <<"Generate Receipt">>,
              action => <<"Create build receipt with checksums and metadata">>,
              validation => <<"Receipt validates against schema">>,
              expected_time_ms => 500,
              failure_action => <<"Block release if receipt generation fails">>}
        ],
        slo_time_budget_ms => 10000,
        slo_success_rate => 0.995,
        receipts => [
            #{receipt_type => <<"Build Receipt">>,
              description => <<"Proof of successful compilation">>,
              path => <<"receipts/build/{sku_id}.json">>,
              schema_url => <<"https://erlmcp.dev/schemas/tcps/receipt-v1.json">>}
        ],
        tools => [
            #{name => <<"rebar3">>, command => <<"rebar3 as prod compile">>,
              description => <<"Erlang build tool">>},
            #{name => <<"erlc">>, command => <<"erlc +debug_info">>,
              description => <<"Erlang compiler">>}
        ],
        failure_modes => [
            #{failure_mode => <<"Compilation Error">>, severity => <<"critical">>,
              root_cause => <<"Syntax error or type mismatch in source">>,
              detection_method => <<"erlc exit code != 0">>,
              mitigation => <<"Fix source code, regenerate from templates if auto-generated">>,
              prevention => <<"Add pre-commit syntax checking">>},
            #{failure_mode => <<"Dependency Failure">>, severity => <<"critical">>,
              root_cause => <<"Cannot fetch or compile dependency">>,
              detection_method => <<"rebar3 get-deps fails">>,
              mitigation => <<"Check network, verify rebar.lock, update mirrors">>,
              prevention => <<"Vendor dependencies in git">>},
            #{failure_mode => <<"Timeout">>, severity => <<"major">>,
              root_cause => <<"Build exceeds SLO time budget">>,
              detection_method => <<"Duration > slo_time_budget_ms">>,
              mitigation => <<"Investigate slow compilation, add incremental builds">>,
              prevention => <<"Profile compilation, optimize hot modules">>}
        ],
        dependencies => [
            #{stage_name => <<"Source Generation">>, stage_id => <<"generate">>,
              dependency_type => <<"must-complete-before">>}
        ],
        quality_gates => [
            <<"All source files compile without errors">>,
            <<"No compilation warnings in production mode">>,
            <<"All dependencies resolved and compiled">>,
            <<"Build completes within SLO time budget">>,
            <<"Build receipt generated and validates against schema">>
        ],
        kaizen_opportunities => [
            <<"Implement incremental compilation to reduce build time">>,
            <<"Cache dependency builds across CI runs">>,
            <<"Parallelize compilation with -j flag">>
        ]
    },
    {ok, StandardWork} = erlmcp_templates:render_standard_work(Context),
    file:write_file("docs/standard-work/build-stage.md", StandardWork),
    io:format("Standard work documentation generated~n"),
    StandardWork.

%%%===================================================================
%%% Example 6: Kaizen Report
%%%===================================================================

generate_kaizen_report() ->
    Context = #{
        report_id => <<"kaizen-2026-q1">>,
        report_period_start => <<"2026-01-01T00:00:00Z">>,
        report_period_end => <<"2026-03-31T23:59:59Z">>,
        generated_at => iso8601_now(),
        generated_by => <<"mcp-kaizen-agent">>,
        metrics_source => <<"SPARQL query: sparql/quality_metrics.rq">>,
        summary => <<"
Q1 2026 showed significant improvements in lead time (-23%) and defect rate (-40%).
Automation coverage increased to 87%. Key wins: incremental builds, parallel testing,
and automated receipt generation. Focus areas for Q2: reduce P95 lead time variance
and eliminate manual smoke testing.
">>,
        lead_time_data => #{
            average_lead_time_ms => 18500,
            average_lead_time_seconds => 18.5,
            median_lead_time_ms => 16000,
            p95_lead_time_ms => 32000,
            p99_lead_time_ms => 45000
        },
        lead_time_trend => #{
            trend_points => [
                #{period => <<"2025-Q4">>, value => 24000, change_percent => 0,
                  trend_icon => <<"⚠️">>},
                #{period => <<"2026-Q1">>, value => 18500, change_percent => -23,
                  trend_icon => <<"✅">>}
            ]
        },
        lead_time_target_ms => 15000,
        lead_time_current_ms => 18500,
        lead_time_gap_ms => 3500,
        lead_time_gap_percent => 23,
        defect_rate_data => #{
            total_runs => 2400,
            failed_runs => 12,
            defect_rate_percent => 0.5,
            target_defect_rate_percent => 0.3,
            six_sigma_level => 4.2
        },
        opportunities => [
            #{
                title => <<"Eliminate manual smoke testing">>,
                category => <<"automation">>,
                priority => <<"high">>,
                effort_estimate => <<"2 weeks">>,
                impact_estimate => <<"high">>,
                description => <<"
Current smoke testing requires manual verification of /health, /pubsub, /marketplace
endpoints. This adds 10-15 minutes per release and is error-prone.
">>,
                actions => [
                    <<"Write automated smoke test suite">>,
                    <<"Integrate into CI pipeline">>,
                    <<"Add OpenTelemetry validation">>
                ],
                benefits => [
                    <<"Reduce release time by 15 minutes">>,
                    <<"Eliminate human error">>,
                    <<"Enable continuous deployment">>
                ]
            }
        ]
    },
    {ok, Report} = erlmcp_templates:render_kaizen_report(Context),
    file:write_file("reports/kaizen/2026-q1.md", Report),
    io:format("Kaizen report generated~n"),
    Report.

%%%===================================================================
%%% Generate All Examples
%%%===================================================================

generate_all_examples() ->
    filelib:ensure_dir("receipts/build/"),
    filelib:ensure_dir("receipts/andon/"),
    filelib:ensure_dir("ontology/work_orders/"),
    filelib:ensure_dir("dist/marketplace/"),
    filelib:ensure_dir("docs/standard-work/"),
    filelib:ensure_dir("reports/kaizen/"),

    io:format("~n=== Generating All TCPS Template Examples ===~n~n"),

    generate_build_receipt(),
    generate_work_order(),
    generate_andon_event(),
    generate_sku_listing(),
    generate_standard_work(),
    generate_kaizen_report(),

    io:format("~n=== All examples generated successfully ===~n").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

iso8601_now() ->
    {{Y, M, D}, {H, Min, S}} = calendar:universal_time(),
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                      [Y, M, D, H, Min, S])
    ).

iso8601_future(Days) ->
    Now = calendar:universal_time(),
    Future = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + (Days * 86400)
    ),
    {{Y, M, D}, {H, Min, S}} = Future,
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                      [Y, M, D, H, Min, S])
    ).
