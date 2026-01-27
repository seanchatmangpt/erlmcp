#!/usr/bin/env escript
%%% -*- erlang -*-
%%%-----------------------------------------------------------------------------
%%% @doc TCPS MCP Diataxis Simulator - Telemetry Demo
%%%
%%% Demonstrates the complete telemetry and visualization system for the
%%% TCPS MCP Diataxis simulator.
%%%
%%% Usage:
%%% ```
%%% ./examples/tcps_telemetry_demo.erl
%%% '''
%%%
%%% @end
%%%-----------------------------------------------------------------------------

main(_Args) ->
    io:format("~n=== TCPS MCP Diataxis Simulator - Telemetry Demo ===~n~n"),

    %% Start telemetry systems
    io:format("Starting telemetry systems...~n"),
    {ok, TelemetryPid} = tcps_simulator_telemetry:start_link(#{
        otlp_endpoint => "http://localhost:4318",
        prometheus_port => 9464,
        export_interval => 10000,
        enable_tracing => true,
        enable_metrics => true,
        sample_rate => 1.0
    }),
    io:format("  ✓ Telemetry started (PID: ~p)~n", [TelemetryPid]),

    {ok, CollectorPid} = tcps_metrics_collector:start_link(#{
        aggregation_interval => 1000,
        retention_period => 86400000,
        enable_anomaly_detection => true,
        anomaly_threshold => 2.0
    }),
    io:format("  ✓ Metrics collector started (PID: ~p)~n~n", [CollectorPid]),

    %% Simulate learning session
    io:format("Simulating learning session...~n"),

    %% Start session
    SessionSpan = tcps_simulator_telemetry:start_session(#{
        session_id => <<"session-demo-001">>,
        user_id => <<"demo-user">>,
        quadrant => tutorial,
        difficulty_level => 3,
        start_time => iso8601:format(erlang:timestamp()),
        estimated_duration => 3600
    }),
    tcps_metrics_collector:record_session_start(#{
        session_id => <<"session-demo-001">>,
        timestamp => erlang:system_time(millisecond)
    }),
    io:format("  ✓ Session started~n"),

    %% Start scenario
    ScenarioSpan = tcps_simulator_telemetry:start_scenario(#{
        scenario_id => <<"scenario-security-basics">>,
        scenario_type => <<"CVE remediation">>,
        work_orders_count => 3,
        complexity => 5,
        expected_outcomes => [<<"CVE assessment">>, <<"Fix implementation">>, <<"Testing">>]
    }, SessionSpan),
    io:format("  ✓ Scenario started~n"),

    %% Simulate work orders
    WorkOrderResults = lists:map(fun(N) ->
        simulate_work_order(N, ScenarioSpan)
    end, lists:seq(1, 3)),
    io:format("  ✓ Completed 3 work orders~n"),

    %% End scenario
    tcps_simulator_telemetry:end_scenario(ScenarioSpan, #{
        work_orders_completed => 3,
        total_cycle_time => 5400000,
        quality_score => 92
    }),
    io:format("  ✓ Scenario completed~n"),

    %% Simulate Andon event
    AndonSpan = tcps_simulator_telemetry:start_andon_event(#{
        event_id => <<"andon-test-failure">>,
        severity => medium,
        root_cause => <<"Test environment issue">>,
        impact_radius => 1
    }, SessionSpan),
    tcps_metrics_collector:record_andon_event(#{
        event_id => <<"andon-test-failure">>,
        severity => medium,
        root_cause => <<"Test environment issue">>,
        impact_radius => 1,
        timestamp => erlang:system_time(millisecond)
    }),
    timer:sleep(100),
    tcps_simulator_telemetry:end_andon_event(AndonSpan, #{
        resolved => true,
        resolution_time => 300,
        escalated => false,
        lessons_learned => [<<"Update test environment setup guide">>]
    }),
    tcps_metrics_collector:record_andon_resolution(#{
        event_id => <<"andon-test-failure">>,
        resolution_time => 300000,
        escalated => false,
        timestamp => erlang:system_time(millisecond)
    }),
    io:format("  ✓ Andon event resolved~n"),

    %% Simulate navigation
    NavSpan = tcps_simulator_telemetry:start_diataxis_navigation(#{
        from_quadrant => tutorial,
        to_quadrant => reference,
        reason => <<"Looking up API reference">>,
        documentation_accessed => [<<"tcps_quality_gates API">>]
    }, SessionSpan),
    tcps_metrics_collector:record_diataxis_navigation(#{
        from_quadrant => tutorial,
        to_quadrant => reference,
        reason => <<"Looking up API reference">>,
        timestamp => erlang:system_time(millisecond)
    }),
    tcps_simulator_telemetry:end_diataxis_navigation(NavSpan, #{}),
    io:format("  ✓ Navigation tracked~n"),

    %% End session
    tcps_simulator_telemetry:end_session(SessionSpan, #{
        work_orders_completed => 3,
        quality_gates_passed => 9,
        quality_gates_failed => 3,
        andon_events => 1,
        learning_outcomes_achieved => [<<"CVE assessment">>, <<"Fix implementation">>],
        user_satisfaction => 4
    }),
    tcps_metrics_collector:record_session_complete(#{
        session_id => <<"session-demo-001">>,
        quadrant => tutorial,
        duration => 3600000,
        work_orders_completed => 3,
        user_satisfaction => 4,
        timestamp => erlang:system_time(millisecond)
    }),
    io:format("  ✓ Session completed~n~n"),

    %% Generate visualizations
    io:format("Generating visualizations...~n"),

    KanbanHeatmap = tcps_visualization_data:kanban_heatmap(#{
        window => '1hour',
        buckets => [security, reliability, cost, compliance],
        format => chartjs
    }),
    io:format("  ✓ Kanban heatmap: ~p datasets~n", [length(maps:get(datasets, KanbanHeatmap))]),

    QualityFunnel = tcps_visualization_data:quality_gate_funnel(#{
        gates => [code_coverage, test_pass_rate, security_scan],
        format => chartjs
    }),
    io:format("  ✓ Quality gate funnel: ~p~n", [maps:is_key(labels, QualityFunnel)]),

    AndonTimeline = tcps_visualization_data:andon_timeline(#{
        window => '1day',
        severity_filter => [medium, high, critical],
        format => d3
    }),
    io:format("  ✓ Andon timeline: ~p events~n", [length(AndonTimeline)]),

    Dashboard = tcps_visualization_data:learning_dashboard(#{
        session_id => <<"session-demo-001">>,
        format => chartjs
    }),
    io:format("  ✓ Learning dashboard: ~p sections~n", [maps:size(Dashboard)]),

    ToolAnalytics = tcps_visualization_data:tool_usage_analytics(#{
        tools => [list_tools, get_prompt, call_tool],
        window => '1hour',
        format => chartjs
    }),
    io:format("  ✓ Tool analytics: ~p metrics~n~n", [maps:size(ToolAnalytics)]),

    %% Export metrics
    io:format("Exporting metrics...~n"),

    {ok, Prometheus} = tcps_simulator_telemetry:export_prometheus(),
    io:format("  ✓ Prometheus: ~p bytes~n", [byte_size(Prometheus)]),

    {ok, Json} = tcps_simulator_telemetry:export_json(),
    io:format("  ✓ JSON: ~p keys~n", [maps:size(Json)]),

    CsvFile = "/tmp/tcps_telemetry_demo.csv",
    ok = tcps_simulator_telemetry:export_csv(CsvFile),
    {ok, CsvInfo} = file:read_file_info(CsvFile),
    io:format("  ✓ CSV: ~p bytes (~s)~n", [element(2, CsvInfo), CsvFile]),

    %% Get summary
    Summary = tcps_metrics_collector:get_summary(),
    io:format("~nMetrics Summary:~n"),
    io:format("  Total metrics: ~p~n", [maps:get(total_metrics, Summary)]),
    io:format("  Status: ~p~n~n", [maps:get(status, Summary)]),

    %% Display sample Prometheus output
    io:format("Sample Prometheus Metrics:~n"),
    PrometheusLines = binary:split(Prometheus, <<"\n">>, [global]),
    lists:foreach(fun(Line) ->
        case Line of
            <<>> -> ok;
            _ -> io:format("  ~s~n", [Line])
        end
    end, lists:sublist(PrometheusLines, 10)),
    io:format("  ... (~p total lines)~n~n", [length(PrometheusLines)]),

    %% Cleanup
    io:format("Cleaning up...~n"),
    tcps_simulator_telemetry:stop(),
    tcps_metrics_collector:stop(),
    io:format("  ✓ Systems stopped~n~n"),

    io:format("=== Demo Complete ===~n~n"),
    io:format("For full documentation, see: docs/TCPS_SIMULATOR_TELEMETRY.md~n~n"),

    halt(0).

%%------------------------------------------------------------------------------
%% Internal Functions
%%------------------------------------------------------------------------------

simulate_work_order(N, ParentSpan) ->
    Bucket = lists:nth(((N - 1) rem 4) + 1, [security, reliability, cost, compliance]),
    WorkOrderId = iolist_to_binary(io_lib:format("WO-~3..0B", [N])),

    %% Start work order
    WOSpan = tcps_simulator_telemetry:start_work_order(#{
        work_order_id => WorkOrderId,
        bucket => Bucket,
        priority => 7 + N,
        source => cve,
        sla_target => 86400,
        assigned_to => <<"demo-team">>
    }, ParentSpan),

    tcps_metrics_collector:record_work_order_created(#{
        work_order_id => WorkOrderId,
        bucket => Bucket,
        priority => 7 + N,
        source => cve,
        timestamp => erlang:system_time(millisecond)
    }),

    %% Update WIP
    tcps_simulator_telemetry:record_wip(Bucket, N),

    %% Simulate quality gates
    simulate_quality_gates(WOSpan),

    %% Simulate MCP tool calls
    simulate_mcp_tools(WOSpan),

    %% Complete work order
    CycleTime = (1500 + (N * 300)) * 1000,
    tcps_simulator_telemetry:end_work_order(WOSpan, #{
        status => completed,
        cycle_time => CycleTime,
        sla_met => true,
        quality_score => 90 + N
    }),

    tcps_metrics_collector:record_work_order_completed(#{
        work_order_id => WorkOrderId,
        bucket => Bucket,
        cycle_time => CycleTime,
        sla_met => true,
        quality_score => 90 + N,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_simulator_telemetry:record_cycle_time(CycleTime),

    #{work_order_id => WorkOrderId, bucket => Bucket, cycle_time => CycleTime}.

simulate_quality_gates(ParentSpan) ->
    Gates = [
        {code_coverage, 80, 85},
        {test_pass_rate, 95, 98},
        {security_scan, 100, 100},
        {performance, 90, 92}
    ],

    lists:foreach(fun({GateType, Threshold, ActualValue}) ->
        GateId = iolist_to_binary(io_lib:format("gate-~s", [atom_to_list(GateType)])),
        GateSpan = tcps_simulator_telemetry:start_quality_gate(#{
            gate_id => GateId,
            gate_type => GateType,
            threshold => Threshold,
            actual_value => ActualValue
        }, ParentSpan),

        Passed = ActualValue >= Threshold,

        tcps_simulator_telemetry:end_quality_gate(GateSpan, #{
            passed => Passed,
            remediation_required => not Passed
        }),

        tcps_metrics_collector:record_quality_gate_result(#{
            gate_id => GateId,
            gate_type => GateType,
            passed => Passed,
            score => ActualValue,
            threshold => Threshold,
            actual_value => ActualValue,
            timestamp => erlang:system_time(millisecond)
        }),

        if
            Passed ->
                tcps_simulator_telemetry:increment_quality_gates_passed(),
                tcps_simulator_telemetry:record_quality_gate_score(ActualValue);
            true ->
                tcps_simulator_telemetry:increment_quality_gates_failed()
        end
    end, Gates).

simulate_mcp_tools(ParentSpan) ->
    Tools = [
        {<<"list_tools">>, <<"1.0.0">>, 45, 512},
        {<<"get_prompt">>, <<"1.0.0">>, 120, 2048},
        {<<"call_tool">>, <<"1.0.0">>, 350, 4096}
    ],

    lists:foreach(fun({ToolName, Version, Latency, ResultSize}) ->
        ToolSpan = tcps_simulator_telemetry:start_mcp_tool_call(#{
            tool_name => ToolName,
            tool_version => Version,
            parameters => #{},
            caller_context => <<"work_order_execution">>
        }, ParentSpan),

        CacheHit = rand:uniform() > 0.3,

        tcps_simulator_telemetry:end_mcp_tool_call(ToolSpan, #{
            result_size => ResultSize,
            latency => Latency,
            cache_hit => CacheHit
        }),

        tcps_metrics_collector:record_tool_call(#{
            tool_name => binary_to_atom(ToolName),
            tool_version => Version,
            latency => Latency,
            result_size => ResultSize,
            cache_hit => CacheHit,
            timestamp => erlang:system_time(millisecond)
        }),

        tcps_simulator_telemetry:increment_mcp_tool_calls(),
        tcps_simulator_telemetry:record_mcp_tool_latency(binary_to_atom(ToolName), Latency)
    end, Tools).
