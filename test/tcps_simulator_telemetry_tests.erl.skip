%%%-----------------------------------------------------------------------------
%%% @doc Tests for TCPS MCP Diataxis Simulator Telemetry
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_simulator_telemetry_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Descriptions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Telemetry Start/Stop Tests
%%------------------------------------------------------------------------------

start_stop_test() ->
    {ok, Pid} = tcps_simulator_telemetry:start_link(),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, tcps_simulator_telemetry:stop()).

start_with_config_test() ->
    Config = #{
        otlp_endpoint => "http://localhost:4318",
        prometheus_port => 9464,
        export_interval => 5000
    },
    {ok, Pid} = tcps_simulator_telemetry:start_link(Config),
    ?assert(is_pid(Pid)),
    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% Session Span Tests
%%------------------------------------------------------------------------------

session_span_lifecycle_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    %% Start session span
    SessionSpan = tcps_simulator_telemetry:start_session(#{
        session_id => <<"session-001">>,
        user_id => <<"user-123">>,
        quadrant => tutorial,
        difficulty_level => 3,
        start_time => <<"2024-01-01T00:00:00Z">>,
        estimated_duration => 3600
    }),

    ?assert(is_binary(SessionSpan)),

    %% End session span
    Result = tcps_simulator_telemetry:end_session(SessionSpan, #{
        work_orders_completed => 5,
        quality_gates_passed => 4,
        quality_gates_failed => 1,
        andon_events => 2,
        user_satisfaction => 4
    }),

    ?assertEqual(ok, Result),

    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% Work Order Span Tests
%%------------------------------------------------------------------------------

work_order_span_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    SessionSpan = tcps_simulator_telemetry:start_session(#{
        session_id => <<"session-001">>,
        user_id => <<"user-123">>
    }),

    %% Start work order span
    WOSpan = tcps_simulator_telemetry:start_work_order(#{
        work_order_id => <<"WO-001">>,
        bucket => security,
        priority => 9,
        source => cve,
        sla_target => 86400,
        assigned_to => <<"dev-team">>
    }, SessionSpan),

    ?assert(is_binary(WOSpan)),

    %% End work order span
    Result = tcps_simulator_telemetry:end_work_order(WOSpan, #{
        status => completed,
        cycle_time => 3600000,
        sla_met => true,
        quality_score => 95
    }),

    ?assertEqual(ok, Result),

    tcps_simulator_telemetry:end_session(SessionSpan, #{}),
    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% Quality Gate Span Tests
%%------------------------------------------------------------------------------

quality_gate_span_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    SessionSpan = tcps_simulator_telemetry:start_session(#{session_id => <<"s1">>}),
    WOSpan = tcps_simulator_telemetry:start_work_order(#{work_order_id => <<"wo1">>}, SessionSpan),

    %% Start quality gate span
    GateSpan = tcps_simulator_telemetry:start_quality_gate(#{
        gate_id => <<"gate-coverage">>,
        gate_type => code_coverage,
        threshold => 80,
        actual_value => 85
    }, WOSpan),

    ?assert(is_binary(GateSpan)),

    %% End quality gate span
    Result = tcps_simulator_telemetry:end_quality_gate(GateSpan, #{
        passed => true,
        remediation_required => false
    }),

    ?assertEqual(ok, Result),

    tcps_simulator_telemetry:end_work_order(WOSpan, #{}),
    tcps_simulator_telemetry:end_session(SessionSpan, #{}),
    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% Andon Event Span Tests
%%------------------------------------------------------------------------------

andon_event_span_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    SessionSpan = tcps_simulator_telemetry:start_session(#{session_id => <<"s1">>}),

    %% Start Andon event span
    AndonSpan = tcps_simulator_telemetry:start_andon_event(#{
        event_id => <<"andon-001">>,
        severity => high,
        root_cause => <<"test_failure">>,
        impact_radius => 3
    }, SessionSpan),

    ?assert(is_binary(AndonSpan)),

    %% End Andon event span
    Result = tcps_simulator_telemetry:end_andon_event(AndonSpan, #{
        resolved => true,
        resolution_time => 1200,
        escalated => false
    }),

    ?assertEqual(ok, Result),

    tcps_simulator_telemetry:end_session(SessionSpan, #{}),
    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% Metrics Recording Tests
%%------------------------------------------------------------------------------

metrics_recording_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    %% Record WIP metrics
    ok = tcps_simulator_telemetry:record_wip(security, 3),
    ok = tcps_simulator_telemetry:record_wip(reliability, 5),

    %% Record active sessions
    ok = tcps_simulator_telemetry:record_active_sessions(2),

    %% Increment counters
    ok = tcps_simulator_telemetry:increment_work_orders_created(),
    ok = tcps_simulator_telemetry:increment_quality_gates_passed(),
    ok = tcps_simulator_telemetry:increment_andon_events(),

    %% Record histograms
    ok = tcps_simulator_telemetry:record_cycle_time(3600000),
    ok = tcps_simulator_telemetry:record_session_duration(7200),
    ok = tcps_simulator_telemetry:record_mcp_tool_latency(list_tools, 50),

    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% Export Tests
%%------------------------------------------------------------------------------

export_prometheus_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    %% Record some metrics
    tcps_simulator_telemetry:increment_work_orders_created(),
    tcps_simulator_telemetry:record_wip(security, 3),
    tcps_simulator_telemetry:record_cycle_time(1000),

    %% Export Prometheus format
    {ok, Prometheus} = tcps_simulator_telemetry:export_prometheus(),
    ?assert(is_binary(Prometheus)),
    ?assert(byte_size(Prometheus) > 0),

    tcps_simulator_telemetry:stop().

export_json_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    %% Record some metrics
    tcps_simulator_telemetry:increment_work_orders_created(),
    tcps_simulator_telemetry:record_wip(security, 3),

    %% Export JSON format
    {ok, Json} = tcps_simulator_telemetry:export_json(),
    ?assert(is_map(Json)),
    ?assert(maps:is_key(metrics, Json)),

    tcps_simulator_telemetry:stop().

export_csv_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    %% Record some metrics
    tcps_simulator_telemetry:increment_work_orders_created(),
    tcps_simulator_telemetry:record_cycle_time(1000),

    %% Export CSV
    Filename = "/tmp/tcps_metrics_test.csv",
    Result = tcps_simulator_telemetry:export_csv(Filename),
    ?assertEqual(ok, Result),

    %% Verify file exists
    ?assert(filelib:is_file(Filename)),

    %% Cleanup
    file:delete(Filename),
    tcps_simulator_telemetry:stop().

metrics_summary_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    %% Record metrics
    tcps_simulator_telemetry:increment_work_orders_created(),
    tcps_simulator_telemetry:increment_quality_gates_passed(),
    tcps_simulator_telemetry:record_cycle_time(1000),

    %% Get summary
    Summary = tcps_simulator_telemetry:get_metrics_summary(),
    ?assert(is_map(Summary)),
    ?assert(maps:is_key(metrics, Summary)),

    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% Configuration Tests
%%------------------------------------------------------------------------------

configuration_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    %% Get default config
    DefaultConfig = tcps_simulator_telemetry:get_config(),
    ?assert(is_map(DefaultConfig)),

    %% Update config
    NewConfig = #{
        export_interval => 5000,
        enable_tracing => false
    },
    ok = tcps_simulator_telemetry:configure(NewConfig),

    %% Verify config updated
    UpdatedConfig = tcps_simulator_telemetry:get_config(),
    ?assertEqual(5000, maps:get(export_interval, UpdatedConfig)),

    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% Span Hierarchy Tests
%%------------------------------------------------------------------------------

span_hierarchy_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    %% Create span hierarchy
    SessionSpan = tcps_simulator_telemetry:start_session(#{session_id => <<"s1">>}),
    ScenarioSpan = tcps_simulator_telemetry:start_scenario(#{scenario_id => <<"sc1">>}, SessionSpan),
    WOSpan = tcps_simulator_telemetry:start_work_order(#{work_order_id => <<"wo1">>}, ScenarioSpan),
    GateSpan = tcps_simulator_telemetry:start_quality_gate(#{gate_id => <<"g1">>}, WOSpan),

    %% End spans in reverse order
    ok = tcps_simulator_telemetry:end_quality_gate(GateSpan, #{}),
    ok = tcps_simulator_telemetry:end_work_order(WOSpan, #{}),
    ok = tcps_simulator_telemetry:end_scenario(ScenarioSpan, #{}),
    ok = tcps_simulator_telemetry:end_session(SessionSpan, #{}),

    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% Tutorial and Navigation Span Tests
%%------------------------------------------------------------------------------

tutorial_navigation_spans_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    SessionSpan = tcps_simulator_telemetry:start_session(#{session_id => <<"s1">>}),

    %% Tutorial step span
    TutorialSpan = tcps_simulator_telemetry:start_tutorial_step(#{
        step_number => 1,
        step_type => instruction
    }, SessionSpan),

    ok = tcps_simulator_telemetry:end_tutorial_step(TutorialSpan, #{
        completion_status => completed,
        time_spent => 300,
        hints_used => 1
    }),

    %% Navigation span
    NavSpan = tcps_simulator_telemetry:start_diataxis_navigation(#{
        from_quadrant => tutorial,
        to_quadrant => how_to
    }, SessionSpan),

    ok = tcps_simulator_telemetry:end_diataxis_navigation(NavSpan, #{}),

    tcps_simulator_telemetry:end_session(SessionSpan, #{}),
    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% MCP Tool Call Span Tests
%%------------------------------------------------------------------------------

mcp_tool_call_span_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    SessionSpan = tcps_simulator_telemetry:start_session(#{session_id => <<"s1">>}),

    %% MCP tool call span
    ToolSpan = tcps_simulator_telemetry:start_mcp_tool_call(#{
        tool_name => <<"list_tools">>,
        tool_version => <<"1.0.0">>,
        parameters => #{}
    }, SessionSpan),

    ok = tcps_simulator_telemetry:end_mcp_tool_call(ToolSpan, #{
        result_size => 1024,
        latency => 50,
        cache_hit => true
    }),

    tcps_simulator_telemetry:end_session(SessionSpan, #{}),
    tcps_simulator_telemetry:stop().

%%------------------------------------------------------------------------------
%% High-Volume Metrics Tests
%%------------------------------------------------------------------------------

high_volume_metrics_test() ->
    {ok, _} = tcps_simulator_telemetry:start_link(),

    %% Record many metrics rapidly
    [tcps_simulator_telemetry:increment_work_orders_created() || _ <- lists:seq(1, 100)],
    [tcps_simulator_telemetry:record_cycle_time(rand:uniform(10000)) || _ <- lists:seq(1, 100)],

    %% Get summary
    Summary = tcps_simulator_telemetry:get_metrics_summary(),
    Metrics = maps:get(metrics, Summary),
    ?assertEqual(100, maps:get(work_orders_created, Metrics)),

    tcps_simulator_telemetry:stop().
