%%%-----------------------------------------------------------------------------
%%% @doc Tests for TCPS Metrics Collector
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_metrics_collector_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Descriptions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Start/Stop Tests
%%------------------------------------------------------------------------------

start_stop_test() ->
    {ok, Pid} = tcps_metrics_collector:start_link(),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, tcps_metrics_collector:stop()).

start_with_config_test() ->
    Config = #{
        aggregation_interval => 2000,
        retention_period => 3600000,
        enable_anomaly_detection => true
    },
    {ok, Pid} = tcps_metrics_collector:start_link(Config),
    ?assert(is_pid(Pid)),
    tcps_metrics_collector:stop().

%%------------------------------------------------------------------------------
%% Work Order Metrics Tests
%%------------------------------------------------------------------------------

record_work_order_created_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    ok = tcps_metrics_collector:record_work_order_created(#{
        work_order_id => <<"WO-001">>,
        bucket => security,
        priority => 9,
        source => cve,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_metrics_collector:stop().

record_work_order_completed_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    ok = tcps_metrics_collector:record_work_order_completed(#{
        work_order_id => <<"WO-001">>,
        bucket => security,
        cycle_time => 3600000,
        sla_met => true,
        quality_score => 95,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_metrics_collector:stop().

get_work_order_metrics_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record some work orders
    tcps_metrics_collector:record_work_order_created(#{
        bucket => security,
        timestamp => erlang:system_time(millisecond)
    }),
    tcps_metrics_collector:record_work_order_completed(#{
        bucket => security,
        timestamp => erlang:system_time(millisecond)
    }),

    %% Get metrics
    Metrics = tcps_metrics_collector:get_work_order_metrics(#{
        window => '5min',
        buckets => [security, reliability]
    }),

    ?assert(is_map(Metrics)),

    tcps_metrics_collector:stop().

%%------------------------------------------------------------------------------
%% Quality Gate Metrics Tests
%%------------------------------------------------------------------------------

record_quality_gate_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    ok = tcps_metrics_collector:record_quality_gate_result(#{
        gate_id => <<"gate-coverage">>,
        gate_type => code_coverage,
        passed => true,
        score => 85,
        threshold => 80,
        actual_value => 85,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_metrics_collector:stop().

get_quality_gate_metrics_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record quality gates
    tcps_metrics_collector:record_quality_gate_result(#{
        gate_type => code_coverage,
        passed => true,
        timestamp => erlang:system_time(millisecond)
    }),

    %% Get metrics
    Metrics = tcps_metrics_collector:get_quality_gate_metrics(#{
        window => '5min'
    }),

    ?assert(is_map(Metrics)),

    tcps_metrics_collector:stop().

%%------------------------------------------------------------------------------
%% Andon Event Metrics Tests
%%------------------------------------------------------------------------------

record_andon_event_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    ok = tcps_metrics_collector:record_andon_event(#{
        event_id => <<"andon-001">>,
        severity => high,
        root_cause => <<"test_failure">>,
        impact_radius => 3,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_metrics_collector:stop().

record_andon_resolution_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    ok = tcps_metrics_collector:record_andon_resolution(#{
        event_id => <<"andon-001">>,
        resolution_time => 1200000,
        escalated => false,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_metrics_collector:stop().

get_andon_metrics_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record events
    tcps_metrics_collector:record_andon_event(#{
        severity => high,
        timestamp => erlang:system_time(millisecond)
    }),

    %% Get metrics
    Metrics = tcps_metrics_collector:get_andon_metrics(#{
        window => '1day'
    }),

    ?assert(is_map(Metrics)),

    tcps_metrics_collector:stop().

%%------------------------------------------------------------------------------
%% Learning Metrics Tests
%%------------------------------------------------------------------------------

record_session_lifecycle_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    SessionId = <<"session-001">>,

    %% Record session start
    ok = tcps_metrics_collector:record_session_start(#{
        session_id => SessionId,
        timestamp => erlang:system_time(millisecond)
    }),

    %% Record session completion
    ok = tcps_metrics_collector:record_session_complete(#{
        session_id => SessionId,
        quadrant => tutorial,
        duration => 3600000,
        work_orders_completed => 5,
        user_satisfaction => 4,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_metrics_collector:stop().

record_tutorial_step_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    ok = tcps_metrics_collector:record_tutorial_step(#{
        step_number => 1,
        step_type => instruction,
        completion_status => completed,
        time_spent => 300000,
        hints_used => 1,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_metrics_collector:stop().

record_diataxis_navigation_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    ok = tcps_metrics_collector:record_diataxis_navigation(#{
        from_quadrant => tutorial,
        to_quadrant => how_to,
        reason => <<"seeking_specific_task">>,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_metrics_collector:stop().

get_learning_metrics_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record learning events
    tcps_metrics_collector:record_session_start(#{
        session_id => <<"s1">>,
        timestamp => erlang:system_time(millisecond)
    }),

    %% Get metrics
    Metrics = tcps_metrics_collector:get_learning_metrics(#{
        window => '1day'
    }),

    ?assert(is_map(Metrics)),

    tcps_metrics_collector:stop().

%%------------------------------------------------------------------------------
%% MCP Tool Metrics Tests
%%------------------------------------------------------------------------------

record_tool_call_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    ok = tcps_metrics_collector:record_tool_call(#{
        tool_name => <<"list_tools">>,
        tool_version => <<"1.0.0">>,
        latency => 50,
        result_size => 1024,
        cache_hit => true,
        timestamp => erlang:system_time(millisecond)
    }),

    tcps_metrics_collector:stop().

get_tool_metrics_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record tool calls
    tcps_metrics_collector:record_tool_call(#{
        tool_name => <<"list_tools">>,
        latency => 50,
        timestamp => erlang:system_time(millisecond)
    }),

    %% Get metrics
    Metrics = tcps_metrics_collector:get_tool_metrics(#{
        window => '1hour'
    }),

    ?assert(is_map(Metrics)),

    tcps_metrics_collector:stop().

%%------------------------------------------------------------------------------
%% Aggregation Tests
%%------------------------------------------------------------------------------

get_all_metrics_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record various metrics
    tcps_metrics_collector:record_work_order_created(#{
        bucket => security,
        timestamp => erlang:system_time(millisecond)
    }),
    tcps_metrics_collector:record_quality_gate_result(#{
        gate_type => code_coverage,
        passed => true,
        timestamp => erlang:system_time(millisecond)
    }),

    %% Get all metrics
    AllMetrics = tcps_metrics_collector:get_metrics(#{
        window => '5min',
        include_trends => true
    }),

    ?assert(is_map(AllMetrics)),

    tcps_metrics_collector:stop().

get_trends_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    Trends = tcps_metrics_collector:get_trends(#{
        metric_type => work_order,
        window => '1hour',
        trend_type => linear
    }),

    ?assert(is_map(Trends)),

    tcps_metrics_collector:stop().

detect_anomalies_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    Anomalies = tcps_metrics_collector:detect_anomalies(#{
        metric => work_order_creation_rate,
        threshold => 2.0,
        window => '1hour'
    }),

    ?assert(is_list(Anomalies)),

    tcps_metrics_collector:stop().

get_summary_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record some metrics
    tcps_metrics_collector:record_work_order_created(#{
        bucket => security,
        timestamp => erlang:system_time(millisecond)
    }),

    Summary = tcps_metrics_collector:get_summary(),
    ?assert(is_map(Summary)),
    ?assert(maps:is_key(total_metrics, Summary)),

    tcps_metrics_collector:stop().

reset_metrics_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record metrics
    tcps_metrics_collector:record_work_order_created(#{
        bucket => security,
        timestamp => erlang:system_time(millisecond)
    }),

    %% Reset
    ok = tcps_metrics_collector:reset_metrics(),

    %% Verify reset
    Summary = tcps_metrics_collector:get_summary(),
    ?assertEqual(0, maps:get(total_metrics, Summary)),

    tcps_metrics_collector:stop().

%%------------------------------------------------------------------------------
%% Performance Tests
%%------------------------------------------------------------------------------

high_volume_recording_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record 1000 metrics
    Start = erlang:monotonic_time(millisecond),
    [tcps_metrics_collector:record_work_order_created(#{
        bucket => security,
        timestamp => erlang:system_time(millisecond)
    }) || _ <- lists:seq(1, 1000)],
    End = erlang:monotonic_time(millisecond),

    Duration = End - Start,
    ?assert(Duration < 1000),  % Should complete in less than 1 second

    tcps_metrics_collector:stop().

concurrent_recording_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Spawn multiple processes recording metrics
    Pids = [spawn(fun() ->
        [tcps_metrics_collector:record_work_order_created(#{
            bucket => security,
            timestamp => erlang:system_time(millisecond)
        }) || _ <- lists:seq(1, 100)]
    end) || _ <- lists:seq(1, 10)],

    %% Wait for all processes
    [receive after 100 -> ok end || _ <- Pids],

    Summary = tcps_metrics_collector:get_summary(),
    ?assert(maps:get(total_metrics, Summary) >= 1000),

    tcps_metrics_collector:stop().

%%------------------------------------------------------------------------------
%% Window Tests
%%------------------------------------------------------------------------------

window_aggregation_test() ->
    {ok, _} = tcps_metrics_collector:start_link(),

    %% Record metrics at different times
    Now = erlang:system_time(millisecond),
    tcps_metrics_collector:record_work_order_created(#{
        bucket => security,
        timestamp => Now
    }),
    tcps_metrics_collector:record_work_order_created(#{
        bucket => security,
        timestamp => Now - 120000  % 2 minutes ago
    }),
    tcps_metrics_collector:record_work_order_created(#{
        bucket => security,
        timestamp => Now - 600000  % 10 minutes ago
    }),

    %% Get 1-minute window
    Metrics1Min = tcps_metrics_collector:get_work_order_metrics(#{
        window => '1min',
        buckets => [security]
    }),

    %% Get 5-minute window
    Metrics5Min = tcps_metrics_collector:get_work_order_metrics(#{
        window => '5min',
        buckets => [security]
    }),

    ?assert(is_map(Metrics1Min)),
    ?assert(is_map(Metrics5Min)),

    tcps_metrics_collector:stop().
