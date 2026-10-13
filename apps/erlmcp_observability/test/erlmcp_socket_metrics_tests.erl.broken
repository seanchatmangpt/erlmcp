-module(erlmcp_socket_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test fixtures
-define(TEST_TRANSPORT_ID, test_transport).
-define(TEST_TRANSPORT_ID_2, test_transport_2).

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test starting the socket metrics server
start_link_test() ->
    {ok, Pid} = ?assertMatch({ok, _}, erlmcp_socket_metrics:start_link()),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(erlmcp_socket_metrics)),
    %% Clean up
    gen_server:stop(Pid).

%% @doc Test recording buffer usage metrics
record_buffer_usage_test() ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    BufferData = #{
        rcvbuf_used => 2048,
        sndbuf_used => 4096,
        rcvbuf_size => 8192,
        sndbuf_size => 8192
    },

    ?assertEqual(ok,
        erlmcp_socket_metrics:record_buffer_usage(?TEST_TRANSPORT_ID, BufferData)),

    %% Verify metrics were recorded
    {ok, Metrics} = erlmcp_socket_metrics:get_socket_metrics(),
    BufferMetrics = maps:get(buffer_metrics, Metrics, []),
    ?assert(length(BufferMetrics) > 0),

    %% Find our transport's metrics
    [Metric | _] = [M || M <- BufferMetrics,
                         M#erlmcp_socket_metrics.buffer_metrics.transport_id =:= ?TEST_TRANSPORT_ID],
    ?assertEqual(2048, Metric#erlmcp_socket_metrics.buffer_metrics.rcvbuf_used),
    ?assertEqual(4096, Metric#erlmcp_socket_metrics.buffer_metrics.sndbuf_used),

    %% Check utilization calculations
    RcvUtil = Metric#erlmcp_socket_metrics.buffer_metrics.rcvbuf_utilization,
    SndUtil = Metric#erlmcp_socket_metrics.buffer_metrics.sndbuf_utilization,
    ?assert(RcvUtil > 0),
    ?assert(SndUtil > 0),

    gen_server:stop(erlmcp_socket_metrics).

%% @doc Test recording packet drops
record_packet_drop_test() ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    ?assertEqual(ok,
        erlmcp_socket_metrics:record_packet_drop(?TEST_TRANSPORT_ID, buffer_overflow)),

    %% Verify packet drop was recorded
    {ok, Metrics} = erlmcp_socket_metrics:get_socket_metrics(?TEST_TRANSPORT_ID),
    PacketDrops = maps:get(packet_drops, Metrics, []),
    ?assert(length(PacketDrops) > 0),

    [Drop | _] = PacketDrops,
    ?assertEqual(?TEST_TRANSPORT_ID,
        Drop#erlmcp_socket_metrics.packet_drops.transport_id),
    ?assertEqual(buffer_overflow,
        Drop#erlmcp_socket_metrics.packet_drops.reason),

    gen_server:stop(erlmcp_socket_metrics).

%% @doc Test recording backpressure events
record_backpressure_event_test() ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    EventData = #{
        type => activated,
        buffer_size => 7000,
        threshold => 8192,
        duration_ms => undefined
    },

    ?assertEqual(ok,
        erlmcp_socket_metrics:record_backpressure_event(?TEST_TRANSPORT_ID, EventData)),

    %% Verify backpressure event was recorded
    {ok, Metrics} = erlmcp_socket_metrics:get_socket_metrics(?TEST_TRANSPORT_ID),
    BackpressureEvents = maps:get(backpressure_events, Metrics, []),
    ?assert(length(BackpressureEvents) > 0),

    [Event | _] = BackpressureEvents,
    ?assertEqual(?TEST_TRANSPORT_ID,
        Event#erlmcp_socket_metrics.backpressure_event.transport_id),
    ?assertEqual(activated,
        Event#erlmcp_socket_metrics.backpressure_event.event_type),
    ?assertEqual(7000,
        Event#erlmcp_socket_metrics.backpressure_event.buffer_size),

    %% Verify active backpressure tracking
    ActiveBackpressure = maps:get(active_backpressure, Metrics, undefined),
    ?assertNotEqual(undefined, ActiveBackpressure),

    gen_server:stop(erlmcp_socket_metrics).

%% @doc Test recording socket statistics
record_socket_stats_test() ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    Stats = #{
        bytes_sent => 1024,
        bytes_received => 2048,
        packets_sent => 10,
        packets_received => 20,
        read_count => 15,
        write_count => 12
    },

    ?assertEqual(ok,
        erlmcp_socket_metrics:record_socket_stats(?TEST_TRANSPORT_ID, Stats)),

    %% Verify stats were recorded
    {ok, Metrics} = erlmcp_socket_metrics:get_socket_metrics(?TEST_TRANSPORT_ID),
    SocketStats = maps:get(socket_stats, Metrics, []),
    ?assert(length(SocketStats) > 0),

    [Stat | _] = SocketStats,
    ?assertEqual(?TEST_TRANSPORT_ID,
        Stat#erlmcp_socket_metrics.socket_stats.transport_id),
    ?assertEqual(1024,
        Stat#erlmcp_socket_metrics.socket_stats.bytes_sent),
    ?assertEqual(2048,
        Stat#erlmcp_socket_stats.socket_stats.bytes_received),

    gen_server:stop(erlmcp_socket_metrics).

%% @doc Test getting metrics for specific transport
get_metrics_for_transport_test() ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    %% Record metrics for two different transports
    BufferData = #{
        rcvbuf_used => 2048,
        sndbuf_used => 4096,
        rcvbuf_size => 8192,
        sndbuf_size => 8192
    },

    erlmcp_socket_metrics:record_buffer_usage(?TEST_TRANSPORT_ID, BufferData),
    erlmcp_socket_metrics:record_buffer_usage(?TEST_TRANSPORT_ID_2, BufferData),

    %% Get metrics for first transport
    {ok, Metrics1} = erlmcp_socket_metrics:get_socket_metrics(?TEST_TRANSPORT_ID),
    BufferMetrics1 = maps:get(buffer_metrics, Metrics1, []),
    ?assert(length(BufferMetrics1) > 0),

    %% Get metrics for second transport
    {ok, Metrics2} = erlmcp_socket_metrics:get_socket_metrics(?TEST_TRANSPORT_ID_2),
    BufferMetrics2 = maps:get(buffer_metrics, Metrics2, []),
    ?assert(length(BufferMetrics2) > 0),

    %% Verify they are separate
    [M1 | _] = BufferMetrics1,
    [M2 | _] = BufferMetrics2,
    ?assertEqual(?TEST_TRANSPORT_ID, M1#erlmcp_socket_metrics.buffer_metrics.transport_id),
    ?assertEqual(?TEST_TRANSPORT_ID_2, M2#erlmcp_socket_metrics.buffer_metrics.transport_id),

    gen_server:stop(erlmcp_socket_metrics).

%% @doc Test resetting metrics
reset_metrics_test() ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    %% Record some metrics
    BufferData = #{
        rcvbuf_used => 2048,
        sndbuf_used => 4096,
        rcvbuf_size => 8192,
        sndbuf_size => 8192
    },

    erlmcp_socket_metrics:record_buffer_usage(?TEST_TRANSPORT_ID, BufferData),
    erlmcp_socket_metrics:record_packet_drop(?TEST_TRANSPORT_ID, overflow),

    %% Verify metrics exist
    {ok, MetricsBefore} = erlmcp_socket_metrics:get_socket_metrics(),
    BufferMetricsBefore = maps:get(buffer_metrics, MetricsBefore, []),
    ?assert(length(BufferMetricsBefore) > 0),

    %% Reset metrics
    ?assertEqual(ok, erlmcp_socket_metrics:reset_socket_metrics()),

    %% Verify metrics were cleared
    {ok, MetricsAfter} = erlmcp_socket_metrics:get_socket_metrics(),
    BufferMetricsAfter = maps:get(buffer_metrics, MetricsAfter, []),
    ?assertEqual(0, length(BufferMetricsAfter)),

    gen_server:stop(erlmcp_socket_metrics).

%% @doc Test buffer summary calculation
get_buffer_summary_test() ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    %% Record buffer metrics for multiple transports
    BufferData1 = #{
        rcvbuf_used => 7000,  % High utilization (85%)
        sndbuf_used => 2048,
        rcvbuf_size => 8192,
        sndbuf_size => 8192
    },

    BufferData2 = #{
        rcvbuf_used => 2048,
        sndbuf_used => 2048,
        rcvbuf_size => 8192,
        sndbuf_size => 8192
    },

    erlmcp_socket_metrics:record_buffer_usage(?TEST_TRANSPORT_ID, BufferData1),
    erlmcp_socket_metrics:record_buffer_usage(?TEST_TRANSPORT_ID_2, BufferData2),

    %% Get buffer summary
    Summary = erlmcp_socket_metrics:get_buffer_summary(),

    %% Verify summary fields
    TotalTransports = maps:get(total_transports, Summary, 0),
    ?assert(TotalTransports >= 2),

    AvgRcvUtil = maps:get(avg_rcvbuf_utilization, Summary, 0.0),
    AvgSndUtil = maps:get(avg_sndbuf_utilization, Summary, 0.0),
    ?assert(AvgRcvUtil > 0),
    ?assert(AvgSndUtil > 0),

    %% Verify over-threshold detection (transport 1 is over 80%)
    OverThreshold = maps:get(transports_over_threshold, Summary, []),
    ?assert(lists:member(?TEST_TRANSPORT_ID, OverThreshold)),

    gen_server:stop(erlmcp_socket_metrics).

%% @doc Test backpressure lifecycle (activate -> deactivate)
backpressure_lifecycle_test() ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    %% Activate backpressure
    ActivateEvent = #{
        type => activated,
        buffer_size => 7000,
        threshold => 8192,
        duration_ms => undefined
    },
    erlmcp_socket_metrics:record_backpressure_event(?TEST_TRANSPORT_ID, ActivateEvent),

    {ok, Metrics1} = erlmcp_socket_metrics:get_socket_metrics(?TEST_TRANSPORT_ID),
    Active1 = maps:get(active_backpressure, Metrics1, undefined),
    ?assertNotEqual(undefined, Active1),

    %% Deactivate backpressure
    DeactivateEvent = #{
        type => deactivated,
        buffer_size => 2000,
        threshold => 8192,
        duration_ms => 5000
    },
    erlmcp_socket_metrics:record_backpressure_event(?TEST_TRANSPORT_ID, DeactivateEvent),

    {ok, Metrics2} = erlmcp_socket_metrics:get_socket_metrics(?TEST_TRANSPORT_ID),
    Active2 = maps:get(active_backpressure, Metrics2, undefined),
    ?assertEqual(undefined, Active2),

    %% Verify duration was recorded
    BackpressureEvents = maps:get(backpressure_events, Metrics2, []),
    DeactivateRec = lists:keyfind(deactivated,
        #erlmcp_socket_metrics.backpressure_event.event_type, BackpressureEvents),
    ?assertNotEqual(false, DeactivateRec),
    ?assertEqual(5000,
        DeactivateRec#erlmcp_socket_metrics.backpressure_event.duration_ms),

    gen_server:stop(erlmcp_socket_metrics).

%%====================================================================
%% Integration Tests
%%====================================================================

%% @doc Test full metrics collection lifecycle
metrics_lifecycle_test() ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    %% Simulate transport operations
    BufferData = #{
        rcvbuf_used => 2048,
        sndbuf_used => 3072,
        rcvbuf_size => 8192,
        sndbuf_size => 8192
    },

    SocketStats = #{
        bytes_sent => 512,
        bytes_received => 1024,
        packets_sent => 5,
        packets_received => 10,
        read_count => 8,
        write_count => 6
    },

    %% Record multiple metrics
    lists:foreach(fun(_) ->
        erlmcp_socket_metrics:record_buffer_usage(?TEST_TRANSPORT_ID, BufferData),
        erlmcp_socket_metrics:record_socket_stats(?TEST_TRANSPORT_ID, SocketStats)
    end, lists:seq(1, 10)),

    %% Get all metrics
    {ok, AllMetrics} = erlmcp_socket_metrics:get_socket_metrics(),

    %% Verify all metric types have data
    BufferMetrics = maps:get(buffer_metrics, AllMetrics, []),
    SocketStatsList = maps:get(socket_stats, AllMetrics, []),

    ?assert(length(BufferMetrics) >= 10),
    ?assert(length(SocketStatsList) >= 10),

    %% Get buffer summary
    Summary = erlmcp_socket_metrics:get_buffer_summary(),
    TotalTransports = maps:get(total_transports, Summary, 0),
    ?assert(TotalTransports >= 1),

    %% Reset
    erlmcp_socket_metrics:reset_socket_metrics(),
    {ok, ResetMetrics} = erlmcp_socket_metrics:get_socket_metrics(),
    ?assertEqual(0, length(maps:get(buffer_metrics, ResetMetrics, []))),

    gen_server:stop(erlmcp_socket_metrics).

%%====================================================================
%% Performance Benchmarks
%%====================================================================

%% @doc Benchmark metrics recording performance
metrics_recording_benchmark_test(_Config) ->
    {ok, _Pid} = erlmcp_socket_metrics:start_link(),

    Iterations = 1000,
    BufferData = #{
        rcvbuf_used => 2048,
        sndbuf_used => 4096,
        rcvbuf_size => 8192,
        sndbuf_size => 8192
    },

    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_) ->
        erlmcp_socket_metrics:record_buffer_usage(?TEST_TRANSPORT_ID, BufferData)
    end, lists:seq(1, Iterations)),

    EndTime = erlang:monotonic_time(microsecond),
    DurationMs = (EndTime - StartTime) / 1000.0,
    AvgPerOp = DurationMs / Iterations,

    ?debugFmt("Metrics recording benchmark: ~p operations in ~.2fms (~.4fms/op)",
             [Iterations, DurationMs, AvgPerOp]),
    ?assert(AvgPerOp < 1.0),  % Should be under 1ms per operation

    gen_server:stop(erlmcp_socket_metrics).

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Start any required processes
    ok.

cleanup(_Ctx) ->
    %% Clean up any resources
    case whereis(erlmcp_socket_metrics) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.
