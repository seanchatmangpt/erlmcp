%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for erlmcp_cli_metrics module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test metrics system initialization
metrics_init_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Verify process is alive
    ?assert(is_process_alive(Pid)),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc Test counter metrics
counter_metrics_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Initialize a counter
    erlmcp_cli_metrics:register_counter(<<"test.counter">>, <<"Test counter">>),

    %% Increment counter
    erlmcp_cli_metrics:increment_counter(<<"test.counter">>),
    erlmcp_cli_metrics:increment_counter(<<"test.counter">>, 5),

    %% Get counter value
    Value = erlmcp_cli_metrics:get_counter_value(<<"test.counter">>),
    ?assertEqual(6, Value),

    %% Get all counters
    Counters = erlmcp_cli_metrics:get_all_counters(),
    ?assertEqual(1, maps:size(Counters)),
    ?assertEqual(6, maps:get(<<"test.counter">>, Counters)),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc Test gauge metrics
gauge_metrics_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Initialize a gauge
    erlmcp_cli_metrics:register_gauge(<<"test.gauge">>, <<"Test gauge">>),

    %% Set gauge value
    erlmcp_cli_metrics:set_gauge(<<"test.gauge">>, 42.5),
    erlmcp_cli_metrics:increment_gauge(<<"test.gauge">>, 5.0),

    %% Get gauge value
    Value = erlmcp_cli_metrics:get_gauge_value(<<"test.gauge">>),
    ?assertEqual(47.5, Value),

    %% Get all gauges
    Gauges = erlmcp_cli_metrics:get_all_gauges(),
    ?assertEqual(1, maps:size(Gauges)),
    ?assertEqual(47.5, maps:get(<<"test.gauge">>, Gauges)),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc Test histogram metrics
histogram_metrics_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Initialize a histogram
    erlmcp_cli_metrics:register_histogram(<<"test.histogram">>, <<"Test histogram">>),

    %% Record observations
    erlmcp_cli_metrics:record_histogram(<<"test.histogram">>, 10),
    erlmcp_cli_metrics:record_histogram(<<"test.histogram">>, 20),
    erlmcp_cli_metrics:record_histogram(<<"test.histogram">>, 30),
    erlmcp_cli_metrics:record_histogram(<<"test.histogram">>, 40),

    %% Get histogram statistics
    Stats = erlmcp_cli_metrics:get_histogram_stats(<<"test.histogram">>),
    ?assert(is_map(Stats)),
    ?assertEqual(4, maps:get(<<"count">>, Stats)),
    ?assertEqual(25.0, maps:get(<<"mean">>, Stats)),
    ?assertEqual(10, maps:get(<<"min">>, Stats)),
    ?assertEqual(40, maps:get(<<"max">>, Stats)),

    %% Get all histograms
    Histograms = erlmcp_cli_metrics:get_all_histograms(),
    ?assertEqual(1, maps:size(Histograms)),
    ?assert(is_map(maps:get(<<"test.histogram">>, Histograms))),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc test otel integration
otel_integration_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Test OTEL metrics recording
    erlmcp_cli_metrics:record_otel_metric(<<"test.otel.counter">>, <<"counter">>, 10.0),
    erlmcp_cli_metrics:record_otel_metric(<<"test.otel.gauge">>, <<"gauge">>, 25.5),
    erlmcp_cli_metrics:record_otel_metric(<<"test.otel.histogram">>, <<"histogram">>, 15.0),

    %% Verify OTEL metrics are recorded
    OtelMetrics = erlmcp_cli_metrics:get_otel_metrics(),
    ?assertEqual(3, maps:size(OtelMetrics)),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc Test metrics aggregation
metrics_aggregation_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Register multiple counters
    erlmcp_cli_metrics:register_counter(<<"counter1">>, <<"Counter 1">>),
    erlmcp_cli_metrics:register_counter(<<"counter2">>, <<"Counter 2">>),

    %% Increment counters
    erlmcp_cli_metrics:increment_counter(<<"counter1">>),
    erlmcp_cli_metrics:increment_counter(<<"counter1">>),
    erlmcp_cli_metrics:increment_counter(<<"counter2">>),

    %% Get aggregated metrics
    AllMetrics = erlmcp_cli_metrics:get_all_metrics(),
    ?assertEqual(2, maps:size(AllMetrics)),
    ?assertEqual(2, maps:get(<<"counter1">>, AllMetrics)),
    ?assertEqual(1, maps:get(<<"counter2">>, AllMetrics)),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc Test metrics persistence
metrics_persistence_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Record some metrics
    erlmcp_cli_metrics:register_counter(<<"persist.counter">>, <<"Persistent counter">>),
    erlmcp_cli_metrics:increment_counter(<<"persist.counter">>),
    erlmcp_cli_metrics:increment_counter(<<"persist.counter">>),

    %% Get current metrics
    MetricsBefore = erlmcp_cli_metrics:get_all_counters(),

    %% Stop and restart metrics system
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    {ok, _Pid} = erlmcp_cli_metrics:start_link(),
    timer:sleep(50),

    %% Metrics should be reset (ETS tables are not persisted)
    MetricsAfter = erlmcp_cli_metrics:get_all_counters(),
    ?assertNotEqual(MetricsBefore, MetricsAfter),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc Test error handling for invalid metrics operations
metrics_error_handling_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Test getting non-existent counter
    ?assertEqual(0, erlmcp_cli_metrics:get_counter_value(<<"nonexistent">>)),

    %% Test getting non-existent gauge
    ?assertEqual(0.0, erlmcp_cli_metrics:get_gauge_value(<<"nonexistent">>)),

    %% Test getting non-existent histogram
    ?assertEqual(#{}, erlmcp_cli_metrics:get_histogram_stats(<<"nonexistent">>)),

    %% Test incrementing non-existent counter
    %% Should not crash
    erlmcp_cli_metrics:increment_counter(<<"nonexistent.counter">>),
    ?assertEqual(1, erlmcp_cli_metrics:get_counter_value(<<"nonexistent.counter">>)),

    %% Test setting non-existent gauge
    %% Should not crash
    erlmcp_cli_metrics:set_gauge(<<"nonexistent.gauge">>, 42.0),
    ?assertEqual(42.0, erlmcp_cli_metrics:get_gauge_value(<<"nonexistent.gauge">>)),

    %% Test recording to non-existent histogram
    %% Should not crash
    erlmcp_cli_metrics:record_histogram(<<"nonexistent.histogram">>, 10),
    Stats = erlmcp_cli_metrics:get_histogram_stats(<<"nonexistent.histogram">>),
    ?assertEqual(1, maps:get(<<"count">>, Stats)),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc Test metrics export
metrics_export_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Record some metrics
    erlmcp_cli_metrics:register_counter(<<"export.counter">>, <<"Export counter">>),
    erlmcp_cli_metrics:increment_counter(<<"export.counter">>, 5),

    erlmcp_cli_metrics:register_gauge(<<"export.gauge">>, <<"Export gauge">>),
    erlmcp_cli_metrics:set_gauge(<<"export.gauge">>, 25.0),

    %% Get metrics for export
    ExportData = erlmcp_cli_metrics:export_metrics(),
    ?assert(is_map(ExportData)),
    ?assertEqual(2, maps:size(ExportData)),

    %% Verify exported data structure
    Counters = maps:get(<<"counters">>, ExportData),
    Gauges = maps:get(<<"gauges">>, ExportData),
    Histograms = maps:get(<<"histograms">>, ExportData),

    ?assertEqual(1, maps:size(Counters)),
    ?assertEqual(5, maps:get(<<"export.counter">>, Counters)),

    ?assertEqual(1, maps:size(Gauges)),
    ?assertEqual(25.0, maps:get(<<"export.gauge">>, Gauges)),

    ?assertEqual(0, maps:size(Histograms)),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc Test metrics filtering
metrics_filtering_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Register metrics with different prefixes
    erlmcp_cli_metrics:register_counter(<<"cli.command.health">>, <<"Command health">>),
    erlmcp_cli_metrics:register_counter(<<"cli.transport.http">>, <<"HTTP transport">>),
    erlmcp_cli_metrics:register_counter(<<"core.connection">>, <<"Core connection">>),

    %% Increment counters
    erlmcp_cli_metrics:increment_counter(<<"cli.command.health">>),
    erlmcp_cli_metrics:increment_counter(<<"cli.command.health">>),
    erlmcp_cli_metrics:increment_counter(<<"cli.transport.http">>),
    erlmcp_cli_metrics:increment_counter(<<"core.connection">>),

    %% Filter by prefix
    CliMetrics = erlmcp_cli_metrics:get_metrics_by_prefix(<<"cli">>),
    ?assertEqual(2, maps:size(CliMetrics)),

    TransportMetrics = erlmcp_cli_metrics:get_metrics_by_prefix(<<"cli.transport">>),
    ?assertEqual(1, maps:size(TransportMetrics)),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.

%% @doc Test metrics reset
metrics_reset_test() ->
    %% Start metrics system
    {ok, Pid} = erlmcp_cli_metrics:start_link(),

    %% Record some metrics
    erlmcp_cli_metrics:register_counter(<<"test.reset.counter">>, <<"Reset counter">>),
    erlmcp_cli_metrics:increment_counter(<<"test.reset.counter">>),
    erlmcp_cli_metrics:increment_counter(<<"test.reset.counter">>),

    %% Verify metrics exist
    ?assertEqual(2, erlmcp_cli_metrics:get_counter_value(<<"test.reset.counter">>)),

    %% Reset all metrics
    erlmcp_cli_metrics:reset_all_metrics(),

    %% Verify metrics are reset
    ?assertEqual(0, erlmcp_cli_metrics:get_counter_value(<<"test.reset.counter">>)),

    %% Cleanup
    erlmcp_cli_metrics:stop(),
    timer:sleep(50),
    ok.