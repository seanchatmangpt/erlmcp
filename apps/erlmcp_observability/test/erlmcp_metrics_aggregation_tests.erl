%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_metrics aggregation following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL erlmcp_metrics gen_server (no mocks, no dummy processes)
%%% - NO internal state inspection (test API boundaries only)
%%% - NO record duplication (respect encapsulation)
%%% - Focused module: aggregation and performance summary tests
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrics_aggregation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup/teardown for metrics aggregation tests
aggregation_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     [{"Get performance summary", fun test_get_performance_summary/0},
      {"Histogram calculations", fun test_histogram_calculations/0},
      {"Percentiles calculation", fun test_percentiles/0},
      {"Metric labels", fun test_metric_labels/0}]}.

setup_metrics() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Pid.

cleanup_metrics(Pid) ->
    catch gen_server:stop(Pid),
    catch unregister(erlmcp_metrics).

%%====================================================================
%% Performance Summary Tests
%%====================================================================

%% Test get_performance_summary returns a map with expected keys
test_get_performance_summary() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),
    erlmcp_metrics:record_server_operation(<<"server_1">>, call, 20, #{<<"result">> => ok}),

    Summary = erlmcp_metrics:get_performance_summary(),
    ?assert(is_map(Summary)),

    % Verify expected top-level keys (observable behavior)
    ?assert(maps:is_key(<<"uptime_ms">>, Summary)),
    ?assert(maps:is_key(<<"total_metrics_recorded">>, Summary)),
    ?assert(maps:is_key(<<"counters">>, Summary)),
    ?assert(maps:is_key(<<"histograms">>, Summary)),
    ?assert(maps:is_key(<<"gauges">>, Summary)),

    gen_server:stop(Pid).

%%====================================================================
%% Histogram Tests
%%====================================================================

%% Test histogram calculations in performance summary
test_histogram_calculations() ->
    {ok, Pid} = erlmcp_metrics:start_link(),

    % Record multiple operations to build histogram
    lists:foreach(fun(N) ->
                     erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, N)
                  end,
                  [10, 20, 30, 40, 50]),

    Summary = erlmcp_metrics:get_performance_summary(),
    Histograms = maps:get(<<"histograms">>, Summary),

    % Verify histogram exists for transport operations
    ?assert(maps:is_key(<<"transport_operation_duration_ms">>, Histograms)),

    TransportHist = maps:get(<<"transport_operation_duration_ms">>, Histograms),

    % Verify histogram statistics (observable behavior)
    ?assertEqual(5, maps:get(<<"count">>, TransportHist)),
    ?assertEqual(10, maps:get(<<"min">>, TransportHist)),
    ?assertEqual(50, maps:get(<<"max">>, TransportHist)),
    ?assertEqual(30.0, maps:get(<<"avg">>, TransportHist)),

    gen_server:stop(Pid).

%%====================================================================
%% Percentile Tests
%%====================================================================

%% Test percentiles calculation
test_percentiles() ->
    {ok, Pid} = erlmcp_metrics:start_link(),

    % Record 100 operations to test percentiles
    lists:foreach(fun(N) ->
                     erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, N)
                  end,
                  lists:seq(1, 100)),

    Summary = erlmcp_metrics:get_performance_summary(),
    Percentiles = maps:get(<<"percentiles">>, Summary),

    % Verify percentiles exist
    ?assert(maps:is_key(<<"transport_operation_duration_ms_percentiles">>, Percentiles)),

    TransportPcts = maps:get(<<"transport_operation_duration_ms_percentiles">>, Percentiles),

    % Verify percentile keys exist
    ?assert(maps:is_key(<<"p50">>, TransportPcts)),
    ?assert(maps:is_key(<<"p90">>, TransportPcts)),
    ?assert(maps:is_key(<<"p95">>, TransportPcts)),
    ?assert(maps:is_key(<<"p99">>, TransportPcts)),

    gen_server:stop(Pid).

%%====================================================================
%% Label Tests
%%====================================================================

%% Test metrics are stored with correct labels
test_metric_labels() ->
    {ok, Pid} = erlmcp_metrics:start_link(),

    erlmcp_metrics:record_transport_operation(<<"tcp_conn_42">>, tcp, send, 25),

    Metrics = erlmcp_metrics:get_metrics(),
    [FirstMetric | _] = Metrics,

    % Verify labels are stored correctly (observable behavior)
    Labels = maps:get(labels, FirstMetric),
    ?assertEqual(<<"tcp_conn_42">>, maps:get(<<"transport_id">>, Labels)),
    ?assertEqual(tcp, maps:get(<<"transport_type">>, Labels)),
    ?assertEqual(send, maps:get(<<"operation">>, Labels)),

    gen_server:stop(Pid).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test summary with no metrics
empty_summary_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     fun(_Pid) ->
        [?_test(begin
                    % Get summary without recording any metrics
                    Summary = erlmcp_metrics:get_performance_summary(),

                    % Should still return valid structure
                    ?assert(is_map(Summary)),
                    ?assert(maps:is_key(<<"uptime_ms">>, Summary)),
                    ?assertEqual(0, maps:get(<<"total_metrics_recorded">>, Summary))
                end)]
     end}.

%% Test histogram with single value
single_value_histogram_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     fun(_Pid) ->
        [?_test(begin
                    % Record single metric
                    ok = erlmcp_metrics:record_transport_operation(<<"tcp">>, tcp, send, 100),

                    Summary = erlmcp_metrics:get_performance_summary(),
                    Histograms = maps:get(<<"histograms">>, Summary),
                    TransportHist = maps:get(<<"transport_operation_duration_ms">>, Histograms),

                    % Single value histogram: min = max = avg
                    ?assertEqual(1, maps:get(<<"count">>, TransportHist)),
                    ?assertEqual(100, maps:get(<<"min">>, TransportHist)),
                    ?assertEqual(100, maps:get(<<"max">>, TransportHist)),
                    ?assertEqual(100.0, maps:get(<<"avg">>, TransportHist))
                end)]
     end}.

%% Test counters increment correctly
counter_increment_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     fun(_Pid) ->
        [?_test(begin
                    % Record multiple metrics of same type
                    ok = erlmcp_metrics:record_transport_operation(<<"tcp1">>, tcp, send, 10),
                    ok = erlmcp_metrics:record_transport_operation(<<"tcp2">>, tcp, send, 20),
                    ok = erlmcp_metrics:record_transport_operation(<<"tcp3">>, tcp, send, 30),

                    Summary = erlmcp_metrics:get_performance_summary(),
                    Counters = maps:get(<<"counters">>, Summary),

                    % Counter should be incremented
                    ?assert(maps:is_key(<<"transport_operation_duration_ms">>, Counters))
                end)]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test complete aggregation workflow
aggregation_workflow_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     fun(_Pid) ->
        [?_test(begin
                    % Record various operations
                    ok = erlmcp_metrics:record_transport_operation(<<"conn1">>, tcp, send, 10),
                    ok = erlmcp_metrics:record_transport_operation(<<"conn2">>, tcp, recv, 20),
                    ok = erlmcp_metrics:record_server_operation(<<"srv1">>, call, 30, #{}),
                    ok = erlmcp_metrics:record_server_operation(<<"srv2">>, init, 40, #{}),
                    ok = erlmcp_metrics:record_registry_operation(register, 5, #{}),
                    ok = erlmcp_metrics:record_registry_operation(lookup, 15, #{}),

                    % Get performance summary
                    Summary = erlmcp_metrics:get_performance_summary(),

                    % Verify total metrics count
                    TotalMetrics = maps:get(<<"total_metrics_recorded">>, Summary),
                    ?assertEqual(6, TotalMetrics),

                    % Verify histograms exist for each metric type
                    Histograms = maps:get(<<"histograms">>, Summary),
                    ?assert(maps:is_key(<<"transport_operation_duration_ms">>, Histograms)),
                    ?assert(maps:is_key(<<"server_operation_duration_ms">>, Histograms)),
                    ?assert(maps:is_key(<<"registry_operation_duration_ms">>, Histograms)),

                    % Verify counters
                    Counters = maps:get(<<"counters">>, Summary),
                    ?assert(maps:is_key(<<"transport_operation_duration_ms">>, Counters)),

                    % Verify uptime is positive
                    Uptime = maps:get(<<"uptime_ms">>, Summary),
                    ?assert(Uptime > 0)
                end)]
     end}.
