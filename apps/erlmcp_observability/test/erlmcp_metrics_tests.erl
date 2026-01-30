-module(erlmcp_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

-define(METRIC_RECORD, true).
-record(metric, {
    name :: binary(),
    value :: number(),
    labels :: #{binary() => term()},
    timestamp :: integer()
}).

%%====================================================================
%% Tests for erlmcp_metrics module (gen_server-based metrics)
%%====================================================================

%% Test recording transport operations with correct API signature
%% API: record_transport_operation(TransportId, TransportType, Operation, Duration)
record_transport_operation_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Result = erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),
    ?assertEqual(ok, Result),
    gen_server:stop(Pid).

%% Test recording server operations with correct API signature
%% API: record_server_operation(ServerId, Operation, Duration, ExtraLabels)
record_server_operation_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Labels = #{<<"result">> => ok},
    Result = erlmcp_metrics:record_server_operation(<<"server_1">>, initialize, 50, Labels),
    ?assertEqual(ok, Result),
    gen_server:stop(Pid).

%% Test recording registry operations with correct API signature
%% API: record_registry_operation(Operation, Duration, ExtraLabels)
record_registry_operation_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Labels = #{},
    Result = erlmcp_metrics:record_registry_operation(register, 10, Labels),
    ?assertEqual(ok, Result),
    gen_server:stop(Pid).

%% Test get_metrics returns a list of metric records, not a map
get_metrics_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 10),
    Result = erlmcp_metrics:get_metrics(),
    ?assert(is_list(Result)),
    ?assertEqual(1, length(Result)),
    [#metric{name = Name}] = Result,
    ?assertEqual(<<"transport_operation_duration_ms">>, Name),
    gen_server:stop(Pid).

%% Test full lifecycle with multiple metric types
metrics_lifecycle_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),
    erlmcp_metrics:record_server_operation(<<"server_1">>, call, 20, #{<<"result">> => ok}),
    erlmcp_metrics:record_registry_operation(lookup, 5, #{}),

    Metrics = erlmcp_metrics:get_metrics(),
    ?assert(is_list(Metrics)),
    ?assertEqual(3, length(Metrics)),

    gen_server:stop(Pid).

%% Test get_performance_summary returns a map with expected keys
get_performance_summary_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),
    erlmcp_metrics:record_server_operation(<<"server_1">>, call, 20, #{<<"result">> => ok}),

    Summary = erlmcp_metrics:get_performance_summary(),
    ?assert(is_map(Summary)),
    ?assert(maps:is_key(<<"uptime_ms">>, Summary)),
    ?assert(maps:is_key(<<"total_metrics_recorded">>, Summary)),
    ?assert(maps:is_key(<<"counters">>, Summary)),
    ?assert(maps:is_key(<<"histograms">>, Summary)),
    ?assert(maps:is_key(<<"gauges">>, Summary)),

    gen_server:stop(Pid).

%% Test reset_metrics clears all recorded metrics
reset_metrics_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),

    Metrics1 = erlmcp_metrics:get_metrics(),
    ?assertEqual(1, length(Metrics1)),

    ok = erlmcp_metrics:reset_metrics(),

    Metrics2 = erlmcp_metrics:get_metrics(),
    ?assertEqual(0, length(Metrics2)),

    gen_server:stop(Pid).

%% Test get_metrics/1 filters by metric name
get_metrics_by_name_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),
    erlmcp_metrics:record_server_operation(<<"server_1">>, call, 20, #{}),

    TransportMetrics = erlmcp_metrics:get_metrics(<<"transport_operation_duration_ms">>),
    ?assertEqual(1, length(TransportMetrics)),

    ServerMetrics = erlmcp_metrics:get_metrics(<<"server_operation_duration_ms">>),
    ?assertEqual(1, length(ServerMetrics)),

    gen_server:stop(Pid).

%% Test with_metrics helper function
with_metrics_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Labels = #{<<"test">> => true},

    Result = erlmcp_metrics:with_metrics(<<"test_operation">>, Labels, fun() ->
        timer:sleep(10),
        success
    end),

    ?assertEqual(success, Result),

    Metrics = erlmcp_metrics:get_metrics(<<"test_operation">>),
    ?assertEqual(1, length(Metrics)),
    [#metric{value = Duration}] = Metrics,
    ?assert(Duration >= 10),

    gen_server:stop(Pid).

%% Test histogram calculations in performance summary
histogram_calculations_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),

    %% Record multiple operations to build histogram
    lists:foreach(fun(N) ->
        erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, N)
    end, [10, 20, 30, 40, 50]),

    Summary = erlmcp_metrics:get_performance_summary(),
    Histograms = maps:get(<<"histograms">>, Summary),
    TransportHist = maps:get(<<"transport_operation_duration_ms">>, Histograms),

    ?assertEqual(5, maps:get(<<"count">>, TransportHist)),
    ?assertEqual(10, maps:get(<<"min">>, TransportHist)),
    ?assertEqual(50, maps:get(<<"max">>, TransportHist)),
    ?assertEqual(30.0, maps:get(<<"avg">>, TransportHist)),

    gen_server:stop(Pid).

%% Test percentiles calculation
percentiles_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),

    %% Record 100 operations to test percentiles
    lists:foreach(fun(N) ->
        erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, N)
    end, lists:seq(1, 100)),

    Summary = erlmcp_metrics:get_performance_summary(),
    Percentiles = maps:get(<<"percentiles">>, Summary),
    TransportPcts = maps:get(<<"transport_operation_duration_ms_percentiles">>, Percentiles),

    ?assert(maps:is_key(<<"p50">>, TransportPcts)),
    ?assert(maps:is_key(<<"p90">>, TransportPcts)),
    ?assert(maps:is_key(<<"p95">>, TransportPcts)),
    ?assert(maps:is_key(<<"p99">>, TransportPcts)),

    gen_server:stop(Pid).

%% Test metrics are stored with correct labels
metric_labels_test() ->
    {ok, Pid} = erlmcp_metrics:start_link(),

    erlmcp_metrics:record_transport_operation(<<"tcp_conn_42">>, tcp, send, 25),

    Metrics = erlmcp_metrics:get_metrics(),
    [#metric{labels = Labels}] = Metrics,

    ?assertEqual(<<"tcp_conn_42">>, maps:get(<<"transport_id">>, Labels)),
    ?assertEqual(tcp, maps:get(<<"transport_type">>, Labels)),
    ?assertEqual(send, maps:get(<<"operation">>, Labels)),

    gen_server:stop(Pid).
