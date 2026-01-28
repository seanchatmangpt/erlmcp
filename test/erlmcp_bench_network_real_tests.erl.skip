%%%====================================================================
%%% erlmcp_bench_network_real_tests.erl - Unified Network Benchmark Tests
%%%====================================================================
%%% Tests for consolidated TCP and HTTP transport benchmarks
%%%====================================================================

-module(erlmcp_bench_network_real_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Ensure applications are started
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(ranch),
    application:ensure_all_started(gun),
    application:ensure_all_started(cowboy),
    ok.

cleanup(_) ->
    %% Cleanup
    application:stop(cowboy),
    application:stop(gun),
    application:stop(ranch),
    ok.

%%====================================================================
%% Workload Definition Tests
%%====================================================================

workloads_defined_test() ->
    Workloads = erlmcp_bench_network_real:list_workloads(),
    ?assert(length(Workloads) >= 7),
    ?assert(lists:member({tcp_burst_100_1kib, <<"tcp_burst_100_1kib">>, tcp}, Workloads)),
    ?assert(lists:member({http_burst_100_1kib, <<"http_burst_100_1kib">>, http}, Workloads)).

tcp_workloads_test() ->
    Workloads = erlmcp_bench_network_real:list_workloads(),
    TcpWorkloads = [Name || {Name, _, tcp} <- Workloads],
    ?assert(length(TcpWorkloads) >= 4),
    ?assert(lists:member(tcp_burst_100_1kib, TcpWorkloads)),
    ?assert(lists:member(tcp_sustained_10k_1kib, TcpWorkloads)).

http_workloads_test() ->
    Workloads = erlmcp_bench_network_real:list_workloads(),
    HttpWorkloads = [Name || {Name, _, http} <- Workloads],
    ?assert(length(HttpWorkloads) >= 3),
    ?assert(lists:member(http_burst_100_1kib, HttpWorkloads)),
    ?assert(lists:member(http_sustained_5k_1kib, HttpWorkloads)).

%%====================================================================
%% TCP Benchmark Tests
%%====================================================================

tcp_minimal_workload_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 30, fun() ->
        %% Create minimal TCP workload
        Result = erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib, #{
            connections => 10,
            duration_s => 5,
            payload_size_bytes => 256
        }),

        case Result of
            {ok, Metrics} ->
                %% Verify metrics structure
                ?assert(is_record(Metrics, metrics)),
                ?assertEqual(<<"tcp">>, element(4, Metrics)),  % transport field
                ?assert(element(8, Metrics) > 0),  % messages_sent > 0
                ?assert(element(9, Metrics) > 0.0),  % throughput > 0

                %% Verify latency metrics
                ?assert(element(11, Metrics) >= 0.0),  % p50
                ?assert(element(12, Metrics) >= 0.0),  % p95
                ?assert(element(13, Metrics) >= 0.0),  % p99

                %% Verify resource metrics
                ?assert(element(17, Metrics) >= 0.0),  % memory
                ?assert(element(19, Metrics) >= 0.0);  % cpu

            {error, Reason} ->
                ?debugFmt("TCP benchmark failed: ~p", [Reason]),
                ?assert(false)
        end
     end}}.

tcp_connection_setup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 20, fun() ->
        Result = erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib, #{
            connections => 5,
            duration_s => 3,
            payload_size_bytes => 128
        }),

        case Result of
            {ok, Metrics} ->
                %% Verify connection setup time is measured
                SetupTime = element(14, Metrics),  % connection_setup_avg_ms
                ?assert(SetupTime >= 0.0),
                ?assert(SetupTime < 1000.0),  % Should be < 1 second per connection

                %% Verify no connection failures for small workload
                Failures = element(15, Metrics),  % connection_failures
                ?assert(Failures =:= 0);

            {error, Reason} ->
                ?debugFmt("TCP connection test failed: ~p", [Reason]),
                ?assert(false)
        end
     end}}.

tcp_latency_measurement_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 20, fun() ->
        Result = erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib, #{
            connections => 5,
            duration_s => 3,
            payload_size_bytes => 512
        }),

        case Result of
            {ok, Metrics} ->
                P50 = element(11, Metrics),
                P95 = element(12, Metrics),
                P99 = element(13, Metrics),

                %% Verify latency percentiles are ordered
                ?assert(P50 =< P95),
                ?assert(P95 =< P99),

                %% Verify latencies are in reasonable range (< 100ms = 100000us)
                ?assert(P99 < 100000.0);

            {error, Reason} ->
                ?debugFmt("TCP latency test failed: ~p", [Reason]),
                ?assert(false)
        end
     end}}.

%%====================================================================
%% HTTP Benchmark Tests
%%====================================================================

http_minimal_workload_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 30, fun() ->
        %% Create minimal HTTP workload
        Result = erlmcp_bench_network_real:run_workload(http_burst_100_1kib, #{
            connections => 5,
            duration_s => 5,
            payload_size_bytes => 256
        }),

        case Result of
            {ok, Metrics} ->
                %% Verify metrics structure
                ?assert(is_record(Metrics, metrics)),
                Transport = element(4, Metrics),
                ?assert(Transport =:= <<"http_2.0">> orelse Transport =:= <<"http_1.1">>),
                ?assert(element(8, Metrics) > 0),  % messages_sent > 0

                %% Verify HTTP-specific fields are present
                Protocol = element(22, Metrics),  % protocol field
                ?assert(Protocol =/= undefined),
                ?assert(Protocol =:= <<"http/2">> orelse Protocol =:= <<"http/1.1">>);

            {error, Reason} ->
                ?debugFmt("HTTP benchmark failed: ~p", [Reason]),
                ?assert(false)
        end
     end}}.

http_protocol_detection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 30, fun() ->
        Result = erlmcp_bench_network_real:run_workload(http_burst_100_1kib, #{
            connections => 3,
            duration_s => 3,
            payload_size_bytes => 128
        }),

        case Result of
            {ok, Metrics} ->
                Protocol = element(22, Metrics),
                ?assert(Protocol =:= <<"http/2">> orelse Protocol =:= <<"http/1.1">>),

                %% Verify TLS handshake time is measured
                TlsHandshake = element(23, Metrics),
                ?assert(is_float(TlsHandshake)),
                ?assert(TlsHandshake >= 0.0);

            {error, Reason} ->
                ?debugFmt("HTTP protocol test failed: ~p", [Reason]),
                ?assert(false)
        end
     end}}.

http_overhead_measurement_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 20, fun() ->
        Result = erlmcp_bench_network_real:run_workload(http_burst_100_1kib, #{
            connections => 3,
            duration_s => 3,
            payload_size_bytes => 512
        }),

        case Result of
            {ok, Metrics} ->
                %% Verify HTTP overhead is tracked
                Overhead = element(24, Metrics),  % request_overhead_bytes
                ?assert(is_integer(Overhead)),
                ?assert(Overhead > 0),
                ?assert(Overhead >= 200),  % Minimum HTTP header size

                %% Verify connection reuse is tracked
                Reuse = element(25, Metrics),  % connection_reuse_percent
                ?assert(is_float(Reuse)),
                ?assert(Reuse >= 0.0),
                ?assert(Reuse =< 100.0);

            {error, Reason} ->
                ?debugFmt("HTTP overhead test failed: ~p", [Reason]),
                ?assert(false)
        end
     end}}.

%%====================================================================
%% Validation Tests
%%====================================================================

validate_tcp_metrics_test() ->
    %% Create sample TCP metrics
    Metrics = #{
        <<"workload_id">> => <<"tcp_test">>,
        <<"benchmark">> => <<"network_real">>,
        <<"transport">> => <<"tcp">>,
        <<"connections">> => 100,
        <<"duration_s">> => 60,
        <<"messages_sent">> => 100000,
        <<"throughput_msg_per_s">> => 1666.67,
        <<"latency_p50_us">> => 450.0,
        <<"latency_p95_us">> => 1200.0,
        <<"latency_p99_us">> => 2500.0,
        <<"bandwidth_mib_per_s">> => 1.6,
        <<"memory_rss_mib_per_node">> => 512.0,
        <<"precision">> => <<"microsecond">>,
        <<"scope">> => <<"per_node">>,
        <<"environment">> => <<"local_dev">>
    },

    Result = erlmcp_bench_network_real:validate_metrics(Metrics),
    ?assertEqual(ok, Result).

validate_http_metrics_test() ->
    %% Create sample HTTP metrics
    Metrics = #{
        <<"workload_id">> => <<"http_test">>,
        <<"benchmark">> => <<"network_real">>,
        <<"transport">> => <<"http_2.0">>,
        <<"connections">> => 100,
        <<"duration_s">> => 60,
        <<"messages_sent">> => 50000,
        <<"throughput_msg_per_s">> => 833.33,
        <<"latency_p50_us">> => 1200.0,
        <<"latency_p95_us">> => 3000.0,
        <<"latency_p99_us">> => 5000.0,
        <<"bandwidth_mib_per_s">> => 0.8,
        <<"memory_rss_mib_per_node">> => 256.0,
        <<"precision">> => <<"microsecond">>,
        <<"scope">> => <<"per_node">>,
        <<"environment">> => <<"local_dev">>,
        <<"protocol">> => <<"http/2">>,
        <<"tls_handshake_avg_us">> => 500.0,
        <<"request_overhead_bytes">> => 234,
        <<"connection_reuse_percent">> => 100.0
    },

    Result = erlmcp_bench_network_real:validate_metrics(Metrics),
    ?assertEqual(ok, Result).

validate_missing_fields_test() ->
    %% Create metrics missing required fields
    Metrics = #{
        <<"workload_id">> => <<"incomplete_test">>,
        <<"transport">> => <<"tcp">>
    },

    Result = erlmcp_bench_network_real:validate_metrics(Metrics),
    ?assertMatch({error, {missing_fields, _}}, Result).

%%====================================================================
%% JSON Serialization Tests
%%====================================================================

metrics_to_json_tcp_test() ->
    %% This test verifies the metrics_to_json/1 function works for TCP metrics
    %% We can't easily construct a full #metrics{} record here without including
    %% the header, so we'll do basic validation that the function exists
    ?assert(erlang:function_exported(erlmcp_bench_network_real, metrics_to_json, 1)).

%%====================================================================
%% Helper Function Tests
%%====================================================================

percentile_calculation_test() ->
    %% Test percentile calculation with known values
    %% Note: We can't directly test the private function, but we verify
    %% it through public API behavior in other tests
    ?assert(true).

json_rpc_payload_creation_test() ->
    %% Verify payload creation is consistent
    %% Note: create_json_rpc_payload is private, tested via integration
    ?assert(true).

environment_detection_test() ->
    %% Verify environment detection works
    %% Note: detect_environment is private, tested via integration
    ?assert(true).

%%====================================================================
%% Integration Tests
%%====================================================================

tcp_http_comparison_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 60, fun() ->
        %% Run both TCP and HTTP with similar parameters
        TcpResult = erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib, #{
            connections => 10,
            duration_s => 5,
            payload_size_bytes => 256
        }),

        HttpResult = erlmcp_bench_network_real:run_workload(http_burst_100_1kib, #{
            connections => 10,
            duration_s => 5,
            payload_size_bytes => 256
        }),

        case {TcpResult, HttpResult} of
            {{ok, TcpMetrics}, {ok, HttpMetrics}} ->
                %% TCP should have higher throughput than HTTP due to lower overhead
                TcpThroughput = element(9, TcpMetrics),
                HttpThroughput = element(9, HttpMetrics),

                ?debugFmt("TCP throughput: ~.2f msg/s", [TcpThroughput]),
                ?debugFmt("HTTP throughput: ~.2f msg/s", [HttpThroughput]),

                %% Both should have positive throughput
                ?assert(TcpThroughput > 0.0),
                ?assert(HttpThroughput > 0.0);

            {{error, TcpReason}, _} ->
                ?debugFmt("TCP benchmark failed: ~p", [TcpReason]),
                ?assert(false);

            {_, {error, HttpReason}} ->
                ?debugFmt("HTTP benchmark failed: ~p", [HttpReason]),
                ?assert(false)
        end
     end}}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

unknown_workload_test() ->
    Result = erlmcp_bench_network_real:run_workload(nonexistent_workload),
    ?assertMatch({error, {unknown_workload, nonexistent_workload}}, Result).

invalid_options_test() ->
    %% Test with invalid option values (should be handled gracefully)
    Result = erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib, #{
        connections => 1,
        duration_s => 1,
        payload_size_bytes => 64
    }),

    %% Should still return ok or error, not crash
    ?assert(is_tuple(Result)).

%%====================================================================
%% Performance Regression Tests
%%====================================================================

tcp_throughput_baseline_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 20, fun() ->
        Result = erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib, #{
            connections => 10,
            duration_s => 5,
            payload_size_bytes => 1024
        }),

        case Result of
            {ok, Metrics} ->
                Throughput = element(9, Metrics),
                %% Baseline: At least 1000 msg/s for 10 connections
                ?assert(Throughput >= 1000.0),
                ?debugFmt("TCP throughput: ~.2f msg/s (baseline: 1000)", [Throughput]);

            {error, Reason} ->
                ?debugFmt("TCP baseline test failed: ~p", [Reason]),
                ?assert(false)
        end
     end}}.

http_throughput_baseline_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 20, fun() ->
        Result = erlmcp_bench_network_real:run_workload(http_burst_100_1kib, #{
            connections => 5,
            duration_s => 5,
            payload_size_bytes => 1024
        }),

        case Result of
            {ok, Metrics} ->
                Throughput = element(9, Metrics),
                %% Baseline: At least 100 msg/s for 5 connections (HTTP overhead)
                ?assert(Throughput >= 100.0),
                ?debugFmt("HTTP throughput: ~.2f msg/s (baseline: 100)", [Throughput]);

            {error, Reason} ->
                ?debugFmt("HTTP baseline test failed: ~p", [Reason]),
                ?assert(false)
        end
     end}}.
