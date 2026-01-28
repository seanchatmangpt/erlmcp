%%%====================================================================
%%% tcp_real_bench_tests.erl - Tests for Real TCP Transport Benchmark
%%%====================================================================

-module(tcp_real_bench_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

%% Basic functionality tests
tcp_real_bench_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"Small workload completes", fun test_small_workload/0},
         {"JSON output format is correct", fun test_json_format/0},
         {"Metrics validation works", fun test_metrics_validation/0},
         {"Connection handling is correct", fun test_connection_handling/0},
         {"Latency measurement is accurate", fun test_latency_measurement/0},
         {"Results are persisted", fun test_results_persistence/0}
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    application:stop(erlmcp),
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_small_workload() ->
    %% Run a minimal workload for testing
    Result = tcp_real_bench:run_workload(small_burst, #{
        connections => 10,
        duration_s => 5,
        payload_size_bytes => 1024
    }),

    ?assertMatch({ok, _Metrics}, Result),

    {ok, Metrics} = Result,

    %% Verify metrics record structure
    ?assert(is_record(Metrics, metrics)),

    %% Verify basic metrics
    ?assertEqual(10, element(4, Metrics)),  % connections
    ?assert(element(6, Metrics) > 0),       % messages_sent
    ?assert(element(7, Metrics) > 0),       % throughput_msg_per_s

    %% Verify latency values are reasonable
    P50 = element(8, Metrics),
    P95 = element(9, Metrics),
    P99 = element(10, Metrics),

    ?assert(P50 > 0),
    ?assert(P95 >= P50),
    ?assert(P99 >= P95),
    ?assert(P99 < 1000000),  % Less than 1 second in microseconds

    ok.

test_json_format() ->
    %% Run minimal benchmark
    {ok, Metrics} = tcp_real_bench:run_workload(small_burst, #{
        connections => 5,
        duration_s => 3,
        payload_size_bytes => 512
    }),

    %% Convert to JSON
    JsonMap = tcp_real_bench:metrics_to_json(Metrics),

    %% Verify all required fields
    RequiredFields = [
        <<"workload_id">>, <<"transport">>, <<"tls_enabled">>,
        <<"connections">>, <<"duration_s">>, <<"messages_sent">>,
        <<"throughput_msg_per_s">>, <<"latency_p50_us">>,
        <<"latency_p95_us">>, <<"latency_p99_us">>,
        <<"bandwidth_mib_per_s">>, <<"cpu_percent_per_node">>,
        <<"memory_rss_mib_per_node">>, <<"memory_heap_mib_per_conn">>,
        <<"precision">>, <<"scope">>, <<"environment">>,
        <<"timestamp">>, <<"error_count">>, <<"actual_duration_s">>
    ],

    lists:foreach(fun(Field) ->
        ?assert(maps:is_key(Field, JsonMap),
               io_lib:format("Missing field: ~s", [Field]))
    end, RequiredFields),

    %% Verify field types
    ?assertEqual(<<"tcp">>, maps:get(<<"transport">>, JsonMap)),
    ?assertEqual(<<"microsecond">>, maps:get(<<"precision">>, JsonMap)),
    ?assertEqual(<<"per_node">>, maps:get(<<"scope">>, JsonMap)),

    ?assert(is_boolean(maps:get(<<"tls_enabled">>, JsonMap))),
    ?assert(is_integer(maps:get(<<"connections">>, JsonMap))),
    ?assert(is_number(maps:get(<<"throughput_msg_per_s">>, JsonMap))),

    %% Verify JSON encoding works
    JsonBinary = jsx:encode(JsonMap),
    ?assert(is_binary(JsonBinary)),
    ?assert(byte_size(JsonBinary) > 100),

    ok.

test_metrics_validation() ->
    %% Valid metrics
    ValidMetrics = #{
        <<"workload_id">> => <<"test_workload">>,
        <<"transport">> => <<"tcp">>,
        <<"connections">> => 100,
        <<"duration_s">> => 60,
        <<"messages_sent">> => 10000,
        <<"throughput_msg_per_s">> => 166.67,
        <<"latency_p50_us">> => 450.0,
        <<"latency_p95_us">> => 1200.0,
        <<"latency_p99_us">> => 2500.0,
        <<"bandwidth_mib_per_s">> => 1.5,
        <<"memory_rss_mib_per_node">> => 512.0,
        <<"precision">> => <<"microsecond">>,
        <<"scope">> => <<"per_node">>,
        <<"environment">> => <<"test">>
    },

    ?assertEqual(ok, tcp_real_bench:validate_metrics(ValidMetrics)),

    %% Missing required field
    InvalidMetrics = maps:remove(<<"workload_id">>, ValidMetrics),
    ?assertMatch({error, {missing_fields, _}},
                tcp_real_bench:validate_metrics(InvalidMetrics)),

    ok.

test_connection_handling() ->
    %% Test that multiple connections can be established
    application:ensure_all_started(erlmcp),

    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),

    {ok, ServerState} = gen_server:call(ServerPid, get_state),
    Port = element(8, ServerState),

    %% Start 10 clients
    ClientPids = [begin
        {ok, Pid} = erlmcp_transport_tcp:start_client(#{
            mode => client,
            host => "localhost",
            port => Port
        }),
        Pid
    end || _ <- lists:seq(1, 10)],

    timer:sleep(500),  % Let connections establish

    %% Verify all clients are alive
    AliveClients = [Pid || Pid <- ClientPids, is_process_alive(Pid)],
    ?assertEqual(10, length(AliveClients)),

    %% Cleanup
    lists:foreach(fun(Pid) -> gen_server:stop(Pid) end, ClientPids),
    gen_server:stop(ServerPid),

    ok.

test_latency_measurement() ->
    %% Test that latency measurement produces reasonable values
    Latencies = [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000],

    P50 = tcp_real_bench:percentile(Latencies, 0.50),
    P95 = tcp_real_bench:percentile(Latencies, 0.95),
    P99 = tcp_real_bench:percentile(Latencies, 0.99),

    ?assertEqual(500, P50),  % Median of 10 values
    ?assertEqual(950, P95),  % 95th percentile
    ?assertEqual(990, P99),  % 99th percentile

    %% Test with empty list
    ?assertEqual(0.0, tcp_real_bench:percentile([], 0.50)),

    ok.

test_results_persistence() ->
    %% Run minimal benchmark
    {ok, Metrics} = tcp_real_bench:run_workload(small_burst, #{
        connections => 3,
        duration_s => 2,
        payload_size_bytes => 256
    }),

    %% Save results
    ok = tcp_real_bench:validate_and_save(Metrics, test_workload),

    %% Verify file was created
    {ok, Files} = file:list_dir("bench/results"),
    JsonFiles = [F || F <- Files, string:str(F, "transport_real_tcp_") > 0,
                      string:str(F, ".json") > 0],

    ?assert(length(JsonFiles) > 0, "No JSON result files found"),

    %% Read and verify file contents
    [FirstFile | _] = lists:sort(fun(A, B) -> A > B end, JsonFiles),
    {ok, JsonBinary} = file:read_file("bench/results/" ++ FirstFile),

    %% Verify it's valid JSON
    DecodedMap = jsx:decode(JsonBinary, [return_maps]),

    ?assert(is_map(DecodedMap)),
    ?assert(maps:is_key(<<"workload_id">>, DecodedMap)),
    ?assert(maps:is_key(<<"throughput_msg_per_s">>, DecodedMap)),

    ok.

%%====================================================================
%% Property-Based Tests (if proper is available)
%%====================================================================

-ifdef(PROPER).

-include_lib("proper/include/proper.hrl").

prop_percentile_bounds() ->
    ?FORALL(Values, non_empty(list(pos_integer())),
        begin
            Sorted = lists:sort(Values),
            P50 = tcp_real_bench:percentile(Sorted, 0.50),
            P95 = tcp_real_bench:percentile(Sorted, 0.95),
            P99 = tcp_real_bench:percentile(Sorted, 0.99),

            P50 =< P95 andalso P95 =< P99 andalso
            P99 >= lists:min(Values) andalso P99 =< lists:max(Values)
        end).

prop_json_roundtrip() ->
    ?FORALL({Connections, Duration, Messages},
            {pos_integer(), pos_integer(), non_neg_integer()},
        begin
            Metrics = #metrics{
                workload_id = <<"test">>,
                tls_enabled = false,
                connections = Connections,
                duration_s = Duration,
                messages_sent = Messages,
                throughput_msg_per_s = 1000.0,
                latency_p50_us = 100.0,
                latency_p95_us = 500.0,
                latency_p99_us = 1000.0,
                bandwidth_mib_per_s = 10.0,
                cpu_percent_per_node = 50.0,
                memory_rss_mib_per_node = 1024.0,
                memory_heap_mib_per_conn = 0.1,
                environment = <<"test">>,
                timestamp = erlang:system_time(second),
                error_count = 0,
                actual_duration_s = Duration * 1.0
            },

            JsonMap = tcp_real_bench:metrics_to_json(Metrics),
            JsonBinary = jsx:encode(JsonMap),
            DecodedMap = jsx:decode(JsonBinary, [return_maps]),

            DecodedMap =:= JsonMap
        end).

-endif.
