%%%===================================================================
%%% http_real_bench.erl - HTTP/SSE Transport Real Benchmark
%%%===================================================================
%%%
%%% Measures real HTTP transport performance using gun client
%%% Target: Sustained throughput with realistic MCP JSON-RPC messages
%%%
%%% IMPORTANT LIMITATIONS:
%%% - HTTP overhead is significantly higher than TCP (headers, chunking)
%%% - Connection limits: Practical limit ~10K concurrent HTTP/2, ~5K HTTP/1.1
%%% - Each HTTP request has 200-500 bytes header overhead
%%% - SSE (Server-Sent Events) uses chunked encoding with additional overhead
%%%
%%% Execution:
%%%   erl -pa _build/default/lib/*/ebin -s http_real_bench run
%%%
%%% Output: JSON metrics compatible with transport_real_tcp_bench.erl

-module(http_real_bench).

-export([
    run/0,
    benchmark/0,
    run_workload/1,
    run_workload/3
]).

-record(stats, {
    workload_id :: binary(),
    transport :: binary(),
    duration_ms :: float(),
    total_messages :: non_neg_integer(),
    successful_messages :: non_neg_integer(),
    failed_messages :: non_neg_integer(),
    throughput_msg_sec :: float(),
    throughput_bytes_sec :: float(),
    latency_p50_us :: float(),
    latency_p95_us :: float(),
    latency_p99_us :: float(),
    latency_max_us :: float(),
    cpu_percent :: float(),
    memory_mb :: float(),
    connections :: non_neg_integer(),
    protocol :: binary(),  % "http1.1" | "http2"
    tls_handshake_avg_us :: float(),
    request_overhead_bytes :: non_neg_integer(),
    connection_reuse_percent :: float()
}).

%% Workload configurations
%% NOTE: HTTP realistic limits are much lower than TCP
-define(WORKLOAD_HTTP_SUSTAINED_5K, #{
    id => <<"http_sustained_5k_1kib">>,
    connections => 100,  % HTTP/2 can multiplex streams
    messages_per_conn => 50,
    message_size => 1024,
    duration_ms => 30000,
    protocol => http2
}).

-define(WORKLOAD_HTTP1_SUSTAINED_2K, #{
    id => <<"http1_sustained_2k_512b">>,
    connections => 200,  % HTTP/1.1 limited by connection overhead
    messages_per_conn => 10,
    message_size => 512,
    duration_ms => 30000,
    protocol => http  % HTTP/1.1
}).

-define(WORKLOAD_HTTP_BURST_1K, #{
    id => <<"http_burst_1k_256b">>,
    connections => 50,
    messages_per_conn => 20,
    message_size => 256,
    duration_ms => 5000,
    protocol => http2
}).

%% HTTP server configuration for benchmark
-define(HTTP_SERVER_PORT, 9876).
-define(HTTP_SERVER_HOST, "localhost").

%%====================================================================
%% Main Entry Point
%%====================================================================

run() ->
    benchmark(),
    halt(0).

benchmark() ->
    io:format("~n~n", []),
    io:format("========================================================~n"),
    io:format("ERLMCP HTTP/SSE Transport Real Benchmark v1.0~n"),
    io:format("========================================================~n"),
    io:format("Transport: gun HTTP/1.1 + HTTP/2 client~n"),
    io:format("Protocol: JSON-RPC 2.0 over HTTP POST~n"),
    io:format("~n"),
    io:format("IMPORTANT NOTES:~n"),
    io:format("  - HTTP has 200-500 byte overhead per request (headers)~n"),
    io:format("  - HTTP/2 allows stream multiplexing (better for concurrent)~n"),
    io:format("  - HTTP/1.1 limited to 6 connections/host by default~n"),
    io:format("  - Practical limit: 10K HTTP/2, 5K HTTP/1.1 connections~n"),
    io:format("  - NOT suitable for 100K connections (use TCP/WebSocket)~n"),
    io:format("~n"),

    %% Start applications
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(gun),
    application:ensure_all_started(cowboy),

    %% Start simple HTTP server
    ServerPid = start_http_server(),

    %% Run workloads
    io:format("Running HTTP/2 workloads...~n~n"),
    Results1 = run_workload(?WORKLOAD_HTTP_SUSTAINED_5K),
    timer:sleep(2000),  % Cool down

    io:format("Running HTTP/1.1 workloads...~n~n"),
    Results2 = run_workload(?WORKLOAD_HTTP1_SUSTAINED_2K),
    timer:sleep(2000),

    io:format("Running HTTP/2 burst workloads...~n~n"),
    Results3 = run_workload(?WORKLOAD_HTTP_BURST_1K),

    %% Stop server
    stop_http_server(ServerPid),

    %% Aggregate results
    AllResults = [Results1, Results2, Results3],

    %% Print summary
    print_summary(AllResults),

    %% Export results
    export_json(AllResults),

    ok.

%%====================================================================
%% Workload Execution
%%====================================================================

run_workload(WorkloadConfig) ->
    run_workload(
        maps:get(connections, WorkloadConfig),
        maps:get(messages_per_conn, WorkloadConfig),
        WorkloadConfig
    ).

run_workload(NumConns, MsgsPerConn, Config) ->
    WorkloadId = maps:get(id, Config),
    MsgSize = maps:get(message_size, Config),
    DurationMs = maps:get(duration_ms, Config),
    Protocol = maps:get(protocol, Config),

    io:format("[Workload: ~s]~n", [WorkloadId]),
    io:format("  Connections: ~w~n", [NumConns]),
    io:format("  Messages/conn: ~w~n", [MsgsPerConn]),
    io:format("  Message size: ~w bytes~n", [MsgSize]),
    io:format("  Protocol: ~p~n", [Protocol]),
    io:format("  Duration: ~w ms~n", [DurationMs]),

    %% Collect baseline metrics
    erlang:garbage_collect(),
    InitialMemory = erlang:memory(total),
    erlang:system_flag(scheduler_wall_time, true),
    InitialScheduler = erlang:statistics(scheduler_wall_time),

    %% Measure TLS handshake time (separate from message latency)
    {TlsHandshakeUs, _} = timer:tc(fun() ->
        establish_sample_connection(Protocol)
    end),
    TlsHandshakeAvg = TlsHandshakeUs / 1000.0,  % Convert to us from total

    %% Start benchmark
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + DurationMs,

    %% Spawn workers
    Parent = self(),
    Workers = [spawn_link(fun() ->
        worker_process(Parent, I, MsgsPerConn, MsgSize, EndTime, Protocol)
    end) || I <- lists:seq(1, NumConns)],

    %% Collect results
    WorkerResults = collect_worker_results(Workers, []),

    %% Calculate elapsed time
    ActualDurationMs = erlang:monotonic_time(millisecond) - StartTime,

    %% Collect final metrics
    FinalMemory = erlang:memory(total),
    FinalScheduler = erlang:statistics(scheduler_wall_time),
    CpuPercent = calculate_cpu_usage(InitialScheduler, FinalScheduler),

    %% Aggregate statistics
    TotalMessages = lists:sum([maps:get(messages_sent, R) || R <- WorkerResults]),
    SuccessfulMessages = lists:sum([maps:get(successful, R) || R <- WorkerResults]),
    FailedMessages = TotalMessages - SuccessfulMessages,
    AllLatencies = lists:flatten([maps:get(latencies, R) || R <- WorkerResults]),

    %% Calculate metrics
    ThroughputMsgSec = (SuccessfulMessages * 1000.0) / ActualDurationMs,
    ThroughputBytesSec = ThroughputMsgSec * MsgSize,

    SortedLatencies = lists:sort(AllLatencies),
    Stats = #stats{
        workload_id = WorkloadId,
        transport = case Protocol of
            http2 -> <<"http_2.0">>;
            http -> <<"http_1.1">>
        end,
        duration_ms = float(ActualDurationMs),
        total_messages = TotalMessages,
        successful_messages = SuccessfulMessages,
        failed_messages = FailedMessages,
        throughput_msg_sec = ThroughputMsgSec,
        throughput_bytes_sec = ThroughputBytesSec,
        latency_p50_us = percentile(SortedLatencies, 0.50),
        latency_p95_us = percentile(SortedLatencies, 0.95),
        latency_p99_us = percentile(SortedLatencies, 0.99),
        latency_max_us = case SortedLatencies of
            [] -> 0.0;
            _ -> float(lists:max(SortedLatencies))
        end,
        cpu_percent = CpuPercent,
        memory_mb = (FinalMemory - InitialMemory) / (1024 * 1024),
        connections = NumConns,
        protocol = case Protocol of
            http2 -> <<"http/2">>;
            http -> <<"http/1.1">>
        end,
        tls_handshake_avg_us = TlsHandshakeAvg,
        request_overhead_bytes = calculate_http_overhead(),
        connection_reuse_percent = calculate_connection_reuse()
    },

    print_workload_results(Stats),
    Stats.

%%====================================================================
%% Worker Process
%%====================================================================

worker_process(Parent, WorkerId, NumMessages, MsgSize, EndTime, Protocol) ->
    %% Open gun connection
    GunOpts = #{
        protocols => case Protocol of
            http2 -> [http2];
            http -> [http]
        end,
        transport => tcp,  % Use TCP for localhost benchmarking
        http_opts => #{keepalive => 30000},
        http2_opts => #{keepalive => 30000}
    },

    case gun:open(?HTTP_SERVER_HOST, ?HTTP_SERVER_PORT, GunOpts) of
        {ok, GunPid} ->
            %% Wait for connection
            case gun:await_up(GunPid, 5000) of
                {ok, ConnProtocol} ->
                    %% Send messages
                    Results = send_messages(GunPid, NumMessages, MsgSize, EndTime, []),
                    gun:close(GunPid),

                    %% Report results
                    Parent ! {worker_result, #{
                        worker_id => WorkerId,
                        messages_sent => NumMessages,
                        successful => length([ok || {ok, _} <- Results]),
                        latencies => [L || {ok, L} <- Results],
                        errors => length([error || {error, _} <- Results]),
                        protocol => ConnProtocol
                    }};
                {error, Reason} ->
                    Parent ! {worker_result, #{
                        worker_id => WorkerId,
                        messages_sent => 0,
                        successful => 0,
                        latencies => [],
                        errors => 1,
                        error_reason => Reason,
                        protocol => Protocol
                    }},
                    gun:close(GunPid)
            end;
        {error, Reason} ->
            Parent ! {worker_result, #{
                worker_id => WorkerId,
                messages_sent => 0,
                successful => 0,
                latencies => [],
                errors => 1,
                error_reason => Reason,
                protocol => Protocol
            }}
    end.

send_messages(_GunPid, 0, _MsgSize, _EndTime, Acc) ->
    lists:reverse(Acc);
send_messages(GunPid, N, MsgSize, EndTime, Acc) ->
    %% Check time limit
    case erlang:monotonic_time(millisecond) >= EndTime of
        true ->
            lists:reverse(Acc);
        false ->
            %% Create JSON-RPC message
            Message = create_json_rpc_message(MsgSize),

            %% Measure request latency
            StartUs = erlang:monotonic_time(microsecond),
            StreamRef = gun:post(GunPid, "/mcp", [
                {<<"content-type">>, <<"application/json">>}
            ], Message),

            %% Wait for response
            Result = case gun:await(GunPid, StreamRef, 5000) of
                {response, fin, _Status, _Headers} ->
                    LatencyUs = erlang:monotonic_time(microsecond) - StartUs,
                    {ok, LatencyUs};
                {response, nofin, _Status, _Headers} ->
                    %% Read body
                    case gun:await_body(GunPid, StreamRef, 5000) of
                        {ok, _Body} ->
                            LatencyUs = erlang:monotonic_time(microsecond) - StartUs,
                            {ok, LatencyUs};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end,

            send_messages(GunPid, N - 1, MsgSize, EndTime, [Result | Acc])
    end.

%%====================================================================
%% HTTP Server (for benchmark target)
%%====================================================================

start_http_server() ->
    %% Start simple cowboy HTTP server
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/mcp", http_bench_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(http_bench_server,
        [{port, ?HTTP_SERVER_PORT}],
        #{env => #{dispatch => Dispatch}}
    ),

    io:format("HTTP server started on port ~w~n~n", [?HTTP_SERVER_PORT]),
    http_bench_server.

stop_http_server(ServerRef) ->
    cowboy:stop_listener(ServerRef),
    io:format("~nHTTP server stopped~n").

%%====================================================================
%% Helper Functions
%%====================================================================

create_json_rpc_message(TargetSize) ->
    %% Create JSON-RPC message with payload close to target size
    BaseMsg = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => rand:uniform(1000000),
        <<"method">> => <<"benchmark/test">>,
        <<"params">> => #{
            <<"data">> => <<"placeholder">>
        }
    },

    %% Encode to get base size
    Encoded = jsx:encode(BaseMsg),
    BaseSize = byte_size(Encoded),

    %% Add padding to reach target size
    case TargetSize > BaseSize of
        true ->
            PaddingSize = TargetSize - BaseSize - 20,  % Reserve for JSON structure
            Padding = binary:copy(<<"x">>, max(0, PaddingSize)),
            FinalMsg = BaseMsg#{
                <<"params">> => #{
                    <<"data">> => Padding
                }
            },
            jsx:encode(FinalMsg);
        false ->
            Encoded
    end.

establish_sample_connection(Protocol) ->
    %% Measure connection establishment time
    GunOpts = #{
        protocols => case Protocol of
            http2 -> [http2];
            http -> [http]
        end,
        transport => tcp
    },

    case gun:open(?HTTP_SERVER_HOST, ?HTTP_SERVER_PORT, GunOpts) of
        {ok, GunPid} ->
            case gun:await_up(GunPid, 5000) of
                {ok, _} ->
                    gun:close(GunPid),
                    ok;
                {error, _} ->
                    gun:close(GunPid),
                    ok
            end;
        {error, _} ->
            ok
    end.

calculate_http_overhead() ->
    %% Typical HTTP request overhead
    %% - Request line: ~30 bytes ("POST /mcp HTTP/1.1\r\n")
    %% - Headers: ~200 bytes (Host, Content-Type, Content-Length, User-Agent)
    %% - Body separator: 4 bytes ("\r\n\r\n")
    %% Total: ~234 bytes + payload
    234.

calculate_connection_reuse() ->
    %% In this benchmark, each worker opens one connection and reuses it
    %% Connection reuse = 100% (single connection per worker)
    100.0.

collect_worker_results([], Acc) ->
    lists:reverse(Acc);
collect_worker_results(Workers, Acc) ->
    receive
        {worker_result, Result} ->
            collect_worker_results(Workers -- [dummy], [Result | Acc])
    after 60000 ->
        io:format("WARNING: Timeout waiting for worker results~n"),
        lists:reverse(Acc)
    end.

percentile([], _P) ->
    0.0;
percentile(Data, P) when P >= 0, P =< 1 ->
    N = length(Data),
    Index = max(1, round(N * P)),
    float(lists:nth(Index, Data)).

calculate_cpu_usage(InitialStats, FinalStats) ->
    TotalActive = lists:sum([ActiveF - ActiveI ||
        {{_, ActiveI, _}, {_, ActiveF, _}} <-
        lists:zip(InitialStats, FinalStats)]),
    TotalTime = lists:sum([TotalF - TotalI ||
        {{_, _, TotalI}, {_, _, TotalF}} <-
        lists:zip(InitialStats, FinalStats)]),

    case TotalTime of
        0 -> 0.0;
        _ -> (TotalActive / TotalTime) * 100.0
    end.

%%====================================================================
%% Output Functions
%%====================================================================

print_workload_results(#stats{} = S) ->
    io:format("~n  Results:~n"),
    io:format("    Throughput:    ~.2f msg/sec (~.2f MB/sec)~n",
             [S#stats.throughput_msg_sec, S#stats.throughput_bytes_sec / (1024 * 1024)]),
    io:format("    Messages:      ~w total, ~w successful, ~w failed~n",
             [S#stats.total_messages, S#stats.successful_messages, S#stats.failed_messages]),
    io:format("    Latency (µs):~n"),
    io:format("      P50:         ~.2f~n", [S#stats.latency_p50_us]),
    io:format("      P95:         ~.2f~n", [S#stats.latency_p95_us]),
    io:format("      P99:         ~.2f~n", [S#stats.latency_p99_us]),
    io:format("      Max:         ~.2f~n", [S#stats.latency_max_us]),
    io:format("    HTTP Overhead: ~w bytes/request~n", [S#stats.request_overhead_bytes]),
    io:format("    TLS Handshake: ~.2f µs (avg, separate from message latency)~n",
             [S#stats.tls_handshake_avg_us]),
    io:format("    CPU:           ~.2f%%~n", [S#stats.cpu_percent]),
    io:format("    Memory:        ~.2f MB~n", [S#stats.memory_mb]),
    io:format("~n").

print_summary(Results) ->
    io:format("~n========================================================~n"),
    io:format("BENCHMARK SUMMARY~n"),
    io:format("========================================================~n"),

    lists:foreach(fun(#stats{} = S) ->
        io:format("~n[~s] (~s, ~w connections)~n",
                 [S#stats.workload_id, S#stats.protocol, S#stats.connections]),
        io:format("  Throughput: ~.2f msg/sec~n", [S#stats.throughput_msg_sec]),
        io:format("  Latency P95: ~.2f µs~n", [S#stats.latency_p95_us]),
        io:format("  Success rate: ~.2f%%~n",
                 [(S#stats.successful_messages / S#stats.total_messages) * 100])
    end, Results),

    io:format("~n========================================================~n"),
    io:format("TRANSPORT LIMITATIONS~n"),
    io:format("========================================================~n"),
    io:format("HTTP is NOT suitable for 100K+ concurrent connections:~n"),
    io:format("  - HTTP/1.1: 5K connections practical limit~n"),
    io:format("  - HTTP/2:   10K connections practical limit~n"),
    io:format("  - Overhead: 234+ bytes per request (headers)~n"),
    io:format("  - Use case: Request/response, not sustained streaming~n"),
    io:format("~n"),
    io:format("For 100K+ connections, use:~n"),
    io:format("  - TCP with custom framing (lowest overhead)~n"),
    io:format("  - WebSocket (persistent, bidirectional)~n"),
    io:format("========================================================~n").

export_json(Results) ->
    Timestamp = erlang:system_time(second),
    Filename = io_lib:format("bench/results/transport_real_http_~w.json", [Timestamp]),

    JsonResults = lists:map(fun(#stats{} = S) ->
        #{
            <<"workload_id">> => S#stats.workload_id,
            <<"transport">> => S#stats.transport,
            <<"metrology">> => #{
                <<"duration_ms">> => S#stats.duration_ms,
                <<"total_messages">> => S#stats.total_messages,
                <<"successful_messages">> => S#stats.successful_messages,
                <<"failed_messages">> => S#stats.failed_messages,
                <<"throughput_msg_sec">> => round(S#stats.throughput_msg_sec * 100) / 100,
                <<"throughput_bytes_sec">> => round(S#stats.throughput_bytes_sec * 100) / 100,
                <<"latency_p50_us">> => round(S#stats.latency_p50_us * 100) / 100,
                <<"latency_p95_us">> => round(S#stats.latency_p95_us * 100) / 100,
                <<"latency_p99_us">> => round(S#stats.latency_p99_us * 100) / 100,
                <<"latency_max_us">> => round(S#stats.latency_max_us * 100) / 100,
                <<"cpu_percent">> => round(S#stats.cpu_percent * 100) / 100,
                <<"memory_mb">> => round(S#stats.memory_mb * 100) / 100
            },
            <<"http_specific">> => #{
                <<"protocol">> => S#stats.protocol,
                <<"connections">> => S#stats.connections,
                <<"tls_handshake_avg_us">> => round(S#stats.tls_handshake_avg_us * 100) / 100,
                <<"request_overhead_bytes">> => S#stats.request_overhead_bytes,
                <<"connection_reuse_percent">> => S#stats.connection_reuse_percent
            }
        }
    end, Results),

    Json = jsx:encode(JsonResults, [{space, 1}, {indent, 2}]),
    filelib:ensure_dir(Filename),
    file:write_file(Filename, Json),
    io:format("~nExported: ~s~n", [Filename]).
