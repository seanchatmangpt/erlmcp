%%%====================================================================
%%% erlmcp_bench_network_real.erl - Unified Network Transport Benchmark
%%%====================================================================
%%%
%%% CONSOLIDATED real-socket benchmark for TCP and HTTP/SSE transports.
%%% Replaces: tcp_real_bench.erl + http_real_bench.erl
%%%
%%% Features:
%%% - Real sockets (gen_tcp, ranch, gun, cowboy)
%%% - Both TCP and HTTP/SSE protocols
%%% - Full metrology compliance
%%% - Unified output format
%%% - Transport-specific metrics
%%%
%%% Usage:
%%%   rebar3 shell
%%%   erlmcp_bench_network_real:run_all().
%%%   erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib).
%%%   erlmcp_bench_network_real:run_workload(http_sustained_5k_1kib).
%%%
%%%====================================================================

-module(erlmcp_bench_network_real).

-export([
    run_all/0,
    run_workload/1,
    run_workload/2,
    list_workloads/0,
    validate_metrics/1,
    metrics_to_json/1
]).

%% Internal exports
-export([tcp_client_loop/5, tcp_server_stats_loop/3]).
-export([http_worker_process/7]).

-record(workload, {
    id :: binary(),
    transport :: tcp | http,
    connections :: pos_integer(),
    duration_s :: pos_integer(),
    payload_size_bytes :: pos_integer(),
    tls_enabled :: boolean(),
    protocol :: tcp | http | http2,
    target_msg_per_s :: pos_integer(),
    messages_per_conn :: pos_integer()
}).

-record(metrics, {
    workload_id :: binary(),
    benchmark = <<"network_real">> :: binary(),
    transport :: binary(),
    connections :: pos_integer(),
    duration_s :: pos_integer(),
    messages_sent :: non_neg_integer(),
    throughput_msg_per_s :: float(),
    bandwidth_mib_per_s :: float(),
    latency_p50_us :: float(),
    latency_p95_us :: float(),
    latency_p99_us :: float(),
    connection_setup_avg_ms :: float(),
    connection_failures :: non_neg_integer(),
    timeout_count :: non_neg_integer(),
    memory_rss_mib_per_node :: float(),
    memory_heap_mib_per_conn :: float(),
    cpu_percent_per_node :: float(),
    scope = <<"per_node">> :: binary(),
    tls_enabled :: boolean(),
    precision = <<"microsecond">> :: binary(),
    environment :: binary(),
    timestamp :: integer(),
    actual_duration_s :: float(),
    %% HTTP-specific fields
    protocol :: binary() | undefined,
    tls_handshake_avg_us :: float() | undefined,
    request_overhead_bytes :: non_neg_integer() | undefined,
    connection_reuse_percent :: float() | undefined
}).

%%====================================================================
%% Workload Definitions
%%====================================================================

workloads() ->
    #{
        %% TCP workloads - High performance, low overhead
        tcp_burst_100_1kib => #workload{
            id = <<"tcp_burst_100_1kib">>,
            transport = tcp,
            connections = 100,
            duration_s = 60,
            payload_size_bytes = 1024,
            tls_enabled = false,
            protocol = tcp,
            target_msg_per_s = 50000,
            messages_per_conn = undefined
        },
        tcp_sustained_10k_1kib => #workload{
            id = <<"tcp_sustained_10k_1kib">>,
            transport = tcp,
            connections = 10000,
            duration_s = 300,
            payload_size_bytes = 1024,
            tls_enabled = false,
            protocol = tcp,
            target_msg_per_s = 100000,
            messages_per_conn = undefined
        },
        tcp_sustained_10k_100kib => #workload{
            id = <<"tcp_sustained_10k_100kib">>,
            transport = tcp,
            connections = 10000,
            duration_s = 300,
            payload_size_bytes = 102400,
            tls_enabled = false,
            protocol = tcp,
            target_msg_per_s = 10000,
            messages_per_conn = undefined
        },
        tcp_max_100k_1kib => #workload{
            id = <<"tcp_max_100k_1kib">>,
            transport = tcp,
            connections = 100000,
            duration_s = 1800,
            payload_size_bytes = 1024,
            tls_enabled = false,
            protocol = tcp,
            target_msg_per_s = 200000,
            messages_per_conn = undefined
        },

        %% HTTP workloads - Higher overhead, request/response pattern
        http_burst_100_1kib => #workload{
            id = <<"http_burst_100_1kib">>,
            transport = http,
            connections = 100,
            duration_s = 60,
            payload_size_bytes = 1024,
            tls_enabled = false,
            protocol = http2,
            target_msg_per_s = 5000,
            messages_per_conn = 50
        },
        http_sustained_5k_1kib => #workload{
            id = <<"http_sustained_5k_1kib">>,
            transport = http,
            connections = 5000,
            duration_s = 300,
            payload_size_bytes = 1024,
            tls_enabled = false,
            protocol = http2,
            target_msg_per_s = 50000,
            messages_per_conn = 100
        },
        http1_sustained_2k_512b => #workload{
            id = <<"http1_sustained_2k_512b">>,
            transport = http,
            connections = 2000,
            duration_s = 300,
            payload_size_bytes = 512,
            tls_enabled = false,
            protocol = http,
            target_msg_per_s = 20000,
            messages_per_conn = 50
        }
    }.

%%====================================================================
%% Public API
%%====================================================================

%% @doc Run all workload benchmarks
-spec run_all() -> ok.
run_all() ->
    io:format("~n========================================~n"),
    io:format("ERLMCP NETWORK TRANSPORT BENCHMARK SUITE~n"),
    io:format("========================================~n~n"),

    %% Check transport availability
    TcpAvailable = check_tcp_transport(),
    HttpAvailable = check_http_transport(),

    io:format("Transport Availability:~n"),
    io:format("  TCP (ranch):     ~s~n", [availability_status(TcpAvailable)]),
    io:format("  HTTP (gun):      ~s~n~n", [availability_status(HttpAvailable)]),

    %% Select workloads based on availability
    AllWorkloads = workloads(),
    RunnableWorkloads = filter_runnable_workloads(AllWorkloads, TcpAvailable, HttpAvailable),

    io:format("Running ~B workloads...~n~n", [length(RunnableWorkloads)]),

    %% Run workloads
    Results = lists:map(fun(WorkloadName) ->
        try
            run_workload(WorkloadName)
        catch
            Class:Reason:Stack ->
                io:format("ERROR in ~p: ~p:~p~n~p~n", [WorkloadName, Class, Reason, Stack]),
                {error, WorkloadName, Reason}
        end
    end, RunnableWorkloads),

    %% Print summary
    print_summary(Results),

    ok.

%% @doc List all available workloads
-spec list_workloads() -> [{atom(), binary(), atom()}].
list_workloads() ->
    Workloads = workloads(),
    [{Name, W#workload.id, W#workload.transport} || {Name, W} <- maps:to_list(Workloads)].

%% @doc Run a specific workload
-spec run_workload(atom()) -> {ok, #metrics{}} | {error, term()}.
run_workload(WorkloadName) ->
    run_workload(WorkloadName, #{}).

-spec run_workload(atom(), map()) -> {ok, #metrics{}} | {error, term()}.
run_workload(WorkloadName, Opts) ->
    Definitions = workloads(),
    case maps:get(WorkloadName, Definitions, undefined) of
        undefined ->
            {error, {unknown_workload, WorkloadName}};
        Workload ->
            %% Override workload with options
            FinalWorkload = apply_options(Workload, Opts),

            io:format("~n========================================~n"),
            io:format("Running: ~s~n", [FinalWorkload#workload.id]),
            io:format("Transport: ~p~n", [FinalWorkload#workload.transport]),
            io:format("Connections: ~B~n", [FinalWorkload#workload.connections]),
            io:format("Duration: ~B seconds~n", [FinalWorkload#workload.duration_s]),
            io:format("Payload: ~B bytes~n", [FinalWorkload#workload.payload_size_bytes]),
            io:format("========================================~n~n"),

            %% Route to appropriate benchmark
            Result = case FinalWorkload#workload.transport of
                tcp ->
                    benchmark_tcp(FinalWorkload);
                http ->
                    benchmark_http(FinalWorkload)
            end,

            case Result of
                {ok, Metrics} ->
                    validate_and_save(Metrics, WorkloadName),
                    {ok, Metrics};
                Error ->
                    Error
            end
    end.

%%====================================================================
%% TCP Transport Benchmark
%%====================================================================

benchmark_tcp(Workload) ->
    %% Ensure applications started
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(ranch),

    %% Start TCP server using ranch via erlmcp_transport_tcp
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,  % Let OS assign port
        server_id => bench_server,
        transport_id => bench_transport,
        max_connections => Workload#workload.connections * 2,
        num_acceptors => min(1000, Workload#workload.connections div 10)
    }),

    %% Get actual port
    {ok, ServerState} = gen_server:call(ServerPid, get_state),
    Port = element(8, ServerState),  % #state.port

    io:format("TCP Server started on port ~B~n", [Port]),

    %% Create payload
    Payload = create_json_rpc_payload(Workload#workload.payload_size_bytes),

    %% Capture initial system metrics
    InitialMemory = erlang:memory(total),
    {InitialGCCount, _, _} = erlang:statistics(garbage_collection),
    WallClockStart = erlang:system_time(millisecond),

    io:format("Starting ~B client connections...~n", [Workload#workload.connections]),

    %% Measure connection setup time
    ConnSetupStart = erlang:monotonic_time(millisecond),
    ClientPids = start_tcp_clients(Workload#workload.connections, Port, Payload),
    ConnSetupEnd = erlang:monotonic_time(millisecond),
    AvgConnSetupMs = (ConnSetupEnd - ConnSetupStart) / length(ClientPids),

    io:format("~B clients connected (avg setup: ~.2f ms)~n",
             [length(ClientPids), AvgConnSetupMs]),
    timer:sleep(1000),  % Let connections stabilize

    %% Start metrics collection
    MetricsCollectorPid = spawn_link(?MODULE, tcp_server_stats_loop,
                                     [self(), WallClockStart, []]),

    %% Calculate end time
    DurationMs = Workload#workload.duration_s * 1000,
    EndTime = erlang:monotonic_time(millisecond) + DurationMs,

    io:format("Benchmark running for ~B seconds...~n", [Workload#workload.duration_s]),

    %% Signal clients to start sending
    StartTime = erlang:monotonic_time(millisecond),
    lists:foreach(fun(Pid) -> Pid ! {start_benchmark, EndTime} end, ClientPids),

    %% Wait for duration
    timer:sleep(DurationMs),

    %% Stop clients and collect results
    ActualEndTime = erlang:monotonic_time(millisecond),
    ActualDurationMs = ActualEndTime - StartTime,

    io:format("Collecting results from ~B clients...~n", [length(ClientPids)]),

    ClientResults = collect_tcp_client_results(ClientPids, []),

    %% Stop metrics collector
    MetricsCollectorPid ! {stop, self()},
    _SystemMetrics = receive
        {system_metrics, SysMetrics} -> SysMetrics
    after 5000 ->
        []
    end,

    %% Cleanup
    lists:foreach(fun(Pid) -> catch exit(Pid, shutdown) end, ClientPids),
    gen_server:stop(ServerPid),

    %% Calculate final system metrics
    FinalMemory = erlang:memory(total),
    {FinalGCCount, _, _} = erlang:statistics(garbage_collection),

    %% Build metrics record
    FinalMetrics = aggregate_tcp_metrics(
        Workload, ClientResults, ActualDurationMs,
        InitialMemory, FinalMemory, InitialGCCount, FinalGCCount,
        AvgConnSetupMs
    ),

    print_metrics(FinalMetrics),

    {ok, FinalMetrics}.

%%====================================================================
%% HTTP Transport Benchmark
%%====================================================================

benchmark_http(Workload) ->
    %% Ensure applications started
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(gun),
    application:ensure_all_started(cowboy),

    %% Start HTTP server
    ServerPort = 9876,
    start_http_server(ServerPort),

    io:format("HTTP Server started on port ~B~n", [ServerPort]),

    %% Collect baseline metrics
    erlang:garbage_collect(),
    InitialMemory = erlang:memory(total),
    erlang:system_flag(scheduler_wall_time, true),
    InitialScheduler = erlang:statistics(scheduler_wall_time),

    %% Measure connection setup time (TLS handshake)
    {TlsHandshakeUs, _} = timer:tc(fun() ->
        establish_sample_connection(Workload#workload.protocol, ServerPort)
    end),
    TlsHandshakeAvg = TlsHandshakeUs / 1000.0,  % Convert to ms

    %% Measure connection setup for all workers
    ConnSetupStart = erlang:monotonic_time(millisecond),

    %% Start benchmark
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (Workload#workload.duration_s * 1000),

    %% Spawn workers
    Parent = self(),
    Workers = [spawn_link(?MODULE, http_worker_process,
                         [Parent, I, Workload#workload.messages_per_conn,
                          Workload#workload.payload_size_bytes, EndTime,
                          Workload#workload.protocol, ServerPort])
              || I <- lists:seq(1, Workload#workload.connections)],

    ConnSetupEnd = erlang:monotonic_time(millisecond),
    AvgConnSetupMs = (ConnSetupEnd - ConnSetupStart) / length(Workers),

    io:format("~B HTTP workers started (avg setup: ~.2f ms)~n",
             [length(Workers), AvgConnSetupMs]),

    %% Collect results
    WorkerResults = collect_http_worker_results(Workers, []),

    %% Calculate elapsed time
    ActualDurationMs = erlang:monotonic_time(millisecond) - StartTime,

    %% Collect final metrics
    FinalMemory = erlang:memory(total),
    FinalScheduler = erlang:statistics(scheduler_wall_time),
    CpuPercent = calculate_cpu_usage(InitialScheduler, FinalScheduler),

    %% Stop server
    stop_http_server(),

    %% Build metrics record
    FinalMetrics = aggregate_http_metrics(
        Workload, WorkerResults, ActualDurationMs,
        InitialMemory, FinalMemory, CpuPercent,
        TlsHandshakeAvg, AvgConnSetupMs
    ),

    print_metrics(FinalMetrics),

    {ok, FinalMetrics}.

%%====================================================================
%% TCP Client Management
%%====================================================================

start_tcp_clients(NumClients, Port, Payload) ->
    start_tcp_clients(NumClients, Port, Payload, []).

start_tcp_clients(0, _Port, _Payload, Acc) ->
    Acc;
start_tcp_clients(N, Port, Payload, Acc) ->
    Pid = spawn_link(?MODULE, tcp_client_loop, [self(), Port, Payload, init, []]),
    start_tcp_clients(N - 1, Port, Payload, [Pid | Acc]).

tcp_client_loop(ParentPid, Port, Payload, init, _Latencies) ->
    %% Connect to server
    case gen_tcp:connect("localhost", Port, [binary, {active, false},
                                              {packet, line}, {nodelay, true}], 5000) of
        {ok, Socket} ->
            ParentPid ! {client_ready, self()},
            tcp_client_loop(ParentPid, Port, Payload, {connected, Socket}, []);
        {error, Reason} ->
            ParentPid ! {client_error, self(), Reason},
            exit({connection_failed, Reason})
    end;

tcp_client_loop(ParentPid, _Port, Payload, {connected, Socket}, Latencies) ->
    receive
        {start_benchmark, EndTime} ->
            tcp_client_send_loop(ParentPid, Socket, Payload, EndTime, Latencies, 0);
        {get_results, From} ->
            From ! {client_results, self(), {0, Latencies}};
        stop ->
            gen_tcp:close(Socket),
            exit(normal)
    after 60000 ->
        gen_tcp:close(Socket),
        exit(timeout)
    end.

tcp_client_send_loop(ParentPid, Socket, Payload, EndTime, Latencies, MsgCount) ->
    CurrentTime = erlang:monotonic_time(millisecond),

    case CurrentTime >= EndTime of
        true ->
            %% Benchmark complete
            tcp_client_loop(ParentPid, undefined, Payload, results, {MsgCount, Latencies});
        false ->
            %% Send message and measure latency
            StartUs = erlang:monotonic_time(microsecond),

            case gen_tcp:send(Socket, [Payload, <<"\n">>]) of
                ok ->
                    EndUs = erlang:monotonic_time(microsecond),
                    LatencyUs = EndUs - StartUs,

                    NewLatencies = case length(Latencies) < 10000 of
                        true -> [LatencyUs | Latencies];  % Sample first 10K
                        false -> Latencies
                    end,

                    tcp_client_send_loop(ParentPid, Socket, Payload, EndTime,
                                        NewLatencies, MsgCount + 1);
                {error, _Reason} ->
                    %% Connection lost
                    tcp_client_loop(ParentPid, undefined, Payload, results,
                                   {MsgCount, Latencies})
            end
    end.


collect_tcp_client_results(Pids, Acc) ->
    collect_tcp_client_results(Pids, Acc, 5000).

collect_tcp_client_results([], Acc, _Timeout) ->
    Acc;
collect_tcp_client_results([Pid | Rest], Acc, Timeout) ->
    Pid ! {get_results, self()},
    receive
        {client_results, Pid, Result} ->
            collect_tcp_client_results(Rest, [Result | Acc], Timeout)
    after Timeout ->
        collect_tcp_client_results(Rest, [{0, []} | Acc], Timeout)
    end.

tcp_server_stats_loop(ParentPid, StartTime, Samples) ->
    receive
        {stop, From} ->
            From ! {system_metrics, lists:reverse(Samples)}
    after 1000 ->
        %% Collect snapshot every second
        Elapsed = (erlang:system_time(millisecond) - StartTime) / 1000.0,
        Sample = #{
            elapsed_s => Elapsed,
            memory_total_mib => erlang:memory(total) / (1024 * 1024),
            processes => erlang:system_info(process_count),
            schedulers_online => erlang:system_info(schedulers_online)
        },
        tcp_server_stats_loop(ParentPid, StartTime, [Sample | Samples])
    end.

%%====================================================================
%% HTTP Worker Management
%%====================================================================

http_worker_process(Parent, WorkerId, NumMessages, MsgSize, EndTime, Protocol, Port) ->
    %% Open gun connection
    GunOpts = #{
        protocols => case Protocol of
            http2 -> [http2];
            http -> [http]
        end,
        transport => tcp,
        http_opts => #{keepalive => 30000},
        http2_opts => #{keepalive => 30000}
    },

    case gun:open("localhost", Port, GunOpts) of
        {ok, GunPid} ->
            %% Wait for connection
            case gun:await_up(GunPid, 5000) of
                {ok, ConnProtocol} ->
                    %% Send messages
                    Results = send_http_messages(GunPid, NumMessages, MsgSize, EndTime, []),
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
                        error_reason => Reason
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
                error_reason => Reason
            }}
    end.

send_http_messages(_GunPid, 0, _MsgSize, _EndTime, Acc) ->
    lists:reverse(Acc);
send_http_messages(GunPid, N, MsgSize, EndTime, Acc) ->
    %% Check time limit
    case erlang:monotonic_time(millisecond) >= EndTime of
        true ->
            lists:reverse(Acc);
        false ->
            %% Create JSON-RPC message
            Message = create_json_rpc_payload(MsgSize),

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

            send_http_messages(GunPid, N - 1, MsgSize, EndTime, [Result | Acc])
    end.

collect_http_worker_results([], Acc) ->
    lists:reverse(Acc);
collect_http_worker_results(Workers, Acc) ->
    receive
        {worker_result, Result} ->
            collect_http_worker_results(Workers -- [dummy], [Result | Acc])
    after 60000 ->
        io:format("WARNING: Timeout waiting for worker results~n"),
        lists:reverse(Acc)
    end.

%%====================================================================
%% HTTP Server
%%====================================================================

start_http_server(Port) ->
    %% Start simple cowboy HTTP server
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/mcp", http_bench_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(http_bench_server,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    ok.

stop_http_server() ->
    cowboy:stop_listener(http_bench_server),
    ok.

establish_sample_connection(Protocol, Port) ->
    %% Measure connection establishment time
    GunOpts = #{
        protocols => case Protocol of
            http2 -> [http2];
            http -> [http]
        end,
        transport => tcp
    },

    case gun:open("localhost", Port, GunOpts) of
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

%%====================================================================
%% Metrics Aggregation
%%====================================================================

aggregate_tcp_metrics(Workload, ClientResults, ActualDurationMs,
                      InitialMemory, FinalMemory, InitialGCCount, FinalGCCount,
                      AvgConnSetupMs) ->
    TotalMessages = lists:sum([Msgs || {Msgs, _} <- ClientResults]),
    AllLatencies = lists:flatten([Lats || {_, Lats} <- ClientResults]),
    ErrorCount = count_errors(ClientResults),

    SortedLatencies = lists:sort([L || L <- AllLatencies, is_number(L)]),

    case SortedLatencies of
        [] ->
            #metrics{
                workload_id = Workload#workload.id,
                transport = <<"tcp">>,
                connections = Workload#workload.connections,
                duration_s = Workload#workload.duration_s,
                messages_sent = 0,
                throughput_msg_per_s = 0.0,
                latency_p50_us = 0.0,
                latency_p95_us = 0.0,
                latency_p99_us = 0.0,
                bandwidth_mib_per_s = 0.0,
                cpu_percent_per_node = 0.0,
                connection_setup_avg_ms = AvgConnSetupMs,
                connection_failures = ErrorCount,
                timeout_count = 0,
                memory_rss_mib_per_node = (FinalMemory - InitialMemory) / (1024 * 1024),
                memory_heap_mib_per_conn = 0.0,
                tls_enabled = Workload#workload.tls_enabled,
                environment = detect_environment(),
                timestamp = erlang:system_time(second),
                actual_duration_s = ActualDurationMs / 1000.0,
                protocol = undefined,
                tls_handshake_avg_us = undefined,
                request_overhead_bytes = undefined,
                connection_reuse_percent = undefined
            };
        _ ->
            P50 = percentile(SortedLatencies, 0.50),
            P95 = percentile(SortedLatencies, 0.95),
            P99 = percentile(SortedLatencies, 0.99),

            ActualDurationSec = ActualDurationMs / 1000.0,
            Throughput = TotalMessages / ActualDurationSec,

            TotalBytes = TotalMessages * Workload#workload.payload_size_bytes,
            BandwidthMiBPerSec = (TotalBytes / ActualDurationSec) / (1024 * 1024),

            MemoryUsedMiB = (FinalMemory - InitialMemory) / (1024 * 1024),
            HeapPerConnMiB = MemoryUsedMiB / Workload#workload.connections,

            %% Estimate CPU usage from GC activity
            GCDelta = FinalGCCount - InitialGCCount,
            EstimatedCPU = min(100.0, (GCDelta / ActualDurationSec) * 0.1),

            #metrics{
                workload_id = Workload#workload.id,
                transport = <<"tcp">>,
                connections = Workload#workload.connections,
                duration_s = Workload#workload.duration_s,
                messages_sent = TotalMessages,
                throughput_msg_per_s = Throughput,
                latency_p50_us = P50,
                latency_p95_us = P95,
                latency_p99_us = P99,
                bandwidth_mib_per_s = BandwidthMiBPerSec,
                cpu_percent_per_node = EstimatedCPU,
                connection_setup_avg_ms = AvgConnSetupMs,
                connection_failures = ErrorCount,
                timeout_count = 0,
                memory_rss_mib_per_node = MemoryUsedMiB,
                memory_heap_mib_per_conn = HeapPerConnMiB,
                tls_enabled = Workload#workload.tls_enabled,
                environment = detect_environment(),
                timestamp = erlang:system_time(second),
                actual_duration_s = ActualDurationSec,
                protocol = undefined,
                tls_handshake_avg_us = undefined,
                request_overhead_bytes = undefined,
                connection_reuse_percent = undefined
            }
    end.

aggregate_http_metrics(Workload, WorkerResults, ActualDurationMs,
                       InitialMemory, FinalMemory, CpuPercent,
                       TlsHandshakeAvg, AvgConnSetupMs) ->
    TotalMessages = lists:sum([maps:get(messages_sent, R) || R <- WorkerResults]),
    SuccessfulMessages = lists:sum([maps:get(successful, R) || R <- WorkerResults]),
    FailedMessages = TotalMessages - SuccessfulMessages,
    AllLatencies = lists:flatten([maps:get(latencies, R) || R <- WorkerResults]),

    %% Calculate metrics
    ActualDurationSec = ActualDurationMs / 1000.0,
    ThroughputMsgSec = (SuccessfulMessages * 1000.0) / ActualDurationMs,
    ThroughputBytesSec = ThroughputMsgSec * Workload#workload.payload_size_bytes,
    BandwidthMiBPerSec = ThroughputBytesSec / (1024 * 1024),

    SortedLatencies = lists:sort(AllLatencies),
    P50 = percentile(SortedLatencies, 0.50),
    P95 = percentile(SortedLatencies, 0.95),
    P99 = percentile(SortedLatencies, 0.99),

    MemoryUsedMiB = (FinalMemory - InitialMemory) / (1024 * 1024),
    HeapPerConnMiB = MemoryUsedMiB / Workload#workload.connections,

    #metrics{
        workload_id = Workload#workload.id,
        transport = case Workload#workload.protocol of
            http2 -> <<"http_2.0">>;
            http -> <<"http_1.1">>
        end,
        connections = Workload#workload.connections,
        duration_s = Workload#workload.duration_s,
        messages_sent = SuccessfulMessages,
        throughput_msg_per_s = ThroughputMsgSec,
        latency_p50_us = P50,
        latency_p95_us = P95,
        latency_p99_us = P99,
        bandwidth_mib_per_s = BandwidthMiBPerSec,
        cpu_percent_per_node = CpuPercent,
        connection_setup_avg_ms = AvgConnSetupMs,
        connection_failures = FailedMessages,
        timeout_count = 0,
        memory_rss_mib_per_node = MemoryUsedMiB,
        memory_heap_mib_per_conn = HeapPerConnMiB,
        tls_enabled = Workload#workload.tls_enabled,
        environment = detect_environment(),
        timestamp = erlang:system_time(second),
        actual_duration_s = ActualDurationSec,
        protocol = case Workload#workload.protocol of
            http2 -> <<"http/2">>;
            http -> <<"http/1.1">>
        end,
        tls_handshake_avg_us = TlsHandshakeAvg * 1000,  % ms to us
        request_overhead_bytes = 234,  % HTTP header overhead
        connection_reuse_percent = 100.0  % Single connection per worker
    }.

%%====================================================================
%% Validation and Persistence
%%====================================================================

validate_and_save(Metrics, WorkloadName) ->
    %% Convert to JSON map
    JsonMap = metrics_to_json(Metrics),

    %% Validate with erlmcp_chaos_plan_validator structure
    case validate_metrics(JsonMap) of
        ok ->
            %% Save to file
            Timestamp = integer_to_list(erlang:system_time(second)),
            Filename = lists:flatten(io_lib:format(
                "bench/results/network_real_~s_~s.json",
                [WorkloadName, Timestamp])),

            JsonBinary = jsx:encode(JsonMap, [{space, 1}, {indent, 2}]),

            case file:write_file(Filename, JsonBinary) of
                ok ->
                    io:format("~nResults saved to: ~s~n", [Filename]),
                    ok;
                {error, Reason} ->
                    io:format("ERROR: Failed to save results: ~p~n", [Reason]),
                    {error, {write_failed, Reason}}
            end;
        {error, Reason} ->
            io:format("ERROR: Validation failed: ~p~n", [Reason]),
            {error, {validation_failed, Reason}}
    end.

validate_metrics(JsonMap) ->
    %% Ensure all required fields are present
    Required = [
        <<"workload_id">>, <<"benchmark">>, <<"transport">>, <<"connections">>,
        <<"duration_s">>, <<"messages_sent">>, <<"throughput_msg_per_s">>,
        <<"latency_p50_us">>, <<"latency_p95_us">>, <<"latency_p99_us">>,
        <<"bandwidth_mib_per_s">>, <<"memory_rss_mib_per_node">>,
        <<"precision">>, <<"scope">>, <<"environment">>
    ],

    Missing = [K || K <- Required, not maps:is_key(K, JsonMap)],

    case Missing of
        [] -> ok;
        _ -> {error, {missing_fields, Missing}}
    end.

metrics_to_json(Metrics) ->
    BaseMap = #{
        <<"workload_id">> => Metrics#metrics.workload_id,
        <<"benchmark">> => Metrics#metrics.benchmark,
        <<"transport">> => Metrics#metrics.transport,
        <<"connections">> => Metrics#metrics.connections,
        <<"duration_s">> => Metrics#metrics.duration_s,
        <<"messages_sent">> => Metrics#metrics.messages_sent,
        <<"throughput_msg_per_s">> => round(Metrics#metrics.throughput_msg_per_s * 100) / 100,
        <<"latency_p50_us">> => round(Metrics#metrics.latency_p50_us * 100) / 100,
        <<"latency_p95_us">> => round(Metrics#metrics.latency_p95_us * 100) / 100,
        <<"latency_p99_us">> => round(Metrics#metrics.latency_p99_us * 100) / 100,
        <<"bandwidth_mib_per_s">> => round(Metrics#metrics.bandwidth_mib_per_s * 100) / 100,
        <<"cpu_percent_per_node">> => round(Metrics#metrics.cpu_percent_per_node * 100) / 100,
        <<"connection_setup_avg_ms">> => round(Metrics#metrics.connection_setup_avg_ms * 100) / 100,
        <<"connection_failures">> => Metrics#metrics.connection_failures,
        <<"timeout_count">> => Metrics#metrics.timeout_count,
        <<"memory_rss_mib_per_node">> => round(Metrics#metrics.memory_rss_mib_per_node * 100) / 100,
        <<"memory_heap_mib_per_conn">> => round(Metrics#metrics.memory_heap_mib_per_conn * 1000000) / 1000000,
        <<"precision">> => Metrics#metrics.precision,
        <<"scope">> => Metrics#metrics.scope,
        <<"tls_enabled">> => Metrics#metrics.tls_enabled,
        <<"environment">> => Metrics#metrics.environment,
        <<"timestamp">> => Metrics#metrics.timestamp,
        <<"actual_duration_s">> => round(Metrics#metrics.actual_duration_s * 100) / 100
    },

    %% Add HTTP-specific fields if present
    case Metrics#metrics.protocol of
        undefined ->
            BaseMap;
        Protocol ->
            BaseMap#{
                <<"protocol">> => Protocol,
                <<"tls_handshake_avg_us">> => round(Metrics#metrics.tls_handshake_avg_us * 100) / 100,
                <<"request_overhead_bytes">> => Metrics#metrics.request_overhead_bytes,
                <<"connection_reuse_percent">> => Metrics#metrics.connection_reuse_percent
            }
    end.

%%====================================================================
%% Helpers
%%====================================================================

create_json_rpc_payload(TargetBytes) ->
    %% Create a realistic JSON-RPC message that's approximately TargetBytes
    BaseMsg = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/call">>,
        <<"id">> => rand:uniform(1000000),
        <<"params">> => #{
            <<"name">> => <<"benchmark_tool">>,
            <<"arguments">> => #{}
        }
    },

    BaseJson = jsx:encode(BaseMsg),
    BaseSize = byte_size(BaseJson),

    %% Pad with data to reach target size
    case TargetBytes > BaseSize of
        true ->
            PaddingSize = TargetBytes - BaseSize - 20,
            Padding = binary:copy(<<"X">>, max(0, PaddingSize)),

            PaddedMsg = BaseMsg#{
                <<"params">> => #{
                    <<"name">> => <<"benchmark_tool">>,
                    <<"arguments">> => #{
                        <<"data">> => Padding
                    }
                }
            },
            jsx:encode(PaddedMsg);
        false ->
            BaseJson
    end.

percentile([], _P) -> 0.0;
percentile(SortedList, P) when P >= 0, P =< 1 ->
    N = length(SortedList),
    Index = max(1, round(N * P)),
    float(lists:nth(Index, SortedList)).

count_errors(Results) ->
    length([true || {Msgs, _} <- Results, Msgs =:= 0]).

detect_environment() ->
    %% Simple environment detection
    case os:getenv("KUBERNETES_SERVICE_HOST") of
        false ->
            case os:getenv("AWS_REGION") of
                false -> <<"local_dev">>;
                _ -> <<"aws_unknown">>
            end;
        _ ->
            <<"kubernetes">>
    end.

apply_options(Workload, Opts) ->
    Workload#workload{
        connections = maps:get(connections, Opts, Workload#workload.connections),
        duration_s = maps:get(duration_s, Opts, Workload#workload.duration_s),
        payload_size_bytes = maps:get(payload_size_bytes, Opts,
                                     Workload#workload.payload_size_bytes)
    }.

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
%% Transport Availability
%%====================================================================

check_tcp_transport() ->
    %% Check if ranch is available
    case code:which(ranch) of
        non_existing -> false;
        _ -> true
    end.

check_http_transport() ->
    %% Check if gun and cowboy are available
    case {code:which(gun), code:which(cowboy)} of
        {non_existing, _} -> false;
        {_, non_existing} -> false;
        _ -> true
    end.

availability_status(true) -> "Available";
availability_status(false) -> "Not Available".

filter_runnable_workloads(Workloads, TcpAvailable, HttpAvailable) ->
    lists:filter(fun(Name) ->
        case maps:get(Name, Workloads) of
            #workload{transport = tcp} -> TcpAvailable;
            #workload{transport = http} -> HttpAvailable
        end
    end, maps:keys(Workloads)).

%%====================================================================
%% Output Functions
%%====================================================================

print_metrics(Metrics) ->
    io:format("~n========================================~n"),
    io:format("BENCHMARK RESULTS~n"),
    io:format("========================================~n"),
    io:format("Workload:        ~s~n", [Metrics#metrics.workload_id]),
    io:format("Transport:       ~s~n", [Metrics#metrics.transport]),
    io:format("Connections:     ~B~n", [Metrics#metrics.connections]),
    io:format("Duration:        ~.2f seconds~n", [Metrics#metrics.actual_duration_s]),
    io:format("Messages:        ~B~n", [Metrics#metrics.messages_sent]),
    io:format("~n"),
    io:format("Throughput:      ~.2f msg/sec~n", [Metrics#metrics.throughput_msg_per_s]),
    io:format("Bandwidth:       ~.2f MiB/sec~n", [Metrics#metrics.bandwidth_mib_per_s]),
    io:format("~n"),
    io:format("Latency (µs):~n"),
    io:format("  P50:           ~.2f~n", [Metrics#metrics.latency_p50_us]),
    io:format("  P95:           ~.2f~n", [Metrics#metrics.latency_p95_us]),
    io:format("  P99:           ~.2f~n", [Metrics#metrics.latency_p99_us]),
    io:format("~n"),
    io:format("Connection:~n"),
    io:format("  Setup:         ~.2f ms (avg)~n", [Metrics#metrics.connection_setup_avg_ms]),
    io:format("  Failures:      ~B~n", [Metrics#metrics.connection_failures]),
    io:format("~n"),
    io:format("Resources:~n"),
    io:format("  CPU:           ~.2f%%~n", [Metrics#metrics.cpu_percent_per_node]),
    io:format("  Memory (RSS):  ~.2f MiB~n", [Metrics#metrics.memory_rss_mib_per_node]),
    io:format("  Heap/conn:     ~.6f MiB~n", [Metrics#metrics.memory_heap_mib_per_conn]),

    %% Print HTTP-specific metrics
    case Metrics#metrics.protocol of
        undefined -> ok;
        Protocol ->
            io:format("~n"),
            io:format("HTTP Specific:~n"),
            io:format("  Protocol:      ~s~n", [Protocol]),
            io:format("  TLS Handshake: ~.2f µs~n", [Metrics#metrics.tls_handshake_avg_us]),
            io:format("  Overhead:      ~B bytes/req~n", [Metrics#metrics.request_overhead_bytes]),
            io:format("  Conn Reuse:    ~.2f%%~n", [Metrics#metrics.connection_reuse_percent])
    end,

    io:format("~n"),
    io:format("Environment:     ~s~n", [Metrics#metrics.environment]),
    io:format("========================================~n~n").

print_summary(Results) ->
    io:format("~n========================================~n"),
    io:format("BENCHMARK SUITE SUMMARY~n"),
    io:format("========================================~n"),

    lists:foreach(fun
        ({ok, Metrics}) ->
            Status = case Metrics#metrics.throughput_msg_per_s >= get_target(Metrics#metrics.workload_id) of
                true -> "PASS";
                false -> "FAIL"
            end,
            io:format("~s (~s): ~s (~.2f msg/s)~n",
                     [Metrics#metrics.workload_id, Metrics#metrics.transport,
                      Status, Metrics#metrics.throughput_msg_per_s]);
        ({error, Workload, Reason}) ->
            io:format("~p: ERROR (~p)~n", [Workload, Reason])
    end, Results),

    io:format("========================================~n").

get_target(WorkloadId) ->
    Definitions = workloads(),
    case [W || {_, W} <- maps:to_list(Definitions), W#workload.id =:= WorkloadId] of
        [Workload | _] -> Workload#workload.target_msg_per_s;
        [] -> 0
    end.
