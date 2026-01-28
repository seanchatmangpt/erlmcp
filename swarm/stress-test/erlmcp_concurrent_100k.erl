%%%-------------------------------------------------------------------
%%% @doc
%%% ErlMCP 100K Concurrent Connection Stress Test
%%%
%%% Generates 100,000 concurrent connections to erlmcp servers running
%%% in Docker Swarm and validates real-time performance metrics.
%%%
%%% Usage:
%%%   erlc erlmcp_concurrent_100k.erl
%%%   erl -noshell -run erlmcp_concurrent_100k stress_test http://localhost:8080 100000 5 -s init stop
%%%
%%% Parameters:
%%%   URL: Target server URL (e.g., http://localhost:8080)
%%%   Connections: Number of concurrent connections (e.g., 100000)
%%%   Duration: Test duration in minutes (e.g., 5)
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_concurrent_100k).

-export([stress_test/3, stress_test/4]).

-record(test_state, {
    target_url :: string(),
    num_connections :: integer(),
    duration_seconds :: integer(),
    start_time :: integer(),
    connections = [] :: list(),
    metrics = #{
        requests_sent = 0,
        requests_success = 0,
        requests_failed = 0,
        responses_received = 0,
        errors = 0,
        total_latency_ms = 0,
        min_latency_ms = 999999,
        max_latency_ms = 0,
        bytes_sent = 0,
        bytes_received = 0
    } :: map()
}).

%% Main entry point for stress test
stress_test(URL, Connections, Duration) ->
    stress_test(URL, Connections, Duration, #test_state{
        target_url = URL,
        num_connections = Connections,
        duration_seconds = Duration * 60
    }).

stress_test(URL, Connections, DurationMinutes, State) ->
    io:format("~n=== ErlMCP 100K Concurrent Connection Stress Test ===~n", []),
    io:format("Target URL: ~s~n", [URL]),
    io:format("Number of Connections: ~w~n", [Connections]),
    io:format("Duration: ~w minutes (~w seconds)~n", [DurationMinutes, DurationMinutes * 60]),
    io:format("Start Time: ~w~n", [erlang:system_time(millisecond)]),
    io:format("~n", []),

    % Parse URL to get host and port
    {Host, Port} = parse_url(URL),

    % Start HTTP client process pool
    io:format("Initializing connection pool...~n", []),
    MaxConnections = min(Connections, 10000),
    start_connection_pool(MaxConnections, Host, Port),

    % Start metrics collector
    io:format("Starting metrics collection...~n", []),
    start_metrics_server(),

    % Launch connections in batches
    io:format("Launching ~w concurrent connections...~n", [Connections]),
    spawn_connections(Connections, Host, Port),

    % Run test for specified duration
    start_time_ts(Connections, DurationMinutes * 60, Host, Port).

%% Parse URL to extract host and port
parse_url(URL) ->
    case http_uri:parse(URL) of
        {ok, {_Scheme, _User, Host, Port, _Path, _Query}} ->
            {Host, Port};
        {ok, {_Scheme, _User, Host, Port, _Path}} ->
            {Host, Port};
        _ ->
            % Default parse
            case string:split(URL, "://") of
                [_, HostPort] ->
                    case string:split(HostPort, ":") of
                        [H, P] -> {H, list_to_integer(P)};
                        [H] -> {H, 80}
                    end;
                _ -> {"localhost", 8080}
            end
    end.

%% Connection pool supervisor
start_connection_pool(MaxConnections, Host, Port) ->
    register(connection_pool, spawn(fun() ->
        connection_pool_loop(MaxConnections, Host, Port, [])
    end)).

connection_pool_loop(Max, Host, Port, Pids) ->
    receive
        {get_connection, From} ->
            case length(Pids) < Max of
                true ->
                    {ok, Conn} = establish_connection(Host, Port),
                    From ! {connection, Conn},
                    connection_pool_loop(Max, Host, Port, [Conn | Pids]);
                false ->
                    % Reuse a connection from pool
                    case Pids of
                        [Conn | Rest] ->
                            From ! {connection, Conn},
                            connection_pool_loop(Max, Host, Port, Rest ++ [Conn]);
                        [] ->
                            {ok, Conn} = establish_connection(Host, Port),
                            From ! {connection, Conn},
                            connection_pool_loop(Max, Host, Port, [Conn])
                    end
            end;
        {return_connection, Conn} ->
            connection_pool_loop(Max, Host, Port, [Conn | Pids]);
        stop ->
            lists:foreach(fun(Conn) -> catch http:close(Conn) end, Pids),
            ok;
        _ ->
            connection_pool_loop(Max, Host, Port, Pids)
    end.

%% Establish TCP connection
establish_connection(Host, Port) ->
    case gen_tcp:connect(Host, Port, [
        binary,
        {packet, raw},
        {nodelay, true},
        {recbuf, 65536},
        {sndbuf, 65536},
        {keepalive, true}
    ]) of
        {ok, Socket} -> {ok, Socket};
        {error, _} -> {error, connection_failed}
    end.

%% Metrics server
start_metrics_server() ->
    register(metrics_server, spawn(fun() ->
        metrics_loop(#{})
    end)).

metrics_loop(Metrics) ->
    receive
        {record_metric, Key, Value} ->
            NewMetrics = maps:update_with(Key, fun(V) -> V + Value end, Value, Metrics),
            metrics_loop(NewMetrics);
        {get_metrics, From} ->
            From ! {metrics, Metrics},
            metrics_loop(Metrics);
        stop ->
            ok;
        _ ->
            metrics_loop(Metrics)
    end.

%% Spawn concurrent connections
spawn_connections(N, Host, Port) ->
    spawn_connections(N, Host, Port, 1, 0, 0).

spawn_connections(0, _Host, _Port, _Counter, _BatchSize, _Spawned) ->
    ok;
spawn_connections(N, Host, Port, Counter, BatchSize, Spawned) when BatchSize >= 1000 ->
    % Wait between batches to avoid overwhelming the system
    timer:sleep(10),
    spawn_connections(N, Host, Port, Counter, 0, Spawned);
spawn_connections(N, Host, Port, Counter, BatchSize, Spawned) ->
    ConnId = Counter,
    spawn(fun() ->
        connection_worker(ConnId, Host, Port)
    end),
    spawn_connections(N - 1, Host, Port, Counter + 1, BatchSize + 1, Spawned + 1).

%% Individual connection worker
connection_worker(ConnId, Host, Port) ->
    case establish_connection(Host, Port) of
        {ok, Socket} ->
            % Send HTTP request
            RequestStart = erlang:system_time(millisecond),

            Request = <<"GET /health HTTP/1.1\r\n"
                       "Host: ", (atom_to_binary(erlang:list_to_atom(Host), utf8))/binary, "\r\n"
                       "Connection: keep-alive\r\n"
                       "User-Agent: ErlMCP-Stress/1.0\r\n"
                       "\r\n">>,

            case gen_tcp:send(Socket, Request) of
                ok ->
                    metrics_server ! {record_metric, requests_sent, 1},

                    % Receive response
                    case gen_tcp:recv(Socket, 0, 5000) of
                        {ok, _Response} ->
                            RequestEnd = erlang:system_time(millisecond),
                            Latency = RequestEnd - RequestStart,
                            metrics_server ! {record_metric, requests_success, 1},
                            metrics_server ! {record_metric, responses_received, 1},
                            metrics_server ! {record_metric, total_latency_ms, Latency};
                        {error, _} ->
                            metrics_server ! {record_metric, requests_failed, 1},
                            metrics_server ! {record_metric, errors, 1}
                    end;
                {error, _} ->
                    metrics_server ! {record_metric, requests_failed, 1},
                    metrics_server ! {record_metric, errors, 1}
            end,

            catch gen_tcp:close(Socket);
        {error, _} ->
            metrics_server ! {record_metric, requests_failed, 1},
            metrics_server ! {record_metric, errors, 1}
    end.

%% Run test for duration
start_time_ts(Connections, DurationSeconds, Host, Port) ->
    StartTime = erlang:system_time(millisecond),
    EndTime = StartTime + (DurationSeconds * 1000),

    % Monitor test execution
    monitor_test(StartTime, EndTime, Connections, Host, Port).

%% Monitor test progress
monitor_test(StartTime, EndTime, TotalConnections, _Host, _Port) ->
    Now = erlang:system_time(millisecond),
    ElapsedSeconds = (Now - StartTime) div 1000,

    % Get metrics
    metrics_server ! {get_metrics, self()},
    receive
        {metrics, Metrics} ->
            RequestsSent = maps:get(requests_sent, Metrics, 0),
            RequestsSuccess = maps:get(requests_success, Metrics, 0),
            RequestsFailed = maps:get(requests_failed, Metrics, 0),
            TotalLatency = maps:get(total_latency_ms, Metrics, 0),
            Errors = maps:get(errors, Metrics, 0),

            AvgLatency = case RequestsSuccess of
                0 -> 0;
                _ -> TotalLatency div RequestsSuccess
            end,

            io:format("~n[~3ws] Connections: ~w | Sent: ~w | Success: ~w | Failed: ~w | Errors: ~w | Avg Latency: ~wms~n",
                     [ElapsedSeconds, TotalConnections, RequestsSent, RequestsSuccess, RequestsFailed, Errors, AvgLatency]),

            case Now < EndTime of
                true ->
                    timer:sleep(1000),
                    monitor_test(StartTime, EndTime, TotalConnections, _Host, _Port);
                false ->
                    print_final_report(StartTime, Now, Metrics, TotalConnections)
            end
    after
        1000 ->
            io:format("[~3ws] Waiting for metrics...~n", [ElapsedSeconds]),
            monitor_test(StartTime, EndTime, TotalConnections, _Host, _Port)
    end.

%% Print final test report
print_final_report(StartTime, EndTime, Metrics, TotalConnections) ->
    ElapsedSeconds = (EndTime - StartTime) div 1000,

    RequestsSent = maps:get(requests_sent, Metrics, 0),
    RequestsSuccess = maps:get(requests_success, Metrics, 0),
    RequestsFailed = maps:get(requests_failed, Metrics, 0),
    TotalLatency = maps:get(total_latency_ms, Metrics, 0),
    Errors = maps:get(errors, Metrics, 0),

    AvgLatency = case RequestsSuccess of
        0 -> 0;
        _ -> TotalLatency div RequestsSuccess
    end,

    Throughput = case ElapsedSeconds of
        0 -> 0;
        _ -> RequestsSuccess div ElapsedSeconds
    end,

    SuccessRate = case RequestsSent of
        0 -> 0;
        _ -> (RequestsSuccess * 100) div RequestsSent
    end,

    io:format("~n~n=== FINAL TEST REPORT ===~n", []),
    io:format("Test Duration: ~w seconds~n", [ElapsedSeconds]),
    io:format("Total Connections Attempted: ~w~n", [TotalConnections]),
    io:format("~n--- REQUEST METRICS ---~n", []),
    io:format("Requests Sent: ~w~n", [RequestsSent]),
    io:format("Successful Requests: ~w~n", [RequestsSuccess]),
    io:format("Failed Requests: ~w~n", [RequestsFailed]),
    io:format("Success Rate: ~w%~n", [SuccessRate]),
    io:format("~n--- LATENCY METRICS ---~n", []),
    io:format("Average Latency: ~wms~n", [AvgLatency]),
    io:format("~n--- THROUGHPUT ---~n", []),
    io:format("Requests/Second: ~w~n", [Throughput]),
    io:format("~n--- ERROR METRICS ---~n", []),
    io:format("Total Errors: ~w~n", [Errors]),
    io:format("Error Rate: ~w%~n", [case RequestsSent of 0 -> 0; _ -> (Errors * 100) div RequestsSent end]),
    io:format("~n=== TEST COMPLETE ===~n~n", []),

    % Stop metrics server
    metrics_server ! stop,
    connection_pool ! stop,

    % Exit with proper code
    case SuccessRate >= 95 of
        true -> init:stop(0);
        false -> init:stop(1)
    end.
