%%%-------------------------------------------------------------------
%%% @doc Example of using poolboy connection pooling with erlmcp
%%% @end
%%%-------------------------------------------------------------------
-module(poolboy_example).

-export([
    start/0,
    start_tcp_pool/0,
    start_http_pool/0,
    start_all_pools/0,
    send_tcp_message/1,
    make_http_request/1,
    monitor_pools/0,
    check_pool_health/0,
    stop_all_pools/0
]).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Start a simple example with TCP connection pool
start() ->
    io:format("Starting poolboy example...~n"),

    % Start TCP pool
    case start_tcp_pool() of
        {ok, _Pid} ->
            io:format("TCP pool started successfully~n"),

            % Send some test messages
            lists:foreach(fun(N) ->
                Msg = list_to_binary(io_lib:format("Message ~p", [N])),
                send_tcp_message(Msg)
            end, lists:seq(1, 10)),

            % Check pool status
            check_pool_health(),

            % Cleanup
            timer:sleep(1000),
            stop_all_pools(),
            ok;
        {error, Reason} ->
            io:format("Failed to start TCP pool: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Start a TCP connection pool
start_tcp_pool() ->
    io:format("Starting TCP connection pool...~n"),

    % Configure pool
    PoolConfig = #{
        pool_size => 10,        % 10 permanent workers
        max_overflow => 5       % 5 additional temporary workers
    },

    % Configure TCP transport
    TransportConfig = #{
        host => <<"localhost">>,
        port => 8080,
        keepalive => true,
        connect_timeout => 5000,
        recv_timeout => 30000,
        send_timeout => 5000,
        nodelay => true,
        buffer_size => 4096
    },

    % Start the pool
    case erlmcp:setup_connection_pool(tcp, PoolConfig, TransportConfig) of
        {ok, PoolPid} ->
            io:format("TCP pool started with PID: ~p~n", [PoolPid]),
            {ok, PoolPid};
        {error, Reason} ->
            io:format("Failed to start TCP pool: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Start an HTTP connection pool
start_http_pool() ->
    io:format("Starting HTTP connection pool...~n"),

    % Configure pool
    PoolConfig = #{
        pool_size => 20,
        max_overflow => 10
    },

    % Configure HTTP transport
    TransportConfig = #{
        url => <<"https://api.example.com">>,
        method => get,
        headers => #{
            <<"User-Agent">> => <<"erlmcp/0.5.0">>,
            <<"Accept">> => <<"application/json">>
        },
        timeout => 30000,
        max_redirects => 5,
        verify_ssl => true,
        compression => true
    },

    % Start the pool
    case erlmcp:setup_connection_pool(http, PoolConfig, TransportConfig) of
        {ok, PoolPid} ->
            io:format("HTTP pool started with PID: ~p~n", [PoolPid]),
            {ok, PoolPid};
        {error, Reason} ->
            io:format("Failed to start HTTP pool: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Start all available transport pools
start_all_pools() ->
    io:format("Starting all connection pools...~n"),

    % Define all pools to start
    Pools = [
        {tcp, #{pool_size => 10, max_overflow => 5},
         #{host => <<"localhost">>, port => 8080}},

        {http, #{pool_size => 20, max_overflow => 10},
         #{url => <<"https://api.example.com">>}},

        {stdio, #{pool_size => 5, max_overflow => 2}, #{}}
    ],

    % Start each pool
    Results = lists:map(fun({Type, PoolCfg, TransportCfg}) ->
        case erlmcp:setup_connection_pool(Type, PoolCfg, TransportCfg) of
            {ok, Pid} ->
                io:format("~p pool started: ~p~n", [Type, Pid]),
                {ok, Type, Pid};
            {error, Reason} ->
                io:format("~p pool failed: ~p~n", [Type, Reason]),
                {error, Type, Reason}
        end
    end, Pools),

    % Check for failures
    Errors = [E || {error, _, _} = E <- Results],
    case Errors of
        [] ->
            io:format("All pools started successfully~n"),
            {ok, Results};
        _ ->
            io:format("Some pools failed to start: ~p~n", [Errors]),
            {error, Errors}
    end.

%% @doc Send a message via TCP pool
send_tcp_message(Message) when is_binary(Message) ->
    io:format("Sending message: ~s~n", [Message]),

    % Use pool_transaction for automatic worker management
    try
        Result = erlmcp:pool_transaction(tcp, fun(Worker) ->
            % Simulate sending message through worker
            % In real implementation, this would call the worker's send function
            timer:sleep(100),  % Simulate network delay
            {ok, sent, Message}
        end, 5000),

        io:format("Message sent: ~p~n", [Result]),
        Result
    catch
        Class:Error ->
            io:format("Failed to send message: ~p:~p~n", [Class, Error]),
            {error, {Class, Error}}
    end.

%% @doc Make an HTTP request via HTTP pool
make_http_request(Endpoint) when is_binary(Endpoint) ->
    io:format("Making HTTP request to: ~s~n", [Endpoint]),

    % Use with_pool_worker for custom timeout
    case erlmcp:with_pool_worker(http, fun(Worker) ->
        % Simulate HTTP request through worker
        timer:sleep(200),  % Simulate request delay
        {ok, 200, #{
            status => <<"OK">>,
            endpoint => Endpoint,
            timestamp => erlang:system_time(millisecond)
        }}
    end, 10000) of
        {ok, StatusCode, Response} ->
            io:format("HTTP request successful: ~p ~p~n", [StatusCode, Response]),
            {ok, StatusCode, Response};
        {error, Reason} ->
            io:format("HTTP request failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Monitor all pools and print status
monitor_pools() ->
    io:format("~n=== Pool Status Monitor ===~n"),
    monitor_pools_loop().

monitor_pools_loop() ->
    timer:sleep(5000),  % Check every 5 seconds

    Types = [tcp, http, stdio],
    lists:foreach(fun(Type) ->
        case erlmcp:pool_status(Type) of
            {ok, #{pool_name := Name, status := Status}} ->
                io:format("[~s] Pool ~p status: ~p~n",
                    [timestamp(), Name, Status]);
            {error, {pool_not_found, _}} ->
                % Pool not started, skip
                ok;
            {error, Reason} ->
                io:format("[~s] Error getting ~p pool status: ~p~n",
                    [timestamp(), Type, Reason])
        end
    end, Types),

    io:format("~n"),
    monitor_pools_loop().

%% @doc Check health of all pools
check_pool_health() ->
    io:format("~n=== Pool Health Check ===~n"),

    Types = [tcp, http, stdio],
    Results = lists:map(fun(Type) ->
        case erlmcp:pool_status(Type) of
            {ok, Status} ->
                io:format("~p pool: HEALTHY - ~p~n", [Type, Status]),
                {ok, Type, Status};
            {error, {pool_not_found, _}} ->
                io:format("~p pool: NOT STARTED~n", [Type]),
                {not_started, Type};
            {error, Reason} ->
                io:format("~p pool: UNHEALTHY - ~p~n", [Type, Reason]),
                {error, Type, Reason}
        end
    end, Types),

    % Calculate health score
    Healthy = length([R || {ok, _, _} <- Results]),
    Total = length(Results),
    Score = (Healthy / Total) * 100,

    io:format("~nHealth Score: ~.1f% (~p/~p pools healthy)~n",
        [Score, Healthy, Total]),

    case Score >= 100.0 of
        true ->
            io:format("Status: ALL SYSTEMS OPERATIONAL~n~n"),
            ok;
        false ->
            io:format("Status: DEGRADED - Some pools are down~n~n"),
            {degraded, Results}
    end.

%% @doc Stop all pools and cleanup
stop_all_pools() ->
    io:format("Stopping all connection pools...~n"),

    Types = [tcp, http, stdio],
    lists:foreach(fun(Type) ->
        case erlmcp:stop_connection_pool(Type) of
            ok ->
                io:format("~p pool stopped~n", [Type]);
            {error, Reason} ->
                io:format("Error stopping ~p pool: ~p~n", [Type, Reason])
        end
    end, Types),

    io:format("All pools stopped~n"),
    ok.

%%%===================================================================
%%% Advanced Examples
%%%===================================================================

%% @doc Example: Concurrent operations using pool
concurrent_operations_example() ->
    % Start TCP pool
    {ok, _} = start_tcp_pool(),

    % Spawn multiple processes to send messages concurrently
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            Msg = list_to_binary(io_lib:format("Concurrent message ~p", [N])),
            send_tcp_message(Msg)
        end)
    end, lists:seq(1, 100)),

    % Wait for all processes to complete
    lists:foreach(fun(Pid) ->
        monitor(process, Pid),
        receive
            {'DOWN', _, process, Pid, _} -> ok
        after 10000 ->
            exit(Pid, kill)
        end
    end, Pids),

    % Check final pool status
    check_pool_health(),
    stop_all_pools().

%% @doc Example: Error handling and retry logic
retry_on_error_example(Message, MaxRetries) ->
    retry_send(Message, MaxRetries, 0).

retry_send(_Message, MaxRetries, Attempt) when Attempt >= MaxRetries ->
    io:format("Max retries (~p) exceeded~n", [MaxRetries]),
    {error, max_retries_exceeded};

retry_send(Message, MaxRetries, Attempt) ->
    io:format("Attempt ~p of ~p~n", [Attempt + 1, MaxRetries]),

    case send_tcp_message(Message) of
        {ok, _, _} = Success ->
            Success;
        {error, _Reason} ->
            % Exponential backoff
            SleepMs = trunc(math:pow(2, Attempt) * 100),
            io:format("Retrying in ~p ms...~n", [SleepMs]),
            timer:sleep(SleepMs),
            retry_send(Message, MaxRetries, Attempt + 1)
    end.

%% @doc Example: Load testing with pools
load_test_example(NumRequests, NumWorkers) ->
    io:format("Starting load test: ~p requests with ~p workers~n",
        [NumRequests, NumWorkers]),

    % Start pool with specific size
    PoolConfig = #{
        pool_size => NumWorkers,
        max_overflow => NumWorkers div 2
    },
    {ok, _} = erlmcp:setup_connection_pool(tcp, PoolConfig,
        #{host => <<"localhost">>, port => 8080}),

    % Start timer
    StartTime = erlang:monotonic_time(millisecond),

    % Send requests
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            Msg = list_to_binary(io_lib:format("Load test ~p", [N])),
            send_tcp_message(Msg)
        end)
    end, lists:seq(1, NumRequests)),

    % Wait for completion
    lists:foreach(fun(Pid) ->
        monitor(process, Pid),
        receive
            {'DOWN', _, process, Pid, _} -> ok
        after 30000 ->
            exit(Pid, kill)
        end
    end, Pids),

    % Calculate metrics
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    ThroughputPerSec = (NumRequests * 1000) / Duration,

    io:format("Load test complete:~n"),
    io:format("  Total requests: ~p~n", [NumRequests]),
    io:format("  Duration: ~p ms~n", [Duration]),
    io:format("  Throughput: ~.2f req/sec~n", [ThroughputPerSec]),

    stop_all_pools(),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Get current timestamp string
timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    io_lib:format("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",
        [Y, M, D, H, Min, S]).
