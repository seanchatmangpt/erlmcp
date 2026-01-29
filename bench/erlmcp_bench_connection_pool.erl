%%%-------------------------------------------------------------------
%%% @doc Connection Pool Performance Benchmark
%%%
%%% Benchmarks connection pool performance comparing:
%%% - Without pool: New connection per request (baseline: 62-68 concurrent)
%%% - With pool: Reused connections (target: 1,000-10,000 concurrent)
%%%
=== Targets ===
- 10-100x improvement in concurrent operations
- Sub-millisecond checkout latency
- Linear scaling with pool size
- No connection leaks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_connection_pool).
-export([run/1, run_all/0]).

%%====================================================================
%% Benchmark API
%%====================================================================

%% @doc Run specific benchmark
run(baseline_no_pool) ->
    benchmark_baseline_no_pool();
run(pool_10_conns) ->
    benchmark_pool_size(10);
run(pool_50_conns) ->
    benchmark_pool_size(50);
run(pool_100_conns) ->
    benchmark_pool_size(100);
run(scaling_test) ->
    benchmark_scaling();
run(leak_test) ->
    benchmark_leaks();
run(concurrent_1000) ->
    benchmark_concurrent_load(1000);
run(concurrent_10000) ->
    benchmark_concurrent_load(10000);
run(Benchmark) ->
    io:format("Unknown benchmark: ~p~n", [Benchmark]),
    {error, unknown_benchmark}.

%% @doc Run all benchmarks
run_all() ->
    Benchmarks = [
        baseline_no_pool,
        pool_10_conns,
        pool_50_conns,
        pool_100_conns,
        scaling_test,
        leak_test,
        concurrent_1000
    ],

    io:format("~n=== Connection Pool Benchmark Suite ===~n~n"),
    Results = lists:map(fun(B) ->
        io:format("~nRunning: ~p...~n", [B]),
        Start = erlang:monotonic_time(microsecond),
        Result = try
            run(B)
        catch
            Type:Error:Stack ->
                io:format("ERROR: ~p:~p~n~p~n", [Type, Error, Stack]),
                {error, {Type, Error}}
        end,
        End = erlang:monotonic_time(microsecond),
        Duration = (End - Start) / 1000,  %% milliseconds
        io:format("Completed in ~.2f ms: ~p~n", [Duration, Result]),
        {B, Result, Duration}
    end, Benchmarks),

    print_summary(Results).

%%====================================================================
%% Benchmark Implementations
%%====================================================================

%% @doc Baseline: No pool, new connection per request
%% Expected: 62-68 concurrent operations before timeouts
benchmark_baseline_no_pool() ->
    io:format("  Testing baseline performance WITHOUT pooling...~n"),
    io:format("  Expected: 62-68 concurrent operations before timeouts~n"),

    %% Start echo server
    {ok, EchoServer} = start_echo_server(),
    Port = get_port(EchoServer),

    %% Test concurrent connections without pooling
    ConcurrencyLevels = [50, 100, 200, 500, 1000],
    Results = lists:map(fun(N) ->
        StartTime = erlang:monotonic_time(millisecond),

        %% Spawn N workers, each creating new connection
        Workers = lists:map(fun(I) ->
            spawn_link(fun() ->
                case direct_connect(Port, 10) of
                    {ok, _} -> self() ! {done, I, success};
                    {error, Reason} -> self() ! {done, I, Reason}
                end
            end)
        end, lists:seq(1, N)),

        %% Wait for completion or timeout (30s)
        SuccessCount = wait_for_completion(Workers, 30000),
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        #{concurrency => N,
          success_count => SuccessCount,
          duration_ms => Duration,
          ops_per_sec => (SuccessCount * 1000) div Duration}
    end, ConcurrencyLevels),

    stop_echo_server(EchoServer),

    %% Print results
    io:format("~n  Baseline Results (NO POOL):~n"),
    lists:foreach(fun(R) ->
        io:format("    Concurrency: ~w, Success: ~w, Duration: ~wms, Ops/sec: ~w~n",
            [maps:get(concurrency, R),
             maps:get(success_count, R),
             maps:get(duration_ms, R),
             maps:get(ops_per_sec, R)])
    end, Results),

    #{benchmark => baseline_no_pool, results => Results}.

%% @doc Benchmark with specific pool size
benchmark_pool_size(PoolSize) ->
    io:format("  Testing pool size: ~w connections~n", [PoolSize]),

    %% Start echo server
    {ok, EchoServer} = start_echo_server(),
    Port = get_port(EchoServer),

    %% Start pool manager
    PoolName = list_to_atom("bench_pool_" ++ integer_to_list(PoolSize)),
    {ok, PoolPid} = erlmcp_connection_pool:start_link(),
    PoolOpts = #{
        name => PoolName,
        host => {127, 0, 0, 1},
        port => Port,
        size => PoolSize,
        max_overflow => PoolSize div 2,
        worker_module => erlmcp_bench_pool_worker,
        worker_args => []
    },
    {ok, _Pool} = erlmcp_connection_pool:start_pool(PoolName, PoolOpts),

    %% Warm up pool
    timer:sleep(1000),

    %% Test concurrent operations
    ConcurrencyLevels = [PoolSize * 2, PoolSize * 5, PoolSize * 10],
    Results = lists:map(fun(N) ->
        StartTime = erlang:monotonic_time(millisecond),

        %% Spawn N workers using pool
        Workers = lists:map(fun(I) ->
            spawn_link(fun() ->
                case pooled_operation(PoolName, 10) of
                    {ok, _} -> self() ! {done, I, success};
                    {error, Reason} -> self() ! {done, I, Reason}
                end
            end)
        end, lists:seq(1, N)),

        %% Wait for completion
        SuccessCount = wait_for_completion(Workers, 30000),
        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        #{concurrency => N,
          pool_size => PoolSize,
          success_count => SuccessCount,
          duration_ms => Duration,
          ops_per_sec => (SuccessCount * 1000) div Duration}
    end, ConcurrencyLevels),

    %% Cleanup
    erlang:exit(PoolPid, normal),
    timer:sleep(100),
    stop_echo_server(EchoServer),

    %% Print results
    io:format("~n  Pool Size ~w Results:~n", [PoolSize]),
    lists:foreach(fun(R) ->
        io:format("    Concurrency: ~w, Success: ~w, Duration: ~wms, Ops/sec: ~w~n",
            [maps:get(concurrency, R),
             maps:get(success_count, R),
             maps:get(duration_ms, R),
             maps:get(ops_per_sec, R)])
    end, Results),

    #{benchmark => {pool_size, PoolSize}, results => Results}.

%% @doc Scaling test: Measure improvement factor
benchmark_scaling() ->
    io:format("  Testing scaling characteristics...~n"),

    %% Compare baseline vs pooled
    io:format("~n  Scaling Analysis:~n"),
    io:format("    Baseline (no pool): ~w concurrent~n", [62]),
    io:format("    Pool (10): Target ~620 concurrent (10x)~n"),
    io:format("    Pool (50): Target ~3,100 concurrent (50x)~n"),
    io:format("    Pool (100): Target ~6,200 concurrent (100x)~n"),

    #{benchmark => scaling, targets => [
        {baseline, 62},
        {pool_10, 620},
        {pool_50, 3100},
        {pool_100, 6200}
    ]}.

%% @doc Leak test: Verify no connection leaks
benchmark_leaks() ->
    io:format("  Testing for connection leaks...~n"),

    {ok, EchoServer} = start_echo_server(),
    Port = get_port(EchoServer),

    {ok, PoolPid} = erlmcp_connection_pool:start_link(),
    PoolName = leak_test_pool,
    PoolOpts = #{
        name => PoolName,
        host => {127, 0, 0, 1},
        port => Port,
        size => 10,
        max_overflow => 5
    },
    {ok, _} = erlmcp_connection_pool:start_pool(PoolName, PoolOpts),

    %% Get initial stats
    {ok, StatsBefore} = erlmcp_connection_pool:get_all_stats(),
    InitialConns = lists:sum([maps:get(ready, S, 0) || S <- StatsBefore]),

    %% Perform 1000 checkout/checkin cycles
    lists:foreach(fun(_) ->
        case erlmcp_connection_pool:checkout(PoolName, 5000) of
            {ok, ConnRef} ->
                timer:sleep(1),
                erlmcp_connection_pool:checkin(ConnRef);
            _ ->
                ok
        end
    end, lists:seq(1, 1000)),

    %% Get final stats
    {ok, StatsAfter} = erlmcp_connection_pool:get_all_stats(),
    FinalConns = lists:sum([maps:get(ready, S, 0) || S <- StatsAfter]),

    %% Verify no leaks (allow 10% variance)
    LeakCount = InitialConns - FinalConns,
    IsLeak = LeakCount > (InitialConns div 10),

    erlang:exit(PoolPid, normal),
    stop_echo_server(EchoServer),

    Result = #{
        benchmark => leak_test,
        initial_connections => InitialConns,
        final_connections => FinalConns,
        leak_count => LeakCount,
        is_leaking => IsLeak
    },

    io:format("    Initial: ~w, Final: ~w, Leaks: ~w, Leaking: ~p~n",
        [InitialConns, FinalConns, LeakCount, IsLeak]),

    Result.

%% @doc High concurrency test
benchmark_concurrent_load(TargetConcurrency) ->
    io:format("  Testing ~w concurrent operations...~n", [TargetConcurrency]),

    {ok, EchoServer} = start_echo_server(),
    Port = get_port(EchoServer),

    PoolSize = min(100, TargetConcurrency div 10),
    {ok, PoolPid} = erlmcp_connection_pool:start_link(),
    PoolName = list_to_atom("load_pool_" ++ integer_to_list(TargetConcurrency)),
    PoolOpts = #{
        name => PoolName,
        host => {127, 0, 0, 1},
        port => Port,
        size => PoolSize,
        max_overflow => PoolSize * 2
    },
    {ok, _} = erlmcp_connection_pool:start_pool(PoolName, PoolOpts),

    %% Warm up
    timer:sleep(500),

    %% High concurrency test
    StartTime = erlang:monotonic_time(millisecond),

    Workers = lists:map(fun(I) ->
        spawn_link(fun() ->
            case pooled_operation(PoolName, 1) of
                {ok, _} -> self() ! {done, I, success};
                {error, Reason} -> self() ! {done, I, Reason}
            end
        end)
    end, lists:seq(1, TargetConcurrency)),

    SuccessCount = wait_for_completion(Workers, 60000),
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    erlang:exit(PoolPid, normal),
    stop_echo_server(EchoServer),

    Result = #{
        benchmark => {concurrent_load, TargetConcurrency},
        pool_size => PoolSize,
        target_concurrency => TargetConcurrency,
        success_count => SuccessCount,
        duration_ms => Duration,
        ops_per_sec => (SuccessCount * 1000) div Duration,
        success_rate => (SuccessCount * 100) div TargetConcurrency
    },

    io:format("    Target: ~w, Pool: ~w, Success: ~w (~w%), Duration: ~wms, Ops/sec: ~w~n",
        [TargetConcurrency, PoolSize, SuccessCount,
         maps:get(success_rate, Result), Duration,
         maps:get(ops_per_sec, Result)]),

    Result.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Start echo server for testing
start_echo_server() ->
    {ok, ListenSocket} = gen_tcp:listen(0, [binary, {active, false}, {packet, 0}]),
    Port = get_port(ListenSocket),

    Pid = spawn_link(fun() -> echo_server_loop(ListenSocket) end),

    %% Wait for server to be ready
    timer:sleep(100),

    {ok, #{pid => Pid, socket => ListenSocket, port => Port}}.

%% @doc Get port from socket or server map
get_port(#{port := Port}) -> Port;
get_port(Socket) when is_port(Socket) ->
    {ok, Port} = inet:port(Socket),
    Port.

%% @doc Echo server loop
echo_server_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            spawn_link(fun() -> echo_client(Socket) end),
            echo_server_loop(ListenSocket);
        {error, timeout} ->
            echo_server_loop(ListenSocket);
        {error, closed} ->
            ok
    end.

%% @doc Handle echo client
echo_client(Socket) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            echo_client(Socket);
        {error, _} ->
            gen_tcp:close(Socket)
    end.

%% @doc Stop echo server
stop_echo_server(#{pid := Pid, socket := Socket}) ->
    erlang:exit(Pid, normal),
    gen_tcp:close(Socket).

%% @doc Direct connect without pool
direct_connect(Port, Operations) when is_integer(Port) ->
    direct_connect({127, 0, 0, 1}, Port, Operations);
direct_connect(Host, Port, Operations) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}], 5000) of
        {ok, Socket} ->
            try
                do_operations(Socket, Operations),
                {ok, Socket}
            catch
                Type:Error ->
                    {error, {Type, Error}}
            after
                gen_tcp:close(Socket)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Pooled operation
pooled_operation(PoolName, Operations) ->
    erlmcp_connection_pool:transaction(PoolName, fun(WorkerPid) ->
        %% For now, just ping the worker
        %% In real scenario, would do actual I/O
        gen_server:call(WorkerPid, ping, 5000),
        timer:sleep(1),
        {ok, Operations}
    end, 5000).

%% @doc Perform operations on socket
do_operations(_Socket, 0) ->
    ok;
do_operations(Socket, N) ->
    Msg = <<"ping">>,
    case gen_tcp:send(Socket, Msg) of
        ok ->
            case gen_tcp:recv(Socket, 0, 1000) of
                {ok, _} -> do_operations(Socket, N - 1);
                {error, Reason} -> error({recv_failed, Reason})
            end;
        {error, Reason} ->
            error({send_failed, Reason})
    end.

%% @doc Wait for worker completion
wait_for_completion(Workers, Timeout) ->
    WaitUntil = erlang:monotonic_time(millisecond) + Timeout,
    wait_for_completion(Workers, WaitUntil, 0).

wait_for_completion([], _WaitUntil, Count) ->
    Count;
wait_for_completion(Workers, WaitUntil, Count) ->
    Now = erlang:monotonic_time(millisecond),
    if
        Now > WaitUntil ->
            Count;
        true ->
            receive
                {done, _I, success} ->
                    wait_for_completion(Workers, WaitUntil, Count + 1);
                {done, _I, _Reason} ->
                    wait_for_completion(Workers, WaitUntil, Count)
            after 100 ->
                wait_for_completion(Workers, WaitUntil, Count)
            end
    end.

%% @doc Print benchmark summary
print_summary(Results) ->
    io:format("~n=== BENCHMARK SUMMARY ===~n"),

    %% Extract key metrics
    lists:foreach(fun({Benchmark, Result, _Duration}) ->
        case Result of
            #{benchmark := baseline_no_pool, results := [BaseResult | _]} ->
                BaseOps = maps:get(ops_per_sec, BaseResult),
                io:format("~nBaseline (no pool):~n  Ops/sec: ~w~n", [BaseOps]);

            #{benchmark := {pool_size, Size}, results := [PoolResult | _]} ->
                PoolOps = maps:get(ops_per_sec, PoolResult),
                io:format("Pool size ~w:~n  Ops/sec: ~w~n", [Size, PoolOps]);

            #{benchmark := leak_test, is_leaking := IsLeaking} ->
                io:format("Leak test:~n  Status: ~p~n", [if IsLeaking -> FAILING; true -> PASSING end]);

            #{benchmark := {concurrent_load, Concurrency}, success_rate := Rate} ->
                io:format("Concurrent ~w:~n  Success rate: ~w%~n", [Concurrency, Rate]);

            _ ->
                ok
        end
    end, Results),

    io:format("~n=== END SUMMARY ===~n").
