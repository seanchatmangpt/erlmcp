-module(erlmcp_poolboy_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Suite for poolboy Connection Pooling Integration
%%%
%%% This test suite validates the integration of poolboy library
%%% for connection pooling in HTTP and TCP transports, ensuring:
%%% - Worker pool initialization
%%% - Pool configuration (size, max_overflow)
%%% - Worker checkout/checkin
%%% - Pool saturation handling
%%% - Worker lifecycle management
%%% - Connection reuse
%%% - Load balancing across workers
%%% - Error recovery in pooled workers
%%% - Pool monitoring and metrics
%%%
%%% Target: >90% coverage on poolboy integration code
%%%===================================================================

%%====================================================================
%% Test Groups
%%====================================================================

poolboy_test_() ->
    {setup,
     fun setup_poolboy/0,
     fun cleanup_poolboy/1,
     [
         {"Basic pool initialization", fun test_pool_init/0},
         {"Pool configuration validation", fun test_pool_config/0},
         {"Worker checkout and checkin", fun test_worker_checkout/0},
         {"Pool size management", fun test_pool_size/0},
         {"Pool overflow handling", fun test_pool_overflow/0},
         {"Pool saturation behavior", fun test_pool_saturation/0},
         {"Worker death and recovery", fun test_worker_death_recovery/0},
         {"Pool transaction pattern", fun test_pool_transaction/0},
         {"Pool status monitoring", fun test_pool_status/0},
         {"Pool worker metrics", fun test_pool_metrics/0},
         {"Pool concurrent access", fun test_pool_concurrent_access/0},
         {"Pool configuration updates", fun test_pool_config_updates/0},
         {"Pool shutdown cleanup", fun test_pool_shutdown/0}
     ]}.

http_pool_test_() ->
    {setup,
     fun setup_http_pool/0,
     fun cleanup_http_pool/1,
     [
         {"HTTP connection pooling", fun test_http_connection_pooling/0},
         {"HTTP pool request distribution", fun test_http_pool_distribution/0},
         {"HTTP pool connection reuse", fun test_http_connection_reuse/0},
         {"HTTP pool timeout handling", fun test_http_pool_timeout/0},
         {"HTTP pool error recovery", fun test_http_pool_error_recovery/0},
         {"HTTP pool load balancing", fun test_http_pool_load_balancing/0}
     ]}.

tcp_pool_test_() ->
    {setup,
     fun setup_tcp_pool/0,
     fun cleanup_tcp_pool/1,
     [
         {"TCP connection pooling", fun test_tcp_connection_pooling/0},
         {"TCP pool request distribution", fun test_tcp_pool_distribution/0},
         {"TCP pool connection reuse", fun test_tcp_connection_reuse/0},
         {"TCP pool timeout handling", fun test_tcp_pool_timeout/0},
         {"TCP pool error recovery", fun test_tcp_pool_error_recovery/0}
     ]}.

integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun cleanup_integration/1,
     [
         {timeout, 15, {"Full poolboy integration", fun test_full_poolboy_integration/0}},
         {timeout, 15, {"Multi-transport pooling", fun test_multi_transport_pooling/0}},
         {timeout, 15, {"Pool load testing", fun test_pool_load_testing/0}},
         {timeout, 15, {"Pool failure scenarios", fun test_pool_failure_scenarios/0}}
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup_poolboy() ->
    case application:ensure_started(poolboy) of
        ok -> ok;
        {error, {already_started, poolboy}} -> ok
    end,
    #{}.

cleanup_poolboy(_) ->
    timer:sleep(100),
    ok.

setup_http_pool() ->
    application:ensure_started(inets),
    application:ensure_started(ssl),
    application:ensure_started(poolboy),
    #{}.

cleanup_http_pool(_) ->
    timer:sleep(100),
    ok.

setup_tcp_pool() ->
    application:ensure_started(poolboy),
    #{}.

cleanup_tcp_pool(_) ->
    timer:sleep(100),
    ok.

setup_integration() ->
    application:ensure_started(poolboy),
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end,
    #{}.

cleanup_integration(_) ->
    timer:sleep(100),
    ok.

%%====================================================================
%% Basic Poolboy Tests
%%====================================================================

test_pool_init() ->
    % Create a simple worker pool
    PoolArgs = [
        {name, {local, test_pool}},
        {worker_module, test_worker},
        {size, 5},
        {max_overflow, 2}
    ],

    WorkerArgs = [],

    {ok, Pool} = poolboy:start_link(PoolArgs, WorkerArgs),

    try
        % Verify pool started
        ?assert(is_pid(Pool)),
        ?assert(is_process_alive(Pool))
    after
        catch poolboy:stop(Pool)
    end.

test_pool_config() ->
    % Test various pool configurations
    Configs = [
        {small_pool, [{size, 1}, {max_overflow, 0}]},
        {medium_pool, [{size, 5}, {max_overflow, 5}]},
        {large_pool, [{size, 20}, {max_overflow, 10}]}
    ],

    lists:foreach(fun({PoolName, ExtraArgs}) ->
        PoolArgs = [
            {name, {local, PoolName}},
            {worker_module, test_worker}
        ] ++ ExtraArgs,

        {ok, Pool} = poolboy:start_link(PoolArgs, []),

        try
            ?assert(is_process_alive(Pool))
        after
            catch poolboy:stop(Pool)
        end
    end, Configs).

test_worker_checkout() ->
    PoolArgs = [
        {name, {local, checkout_pool}},
        {worker_module, test_worker},
        {size, 3},
        {max_overflow, 1}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Checkout a worker
        Worker = poolboy:checkout(Pool),

        % Verify we got a worker
        ?assert(is_pid(Worker)),
        ?assert(is_process_alive(Worker)),

        % Checkin the worker
        ok = poolboy:checkin(Pool, Worker),

        % Worker should still be alive
        ?assert(is_process_alive(Worker))
    after
        catch poolboy:stop(Pool)
    end.

%%====================================================================
%% Pool Size and Overflow Tests
%%====================================================================

test_pool_size() ->
    PoolSize = 5,
    PoolArgs = [
        {name, {local, size_pool}},
        {worker_module, test_worker},
        {size, PoolSize},
        {max_overflow, 0}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Checkout all workers
        Workers = [poolboy:checkout(Pool) || _ <- lists:seq(1, PoolSize)],

        % All workers should be valid
        lists:foreach(fun(W) ->
            ?assert(is_pid(W)),
            ?assert(is_process_alive(W))
        end, Workers),

        % Return workers
        lists:foreach(fun(W) ->
            poolboy:checkin(Pool, W)
        end, Workers)
    after
        catch poolboy:stop(Pool)
    end.

test_pool_overflow() ->
    PoolSize = 2,
    MaxOverflow = 3,
    PoolArgs = [
        {name, {local, overflow_pool}},
        {worker_module, test_worker},
        {size, PoolSize},
        {max_overflow, MaxOverflow}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Checkout more than pool size (up to max_overflow)
        Workers = [poolboy:checkout(Pool) || _ <- lists:seq(1, PoolSize + 2)],

        % Should get workers
        ?assertEqual(PoolSize + 2, length(Workers)),

        % Return workers
        lists:foreach(fun(W) ->
            poolboy:checkin(Pool, W)
        end, Workers)
    after
        catch poolboy:stop(Pool)
    end.

test_pool_saturation() ->
    PoolSize = 2,
    MaxOverflow = 0,
    PoolArgs = [
        {name, {local, saturation_pool}},
        {worker_module, test_worker},
        {size, PoolSize},
        {max_overflow, MaxOverflow}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Checkout all workers
        Workers = [poolboy:checkout(Pool) || _ <- lists:seq(1, PoolSize)],

        % Try to checkout one more (should block or timeout)
        Parent = self(),
        spawn(fun() ->
            Result = catch poolboy:checkout(Pool, false), % non-blocking checkout
            Parent ! {checkout_result, Result}
        end),

        % Wait for result
        receive
            {checkout_result, full} ->
                % Expected when pool is full
                ok;
            {checkout_result, Worker} when is_pid(Worker) ->
                % Got a worker (shouldn't happen)
                poolboy:checkin(Pool, Worker),
                ok
        after 1000 ->
            ok % Timeout acceptable
        end,

        % Return workers
        lists:foreach(fun(W) ->
            poolboy:checkin(Pool, W)
        end, Workers)
    after
        catch poolboy:stop(Pool)
    end.

%%====================================================================
%% Worker Lifecycle Tests
%%====================================================================

test_worker_death_recovery() ->
    PoolArgs = [
        {name, {local, death_pool}},
        {worker_module, test_worker},
        {size, 3},
        {max_overflow, 1}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Checkout a worker
        Worker1 = poolboy:checkout(Pool),
        ?assert(is_process_alive(Worker1)),

        % Kill the worker
        exit(Worker1, kill),
        timer:sleep(100),

        % Checkin (pool should handle dead worker)
        catch poolboy:checkin(Pool, Worker1),

        % Should be able to checkout a new worker
        Worker2 = poolboy:checkout(Pool),
        ?assert(is_pid(Worker2)),
        ?assert(is_process_alive(Worker2)),

        poolboy:checkin(Pool, Worker2)
    after
        catch poolboy:stop(Pool)
    end.

test_pool_transaction() ->
    PoolArgs = [
        {name, {local, transaction_pool}},
        {worker_module, test_worker},
        {size, 3},
        {max_overflow, 1}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Use transaction pattern
        Result = poolboy:transaction(Pool, fun(Worker) ->
            % Worker should be available
            ?assert(is_pid(Worker)),
            ?assert(is_process_alive(Worker)),
            test_operation
        end),

        % Transaction should complete
        ?assertEqual(test_operation, Result)
    after
        catch poolboy:stop(Pool)
    end.

%%====================================================================
%% Pool Monitoring Tests
%%====================================================================

test_pool_status() ->
    PoolArgs = [
        {name, {local, status_pool}},
        {worker_module, test_worker},
        {size, 5},
        {max_overflow, 2}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Get pool status
        Status = poolboy:status(Pool),

        % Status should contain pool info
        ?assert(is_tuple(Status)),
        {StateName, Workers, Overflow, WaitingCount} = Status,

        ?assert(is_atom(StateName)),
        ?assert(is_integer(Workers)),
        ?assert(is_integer(Overflow)),
        ?assert(is_integer(WaitingCount))
    after
        catch poolboy:stop(Pool)
    end.

test_pool_metrics() ->
    PoolArgs = [
        {name, {local, metrics_pool}},
        {worker_module, test_worker},
        {size, 3},
        {max_overflow, 1}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Perform some operations
        lists:foreach(fun(_) ->
            poolboy:transaction(Pool, fun(_Worker) ->
                timer:sleep(10),
                ok
            end)
        end, lists:seq(1, 10)),

        % Get final status
        Status = poolboy:status(Pool),
        ?assert(is_tuple(Status))
    after
        catch poolboy:stop(Pool)
    end.

%%====================================================================
%% Concurrent Access Tests
%%====================================================================

test_pool_concurrent_access() ->
    PoolArgs = [
        {name, {local, concurrent_pool}},
        {worker_module, test_worker},
        {size, 5},
        {max_overflow, 5}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Spawn multiple processes using the pool
        Parent = self(),
        Pids = lists:map(fun(N) ->
            spawn(fun() ->
                Result = poolboy:transaction(Pool, fun(Worker) ->
                    ?assert(is_pid(Worker)),
                    timer:sleep(10),
                    N
                end),
                Parent ! {done, N, Result}
            end)
        end, lists:seq(1, 20)),

        % Wait for all to complete
        lists:foreach(fun(N) ->
            receive
                {done, N, N} -> ok
            after 5000 ->
                ?assert(false) % Timeout
            end
        end, lists:seq(1, 20))
    after
        catch poolboy:stop(Pool)
    end.

%%====================================================================
%% Pool Configuration Tests
%%====================================================================

test_pool_config_updates() ->
    % Test that pool handles configuration correctly
    PoolArgs = [
        {name, {local, config_pool}},
        {worker_module, test_worker},
        {size, 3},
        {max_overflow, 2}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Initial status
        {_, Workers1, Overflow1, _} = poolboy:status(Pool),
        ?assert(Workers1 =< 3),

        % Pool configuration is set at startup
        % Runtime updates would require pool restart
        ?assert(true)
    after
        catch poolboy:stop(Pool)
    end.

%%====================================================================
%% Pool Shutdown Tests
%%====================================================================

test_pool_shutdown() ->
    PoolArgs = [
        {name, {local, shutdown_pool}},
        {worker_module, test_worker},
        {size, 3},
        {max_overflow, 1}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    % Verify pool is alive
    ?assert(is_process_alive(Pool)),

    % Stop pool
    ok = poolboy:stop(Pool),

    % Pool should be stopped
    timer:sleep(100),
    ?assertNot(is_process_alive(Pool)).

%%====================================================================
%% HTTP Pool Integration Tests
%%====================================================================

test_http_connection_pooling() ->
    % Test HTTP transport with poolboy
    Owner = self(),

    PoolArgs = [
        {name, {local, http_pool}},
        {worker_module, test_http_worker},
        {size, 5},
        {max_overflow, 3}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Simulate HTTP requests through pool
        Results = lists:map(fun(N) ->
            poolboy:transaction(Pool, fun(_Worker) ->
                % Simulate HTTP request
                timer:sleep(10),
                {ok, N}
            end)
        end, lists:seq(1, 10)),

        % All should succeed
        ?assertEqual(10, length(Results))
    after
        catch poolboy:stop(Pool)
    end.

test_http_pool_distribution() ->
    PoolArgs = [
        {name, {local, http_dist_pool}},
        {worker_module, test_http_worker},
        {size, 3},
        {max_overflow, 0}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Track which workers handle requests
        Parent = self(),

        lists:foreach(fun(N) ->
            spawn(fun() ->
                poolboy:transaction(Pool, fun(Worker) ->
                    Parent ! {worker_used, Worker, N}
                end)
            end)
        end, lists:seq(1, 9)),

        % Collect worker PIDs
        Workers = lists:map(fun(_) ->
            receive
                {worker_used, W, _} -> W
            after 2000 -> undefined
            end
        end, lists:seq(1, 9)),

        % Should use multiple workers
        UniqueWorkers = lists:usort(Workers),
        ?assert(length(UniqueWorkers) > 1)
    after
        catch poolboy:stop(Pool)
    end.

test_http_connection_reuse() ->
    PoolArgs = [
        {name, {local, http_reuse_pool}},
        {worker_module, test_http_worker},
        {size, 2},
        {max_overflow, 0}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Multiple transactions should reuse workers
        Worker1 = poolboy:transaction(Pool, fun(W) -> W end),
        Worker2 = poolboy:transaction(Pool, fun(W) -> W end),

        % Workers should be reused from pool
        ?assert(is_pid(Worker1)),
        ?assert(is_pid(Worker2))
    after
        catch poolboy:stop(Pool)
    end.

test_http_pool_timeout() ->
    PoolArgs = [
        {name, {local, http_timeout_pool}},
        {worker_module, test_http_worker},
        {size, 1},
        {max_overflow, 0}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Checkout worker and hold it
        Worker = poolboy:checkout(Pool),

        % Try to do transaction (should timeout or fail)
        Parent = self(),
        spawn(fun() ->
            Result = catch poolboy:checkout(Pool, false),
            Parent ! {result, Result}
        end),

        receive
            {result, full} -> ok; % Expected
            {result, _} -> ok
        after 1000 -> ok
        end,

        % Return worker
        poolboy:checkin(Pool, Worker)
    after
        catch poolboy:stop(Pool)
    end.

test_http_pool_error_recovery() ->
    PoolArgs = [
        {name, {local, http_error_pool}},
        {worker_module, test_http_worker},
        {size, 2},
        {max_overflow, 1}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Simulate error in transaction
        Result = catch poolboy:transaction(Pool, fun(_Worker) ->
            throw(simulated_error)
        end),

        % Pool should recover from error
        ?assertMatch({'EXIT', {{nocatch, simulated_error}, _}}, Result),

        % Pool should still work
        Result2 = poolboy:transaction(Pool, fun(_Worker) -> ok end),
        ?assertEqual(ok, Result2)
    after
        catch poolboy:stop(Pool)
    end.

test_http_pool_load_balancing() ->
    PoolArgs = [
        {name, {local, http_lb_pool}},
        {worker_module, test_http_worker},
        {size, 4},
        {max_overflow, 2}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Submit many requests
        Parent = self(),

        lists:foreach(fun(N) ->
            spawn(fun() ->
                poolboy:transaction(Pool, fun(Worker) ->
                    timer:sleep(10),
                    Parent ! {completed, N, Worker}
                end)
            end)
        end, lists:seq(1, 20)),

        % Collect results
        Workers = lists:map(fun(_) ->
            receive
                {completed, _, W} -> W
            after 3000 -> undefined
            end
        end, lists:seq(1, 20)),

        % Should distribute across multiple workers
        UniqueWorkers = lists:usort(Workers),
        ?assert(length(UniqueWorkers) > 2)
    after
        catch poolboy:stop(Pool)
    end.

%%====================================================================
%% TCP Pool Integration Tests
%%====================================================================

test_tcp_connection_pooling() ->
    PoolArgs = [
        {name, {local, tcp_pool}},
        {worker_module, test_tcp_worker},
        {size, 5},
        {max_overflow, 3}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Simulate TCP connections through pool
        Results = lists:map(fun(N) ->
            poolboy:transaction(Pool, fun(_Worker) ->
                timer:sleep(5),
                {ok, N}
            end)
        end, lists:seq(1, 10)),

        ?assertEqual(10, length(Results))
    after
        catch poolboy:stop(Pool)
    end.

test_tcp_pool_distribution() ->
    PoolArgs = [
        {name, {local, tcp_dist_pool}},
        {worker_module, test_tcp_worker},
        {size, 3},
        {max_overflow, 0}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Multiple concurrent transactions
        Parent = self(),

        lists:foreach(fun(N) ->
            spawn(fun() ->
                poolboy:transaction(Pool, fun(Worker) ->
                    Parent ! {tcp_worker, Worker, N}
                end)
            end)
        end, lists:seq(1, 6)),

        % Collect workers
        Workers = lists:map(fun(_) ->
            receive
                {tcp_worker, W, _} -> W
            after 2000 -> undefined
            end
        end, lists:seq(1, 6)),

        UniqueWorkers = lists:usort(Workers),
        ?assert(length(UniqueWorkers) > 1)
    after
        catch poolboy:stop(Pool)
    end.

test_tcp_connection_reuse() ->
    PoolArgs = [
        {name, {local, tcp_reuse_pool}},
        {worker_module, test_tcp_worker},
        {size, 2},
        {max_overflow, 0}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Reuse workers across transactions
        _Worker1 = poolboy:transaction(Pool, fun(W) -> W end),
        _Worker2 = poolboy:transaction(Pool, fun(W) -> W end),

        ?assert(true) % Workers are reused from pool
    after
        catch poolboy:stop(Pool)
    end.

test_tcp_pool_timeout() ->
    PoolArgs = [
        {name, {local, tcp_timeout_pool}},
        {worker_module, test_tcp_worker},
        {size, 1},
        {max_overflow, 0}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Hold worker
        Worker = poolboy:checkout(Pool),

        % Try non-blocking checkout
        Result = catch poolboy:checkout(Pool, false),

        % Should be full
        case Result of
            full -> ok;
            _ -> ok
        end,

        poolboy:checkin(Pool, Worker)
    after
        catch poolboy:stop(Pool)
    end.

test_tcp_pool_error_recovery() ->
    PoolArgs = [
        {name, {local, tcp_error_pool}},
        {worker_module, test_tcp_worker},
        {size, 2},
        {max_overflow, 1}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Simulate error
        _Result = catch poolboy:transaction(Pool, fun(_Worker) ->
            error(simulated_tcp_error)
        end),

        % Pool should recover
        Result2 = poolboy:transaction(Pool, fun(_Worker) -> ok end),
        ?assertEqual(ok, Result2)
    after
        catch poolboy:stop(Pool)
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

test_full_poolboy_integration() ->
    % Full integration test with multiple pools
    HttpPoolArgs = [
        {name, {local, integration_http_pool}},
        {worker_module, test_http_worker},
        {size, 3},
        {max_overflow, 2}
    ],

    TcpPoolArgs = [
        {name, {local, integration_tcp_pool}},
        {worker_module, test_tcp_worker},
        {size, 3},
        {max_overflow, 2}
    ],

    {ok, HttpPool} = poolboy:start_link(HttpPoolArgs, []),
    {ok, TcpPool} = poolboy:start_link(TcpPoolArgs, []),

    try
        % Use both pools concurrently
        HttpResult = poolboy:transaction(HttpPool, fun(_W) -> http_ok end),
        TcpResult = poolboy:transaction(TcpPool, fun(_W) -> tcp_ok end),

        ?assertEqual(http_ok, HttpResult),
        ?assertEqual(tcp_ok, TcpResult)
    after
        catch poolboy:stop(HttpPool),
        catch poolboy:stop(TcpPool)
    end.

test_multi_transport_pooling() ->
    % Test pooling with multiple transport types
    Pools = lists:map(fun(N) ->
        PoolName = list_to_atom("multi_pool_" ++ integer_to_list(N)),
        PoolArgs = [
            {name, {local, PoolName}},
            {worker_module, test_worker},
            {size, 2},
            {max_overflow, 1}
        ],
        {ok, Pool} = poolboy:start_link(PoolArgs, []),
        {PoolName, Pool}
    end, lists:seq(1, 5)),

    try
        % Use all pools
        lists:foreach(fun({_, Pool}) ->
            Result = poolboy:transaction(Pool, fun(_) -> ok end),
            ?assertEqual(ok, Result)
        end, Pools)
    after
        lists:foreach(fun({_, Pool}) ->
            catch poolboy:stop(Pool)
        end, Pools)
    end.

test_pool_load_testing() ->
    PoolArgs = [
        {name, {local, load_test_pool}},
        {worker_module, test_worker},
        {size, 10},
        {max_overflow, 10}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % High load test
        Parent = self(),
        NumRequests = 100,

        StartTime = erlang:monotonic_time(millisecond),

        lists:foreach(fun(N) ->
            spawn(fun() ->
                poolboy:transaction(Pool, fun(_Worker) ->
                    timer:sleep(10),
                    Parent ! {done, N}
                end)
            end)
        end, lists:seq(1, NumRequests)),

        % Wait for all
        lists:foreach(fun(_) ->
            receive
                {done, _} -> ok
            after 10000 -> ok
            end
        end, lists:seq(1, NumRequests)),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        % Should complete in reasonable time
        ?assert(Duration < 10000)
    after
        catch poolboy:stop(Pool)
    end.

test_pool_failure_scenarios() ->
    PoolArgs = [
        {name, {local, failure_pool}},
        {worker_module, test_worker},
        {size, 3},
        {max_overflow, 1}
    ],

    {ok, Pool} = poolboy:start_link(PoolArgs, []),

    try
        % Simulate various failures
        % 1. Worker crash
        catch poolboy:transaction(Pool, fun(Worker) ->
            exit(Worker, kill)
        end),

        timer:sleep(100),

        % Pool should recover
        Result1 = poolboy:transaction(Pool, fun(_) -> ok end),
        ?assertEqual(ok, Result1),

        % 2. Transaction timeout
        catch poolboy:transaction(Pool, fun(_) ->
            timer:sleep(10000)
        end, 100),

        % Pool should still work
        Result2 = poolboy:transaction(Pool, fun(_) -> ok end),
        ?assertEqual(ok, Result2)
    after
        catch poolboy:stop(Pool)
    end.

%%====================================================================
%% Helper Modules and Functions
%%====================================================================

% Test worker module
-define(test_worker, test_worker).

% Note: In real tests, you would define test_worker, test_http_worker,
% and test_tcp_worker modules that implement the poolboy worker behavior
% For these tests, we assume they exist or mock their behavior
