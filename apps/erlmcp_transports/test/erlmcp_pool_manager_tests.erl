%%%====================================================================
%%% erlmcp_pool_manager_tests.erl - Pool Manager Test Suite
%%%====================================================================

-module(erlmcp_pool_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Dummy worker module for testing
-export([start_link/1]).

start_link(_Opts) ->
    Pid = spawn_link(fun() ->
        receive
            stop -> ok
        end
    end),
    {ok, Pid}.

%%====================================================================
%% Basic Operations Tests
%%====================================================================

pool_creation_test() ->
    Opts = #{
        min_size => 5,
        max_size => 20,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),
    ?assert(is_pid(Pool)),

    Status = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(5, maps:get(current_size, Status)),
    ?assertEqual(5, maps:get(idle_count, Status)),
    ?assertEqual(0, maps:get(active_count, Status)),

    erlmcp_pool_manager:stop(Pool).

checkout_checkin_test() ->
    Opts = #{
        min_size => 3,
        max_size => 10,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Checkout connection
    {ok, Conn1} = erlmcp_pool_manager:checkout(Pool),
    ?assert(is_pid(Conn1)),

    %% Check status
    Status = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(1, maps:get(active_count, Status)),
    ?assertEqual(2, maps:get(idle_count, Status)),

    %% Check in connection
    ok = erlmcp_pool_manager:checkin(Pool, Conn1),

    %% Verify connection returned
    Status2 = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(0, maps:get(active_count, Status2)),
    ?assertEqual(3, maps:get(idle_count, Status2)),

    erlmcp_pool_manager:stop(Pool).

multiple_checkouts_test() ->
    Opts = #{
        min_size => 5,
        max_size => 20,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Checkout multiple connections
    {ok, Conn1} = erlmcp_pool_manager:checkout(Pool),
    {ok, Conn2} = erlmcp_pool_manager:checkout(Pool),
    {ok, Conn3} = erlmcp_pool_manager:checkout(Pool),

    %% All should be different
    ?assert(Conn1 =/= Conn2),
    ?assert(Conn2 =/= Conn3),
    ?assert(Conn1 =/= Conn3),

    %% Check status
    Status = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(3, maps:get(active_count, Status)),
    ?assertEqual(2, maps:get(idle_count, Status)),

    %% Return all
    ok = erlmcp_pool_manager:checkin(Pool, Conn1),
    ok = erlmcp_pool_manager:checkin(Pool, Conn2),
    ok = erlmcp_pool_manager:checkin(Pool, Conn3),

    Status2 = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(0, maps:get(active_count, Status2)),
    ?assertEqual(5, maps:get(idle_count, Status2)),

    erlmcp_pool_manager:stop(Pool).

%%====================================================================
%% Strategy Tests
%%====================================================================

round_robin_strategy_test() ->
    Opts = #{
        min_size => 3,
        max_size => 10,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Checkout and return multiple times to verify round-robin
    Conns = [begin
        {ok, Conn} = erlmcp_pool_manager:checkout(Pool),
        ok = erlmcp_pool_manager:checkin(Pool, Conn),
        Conn
    end || _ <- lists:seq(1, 6)],

    %% Should cycle through connections
    [C1, C2, C3, C4, C5, C6] = Conns,
    ?assertEqual(C1, C4),  % Should wrap around
    ?assertEqual(C2, C5),
    ?assertEqual(C3, C6),

    erlmcp_pool_manager:stop(Pool).

least_loaded_strategy_test() ->
    Opts = #{
        min_size => 3,
        max_size => 10,
        strategy => least_loaded,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Checkout connection (should get least loaded)
    {ok, Conn1} = erlmcp_pool_manager:checkout(Pool),
    ?assert(is_pid(Conn1)),

    %% Return and checkout again
    ok = erlmcp_pool_manager:checkin(Pool, Conn1),
    {ok, Conn2} = erlmcp_pool_manager:checkout(Pool),

    %% With least_loaded, after equal usage, should select first available
    ?assert(is_pid(Conn2)),

    erlmcp_pool_manager:stop(Pool).

random_strategy_test() ->
    Opts = #{
        min_size => 10,
        max_size => 50,
        strategy => random,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Checkout many connections
    Conns = [begin
        {ok, Conn} = erlmcp_pool_manager:checkout(Pool),
        ok = erlmcp_pool_manager:checkin(Pool, Conn),
        Conn
    end || _ <- lists:seq(1, 20)],

    %% Should have some distribution (not all same)
    UniqueConns = lists:usort(Conns),
    ?assert(length(UniqueConns) > 1),

    erlmcp_pool_manager:stop(Pool).

%%====================================================================
%% Pool Sizing Tests
%%====================================================================

pool_resize_grow_test() ->
    Opts = #{
        min_size => 5,
        max_size => 50,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Initial size
    Status1 = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(5, maps:get(current_size, Status1)),

    %% Grow pool
    ok = erlmcp_pool_manager:resize(Pool, 15),

    %% Verify growth
    Status2 = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(15, maps:get(current_size, Status2)),

    erlmcp_pool_manager:stop(Pool).

pool_resize_shrink_test() ->
    Opts = #{
        min_size => 5,
        max_size => 50,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Grow first
    ok = erlmcp_pool_manager:resize(Pool, 20),
    Status1 = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(20, maps:get(current_size, Status1)),

    %% Shrink pool
    ok = erlmcp_pool_manager:resize(Pool, 10),

    %% Verify shrinkage
    Status2 = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(10, maps:get(current_size, Status2)),

    erlmcp_pool_manager:stop(Pool).

pool_resize_bounds_test() ->
    Opts = #{
        min_size => 5,
        max_size => 50,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Try to shrink below minimum
    {error, {below_minimum, 5}} = erlmcp_pool_manager:resize(Pool, 3),

    %% Try to grow above maximum
    {error, {above_maximum, 50}} = erlmcp_pool_manager:resize(Pool, 100),

    erlmcp_pool_manager:stop(Pool).

%%====================================================================
%% Metrics Tests
%%====================================================================

metrics_tracking_test() ->
    Opts = #{
        min_size => 5,
        max_size => 20,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Initial metrics
    Metrics1 = erlmcp_pool_manager:get_metrics(Pool),
    ?assertEqual(0, maps:get(total_checkouts, Metrics1)),
    ?assertEqual(5, maps:get(idle_connections, Metrics1)),
    ?assertEqual(0, maps:get(active_connections, Metrics1)),

    %% Checkout and checkin
    {ok, Conn} = erlmcp_pool_manager:checkout(Pool),
    ok = erlmcp_pool_manager:checkin(Pool, Conn),

    %% Check metrics updated
    Metrics2 = erlmcp_pool_manager:get_metrics(Pool),
    ?assertEqual(1, maps:get(total_checkouts, Metrics2)),

    %% Verify utilization calculation
    ?assert(is_float(maps:get(pool_utilization_percent, Metrics2))),
    ?assert(is_float(maps:get(avg_checkout_time_us, Metrics2))),

    erlmcp_pool_manager:stop(Pool).

utilization_calculation_test() ->
    Opts = #{
        min_size => 10,
        max_size => 20,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Checkout half the connections
    Conns = [begin
        {ok, Conn} = erlmcp_pool_manager:checkout(Pool),
        Conn
    end || _ <- lists:seq(1, 5)],

    %% Check utilization
    Metrics = erlmcp_pool_manager:get_metrics(Pool),
    Utilization = maps:get(pool_utilization_percent, Metrics),

    %% Should be approximately 50%
    ?assert(Utilization >= 40.0 andalso Utilization =< 60.0),

    %% Return connections
    [erlmcp_pool_manager:checkin(Pool, C) || C <- Conns],

    erlmcp_pool_manager:stop(Pool).

%%====================================================================
%% Health Check Tests
%%====================================================================

health_check_test_() ->
    {timeout, 10, fun() ->
        Opts = #{
            min_size => 5,
            max_size => 20,
            strategy => round_robin,
            health_check_interval => 500,  % Fast health checks for testing
            worker_module => ?MODULE,
            worker_opts => #{}
        },

        {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

        %% Wait for at least one health check cycle
        timer:sleep(1000),

        %% Status should still show healthy pool
        Status = erlmcp_pool_manager:get_status(Pool),
        ?assertEqual(healthy, maps:get(health_status, Status)),

        erlmcp_pool_manager:stop(Pool)
    end}.

connection_recovery_test() ->
    Opts = #{
        min_size => 5,
        max_size => 20,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Checkout a connection
    {ok, Conn} = erlmcp_pool_manager:checkout(Pool),

    %% Kill the connection
    exit(Conn, kill),

    %% Wait for recovery
    timer:sleep(100),

    %% Pool should maintain minimum size
    Status = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(5, maps:get(current_size, Status)),

    erlmcp_pool_manager:stop(Pool).

%%====================================================================
%% Concurrent Access Tests
%%====================================================================

concurrent_checkouts_test_() ->
    {timeout, 15, fun() ->
        Opts = #{
            min_size => 10,
            max_size => 100,
            strategy => round_robin,
            worker_module => ?MODULE,
            worker_opts => #{}
        },

        {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

        %% Spawn concurrent workers
        Parent = self(),
        Workers = [spawn(fun() ->
            {ok, Conn} = erlmcp_pool_manager:checkout(Pool),
            timer:sleep(10),
            ok = erlmcp_pool_manager:checkin(Pool, Conn),
            Parent ! {worker_done, self()}
        end) || _ <- lists:seq(1, 50)],

        %% Wait for all workers
        [receive {worker_done, W} -> ok after 5000 -> error(timeout) end || W <- Workers],

        %% Verify pool is consistent
        Status = erlmcp_pool_manager:get_status(Pool),
        ?assert(maps:get(current_size, Status) >= 10),
        ?assertEqual(0, maps:get(active_count, Status)),

        erlmcp_pool_manager:stop(Pool)
    end}.

high_concurrency_stress_test_() ->
    {timeout, 30, fun() ->
        Opts = #{
            min_size => 20,
            max_size => 200,
            strategy => least_loaded,
            worker_module => ?MODULE,
            worker_opts => #{}
        },

        {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

        %% Spawn many concurrent workers
        Parent = self(),
        Workers = [spawn(fun() ->
            lists:foreach(fun(_) ->
                {ok, Conn} = erlmcp_pool_manager:checkout(Pool),
                ok = erlmcp_pool_manager:checkin(Pool, Conn)
            end, lists:seq(1, 10)),
            Parent ! {worker_done, self()}
        end) || _ <- lists:seq(1, 100)],

        %% Wait for completion
        [receive {worker_done, W} -> ok after 10000 -> error(timeout) end || W <- Workers],

        %% Check metrics
        Metrics = erlmcp_pool_manager:get_metrics(Pool),
        ?assertEqual(1000, maps:get(total_checkouts, Metrics)),  % 100 workers * 10 checkouts
        ?assertEqual(0, maps:get(failed_checkouts, Metrics)),

        erlmcp_pool_manager:stop(Pool)
    end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

no_connections_available_test() ->
    Opts = #{
        min_size => 2,
        max_size => 2,  % Small pool
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{},
        checkout_timeout => 100  % Short timeout
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Checkout all connections
    {ok, Conn1} = erlmcp_pool_manager:checkout(Pool),
    {ok, Conn2} = erlmcp_pool_manager:checkout(Pool),

    %% Try to checkout when none available (pool at max size)
    Status = erlmcp_pool_manager:get_status(Pool),
    ?assertEqual(2, maps:get(active_count, Status)),
    ?assertEqual(0, maps:get(idle_count, Status)),

    %% Return connections
    ok = erlmcp_pool_manager:checkin(Pool, Conn1),
    ok = erlmcp_pool_manager:checkin(Pool, Conn2),

    erlmcp_pool_manager:stop(Pool).

invalid_checkin_test() ->
    Opts = #{
        min_size => 5,
        max_size => 20,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Try to check in unknown connection
    FakePid = spawn(fun() -> ok end),
    ok = erlmcp_pool_manager:checkin(Pool, FakePid),  % Should not crash

    erlmcp_pool_manager:stop(Pool).

%%====================================================================
%% Integration Tests
%%====================================================================

pool_lifecycle_test() ->
    Opts = #{
        min_size => 10,
        max_size => 50,
        strategy => round_robin,
        worker_module => ?MODULE,
        worker_opts => #{}
    },

    %% Start pool
    {ok, Pool} = erlmcp_pool_manager:start_link(Opts),

    %% Simulate typical usage pattern
    lists:foreach(fun(_) ->
        {ok, Conn} = erlmcp_pool_manager:checkout(Pool),
        timer:sleep(1),
        ok = erlmcp_pool_manager:checkin(Pool, Conn)
    end, lists:seq(1, 100)),

    %% Verify metrics
    Metrics = erlmcp_pool_manager:get_metrics(Pool),
    ?assertEqual(100, maps:get(total_checkouts, Metrics)),
    ?assertEqual(0, maps:get(failed_checkouts, Metrics)),

    %% Clean shutdown
    erlmcp_pool_manager:stop(Pool).
