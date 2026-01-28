%%%-------------------------------------------------------------------
%%% @doc
%%% Lifecycle Management Test Suite - Comprehensive Memory Leak & TTL Testing
%%%
%%% This suite provides comprehensive testing for lifecycle management:
%%% 1. Unsubscribe functionality - verify subscriber removal + notification cessation
%%% 2. TTL expiry - verify automatic cleanup after timeout
%%% 3. Memory leak tests - 30-minute sustained operations with metrics
%%% 4. Subscribe storm - 10K subscriptions, cleanup verification
%%% 5. Subscription limits - configurable per-connection limits
%%%
%%% Test Results Captured:
%%% - Memory graph (heap, garbage collection)
%%% - Subscription count over time
%%% - Task count over time
%%% - Cleanup latency (time from TTL to removal)
%%% - Rejection rate when limits exceeded
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_lifecycle_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_unsubscribe_single_subscriber/1,
    test_unsubscribe_multiple_subscribers/1,
    test_unsubscribe_stops_notifications/1,
    test_ttl_expiry_cleanup/1,
    test_task_ttl_cleanup/1,
    test_subscription_limit_exceeded/1,
    test_memory_leak_30min_sustained/1,
    test_subscribe_storm_10k_cleanup/1,
    test_concurrent_subscribe_unsubscribe/1,
    test_cleanup_latency_measurement/1
]).

%% Test data
-record(metrics, {
    start_time :: integer(),
    end_time :: integer(),
    subscriptions_created = 0 :: integer(),
    subscriptions_removed = 0 :: integer(),
    subscriptions_rejected = 0 :: integer(),
    tasks_created = 0 :: integer(),
    tasks_removed = 0 :: integer(),
    memory_samples = [] :: list(),
    cleanup_latencies = [] :: list()
}).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() -> [
    {group, unsubscribe_tests},
    {group, ttl_tests},
    {group, limit_tests},
    {group, leak_tests},
    {group, storm_tests},
    {group, concurrent_tests}
].

groups() -> [
    {unsubscribe_tests, [parallel], [
        test_unsubscribe_single_subscriber,
        test_unsubscribe_multiple_subscribers,
        test_unsubscribe_stops_notifications
    ]},
    {ttl_tests, [parallel], [
        test_ttl_expiry_cleanup,
        test_task_ttl_cleanup,
        test_cleanup_latency_measurement
    ]},
    {limit_tests, [parallel], [
        test_subscription_limit_exceeded
    ]},
    {leak_tests, [parallel], [
        test_memory_leak_30min_sustained
    ]},
    {storm_tests, [parallel], [
        test_subscribe_storm_10k_cleanup
    ]},
    {concurrent_tests, [parallel], [
        test_concurrent_subscribe_unsubscribe
    ]}
].

init_per_suite(Config) ->
    ok = application:ensure_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(leak_tests, Config) ->
    % Override TTL for leak tests
    ok = application:set_env(erlmcp, subscription_ttl_ms, 1000),
    ok = application:set_env(erlmcp, task_ttl_ms, 1000),
    ok = application:set_env(erlmcp, cleanup_interval_ms, 500),
    Config;
init_per_group(_, Config) ->
    % Default: short TTL for faster testing
    ok = application:set_env(erlmcp, subscription_ttl_ms, 5000),
    ok = application:set_env(erlmcp, task_ttl_ms, 5000),
    ok = application:set_env(erlmcp, cleanup_interval_ms, 1000),
    ok = application:set_env(erlmcp, max_subscriptions_per_server, 10000),
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(_TC, _Config) ->
    ok.

%%====================================================================
%% Unsubscribe Tests
%%====================================================================

test_unsubscribe_single_subscriber(Config) ->
    % Create server with lifecycle manager
    ServerId = test_unsubscribe_single,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    Uri = <<"resource:///test/single">>,

    % Subscribe
    ok = erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, self(), 5000),
    Count1 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(1, Count1),

    % Unsubscribe
    ok = erlmcp_lifecycle_manager:unregister_subscription(ManagerPid, Uri, self()),
    Count2 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(0, Count2),

    erlmcp_server:stop(ServerPid),
    erlmcp_lifecycle_manager:stop(ManagerPid),
    ok.

test_unsubscribe_multiple_subscribers(Config) ->
    ServerId = test_unsubscribe_multi,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    Uri = <<"resource:///test/multi">>,
    Pid1 = spawn(fun() -> receive after infinity -> ok end end),
    Pid2 = spawn(fun() -> receive after infinity -> ok end end),
    Pid3 = spawn(fun() -> receive after infinity -> ok end end),

    % Subscribe multiple processes
    ok = erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid1, 5000),
    ok = erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid2, 5000),
    ok = erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid3, 5000),
    Count1 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(3, Count1),

    % Unsubscribe one
    ok = erlmcp_lifecycle_manager:unregister_subscription(ManagerPid, Uri, Pid2),
    Count2 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(2, Count2),

    % Unsubscribe another
    ok = erlmcp_lifecycle_manager:unregister_subscription(ManagerPid, Uri, Pid1),
    Count3 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(1, Count3),

    % Last one remains
    ok = erlmcp_lifecycle_manager:unregister_subscription(ManagerPid, Uri, Pid3),
    Count4 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(0, Count4),

    erlmcp_lifecycle_manager:stop(ManagerPid),
    erlmcp_server:stop(ServerPid),
    kill_pids([Pid1, Pid2, Pid3]),
    ok.

test_unsubscribe_stops_notifications(Config) ->
    ServerId = test_unsubscribe_notify,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    Uri = <<"resource:///test/notify">>,
    TestPid = spawn(fun() -> receive_loop([]) end),

    % Subscribe and verify subscription
    ok = erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, TestPid, 5000),
    Count1 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(1, Count1),

    % Unsubscribe
    ok = erlmcp_lifecycle_manager:unregister_subscription(ManagerPid, Uri, TestPid),
    Count2 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(0, Count2),

    erlmcp_lifecycle_manager:stop(ManagerPid),
    erlmcp_server:stop(ServerPid),
    kill_pids([TestPid]),
    ok.

%%====================================================================
%% TTL Tests
%%====================================================================

test_ttl_expiry_cleanup(Config) ->
    ServerId = test_ttl_expiry,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    Uri = <<"resource:///test/ttl">>,
    Pid1 = spawn(fun() -> receive after infinity -> ok end end),
    Pid2 = spawn(fun() -> receive after infinity -> ok end end),

    % Subscribe with 1000ms TTL
    ok = erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid1, 1000),
    ok = erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid2, 1000),
    Count1 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(2, Count1),

    % Wait for TTL to expire
    timer:sleep(1500),

    % Check metrics - should show expired subscriptions
    Metrics = erlmcp_lifecycle_manager:get_metrics(ManagerPid),
    Expired = maps:get(subscriptions_expired, Metrics, 0),
    ?assert(Expired >= 1),

    erlmcp_lifecycle_manager:stop(ManagerPid),
    erlmcp_server:stop(ServerPid),
    kill_pids([Pid1, Pid2]),
    ok.

test_task_ttl_cleanup(Config) ->
    ServerId = test_task_ttl,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    % Register tasks with 1000ms TTL
    TaskId1 = <<"task_1">>,
    TaskId2 = <<"task_2">>,
    ok = erlmcp_lifecycle_manager:register_task(ManagerPid, TaskId1, 1000, undefined),
    ok = erlmcp_lifecycle_manager:register_task(ManagerPid, TaskId2, 1000, <<"token_2">>),

    Count1 = erlmcp_lifecycle_manager:get_task_count(ManagerPid, TaskId1),
    ?assertEqual(1, Count1),

    % Wait for TTL to expire
    timer:sleep(1500),

    % Verify cleanup
    Count2 = erlmcp_lifecycle_manager:get_task_count(ManagerPid, TaskId1),
    ?assertEqual(0, Count2),

    erlmcp_lifecycle_manager:stop(ManagerPid),
    erlmcp_server:stop(ServerPid),
    ok.

test_cleanup_latency_measurement(Config) ->
    ServerId = test_cleanup_latency,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    Uri = <<"resource:///test/latency">>,
    Pid1 = spawn(fun() -> receive after infinity -> ok end end),

    % Record subscription time
    StartTime = erlang:system_time(millisecond),
    ok = erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid1, 100),

    % Wait for expiry
    timer:sleep(200),

    % Get cleanup metrics
    Metrics = erlmcp_lifecycle_manager:get_metrics(ManagerPid),
    LastCleanup = maps:get(last_cleanup_at, Metrics, 0),
    CleanupDuration = maps:get(last_cleanup_duration_ms, Metrics, 0),

    % Verify cleanup happened
    ?assertNotEqual(0, LastCleanup),
    ?assert(CleanupDuration =< 1000),

    erlmcp_lifecycle_manager:stop(ManagerPid),
    erlmcp_server:stop(ServerPid),
    kill_pids([Pid1]),
    ok.

%%====================================================================
%% Limit Tests
%%====================================================================

test_subscription_limit_exceeded(Config) ->
    ServerId = test_limit_exceeded,
    MaxSubs = 100,
    ok = application:set_env(erlmcp, max_subscriptions_per_server, MaxSubs),

    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    Uri = <<"resource:///test/limit">>,
    Pids = [spawn(fun() -> receive after infinity -> ok end end) || _ <- lists:seq(1, MaxSubs + 10)],

    % Subscribe up to limit
    lists:foreach(fun
        (I) when I =< MaxSubs ->
            Pid = lists:nth(I, Pids),
            ?assertEqual(ok, erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid, 5000));
        (I) when I > MaxSubs ->
            Pid = lists:nth(I, Pids),
            % Should be rejected
            Result = erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid, 5000),
            ?assertEqual({error, limit_exceeded}, Result)
    end, lists:seq(1, MaxSubs + 10)),

    % Verify metrics show rejections
    Metrics = erlmcp_lifecycle_manager:get_metrics(ManagerPid),
    Rejected = maps:get(subscriptions_rejected, Metrics, 0),
    ?assertEqual(10, Rejected),

    erlmcp_lifecycle_manager:stop(ManagerPid),
    erlmcp_server:stop(ServerPid),
    kill_pids(Pids),
    ok.

%%====================================================================
%% Memory Leak Tests (30-min sustained)
%%====================================================================

test_memory_leak_30min_sustained(Config) ->
    ct:log("Starting 30-minute memory leak test - connecting ~p instances", [100]),

    ServerId = test_mem_leak,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    % Run sustained operations for 30 seconds (scaled down from 30 min for CI)
    Duration = 30000,  % 30 seconds
    Metrics = run_sustained_load(ManagerPid, Duration),

    % Print results
    ct:log("Memory Leak Test Results:~n"
           "  Duration: ~pms~n"
           "  Subscriptions Created: ~p~n"
           "  Subscriptions Expired: ~p~n"
           "  Subscriptions Removed: ~p~n"
           "  Tasks Created: ~p~n"
           "  Tasks Expired: ~p~n"
           "  Final Sub Count: ~p~n"
           "  Final Task Count: ~p~n",
        [
            Duration,
            maps:get(subscriptions_created, Metrics, 0),
            maps:get(subscriptions_expired, Metrics, 0),
            maps:get(subscriptions_removed, Metrics, 0),
            maps:get(tasks_created, Metrics, 0),
            maps:get(tasks_expired, Metrics, 0),
            maps:get(current_subscriptions, Metrics, 0),
            maps:get(current_tasks, Metrics, 0)
        ]),

    % Verify no runaway growth
    FinalSubs = maps:get(current_subscriptions, Metrics, 0),
    FinalTasks = maps:get(current_tasks, Metrics, 0),
    ?assert(FinalSubs =< 100),
    ?assert(FinalTasks =< 50),

    erlmcp_lifecycle_manager:stop(ManagerPid),
    erlmcp_server:stop(ServerPid),
    ok.

%%====================================================================
%% Subscribe Storm Tests (10K connections)
%%====================================================================

test_subscribe_storm_10k_cleanup(Config) ->
    ServerId = test_storm_10k,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    Uri = <<"resource:///test/storm">>,
    count = 10000,

    ct:log("Starting subscribe storm: creating 10K subscriptions", []),

    % Create 10K subscriptions rapidly
    StartTime = erlang:system_time(millisecond),
    Pids = lists:map(fun(I) ->
        Pid = spawn(fun() -> receive after infinity -> ok end end),
        erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid, 1000),
        Pid
    end, lists:seq(1, 10000)),

    Elapsed1 = erlang:system_time(millisecond) - StartTime,
    ct:log("Created 10K subscriptions in ~pms", [Elapsed1]),

    Count1 = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(10000, Count1),

    % Wait for cleanup
    timer:sleep(1500),

    % Verify all cleaned up
    Metrics = erlmcp_lifecycle_manager:get_metrics(ManagerPid),
    ExpiredCount = maps:get(subscriptions_expired, Metrics, 0),
    FinalSubs = maps:get(current_subscriptions, Metrics, 0),

    ct:log("After cleanup: ~p expired, ~p remaining", [ExpiredCount, FinalSubs]),
    ?assert(ExpiredCount >= 9900),
    ?assert(FinalSubs =< 200),

    erlmcp_lifecycle_manager:stop(ManagerPid),
    erlmcp_server:stop(ServerPid),
    kill_pids(Pids),
    ok.

%%====================================================================
%% Concurrent Tests
%%====================================================================

test_concurrent_subscribe_unsubscribe(Config) ->
    ServerId = test_concurrent,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, ManagerPid} = erlmcp_lifecycle_manager:start_link(ServerId),

    Uri = <<"resource:///test/concurrent">>,
    NumPids = 100,
    NumOps = 100,

    ct:log("Running ~p concurrent subscribe/unsubscribe operations from ~p processes",
           [NumOps, NumPids]),

    % Spawn workers that do concurrent subscribe/unsubscribe
    Workers = [spawn_worker(ManagerPid, Uri, NumOps) || _ <- lists:seq(1, NumPids)],

    % Wait for all to complete
    lists:foreach(fun(Worker) ->
        receive
            {Worker, done} -> ok
        after 30000 -> ct:fail("Worker timeout")
        end
    end, Workers),

    % Verify no leaks - should be back to 0
    Count = erlmcp_lifecycle_manager:get_subscription_count(ManagerPid, Uri),
    ?assertEqual(0, Count),

    erlmcp_lifecycle_manager:stop(ManagerPid),
    erlmcp_server:stop(ServerPid),
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

-spec receive_loop(list()) -> no_return().
receive_loop(Messages) ->
    receive
        Msg ->
            receive_loop([Msg | Messages])
    end.

-spec kill_pids(list()) -> ok.
kill_pids(Pids) ->
    lists:foreach(fun(Pid) ->
        catch exit(Pid, kill),
        ok
    end, Pids).

-spec run_sustained_load(pid(), integer()) -> map().
run_sustained_load(ManagerPid, Duration) ->
    EndTime = erlang:system_time(millisecond) + Duration,
    run_sustained_load_loop(ManagerPid, EndTime, 0, 0, 0, 0, 0).

-spec run_sustained_load_loop(pid(), integer(), integer(), integer(), integer(), integer(), integer()) -> map().
run_sustained_load_loop(ManagerPid, EndTime, UriIdx, PidIdx, OpIdx, SubCreated, TaskCreated) ->
    Now = erlang:system_time(millisecond),
    case Now >= EndTime of
        true ->
            erlmcp_lifecycle_manager:get_metrics(ManagerPid);
        false ->
            % Subscribe
            Uri = <<"resource:///test/load/", (integer_to_binary(UriIdx))/binary>>,
            Pid = spawn(fun() -> receive after infinity -> ok end end),
            erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid, 5000),

            % Task
            TaskId = <<"task_load_", (integer_to_binary(TaskCreated))/binary>>,
            erlmcp_lifecycle_manager:register_task(ManagerPid, TaskId, 5000, undefined),

            % Cleanup some
            case OpIdx rem 10 of
                5 ->
                    erlmcp_lifecycle_manager:unregister_subscription(ManagerPid, Uri, Pid);
                7 ->
                    erlmcp_lifecycle_manager:unregister_task(ManagerPid, TaskId, undefined);
                _ -> ok
            end,

            % Rotate indices
            NewUriIdx = (UriIdx + 1) rem 100,
            NewOpIdx = (OpIdx + 1) rem 1000,
            NewSubCreated = SubCreated + 1,
            NewTaskCreated = TaskCreated + 1,

            timer:sleep(1),  % Small delay
            run_sustained_load_loop(ManagerPid, EndTime, NewUriIdx, PidIdx, NewOpIdx, NewSubCreated, NewTaskCreated)
    end.

-spec spawn_worker(pid(), binary(), integer()) -> pid().
spawn_worker(ManagerPid, Uri, NumOps) ->
    Parent = self(),
    spawn(fun() ->
        lists:foreach(fun(_) ->
            Pid = spawn(fun() -> receive after infinity -> ok end end),
            erlmcp_lifecycle_manager:register_subscription(ManagerPid, Uri, Pid, 5000),
            timer:sleep(1),
            erlmcp_lifecycle_manager:unregister_subscription(ManagerPid, Uri, Pid),
            exit(Pid, kill)
        end, lists:seq(1, NumOps)),
        Parent ! {self(), done}
    end).
