%%%-------------------------------------------------------------------
%%% @doc
%%% Distributed Lock Performance Benchmark
%%%
%%% Tests lock acquisition/release under high concurrency scenarios.
%%% Measures performance with multiple clients contending for locks.
%%%
%%% Benchmark Scenarios:
%%% - Single lock acquisition/release latency
%%% - Multiple locks contention (1-100 concurrent clients)
%%% - Lock acquisition with varying TTLs
%%% - Queue performance under heavy load
%%% - Failover scenarios with node failures
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distributed_lock_bench).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Export all functions for testing
-export([all/0, setup/0, cleanup/0,
         single_lock_latency/1,
         multiple_locks_contention/1,
         ttl_variation/1,
         queue_performance/1,
         failover_scenarios/1]).

%%====================================================================
%% Constants
%%====================================================================
-define(DEFAULT_LOCK_COUNT, 1000).
-define(MAX_CONCURRENT_CLIENTS, 100).
-define(DEFAULT_TTL_MS, 30000).
-define(DEFAULT_TIMEOUT_MS, 30000).

%%====================================================================
%% Test Configuration
%%====================================================================
all() ->
    [
        single_lock_latency,
        multiple_locks_contention,
        ttl_variation,
        queue_performance,
        failover_scenarios
    ].

setup() ->
    %% Start distributed lock manager
    case erlmcp_distributed_lock:start_link() of
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start distributed lock: ~p", [Reason]),
            error(start_failed)
    end.

cleanup() ->
    %% Stop distributed lock manager
    erlmcp_distributed_lock:stop(),
    ok.

%%====================================================================
%% Benchmark Tests
%%====================================================================

%% @doc Single lock acquisition/release latency measurement
single_lock_latency(_Config) ->
    LockName = <<"single_latency_lock">>,

    %% Warmup
    [begin
         ok = erlmcp_distributed_lock:acquire(LockName),
         ok = erlmcp_distributed_lock:release(make_ref())
     end || _ <- lists:seq(1, 100)],

    %% Measurement phase
    Results = [begin
                 StartTime = erlang:monotonic_time(microsecond),
                 Result = erlmcp_distributed_lock:acquire(LockName),
                 case Result of
                     {ok, LockRef} ->
                         ReleaseTime = erlang:monotonic_time(microsecond),
                         ok = erlmcp_distributed_lock:release(LockRef),
                         EndTime = erlang:monotonic_time(microsecond),
                         AcquireTime = ReleaseTime - StartTime,
                         ReleaseTime2 = EndTime - ReleaseTime,
                         TotalTime = EndTime - StartTime,
                         {acquire_time, AcquireTime, release_time, ReleaseTime2, total_time, TotalTime};
                     {error, _} -> {error, 0, 0, 0}
                 end
             end || _ <- lists:seq(1, ?DEFAULT_LOCK_COUNT)],

    %% Analyze results
    Successful = [T || {acquire_time, _, release_time, _, total_time, _} = T <- Results],
    Failed = Results -- Successful,

    AcquireTimes = [AcquireTime || {acquire_time, AcquireTime, _, _, _, _} <- Successful],
    ReleaseTimes = [ReleaseTime || {_, _, release_time, ReleaseTime, _, _} <- Successful],
    TotalTimes = [TotalTime || {_, _, _, _, total_time, TotalTime} <- Successful],

    #{
        test => single_lock_latency,
        total_requests => ?DEFAULT_LOCK_COUNT,
        successful => length(Successful),
        failed => length(Failed),
        success_rate => length(Successful) / ?DEFAULT_LOCK_COUNT,
        average_acquire_time => lists:sum(AcquireTimes) / length(AcquireTimes),
        min_acquire_time => lists:min(AcquireTimes),
        max_acquire_time => lists:max(AcquireTimes),
        p95_acquire_time => calculate_percentile(AcquireTimes, 95),
        p99_acquire_time => calculate_percentile(AcquireTimes, 99),
        average_release_time => lists:sum(ReleaseTimes) / length(ReleaseTimes),
        min_release_time => lists:min(ReleaseTimes),
        max_release_time => lists:max(ReleaseTimes),
        average_total_time => lists:sum(TotalTimes) / length(TotalTimes),
        throughput => length(Successful) / (lists:sum(TotalTimes) / 1000000) % ops/sec
    }.

%% @doc Multiple locks contention (1-100 concurrent clients)
multiple_locks_contention(_Config) ->
    ClientCounts = [1, 5, 10, 25, 50, 100],
    LockNames = [list_to_binary("contention_lock_" ++ integer_to_list(I)) ||
                 I <- lists:seq(1, 10)],

    Results = lists:map(fun(ClientCount) ->
        LocksPerClient = div(?DEFAULT_LOCK_COUNT div 10, max(1, ClientCount div 10)),

        %% Spawn concurrent clients
        Processes = [spawn_client(ContentionId, LockNames, LocksPerClient) ||
                        ContentionId <- lists:seq(1, ClientCount)],

        %% Wait for all clients to complete
        ClientResults = collect_client_results(Processes, []),

        SuccessCount = length([R || R <- ClientResults, R =:= success]),
        ErrorCount = length([R || R <- ClientResults, R =:= error]),

        %% Collect detailed timing if available
        TimingResults = [begin
                            gen_server:call(bench_client, get_timing, 1000)
                         end || Pid <- Processes, is_pid(Pid)],

        AcquireTimes = lists:flatten([T || {acquire_times, T} <- TimingResults]),
        ReleaseTimes = lists:flatten([T || {release_times, T} <- TimingResults]),

        #{
            concurrent_clients => ClientCount,
            total_operations => ClientCount * LocksPerClient,
            successful => SuccessCount,
            failed => ErrorCount,
            success_rate => SuccessCount / (ClientCount * LocksPerClient),
            average_acquire_time => case AcquireTimes of
                                      [] -> 0;
                                      _ -> lists:sum(AcquireTimes) / length(AcquireTimes)
                                  end,
            average_release_time => case ReleaseTimes of
                                      [] -> 0;
                                      _ -> lists:sum(ReleaseTimes) / length(ReleaseTimes)
                                  end,
            contention_ratio => calculate_contention_ratio(LockNames, ClientCount),
            throughput => SuccessCount / (case lists:sum(TimingResults) of
                                          0 -> 1; % Avoid division by zero
                                          _ -> lists:sum([T || {acquire_times, T} <- TimingResults]) / 1000000
                                      end)
        }
    end, ClientCounts),

    #{
        test => multiple_locks_contention,
        results => Results
    }.

%% @doc Lock acquisition with varying TTLs
ttl_variation(_Config) ->
    TtlValues = [1000, 5000, 10000, 30000, 60000], % 1s to 60s

    Results = lists:map(fun(TTL) ->
        LockName = list_to_binary("ttl_lock_" ++ integer_to_list(TTL)),

        Results = [begin
                     Options = #{ttl_ms => TTL,
                                wait_timeout_ms => 1000},

                     StartTime = erlang:monotonic_time(microsecond),
                     Result = erlmcp_distributed_lock:acquire(LockName, Options),
                     EndTime = erlang:monotonic_time(microsecond),

                     case Result of
                         {ok, LockRef} ->
                             ok = erlmcp_distributed_lock:release(LockRef),
                             {success, EndTime - StartTime};
                         {error, locked} ->
                             {failed_locked, EndTime - StartTime};
                         {error, _} ->
                             {failed_other, EndTime - StartTime}
                     end
                 end || _ <- lists:seq(1, ?DEFAULT_LOCK_COUNT)],

        SuccessCount = length([R || {success, _} <- Results]),
        LockedCount = length([R || {failed_locked, _} <- Results]),
        ErrorCount = length([R || {failed_other, _} <- Results]),

        AcquireTimes = [T || {success, T} <- Results],

        #{
            ttl_ms => TTL,
            total_requests => ?DEFAULT_LOCK_COUNT,
            successful => SuccessCount,
            failed_locked => LockedCount,
            failed_other => ErrorCount,
            success_rate => SuccessCount / ?DEFAULT_LOCK_COUNT,
            average_acquire_time => case AcquireTimes of
                                      [] -> 0;
                                      _ -> lists:sum(AcquireTimes) / length(AcquireTimes)
                                  end,
            min_acquire_time => case AcquireTimes of
                                   [] -> 0;
                                   _ -> lists:min(AcquireTimes)
                               end,
            max_acquire_time => case AcquireTimes of
                                   [] -> 0;
                                   _ -> lists:max(AcquireTimes)
                               end,
            lock_percentage => LockedCount / ?DEFAULT_LOCK_COUNT * 100
        }
    end, TtlValues),

    #{
        test => ttl_variation,
        results => Results
    }.

%% @doc Queue performance under heavy load
queue_performance(_Config) ->
    LockName = <<"queue_performance_lock">>,
    ClientCounts = [10, 20, 50, 100],
    QueueSizes = [1, 5, 10, 20],

    Results = lists:map(fun({ClientCount, QueueSize}) ->
        %% Configure lock with priority-based queue
        Options = #{wait_timeout_ms => 30000, retry_interval_ms => 100},

        %% Spawn clients trying to acquire the same lock
        Processes = [spawn_queue_client(QueueId, LockName, Options, QueueSize) ||
                        QueueId <- lists:seq(1, ClientCount)],

        %% Monitor queue behavior
        QueueStats = monitor_queue_behavior(Processes, 10000),

        %% Collect results
        ClientResults = collect_client_results(Processes, []),

        SuccessCount = length([R || R <- ClientResults, R =:= success]),
        TotalTime = QueueStats#queue_stats.total_time,

        #{
            concurrent_clients => ClientCount,
            queue_size => QueueSize,
            successful => SuccessCount,
            queue_stats => QueueStats,
            throughput => SuccessCount / (TotalTime / 1000000),
            average_queue_wait_time => QueueStats#queue_stats.average_wait_time,
            max_queue_wait_time => QueueStats#queue_stats.max_wait_time,
            queue_efficiency => calculate_queue_efficiency(QueueStats)
        }
    end, lists:zip(ClientCounts, QueueSizes)),

    #{
        test => queue_performance,
        results => Results
    }.

%% @doc Failover scenarios with node failures
failover_scenarios(_Config) ->
    LockName = <<"failover_lock">>,

    %% Test normal operation first
    NormalResults = [begin
                     Result = erlmcp_distributed_lock:acquire(LockName),
                     case Result of
                         {ok, Ref} ->
                             ok = erlmcp_distributed_lock:release(Ref),
                             success;
                         {error, _} -> failure
                     end
                 end || _ <- lists:seq(1, 100)],

    NormalSuccess = length([R || R <- NormalResults, R =:= success]) / 100,

    %% Test with simulated node failure
    FailedResults = [begin
                       %% Simulate node crash
                       process_flag(trap_exit, true),
                       spawn(fun() ->
                               Result = erlmcp_distributed_lock:acquire(LockName, #{ttl_ms => 5000}),
                               case Result of
                                   {ok, Ref} ->
                                       self() ! {acquired, Ref},
                                       receive
                                           shutdown ->
                                               %% Simulate crash during hold
                                               exit(killed)
                                       after 1000 ->
                                           %% Release normally
                                           ok = erlmcp_distributed_lock:release(Ref)
                                       end;
                                   {error, _} ->
                                       self() ! {failed}
                               end
                           end),

                       %% Wait a bit then terminate the process
                       timer:sleep(100),
                       exit(whereis(erlmcp_distributed_lock), kill),

                       %% Try to acquire the lock again
                       timer:sleep(1000),
                       Result2 = erlmcp_distributed_lock:acquire(LockName),
                       case Result2 of
                           {ok, Ref2} ->
                               ok = erlmcp_distributed_lock:release(Ref2),
                               recovered;
                           {error, _} -> failed
                       end
                   end || _ <- lists:seq(1, 50)],

    Recovered = length([R || R <- FailedResults, R =:= recovered]) / 50,
    FailedRecovery = length([R || R <- FailedResults, R =:= failed]) / 50,

    %% Test lock expiration behavior
    ExpiredResults = [begin
                       Options = #{ttl_ms => 1000}, % 1s TTL
                       Result = erlmcp_distributed_lock:acquire(LockName, Options),
                       case Result of
                           {ok, Ref} ->
                               %% Don't release, let it expire
                               timer:sleep(2000), % Wait for expiration
                               after_expire = erlmcp_distributed_lock:acquire(LockName),
                               case after_expire of
                                   {ok, Ref2} ->
                                       ok = erlmcp_distributed_lock:release(Ref2),
                                       expired_recovered;
                                   {error, _} ->
                                       expired_failed
                               end;
                           {error, _} ->
                               failed
                       end
                   end || _ <- lists:seq(1, 50)],

    ExpiredRecovered = length([R || R <- ExpiredResults, R =:= expired_recovered]) / 50,
    ExpiredFailed = length([R || R <- ExpiredResults, R =:= expired_failed]) / 50,

    #{
        test => failover_scenarios,
        normal_success_rate => NormalSuccess,
        recovery_success_rate => Recovered,
        recovery_failure_rate => FailedRecovery,
        expiration_recovery_rate => ExpiredRecovered,
        expiration_failure_rate => ExpiredFailed,
        overall_failover_efficiency => (Recovered + ExpiredRecovered) / 2
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

spawn_client(ClientId, LockNames, OperationsCount) ->
    Pid = spawn(fun() ->
                    client_loop(ClientId, LockNames, OperationsCount, [])
                end),
    register(list_to_atom("bench_client_" ++ integer_to_list(ClientId)), Pid).

client_loop(ClientId, _LockNames, 0, Results) ->
    %% Send final timing results
    AcquireTimes = lists:flatten([T || {acquire, T} <- Results]),
    ReleaseTimes = lists:flatten([T || {release, T} <- Results]),
    gen_server:call(bench_server, {client_results, ClientId, {acquire_times, AcquireTimes}, {release_times, ReleaseTimes}}),
    ok;

client_loop(ClientId, LockNames, RemainingCount, Results) ->
    LockName = lists:nth(rand:uniform(length(LockNames)), LockNames),
    StartTime = erlang:monotonic_time(microsecond),

    case erlmcp_distributed_lock:acquire(LockName) of
        {ok, LockRef} ->
            AcquireTime = erlang:monotonic_time(microsecond) - StartTime,

            %% Simulate work
            timer:sleep(rand:uniform(10)),

            ReleaseTime = erlang:monotonic_time(microsecond),
            ok = erlmcp_distributed_lock:release(LockRef),
            ReleaseTime2 = erlang:monotonic_time(microsecond) - ReleaseTime,

            NewResults = [{acquire, AcquireTime}, {release, ReleaseTime2} | Results],

            client_loop(ClientId, LockNames, RemainingCount - 1, NewResults);
        {error, _} ->
            client_loop(ClientId, LockNames, RemainingCount, Results)
    end.

spawn_queue_client(QueueId, LockName, Options, QueueSize) ->
    Pid = spawn(fun() ->
                    queue_client_loop(QueueId, LockName, Options, QueueSize, 0, 0)
                end),
    register(list_to_atom("queue_client_" ++ integer_to_list(QueueId)), Pid).

queue_client_loop(QueueId, LockName, Options, QueueSize, Attempts, SuccessCount) ->
    case erlmcp_distributed_lock:acquire(LockName, Options) of
        {ok, LockRef} ->
            %% Hold lock for some time
            HoldTime = min(QueueSize * 10, 1000), % Hold time based on queue position
            timer:sleep(HoldTime),

            ok = erlmcp_distributed_lock:release(LockRef),

            %% Try again until we've done enough operations
            if SuccessCount < 10 ->
                    queue_client_loop(QueueId, LockName, Options, QueueSize, Attempts + 1, SuccessCount + 1);
               true ->
                    gen_server:call(bench_queue_monitor, {client_done, QueueId, SuccessCount})
            end;
        {error, _} ->
            queue_client_loop(QueueId, LockName, Options, QueueSize, Attempts + 1, SuccessCount)
    end.

collect_client_results([], Acc) -> Acc;
collect_client_results([Pid | Rest], Acc) ->
    Pid ! {get_result, self()},
    receive
        {result, Result} ->
            collect_client_results(Rest, [Result | Acc])
    after 5000 ->
        collect_client_results(Rest, [timeout | Acc])
    end.

monitor_queue_behavior(Processes, Timeout) ->
    StartTime = erlang:monotonic_time(microsecond),
    Results = monitor_queue_loop(Processes, Timeout, [], []),
    EndTime = erlang:monotonic_time(microsecond),

    QueueWaitTimes = [T || {wait_time, T} <- Results],
    TotalTime = EndTime - StartTime,

    #queue_stats{
        total_time = TotalTime,
        success_count = length([R || {success, _} <- Results]),
        wait_times = QueueWaitTimes,
        average_wait_time = case QueueWaitTimes of
                               [] -> 0;
                               _ -> lists:sum(QueueWaitTimes) / length(QueueWaitTimes)
                           end,
        max_wait_time = case QueueWaitTimes of
                            [] -> 0;
                            _ -> lists:max(QueueWaitTimes)
                        end,
        total_operations = length(Results)
    }.

monitor_queue_loop([], _Timeout, Acc, _) -> Acc;
monitor_queue_loop([], _Timeout, Acc, WaitTimes) -> Acc;
monitor_queue_loop(Processes, Timeout, Acc, WaitTimes) ->
    receive
        {client_done, ClientId, SuccessCount} ->
            WaitTime = rand:uniform(5000), % Simulated wait time
            monitor_queue_loop(Processes, Timeout, [{success, SuccessCount} | Acc], [{wait_time, WaitTime} | WaitTimes]);
        timeout ->
            Acc
    after 100 ->
        monitor_queue_loop(Processes, Timeout, Acc, WaitTimes)
    end.

calculate_contention_ratio(LockNames, ClientCount) ->
    AverageLocksPerClient = ?DEFAULT_LOCK_COUNT div length(LockNames) div ClientCount,
    if AverageLocksPerClient > 1 ->
            1.0 / AverageLocksPerClient;
       true -> 1.0
    end.

calculate_queue_efficiency(QueueStats) ->
    SuccessCount = QueueStats#queue_stats.success_count,
    TotalOps = QueueStats#queue_stats.total_operations,
    case TotalOps of
        0 -> 0;
        _ -> SuccessCount / TotalOps * (1.0 / (QueueStats#queue_stats.average_wait_time / 1000))
    end.

calculate_percentile(List, Percentile) when length(List) > 0 ->
    Sorted = lists:sort(List),
    Index = trunc((Percentile / 100) * length(Sorted)),
    lists:nth(min(Index + 1, length(Sorted)), Sorted).

%%====================================================================
%% Record Definitions
%%====================================================================
-record(queue_stats, {
    total_time :: integer(),
    success_count :: integer(),
    wait_times :: [integer()],
    average_wait_time :: integer(),
    max_wait_time :: integer(),
    total_operations :: integer()
}).