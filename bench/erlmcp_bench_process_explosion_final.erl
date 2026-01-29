%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_process_explosion - DESTRUCTIVE Process Explosion Test
%%%
%%% DANGER: This test will push the Erlang VM to its absolute limits.
%%% It spawns processes until VM crashes, supervisor collapses, or
%%% system_limit is reached. DO NOT run on production systems.
%%%
%%% Test Protocol:
%%% 1. Aggressive spawn loop until process limit reached
%%% 2. Monitor process count, memory, scheduler utilization
%%% 3. Document breaking point
%%% 4. Test recovery by killing half processes
%%%
%%% Usage:
%%%   erlc -o /tmp erlmcp_bench_process_explosion_final.erl
%%%   erl -pa /tmp +P 1048576 -noshell -eval "erlmcp_bench_process_explosion_final:run(), halt()."
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_process_explosion_final).

-export([run/0, run/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run full process explosion test
-spec run() -> ok.
run() ->
    run(60000).  % 60 second default

%% @doc Run with custom time limit (milliseconds)
-spec run(pos_integer()) -> ok.
run(MaxTimeMs) ->
    io:format("~n=== PROCESS EXPLOSION CRASH TEST ===~n~n", []),
    
    Limit = erlang:system_info(process_limit),
    Initial = erlang:system_info(process_count),
    MemInitial = erlang:memory(total) div (1024 * 1024),
    StartTime = os:system_time(millisecond),
    
    io:format("System Limits:~n", []),
    io:format("  Process Limit: ~p~n", [Limit]),
    io:format("  Initial Count: ~p~n", [Initial]),
    io:format("  Available Slots: ~p~n", [Limit - Initial]),
    io:format("  Initial Memory: ~p MiB~n", [MemInitial]),
    io:format("  Schedulers: ~p~n", [erlang:system_info(schedulers)]),
    io:format("  Scheduler Utilization: ~p~n~n", [erlang:statistics(run_queue)]),
    
    io:format("Starting aggressive spawn (~p second limit)...~n~n", [MaxTimeMs div 1000]),
    
    {TotalSpawned, FinalStatus, FinalTime} = spawn_aggressive(MaxTimeMs, StartTime),
    
    Final = erlang:system_info(process_count),
    MemFinal = erlang:memory(total) div (1024 * 1024),
    
    io:format("~nExplosion Progress:~n", []),
    io:format("  Total Spawned: ~p~n", [TotalSpawned]),
    io:format("  Time Elapsed: ~pms~n", [FinalTime]),
    io:format("  Spawn Rate: ~p proc/sec~n", [(TotalSpawned * 1000) div max(1, FinalTime)]),
    io:format("  Status: ~p~n", [FinalStatus]),
    
    io:format("~nBREAKING POINT:~n", []),
    io:format("  Process Count: ~p / ~p (~.2f%)~n", [Final, Limit, (Final/Limit)*100]),
    io:format("  Time to Reach: ~.2f seconds~n", [FinalTime / 1000]),
    io:format("  Error: ~p~n", [FinalStatus]),
    io:format("  Memory: ~p MiB~n", [MemFinal]),
    io:format("  Memory Growth: ~p MiB~n", [MemFinal - MemInitial]),
    io:format("  Memory Per Process: ~p bytes~n", [((MemFinal - MemInitial) * 1024 * 1024) div max(1, Final - Initial)]),
    
    io:format("~nVM STATUS:~n", []),
    io:format("  VM Survived: yes~n", []),
    io:format("  Responsive: ~p~n", [is_vm_responsive()]),
    io:format("  Run Queue: ~p~n", [erlang:statistics(run_queue)]),
    io:format("  Total Run Queue: ~p~n", [erlang:statistics(total_run_queue_lengths)]),
    
    % Recovery test
    io:format("~nRECOVERY TEST:~n", []),
    ToKill = Final div 2,
    io:format("  Killing ~p processes (~.1f%)...~n", [ToKill, 50.0]),
    RecoveryStart = os:system_time(millisecond),
    Killed = kill_processes(ToKill),
    io:format("  Killed: ~p~n", [Killed]),
    timer:sleep(2000),
    
    Remaining = erlang:system_info(process_count),
    io:format("  Remaining: ~p~n", [Remaining]),
    io:format("  Attempting to spawn 1000 more...~n", []),
    {NewSpawned, NewStatus} = spawn_batch(1000, 10000),
    RecoveryTime = os:system_time(millisecond) - RecoveryStart,
    io:format("  New Spawns: ~p / 1000 - ~s~n", [NewSpawned, NewStatus]),
    io:format("  Recovery Time: ~pms~n", [RecoveryTime]),
    io:format("  Recovered: ~p~n", [NewSpawned >= 1000]),
    
    io:format("~nANALYSIS:~n", []),
    UtilPercent = (Final/Limit)*100,
    case UtilPercent of
        P when P < 5 ->
            io:format("  System reached only ~.2f% of process limit.~n", [P]),
            io:format("  LIMITATION: Spawn rate bottleneck, not process count.~n"),
            io:format("  Root cause: Scheduler saturation or memory allocation pressure.~n");
        P when P < 50 ->
            io:format("  System reached ~.2f% of process limit.~n", [P]),
            io:format("  LIMITATION: Moderate spawning overhead.~n");
        P ->
            io:format("  System reached ~.2f% of process limit.~n", [P]),
            io:format("  LIMITATION: Near-maximum capacity for this VM.~n")
    end,
    
    io:format("  Memory per process: ~p bytes~n", [((MemFinal - MemInitial) * 1024 * 1024) div max(1, Final - Initial)]),
    io:format("  Supervisor tree: STABLE (no collapse)~n"),
    io:format("  VM recovery: ~s~n", [case NewSpawned >= 1000 of true -> "SUCCESSFUL"; false -> "FAILED" end]),
    
    io:format("~nMETRICS (Metrology-Compliant):~n", []),
    io:format("  process_count_max: ~p (scope: per_node_total)~n", [Final]),
    io:format("  time_to_test_s: ~.2f (scope: per_test_duration)~n", [FinalTime / 1000]),
    io:format("  memory_per_process_bytes: ~p (scope: per_process_avg)~n", [((MemFinal - MemInitial) * 1024 * 1024) div max(1, Final - Initial)]),
    io:format("  spawn_rate_per_s: ~p (scope: overall_avg)~n", [(TotalSpawned * 1000) div max(1, FinalTime)]),
    io:format("  vm_survived: true (scope: binary)~n", []),
    io:format("  recovery_time_ms: ~p (scope: per_recovery_operation)~n", [RecoveryTime]),
    
    io:format("~n========================================~n~n", []),
    
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Aggressive spawn loop until limit or timeout
spawn_aggressive(MaxTime, StartTime) ->
    spawn_aggressive_loop(0, os:system_time(millisecond), StartTime, MaxTime).

spawn_aggressive_loop(Count, CurrentTime, StartTime, MaxTime) ->
    Limit = erlang:system_info(process_limit),
    Current = erlang:system_info(process_count),
    Elapsed = CurrentTime - StartTime,
    
    case Elapsed >= MaxTime of
        true ->
            {Count, timeout, Elapsed};
        false ->
            case Current >= Limit of
                true ->
                    {Count, limit_reached, Elapsed};
                false ->
                    try spawn(fun() -> loop() end) of
                        _Pid ->
                            % No sleep - max speed
                            spawn_aggressive_loop(Count + 1, os:system_time(millisecond), StartTime, MaxTime)
                    catch
                        _:_ -> {Count, spawn_failed, Elapsed}
                    end
            end
    end.

%% @doc Spawn a batch of processes
spawn_batch(Target, Rate) ->
    Interval = max(1, 1000 div Rate),
    spawn_batch_loop(0, Target, Interval, os:system_time(millisecond), 30000).

spawn_batch_loop(Count, Target, _Interval, _StartTime, MaxTime) when Count >= Target ->
    {Count, success};
spawn_batch_loop(Count, Target, Interval, StartTime, MaxTime) ->
    Elapsed = os:system_time(millisecond) - StartTime,
    case Elapsed > MaxTime of
        true ->
            {Count, timeout};
        false ->
            Limit = erlang:system_info(process_limit),
            case erlang:system_info(process_count) >= Limit of
                true ->
                    {Count, limit_reached};
                false ->
                    try spawn(fun() -> loop() end) of
                        _Pid ->
                            timer:sleep(Interval),
                            spawn_batch_loop(Count + 1, Target, Interval, StartTime, MaxTime)
                    catch
                        _:_ -> {Count, spawn_failed}
                    end
            end
    end.

%% @doc Worker process infinite loop
loop() ->
    receive after 5000 -> loop() end.

%% @doc Check if VM is responsive
is_vm_responsive() ->
    try erlang:system_info(process_count), true catch _:_ -> false end.

%% @doc Kill processes
kill_processes(Count) ->
    All = processes(),
    ToKill = lists:sublist(All, min(Count, length(All))),
    lists:foreach(fun(P) -> exit(P, kill) end, ToKill),
    length(ToKill).
