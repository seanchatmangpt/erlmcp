%%%-------------------------------------------------------------------
%%% @doc Simple Process Explosion Test - Pushes VM to absolute limits
%%%
%%% DANGER: This spawns processes until VM crashes or limit reached
%%% DO NOT run on production systems
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_process_explosion_simple).

-export([run/0, run/1, run_phase/3]).
-export([worker_loop/0]).

-include_lib("kernel/include/logger.hrl").

-record(phase_result, {
    phase :: integer(),
    target :: integer(),
    spawned :: integer(),
    duration_ms :: integer(),
    process_count :: integer(),
    memory_mb :: float(),
    result :: atom() | tuple()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Run full explosion test
-spec run() -> ok.
run() ->
    run(10000000).  % Default: 10M processes

%% @doc Run with custom target
-spec run(pos_integer()) -> ok.
run(TargetProcesses) ->
    logger:warning("=== PROCESS EXPLOSION CRASH TEST ==="),
    logger:warning("DANGER: Pushing VM to absolute limits"),
    logger:warning("Target: ~p processes", [TargetProcesses]),
    
    % Get system limits
    ProcessLimit = erlang:system_info(process_limit),
    InitialCount = erlang:system_info(process_count),
    InitialMemory = erlang:memory(total) / (1024 * 1024),
    
    io:format("~n=== SYSTEM LIMITS ===~n", []),
    io:format("Process Limit: ~p~n", [ProcessLimit]),
    io:format("Initial Count: ~p~n", [InitialCount]),
    io:format("Available Slots: ~p~n", [ProcessLimit - InitialCount]),
    io:format("Initial Memory: ~.2f MiB~n~n", [InitialMemory]),
    
    % Run phases
    Phases = [
        {1, 1000, 10},      % 1K @ 10/sec
        {2, 10000, 100},    % 10K @ 100/sec
        {3, 100000, 1000},  % 100K @ 1K/sec
        {4, 1000000, 10000}, % 1M @ 10K/sec
        {5, TargetProcesses, 100000}  # Target max @ 100K/sec
    ],
    
    Results = run_phases(Phases, []),
    
    % Final report
    print_final_report(Results, ProcessLimit, InitialCount, InitialMemory),
    
    ok.

%% @doc Run a single phase
-spec run_phase(integer(), integer(), integer()) -> {ok, #phase_result{}}.
run_phase(PhaseNum, Target, RatePerSec) ->
    logger:warning("=== PHASE ~p: Spawning ~p processes @ ~p/sec ===", [
        PhaseNum, Target, RatePerSec
    ]),
    
    StartTime = os:system_time(millisecond),
    IntervalMs = case RatePerSec of
        0 -> 1;
        _ -> max(1, 1000 div RatePerSec)
    end,
    
    Result = spawn_loop(0, Target, IntervalMs, StartTime, PhaseNum),
    
    EndTime = os:system_time(millisecond),
    Duration = EndTime - StartTime,
    ProcessCount = erlang:system_info(process_count),
    MemoryMB = erlang:memory(total) / (1024 * 1024),
    
    PhaseResult = #phase_result{
        phase = PhaseNum,
        target = Target,
        spawned = element(1, Result),
        duration_ms = Duration,
        process_count = ProcessCount,
        memory_mb = MemoryMB,
        result = element(2, Result)
    },
    
    logger:warning("Phase ~p Complete:", [PhaseNum]),
    logger:warning("  Spawned: ~p / ~p", [PhaseResult#phase_result.spawned, Target]),
    logger:warning("  Process Count: ~p / ~p", [ProcessCount, erlang:system_info(process_limit)]),
    logger:warning("  Memory: ~.2f MiB", [MemoryMB]),
    logger:warning("  Duration: ~.2f sec", [Duration / 1000]),
    logger:warning("  Result: ~p", [PhaseResult#phase_result.result]),
    
    {ok, PhaseResult}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Run all phases sequentially
run_phases([], Acc) ->
    lists:reverse(Acc);
run_phases([{Phase, Target, Rate} | Rest], Acc) ->
    {ok, Result} = run_phase(Phase, Target, Rate),
    
    % Check if we crashed (couldn't spawn all)
    case Result#phase_result.spawned < Target of
        true ->
            logger:error("CRASH DETECTED at phase ~p - stopping test", [Phase]),
            lists:reverse([Result | Acc]);
        false ->
            run_phases(Rest, [Result | Acc])
    end.

%% @doc Main spawn loop
-spec spawn_loop(integer(), integer(), integer(), integer(), integer()) ->
    {integer(), atom() | tuple()}.
spawn_loop(Count, Target, _Interval, _StartTime, _Phase) when Count >= Target ->
    {Count, success};
spawn_loop(Count, Target, Interval, StartTime, Phase) ->
    % Check process limit
    ProcessLimit = erlang:system_info(process_limit),
    CurrentCount = erlang:system_info(process_count),
    
    case CurrentCount >= ProcessLimit of
        true ->
            logger:error("PROCESS LIMIT REACHED: ~p / ~p", [CurrentCount, ProcessLimit]),
            {Count, {system_limit, maximum_processes_exceeded, CurrentCount}};
        false ->
            % Try to spawn
            case spawn_worker(Phase, Count) of
                {ok, _Pid} ->
                    timer:sleep(Interval),
                    spawn_loop(Count + 1, Target, Interval, StartTime, Phase);
                {error, Reason} ->
                    logger:error("Spawn failed at ~p processes: ~p", [CurrentCount, Reason]),
                    {Count, {spawn_error, Reason, CurrentCount}}
            end
    end.

%% @doc Spawn a worker process
-spec spawn_worker(integer(), integer()) -> {ok, pid()} | {error, term()}.
spawn_worker(Phase, Id) ->
    try
        Pid = spawn(fun() -> worker_loop() end),
        {ok, Pid}
    catch
        exit:system_limit ->
            {error, system_limit};
        error:Reason ->
            {error, Reason}
    end.

%% @doc Worker process infinite loop
-spec worker_loop() -> no_return().
worker_loop() ->
    % Infinite loop echo process
    receive
        {echo, From, Msg} ->
            From ! {echo, Msg},
            worker_loop()
    after 5000 ->
        % Heartbeat every 5 seconds
        worker_loop()
    end.

%% @doc Print final report
print_final_report(Results, ProcessLimit, InitialCount, InitialMemory) ->
    io:format("~n=== PROCESS EXPLOSION TEST COMPLETE ===~n~n", []),
    
    io:format("System Limits:~n", []),
    io:format("  Process Limit: ~p~n", [ProcessLimit]),
    io:format("  Initial Count: ~p~n", [InitialCount]),
    io:format("  Available Slots: ~p~n~n", [ProcessLimit - InitialCount]),
    
    io:format("Explosion Progress:~n", []),
    lists:foreach(
        fun(R) ->
            SpawnRate = case R#phase_result.duration_ms of
                0 -> 0;
                Ms -> (R#phase_result.spawned * 1000) div Ms
            end,
            io:format("  Phase ~p: ~p / ~p spawned in ~.2f sec (~p proc/sec, ~.2f MiB) - ~p~n", [
                R#phase_result.phase,
                R#phase_result.spawned,
                R#phase_result.target,
                R#phase_result.duration_ms / 1000,
                SpawnRate,
                R#phase_result.memory_mb,
                R#phase_result.result
            ])
        end,
        Results
    ),
    
    % Find breaking point
    CrashedPhases = [R || R <- Results, R#phase_result.spawned < R#phase_result.target],
    case CrashedPhases of
        [] ->
            io:format("~nNo breaking point reached - all phases passed~n", []);
        [FirstCrash | _] ->
            io:format("~nBREAKING POINT:~n", []),
            io:format("  Phase: ~p~n", [FirstCrash#phase_result.phase]),
            io:format("  Process Count: ~p~n", [FirstCrash#phase_result.process_count]),
            io:format("  Time to Reach: ~.2f seconds~n", [FirstCrash#phase_result.duration_ms / 1000]),
            io:format("  Error: ~p~n", [FirstCrash#phase_result.result]),
            io:format("  Memory: ~.2f MiB~n", [FirstCrash#phase_result.memory_mb]),
            
            PercentUsed = (FirstCrash#phase_result.process_count / ProcessLimit) * 100,
            io:format("  Utilization: ~.2f%~n", [PercentUsed])
    end,
    
    FinalCount = erlang:system_info(process_count),
    FinalMemory = erlang:memory(total) / (1024 * 1024),
    
    io:format("~nVM STATUS:~n", []),
    io:format("  VM Survived: yes~n", []),
    io:format("  Responsive: ~p~n", [is_vm_responsive()]),
    io:format("  Final Process Count: ~p~n", [FinalCount]),
    io:format("  Final Memory: ~.2f MiB~n", [FinalMemory]),
    io:format("  Memory Growth: ~.2f MiB~n", [FinalMemory - InitialMemory]),
    
    io:format("~nRECOVERY TEST:~n", []),
    io:format("  Killing half of ~p processes...~n", [FinalCount]),
    ToKill = FinalCount div 2,
    KillCount = kill_processes(ToKill),
    io:format("  Killed: ~p processes~n", [KillCount]),
    
    timer:sleep(2000),
    
    io:format("  Attempting to spawn 1000 new processes...~n", []),
    case spawn_loop(0, 1000, 10, os:system_time(millisecond), 99) of
        {1000, success} ->
            io:format("  Recovery: SUCCESSFUL~n");
        {Count, Result} ->
            io:format("  Recovery: PARTIAL (~p / 1000, ~p)~n", [Count, Result])
    end,
    
    io:format("~nANALYSIS:~n", []),
    case CrashedPhases of
        [] ->
            io:format("  System handled all phases without crashing.~n", []),
            io:format("  Process limit not reached.~n", []);
        [FirstCrash | _] ->
            io:format("  System crashed at ~p processes (~.2f% of limit).~n", [
                FirstCrash#phase_result.process_count,
                (FirstCrash#phase_result.process_count / ProcessLimit) * 100
            ]),
            io:format("  This is the absolute maximum concurrent processes for this VM.~n")
    end,
    
    io:format("~n========================================~n~n", []).

%% @doc Check if VM is responsive
is_vm_responsive() ->
    try
        _ = erlang:system_info(process_count),
        _ = erlang:memory(total),
        _ = processes(),
        true
    catch
        _:_ -> false
    end.

%% @doc Kill processes
kill_processes(Count) ->
    Pids = processes(),
    ToKill = lists:sublist(Pids, min(Count, length(Pids))),
    lists:foreach(fun(P) -> exit(P, kill) end, ToKill),
    length(ToKill).
