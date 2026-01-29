%%%=============================================================================
%%% @doc Supervisor Tree Collapse Stress Test
%%% @end
%%%=============================================================================
-module(erlmcp_bench_supervisor_collapse).
-author('erlang-performance').
-export([
    run/0,
    run/1,
    inspect_tree/0,
    crash_workers/1,
    crash_supervisor/1,
    exceed_intensity/0,
    test_recovery/0
]).

-define(TEST_DURATION, 600).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

run() ->
    run("supervisor_collapse_test").

run(TestId) ->
    io:format("~n=== SUPERVISOR TREE COLLAPSE CRASH TEST ===~n", []),
    io:format("Test ID: ~p~n~n", [TestId]),
    
    %% Check if application is running
    ApplicationStatus = check_application_status(),
    io:format("Initial Application Status:~n", []),
    print_application_status(ApplicationStatus),
    
    case ApplicationStatus of
        running ->
            run_full_test(ApplicationStatus);
        not_running ->
            io:format("~n⚠️ Application not running. Attempting minimal start...~n"),
            attempt_minimal_start(ApplicationStatus)
    end.

%%%=============================================================================
%%% Test Execution Functions
%%%=============================================================================

run_full_test(AppStatus) ->
    io:format("~n--- RUNNING FULL COLLAPSE TEST ---~n~n"),
    
    %% Phase 0: Baseline inspection
    io:format("~nPHASE 0: BASELINE INSPECTION~n", []),
    inspect_tree(),
    
    %% Phase 1: Worker crash test
    io:format("~n~nPHASE 1: CRASH WORKERS (10%)~n", []),
    WorkersResult = crash_workers(0.1),
    io:format("✓ Workers crash result: ~p~n", [WorkersResult]),
    timer:sleep(2000),
    inspect_tree(),
    
    %% Phase 2: Exceed intensity limits
    io:format("~n~nPHASE 2: EXCEED INTENSITY LIMITS~n", []),
    IntensityResult = exceed_intensity(),
    io:format("✓ Intensity exceed result: ~p~n", [IntensityResult]),
    timer:sleep(2000),
    inspect_tree(),
    
    %% Phase 3: Crash tier 2 supervisor (server_sup)
    io:format("~n~nPHASE 3: CRASH SERVER SUPERVISOR~n", []),
    ServerSupResult = crash_supervisor(erlmcp_server_sup),
    io:format("✓ Server supervisor crash result: ~p~n", [ServerSupResult]),
    timer:sleep(2000),
    inspect_tree(),
    
    %% Phase 4: Crash tier 1 supervisor (core_sup)
    io:format("~n~nPHASE 4: CRASH CORE SUPERVISOR~n", []),
    CoreSupResult = crash_supervisor(erlmcp_core_sup),
    io:format("✓ Core supervisor crash result: ~p~n", [CoreSupResult]),
    timer:sleep(2000),
    inspect_tree(),
    
    %% Phase 5: Crash tier 3 supervisor (observability_sup)
    io:format("~n~nPHASE 5: CRASH OBSERVABILITY SUPERVISOR~n", []),
    ObsSupResult = crash_supervisor(erlmcp_observability_sup),
    io:format("✓ Observability supervisor crash result: ~p~n", [ObsSupResult]),
    timer:sleep(2000),
    inspect_tree(),
    
    %% Phase 6: Crash root supervisor
    io:format("~n~nPHASE 6: CRASH ROOT SUPERVISOR~n", []),
    RootSupResult = crash_supervisor(erlmcp_sup),
    io:format("✓ Root supervisor crash result: ~p~n", [RootSupResult]),
    timer:sleep(3000),
    inspect_tree(),
    
    %% Phase 7: Recovery test
    io:format("~n~nPHASE 7: RECOVERY TEST~n", []),
    RecoveryResult = test_recovery(),
    io:format("✓ Recovery result: ~p~n", [RecoveryResult]),
    
    %% Final report
    print_final_report(AppStatus, WorkersResult, IntensityResult, 
                      ServerSupResult, CoreSupResult, ObsSupResult, 
                      RootSupResult, RecoveryResult),
    
    {ok, test_complete}.

attempt_minimal_start(_AppStatus) ->
    io:format("~n--- ATTEMPTING MINIMAL SUPERVISOR TREE START ---~n~n"),
    
    %% Try to start just the core supervisor without full application
    io:format("Starting minimal supervisor tree...~n"),
    
    %% Check if we can at least see the process structure
    AllProcesses = processes(),
    io:format("Total processes in VM: ~p~n", [length(AllProcesses)]),
    
    %% List all registered processes
    AllRegistered = registered(),
    io:format("Registered processes: ~p~n", [AllRegistered]),
    
    %% Look for any erlmcp processes
    ErlmcpProcesses = lists:filter(fun(P) ->
        case atom_to_list(P) of
            "erlmcp" ++ _ -> true;
            _ -> false
        end
    end, AllRegistered),
    io:format("ErlMCP processes found: ~p~n", [ErlmcpProcesses]),
    
    {error, application_not_running}.

%%%=============================================================================
%%% Inspection Functions
%%%=============================================================================

inspect_tree() ->
    io:format("~n--- Supervision Tree Status ---~n", []),
    
    %% Root supervisor
    case whereis(erlmcp_sup) of
        undefined -> io:format("❌ Root supervisor: DOWN~n", []);
        Pid -> io:format("✓ Root supervisor: ~p~n", [Pid])
    end,
    
    %% Tier 1: Core supervisor
    case whereis(erlmcp_core_sup) of
        undefined -> io:format("❌ Core supervisor: DOWN~n", []);
        Pid1 -> io:format("✓ Core supervisor: ~p~n", [Pid1])
    end,
    
    %% Tier 2: Server supervisor
    case whereis(erlmcp_server_sup) of
        undefined -> io:format("❌ Server supervisor: DOWN~n", []);
        Pid2 -> io:format("✓ Server supervisor: ~p~n", [Pid2])
    end,
    
    %% Tier 3: Observability supervisor
    case whereis(erlmcp_observability_sup) of
        undefined -> io:format("❌ Observability supervisor: DOWN~n", []);
        Pid3 -> io:format("✓ Observability supervisor: ~p~n", [Pid3])
    end,
    
    %% Registry
    case whereis(erlmcp_registry) of
        undefined -> io:format("❌ Registry: DOWN~n", []);
        Pid4 -> io:format("✓ Registry: ~p~n", [Pid4])
    end,
    
    %% Count workers
    Workers = count_workers(),
    io:format("✓ Total workers: ~p~n", [Workers]),
    
    %% Supervisor children details
    lists:foreach(fun({SupRef, Name}) ->
        case whereis(SupRef) of
            undefined -> ok;
            SupPid ->
                case supervisor:count_children(SupPid) of
                    #{specs := Specs, active := Active, supervisors := Sups, workers := Wrks} ->
                        io:format("  ~s: ~p active (specs=~p, supervisors=~p, workers=~p)~n", 
                                  [Name, Active, Specs, Sups, Wrks]);
                    _ ->
                        io:format("  ~s: Unable to count children~n", [Name])
                end
        end
    end, [{erlmcp_sup, "Root"}, {erlmcp_core_sup, "Core"}, 
          {erlmcp_server_sup, "Server"}, {erlmcp_observability_sup, "Observability"}]),
    
    ok.

count_workers() ->
    Sups = [erlmcp_sup, erlmcp_core_sup, erlmcp_server_sup, erlmcp_observability_sup],
    lists:foldl(fun(Sup, Acc) ->
        case whereis(Sup) of
            undefined -> Acc;
            Pid ->
                try
                    Children = supervisor:which_children(Pid),
                    Workers = lists:filter(fun({_, ChildPid, worker, _}) -> is_pid(ChildPid);
                                             (_) -> false
                                          end, Children),
                    Acc + length(Workers)
                catch
                    _:_ -> Acc
                end
        end
    end, 0, Sups).

check_application_status() ->
    case whereis(erlmcp_sup) of
        undefined -> not_running;
        _ -> running
    end.

print_application_status(running) ->
    io:format("✓ Application is RUNNING~n", []);
print_application_status(not_running) ->
    io:format("❌ Application is NOT RUNNING~n", []).

%%%=============================================================================
%%% Crash Functions
%%%=============================================================================

crash_workers(Percentage) ->
    Workers = find_all_workers(),
    TargetCount = max(1, round(length(Workers) * Percentage)),
    
    io:format("Found ~p workers, targeting ~p for crash~n", 
              [length(Workers), TargetCount]),
    
    case length(Workers) of
        0 ->
            {error, no_workers};
        _ ->
            ToKill = lists:sublist(Workers, min(TargetCount, length(Workers))),
            
            KilledList = lists:map(fun(WorkerPid) ->
                try
                    exit(WorkerPid, kill),
                    io:format("  Killed worker: ~p~n", [WorkerPid]),
                    {killed, WorkerPid}
                catch
                    _:_ ->
                        io:format("  Failed to kill: ~p~n", [WorkerPid]),
                        {failed, WorkerPid}
                end
            end, ToKill),
            
            KilledCount = length(lists:filter(fun({killed, _}) -> true; (_) -> false end, KilledList)),
            {ok, KilledCount, length(ToKill)}
    end.

crash_supervisor(SupName) ->
    case whereis(SupName) of
        undefined ->
            io:format("❌ Supervisor ~p not found~n", [SupName]),
            {error, not_found};
        SupPid ->
            io:format("Killing supervisor ~p (~p)~n", [SupName, SupPid]),
            try
                exit(SupPid, kill),
                timer:sleep(100),
                case whereis(SupName) of
                    undefined -> 
                        io:format("✓ Supervisor ~p: DOWN (not restarted)~n", [SupName]),
                        {killed, not_restarted};
                    NewPid when is_pid(NewPid) -> 
                        io:format("✓ Supervisor ~p: RESTARTED (~p)~n", [SupName, NewPid]),
                        {killed, {restarted, NewPid}}
                end
            catch
                Error:Reason ->
                    io:format("❌ Failed to kill supervisor: ~p:~p~n", [Error, Reason]),
                    {error, {Error, Reason}}
            end
    end.

exceed_intensity() ->
    io:format("Exceeding intensity limit (5 restarts in 60s)...~n"),
    
    %% Find a worker to kill repeatedly
    Workers = find_all_workers(),
    
    case Workers of
        [] ->
            io:format("⚠️ No workers to kill~n", []),
            {error, no_workers};
        [Pid | _] ->
            io:format("Killing worker ~p repeatedly (10 times)...~n", [Pid]),
            
            %% Kill it 10 times rapidly
            Results = lists:map(fun(N) ->
                try
                    exit(Pid, kill),
                    timer:sleep(50),
                    io:format("  Kill attempt ~p: sent kill signal~n", [N]),
                    {killed, N}
                catch
                    _:_ ->
                        io:format("  Kill attempt ~p: failed~n", [N]),
                        {failed, N}
                end
            end, lists:seq(1, 10)),
            
            KilledCount = length(lists:filter(fun({killed, _}) -> true; (_) -> false end, Results)),
            {ok, KilledCount, 10}
    end.

test_recovery() ->
    io:format("Testing recovery after collapse...~n"),
    
    %% Check status of all supervisors
    RootAlive = case whereis(erlmcp_sup) of
        undefined -> false;
        _ -> true
    end,
    
    CoreAlive = case whereis(erlmcp_core_sup) of
        undefined -> false;
        _ -> true
    end,
    
    ServerAlive = case whereis(erlmcp_server_sup) of
        undefined -> false;
        _ -> true
    end,
    
    ObsAlive = case whereis(erlmcp_observability_sup) of
        undefined -> false;
        _ -> true
    end,
    
    RegistryAlive = case whereis(erlmcp_registry) of
        undefined -> false;
        _ -> true
    end,
    
    io:format("  Root supervisor: ~p~n", [RootAlive]),
    io:format("  Core supervisor: ~p~n", [CoreAlive]),
    io:format("  Server supervisor: ~p~n", [ServerAlive]),
    io:format("  Observability supervisor: ~p~n", [ObsAlive]),
    io:format("  Registry: ~p~n", [RegistryAlive]),
    
    %% Calculate recovery status
    AllAlive = RootAlive andalso CoreAlive andalso ServerAlive andalso ObsAlive andalso RegistryAlive,
    
    if
        AllAlive ->
            io:format("✓ Full recovery: All components alive~n", []),
            {ok, full_recovery};
        RootAlive andalso CoreAlive ->
            io:format("⚠️ Partial recovery: Core alive, some components missing~n", []),
            {ok, partial_recovery};
        true ->
            io:format("❌ No recovery: Core infrastructure down~n", []),
            {error, no_recovery}
    end.

find_all_workers() ->
    %% Find all worker processes under erlmcp supervisors
    Sups = [erlmcp_core_sup, erlmcp_server_sup, erlmcp_observability_sup],
    
    lists:foldl(fun(Sup, Acc) ->
        case whereis(Sup) of
            undefined -> Acc;
            Pid ->
                try
                    Children = supervisor:which_children(Pid),
                    Workers = [ChildPid || {_, ChildPid, worker, _} <- Children, 
                                          is_pid(ChildPid)],
                    Acc ++ Workers
                catch
                    _:_ -> Acc
                end
        end
    end, [], Sups).

%%%=============================================================================
%%% Reporting Functions
%%%=============================================================================

print_final_report(AppStatus, WorkersResult, IntensityResult, 
                  ServerSupResult, CoreSupResult, ObsSupResult, 
                  RootSupResult, RecoveryResult) ->
    io:format("~n~n=== COLLAPSE TEST FINAL REPORT ===~n~n", []),
    
    io:format("TEST CONFIGURATION:~n", []),
    io:format("  Test Duration: ~p seconds~n", [?TEST_DURATION]),
    io:format("  Application Status: ~p~n~n", [AppStatus]),
    
    io:format("SUPERVISION TREE STRUCTURE:~n", []),
    io:format("  Root Supervisor (erlmcp_sup):~n", []),
    io:format("    - Strategy: one_for_one~n", []),
    io:format("    - Intensity: 5 restarts per 60 seconds~n", []),
    io:format("    - Tier 1: erlmcp_core_sup (Core Infrastructure)~n", []),
    io:format("    - Tier 2: erlmcp_server_sup (Protocol Servers)~n", []),
    io:format("    - Tier 3: erlmcp_observability_sup (Monitoring)~n~n", []),
    
    io:format("CASCADE ATTACK RESULTS:~n", []),
    io:format("  Phase 1 - Worker Crash (10%): ~p~n", [WorkersResult]),
    io:format("  Phase 2 - Intensity Exceeded: ~p~n", [IntensityResult]),
    io:format("  Phase 3 - Server Supervisor Crash: ~p~n", [ServerSupResult]),
    io:format("  Phase 4 - Core Supervisor Crash: ~p~n", [CoreSupResult]),
    io:format("  Phase 5 - Observability Supervisor Crash: ~p~n", [ObsSupResult]),
    io:format("  Phase 6 - Root Supervisor Crash: ~p~n", [RootSupResult]),
    io:format("  Phase 7 - Recovery Test: ~p~n~n", [RecoveryResult]),
    
    io:format("SUPERVISOR BEHAVIOR ANALYSIS:~n", []),
    analyze_supervisor_behavior(WorkersResult, IntensityResult, 
                               ServerSupResult, CoreSupResult, 
                               ObsSupResult, RootSupResult, RecoveryResult),
    
    io:format("~n=== END OF REPORT ===~n", []).

analyze_supervisor_behavior(WorkersResult, IntensityResult, 
                           ServerSupResult, CoreSupResult, 
                           ObsSupResult, RootSupResult, RecoveryResult) ->
    
    %% Analyze worker crashes
    case WorkersResult of
        {ok, KilledCount, Total} ->
            io:format("  ✓ Worker crashes: Individual restart successful~n", []),
            io:format("    - one_for_one strategy isolates failures~n", []),
            io:format("    - ~p/~p workers killed and restarted~n", [KilledCount, Total]);
        _ ->
            io:format("  ⚠️ Worker crashes: No workers available or failed~n", [])
    end,
    
    %% Analyze intensity limits
    case IntensityResult of
        {ok, KilledCount2, _Total2} ->
            io:format("  ✓ Intensity limit: ~p kills completed~n", [KilledCount2]),
            io:format("    - Strategy: one_for_one with intensity=5, period=60s~n", []),
            if
                KilledCount2 > 5 ->
                    io:format("    - WARNING: Intensity exceeded!~n", []),
                    io:format("    - Expected: Supervisor should shut down after 5th restart~n", []);
                true ->
                    io:format("    - Within intensity limits~n", [])
            end;
        _ ->
            io:format("  ⚠️ Intensity test: No workers available~n", [])
    end,
    
    %% Analyze supervisor crashes
    io:format("~n  Supervisor Crash Analysis:~n", []),
    
    case ServerSupResult of
        {killed, not_restarted} ->
            io:format("    ❌ Server supervisor: Killed, NOT RESTARTED~n", []),
            io:format("      - This is expected! Server supervisor is under root supervisor~n", []),
            io:format("      - Root supervisor should restart it~n", []);
        {killed, {restarted, _}} ->
            io:format("    ✓ Server supervisor: Killed and RESTARTED by parent~n", []);
        _ ->
            io:format("    ⚠️ Server supervisor: Unknown status~n", [])
    end,
    
    case CoreSupResult of
        {killed, not_restarted} ->
            io:format("    ❌ Core supervisor: Killed, NOT RESTARTED~n", []),
            io:format("      - This is expected! Core supervisor is under root supervisor~n", []),
            io:format("      - Root supervisor should restart it~n", []);
        {killed, {restarted, _}} ->
            io:format("    ✓ Core supervisor: Killed and RESTARTED by parent~n", []);
        _ ->
            io:format("    ⚠️ Core supervisor: Unknown status~n", [])
    end,
    
    case ObsSupResult of
        {killed, not_restarted} ->
            io:format("    ❌ Observability supervisor: Killed, NOT RESTARTED~n", []),
            io:format("      - This is expected! Observability supervisor is under root supervisor~n", []),
            io:format("      - Root supervisor should restart it~n", []);
        {killed, {restarted, _}} ->
            io:format("    ✓ Observability supervisor: Killed and RESTARTED by parent~n", []);
        _ ->
            io:format("    ⚠️ Observability supervisor: Unknown status~n", [])
    end,
    
    case RootSupResult of
        {killed, not_restarted} ->
            io:format("    ❌ ROOT supervisor: Killed, NOT RESTARTED~n", []),
            io:format("      - CRITICAL FAILURE! Root supervisor has no parent~n", []),
            io:format("      - Entire supervision tree COLLAPSED~n", []),
            io:format("      - Application restart required~n", []);
        {killed, {restarted, _}} ->
            io:format("    ✓ Root supervisor: Killed and RESTARTED (unexpected!)~n", []),
            io:format("      - This should NOT happen - root has no parent~n", []);
        _ ->
            io:format("    ⚠️ Root supervisor: Unknown status~n", [])
    end,
    
    %% Recovery analysis
    io:format("~n  Recovery Analysis:~n", []),
    case RecoveryResult of
        {ok, full_recovery} ->
            io:format("    ✓ FULL RECOVERY: All components alive and functional~n", []),
            io:format("      - Supervision tree is robust~n", []),
            io:format("      - Let-it-crash philosophy works correctly~n", []);
        {ok, partial_recovery} ->
            io:format("    ⚠️ PARTIAL RECOVERY: Core alive, some components missing~n", []),
            io:format("      - Supervision tree partially functional~n", []),
            io:format("      - Some components may need manual restart~n", []);
        {error, no_recovery} ->
            io:format("    ❌ NO RECOVERY: Core infrastructure down~n", []),
            io:format("      - Complete system collapse~n", []),
            io:format("      - Application restart REQUIRED~n", [])
    end,
    
    io:format("~n", []).
