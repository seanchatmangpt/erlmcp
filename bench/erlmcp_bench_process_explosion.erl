%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_process_explosion - DESTRUCTIVE Process Explosion Test
%%%
%%% DANGER: This test will push the Erlang VM to its absolute limits.
%%% It spawns processes until VM crashes, supervisor collapses, or
%%% system_limit is reached. DO NOT run on production systems.
%%%
%%% Test Protocol:
%%% 1. Spawn MCP server on port 10005
%%% 2. Spawn workers in phases:
%%%    - Phase 1: 1,000 workers (10/sec)
%%%    - Phase 2: 10,000 workers (100/sec)
%%%    - Phase 3: 100,000 workers (1,000/sec)
%%%    - Phase 4: 1,000,000 workers (max speed)
%%%    - Phase 5: 10,000,000 workers (until crash)
%%% 3. Monitor:
%%%    - Process count vs limit
%%%    - Supervisor child count
%%%    - Memory per process
%%%    - Scheduler utilization
%%% 4. Document breaking point
%%% 5. Test recovery by killing half processes
%%%
%%% Metrics (Metrology-Compliant):
%%% - process_count_max (scope: per_node_total)
%%% - time_to_limit_s (scope: per_test_duration)
%%% - memory_per_process_bytes (scope: per_process_avg)
%%% - spawn_rate_per_s (scope: per_phase_avg)
%%% - supervisor_state (scope: health_status)
%%% - vm_survived (scope: binary)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_process_explosion).

-behaviour(gen_server).

%% API
-export([
    run/0,
    run/1,
    run_scenario/1,
    scenarios/0,
    spawn_workers/2,
    kill_half_workers/1,
    get_test_status/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record(explosion_state, {
    test_id :: binary(),
    phase = 1 :: integer(),
    workers = [] :: [pid()],
    worker_sup :: pid() | undefined,
    server_pid :: pid() | undefined,
    start_time :: integer(),
    phase_start_time :: integer(),
    metrics = [] :: [map()],
    crashed = false :: boolean(),
    crash_reason :: term() | undefined
}).

-record(phase_config, {
    target_count :: integer(),
    spawn_rate :: integer(),  % processes per second
    duration :: integer()      % maximum seconds for this phase
}).

-type explosion_result() :: #{
    workload_id := binary(),
    benchmark := binary(),
    scenario := binary(),
    system_process_limit := integer(),
    initial_process_count := integer(),
    phases := [map()],
    breaking_point := map() | undefined,
    vm_survived := boolean(),
    vm_responsive := boolean(),
    recovery_test := map(),
    analysis := binary(),
    timestamp := integer()
}.

-export_type([explosion_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run full process explosion test to crash
-spec run() -> {ok, explosion_result()}.
run() ->
    run(#{}).

%% @doc Run with custom config
-spec run(map()) -> {ok, explosion_result()}.
run(Config) ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []),
    
    % Run the test
    gen_server:call(Pid, run_explosion_test, infinity).

%% @doc Get all test scenarios
-spec scenarios() -> [map()].
scenarios() ->
    [
        #{
            id => <<"process_explosion_full">>,
            name => "Full Process Explosion to Crash",
            description => "Spawn 1 -> 10M processes until VM crashes",
            phases => [
                #phase_config{
                    target_count = 1000,
                    spawn_rate = 10,
                    duration = 120
                },
                #phase_config{
                    target_count = 10000,
                    spawn_rate = 100,
                    duration = 180
                },
                #phase_config{
                    target_count = 100000,
                    spawn_rate = 1000,
                    duration = 300
                },
                #phase_config{
                    target_count = 1000000,
                    spawn_rate = 10000,
                    duration = 600
                },
                #phase_config{
                    target_count = 10000000,
                    spawn_rate = 100000,
                    duration = 3600
                }
            ]
        },
        #{
            id => <<"process_explosion_rapid">>,
            name => "Rapid Process Explosion",
            description => "Spawn at max speed until crash",
            phases => [
                #phase_config{
                    target_count = 10000000,
                    spawn_rate = 1000000,
                    duration = 3600
                }
            ]
        }
    ].

%% @doc Run specific scenario
-spec run_scenario(binary()) -> {ok, explosion_result()}.
run_scenario(ScenarioId) ->
    Scenarios = scenarios(),
    Scenario = lists:keyfind(ScenarioId, #scenario.id, Scenarios),
    run(Scenario).

%% @doc Get current test status
-spec get_test_status() -> map().
get_test_status() ->
    gen_server:call(?MODULE, get_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #explosion_state{}}.
init(_Config) ->
    logger:warning("=== PROCESS EXPLOSION TEST INITIALIZED ==="),
    logger:warning("DANGER: This will push VM to absolute limits"),
    
    {ok, #explosion_state{
        test_id = <<"explosion_", (integer_to_binary(os:system_time(millisecond)))/binary>>,
        start_time = os:system_time(millisecond),
        phase_start_time = os:system_time(millisecond)
    }}.

-spec handle_call(term(), {pid(), term()}, #explosion_state{}) ->
    {reply, term(), #explosion_state{}} | {noreply, #explosion_state{}}.

handle_call(run_explosion_test, _From, State) ->
    logger:info("Starting PROCESS EXPLOSION test"),
    
    % Get system limits
    ProcessLimit = erlang:system_info(process_limit),
    InitialCount = erlang:system_info(process_count),
    
    logger:info("System Limits:"),
    logger:info("  Process Limit: ~p", [ProcessLimit]),
    logger:info("  Initial Count: ~p", [InitialCount]),
    logger:info("  Available Slots: ~p", [ProcessLimit - InitialCount]),
    
    % Start MCP server
    {ok, ServerPid} = start_test_server(),
    
    % Start worker supervisor
    {ok, WorkerSup} = start_worker_supervisor(),
    
    % Run test phases
    {ok, Result} = run_explosion_phases(State#explosion_state{
        worker_sup = WorkerSup,
        server_pid = ServerPid
    }),
    
    {reply, {ok, Result}, State};

handle_call(get_status, _From, State) ->
    Status = #{
        phase = State#explosion_state.phase,
        worker_count = length(State#explosion_state.workers),
        crashed = State#explosion_state.crashed,
        crash_reason = State#explosion_state.crash_reason,
        elapsed_ms = os:system_time(millisecond) - State#explosion_state.start_time
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #explosion_state{}) -> {noreply, #explosion_state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #explosion_state{}) -> {noreply, #explosion_state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #explosion_state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("Process explosion test terminating"),
    ok.

-spec code_change(term(), #explosion_state{}, term()) -> {ok, #explosion_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Run explosion phases until crash
run_explosion_phases(State) ->
    Scenarios = scenarios(),
    Scenario = hd(Scenarios),
    Phases = Scenario#scenario.phases,
    
    ProcessLimit = erlang:system_info(process_limit),
    InitialCount = erlang:system_info(process_count),
    
    PhaseResults = run_phases(Phases, State, []),
    
    BreakingPoint = identify_breaking_point(PhaseResults),
    
    % Test recovery
    RecoveryResult = test_recovery(State),
    
    % VM status check
    VMSurvived = not State#explosion_state.crashed,
    VMResponsive = check_vm_responsive(),
    
    Result = #{
        workload_id => <<"process_explosion_full">>,
        benchmark => <<"chaos">>,
        scenario => <<"Process explosion to crash">>,
        system_process_limit => ProcessLimit,
        initial_process_count => InitialCount,
        phases => PhaseResults,
        breaking_point => BreakingPoint,
        vm_survived => VMSurvived,
        vm_responsive => VMResponsive,
        recovery_test => RecoveryResult,
        analysis => generate_analysis(PhaseResults, BreakingPoint, RecoveryResult),
        timestamp => os:system_time(second)
    },
    
    print_final_report(Result),
    
    {ok, Result}.

%% @doc Run all phases sequentially
run_phases([], _State, Acc) ->
    lists:reverse(Acc);
run_phases([Phase | Rest], State, Acc) ->
    #phase_config{
        target_count = Target,
        spawn_rate = Rate,
        duration = MaxDuration
    } = Phase,
    
    PhaseNum = State#explosion_state.phase,
    logger:warning("=== PHASE ~p: Spawning ~p processes at ~p/sec ===", [
        PhaseNum, Target, Rate
    ]),
    
    StartTime = os:system_time(millisecond),
    
    % Spawn workers
    {WorkersSpawned, SpawnResult} = spawn_workers_at_rate(
        Target,
        Rate,
        MaxDuration * 1000,
        State#explosion_state.worker_sup
    ),
    
    EndTime = os:system_time(millisecond),
    DurationS = (EndTime - StartTime) / 1000,
    
    % Collect metrics
    ProcessCount = erlang:system_info(process_count),
    MemoryTotal = erlang:memory(total),
    MemoryPerProc = case ProcessCount of
        0 -> 0;
        _ -> MemoryTotal div ProcessCount
    end,
    
    PhaseResult = #{
        phase => PhaseNum,
        target_count => Target,
        actual_spawned => WorkersSpawned,
        spawn_rate_target => Rate,
        spawn_rate_actual => case DurationS of
            0 -> 0;
            _ -> WorkersSpawned / DurationS
        end,
        duration_s => DurationS,
        process_count => ProcessCount,
        process_limit => erlang:system_info(process_limit),
        memory_total_mib => MemoryTotal / (1024 * 1024),
        memory_per_process_bytes => MemoryPerProc,
        scheduler_utilization => get_scheduler_utilization(),
        result => SpawnResult,
        crashed => WorkersSpawned < Target
    },
    
    logger:warning("Phase ~p Complete:", [PhaseNum]),
    logger:warning("  Spawned: ~p / ~p", [WorkersSpawned, Target]),
    logger:warning("  Process Count: ~p / ~p", [
        ProcessCount, erlang:system_info(process_limit)
    ]),
    logger:warning("  Memory: ~p MiB (~p bytes/process)", [
        PhaseResult#phase.memory_total_mib,
        MemoryPerProc
    ]),
    logger:warning("  Result: ~p", [SpawnResult]),
    
    % Check if we crashed
    case WorkersSpawned < Target of
        true ->
            logger:error("CRASH DETECTED at phase ~p", [PhaseNum]),
            logger:error("Could only spawn ~p of ~p processes", [WorkersSpawned, Target]),
            % Stop testing, we hit the limit
            NewState = State#explosion_state{
                crashed = true,
                crash_reason = SpawnResult
            },
            lists:reverse([PhaseResult | Acc]);
        false ->
            % Continue to next phase
            NewState = State#explosion_state{
                phase = PhaseNum + 1,
                phase_start_time = os:system_time(millisecond)
            },
            run_phases(Rest, NewState, [PhaseResult | Acc])
    end.

%% @doc Spawn workers at specified rate
spawn_workers_at_rate(Target, Rate, MaxDurationMs, Supervisor) ->
    SpawnIntervalMs = case Rate of
        0 -> 1;
        _ -> 1000 div Rate
    end,
    
    spawn_loop(0, Target, SpawnIntervalMs, MaxDurationMs, os:system_time(millisecond), Supervisor).

%% @doc Main spawn loop
spawn_loop(Count, Target, _Interval, MaxDurationMs, StartTime, _Sup) when Count >= Target ->
    {Count, success};
spawn_loop(Count, Target, Interval, MaxDurationMs, StartTime, Sup) ->
    % Check timeout
    Elapsed = os:system_time(millisecond) - StartTime,
    case Elapsed > MaxDurationMs of
        true ->
            logger:error("Phase timeout after ~p ms, spawned ~p / ~p", [
                Elapsed, Count, Target
            ]),
            {Count, timeout};
        false ->
            % Check process limit
            ProcessLimit = erlang:system_info(process_limit),
            ProcessCount = erlang:system_info(process_count),
            
            case ProcessCount >= ProcessLimit of
                true ->
                    logger:error("PROCESS LIMIT REACHED: ~p / ~p", [
                        ProcessCount, ProcessLimit
                    ]),
                    {Count, {system_limit, maximum_processes_exceeded}};
                false ->
                    % Try to spawn
                    case spawn_worker(Sup, Count) of
                        {ok, _Pid} ->
                            timer:sleep(Interval),
                            spawn_loop(Count + 1, Target, Interval, MaxDurationMs, StartTime, Sup);
                        {error, Reason} ->
                            logger:error("Spawn failed at ~p processes: ~p", [
                                ProcessCount, Reason
                            ]),
                            {Count, {spawn_error, Reason}}
                    end
            end
    end.

%% @doc Spawn a single worker process
spawn_worker(Supervisor, Id) ->
    WorkerSpec = #{
        id => {explosion_worker, Id},
        start => {explosion_worker, start_link, [Id]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [explosion_worker]
    },
    
    try supervisor:start_child(Supervisor, WorkerSpec) of
        {ok, Pid} -> {ok, Pid};
        {ok, Pid, _Info} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    catch
        exit:Reason -> {error, {exit, Reason}};
        error:Reason -> {error, {error, Reason}}
    end.

%% @doc Identify breaking point from phase results
identify_breaking_point(PhaseResults) ->
    CrashedPhases = [P || P <- PhaseResults, P#phase.crashed],
    case CrashedPhases of
        [] -> undefined;
        [FirstCrash | _] ->
            #{
                phase => FirstCrash#phase.phase,
                process_count => FirstCrash#phase.process_count,
                time_to_limit_s => FirstCrash#phase.duration_s,
                error => FirstCrash#phase.result,
                memory_at_crash_mib => FirstCrash#phase.memory_total_mib
            }
    end.

%% @doc Test recovery by killing half the workers
test_recovery(State) ->
    Workers = supervisor:which_children(State#explosion_state.worker_sup),
    WorkerCount = length(Workers),
    ToKill = WorkerCount div 2,
    
    logger:warning("=== RECOVERY TEST: Killing ~p of ~p workers ===", [
        ToKill, WorkerCount
    ]),
    
    StartTime = os:system_time(millisecond),
    
    % Kill half the workers
    KillList = lists:sublist(Workers, ToKill),
    lists:foreach(
        fun({_Id, Pid, _Type, _Modules}) ->
            exit(Pid, kill)
        end,
        KillList
    ),
    
    % Wait for cleanup
    timer:sleep(5000),
    
    % Try to spawn new workers
    NewWorkersToSpawn = min(1000, ToKill),
    {NewSpawned, SpawnResult} = spawn_workers_at_rate(
        NewWorkersToSpawn,
        100,  % 100/sec
        30000,  % 30 second timeout
        State#explosion_state.worker_sup
    ),
    
    RecoveryTime = os:system_time(millisecond) - StartTime,
    
    #{
        workers_killed => ToKill,
        workers_remaining => WorkerCount - ToKill,
        new_workers_spawned => NewSpawned,
        spawn_result => SpawnResult,
        recovery_time_ms => RecoveryTime,
        recovered => NewSpawned >= NewWorkersToSpawn div 2
    }.

%% @doc Check if VM is responsive
check_vm_responsive() ->
    try
        % Try simple operations
        _ = erlang:system_info(process_count),
        _ = erlang:memory(total),
        _ = processes(),
        true
    catch
        _:_ -> false
    end.

%% @doc Get scheduler utilization
get_scheduler_utilization() ->
    try
        Schedulers = erlang:system_info(schedulers),
        Stats = [erlang:statistics(I) || I <- [run_queue, total_run_queue_lengths]],
        #{
            schedulers => Schedulers,
            run_queue => proplists:get_value(run_queue, Stats),
            total_run_queue => proplists:get_value(total_run_queue_lengths, Stats)
        }
    catch
        _:_ -> #{error => unable_to_get_stats}
    end.

%% @doc Start test MCP server
start_test_server() ->
    {ok, Sup} = erlmcp_worker_sup:start_link(),
    {ok, Server} = erlmcp_server:start_link(
        explosion_test_server,
        #mcp_server_capabilities{}
    ),
    
    % Add an echo tool for workers
    EchoHandler = fun(Args) ->
        #{
            jsonrpc => <<"2.0">>,
            id => maps:get(<<"id">>, Args),
            result => maps:get(<<"params">>, Args)
        }
    end,
    
    erlmcp_server:add_tool(Server, <<"echo">>, EchoHandler),
    
    {ok, Sup}.

%% @doc Start worker supervisor
start_worker_supervisor() ->
    {ok, Pid} = erlmcp_worker_sup:start_link(),
    {ok, Pid}.

%% @doc Generate analysis text
generate_analysis(_PhaseResults, undefined, _Recovery) ->
    <<"System did not crash. Hit phase limit or test stopped.">>;
generate_analysis(_PhaseResults, BreakingPoint, Recovery) ->
    ProcessCount = maps:get(process_count, BreakingPoint),
    ProcessLimit = erlang:system_info(process_limit),
    PercentUsed = (ProcessCount / ProcessLimit) * 100,
    
    Analysis = io_lib:format(
        "VM crashed at ~p processes (~.1f of limit).~n"
        "Memory at crash: ~.2f MiB.~n"
        "Recovery: ~s",
        [
            ProcessCount,
            PercentUsed / 100,
            maps:get(memory_at_crash_mib, BreakingPoint),
            case maps:get(recovered, Recovery) of
                true -> "SUCCESSFUL";
                false -> "FAILED"
            end
        ]
    ),
    iolist_to_binary(Analysis).

%% @doc Print final report
print_final_report(Result) ->
    io:format("~n=== PROCESS EXPLOSION CRASH TEST COMPLETE ===~n~n", []),
    
    io:format("System Limits:~n", []),
    io:format("  Process Limit: ~p~n", [maps:get(system_process_limit, Result)]),
    io:format("  Initial Count: ~p~n", [maps:get(initial_process_count, Result)]),
    io:format("  Available Slots: ~p~n~n", [
        maps:get(system_process_limit, Result) - maps:get(initial_process_count, Result)
    ]),
    
    Phases = maps:get(phases, Result),
    io:format("Explosion Progress:~n", []),
    lists:foreach(
        fun(Phase) ->
            io:format("  Phase ~p: ~p / ~p spawned (~p proc, ~.2f MiB) - ~s~n", [
                maps:get(phase, Phase),
                maps:get(actual_spawned, Phase),
                maps:get(target_count, Phase),
                maps:get(process_count, Phase),
                maps:get(memory_total_mib, Phase),
                maps:get(result, Phase)
            ])
        end,
        Phases
    ),
    
    case maps:get(breaking_point, Result) of
        undefined ->
            io:format("~nNo breaking point reached - test stopped~n", []);
        BreakingPoint ->
            io:format("~nBREAKING POINT:~n", []),
            io:format("  Phase: ~p~n", [maps:get(phase, BreakingPoint)]),
            io:format("  Process Count: ~p~n", [maps:get(process_count, BreakingPoint)]),
            io:format("  Time to Reach: ~.2f seconds~n", [maps:get(time_to_limit_s, BreakingPoint)]),
            io:format("  Error: ~p~n", [maps:get(error, BreakingPoint)]),
            io:format("  Memory: ~.2f MiB~n", [maps:get(memory_at_crash_mib, BreakingPoint)])
    end,
    
    io:format("~nVM STATUS:~n", []),
    io:format("  VM Survived: ~p~n", [maps:get(vm_survived, Result)]),
    io:format("  Responsive: ~p~n", [maps:get(vm_responsive, Result)]),
    
    Recovery = maps:get(recovery_test, Result),
    io:format("~nRECOVERY TEST:~n", []),
    io:format("  Killed: ~p workers~n", [maps:get(workers_killed, Recovery)]),
    io:format("  Remaining: ~p workers~n", [maps:get(workers_remaining, Recovery)]),
    io:format("  New Spawns: ~p / ~p~n", [
        maps:get(new_workers_spawned, Recovery),
        1000
    ]),
    io:format("  Recovered: ~p~n", [maps:get(recovered, Recovery)]),
    io:format("  Recovery Time: ~p ms~n", [maps:get(recovery_time_ms, Recovery)]),
    
    io:format("~nANALYSIS:~n  ~s~n", [maps:get(analysis, Result)]),
    io:format("~n========================================~n~n", []).

%%====================================================================
%% Worker Process (Infinite Loop Echo)
%%====================================================================

-module(explosion_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

init([Id]) ->
    logger:debug("Explosion worker ~p starting", [Id]),
    {ok, #{id => Id, loops => 0}}.

handle_call(_Req, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(infinite_loop, State = #{loops => N}) ->
    % Infinite loop to consume CPU
    erlang:send_after(1000, self(), infinite_loop),
    {noreply, State#{loops => N + 1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Worker Supervisor
%%====================================================================

-module(erlmcp_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 100,  % High intensity for explosion test
        period => 1
    },
    
    ChildSpecs = [
        #{
            id => explosion_worker,
            start => {explosion_worker, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker,
            modules => [explosion_worker]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
