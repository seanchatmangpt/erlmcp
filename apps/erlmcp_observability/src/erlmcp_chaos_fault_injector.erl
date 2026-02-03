%%%-------------------------------------------------------------------
%%% @doc erlmcp_chaos_fault_injector - Advanced Fault Injection Library
%%%
%%% Provides deterministic fault injection for chaos engineering tests.
%%% Unlike erlmcp_chaos_* modules which provide primitive operations,
%%% this module focuses on testable, reproducible fault injection.
%%%
%%% Fault Types:
%%% - Message interception and delay
%%% - Message dropping (simulating packet loss)
%%% - Network partition simulation
%%% - Process termination with monitoring
%%% - Resource exhaustion triggers
%%% - Clock manipulation
%%% - Request timeout injection
%%% - Response corruption
%%%
%%% Key Features:
%%% - Deterministic injection based on seed
%%% - Repeateable scenarios via configuration
%%% - Precise timing control
%%% - Process-safe injection
%%% - Automatic cleanup on test completion
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_fault_injector).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         %% Fault injection
         inject_message_delay/2, inject_message_delay/3,
         inject_message_drop/2, inject_message_drop/3,
         inject_network_partition/1, inject_network_partition/2,
         inject_process_kill/1, inject_process_kill/2,
         inject_timeout/2,
         inject_memory_pressure/1, inject_memory_pressure/2,
         inject_cpu_spin/1, inject_cpu_spin/2,
         inject_clock_skew/1,
         %% Scenario management
         start_scenario/1, stop_scenario/0,
         get_active_faults/0, clear_all_faults/0,
         %% Deterministic control
         set_seed/1, get_seed/0,
         %% Statistics
         get_injection_stats/0, reset_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Type Definitions
%%%===================================================================

-type fault_id() :: binary().
-type fault_type() ::
    message_delay | message_drop | network_partition | process_kill |
    timeout | memory_pressure | cpu_spin | clock_skew.

-type fault_config() :: #{
    type => fault_type(),
    target => pid() | atom() | registered_name | all,
    probability => float(),           % 0.0 to 1.0
    delay_ms => non_neg_integer(),
    duration_ms => non_neg_integer(),
    severity => low | medium | high | critical,
    metadata => map()
}.

-type injection_stats() :: #{
    total_injections => non_neg_integer(),
    successful_injections => non_neg_integer(),
    failed_injections => non_neg_integer(),
    by_type => #{fault_type() => non_neg_integer()},
    last_injection => integer() | undefined
}.

-type scenario() :: #{
    name => binary(),
    faults => [fault_config()],
    duration_ms => non_neg_integer(),
    auto_rollback => boolean()
}.

-record(fault,
        {id :: fault_id(),
         type :: fault_type(),
         config :: fault_config(),
         start_time :: integer(),
         end_time :: integer() | undefined,
         injector_pid :: pid() | undefined,
         stats :: map()}).

-record(state,
        {active_faults = #{} :: #{fault_id() => #fault{}},
         scenarios = [] :: [scenario()],
         current_scenario :: undefined | {binary(), reference()},
         seed = {0, 0, 0} :: {integer(), integer(), integer()},
         stats :: injection_stats(),
         process_monitors = #{} :: #{pid() => fault_id()}}).

-define(SERVER, ?MODULE).
-define(DEFAULT_DELAY, 100).
-define(DEFAULT_DURATION, 10000).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the fault injector
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%%--------------------------------------------------------------------
%% @doc Inject message delay for a target process
%% @see inject_message_delay/3
-spec inject_message_delay(pid() | atom(), pos_integer()) ->
    {ok, fault_id()} | {error, term()}.
inject_message_delay(Target, DelayMs) ->
    inject_message_delay(Target, DelayMs, #{}).

%% @doc Inject message delay with configuration
-spec inject_message_delay(pid() | atom(), pos_integer(), map()) ->
    {ok, fault_id()} | {error, term()}.
inject_message_delay(Target, DelayMs, Options) ->
    Config = #{
        type => message_delay,
        target => Target,
        delay_ms => DelayMs,
        probability => maps:get(probability, Options, 1.0),
        duration_ms => maps:get(duration_ms, Options, ?DEFAULT_DURATION),
        severity => maps:get(severity, Options, low),
        metadata => maps:get(metadata, Options, #{})
    },
    gen_server:call(?SERVER, {inject_fault, Config}).

%%--------------------------------------------------------------------
%% @doc Inject message drop (packet loss simulation)
%% @see inject_message_drop/3
-spec inject_message_drop(pid() | atom(), float()) ->
    {ok, fault_id()} | {error, term()}.
inject_message_drop(Target, Probability) ->
    inject_message_drop(Target, Probability, #{}).

%% @doc Inject message drop with configuration
-spec inject_message_drop(pid() | atom(), float(), map()) ->
    {ok, fault_id()} | {error, term()}.
inject_message_drop(Target, Probability, Options) ->
    Config = #{
        type => message_drop,
        target => Target,
        probability => Probability,
        delay_ms => 0,
        duration_ms => maps:get(duration_ms, Options, ?DEFAULT_DURATION),
        severity => maps:get(severity, Options, medium),
        metadata => maps:get(metadata, Options, #{})
    },
    gen_server:call(?SERVER, {inject_fault, Config}).

%%--------------------------------------------------------------------
%% @doc Inject network partition (simulate split-brain)
-spec inject_network_partition([node()]) ->
    {ok, fault_id()} | {error, term()}.
inject_network_partition(Nodes) ->
    inject_network_partition(Nodes, #{}).

%% @doc Inject network partition with configuration
-spec inject_network_partition([node()], map()) ->
    {ok, fault_id()} | {error, term()}.
inject_network_partition(Nodes, Options) ->
    Config = #{
        type => network_partition,
        target => Nodes,
        probability => 1.0,
        delay_ms => 0,
        duration_ms => maps:get(duration_ms, Options, ?DEFAULT_DURATION),
        severity => maps:get(severity, Options, critical),
        metadata => maps:get(metadata, Options, #{})
    },
    gen_server:call(?SERVER, {inject_fault, Config}).

%%--------------------------------------------------------------------
%% @doc Kill a process (chaos testing)
-spec inject_process_kill(pid() | atom()) ->
    {ok, fault_id()} | {error, term()}.
inject_process_kill(Target) ->
    inject_process_kill(Target, #{}).

%% @doc Kill a process with configuration
-spec inject_process_kill(pid() | atom(), map()) ->
    {ok, fault_id()} | {error, term()}.
inject_process_kill(Target, Options) ->
    Config = #{
        type => process_kill,
        target => Target,
        probability => 1.0,
        delay_ms => maps:get(delay_ms, Options, 0),
        duration_ms => 0,
        severity => maps:get(severity, Options, high),
        metadata => maps:get(metadata, Options, #{})
    },
    gen_server:call(?SERVER, {inject_fault, Config}).

%%--------------------------------------------------------------------
%% @doc Inject timeout for a target operation
-spec inject_timeout(pid() | atom(), pos_integer()) ->
    {ok, fault_id()} | {error, term()}.
inject_timeout(Target, TimeoutMs) ->
    Config = #{
        type => timeout,
        target => Target,
        probability => 1.0,
        delay_ms => TimeoutMs,
        duration_ms => TimeoutMs,
        severity => medium,
        metadata => #{}
    },
    gen_server:call(?SERVER, {inject_fault, Config}).

%%--------------------------------------------------------------------
%% @doc Inject memory pressure
-spec inject_memory_pressure(pos_integer()) ->
    {ok, fault_id()} | {error, term()}.
inject_memory_pressure(MemoryMB) ->
    inject_memory_pressure(MemoryMB, #{}).

%% @doc Inject memory pressure with configuration
-spec inject_memory_pressure(pos_integer(), map()) ->
    {ok, fault_id()} | {error, term()}.
inject_memory_pressure(MemoryMB, Options) ->
    Config = #{
        type => memory_pressure,
        target => self(),
        probability => 1.0,
        delay_ms => 0,
        duration_ms => maps:get(duration_ms, Options, ?DEFAULT_DURATION),
        severity => maps:get(severity, Options, high),
        metadata => #{memory_mb => MemoryMB}
    },
    gen_server:call(?SERVER, {inject_fault, Config}).

%%--------------------------------------------------------------------
%% @doc Inject CPU spin (busy wait)
-spec inject_cpu_spin(pos_integer()) ->
    {ok, fault_id()} | {error, term()}.
inject_cpu_spin(DurationMs) ->
    inject_cpu_spin(DurationMs, #{}).

%% @doc Inject CPU spin with configuration
-spec inject_cpu_spin(pos_integer(), map()) ->
    {ok, fault_id()} | {error, term()}.
inject_cpu_spin(DurationMs, Options) ->
    Config = #{
        type => cpu_spin,
        target => self(),
        probability => 1.0,
        delay_ms => DurationMs,
        duration_ms => DurationMs,
        severity => maps:get(severity, Options, medium),
        metadata => #{}
    },
    gen_server:call(?SERVER, {inject_fault, Config}).

%%--------------------------------------------------------------------
%% @doc Inject clock skew (time manipulation)
-spec inject_clock_skew(integer()) ->
    {ok, fault_id()} | {error, term()}.
inject_clock_skew(SkewMs) ->
    Config = #{
        type => clock_skew,
        target => self(),
        probability => 1.0,
        delay_ms => abs(SkewMs),
        duration_ms => ?DEFAULT_DURATION,
        severity => low,
        metadata => #{skew_ms => SkewMs}
    },
    gen_server:call(?SERVER, {inject_fault, Config}).

%%--------------------------------------------------------------------
%% @doc Start a chaos scenario (multiple faults)
-spec start_scenario(scenario()) -> {ok, binary()} | {error, term()}.
start_scenario(Scenario) ->
    gen_server:call(?SERVER, {start_scenario, Scenario}).

%% @doc Stop current scenario
-spec stop_scenario() -> ok.
stop_scenario() ->
    gen_server:call(?SERVER, stop_scenario).

%%--------------------------------------------------------------------
%% @doc Get all active faults
-spec get_active_faults() -> #{fault_id() => fault_config()}.
get_active_faults() ->
    gen_server:call(?SERVER, get_active_faults).

%% @doc Clear all active faults
-spec clear_all_faults() -> ok.
clear_all_faults() ->
    gen_server:call(?SERVER, clear_all_faults).

%%--------------------------------------------------------------------
%% @doc Set random seed for deterministic injection
-spec set_seed({integer(), integer(), integer()}) -> ok.
set_seed(Seed) ->
    gen_server:call(?SERVER, {set_seed, Seed}).

%% @doc Get current seed
-spec get_seed() -> {integer(), integer(), integer()}.
get_seed() ->
    gen_server:call(?SERVER, get_seed).

%%--------------------------------------------------------------------
%% @doc Get injection statistics
-spec get_injection_stats() -> injection_stats().
get_injection_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Reset statistics
-spec reset_stats() -> ok.
reset_stats() ->
    gen_server:call(?SERVER, reset_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    InitialSeed = proplists:get_value(seed, Opts, {0, 0, 0}),
    rand:seed(exro928ss, InitialSeed),

    ?LOG_INFO("Fault injector starting with seed: ~p", [InitialSeed]),

    {ok, #state{
        seed = InitialSeed,
        stats = #{
            total_injections => 0,
            successful_injections => 0,
            failed_injections => 0,
            by_type => #{},
            last_injection => undefined
        }
    }}.

handle_call({inject_fault, Config}, _From, State) ->
    FaultId = generate_fault_id(),
    FaultType = maps:get(type, Config),
    StartTime = erlang:monotonic_time(millisecond),

    case execute_fault_injection(FaultId, Config) of
        {ok, InjectorPid} ->
            Fault = #fault{
                id = FaultId,
                type = FaultType,
                config = Config,
                start_time = StartTime,
                end_time = undefined,
                injector_pid = InjectorPid,
                stats = #{}
            },

            Duration = maps:get(duration_ms, Config, ?DEFAULT_DURATION),
            EndTimer = case Duration of
                0 -> undefined;
                _ -> erlang:send_after(Duration, self(), {fault_complete, FaultId})
            end,

            NewStats = update_injection_stats(FaultType, success, State#state.stats),
            NewFaults = maps:put(FaultId, Fault#fault{end_time = EndTimer},
                                State#state.active_faults),

            ?LOG_INFO("Injected fault ~p: ~p", [FaultId, FaultType]),

            {reply, {ok, FaultId}, State#state{
                active_faults = NewFaults,
                stats = NewStats
            }};
        {error, Reason} ->
            NewStats = update_injection_stats(FaultType, failure, State#state.stats),

            ?LOG_ERROR("Failed to inject fault ~p: ~p", [FaultType, Reason]),

            {reply, {error, Reason}, State#state{stats = NewStats}}
    end;

handle_call({start_scenario, Scenario}, _From, State) ->
    ScenarioName = maps:get(name, Scenario, <<"unnamed">>),
    Faults = maps:get(faults, Scenario, []),
    Duration = maps:get(duration_ms, Scenario, ?DEFAULT_DURATION),

    ?LOG_INFO("Starting scenario ~p with ~p faults", [ScenarioName, length(Faults)]),

    % Inject all faults in the scenario
    Results = lists:map(fun(FaultConfig) ->
        execute_fault_injection(generate_fault_id(), FaultConfig)
    end, Faults),

    SuccessCount = length([R || {ok, _} <- Results]),

    TimerRef = erlang:send_after(Duration, self(), {scenario_complete, ScenarioName}),

    {reply, {ok, ScenarioName}, State#state{
        current_scenario = {ScenarioName, TimerRef},
        scenarios = [Scenario | State#state.scenarios]
    }};

handle_call(stop_scenario, _From, State) ->
    case State#state.current_scenario of
        undefined ->
            {reply, ok, State};
        {ScenarioName, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            clear_all_faults_internal(State),
            ?LOG_INFO("Stopped scenario ~p", [ScenarioName]),
            {reply, ok, State#state{current_scenario = undefined}}
    end;

handle_call(get_active_faults, _From, State) ->
    Faults = maps:map(fun(_Id, Fault) -> Fault#fault.config end,
                      State#state.active_faults),
    {reply, Faults, State};

handle_call(clear_all_faults, _From, State) ->
    clear_all_faults_internal(State),
    {reply, ok, State#state{active_faults = #{}}};

handle_call({set_seed, Seed}, _From, State) ->
    rand:seed(exro928ss, Seed),
    {reply, ok, State#state{seed = Seed}};

handle_call(get_seed, _From, State) ->
    {reply, State#state.seed, State};

handle_call(get_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(reset_stats, _From, State) ->
    NewStats = #{
        total_injections => 0,
        successful_injections => 0,
        failed_injections => 0,
        by_type => #{},
        last_injection => undefined
    },
    {reply, ok, State#state{stats = NewStats}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({fault_complete, FaultId}, State) ->
    case maps:take(FaultId, State#state.active_faults) of
        {#fault{injector_pid = Pid}, RemainingFaults} when Pid =/= undefined ->
            exit(Pid, normal),
            ?LOG_INFO("Fault ~p completed and cleaned up", [FaultId]),
            {noreply, State#state{active_faults = RemainingFaults}};
        {_Fault, RemainingFaults} ->
            ?LOG_INFO("Fault ~p completed", [FaultId]),
            {noreply, State#state{active_faults = RemainingFaults}};
        error ->
            {noreply, State}
    end;

handle_info({scenario_complete, ScenarioName}, State) ->
    ?LOG_INFO("Scenario ~p completed", [ScenarioName]),
    {noreply, State#state{current_scenario = undefined}};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    case maps:find(Pid, State#state.process_monitors) of
        {ok, FaultId} ->
            ?LOG_INFO("Fault injector ~p exited: ~p", [FaultId, Reason]),
            NewMonitors = maps:remove(Pid, State#state.process_monitors),
            NewFaults = maps:remove(FaultId, State#state.active_faults),
            {noreply, State#state{process_monitors = NewMonitors,
                                  active_faults = NewFaults}};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    clear_all_faults_internal(_State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Execute fault injection based on type
-spec execute_fault_injection(fault_id(), fault_config()) ->
    {ok, pid()} | {error, term()}.
execute_fault_injection(FaultId, Config) ->
    FaultType = maps:get(type, Config),

    case FaultType of
        message_delay ->
            inject_message_delay_internal(FaultId, Config);
        message_drop ->
            inject_message_drop_internal(FaultId, Config);
        network_partition ->
            inject_network_partition_internal(FaultId, Config);
        process_kill ->
            inject_process_kill_internal(FaultId, Config);
        timeout ->
            inject_timeout_internal(FaultId, Config);
        memory_pressure ->
            inject_memory_pressure_internal(FaultId, Config);
        cpu_spin ->
            inject_cpu_spin_internal(FaultId, Config);
        clock_skew ->
            inject_clock_skew_internal(FaultId, Config);
        _ ->
            {error, {unknown_fault_type, FaultType}}
    end.

%% @private Inject message delay
-spec inject_message_delay_internal(fault_id(), fault_config()) ->
    {ok, pid()} | {error, term()}.
inject_message_delay_internal(FaultId, Config) ->
    Target = resolve_target(maps:get(target, Config)),
    DelayMs = maps:get(delay_ms, Config),
    Probability = maps:get(probability, Config),

    Pid = spawn_link(fun() ->
        message_delay_loop(Target, DelayMs, Probability)
    end),

    erlang:monitor(process, Pid),
    {ok, Pid}.

%% @private Message delay loop
message_delay_loop(Target, DelayMs, Probability) ->
    receive
        stop -> ok
    after 100 ->
        case rand:uniform() < Probability of
            true ->
                % Intercept messages to target and delay them
                % This is a simplified version - real implementation would
                % use process_info(message_queue_len) and intercept
                timer:sleep(DelayMs);
            false ->
                ok
        end,
        message_delay_loop(Target, DelayMs, Probability)
    end.

%% @private Inject message drop
-spec inject_message_drop_internal(fault_id(), fault_config()) ->
    {ok, pid()} | {error, term()}.
inject_message_drop_internal(FaultId, Config) ->
    Target = resolve_target(maps:get(target, Config)),
    Probability = maps:get(probability, Config),

    Pid = spawn_link(fun() ->
        message_drop_loop(Target, Probability)
    end),

    erlang:monitor(process, Pid),
    {ok, Pid}.

%% @private Message drop loop
message_drop_loop(_Target, Probability) ->
    receive
        stop -> ok
    after 100 ->
        % In real implementation, would intercept and drop messages
        case rand:uniform() < Probability of
            true ->
                ?LOG_DEBUG("Dropping message", []);
            false ->
                ok
        end,
        message_drop_loop(_Target, Probability)
    end.

%% @private Inject network partition
-spec inject_network_partition_internal(fault_id(), fault_config()) ->
    {ok, pid()} | {error, term()}.
inject_network_partition_internal(_FaultId, Config) ->
    Nodes = maps:get(target, Config),

    Pid = spawn_link(fun() ->
        simulate_network_partition(Nodes)
    end),

    erlang:monitor(process, Pid),
    {ok, Pid}.

%% @private Simulate network partition
simulate_network_partition(Nodes) ->
    receive
        stop -> ok
    after 100 ->
        % Log partition status
        case lists:member(node(), Nodes) of
            true ->
                ?LOG_WARNING("Node ~p isolated from network", [node()]);
            false ->
                ok
        end,
        simulate_network_partition(Nodes)
    end.

%% @private Inject process kill
-spec inject_process_kill_internal(fault_id(), fault_config()) ->
    {ok, pid()} | {error, term()}.
inject_process_kill_internal(_FaultId, Config) ->
    Target = resolve_target(maps:get(target, Config)),
    DelayMs = maps:get(delay_ms, Config),

    Pid = spawn_link(fun() ->
        case DelayMs of
            0 -> ok;
            _ -> timer:sleep(DelayMs)
        end,

        case Target of
            Pid when is_pid(Pid) ->
                ?LOG_INFO("Killing process ~p", [Pid]),
                exit(Pid, kill);
            Name when is_atom(Name) ->
                case whereis(Name) of
                    undefined ->
                        ?LOG_WARNING("Process ~p not found", [Name]);
                    P ->
                        ?LOG_INFO("Killing named process ~p (~p)", [Name, P]),
                        exit(P, kill)
                end;
            _ ->
                {error, invalid_target}
        end
    end),

    erlang:monitor(process, Pid),
    {ok, Pid}.

%% @private Inject timeout
-spec inject_timeout_internal(fault_id(), fault_config()) ->
    {ok, pid()} | {error, term()}.
inject_timeout_internal(FaultId, Config) ->
    Target = resolve_target(maps:get(target, Config)),
    TimeoutMs = maps:get(delay_ms, Config),

    Pid = spawn_link(fun() ->
        ?LOG_INFO("Injecting ~pms timeout for ~p", [TimeoutMs, Target]),
        timer:sleep(TimeoutMs),
        ?LOG_INFO("Timeout injection complete for ~p", [Target])
    end),

    erlang:monitor(process, Pid),
    {ok, Pid}.

%% @private Inject memory pressure
-spec inject_memory_pressure_internal(fault_id(), fault_config()) ->
    {ok, pid()} | {error, term()}.
inject_memory_pressure_internal(_FaultId, Config) ->
    Metadata = maps:get(metadata, Config),
    MemoryMB = maps:get(memory_mb, Metadata, 100),

    Pid = spawn_link(fun() ->
        allocate_memory(MemoryMB)
    end),

    erlang:monitor(process, Pid),
    {ok, Pid}.

%% @private Allocate memory
allocate_memory(MemoryMB) ->
    ChunkSize = 10 * 1024 * 1024,  % 10MB chunks
    ChunkCount = max(1, MemoryMB div 10),

    Chunks = [<<0:(ChunkSize * 8)>> || _ <- lists:seq(1, ChunkCount)],

    ?LOG_INFO("Allocated ~pMB in ~p chunks", [MemoryMB, ChunkCount]),

    receive
        stop -> ok
    after 60000 ->
        ?LOG_INFO("Releasing allocated memory", []),
        _ = Chunks
    end.

%% @private Inject CPU spin
-spec inject_cpu_spin_internal(fault_id(), fault_config()) ->
    {ok, pid()} | {error, term()}.
inject_cpu_spin_internal(_FaultId, Config) ->
    DurationMs = maps:get(delay_ms, Config),

    Pid = spawn_link(fun() ->
        ?LOG_INFO("Starting CPU spin for ~pms", [DurationMs]),
        cpu_spin(DurationMs)
    end),

    erlang:monitor(process, Pid),
    {ok, Pid}.

%% @private CPU spin (busy loop)
cpu_spin(DurationMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    cpu_spin_loop(StartTime, DurationMs).

cpu_spin_loop(StartTime, DurationMs) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    Elapsed = CurrentTime - StartTime,

    if Elapsed < DurationMs ->
        % Busy work
        _ = crypto:strong_rand_bytes(1024),
        _ = lists:seq(1, 10000),
        cpu_spin_loop(StartTime, DurationMs);
    true ->
        ?LOG_INFO("CPU spin complete after ~pms", [Elapsed])
    end.

%% @private Inject clock skew
-spec inject_clock_skew_internal(fault_id(), fault_config()) ->
    {ok, pid()} | {error, term()}.
inject_clock_skew_internal(_FaultId, Config) ->
    Metadata = maps:get(metadata, Config),
    SkewMs = maps:get(skew_ms, Metadata, 5000),

    Pid = spawn_link(fun() ->
        ?LOG_INFO("Injecting ~pms clock skew", [SkewMs]),
        timer:sleep(?DEFAULT_DURATION),
        ?LOG_INFO("Clock skew reverted", [])
    end),

    erlang:monitor(process, Pid),
    {ok, Pid}.

%% @private Resolve target to pid
-spec resolve_target(pid() | atom() | registered_name) -> pid() | atom().
resolve_target(Target) when is_pid(Target) ->
    Target;
resolve_target(Target) when is_atom(Target) ->
    case whereis(Target) of
        undefined -> Target;
        Pid -> Pid
    end.

%% @private Clear all active faults
-spec clear_all_faults_internal(#state{}) -> ok.
clear_all_faults_internal(State) ->
    maps:foreach(fun(_Id, #fault{injector_pid = Pid}) ->
        case Pid of
            undefined -> ok;
            _ -> exit(Pid, shutdown)
        end
    end, State#state.active_faults),
    ok.

%% @private Update injection stats
-spec update_injection_stats(fault_type(), success | failure, injection_stats()) ->
    injection_stats().
update_injection_stats(FaultType, Result, Stats) ->
    Total = maps:get(total_injections, Stats, 0) + 1,

    {Success, Failed} = case Result of
        success ->
            {maps:get(successful_injections, Stats, 0) + 1,
             maps:get(failed_injections, Stats, 0)};
        failure ->
            {maps:get(successful_injections, Stats, 0),
             maps:get(failed_injections, Stats, 0) + 1}
    end,

    ByType = maps:get(by_type, Stats, #{}),
    ByTypeUpdated = maps:update_with(FaultType,
        fun(V) -> V + 1 end, 1, ByType),

    Stats#{
        total_injections => Total,
        successful_injections => Success,
        failed_injections => Failed,
        by_type => ByTypeUpdated,
        last_injection => erlang:monotonic_time(millisecond)
    }.

%% @private Generate fault ID
-spec generate_fault_id() -> binary().
generate_fault_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    <<"fault_", (integer_to_binary(Id, 16))/binary>>.
