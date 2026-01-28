%%%-------------------------------------------------------------------
%%% @doc erlmcp_chaos - Chaos Engineering Framework
%%%
%%% Main chaos engineering orchestration module. Provides high-level API
%%% for running chaos experiments with safety controls, monitoring, and
%%% automatic rollback capabilities.
%%%
%%% Features:
%%% - Network failure injection (latency, partition, packet loss)
%%% - Process crash scenarios (kill random processes)
%%% - Resource exhaustion (CPU, memory, disk)
%%% - Clock skew injection
%%% - Safety controls (blast radius, auto-rollback, dry-run)
%%% - Integration with health monitoring
%%% - Experiment orchestration and reporting
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    run/1,
    run/2,
    stop_experiment/1,
    stop_all_experiments/0,
    get_active_experiments/0,
    get_experiment_status/1,
    get_chaos_report/0,
    dry_run/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type experiment_id() :: binary() | atom().
-type experiment_type() :: network_latency | network_partition | packet_loss |
                          kill_servers | kill_random | resource_memory |
                          resource_cpu | resource_disk | clock_skew.
-type experiment_config() :: #{
    experiment := experiment_type(),
    target => module() | pid() | atom(),
    rate => float(),           % 0.0 to 1.0, percentage affected
    latency => pos_integer(),  % milliseconds
    interval => pos_integer(), % milliseconds between injections
    duration => pos_integer(), % total duration in milliseconds
    max_blast_radius => float(), % max % of system affected
    auto_rollback => boolean(),
    dry_run => boolean(),
    safety_checks => boolean(),
    sla_threshold => map()     % SLA violations trigger stop
}.

-type experiment_status() :: #{
    id := experiment_id(),
    type := experiment_type(),
    state := running | stopped | failed | completed,
    start_time := erlang:timestamp(),
    end_time => erlang:timestamp(),
    targets_affected := non_neg_integer(),
    total_targets := non_neg_integer(),
    blast_radius := float(),
    incidents := [term()],
    metrics := map()
}.

-record(experiment, {
    id :: experiment_id(),
    type :: experiment_type(),
    config :: experiment_config(),
    state = running :: running | stopped | failed | completed,
    start_time :: erlang:timestamp(),
    end_time :: erlang:timestamp() | undefined,
    timer_ref :: reference() | undefined,
    worker_pid :: pid() | undefined,
    targets_affected = 0 :: non_neg_integer(),
    total_targets = 0 :: non_neg_integer(),
    incidents = [] :: [term()],
    metrics = #{} :: map()
}).

-record(state, {
    experiments = #{} :: #{experiment_id() => #experiment{}},
    safety_enabled = true :: boolean(),
    monitor_integration = true :: boolean(),
    global_limits = #{} :: map()
}).

-define(DEFAULT_MAX_BLAST_RADIUS, 0.3).  % 30% max affected
-define(DEFAULT_INTERVAL, 30000).         % 30 seconds
-define(DEFAULT_DURATION, 300000).        % 5 minutes
-define(SAFETY_CHECK_INTERVAL, 5000).     % 5 seconds

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Run a chaos experiment with default config
-spec run(experiment_config()) -> {ok, experiment_id()} | {error, term()}.
run(Config) ->
    run(generate_experiment_id(), Config).

%% @doc Run a chaos experiment with specific ID and config
-spec run(experiment_id(), experiment_config()) -> {ok, experiment_id()} | {error, term()}.
run(ExperimentId, Config) ->
    gen_server:call(?MODULE, {run_experiment, ExperimentId, Config}).

%% @doc Stop a running experiment
-spec stop_experiment(experiment_id()) -> ok | {error, term()}.
stop_experiment(ExperimentId) ->
    gen_server:call(?MODULE, {stop_experiment, ExperimentId}).

%% @doc Stop all running experiments
-spec stop_all_experiments() -> ok.
stop_all_experiments() ->
    gen_server:call(?MODULE, stop_all_experiments).

%% @doc Get all active experiments
-spec get_active_experiments() -> [experiment_status()].
get_active_experiments() ->
    gen_server:call(?MODULE, get_active_experiments).

%% @doc Get status of specific experiment
-spec get_experiment_status(experiment_id()) -> {ok, experiment_status()} | {error, not_found}.
get_experiment_status(ExperimentId) ->
    gen_server:call(?MODULE, {get_experiment_status, ExperimentId}).

%% @doc Get comprehensive chaos report
-spec get_chaos_report() -> map().
get_chaos_report() ->
    gen_server:call(?MODULE, get_chaos_report).

%% @doc Run experiment in dry-run mode (observe without damage)
-spec dry_run(experiment_config()) -> {ok, map()}.
dry_run(Config) ->
    gen_server:call(?MODULE, {dry_run, Config}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    ?LOG_INFO("Starting chaos engineering framework", []),
    
    % Trap exits to cleanup experiments
    process_flag(trap_exit, true),
    
    SafetyEnabled = proplists:get_value(safety_enabled, Opts, true),
    MonitorIntegration = proplists:get_value(monitor_integration, Opts, true),
    
    GlobalLimits = #{
        max_concurrent_experiments => 5,
        max_global_blast_radius => 0.5,  % 50% max across all experiments
        min_healthy_components => 0.7     % 70% must stay healthy
    },
    
    % Start safety check timer
    erlang:send_after(?SAFETY_CHECK_INTERVAL, self(), safety_check),
    
    {ok, #state{
        safety_enabled = SafetyEnabled,
        monitor_integration = MonitorIntegration,
        global_limits = GlobalLimits
    }}.

handle_call({run_experiment, ExperimentId, Config}, _From, State) ->
    case validate_experiment_config(Config) of
        ok ->
            case check_safety_constraints(Config, State) of
                ok ->
                    case start_experiment(ExperimentId, Config, State) of
                        {ok, NewState} ->
                            {reply, {ok, ExperimentId}, NewState};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                {error, Reason} ->
                    ?LOG_WARNING("Safety check failed for experiment ~p: ~p", 
                                [ExperimentId, Reason]),
                    {reply, {error, {safety_violation, Reason}}, State}
            end;
        {error, Reason} ->
            {reply, {error, {invalid_config, Reason}}, State}
    end;

handle_call({stop_experiment, ExperimentId}, _From, State) ->
    case maps:find(ExperimentId, State#state.experiments) of
        {ok, Experiment} ->
            NewState = stop_experiment_internal(Experiment, State),
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(stop_all_experiments, _From, State) ->
    NewState = stop_all_experiments_internal(State),
    {reply, ok, NewState};

handle_call(get_active_experiments, _From, State) ->
    ActiveExperiments = get_active_experiments_internal(State),
    {reply, ActiveExperiments, State};

handle_call({get_experiment_status, ExperimentId}, _From, State) ->
    case maps:find(ExperimentId, State#state.experiments) of
        {ok, Experiment} ->
            Status = experiment_to_status(Experiment),
            {reply, {ok, Status}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_chaos_report, _From, State) ->
    Report = generate_chaos_report(State),
    {reply, Report, State};

handle_call({dry_run, Config}, _From, State) ->
    Result = perform_dry_run(Config),
    {reply, {ok, Result}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({experiment_complete, ExperimentId}, State) ->
    NewState = handle_experiment_complete(ExperimentId, State),
    {noreply, NewState};

handle_info({experiment_failed, ExperimentId, Reason}, State) ->
    NewState = handle_experiment_failed(ExperimentId, Reason, State),
    {noreply, NewState};

handle_info({experiment_incident, ExperimentId, Incident}, State) ->
    NewState = record_incident(ExperimentId, Incident, State),
    {noreply, NewState};

handle_info(safety_check, State) ->
    NewState = perform_safety_checks(State),
    erlang:send_after(?SAFETY_CHECK_INTERVAL, self(), safety_check),
    {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    NewState = handle_worker_down(Pid, Reason, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?LOG_INFO("Chaos engineering framework terminating", []),
    % Stop all running experiments
    _ = stop_all_experiments_internal(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec validate_experiment_config(experiment_config()) -> ok | {error, term()}.
validate_experiment_config(Config) ->
    case maps:find(experiment, Config) of
        {ok, Type} when is_atom(Type) ->
            % Validate type-specific requirements
            validate_experiment_type_config(Type, Config);
        {ok, _} ->
            {error, invalid_experiment_type};
        error ->
            {error, missing_experiment_type}
    end.

-spec validate_experiment_type_config(experiment_type(), map()) -> ok | {error, term()}.
validate_experiment_type_config(network_latency, Config) ->
    case maps:find(latency, Config) of
        {ok, Latency} when is_integer(Latency), Latency > 0 -> ok;
        {ok, _} -> {error, invalid_latency};
        error -> {error, missing_latency}
    end;
validate_experiment_type_config(network_partition, _Config) ->
    ok;
validate_experiment_type_config(packet_loss, Config) ->
    case maps:find(rate, Config) of
        {ok, Rate} when is_float(Rate), Rate >= 0.0, Rate =< 1.0 -> ok;
        {ok, _} -> {error, invalid_rate};
        error -> {error, missing_rate}
    end;
validate_experiment_type_config(kill_servers, Config) ->
    case maps:find(target, Config) of
        {ok, Target} when is_atom(Target) -> ok;
        {ok, _} -> {error, invalid_target};
        error -> {error, missing_target}
    end;
validate_experiment_type_config(_, _Config) ->
    ok.

-spec check_safety_constraints(experiment_config(), #state{}) -> ok | {error, term()}.
check_safety_constraints(Config, State) ->
    case State#state.safety_enabled of
        false ->
            ok;
        true ->
            MaxBlastRadius = maps:get(max_blast_radius, Config, ?DEFAULT_MAX_BLAST_RADIUS),
            
            % Check against global limits
            GlobalLimits = State#state.global_limits,
            MaxGlobalBlastRadius = maps:get(max_global_blast_radius, GlobalLimits),
            
            CurrentBlastRadius = calculate_current_blast_radius(State),
            ProjectedBlastRadius = CurrentBlastRadius + MaxBlastRadius,
            
            if
                ProjectedBlastRadius > MaxGlobalBlastRadius ->
                    {error, {blast_radius_exceeded, ProjectedBlastRadius}};
                true ->
                    % Check concurrent experiments limit
                    MaxConcurrent = maps:get(max_concurrent_experiments, GlobalLimits),
                    ActiveCount = length(get_active_experiments_internal(State)),
                    if
                        ActiveCount >= MaxConcurrent ->
                            {error, too_many_concurrent_experiments};
                        true ->
                            % Check system health if monitoring is enabled
                            check_system_health_constraints(State)
                    end
            end
    end.

-spec check_system_health_constraints(#state{}) -> ok | {error, term()}.
check_system_health_constraints(#state{monitor_integration = false}) ->
    ok;
check_system_health_constraints(State) ->
    try
        SystemHealth = erlmcp_health_monitor:get_system_health(),
        OverallStatus = maps:get(overall_status, SystemHealth, unknown),
        
        case OverallStatus of
            unhealthy ->
                {error, system_unhealthy};
            _ ->
                % Check minimum healthy components
                ComponentHealth = maps:get(component_health, SystemHealth, #{}),
                HealthyPercentage = maps:get(healthy_percentage, ComponentHealth, 0.0),
                MinHealthy = maps:get(min_healthy_components, State#state.global_limits),
                
                if
                    HealthyPercentage < MinHealthy ->
                        {error, {insufficient_healthy_components, HealthyPercentage}};
                    true ->
                        ok
                end
        end
    catch
        _:_ ->
            % Health monitor not available
            ok
    end.

-spec start_experiment(experiment_id(), experiment_config(), #state{}) -> 
    {ok, #state{}} | {error, term()}.
start_experiment(ExperimentId, Config, State) ->
    ExperimentType = maps:get(experiment, Config),
    Duration = maps:get(duration, Config, ?DEFAULT_DURATION),
    
    % Create experiment record
    Experiment = #experiment{
        id = ExperimentId,
        type = ExperimentType,
        config = Config,
        state = running,
        start_time = erlang:timestamp()
    },
    
    % Start experiment worker
    Parent = self(),
    WorkerPid = spawn_link(fun() ->
        run_experiment_worker(Parent, ExperimentId, ExperimentType, Config)
    end),
    
    % Monitor worker
    erlang:monitor(process, WorkerPid),
    
    % Set completion timer
    TimerRef = erlang:send_after(Duration, self(), {experiment_complete, ExperimentId}),
    
    UpdatedExperiment = Experiment#experiment{
        worker_pid = WorkerPid,
        timer_ref = TimerRef
    },
    
    NewExperiments = maps:put(ExperimentId, UpdatedExperiment, State#state.experiments),
    
    ?LOG_INFO("Started chaos experiment ~p: ~p", [ExperimentId, ExperimentType]),
    
    {ok, State#state{experiments = NewExperiments}}.

-spec run_experiment_worker(pid(), experiment_id(), experiment_type(), map()) -> ok.
run_experiment_worker(Parent, ExperimentId, Type, Config) ->
    try
        case Type of
            network_latency ->
                erlmcp_chaos_network:inject_latency(Config);
            network_partition ->
                erlmcp_chaos_network:inject_partition(Config);
            packet_loss ->
                erlmcp_chaos_network:inject_packet_loss(Config);
            kill_servers ->
                erlmcp_chaos_process:kill_servers(Config);
            kill_random ->
                erlmcp_chaos_process:kill_random(Config);
            resource_memory ->
                erlmcp_chaos_resource:exhaust_memory(Config);
            resource_cpu ->
                erlmcp_chaos_resource:saturate_cpu(Config);
            resource_disk ->
                erlmcp_chaos_resource:fill_disk(Config);
            clock_skew ->
                erlmcp_chaos_process:inject_clock_skew(Config)
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Experiment ~p failed: ~p:~p~n~p", 
                      [ExperimentId, Class, Reason, Stacktrace]),
            Parent ! {experiment_failed, ExperimentId, {Class, Reason}}
    end.

-spec stop_experiment_internal(#experiment{}, #state{}) -> #state{}.
stop_experiment_internal(Experiment, State) ->
    #experiment{id = ExperimentId, worker_pid = WorkerPid, timer_ref = TimerRef} = Experiment,
    
    % Cancel timer
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,
    
    % Stop worker
    case WorkerPid of
        undefined -> ok;
        _ -> exit(WorkerPid, shutdown)
    end,
    
    % Update experiment state
    UpdatedExperiment = Experiment#experiment{
        state = stopped,
        end_time = erlang:timestamp()
    },
    
    NewExperiments = maps:put(ExperimentId, UpdatedExperiment, State#state.experiments),
    
    ?LOG_INFO("Stopped chaos experiment ~p", [ExperimentId]),
    
    State#state{experiments = NewExperiments}.

-spec stop_all_experiments_internal(#state{}) -> #state{}.
stop_all_experiments_internal(State) ->
    ActiveExperiments = maps:filter(
        fun(_Id, Exp) -> Exp#experiment.state =:= running end,
        State#state.experiments
    ),
    
    lists:foldl(
        fun({_Id, Experiment}, AccState) ->
            stop_experiment_internal(Experiment, AccState)
        end,
        State,
        maps:to_list(ActiveExperiments)
    ).

-spec get_active_experiments_internal(#state{}) -> [experiment_status()].
get_active_experiments_internal(State) ->
    ActiveExperiments = maps:filter(
        fun(_Id, Exp) -> Exp#experiment.state =:= running end,
        State#state.experiments
    ),
    [experiment_to_status(Exp) || Exp <- maps:values(ActiveExperiments)].

-spec experiment_to_status(#experiment{}) -> experiment_status().
experiment_to_status(Experiment) ->
    BlastRadius = case Experiment#experiment.total_targets of
        0 -> 0.0;
        Total -> Experiment#experiment.targets_affected / Total
    end,
    
    Status = #{
        id => Experiment#experiment.id,
        type => Experiment#experiment.type,
        state => Experiment#experiment.state,
        start_time => Experiment#experiment.start_time,
        targets_affected => Experiment#experiment.targets_affected,
        total_targets => Experiment#experiment.total_targets,
        blast_radius => BlastRadius,
        incidents => Experiment#experiment.incidents,
        metrics => Experiment#experiment.metrics
    },
    
    case Experiment#experiment.end_time of
        undefined -> Status;
        EndTime -> Status#{end_time => EndTime}
    end.

-spec handle_experiment_complete(experiment_id(), #state{}) -> #state{}.
handle_experiment_complete(ExperimentId, State) ->
    case maps:find(ExperimentId, State#state.experiments) of
        {ok, Experiment} ->
            UpdatedExperiment = Experiment#experiment{
                state = completed,
                end_time = erlang:timestamp()
            },
            NewExperiments = maps:put(ExperimentId, UpdatedExperiment, State#state.experiments),
            ?LOG_INFO("Chaos experiment ~p completed", [ExperimentId]),
            State#state{experiments = NewExperiments};
        error ->
            State
    end.

-spec handle_experiment_failed(experiment_id(), term(), #state{}) -> #state{}.
handle_experiment_failed(ExperimentId, Reason, State) ->
    case maps:find(ExperimentId, State#state.experiments) of
        {ok, Experiment} ->
            UpdatedExperiment = Experiment#experiment{
                state = failed,
                end_time = erlang:timestamp(),
                incidents = [{failure, Reason, erlang:timestamp()} | Experiment#experiment.incidents]
            },
            NewExperiments = maps:put(ExperimentId, UpdatedExperiment, State#state.experiments),
            ?LOG_ERROR("Chaos experiment ~p failed: ~p", [ExperimentId, Reason]),
            State#state{experiments = NewExperiments};
        error ->
            State
    end.

-spec record_incident(experiment_id(), term(), #state{}) -> #state{}.
record_incident(ExperimentId, Incident, State) ->
    case maps:find(ExperimentId, State#state.experiments) of
        {ok, Experiment} ->
            UpdatedExperiment = Experiment#experiment{
                incidents = [{incident, Incident, erlang:timestamp()} | Experiment#experiment.incidents]
            },
            NewExperiments = maps:put(ExperimentId, UpdatedExperiment, State#state.experiments),
            State#state{experiments = NewExperiments};
        error ->
            State
    end.

-spec perform_safety_checks(#state{}) -> #state{}.
perform_safety_checks(State) ->
    case State#state.safety_enabled of
        false ->
            State;
        true ->
            % Check current blast radius
            CurrentBlastRadius = calculate_current_blast_radius(State),
            MaxBlastRadius = maps:get(max_global_blast_radius, State#state.global_limits),
            
            case CurrentBlastRadius > MaxBlastRadius of
                true ->
                    ?LOG_WARNING("Blast radius exceeded: ~.2f > ~.2f, stopping all experiments",
                               [CurrentBlastRadius, MaxBlastRadius]),
                    stop_all_experiments_internal(State);
                false ->
                    % Check system health
                    case check_system_health_constraints(State) of
                        ok ->
                            State;
                        {error, Reason} ->
                            ?LOG_WARNING("System health check failed: ~p, stopping all experiments",
                                       [Reason]),
                            stop_all_experiments_internal(State)
                    end
            end
    end.

-spec calculate_current_blast_radius(#state{}) -> float().
calculate_current_blast_radius(State) ->
    ActiveExperiments = get_active_experiments_internal(State),
    lists:sum([maps:get(blast_radius, Status, 0.0) || Status <- ActiveExperiments]).

-spec handle_worker_down(pid(), term(), #state{}) -> #state{}.
handle_worker_down(Pid, Reason, State) ->
    case find_experiment_by_worker_pid(Pid, State#state.experiments) of
        {ok, ExperimentId} ->
            handle_experiment_failed(ExperimentId, {worker_down, Reason}, State);
        error ->
            State
    end.

-spec find_experiment_by_worker_pid(pid(), map()) -> {ok, experiment_id()} | error.
find_experiment_by_worker_pid(Pid, Experiments) ->
    case maps:to_list(maps:filter(
        fun(_Id, Exp) -> Exp#experiment.worker_pid =:= Pid end,
        Experiments
    )) of
        [{Id, _Exp}] -> {ok, Id};
        [] -> error
    end.

-spec generate_chaos_report(#state{}) -> map().
generate_chaos_report(State) ->
    AllExperiments = maps:values(State#state.experiments),
    ActiveExperiments = [E || E <- AllExperiments, E#experiment.state =:= running],
    CompletedExperiments = [E || E <- AllExperiments, E#experiment.state =:= completed],
    FailedExperiments = [E || E <- AllExperiments, E#experiment.state =:= failed],
    
    TotalIncidents = lists:sum([length(E#experiment.incidents) || E <- AllExperiments]),
    CurrentBlastRadius = calculate_current_blast_radius(State),
    
    #{
        timestamp => erlang:timestamp(),
        total_experiments => length(AllExperiments),
        active_experiments => length(ActiveExperiments),
        completed_experiments => length(CompletedExperiments),
        failed_experiments => length(FailedExperiments),
        total_incidents => TotalIncidents,
        current_blast_radius => CurrentBlastRadius,
        safety_enabled => State#state.safety_enabled,
        monitor_integration => State#state.monitor_integration,
        experiments => [experiment_to_status(E) || E <- AllExperiments]
    }.

-spec perform_dry_run(experiment_config()) -> map().
perform_dry_run(Config) ->
    ExperimentType = maps:get(experiment, Config),
    Target = maps:get(target, Config, undefined),
    Rate = maps:get(rate, Config, 0.1),
    
    % Simulate experiment without actual damage
    #{
        experiment_type => ExperimentType,
        target => Target,
        estimated_affected => calculate_estimated_affected(Target, Rate),
        estimated_blast_radius => Rate,
        safety_checks => perform_safety_check_simulation(Config),
        estimated_duration => maps:get(duration, Config, ?DEFAULT_DURATION),
        risks => identify_risks(ExperimentType, Config),
        recommendations => generate_recommendations(ExperimentType, Config)
    }.

-spec calculate_estimated_affected(term(), float()) -> non_neg_integer().
calculate_estimated_affected(undefined, _Rate) ->
    0;
calculate_estimated_affected(Target, Rate) when is_atom(Target) ->
    % Count processes of this type
    Processes = erlang:processes(),
    TargetProcesses = lists:filter(
        fun(Pid) ->
            case erlang:process_info(Pid, registered_name) of
                {registered_name, Name} ->
                    NameStr = atom_to_list(Name),
                    TargetStr = atom_to_list(Target),
                    string:prefix(NameStr, TargetStr) =/= nomatch;
                _ ->
                    false
            end
        end,
        Processes
    ),
    round(length(TargetProcesses) * Rate);
calculate_estimated_affected(_Target, _Rate) ->
    0.

-spec perform_safety_check_simulation(map()) -> map().
perform_safety_check_simulation(Config) ->
    Rate = maps:get(rate, Config, 0.1),
    MaxBlastRadius = maps:get(max_blast_radius, Config, ?DEFAULT_MAX_BLAST_RADIUS),
    
    #{
        blast_radius_check => Rate =< MaxBlastRadius,
        max_blast_radius => MaxBlastRadius,
        actual_rate => Rate,
        passes_safety_checks => Rate =< MaxBlastRadius
    }.

-spec identify_risks(experiment_type(), map()) -> [binary()].
identify_risks(network_partition, _Config) ->
    [<<"May cause split-brain scenarios">>,
     <<"Distributed state may diverge">>,
     <<"Message loss possible">>];
identify_risks(kill_servers, Config) ->
    Rate = maps:get(rate, Config, 0.1),
    if
        Rate > 0.3 ->
            [<<"High kill rate may overwhelm supervisors">>,
             <<"Cascading failures possible">>,
             <<"Service degradation likely">>];
        true ->
            [<<"Individual process failures">>,
             <<"Should be handled by supervisors">>]
    end;
identify_risks(resource_memory, _Config) ->
    [<<"May trigger OOM killer">>,
     <<"System-wide impact possible">>,
     <<"Automatic rollback critical">>];
identify_risks(resource_cpu, _Config) ->
    [<<"Scheduler saturation">>,
     <<"Response time degradation">>,
     <<"Timeout cascades possible">>];
identify_risks(_, _Config) ->
    [<<"Monitor system health during experiment">>,
     <<"Have rollback plan ready">>].

-spec generate_recommendations(experiment_type(), map()) -> [binary()].
generate_recommendations(Type, Config) ->
    BaseRecommendations = [
        <<"Start with low rate and increase gradually">>,
        <<"Monitor system health continuously">>,
        <<"Have manual rollback procedure ready">>
    ],
    
    TypeRecommendations = case Type of
        network_latency ->
            [<<"Start with 50-100ms latency">>,
             <<"Monitor request timeout rates">>];
        kill_servers ->
            Rate = maps:get(rate, Config, 0.1),
            if
                Rate > 0.2 ->
                    [<<"Consider reducing kill rate below 20%">>,
                     <<"Verify supervisor restart strategies">>];
                true ->
                    [<<"Verify supervisor restart limits">>]
            end;
        resource_memory ->
            [<<"Enable automatic rollback">>,
             <<"Set conservative blast radius limit">>,
             <<"Monitor swap usage">>];
        _ ->
            []
    end,
    
    BaseRecommendations ++ TypeRecommendations.

-spec generate_experiment_id() -> binary().
generate_experiment_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("chaos_~p_~p", [Timestamp, Random])).

