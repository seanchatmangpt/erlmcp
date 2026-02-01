-module(erlmcp_recovery_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/0, start_link/1,
    register_component/3, unregister_component/1,
    trigger_recovery/2, trigger_recovery/3,
    get_recovery_status/1, get_all_recovery_status/0,
    reset_metrics/0, get_metrics/0,
    get_circuit_status/0, get_circuit_status/1,
    set_recovery_policy/2, get_recovery_policy/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type component_id() :: atom().
-type recovery_strategy() :: restart | circuit_breaker | graceful_degradation.
-type circuit_state() :: closed | open | half_open.
-type recovery_policy() :: #{
    strategy := recovery_strategy(),
    max_failures := pos_integer(),
    recovery_timeout := pos_integer(),
    backoff_strategy := exponential | linear | constant,
    initial_backoff := pos_integer(),
    max_backoff := pos_integer()
}.

-record(component, {
    id :: component_id(),
    pid :: pid(),
    policy :: recovery_policy(),
    failures = 0 :: non_neg_integer(),
    last_failure :: undefined | erlang:timestamp(),
    circuit_state = closed :: circuit_state(),
    recovery_attempts = 0 :: non_neg_integer(),
    recovery_start :: undefined | erlang:timestamp(),
    total_recoveries = 0 :: non_neg_integer()
}).

-record(state, {
    components = #{} :: #{component_id() => #component{}},
    default_policy :: recovery_policy(),
    metrics = #{} :: #{atom() => term()},
    recovery_timer :: undefined | timer:tref(),
    restart_cooldowns = #{} :: #{component_id() => erlang:timestamp()}
}).

-define(DEFAULT_POLICY, #{
    strategy => restart,
    max_failures => 5,
    recovery_timeout => 30000, % 30 seconds
    backoff_strategy => exponential,
    initial_backoff => 1000,   % 1 second
    max_backoff => 30000       % 30 seconds
}).

-define(RECOVERY_CHECK_INTERVAL, 5000). % 5 seconds
-define(RESTART_COOLDOWN, 10000). % 10 seconds cooldown between restarts

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec register_component(component_id(), pid(), recovery_policy()) -> ok | {error, term()}.
register_component(ComponentId, Pid, Policy) ->
    gen_server:call(?MODULE, {register_component, ComponentId, Pid, Policy}).

-spec unregister_component(component_id()) -> ok.
unregister_component(ComponentId) ->
    gen_server:cast(?MODULE, {unregister_component, ComponentId}).

-spec trigger_recovery(component_id(), term()) -> ok | {error, term()}.
trigger_recovery(ComponentId, Reason) ->
    trigger_recovery(ComponentId, Reason, #{}).

-spec trigger_recovery(component_id(), term(), map()) -> ok | {error, term()}.
trigger_recovery(ComponentId, Reason, Options) ->
    gen_server:cast(?MODULE, {trigger_recovery, ComponentId, Reason, Options}).

-spec get_recovery_status(component_id()) -> {ok, map()} | {error, not_found}.
get_recovery_status(ComponentId) ->
    gen_server:call(?MODULE, {get_recovery_status, ComponentId}).

-spec get_all_recovery_status() -> map().
get_all_recovery_status() ->
    gen_server:call(?MODULE, get_all_recovery_status).

-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:cast(?MODULE, reset_metrics).

-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

-spec get_circuit_status() -> map().
get_circuit_status() ->
    gen_server:call(?MODULE, get_circuit_status).

-spec get_circuit_status(component_id()) -> circuit_state() | not_found.
get_circuit_status(ComponentId) ->
    gen_server:call(?MODULE, {get_circuit_status, ComponentId}).

-spec set_recovery_policy(component_id(), recovery_policy()) -> ok | {error, term()}.
set_recovery_policy(ComponentId, Policy) ->
    gen_server:call(?MODULE, {set_recovery_policy, ComponentId, Policy}).

-spec get_recovery_policy(component_id()) -> {ok, recovery_policy()} | {error, not_found}.
get_recovery_policy(ComponentId) ->
    gen_server:call(?MODULE, {get_recovery_policy, ComponentId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    ?LOG_INFO("Starting recovery manager with options: ~p", [Opts]),
    
    % Set up process monitoring
    process_flag(trap_exit, true),
    
    DefaultPolicy = maps:merge(?DEFAULT_POLICY, proplists:get_value(default_policy, Opts, #{})),
    
    % Start periodic recovery checks
    {ok, Timer} = timer:send_interval(?RECOVERY_CHECK_INTERVAL, check_recoveries),
    
    State = #state{
        default_policy = DefaultPolicy,
        recovery_timer = Timer,
        metrics = #{
            total_components => 0,
            total_failures => 0,
            total_recoveries => 0,
            successful_recoveries => 0,
            failed_recoveries => 0,
            average_recovery_time => 0.0
        }
    },
    
    ?LOG_INFO("Recovery manager initialized"),
    {ok, State}.

handle_call({register_component, ComponentId, Pid, Policy}, _From, State) ->
    ?LOG_INFO("Registering component ~p with PID ~p", [ComponentId, Pid]),
    
    % Monitor the component process
    erlang:monitor(process, Pid),
    
    MergedPolicy = maps:merge(State#state.default_policy, Policy),
    Component = #component{
        id = ComponentId,
        pid = Pid,
        policy = MergedPolicy
    },
    
    NewComponents = maps:put(ComponentId, Component, State#state.components),
    NewMetrics = update_metric(total_components, 1, State#state.metrics),
    
    NewState = State#state{
        components = NewComponents,
        metrics = NewMetrics
    },
    
    {reply, ok, NewState};

handle_call({get_recovery_status, ComponentId}, _From, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            Status = #{
                id => Component#component.id,
                pid => Component#component.pid,
                failures => Component#component.failures,
                circuit_state => Component#component.circuit_state,
                recovery_attempts => Component#component.recovery_attempts,
                total_recoveries => Component#component.total_recoveries,
                last_failure => Component#component.last_failure,
                policy => Component#component.policy
            },
            {reply, {ok, Status}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_all_recovery_status, _From, State) ->
    AllStatus = maps:map(fun(_Id, Component) ->
        #{
            id => Component#component.id,
            pid => Component#component.pid,
            failures => Component#component.failures,
            circuit_state => Component#component.circuit_state,
            recovery_attempts => Component#component.recovery_attempts,
            total_recoveries => Component#component.total_recoveries,
            last_failure => Component#component.last_failure
        }
    end, State#state.components),
    {reply, AllStatus, State};

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};

handle_call(get_circuit_status, _From, State) ->
    CircuitStatus = maps:map(fun(_Id, Component) ->
        Component#component.circuit_state
    end, State#state.components),
    {reply, CircuitStatus, State};

handle_call({get_circuit_status, ComponentId}, _From, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            {reply, Component#component.circuit_state, State};
        error ->
            {reply, not_found, State}
    end;

handle_call({set_recovery_policy, ComponentId, Policy}, _From, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            MergedPolicy = maps:merge(State#state.default_policy, Policy),
            UpdatedComponent = Component#component{policy = MergedPolicy},
            NewComponents = maps:put(ComponentId, UpdatedComponent, State#state.components),
            NewState = State#state{components = NewComponents},
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_recovery_policy, ComponentId}, _From, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            {reply, {ok, Component#component.policy}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({restart_with_cooldown, SupPid, ComponentId}, _From, State) ->
    case maps:find(ComponentId, State#state.restart_cooldowns) of
        {ok, LastRestart} ->
            TimeSinceLastRestart = timer:now_diff(erlang:timestamp(), LastRestart) div 1000,
            case TimeSinceLastRestart < ?RESTART_COOLDOWN of
                true ->
                    RemainingTime = ?RESTART_COOLDOWN - TimeSinceLastRestart,
                    ?LOG_WARNING("Component ~p is in cooldown, ~p ms remaining",
                                [ComponentId, RemainingTime]),
                    {reply, {error, {cooldown, RemainingTime}}, State};
                false ->
                    % Cooldown expired, proceed with restart
                    case do_restart_component(SupPid, ComponentId) of
                        {ok, NewPid} ->
                            NewCooldowns = maps:put(ComponentId, erlang:timestamp(), State#state.restart_cooldowns),
                            NewMetrics = update_metric(total_recoveries, 1, State#state.metrics),
                            NewMetrics2 = update_metric(successful_recoveries, 1, NewMetrics),
                            {reply, {ok, NewPid}, State#state{restart_cooldowns = NewCooldowns, metrics = NewMetrics2}};
                        {error, Reason} ->
                            NewMetrics = update_metric(failed_recoveries, 1, State#state.metrics),
                            {reply, {error, Reason}, State#state{metrics = NewMetrics}}
                    end
            end;
        error ->
            % No previous restart, proceed
            case do_restart_component(SupPid, ComponentId) of
                {ok, NewPid} ->
                    NewCooldowns = maps:put(ComponentId, erlang:timestamp(), State#state.restart_cooldowns),
                    NewMetrics = update_metric(total_recoveries, 1, State#state.metrics),
                    NewMetrics2 = update_metric(successful_recoveries, 1, NewMetrics),
                    {reply, {ok, NewPid}, State#state{restart_cooldowns = NewCooldowns, metrics = NewMetrics2}};
                {error, Reason} ->
                    NewMetrics = update_metric(failed_recoveries, 1, State#state.metrics),
                    {reply, {error, Reason}, State#state{metrics = NewMetrics}}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({unregister_component, ComponentId}, State) ->
    ?LOG_INFO("Unregistering component ~p", [ComponentId]),
    NewComponents = maps:remove(ComponentId, State#state.components),
    NewMetrics = update_metric(total_components, -1, State#state.metrics),
    NewState = State#state{
        components = NewComponents,
        metrics = NewMetrics
    },
    {noreply, NewState};

handle_cast({trigger_recovery, ComponentId, Reason, Options}, State) ->
    ?LOG_WARNING("Recovery triggered for component ~p: ~p", [ComponentId, Reason]),
    NewState = handle_component_failure(ComponentId, Reason, Options, State),
    {noreply, NewState};

handle_cast(reset_metrics, State) ->
    ?LOG_INFO("Resetting recovery metrics"),
    ResetMetrics = #{
        total_components => maps:size(State#state.components),
        total_failures => 0,
        total_recoveries => 0,
        successful_recoveries => 0,
        failed_recoveries => 0,
        average_recovery_time => 0.0
    },
    NewState = State#state{metrics = ResetMetrics},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    ?LOG_WARNING("Monitored process ~p died with reason: ~p", [Pid, Reason]),
    case find_component_by_pid(Pid, State#state.components) of
        {ok, ComponentId} ->
            NewState = handle_component_failure(ComponentId, Reason, #{}, State),
            {noreply, NewState};
        error ->
            ?LOG_WARNING("Unknown process ~p died", [Pid]),
            {noreply, State}
    end;

handle_info(check_recoveries, State) ->
    NewState = check_and_update_recoveries(State),
    {noreply, NewState};

handle_info(Info, State) ->
    ?LOG_DEBUG("Unexpected info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    ?LOG_INFO("Recovery manager terminating: ~p", [Reason]),
    
    % Cancel recovery timer
    case State#state.recovery_timer of
        undefined -> ok;
        Timer -> timer:cancel(Timer)
    end,
    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec handle_component_failure(component_id(), term(), map(), #state{}) -> #state{}.
handle_component_failure(ComponentId, Reason, Options, State) ->
    case maps:find(ComponentId, State#state.components) of
        {ok, Component} ->
            ?LOG_INFO("Handling failure for component ~p: ~p", [ComponentId, Reason]),
            
            % Update failure count and metrics
            NewFailures = Component#component.failures + 1,
            NewMetrics = update_metric(total_failures, 1, State#state.metrics),
            
            UpdatedComponent = Component#component{
                failures = NewFailures,
                last_failure = erlang:timestamp()
            },
            
            % Determine recovery action based on policy
            Policy = Component#component.policy,
            {NewComponent, Action} = determine_recovery_action(UpdatedComponent, Policy, Options),
            
            % Execute recovery action
            FinalComponent = execute_recovery_action(Action, NewComponent, Reason),
            
            NewComponents = maps:put(ComponentId, FinalComponent, State#state.components),
            
            State#state{
                components = NewComponents,
                metrics = NewMetrics
            };
        error ->
            ?LOG_WARNING("Failure reported for unknown component ~p", [ComponentId]),
            State
    end.

-spec determine_recovery_action(#component{}, recovery_policy(), map()) -> 
    {#component{}, {restart | circuit_break | degrade, map()}}.
determine_recovery_action(Component, Policy, Options) ->
    MaxFailures = maps:get(max_failures, Policy, 5),
    Strategy = maps:get(strategy, Policy, restart),
    
    case Component#component.failures >= MaxFailures of
        true ->
            % Too many failures, activate circuit breaker or degradation
            case Strategy of
                circuit_breaker ->
                    NewComponent = Component#component{circuit_state = open},
                    {NewComponent, {circuit_break, Options}};
                graceful_degradation ->
                    {Component, {degrade, Options}};
                restart ->
                    % Even with restart strategy, use circuit breaker after too many failures
                    NewComponent = Component#component{circuit_state = open},
                    {NewComponent, {circuit_break, Options}}
            end;
        false ->
            % Within failure threshold, attempt restart
            case Component#component.circuit_state of
                open ->
                    % Circuit is open, check if we should try half-open
                    case should_try_half_open(Component, Policy) of
                        true ->
                            NewComponent = Component#component{circuit_state = half_open},
                            {NewComponent, {restart, Options#{circuit_state => half_open}}};
                        false ->
                            {Component, {circuit_break, Options}}
                    end;
                _ ->
                    {Component, {restart, Options}}
            end
    end.

-spec should_try_half_open(#component{}, recovery_policy()) -> boolean().
should_try_half_open(Component, Policy) ->
    RecoveryTimeout = maps:get(recovery_timeout, Policy, 30000),
    case Component#component.last_failure of
        undefined -> false;
        LastFailure ->
            TimeSinceFailure = timer:now_diff(erlang:timestamp(), LastFailure) div 1000,
            TimeSinceFailure >= RecoveryTimeout
    end.

-spec execute_recovery_action({restart | circuit_break | degrade, map()}, #component{}, term()) -> 
    #component{}.
execute_recovery_action({restart, Options}, Component, Reason) ->
    ?LOG_INFO("Attempting to restart component ~p", [Component#component.id]),
    
    RecoveryStart = erlang:timestamp(),
    case attempt_component_restart(Component, Reason, Options) of
        {ok, NewPid} ->
            ?LOG_INFO("Successfully restarted component ~p with new PID ~p", 
                     [Component#component.id, NewPid]),
            
            % Monitor the new process
            erlang:monitor(process, NewPid),
            
            % Update circuit state based on recovery success
            NewCircuitState = case maps:get(circuit_state, Options, closed) of
                half_open -> closed; % Successful recovery from half-open closes circuit
                _ -> Component#component.circuit_state
            end,
            
            Component#component{
                pid = NewPid,
                recovery_attempts = Component#component.recovery_attempts + 1,
                total_recoveries = Component#component.total_recoveries + 1,
                recovery_start = RecoveryStart,
                circuit_state = NewCircuitState,
                failures = 0 % Reset failure count on successful recovery
            };
        {error, RecoveryReason} ->
            ?LOG_ERROR("Failed to restart component ~p: ~p", 
                      [Component#component.id, RecoveryReason]),
            
            Component#component{
                recovery_attempts = Component#component.recovery_attempts + 1,
                recovery_start = RecoveryStart,
                circuit_state = open % Failed recovery opens circuit
            }
    end;

execute_recovery_action({circuit_break, _Options}, Component, _Reason) ->
    ?LOG_INFO("Circuit breaker activated for component ~p", [Component#component.id]),
    
    % Notify health monitor about circuit breaker activation
    erlmcp_health_monitor:report_circuit_breaker(Component#component.id, open),
    
    Component#component{circuit_state = open};

execute_recovery_action({degrade, _Options}, Component, _Reason) ->
    ?LOG_INFO("Activating graceful degradation for component ~p", [Component#component.id]),
    
    % Notify health monitor about degradation
    erlmcp_health_monitor:report_degradation(Component#component.id),
    
    Component.

-spec attempt_component_restart(#component{}, term(), map()) -> 
    {ok, pid()} | {error, term()}.
attempt_component_restart(Component, Reason, Options) ->
    ComponentId = Component#component.id,
    Policy = Component#component.policy,
    
    % Calculate backoff delay
    BackoffDelay = calculate_backoff(Component#component.recovery_attempts, Policy),
    
    % Wait for backoff period
    timer:sleep(BackoffDelay),
    
    % Attempt to restart based on component type
    case restart_component(ComponentId, Reason, Options) of
        {ok, NewPid} ->
            {ok, NewPid};
        {error, RestartReason} ->
            ?LOG_ERROR("Component restart failed: ~p", [RestartReason]),
            {error, RestartReason}
    end.

-spec calculate_backoff(non_neg_integer(), recovery_policy()) -> pos_integer().
calculate_backoff(Attempt, Policy) ->
    Strategy = maps:get(backoff_strategy, Policy, exponential),
    InitialBackoff = maps:get(initial_backoff, Policy, 1000),
    MaxBackoff = maps:get(max_backoff, Policy, 30000),
    
    case Strategy of
        exponential ->
            Backoff = InitialBackoff * round(math:pow(2, Attempt)),
            min(Backoff, MaxBackoff);
        linear ->
            Backoff = InitialBackoff + (InitialBackoff * Attempt),
            min(Backoff, MaxBackoff);
        constant ->
            InitialBackoff
    end.

-spec restart_component(component_id(), term(), map()) -> {ok, pid()} | {error, term()}.
restart_component(ComponentId, _Reason, _Options) ->
    % This is a generic restart function that should be customized based on component type
    % In practice, this would call appropriate supervisor or start functions
    
    case ComponentId of
        % Handle specific component types
        % Note: Registry now uses gproc (external library, no restart needed)
        erlmcp_server ->
            restart_server(ComponentId);
        erlmcp_transport ->
            restart_transport(ComponentId);
        _ ->
            % Generic restart attempt
            restart_generic_component(ComponentId)
    end.

restart_server(ServerId) ->
    case erlmcp_sup:start_server(ServerId, #{}) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

restart_transport(TransportId) ->
    case erlmcp_sup:start_transport(TransportId, stdio, #{}) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Restart a generic component via its supervisor
%% Implements Joe Armstrong's supervision tree principle:
%% "Let it crash" means supervisors restart it
-spec restart_generic_component(component_id()) -> ok | {error, term()}.
restart_generic_component(ComponentId) ->
    ?LOG_INFO("Generic component restart requested for ~p", [ComponentId]),
    case find_component_supervisor(ComponentId) of
        {ok, Sup} ->
            restart_with_cooldown(Sup, ComponentId);
        {error, Reason} ->
            ?LOG_ERROR("Cannot find supervisor for ~p: ~p", [ComponentId, Reason]),
            {error, Reason}
    end.

%% @doc Find which supervisor owns a component
%% Maps component IDs to their supervisor processes
-spec find_component_supervisor(component_id()) -> {ok, pid()} | {error, term()}.
find_component_supervisor(ComponentId) ->
    % Map component IDs to their supervisors
    % This follows the 3-tier supervision tree architecture
    SupervisorMap = #{
        % Tier 1: Core (erlmcp_core_sup)
        erlmcp_registry => erlmcp_core_sup,
        erlmcp_session_manager => erlmcp_core_sup,

        % Tier 2: Protocol (erlmcp_server_sup - simple_one_for_one)
        % For dynamic server instances, we check if they're registered
        erlmcp_server => erlmcp_server_sup,

        % Tier 3: Observability (erlmcp_observability_sup)
        erlmcp_metrics => erlmcp_observability_sup,
        erlmcp_metrics_server => erlmcp_observability_sup,
        erlmcp_metrics_aggregator => erlmcp_observability_sup,
        erlmcp_dashboard_server => erlmcp_observability_sup,
        erlmcp_health_monitor => erlmcp_observability_sup,
        erlmcp_recovery_manager => erlmcp_observability_sup,
        erlmcp_chaos => erlmcp_observability_sup,
        erlmcp_process_monitor => erlmcp_observability_sup
    },

    case maps:find(ComponentId, SupervisorMap) of
        {ok, SupName} ->
            % Find the supervisor PID
            case whereis(SupName) of
                undefined -> {error, supervisor_not_running};
                SupPid when is_pid(SupPid) -> {ok, SupPid}
            end;
        error ->
            % Not in static map - try to find in dynamic supervisors
            find_component_in_dynamic_supervisors(ComponentId)
    end.

%% @doc Find component in dynamic supervisors (simple_one_for_one)
-spec find_component_in_dynamic_supervisors(component_id()) -> {ok, pid()} | {error, term()}.
find_component_in_dynamic_supervisors(ComponentId) ->
    % Check erlmcp_server_sup for dynamic server instances
    case whereis(erlmcp_server_sup) of
        undefined ->
            {error, supervisor_not_found};
        SupPid ->
            % For simple_one_for_one, we need to find the child PID
            % and then restart it via the supervisor
            case supervisor:which_children(SupPid) of
                [] ->
                    {error, no_children};
                Children ->
                    % Look for our component in the children list
                    case lists:keyfind(ComponentId, 1, Children) of
                        {_, ChildPid, _, _} when is_pid(ChildPid) ->
                            % Found it - return supervisor for restart
                            {ok, SupPid};
                        {_, undefined, _, _} ->
                            % Child exists but not running - can restart
                            {ok, SupPid};
                        false ->
                            % Try to find by process registry
                            try_find_via_registry(ComponentId)
                    end
            end
    end.

%% @doc Try to find component via gproc registry
-spec try_find_via_registry(component_id()) -> {ok, pid()} | {error, term()}.
try_find_via_registry(ComponentId) ->
    try
        % Check if component is registered via gproc
        case gproc:where({n, l, ComponentId}) of
            undefined ->
                {error, component_not_found};
            Pid when is_pid(Pid) ->
                % Found it - now find its supervisor via supervision tree
                find_supervisor_for_pid(Pid)
        end
    catch
        error:_ ->
            {error, registry_not_available}
    end.

%% @doc Find supervisor for a given PID by walking supervision tree
-spec find_supervisor_for_pid(pid()) -> {ok, pid()} | {error, term()}.
find_supervisor_for_pid(Pid) ->
    case get_parent_pid(Pid) of
        {ok, ParentPid} ->
            case is_supervisor(ParentPid) of
                true -> {ok, ParentPid};
                false -> find_supervisor_for_pid(ParentPid) % Walk up the tree
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get parent PID using supervisor module
-spec get_parent_pid(pid()) -> {ok, pid()} | {error, term()}.
get_parent_pid(Pid) ->
    try
        % Use supervisor:get_childspec/2 to find parent
        % This works if the child is under a supervisor
        case supervisor:which_children(erlmcp_sup) of
            Children ->
                % Check if any of our top-level supervisors match
                find_matching_supervisor(Children, Pid)
        end
    catch
        error:_ ->
            {error, parent_not_found}
    end.

%% @doc Find matching supervisor from children list
-spec find_matching_supervisor([{atom(), pid() | undefined, term(), term()}], pid()) ->
    {ok, pid()} | {error, term()}.
find_matching_supervisor([{SupId, SupPid, supervisor, _} | _Rest], _ChildPid) when is_pid(SupPid) ->
    % Check if this supervisor manages our child
    case is_child_under_supervisor(SupPid, _ChildPid) of
        true -> {ok, SupPid};
        false -> {error, not_under_this_supervisor}
    end;
find_matching_supervisor([_ | Rest], ChildPid) ->
    find_matching_supervisor(Rest, ChildPid);
find_matching_supervisor([], _ChildPid) ->
    {error, supervisor_not_found}.

%% @doc Check if PID is a supervisor
-spec is_supervisor(pid()) -> boolean().
is_supervisor(Pid) ->
    try
        % Check if process is a supervisor by looking at its module
        {dictionary, Dict} = process_info(Pid, dictionary),
        case proplists:get_value("$initial_call", Dict) of
            {Module, _, _} ->
                % Check if module implements supervisor behavior
                Exports = Module:module_info(exports),
                lists:keymember(init, 1, Exports) andalso
                lists:keymember(handle_call, 1, Exports) =:= false;
            _ ->
                false
        end
    catch
        _:_ -> false
    end.

%% @doc Check if child is under supervisor
-spec is_child_under_supervisor(pid(), pid()) -> boolean().
is_child_under_supervisor(SupPid, ChildPid) ->
    try
        Children = supervisor:which_children(SupPid),
        lists:any(fun({_, Pid, _, _}) -> Pid =:= ChildPid end, Children)
    catch
        _:_ -> false
    end.

%% @doc Restart component with cooldown to prevent thrashing
-spec restart_with_cooldown(pid(), component_id()) -> ok | {error, term()}.
restart_with_cooldown(SupPid, ComponentId) ->
    gen_server:call(?MODULE, {restart_with_cooldown, SupPid, ComponentId}).

-spec find_component_by_pid(pid(), #{component_id() => #component{}}) -> 
    {ok, component_id()} | error.
find_component_by_pid(Pid, Components) ->
    case maps:to_list(maps:filter(fun(_Id, Component) -> 
        Component#component.pid =:= Pid 
    end, Components)) of
        [{ComponentId, _Component}] -> {ok, ComponentId};
        [] -> error;
        [{FirstId, _} | _] = Multiple ->
            ?LOG_WARNING("Multiple components with same PID: ~p", [Multiple]),
            {ok, FirstId}
    end.

-spec check_and_update_recoveries(#state{}) -> #state{}.
check_and_update_recoveries(State) ->
    UpdatedComponents = maps:map(fun(ComponentId, Component) ->
        check_component_recovery(ComponentId, Component)
    end, State#state.components),
    
    State#state{components = UpdatedComponents}.

-spec check_component_recovery(component_id(), #component{}) -> #component{}.
check_component_recovery(ComponentId, Component) ->
    case Component#component.circuit_state of
        open ->
            % Check if circuit should transition to half-open
            case should_try_half_open(Component, Component#component.policy) of
                true ->
                    ?LOG_INFO("Transitioning circuit breaker for ~p to half-open", [ComponentId]),
                    Component#component{circuit_state = half_open};
                false ->
                    Component
            end;
        half_open ->
            % Check if component is healthy in half-open state
            case is_component_healthy(Component) of
                true ->
                    ?LOG_INFO("Component ~p recovered, closing circuit breaker", [ComponentId]),
                    Component#component{circuit_state = closed, failures = 0};
                false ->
                    ?LOG_WARNING("Component ~p failed health check, opening circuit breaker", [ComponentId]),
                    Component#component{circuit_state = open}
            end;
        closed ->
            Component
    end.

-spec is_component_healthy(#component{}) -> boolean().
is_component_healthy(Component) ->
    case Component#component.pid of
        undefined -> false;
        Pid -> is_process_alive(Pid)
    end.

-spec update_metric(atom(), number(), map()) -> map().
update_metric(Key, Delta, Metrics) ->
    CurrentValue = maps:get(Key, Metrics, 0),
    maps:put(Key, CurrentValue + Delta, Metrics).

%% @doc Actually restart the component via its supervisor
%% Implements supervisor restart strategies:
%% - For temporary workers: use restart_child/2
%% - For permanent workers: terminate and let supervisor restart
-spec do_restart_component(pid(), component_id()) -> {ok, pid()} | {error, term()}.
do_restart_component(SupPid, ComponentId) ->
    try
        case supervisor:get_childspec(SupPid, ComponentId) of
            {ok, ChildSpec} ->
                RestartType = maps:get(restart, ChildSpec, permanent),
                case RestartType of
                    temporary ->
                        % Temporary workers can be restarted manually
                        ?LOG_INFO("Restarting temporary component ~p", [ComponentId]),
                        case supervisor:terminate_child(SupPid, ComponentId) of
                            ok ->
                                case supervisor:restart_child(SupPid, ComponentId) of
                                    {ok, Pid} -> {ok, Pid};
                                    {error, Reason} -> {error, {restart_failed, Reason}}
                                end;
                            {error, Reason} ->
                                {error, {terminate_failed, Reason}}
                        end;
                    transient ->
                        % Transient workers restart only on abnormal termination
                        ?LOG_INFO("Restarting transient component ~p", [ComponentId]),
                        case supervisor:terminate_child(SupPid, ComponentId) of
                            ok ->
                                case supervisor:restart_child(SupPid, ComponentId) of
                                    {ok, Pid} -> {ok, Pid};
                                    {error, Reason} -> {error, {restart_failed, Reason}}
                                end;
                            {error, Reason} ->
                                {error, {terminate_failed, Reason}}
                        end;
                    permanent ->
                        % Permanent workers: terminate and let supervisor auto-restart
                        ?LOG_INFO("Terminating permanent component ~p for auto-restart", [ComponentId]),
                        case supervisor:terminate_child(SupPid, ComponentId) of
                            ok ->
                                % Wait a bit for supervisor to restart it
                                timer:sleep(100),
                                case supervisor:which_children(SupPid) of
                                    Children ->
                                        case lists:keyfind(ComponentId, 1, Children) of
                                            {_, Pid, _, _} when is_pid(Pid) ->
                                                {ok, Pid};
                                            {_, undefined, _, _} ->
                                                % Not restarted yet, try to manually restart
                                                case supervisor:restart_child(SupPid, ComponentId) of
                                                    {ok, Pid} -> {ok, Pid};
                                                    {error, Reason} -> {error, {restart_failed, Reason}}
                                                end;
                                            false ->
                                                {error, child_not_found}
                                        end
                                end;
                            {error, Reason} ->
                                {error, {terminate_failed, Reason}}
                        end
                end;
            {error, not_found} ->
                % Child spec not found - might be a dynamic child
                ?LOG_WARNING("Child spec not found for ~p, trying dynamic restart", [ComponentId]),
                restart_dynamic_component(SupPid, ComponentId)
        end
    catch
        error:ErrorReason ->
            ?LOG_ERROR("Exception during component restart: ~p", [ErrorReason]),
            {error, {restart_exception, ErrorReason}}
    end.

%% @doc Restart a dynamic component (simple_one_for_one supervisor)
-spec restart_dynamic_component(pid(), component_id()) -> {ok, pid()} | {error, term()}.
restart_dynamic_component(SupPid, ComponentId) ->
    case supervisor:which_children(SupPid) of
        Children ->
            case lists:keyfind(ComponentId, 1, Children) of
                {_, OldPid, worker, _} when is_pid(OldPid) ->
                    % Terminate old instance
                    exit(OldPid, kill),
                    timer:sleep(100),
                    % For simple_one_for_one, we can't restart - we start a new child
                    % This requires knowing the start arguments
                    {error, {cannot_restart_dynamic, need_start_args}};
                {_, undefined, worker, _} ->
                    % Already terminated, try to restart
                    {error, {cannot_restart_dynamic, need_start_args}};
                false ->
                    {error, child_not_found}
            end
    end.