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
    recovery_timer :: undefined | timer:tref()
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

restart_generic_component(ComponentId) ->
    ?LOG_WARNING("Generic restart attempted for ~p", [ComponentId]),
    {error, {not_implemented, ComponentId}}.

-spec find_component_by_pid(pid(), #{component_id() => #component{}}) -> 
    {ok, component_id()} | error.
find_component_by_pid(Pid, Components) ->
    case maps:to_list(maps:filter(fun(_Id, Component) -> 
        Component#component.pid =:= Pid 
    end, Components)) of
        [{ComponentId, _Component}] -> {ok, ComponentId};
        [] -> error;
        Multiple -> 
            ?LOG_WARNING("Multiple components with same PID: ~p", [Multiple]),
            {ok, element(1, hd(Multiple))}
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