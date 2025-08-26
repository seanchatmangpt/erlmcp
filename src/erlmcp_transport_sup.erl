-module(erlmcp_transport_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/3, stop_child/1, get_child_status/1,
         transport_module/1, get_all_transports/0, restart_transport/1, get_transport_info/1,
         discover_available_transports/0, get_transport_metrics/1, graceful_shutdown/1]).

%% Internal functions exported for testing
-ifdef(TEST).

-export([validate_transport_config/2]).

-endif.

-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%% Enhanced exports for coordination and monitoring

%% Transport state tracking
-record(transport_info,
        {id :: atom(),
         type :: atom(),
         pid :: pid() | undefined,
         module :: module(),
         config :: map(),
         start_time :: erlang:timestamp() | undefined,
         restart_count = 0 :: non_neg_integer()}).

%% ETS table for transport tracking
-define(TRANSPORT_TABLE, erlmcp_transport_registry).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    ?LOG_INFO("Starting transport supervisor with enhanced monitoring"),

    % Create ETS table for transport tracking
    case ets:info(?TRANSPORT_TABLE) of
        undefined ->
            ets:new(?TRANSPORT_TABLE, [named_table, public, set, {keypos, 2}]);
        _ ->
            ok
    end,

    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(), atom(), map()) -> {ok, pid()} | {error, term()}.
start_child(TransportId, Type, Config) ->
    ensure_started(),
    ?LOG_INFO("Starting transport child: ~p type: ~p with config keys: ~p",
              [TransportId, Type, maps:keys(Config)]),

    % Store decision in coordination memory
    Decision =
        #{action => start_child,
          transport_id => TransportId,
          type => Type,
          config_keys => maps:keys(Config),
          timestamp => erlang:timestamp()},
    store_supervisor_decision(Decision),

    try
        % Validate transport type and get module
        Module = transport_module(Type),

        % Enhanced validation with dependency checking
        case validate_transport_config(Type, Config) of
            ok ->
                case check_transport_dependencies(Type, Module) of
                    ok ->
                        proceed_with_start(TransportId, Type, Module, Config);
                    {error, DepError} ->
                        ?LOG_ERROR("Dependency check failed for transport ~p (~p): ~p",
                                   [TransportId, Type, DepError]),
                        notify_coordination_hooks(start_failed, TransportId, DepError),
                        {error, {dependency_check_failed, DepError}}
                end;
            {error, ValidationError} ->
                ?LOG_ERROR("Configuration validation failed for transport ~p (~p): ~p",
                           [TransportId, Type, ValidationError]),
                notify_coordination_hooks(validation_failed, TransportId, ValidationError),
                {error, {config_validation_failed, ValidationError}}
        end
    catch
        error:({unknown_transport_type, _} = Error) ->
            ?LOG_ERROR("Unknown transport type for ~p: ~p", [TransportId, Type]),
            notify_coordination_hooks(type_error, TransportId, Error),
            {error, Error};
        error:StartupError:Stacktrace ->
            ?LOG_ERROR("Exception starting transport ~p: ~p~n~p",
                       [TransportId, StartupError, Stacktrace]),
            notify_coordination_hooks(startup_exception, TransportId, StartupError),
            {error, {startup_exception, StartupError}};
        Class:ExceptionReason:Stacktrace ->
            ?LOG_ERROR("Unexpected error starting transport ~p: ~p:~p~n~p",
                       [TransportId, Class, ExceptionReason, Stacktrace]),
            notify_coordination_hooks(unexpected_error, TransportId, {Class, ExceptionReason}),
            {error, {unexpected_error, {Class, ExceptionReason}}}
    end.

%% Ensure supervisor is started before operations
-spec ensure_started() -> ok.
ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            case ?MODULE:start_link() of
                {ok, _Pid} ->
                    ok;
                {error, {already_started, _Pid}} ->
                    ok;
                {error, _} ->
                    ok
            end;
        _ ->
            ok
    end.

%% Enhanced start procedure with comprehensive error handling
-spec proceed_with_start(atom(), atom(), module(), map()) ->
                            {ok, pid()} | {error, term()}.
proceed_with_start(TransportId, Type, Module, Config) ->
    % Check if transport already exists
    case get_child_status(TransportId) of
        {ok, running} ->
            ?LOG_WARNING("Transport ~p already running, returning existing PID", [TransportId]),
            case supervisor:which_children(?MODULE) of
                Children when is_list(Children) ->
                    case lists:keyfind(TransportId, 1, Children) of
                        {TransportId, Pid, worker, _} when is_pid(Pid) ->
                            {ok, Pid};
                        _ ->
                            {error, inconsistent_state}
                    end;
                Error ->
                    {error, {supervisor_query_failed, Error}}
            end;
        _ ->
            % Proceed with starting new transport
            start_new_transport(TransportId, Type, Module, Config)
    end.

%% Start new transport with enhanced monitoring
-spec start_new_transport(atom(), atom(), module(), map()) ->
                             {ok, pid()} | {error, term()}.
start_new_transport(TransportId, Type, Module, Config) ->
    StartTime = erlang:timestamp(),

    % Create enhanced child specification with adaptive restart strategy
    RestartCount = get_restart_count(TransportId),
    RestartStrategy = determine_restart_strategy(Type, TransportId, RestartCount),
    
    % Check circuit breaker before proceeding
    CircuitState = check_circuit_breaker(TransportId),
    case CircuitState of
        open ->
            ?LOG_WARNING("Circuit breaker OPEN for transport ~p, delaying start", [TransportId]),
            timer:sleep(5000);  % Brief delay to prevent immediate restart storms
        _ ->
            ok
    end,
    
    ChildSpec =
        #{id => TransportId,
          start => {Module, start_link, [TransportId, Config]},
          restart => RestartStrategy,  % Enhanced: Adaptive restart strategy
          shutdown => 15000,           % Enhanced: Extended shutdown timeout
          type => worker,
          modules => [Module]},

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} = Result ->
            ?LOG_INFO("Successfully started transport ~p (~p) with PID ~p",
                      [TransportId, Type, Pid]),

            % Store transport information
            TransportInfo =
                #transport_info{id = TransportId,
                                type = Type,
                                pid = Pid,
                                module = Module,
                                config = Config,
                                start_time = StartTime,
                                restart_count = get_restart_count(TransportId)},
            ets:insert(?TRANSPORT_TABLE, TransportInfo),

            % Register with health monitoring system
            register_transport_health_monitoring(TransportId, Pid, Type),

            % Reset circuit breaker on successful start
            update_circuit_breaker(TransportId, closed),

            % Notify coordination hooks of successful start
            notify_coordination_hooks(transport_started, TransportId, #{pid => Pid, type => Type}),

            % Store success decision in memory
            SuccessDecision =
                #{action => transport_started,
                  transport_id => TransportId,
                  type => Type,
                  pid => Pid,
                  timestamp => erlang:timestamp()},
            store_supervisor_decision(SuccessDecision),

            Result;
        {error, {already_started, Pid}} ->
            ?LOG_WARNING("Transport ~p already started with PID ~p", [TransportId, Pid]),
            % Update our tracking even if it was already started
            update_transport_info(TransportId, Pid),
            {ok, Pid};
        {error, Reason} = Error ->
            ?LOG_ERROR("Failed to start transport ~p (~p): ~p", [TransportId, Type, Reason]),

            % Record failure in circuit breaker
            record_transport_failure(TransportId),

            % Notify coordination hooks of failure
            notify_coordination_hooks(transport_start_failed, TransportId, #{reason => Reason}),

            % Store failure decision in memory
            FailureDecision =
                #{action => transport_start_failed,
                  transport_id => TransportId,
                  type => Type,
                  reason => Reason,
                  circuit_state => check_circuit_breaker(TransportId),
                  restart_count => get_restart_count(TransportId),
                  timestamp => erlang:timestamp()},
            store_supervisor_decision(FailureDecision),

            % Record failure for analysis
            increment_restart_count(TransportId),

            % Check circuit breaker state before recovery attempts
            case check_circuit_breaker(TransportId) of
                open ->
                    ?LOG_WARNING("Circuit breaker OPEN for transport ~p, skipping recovery", [TransportId]),
                    notify_coordination_hooks(recovery_blocked_by_circuit_breaker, 
                                              TransportId, #{reason => Reason}),
                    Error;
                _ ->
                    % Attempt recovery if this is a known transport type failure
                    case should_attempt_recovery(Reason, TransportId) of
                        true ->
                            ?LOG_INFO("Attempting recovery for transport ~p", [TransportId]),
                            notify_coordination_hooks(attempting_recovery,
                                                      TransportId,
                                                      #{reason => Reason}),
                            attempt_transport_recovery(TransportId, Type, Module, Config);
                        false ->
                            notify_coordination_hooks(recovery_abandoned, 
                                                    TransportId, #{reason => Reason}),
                            Error
                    end
            end
    end.

-spec stop_child(atom()) -> ok | {error, term()}.
stop_child(TransportId) ->
    ?LOG_INFO("Stopping transport child: ~p", [TransportId]),

    % Store decision in coordination memory
    StopDecision =
        #{action => stop_child,
          transport_id => TransportId,
          timestamp => erlang:timestamp()},
    store_supervisor_decision(StopDecision),

    % Notify coordination hooks
    notify_coordination_hooks(stopping_transport, TransportId, #{}),

    % Unregister from health monitoring first
    unregister_transport_health_monitoring(TransportId),

    case supervisor:terminate_child(?MODULE, TransportId) of
        ok ->
            case supervisor:delete_child(?MODULE, TransportId) of
                ok ->
                    % Clean up transport tracking
                    ets:delete(?TRANSPORT_TABLE, TransportId),
                    ?LOG_INFO("Successfully stopped and removed transport ~p", [TransportId]),

                    % Notify coordination hooks of successful stop
                    notify_coordination_hooks(transport_stopped, TransportId, #{}),

                    % Store success in memory
                    SuccessDecision =
                        #{action => transport_stopped,
                          transport_id => TransportId,
                          timestamp => erlang:timestamp()},
                    store_supervisor_decision(SuccessDecision),
                    ok;
                {error, Reason} = Error ->
                    ?LOG_ERROR("Failed to delete transport ~p: ~p", [TransportId, Reason]),
                    Error
            end;
        {error, not_found} ->
            % Transport was already stopped, clean up tracking
            ets:delete(?TRANSPORT_TABLE, TransportId),
            ?LOG_INFO("Transport ~p was already stopped, cleaned up tracking", [TransportId]),
            ok;
        {error, Reason} = Error ->
            ?LOG_ERROR("Failed to terminate transport ~p: ~p", [TransportId, Reason]),
            Error
    end.

-spec get_child_status(atom()) -> {ok, running | stopped} | {error, not_found}.
get_child_status(TransportId) ->
    case supervisor:which_children(?MODULE) of
        Children when is_list(Children) ->
            case lists:keyfind(TransportId, 1, Children) of
                {TransportId, Pid, worker, _} when is_pid(Pid) ->
                    case is_process_alive(Pid) of
                        true ->
                            {ok, running};
                        false ->
                            {ok, stopped}
                    end;
                {TransportId, undefined, worker, _} ->
                    {ok, stopped};
                false ->
                    % Fall back to ETS tracking if supervisor has not reflected the child yet
                    case ets:lookup(?TRANSPORT_TABLE, TransportId) of
                        [#transport_info{pid = P}] when is_pid(P), P =/= undefined ->
                            case is_process_alive(P) of
                                true ->
                                    {ok, running};
                                false ->
                                    {ok, stopped}
                            end;
                        _ ->
                            {error, not_found}
                    end
            end;
        Error ->
            ?LOG_ERROR("Failed to get children list: ~p", [Error]),
            {error, supervisor_error}
    end.

%% Enhanced transport module resolution with validation and module checking
-spec transport_module(atom()) -> module().
transport_module(stdio) ->
    Module = erlmcp_transport_stdio_new,
    ensure_module_loaded(Module);
transport_module(tcp) ->
    Module = erlmcp_transport_tcp,
    ensure_module_loaded(Module);
transport_module(http) ->
    Module = erlmcp_transport_http,
    ensure_module_loaded(Module);
transport_module(Type) ->
    ?LOG_ERROR("Unknown transport type: ~p. Supported types: stdio, tcp, http", [Type]),
    error({unknown_transport_type, Type}).

%% Ensure the transport module is loaded and available
-spec ensure_module_loaded(module()) -> module().
ensure_module_loaded(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            Module;
        {error, Reason} ->
            ?LOG_ERROR("Failed to load transport module ~p: ~p", [Module, Reason]),
            error({module_load_failed, Module, Reason})
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Enhanced health monitoring integration
-spec register_transport_health_monitoring(atom(), pid(), atom()) -> ok.
register_transport_health_monitoring(TransportId, Pid, Type) ->
    case whereis(erlmcp_health_monitor) of
        undefined ->
            ?LOG_WARNING("Health monitor not available; skipping registration for ~p",
                         [TransportId]),
            ok;
        _ ->
            HealthCheckFun = create_transport_health_check_function(Type, Pid),
            case catch gen_server:call(erlmcp_health_monitor,
                                       {register_component, TransportId, Pid, HealthCheckFun})
            of
                ok ->
                    ?LOG_DEBUG("Registered transport ~p with health monitoring", [TransportId]);
                {error, Reason} ->
                    ?LOG_WARNING("Failed to register transport ~p with health monitor: ~p",
                                 [TransportId, Reason]);
                {'EXIT', Reason2} ->
                    ?LOG_WARNING("Health monitor registration crashed: ~p", [Reason2])
            end,
            ok
    end.

%% Remove transport from health monitoring
-spec unregister_transport_health_monitoring(atom()) -> ok.
unregister_transport_health_monitoring(TransportId) ->
    erlmcp_health_monitor:unregister_component(TransportId),
    ?LOG_DEBUG("Unregistered transport ~p from health monitoring", [TransportId]),
    ok.

%% Create transport-specific health check functions
-spec create_transport_health_check_function(atom(), pid()) ->
                                                fun(() -> healthy | unhealthy | degraded).
create_transport_health_check_function(Type, Pid) ->
    fun() ->
       try
           case is_process_alive(Pid) of
               false -> unhealthy;
               true ->
                   % Transport-specific health checks
                   case Type of
                       stdio -> check_stdio_transport_health(Pid);
                       tcp -> check_tcp_transport_health(Pid);
                       http -> check_http_transport_health(Pid);
                       _ ->
                           healthy % Default to healthy for unknown types
                   end
           end
       catch
           _:_ -> unhealthy
       end
    end.

%% Transport-specific health checks
-spec check_stdio_transport_health(pid()) -> healthy | unhealthy | degraded.
check_stdio_transport_health(Pid) ->
    % Check if stdio transport can handle basic operations
    case catch gen_server:call(Pid, {health_check}, 5000) of
        {ok, healthy} ->
            healthy;
        {ok, degraded} ->
            degraded;
        _ ->
            unhealthy
    end.

-spec check_tcp_transport_health(pid()) -> healthy | unhealthy | degraded.
check_tcp_transport_health(Pid) ->
    % Check TCP connection status
    case catch gen_server:call(Pid, {connection_status}, 5000) of
        {ok, connected} ->
            healthy;
        {ok, connecting} ->
            degraded;
        _ ->
            unhealthy
    end.

-spec check_http_transport_health(pid()) -> healthy | unhealthy | degraded.
check_http_transport_health(Pid) ->
    % Check HTTP transport responsiveness
    case catch gen_server:call(Pid, {server_status}, 5000) of
        {ok, running} ->
            healthy;
        {ok, degraded} ->
            degraded;
        _ ->
            unhealthy
    end.

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ?LOG_INFO("Initializing transport supervisor"),

    % Enhanced supervisor flags for better reliability and resilience
    SupFlags =
        #{strategy => one_for_one,  % Transport failures are isolated
          intensity => 20,          % Enhanced: Higher restart intensity for reliability
          period => 60,             % Period in seconds for restart intensity
          auto_shutdown => never},    % Enhanced: Never auto-shutdown supervisor

    % Start with empty child specs - transports are added dynamically
    ChildSpecs = [],

    ?LOG_INFO("Transport supervisor initialized with enhanced configuration"),
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Additional API Functions
%%====================================================================

%% Get all active transports
-spec get_all_transports() -> [{atom(), pid(), atom(), map()}].
get_all_transports() ->
    case ets:tab2list(?TRANSPORT_TABLE) of
        TransportInfos when is_list(TransportInfos) ->
            [{Info#transport_info.id,
              Info#transport_info.pid,
              Info#transport_info.type,
              Info#transport_info.config}
             || Info <- TransportInfos];
        _ ->
            []
    end.

%% Restart a specific transport
-spec restart_transport(atom()) -> {ok, pid()} | {error, term()}.
restart_transport(TransportId) ->
    ?LOG_INFO("Restarting transport ~p", [TransportId]),
    case ets:lookup(?TRANSPORT_TABLE, TransportId) of
        [#transport_info{type = Type, config = Config}] ->
            case stop_child(TransportId) of
                ok ->
                    % Wait a bit for clean shutdown
                    timer:sleep(1000),
                    case start_child(TransportId, Type, Config) of
                        {ok, NewPid} = Result ->
                            ?LOG_INFO("Successfully restarted transport ~p with new PID ~p",
                                      [TransportId, NewPid]),
                            Result;
                        {error, Reason} = Error ->
                            ?LOG_ERROR("Failed to restart transport ~p: ~p", [TransportId, Reason]),
                            Error
                    end;
                {error, Reason} = Error ->
                    ?LOG_ERROR("Failed to stop transport ~p for restart: ~p",
                               [TransportId, Reason]),
                    Error
            end;
        [] ->
            {error, transport_not_found}
    end.

%% Get detailed transport information
-spec get_transport_info(atom()) -> {ok, map()} | {error, not_found}.
get_transport_info(TransportId) ->
    case ets:lookup(?TRANSPORT_TABLE, TransportId) of
        [#transport_info{} = Info] ->
            % Get current health status
            HealthStatus =
                case erlmcp_health_monitor:get_component_health(TransportId) of
                    not_found ->
                        unknown;
                    Status ->
                        Status
                end,

            InfoMap =
                #{id => Info#transport_info.id,
                  type => Info#transport_info.type,
                  pid => Info#transport_info.pid,
                  module => Info#transport_info.module,
                  config => Info#transport_info.config,
                  start_time => Info#transport_info.start_time,
                  restart_count => Info#transport_info.restart_count,
                  health_status => HealthStatus,
                  uptime_seconds =>
                      case Info#transport_info.start_time of
                          undefined ->
                              0;
                          StartTime ->
                              timer:now_diff(
                                  erlang:timestamp(), StartTime)
                              div 1000000
                      end},
            {ok, InfoMap};
        [] ->
            {error, not_found}
    end.

%%====================================================================
%% Enhanced Internal Functions with Advanced Error Recovery
%%====================================================================

%% Circuit breaker pattern for transport health monitoring
-record(circuit_breaker,
        {transport_id :: atom(),
         state :: closed | open | half_open,
         failure_count = 0 :: non_neg_integer(),
         failure_threshold = 5 :: non_neg_integer(),
         recovery_timeout = 30000 :: pos_integer(),
         last_failure_time :: erlang:timestamp() | undefined}).

%% Transport module resolution cache
-define(MODULE_CACHE_TABLE, erlmcp_transport_module_cache).

%% Initialize module cache if not exists
-spec ensure_module_cache() -> ok.
ensure_module_cache() ->
    case ets:info(?MODULE_CACHE_TABLE) of
        undefined ->
            ets:new(?MODULE_CACHE_TABLE, [named_table, public, set]);
        _ ->
            ok
    end,
    ok.

%% Enhanced transport module resolution with caching
-spec transport_module_cached(atom()) -> module().
transport_module_cached(Type) ->
    ensure_module_cache(),
    case ets:lookup(?MODULE_CACHE_TABLE, Type) of
        [{Type, Module, true}] ->
            Module;
        _ ->
            try
                Module = transport_module(Type),
                ets:insert(?MODULE_CACHE_TABLE, {Type, Module, true}),
                Module
            catch
                error:({unknown_transport_type, _} = Error) ->
                    ets:insert(?MODULE_CACHE_TABLE, {Type, undefined, false}),
                    error(Error)
            end
    end.

%% Circuit breaker management for transport health
-spec check_circuit_breaker(atom()) -> closed | open | half_open.
check_circuit_breaker(TransportId) ->
    case ets:lookup(?TRANSPORT_TABLE, {circuit_breaker, TransportId}) of
        [{_, #circuit_breaker{state = State, 
                             failure_count = Count,
                             failure_threshold = Threshold,
                             recovery_timeout = Timeout,
                             last_failure_time = LastFailure}}] ->
            case State of
                open when LastFailure =/= undefined ->
                    TimeSinceFailure = timer:now_diff(erlang:timestamp(), LastFailure) div 1000,
                    if TimeSinceFailure >= Timeout ->
                           update_circuit_breaker(TransportId, half_open),
                           half_open;
                       true ->
                           open
                    end;
                closed when Count >= Threshold ->
                    update_circuit_breaker(TransportId, open),
                    open;
                Other ->
                    Other
            end;
        [] ->
            init_circuit_breaker(TransportId),
            closed
    end.

%% Initialize circuit breaker for transport
-spec init_circuit_breaker(atom()) -> ok.
init_circuit_breaker(TransportId) ->
    CircuitBreaker = #circuit_breaker{
        transport_id = TransportId,
        state = closed
    },
    ets:insert(?TRANSPORT_TABLE, {{circuit_breaker, TransportId}, CircuitBreaker}),
    ok.

%% Update circuit breaker state
-spec update_circuit_breaker(atom(), closed | open | half_open) -> ok.
update_circuit_breaker(TransportId, NewState) ->
    case ets:lookup(?TRANSPORT_TABLE, {circuit_breaker, TransportId}) of
        [{_, CircuitBreaker}] ->
            UpdatedCB = case NewState of
                open ->
                    CircuitBreaker#circuit_breaker{
                        state = open,
                        last_failure_time = erlang:timestamp()
                    };
                closed ->
                    CircuitBreaker#circuit_breaker{
                        state = closed,
                        failure_count = 0
                    };
                half_open ->
                    CircuitBreaker#circuit_breaker{
                        state = half_open
                    }
            end,
            ets:insert(?TRANSPORT_TABLE, {{circuit_breaker, TransportId}, UpdatedCB});
        [] ->
            init_circuit_breaker(TransportId)
    end,
    ?LOG_DEBUG("Circuit breaker for transport ~p updated to state: ~p", [TransportId, NewState]),
    ok.

%% Record transport failure for circuit breaker
-spec record_transport_failure(atom()) -> ok.
record_transport_failure(TransportId) ->
    case ets:lookup(?TRANSPORT_TABLE, {circuit_breaker, TransportId}) of
        [{_, CircuitBreaker}] ->
            UpdatedCB = CircuitBreaker#circuit_breaker{
                failure_count = CircuitBreaker#circuit_breaker.failure_count + 1,
                last_failure_time = erlang:timestamp()
            },
            ets:insert(?TRANSPORT_TABLE, {{circuit_breaker, TransportId}, UpdatedCB}),
            ?LOG_WARNING("Transport ~p failure recorded, count: ~p", 
                        [TransportId, UpdatedCB#circuit_breaker.failure_count]);
        [] ->
            init_circuit_breaker(TransportId),
            record_transport_failure(TransportId)
    end,
    ok.

%% Enhanced restart strategy based on transport type and history
-spec determine_restart_strategy(atom(), atom(), non_neg_integer()) -> permanent | temporary | transient.
determine_restart_strategy(Type, TransportId, RestartCount) ->
    CircuitState = check_circuit_breaker(TransportId),
    
    Strategy = case {Type, RestartCount, CircuitState} of
        {stdio, Count, _} when Count < 3 -> permanent;  % STDIO is critical, restart aggressively
        {stdio, _, open} -> temporary;                   % Circuit open, back off
        {stdio, _, _} -> transient;                      % Moderate restart for STDIO
        
        {tcp, Count, _} when Count < 5 -> permanent;     % TCP can handle more restarts
        {tcp, _, open} -> temporary;                     % Circuit open, back off
        {tcp, _, _} -> transient;                        % Standard TCP restart
        
        {http, Count, _} when Count < 2 -> transient;    % HTTP is less critical
        {http, _, open} -> temporary;                    % Circuit open, minimal restart
        {_, _, _} -> temporary                           % Conservative default
    end,
    
    ?LOG_DEBUG("Determined restart strategy for transport ~p (~p): ~p (restarts: ~p, circuit: ~p)",
               [TransportId, Type, Strategy, RestartCount, CircuitState]),
    Strategy.

%% Validate transport configuration
-spec validate_transport_config(atom(), map()) -> ok | {error, term()}.
validate_transport_config(stdio, Config) ->
    validate_stdio_config(Config);
validate_transport_config(tcp, Config) ->
    validate_tcp_config(Config);
validate_transport_config(http, Config) ->
    validate_http_config(Config);
validate_transport_config(Type, _Config) ->
    {error, {unknown_transport_type, Type}}.

%% STDIO configuration validation
-spec validate_stdio_config(map()) -> ok | {error, term()}.
validate_stdio_config(_Config) ->
    % STDIO transport has minimal configuration requirements
    ok.

%% TCP configuration validation
-spec validate_tcp_config(map()) -> ok | {error, term()}.
validate_tcp_config(Config) ->
    RequiredKeys = [host, port],
    case validate_required_keys(Config, RequiredKeys) of
        ok ->
            % Additional TCP-specific validation
            case maps:get(port, Config) of
                Port when is_integer(Port), Port > 0, Port =< 65535 ->
                    ok;
                InvalidPort ->
                    {error, {invalid_port, InvalidPort}}
            end;
        Error ->
            Error
    end.

%% HTTP configuration validation (require url)
-spec validate_http_config(map()) -> ok | {error, term()}.
validate_http_config(Config) ->
    RequiredKeys = [url],
    case validate_required_keys(Config, RequiredKeys) of
        ok ->
            % Additional HTTP-specific validation
            case maps:get(url, Config) of
                Url when is_list(Url); is_binary(Url) ->
                    ok;
                InvalidUrl ->
                    {error, {invalid_url, InvalidUrl}}
            end;
        Error ->
            Error
    end.

%% Validate required configuration keys
-spec validate_required_keys(map(), [atom()]) -> ok | {error, term()}.
validate_required_keys(Config, RequiredKeys) ->
    MissingKeys = [Key || Key <- RequiredKeys, not maps:is_key(Key, Config)],
    case MissingKeys of
        [] ->
            ok;
        _ ->
            {error, {missing_required_keys, MissingKeys}}
    end.

%% Recovery and restart management
-spec should_attempt_recovery(term(), atom()) -> boolean().
should_attempt_recovery(Reason, TransportId) ->
    % Check restart count to avoid infinite restart loops
    RestartCount = get_restart_count(TransportId),
    MaxRestarts = 5, % Maximum automatic restarts

    case {Reason, RestartCount < MaxRestarts} of
        {{shutdown, _}, _} ->
            false; % Don't recover from intentional shutdown
        {normal, _} ->
            false; % Don't recover from normal termination
        {_, true} ->
            true;  % Attempt recovery if under restart limit
        {_, false} ->
            ?LOG_ERROR("Transport ~p has exceeded maximum restart attempts (~p), giving up",
                       [TransportId, MaxRestarts]),
            false
    end.

%% Attempt transport recovery
-spec attempt_transport_recovery(atom(), atom(), module(), map()) ->
                                    {ok, pid()} | {error, term()}.
attempt_transport_recovery(TransportId, Type, Module, Config) ->
    ?LOG_INFO("Attempting recovery for transport ~p", [TransportId]),

    % Wait a bit before retry
    timer:sleep(2000),

    % Try to start again
    case start_new_transport(TransportId, Type, Module, Config) of
        {ok, Pid} = Result ->
            ?LOG_INFO("Successfully recovered transport ~p with new PID ~p", [TransportId, Pid]),
            notify_coordination_hooks(transport_recovered, TransportId, #{pid => Pid}),
            
            % Store recovery success in memory
            RecoveryDecision = #{action => transport_recovered, transport_id => TransportId, 
                                type => Type, new_pid => Pid, timestamp => erlang:timestamp()},
            store_supervisor_decision(RecoveryDecision),
            Result;
        {error, Reason} = Error ->
            ?LOG_ERROR("Recovery failed for transport ~p: ~p", [TransportId, Reason]),
            notify_coordination_hooks(transport_recovery_failed, TransportId, #{reason => Reason}),
            
            % Store recovery failure in memory
            FailureDecision = #{action => transport_recovery_failed, transport_id => TransportId, 
                               type => Type, reason => Reason, timestamp => erlang:timestamp()},
            store_supervisor_decision(FailureDecision),
            Error
    end.

%% Get restart count for a transport
-spec get_restart_count(atom()) -> non_neg_integer().
get_restart_count(TransportId) ->
    case ets:lookup(?TRANSPORT_TABLE, TransportId) of
        [#transport_info{restart_count = Count}] ->
            Count;
        [] ->
            0
    end.

%% Increment restart count
-spec increment_restart_count(atom()) -> ok.
increment_restart_count(TransportId) ->
    case ets:lookup(?TRANSPORT_TABLE, TransportId) of
        [#transport_info{} = Info] ->
            UpdatedInfo =
                Info#transport_info{restart_count = Info#transport_info.restart_count + 1},
            ets:insert(?TRANSPORT_TABLE, UpdatedInfo);
        [] ->
            ok
    end.

%% Update transport info with new PID
-spec update_transport_info(atom(), pid()) -> ok.
update_transport_info(TransportId, NewPid) ->
    case ets:lookup(?TRANSPORT_TABLE, TransportId) of
        [#transport_info{} = Info] ->
            UpdatedInfo = Info#transport_info{pid = NewPid, start_time = erlang:timestamp()},
            ets:insert(?TRANSPORT_TABLE, UpdatedInfo);
        [] ->
            ?LOG_WARNING("Cannot update transport info for unknown transport ~p", [TransportId])
    end,
    ok.

%%====================================================================
%% Enhanced API Functions
%%====================================================================

%% Discover available transport types
-spec discover_available_transports() -> [{atom(), module(), boolean()}].
discover_available_transports() ->
    TransportTypes = [stdio, tcp, http],
    [{Type, catch_transport_module(Type), is_transport_available(Type)} 
     || Type <- TransportTypes].

%% Safe transport module lookup
-spec catch_transport_module(atom()) -> module() | undefined.
catch_transport_module(Type) ->
    try
        transport_module(Type)
    catch
        error:{unknown_transport_type, _} ->
            undefined;
        _:_ ->
            undefined
    end.

%% Check if transport module is available and functioning
-spec is_transport_available(atom()) -> boolean().
is_transport_available(Type) ->
    try
        Module = transport_module(Type),
        case code:ensure_loaded(Module) of
            {module, Module} ->
                % Check if module exports required functions
                case erlang:function_exported(Module, start_link, 2) of
                    true -> true;
                    false -> false
                end;
            _ ->
                false
        end
    catch
        _:_ -> false
    end.

%% Get transport performance metrics
-spec get_transport_metrics(atom()) -> {ok, map()} | {error, not_found}.
get_transport_metrics(TransportId) ->
    case ets:lookup(?TRANSPORT_TABLE, TransportId) of
        [#transport_info{pid = Pid, start_time = StartTime}] when is_pid(Pid) ->
            try
                % Get basic process info
                ProcessInfo = case process_info(Pid, [memory, message_queue_len, reductions]) of
                    undefined -> #{};
                    Info -> maps:from_list(Info)
                end,
                
                % Calculate uptime
                UptimeSeconds = case StartTime of
                    undefined -> 0;
                    ST -> timer:now_diff(erlang:timestamp(), ST) div 1000000
                end,
                
                % Try to get transport-specific metrics
                TransportMetrics = case catch gen_server:call(Pid, {get_metrics}, 5000) of
                    {ok, Metrics} when is_map(Metrics) -> Metrics;
                    _ -> #{}
                end,
                
                AllMetrics = ProcessInfo#{uptime_seconds => UptimeSeconds,
                                         transport_metrics => TransportMetrics},
                {ok, AllMetrics}
            catch
                _:_ -> {error, metrics_unavailable}
            end;
        _ ->
            {error, not_found}
    end.

%% Graceful shutdown of transport
-spec graceful_shutdown(atom()) -> ok | {error, term()}.
graceful_shutdown(TransportId) ->
    ?LOG_INFO("Initiating graceful shutdown for transport ~p", [TransportId]),
    
    case ets:lookup(?TRANSPORT_TABLE, TransportId) of
        [#transport_info{pid = Pid, type = Type}] when is_pid(Pid) ->
            % Notify coordination hooks
            notify_coordination_hooks(graceful_shutdown_initiated, TransportId, #{type => Type}),
            
            % Store decision
            ShutdownDecision = #{action => graceful_shutdown_initiated, transport_id => TransportId, 
                                type => Type, timestamp => erlang:timestamp()},
            store_supervisor_decision(ShutdownDecision),
            
            try
                % Try graceful shutdown first
                case catch gen_server:call(Pid, {prepare_shutdown}, 10000) of
                    ok ->
                        ?LOG_INFO("Transport ~p prepared for shutdown", [TransportId]),
                        timer:sleep(1000), % Give it time to finish current operations
                        stop_child(TransportId);
                    {error, Reason} ->
                        ?LOG_WARNING("Transport ~p graceful shutdown preparation failed: ~p", 
                                    [TransportId, Reason]),
                        stop_child(TransportId);
                    _ ->
                        ?LOG_INFO("Transport ~p doesn't support graceful shutdown, proceeding with normal stop", 
                                 [TransportId]),
                        stop_child(TransportId)
                end
            catch
                _:_ ->
                    ?LOG_WARNING("Exception during graceful shutdown of ~p, proceeding with normal stop", 
                                [TransportId]),
                    stop_child(TransportId)
            end;
        _ ->
            {error, transport_not_found}
    end.

%%====================================================================
%% Coordination and Memory Functions
%%====================================================================

%% Store supervisor decision in coordination memory
-spec store_supervisor_decision(map()) -> ok.
store_supervisor_decision(Decision) ->
    try
        % Convert decision to storable format
        DecisionStr = io_lib:format("~p", [Decision]),
        Key = io_lib:format("phase3/supervisor/~w/~p", 
                           [maps:get(transport_id, Decision, unknown),
                            maps:get(action, Decision, unknown_action)]),
        KeyStr = lists:flatten(Key),
        
        % Store in ETS as primary method
        case ets:info(?TRANSPORT_TABLE) of
            undefined -> 
                % Table doesn't exist, just log
                ?LOG_DEBUG("Supervisor decision (no table): ~p", [Decision]);
            _ ->
