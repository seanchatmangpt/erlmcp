-module(erlmcp_transport_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/3, stop_child/1, get_child_status/1, 
         transport_module/1, get_all_transports/0, restart_transport/1,
         get_transport_info/1]).
%% Internal functions exported for testing
-ifdef(TEST).
-export([validate_transport_config/2]).
-endif.
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%% Transport state tracking
-record(transport_info, {
    id :: atom(),
    type :: atom(),
    pid :: pid() | undefined,
    module :: module(),
    config :: map(),
    start_time :: erlang:timestamp() | undefined,
    restart_count = 0 :: non_neg_integer()
}).

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
    ?LOG_INFO("Starting transport child: ~p type: ~p with config keys: ~p", 
              [TransportId, Type, maps:keys(Config)]),
    
    try
        % Validate transport type and get module
        Module = transport_module(Type),
        
        % Validate configuration for the transport type
        case validate_transport_config(Type, Config) of
            ok ->
                proceed_with_start(TransportId, Type, Module, Config);
            {error, ValidationError} ->
                ?LOG_ERROR("Configuration validation failed for transport ~p (~p): ~p", 
                          [TransportId, Type, ValidationError]),
                {error, {config_validation_failed, ValidationError}}
        end
    catch
        error:{unknown_transport_type, _} = Error ->
            ?LOG_ERROR("Unknown transport type for ~p: ~p", [TransportId, Type]),
            {error, Error};
        error:StartupError:Stacktrace ->
            ?LOG_ERROR("Exception starting transport ~p: ~p~n~p", 
                      [TransportId, StartupError, Stacktrace]),
            {error, {startup_exception, StartupError}};
        Class:ExceptionReason:Stacktrace ->
            ?LOG_ERROR("Unexpected error starting transport ~p: ~p:~p~n~p", 
                      [TransportId, Class, ExceptionReason, Stacktrace]),
            {error, {unexpected_error, {Class, ExceptionReason}}}
    end.

%% Enhanced start procedure with comprehensive error handling
-spec proceed_with_start(atom(), atom(), module(), map()) -> {ok, pid()} | {error, term()}.
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
-spec start_new_transport(atom(), atom(), module(), map()) -> {ok, pid()} | {error, term()}.
start_new_transport(TransportId, Type, Module, Config) ->
    StartTime = erlang:timestamp(),
    
    % Create enhanced child specification
    ChildSpec = #{
        id => TransportId,
        start => {Module, start_link, [TransportId, Config]},
        restart => permanent,  % Use permanent restart for better reliability
        shutdown => 15000,     % Enhanced: Extended shutdown timeout
        type => worker,
        modules => [Module]
    },
    
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} = Result ->
            ?LOG_INFO("Successfully started transport ~p (~p) with PID ~p", 
                     [TransportId, Type, Pid]),
            
            % Store transport information
            TransportInfo = #transport_info{
                id = TransportId,
                type = Type,
                pid = Pid,
                module = Module,
                config = Config,
                start_time = StartTime,
                restart_count = get_restart_count(TransportId)
            },
            ets:insert(?TRANSPORT_TABLE, TransportInfo),
            
            % Register with health monitoring system
            register_transport_health_monitoring(TransportId, Pid, Type),
            
            Result;
        {error, {already_started, Pid}} ->
            ?LOG_WARNING("Transport ~p already started with PID ~p", [TransportId, Pid]),
            % Update our tracking even if it was already started
            update_transport_info(TransportId, Pid),
            {ok, Pid};
        {error, Reason} = Error ->
            ?LOG_ERROR("Failed to start transport ~p (~p): ~p", [TransportId, Type, Reason]),
            
            % Record failure for analysis
            increment_restart_count(TransportId),
            
            % Attempt recovery if this is a known transport type failure
            case should_attempt_recovery(Reason, TransportId) of
                true ->
                    ?LOG_INFO("Attempting recovery for transport ~p", [TransportId]),
                    attempt_transport_recovery(TransportId, Type, Module, Config);
                false ->
                    Error
            end
    end.

-spec stop_child(atom()) -> ok | {error, term()}.
stop_child(TransportId) ->
    ?LOG_INFO("Stopping transport child: ~p", [TransportId]),
    
    % Unregister from health monitoring first
    unregister_transport_health_monitoring(TransportId),
    
    case supervisor:terminate_child(?MODULE, TransportId) of
        ok ->
            case supervisor:delete_child(?MODULE, TransportId) of
                ok ->
                    % Clean up transport tracking
                    ets:delete(?TRANSPORT_TABLE, TransportId),
                    ?LOG_INFO("Successfully stopped and removed transport ~p", [TransportId]),
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
                        true -> {ok, running};
                        false -> {ok, stopped}
                    end;
                {TransportId, undefined, worker, _} ->
                    {ok, stopped};
                false ->
                    {error, not_found}
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
    Module = erlmcp_transport_tcp_new,
    ensure_module_loaded(Module);
transport_module(http) -> 
    Module = erlmcp_transport_http_new,
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
    % Create transport-specific health check function
    HealthCheckFun = create_transport_health_check_function(Type, Pid),
    
    % Register with the health monitoring system
    case erlmcp_health_monitor:register_component(TransportId, Pid, HealthCheckFun) of
        ok ->
            ?LOG_DEBUG("Registered transport ~p with health monitoring", [TransportId]);
        {error, Reason} ->
            ?LOG_WARNING("Failed to register transport ~p with health monitor: ~p", 
                        [TransportId, Reason])
    end,
    ok.

%% Remove transport from health monitoring
-spec unregister_transport_health_monitoring(atom()) -> ok.
unregister_transport_health_monitoring(TransportId) ->
    erlmcp_health_monitor:unregister_component(TransportId),
    ?LOG_DEBUG("Unregistered transport ~p from health monitoring", [TransportId]),
    ok.

%% Create transport-specific health check functions
-spec create_transport_health_check_function(atom(), pid()) -> fun(() -> healthy | unhealthy | degraded).
create_transport_health_check_function(Type, Pid) ->
    fun() ->
        try
            case is_process_alive(Pid) of
                false ->
                    unhealthy;
                true ->
                    % Transport-specific health checks
                    case Type of
                        stdio ->
                            check_stdio_transport_health(Pid);
                        tcp ->
                            check_tcp_transport_health(Pid);
                        http ->
                            check_http_transport_health(Pid);
                        _ ->
                            healthy % Default to healthy for unknown types
                    end
            end
        catch
            _:_ ->
                unhealthy
        end
    end.

%% Transport-specific health checks
-spec check_stdio_transport_health(pid()) -> healthy | unhealthy | degraded.
check_stdio_transport_health(Pid) ->
    % Check if stdio transport can handle basic operations
    case catch gen_server:call(Pid, {health_check}, 5000) of
        {ok, healthy} -> healthy;
        {ok, degraded} -> degraded;
        _ -> unhealthy
    end.

-spec check_tcp_transport_health(pid()) -> healthy | unhealthy | degraded.
check_tcp_transport_health(Pid) ->
    % Check TCP connection status
    case catch gen_server:call(Pid, {connection_status}, 5000) of
        {ok, connected} -> healthy;
        {ok, connecting} -> degraded;
        _ -> unhealthy
    end.

-spec check_http_transport_health(pid()) -> healthy | unhealthy | degraded.
check_http_transport_health(Pid) ->
    % Check HTTP transport responsiveness
    case catch gen_server:call(Pid, {server_status}, 5000) of
        {ok, running} -> healthy;
        {ok, degraded} -> degraded;
        _ -> unhealthy
    end.

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ?LOG_INFO("Initializing transport supervisor"),
    
    % Enhanced supervisor flags for better reliability and resilience
    SupFlags = #{
        strategy => one_for_one,  % Transport failures are isolated
        intensity => 15,          % Enhanced: Higher restart intensity for reliability
        period => 60,             % Period in seconds for restart intensity
        auto_shutdown => never    % Enhanced: Never auto-shutdown supervisor
    },
    
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
              Info#transport_info.config} || Info <- TransportInfos];
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
                    ?LOG_ERROR("Failed to stop transport ~p for restart: ~p", [TransportId, Reason]),
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
            HealthStatus = case erlmcp_health_monitor:get_component_health(TransportId) of
                not_found -> unknown;
                Status -> Status
            end,
            
            InfoMap = #{
                id => Info#transport_info.id,
                type => Info#transport_info.type,
                pid => Info#transport_info.pid,
                module => Info#transport_info.module,
                config => Info#transport_info.config,
                start_time => Info#transport_info.start_time,
                restart_count => Info#transport_info.restart_count,
                health_status => HealthStatus,
                uptime_seconds => case Info#transport_info.start_time of
                    undefined -> 0;
                    StartTime -> timer:now_diff(erlang:timestamp(), StartTime) div 1000000
                end
            },
            {ok, InfoMap};
        [] ->
            {error, not_found}
    end.

%%====================================================================
%% Enhanced Internal Functions
%%====================================================================

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

%% HTTP configuration validation
-spec validate_http_config(map()) -> ok | {error, term()}.
validate_http_config(Config) ->
    RequiredKeys = [port],
    case validate_required_keys(Config, RequiredKeys) of
        ok ->
            % Additional HTTP-specific validation
            case maps:get(port, Config) of
                Port when is_integer(Port), Port > 0, Port =< 65535 ->
                    ok;
                InvalidPort ->
                    {error, {invalid_port, InvalidPort}}
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
-spec attempt_transport_recovery(atom(), atom(), module(), map()) -> {ok, pid()} | {error, term()}.
attempt_transport_recovery(TransportId, Type, Module, Config) ->
    ?LOG_INFO("Attempting recovery for transport ~p", [TransportId]),
    
    % Wait a bit before retry
    timer:sleep(2000),
    
    % Try to start again
    case start_new_transport(TransportId, Type, Module, Config) of
        {ok, Pid} = Result ->
            ?LOG_INFO("Successfully recovered transport ~p with new PID ~p", [TransportId, Pid]),
            Result;
        {error, Reason} = Error ->
            ?LOG_ERROR("Recovery failed for transport ~p: ~p", [TransportId, Reason]),
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
            UpdatedInfo = Info#transport_info{restart_count = Info#transport_info.restart_count + 1},
            ets:insert(?TRANSPORT_TABLE, UpdatedInfo);
        [] ->
            ok
    end.

%% Update transport info with new PID
-spec update_transport_info(atom(), pid()) -> ok.
update_transport_info(TransportId, NewPid) ->
    case ets:lookup(?TRANSPORT_TABLE, TransportId) of
        [#transport_info{} = Info] ->
            UpdatedInfo = Info#transport_info{
                pid = NewPid,
                start_time = erlang:timestamp()
            },
            ets:insert(?TRANSPORT_TABLE, UpdatedInfo);
        [] ->
            ?LOG_WARNING("Cannot update transport info for unknown transport ~p", [TransportId])
    end,
    ok.
