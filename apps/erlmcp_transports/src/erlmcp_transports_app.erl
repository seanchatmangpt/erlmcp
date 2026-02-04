%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_transports_app - Transports Application Callback
%%%
%%% OTP application callback for the transports subsystem.
%%% Starts the transport supervision tree which manages all transport
%%% workers (stdio, tcp, http, ws, sse).
%%%
%%% == Ontology ==
%%% Generated from ggen/ontology/instances/transports_application.ttl
%%%
%%% == Startup Sequence ==
%%% 1. Application starts (kernel launches this callback)
%%% 2. Transport supervisor starts (erlmcp_transport_sup)
%%% 3. Health monitor starts (permanent worker)
%%% 4. Transports are started dynamically via start_child/3
%%%
%%% == Shutdown Sequence ==
%%% 1. prep_stop/1 is called (stop accepting new connections)
%%% 2. Connection pools are drained gracefully
%%% 3. Supervisor terminates all children
%%% 4. stop/1 is called for final cleanup
%%%
%%% == Dependencies ==
%%% - kernel: OTP kernel application
%%% - stdlib: OTP standard library
%%% - ssl: SSL/TLS support
%%% - inets: HTTP client
%%% - erlmcp_core: Core MCP protocol implementation
%%% - opentelemetry_api: Observability
%%% - gun: HTTP/2 client
%%% - ranch: TCP acceptor pool
%%% - cowboy: HTTP server
%%% - poolboy: Connection pooling
%%%
%%% @end
%%% @private
%%%-------------------------------------------------------------------
-module(erlmcp_transports_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).
-export([config_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type start_type() :: normal | {takeover, node()} | {failover, node()}.
-type state() :: #{}.
-type app_env() :: #{atom() => term()}.

%%====================================================================
%% Application Callbacks
%%====================================================================

%% @doc Start the transports application.
%%
%% This callback is invoked by the application controller when the
%% application is started. It initializes the supervision tree for
%% all transport workers.
%%
%% The supervision tree structure:
%% ```
%% erlmcp_transport_sup (one_for_one)
%%   |-- erlmcp_transport_health (permanent)
%%   |-- erlmcp_transport_stdio (temporary, dynamic)
%%   |-- erlmcp_transport_tcp (transient, dynamic)
%%   |-- erlmcp_transport_http (transient, dynamic)
%%   |-- erlmcp_transport_ws (transient, dynamic)
%%   '-- erlmcp_transport_sse (transient, dynamic)
%% '''
%%
%% @param StartType - normal, {takeover, Node}, or {failover, Node}
%% @param StartArgs - empty list (no startup args)
%% @returns {ok, Pid} on success, {error, Reason} on failure
%%
%% @see erlmcp_transport_sup:start_link/0
-spec start(start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    ?LOG_INFO(#{what => erlmcp_transports_start,
                application => erlmcp_transports,
                version => "2.1.0"}),

    %% Validate OTP version requirement
    validate_otp_version(),

    %% Validate erlmcp_core is available
    validate_core_dependency(),

    %% Log configuration
    log_application_config(),

    %% Start the supervisor
    case erlmcp_transport_sup:start_link() of
        {ok, Pid} ->
            ?LOG_INFO(#{what => erlmcp_transports_started_successfully,
                        supervisor_pid => Pid}),
            {ok, Pid};
        {error, Reason} = Error ->
            ?LOG_ERROR(#{what => erlmcp_transports_start_failed,
                         reason => Reason,
                         error_type => startup_failure}),
            Error
    end.

%% @doc Stop the transports application.
%%
%% This callback is invoked when the application is stopped.
%% Called after prep_stop/1 and all children have terminated.
%%
%% @param _State - the state returned from prep_stop/1
%% @returns ok
%%
-spec stop(term()) -> ok.
stop(_State) ->
    ?LOG_INFO(#{what => erlmcp_transports_stopped}),
    ok.

%% @doc Prepare for graceful shutdown.
%%
%% This callback is invoked when the application is about to stop.
%% It initiates graceful shutdown procedures:
%% - Stop accepting new connections
%% - Drain existing connection pools
%% - Allow ongoing requests to complete
%%
%% @param State - the current application state
%% @returns State to pass to stop/1
%%
-spec prep_stop(state()) -> state().
prep_stop(State) ->
    ?LOG_INFO(#{what => erlmcp_transports_prep_stop,
                action => initiating_graceful_shutdown}),

    %% Step 1: Stop accepting new connections
    stop_accepting_new_connections(),

    %% Step 2: Drain connection pools
    drain_connection_pools(),

    %% Step 3: Wait for graceful drain period (configurable)
    drain_wait_period(),

    ?LOG_INFO(#{what => erlmcp_transports_prep_stop_complete,
                action => ready_for_shutdown}),
    State.

%% @doc Handle runtime configuration changes.
%%
%% This callback is invoked when the application environment is
%% changed at runtime (e.g., via application:set_env/3).
%%
%% @param Changed - parameters that changed
%% @param New - new configuration values
%% @param Removed - parameters that were removed
%% @returns ok
%%
-spec config_change(term(), term(), term()) -> ok.
config_change(_Changed, _New, _Removed) ->
    ?LOG_INFO(#{what => erlmcp_transports_config_change,
                action => runtime_configuration_updated}),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Validate OTP version meets minimum requirement (OTP 28+)
-spec validate_otp_version() -> ok.
validate_otp_version() ->
    MajorVersion = erlang:system_info(otp_release),
    try
        VersionNum = list_to_integer(MajorVersion),
        case VersionNum >= 28 of
            true ->
                ?LOG_INFO(#{what => otp_version_valid,
                            otp_version => MajorVersion});
            false ->
                ?LOG_WARNING(#{what => otp_version_below_recommended,
                               otp_version => MajorVersion,
                               recommended => "28+"})
        end
    catch
        error:badarg ->
            %% Non-numeric version string (e.g., "28.0")
            ?LOG_INFO(#{what => otp_version_check,
                        otp_version => MajorVersion})
    end.

%% @private
%% @doc Validate erlmcp_core dependency is available
-spec validate_core_dependency() -> ok.
validate_core_dependency() ->
    case application:ensure_all_started(erlmcp_core) of
        {ok, _} ->
            ?LOG_INFO(#{what => core_dependency_validated,
                        dependency => erlmcp_core});
        {error, {already_started, erlmcp_core}} ->
            ?LOG_DEBUG(#{what => core_dependency_already_started,
                         dependency => erlmcp_core});
        {error, Reason} ->
            ?LOG_ERROR(#{what => core_dependency_validation_failed,
                         dependency => erlmcp_core,
                         reason => Reason}),
            error({missing_dependency, erlmcp_core})
    end.

%% @private
%% @doc Log application configuration for debugging
-spec log_application_config() -> ok.
log_application_config() ->
    Config = application:get_all_env(erlmcp_transports),
    ?LOG_DEBUG(#{what => application_config,
                  config => redact_secrets(Config)}),
    ok.

%% @private
%% @doc Redact sensitive values from configuration for logging
-spec redact_secrets(app_env()) -> app_env().
redact_secrets(Config) ->
    maps:map(fun
        ({password, _}, _) -> {password, "***REDACTED***"};
        ({token, _}, _) -> {token, "***REDACTED***"};
        ({secret, _}, _) -> {secret, "***REDACTED***"};
        ({api_key, _}, _) -> {api_key, "***REDACTED***"};
        (_, Value) -> Value
    end, maps:from_list(Config)).

%% @private
%% @doc Stop accepting new connections
-spec stop_accepting_new_connections() -> ok.
stop_accepting_new_connections() ->
    try
        Pid = whereis(erlmcp_transport_sup),
        case Pid of
            undefined ->
                ?LOG_DEBUG(#{what => transport_sup_not_found,
                             reason => not_started});
            _ ->
                ?LOG_INFO(#{what => stopping_transport_accept}),
                erlmcp_transport_sup:stop_accepting()
        end
    catch
        _:Error ->
            ?LOG_WARNING(#{what => stop_accepting_failed,
                           error => Error}),
            ok
    end.

%% @private
%% @doc Drain existing connection pools
-spec drain_connection_pools() -> ok.
drain_connection_pools() ->
    try
        %% Drain connection pool if exists
        Pools = [erlmcp_connection_pool, erlmcp_transport_pool],
        lists:foreach(fun(PoolName) ->
            Pid = whereis(PoolName),
            case Pid of
                undefined ->
                    ok;
                _ ->
                    ?LOG_INFO(#{what => draining_pool,
                                pool => PoolName}),
                    try
                        PoolName:drain()
                    catch
                        _:_ -> ok
                    end
            end
        end, Pools)
    catch
        _:Error ->
            ?LOG_WARNING(#{what => drain_pools_failed,
                           error => Error}),
            ok
    end.

%% @private
%% @doc Wait for drain period to allow connections to complete
-spec drain_wait_period() -> ok.
drain_wait_period() ->
    %% Get drain timeout from config or use default 5 seconds
    DrainTimeout = application:get_env(erlmcp_transports,
                                       drain_timeout,
                                       5000),
    ?LOG_DEBUG(#{what => drain_wait,
                 timeout_ms => DrainTimeout}),
    timer:sleep(DrainTimeout),
    ok.
