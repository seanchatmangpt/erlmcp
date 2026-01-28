-module(erlmcp_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    start_server/2, stop_server/1,
    start_transport/3, stop_transport/1,
    start_stdio_server/0, start_stdio_server/1, stop_stdio_server/0
]).
-export([init/1]).

-include("erlmcp.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Server management API
-spec start_server(atom(), #{}) -> {ok, pid()} | {error, term()}.
start_server(ServerId, Config) ->
    case supervisor:start_child(erlmcp_server_sup, [ServerId, Config]) of
        {ok, ServerPid} ->
            % Register with registry
            ok = erlmcp_registry:register_server(ServerId, ServerPid, Config),
            
            % Register with recovery manager and health monitor
            RecoveryPolicy = maps:get(recovery_policy, Config, #{}),
            ok = erlmcp_recovery_manager:register_component(ServerId, ServerPid, RecoveryPolicy),
            ok = erlmcp_health_monitor:register_component(ServerId, ServerPid),
            
            {ok, ServerPid};
        {error, _} = Error ->
            Error
    end.

-spec stop_server(atom()) -> ok | {error, term()}.
stop_server(ServerId) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {ServerPid, _Config}} ->
            % Unregister from all systems
            ok = erlmcp_registry:unregister_server(ServerId),
            ok = erlmcp_recovery_manager:unregister_component(ServerId),
            ok = erlmcp_health_monitor:unregister_component(ServerId),
            supervisor:terminate_child(erlmcp_server_sup, ServerPid);
        {error, not_found} ->
            ok
    end.

%% Transport management API
-spec start_transport(atom(), atom(), #{}) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type, Config) ->
    case supervisor:start_child(erlmcp_transport_sup, [TransportId, Type, Config]) of
        {ok, TransportPid} ->
            TransportConfig = Config#{type => Type},
            ok = erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig),
            
            % Register with recovery manager and health monitor
            RecoveryPolicy = maps:get(recovery_policy, Config, #{}),
            ok = erlmcp_recovery_manager:register_component(TransportId, TransportPid, RecoveryPolicy),
            ok = erlmcp_health_monitor:register_component(TransportId, TransportPid),
            
            {ok, TransportPid};
        {error, _} = Error ->
            Error
    end.

-spec stop_transport(atom()) -> ok | {error, term()}.
stop_transport(TransportId) ->
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {TransportPid, _Config}} ->
            % Unregister from all systems
            ok = erlmcp_registry:unregister_transport(TransportId),
            ok = erlmcp_recovery_manager:unregister_component(TransportId),
            ok = erlmcp_health_monitor:unregister_component(TransportId),
            supervisor:terminate_child(erlmcp_transport_sup, TransportPid);
        {error, not_found} ->
            ok
    end.

%% Legacy stdio server support (for backward compatibility)
-spec start_stdio_server() -> {ok, pid()} | {error, term()}.
start_stdio_server() ->
    start_stdio_server(#{}).

-spec start_stdio_server(map()) -> {ok, pid()} | {error, term()}.
start_stdio_server(Options) ->
    % Use the legacy stdio server for now until full implementation
    case erlmcp_stdio_server:start_link(Options) of
        {ok, ServerPid} ->
            {ok, ServerPid};
        Error ->
            Error
    end.

-spec stop_stdio_server() -> ok.
stop_stdio_server() ->
    _ = stop_transport(default_stdio_transport),
    _ = stop_server(default_stdio_server),
    ok.

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% v1.3.0: Bulkhead Supervision Tree Design
    %%
    %% Strategy: Use rest_for_one at top level with isolated subsystems
    %% - Registry subsystem (core infrastructure)
    %% - Protocol servers subsystem (client/server implementations)
    %% - Transports subsystem (I/O layer)
    %% - Monitoring subsystem (observability - can fail without affecting core)
    %%
    %% Each subsystem uses one_for_one or rest_for_one internally to prevent cascades

    SupFlags = #{
        strategy => rest_for_one,  % If dependency fails, restart dependents only
        intensity => 5,
        period => 60
    },

    % Ordered child specs - dependencies must start before dependents
    ChildSpecs = [
        %% ================================================================
        %% TIER 1: REGISTRY SUBSYSTEM (No dependencies)
        %% Failure: Restarts registry shard in isolation
        %% Impact: New messages fail to route until recovery
        %% Recovery: Registry reconnects automatically via gproc
        %% ================================================================
        #{
            id => erlmcp_registry_sup,
            start => {erlmcp_registry_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_registry_sup]
        },

        %% ================================================================
        %% TIER 2: INFRASTRUCTURE (Depends on Registry)
        %% Infrastructure components (sessions, tasks, resources)
        %% Failure: Restarts infrastructure subsystem
        %% Impact: New sessions/tasks fail; existing connections continue
        %% Recovery: Automatic via supervisor
        %% ================================================================
        #{
            id => erlmcp_infrastructure_sup,
            start => {erlmcp_infrastructure_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_infrastructure_sup]
        },

        %% ================================================================
        %% TIER 3: PROTOCOL SERVERS SUBSYSTEM (Depends on Registry + Infrastructure)
        %% Manages MCP client and server instances
        %% Failure: Restarts all servers (graceful reconnect via transport)
        %% Impact: In-flight requests are lost; clients reconnect
        %% Recovery: Automatic via supervisor
        %% ================================================================
        #{
            id => erlmcp_server_sup,
            start => {erlmcp_server_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_server_sup]
        },

        %% ================================================================
        %% TIER 4: TRANSPORTS SUBSYSTEM (Depends on Registry + Servers)
        %% Manages stdio, TCP, HTTP, WebSocket transports
        %% Failure: Restarts transport layer (clients reconnect automatically)
        %% Impact: Network connections are lost temporarily
        %% Recovery: Automatic; clients retry connections
        %% ================================================================
        #{
            id => erlmcp_transport_sup,
            start => {erlmcp_transport_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_transport_sup]
        },

        %% ================================================================
        %% TIER 5: OBSERVABILITY SUBSYSTEM (Optional - independent)
        %% Monitoring, health checks, metrics, dashboards
        %% Failure: Does NOT affect protocol layer (isolated)
        %% Impact: Monitoring data may be incomplete
        %% Recovery: Automatic via supervisor
        %% ================================================================
        #{
            id => erlmcp_monitoring_sup,
            start => {erlmcp_monitoring_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_monitoring_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
