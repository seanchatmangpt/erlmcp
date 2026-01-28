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
    %% v1.4.0: Simplified 3-Tier Supervision Tree
    %%
    %% Strategy: one_for_one - no cascading failures between subsystems
    %% - TIER 1: Core (registry + infrastructure consolidated)
    %% - TIER 2: Protocol (servers with simple_one_for_one)
    %% - TIER 3: Observability (isolated - failures don't affect core)
    %%
    %% Changes from v1.3.0:
    %% - Merged erlmcp_registry_sup + erlmcp_infrastructure_sup → erlmcp_core_sup
    %% - Removed erlmcp_transport_sup (moved to erlmcp_transports app)
    %% - Renamed erlmcp_monitoring_sup → erlmcp_observability_sup
    %% - Changed strategy: rest_for_one → one_for_one (no cascades)

    SupFlags = #{
        strategy => one_for_one,  % Each subsystem fails independently
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% ================================================================
        %% TIER 1: CORE (Registry + Infrastructure)
        %% Foundation layer with no external dependencies
        %% Failure: Individual components restart in isolation
        %% Impact: New registrations/sessions may fail during recovery
        %% Recovery: Automatic via one_for_one strategy
        %% ================================================================
        #{
            id => erlmcp_core_sup,
            start => {erlmcp_core_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_core_sup]
        },

        %% ================================================================
        %% TIER 2: PROTOCOL SERVERS (simple_one_for_one)
        %% Dynamic MCP server instances
        %% Failure: Individual server failures don't affect others
        %% Impact: In-flight requests to failed server are lost
        %% Recovery: Clients can reconnect to new server instance
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
        %% TIER 3: OBSERVABILITY (Isolated)
        %% Monitoring, health checks, metrics, dashboards
        %% Failure: Does NOT affect core or protocol layers
        %% Impact: Monitoring data may be incomplete during recovery
        %% Recovery: Automatic via one_for_one strategy
        %% ================================================================
        #{
            id => erlmcp_observability_sup,
            start => {erlmcp_observability_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_observability_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
