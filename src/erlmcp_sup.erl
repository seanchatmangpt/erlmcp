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
    SupFlags = #{
        strategy => one_for_all,  % If critical components fail, restart everything
        intensity => 5,           % Enhanced: Increased restart intensity for recovery
        period => 60
    },

    % Core infrastructure components with recovery integration
    ChildSpecs = [
        % Health monitor - system health monitoring (start first)
        #{
            id => erlmcp_health_monitor,
            start => {erlmcp_health_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_health_monitor]
        },

        % Recovery manager - failure recovery coordination (start second)
        #{
            id => erlmcp_recovery_manager,
            start => {erlmcp_recovery_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_recovery_manager]
        },

        % Session manager - HTTP session management and tracking
        #{
            id => erlmcp_session_manager,
            start => {erlmcp_session_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_manager]
        },

        % Task manager - MCP tasks API / async job queue
        #{
            id => erlmcp_task_manager,
            start => {erlmcp_task_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_task_manager]
        },

        % Resource subscriptions manager - handles resource update subscriptions
        #{
            id => erlmcp_resource_subscriptions,
            start => {erlmcp_resource_subscriptions, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_resource_subscriptions]
        },

        % SSE Event Store - maintains recent events for stream resumability
        #{
            id => erlmcp_sse_event_store,
            start => {erlmcp_sse_event_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_sse_event_store]
        },

        % Icon Cache - caches icon metadata with TTL enforcement (Gap #37)
        #{
            id => erlmcp_icon_cache,
            start => {erlmcp_icon_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_icon_cache]
        },

        % Registry - central message router with recovery registration
        #{
            id => erlmcp_registry,
            start => {erlmcp_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_registry]
        },

        % Server supervisor - manages server instances
        #{
            id => erlmcp_server_sup,
            start => {erlmcp_server_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_server_sup]
        },

        % Transport supervisor - manages transport instances
        #{
            id => erlmcp_transport_sup,
            start => {erlmcp_transport_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_transport_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
