-module(erlmcp_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    start_server/2, stop_server/1,
    start_transport/3, stop_transport/1
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
            {ok, ServerPid};
        {error, _} = Error ->
            Error
    end.

-spec stop_server(atom()) -> ok | {error, term()}.
stop_server(ServerId) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {ServerPid, _Config}} ->
            ok = erlmcp_registry:unregister_server(ServerId),
            supervisor:terminate_child(erlmcp_server_sup, ServerPid);
        {error, not_found} ->
            ok
    end.

%% Transport management API (stubs for future transport layer)
-spec start_transport(atom(), atom(), #{}) -> {ok, pid()} | {error, term()}.
start_transport(_TransportId, _Type, _Config) ->
    {error, not_implemented}.

-spec stop_transport(atom()) -> ok | {error, term()}.
stop_transport(_TransportId) ->
    {error, not_implemented}.

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
