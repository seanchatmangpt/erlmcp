-module(erlmcp_sup).

-behaviour(supervisor).

-export([start_link/0, start_server/2, stop_server/1, start_transport/3, stop_transport/1,
         list_transports/0]).
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
        {ok, ServerPid} when is_pid(ServerPid) ->
            % Register with registry
            case erlmcp_registry:register_server(ServerId, ServerPid, Config) of
                ok ->
                    {ok, ServerPid};
                {error, Reason} ->
                    % Registry failed - clean up server
                    _ = supervisor:terminate_child(erlmcp_server_sup, ServerPid),
                    {error, {registry_failed, Reason}}
            end;
        {error, Reason} = Error ->
            Error
    end.

-spec stop_server(atom()) -> ok | {error, term()}.
stop_server(ServerId) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {ServerPid, _Config}} ->
            ok = erlmcp_registry:unregister_server(ServerId),
            case supervisor:terminate_child(erlmcp_server_sup, ServerPid) of
                ok ->
                    ok;
                {error, not_found} ->
                    ok;  % Already terminated
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, not_found} ->
            ok
    end.

%% Transport management API
%% @doc Start a transport dynamically via erlmcp_transport_sup
%% Delegates to the transport supervisor in erlmcp_transports app
-spec start_transport(atom(), atom(), #{}) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type, Config) ->
    case erlang:whereis(erlmcp_transport_sup) of
        undefined ->
            {error, transport_sup_not_running};
        _SupPid ->
            % Delegate to transport supervisor
            % erlmcp_transport_sup:start_child handles module resolution
            % and child spec construction
            erlmcp_transport_sup:start_child(TransportId, Type, Config)
    end.

%% @doc Stop a transport dynamically
%% Terminates and deletes the child from transport supervisor
-spec stop_transport(atom()) -> ok | {error, term()}.
stop_transport(TransportId) ->
    case erlang:whereis(erlmcp_transport_sup) of
        undefined ->
            {error, transport_sup_not_running};
        SupPid ->
            % Terminate the child process
            case supervisor:terminate_child(SupPid, TransportId) of
                ok ->
                    % Delete the child spec after termination
                    case supervisor:delete_child(SupPid, TransportId) of
                        ok ->
                            ok;
                        {error, not_found} ->
                            ok  % Already deleted
                    end;
                {error, not_found} ->
                    ok;  % Already terminated
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc List all active transports
%% Returns list of {TransportId, Pid, Type} tuples
-spec list_transports() -> [{atom(), pid() | undefined, atom()}].
list_transports() ->
    case erlang:whereis(erlmcp_transport_sup) of
        undefined ->
            [];
        SupPid ->
            Children = supervisor:which_children(SupPid),
            lists:map(fun({Id, Pid, _Type, Modules}) ->
                         % Extract transport type from module name
                         % Modules is typically [erlmcp_transport_stdio] or similar
                         Module =
                             case Modules of
                                 [Mod] ->
                                     Mod;
                                 Mod when is_atom(Mod) ->
                                     Mod;
                                 _ ->
                                     unknown
                             end,
                         Type = transport_type_from_module(Module),
                         {Id, Pid, Type}
                      end,
                      Children)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Extract transport type from module name
%% e.g., erlmcp_transport_stdio -> stdio
-spec transport_type_from_module(module()) -> atom().
transport_type_from_module(erlmcp_transport_stdio) ->
    stdio;
transport_type_from_module(erlmcp_transport_tcp) ->
    tcp;
transport_type_from_module(erlmcp_transport_http) ->
    http;
transport_type_from_module(erlmcp_transport_ws) ->
    ws;
transport_type_from_module(erlmcp_transport_sse) ->
    sse;
transport_type_from_module(_) ->
    unknown.

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% v2.1.0: Fixed 3-Tier Supervision Tree
    %%
    %% Strategy: one_for_one - individual subsystem restart on failure
    %% - TIER 1: Core (registry + infrastructure) - isolated restart
    %% - TIER 2: Protocol (servers with simple_one_for_one) - isolated restart
    %% - TIER 3: Observability (isolated) - isolated restart
    %%
    %% Critical Fix: Changed from one_for_all to one_for_one
    %% Now failures are isolated and don't cause mass restarts
    %% Each subsystem recovers independently of others
    %%
    %% Changes from v1.4.0:
    %% - Fixed strategy: one_for_all -> one_for_one (proper isolation)
    %% - Now individual components restart independently
    SupFlags =
        #{strategy => one_for_one,  % Individual subsystem restart on failure
          intensity => 5,
          period => 60},

    ChildSpecs =
        [%% ================================================================
         %% TIER 1: CORE (Registry + Infrastructure)
         %% Foundation layer with no external dependencies
         %% Failure: Individual components restart in isolation
         %% Impact: New registrations/sessions may fail during recovery
         %% Recovery: Automatic via one_for_one strategy
         %% ================================================================
         #{id => erlmcp_core_sup,
           start => {erlmcp_core_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_core_sup]},
         %% ================================================================
         %% TIER 2: PROTOCOL SERVERS (simple_one_for_one)
         %% Dynamic MCP server instances
         %% Failure: Individual server failures don't affect others
         %% Impact: In-flight requests to failed server are lost
         %% Recovery: Clients can reconnect to new server instance
         %% ================================================================
         #{id => erlmcp_server_sup,
           start => {erlmcp_server_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_server_sup]},
         %% ================================================================
         %% TIER 3: OBSERVABILITY (Isolated)
         %% Monitoring, health checks, metrics, dashboards
         %% Failure: Does NOT affect core or protocol layers
         %% Impact: Monitoring data may be incomplete during recovery
         %% Recovery: Automatic via one_for_one strategy
         %% ================================================================
         #{id => erlmcp_observability_sup,
           start => {erlmcp_observability_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_observability_sup]}],

    {ok, {SupFlags, ChildSpecs}}.
