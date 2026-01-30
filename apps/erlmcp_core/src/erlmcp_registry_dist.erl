-module(erlmcp_registry_dist).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    register_global/3,
    register_global/4,
    unregister_global/1,
    whereis_global/1,
    list_global_servers/0,
    list_global_transports/0,
    get_cluster_nodes/0,
    is_distributed/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type entity_type() :: server | transport.
-type entity_id() :: server_id() | transport_id().
-type entity_config() :: map().
-type transport_id() :: atom() | binary().

-export_type([entity_type/0, entity_id/0]).

%% State record
-record(dist_state, {
    enabled = false :: boolean(),
    nodes = [] :: [node()],
    node_monitors = #{} :: #{node() => reference()},
    heartbeat_interval = 10000 :: pos_integer(),
    heartbeat_ref :: reference() | undefined,
    split_brain_strategy = winner_takes_all :: winner_takes_all | oldest_node | configured_master
}).

-type state() :: #dist_state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register an entity globally across all cluster nodes
-spec register_global(entity_type(), entity_id(), entity_config()) -> ok | {error, term()}.
register_global(Type, EntityId, Config) when is_pid(Config) ->
    %% Legacy: Config is actually the Pid
    register_global(Type, EntityId, Config, #{});
register_global(Type, EntityId, Config) when is_map(Config) ->
    case maps:get(pid, Config, undefined) of
        undefined ->
            {error, missing_pid};
        Pid when is_pid(Pid) ->
            register_global(Type, EntityId, Pid, Config)
    end.

%% @doc Register an entity globally with explicit pid and config
-spec register_global(entity_type(), entity_id(), pid(), entity_config()) -> ok | {error, term()}.
register_global(Type, EntityId, EntityPid, Config) when is_pid(EntityPid) ->
    gen_server:call(?MODULE, {register_global, Type, EntityId, EntityPid, Config}).

%% @doc Unregister an entity from global registry
-spec unregister_global(entity_type() | {entity_type(), entity_id()}) -> ok.
unregister_global({Type, EntityId}) ->
    gen_server:call(?MODULE, {unregister_global, Type, EntityId});
unregister_global(Type) when is_atom(Type) ->
    %% Legacy: Type only - need entity ID
    {error, missing_entity_id}.

%% @doc Find the global location of an entity
-spec whereis_global({entity_type(), entity_id()}) -> {ok, {node(), pid(), entity_config()}} | {error, not_found}.
whereis_global({Type, EntityId}) ->
    gen_server:call(?MODULE, {whereis_global, Type, EntityId}).

%% @doc List all globally registered servers
-spec list_global_servers() -> [{server_id(), {node(), pid(), map()}}].
list_global_servers() ->
    gen_server:call(?MODULE, list_global_servers).

%% @doc List all globally registered transports
-spec list_global_transports() -> [{transport_id(), {node(), pid(), map()}}].
list_global_transports() ->
    gen_server:call(?MODULE, list_global_transports).

%% @doc Get list of connected cluster nodes
-spec get_cluster_nodes() -> [node()].
get_cluster_nodes() ->
    gen_server:call(?MODULE, get_cluster_nodes).

%% @doc Check if distributed mode is enabled
-spec is_distributed() -> boolean().
is_distributed() ->
    gen_server:call(?MODULE, is_distributed).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    ok = erlmcp_registry_utils:ensure_gproc_started(),

    %% Read cluster configuration
    Enabled = application:get_env(erlmcp_core, cluster_enabled, false),
    ClusterNodes = application:get_env(erlmcp_core, cluster_nodes, []),
    SplitBrainStrategy = application:get_env(erlmcp_core, split_brain_strategy, winner_takes_all),
    HeartbeatInterval = application:get_env(erlmcp_core, cluster_heartbeat_interval, 10000),

    State = #dist_state{
        enabled = Enabled,
        nodes = ClusterNodes,
        split_brain_strategy = SplitBrainStrategy,
        heartbeat_interval = HeartbeatInterval
    },

    case Enabled of
        true ->
            logger:info("Starting distributed registry (enabled=true, nodes=~p)", [ClusterNodes]),
            %% Connect to cluster nodes
            NewState = connect_to_cluster(State),
            %% Start heartbeat
            HeartbeatRef = erlang:send_after(HeartbeatInterval, self(), heartbeat),
            {ok, NewState#dist_state{heartbeat_ref = HeartbeatRef}};
        false ->
            logger:info("Starting distributed registry (enabled=false, local mode only)"),
            {ok, State}
    end.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call({register_global, Type, EntityId, EntityPid, Config}, _From, State) ->
    case State#dist_state.enabled of
        false ->
            {reply, {error, distributed_mode_disabled}, State};
        true ->
            %% Use gproc global registration with idempotency
            Key = {n, g, {mcp_global, Type, EntityId}},
            case gproc:where(Key) of
                undefined ->
                    try
                        %% Register on behalf of the entity process
                        gproc:reg_other(Key, EntityPid, Config),
                        gproc:monitor(Key),
                        logger:info("Registered global ~p ~p with pid ~p on node ~p",
                                   [Type, EntityId, EntityPid, node(EntityPid)]),
                        {reply, ok, State}
                    catch
                        error:badarg ->
                            %% Race condition: another process registered just now
                            logger:warning("Global registration race for ~p ~p", [Type, EntityId]),
                            {reply, {error, already_registered}, State}
                    end;
                ExistingPid when ExistingPid =:= EntityPid ->
                    %% Already registered by same process - this is OK (idempotent)
                    logger:debug("Global ~p ~p already registered by same pid ~p", [Type, EntityId, EntityPid]),
                    {reply, ok, State};
                ExistingPid ->
                    logger:warning("Global ~p ~p already registered with different pid ~p on node ~p (our pid: ~p on ~p)",
                                  [Type, EntityId, ExistingPid, node(ExistingPid), EntityPid, node(EntityPid)]),
                    {reply, {error, already_registered}, State}
            end
    end;

handle_call({unregister_global, Type, EntityId}, _From, State) ->
    case State#dist_state.enabled of
        false ->
            {reply, {error, distributed_mode_disabled}, State};
        true ->
            Key = {n, g, {mcp_global, Type, EntityId}},
            case gproc:where(Key) of
                undefined ->
                    {reply, ok, State};
                Pid ->
                    try
                        gproc:unreg_other(Key, Pid),
                        logger:info("Unregistered global ~p ~p", [Type, EntityId]),
                        {reply, ok, State}
                    catch
                        error:badarg ->
                            {reply, ok, State}
                    end
            end
    end;

handle_call({whereis_global, Type, EntityId}, _From, State) ->
    case State#dist_state.enabled of
        false ->
            {reply, {error, distributed_mode_disabled}, State};
        true ->
            Key = {n, g, {mcp_global, Type, EntityId}},
            case gproc:where(Key) of
                undefined ->
                    {reply, {error, not_found}, State};
                Pid ->
                    Node = node(Pid),
                    Config = gproc:get_value(Key, Pid),
                    {reply, {ok, {Node, Pid, Config}}, State}
            end
    end;

handle_call(list_global_servers, _From, State) ->
    case State#dist_state.enabled of
        false ->
            {reply, [], State};
        true ->
            %% Query all global servers using gproc select
            Pattern = {{{n, g, {mcp_global, server, '$1'}}, '$2', '$3'}, [], [{{'$1', {{'$2', '$3'}}}}]},
            Servers = gproc:select(Pattern),
            %% Add node information
            ServersWithNode = [{Id, {node(Pid), Pid, Config}} || {Id, {Pid, Config}} <- Servers],
            {reply, ServersWithNode, State}
    end;

handle_call(list_global_transports, _From, State) ->
    case State#dist_state.enabled of
        false ->
            {reply, [], State};
        true ->
            %% Query all global transports using gproc select
            Pattern = {{{n, g, {mcp_global, transport, '$1'}}, '$2', '$3'}, [], [{{'$1', {{'$2', '$3'}}}}]},
            Transports = gproc:select(Pattern),
            %% Add node information
            TransportsWithNode = [{Id, {node(Pid), Pid, Config}} || {Id, {Pid, Config}} <- Transports],
            {reply, TransportsWithNode, State}
    end;

handle_call(get_cluster_nodes, _From, State) ->
    case State#dist_state.enabled of
        false ->
            {reply, [node()], State};
        true ->
            %% Return all connected nodes plus self
            AllNodes = [node() | nodes()],
            {reply, AllNodes, State}
    end;

handle_call(is_distributed, _From, State) ->
    {reply, State#dist_state.enabled, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(heartbeat, State = #dist_state{enabled = false}) ->
    {noreply, State};

handle_info(heartbeat, State = #dist_state{enabled = true, heartbeat_interval = Interval}) ->
    %% Check node connectivity
    ConnectedNodes = nodes(),
    ExpectedNodes = State#dist_state.nodes,

    %% Log disconnected nodes
    DisconnectedNodes = ExpectedNodes -- ConnectedNodes,
    case DisconnectedNodes of
        [] -> ok;
        _ -> logger:warning("Cluster nodes disconnected: ~p", [DisconnectedNodes])
    end,

    %% Schedule next heartbeat
    HeartbeatRef = erlang:send_after(Interval, self(), heartbeat),
    {noreply, State#dist_state{heartbeat_ref = HeartbeatRef}};

handle_info({nodedown, Node}, State) ->
    logger:warning("Cluster node down: ~p", [Node]),
    %% Remove node monitor
    NewMonitors = maps:remove(Node, State#dist_state.node_monitors),
    {noreply, State#dist_state{node_monitors = NewMonitors}};

handle_info({nodeup, Node}, State) ->
    logger:info("Cluster node up: ~p", [Node]),
    %% Add node monitor
    MonitorRef = erlang:monitor_node(Node, true),
    NewMonitors = maps:put(Node, MonitorRef, State#dist_state.node_monitors),
    {noreply, State#dist_state{node_monitors = NewMonitors}};

handle_info({gproc, unreg, _Ref, {n, g, {mcp_global, Type, EntityId}}}, State) ->
    logger:warning("Global ~p ~p unregistered (process died)", [Type, EntityId]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Cancel heartbeat
    case State#dist_state.heartbeat_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    %% Demonitor all nodes
    lists:foreach(fun(Node) ->
        erlang:monitor_node(Node, false)
    end, State#dist_state.nodes),

    logger:info("Distributed registry terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec connect_to_cluster(state()) -> state().
connect_to_cluster(State = #dist_state{nodes = []}) ->
    State;
connect_to_cluster(State = #dist_state{nodes = ClusterNodes}) ->
    %% Set distributed cookie if configured
    case application:get_env(erlmcp_core, cluster_cookie) of
        {ok, Cookie} when is_atom(Cookie) ->
            erlang:set_cookie(node(), Cookie),
            logger:info("Set cluster cookie for distributed mode");
        _ ->
            ok
    end,

    %% Connect to each node
    Monitors = lists:foldl(fun(Node, Acc) ->
        case net_kernel:connect_node(Node) of
            true ->
                logger:info("Connected to cluster node: ~p", [Node]),
                erlang:monitor_node(Node, true),
                MonitorRef = make_ref(),
                maps:put(Node, MonitorRef, Acc);
            false ->
                logger:warning("Failed to connect to cluster node: ~p", [Node]),
                Acc;
            ignored ->
                logger:warning("Node connection ignored (not distributed): ~p", [Node]),
                Acc
        end
    end, #{}, ClusterNodes),

    State#dist_state{node_monitors = Monitors}.
