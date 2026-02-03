-module(erlmcp_reload_coordinator).

-behaviour(gen_server).

%% API
-export([start_link/0,
         cluster_reload/2,
         sync_versions/1,
         get_cluster_versions/1,
         check_consistency/1,
         cluster_rollback/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Cluster Reload Coordinator (OTP 27-28)
%% Coordinates code reload across all nodes in cluster

-type module_name() :: module().
-type node_name() :: node().
-type reload_strategy() :: sync_all | quorum | local.

-record(state,
        {node_versions = #{} :: map(),         % Track versions per node
         pending_reloads = #{} :: map(),       % In-flight cluster reloads
         reload_locks = #{} :: map(),          % Prevent concurrent reloads
         sync_strategy = sync_all :: reload_strategy(),  % Default strategy
         node_monitor_refs = #{} :: map()}).   % Monitor refs for nodes

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Coordinate reload across all nodes in cluster
-spec cluster_reload(module_name(), reload_strategy()) -> ok | {error, term()}.
cluster_reload(Module, Strategy) ->
    gen_server:call(?MODULE, {cluster_reload, Module, Strategy}, 30000).

%% @doc Sync module versions across cluster
-spec sync_versions(module_name()) -> {ok, [node_name()]} | {error, term()}.
sync_versions(Module) ->
    gen_server:call(?MODULE, {sync_versions, Module}, 30000).

%% @doc Get versions across all nodes
-spec get_cluster_versions(module_name()) -> #{node_name() => binary()}.
get_cluster_versions(Module) ->
    gen_server:call(?MODULE, {get_cluster_versions, Module}).

%% @doc Check version consistency across cluster
-spec check_consistency(module_name()) -> boolean().
check_consistency(Module) ->
    gen_server:call(?MODULE, {check_consistency, Module}).

%% @doc Rollback across all nodes
-spec cluster_rollback(module_name()) -> ok | {error, term()}.
cluster_rollback(Module) ->
    gen_server:call(?MODULE, {cluster_rollback, Module}, 30000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Join pg scope for cluster coordination
    case pg:join_scope(erlmcp_reload_coordinators, self()) of
        ok ->
            ok;
        {error, {already_joined, _}} ->
            ok
    end,

    %% Monitor existing nodes
    Nodes = [node() | nodes()],
    State = lists:foldl(fun(Node, AccState) ->
                               monitor_node(Node, AccState)
                       end, #state{}, Nodes),

    logger:info("Reload coordinator started on ~p (strategy: ~p)",
                [node(), State#state.sync_strategy]),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                          {reply, term(), state()}.
handle_call({cluster_reload, Module, Strategy}, _From, State) ->
    Result = do_cluster_reload(Module, Strategy, State),
    {reply, Result, State};

handle_call({sync_versions, Module}, _From, State) ->
    Result = do_sync_versions(Module, State),
    {reply, Result, State};

handle_call({get_cluster_versions, Module}, _From, State) ->
    Result = do_get_cluster_versions(Module, State),
    {reply, Result, State};

handle_call({check_consistency, Module}, _From, State) ->
    Result = do_check_consistency(Module, State),
    {reply, Result, State};

handle_call({cluster_rollback, Module}, _From, State) ->
    Result = do_cluster_rollback(Module, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({reload_ack, Module, Node, Version}, State) ->
    NewState = handle_reload_ack(Module, Node, Version, State),
    {noreply, NewState};

handle_cast({reload_nack, Module, Node, Reason}, State) ->
    NewState = handle_reload_nack(Module, Node, Reason, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({nodeup, Node}, State) ->
    logger:info("Node up: ~p", [Node]),
    NewState = monitor_node(Node, State),
    {noreply, NewState};

handle_info({nodedown, Node}, State) ->
    logger:warning("Node down: ~p", [Node]),
    NewState = handle_node_down(Node, State),
    {noreply, NewState};

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    NewState = handle_process_down(MonitorRef, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{node_monitor_refs = Refs}) ->
    %% Demonitor all nodes
    maps:foreach(fun(_Node, Ref) -> erlang:demonitor(Ref, [flush]) end, Refs),

    %% Leave pg scope
    pg:leave_scope(erlmcp_reload_coordinators, self()),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Cluster Reload
%%====================================================================

%% @doc Coordinate cluster-wide reload
-spec do_cluster_reload(module_name(), reload_strategy(), state()) -> ok | {error, term()}.
do_cluster_reload(Module, Strategy, State) ->
    logger:info("Starting cluster reload for ~p (strategy: ~p)", [Module, Strategy]),

    %% Check for concurrent reload
    case maps:is_key(Module, State#state.reload_locks) of
        true ->
            {error, reload_in_progress};
        false ->
            %% Get target version
            case erlmcp_code_loader:get_module_md5(Module) of
                {ok, TargetVersion} ->
                    %% Acquire reload lock
                    ReloadId = make_ref(),

                    %% Send reload requests to all nodes
                    Nodes = [node() | nodes()],
                    send_reload_requests(Nodes, Module, TargetVersion, Strategy),

                    %% Wait for acknowledgments
                    case wait_for_acks(Module, Nodes, Strategy, State) of
                        {ok, SuccessfulNodes} ->
                            logger:info("Cluster reload completed for ~p on ~p nodes",
                                        [Module, length(SuccessfulNodes)]),
                            ok;
                        {error, Reason} ->
                            logger:error("Cluster reload failed for ~p: ~p", [Module, Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, {get_version_failed, Reason}}
            end
    end.

%% @doc Send reload requests to nodes
-spec send_reload_requests([node_name()], module_name(), binary(), reload_strategy()) -> ok.
send_reload_requests(Nodes, Module, Version, Strategy) ->
    lists:foreach(fun(Node) ->
                    spawn(fun() ->
                              reload_on_node(Node, Module, Version, Strategy)
                           end)
                  end, Nodes),
    ok.

%% @doc Reload on specific node
-spec reload_on_node(node_name(), module_name(), binary(), reload_strategy()) -> ok.
reload_on_node(Node, Module, Version, _Strategy) when Node =:= node() ->
    %% Local node
    case erlmcp_code_loader:safe_load(Module) of
        ok ->
            gen_server:cast(?MODULE, {reload_ack, Module, node(), Version});
        {error, Reason} ->
            gen_server:cast(?MODULE, {reload_nack, Module, node(), Reason})
    end;

reload_on_node(Node, Module, Version, Strategy) ->
    %% Remote node
    try
        case rpc:call(Node, erlmcp_code_loader, safe_load, [Module], 5000) of
            ok ->
                gen_server:cast(?MODULE, {reload_ack, Module, Node, Version});
            {error, Reason} ->
                gen_server:cast(?MODULE, {reload_nack, Module, Node, Reason});
            {badrpc, Reason} ->
                gen_server:cast(?MODULE, {reload_nack, Module, Node, {badrpc, Reason}})
        end
    catch
        _:Error ->
            gen_server:cast(?MODULE, {reload_nack, Module, Node, {exception, Error}})
    end.

%% @doc Wait for acknowledgments from nodes
-spec wait_for_acks(module_name(), [node_name()], reload_strategy(), state()) ->
                           {ok, [node_name()]} | {error, term()}.
wait_for_acks(Module, Nodes, Strategy, _State) ->
    %% Wait for responses with timeout
    wait_for_acks_loop(Module, Nodes, Strategy, [], 0).

wait_for_acks_loop(_Module, [], _Strategy, Acc, _Count) ->
    {ok, lists:reverse(Acc)};

wait_for_acks_loop(Module, Nodes, sync_all, Acc, Count) when Count > 30000 ->
    %% Timeout waiting for all nodes
    {error, {timeout, Acc, Nodes}};

wait_for_acks_loop(Module, Nodes, quorum, Acc, Count) when Count > 30000 ->
    %% Quorum: need majority
    TotalNodes = length(Acc) + length(Nodes),
    case length(Acc) >= (TotalNodes div 2 + 1) of
        true ->
            {ok, lists:reverse(Acc)};
        false ->
            {error, {quorum_not_reached, Acc, Nodes}}
    end;

wait_for_acks_loop(Module, Nodes, Strategy, Acc, Count) ->
    receive
        {reload_ack, Module, Node, _Version} ->
            %% Remove node from pending list
            NewNodes = lists:delete(Node, Nodes),
            wait_for_acks_loop(Module, NewNodes, Strategy, [Node | Acc], Count + 100);
        {reload_nack, Module, Node, _Reason} ->
            %% Remove node from pending list (failed)
            NewNodes = lists:delete(Node, Nodes),
            case Strategy of
                sync_all ->
                    %% All nodes must succeed
                    {error, {node_failed, Node}};
                quorum ->
                    %% Continue, need majority
                    wait_for_acks_loop(Module, NewNodes, Strategy, Acc, Count + 100);
                local ->
                    %% Only care about local node
                    case lists:member(node(), Acc) of
                        true -> {ok, Acc};
                        false -> {error, local_failed}
                    end
            end
    after 100 ->
        wait_for_acks_loop(Module, Nodes, Strategy, Acc, Count + 100)
    end.

%% @doc Sync versions across cluster
-spec do_sync_versions(module_name(), state()) -> {ok, [node_name()]} | {error, term()}.
do_sync_versions(Module, _State) ->
    Nodes = [node() | nodes()],

    %% Get versions from all nodes
    Versions = lists:map(fun(Node) ->
                            case rpc:call(Node, erlmcp_code_loader, get_module_md5, [Module], 5000) of
                                {ok, Version} -> {Node, Version};
                                {error, _} -> {Node, undefined};
                                {badrpc, _} -> {Node, undefined}
                            end
                        end, Nodes),

    %% Find most common version
    VersionCounts = lists:foldl(fun({_Node, Ver}, Acc) ->
                                       maps:update_with(Ver, fun(V) -> V + 1 end, 1, Acc)
                               end, #{}, Versions),

    case maps:to_list(VersionCounts) of
        [{_Ver, _Count}] ->
            %% All nodes have same version
            {ok, Nodes};
        [{TargetVer, _} | _] ->
            %% Reload nodes with different version
            TargetNodes = [Node || {Node, Ver} <- Versions, Ver =/= TargetVer],
            case TargetNodes of
                [] ->
                    {ok, Nodes};
                _ ->
                    logger:info("Syncing ~p to version ~p on nodes: ~p",
                               [Module, TargetVer, TargetNodes]),
                    %% Trigger reload on mismatched nodes
                    lists:foreach(fun(Node) ->
                                        spawn(fun() ->
                                                    reload_on_node(Node, Module, TargetVer, sync_all)
                                                 end)
                                  end, TargetNodes),
                    {ok, Nodes}
            end
    end.

%% @doc Get cluster versions
-spec do_get_cluster_versions(module_name(), state()) -> #{node_name() => binary()}.
do_get_cluster_versions(Module, _State) ->
    Nodes = [node() | nodes()],

    lists:foldl(fun(Node, Acc) ->
                    case rpc:call(Node, erlmcp_code_loader, get_module_md5, [Module], 5000) of
                        {ok, Version} ->
                            maps:put(Node, Version, Acc);
                        _ ->
                            Acc
                    end
                end, #{}, Nodes).

%% @doc Check consistency across cluster
-spec do_check_consistency(module_name(), state()) -> boolean().
do_check_consistency(Module, State) ->
    Versions = do_get_cluster_versions(Module, State),
    UniqueVersions = maps:values(Versions) |> sets:from_list(),
    sets:size(UniqueVersions) =:= 1.

%% @doc Cluster rollback
-spec do_cluster_rollback(module_name(), state()) -> ok | {error, term()}.
do_cluster_rollback(Module, _State) ->
    logger:warning("Starting cluster rollback for ~p", [Module]),

    %% Use rollback manager for version rollback
    case erlmcp_rollback_manager:rollback(Module, 1) of
        ok ->
            %% Propagate rollback to all nodes
            Nodes = nodes(),
            lists:foreach(fun(Node) ->
                            rpc:cast(Node, erlmcp_rollback_manager, rollback, [Module, 1])
                          end, Nodes),
            logger:info("Cluster rollback completed for ~p", [Module]),
            ok;
        {error, Reason} ->
            logger:error("Cluster rollback failed for ~p: ~p", [Module, Reason]),
            {error, Reason}
    end.

%%====================================================================
%% Internal Functions - Node Monitoring
%%====================================================================

%% @doc Monitor node
-spec monitor_node(node_name(), state()) -> state().
monitor_node(Node, #state{node_monitor_refs = Refs} = State) ->
    case maps:is_key(Node, Refs) of
        true ->
            State;
        false ->
            MonitorRef = erlang:monitor_node(Node, true),
            State#state{node_monitor_refs = maps:put(Node, MonitorRef, Refs)}
    end.

%% @doc Handle node down
-spec handle_node_down(node_name(), state()) -> state().
handle_node_down(Node, #state{node_versions = Versions,
                             node_monitor_refs = Refs} = State) ->
    %% Remove node from version tracking
    NewVersions = maps:remove(Node, Versions),
    NewRefs = maps:remove(Node, Refs),

    logger:warning("Node ~p removed from tracking", [Node]),

    State#state{node_versions = NewVersions,
                node_monitor_refs = NewRefs}.

%% @doc Handle process down
-spec handle_process_down(reference(), state()) -> state().
handle_process_down(MonitorRef, #state{node_monitor_refs = Refs} = State) ->
    %% Find and remove monitor ref
    NewRefs = maps:filter(fun(_Node, Ref) -> Ref =/= MonitorRef end, Refs),
    State#state{node_monitor_refs = NewRefs}.

%%====================================================================
%% Internal Functions - Ack Handling
%%====================================================================

%% @doc Handle reload acknowledgment
-spec handle_reload_ack(module_name(), node_name(), binary(), state()) -> state().
handle_reload_ack(Module, Node, Version, #state{node_versions = Versions} = State) ->
    logger:info("Reload ack from ~p for ~p (version: ~p)", [Node, Module, Version]),
    State#state{node_versions = maps:put({Module, Node}, Version, Versions)}.

%% @doc Handle reload negative acknowledgment
-spec handle_reload_nack(module_name(), node_name(), term(), state()) -> state().
handle_reload_nack(Module, Node, Reason, State) ->
    logger:error("Reload nack from ~p for ~p: ~p", [Node, Module, Reason]),
    State.
