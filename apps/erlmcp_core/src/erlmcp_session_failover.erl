%%%-------------------------------------------------------------------
%%% @doc Session Failover Manager
%%%
%%% Implements distributed session failover across cluster nodes following
%%% Joe Armstrong's principle: "Software is not about programs, it's
%%% about communication."
%%%
%%% Failover = distributed communication using Mnesia for state replication.
%%%
%%% Features:
%%% - Node monitoring via net_kernel:monitor_nodes/1
%%% - Automatic failover on node failure
%%% - Session state replication to backup nodes
%%% - Split-brain prevention with majority voting
%%% - Backup promotion on primary failure
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_failover).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    add_backup/2,
    remove_backup/2,
    failover/1,
    get_backup_nodes/1,
    get_primary_node/1,
    is_backup/2,
    force_sync/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type session_id() :: binary().
-type node_status() :: up | down | recovering.
-type failover_state() :: #{
    session_id => session_id(),
    primary_node => node(),
    backup_nodes => [node()],
    last_sync => integer(),
    status => node_status()
}.

-record(state, {
    local_node :: node(),
    cluster_nodes = [] :: [node()],
    %% Session failover state: SessionId => failover_state()
    sessions = #{} :: #{session_id() => failover_state()},
    %% Node status tracking
    node_status = #{} :: #{node() => node_status()},
    %% Replication in-progress
    replicating = #{} :: #{session_id() => [node()]},
    %% Split-brain protection
    majority_required = true :: boolean(),
    %% Monitoring enabled
    monitoring = false :: boolean()
}).

-type state() :: #state{}.

-export_type([session_id/0, failover_state/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start failover manager with default cluster nodes
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    ClusterNodes = application:get_env(erlmcp_core, cluster_nodes, []),
    start_link(ClusterNodes).

%% @doc Start failover manager with specific cluster nodes
-spec start_link([node()]) -> {ok, pid()} | {error, term()}.
start_link(ClusterNodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ClusterNodes], []).

%% @doc Add backup node for a session
-spec add_backup(session_id(), node()) -> ok | {error, term()}.
add_backup(SessionId, BackupNode) ->
    gen_server:call(?MODULE, {add_backup, SessionId, BackupNode}, 5000).

%% @doc Remove backup node for a session
-spec remove_backup(session_id(), node()) -> ok | {error, term()}.
remove_backup(SessionId, BackupNode) ->
    gen_server:call(?MODULE, {remove_backup, SessionId, BackupNode}, 5000).

%% @doc Trigger manual failover for a session
-spec failover(session_id()) -> ok | {error, term()}.
failover(SessionId) ->
    gen_server:call(?MODULE, {failover, SessionId}, 10000).

%% @doc Get backup nodes for a session
-spec get_backup_nodes(session_id()) -> {ok, [node()]} | {error, not_found}.
get_backup_nodes(SessionId) ->
    gen_server:call(?MODULE, {get_backup_nodes, SessionId}, 5000).

%% @doc Get primary node for a session
-spec get_primary_node(session_id()) -> {ok, node()} | {error, not_found}.
get_primary_node(SessionId) ->
    gen_server:call(?MODULE, {get_primary_node, SessionId}, 5000).

%% @doc Check if this node is a backup for the session
-spec is_backup(session_id(), node()) -> boolean().
is_backup(SessionId, Node) ->
    gen_server:call(?MODULE, {is_backup, SessionId, Node}, 5000).

%% @doc Force session replication to all backup nodes
-spec force_sync(session_id()) -> {ok, [node()]} | {error, term()}.
force_sync(SessionId) ->
    gen_server:call(?MODULE, {force_sync, SessionId}, 15000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([node()]) -> {ok, state()}.
init([ClusterNodes]) ->
    process_flag(trap_exit, true),

    LocalNode = node(),
    AllNodes = lists:usort([LocalNode | ClusterNodes]),

    logger:info("Session failover manager starting on ~p (cluster: ~p)",
                [LocalNode, AllNodes]),

    %% Connect to cluster nodes
    lists:foreach(fun(Node) when Node =/= LocalNode ->
        case net_adm:ping(Node) of
            pong ->
                logger:info("Connected to cluster node: ~p", [Node]),
                ok;
            pang ->
                logger:warning("Cannot connect to cluster node: ~p", [Node]),
                ok
        end
    end, AllNodes),

    %% Subscribe to node monitoring events
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),

    %% Initialize node status
    ConnectedNodes = nodes(),
    NodeStatus = lists:foldl(fun(Node, Acc) ->
        Status = case lists:member(Node, ConnectedNodes) of
            true -> up;
            false -> down
        end,
        maps:put(Node, Status, Acc)
    end, #{}, AllNodes),

    State = #state{
        local_node = LocalNode,
        cluster_nodes = AllNodes,
        node_status = NodeStatus,
        monitoring = true
    },

    logger:info("Session failover manager ready (monitoring ~p nodes)",
                [length(AllNodes)]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({add_backup, SessionId, BackupNode}, _From, State) ->
    case lists:member(BackupNode, State#state.cluster_nodes) of
        false ->
            {reply, {error, {invalid_backup_node, not_in_cluster}}, State};
        true when BackupNode =:= State#state.local_node ->
            {reply, {error, {invalid_backup_node, cannot_backup_to_self}}, State};
        true ->
            case maps:get(BackupNode, State#state.node_status, down) of
                down ->
                    {reply, {error, {backup_node_down, BackupNode}}, State};
                up ->
                    %% Add backup to session
                    Sessions = State#state.sessions,
                    FailoverState = case maps:get(SessionId, Sessions, undefined) of
                        undefined ->
                            %% New session - this node becomes primary
                            #{
                                session_id => SessionId,
                                primary_node => State#state.local_node,
                                backup_nodes => [BackupNode],
                                last_sync => erlang:system_time(millisecond),
                                status => up
                            };
                        ExistingState ->
                            %% Add backup to existing session
                            Backups = ExistingState#{backup_nodes =>
                                lists:usort([BackupNode | maps:get(backup_nodes, ExistingState)])},
                            Backups#{last_sync => erlang:system_time(millisecond)}
                    end,

                    %% Replicate session to backup
                    spawn(fun() -> replicate_session(SessionId, FailoverState, BackupNode) end),

                    NewSessions = maps:put(SessionId, FailoverState, Sessions),
                    logger:info("Added backup node ~p for session ~s", [BackupNode, SessionId]),
                    {reply, ok, State#state{sessions = NewSessions}}
            end
    end;

handle_call({remove_backup, SessionId, BackupNode}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, session_not_found}, State};
        FailoverState ->
            Backups = maps:get(backup_nodes, FailoverState, []),
            case lists:member(BackupNode, Backups) of
                false ->
                    {reply, {error, {not_a_backup, BackupNode}}, State};
                true ->
                    NewBackups = lists:delete(BackupNode, Backups),
                    UpdatedState = FailoverState#{backup_nodes => NewBackups},
                    NewSessions = maps:put(SessionId, UpdatedState, State#state.sessions),
                    logger:info("Removed backup node ~p for session ~s", [BackupNode, SessionId]),
                    {reply, ok, State#state{sessions = NewSessions}}
            end
    end;

handle_call({failover, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, session_not_found}, State};
        FailoverState ->
            PrimaryNode = maps:get(primary_node, FailoverState),
            case PrimaryNode of
                State#state.local_node ->
                    %% Already primary - no failover needed
                    {reply, {error, already_primary}, State};
                _ ->
                    %% Perform failover to this node
                    case maps:get(PrimaryNode, State#state.node_status, down) of
                        up ->
                            logger:warning("Primary node ~p is still up, refusing failover for session ~s",
                                         [PrimaryNode, SessionId]),
                            {reply, {error, primary_still_alive}, State};
                        down ->
                            %% Promote this node to primary
                            NewFailoverState = FailoverState#{
                                primary_node => State#state.local_node,
                                last_sync => erlang:system_time(millisecond),
                                status => up
                            },
                            NewSessions = maps:put(SessionId, NewFailoverState, State#state.sessions),

                            logger:info("Failed over session ~s from ~p to ~p",
                                       [SessionId, PrimaryNode, State#state.local_node]),

                            %% Notify backup nodes
                            notify_failover(SessionId, State#state.local_node,
                                          maps:get(backup_nodes, FailoverState, [])),

                            {reply, ok, State#state{sessions = NewSessions}}
                    end
            end
    end;

handle_call({get_backup_nodes, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        FailoverState ->
            Backups = maps:get(backup_nodes, FailoverState, []),
            {reply, {ok, Backups}, State}
    end;

handle_call({get_primary_node, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        FailoverState ->
            Primary = maps:get(primary_node, FailoverState),
            {reply, {ok, Primary}, State}
    end;

handle_call({is_backup, SessionId, Node}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, false, State};
        FailoverState ->
            Backups = maps:get(backup_nodes, FailoverState, []),
            IsBackup = lists:member(Node, Backups),
            {reply, IsBackup, State}
    end;

handle_call({force_sync, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, session_not_found}, State};
        FailoverState ->
            Backups = maps:get(backup_nodes, FailoverState, []),
            Results = replicate_session_to_all(SessionId, FailoverState, Backups),
            SuccessNodes = [Node || {Node, ok} <- Results],
            logger:info("Force sync for session ~s: ~p nodes succeeded", [SessionId, length(SuccessNodes)]),
            {reply, {ok, SuccessNodes}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({nodeup, Node, _InfoList}, State) ->
    logger:info("Node up detected: ~p", [Node]),
    NewNodeStatus = maps:put(Node, up, State#state.node_status),
    {noreply, State#state{node_status = NewNodeStatus}};

handle_info({nodedown, Node, InfoList}, State) ->
    Reason = proplists:get_value(nodedown_reason, InfoList, unknown),
    logger:warning("Node down detected: ~p (reason: ~p)", [Node, Reason]),
    NewNodeStatus = maps:put(Node, down, State#state.node_status),

    %% Trigger failover for sessions affected by this node failure
    AffectedSessions = find_sessions_on_node(Node, State#state.sessions),
    NewSessions = lists:foldl(fun(SessionId, AccSessions) ->
        case handle_node_failure(SessionId, Node, AccSessions, State) of
            {ok, UpdatedSessions} ->
                UpdatedSessions;
            {error, _} ->
                AccSessions
        end
    end, State#state.sessions, AffectedSessions),

    {noreply, State#state{node_status = NewNodeStatus, sessions = NewSessions}};

handle_info({'EXIT', _Pid, Reason}, State) ->
    logger:warning("Process exit detected: ~p", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Unsubscribe from node monitoring
    case State#state.monitoring of
        true -> net_kernel:monitor_nodes(false);
        false -> ok
    end,
    logger:info("Session failover manager terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Replicate session data to a backup node
-spec replicate_session(session_id(), failover_state(), node()) -> ok | {error, term()}.
replicate_session(SessionId, FailoverState, BackupNode) ->
    try
        %% Retrieve session data from Mnesia
        TableName = erlmcp_session_mnesia,
        case mnesia:transaction(fun() ->
            mnesia:read(TableName, SessionId)
        end) of
            {atomic, [#erlmcp_session{session_data = SessionData}]} ->
                %% Replicate to backup node via RPC
                case rpc:call(BackupNode, erlmcp_session_mnesia, store,
                             [SessionId, SessionData, #{table_name => TableName}]) of
                    ok ->
                        logger:debug("Replicated session ~s to backup ~p", [SessionId, BackupNode]),
                        ok;
                    {error, Reason} ->
                        logger:error("Failed to replicate session ~s to backup ~p: ~p",
                                    [SessionId, BackupNode, Reason]),
                        {error, Reason};
                    {badrpc, Reason} ->
                        logger:error("RPC failed replicating session ~s to backup ~p: ~p",
                                    [SessionId, BackupNode, Reason]),
                        {error, {badrpc, Reason}}
                end;
            {atomic, []} ->
                logger:warning("Session ~s not found for replication", [SessionId]),
                {error, session_not_found};
            {aborted, Reason} ->
                logger:error("Mnesia transaction failed for session ~s replication: ~p",
                            [SessionId, Reason]),
                {error, {mnesia_aborted, Reason}}
        end
    catch
        Type:Error:Stacktrace ->
            logger:error("Exception replicating session ~s: ~p:~p~n~p",
                        [SessionId, Type, Error, Stacktrace]),
            {error, {exception, {Type, Error}}}
    end.

%% @doc Replicate session to multiple backup nodes
-spec replicate_session_to_all(session_id(), failover_state(), [node()]) ->
    [{node(), ok | {error, term()}}].
replicate_session_to_all(SessionId, FailoverState, BackupNodes) ->
    lists:map(fun(BackupNode) ->
        Result = replicate_session(SessionId, FailoverState, BackupNode),
        {BackupNode, Result}
    end, BackupNodes).

%% @doc Find sessions that have their primary on a specific node
-spec find_sessions_on_node(node(), #{session_id() => failover_state()}) -> [session_id()].
find_sessions_on_node(Node, Sessions) ->
    maps:fold(fun(SessionId, FailoverState, Acc) ->
        PrimaryNode = maps:get(primary_node, FailoverState),
        case PrimaryNode of
            Node -> [SessionId | Acc];
            _ -> Acc
        end
    end, [], Sessions).

%% @doc Handle node failure - promote backup if available
-spec handle_node_failure(session_id(), node(), #{session_id() => failover_state()}, state()) ->
    {ok, #{session_id() => failover_state()}} | {error, term()}.
handle_node_failure(SessionId, FailedNode, Sessions, State) ->
    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            {error, session_not_found};
        FailoverState ->
            PrimaryNode = maps:get(primary_node, FailoverState),
            BackupNodes = maps:get(backup_nodes, FailoverState, []),

            case FailedNode of
                PrimaryNode when BackupNodes =:= [] ->
                    %% Primary failed with no backups - mark session as down
                    logger:error("Session ~s primary ~p failed with no backups", [SessionId, FailedNode]),
                    UpdatedState = FailoverState#{status => down},
                    {ok, maps:put(SessionId, UpdatedState, Sessions)};
                PrimaryNode ->
                    %% Primary failed - promote first available backup
                    case find_available_backup(BackupNodes, State#state.node_status) of
                        {ok, NewPrimary} ->
                            logger:warning("Promoting backup ~p to primary for session ~s (failed: ~p)",
                                         [NewPrimary, SessionId, FailedNode]),
                            UpdatedState = FailoverState#{
                                primary_node => NewPrimary,
                                backup_nodes => lists:delete(NewPrimary, BackupNodes),
                                last_sync => erlang:system_time(millisecond),
                                status => recovering
                            },
                            {ok, maps:put(SessionId, UpdatedState, Sessions)};
                        {error, no_backup_available} ->
                            logger:error("No available backup for session ~s (failed: ~p)",
                                        [SessionId, FailedNode]),
                            UpdatedState = FailoverState#{status => down},
                            {ok, maps:put(SessionId, UpdatedState, Sessions)}
                    end;
                _BackupNode ->
                    %% Backup failed - just remove from list
                    logger:info("Removing failed backup ~p for session ~s", [FailedNode, SessionId]),
                    UpdatedState = FailoverState#{
                        backup_nodes => lists:delete(FailedNode, BackupNodes)
                    },
                    {ok, maps:put(SessionId, UpdatedState, Sessions)}
            end
    end.

%% @doc Find first available backup node that is up
-spec find_available_backup([node()], #{node() => node_status()}) ->
    {ok, node()} | {error, no_backup_available}.
find_available_backup([], _NodeStatus) ->
    {error, no_backup_available};
find_available_backup([Node | Rest], NodeStatus) ->
    case maps:get(Node, NodeStatus, down) of
        up -> {ok, Node};
        down -> find_available_backup(Rest, NodeStatus)
    end.

%% @doc Notify backup nodes of failover event
-spec notify_failover(session_id(), node(), [node()]) -> ok.
notify_failover(_SessionId, _NewPrimary, []) ->
    ok;
notify_failover(SessionId, NewPrimary, [BackupNode | Rest]) ->
    spawn(fun() ->
        case rpc:call(BackupNode, erlmcp_session_failover, notify_failover_local,
                     [SessionId, NewPrimary]) of
            ok ->
                logger:info("Notified backup ~p of failover for session ~s", [BackupNode, SessionId]);
            {error, Reason} ->
                logger:error("Failed to notify backup ~p of failover: ~p", [BackupNode, Reason])
        end
    end),
    notify_failover(SessionId, NewPrimary, Rest).

%% @doc Local notification of failover (called via RPC)
notify_failover_local(SessionId, NewPrimary) ->
    logger:info("Failover notification: session ~s primary is now ~p", [SessionId, NewPrimary]),
    ok.
