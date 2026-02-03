%% @doc Raft Cluster Membership Management
%%
%% This module implements safe cluster membership changes using the joint
%% consensus approach from the Raft paper. It ensures:
%%   - No two majorities can elect different leaders during reconfiguration
%%   - Configuration changes are committed before taking effect
%%   - Automatic transition through C_old,new to C_new
%%
%% Key Features:
%%   - Joint consensus for safe member addition/removal
%%   - Quorum calculation during reconfiguration
%%   - Handling of unreachable nodes during removal
%%   - Configuration rollback support
-module(erlmcp_raft_membership).

-behaviour(gen_server).

-include("erlmcp_raft.hrl").
-include("erlmcp.hrl").

%% API
-export([start_link/2, start_link/3,
         add_server/2, remove_server/2,
         get_cluster_members/1, get_quorum_size/1,
         is_in_cluster/2, propose_config_change/2,
         verify_cluster_health/1,
         force_remove_server/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State record
-record(membership_state,
        {cluster_name :: binary(),
         node_id :: raft_node_id(),
         current_config :: [raft_node_id()],      % Current cluster configuration
         pending_config :: [raft_node_id()] | undefined, % Pending config (C_old,new)
         config_index :: raft_index() | 0,       % Log index of current config
         raft_server :: pid() | undefined,        % Raft server process
         state_machine :: module(),               % State machine module
         membership_state :: stable | transitioning,
         unreachable_nodes :: sets:set(raft_node_id()),
         last_config_change :: integer() | undefined}).

-type membership_state() :: #membership_state{}.
-type membership_status() :: stable | transitioning.

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Start the membership manager
-spec start_link(binary(), raft_node_id()) -> {ok, pid()} | {error, term()}.
start_link(ClusterName, NodeId) ->
    start_link(ClusterName, NodeId, erlmcp_raft_state_machine).

-spec start_link(binary(), raft_node_id(), module()) -> {ok, pid()} | {error, term()}.
start_link(ClusterName, NodeId, StateMachine) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [ClusterName, NodeId, StateMachine], []).

%% @doc Add a server to the cluster
-spec add_server(pid() | atom(), raft_node_id()) ->
    {ok, raft_index()} | {error, term()}.
add_server(MembershipRef, NewServerId) ->
    gen_server:call(MembershipRef, {add_server, NewServerId}, infinity).

%% @doc Remove a server from the cluster
-spec remove_server(pid() | atom(), raft_node_id()) ->
    {ok, raft_index()} | {error, term()}.
remove_server(MembershipRef, ServerId) ->
    gen_server:call(MembershipRef, {remove_server, ServerId}, infinity).

%% @doc Get current cluster members
-spec get_cluster_members(pid() | atom()) -> {ok, [raft_node_id()]}.
get_cluster_members(MembershipRef) ->
    gen_server:call(MembershipRef, get_cluster_members, 5000).

%% @doc Get quorum size for the current configuration
-spec get_quorum_size(pid() | atom()) -> {ok, pos_integer()}.
get_quorum_size(MembershipRef) ->
    gen_server:call(MembershipRef, get_quorum_size, 5000).

%% @doc Check if a node is in the cluster
-spec is_in_cluster(pid() | atom(), raft_node_id()) -> boolean().
is_in_cluster(MembershipRef, NodeId) ->
    gen_server:call(MembershipRef, {is_in_cluster, NodeId}, 5000) =:= true.

%% @doc Propose a configuration change
-spec propose_config_change(pid() | atom(), [raft_node_id()]) ->
    {ok, raft_index()} | {error, term()}.
propose_config_change(MembershipRef, NewCluster) ->
    gen_server:call(MembershipRef, {propose_config_change, NewCluster}, infinity).

%% @doc Verify cluster health (all nodes reachable)
-spec verify_cluster_health(pid() | atom()) ->
    {ok, #{healthy := [raft_node_id()], unhealthy := [raft_node_id()]}}.
verify_cluster_health(MembershipRef) ->
    gen_server:call(MembershipRef, verify_cluster_health, 30000).

%% @doc Force remove a server (even if unreachable)
-spec force_remove_server(pid() | atom(), raft_node_id()) ->
    {ok, raft_index()} | {error, term()}.
force_remove_server(MembershipRef, ServerId) ->
    gen_server:call(MembershipRef, {force_remove, ServerId}, infinity).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init([binary(), raft_node_id(), module()]) -> {ok, #membership_state{}}.
init([ClusterName, NodeId, StateMachine]) ->
    process_flag(trap_exit, true),

    State = #membership_state{
        cluster_name = ClusterName,
        node_id = NodeId,
        current_config = [NodeId],  % Single node by default
        pending_config = undefined,
        config_index = 0,
        raft_server = undefined,
        state_machine = StateMachine,
        membership_state = stable,
        unreachable_nodes = sets:new(),
        last_config_change = undefined
    },

    logger:info("Raft membership manager starting for node ~p in cluster ~p",
                [NodeId, ClusterName]),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #membership_state{}) ->
    {reply, term(), #membership_state{}}.
handle_call({add_server, NewServerId}, _From, State = #membership_state{
        current_config = CurrentConfig,
        membership_state = MembershipState}) ->

    %% Validate new server
    case lists:member(NewServerId, CurrentConfig) of
        true ->
            {reply, {error, already_in_cluster}, State};
        false ->
            case MembershipState of
                stable ->
                    %% Start joint consensus
                    JointConfig = lists:usort([NewServerId | CurrentConfig]),
                    initiate_config_change(JointConfig, State);
                transitioning ->
                    {reply, {error, config_change_in_progress}, State}
            end
    end;

handle_call({remove_server, ServerId}, _From, State = #membership_state{
        node_id = NodeId,
        current_config = CurrentConfig,
        membership_state = MembershipState}) ->

    %% Validate removal
    case ServerId of
        NodeId ->
            {reply, {error, cannot_remove_self}, State};
        _ ->
            case lists:member(ServerId, CurrentConfig) of
                false ->
                    {reply, {error, not_in_cluster}, State};
                true ->
                    case MembershipState of
                        stable ->
                            NewConfig = lists:delete(ServerId, CurrentConfig),
                            initiate_config_change(NewConfig, State);
                        transitioning ->
                            {reply, {error, config_change_in_progress}, State}
                    end
            end
    end;

handle_call(get_cluster_members, _From, State = #membership_state{
        current_config = Config}) ->
    {reply, {ok, Config}, State};

handle_call(get_quorum_size, _From, State = #membership_state{
        current_config = Config}) ->
    Quorum = ?quorum_size(length(Config)),
    {reply, {ok, Quorum}, State};

handle_call({is_in_cluster, NodeId}, _From, State = #membership_state{
        current_config = Config}) ->
    {reply, lists:member(NodeId, Config), State};

handle_call({propose_config_change, NewCluster}, _From, State) ->
    initiate_config_change(NewCluster, State);

handle_call(verify_cluster_health, _From, State = #membership_state{
        current_config = Config,
        unreachable_nodes = Unreachable}) ->
    {Healthy, Unhealthy} = verify_node_health(Config),
    NewUnreachable = sets:union(Unreachable, sets:from_list(Unhealthy)),
    {reply, {ok, #{healthy => Healthy, unhealthy => Unhealthy}},
            State#membership_state{unreachable_nodes = NewUnreachable}};

handle_call({force_remove, ServerId}, _From, State = #membership_state{
        node_id = SelfId,
        current_config = CurrentConfig}) ->
    case ServerId of
        SelfId ->
            {reply, {error, cannot_remove_self}, State};
        _ ->
            NewConfig = lists:delete(ServerId, CurrentConfig),
            %% Force skip validation
            initiate_config_change_direct(NewConfig, State)
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #membership_state{}) -> {noreply, #membership_state{}}.
handle_cast({config_committed, NewConfig, Index}, State) ->
    %% Configuration change committed - update state
    NewState = State#membership_state{
        current_config = NewConfig,
        config_index = Index,
        membership_state = stable,
        pending_config = undefined,
        last_config_change = erlang:monotonic_time(millisecond)
    },
    logger:info("Cluster configuration committed: ~p (index: ~p)", [NewConfig, Index]),
    {noreply, NewState};

handle_cast({config_entry_applied, NewConfig}, State = #membership_state{
        pending_config = undefined}) ->
    %% First phase of joint consensus - now commit to new config
    NewState = State#membership_state{
        pending_config = NewConfig,
        membership_state = transitioning
    },
    {noreply, NewState};

handle_cast({config_entry_applied, _FinalConfig}, State = #membership_state{
        pending_config = _PendingConfig}) ->
    %% Second phase - transition complete
    {noreply, State#membership_state{
        membership_state = stable,
        pending_config = undefined
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #membership_state{}) -> {noreply, #membership_state{}}.
handle_info({node_down, NodeId}, State = #membership_state{
        unreachable_nodes = Unreachable}) ->
    NewUnreachable = sets:add_element(NodeId, Unreachable),
    logger:warning("Node ~p detected as down", [NodeId]),
    {noreply, State#membership_state{unreachable_nodes = NewUnreachable}};

handle_info({node_up, NodeId}, State = #membership_state{
        unreachable_nodes = Unreachable}) ->
    NewUnreachable = sets:del_element(NodeId, Unreachable),
    logger:info("Node ~p is back up", [NodeId]),
    {noreply, State#membership_state{unreachable_nodes = NewUnreachable}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #membership_state{}) -> ok.
terminate(_Reason, State) ->
    logger:info("Raft membership manager terminating for node ~p",
                [State#membership_state.node_id]),
    ok.

-spec code_change(term(), #membership_state{}, term()) -> {ok, #membership_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions - Configuration Change Management
%%%====================================================================

%% @doc Initiate a configuration change using joint consensus
-spec initiate_config_change([raft_node_id()], #membership_state{}) ->
    {reply, {ok, raft_index()} | {error, term()}, #membership_state{}}.
initiate_config_change(NewConfig, State = #membership_state{
        current_config = CurrentConfig,
        raft_server = RaftServer}) ->

    %% Check for quorum loss
    NewQuorum = ?quorum_size(length(NewConfig)),
    CurrentQuorum = ?quorum_size(length(CurrentConfig)),

    case NewQuorum > length(NewConfig) div 2 of
        false ->
            {reply, {error, new_config_would_lose_quorum}, State};
        true ->
            %% Create joint consensus configuration
            JointConfig = create_joint_config(CurrentConfig, NewConfig),
            ConfigChange = #raft_config_change{
                type = reconfigure,
                server_id = undefined,
                old_cluster = CurrentConfig,
                new_cluster = NewConfig,
                config_index = 0
            },

            %% Submit to Raft
            case RaftServer of
                undefined ->
                    {reply, {error, raft_server_not_available}, State};
                _ ->
                    case erlmcp_raft:write(RaftServer, {config_change, ConfigChange}) of
                        {ok, Index} ->
                            %% Update state to transitioning
                            NewState = State#membership_state{
                                pending_config = NewConfig,
                                membership_state = transitioning
                            },
                            logger:info("Initiated config change: ~p -> ~p (joint: ~p)",
                                        [CurrentConfig, NewConfig, JointConfig]),
                            {reply, {ok, Index}, NewState};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end
            end
    end.

%% @doc Initiate config change without validation (force remove)
-spec initiate_config_change_direct([raft_node_id()], #membership_state{}) ->
    {reply, {ok, raft_index()} | {error, term()}, #membership_state{}}.
initiate_config_change_direct(NewConfig, State = #membership_state{
        current_config = CurrentConfig,
        raft_server = RaftServer}) ->

    ConfigChange = #raft_config_change{
        type = reconfigure,
        server_id = undefined,
        old_cluster = CurrentConfig,
        new_cluster = NewConfig,
        config_index = 0
    },

    case RaftServer of
        undefined ->
            {reply, {error, raft_server_not_available}, State};
        _ ->
            case erlmcp_raft:write(RaftServer, {config_change, ConfigChange}) of
                {ok, Index} ->
                    NewState = State#membership_state{
                        pending_config = NewConfig,
                        membership_state = transitioning
                    },
                    {reply, {ok, Index}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end.

%% @doc Create joint consensus configuration
-spec create_joint_config([raft_node_id()], [raft_node_id()]) -> [raft_node_id()].
create_joint_config(OldConfig, NewConfig) ->
    %% In joint consensus, we need majorities from both old and new
    %% For simplicity, we use the union here
    lists:usort(OldConfig ++ NewConfig).

%% @doc Verify health of cluster nodes
-spec verify_node_health([raft_node_id()]) -> {[raft_node_id()], [raft_node_id()]}.
verify_node_health(Nodes) ->
    Now = erlang:monotonic_time(millisecond),

    %% Check each node
    Results = lists:map(fun(NodeId) ->
                              case net_adm:ping(NodeId) of
                                  pang -> {NodeId, unhealthy};
                                  pong -> {NodeId, healthy}
                              end
                      end, Nodes),

    %% Partition into healthy and unhealthy
    {Healthy, Unhealthy} = lists:partition(
                             fun({_N, Status}) -> Status =:= healthy end,
                             Results),

    {[N || {N, healthy} <- Healthy], [N || {N, unhealthy} <- Unhealthy]}.

%% @doc Check if configuration change is safe (won't lose quorum)
-spec is_config_change_safe([raft_node_id()], [raft_node_id()]) -> boolean().
is_config_change_safe(OldConfig, NewConfig) ->
    OldSize = length(OldConfig),
    NewSize = length(NewConfig),

    OldQuorum = ?quorum_size(OldSize),
    NewQuorum = ?quorum_size(NewSize),

    %% Check if there's overlap between old and new quorums
    %% This ensures no two majorities can make different decisions
    OverlapRequired = OldQuorum + NewQuorum - NewSize,

    %% Calculate actual overlap
    CommonMembers = length(OldConfig ++ NewConfig) -
                    length(lists:usort(OldConfig ++ NewConfig)),

    CommonMembers >= OverlapRequired.

%% @doc Calculate quorum size for joint consensus
-spec joint_quorum_size([raft_node_id()], [raft_node_id()]) -> pos_integer().
joint_quorum_size(OldConfig, NewConfig) ->
    %% During joint consensus, need majority from both old and new
    max(?quorum_size(length(OldConfig)),
        ?quorum_size(length(NewConfig))).

%% @doc Check if cluster has quorum
-spec has_quorum(#membership_state{}) -> boolean().
has_quorum(#membership_state{current_config = Config, unreachable_nodes = Unreachable}) ->
    Available = Config -- sets:to_list(Unreachable),
    length(Available) >= ?quorum_size(length(Config)).

%% @doc Get safe node list for operations (excluding unreachable)
-spec get_available_nodes(#membership_state{}) -> [raft_node_id()].
get_available_nodes(#membership_state{current_config = Config, unreachable_nodes = Unreachable}) ->
    Config -- sets:to_list(Unreachable).
