%% @doc Raft Consensus Supervisor
%%
%% This supervisor manages the Raft consensus components:
%%   - Raft server (gen_server implementing Raft protocol)
%%   - Log storage module
%%   - State machine
%%   - Transport layer
%%
%% The supervision tree ensures fault isolation and automatic recovery
%% following OTP let-it-crash principles.
-module(erlmcp_raft_sup).

-behaviour(supervisor).

-include("erlmcp_raft.hrl").
-include("erlmcp.hrl").

%% API
-export([start_cluster/4, start_cluster/5, start_cluster/6,
         start_link/0, start_link/3,
         add_node/2, remove_node/2,
         get_cluster_status/1]).

%% supervisor callbacks
-export([init/1]).

%% Supervisor defaults
-define(SERVER, ?MODULE).

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Start the Raft supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_link(binary(), raft_node_id(), [raft_node_id()]) ->
    {ok, pid()} | {error, term()}.
start_link(ClusterName, NodeId, Peers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ClusterName, NodeId, Peers]).

%% @doc Start a Raft cluster with default settings
-spec start_cluster(binary(), [raft_node_id()], module(), term()) ->
    {ok, pid()} | {error, term()}.
start_cluster(ClusterName, Nodes, StateMachine, InitArgs) ->
    start_cluster(ClusterName, Nodes, StateMachine, InitArgs, erlmcp_raft_log, erlmcp_raft_transport).

%% @doc Start a Raft cluster with custom log module
-spec start_cluster(binary(), [raft_node_id()], module(), term(), module()) ->
    {ok, pid()} | {error, term()}.
start_cluster(ClusterName, Nodes, StateMachine, InitArgs, LogModule) ->
    start_cluster(ClusterName, Nodes, StateMachine, InitArgs, LogModule, erlmcp_raft_transport).

%% @doc Start a Raft cluster with full customization
-spec start_cluster(binary(), [raft_node_id()], module(), term(), module(), module()) ->
    {ok, pid()} | {error, term()}.
start_cluster(ClusterName, Nodes, StateMachine, InitArgs, LogModule, Transport) ->
    case length(Nodes) of
        N when N < 1 ->
            {error, {invalid_cluster_size, N}};
        _ ->
            %% Start cluster on each node (in distributed setup, this would run on each node)
            %% For now, start local supervisor
            supervisor:start_link({local, ?MODULE}, ?MODULE,
                                 [ClusterName, Nodes, StateMachine, InitArgs, LogModule, Transport])
    end.

%% @doc Add a node to an existing cluster
-spec add_node(pid() | atom(), raft_node_id()) -> ok | {error, term()}.
add_node(SupRef, NewNodeId) ->
    %% Find the Raft server and request membership change
    case get_child_pid(SupRef, erlmcp_raft_server) of
        {ok, RaftPid} ->
            erlmcp_raft:add_server(RaftPid, NewNodeId);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Remove a node from the cluster
-spec remove_node(pid() | atom(), raft_node_id()) -> ok | {error, term()}.
remove_node(SupRef, NodeId) ->
    case get_child_pid(SupRef, erlmcp_raft_server) of
        {ok, RaftPid} ->
            erlmcp_raft:remove_server(RaftPid, NodeId);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get cluster status
-spec get_cluster_status(pid() | atom()) -> {ok, map()} | {error, term()}.
get_cluster_status(SupRef) ->
    case get_child_pid(SupRef, erlmcp_raft_server) of
        {ok, RaftPid} ->
            erlmcp_raft:status(RaftPid);
        {error, Reason} ->
            {error, Reason}
    end.

%%%====================================================================
%%% supervisor Callbacks
%%%====================================================================

-spec init([] | [term()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Default init for standalone supervisor
    init([<<"default_cluster">>, node()], [], erlmcp_raft_state_machine, [], erlmcp_raft_log, erlmcp_raft_transport);

init([ClusterName, Nodes]) when is_list(Nodes) ->
    init([ClusterName, Nodes], erlmcp_raft_state_machine, [], erlmcp_raft_log, erlmcp_raft_transport);

init([ClusterName, NodeId, Peers]) ->
    init([ClusterName, NodeId, Peers], erlmcp_raft_state_machine, [], erlmcp_raft_log, erlmcp_raft_transport);

init([ClusterName, Nodes, StateMachine, InitArgs, LogModule, Transport]) when is_list(Nodes) ->
    %% Starting entire cluster (multiple nodes)
    %% This would typically be called on each node in the cluster
    [NodeId | _] = Nodes,
    Peers = Nodes -- [NodeId],
    init([ClusterName, NodeId, Peers, StateMachine, InitArgs, LogModule, Transport]);

init([ClusterName, NodeId, Peers, StateMachine, InitArgs, LogModule, Transport]) ->
    %% Start individual Raft node
    logger:info("Starting Raft supervisor for node ~p in cluster ~p with peers ~p",
                [NodeId, ClusterName, Peers]),

    %% Define supervision strategy
    %% - one_for_one: If a child crashes, only restart that child
    %% - Intensity 10, Max 60: Allow up to 10 restarts in 60 seconds
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    %% Child specifications
    TransportChild = #{
        id => erlmcp_raft_transport,
        start => {Transport, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [Transport]
    },

    MembershipChild = #{
        id => erlmcp_raft_membership,
        start => {erlmcp_raft_membership, start_link, [ClusterName, NodeId, StateMachine]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_raft_membership]
    },

    RaftServerChild = #{
        id => erlmcp_raft_server,
        start => {erlmcp_raft, start_link, [ClusterName, NodeId, StateMachine, Peers,
                                             LogModule, Transport]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_raft]
    },

    {ok, {SupFlags, [TransportChild, MembershipChild, RaftServerChild]}};

init([ClusterName, Nodes, StateMachine, InitArgs, LogModule, Transport]) ->
    init([ClusterName, Nodes, StateMachine, InitArgs, LogModule, Transport]).

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Get the PID of a child process by ID
-spec get_child_pid(pid() | atom(), atom()) -> {ok, pid()} | {error, term()}.
get_child_pid(SupRef, ChildId) ->
    Children = supervisor:which_children(SupRef),
    case lists:keyfind(ChildId, 1, Children) of
        {ChildId, Pid, _Type, _Modules} when is_pid(Pid) ->
            {ok, Pid};
        {ChildId, undefined, _Type, _Modules} ->
            {error, child_not_running};
        false ->
            {error, child_not_found}
    end.

%% @doc Validate cluster configuration
-spec validate_cluster([raft_node_id()]) -> ok | {error, term()}.
validate_cluster(Nodes) when length(Nodes) < 1 ->
    {error, cluster_too_small};
validate_cluster(Nodes) when length(Nodes) rem 2 =:= 0 ->
    {error, cluster_must_have_odd_size};
validate_cluster(_Nodes) ->
    ok.

%% @doc Calculate recommended cluster size
-spec recommended_cluster_size(pos_integer()) -> pos_integer().
recommended_cluster_size(TargetFaultTolerance) ->
    %% For f fault tolerance, need 2f + 1 nodes
    (TargetFaultTolerance * 2) + 1.
