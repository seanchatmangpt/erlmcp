%%%-------------------------------------------------------------------
%%% @doc erlmcp_cluster_membership - Cluster Membership Protocol
%%%
%%% Implements SWIM-like gossip protocol for cluster membership:
%%% - Heartbeat mechanism with failure detection
%%% - Node join/leave handling
%%% - Network partition detection
%%% - Membership state propagation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cluster_membership).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         join_cluster/1, leave_cluster/0,
         get_members/0, get_member_status/1,
         set_heartbeat_interval/1,
         get_membership_view/0,
         force_sync/0,
         subscribe/0, unsubscribe/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_HEARTBEAT_INTERVAL, 5000).
-define(DEFAULT_FAILURE_TIMEOUT, 15000).
-define(DEFAULT_SUSPECT_TIMEOUT, 30000).
-define(DEFAULT_SYNC_INTERVAL, 10000).
-define(MAX_SUSPECT_COUNT, 3).

%%====================================================================
%% Types
%%====================================================================

-type member_status() :: alive | suspect | dead | left.
-type incarnation() :: non_neg_integer().

-record(member, {
    node :: node(),
    status :: member_status(),
    incarnation = 0 :: incarnation(),
    last_heartbeat :: integer() | undefined,
    suspect_count = 0 :: non_neg_integer(),
    suspect_start :: integer() | undefined}).

-record(gossip_state, {
    members = #{} :: #{node() => #member{}},
    incarnation = 0 :: incarnation()}).

-record(state, {
    local_node :: node(),
    heartbeat_interval :: pos_integer(),
    failure_timeout :: pos_integer(),
    suspect_timeout :: pos_integer(),
    sync_interval :: pos_integer(),
    heartbeat_ref :: reference() | undefined,
    sync_ref :: reference() | undefined,
    subscribers = [] :: [pid()],
    gossip_state :: #gossip_state{},
    partition_detected = false :: boolean(),
    partition_start :: integer() | undefined}).

-type state() :: #state{}.

-export_type([member_status/0, incarnation/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start membership with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start membership with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Join a cluster
-spec join_cluster([node()] | node()) -> ok | {error, term()}.
join_cluster(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {join_cluster, Nodes});
join_cluster(Node) when is_atom(Node) ->
    join_cluster([Node]).

%% @doc Leave the cluster gracefully
-spec leave_cluster() -> ok.
leave_cluster() ->
    gen_server:call(?SERVER, leave_cluster, 30000).

%% @doc Get all cluster members
-spec get_members() -> #{node() => member_status()}.
get_members() ->
    gen_server:call(?SERVER, get_members).

%% @doc Get status of a specific member
-spec get_member_status(node()) -> {ok, member_status()} | {error, not_found}.
get_member_status(Node) ->
    gen_server:call(?SERVER, {get_member_status, Node}).

%% @doc Set heartbeat interval
-spec set_heartbeat_interval(pos_integer()) -> ok.
set_heartbeat_interval(Interval) when is_integer(Interval), Interval > 0 ->
    gen_server:call(?SERVER, {set_heartbeat_interval, Interval}).

%% @doc Get full membership view
-spec get_membership_view() -> map().
get_membership_view() ->
    gen_server:call(?SERVER, get_membership_view).

%% @doc Force sync with all nodes
-spec force_sync() -> ok.
force_sync() ->
    gen_server:cast(?SERVER, force_sync).

%% @doc Subscribe to membership events
-spec subscribe() -> ok.
subscribe() ->
    gen_server:call(?SERVER, subscribe).

%% @doc Unsubscribe from membership events
-spec unsubscribe() -> ok.
unsubscribe() ->
    gen_server:call(?SERVER, unsubscribe).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map() | []) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),

    LocalNode = node(),
    HeartbeatInterval = maps:get(heartbeat_interval, Options, ?DEFAULT_HEARTBEAT_INTERVAL),
    FailureTimeout = maps:get(failure_timeout, Options, ?DEFAULT_FAILURE_TIMEOUT),
    SuspectTimeout = maps:get(suspect_timeout, Options, ?DEFAULT_SUSPECT_TIMEOUT),
    SyncInterval = maps:get(sync_interval, Options, ?DEFAULT_SYNC_INTERVAL),

    % Initialize gossip state with local node
    GossipState = #gossip_state{
        members = #{LocalNode => #member{
            node = LocalNode,
            status = alive,
            incarnation = 0,
            last_heartbeat = erlang:system_time(millisecond)
        }}
    },

    % Enable node monitoring (non-fatal if not in distributed mode)
    _ = case net_kernel:monitor_nodes(true, [{node_type, all}, {nodedown_reason, true}]) of
        ok -> ok;
        {error, _} ->
            % Fallback: try basic node monitoring without extended options
            case net_kernel:monitor_nodes(true) of
                ok -> ok;
                {error, _Reason} -> ok  % Running in non-distributed mode
            end
    end,

    % Start timers
    HeartbeatRef = erlang:send_after(HeartbeatInterval, self(), send_heartbeat),
    SyncRef = erlang:send_after(SyncInterval, self(), sync_membership),

    ?LOG_INFO("Starting cluster membership on ~p: heartbeat=~pms, failure_timeout=~pms",
              [LocalNode, HeartbeatInterval, FailureTimeout]),

    State = #state{
        local_node = LocalNode,
        heartbeat_interval = HeartbeatInterval,
        failure_timeout = FailureTimeout,
        suspect_timeout = SuspectTimeout,
        sync_interval = SyncInterval,
        heartbeat_ref = HeartbeatRef,
        sync_ref = SyncRef,
        gossip_state = GossipState
    },

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({join_cluster, Nodes}, _From, State) ->
    case lists:member(State#state.local_node, Nodes) of
        true ->
            {reply, {error, cannot_join_self}, State};
        false ->
            NewState = join_nodes(Nodes, State),
            {reply, ok, NewState, hibernate}
    end;

handle_call(leave_cluster, {From, _Tag}, State) ->
    % Set status to left
    GossipState = State#state.gossip_state,
    Members = GossipState#gossip_state.members,
    LocalMember = maps:get(State#state.local_node, Members),
    UpdatedMember = LocalMember#member{status = left},
    UpdatedMembers = maps:put(State#state.local_node, UpdatedMember, Members),
    UpdatedGossip = GossipState#gossip_state{members = UpdatedMembers},

    % Notify all nodes we're leaving
    broadcast_leave(State#state.local_node, UpdatedMembers),

    % Notify subscribers
    notify_subscribers(State#state.subscribers, {member_left, State#state.local_node}),

    {reply, ok, State#state{gossip_state = UpdatedGossip}, hibernate};

handle_call(get_members, _From, State) ->
    Members = maps:map(fun(_Node, Member) -> Member#member.status end,
                       State#state.gossip_state#gossip_state.members),
    {reply, Members, State, hibernate};

handle_call({get_member_status, Node}, _From, State) ->
    Reply = case maps:get(Node, State#state.gossip_state#gossip_state.members, undefined) of
        undefined -> {error, not_found};
        Member -> {ok, Member#member.status}
    end,
    {reply, Reply, State, hibernate};

handle_call({set_heartbeat_interval, Interval}, _From, State) ->
    {reply, ok, State#state{heartbeat_interval = Interval}, hibernate};

handle_call(get_membership_view, _From, State) ->
    View = #{
        local_node => State#state.local_node,
        members => maps:map(fun(_N, M) -> #{
            status => M#member.status,
            incarnation => M#member.incarnation,
            last_heartbeat => M#member.last_heartbeat,
            suspect_count => M#member.suspect_count
        } end, State#state.gossip_state#gossip_state.members),
        partition_detected => State#state.partition_detected,
        partition_start => State#state.partition_start
    },
    {reply, View, State, hibernate};

handle_call(subscribe, {Pid, _Tag}, State) ->
    NewSubscribers = lists:usort([Pid | State#state.subscribers]),
    {reply, ok, State#state{subscribers = NewSubscribers}, hibernate};

handle_call(unsubscribe, {Pid, _Tag}, State) ->
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    {reply, ok, State#state{subscribers = NewSubscribers}, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast(force_sync, State) ->
    NewState = sync_membership(State),
    {noreply, NewState, hibernate};

handle_cast({heartbeat, FromNode, Incarnation}, State) ->
    NewState = handle_heartbeat(FromNode, Incarnation, State),
    {noreply, NewState, hibernate};

handle_cast({gossip, FromNode, GossipData}, State) ->
    NewState = handle_gossip(FromNode, GossipData, State),
    {noreply, NewState, hibernate};

handle_cast({sync_request, FromNode}, State) ->
    NewState = handle_sync_request(FromNode, State),
    {noreply, NewState, hibernate};

handle_cast({sync_response, FromNode, Members}, State) ->
    NewState = handle_sync_response(FromNode, Members, State),
    {noreply, NewState, hibernate};

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info(send_heartbeat, State) ->
    NewState = broadcast_heartbeat(State),
    HeartbeatRef = erlang:send_after(State#state.heartbeat_interval, self(), send_heartbeat),
    {noreply, NewState#state{heartbeat_ref = HeartbeatRef}, hibernate};

handle_info(sync_membership, State) ->
    NewState = sync_membership(State),
    SyncRef = erlang:send_after(State#state.sync_interval, self(), sync_membership),
    {noreply, NewState#state{sync_ref = SyncRef}, hibernate};

handle_info(check_failures, State) ->
    NewState = check_node_failures(State),
    {noreply, NewState, hibernate};

handle_info({nodeup, Node, _InfoList}, State) ->
    ?LOG_INFO("Node up detected: ~p", [Node]),
    NewState = add_member(Node, alive, State),
    {noreply, NewState, hibernate};

handle_info({nodedown, Node, InfoList}, State) ->
    Reason = proplists:get_value(nodedown_reason, InfoList, unknown),
    ?LOG_WARNING("Node down detected: ~p (reason: ~p)", [Node, Reason]),
    NewState = mark_suspect(Node, State),
    {noreply, NewState, hibernate};

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    case State#state.heartbeat_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    case State#state.sync_ref of
        undefined -> ok;
        Ref2 -> erlang:cancel_timer(Ref2)
    end,
    net_kernel:monitor_nodes(false),
    ?LOG_INFO("Cluster membership terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Join cluster by connecting to nodes
-spec join_nodes([node()], state()) -> state().
join_nodes(Nodes, State) ->
    % Connect to all nodes
    lists:foreach(fun(Node) ->
        case net_kernel:connect_node(Node) of
            true -> ?LOG_INFO("Connected to node: ~p", [Node]);
            false -> ?LOG_WARNING("Failed to connect to node: ~p", [Node]);
            ignored -> ok
        end
    end, Nodes),

    % Add to member list
    NewState = lists:foldl(fun(Node, AccState) ->
        add_member(Node, alive, AccState)
    end, State, Nodes),

    % Request sync from all nodes
    sync_membership(NewState).

%% @doc Add a member to the cluster
-spec add_member(node(), member_status(), state()) -> state().
add_member(Node, Status, State) ->
    GossipState = State#state.gossip_state,
    Members = GossipState#gossip_state.members,

    case maps:get(Node, Members, undefined) of
        undefined ->
            NewMember = #member{
                node = Node,
                status = Status,
                incarnation = 0,
                last_heartbeat = erlang:system_time(millisecond)
            },
            NewMembers = maps:put(Node, NewMember, Members),
            NewGossip = GossipState#gossip_state{members = NewMembers},
            ?LOG_INFO("Added member ~p with status ~p", [Node, Status]),
            notify_subscribers(State#state.subscribers, {member_joined, Node, Status}),
            State#state{gossip_state = NewGossip};
        ExistingMember when ExistingMember#member.status =/= Status ->
            NewMember = ExistingMember#member{status = Status},
            NewMembers = maps:put(Node, NewMember, Members),
            NewGossip = GossipState#gossip_state{members = NewMembers},
            notify_subscribers(State#state.subscribers, {member_status_changed, Node, Status}),
            State#state{gossip_state = NewGossip};
        _ ->
            State
    end.

%% @doc Mark a node as suspect
-spec mark_suspect(node()) -> ok.
mark_suspect(Node) ->
    gen_server:cast(?SERVER, {mark_suspect, Node}).

-spec mark_suspect(node(), state()) -> state().
mark_suspect(Node, State) ->
    GossipState = State#state.gossip_state,
    Members = GossipState#gossip_state.members,

    case maps:get(Node, Members, undefined) of
        undefined ->
            State;
        Member when Member#member.status =:= alive ->
            Now = erlang:system_time(millisecond),
            NewMember = Member#member{
                status = suspect,
                suspect_count = 1,
                suspect_start = Now
            },
            NewMembers = maps:put(Node, NewMember, Members),
            NewGossip = GossipState#gossip_state{members = NewMembers},
            ?LOG_WARNING("Node ~p marked as suspect", [Node]),
            notify_subscribers(State#state.subscribers, {member_suspect, Node}),
            State#state{gossip_state = NewGossip};
        Member ->
            State
    end.

%% @doc Handle heartbeat from a node
-spec handle_heartbeat(node(), incarnation(), state()) -> state().
handle_heartbeat(Node, Incarnation, State) ->
    GossipState = State#state.gossip_state,
    Members = GossipState#gossip_state.members,

    case maps:get(Node, Members, undefined) of
        undefined ->
            % New node, add it
            add_member(Node, alive, State);
        Member ->
            % Update heartbeat
            NewMember = case Incarnation > Member#member.incarnation of
                true ->
                    % Higher incarnation means node restarted
                    Member#member{
                        status = alive,
                        incarnation = Incarnation,
                        last_heartbeat = erlang:system_time(millisecond),
                        suspect_count = 0,
                        suspect_start = undefined
                    };
                false when Member#member.status =:= suspect ->
                    % Clear suspect status
                    Member#member{
                        status = alive,
                        last_heartbeat = erlang:system_time(millisecond),
                        suspect_count = 0,
                        suspect_start = undefined
                    };
                false ->
                    Member#member{last_heartbeat = erlang:system_time(millisecond)}
            end,
            NewMembers = maps:put(Node, NewMember, Members),
            State#state{gossip_state = GossipState#gossip_state{members = NewMembers}}
    end.

%% @doc Broadcast heartbeat to all members
-spec broadcast_heartbeat(state()) -> state().
broadcast_heartbeat(State) ->
    GossipState = State#state.gossip_state,
    Incarnation = GossipState#gossip_state.incarnation,
    Members = GossipState#gossip_state.members,

    % Update our own heartbeat time
    LocalMember = maps:get(State#state.local_node, Members),
    UpdatedLocalMember = LocalMember#member{
        last_heartbeat = erlang:system_time(millisecond)
    },
    UpdatedMembers = maps:put(State#state.local_node, UpdatedLocalMember, Members),
    UpdatedGossip = GossipState#gossip_state{members = UpdatedMembers},

    % Send heartbeat to all alive members
    lists:foreach(fun({Node, Member}) when Node =/= State#state.local_node, Member#member.status =:= alive ->
        erlang:send({?SERVER, Node}, {heartbeat, State#state.local_node, Incarnation}),
        ok;
       (_) ->
        ok
    end, maps:to_list(UpdatedMembers)),

    State#state{gossip_state = UpdatedGossip}.

%% @doc Sync membership with random nodes
-spec sync_membership(state()) -> state().
sync_membership(State) ->
    GossipState = State#state.gossip_state,
    Members = maps:keys(GossipState#gossip_state.members),
    AliveMembers = [N || N <- Members, N =/= State#state.local_node,
                         (case maps:get(N, GossipState#gossip_state.members, undefined) of
                              undefined -> false;
                              M -> M#member.status =:= alive
                          end)],

    % Select random nodes to sync with
    SyncCount = min(3, length(AliveMembers)),
    case AliveMembers of
        [] ->
            State;
        _ ->
            SyncNodes = take_random(AliveMembers, SyncCount),
            lists:foreach(fun(Node) ->
                erlang:send({?SERVER, Node}, {sync_request, State#state.local_node})
            end, SyncNodes),
            State
    end.

%% @doc Handle sync request from a node
-spec handle_sync_request(node(), state()) -> state().
handle_sync_request(FromNode, State) ->
    GossipState = State#state.gossip_state,
    Members = GossipState#gossip_state.members,

    % Send our view back
    MemberList = maps:fold(fun(_Node, Member, Acc) ->
        [#{
            node => Member#member.node,
            status => Member#member.status,
            incarnation => Member#member.incarnation,
            last_heartbeat => Member#member.last_heartbeat
        } | Acc]
    end, [], Members),

    erlang:send({?SERVER, FromNode}, {sync_response, State#state.local_node, MemberList}),
    State.

%% @doc Handle sync response from a node
-spec handle_sync_response(node(), [map()], state()) -> state().
handle_sync_response(FromNode, MemberList, State) ->
    % Merge member list with our view
    lists:foldl(fun(MemberMap, AccState) ->
        Node = maps:get(node, MemberMap),
        Status = maps:get(status, MemberMap),
        add_member(Node, Status, AccState)
    end, State, MemberList).

%% @doc Handle gossip message from a node.
%% Merges the incoming GossipState into local state by taking the
%% max incarnation (vector-clock) per node and retaining the entry
%% with the larger incarnation value.
-spec handle_gossip(node(), #gossip_state{}, state()) -> state().
handle_gossip(_FromNode, IncomingGossip, State) when is_record(IncomingGossip, gossip_state) ->
    LocalGossip = State#state.gossip_state,
    LocalMembers = LocalGossip#gossip_state.members,
    IncomingMembers = IncomingGossip#gossip_state.members,

    %% Merge: for each node in incoming, keep the higher-incarnation entry
    MergedMembers = maps:fold(fun(Node, IncomingMember, Acc) ->
        case maps:get(Node, Acc, undefined) of
            undefined ->
                %% New node we haven't seen — add it
                maps:put(Node, IncomingMember, Acc);
            LocalMember ->
                %% Keep whichever entry has the larger incarnation
                case IncomingMember#member.incarnation > LocalMember#member.incarnation of
                    true  -> maps:put(Node, IncomingMember, Acc);
                    false -> Acc
                end
        end
    end, LocalMembers, IncomingMembers),

    %% Take max global incarnation
    NewIncarnation = max(LocalGossip#gossip_state.incarnation,
                         IncomingGossip#gossip_state.incarnation),

    NewGossip = LocalGossip#gossip_state{
        members     = MergedMembers,
        incarnation = NewIncarnation
    },
    State#state{gossip_state = NewGossip};
handle_gossip(_FromNode, _GossipData, State) ->
    %% Unknown gossip format — ignore
    State.

%% @doc Check for node failures
-spec check_node_failures(state()) -> state().
check_node_failures(State) ->
    GossipState = State#state.gossip_state,
    Now = erlang:system_time(millisecond),
    Members = GossipState#gossip_state.members,

    NewMembers = maps:map(fun(Node, Member) ->
        if
            Node =:= State#state.local_node ->
                Member;
            Member#member.status =:= alive ->
                LastHeartbeat = Member#member.last_heartbeat,
                case Now - LastHeartbeat > State#state.failure_timeout of
                    true ->
                        ?LOG_WARNING("Node ~p failure detected (no heartbeat for ~pms)",
                                    [Node, Now - LastHeartbeat]),
                        Member#member{status = suspect, suspect_count = 1, suspect_start = Now};
                    false ->
                        Member
                end;
            Member#member.status =:= suspect ->
                SuspectStart = Member#member.suspect_start,
                case Now - SuspectStart > State#state.suspect_timeout of
                    true ->
                        ?LOG_ERROR("Node ~p declared dead", [Node]),
                        notify_subscribers(State#state.subscribers, {member_dead, Node}),
                        Member#member{status = dead};
                    false ->
                        Member
                end;
            true ->
                Member
        end
    end, Members),

    % Check for network partition
    AliveCount = maps:fold(fun(_N, M, Acc) ->
        case M#member.status of
            alive -> Acc + 1;
            _ -> Acc
        end
    end, 0, NewMembers),

    TotalCount = map_size(NewMembers),
    PartitionDetected = (AliveCount * 2 < TotalCount) and (TotalCount > 2),

    NewState = case {PartitionDetected, State#state.partition_detected} of
        {true, false} ->
            ?LOG_ERROR("Network partition detected! Only ~p/~p nodes alive",
                      [AliveCount, TotalCount]),
            notify_subscribers(State#state.subscribers, partition_detected),
            State#state{
                gossip_state = GossipState#gossip_state{members = NewMembers},
                partition_detected = true,
                partition_start = Now
            };
        {false, true} ->
            ?LOG_INFO("Network partition healed"),
            notify_subscribers(State#state.subscribers, partition_healed),
            State#state{
                gossip_state = GossipState#gossip_state{members = NewMembers},
                partition_detected = false,
                partition_start = undefined
            };
        _ ->
            State#state{gossip_state = GossipState#gossip_state{members = NewMembers}}
    end,

    NewState.

%% @doc Broadcast leave message to all nodes
-spec broadcast_leave(node(), #{node() => #member{}}) -> ok.
broadcast_leave(LocalNode, Members) ->
    maps:foreach(fun(Node, _Member) when Node =/= LocalNode ->
        erlang:send({?SERVER, Node}, {member_left, LocalNode});
       (_, _) ->
        ok
    end, Members).

%% @doc Notify subscribers of events
-spec notify_subscribers([pid()], term()) -> ok.
notify_subscribers(Subscribers, Event) ->
    lists:foreach(fun(Pid) ->
        Pid ! {membership_event, Event}
    end, Subscribers),
    ok.

%% @doc Take N random elements from list
-spec take_random([T], non_neg_integer()) -> [T].
take_random(List, N) when length(List) =< N ->
    List;
take_random(List, N) ->
    Shuffled = lists:sort(fun(_, _) -> rand:uniform() > 0.5 end, List),
    lists:sublist(Shuffled, N).
