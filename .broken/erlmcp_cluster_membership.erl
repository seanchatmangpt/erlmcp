%% @doc Cluster Membership Manager for erlmcp v3
%%
%% This module manages cluster membership including:
%%   - Member discovery via gproc registry
%%   - Join/leave protocol
%%   - Member health tracking
%%   - Membership changes notification
%%   - Quorum calculation and enforcement
%%   - Member role assignment (leader, follower, voter, observer)
-module(erlmcp_cluster_membership).

-behaviour(gen_server).

-include("erlmcp_cluster_coordinator.hrl").

%% API
-export([start_link/0, start_link/1]).
-export([join_cluster/2, leave_cluster/1, approve_join/2, reject_join/2]).
-export([get_members/0, get_member_info/1]).
-export([subscribe/1, unsubscribe/1]).
-export([get_quorum_size/0, has_quorum/0]).
-export([update_member_status/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal state
-record(membership_state,
        {
         cluster_name :: binary(),
         node_id :: node(),
         members :: #{node() => member_info()},
         pending_joins :: #{node() => join_request()},
         pending_leaves :: #{node() => leave_request()},
         subscribers :: sets:set(pid()),
         quorum_size :: pos_integer(),
         join_requests_from :: #{node() => {pid(), reference()}},
         join_timestamps = [] :: [{node(), integer()}],
         max_joins_per_minute = 10 :: pos_integer()
        }).

%%%====================================================================
%%% API Functions
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{cluster_name => <<"default">>}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Request to join cluster
-spec join_cluster(node(), map()) -> {ok, pending} | {error, term()}.
join_cluster(SeedNode, Metadata) ->
    join_cluster(SeedNode, Metadata, undefined).

%% @doc Request to join cluster with authentication token
-spec join_cluster(node(), map(), binary() | undefined) -> {ok, pending} | {error, term()}.
join_cluster(SeedNode, Metadata, Token) ->
    gen_server:call(?MODULE, {join_cluster, SeedNode, Metadata, Token}, 30000).

%% @doc Leave cluster gracefully
-spec leave_cluster(term()) -> ok | {error, term()}.
leave_cluster(Reason) ->
    gen_server:call(?MODULE, {leave_cluster, Reason}, 30000).

%% @doc Approve a pending join request
-spec approve_join(node(), map()) -> ok | {error, term()}.
approve_join(Node, Metadata) ->
    gen_server:call(?MODULE, {approve_join, Node, Metadata}).

%% @doc Reject a pending join request
-spec reject_join(node(), term()) -> ok | {error, term()}.
reject_join(Node, Reason) ->
    gen_server:call(?MODULE, {reject_join, Node, Reason}).

%% @doc Get all cluster members
-spec get_members() -> {ok, [node()]}.
get_members() ->
    gen_server:call(?MODULE, get_members).

%% @doc Get detailed member info
-spec get_member_info(node()) -> {ok, member_info()} | {error, not_found}.
get_member_info(Node) ->
    gen_server:call(?MODULE, {get_member_info, Node}).

%% @doc Subscribe to membership changes
-spec subscribe(pid()) -> ok.
subscribe(Subscriber) ->
    gen_server:cast(?MODULE, {subscribe, Subscriber}).

%% @doc Unsubscribe from membership changes
-spec unsubscribe(pid()) -> ok.
unsubscribe(Subscriber) ->
    gen_server:cast(?MODULE, {unsubscribe, Subscriber}).

%% @doc Get quorum size
-spec get_quorum_size() -> pos_integer().
get_quorum_size() ->
    gen_server:call(?MODULE, get_quorum_size).

%% @doc Check if cluster has quorum
-spec has_quorum() -> boolean().
has_quorum() ->
    gen_server:call(?MODULE, has_quorum).

%% @doc Update member status (for health monitoring)
-spec update_member_status(node(), map()) -> ok.
update_member_status(Node, StatusUpdate) ->
    gen_server:cast(?MODULE, {update_member_status, Node, StatusUpdate}).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init(map()) -> {ok, #membership_state{}}.
init(Options) ->
    process_flag(trap_exit, true),

    ClusterName = maps:get(cluster_name, Options, <<"default">>),
    NodeId = node(),

    State = #membership_state{
        cluster_name = ClusterName,
        node_id = NodeId,
        members = initialize_members(ClusterName),
        pending_joins = #{},
        pending_leaves = #{},
        subscribers = sets:new(),
        quorum_size = calculate_quorum(ClusterName),
        join_requests_from = #{}
    },

    %% Register with gproc
    gproc:add_local_name({cluster_membership, ClusterName}),
    gproc:reg({p, l, cluster_membership_change}),

    logger:info("Cluster membership manager started for ~p", [ClusterName]),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #membership_state{}) ->
    {reply, term(), #membership_state{}}.
handle_call({join_cluster, SeedNode, Metadata, Token}, From, State) ->
    case validate_join_token(Token) of
        ok ->
            case check_join_rate_limit(SeedNode, State) of
                ok ->
                    handle_join_request(SeedNode, Metadata, From, State);
                {error, rate_limited} ->
                    logger:warning("Join rate limited for ~p", [SeedNode]),
                    {reply, {error, rate_limited}, State}
            end;
        error ->
            logger:warning("Invalid join token from ~p", [SeedNode]),
            {reply, {error, invalid_token}, State}
    end;

handle_call({join_cluster, SeedNode, Metadata}, From, State) ->
    %% Legacy API - no token provided
    case validate_join_token(undefined) of
        ok ->
            case check_join_rate_limit(SeedNode, State) of
                ok ->
                    handle_join_request(SeedNode, Metadata, From, State);
                {error, rate_limited} ->
                    logger:warning("Join rate limited for ~p", [SeedNode]),
                    {reply, {error, rate_limited}, State}
            end;
        error ->
            logger:warning("No join token provided and token required"),
            {reply, {error, invalid_token}, State}
    end;

handle_call({leave_cluster, Reason}, _From, State) ->
    handle_leave_request(Reason, State);

handle_call({approve_join, Node, Metadata}, _From, State) ->
    Result = do_approve_join(Node, Metadata, State),
    {reply, Result, State};

handle_call({reject_join, Node, Reason}, _From, State) ->
    Result = do_reject_join(Node, Reason, State),
    {reply, Result, State};

handle_call(get_members, _From, State) ->
    Members = maps:keys(State#membership_state.members),
    {reply, {ok, Members}, State};

handle_call({get_member_info, Node}, _From, State) ->
    case maps:get(Node, State#membership_state.members, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Info ->
            {reply, {ok, Info}, State}
    end;

handle_call(get_quorum_size, _From, State) ->
    {reply, State#membership_state.quorum_size, State};

handle_call(has_quorum, _From, State) ->
    MemberCount = maps:size(State#membership_state.members),
    HasQuorum = MemberCount >= State#membership_state.quorum_size,
    {reply, HasQuorum, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #membership_state{}) -> {noreply, #membership_state{}}.
handle_cast({subscribe, Subscriber}, State) ->
    monitor(process, Subscriber),
    NewSubscribers = sets:add_element(Subscriber, State#membership_state.subscribers),
    {noreply, State#membership_state{subscribers = NewSubscribers}};

handle_cast({unsubscribe, Subscriber}, State) ->
    NewSubscribers = sets:del_element(Subscriber, State#membership_state.subscribers),
    {noreply, State#membership_state{subscribers = NewSubscribers}};

handle_cast({update_member_status, Node, StatusUpdate}, State) ->
    NewMembers = maps:update_with(Node,
        fun(Info) -> maps:merge(Info, StatusUpdate) end,
        State#membership_state.members),
    {noreply, State#membership_state{members = NewMembers}};

handle_cast({member_up, Node}, State) ->
    NewState = add_member(Node, State),
    {noreply, NewState};

handle_cast({member_down, Node}, State) ->
    NewState = remove_member(Node, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #membership_state{}) -> {noreply, #membership_state{}}.
handle_info({nodeup, Node}, State) ->
    logger:info("Node ~p detected up", [Node]),
    handle_member_up(Node, State);

handle_info({nodedown, Node, _Info}, State) ->
    logger:warning("Node ~p detected down", [Node]),
    handle_member_down(Node, State);

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewSubscribers = sets:filter(fun(S) -> S =/= Pid end, State#membership_state.subscribers),
    {noreply, State#membership_state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #membership_state{}) -> ok.
terminate(_Reason, #membership_state{cluster_name = ClusterName}) ->
    gproc:goodbye(),
    logger:info("Cluster membership manager terminating for ~p", [ClusterName]),
    ok.

-spec code_change(term(), #membership_state{}, term()) -> {ok, #membership_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Initialize members from gproc
-spec initialize_members(binary()) -> #{node() => member_info()}.
initialize_members(ClusterName) ->
    %% Find all registered cluster members
    Members = gproc:lookup_values({p, l, {cluster_node, ClusterName}}),

    lists:foldl(fun({{p, l, {cluster_node, ClusterName}}, Node}, Acc) ->
        maps:put(Node, create_member_info(Node), Acc)
    end, #{}, Members).

%% @doc Create member info for a node
-spec create_member_info(node()) -> member_info().
create_member_info(Node) ->
    #{
        node => Node,
        status => up,
        role => voter,
        last_heartbeat => erlang:system_time(millisecond),
        generation => 0,
        capabilities => get_node_capabilities(Node),
        metadata => #{}
    }.

%% @doc Get node capabilities
-spec get_node_capabilities(node()) -> [binary()].
get_node_capabilities(_Node) ->
    %% Query node for capabilities
    [<<"consensus">>, <<"persistence">>, <<"routing">>].

%% @doc Calculate quorum size (majority)
-spec calculate_quorum(binary()) -> pos_integer().
calculate_quorum(ClusterName) ->
    case application:get_env(erlmcp_core, quorum_override) of
        {ok, Quorum} when is_integer(Quorum), Quorum > 0 ->
            Quorum;
        _ ->
            %% Default: majority of nodes
            MemberCount = length(gproc:lookup_values({p, l, {cluster_node, ClusterName}})),
            max(2, (MemberCount div 2) + 1)
    end.

%% @doc Handle join request
-spec handle_join_request(node(), map(), {pid(), term()}, #membership_state{}) ->
    {reply, term(), #membership_state{}}.
handle_join_request(SeedNode, Metadata, From, State) ->
    NodeId = node(),

    %% Forward join request to seed node
    case net_adm:ping(SeedNode) of
        pong ->
            %% Record join timestamp for rate limiting
            Now = erlang:system_time(second),
            NewTimestamps = cleanup_old_timestamps(State#membership_state.join_timestamps),
            UpdatedTimestamps = [{SeedNode, Now} | NewTimestamps],

            gen_server:cast({?MODULE, SeedNode}, {join_request, NodeId, Metadata, From}),

            JoinRequest = #{
                node => NodeId,
                requested_at => erlang:system_time(millisecond),
                status => pending,
                voter_approval => #{},
                metadata => Metadata
            },

            NewPendingJoins = maps:put(SeedNode, JoinRequest, State#membership_state.pending_joins),

            NewState = State#membership_state{
                pending_joins = NewPendingJoins,
                join_timestamps = UpdatedTimestamps
            },

            {reply, {ok, pending}, NewState};
        pang ->
            {reply, {error, {seed_unreachable, SeedNode}}, State}
    end.

%% @doc Handle leave request
-spec handle_leave_request(term(), #membership_state{}) ->
    {reply, term(), #membership_state{}}.
handle_leave_request(Reason, State) ->
    NodeId = node(),

    %% Notify all members of departure
    notify_members_leaving(NodeId, Reason, State),

    %% Remove from membership
    NewMembers = maps:remove(NodeId, State#membership_state.members),

    NewState = State#membership_state{members = NewMembers},

    {reply, ok, NewState}.

%% @doc Approve join request
-spec do_approve_join(node(), map(), #membership_state{}) -> ok | {error, term()}.
do_approve_join(Node, Metadata, #membership_state{pending_joins = PendingJoins} = State) ->
    case maps:get(Node, PendingJoins, undefined) of
        undefined ->
            {error, no_pending_join};
        _JoinRequest ->
            %% Add to membership
            NewMembers = maps:put(Node, create_member_info(Node), State#membership_state.members),
            NewState = State#membership_state{members = NewMembers},

            %% Remove from pending
            NewPending = maps:remove(Node, PendingJoins),

            %% Notify subscriber
            notify_subscribers(NewState, {member_joined, Node}),

            %% Send approval to joining node
            gen_server:cast({?MODULE, Node}, {join_approved, node(), Metadata}),

            ok
    end.

%% @doc Reject join request
-spec do_reject_join(node(), term(), #membership_state{}) -> ok | {error, term()}.
do_reject_join(Node, Reason, #membership_state{pending_joins = PendingJoins}) ->
    case maps:get(Node, PendingJoins, undefined) of
        undefined ->
            {error, no_pending_join};
        _JoinRequest ->
            %% Send rejection
            gen_server:cast({?MODULE, Node}, {join_rejected, Reason}),

            ok
    end.

%% @doc Add member to cluster
-spec add_member(node(), #membership_state{}) -> #membership_state{}.
add_member(Node, #membership_state{members = Members} = State) ->
    case maps:is_key(Node, Members) of
        true ->
            State;
        false ->
            NewMembers = maps:put(Node, create_member_info(Node), Members),
            NewState = State#membership_state{members = NewMembers},

            notify_subscribers(NewState, {member_joined, Node}),

            NewState
    end.

%% @doc Remove member from cluster
-spec remove_member(node(), #membership_state{}) -> #membership_state{}.
remove_member(Node, #membership_state{members = Members} = State) ->
    case maps:is_key(Node, Members) of
        false ->
            State;
        true ->
            NewMembers = maps:remove(Node, Members),
            NewState = State#membership_state{members = NewMembers},

            notify_subscribers(NewState, {member_left, Node}),

            NewState
    end.

%% @doc Handle member coming up
-spec handle_member_up(node(), #membership_state{}) -> {noreply, #membership_state{}}.
handle_member_up(Node, State) ->
    NewState = add_member(Node, State),
    {noreply, NewState}.

%% @doc Handle member going down
-spec handle_member_down(node(), #membership_state{}) -> {noreply, #membership_state{}}.
handle_member_down(Node, State) ->
    NewState = remove_member(Node, State),
    {noreply, NewState}.

%% @doc Notify members of leaving node
-spec notify_members_leaving(node(), term(), #membership_state{}) -> ok.
notify_members_leaving(Node, Reason, #membership_state{members = Members}) ->
    maps:foreach(fun(Member, _Info) ->
        case Member of
            Node -> skip;
            _ ->
                gen_server:cast({?MODULE, Member}, {member_leaving, Node, Reason})
        end
    end, Members),
    ok.

%% @doc Notify subscribers of membership change
-spec notify_subscribers(#membership_state{}, term()) -> ok.
notify_subscribers(#membership_state{subscribers = Subscribers}, Event) ->
    sets:foreach(fun(Subscriber) ->
        case is_process_alive(Subscriber) of
            true ->
                Subscriber ! {membership_event, Event};
            false ->
                ok
        end
    end, Subscribers),
    ok.

%%%===================================================================
%%% Admission Control Functions
%%%===================================================================

%% @doc Validate join token for cluster admission
%% @doc Returns ok if token is valid or no token is configured
-spec validate_join_token(binary() | undefined) -> ok | error.
validate_join_token(undefined) ->
    case application:get_env(erlmcp_core, cluster_join_token) of
        undefined ->
            logger:warning("No join token configured - allowing joins"),
            ok;
        _ExpectedToken ->
            logger:warning("Join token required but not provided"),
            error
    end;
validate_join_token(Token) when is_binary(Token) ->
    case application:get_env(erlmcp_core, cluster_join_token) of
        undefined ->
            logger:warning("No join token configured - allowing joins"),
            ok;
        ExpectedToken ->
            case constant_time_compare(Token, ExpectedToken) of
                true -> ok;
                false -> error
            end
    end;
validate_join_token(_Other) ->
    error.

%% @doc Check join rate limit (max N joins per minute)
%% @doc Returns ok if under limit, {error, rate_limited} otherwise
-spec check_join_rate_limit(node(), #membership_state{}) -> ok | {error, rate_limited}.
check_join_rate_limit(_Node, #membership_state{join_timestamps = Timestamps,
                                                max_joins_per_minute = MaxJoins}) ->
    %% Clean old timestamps (older than 60 seconds)
    Recent = cleanup_old_timestamps(Timestamps),
    case length(Recent) >= MaxJoins of
        true -> {error, rate_limited};
        false -> ok
    end.

%% @doc Clean up timestamps older than 60 seconds
-spec cleanup_old_timestamps([{node(), integer()}]) -> [{node(), integer()}].
cleanup_old_timestamps(Timestamps) ->
    Now = erlang:system_time(second),
    lists:filter(fun({_N, T}) -> Now - T < 60 end, Timestamps).

%% @doc Constant-time comparison for tokens to prevent timing attacks
%% @doc Only works for binaries of equal size
-spec constant_time_compare(binary(), binary()) -> boolean().
constant_time_compare(A, B) when byte_size(A) =:= byte_size(B) ->
    Res = crypto:bytes_to_integer(crypto:exor(A, B)),
    Res =:= 0;
constant_time_compare(_, _) ->
    false.
