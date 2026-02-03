%% @doc Cluster Heartbeat Manager for erlmcp v3
%%
%% This module manages cluster-wide heartbeat monitoring:
%%   - Periodic heartbeat broadcasting
%%   - Failure detection through missed heartbeats
%%   - Configurable timeout and interval
%%   - Integration with node monitoring
%%   - Health score calculation
%%   - Adaptive heartbeat frequency
-module(erlmcp_cluster_heartbeat).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([send_heartbeat/1, receive_heartbeat/2]).
-export([get_heartbeat_status/0, get_node_health/1]).
-export([configure/2, force_check/0]).
-export([subscribe/1, unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal state
-record(heartbeat_state,
        {
         node_id :: node(),
         interval :: pos_integer(),
         timeout :: pos_integer(),
         missed_threshold :: pos_integer(),
         heartbeat_ref :: reference() | undefined,
         received_heartbeats :: #{node() => heartbeat_info()},
         missed_counts :: #{node() => pos_integer()},
         health_scores :: #{node() => float()},
         subscribers :: sets:set(pid()),
         adaptive_mode :: boolean(),
         failure_detector :: failure_detector()
        }).

-type heartbeat_info() :: #{
        last_seen => integer(),
        count => non_neg_integer(),
        latency => non_neg_integer(),
        metadata => map()
       }.

-type failure_detector() ::
    {phi_accrual, float()} |          %% Phi accrual detector
    {timeout, pos_integer()} |         %% Simple timeout
    {chen, float(), float()} |         %% Chen's algorithm
    custom.                            %% Custom detector

%%%====================================================================
%%% API Functions
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Send heartbeat to cluster
-spec send_heartbeat(map()) -> ok.
send_heartbeat(Metadata) ->
    gen_server:cast(?MODULE, {send_heartbeat, Metadata}).

%% @doc Record received heartbeat
-spec receive_heartbeat(node(), map()) -> ok.
receive_heartbeat(FromNode, Metadata) ->
    gen_server:cast(?MODULE, {receive_heartbeat, FromNode, Metadata}).

%% @doc Get heartbeat status for all nodes
-spec get_heartbeat_status() -> {ok, map()}.
get_heartbeat_status() ->
    gen_server:call(?MODULE, get_status).

%% @doc Get health status for specific node
-spec get_node_health(node()) -> {ok, float(), map()} | {error, not_found}.
get_node_health(Node) ->
    gen_server:call(?MODULE, {get_node_health, Node}).

%% @doc Configure heartbeat parameters
-spec configure(atom(), term()) -> ok.
configure(Parameter, Value) ->
    gen_server:cast(?MODULE, {configure, Parameter, Value}).

%% @doc Force immediate heartbeat check
-spec force_check() -> ok.
force_check() ->
    gen_server:cast(?MODULE, force_check).

%% @doc Subscribe to heartbeat events
-spec subscribe(pid()) -> ok.
subscribe(Subscriber) ->
    gen_server:cast(?MODULE, {subscribe, Subscriber}).

%% @doc Unsubscribe from heartbeat events
-spec unsubscribe(pid()) -> ok.
unsubscribe(Subscriber) ->
    gen_server:cast(?MODULE, {unsubscribe, Subscriber}).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init(map()) -> {ok, #heartbeat_state{}}.
init(Options) ->
    process_flag(trap_exit, true),

    NodeId = node(),
    Interval = maps:get(interval, Options, 5000),
    Timeout = maps:get(timeout, Options, 15000),
    MissedThreshold = maps:get(missed_threshold, Options, 3),
    AdaptiveMode = maps:get(adaptive_mode, Options, true),

    State = #heartbeat_state{
        node_id = NodeId,
        interval = Interval,
        timeout = Timeout,
        missed_threshold = MissedThreshold,
        heartbeat_ref = undefined,
        received_heartbeats => #{},
        missed_counts => #{},
        health_scores => #{},
        subscribers = sets:new(),
        adaptive_mode = AdaptiveMode,
        failure_detector = {phi_accrual, 0.99}
    },

    %% Start heartbeat timer
    NewState = start_heartbeat_timer(State),

    %% Subscribe to node events
    ok = net_kernel:monitor_nodes(true),

    logger:info("Heartbeat manager started (interval=~p, timeout=~p)",
                [Interval, Timeout]),

    {ok, NewState}.

-spec handle_call(term(), {pid(), term()}, #heartbeat_state{}) ->
    {reply, term(), #heartbeat_state{}}.
handle_call(get_status, _From, State) ->
    Status = #{
        interval => State#heartbeat_state.interval,
        timeout => State#heartbeat_state.timeout,
        nodes => maps:size(State#heartbeat_state.received_heartbeats),
        health_scores => State#heartbeat_state.health_scores,
        missed_counts => State#heartbeat_state.missed_counts
    },
    {reply, {ok, Status}, State};

handle_call({get_node_health, Node}, _From, State) ->
    case maps:get(Node, State#heartbeat_state.health_scores, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Score ->
            LastSeen = case maps:get(Node, State#heartbeat_state.received_heartbeats) of
                undefined => undefined;
                Info -> maps:get(last_seen, Info)
            end,
            {reply, {ok, Score, #{last_seen => LastSeen}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #heartbeat_state{}) -> {noreply, #heartbeat_state{}}.
handle_cast({send_heartbeat, Metadata}, State) ->
    NewState = do_send_heartbeat(State, Metadata),
    {noreply, NewState};

handle_cast({receive_heartbeat, FromNode, Metadata}, State) ->
    NewState = do_receive_heartbeat(FromNode, Metadata, State),
    {noreply, NewState};

handle_cast({configure, Parameter, Value}, State) ->
    NewState = apply_config(Parameter, Value, State),
    {noreply, NewState};

handle_cast(force_check, State) ->
    NewState = perform_health_check(State),
    {noreply, NewState};

handle_cast({subscribe, Subscriber}, State) ->
    monitor(process, Subscriber),
    NewSubscribers = sets:add_element(Subscriber, State#heartbeat_state.subscribers),
    {noreply, State#heartbeat_state{subscribers = NewSubscribers}};

handle_cast({unsubscribe, Subscriber}, State) ->
    NewSubscribers = sets:del_element(Subscriber, State#heartbeat_state.subscribers),
    {noreply, State#heartbeat_state{subscribers = NewSubscribers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #heartbeat_state{}) -> {noreply, #heartbeat_state{}}.
handle_info(send_heartbeat, State) ->
    NewState = do_send_heartbeat(State, #{}),
    {noreply, NewState};

handle_info(check_timeouts, State) ->
    NewState = check_heartbeat_timeouts(State),
    {noreply, NewState};

handle_info({nodeup, Node}, State) ->
    logger:info("Node ~p up, initializing heartbeat tracking", [Node]),
    NewState = initialize_node_tracking(Node, State),
    {noreply, NewState};

handle_info({nodedown, Node, _Info}, State) ->
    logger:warning("Node ~p down, stopping heartbeat tracking", [Node]),
    NewState = remove_node_tracking(Node, State),
    {noreply, NewState};

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewSubscribers = sets:filter(fun(S) -> S =/= Pid end, State#heartbeat_state.subscribers),
    {noreply, State#heartbeat_state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #heartbeat_state{}) -> ok.
terminate(_Reason, #heartbeat_state{heartbeat_ref = Ref}) ->
    case Ref of
        undefined -> ok;
        _ -> erlang:cancel_timer(Ref)
    end,
    net_kernel:monitor_nodes(false),
    logger:info("Heartbeat manager terminating"),
    ok.

-spec code_change(term(), #heartbeat_state{}, term()) -> {ok, #heartbeat_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Start heartbeat timer
-spec start_heartbeat_timer(#heartbeat_state{}) -> #heartbeat_state{}.
start_heartbeat_timer(#heartbeat_state{interval = Interval} = State) ->
    Ref = erlang:send_after(Interval, self(), send_heartbeat),
    State#heartbeat_state{heartbeat_ref = Ref}.

%% @doc Send heartbeat to all cluster members
-spec do_send_heartbeat(#heartbeat_state{}, map()) -> #heartbeat_state{}.
do_send_heartbeat(#heartbeat_state{node_id = NodeId} = State, Metadata) ->
    %% Get all cluster members
    Members = get_cluster_members(),

    Heartbeat = #{
        from => NodeId,
        timestamp => erlang:system_time(millisecond),
        sequence => get_sequence_number(),
        metadata => Metadata
    },

    %% Broadcast heartbeat
    lists:foreach(fun(Member) ->
        case Member of
            NodeId -> skip;
            _ ->
                gen_server:cast({?MODULE, Member}, {heartbeat, Heartbeat})
        end
    end, Members),

    %% Schedule next heartbeat
    NewState = start_heartbeat_timer(State),

    %% Schedule timeout check
    erlang:send_after(State#heartbeat_state.timeout, self(), check_timeouts),

    NewState.

%% @doc Process received heartbeat
-spec do_receive_heartbeat(node(), map(), #heartbeat_state{}) -> #heartbeat_state{}.
do_receive_heartbeat(FromNode, Metadata, #heartbeat_state{
        received_heartbeats = Received,
        missed_counts = MissedCounts,
        health_scores = HealthScores,
        failure_detector = Detector
    } = State) ->

    Now = erlang:system_time(millisecond),

    %% Update heartbeat info
    NewInfo = case maps:get(FromNode, Received, undefined) of
        undefined ->
            #{
                last_seen => Now,
                count => 1,
                latency => 0,
                metadata => Metadata
            };
        OldInfo ->
            #{
                last_seen => Now,
                count => maps:get(count, OldInfo, 0) + 1,
                latency => calculate_latency(Now, OldInfo),
                metadata => Metadata
            }
    end,

    NewReceived = maps:put(FromNode, NewInfo, Received),

    %% Reset missed count
    NewMissedCounts = maps:remove(FromNode, MissedCounts),

    %% Calculate health score
    NewHealthScore = calculate_health_score(FromNode, NewInfo, Detector),
    NewHealthScores = maps:put(FromNode, NewHealthScore, HealthScores),

    State#heartbeat_state{
        received_heartbeats = NewReceived,
        missed_counts = NewMissedCounts,
        health_scores = NewHealthScores
    }.

%% @doc Check for heartbeat timeouts
-spec check_heartbeat_timeouts(#heartbeat_state{}) -> #heartbeat_state{}.
check_heartbeat_timeouts(#heartbeat_state{
        received_heartbeats = Received,
        missed_counts = MissedCounts,
        missed_threshold = Threshold,
        timeout = Timeout
    } = State) ->

    Now = erlang:system_time(millisecond),

    %% Find nodes that have timed out
    TimeoutNodes = maps:fold(fun(Node, Info, Acc) ->
        LastSeen = maps:get(last_seen, Info),
        case (Now - LastSeen) > Timeout of
            true -> [Node | Acc];
            false -> Acc
        end
    end, [], Received),

    %% Update missed counts and notify
    NewMissedCounts = lists:foldl(fun(Node, Acc) ->
        Count = maps:get(Node, Acc, 0) + 1,
        NewAcc = maps:put(Node, Count, Acc),

        %% Notify if threshold exceeded
        case Count >= Threshold of
            true ->
                notify_subscribers(State, {node_unhealthy, Node, Count}),
                maybe_mark_node_down(Node, State);
            false ->
                ok
        end,

        NewAcc
    end, MissedCounts, TimeoutNodes),

    State#heartbeat_state{missed_counts = NewMissedCounts}.

%% @doc Calculate health score using failure detector
-spec calculate_health_score(node(), heartbeat_info(), failure_detector()) -> float().
calculate_health_score(_Node, Info, {phi_accrual, Threshold}) ->
    %% Phi accrual failure detector
    LastSeen = maps:get(last_seen, Info),
    Now = erlang:system_time(millisecond),
    TimeDelta = (Now - LastSeen) / 1000.0,  %% Convert to seconds

    %% Simple phi calculation (phi = -log10(1 - CDF(time)))
    %% Assuming exponential distribution with lambda = 1
    Phi = case TimeDelta of
        0.0 -> 0.0;
        _ -> TimeDelta * 2.302585092994046  %% log(10)
    end,

    %% Convert to health score (1.0 = healthy, 0.0 = failed)
    case Phi of
        Phi when Phi < Threshold -> 1.0 - (Phi / Threshold);
        _ -> 0.0
    end;

calculate_health_score(_Node, Info, {timeout, Timeout}) ->
    %% Simple timeout-based scoring
    LastSeen = maps:get(last_seen, Info),
    Now = erlang:system_time(millisecond),
    TimeDelta = Now - LastSeen,

    case TimeDelta of
        Delta when Delta < Timeout / 2 -> 1.0;
        Delta when Delta < Timeout -> 0.5;
        _ -> 0.0
    end;

calculate_health_score(_Node, _Info, _CustomDetector) ->
    %% Default healthy score
    1.0.

%% @doc Calculate heartbeat latency
-spec calculate_latency(integer(), heartbeat_info()) -> non_neg_integer().
calculate_latency(Now, Info) ->
    case maps:get(last_seen, Info, undefined) of
        undefined -> 0;
        LastSeen -> Now - LastSeen
    end.

%% @doc Get cluster members
-spec get_cluster_members() -> [node()].
get_cluster_members() ->
    case erlmcp_cluster_membership:get_members() of
        {ok, Members} -> Members;
        {error, _} -> nodes()
    end.

%% @doc Get sequence number for heartbeat
-spec get_sequence_number() -> non_neg_integer().
get_sequence_number() ->
    erlang:unique_integer([positive, monotonic]) band 16#FFFFFF.

%% @doc Initialize tracking for new node
-spec initialize_node_tracking(node(), #heartbeat_state{}) -> #heartbeat_state{}.
initialize_node_tracking(Node, State) ->
    State.

%% @doc Remove tracking for node
-spec remove_node_tracking(node(), #heartbeat_state{}) -> #heartbeat_state{}.
remove_node_tracking(Node,
                     #heartbeat_state{
                        received_heartbeats = Received,
                        missed_counts = MissedCounts,
                        health_scores = HealthScores
                     } = State) ->

    NewReceived = maps:remove(Node, Received),
    NewMissed = maps:remove(Node, MissedCounts),
    NewHealthScores = maps:remove(Node, HealthScores),

    State#heartbeat_state{
        received_heartbeats = NewReceived,
        missed_counts = NewMissed,
        health_scores = NewHealthScores
    }.

%% @doc Apply configuration change
-spec apply_config(atom(), term(), #heartbeat_state{}) -> #heartbeat_state{}.
apply_config(interval, Value, State) when is_integer(Value), Value > 0 ->
    State#heartbeat_state{interval = Value};

apply_config(timeout, Value, State) when is_integer(Value), Value > 0 ->
    State#heartbeat_state{timeout = Value};

apply_config(missed_threshold, Value, State) when is_integer(Value), Value > 0 ->
    State#heartbeat_state{missed_threshold = Value};

apply_config(adaptive_mode, Value, State) when is_boolean(Value) ->
    State#heartbeat_state{adaptive_mode = Value};

apply_config(_Parameter, _Value, State) ->
    State.

%% @doc Perform health check
-spec perform_health_check(#heartbeat_state{}) -> #heartbeat_state{}.
perform_health_check(State) ->
    check_heartbeat_timeouts(State).

%% @doc Maybe mark node as down
-spec maybe_mark_node_down(node(), #heartbeat_state{}) -> ok.
maybe_mark_node_down(Node, State) ->
    %% Check if node is actually down
    case net_adm:ping(Node) of
        pang ->
            logger:error("Node ~p appears to be down after ~p missed heartbeats",
                        [Node, maps:get(Node, State#heartbeat_state.missed_counts, 0)]),
            notify_subscribers(State, {node_down, Node});
        pong ->
            logger:warning("Node ~p is reachable but missing heartbeats", [Node])
    end.

%% @doc Notify subscribers of heartbeat events
-spec notify_subscribers(#heartbeat_state{}, term()) -> ok.
notify_subscribers(#heartbeat_state{subscribers = Subscribers}, Event) ->
    sets:foreach(fun(Subscriber) ->
        case is_process_alive(Subscriber) of
            true ->
                Subscriber ! {heartbeat_event, Event};
            false ->
                ok
        end
    end, Subscribers),
    ok.
