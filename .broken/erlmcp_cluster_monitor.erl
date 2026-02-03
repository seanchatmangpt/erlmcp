%% @doc Cluster Monitoring and Metrics for erlmcp v3
%%
%% This module provides comprehensive cluster monitoring:
%%   - Health status tracking
%%   - Performance metrics collection
%%   - OTEL integration
%%   - Alert generation
%%   - Dashboard data
%%   - Historical statistics
-module(erlmcp_cluster_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([get_health/0, get_metrics/0]).
-export([record_event/2, record_metric/3]).
-export([get_cluster_overview/0]).
-export([subscribe_alerts/1, unsubscribe_alerts/1]).
-export([get_node_metrics/1]).
-export([get_history/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal state
-record(monitor_state,
        {
         node_id :: node(),
         health :: health_status(),
         metrics :: cluster_metrics(),
         event_history :: [cluster_event()],
         metric_history :: #{binary() => [metric_point()]},
         alerts :: [alert()],
         alert_subscribers :: sets:set(pid()),
         collection_interval :: pos_integer(),
         history_limit :: pos_integer(),
         timer_ref :: reference() | undefined
        }).

-type health_status() :: healthy | degraded | critical | recovering.
-type cluster_metrics() :: #{
        nodes_up => non_neg_integer(),
        nodes_down => non_neg_integer(),
        messages_sent => non_neg_integer(),
        messages_received => non_neg_integer(),
        sync_operations => non_neg_integer(),
        elections => non_neg_integer(),
        partitions => non_neg_integer(),
        conflicts => non_neg_integer()
       }.

-type cluster_event() :: #{
        timestamp => integer(),
        type => binary(),
        source => node(),
        details => map()
       }.

-type metric_point() :: #{
        timestamp => integer(),
        value => number(),
        tags => map()
       }.

-type alert() :: #{
        id => binary(),
        severity => info | warning | error | critical,
        type => binary(),
        message => binary(),
        timestamp => integer(),
        resolved => boolean()
       }.

%%%====================================================================
%%% API Functions
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Get current cluster health
-spec get_health() -> {ok, health_status(), map()}.
get_health() ->
    gen_server:call(?MODULE, get_health).

%% @doc Get current cluster metrics
-spec get_metrics() -> {ok, cluster_metrics()}.
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

%% @doc Record a cluster event
-spec record_event(binary(), map()) -> ok.
record_event(Type, Details) ->
    gen_server:cast(?MODULE, {record_event, Type, Details}).

%% @doc Record a metric value
-spec record_metric(binary(), number(), map()) -> ok.
record_metric(Name, Value, Tags) ->
    gen_server:cast(?MODULE, {record_metric, Name, Value, Tags}).

%% @doc Get cluster overview
-spec get_cluster_overview() -> {ok, map()}.
get_cluster_overview() ->
    gen_server:call(?MODULE, get_cluster_overview).

%% @doc Subscribe to alerts
-spec subscribe_alerts(pid()) -> ok.
subscribe_alerts(Subscriber) ->
    gen_server:cast(?MODULE, {subscribe_alerts, Subscriber}).

%% @doc Unsubscribe from alerts
-spec unsubscribe_alerts(pid()) -> ok.
unsubscribe_alerts(Subscriber) ->
    gen_server:cast(?MODULE, {unsubscribe_alerts, Subscriber}).

%% @doc Get metrics for specific node
-spec get_node_metrics(node()) -> {ok, map()} | {error, not_found}.
get_node_metrics(Node) ->
    gen_server:call(?MODULE, {get_node_metrics, Node}).

%% @doc Get historical data
-spec get_history(binary(), pos_integer()) -> {ok, [metric_point()]}.
get_history(MetricName, Limit) ->
    gen_server:call(?MODULE, {get_history, MetricName, Limit}).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init(map()) -> {ok, #monitor_state{}}.
init(Options) ->
    process_flag(trap_exit, true),

    NodeId = node(),
    CollectionInterval = maps:get(collection_interval, Options, 10000),
    HistoryLimit = maps:get(history_limit, Options, 1000),

    State = #monitor_state{
        node_id = NodeId,
        health = healthy,
        metrics => initialize_metrics(),
        event_history => [],
        metric_history => #{},
        alerts => [],
        alert_subscribers = sets:new(),
        collection_interval = CollectionInterval,
        history_limit = HistoryLimit,
        timer_ref = undefined
    },

    %% Start periodic collection
    NewState = start_collection_timer(State),

    logger:info("Cluster monitor started (interval=~p)", [CollectionInterval]),

    {ok, NewState}.

-spec handle_call(term(), {pid(), term()}, #monitor_state{}) ->
    {reply, term(), #monitor_state{}}.
handle_call(get_health, _From, State) ->
    {reply, {ok, State#monitor_state.health, get_health_details(State)}, State};

handle_call(get_metrics, _From, State) ->
    {reply, {ok, State#monitor_state.metrics}, State};

handle_call(get_cluster_overview, _From, State) ->
    Overview = #{
        health => State#monitor_state.health,
        metrics => State#monitor_state.metrics,
        active_alerts => [A || A <- State#monitor_state.alerts, not maps:get(resolved, A)],
        event_count => length(State#monitor_state.event_history),
        timestamp => erlang:system_time(millisecond)
    },
    {reply, {ok, Overview}, State};

handle_call({get_node_metrics, Node}, _From, State) ->
    case lists:member(Node, nodes()) of
        true ->
            {reply, {ok, #{node => Node, status => up}}, State};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_history, MetricName, Limit}, _From, State) ->
    History = case maps:get(MetricName, State#monitor_state.metric_history, []) of
        [] -> [];
        FullHistory -> lists:sublist(FullHistory, min(Limit, length(FullHistory)))
    end,
    {reply, {ok, History}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #monitor_state{}) -> {noreply, #monitor_state{}}.
handle_cast({record_event, Type, Details}, State) ->
    Event = #{
        timestamp => erlang:system_time(millisecond),
        type => Type,
        source => State#monitor_state.node_id,
        details => Details
    },
    NewHistory = add_event(Event, State#monitor_state.event_history,
                          State#monitor_state.history_limit),
    NewState = State#monitor_state{event_history = NewHistory},
    UpdatedState = check_alerts(Event, NewState),
    {noreply, UpdatedState};

handle_cast({record_metric, Name, Value, Tags}, State) ->
    Point = #{
        timestamp => erlang:system_time(millisecond),
        value => Value,
        tags => Tags
    },
    NewHistory = add_metric_point(Name, Point, State#monitor_state.metric_history,
                                 State#monitor_state.history_limit),
    NewState = State#monitor_state{metric_history = NewHistory},
    emit_otel_metric(Name, Value, Tags),
    {noreply, NewState};

handle_cast({subscribe_alerts, Subscriber}, State) ->
    monitor(process, Subscriber),
    NewSubscribers = sets:add_element(Subscriber, State#monitor_state.alert_subscribers),
    {noreply, State#monitor_state{alert_subscribers = NewSubscribers}};

handle_cast({unsubscribe_alerts, Subscriber}, State) ->
    NewSubscribers = sets:del_element(Subscriber, State#monitor_state.alert_subscribers),
    {noreply, State#monitor_state{alert_subscribers = NewSubscribers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #monitor_state{}) -> {noreply, #monitor_state{}}.
handle_info(collect_metrics, State) ->
    NewState = collect_cluster_metrics(State),
    TimerRef = erlang:send_after(State#monitor_state.collection_interval,
                                self(), collect_metrics),
    {noreply, NewState#monitor_state{timer_ref = TimerRef}};

handle_info({'EXIT', Pid, _Reason}, State) ->
    NewSubscribers = sets:filter(fun(S) -> S =/= Pid end,
                                 State#monitor_state.alert_subscribers),
    {noreply, State#monitor_state{alert_subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #monitor_state{}) -> ok.
terminate(_Reason, #monitor_state{timer_ref = Ref}) ->
    case Ref of
        undefined -> ok;
        _ -> erlang:cancel_timer(Ref)
    end,
    logger:info("Cluster monitor terminating"),
    ok.

-spec code_change(term(), #monitor_state{}, term()) -> {ok, #monitor_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

initialize_metrics() ->
    #{
        nodes_up => 0,
        nodes_down => 0,
        messages_sent => 0,
        messages_received => 0,
        sync_operations => 0,
        elections => 0,
        partitions => 0,
        conflicts => 0
    }.

add_event(Event, History, Limit) ->
    NewHistory = [Event | History],
    case length(NewHistory) > Limit of
        true -> lists:sublist(NewHistory, Limit);
        false -> NewHistory
    end.

add_metric_point(Name, Point, History, Limit) ->
    Current = maps:get(Name, History, []),
    NewPoints = [Point | Current],
    NewHistory = case length(NewPoints) > Limit of
        true -> maps:put(Name, lists:sublist(NewPoints, Limit), History);
        false -> maps:put(Name, NewPoints, History)
    end,
    NewHistory.

collect_cluster_metrics(State) ->
    {ok, Members} = erlmcp_cluster_membership:get_members(),
    ConnectedNodes = nodes(),
    UpNodes = length(ConnectedNodes) + 1,
    TotalNodes = length(Members),
    NewHealth = case UpNodes of
        N when N >= TotalNodes * 0.8 -> healthy;
        N when N >= TotalNodes * 0.5 -> degraded;
        _ -> critical
    end,
    NewMetrics = State#monitor_state.metrics#{
        nodes_up => UpNodes,
        nodes_down => TotalNodes - UpNodes
    },
    State#monitor_state{health = NewHealth, metrics = NewMetrics}.

check_alerts(#{type := <<"node_down">>} = Event, State) ->
    create_alert(critical, <<"node_down">>,
                 io_lib:format("Node down detected: ~p", [maps:get(details, Event)]),
                 State);
check_alerts(#{type := <<"partition_detected">>} = Event, State) ->
    create_alert(error, <<"partition">>,
                 io_lib:format("Network partition detected: ~p", [maps:get(details, Event)]),
                 State);
check_alerts(#{type := <<"quorum_lost">>} = Event, State) ->
    create_alert(critical, <<"quorum_lost">>,
                 io_lib:format("Quorum lost: ~p", [maps:get(details, Event)]),
                 State);
check_alerts(_Event, State) ->
    State.

create_alert(Severity, Type, Message, State) ->
    Alert = #{
        id => generate_alert_id(),
        severity => Severity,
        type => Type,
        message => iolist_to_binary(Message),
        timestamp => erlang:system_time(millisecond),
        resolved => false
    },
    NewAlerts = [Alert | State#monitor_state.alerts],
    notify_alert_subscribers(State, Alert),
    State#monitor_state{alerts = NewAlerts}.

notify_alert_subscribers(#monitor_state{alert_subscribers = Subscribers}, Alert) ->
    sets:foreach(fun(Subscriber) ->
        case is_process_alive(Subscriber) of
            true -> Subscriber ! {cluster_alert, Alert};
            false -> ok
        end
    end, Subscribers),
    ok.

generate_alert_id() ->
    <<(binary:encode_hex(crypto:strong_rand_bytes(8)))/binary>>.

get_health_details(State) ->
    #{
        nodes_up => maps:get(nodes_up, State#monitor_state.metrics),
        nodes_down => maps:get(nodes_down, State#monitor_state.metrics),
        active_alerts => length([A || A <- State#monitor_state.alerts,
                                  not maps:get(resolved, A)])
    }.

start_collection_timer(#monitor_state{collection_interval = Interval} = State) ->
    Ref = erlang:send_after(Interval, self(), collect_metrics),
    State#monitor_state{timer_ref = Ref}.

emit_otel_metric(Name, Value, Tags) ->
    try
        case whereis(erlmcp_otel) of
            undefined -> ok;
            _ -> erlmcp_otel:record_metric(Name, Value, Tags)
        end
    catch
        _:_ -> ok
    end.
