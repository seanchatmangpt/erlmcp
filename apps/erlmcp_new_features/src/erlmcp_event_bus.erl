-module(erlmcp_event_bus).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([subscribe/2]).
-export([unsubscribe/2]).
-export([publish/2]).
-export([publish_sync/2]).
-export([list_subscribers/1]).
-export([get_metrics/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_QUEUE_SIZE, 1000).

-type event_type() :: atom() | binary().
-type event() :: map().
-type subscriber() :: {pid(), reference()}.
-type subscription_id() :: reference().

-record(subscription, {
    id :: subscription_id(),
    pid :: pid(),
    event_type :: event_type(),
    filter :: fun((event()) -> boolean()) | undefined
}).

-record(metrics, {
    events_published = 0 :: non_neg_integer(),
    events_delivered = 0 :: non_neg_integer(),
    events_dropped = 0 :: non_neg_integer(),
    subscribers = 0 :: non_neg_integer()
}).

-record(state, {
    subscribers = #{},     % event_type => [subscription()]
    by_pid = #{},          % pid => [subscription_id()]
    metrics = #metrics{}
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec subscribe(event_type(), pid() | undefined) -> {ok, subscription_id()} | {error, term()}.
subscribe(EventType, Pid) when is_atom(EventType); is_binary(EventType) ->
    SubscriberPid = case Pid of
        undefined -> self();
        _ -> Pid
    end,
    gen_server:call(?SERVER, {subscribe, EventType, SubscriberPid}).

-spec unsubscribe(event_type(), subscription_id()) -> ok.
unsubscribe(EventType, SubscriptionId) ->
    gen_server:call(?SERVER, {unsubscribe, EventType, SubscriptionId}).

-spec publish(event_type(), event()) -> ok.
publish(EventType, Event) when is_map(Event) ->
    gen_server:cast(?SERVER, {publish, EventType, Event}).

-spec publish_sync(event_type(), event()) -> {ok, non_neg_integer()} | {error, term()}.
publish_sync(EventType, Event) when is_map(Event) ->
    gen_server:call(?SERVER, {publish_sync, EventType, Event}).

-spec list_subscribers(event_type()) -> [pid()].
list_subscribers(EventType) ->
    gen_server:call(?SERVER, {list_subscribers, EventType}).

-spec get_metrics() -> #{
    events_published => non_neg_integer(),
    events_delivered => non_neg_integer(),
    events_dropped => non_neg_integer(),
    subscribers => non_neg_integer()
}.
get_metrics() ->
    gen_server:call(?SERVER, get_metrics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    % Monitor the process for early detection of issues
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({subscribe, EventType, Pid}, _From, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    TraceContext = erlmcp_observability:trace_span(<<"event_bus_subscribe">>, #{
        event_type => atom_to_binary(EventType),
        subscriber_pid => pid_to_list(Pid)
    }),

    % Record subscription request
    erlmcp_observability:counter(<<"event_bus_subscribe_requests_total">>, #{
        service => ?MODULE
    }),

    case is_process_alive(Pid) of
        false ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,

            % Record dead process error
            erlmcp_observability:counter(<<"event_bus_subscribe_requests_total">>, #{
                service => ?MODULE,
                status => "error"
            }),
            erlmcp_observability:histogram_observe(<<"event_bus_subscribe_duration_ms">>, Duration),
            erlmcp_observability:counter(<<"event_bus_errors_total">>, #{
                type => "dead_process"
            }),

            erlmcp_observability:log(<<"event_bus_subscribe_failed">>, #{
                event_type => atom_to_binary(EventType),
                subscriber_pid => pid_to_list(Pid),
                duration => Duration,
                reason => "dead_process"
            }, TraceContext),

            {reply, {error, dead_process}, State};
        true ->
            SubscriptionId = make_ref(),
            Subscription = #subscription{
                id = SubscriptionId,
                pid = Pid,
                event_type = EventType
            },

            % Add to subscribers index
            CurrentSubscribers = maps:get(EventType, State#state.subscribers, []),
            NewSubscribers = [Subscription | CurrentSubscribers],
            NewSubscribersMap = maps:put(EventType, NewSubscribers, State#state.subscribers),

            % Add to pid index
            CurrentById = maps:get(Pid, State#state.by_pid, []),
            NewById = maps:put(Pid, [SubscriptionId | CurrentById], State#state.by_pid),

            % Update metrics
            SubCount = State#state.metrics#metrics.subscribers + 1,
            NewMetrics = State#state.metrics#metrics{subscribers = SubCount},

            % Monitor the subscriber
            erlang:monitor(process, Pid),

            Duration = erlang:monotonic_time(millisecond) - StartTime,

            % Record success metrics
            erlmcp_observability:counter(<<"event_bus_subscribe_requests_total">>, #{
                service => ?MODULE,
                status => "success"
            }),
            erlmcp_observability:histogram_observe(<<"event_bus_subscribe_duration_ms">>, Duration),
            erlmcp_observability:gauge_set(<<"event_bus_active_subscribers">>, #{
                service => ?MODULE
            }, SubCount),

            erlmcp_observability:log(<<"event_bus_subscribe_success">>, #{
                event_type => atom_to_binary(EventType),
                subscriber_pid => pid_to_list(Pid),
                duration => Duration,
                total_subscribers => SubCount
            }, TraceContext),

            {reply, {ok, SubscriptionId}, State#state{
                subscribers = NewSubscribersMap,
                by_pid = NewById,
                metrics = NewMetrics
            }}
    end;

handle_call({unsubscribe, EventType, SubscriptionId}, _From, State) ->
    case maps:get(EventType, State#state.subscribers, []) of
        [] ->
            {reply, ok, State};
        Subscribers ->
            Filtered = lists:filter(
                fun(S) -> S#subscription.id =/= SubscriptionId end,
                Subscribers
            ),
            NewSubscribersMap = maps:put(EventType, Filtered, State#state.subscribers),

            % Remove from pid index (simplified - in production track per sub)
            NewMetrics = State#state.metrics#metrics{
                subscribers = max(0, State#state.metrics#metrics.subscribers - 1)
            },

            {reply, ok, State#state{
                subscribers = NewSubscribersMap,
                metrics = NewMetrics
            }}
    end;

handle_call({list_subscribers, EventType}, _From, State) ->
    Subscribers = maps:get(EventType, State#state.subscribers, []),
    Pids = [S#subscription.pid || S <- Subscribers, is_process_alive(S#subscription.pid)],
    {reply, Pids, State};

handle_call({publish_sync, EventType, Event}, _From, State) ->
    #state{subscribers = SubscribersMap, metrics = Metrics0} = State,
    Subscribers = maps:get(EventType, SubscribersMap, []),

    % Filter to alive subscribers
    AliveSubs = [S || S <- Subscribers, is_process_alive(S#subscription.pid)],

    % Deliver to each subscriber
    Delivered = lists:foldl(
        fun(#subscription{pid = Pid}, Count) ->
            case erlang:send(Pid, {event, EventType, Event}, [noconnect]) of
                ok -> Count + 1;
                _ -> Count
            end
        end,
        0,
        AliveSubs
    ),

    % Update metrics
    NewMetrics = Metrics0#metrics{
        events_published = Metrics0#metrics.events_published + 1,
        events_delivered = Metrics0#metrics.events_delivered + Delivered,
        events_dropped = Metrics0#metrics.events_dropped + (length(AliveSubs) - Delivered)
    },

    {reply, {ok, Delivered}, State#state{metrics = NewMetrics}};

handle_call(get_metrics, _From, State) ->
    #metrics{events_published = EP, events_delivered = ED,
             events_dropped = EDr, subscribers = S} = State#state.metrics,
    MetricsMap = #{
        events_published => EP,
        events_delivered => ED,
        events_dropped => EDr,
        subscribers => S
    },
    {reply, MetricsMap, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({publish, EventType, Event}, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    TraceContext = erlmcp_observability:trace_span(<<"event_bus_publish">>, #{
        event_type => atom_to_binary(EventType),
        event_size => size(jsone:encode(Event))
    }),

    % Record publish request
    erlmcp_observability:counter(<<"event_bus_publish_requests_total">>, #{
        service => ?MODULE
    }),

    #state{subscribers = SubscribersMap, metrics = Metrics0} = State,
    Subscribers = maps:get(EventType, SubscribersMap, []),

    % Filter to alive subscribers
    AliveSubs = [S || S <- Subscribers, is_process_alive(S#subscription.pid)],

    % Deliver to each subscriber asynchronously
    lists:foreach(
        fun(#subscription{pid = Pid}) ->
            Pid ! {event, EventType, Event}
        end,
        AliveSubs
    ),

    % Update metrics
    DeliveredCount = length(AliveSubs),
    NewMetrics = Metrics0#metrics{
        events_published = Metrics0#metrics.events_published + 1,
        events_delivered = Metrics0#metrics.events_delivered + DeliveredCount,
        events_dropped = Metrics0#metrics.events_dropped + (length(Subscribers) - DeliveredCount)
    },

    Duration = erlang:monotonic_time(millisecond) - StartTime,

    % Record publish metrics
    erlmcp_observability:counter(<<"event_bus_publish_requests_total">>, #{
        service => ?MODULE,
        status => "success"
    }),
    erlmcp_observability:histogram_observe(<<"event_bus_publish_duration_ms">>, Duration),
    erlmcp_observability:histogram_observe(<<"event_bus_subscribers_per_event">>, DeliveredCount),

    % Update active connections gauge
    erlmcp_observability:gauge_set(<<"event_bus_active_publishers">>, #{
        service => ?MODULE
    }, 1),

    % Log publish activity
    erlmcp_observability:log(<<"event_published">>, #{
        event_type => atom_to_binary(EventType),
        total_subscribers => length(Subscribers),
        alive_subscribers => DeliveredCount,
        event_size => size(jsone:encode(Event)),
        duration => Duration
    }, TraceContext),

    {noreply, State#state{metrics = NewMetrics}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    % Remove all subscriptions for dead process
    SubIds = maps:get(Pid, State#state.by_pid, []),

    NewSubscribersMap = lists:foldl(
        fun(SubId, Acc) ->
            maps:map(
                fun(_EventType, Subs) ->
                    lists:filter(
                        fun(S) -> S#subscription.id =/= SubId end,
                        Subs
                    )
                end,
                Acc
            )
        end,
        State#state.subscribers,
        SubIds
    ),

    NewById = maps:remove(Pid, State#state.by_pid),

    NewMetrics = State#state.metrics#metrics{
        subscribers = max(0, State#state.metrics#metrics.subscribers - length(SubIds))
    },

    {noreply, State#state{
        subscribers = NewSubscribersMap,
        by_pid = NewById,
        metrics = NewMetrics
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
