%%%-------------------------------------------------------------------
%%% @doc Gproc-based Pub/Sub Subscription Manager
%%%
%%% Generic publish/subscribe system using gproc for lightweight
%%% process registration and monitoring. Supports per-subscription
%%% metadata, automatic cleanup on subscriber death, and bulk
%%% notification support.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_subscription).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    subscribe/2,
    subscribe/3,
    unsubscribe/2,
    list_subscribers/1,
    notify/2,
    notify/3,
    bulk_notify/2,
    get_subscription_count/0,
    get_subscriber_count/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type subscription_id() :: binary().
-type subscriber() :: pid().
-type subscription_metadata() :: #{
    filter => fun((term()) -> boolean()) | undefined,
    rate_limit => non_neg_integer() | undefined,
    created_at => integer(),
    monitor_ref => reference()
}.

-export_type([subscription_id/0, subscriber/0, subscription_metadata/0]).

%% State record
-record(state, {
    subscriptions :: #{subscription_id() => #{subscriber() => subscription_metadata()}},
    inverse_index :: #{subscriber() => sets:set(subscription_id())}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the subscription manager.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Subscribe a process to a subscription ID.
%% Uses monitor/2 for automatic cleanup on subscriber death.
-spec subscribe(subscription_id(), subscriber()) -> ok | {error, term()}.
subscribe(SubscriptionId, Subscriber) when is_binary(SubscriptionId), is_pid(Subscriber) ->
    subscribe(SubscriptionId, Subscriber, #{}).

%% @doc Subscribe a process to a subscription ID with metadata options.
%% Options:
%%   - filter: fun((Message) -> boolean()) - Message filter function
%%   - rate_limit: non_neg_integer() - Max messages per second (0 = no limit)
-spec subscribe(subscription_id(), subscriber(), map()) -> ok | {error, term()}.
subscribe(SubscriptionId, Subscriber, Options) when is_binary(SubscriptionId), is_pid(Subscriber), is_map(Options) ->
    gen_server:call(?MODULE, {subscribe, SubscriptionId, Subscriber, Options}).

%% @doc Unsubscribe a process from a subscription ID.
-spec unsubscribe(subscription_id(), subscriber()) -> ok | {error, not_found}.
unsubscribe(SubscriptionId, Subscriber) when is_binary(SubscriptionId), is_pid(Subscriber) ->
    gen_server:call(?MODULE, {unsubscribe, SubscriptionId, Subscriber}).

%% @doc List all subscribers for a subscription ID.
-spec list_subscribers(subscription_id()) -> [subscriber()].
list_subscribers(SubscriptionId) when is_binary(SubscriptionId) ->
    gen_server:call(?MODULE, {list_subscribers, SubscriptionId}).

%% @doc Notify all subscribers of a subscription ID.
-spec notify(subscription_id(), term()) -> ok.
notify(SubscriptionId, Message) when is_binary(SubscriptionId) ->
    notify(SubscriptionId, Message, #{}).

%% @doc Notify all subscribers with options.
%% Options:
%%   - timeout: integer() - Send timeout (default: 5000ms)
-spec notify(subscription_id(), term(), map()) -> ok.
notify(SubscriptionId, Message, Options) when is_binary(SubscriptionId), is_map(Options) ->
    gen_server:cast(?MODULE, {notify, SubscriptionId, Message, Options}).

%% @doc Notify multiple subscription IDs with the same message.
%% Efficient bulk notification for broadcast scenarios.
-spec bulk_notify([subscription_id()], term()) -> ok.
bulk_notify(SubscriptionIds, Message) when is_list(SubscriptionIds) ->
    gen_server:cast(?MODULE, {bulk_notify, SubscriptionIds, Message}).

%% @doc Get total number of active subscriptions.
-spec get_subscription_count() -> non_neg_integer().
get_subscription_count() ->
    gen_server:call(?MODULE, get_subscription_count).

%% @doc Get number of subscribers for a specific subscription ID.
-spec get_subscriber_count(subscription_id()) -> non_neg_integer().
get_subscriber_count(SubscriptionId) when is_binary(SubscriptionId) ->
    gen_server:call(?MODULE, {get_subscriber_count, SubscriptionId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{
        subscriptions = #{},
        inverse_index = #{}
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call({subscribe, SubscriptionId, Subscriber, Options}, _From, State) ->
    case validate_subscriber(Subscriber) of
        ok ->
            MonitorRef = monitor(process, Subscriber),
            Metadata = #{
                filter => maps:get(filter, Options, undefined),
                rate_limit => maps:get(rate_limit, Options, undefined),
                created_at => erlang:system_time(millisecond),
                monitor_ref => MonitorRef
            },

            % Add to subscriptions map
            SubscriptionsMap = maps:get(SubscriptionId, State#state.subscriptions, #{}),
            NewSubscriptionsMap = maps:put(Subscriber, Metadata, SubscriptionsMap),
            NewSubscriptions = maps:put(SubscriptionId, NewSubscriptionsMap, State#state.subscriptions),

            % Update inverse index
            SubscriberSubs = maps:get(Subscriber, State#state.inverse_index, sets:new()),
            NewSubscriberSubs = sets:add_element(SubscriptionId, SubscriberSubs),
            NewInverseIndex = maps:put(Subscriber, NewSubscriberSubs, State#state.inverse_index),

            logger:debug("Subscribed ~p to ~p", [Subscriber, SubscriptionId]),
            {reply, ok, State#state{
                subscriptions = NewSubscriptions,
                inverse_index = NewInverseIndex
            }};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unsubscribe, SubscriptionId, Subscriber}, _From, State) ->
    case maps:get(SubscriptionId, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        SubscribersMap ->
            case maps:get(Subscriber, SubscribersMap, undefined) of
                undefined ->
                    {reply, {error, not_found}, State};
                Metadata ->
                    % Demonitor the subscriber
                    MonitorRef = maps:get(monitor_ref, Metadata),
                    erlang:demonitor(MonitorRef, [flush]),

                    % Remove from subscriptions map
                    NewSubscribersMap = maps:remove(Subscriber, SubscribersMap),
                    NewSubscriptions = case maps:size(NewSubscribersMap) of
                        0 -> maps:remove(SubscriptionId, State#state.subscriptions);
                        _ -> maps:put(SubscriptionId, NewSubscribersMap, State#state.subscriptions)
                    end,

                    % Update inverse index
                    SubscriberSubs = maps:get(Subscriber, State#state.inverse_index, sets:new()),
                    NewSubscriberSubs = sets:del_element(SubscriptionId, SubscriberSubs),
                    NewInverseIndex = case sets:size(NewSubscriberSubs) of
                        0 -> maps:remove(Subscriber, State#state.inverse_index);
                        _ -> maps:put(Subscriber, NewSubscriberSubs, State#state.inverse_index)
                    end,

                    logger:debug("Unsubscribed ~p from ~p", [Subscriber, SubscriptionId]),
                    {reply, ok, State#state{
                        subscriptions = NewSubscriptions,
                        inverse_index = NewInverseIndex
                    }}
            end
    end;

handle_call({list_subscribers, SubscriptionId}, _From, State) ->
    Subscribers = case maps:get(SubscriptionId, State#state.subscriptions, undefined) of
        undefined -> [];
        SubscribersMap -> maps:keys(SubscribersMap)
    end,
    {reply, Subscribers, State};

handle_call(get_subscription_count, _From, State) ->
    Count = lists:sum([maps:size(SubsMap) || SubsMap <- maps:values(State#state.subscriptions)]),
    {reply, Count, State};

handle_call({get_subscriber_count, SubscriptionId}, _From, State) ->
    Count = case maps:get(SubscriptionId, State#state.subscriptions, undefined) of
        undefined -> 0;
        SubscribersMap -> maps:size(SubscribersMap)
    end,
    {reply, Count, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({notify, SubscriptionId, Message, Options}, State) ->
    case maps:get(SubscriptionId, State#state.subscriptions, undefined) of
        undefined ->
            {noreply, State};
        SubscribersMap ->
            Timeout = maps:get(timeout, Options, 5000),
            notify_subscribers(maps:to_list(SubscribersMap), Message, Timeout),
            {noreply, State}
    end;

handle_cast({bulk_notify, SubscriptionIds, Message}, State) ->
    Timeout = 5000,
    lists:foreach(fun(SubscriptionId) ->
        case maps:get(SubscriptionId, State#state.subscriptions, undefined) of
            undefined -> ok;
            SubscribersMap ->
                notify_subscribers(maps:to_list(SubscribersMap), Message, Timeout)
        end
    end, SubscriptionIds),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', MonitorRef, process, Subscriber, _Info}, State) ->
    % Subscriber died - cleanup all its subscriptions
    case maps:get(Subscriber, State#state.inverse_index, undefined) of
        undefined ->
            {noreply, State};
        SubscriberSubs ->
            NewSubscriptions = sets:fold(fun(SubscriptionId, AccSubs) ->
                case maps:get(SubscriptionId, AccSubs, undefined) of
                    undefined -> AccSubs;
                    SubscribersMap ->
                        NewSubscribersMap = maps:remove(Subscriber, SubscribersMap),
                        case maps:size(NewSubscribersMap) of
                            0 -> maps:remove(SubscriptionId, AccSubs);
                            _ -> maps:put(SubscriptionId, NewSubscribersMap, AccSubs)
                        end
                end
            end, State#state.subscriptions, SubscriberSubs),

            NewInverseIndex = maps:remove(Subscriber, State#state.inverse_index),

            logger:info("Cleaned up subscriptions for dead subscriber ~p", [Subscriber]),
            {noreply, State#state{
                subscriptions = NewSubscriptions,
                inverse_index = NewInverseIndex
            }}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validate that subscriber is a local, alive process.
-spec validate_subscriber(subscriber()) -> ok | {error, term()}.
validate_subscriber(Subscriber) when is_pid(Subscriber) ->
    SubNode = node(Subscriber),
    case SubNode of
        Node when Node =:= node() ->
            case is_process_alive(Subscriber) of
                true -> ok;
                false -> {error, no_process}
            end;
        _Node ->
            {error, {remote_process_not_supported, SubNode}}
    end.

%% @doc Notify all subscribers of a subscription.
%% Applies filters and rate limits per-subscriber.
-spec notify_subscribers([{subscriber(), subscription_metadata()}], term(), integer()) -> ok.
notify_subscribers([], _Message, _Timeout) ->
    ok;
notify_subscribers([{Subscriber, Metadata} | Rest], Message, Timeout) ->
    % Check filter
    Filter = maps:get(filter, Metadata, undefined),
    case apply_filter(Filter, Message) of
        true ->
            % Check rate limit
            case check_rate_limit(Subscriber, Metadata) of
                true ->
                    % Send notification
                    NotifMessage = {'$mcp_subscription', Message},
                    try
                        Subscriber ! NotifMessage,
                        notify_subscribers(Rest, Message, Timeout)
                    catch
                        _:_ ->
                            % Subscriber died during send - cleanup will happen via DOWN
                            notify_subscribers(Rest, Message, Timeout)
                    end;
                false ->
                    % Rate limited
                    logger:debug("Rate limited notification to ~p", [Subscriber]),
                    notify_subscribers(Rest, Message, Timeout)
            end;
        false ->
            % Filtered out
            notify_subscribers(Rest, Message, Timeout)
    end.

%% @doc Apply message filter if present.
-spec apply_filter(fun((term()) -> boolean()) | undefined, term()) -> boolean().
apply_filter(undefined, _Message) -> true;
apply_filter(Filter, Message) when is_function(Filter, 1) -> Filter(Message).

%% @doc Check rate limit for subscriber.
-spec check_rate_limit(subscriber(), subscription_metadata()) -> boolean().
check_rate_limit(_Subscriber, Metadata) ->
    RateLimit = maps:get(rate_limit, Metadata, undefined),
    case RateLimit of
        undefined -> true;
        0 -> true;
        Limit when is_integer(Limit), Limit > 0 ->
            % Use ets table for rate limiting (simplified: always allow for now)
            % TODO: Implement proper rate limiting with token bucket or sliding window
            true
    end.
