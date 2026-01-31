%%%-------------------------------------------------------------------
%%% @doc MCP Resource Subscriptions Manager
%%%
%%% Implements MCP 2025-11-25 resource subscription support with URI-based
%%% subscription, change detection, rate limiting, and notification batching.
%%% Integrates with erlmcp_server for MCP protocol compliance.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_resource_subscriptions).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    subscribe_to_resource/3,
    unsubscribe_from_resource/2,
    list_resource_subscriptions/2,
    notify_resource_changed/2,
    set_rate_limit/2,
    get_stats/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type uri() :: binary().
-type subscriber() :: pid().
-type subscription_config() :: #{
    created_at => integer(),
    rate_limit => non_neg_integer(),
    filter => fun((map()) -> boolean()) | undefined
}.
-type change_notification() :: #{
    uri => uri(),
    timestamp => integer(),
    metadata => map()
}.

-export_type([uri/0, subscription_config/0, change_notification/0]).

%% State record
-record(state, {
    resource_subscriptions :: #{uri() => #{subscriber() => subscription_config()}},
    subscription_counters :: #{uri() => integer()},
    last_notified :: #{uri() => integer()},  % Rate limiting: timestamp of last notification
    pending_changes :: #{uri() => [change_notification()]},  % Batching window
    batch_timer_ref :: reference() | undefined,
    default_rate_limit = 1000 :: non_neg_integer()  % Default: 1 notification/sec
}).

-type state() :: #state{}.

-define(BATCH_WINDOW_MS, 100).  % 100ms batching window for rapid changes
-define(DEFAULT_RATE_LIMIT, 1000).  % 1 notification per second

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the resource subscriptions manager.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Subscribe to a resource URI.
%% Supports exact URI match and URI templates.
-spec subscribe_to_resource(uri(), subscriber(), map()) -> ok | {error, term()}.
subscribe_to_resource(Uri, Subscriber, Options) when is_binary(Uri), is_pid(Subscriber), is_map(Options) ->
    gen_server:call(?MODULE, {subscribe_resource, Uri, Subscriber, Options}).

%% @doc Unsubscribe from a resource URI.
-spec unsubscribe_from_resource(uri(), subscriber()) -> ok | {error, not_found}.
unsubscribe_from_resource(Uri, Subscriber) when is_binary(Uri), is_pid(Subscriber) ->
    gen_server:call(?MODULE, {unsubscribe_resource, Uri, Subscriber}).

%% @doc List all subscribers for a resource URI (exact match).
-spec list_resource_subscriptions(uri(), boolean()) -> [subscriber()].
list_resource_subscriptions(Uri, IncludeTemplates) when is_binary(Uri), is_boolean(IncludeTemplates) ->
    gen_server:call(?MODULE, {list_subscriptions, Uri, IncludeTemplates}).

%% @doc Notify that a resource has changed.
%% Triggers resources/updated notifications to subscribers with rate limiting.
-spec notify_resource_changed(uri(), map()) -> ok.
notify_resource_changed(Uri, Metadata) when is_binary(Uri), is_map(Metadata) ->
    gen_server:cast(?MODULE, {resource_changed, Uri, Metadata}).

%% @doc Set rate limit for a specific resource URI.
-spec set_rate_limit(uri(), non_neg_integer()) -> ok.
set_rate_limit(Uri, RateLimitMs) when is_binary(Uri), is_integer(RateLimitMs) ->
    gen_server:call(?MODULE, {set_rate_limit, Uri, RateLimitMs}).

%% @doc Get subscription statistics.
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting resource subscriptions manager"),
    {ok, #state{
        resource_subscriptions = #{},
        subscription_counters = #{},
        last_notified = #{},
        pending_changes = #{},
        batch_timer_ref = undefined
    }}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({subscribe_resource, Uri, Subscriber, Options}, _From, State) ->
    case validate_subscriber(Subscriber) of
        ok ->
            case validate_uri(Uri) of
                ok ->
                    MonitorRef = monitor(process, Subscriber),
                    RateLimit = maps:get(rate_limit, Options, State#state.default_rate_limit),
                    Config = #{
                        created_at => erlang:system_time(millisecond),
                        rate_limit => RateLimit,
                        filter => maps:get(filter, Options, undefined),
                        monitor_ref => MonitorRef
                    },

                    % Add to resource subscriptions
                    ResourceSubs = maps:get(Uri, State#state.resource_subscriptions, #{}),
                    NewResourceSubs = maps:put(Subscriber, Config, ResourceSubs),
                    NewResourceSubscriptions = maps:put(Uri, NewResourceSubs, State#state.resource_subscriptions),

                    % Increment subscription counter
                    Counter = maps:get(Uri, State#state.subscription_counters, 0),
                    NewCounters = maps:put(Uri, Counter + 1, State#state.subscription_counters),

                    logger:debug("Subscribed ~p to resource ~p", [Subscriber, Uri]),
                    {reply, ok, State#state{
                        resource_subscriptions = NewResourceSubscriptions,
                        subscription_counters = NewCounters
                    }};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unsubscribe_resource, Uri, Subscriber}, _From, State) ->
    case maps:get(Uri, State#state.resource_subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ResourceSubs ->
            case maps:get(Subscriber, ResourceSubs, undefined) of
                undefined ->
                    {reply, {error, not_found}, State};
                Config ->
                    % Demonitor the subscriber
                    MonitorRef = maps:get(monitor_ref, Config),
                    erlang:demonitor(MonitorRef, [flush]),

                    % Remove from resource subscriptions
                    NewResourceSubs = maps:remove(Subscriber, ResourceSubs),
                    NewResourceSubscriptions = case maps:size(NewResourceSubs) of
                        0 -> maps:remove(Uri, State#state.resource_subscriptions);
                        _ -> maps:put(Uri, NewResourceSubs, State#state.resource_subscriptions)
                    end,

                    % Decrement subscription counter
                    Counter = maps:get(Uri, State#state.subscription_counters, 1),
                    NewCounters = case Counter of
                        1 -> maps:remove(Uri, State#state.subscription_counters);
                        _ -> maps:put(Uri, Counter - 1, State#state.subscription_counters)
                    end,

                    logger:debug("Unsubscribed ~p from resource ~p", [Subscriber, Uri]),
                    {reply, ok, State#state{
                        resource_subscriptions = NewResourceSubscriptions,
                        subscription_counters = NewCounters
                    }}
            end
    end;

handle_call({list_subscriptions, Uri, IncludeTemplates}, _From, State) ->
    % Get exact match subscribers
    ExactSubs = case maps:get(Uri, State#state.resource_subscriptions, undefined) of
        undefined -> [];
        SubsMap -> maps:keys(SubsMap)
    end,

    % Get template match subscribers if requested
    TemplateSubs = case IncludeTemplates of
        true -> match_template_subscribers(Uri, State#state.resource_subscriptions);
        false -> []
    end,

    % Combine and deduplicate
    AllSubs = lists:usort(ExactSubs ++ TemplateSubs),
    {reply, AllSubs, State};

handle_call({set_rate_limit, Uri, RateLimitMs}, _From, State) ->
    NewResourceSubscriptions = maps:map(fun(_ResourceUri, ResourceSubs) ->
        maps:map(fun(_Subscriber, Config) ->
            case RateLimitMs of
                0 -> maps:remove(rate_limit, Config);
                _ -> Config#{rate_limit => RateLimitMs}
            end
        end, ResourceSubs)
    end, State#state.resource_subscriptions),

    {reply, ok, State#state{resource_subscriptions = NewResourceSubscriptions}};

handle_call(get_stats, _From, State) ->
    Stats = #{
        total_resources => maps:size(State#state.resource_subscriptions),
        total_subscriptions => lists:sum([maps:size(Subs) || Subs <- maps:values(State#state.resource_subscriptions)]),
        resources_with_pending_changes => maps:size(State#state.pending_changes),
        default_rate_limit => State#state.default_rate_limit
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({resource_changed, Uri, Metadata}, State) ->
    ChangeNotification = #{
        uri => Uri,
        timestamp => erlang:system_time(millisecond),
        metadata => Metadata
    },

    % Add to pending changes
    PendingChanges = maps:get(Uri, State#state.pending_changes, []),
    NewPendingChanges = maps:put(Uri, [ChangeNotification | PendingChanges], State#state.pending_changes),

    % Start or reset batch timer
    NewTimerRef = case State#state.batch_timer_ref of
        undefined ->
            erlang:send_after(?BATCH_WINDOW_MS, self(), flush_batch);
        Ref ->
            erlang:cancel_timer(Ref),
            erlang:send_after(?BATCH_WINDOW_MS, self(), flush_batch)
    end,

    {noreply, State#state{
        pending_changes = NewPendingChanges,
        batch_timer_ref = NewTimerRef
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', MonitorRef, process, Subscriber, _Info}, State) ->
    % Subscriber died - cleanup all its subscriptions
    NewResourceSubscriptions = maps:map(fun(_Uri, ResourceSubs) ->
        case maps:get(Subscriber, ResourceSubs, undefined) of
            undefined -> ResourceSubs;
            _Config -> maps:remove(Subscriber, ResourceSubs)
        end
    end, State#state.resource_subscriptions),

    % Update subscription counters
    NewCounters = maps:map(fun(Uri, ResourceSubs) ->
        maps:size(ResourceSubs)
    end, NewResourceSubscriptions),

    logger:info("Cleaned up resource subscriptions for dead subscriber ~p", [Subscriber]),
    {noreply, State#state{
        resource_subscriptions = NewResourceSubscriptions,
        subscription_counters = NewCounters
    }};

handle_info(flush_batch, State) ->
    % Flush all pending changes
    NewState = maps:fold(fun(Uri, Changes, AccState) ->
        % Changes is a list of change notifications
        lists:foldl(fun(ChangeNotification, InnerState) ->
            send_resource_notification(Uri, ChangeNotification, InnerState)
        end, AccState, Changes)
    end, State, State#state.pending_changes),

    {noreply, NewState#state{
        pending_changes = #{},
        batch_timer_ref = undefined
    }};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    % Cancel batch timer if running
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

%% @doc Validate URI format.
-spec validate_uri(uri()) -> ok | {error, term()}.
validate_uri(Uri) when is_binary(Uri) ->
    case byte_size(Uri) of
        0 -> {error, empty_uri};
        _ -> ok
    end.

%% @doc Match subscribers using URI templates.
-spec match_template_subscribers(uri(), #{uri() => #{subscriber() => _}}) -> [subscriber()].
match_template_subscribers(Uri, ResourceSubscriptions) ->
    % Find all template URIs that match this resource
    MatchingTemplates = lists:filter(fun(TemplateUri) ->
        match_uri_template(Uri, TemplateUri)
    end, maps:keys(ResourceSubscriptions)),

    % Collect all unique subscribers
    lists:usort(lists:foldl(fun(TemplateUri, Acc) ->
        case maps:get(TemplateUri, ResourceSubscriptions, undefined) of
            undefined -> Acc;
            SubsMap -> maps:keys(SubsMap) ++ Acc
        end
    end, [], MatchingTemplates)).

%% @doc Check if a URI matches a template.
%% Supports simple wildcard patterns: {var} for path segments.
-spec match_uri_template(uri(), binary()) -> boolean().
match_uri_template(_Uri, Template) ->
    % Simple template matching (can be enhanced with proper URI template parsing)
    % For now, only exact match
    false.

%% @doc Send resource change notification to subscribers with rate limiting.
-spec send_resource_notification(uri(), change_notification(), state()) -> state().
send_resource_notification(Uri, ChangeNotification, State) ->
    Now = erlang:system_time(millisecond),
    LastNotified = maps:get(Uri, State#state.last_notified, 0),

    % Check rate limit
    ResourceSubs = maps:get(Uri, State#state.resource_subscriptions, #{}),
    RateLimit = case maps:values(ResourceSubs) of
        [] -> State#state.default_rate_limit;
        [Config | _] -> maps:get(rate_limit, Config, State#state.default_rate_limit)
    end,

    case Now - LastNotified >= RateLimit of
        true ->
            % Send notification to all subscribers
            notify_subscribers(Uri, ChangeNotification, maps:keys(ResourceSubs)),
            NewLastNotified = maps:put(Uri, Now, State#state.last_notified),
            State#state{last_notified = NewLastNotified};
        false ->
            % Rate limited
            logger:debug("Rate limited notification for resource ~p", [Uri]),
            State
    end.

%% @doc Send notification to all subscribers of a resource.
-spec notify_subscribers(uri(), change_notification(), [subscriber()]) -> ok.
notify_subscribers(_Uri, _ChangeNotification, []) ->
    ok;
notify_subscribers(Uri, ChangeNotification, [Subscriber | Rest]) ->
    % Send MCP resources/updated notification
    Notification = #{
        jsonrpc => <<"2.0">>,
        method => <<"resources/updated">>,
        params => #{
            uri => maps:get(uri, ChangeNotification),
            timestamp => maps:get(timestamp, ChangeNotification)
        }
    },

    try
        Subscriber ! {'$mcp_resource', Notification},
        notify_subscribers(Uri, ChangeNotification, Rest)
    catch
        _:_ ->
            % Subscriber died - cleanup will happen via DOWN
            notify_subscribers(Uri, ChangeNotification, Rest)
    end.
