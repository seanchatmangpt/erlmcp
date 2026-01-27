-module(erlmcp_change_notifier).
-behaviour(gen_server).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API exports
-export([
    start_link/0,
    subscribe_to_changes/2,
    unsubscribe_from_changes/2,
    notify_list_changed/1,
    get_subscribers/1,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type feature() :: prompts | resources | tools.
-type client_pid() :: pid().

-export_type([feature/0]).

%% State record
-record(state, {
    subscribers = #{} :: #{feature() => sets:set(client_pid())},
    monitors = #{} :: #{client_pid() => reference()}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec subscribe_to_changes(feature(), client_pid()) -> ok | {error, term()}.
subscribe_to_changes(Feature, ClientPid) when is_pid(ClientPid), is_atom(Feature) ->
    gen_server:call(?MODULE, {subscribe, Feature, ClientPid}).

-spec unsubscribe_from_changes(feature(), client_pid()) -> ok.
unsubscribe_from_changes(Feature, ClientPid) when is_pid(ClientPid), is_atom(Feature) ->
    gen_server:call(?MODULE, {unsubscribe, Feature, ClientPid}).

-spec notify_list_changed(feature()) -> ok.
notify_list_changed(Feature) when is_atom(Feature) ->
    gen_server:cast(?MODULE, {notify_list_changed, Feature}).

-spec get_subscribers(feature()) -> [client_pid()].
get_subscribers(Feature) when is_atom(Feature) ->
    gen_server:call(?MODULE, {get_subscribers, Feature}).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting MCP change notifier"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.

handle_call({subscribe, Feature, ClientPid}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"change_notifier.subscribe">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"feature">> => atom_to_binary(Feature, utf8),
            <<"client_pid">> => erlmcp_tracing:pid_to_string(ClientPid)
        }),

        % Get current set of subscribers for this feature
        Subscribers = maps:get(Feature, State#state.subscribers, sets:new()),

        % Check if already subscribed
        case sets:is_element(ClientPid, Subscribers) of
            true ->
                erlmcp_tracing:set_status(SpanCtx, ok),
                {reply, ok, State};
            false ->
                % Add to subscribers
                NewSubscribers = sets:add_element(ClientPid, Subscribers),
                NewFeatureMap = maps:put(Feature, NewSubscribers, State#state.subscribers),

                % Monitor the client if not already monitored
                NewMonitors = case maps:is_key(ClientPid, State#state.monitors) of
                    true -> State#state.monitors;
                    false ->
                        Ref = monitor(process, ClientPid),
                        maps:put(ClientPid, Ref, State#state.monitors)
                end,

                erlmcp_tracing:set_status(SpanCtx, ok),
                NewState = State#state{subscribers = NewFeatureMap, monitors = NewMonitors},
                {reply, ok, NewState}
        end
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            {reply, {error, Reason}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call({unsubscribe, Feature, ClientPid}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"change_notifier.unsubscribe">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"feature">> => atom_to_binary(Feature, utf8),
            <<"client_pid">> => erlmcp_tracing:pid_to_string(ClientPid)
        }),

        % Get current set of subscribers for this feature
        Subscribers = maps:get(Feature, State#state.subscribers, sets:new()),
        NewSubscribers = sets:del_element(ClientPid, Subscribers),
        NewFeatureMap = maps:put(Feature, NewSubscribers, State#state.subscribers),

        % Check if client has subscriptions to other features
        HasOtherSubscriptions = maps:any(fun(F, S) ->
            (F =/= Feature) andalso sets:is_element(ClientPid, S)
        end, NewFeatureMap),

        % Cleanup monitor if no more subscriptions
        NewMonitors = case HasOtherSubscriptions of
            true -> State#state.monitors;
            false ->
                case maps:get(ClientPid, State#state.monitors, undefined) of
                    undefined -> State#state.monitors;
                    Ref ->
                        demonitor(Ref, [flush]),
                        maps:remove(ClientPid, State#state.monitors)
                end
        end,

        erlmcp_tracing:set_status(SpanCtx, ok),
        NewState = State#state{subscribers = NewFeatureMap, monitors = NewMonitors},
        {reply, ok, NewState}
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            {reply, {error, Reason}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call({get_subscribers, Feature}, _From, State) ->
    Subscribers = maps:get(Feature, State#state.subscribers, sets:new()),
    SubscriberList = sets:to_list(Subscribers),
    {reply, SubscriberList, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({notify_list_changed, Feature}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"change_notifier.notify_list_changed">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"feature">> => atom_to_binary(Feature, utf8)
        }),

        % Get the notification method
        Method = notification_method(Feature),

        % Get all subscribers for this feature
        Subscribers = maps:get(Feature, State#state.subscribers, sets:new()),

        % Send notification to each subscriber
        NumSubscribers = sets:size(Subscribers),
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"subscriber_count">> => NumSubscribers
        }),

        sets:fold(fun(ClientPid, _) ->
            send_notification_to_client(ClientPid, Method)
        end, ok, Subscribers),

        erlmcp_tracing:set_status(SpanCtx, ok),
        {noreply, State}
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
    % Client process died - clean up all subscriptions
    SpanCtx = erlmcp_tracing:start_span(<<"change_notifier.cleanup_client">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"client_pid">> => erlmcp_tracing:pid_to_string(ClientPid)
        }),

        % Remove from all subscriber sets
        NewSubscribers = maps:map(fun(_Feature, Subs) ->
            sets:del_element(ClientPid, Subs)
        end, State#state.subscribers),

        % Remove monitor
        NewMonitors = maps:remove(ClientPid, State#state.monitors),

        logger:info("Cleaned up subscriptions for dead client ~p", [ClientPid]),
        erlmcp_tracing:set_status(SpanCtx, ok),

        {noreply, State#state{subscribers = NewSubscribers, monitors = NewMonitors}}
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("MCP change notifier terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal helper functions
%%====================================================================

-spec notification_method(feature()) -> binary().
notification_method(prompts) ->
    ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED;
notification_method(resources) ->
    ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED;
notification_method(tools) ->
    ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED;
notification_method(_) ->
    error(invalid_feature).

-spec send_notification_to_client(client_pid(), binary()) -> ok.
send_notification_to_client(ClientPid, Method) ->
    try
        % Build JSON-RPC notification
        Notification = erlmcp_json_rpc:encode_notification(Method, #{}),

        % Send to client as {list_changed_notification, Method, Data}
        ClientPid ! {list_changed_notification, Method, Notification},
        ok
    catch
        Class:Reason ->
            logger:warning("Failed to send notification to client ~p: ~p:~p",
                          [ClientPid, Class, Reason]),
            ok
    end.
