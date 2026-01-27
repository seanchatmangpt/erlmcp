-module(erlmcp_tool_change_notifier).
-behaviour(gen_server).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API exports
-export([
    start_link/0,
    notify_tool_added/2,
    notify_tool_removed/2,
    notify_tool_updated/2,
    subscribe_to_changes/1,
    unsubscribe_from_changes/1,
    get_subscribers/0,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State record
-record(state, {
    subscribers = sets:new() :: sets:set(pid()),
    monitors = #{} :: #{pid() => reference()}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec notify_tool_added(binary(), #mcp_tool{}) -> ok.
notify_tool_added(ToolName, Tool) when is_binary(ToolName), is_record(Tool, mcp_tool) ->
    gen_server:cast(?MODULE, {notify_tool_added, ToolName, Tool}).

-spec notify_tool_removed(binary(), #mcp_tool{}) -> ok.
notify_tool_removed(ToolName, Tool) when is_binary(ToolName), is_record(Tool, mcp_tool) ->
    gen_server:cast(?MODULE, {notify_tool_removed, ToolName, Tool}).

-spec notify_tool_updated(binary(), #mcp_tool{}) -> ok.
notify_tool_updated(ToolName, Tool) when is_binary(ToolName), is_record(Tool, mcp_tool) ->
    gen_server:cast(?MODULE, {notify_tool_updated, ToolName, Tool}).

-spec subscribe_to_changes(pid()) -> ok | {error, term()}.
subscribe_to_changes(SubscriberPid) when is_pid(SubscriberPid) ->
    gen_server:call(?MODULE, {subscribe, SubscriberPid}).

-spec unsubscribe_from_changes(pid()) -> ok.
unsubscribe_from_changes(SubscriberPid) when is_pid(SubscriberPid) ->
    gen_server:call(?MODULE, {unsubscribe, SubscriberPid}).

-spec get_subscribers() -> [pid()].
get_subscribers() ->
    gen_server:call(?MODULE, get_subscribers).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting MCP tool change notifier"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.

handle_call({subscribe, SubscriberPid}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"tool_change_notifier.subscribe">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"subscriber_pid">> => erlmcp_tracing:pid_to_string(SubscriberPid)
        }),

        Subscribers = State#state.subscribers,
        case sets:is_element(SubscriberPid, Subscribers) of
            true ->
                erlmcp_tracing:set_status(SpanCtx, ok),
                {reply, ok, State};
            false ->
                NewSubscribers = sets:add_element(SubscriberPid, Subscribers),
                Ref = monitor(process, SubscriberPid),
                NewMonitors = maps:put(SubscriberPid, Ref, State#state.monitors),

                erlmcp_tracing:set_status(SpanCtx, ok),
                NewState = State#state{subscribers = NewSubscribers, monitors = NewMonitors},
                {reply, ok, NewState}
        end
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            {reply, {error, Reason}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call({unsubscribe, SubscriberPid}, _From, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"tool_change_notifier.unsubscribe">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"subscriber_pid">> => erlmcp_tracing:pid_to_string(SubscriberPid)
        }),

        Subscribers = sets:del_element(SubscriberPid, State#state.subscribers),
        NewMonitors = case maps:get(SubscriberPid, State#state.monitors, undefined) of
            undefined -> State#state.monitors;
            Ref ->
                demonitor(Ref, [flush]),
                maps:remove(SubscriberPid, State#state.monitors)
        end,

        erlmcp_tracing:set_status(SpanCtx, ok),
        NewState = State#state{subscribers = Subscribers, monitors = NewMonitors},
        {reply, ok, NewState}
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            {reply, {error, Reason}, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_call(get_subscribers, _From, State) ->
    Subscribers = sets:to_list(State#state.subscribers),
    {reply, Subscribers, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({notify_tool_added, ToolName, Tool}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"tool_change_notifier.notify_tool_added">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"tool_name">> => ToolName,
            <<"operation">> => <<"added">>
        }),

        Notification = build_tool_change_notification(ToolName, Tool, added),
        broadcast_notification(Notification, State#state.subscribers),

        erlmcp_tracing:set_status(SpanCtx, ok),
        {noreply, State}
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_cast({notify_tool_removed, ToolName, Tool}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"tool_change_notifier.notify_tool_removed">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"tool_name">> => ToolName,
            <<"operation">> => <<"removed">>
        }),

        Notification = build_tool_change_notification(ToolName, Tool, removed),
        broadcast_notification(Notification, State#state.subscribers),

        erlmcp_tracing:set_status(SpanCtx, ok),
        {noreply, State}
    catch
        Class:Reason:Stack ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stack),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

handle_cast({notify_tool_updated, ToolName, Tool}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"tool_change_notifier.notify_tool_updated">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"tool_name">> => ToolName,
            <<"operation">> => <<"updated">>
        }),

        Notification = build_tool_change_notification(ToolName, Tool, updated),
        broadcast_notification(Notification, State#state.subscribers),

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

handle_info({'DOWN', _Ref, process, SubscriberPid, _Reason}, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"tool_change_notifier.cleanup_subscriber">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"subscriber_pid">> => erlmcp_tracing:pid_to_string(SubscriberPid)
        }),

        NewSubscribers = sets:del_element(SubscriberPid, State#state.subscribers),
        NewMonitors = maps:remove(SubscriberPid, State#state.monitors),

        logger:info("Cleaned up tool change notifier subscription for dead subscriber ~p", [SubscriberPid]),
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
    logger:info("MCP tool change notifier terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal helper functions
%%====================================================================

-spec build_tool_change_notification(binary(), #mcp_tool{}, atom()) -> map().
build_tool_change_notification(ToolName, Tool, Operation) ->
    OperationBin = atom_to_binary(Operation, utf8),

    % Build tool metadata
    ToolMetadata = #{
        <<"name">> => ToolName,
        <<"description">> => Tool#mcp_tool.description
    },

    % Add input schema if present
    ToolMetadataWithSchema = case Tool#mcp_tool.input_schema of
        undefined -> ToolMetadata;
        Schema -> ToolMetadata#{<<"inputSchema">> => Schema}
    end,

    % Build notification parameters
    Params = #{
        <<"operation">> => OperationBin,
        <<"tool">> => ToolMetadataWithSchema
    },

    % Build JSON-RPC 2.0 notification
    erlmcp_json_rpc:encode_notification(?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED, Params).

-spec broadcast_notification(binary(), sets:set(pid())) -> ok.
broadcast_notification(Notification, Subscribers) ->
    sets:fold(fun(SubscriberPid, _Acc) ->
        send_notification_to_subscriber(SubscriberPid, Notification)
    end, ok, Subscribers).

-spec send_notification_to_subscriber(pid(), binary()) -> ok.
send_notification_to_subscriber(SubscriberPid, Notification) ->
    try
        SubscriberPid ! {tool_list_changed_notification, Notification},
        ok
    catch
        _Class:_Reason ->
            logger:warning("Failed to send tool change notification to ~p", [SubscriberPid]),
            ok
    end.
