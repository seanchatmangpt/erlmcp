%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_event_metrics - Metrics Collection Event Handler
%%%
%%% gen_event handler that collects metrics from MCP events.
%%% Integrates with erlmcp_metrics and erlang telemetry.
%%%
%%% Metrics Collected:
%%% - Tool execution count and duration
%%% - Resource update count
%%% - Connection state changes
%%% - Error count by category
%%% - Request/response latency
%%% - Session lifecycle events
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_event_metrics).
-behaviour(gen_event).

%% gen_event callbacks
-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% State record
-record(state, {
    tool_executions = #{} :: #{binary() => non_neg_integer()},
    resource_updates = #{} :: #{binary() => non_neg_integer()},
    error_counts = #{} :: #{atom() => non_neg_integer()},
    connection_events = 0 :: non_neg_integer(),
    request_count = 0 :: non_neg_integer(),
    response_count = 0 :: non_neg_integer(),
    notification_count = 0 :: non_neg_integer(),
    session_count = 0 :: non_neg_integer(),
    start_time :: integer()
}).

%%====================================================================
%% gen_event Callbacks
%%====================================================================

%% @doc Initialize the metrics handler.
-spec init(term()) -> {ok, #state{}}.
init(_Args) ->
    {ok, #state{
        start_time = erlang:system_time(millisecond)
    }}.

%% @doc Handle events by collecting metrics.
-spec handle_event(term(), #state{}) -> {ok, #state{}}.

%% Tool execution events
handle_event({tool_executed, ToolName, Duration, _Result}, State) ->
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, event, tool_executed],
        #{duration => Duration, count => 1},
        #{tool_name => ToolName}
    ),

    %% Update tool execution count
    ToolExecs = State#state.tool_executions,
    Count = maps:get(ToolName, ToolExecs, 0),
    NewToolExecs = ToolExecs#{ToolName => Count + 1},

    {ok, State#state{tool_executions = NewToolExecs}};

%% Resource update events
handle_event({resource_updated, Uri, _Metadata}, State) ->
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, event, resource_updated],
        #{count => 1},
        #{uri => Uri}
    ),

    %% Update resource update count
    ResUpdates = State#state.resource_updates,
    Count = maps:get(Uri, ResUpdates, 0),
    NewResUpdates = ResUpdates#{Uri => Count + 1},

    {ok, State#state{resource_updates = NewResUpdates}};

%% Connection state events
handle_event({connection_state, ConnectionState, Info}, State) ->
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, event, connection_state],
        #{count => 1},
        #{state => ConnectionState, info => Info}
    ),

    {ok, State#state{connection_events = State#state.connection_events + 1}};

%% Error events
handle_event({error, Category, Reason}, State) ->
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, event, error],
        #{count => 1},
        #{category => Category, reason => Reason}
    ),

    %% Update error count by category
    ErrorCounts = State#state.error_counts,
    Count = maps:get(Category, ErrorCounts, 0),
    NewErrorCounts = ErrorCounts#{Category => Count + 1},

    {ok, State#state{error_counts = NewErrorCounts}};

%% Request received events
handle_event({request_received, Method, _RequestId}, State) ->
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, event, request_received],
        #{count => 1},
        #{method => Method}
    ),

    {ok, State#state{request_count = State#state.request_count + 1}};

%% Response sent events
handle_event({response_sent, Method, _RequestId, Duration}, State) ->
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, event, response_sent],
        #{duration => Duration, count => 1},
        #{method => Method}
    ),

    {ok, State#state{response_count = State#state.response_count + 1}};

%% Notification sent events
handle_event({notification_sent, Method, _Params}, State) ->
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, event, notification_sent],
        #{count => 1},
        #{method => Method}
    ),

    {ok, State#state{notification_count = State#state.notification_count + 1}};

%% Session created events
handle_event({session_created, SessionId, Metadata}, State) ->
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, event, session_created],
        #{count => 1},
        #{session_id => SessionId, metadata => Metadata}
    ),

    {ok, State#state{session_count = State#state.session_count + 1}};

%% Session terminated events
handle_event({session_terminated, SessionId, Reason}, State) ->
    %% Emit telemetry event
    telemetry:execute(
        [erlmcp, event, session_terminated],
        #{count => 1},
        #{session_id => SessionId, reason => Reason}
    ),

    {ok, State};

%% Unknown events
handle_event(_Event, State) ->
    {ok, State}.

%% @doc Handle synchronous calls.
-spec handle_call(term(), #state{}) -> {ok, term(), #state{}}.
handle_call(get_metrics, State) ->
    Metrics = #{
        tool_executions => State#state.tool_executions,
        resource_updates => State#state.resource_updates,
        error_counts => State#state.error_counts,
        connection_events => State#state.connection_events,
        request_count => State#state.request_count,
        response_count => State#state.response_count,
        notification_count => State#state.notification_count,
        session_count => State#state.session_count,
        uptime_ms => erlang:system_time(millisecond) - State#state.start_time
    },
    {ok, Metrics, State};
handle_call(reset_metrics, State) ->
    NewState = #state{
        start_time = erlang:system_time(millisecond)
    },
    {ok, ok, NewState};
handle_call(_Request, State) ->
    {ok, {error, unknown_request}, State}.

%% @doc Handle info messages.
-spec handle_info(term(), #state{}) -> {ok, #state{}}.
handle_info(_Info, State) ->
    {ok, State}.

%% @doc Cleanup on handler termination.
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code changes.
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================
