%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_event_logger - Logging Event Handler
%%%
%%% gen_event handler that logs MCP events to the Erlang logger.
%%% Provides structured logging with appropriate log levels.
%%%
%%% Log Levels:
%%% - debug: Request/response events
%%% - info: Tool executions, resource updates, session events
%%% - warning: Connection state changes
%%% - error: Error events
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_event_logger).
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
    log_level = info :: logger:level(),
    event_count = 0 :: non_neg_integer(),
    start_time :: integer()
}).

%%====================================================================
%% gen_event Callbacks
%%====================================================================

%% @doc Initialize the logging handler.
-spec init(map() | []) -> {ok, #state{}}.
init(Args) when is_map(Args) ->
    LogLevel = maps:get(log_level, Args, info),
    {ok, #state{
        log_level = LogLevel,
        start_time = erlang:system_time(millisecond)
    }};
init(_Args) ->
    {ok, #state{
        start_time = erlang:system_time(millisecond)
    }}.

%% @doc Handle events by logging them.
-spec handle_event(term(), #state{}) -> {ok, #state{}}.

%% Tool execution events
handle_event({tool_executed, ToolName, Duration, Result}, State) ->
    Status = case Result of
        {ok, _} -> "succeeded";
        ok -> "succeeded";
        {error, _} -> "failed";
        error -> "failed";
        _ -> "completed"
    end,
    logger:info("Tool executed: ~s (~s) in ~pms", [ToolName, Status, Duration div 1000]),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Resource update events
handle_event({resource_updated, Uri, Metadata}, State) ->
    logger:info("Resource updated: ~s (metadata: ~p)", [Uri, Metadata]),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Connection state events
handle_event({connection_state, connected, Info}, State) ->
    logger:warning("Connection established: ~p", [Info]),
    {ok, State#state{event_count = State#state.event_count + 1}};
handle_event({connection_state, disconnected, Info}, State) ->
    logger:warning("Connection lost: ~p", [Info]),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Error events
handle_event({error, Category, Reason}, State) ->
    logger:error("MCP error [~p]: ~p", [Category, Reason]),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Request received events
handle_event({request_received, Method, RequestId}, State) ->
    logger:debug("Request received: ~s (id: ~p)", [Method, RequestId]),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Response sent events
handle_event({response_sent, Method, RequestId, Duration}, State) ->
    logger:debug("Response sent: ~s (id: ~p) in ~pms", [Method, RequestId, Duration div 1000]),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Notification sent events
handle_event({notification_sent, Method, _Params}, State) ->
    logger:debug("Notification sent: ~s", [Method]),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Session created events
handle_event({session_created, SessionId, Metadata}, State) ->
    logger:info("Session created: ~s (metadata: ~p)", [SessionId, Metadata]),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Session terminated events
handle_event({session_terminated, SessionId, Reason}, State) ->
    logger:info("Session terminated: ~s (reason: ~p)", [SessionId, Reason]),
    {ok, State#state{event_count = State#state.event_count + 1}};

%% Unknown events
handle_event(Event, State) ->
    logger:warning("Unknown event: ~p", [Event]),
    {ok, State#state{event_count = State#state.event_count + 1}}.

%% @doc Handle synchronous calls.
-spec handle_call(term(), #state{}) -> {ok, term(), #state{}}.
handle_call(get_stats, State) ->
    Stats = #{
        event_count => State#state.event_count,
        uptime_ms => erlang:system_time(millisecond) - State#state.start_time,
        log_level => State#state.log_level
    },
    {ok, Stats, State};
handle_call(_Request, State) ->
    {ok, {error, unknown_request}, State}.

%% @doc Handle info messages.
-spec handle_info(term(), #state{}) -> {ok, #state{}}.
handle_info(_Info, State) ->
    {ok, State}.

%% @doc Cleanup on handler termination.
-spec terminate(term(), #state{}) -> ok.
terminate(Reason, State) ->
    logger:info("Event logger terminating (reason: ~p, logged ~p events)",
                [Reason, State#state.event_count]),
    ok.

%% @doc Handle code changes.
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================
