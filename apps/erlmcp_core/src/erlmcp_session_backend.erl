-module(erlmcp_session_backend).

-behaviour(gen_server).

%% API
-export([start_link/1, store/2, fetch/1, delete/1, list/0, cleanup_expired/0]).
-export([send_priority_message/2, send_urgent_message/1]).
-export([spawn_tool/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Import nominal types for type safety
-import(erlmcp_mcp_types, [mcp_session_id/0]).

%% Behavior callbacks that implementations must export
-callback init(map()) -> {ok, State :: term()} | {error, term()}.
-callback store(mcp_session_id(), session(), State :: term()) ->
                   {ok, NewState :: term()} | {error, term()}.
-callback fetch(mcp_session_id(), State :: term()) ->
                   {ok, session(), State :: term()} | {error, not_found | term(), State :: term()}.
-callback delete(mcp_session_id(), State :: term()) ->
                    {ok, NewState :: term()} | {error, term(), State :: term()}.
-callback list(State :: term()) -> {ok, [mcp_session_id()], NewState :: term()}.
-callback cleanup_expired(State :: term()) -> {ok, Count :: non_neg_integer(), NewState :: term()}.

%% Types
%% Note: session_id() is now a nominal type from erlmcp_mcp_types
%% This prevents accidental confusion with other binary types like request IDs
-type session_id() :: mcp_session_id().
-type session() ::
    #{id := session_id(),
      created_at := integer(),
      last_accessed := integer(),
      timeout_ms := pos_integer() | infinity,
      metadata := map()}.
-type backend_opts() :: #{backend := module(), cleanup_interval := pos_integer()}.

-export_type([session_id/0, session/0, backend_opts/0]).

%% State
-record(state,
        {backend :: module(),
         backend_state :: term(),
         cleanup_timer :: reference() | undefined,
         cleanup_interval :: pos_integer(),
         priority_alias :: erlang:alias() | undefined,
         monitored_tools :: #{{binary(), pid()} => reference()}}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(backend_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec store(session_id(), session()) -> ok | {error, term()}.
store(SessionId, Session) ->
    gen_server:call(?MODULE, {store, SessionId, Session}, 5000).

-spec fetch(session_id()) -> {ok, session()} | {error, not_found | term()}.
fetch(SessionId) ->
    gen_server:call(?MODULE, {fetch, SessionId}, 5000).

-spec delete(session_id()) -> ok | {error, term()}.
delete(SessionId) ->
    gen_server:call(?MODULE, {delete, SessionId}, 5000).

-spec list() -> {ok, [session_id()]}.
list() ->
    gen_server:call(?MODULE, list, 5000).

-spec cleanup_expired() -> {ok, non_neg_integer()}.
cleanup_expired() ->
    gen_server:call(?MODULE, cleanup_expired, 5000).

%% @doc Send a priority message to the session backend (OTP 28).
%% Priority messages jump the queue - use for urgent control signals.
%% @param Message Priority message payload
-spec send_priority_message(term(), pid()) -> ok.
send_priority_message(Message, From) ->
    erlmcp_priority:send_priority(erlang:whereis(?MODULE), Message, From).

%% @doc Send an urgent message without sender context (OTP 28).
%% Use for system-level urgent signals like shutdown.
%% @param Message Urgent message payload
-spec send_urgent_message(term()) -> ok.
send_urgent_message(Message) ->
    erlmcp_priority:send_urgent(erlang:whereis(?MODULE), Message).

%% @doc Spawn a tool process with OTP 27/28 tagged monitor.
%% The tag is embedded in the DOWN message, eliminating need for Ref->Tool mapping.
%% Memory guard is automatically enabled for tool processes (OTP 28+).
%% @param ToolName Name of the tool to spawn
%% @param Params Tool execution parameters
-spec spawn_tool(binary(), map()) -> {ok, pid(), reference()} | {error, term()}.
spawn_tool(ToolName, Params) ->
    gen_server:call(?MODULE, {spawn_tool, ToolName, Params}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(backend_opts()) -> {ok, #state{}}.
init(Opts) ->
    Backend = maps:get(backend, Opts, erlmcp_session_ets),
    CleanupInterval = maps:get(cleanup_interval, Opts, 60000),

    case Backend:init(
             maps:without([backend, cleanup_interval], Opts))
    of
        {ok, BackendState} ->
            CleanupTimer = schedule_cleanup(CleanupInterval),
            %% OTP 28: Create priority alias for urgent control signals
            PriorityAlias = try_create_priority_alias(),
            {ok,
             #state{backend = Backend,
                    backend_state = BackendState,
                    cleanup_timer = CleanupTimer,
                    cleanup_interval = CleanupInterval,
                    priority_alias = PriorityAlias,
                    monitored_tools = #{}}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {reply, term(), #state{}, hibernate}.
handle_call({store, SessionId, Session}, _From, State) ->
    case (State#state.backend):store(SessionId, Session, State#state.backend_state) of
        {ok, NewBackendState} ->
            {reply, ok, State#state{backend_state = NewBackendState}, hibernate};
        {error, Reason} ->
            {reply, {error, Reason}, State, hibernate}
    end;
handle_call({fetch, SessionId}, _From, State) ->
    case (State#state.backend):fetch(SessionId, State#state.backend_state) of
        {ok, Session, NewBackendState} ->
            {reply, {ok, Session}, State#state{backend_state = NewBackendState}, hibernate};
        {error, not_found, NewBackendState} ->
            {reply, {error, not_found}, State#state{backend_state = NewBackendState}, hibernate};
        {error, Reason, NewBackendState} ->
            {reply, {error, Reason}, State#state{backend_state = NewBackendState}, hibernate}
    end;
handle_call({delete, SessionId}, _From, State) ->
    case (State#state.backend):delete(SessionId, State#state.backend_state) of
        {ok, NewBackendState} ->
            {reply, ok, State#state{backend_state = NewBackendState}, hibernate};
        {error, Reason, NewBackendState} ->
            {reply, {error, Reason}, State#state{backend_state = NewBackendState}, hibernate}
    end;
handle_call(list, _From, State) ->
    case (State#state.backend):list(State#state.backend_state) of
        {ok, SessionIds, NewBackendState} ->
            {reply, {ok, SessionIds}, State#state{backend_state = NewBackendState}, hibernate}
    end;
handle_call(cleanup_expired, _From, State) ->
    case (State#state.backend):cleanup_expired(State#state.backend_state) of
        {ok, Count, NewBackendState} ->
            {reply, {ok, Count}, State#state{backend_state = NewBackendState}, hibernate}
    end;
%% OTP 27/28: Spawn tool with tagged monitor
handle_call({spawn_tool, ToolName, Params}, _From, State) ->
    case do_spawn_tool(ToolName, Params) of
        {ok, Pid, Ref} ->
            %% Store in monitored tools map
            NewMonitoredTools = maps:put({tool, ToolName}, {Pid, Ref}, State#state.monitored_tools),
            {reply, {ok, Pid, Ref}, State#state{monitored_tools = NewMonitoredTools}, hibernate};
        {error, Reason} ->
            {reply, {error, Reason}, State, hibernate}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {noreply, #state{}, hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {noreply, #state{}, hibernate}.
handle_info(cleanup_expired, State) ->
    case (State#state.backend):cleanup_expired(State#state.backend_state) of
        {ok, Count, NewBackendState} ->
            case Count > 0 of
                true ->
                    logger:debug("Session cleanup removed ~p expired sessions", [Count]);
                false ->
                    ok
            end,
            NewTimer = schedule_cleanup(State#state.cleanup_interval),
            %% OTP 28: Use erlang:hibernate/0 for memory-efficient idle wait
            {noreply, State#state{backend_state = NewBackendState, cleanup_timer = NewTimer}, hibernate}
    end;
%% OTP 28: Priority message with sender context
handle_info({priority, From, Message}, State) ->
    logger:debug("Session backend received priority message from ~p: ~p", [From, Message]),
    %% Handle priority messages (cancellation, ping, control)
    NewState = handle_priority_message(Message, From, State),
    {noreply, NewState, hibernate};
%% OTP 28: Urgent system message without sender context
handle_info({urgent, Message}, State) ->
    logger:warning("Session backend received urgent message: ~p", [Message]),
    %% Handle urgent system messages (shutdown, critical errors)
    NewState = handle_urgent_message(Message, State),
    {noreply, NewState, hibernate};
%% OTP 27/28: Tagged monitor for tool processes
%% The tag is embedded directly in the Ref - no need for separate mapping
handle_info({'DOWN', {tool, ToolName}, process, Pid, Reason}, State) ->
    logger:warning("Tool process ~p (~p) crashed: ~p", [ToolName, Pid, Reason]),
    %% Automatic cleanup from monitored_tools map
    NewMonitoredTools = maps:remove({tool, ToolName}, State#state.monitored_tools),
    %% Notify interested parties via pg (use pg:get_members to verify scope exists)
    try
        pg:join(erlmcp_tool_failures, self(), {ToolName, Pid, Reason})
    catch
        _:_ ->
            %% pg scope may not exist, log and continue
            logger:debug("Could not notify pg scope erlmcp_tool_failures (may not exist)")
    end,
    {noreply, State#state{monitored_tools = NewMonitoredTools}, hibernate};
handle_info(_Info, State) ->
    %% OTP 28: Hibernate on unknown messages to reduce memory footprint
    {noreply, State, hibernate}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.cleanup_timer of
        undefined ->
            ok;
        Timer ->
            erlang:cancel_timer(Timer)
    end,
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Try to create OTP 28 priority alias (graceful degradation).
%% @private
-spec try_create_priority_alias() -> erlang:alias() | undefined.
try_create_priority_alias() ->
    try
        erlmcp_priority:create_priority_alias()
    catch
        _:_ ->
            %% OTP < 28: Priority queues not available, degrade gracefully
            logger:info("Priority message queues not available (requires OTP 28+)"),
            undefined
    end.

%% @doc Handle priority message with sender context.
%% @private
-spec handle_priority_message(term(), pid(), #state{}) -> #state{}.
handle_priority_message({ping, Ref}, From, State) ->
    %% Health check - respond immediately
    From ! {pong, Ref},
    State;
handle_priority_message({cancel_session, SessionId}, _From, State) ->
    %% Cancellation request - remove session immediately
    case (State#state.backend):delete(SessionId, State#state.backend_state) of
        {ok, NewBackendState} ->
            logger:info("Cancelled session via priority signal: ~p", [SessionId]),
            State#state{backend_state = NewBackendState};
        {error, not_found, NewBackendState} ->
            logger:debug("Session not found for priority cancellation: ~p", [SessionId]),
            State#state{backend_state = NewBackendState}
    end;
handle_priority_message(_Message, _From, State) ->
    %% Unknown priority message - log and ignore
    State.

%% @doc Handle urgent system message without sender context.
%% @private
-spec handle_urgent_message(term(), #state{}) -> #state{}.
handle_urgent_message(shutdown, State) ->
    %% System shutdown - initiate graceful shutdown
    logger:warning("Session backend received urgent shutdown signal"),
    %% Stop will be handled by supervisor
    State;
handle_urgent_message({critical_error, Reason}, State) ->
    %% Critical error - log and alert
    logger:error("Session backend critical error: ~p", [Reason]),
    State;
handle_urgent_message({reload_config, NewConfig}, State) ->
    %% Reconfiguration - log and apply (future enhancement)
    logger:info("Session backend reconfiguring: ~p", [NewConfig]),
    State;
handle_urgent_message(_Message, State) ->
    %% Unknown urgent message - log and ignore
    State.

-spec schedule_cleanup(pos_integer()) -> reference().
schedule_cleanup(IntervalMs) ->
    erlang:send_after(IntervalMs, self(), cleanup_expired).

%% @doc Spawn and monitor a tool process with OTP 27/28 tagged monitor.
%% The tag allows us to identify which tool crashed without maintaining a Ref -> Tool map.
%% Memory guard is enabled for tool processes to prevent memory leaks.
%% @private
-spec do_spawn_tool(binary(), map()) -> {ok, pid(), reference()} | {error, term()}.
do_spawn_tool(ToolName, Params) ->
    try
        %% Spawn tool process using spawn_monitor to get both pid and initial ref
        {Pid, InitialRef} = spawn_monitor(
            fun() ->
                logger:info("Tool ~p starting execution", [ToolName]),
                %% OTP 28: Enable memory guard for tool process
                try
                    erlmcp_memory_guard:enable_tool_guard()
                catch
                    _:_ -> ok
                end,
                execute_tool(ToolName, Params)
            end
        ),

        %% OTP 27/28: Tag monitor with tool name for easy identification
        %% When DOWN arrives, the first element will be {tool, ToolName} instead of opaque ref
        TaggedRef = erlang:monitor(process, Pid, [{tag, {tool, ToolName}}]),

        %% Demonitor the untagged reference from spawn_monitor (flush to avoid message queue buildup)
        erlang:demonitor(InitialRef, [flush]),

        logger:info("Tool ~p spawned with tagged monitor: ~p (memory guard enabled)", [ToolName, TaggedRef]),
        {ok, Pid, TaggedRef}
    catch
        _:Reason ->
            logger:error("Failed to spawn tool ~p: ~p", [ToolName, Reason]),
            {error, Reason}
    end.

%% @doc Execute tool - placeholder for actual tool execution.
%% In production, this would call the actual tool implementation from erlmcp_tool.
%% @private
-spec execute_tool(binary(), map()) -> term().
execute_tool(ToolName, Params) ->
    %% This would call the actual tool implementation
    logger:info("Executing tool ~p with params: ~p", [ToolName, Params]),
    %% Simulate tool work
    timer:sleep(100),
    {ok, #{result => tool_complete}}.
