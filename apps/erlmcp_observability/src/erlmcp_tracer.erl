%%%-------------------------------------------------------------------
%%% @doc
%%% OTP 28 trace:system/3 based distributed tracer for erlmcp
%%%
%%% This module provides OTP 28's modern tracing interface for MCP
%%% tool invocation chains, session lifecycle, and inter-process
%%% message tracing.
%%%
%%% == OTP 28 Innovation ==
%%% Uses trace:system/3 instead of global dbg trace:
%%% - Modular trace sessions (isolated from other tools)
%%% - Integrates with system monitors (long_gc, long_schedule, etc.)
%%% - Better performance and cleaner cleanup
%%%
%%% == Architecture ==
%%% - Sessions: Isolated trace contexts with own tracer process
%%% - Trace Points: Tool calls, session events, messages
%%% - Distributed: Correlate traces across nodes by request ID
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tracer).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([start_trace_session/1, start_trace_session/2]).
-export([trace_session/1, trace_session/2]).
-export([trace_tool_calls/0, trace_tool_calls/1]).
-export([trace_messages/0, trace_messages/1]).
-export([collect_trace/0, collect_trace/1]).
-export([stop_trace_session/0, stop_trace_session/1]).
-export([get_active_sessions/0]).
-export([enable_system_monitor/0, enable_system_monitor/1]).
-export([disable_system_monitor/0, disable_system_monitor/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type trace_session_id() :: binary().
-type trace_session() :: #{id := trace_session_id(),
                            session := trace:session(),
                            tracer_pid := pid(),
                            trace_opts := map(),
                            start_time := integer()}.
-type trace_event() :: #{timestamp := integer(),
                         event_type := atom(),
                         data := term()}.
-type system_event() :: long_gc | long_schedule | long_message_queue |
                        large_heap | busy_port | busy_dist_port.

-record(state,
        {active_sessions = #{} :: #{trace_session_id() => trace_session()},
         default_tracer :: pid() | undefined,
         trace_dir :: file:filename()}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the tracer with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the tracer with custom options
%% Options:
%%   - trace_dir: Directory for trace file storage
%%   - auto_system_monitor: Enable system monitoring on all sessions
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Start a new trace session for MCP tool invocation chains
%% Uses OTP 28 trace:session_create/3 for isolated tracing
-spec start_trace_session([node()]) -> {ok, trace_session_id(), pid()} | {error, term()}.
start_trace_session(Nodes) ->
    start_trace_session(Nodes, #{}).

%% @doc Start a trace session with custom options
%% Options:
%%   - session_name: Atom name for the session (default: mcp_trace)
%%   - trace_messages: Enable message tracing (default: true)
%%   - trace_tools: Enable tool call tracing (default: true)
%%   - trace_gc: Enable garbage collection tracing (default: false)
%%   - system_monitor: Enable system event monitoring (default: true)
-spec start_trace_session([node()], map()) -> {ok, trace_session_id(), pid()} | {error, term()}.
start_trace_session(Nodes, Options) ->
    gen_server:call(?MODULE, {start_trace_session, Nodes, Options}).

%% @doc Trace all events for a specific MCP session
%% Correlates trace events by session ID
-spec trace_session(binary()) -> ok | {error, term()}.
trace_session(SessionId) ->
    trace_session(SessionId, []).

%% @doc Trace session with match spec options
-spec trace_session(binary(), list()) -> ok | {error, term()}.
trace_session(SessionId, MatchSpec) ->
    gen_server:call(?MODULE, {trace_session, SessionId, MatchSpec}).

%% @doc Enable tracing for all tool calls across the system
%% Traces erlmcp_server:handle_call for tool invocation patterns
-spec trace_tool_calls() -> {ok, trace_session_id()} | {error, term()}.
trace_tool_calls() ->
    trace_tool_calls([]).

%% @doc Enable tool call tracing with match specifications
-spec trace_tool_calls(list()) -> {ok, trace_session_id()} | {error, term()}.
trace_tool_calls(Options) ->
    gen_server:call(?MODULE, {trace_tool_calls, Options}).

%% @doc Enable tracing for all message passing
%% Traces send and receive events with MCP message patterns
-spec trace_messages() -> {ok, trace_session_id()} | {error, term()}.
trace_messages() ->
    trace_messages([]).

%% @doc Enable message tracing with custom options
-spec trace_messages(list()) -> {ok, trace_session_id()} | {error, term()}.
trace_messages(Options) ->
    gen_server:call(?MODULE, {trace_messages, Options}).

%% @doc Collect trace results from all active sessions
%% Aggregates trace events from all nodes
-spec collect_trace() -> {ok, [trace_event()]} | {error, term()}.
collect_trace() ->
    gen_server:call(?MODULE, collect_trace, 30000).

%% @doc Collect trace results from specific session
-spec collect_trace(trace_session_id()) -> {ok, [trace_event()]} | {error, term()}.
collect_trace(SessionId) ->
    gen_server:call(?MODULE, {collect_trace, SessionId}, 30000).

%% @doc Stop all active trace sessions
-spec stop_trace_session() -> ok.
stop_trace_session() ->
    gen_server:call(?MODULE, stop_trace_session).

%% @doc Stop a specific trace session
-spec stop_trace_session(trace_session_id()) -> ok | {error, term()}.
stop_trace_session(SessionId) ->
    gen_server:call(?MODULE, {stop_trace_session, SessionId}).

%% @doc Get list of all active trace sessions
-spec get_active_sessions() -> {ok, [trace_session_id()]}.
get_active_sessions() ->
    gen_server:call(?MODULE, get_active_sessions).

%% @doc Enable system event monitoring on default session
%% Monitors: long_gc, long_schedule, busy_port, etc.
-spec enable_system_monitor() -> ok | {error, term()}.
enable_system_monitor() ->
    enable_system_monitor([]).

%% @doc Enable system monitoring with custom options
-spec enable_system_monitor(list()) -> ok | {error, term()}.
enable_system_monitor(Options) ->
    gen_server:call(?MODULE, {enable_system_monitor, Options}).

%% @doc Disable system event monitoring
-spec disable_system_monitor() -> ok.
disable_system_monitor() ->
    disable_system_monitor(default).

%% @doc Disable system monitoring for specific session
-spec disable_system_monitor(trace_session_id() | default) -> ok | {error, term()}.
disable_system_monitor(SessionId) when is_binary(SessionId) ->
    gen_server:call(?MODULE, {disable_system_monitor, SessionId});
disable_system_monitor(default) ->
    gen_server:call(?MODULE, {disable_system_monitor, default}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
-spec init(map()) -> {ok, state()}.
init(Options) ->
    TraceDir = maps:get(trace_dir, Options, "log/traces"),
    ok = filelib:ensure_path(TraceDir),
    {ok,
     #state{trace_dir = TraceDir,
            active_sessions = #{},
            default_tracer = undefined}}.

%% @private
-spec handle_call(term(), {pid(), term()}, state()) ->
                         {reply, term(), state()} |
                         {reply, term(), state(), hibernate}.
handle_call({start_trace_session, Nodes, Opts}, _From, State) ->
    case do_start_trace_session(Nodes, Opts, State) of
        {ok, SessionId, TracerPid, NewState} ->
            {reply, {ok, SessionId, TracerPid}, NewState, hibernate};
        {error, Reason} = Error ->
            {reply, Error, State, hibernate}
    end;

handle_call({trace_session, SessionId, MatchSpec}, _From, State) ->
    Reply = do_trace_session(SessionId, MatchSpec, State),
    {reply, Reply, State, hibernate};

handle_call({trace_tool_calls, Options}, _From, State) ->
    case do_trace_tool_calls(Options, State) of
        {ok, SessionId, NewState} ->
            {reply, {ok, SessionId}, NewState, hibernate};
        {error, Reason} = Error ->
            {reply, Error, State, hibernate}
    end;

handle_call({trace_messages, Options}, _From, State) ->
    case do_trace_messages(Options, State) of
        {ok, SessionId, NewState} ->
            {reply, {ok, SessionId}, NewState, hibernate};
        {error, Reason} = Error ->
            {reply, Error, State, hibernate}
    end;

handle_call(collect_trace, _From, State) ->
    {Reply, NewState} = do_collect_trace(State),
    {reply, Reply, NewState, hibernate};

handle_call({collect_trace, SessionId}, _From, State) ->
    Reply = do_collect_trace(SessionId, State),
    {reply, Reply, State, hibernate};

handle_call(stop_trace_session, _From, State) ->
    {Reply, NewState} = do_stop_all_sessions(State),
    {reply, Reply, NewState, hibernate};

handle_call({stop_trace_session, SessionId}, _From, State) ->
    {Reply, NewState} = do_stop_session(SessionId, State),
    {reply, Reply, NewState, hibernate};

handle_call(get_active_sessions, _From, State) ->
    Sessions = maps:keys(State#state.active_sessions),
    {reply, {ok, Sessions}, State, hibernate};

handle_call({enable_system_monitor, Options}, _From, State) ->
    Reply = do_enable_system_monitor(Options, State),
    {reply, Reply, State, hibernate};

handle_call({disable_system_monitor, SessionId}, _From, State) ->
    {Reply, NewState} = do_disable_system_monitor(SessionId, State),
    {reply, Reply, NewState, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({trace, _Pid, call, {M, F, A}} = TraceMsg, State) ->
    %% Capture trace events from tool calls
    ?LOG(debug, "Trace event: ~p", [TraceMsg]),
    {noreply, State};

handle_info({trace, _Pid, send, _Msg, _To} = TraceMsg, State) ->
    %% Capture message send events
    ?LOG(debug, "Trace send: ~p", [TraceMsg]),
    {noreply, State};

handle_info({trace, _Pid, 'receive', _Msg} = TraceMsg, State) ->
    %% Capture message receive events
    ?LOG(debug, "Trace receive: ~p", [TraceMsg]),
    {noreply, State};

handle_info({monitor, _Pid, SystemEvent, _Info} = MonitorMsg, State) ->
    %% Capture system monitor events (long_gc, long_schedule, etc.)
    ?LOG(debug, "System monitor: ~p", [MonitorMsg]),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    %% Handle tracer process death
    NewState = cleanup_tracer(Pid, Reason, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Cleanup all trace sessions
    maps:foreach(fun(SessionId, _Session) ->
                    trace:session_destroy(SessionId)
                end,
                State#state.active_sessions),
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% Start a new trace session with OTP 28 trace:session_create/3
-spec do_start_trace_session([node()], map(), state()) ->
                                    {ok, trace_session_id(), pid(), state()} |
                                    {error, term()}.
do_start_trace_session(Nodes, Options, State) ->
    SessionName = maps:get(session_name, Options, mcp_trace),
    TracerOpts = maps:get(tracer_opts, Options, []),

    %% Create tracer process
    TracerPid = spawn_tracer_process(TracerOpts),

    %% OTP 28: Create trace session
    case trace:session_create(SessionName, TracerPid, []) of
        Session = {StrongRef, _WeakRef} ->
            SessionId = generate_session_id(),
            TraceOpts = #{
                nodes => Nodes,
                trace_messages => maps:get(trace_messages, Options, true),
                trace_tools => maps:get(trace_tools, Options, true),
                trace_gc => maps:get(trace_gc, Options, false),
                system_monitor => maps:get(system_monitor, Options, true)
            },

            SessionMap = #{
                id => SessionId,
                session => Session,
                tracer_pid => TracerPid,
                trace_opts => TraceOpts,
                start_time => erlang:system_time(millisecond)
            },

            NewState =
                State#state{active_sessions =>
                                maps:put(SessionId, SessionMap, State#state.active_sessions),
                            default_tracer => TracerPid},

            %% Enable system monitoring if requested
            SystemMonitor = maps:get(system_monitor, TraceOpts, true),
            case SystemMonitor of
                true ->
                    enable_system_events(Session, Options);
                false ->
                    ok
            end,

            {ok, SessionId, TracerPid, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% Spawn a tracer process that collects trace events
-spec spawn_tracer_process(list()) -> pid().
spawn_tracer_process(_Options) ->
    spawn(fun() -> tracer_loop(#{events => [], start_time => erlang:timestamp()}) end).

%% @private
%% Tracer loop - receives and buffers trace events
tracer_loop(State = #{events := Events}) ->
    receive
        {trace, _Pid, _Tag, _Data} = TraceMsg ->
            ?LOG(debug, "Tracer received: ~p", [TraceMsg]),
            NewEvents = [normalize_trace_event(TraceMsg) | Events],
            tracer_loop(State#{events => NewEvents});

        {get_events, From} ->
            From ! {events, lists:reverse(Events)},
            tracer_loop(State#{events => []});

        {stop, From} ->
            From ! {stopped, lists:reverse(Events)}

    after 60000 ->
              %% Timeout - keep loop alive
              tracer_loop(State)
    end.

%% @private
%% Normalize trace events to consistent format
-spec normalize_trace_event(term()) -> map().
normalize_trace_event({trace, Pid, call, {M, F, A}}) ->
    #{timestamp => erlang:system_time(microsecond),
      event_type => call,
      pid => Pid,
      module => M,
      function => F,
      arity => length(A),
      arguments => A};

normalize_trace_event({trace, Pid, send, Msg, To}) ->
    #{timestamp => erlang:system_time(microsecond),
      event_type => send,
      pid => Pid,
      message => Msg,
      to => To};

normalize_trace_event({trace, Pid, 'receive', Msg}) ->
    #{timestamp => erlang:system_time(microsecond),
      event_type => receive,
      pid => Pid,
      message => Msg};

normalize_trace_event({monitor, Pid, SystemEvent, Info}) ->
    #{timestamp => erlang:system_time(microsecond),
      event_type => system_monitor,
      pid => Pid,
      system_event => SystemEvent,
      info => Info};

normalize_trace_event(TraceMsg) ->
    #{timestamp => erlang:system_time(microsecond),
      event_type => unknown,
      raw => TraceMsg}.

%% @private
%% Trace events for specific MCP session
-spec do_trace_session(binary(), list(), state()) -> ok | {error, term()}.
do_trace_session(_SessionId, _MatchSpec, _State) ->
    %% TODO: Implement session-specific trace patterns
    %% This requires correlating by session ID in trace patterns
    ok.

%% @private
%% Enable tracing for tool calls across erlmcp_server
-spec do_trace_tool_calls(list(), state()) ->
                                 {ok, trace_session_id(), state()} |
                                 {error, term()}.
do_trace_tool_calls(Options, State) ->
    case maps:get(default_tracer, State, undefined) of
        undefined ->
            {error, no_default_tracer};
        _TracerPid ->
            %% Find active session or create new one
            case maps:values(State#state.active_sessions) of
                [Session | _] ->
                    TraceSession = Session,
                    NewState = State;
                [] ->
                    {ok, SessionId, _Tracer, NewState} =
                        do_start_trace_session([node()], Options, State),
                    TraceSession = maps:get(SessionId, NewState#state.active_sessions)
            end,

            %% OTP 28: Trace tool call patterns in erlmcp_server
            TraceSess = maps:get(session, TraceSession),

            %% Trace handle_call for tool invocations
            trace:function(TraceSess,
                          {erlmcp_server, handle_call, 3},
                          [{'_', [], [{return_trace}]}],
                          [local]),

            %% Trace call_tool execution
            trace:function(TraceSess,
                          {erlmcp_server, execute_tool_call, 3},
                          [{'_', [], [{return_trace}, {exception_trace}]}],
                          [local]),

            SessionId = maps:get(id, TraceSession),
            {ok, SessionId, NewState}
    end.

%% @private
%% Enable message tracing
-spec do_trace_messages(list(), state()) ->
                                {ok, trace_session_id(), state()} |
                                {error, term()}.
do_trace_messages(Options, State) ->
    case maps:get(default_tracer, State, undefined) of
        undefined ->
            {error, no_default_tracer};
        _TracerPid ->
            %% Find active session or create new one
            case maps:values(State#state.active_sessions) of
                [Session | _] ->
                    TraceSession = Session,
                    NewState = State;
                [] ->
                    {ok, SessionId, _Tracer, NewState} =
                        do_start_trace_session([node()], Options, State),
                    TraceSession = maps:get(SessionId, NewState#state.active_sessions)
            end,

            TraceSess = maps:get(session, TraceSession),

            %% Trace all message sends
            trace:send(TraceSess, true, []),

            %% Trace all message receives
            trace:recv(TraceSess, true, []),

            SessionId = maps:get(id, TraceSession),
            {ok, SessionId, NewState}
    end.

%% @private
%% Collect trace results from all sessions
-spec do_collect_trace(state()) -> {{ok, [trace_event()]}, state()}.
do_collect_trace(State) ->
    AllEvents =
        maps:fold(fun(_SessionId, Session, Acc) ->
                     TracerPid = maps:get(tracer_pid, Session),
                     Events = get_tracer_events(TracerPid),
                     Events ++ Acc
                  end,
                  [],
                  State#state.active_sessions),

    {{ok, AllEvents}, State}.

%% @private
%% Collect trace results from specific session
-spec do_collect_trace(trace_session_id(), state()) ->
                               {ok, [trace_event()]} | {error, term()}.
do_collect_trace(SessionId, State) ->
    case maps:get(SessionId, State#state.active_sessions, undefined) of
        undefined ->
            {error, session_not_found};
        Session ->
            TracerPid = maps:get(tracer_pid, Session),
            Events = get_tracer_events(TracerPid),
            {ok, Events}
    end.

%% @private
%% Get events from tracer process
-spec get_tracer_events(pid()) -> [trace_event()].
get_tracer_events(TracerPid) ->
    TracerPid ! {get_events, self()},
    receive
        {events, Events} ->
            Events
    after 5000 ->
        []
    end.

%% @private
%% Stop all trace sessions
-spec do_stop_all_sessions(state()) -> {ok, state()}.
do_stop_all_sessions(State) ->
    maps:fold(fun(SessionId, Session, AccState) ->
                 TraceSess = maps:get(session, Session),
                 trace:session_destroy(TraceSess),
                 maps:remove(SessionId, AccState)
              end,
              State#state{active_sessions = #{}},
              State#state.active_sessions),
    {ok, State}.

%% @private
%% Stop specific trace session
-spec do_stop_session(trace_session_id(), state()) ->
                              {ok, state()} | {error, term()}.
do_stop_session(SessionId, State) ->
    case maps:get(SessionId, State#state.active_sessions, undefined) of
        undefined ->
            {{error, session_not_found}, State};
        Session ->
            TraceSess = maps:get(session, Session),
            trace:session_destroy(TraceSess),
            {ok, State#state{active_sessions =>
                                 maps:remove(SessionId, State#state.active_sessions)}}
    end.

%% @private
%% Enable system event monitoring using OTP 28 trace:system/3
-spec do_enable_system_monitor(list(), state()) -> ok | {error, term()}.
do_enable_system_monitor(Options, State) ->
    case maps:get(default_tracer, State, undefined) of
        undefined ->
            {error, no_default_tracer};
        TracerPid when is_pid(TracerPid) ->
            %% Get or create trace session
            case maps:values(State#state.active_sessions) of
                [Session | _] ->
                    TraceSession = maps:get(session, Session);
                [] ->
                    {ok, _SessionId, _Tracer, NewState} =
                        do_start_trace_session([node()], Options, State),
                    [FirstSession | _] = maps:values(NewState#state.active_sessions),
                    TraceSession = maps:get(session, FirstSession)
            end,

            %% Enable system monitoring via OTP 28 trace:system/3
            enable_system_events(TraceSession, Options),
            ok
    end.

%% @private
%% Enable system event monitoring on trace session
-spec enable_system_events(trace:session(), map()) -> ok.
enable_system_events(Session, Options) ->
    %% Long garbage collections (> 10ms)
    LongGC = maps:get(long_gc, Options, 10),
    trace:system(Session, long_gc, LongGC),

    %% Long schedules (> 100ms)
    LongSchedule = maps:get(long_schedule, Options, 100),
    trace:system(Session, long_schedule, LongSchedule),

    %% Long message queue
    LongMQ = maps:get(long_message_queue, Options, {100, 50}),
    trace:system(Session, long_message_queue, LongMQ),

    %% Large heap (> 1MB)
    LargeHeap = maps:get(large_heap, Options, 100000),
    trace:system(Session, large_heap, LargeHeap),

    %% Busy ports
    trace:system(Session, busy_port, true),
    trace:system(Session, busy_dist_port, true),

    ok.

%% @private
%% Disable system event monitoring
-spec do_disable_system_monitor(trace_session_id() | default, state()) ->
                                        {ok, state()}.
do_disable_system_monitor(default, State) ->
    %% Disable system monitoring on default session
    case maps:values(State#state.active_sessions) of
        [Session | _] ->
            TraceSess = maps:get(session, Session),
            disable_system_events(TraceSess);
        [] ->
            ok
    end,
    {ok, State};

do_disable_system_monitor(SessionId, State) ->
    case maps:get(SessionId, State#state.active_sessions, undefined) of
        undefined ->
            {{error, session_not_found}, State};
        Session ->
            TraceSess = maps:get(session, Session),
            disable_system_events(TraceSess),
            {ok, State}
    end.

%% @private
%% Disable system event monitoring on trace session
-spec disable_system_events(trace:session()) -> ok.
disable_system_events(Session) ->
    trace:system(Session, long_gc, false),
    trace:system(Session, long_schedule, false),
    trace:system(Session, long_message_queue, false),
    trace:system(Session, large_heap, false),
    trace:system(Session, busy_port, false),
    trace:system(Session, busy_dist_port, false),
    ok.

%% @private
%% Cleanup after tracer process dies
-spec cleanup_tracer(pid(), term(), state()) -> state().
cleanup_tracer(TracerPid, _Reason, State) ->
    %% Find and remove session with dead tracer
    NewSessions =
        maps:filter(fun(_K, V) -> maps:get(tracer_pid, V) =/= TracerPid end,
                    State#state.active_sessions),
    State#state{active_sessions => NewSessions}.

%% @private
%% Generate unique trace session ID
-spec generate_session_id() -> binary().
generate_session_id() ->
    Unique = erlang:unique_integer([positive]),
    Time = erlang:system_time(microsecond),
    iolist_to_binary(io_lib:fwrite("trace_~p_~p", [Time, Unique])).
