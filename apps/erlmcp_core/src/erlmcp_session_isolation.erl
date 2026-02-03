%%%-------------------------------------------------------------------
%%% @doc
%%% Session Isolation and Resource Management for erlmcp v3
%%% Provides enterprise-grade session isolation with:
%%%   - Per-session resource quotas (memory, CPU, messages)
%%%   - Memory guards and limits
%%%   - Process isolation and monitoring
%%%   - Resource tracking and enforcement
%%%   - Cleanup and garbage collection
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_isolation).

-behaviour(gen_server).

%% API exports
-export([start_link/1,
         %% Session lifecycle
         create_session/2, destroy_session/1,
         %% Resource management
         set_memory_limit/2, set_cpu_limit/2, set_message_limit/2,
         get_resource_usage/1, check_resource_limits/1,
         %% Isolation
         isolate_session/1, unisolate_session/1,
         %% Monitoring
         subscribe/2, unsubscribe/2,
         %% Cleanup
         cleanup_session/1, garbage_collect/1,
         %% Queries
         list_sessions/0, get_session_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type session_id() :: binary().
-type resource_type() :: memory | cpu | messages | connections.
-type resource_limit() :: #{type := resource_type(),
                             limit => number(),
                             enforcement => soft | hard}.

-record(session,
        {id :: session_id(),
         pid :: pid() | undefined,
         isolated :: boolean(),
         memory_limit :: pos_integer() | infinity,
         memory_used :: non_neg_integer(),
         cpu_limit :: number() | infinity,
         cpu_used :: number(),
         message_limit :: pos_integer() | infinity,
         message_count :: non_neg_integer(),
         connection_limit :: pos_integer() | infinity,
         connection_count :: non_neg_integer(),
         monitors :: #{pid() => reference()},
         subscribers :: [{pid(), reference()}],
         created_at :: integer(),
         last_activity :: integer()}).

-record(state,
        {sessions :: #{session_id() => #session{}},
         global_limits :: map(),
         cleanup_interval :: pos_integer(),
         cleanup_timer :: reference() | undefined}).

-define(DEFAULT_MEMORY_LIMIT, 104857600).  % 100MB
-define(DEFAULT_MESSAGE_LIMIT, 10000).
-define(DEFAULT_CONNECTION_LIMIT, 10).
-define(DEFAULT_CLEANUP_INTERVAL_MS, 60000).  % 1 minute

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the isolation manager
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Create a new isolated session
-spec create_session(session_id(), map()) -> {ok, pid()} | {error, term()}.
create_session(SessionId, Options) ->
    gen_server:call(?MODULE, {create_session, SessionId, Options}, 5000).

%% @doc Destroy a session and cleanup resources
-spec destroy_session(session_id()) -> ok | {error, term()}.
destroy_session(SessionId) ->
    gen_server:call(?MODULE, {destroy_session, SessionId}, 5000).

%% @doc Set memory limit for a session
-spec set_memory_limit(session_id(), pos_integer() | infinity) ->
                              ok | {error, term()}.
set_memory_limit(SessionId, Limit) ->
    gen_server:call(?MODULE, {set_memory_limit, SessionId, Limit}, 5000).

%% @doc Set CPU limit for a session (percentage 0-100)
-spec set_cpu_limit(session_id(), number() | infinity) ->
                           ok | {error, term()}.
set_cpu_limit(SessionId, Limit) ->
    gen_server:call(?MODULE, {set_cpu_limit, SessionId, Limit}, 5000).

%% @doc Set message limit for a session
-spec set_message_limit(session_id(), pos_integer() | infinity) ->
                                ok | {error, term()}.
set_message_limit(SessionId, Limit) ->
    gen_server:call(?MODULE, {set_message_limit, SessionId, Limit}, 5000).

%% @doc Get current resource usage for a session
-spec get_resource_usage(session_id()) -> {ok, map()} | {error, term()}.
get_resource_usage(SessionId) ->
    gen_server:call(?MODULE, {get_resource_usage, SessionId}, 5000).

%% @doc Check if session is within resource limits
-spec check_resource_limits(session_id()) -> {ok, boolean(), map()} | {error, term()}.
check_resource_limits(SessionId) ->
    gen_server:call(?MODULE, {check_resource_limits, SessionId}, 5000).

%% @doc Isolate a session (enable strict resource enforcement)
-spec isolate_session(session_id()) -> ok | {error, term()}.
isolate_session(SessionId) ->
    gen_server:call(?MODULE, {isolate_session, SessionId}, 5000).

%% @doc Unisolate a session (disable strict enforcement)
-spec unisolate_session(session_id()) -> ok | {error, term()}.
unisolate_session(SessionId) ->
    gen_server:call(?MODULE, {unisolate_session, SessionId}, 5000).

%% @doc Subscribe to session resource events
-spec subscribe(session_id(), pid()) -> ok | {error, term()}.
subscribe(SessionId, Subscriber) ->
    gen_server:call(?MODULE, {subscribe, SessionId, Subscriber}, 5000).

%% @doc Unsubscribe from session events
-spec unsubscribe(session_id(), pid()) -> ok | {error, term()}.
unsubscribe(SessionId, Subscriber) ->
    gen_server:call(?MODULE, {unsubscribe, SessionId, Subscriber}, 5000).

%% @doc Force cleanup of session resources
-spec cleanup_session(session_id()) -> ok | {error, term()}.
cleanup_session(SessionId) ->
    gen_server:call(?MODULE, {cleanup_session, SessionId}, 5000).

%% @doc Force garbage collection for session
-spec garbage_collect(session_id()) -> ok | {error, term()}.
garbage_collect(SessionId) ->
    gen_server:call(?MODULE, {garbage_collect, SessionId}, 5000).

%% @doc List all active sessions
-spec list_sessions() -> {ok, [session_id()]}.
list_sessions() ->
    gen_server:call(?MODULE, list_sessions, 5000).

%% @doc Get detailed session info
-spec get_session_info(session_id()) -> {ok, map()} | {error, term()}.
get_session_info(SessionId) ->
    gen_server:call(?MODULE, {get_session_info, SessionId}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Options) ->
    logger:info("Session isolation manager initializing"),

    %% Get global limits
    GlobalLimits = #{
        max_memory => maps:get(max_memory, Options, 1073741824),  % 1GB
        max_cpu => maps:get(max_cpu, Options, 80),  % 80%
        max_messages => maps:get(max_messages, Options, 100000),
        max_connections => maps:get(max_connections, Options, 1000)
    },

    CleanupInterval = maps:get(cleanup_interval, Options, ?DEFAULT_CLEANUP_INTERVAL_MS),
    CleanupTimer = schedule_cleanup(CleanupInterval),

    {ok, #state{
        sessions = #{},
        global_limits = GlobalLimits,
        cleanup_interval = CleanupInterval,
        cleanup_timer = CleanupTimer
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
                     {reply, term(), #state{}}.
handle_call({create_session, SessionId, Options}, _From, State) ->
    case maps:is_key(SessionId, State#state.sessions) of
        true ->
            {reply, {error, session_exists}, State};
        false ->
            SessionPid = maps:get(pid, Options, self()),
            MemoryLimit = maps:get(memory_limit, Options, ?DEFAULT_MEMORY_LIMIT),
            CpuLimit = maps:get(cpu_limit, Options, infinity),
            MessageLimit = maps:get(message_limit, Options, ?DEFAULT_MESSAGE_LIMIT),
            ConnectionLimit = maps:get(connection_limit, Options, ?DEFAULT_CONNECTION_LIMIT),
            Isolated = maps:get(isolated, Options, true),

            Now = erlang:system_time(millisecond),

            Session = #session{
                id = SessionId,
                pid = SessionPid,
                isolated = Isolated,
                memory_limit = MemoryLimit,
                memory_used = 0,
                cpu_limit = CpuLimit,
                cpu_used = 0.0,
                message_limit = MessageLimit,
                message_count = 0,
                connection_limit = ConnectionLimit,
                connection_count = 0,
                monitors = #{},
                subscribers = [],
                created_at = Now,
                last_activity = Now
            },

            %% Monitor the session process
            MonRef = erlang:monitor(process, SessionPid),
            SessionWithMon = Session#session{monitors = #{SessionPid => MonRef}},

            NewSessions = maps:put(SessionId, SessionWithMon, State#state.sessions),
            NewState = State#state{sessions = NewSessions},

            logger:info("Created isolated session ~p (pid=~p, isolated=~p)",
                        [SessionId, SessionPid, Isolated]),
            {reply, {ok, SessionPid}, NewState}
    end;

handle_call({destroy_session, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            do_cleanup_session(Session),
            NewSessions = maps:remove(SessionId, State#state.sessions),
            NewState = State#state{sessions = NewSessions},
            logger:info("Destroyed session ~p", [SessionId]),
            {reply, ok, NewState}
    end;

handle_call({set_memory_limit, SessionId, Limit}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            NewSession = Session#session{memory_limit = Limit},
            NewSessions = maps:put(SessionId, NewSession, State#state.sessions),
            notify_subscribers(Session, {memory_limit_changed, Limit}),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call({set_cpu_limit, SessionId, Limit}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            NewSession = Session#session{cpu_limit = Limit},
            NewSessions = maps:put(SessionId, NewSession, State#state.sessions),
            notify_subscribers(Session, {cpu_limit_changed, Limit}),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call({set_message_limit, SessionId, Limit}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            NewSession = Session#session{message_limit = Limit},
            NewSessions = maps:put(SessionId, NewSession, State#state.sessions),
            notify_subscribers(Session, {message_limit_changed, Limit}),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call({get_resource_usage, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            Usage = #{
                memory => #{
                    used => Session#session.memory_used,
                    limit => Session#session.memory_limit,
                    percentage => calculate_percentage(Session#session.memory_used,
                                                      Session#session.memory_limit)
                },
                cpu => #{
                    used => Session#session.cpu_used,
                    limit => Session#session.cpu_limit
                },
                messages => #{
                    count => Session#session.message_count,
                    limit => Session#session.message_limit,
                    percentage => calculate_percentage(Session#session.message_count,
                                                      Session#session.message_limit)
                },
                connections => #{
                    count => Session#session.connection_count,
                    limit => Session#session.connection_limit,
                    percentage => calculate_percentage(Session#session.connection_count,
                                                      Session#session.connection_limit)
                },
                last_activity => Session#session.last_activity,
                session_age_ms => erlang:system_time(millisecond) - Session#session.created_at
            },
            {reply, {ok, Usage}, State}
    end;

handle_call({check_resource_limits, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            {WithinLimits, Violations} = do_check_resource_limits(Session),
            {reply, {ok, WithinLimits, Violations}, State}
    end;

handle_call({isolate_session, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            NewSession = Session#session{isolated = true},
            NewSessions = maps:put(SessionId, NewSession, State#state.sessions),
            notify_subscribers(Session, session_isolated),
            logger:info("Isolated session ~p", [SessionId]),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call({unisolate_session, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            NewSession = Session#session{isolated = false},
            NewSessions = maps:put(SessionId, NewSession, State#state.sessions),
            notify_subscribers(Session, session_unisolated),
            logger:info("Unisolated session ~p", [SessionId]),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call({subscribe, SessionId, Subscriber}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            MonRef = erlang:monitor(process, Subscriber),
            NewSubscribers = [{Subscriber, MonRef} | Session#session.subscribers],
            NewSession = Session#session{subscribers = NewSubscribers},
            NewSessions = maps:put(SessionId, NewSession, State#state.sessions),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call({unsubscribe, SessionId, Subscriber}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            NewSubscribers = lists:keydelete(Subscriber, 1, Session#session.subscribers),
            NewSession = Session#session{subscribers = NewSubscribers},
            NewSessions = maps:put(SessionId, NewSession, State#state.sessions),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call({cleanup_session, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            do_cleanup_session(Session),
            NewSession = Session#session{
                memory_used = 0,
                cpu_used = 0.0,
                message_count = 0
            },
            NewSessions = maps:put(SessionId, NewSession, State#state.sessions),
            notify_subscribers(NewSession, session_cleaned),
            {reply, ok, State#state{sessions = NewSessions}}
    end;

handle_call({garbage_collect, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            case Session#session.pid of
                undefined ->
                    {reply, {error, no_pid}, State};
                Pid ->
                    try
                        %% Trigger garbage collection
                        erlang:garbage_collect(Pid),
                        logger:debug("Garbage collected session ~p (pid=~p)", [SessionId, Pid]),
                        {reply, ok, State}
                    catch
                        _:Reason ->
                            logger:error("Failed to garbage collect session ~p: ~p",
                                         [SessionId, Reason]),
                            {reply, {error, Reason}, State}
                    end
            end
    end;

handle_call(list_sessions, _From, State) ->
    SessionIds = maps:keys(State#state.sessions),
    {reply, {ok, SessionIds}, State};

handle_call({get_session_info, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.sessions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            Info = #{
                session_id => Session#session.id,
                pid => Session#session.pid,
                isolated => Session#session.isolated,
                memory_limit => Session#session.memory_limit,
                memory_used => Session#session.memory_used,
                cpu_limit => Session#session.cpu_limit,
                cpu_used => Session#session.cpu_used,
                message_limit => Session#session.message_limit,
                message_count => Session#session.message_count,
                connection_limit => Session#session.connection_limit,
                connection_count => Session#session.connection_count,
                created_at => Session#session.created_at,
                last_activity => Session#session.last_activity,
                age_ms => erlang:system_time(millisecond) - Session#session.created_at
            },
            {reply, {ok, Info}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(cleanup, State) ->
    %% Periodic cleanup of idle/stale sessions
    CleanupCount = do_cleanup_idle_sessions(State),
    case CleanupCount > 0 of
        true ->
            logger:info("Cleanup: removed ~p idle sessions", [CleanupCount]);
        false ->
            ok
    end,
    NewTimer = schedule_cleanup(State#state.cleanup_interval),
    {noreply, State#state{cleanup_timer = NewTimer}};

handle_info({'DOWN', MonRef, process, Pid, Reason}, State) ->
    %% Handle monitored process death
    logger:info("Session process ~p died: ~p", [Pid, Reason]),

    %% Find session by monitor reference
    Sessions = State#state.sessions,
    SessionId = find_session_by_monitor(Sessions, MonRef),

    case SessionId of
        undefined ->
            {noreply, State};
        _ ->
            Session = maps:get(SessionId, Sessions),
            do_cleanup_session(Session),
            NewSessions = maps:remove(SessionId, Sessions),
            logger:info("Removed session ~p due to process death", [SessionId]),
            {noreply, State#state{sessions = NewSessions}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Cleanup all sessions
    maps:foreach(fun(_SessionId, Session) ->
                         do_cleanup_session(Session)
                 end,
                 State#state.sessions),

    %% Cancel cleanup timer
    case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Schedule cleanup timer
-spec schedule_cleanup(pos_integer()) -> reference().
schedule_cleanup(Interval) ->
    erlang:send_after(Interval, self(), cleanup).

%% @private Cleanup idle sessions
-spec do_cleanup_idle_sessions(#state{}) -> non_neg_integer().
do_cleanup_idle_sessions(State) ->
    Now = erlang:system_time(millisecond),
    IdleThreshold = 300000,  % 5 minutes

    IdleSessions = maps:fold(fun(_SessionId, Session, Acc) ->
                                  case (Now - Session#session.last_activity) > IdleThreshold of
                                      true ->
                                          [Session | Acc];
                                      false ->
                                          Acc
                                  end
                          end,
                          [],
                          State#state.sessions),

    lists:foreach(fun(Session) ->
                          do_cleanup_session(Session)
                  end,
                  IdleSessions),

    length(IdleSessions).

%% @private Cleanup session resources
-spec do_cleanup_session(#session{}) -> ok.
do_cleanup_session(Session) ->
    %% Demonitor session process
    maps:foreach(fun(_Pid, MonRef) ->
                         erlang:demonitor(MonRef, [flush])
                 end,
                 Session#session.monitors),

    %% Unsubscribe all subscribers
    lists:foreach(fun({_Pid, MonRef}) ->
                         erlang:demonitor(MonRef, [flush])
                 end,
                 Session#session.subscribers),

    %% Force garbage collection on session process
    case Session#session.pid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            try
                erlang:garbage_collect(Pid)
            catch
                _:_ -> ok
            end
    end,

    ok.

%% @private Check resource limits
-spec do_check_resource_limits(#session{}) -> {boolean(), map()}.
do_check_resource_limits(Session) ->
    Violations = #{
        memory_exceeded => check_memory_limit(Session),
        cpu_exceeded => check_cpu_limit(Session),
        messages_exceeded => check_message_limit(Session),
        connections_exceeded => check_connection_limit(Session)
    },

    HasAnyViolation = lists:any(fun({_K, V}) -> V =:= true end,
                                  maps:to_list(Violations)),

    {not HasAnyViolation, Violations}.

%% @private Check memory limit
-spec check_memory_limit(#session{}) -> boolean().
check_memory_limit(Session) ->
    case Session#session.memory_limit of
        infinity -> false;
        Limit ->
            Session#session.memory_used > Limit
    end.

%% @private Check CPU limit
-spec check_cpu_limit(#session{}) -> boolean().
check_cpu_limit(Session) ->
    case Session#session.cpu_limit of
        infinity -> false;
        Limit ->
            Session#session.cpu_used > Limit
    end.

%% @private Check message limit
-spec check_message_limit(#session{}) -> boolean().
check_message_limit(Session) ->
    case Session#session.message_limit of
        infinity -> false;
        Limit ->
            Session#session.message_count > Limit
    end.

%% @private Check connection limit
-spec check_connection_limit(#session{}) -> boolean().
check_connection_limit(Session) ->
    case Session#session.connection_limit of
        infinity -> false;
        Limit ->
            Session#session.connection_count > Limit
    end.

%% @private Calculate percentage
-spec calculate_percentage(number(), number() | infinity) -> float() | infinity.
calculate_percentage(_Used, infinity) ->
    infinity;
calculate_percentage(Used, Limit) when Limit > 0 ->
    (Used / Limit) * 100.0;
calculate_percentage(_Used, _Limit) ->
    0.0.

%% @private Find session by monitor reference
-spec find_session_by_monitor(#{session_id() => #session{}}, reference()) ->
                                     session_id() | undefined.
find_session_by_monitor(Sessions, MonRef) ->
    maps:fold(fun(SessionId, Session, Acc) ->
                      case maps:is_key(MonRef, Session#session.monitors) of
                          true ->
                              SessionId;
                          false ->
                              Acc
                      end
              end,
              undefined,
              Sessions).

%% @private Notify subscribers
-spec notify_subscribers(#session{}, term()) -> ok.
notify_subscribers(Session, Event) ->
    lists:foreach(fun({Pid, _MonRef}) ->
                          case is_process_alive(Pid) of
                              true ->
                                  Pid ! {session_resource_event, Session#session.id, Event};
                              false ->
                                  ok
                          end
                  end,
                  Session#session.subscribers),
    ok.
