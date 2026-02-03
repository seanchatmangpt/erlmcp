%%%-------------------------------------------------------------------
%%% @doc
%%% Session Reaper and Garbage Collection for erlmcp v3
%%% Provides enterprise-grade session cleanup with:
%%%   - Automatic session reaping
%%%   - Configurable cleanup policies
%%%   - Resource reclamation
%%%   - Graceful shutdown
%%%   - Monitoring and metrics
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_reaper).

-behaviour(gen_server).

%% API exports
-export([start_link/1,
         %% Reaper control
         reap/0, reap/1,
         set_policy/2,
         %% Metrics
         get_stats/0,
         get_reaped_count/0,
         %% Configuration
         set_interval/2,
         pause/0, resume/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type reaper_policy() :: idle_timeout |
                         expired |
                         orphaned |
                         all |
                         custom | fun((map()) -> boolean()).

-type session_info() :: #{id := binary(),
                          state => term(),
                          created_at => integer(),
                          last_accessed => integer(),
                          pid => pid() | undefined}.

-record(state,
        {reaper_timer :: reference() | undefined,
         interval_ms :: pos_integer(),
         paused = false :: boolean(),
         policy :: reaper_policy(),
         idle_threshold_ms :: pos_integer(),
         stats :: #{total_reaped := non_neg_integer(),
                    last_reap_at => integer(),
                    last_reap_count => non_neg_integer()}}).

-define(DEFAULT_INTERVAL_MS, 60000).  % 1 minute
-define(DEFAULT_IDLE_THRESHOLD_MS, 1800000).  % 30 minutes

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the session reaper
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Trigger immediate reaping with default policy
-spec reap() -> {ok, non_neg_integer()}.
reap() ->
    reap(all).

%% @doc Trigger immediate reaping with specific policy
-spec reap(reaper_policy()) -> {ok, non_neg_integer()}.
reap(Policy) ->
    gen_server:call(?MODULE, {reap, Policy}, 30000).

%% @doc Set reaper policy
-spec set_policy(reaper_policy(), map()) -> ok.
set_policy(Policy, Options) ->
    gen_server:cast(?MODULE, {set_policy, Policy, Options}).

%% @doc Get reaper statistics
-spec get_stats() -> {ok, map()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats, 5000).

%% @doc Get total reaped session count
-spec get_reaped_count() -> {ok, non_neg_integer()}.
get_reaped_count() ->
    gen_server:call(?MODULE, get_reaped_count, 5000).

%% @doc Set reaper interval
-spec set_interval(pos_integer(), map()) -> ok.
set_interval(IntervalMs, Options) ->
    gen_server:cast(?MODULE, {set_interval, IntervalMs, Options}).

%% @doc Pause reaper
-spec pause() -> ok.
pause() ->
    gen_server:cast(?MODULE, pause).

%% @doc Resume reaper
-spec resume() -> ok.
resume() ->
    gen_server:cast(?MODULE, resume).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Options) ->
    logger:info("Session reaper initializing"),

    IntervalMs = maps:get(interval_ms, Options, ?DEFAULT_INTERVAL_MS),
    IdleThresholdMs = maps:get(idle_threshold_ms, Options, ?DEFAULT_IDLE_THRESHOLD_MS),
    Policy = maps:get(policy, Options, idle_timeout),

    %% Schedule first reap
    ReaperTimer = schedule_reap(IntervalMs),

    InitialStats = #{total_reaped => 0,
                     last_reap_at => undefined,
                     last_reap_count => 0},

    {ok, #state{
        reaper_timer = ReaperTimer,
        interval_ms = IntervalMs,
        paused = false,
        policy = Policy,
        idle_threshold_ms = IdleThresholdMs,
        stats = InitialStats
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
                     {reply, term(), #state{}}.
handle_call({reap, Policy}, _From, State) ->
    ReapedCount = do_reap(Policy, State),
    logger:info("Manual reaped ~p sessions with policy ~p", [ReapedCount, Policy]),

    NewStats = (State#state.stats)#{
        total_reaped => maps:get(total_reaped, State#state.stats) + ReapedCount,
        last_reap_at => erlang:system_time(millisecond),
        last_reap_count => ReapedCount
    },

    {reply, {ok, ReapedCount}, State#state{stats = NewStats}};

handle_call(get_stats, _From, State) ->
    Stats = (State#state.stats)#{
        interval_ms => State#state.interval_ms,
        paused => State#state.paused,
        policy => State#state.policy,
        idle_threshold_ms => State#state.idle_threshold_ms
    },
    {reply, {ok, Stats}, State};

handle_call(get_reaped_count, _From, State) ->
    {reply, {ok, maps:get(total_reaped, State#state.stats)}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({set_policy, Policy, Options}, State) ->
    IdleThreshold = maps:get(idle_threshold_ms, Options, State#state.idle_threshold_ms),
    logger:info("Reaper policy changed to ~p", [Policy]),
    {noreply, State#state{policy = Policy, idle_threshold_ms = IdleThreshold}};

handle_cast({set_interval, IntervalMs, Options}, State) ->
    %% Cancel existing timer and schedule new one
    case State#state.reaper_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    NewTimer = schedule_reap(IntervalMs),
    logger:info("Reaper interval changed to ~p ms", [IntervalMs]),
    {noreply, State#state{interval_ms = IntervalMs, reaper_timer = NewTimer}};

handle_cast(pause, State) ->
    logger:info("Reaper paused"),
    {noreply, State#state{paused = true}};

handle_cast(resume, State) ->
    logger:info("Reaper resumed"),
    {noreply, State#state{paused = false}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(reap, State) ->
    case State#state.paused of
        true ->
            %% Don't reap if paused, just reschedule
            NewTimer = schedule_reap(State#state.interval_ms),
            {noreply, State#state{reaper_timer = NewTimer}};
        false ->
            %% Perform reaping
            ReapedCount = do_reap(State#state.policy, State),

            NewStats = (State#state.stats)#{
                total_reaped => maps:get(total_reaped, State#state.stats) + ReapedCount,
                last_reap_at => erlang:system_time(millisecond),
                last_reap_count => ReapedCount
            },

            %% Schedule next reap
            NewTimer = schedule_reap(State#state.interval_ms),

            case ReapedCount > 0 of
                true ->
                    logger:info("Reaped ~p sessions", [ReapedCount]);
                false ->
                    ok
            end,

            {noreply, State#state{stats = NewStats, reaper_timer = NewTimer}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.reaper_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    logger:info("Session reaper terminated"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Schedule reap timer
-spec schedule_reap(pos_integer()) -> reference().
schedule_reap(IntervalMs) ->
    erlang:send_after(IntervalMs, self(), reap).

%% @private Perform reaping with given policy
-spec do_reap(reaper_policy(), #state{}) -> non_neg_integer().
do_reap(Policy, State) ->
    %% Get all sessions from session manager
    Sessions = fetch_all_sessions(),

    %% Filter sessions based on policy
    SessionsToReap = filter_sessions(Sessions, Policy, State),

    %% Reap each session
    lists:foreach(fun(SessionInfo) ->
                         reap_session(SessionInfo)
                 end,
                 SessionsToReap),

    length(SessionsToReap).

%% @private Fetch all sessions
-spec fetch_all_sessions() -> [session_info()].
fetch_all_sessions() ->
    try
        %% Try to get sessions from session manager
        case whereis(erlmcp_session_manager) of
            undefined ->
                [];
            _Pid ->
                {ok, SessionIds} = erlmcp_session_manager:list_sessions(),
                lists:filtermap(fun(SessionId) ->
                                        case erlmcp_session_manager:get_session(SessionId) of
                                            {ok, SessionData} ->
                                                Info = #{
                                                    id => SessionId,
                                                    state => maps:get(state, SessionData, undefined),
                                                    created_at => maps:get(created_at, SessionData, 0),
                                                    last_accessed => maps:get(last_accessed, SessionData, 0),
                                                    pid => maps:get(pid, SessionData, undefined)
                                                },
                                                {true, Info};
                                            {error, _} ->
                                                false
                                        end
                                end,
                                SessionIds)
        end
    catch
        _:_ ->
            []
    end.

%% @private Filter sessions based on policy
-spec filter_sessions([session_info()], reaper_policy(), #state{}) -> [session_info()].
filter_sessions(Sessions, Policy, State) ->
    Now = erlang:system_time(millisecond),
    IdleThreshold = State#state.idle_threshold_ms,

    lists:filter(fun(SessionInfo) ->
                        should_reap(SessionInfo, Policy, Now, IdleThreshold)
                end,
                Sessions).

%% @private Check if session should be reaped
-spec should_reap(session_info(), reaper_policy(), integer(), pos_integer()) -> boolean().
should_reap(_SessionInfo, all, _Now, _Threshold) ->
    true;
should_reap(SessionInfo, idle_timeout, Now, Threshold) ->
    LastAccessed = maps:get(last_accessed, SessionInfo, Now),
    (Now - LastAccessed) > Threshold;
should_reap(SessionInfo, expired, Now, _Threshold) ->
    case maps:get(timeout_ms, SessionInfo, infinity) of
        infinity -> false;
        Timeout ->
            LastAccessed = maps:get(last_accessed, SessionInfo, Now),
            (Now - LastAccessed) > Timeout
    end;
should_reap(SessionInfo, orphaned, _Now, _Threshold) ->
    %% Check if session process is dead
    Pid = maps:get(pid, SessionInfo, undefined),
    case Pid of
        undefined -> false;
        _ ->
            not is_process_alive(Pid)
    end;
should_reap(SessionInfo, CustomPolicy, Now, Threshold) when is_function(CustomPolicy, 1) ->
    try
        CustomPolicy(SessionInfo)
    catch
        _:_ -> false
    end;
should_reap(_SessionInfo, _Policy, _Now, _Threshold) ->
    false.

%% @private Reap a single session
-spec reap_session(session_info()) -> ok.
reap_session(SessionInfo) ->
    SessionId = maps:get(id, SessionInfo),

    %% Try to delete from session manager
    try
        ok = erlmcp_session_manager:delete_session(SessionId),

        %% Force garbage collection if pid exists
        Pid = maps:get(pid, SessionInfo, undefined),
        case Pid of
            undefined -> ok;
            _ when is_pid(Pid) ->
                try
                    erlang:garbage_collect(Pid)
                catch
                    _:_ -> ok
                end
        end,

        logger:debug("Reaped session ~p", [SessionId])
    catch
        _:_ ->
            logger:warning("Failed to reap session ~p", [SessionId])
    end.
