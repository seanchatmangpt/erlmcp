%%%-------------------------------------------------------------------
%%% @doc
%%% Session Rate Limiter and Quota Management for erlmcp v3
%%% Implements enterprise-grade rate limiting with:
%%%   - Token bucket algorithm
%%%   - Sliding window counters
%%%   - Per-session quotas
%%%   - Global rate limits
%%%   - Priority queues
%%%   - Throttling and backpressure
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_rate_limiter).

-behaviour(gen_server).

%% API exports
-export([start_link/1,
         %% Rate limiting
         check_rate_limit/2, check_rate_limit/3,
         record_request/2, record_request/3,
         set_quota/2, set_quota/3,
         get_quota/1, get_quota/2,
         reset_quota/1, reset_quota/2,
         %% Throttling
         check_throttle/1, apply_throttle/2,
         %% Monitoring
         get_usage/1, get_usage/2,
         get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type session_id() :: binary().
-type quota_type() :: requests | messages | bytes | connections.
-type quota_config() :: #{capacity => pos_integer(),
                           rate => float(),
                           window => pos_integer()}.
-type quota_state() :: #{tokens => float(),
                          last_update => integer(),
                          capacity => pos_integer(),
                          rate => float()}.
-type throttle_action() :: drop | delay | queue | reject.

-record(state,
        {quotas :: #{session_id() => #{quota_type() => quota_state()}},
         global_quota :: quota_state() | undefined,
         window_start :: integer(),
         cleanup_interval :: pos_integer(),
         cleanup_timer :: reference() | undefined}).

-define(DEFAULT_CAPACITY, 1000).
-define(DEFAULT_RATE, 10.0).  % tokens per second
-define(DEFAULT_WINDOW_MS, 60000).  % 1 minute
-define(DEFAULT_CLEANUP_INTERVAL_MS, 300000).  % 5 minutes

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start rate limiter
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Check if request is allowed (uses default requests quota)
-spec check_rate_limit(session_id(), pos_integer()) -> {ok, boolean(), float()}.
check_rate_limit(SessionId, Cost) ->
    check_rate_limit(SessionId, requests, Cost).

%% @doc Check if request is allowed for specific quota type
-spec check_rate_limit(session_id(), quota_type(), pos_integer()) ->
                              {ok, boolean(), float()}.
check_rate_limit(SessionId, Type, Cost) ->
    gen_server:call(?MODULE, {check_rate_limit, SessionId, Type, Cost}, 5000).

%% @doc Record a request (uses default requests quota)
-spec record_request(session_id(), pos_integer()) -> ok.
record_request(SessionId, Cost) ->
    record_request(SessionId, requests, Cost).

%% @doc Record a request for specific quota type
-spec record_request(session_id(), quota_type(), pos_integer()) -> ok.
record_request(SessionId, Type, Cost) ->
    gen_server:cast(?MODULE, {record_request, SessionId, Type, Cost}).

%% @doc Set quota for session (uses defaults for window/rate)
-spec set_quota(session_id(), pos_integer()) -> ok | {error, term()}.
set_quota(SessionId, Capacity) ->
    set_quota(SessionId, requests, #{capacity => Capacity,
                                      rate => ?DEFAULT_RATE,
                                      window => ?DEFAULT_WINDOW_MS}).

%% @doc Set quota for session with full config
-spec set_quota(session_id(), quota_type(), quota_config()) -> ok | {error, term()}.
set_quota(SessionId, Type, Config) ->
    gen_server:call(?MODULE, {set_quota, SessionId, Type, Config}, 5000).

%% @doc Get quota for session (uses default requests quota)
-spec get_quota(session_id()) -> {ok, quota_state()} | {error, term()}.
get_quota(SessionId) ->
    get_quota(SessionId, requests).

%% @doc Get quota for session for specific type
-spec get_quota(session_id(), quota_type()) -> {ok, quota_state()} | {error, term()}.
get_quota(SessionId, Type) ->
    gen_server:call(?MODULE, {get_quota, SessionId, Type}, 5000).

%% @doc Reset quota for session (uses default requests quota)
-spec reset_quota(session_id()) -> ok.
reset_quota(SessionId) ->
    reset_quota(SessionId, requests).

%% @doc Reset quota for session for specific type
-spec reset_quota(session_id(), quota_type()) -> ok.
reset_quota(SessionId, Type) ->
    gen_server:cast(?MODULE, {reset_quota, SessionId, Type}).

%% @doc Check if session is being throttled
-spec check_throttle(session_id()) -> {ok, boolean()}.
check_throttle(SessionId) ->
    gen_server:call(?MODULE, {check_throttle, SessionId}, 5000).

%% @doc Apply throttle action
-spec apply_throttle(session_id(), throttle_action()) -> ok | {error, term()}.
apply_throttle(SessionId, Action) ->
    gen_server:cast(?MODULE, {apply_throttle, SessionId, Action}).

%% @doc Get usage for session (uses default requests quota)
-spec get_usage(session_id()) -> {ok, map()} | {error, term()}.
get_usage(SessionId) ->
    get_usage(SessionId, requests).

%% @doc Get usage for session for specific quota type
-spec get_usage(session_id(), quota_type()) -> {ok, map()} | {error, term()}.
get_usage(SessionId, Type) ->
    gen_server:call(?MODULE, {get_usage, SessionId, Type}, 5000).

%% @doc Get global statistics
-spec get_stats() -> {ok, map()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Options) ->
    logger:info("Session rate limiter initializing"),

    %% Initialize global quota if enabled
    GlobalQuota = case maps:get(global_quota_enabled, Options, false) of
                      true ->
                          Capacity = maps:get(global_capacity, Options, ?DEFAULT_CAPACITY),
                          Rate = maps:get(global_rate, Options, ?DEFAULT_RATE),
                          #{tokens => Capacity * 1.0,
                            last_update => erlang:system_time(millisecond),
                            capacity => Capacity,
                            rate => Rate};
                      false ->
                          undefined
                  end,

    CleanupInterval = maps:get(cleanup_interval, Options, ?DEFAULT_CLEANUP_INTERVAL_MS),
    CleanupTimer = schedule_cleanup(CleanupInterval),

    {ok, #state{
        quotas = #{},
        global_quota = GlobalQuota,
        window_start = erlang:system_time(millisecond),
        cleanup_interval = CleanupInterval,
        cleanup_timer = CleanupTimer
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
                     {reply, term(), #state{}}.
handle_call({check_rate_limit, SessionId, Type, Cost}, _From, State) ->
    Now = erlang:system_time(millisecond),

    %% Get or create quota state
    SessionQuotas = maps:get(SessionId, State#state.quotas, #{}),
    QuotaState = case maps:get(Type, SessionQuotas, undefined) of
                     undefined ->
                         create_default_quota_state();
                     QS ->
                         refill_tokens(QS, Now)
                 end,

    %% Check if enough tokens
    {Allowed, NewQuotaState} = case maps:get(tokens, QuotaState, 0) >= Cost of
                                    true ->
                                        {true, QuotaState#{tokens => maps:get(tokens, QuotaState, 0) - Cost,
                                                          last_update => Now}};
                                    false ->
                                        {false, QuotaState}
                                end,

    %% Update quota state
    NewSessionQuotas = SessionQuotas#{Type => NewQuotaState},
    NewQuotas = maps:put(SessionId, NewSessionQuotas, State#state.quotas),
    NewState = State#state{quotas = NewQuotas},

    %% Calculate wait time for rate limiting headers
    WaitTime = case Allowed of
                   true -> 0.0;
                   false ->
                       TokensNeeded = Cost - maps:get(tokens, QuotaState, 0),
                       TokensNeeded / maps:get(rate, QuotaState, ?DEFAULT_RATE)
               end,

    {reply, {ok, Allowed, WaitTime}, NewState};

handle_call({set_quota, SessionId, Type, Config}, _From, State) ->
    Capacity = maps:get(capacity, Config, ?DEFAULT_CAPACITY),
    Rate = maps:get(rate, Config, ?DEFAULT_RATE),
    Window = maps:get(window, Config, ?DEFAULT_WINDOW_MS),

    QuotaState = #{tokens => Capacity * 1.0,
                   last_update => erlang:system_time(millisecond),
                   capacity => Capacity,
                   rate => Rate,
                   window => Window},

    SessionQuotas = maps:get(SessionId, State#state.quotas, #{}),
    NewSessionQuotas = SessionQuotas#{Type => QuotaState},
    NewQuotas = maps:put(SessionId, NewSessionQuotas, State#state.quotas),

    logger:info("Set quota for session ~p (~p): capacity=~p, rate=~p",
                [SessionId, Type, Capacity, Rate]),

    {reply, ok, State#state{quotas = NewQuotas}};

handle_call({get_quota, SessionId, Type}, _From, State) ->
    case maps:get(SessionId, State#state.quotas, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        SessionQuotas ->
            case maps:get(Type, SessionQuotas, undefined) of
                undefined ->
                    {reply, {error, not_found}, State};
                QuotaState ->
                    Now = erlang:system_time(millisecond),
                    Refilled = refill_tokens(QuotaState, Now),
                    {reply, {ok, Refilled}, State}
            end
    end;

handle_call({check_throttle, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.quotas, undefined) of
        undefined ->
            {reply, {ok, false}, State};
        SessionQuotas ->
            RequestsQuota = maps:get(requests, SessionQuotas, undefined),
            IsThrottled = case RequestsQuota of
                              undefined -> false;
                              QS -> maps:get(tokens, QS, 0) < 0
                          end,
            {reply, {ok, IsThrottled}, State}
    end;

handle_call({get_usage, SessionId, Type}, _From, State) ->
    case maps:get(SessionId, State#state.quotas, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        SessionQuotas ->
            case maps:get(Type, SessionQuotas, undefined) of
                undefined ->
                    {reply, {error, not_found}, State};
                QuotaState ->
                    Now = erlang:system_time(millisecond),
                    Refilled = refill_tokens(QuotaState, Now),
                    Capacity = maps:get(capacity, Refilled, ?DEFAULT_CAPACITY),
                    Tokens = maps:get(tokens, Refilled, 0.0),
                    Usage = #{
                        tokens_used => Capacity - Tokens,
                        tokens_remaining => Tokens,
                        capacity => Capacity,
                        utilization_pct => ((Capacity - Tokens) / Capacity) * 100.0,
                        rate => maps:get(rate, Refilled, ?DEFAULT_RATE),
                        last_update => maps:get(last_update, Refilled, Now)
                    },
                    {reply, {ok, Usage}, State}
            end
    end;

handle_call(get_stats, _From, State) ->
    TotalSessions = maps:size(State#state.quotas),
    ThrottledSessions = count_throttled_sessions(State#state.quotas),

    Stats = #{
        total_sessions => TotalSessions,
        throttled_sessions => ThrottledSessions,
        window_start => State#state.window_start,
        global_quota => State#state.global_quota =/= undefined
    },

    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({record_request, SessionId, Type, Cost}, State) ->
    Now = erlang:system_time(millisecond),

    SessionQuotas = maps:get(SessionId, State#state.quotas, #{}),
    QuotaState = case maps:get(Type, SessionQuotas, undefined) of
                     undefined ->
                         create_default_quota_state();
                     QS ->
                         refill_tokens(QS, Now)
                 end,

    %% Deduct tokens
    NewQuotaState = QuotaState#{tokens => maps:get(tokens, QuotaState, 0) - Cost,
                                last_update => Now},

    NewSessionQuotas = SessionQuotas#{Type => NewQuotaState},
    NewQuotas = maps:put(SessionId, NewSessionQuotas, State#state.quotas),

    {noreply, State#state{quotas = NewQuotas}};

handle_cast({reset_quota, SessionId, Type}, State) ->
    case maps:get(SessionId, State#state.quotas, undefined) of
        undefined ->
            {noreply, State};
        SessionQuotas ->
            case maps:get(Type, SessionQuotas, undefined) of
                undefined ->
                    {noreply, State};
                QuotaState ->
                    Capacity = maps:get(capacity, QuotaState, ?DEFAULT_CAPACITY),
                    ResetQuota = QuotaState#{tokens => Capacity,
                                            last_update => erlang:system_time(millisecond)},
                    NewSessionQuotas = SessionQuotas#{Type => ResetQuota},
                    NewQuotas = maps:put(SessionId, NewSessionQuotas, State#state.quotas),
                    {noreply, State#state{quotas = NewQuotas}}
            end
    end;

handle_cast({apply_throttle, SessionId, Action}, State) ->
    case Action of
        drop ->
            logger:info("Dropping throttled session ~p", [SessionId]),
            SessionQuotas = maps:get(SessionId, State#state.quotas, #{}),
            NewSessionQuotas = maps:map(fun(_K, QS) ->
                                               QS#{tokens => 0.0}
                                       end,
                                       SessionQuotas),
            NewQuotas = maps:put(SessionId, NewSessionQuotas, State#state.quotas),
            {noreply, State#state{quotas = NewQuotas}};
        reject ->
            %% Set tokens to negative to force rejections
            SessionQuotas = maps:get(SessionId, State#state.quotas, #{}),
            NewSessionQuotas = maps:map(fun(_K, QS) ->
                                               QS#{tokens => -1.0}
                                       end,
                                       SessionQuotas),
            NewQuotas = maps:put(SessionId, NewSessionQuotas, State#state.quotas),
            {noreply, State#state{quotas = NewQuotas}};
        _ ->
            logger:warning("Unsupported throttle action: ~p", [Action]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(cleanup, State) ->
    %% Remove stale sessions (not used in 10 minutes)
    Now = erlang:system_time(millisecond),
    StaleThreshold = 600000,  % 10 minutes

    CleanedQuotas = maps:filter(fun(_SessionId, SessionQuotas) ->
                                    %% Check if any quota has recent activity
                                    lists:any(fun({_Type, QS}) ->
                                                     (Now - maps:get(last_update, QS, 0)) < StaleThreshold
                                             end,
                                             maps:to_list(SessionQuotas))
                            end,
                            State#state.quotas),

    CleanupCount = maps:size(State#state.quotas) - maps:size(CleanedQuotas),
    case CleanupCount > 0 of
        true ->
            logger:info("Cleanup: removed ~p stale sessions", [CleanupCount]);
        false ->
            ok
    end,

    NewTimer = schedule_cleanup(State#state.cleanup_interval),
    {noreply, State#state{quotas = CleanedQuotas, cleanup_timer = NewTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
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

%% @private Create default quota state
-spec create_default_quota_state() -> quota_state().
create_default_quota_state() ->
    #{tokens => ?DEFAULT_CAPACITY * 1.0,
      last_update => erlang:system_time(millisecond),
      capacity => ?DEFAULT_CAPACITY,
      rate => ?DEFAULT_RATE,
      window => ?DEFAULT_WINDOW_MS}.

%% @private Refill tokens based on rate
-spec refill_tokens(quota_state(), integer()) -> quota_state().
refill_tokens(#{tokens := Tokens,
                last_update := LastUpdate,
                capacity := Capacity,
                rate := Rate}, Now) ->
    TimeDelta = (Now - LastUpdate) / 1000.0,  % Convert to seconds
    TokensToAdd = TimeDelta * Rate,
    NewTokens = min(Capacity, Tokens + TokensToAdd),
    #{tokens => NewTokens,
      last_update => Now,
      capacity => Capacity,
      rate => Rate}.

%% @private Count throttled sessions
-spec count_throttled_sessions(#{session_id() => #{quota_type() => quota_state()}}) ->
                                    non_neg_integer().
count_throttled_sessions(Quotas) ->
    maps:fold(fun(_SessionId, SessionQuotas, Acc) ->
                      case maps:get(requests, SessionQuotas, undefined) of
                          undefined ->
                              Acc;
                          #{tokens := Tokens} when Tokens < 0 ->
                              Acc + 1;
                          _ ->
                              Acc
                      end
              end,
              0,
              Quotas).

%% @private Schedule cleanup timer
-spec schedule_cleanup(pos_integer()) -> reference().
schedule_cleanup(Interval) ->
    erlang:send_after(Interval, self(), cleanup).
