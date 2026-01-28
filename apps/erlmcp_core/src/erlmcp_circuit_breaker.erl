-module(erlmcp_circuit_breaker).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    register_breaker/2,
    register_breaker/3,
    unregister_breaker/1,
    call/2,
    call/3,
    call_with_fallback/3,
    call_with_fallback/4,
    get_state/1,
    get_all_states/0,
    reset/1,
    reset_all/0,
    force_open/1,
    force_close/1,
    get_stats/1,
    get_all_stats/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type breaker_name() :: atom() | binary().
-type breaker_state() :: closed | open | half_open.
-type breaker_result() :: {ok, term()} | {error, term()}.
-type breaker_fun() :: fun(() -> breaker_result()).
-type fallback_fun() :: fun(() -> breaker_result()).

-type breaker_config() ::
    #{failure_threshold => pos_integer(),      % Failures to trip (default: 5)
      success_threshold => pos_integer(),      % Successes in half_open to close (default: 2)
      timeout => pos_integer(),                % Time in ms before half_open attempt (default: 60000)
      window_size => pos_integer(),            % Rolling window for failure rate (default: 10)
      failure_rate_threshold => float(),       % Percentage 0.0-1.0 (default: 0.5)
      reset_timeout => pos_integer()}.         % Auto-reset timeout (default: infinity)

-record(breaker, {
    name :: breaker_name(),
    state = closed :: breaker_state(),
    failures = 0 :: non_neg_integer(),
    successes = 0 :: non_neg_integer(),
    consecutive_failures = 0 :: non_neg_integer(),
    consecutive_successes = 0 :: non_neg_integer(),
    config :: breaker_config(),
    last_failure_time :: undefined | erlang:timestamp(),
    last_state_change :: erlang:timestamp(),
    open_until :: undefined | erlang:timestamp(),
    timeout_ref :: undefined | reference(),

    %% Statistics
    total_calls = 0 :: non_neg_integer(),
    total_successes = 0 :: non_neg_integer(),
    total_failures = 0 :: non_neg_integer(),
    total_rejected = 0 :: non_neg_integer(),

    %% Rolling window for failure rate calculation
    call_history = [] :: [{erlang:timestamp(), success | failure}],

    %% Bulkhead isolation
    resource_quotas = #{} :: #{atom() => term()}
}).

-record(state, {
    breakers = #{} :: #{breaker_name() => #breaker{}},
    default_config :: breaker_config(),
    monitor_refs = #{} :: #{reference() => breaker_name()},
    health_monitor_pid :: undefined | pid()
}).

%%====================================================================
%% Default Configuration
%%====================================================================

-define(DEFAULT_CONFIG, #{
    failure_threshold => 5,           % 5 consecutive failures
    success_threshold => 2,           % 2 consecutive successes to close from half_open
    timeout => 60000,                 % 60 seconds before retry
    window_size => 10,                % Last 10 calls
    failure_rate_threshold => 0.5,    % 50% failure rate
    reset_timeout => infinity         % Never auto-reset
}).

-define(BREAKER_OPEN_ERROR, circuit_breaker_open).
-define(BREAKER_TIMEOUT_ERROR, circuit_breaker_timeout).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Register a circuit breaker with default config
-spec register_breaker(breaker_name(), breaker_config()) -> ok | {error, term()}.
register_breaker(Name, Config) ->
    register_breaker(Name, Config, undefined).

%% @doc Register a circuit breaker with optional health monitor integration
-spec register_breaker(breaker_name(), breaker_config(), pid() | undefined) ->
                          ok | {error, term()}.
register_breaker(Name, Config, MonitorPid) ->
    gen_server:call(?MODULE, {register_breaker, Name, Config, MonitorPid}).

%% @doc Unregister a circuit breaker
-spec unregister_breaker(breaker_name()) -> ok.
unregister_breaker(Name) ->
    gen_server:cast(?MODULE, {unregister_breaker, Name}).

%% @doc Execute a function through the circuit breaker (5 second timeout)
-spec call(breaker_name(), breaker_fun()) -> breaker_result().
call(Name, Fun) ->
    call(Name, Fun, 5000).

%% @doc Execute a function through the circuit breaker with custom timeout
-spec call(breaker_name(), breaker_fun(), timeout()) -> breaker_result().
call(Name, Fun, Timeout) when is_function(Fun, 0) ->
    gen_server:call(?MODULE, {call, Name, Fun}, Timeout).

%% @doc Execute with fallback on circuit open (5 second timeout)
-spec call_with_fallback(breaker_name(), breaker_fun(), fallback_fun()) ->
                            breaker_result().
call_with_fallback(Name, Fun, Fallback) ->
    call_with_fallback(Name, Fun, Fallback, 5000).

%% @doc Execute with fallback on circuit open, custom timeout
-spec call_with_fallback(breaker_name(), breaker_fun(), fallback_fun(), timeout()) ->
                            breaker_result().
call_with_fallback(Name, Fun, Fallback, Timeout)
  when is_function(Fun, 0), is_function(Fallback, 0) ->
    case call(Name, Fun, Timeout) of
        {error, ?BREAKER_OPEN_ERROR} ->
            ?LOG_INFO("Circuit breaker ~p open, using fallback", [Name]),
            Fallback();
        Other ->
            Other
    end.

%% @doc Get current state of a circuit breaker
-spec get_state(breaker_name()) -> breaker_state() | not_found.
get_state(Name) ->
    gen_server:call(?MODULE, {get_state, Name}).

%% @doc Get all circuit breaker states
-spec get_all_states() -> #{breaker_name() => breaker_state()}.
get_all_states() ->
    gen_server:call(?MODULE, get_all_states).

%% @doc Reset a circuit breaker to closed state
-spec reset(breaker_name()) -> ok | {error, term()}.
reset(Name) ->
    gen_server:call(?MODULE, {reset, Name}).

%% @doc Reset all circuit breakers
-spec reset_all() -> ok.
reset_all() ->
    gen_server:cast(?MODULE, reset_all).

%% @doc Force a circuit breaker to open state
-spec force_open(breaker_name()) -> ok | {error, term()}.
force_open(Name) ->
    gen_server:call(?MODULE, {force_state, Name, open}).

%% @doc Force a circuit breaker to closed state
-spec force_close(breaker_name()) -> ok | {error, term()}.
force_close(Name) ->
    gen_server:call(?MODULE, {force_state, Name, closed}).

%% @doc Get statistics for a circuit breaker
-spec get_stats(breaker_name()) -> {ok, map()} | {error, not_found}.
get_stats(Name) ->
    gen_server:call(?MODULE, {get_stats, Name}).

%% @doc Get statistics for all circuit breakers
-spec get_all_stats() -> #{breaker_name() => map()}.
get_all_stats() ->
    gen_server:call(?MODULE, get_all_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    ?LOG_INFO("Starting circuit breaker manager with options: ~p", [Opts]),

    process_flag(trap_exit, true),

    DefaultConfig =
        maps:merge(?DEFAULT_CONFIG, proplists:get_value(default_config, Opts, #{})),

    % Optional health monitor integration
    HealthMonitorPid = case whereis(erlmcp_health_monitor) of
        undefined -> undefined;
        Pid when is_pid(Pid) -> Pid
    end,

    State = #state{
        default_config = DefaultConfig,
        health_monitor_pid = HealthMonitorPid
    },

    ?LOG_INFO("Circuit breaker manager initialized"),
    {ok, State}.

handle_call({register_breaker, Name, Config, _MonitorPid}, _From, State) ->
    MergedConfig = maps:merge(State#state.default_config, Config),

    Breaker = #breaker{
        name = Name,
        state = closed,
        config = MergedConfig,
        last_state_change = erlang:timestamp()
    },

    % Report to health monitor if available
    case State#state.health_monitor_pid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            catch erlmcp_health_monitor:report_circuit_breaker(Name, closed)
    end,

    NewBreakers = maps:put(Name, Breaker, State#state.breakers),
    NewState = State#state{breakers = NewBreakers},

    ?LOG_INFO("Registered circuit breaker: ~p with config: ~p", [Name, MergedConfig]),
    {reply, ok, NewState};

handle_call({call, Name, Fun}, _From, State) ->
    case maps:find(Name, State#state.breakers) of
        {ok, Breaker} ->
            {Result, NewBreaker} = execute_call(Fun, Breaker),
            NewBreakers = maps:put(Name, NewBreaker, State#state.breakers),
            NewState = State#state{breakers = NewBreakers},

            % Report state changes to health monitor
            maybe_report_state_change(Breaker, NewBreaker, State#state.health_monitor_pid),

            {reply, Result, NewState};
        error ->
            {reply, {error, breaker_not_found}, State}
    end;

handle_call({get_state, Name}, _From, State) ->
    Result = case maps:find(Name, State#state.breakers) of
        {ok, #breaker{state = BreakerState}} -> BreakerState;
        error -> not_found
    end,
    {reply, Result, State};

handle_call(get_all_states, _From, State) ->
    States = maps:fold(
        fun(Name, #breaker{state = BreakerState}, Acc) ->
            maps:put(Name, BreakerState, Acc)
        end,
        #{},
        State#state.breakers
    ),
    {reply, States, State};

handle_call({reset, Name}, _From, State) ->
    case maps:find(Name, State#state.breakers) of
        {ok, Breaker} ->
            NewBreaker = reset_breaker(Breaker),
            NewBreakers = maps:put(Name, NewBreaker, State#state.breakers),
            NewState = State#state{breakers = NewBreakers},

            % Report reset to health monitor
            maybe_report_state_change(Breaker, NewBreaker, State#state.health_monitor_pid),

            ?LOG_INFO("Reset circuit breaker: ~p", [Name]),
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({force_state, Name, NewState}, _From, State) ->
    case maps:find(Name, State#state.breakers) of
        {ok, Breaker} ->
            NewBreaker = Breaker#breaker{
                state = NewState,
                last_state_change = erlang:timestamp()
            },
            NewBreakers = maps:put(Name, NewBreaker, State#state.breakers),
            UpdatedState = State#state{breakers = NewBreakers},

            % Report forced state change
            maybe_report_state_change(Breaker, NewBreaker, State#state.health_monitor_pid),

            ?LOG_WARNING("Forced circuit breaker ~p to state: ~p", [Name, NewState]),
            {reply, ok, UpdatedState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_stats, Name}, _From, State) ->
    Result = case maps:find(Name, State#state.breakers) of
        {ok, Breaker} ->
            {ok, breaker_stats(Breaker)};
        error ->
            {error, not_found}
    end,
    {reply, Result, State};

handle_call(get_all_stats, _From, State) ->
    Stats = maps:fold(
        fun(Name, Breaker, Acc) ->
            maps:put(Name, breaker_stats(Breaker), Acc)
        end,
        #{},
        State#state.breakers
    ),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({unregister_breaker, Name}, State) ->
    NewBreakers = maps:remove(Name, State#state.breakers),
    NewState = State#state{breakers = NewBreakers},
    ?LOG_INFO("Unregistered circuit breaker: ~p", [Name]),
    {noreply, NewState};

handle_cast(reset_all, State) ->
    NewBreakers = maps:map(
        fun(_Name, Breaker) ->
            reset_breaker(Breaker)
        end,
        State#state.breakers
    ),
    NewState = State#state{breakers = NewBreakers},
    ?LOG_INFO("Reset all circuit breakers"),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, Name}, State) ->
    % Transition from open to half_open
    case maps:find(Name, State#state.breakers) of
        {ok, Breaker = #breaker{state = open}} ->
            NewBreaker = transition_to_half_open(Breaker),
            NewBreakers = maps:put(Name, NewBreaker, State#state.breakers),
            NewState = State#state{breakers = NewBreakers},

            maybe_report_state_change(Breaker, NewBreaker, State#state.health_monitor_pid),

            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ?LOG_INFO("Circuit breaker manager terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Execute a call through the circuit breaker state machine
-spec execute_call(breaker_fun(), #breaker{}) -> {breaker_result(), #breaker{}}.
execute_call(Fun, Breaker = #breaker{state = closed}) ->
    execute_and_track(Fun, Breaker, fun handle_closed_result/2);

execute_call(Fun, Breaker = #breaker{state = open}) ->
    Now = erlang:timestamp(),
    OpenUntil = Breaker#breaker.open_until,

    case should_attempt_half_open(Now, OpenUntil) of
        true ->
            % Transition to half_open and try the call
            NewBreaker = transition_to_half_open(Breaker),
            execute_and_track(Fun, NewBreaker, fun handle_half_open_result/2);
        false ->
            % Reject the call
            Result = {error, ?BREAKER_OPEN_ERROR},
            UpdatedBreaker = Breaker#breaker{
                total_calls = Breaker#breaker.total_calls + 1,
                total_rejected = Breaker#breaker.total_rejected + 1
            },
            {Result, UpdatedBreaker}
    end;

execute_call(Fun, Breaker = #breaker{state = half_open}) ->
    execute_and_track(Fun, Breaker, fun handle_half_open_result/2).

%% @private Execute function and track result
-spec execute_and_track(breaker_fun(), #breaker{}, fun((term(), #breaker{}) -> #breaker{})) ->
                           {breaker_result(), #breaker{}}.
execute_and_track(Fun, Breaker, ResultHandler) ->
    _StartTime = erlang:timestamp(),

    Result = try
        Fun()
    catch
        Class:Reason:Stacktrace ->
            ?LOG_WARNING("Circuit breaker ~p function failed: ~p:~p",
                        [Breaker#breaker.name, Class, Reason]),
            {error, {exception, Class, Reason, Stacktrace}}
    end,

    EndTime = erlang:timestamp(),

    % Track call in history
    CallResult = case Result of
        {ok, _} -> success;
        _ -> failure
    end,

    UpdatedBreaker = add_call_to_history(Breaker, EndTime, CallResult),
    NewBreaker = ResultHandler(Result, UpdatedBreaker),

    {Result, NewBreaker}.

%% @private Handle result when breaker is closed
-spec handle_closed_result(breaker_result(), #breaker{}) -> #breaker{}.
handle_closed_result({ok, _}, Breaker) ->
    Breaker#breaker{
        consecutive_failures = 0,
        consecutive_successes = Breaker#breaker.consecutive_successes + 1,
        total_calls = Breaker#breaker.total_calls + 1,
        total_successes = Breaker#breaker.total_successes + 1
    };

handle_closed_result({error, _}, Breaker) ->
    NewConsecutiveFailures = Breaker#breaker.consecutive_failures + 1,

    UpdatedBreaker = Breaker#breaker{
        consecutive_failures = NewConsecutiveFailures,
        consecutive_successes = 0,
        total_calls = Breaker#breaker.total_calls + 1,
        total_failures = Breaker#breaker.total_failures + 1,
        last_failure_time = erlang:timestamp()
    },

    % Check failure threshold or failure rate
    ShouldTrip = should_trip_breaker(UpdatedBreaker),

    case ShouldTrip of
        true ->
            ?LOG_WARNING("Circuit breaker ~p tripping to OPEN state (failures: ~p)",
                        [Breaker#breaker.name, NewConsecutiveFailures]),
            transition_to_open(UpdatedBreaker);
        false ->
            UpdatedBreaker
    end.

%% @private Handle result when breaker is half_open
-spec handle_half_open_result(breaker_result(), #breaker{}) -> #breaker{}.
handle_half_open_result({ok, _}, Breaker) ->
    NewConsecutiveSuccesses = Breaker#breaker.consecutive_successes + 1,
    Config = Breaker#breaker.config,
    SuccessThreshold = maps:get(success_threshold, Config),

    UpdatedBreaker = Breaker#breaker{
        consecutive_successes = NewConsecutiveSuccesses,
        consecutive_failures = 0,
        total_calls = Breaker#breaker.total_calls + 1,
        total_successes = Breaker#breaker.total_successes + 1
    },

    case NewConsecutiveSuccesses >= SuccessThreshold of
        true ->
            ?LOG_INFO("Circuit breaker ~p closing (successes: ~p)",
                     [Breaker#breaker.name, NewConsecutiveSuccesses]),
            transition_to_closed(UpdatedBreaker);
        false ->
            UpdatedBreaker
    end;

handle_half_open_result({error, _}, Breaker) ->
    ?LOG_WARNING("Circuit breaker ~p re-opening from half_open (failure)",
                [Breaker#breaker.name]),
    UpdatedBreaker = Breaker#breaker{
        consecutive_failures = Breaker#breaker.consecutive_failures + 1,
        consecutive_successes = 0,
        total_calls = Breaker#breaker.total_calls + 1,
        total_failures = Breaker#breaker.total_failures + 1,
        last_failure_time = erlang:timestamp()
    },
    transition_to_open(UpdatedBreaker).

%% @private Transition breaker to open state
-spec transition_to_open(#breaker{}) -> #breaker{}.
transition_to_open(Breaker) ->
    Config = Breaker#breaker.config,
    Timeout = maps:get(timeout, Config),
    Now = erlang:timestamp(),
    OpenUntil = add_milliseconds(Now, Timeout),

    % Schedule transition to half_open
    TimerRef = erlang:send_after(Timeout, self(), {timeout, Breaker#breaker.name}),

    Breaker#breaker{
        state = open,
        last_state_change = Now,
        open_until = OpenUntil,
        timeout_ref = TimerRef,
        consecutive_successes = 0
    }.

%% @private Transition breaker to half_open state
-spec transition_to_half_open(#breaker{}) -> #breaker{}.
transition_to_half_open(Breaker) ->
    ?LOG_INFO("Circuit breaker ~p transitioning to HALF_OPEN state",
             [Breaker#breaker.name]),

    % Cancel timeout if exists
    case Breaker#breaker.timeout_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    Breaker#breaker{
        state = half_open,
        last_state_change = erlang:timestamp(),
        timeout_ref = undefined,
        open_until = undefined,
        consecutive_failures = 0,
        consecutive_successes = 0
    }.

%% @private Transition breaker to closed state
-spec transition_to_closed(#breaker{}) -> #breaker{}.
transition_to_closed(Breaker) ->
    Breaker#breaker{
        state = closed,
        last_state_change = erlang:timestamp(),
        consecutive_failures = 0,
        open_until = undefined,
        timeout_ref = undefined
    }.

%% @private Reset breaker to initial state
-spec reset_breaker(#breaker{}) -> #breaker{}.
reset_breaker(Breaker) ->
    % Cancel timeout if exists
    case Breaker#breaker.timeout_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    Breaker#breaker{
        state = closed,
        failures = 0,
        successes = 0,
        consecutive_failures = 0,
        consecutive_successes = 0,
        last_failure_time = undefined,
        last_state_change = erlang:timestamp(),
        open_until = undefined,
        timeout_ref = undefined,
        call_history = []
    }.

%% @private Determine if breaker should trip to open
-spec should_trip_breaker(#breaker{}) -> boolean().
should_trip_breaker(Breaker) ->
    Config = Breaker#breaker.config,

    % Check consecutive failures threshold
    FailureThreshold = maps:get(failure_threshold, Config),
    ConsecutiveFailures = Breaker#breaker.consecutive_failures,

    ConsecutiveCheck = ConsecutiveFailures >= FailureThreshold,

    % Check failure rate in rolling window (only if we have enough samples)
    FailureRateThreshold = maps:get(failure_rate_threshold, Config),
    WindowSize = maps:get(window_size, Config),
    HistorySize = length(Breaker#breaker.call_history),

    % Only apply failure rate check if window is full
    FailureRateCheck = case HistorySize >= WindowSize of
        true ->
            FailureRate = calculate_failure_rate(Breaker),
            FailureRate >= FailureRateThreshold;
        false ->
            false
    end,

    ConsecutiveCheck orelse FailureRateCheck.

%% @private Calculate failure rate from rolling window
-spec calculate_failure_rate(#breaker{}) -> float().
calculate_failure_rate(#breaker{call_history = []}) ->
    0.0;
calculate_failure_rate(#breaker{call_history = History}) ->
    Failures = length([R || {_, R} <- History, R =:= failure]),
    Total = length(History),
    case Total of
        0 -> 0.0;
        _ -> Failures / Total
    end.

%% @private Add call to rolling history window
-spec add_call_to_history(#breaker{}, erlang:timestamp(), success | failure) -> #breaker{}.
add_call_to_history(Breaker, Timestamp, Result) ->
    Config = Breaker#breaker.config,
    WindowSize = maps:get(window_size, Config),

    NewHistory = [{Timestamp, Result} | Breaker#breaker.call_history],
    TrimmedHistory = lists:sublist(NewHistory, WindowSize),

    Breaker#breaker{call_history = TrimmedHistory}.

%% @private Check if enough time has passed to attempt half_open
-spec should_attempt_half_open(erlang:timestamp(), undefined | erlang:timestamp()) -> boolean().
should_attempt_half_open(_Now, undefined) ->
    false;
should_attempt_half_open(Now, OpenUntil) ->
    timer:now_diff(Now, OpenUntil) >= 0.

%% @private Add milliseconds to timestamp
-spec add_milliseconds(erlang:timestamp(), pos_integer()) -> erlang:timestamp().
add_milliseconds({MegaSecs, Secs, MicroSecs}, Milliseconds) ->
    TotalMicros = MicroSecs + (Milliseconds * 1000),
    ExtraSecs = TotalMicros div 1000000,
    NewMicroSecs = TotalMicros rem 1000000,

    TotalSecs = Secs + ExtraSecs,
    ExtraMegaSecs = TotalSecs div 1000000,
    NewSecs = TotalSecs rem 1000000,

    {MegaSecs + ExtraMegaSecs, NewSecs, NewMicroSecs}.

%% @private Report state change to health monitor if state changed
-spec maybe_report_state_change(#breaker{}, #breaker{}, pid() | undefined) -> ok.
maybe_report_state_change(#breaker{state = OldState}, #breaker{state = NewState, name = Name},
                         HealthMonitorPid)
  when OldState =/= NewState, is_pid(HealthMonitorPid) ->
    CircuitState = case NewState of
        open -> open;
        _ -> closed
    end,
    catch erlmcp_health_monitor:report_circuit_breaker(Name, CircuitState),
    ok;
maybe_report_state_change(_, _, _) ->
    ok.

%% @private Extract statistics from breaker
-spec breaker_stats(#breaker{}) -> map().
breaker_stats(Breaker) ->
    FailureRate = calculate_failure_rate(Breaker),
    SuccessRate = case Breaker#breaker.total_calls of
        0 -> 0.0;
        Total -> Breaker#breaker.total_successes / Total
    end,

    #{
        name => Breaker#breaker.name,
        state => Breaker#breaker.state,
        consecutive_failures => Breaker#breaker.consecutive_failures,
        consecutive_successes => Breaker#breaker.consecutive_successes,
        total_calls => Breaker#breaker.total_calls,
        total_successes => Breaker#breaker.total_successes,
        total_failures => Breaker#breaker.total_failures,
        total_rejected => Breaker#breaker.total_rejected,
        failure_rate => FailureRate,
        success_rate => SuccessRate,
        last_failure_time => Breaker#breaker.last_failure_time,
        last_state_change => Breaker#breaker.last_state_change,
        config => Breaker#breaker.config
    }.
