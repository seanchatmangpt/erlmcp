-module(erlmcp_circuit_breaker).
-behaviour(gen_statem).

%% API
-export([
    start_link/0,
    start_link/1,
    start_link/2,
    call/2,
    call/3,
    call_with_fallback/3,
    call_with_fallback/4,
    get_state/1,
    get_stats/1,
    reset/1,
    force_open/1,
    force_close/1,
    stop/1,
    % Manager compatibility API
    register_breaker/2,
    register_breaker/3,
    unregister_breaker/1,
    get_all_states/0,
    get_all_stats/0,
    reset_all/0
]).

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    terminate/3,
    code_change/4,
    format_status/1
]).

%% State functions
-export([
    closed/3,
    open/3,
    half_open/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type breaker_name() :: atom() | {global, term()} | {via, module(), term()}.
-type breaker_result() :: {ok, term()} | {error, term()}.
-type breaker_fun() :: fun(() -> breaker_result()).
-type fallback_fun() :: fun(() -> breaker_result()).

-type config() ::
    #{failure_threshold => pos_integer(),      % Failures to trip (default: 5)
      success_threshold => pos_integer(),      % Successes in half_open to close (default: 2)
      timeout => pos_integer(),                % Time in ms before half_open attempt (default: 60000)
      window_size => pos_integer(),            % Rolling window for failure rate (default: 10)
      failure_rate_threshold => float()}.      % Percentage 0.0-1.0 (default: 0.5)

-record(data, {
    name :: atom(),
    config :: config(),
    % Counters
    consecutive_failures = 0 :: non_neg_integer(),
    consecutive_successes = 0 :: non_neg_integer(),
    total_calls = 0 :: non_neg_integer(),
    total_successes = 0 :: non_neg_integer(),
    total_failures = 0 :: non_neg_integer(),
    total_rejected = 0 :: non_neg_integer(),
    % Timestamps
    last_failure_time :: undefined | integer(),
    last_state_change :: integer(),
    % Rolling window for failure rate
    call_history = [] :: [{integer(), success | failure}]
}).

-type state_name() :: closed | open | half_open.
-type data() :: #data{}.


%% Hibernation configuration for idle circuit breakers
%% Reduces memory per idle circuit breaker from ~50KB to ~5KB
-define(HIBERNATE_AFTER_MS, 30000). % 30 seconds of inactivity triggers hibernation
-define(DEFAULT_CONFIG, #{
    failure_threshold => 5,
    success_threshold => 2,
    timeout => 60000,
    window_size => 10,
    failure_rate_threshold => 0.5
}).

-define(BREAKER_OPEN_ERROR, circuit_breaker_open).
-define(MANAGER_TABLE, erlmcp_circuit_breaker_registry).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a circuit breaker with default configuration
%% Used by supervisor when started as a dependency
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(default_config()).

%% @doc Start a circuit breaker with default name
-spec start_link(config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    Name = make_ref(),
    start_link(Name, Config).

%% @doc Start a circuit breaker with a name
-spec start_link(atom(), config()) -> {ok, pid()} | {error, term()}.
start_link(Name, Config) when is_atom(Name) ->
    %% Enable hibernation after 30 seconds of inactivity to reduce memory usage
    gen_statem:start_link({local, Name}, ?MODULE, {Name, Config}, [{hibernate_after, ?HIBERNATE_AFTER_MS}]);
start_link(Name, Config) ->
    %% Enable hibernation after 30 seconds of inactivity to reduce memory usage
    gen_statem:start_link(?MODULE, {Name, Config}, [{hibernate_after, ?HIBERNATE_AFTER_MS}]).

%% @doc Execute a function through the circuit breaker (5 second timeout)
-spec call(pid() | atom(), breaker_fun()) -> breaker_result().
call(Breaker, Fun) ->
    call(Breaker, Fun, 5000).

%% @doc Execute a function through the circuit breaker with custom timeout
-spec call(pid() | atom(), breaker_fun(), timeout()) -> breaker_result().
call(Breaker, Fun, Timeout) when is_function(Fun, 0) ->
    gen_statem:call(Breaker, {execute, Fun}, Timeout).

%% @doc Execute with fallback on circuit open (5 second timeout)
-spec call_with_fallback(pid() | atom(), breaker_fun(), fallback_fun()) ->
                            breaker_result().
call_with_fallback(Breaker, Fun, Fallback) ->
    call_with_fallback(Breaker, Fun, Fallback, 5000).

%% @doc Execute with fallback on circuit open, custom timeout
-spec call_with_fallback(pid() | atom(), breaker_fun(), fallback_fun(), timeout()) ->
                            breaker_result().
call_with_fallback(Breaker, Fun, Fallback, Timeout)
  when is_function(Fun, 0), is_function(Fallback, 0) ->
    case call(Breaker, Fun, Timeout) of
        {error, ?BREAKER_OPEN_ERROR} ->
            ?LOG_INFO("Circuit breaker ~p open, using fallback", [Breaker]),
            Fallback();
        Other ->
            Other
    end.

%% @doc Get current state of the circuit breaker
-spec get_state(pid() | atom()) -> state_name().
get_state(Breaker) ->
    gen_statem:call(Breaker, get_state).

%% @doc Get statistics for the circuit breaker
-spec get_stats(pid() | atom()) -> {ok, map()}.
get_stats(Breaker) ->
    gen_statem:call(Breaker, get_stats).

%% @doc Reset circuit breaker to closed state
-spec reset(pid() | atom()) -> ok.
reset(Breaker) ->
    gen_statem:call(Breaker, reset).

%% @doc Force circuit breaker to open state
-spec force_open(pid() | atom()) -> ok.
force_open(Breaker) ->
    gen_statem:call(Breaker, force_open).

%% @doc Force circuit breaker to closed state
-spec force_close(pid() | atom()) -> ok.
force_close(Breaker) ->
    gen_statem:call(Breaker, force_close).

%% @doc Stop the circuit breaker
-spec stop(pid() | atom()) -> ok.
stop(Breaker) ->
    gen_statem:stop(Breaker).

%% @doc Get default configuration for circuit breaker
-spec default_config() -> config().
default_config() ->
    #{
        threshold => 5,
        timeout_ms => 60000,
        reset_timeout_ms => 30000,
        call_timeout_ms => 5000
    }.

%%====================================================================
%% Manager Compatibility API (for existing tests)
%%====================================================================

%% @doc Register a circuit breaker with default config
-spec register_breaker(atom(), config()) -> ok | {error, term()}.
register_breaker(Name, Config) ->
    register_breaker(Name, Config, undefined).

%% @doc Register a circuit breaker (manager compatibility)
-spec register_breaker(atom(), config(), pid() | undefined) -> ok | {error, term()}.
register_breaker(Name, Config, _MonitorPid) ->
    ensure_manager_table(),
    case ets:lookup(?MANAGER_TABLE, Name) of
        [{Name, _Pid}] ->
            {error, already_registered};
        [] ->
            case start_link(Name, Config) of
                {ok, Pid} ->
                    ets:insert(?MANAGER_TABLE, {Name, Pid}),
                    ok;
                Error ->
                    Error
            end
    end.

%% @doc Unregister a circuit breaker
-spec unregister_breaker(atom()) -> ok.
unregister_breaker(Name) ->
    ensure_manager_table(),
    case ets:lookup(?MANAGER_TABLE, Name) of
        [{Name, Pid}] ->
            catch stop(Pid),
            ets:delete(?MANAGER_TABLE, Name),
            ok;
        [] ->
            ok
    end.

%% @doc Get all circuit breaker states
-spec get_all_states() -> #{atom() => state_name()}.
get_all_states() ->
    ensure_manager_table(),
    ets:foldl(
        fun({Name, Pid}, Acc) ->
            case is_process_alive(Pid) of
                true ->
                    State = get_state(Pid),
                    maps:put(Name, State, Acc);
                false ->
                    ets:delete(?MANAGER_TABLE, Name),
                    Acc
            end
        end,
        #{},
        ?MANAGER_TABLE
    ).

%% @doc Get all circuit breaker stats
-spec get_all_stats() -> #{atom() => map()}.
get_all_stats() ->
    ensure_manager_table(),
    ets:foldl(
        fun({Name, Pid}, Acc) ->
            case is_process_alive(Pid) of
                true ->
                    {ok, Stats} = get_stats(Pid),
                    maps:put(Name, Stats, Acc);
                false ->
                    ets:delete(?MANAGER_TABLE, Name),
                    Acc
            end
        end,
        #{},
        ?MANAGER_TABLE
    ).

%% @doc Reset all circuit breakers
-spec reset_all() -> ok.
reset_all() ->
    ensure_manager_table(),
    ets:foldl(
        fun({_Name, Pid}, _Acc) ->
            catch reset(Pid)
        end,
        ok,
        ?MANAGER_TABLE
    ),
    ok.

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() ->
    [state_functions, state_enter].

init({Name, UserConfig}) ->
    process_flag(trap_exit, true),
    Config = maps:merge(?DEFAULT_CONFIG, UserConfig),
    Data = #data{
        name = Name,
        config = Config,
        last_state_change = erlang:monotonic_time(millisecond)
    },
    ?LOG_INFO("Circuit breaker ~p starting in closed state with config: ~p", [Name, Config]),
    {ok, closed, Data}.

terminate(_Reason, _State, #data{name = Name}) ->
    ?LOG_INFO("Circuit breaker ~p terminating", [Name]),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

format_status(#{state := State, data := Data}) ->
    #{
        state => State,
        name => Data#data.name,
        consecutive_failures => Data#data.consecutive_failures,
        consecutive_successes => Data#data.consecutive_successes,
        total_calls => Data#data.total_calls
    }.

%%====================================================================
%% State Functions
%%====================================================================

%% CLOSED state - Normal operation
closed(enter, _OldState, Data) ->
    ?LOG_DEBUG("Circuit breaker ~p entered CLOSED state", [Data#data.name]),
    NewData = Data#data{
        consecutive_failures = 0,
        last_state_change = erlang:monotonic_time(millisecond)
    },
    {keep_state, NewData};

closed({call, From}, {execute, Fun}, Data) ->
    try Fun() of
        {ok, _} = Result ->
            NewData = handle_success(Data),
            {keep_state, NewData, [{reply, From, Result}]};
        {error, _} = Result ->
            NewData = handle_failure(Data),
            case should_trip(NewData) of
                true ->
                    ?LOG_WARNING("Circuit breaker ~p tripping to OPEN (failures: ~p)",
                                [Data#data.name, NewData#data.consecutive_failures]),
                    {next_state, open, NewData, [{reply, From, Result}]};
                false ->
                    {keep_state, NewData, [{reply, From, Result}]}
            end
    catch
        Class:Reason:_Stacktrace ->
            ?LOG_WARNING("Circuit breaker ~p function failed: ~p:~p",
                        [Data#data.name, Class, Reason]),
            Result = {error, {exception, Class, Reason}},
            NewData = handle_failure(Data),
            case should_trip(NewData) of
                true ->
                    {next_state, open, NewData, [{reply, From, Result}]};
                false ->
                    {keep_state, NewData, [{reply, From, Result}]}
            end
    end;

closed({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, closed}]};

closed({call, From}, get_stats, Data) ->
    Stats = build_stats(closed, Data),
    {keep_state_and_data, [{reply, From, {ok, Stats}}]};

closed({call, From}, reset, Data) ->
    NewData = reset_data(Data),
    {keep_state, NewData, [{reply, From, ok}]};

closed({call, From}, force_open, Data) ->
    ?LOG_WARNING("Circuit breaker ~p forced to OPEN state", [Data#data.name]),
    {next_state, open, Data, [{reply, From, ok}]};

closed({call, From}, force_close, _Data) ->
    {keep_state_and_data, [{reply, From, ok}]};

closed(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, Data).

%% OPEN state - Failing, reject requests
open(enter, _OldState, Data) ->
    ?LOG_WARNING("Circuit breaker ~p entered OPEN state", [Data#data.name]),
    Config = Data#data.config,
    Timeout = maps:get(timeout, Config),
    NewData = Data#data{
        consecutive_successes = 0,
        last_state_change = erlang:monotonic_time(millisecond)
    },
    {keep_state, NewData, [{state_timeout, Timeout, attempt_recovery}]};

open(state_timeout, attempt_recovery, Data) ->
    ?LOG_INFO("Circuit breaker ~p timeout expired, transitioning to HALF_OPEN",
             [Data#data.name]),
    {next_state, half_open, Data};

open({call, From}, {execute, _Fun}, Data) ->
    Result = {error, ?BREAKER_OPEN_ERROR},
    NewData = Data#data{
        total_calls = Data#data.total_calls + 1,
        total_rejected = Data#data.total_rejected + 1
    },
    {keep_state, NewData, [{reply, From, Result}]};

open({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, open}]};

open({call, From}, get_stats, Data) ->
    Stats = build_stats(open, Data),
    {keep_state_and_data, [{reply, From, {ok, Stats}}]};

open({call, From}, reset, Data) ->
    ?LOG_INFO("Circuit breaker ~p reset from OPEN to CLOSED", [Data#data.name]),
    NewData = reset_data(Data),
    {next_state, closed, NewData, [{reply, From, ok}]};

open({call, From}, force_open, _Data) ->
    {keep_state_and_data, [{reply, From, ok}]};

open({call, From}, force_close, Data) ->
    ?LOG_WARNING("Circuit breaker ~p forced to CLOSED state", [Data#data.name]),
    {next_state, closed, Data, [{reply, From, ok}]};

open(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, Data).

%% HALF_OPEN state - Testing recovery
half_open(enter, _OldState, Data) ->
    ?LOG_INFO("Circuit breaker ~p entered HALF_OPEN state", [Data#data.name]),
    NewData = Data#data{
        consecutive_failures = 0,
        consecutive_successes = 0,
        last_state_change = erlang:monotonic_time(millisecond)
    },
    {keep_state, NewData};

half_open({call, From}, {execute, Fun}, Data) ->
    try Fun() of
        {ok, _} = Result ->
            NewData = handle_success(Data),
            Config = Data#data.config,
            SuccessThreshold = maps:get(success_threshold, Config),
            case NewData#data.consecutive_successes >= SuccessThreshold of
                true ->
                    ?LOG_INFO("Circuit breaker ~p closing after ~p successes",
                             [Data#data.name, NewData#data.consecutive_successes]),
                    {next_state, closed, NewData, [{reply, From, Result}]};
                false ->
                    {keep_state, NewData, [{reply, From, Result}]}
            end;
        {error, _} = Result ->
            ?LOG_WARNING("Circuit breaker ~p re-opening from HALF_OPEN (failure)",
                        [Data#data.name]),
            NewData = handle_failure(Data),
            {next_state, open, NewData, [{reply, From, Result}]}
    catch
        Class:Reason:_Stacktrace ->
            ?LOG_WARNING("Circuit breaker ~p function failed in HALF_OPEN: ~p:~p",
                        [Data#data.name, Class, Reason]),
            Result = {error, {exception, Class, Reason}},
            NewData = handle_failure(Data),
            {next_state, open, NewData, [{reply, From, Result}]}
    end;

half_open({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, half_open}]};

half_open({call, From}, get_stats, Data) ->
    Stats = build_stats(half_open, Data),
    {keep_state_and_data, [{reply, From, {ok, Stats}}]};

half_open({call, From}, reset, Data) ->
    ?LOG_INFO("Circuit breaker ~p reset from HALF_OPEN to CLOSED", [Data#data.name]),
    NewData = reset_data(Data),
    {next_state, closed, NewData, [{reply, From, ok}]};

half_open({call, From}, force_open, Data) ->
    ?LOG_WARNING("Circuit breaker ~p forced to OPEN state", [Data#data.name]),
    {next_state, open, Data, [{reply, From, ok}]};

half_open({call, From}, force_close, Data) ->
    ?LOG_WARNING("Circuit breaker ~p forced to CLOSED state", [Data#data.name]),
    {next_state, closed, Data, [{reply, From, ok}]};

half_open(EventType, EventContent, Data) ->
    handle_common(EventType, EventContent, Data).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Handle common events across all states
handle_common(_EventType, _EventContent, _Data) ->
    keep_state_and_data.

%% @private Handle successful call
-spec handle_success(data()) -> data().
handle_success(Data) ->
    Now = erlang:monotonic_time(millisecond),
    NewHistory = add_to_history(Data#data.call_history, Now, success, Data#data.config),
    Data#data{
        consecutive_successes = Data#data.consecutive_successes + 1,
        consecutive_failures = 0,
        total_calls = Data#data.total_calls + 1,
        total_successes = Data#data.total_successes + 1,
        call_history = NewHistory
    }.

%% @private Handle failed call
-spec handle_failure(data()) -> data().
handle_failure(Data) ->
    Now = erlang:monotonic_time(millisecond),
    NewHistory = add_to_history(Data#data.call_history, Now, failure, Data#data.config),
    Data#data{
        consecutive_failures = Data#data.consecutive_failures + 1,
        consecutive_successes = 0,
        total_calls = Data#data.total_calls + 1,
        total_failures = Data#data.total_failures + 1,
        last_failure_time = Now,
        call_history = NewHistory
    }.

%% @private Determine if breaker should trip to open
-spec should_trip(data()) -> boolean().
should_trip(Data) ->
    Config = Data#data.config,

    % Check consecutive failures threshold
    FailureThreshold = maps:get(failure_threshold, Config),
    ConsecutiveCheck = Data#data.consecutive_failures >= FailureThreshold,

    % Check failure rate in rolling window (only if window is full)
    WindowSize = maps:get(window_size, Config),
    HistorySize = length(Data#data.call_history),

    FailureRateCheck = case HistorySize >= WindowSize of
        true ->
            FailureRateThreshold = maps:get(failure_rate_threshold, Config),
            FailureRate = calculate_failure_rate(Data#data.call_history),
            FailureRate >= FailureRateThreshold;
        false ->
            false
    end,

    ConsecutiveCheck orelse FailureRateCheck.

%% @private Calculate failure rate from history
-spec calculate_failure_rate([{integer(), success | failure}]) -> float().
calculate_failure_rate([]) ->
    0.0;
calculate_failure_rate(History) ->
    Failures = length([R || {_, R} <- History, R =:= failure]),
    Total = length(History),
    case Total of
        0 -> 0.0;
        _ -> Failures / Total
    end.

%% @private Add call to rolling history window
-spec add_to_history([{integer(), success | failure}], integer(), success | failure, config()) ->
                        [{integer(), success | failure}].
add_to_history(History, Timestamp, Result, Config) ->
    WindowSize = maps:get(window_size, Config),
    NewHistory = [{Timestamp, Result} | History],
    lists:sublist(NewHistory, WindowSize).

%% @private Reset data to initial state
-spec reset_data(data()) -> data().
reset_data(Data) ->
    Data#data{
        consecutive_failures = 0,
        consecutive_successes = 0,
        call_history = [],
        last_failure_time = undefined,
        last_state_change = erlang:monotonic_time(millisecond)
    }.

%% @private Build statistics map
-spec build_stats(state_name(), data()) -> map().
build_stats(State, Data) ->
    FailureRate = calculate_failure_rate(Data#data.call_history),
    SuccessRate = case Data#data.total_calls of
        0 -> 0.0;
        Total -> Data#data.total_successes / Total
    end,

    #{
        name => Data#data.name,
        state => State,
        consecutive_failures => Data#data.consecutive_failures,
        consecutive_successes => Data#data.consecutive_successes,
        total_calls => Data#data.total_calls,
        total_successes => Data#data.total_successes,
        total_failures => Data#data.total_failures,
        total_rejected => Data#data.total_rejected,
        failure_rate => FailureRate,
        success_rate => SuccessRate,
        last_failure_time => Data#data.last_failure_time,
        last_state_change => Data#data.last_state_change,
        config => Data#data.config
    }.

%% @private Ensure manager table exists
-spec ensure_manager_table() -> ok.
ensure_manager_table() ->
    case ets:whereis(?MANAGER_TABLE) of
        undefined ->
            ets:new(?MANAGER_TABLE, [named_table, public, set]),
            ok;
        _ ->
            ok
    end.
