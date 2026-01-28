%%%-------------------------------------------------------------------
%% @doc Global Circuit Breaker for System Overload Protection
%%
%% Monitors system health and automatically opens circuit when 
%% system approaches saturation.
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_circuit_breaker).

-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    stop/0,
    get_status/0,
    is_open/0,
    can_execute/0,
    record_request/2,
    record_error/1,
    record_success/0,
    record_retry_attempt/0,
    get_metrics/0,
    reset/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type circuit_state() :: closed | open | half_open.
-type retry_action() :: allow | deny.
-type metrics() :: #{
    p95_latency_ms => non_neg_integer(),
    error_rate_percent => float(),
    cpu_usage_percent => float(),
    memory_usage_percent => float(),
    total_requests => non_neg_integer(),
    total_errors => non_neg_integer(),
    requests_in_window => non_neg_integer(),
    errors_in_window => non_neg_integer(),
    retry_attempts_blocked => non_neg_integer(),
    state_transitions => non_neg_integer(),
    last_state_change => integer() | undefined,
    last_update => integer()
}.

%% Server state
-record(state, {
    config :: #{atom() => any()},
    circuit_state :: circuit_state(),
    circuit_open_time :: integer() | undefined,
    circuit_half_open_time :: integer() | undefined,
    last_state_change :: integer() | undefined,
    metrics :: ets:table(),
    request_window :: queue:queue(),
    error_window :: queue:queue(),
    retry_window :: queue:queue(),
    success_count :: non_neg_integer(),
    failure_count :: non_neg_integer(),
    recovery_timer :: reference() | undefined,
    metrics_timer :: reference() | undefined,
    window_size = 1000 :: pos_integer(),
    state_transition_log :: [term()]
}).

-define(ETS_METRICS, circuit_breaker_metrics).
-define(METRICS_UPDATE_INTERVAL, 5000).
-define(REQUEST_WINDOW_SIZE, 1000).
-define(ERROR_WINDOW_SIZE, 1000).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec get_status() -> {ok, circuit_state()}.
get_status() ->
    gen_server:call(?MODULE, {get_status}, 5000).

-spec is_open() -> boolean().
is_open() ->
    gen_server:call(?MODULE, {is_open}, 5000).

-spec can_execute() -> {true | false, retry_action()}.
can_execute() ->
    gen_server:call(?MODULE, {can_execute}, 5000).

-spec record_request(pos_integer(), non_neg_integer()) -> ok.
record_request(RequestId, LatencyMs) ->
    gen_server:cast(?MODULE, {record_request, RequestId, LatencyMs}).

-spec record_error(pos_integer()) -> ok.
record_error(RequestId) ->
    gen_server:cast(?MODULE, {record_error, RequestId}).

-spec record_success() -> ok.
record_success() ->
    gen_server:cast(?MODULE, {record_success}).

-spec record_retry_attempt() -> ok.
record_retry_attempt() ->
    gen_server:cast(?MODULE, {record_retry_attempt}).

-spec get_metrics() -> {ok, metrics()}.
get_metrics() ->
    gen_server:call(?MODULE, {get_metrics}, 5000).

-spec reset() -> ok.
reset() ->
    gen_server:call(?MODULE, {reset}, 5000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    Config = load_config(),
    Metrics = ets:new(?ETS_METRICS, [set, public]),
    init_metrics(Metrics),
    RequestWindow = queue:new(),
    ErrorWindow = queue:new(),
    RetryWindow = queue:new(),
    MetricsTimer = erlang:send_after(?METRICS_UPDATE_INTERVAL, self(), update_metrics),

    logger:info("Circuit breaker started with config: ~p", [Config]),

    State = #state{
        config = Config,
        circuit_state = closed,
        circuit_open_time = undefined,
        circuit_half_open_time = undefined,
        last_state_change = erlang:system_time(millisecond),
        metrics = Metrics,
        request_window = RequestWindow,
        error_window = ErrorWindow,
        retry_window = RetryWindow,
        success_count = 0,
        failure_count = 0,
        recovery_timer = undefined,
        metrics_timer = MetricsTimer,
        window_size = ?REQUEST_WINDOW_SIZE,
        state_transition_log = []
    },
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.

handle_call({get_status}, _From, State) ->
    {reply, {ok, State#state.circuit_state}, State};

handle_call({is_open}, _From, State) ->
    IsOpen = State#state.circuit_state =:= open,
    {reply, IsOpen, State};

handle_call({can_execute}, _From, State) ->
    case State#state.circuit_state of
        closed ->
            {reply, {true, allow}, State};
        half_open ->
            {reply, {true, allow}, State};
        open ->
            {reply, {false, deny}, State}
    end;

handle_call({get_metrics}, _From, State) ->
    Metrics = get_metrics_internal(State),
    {reply, {ok, Metrics}, State};

handle_call({reset}, _From, State) ->
    NewState = reset_internal(State),
    logger:info("Circuit breaker reset", []),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast({record_request, _RequestId, LatencyMs}, State) ->
    TimeNowMs = erlang:system_time(millisecond),
    NewRequestWindow = add_to_window(State#state.request_window, {TimeNowMs, LatencyMs}, State#state.window_size),
    {noreply, State#state{request_window = NewRequestWindow}};

handle_cast({record_error, _RequestId}, State) ->
    TimeNowMs = erlang:system_time(millisecond),
    NewErrorWindow = add_to_window(State#state.error_window, TimeNowMs, State#state.window_size),
    NewFailureCount = State#state.failure_count + 1,
    NewState = State#state{
        error_window = NewErrorWindow,
        failure_count = NewFailureCount
    },
    {noreply, NewState};

handle_cast({record_success}, State) ->
    NewSuccessCount = State#state.success_count + 1,
    {noreply, State#state{success_count = NewSuccessCount}};

handle_cast({record_retry_attempt}, State) ->
    TimeNowMs = erlang:system_time(millisecond),
    NewRetryWindow = add_to_window(State#state.retry_window, TimeNowMs, State#state.window_size),
    Metrics = State#state.metrics,
    [{retry_attempts_blocked, CurrentBlocked}] = ets:lookup(Metrics, retry_attempts_blocked),
    ets:insert(Metrics, {retry_attempts_blocked, CurrentBlocked + 1}),
    {noreply, State#state{retry_window = NewRetryWindow}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(update_metrics, State) ->
    NewState = evaluate_circuit(State),
    MetricsTimer = erlang:send_after(?METRICS_UPDATE_INTERVAL, self(), update_metrics),
    {noreply, NewState#state{metrics_timer = MetricsTimer}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.metrics_timer of
        undefined -> ok;
        Ref1 -> erlang:cancel_timer(Ref1)
    end,
    case State#state.recovery_timer of
        undefined -> ok;
        Ref2 -> erlang:cancel_timer(Ref2)
    end,
    ets:delete(State#state.metrics),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec load_config() -> #{atom() => any()}.
load_config() ->
    DefaultConfig = #{
        p95_latency_threshold_ms => 200,
        error_rate_threshold_percent => 1.0,
        cpu_threshold_percent => 90,
        memory_threshold_percent => 85,
        recovery_timeout_ms => 60000,
        half_open_timeout_ms => 30000,
        failure_threshold => 5,
        cool_down_time_ms => 30000,
        enabled => true
    },

    case application:get_env(erlmcp, circuit_breaker) of
        {ok, Config} when is_map(Config) ->
            maps:merge(DefaultConfig, Config);
        {ok, Config} when is_list(Config) ->
            maps:merge(DefaultConfig, maps:from_list(Config));
        _ ->
            DefaultConfig
    end.

-spec init_metrics(ets:table()) -> ok.
init_metrics(Metrics) ->
    TimeNowMs = erlang:system_time(millisecond),
    ets:insert(Metrics, {p95_latency_ms, 0}),
    ets:insert(Metrics, {error_rate_percent, 0.0}),
    ets:insert(Metrics, {cpu_usage_percent, 0.0}),
    ets:insert(Metrics, {memory_usage_percent, 0.0}),
    ets:insert(Metrics, {total_requests, 0}),
    ets:insert(Metrics, {total_errors, 0}),
    ets:insert(Metrics, {requests_in_window, 0}),
    ets:insert(Metrics, {errors_in_window, 0}),
    ets:insert(Metrics, {retry_attempts_blocked, 0}),
    ets:insert(Metrics, {state_transitions, 0}),
    ets:insert(Metrics, {last_state_change, TimeNowMs}),
    ets:insert(Metrics, {last_update, TimeNowMs}),
    ok.

-spec add_to_window(queue:queue(), any(), pos_integer()) -> queue:queue().
add_to_window(Window, Item, MaxSize) ->
    NewWindow = queue:in(Item, Window),
    case queue:len(NewWindow) > MaxSize of
        true -> {_, Q} = queue:out(NewWindow), Q;
        false -> NewWindow
    end.

-spec calculate_p95_latency(queue:queue()) -> non_neg_integer().
calculate_p95_latency(RequestWindow) ->
    case queue:to_list(RequestWindow) of
        [] ->
            0;
        Requests ->
            Latencies = [Latency || {_Time, Latency} <- Requests],
            Sorted = lists:sort(Latencies),
            Index = max(1, round(length(Sorted) * 0.95)),
            lists:nth(Index, Sorted)
    end.

-spec calculate_error_rate(pos_integer(), pos_integer()) -> float().
calculate_error_rate(0, 0) ->
    0.0;
calculate_error_rate(Errors, Requests) when Requests > 0 ->
    (Errors * 100.0) / Requests;
calculate_error_rate(_, _) ->
    100.0.

-spec evaluate_circuit(#state{}) -> #state{}.
evaluate_circuit(State) ->
    Config = State#state.config,
    Enabled = maps:get(enabled, Config, true),

    case Enabled of
        false ->
            State;
        true ->
            P95Latency = calculate_p95_latency(State#state.request_window),
            ErrorCount = queue:len(State#state.error_window),
            RequestCount = queue:len(State#state.request_window),
            ErrorRate = calculate_error_rate(ErrorCount, RequestCount),
            FailureThreshold = maps:get(failure_threshold, Config, 5),
            CoolDownTimeMs = maps:get(cool_down_time_ms, Config, 30000),

            Metrics = State#state.metrics,
            ets:insert(Metrics, {p95_latency_ms, P95Latency}),
            ets:insert(Metrics, {error_rate_percent, ErrorRate}),
            ets:insert(Metrics, {requests_in_window, RequestCount}),
            ets:insert(Metrics, {errors_in_window, ErrorCount}),
            ets:insert(Metrics, {last_update, erlang:system_time(millisecond)}),

            P95Threshold = maps:get(p95_latency_threshold_ms, Config, 200),
            ErrorThreshold = maps:get(error_rate_threshold_percent, Config, 1.0),

            ThresholdExceeded = P95Latency > P95Threshold orelse
                               ErrorRate > ErrorThreshold,

            case State#state.circuit_state of
                closed ->
                    % Check if failure threshold exceeded
                    case State#state.failure_count >= FailureThreshold orelse ThresholdExceeded of
                        true ->
                            emit_event(open, State),
                            logger:error("Circuit breaker opened! Failures: ~p/~p, ErrorRate: ~f%",
                                        [State#state.failure_count, FailureThreshold, ErrorRate]),
                            TimeNowMs = erlang:system_time(millisecond),
                            NewState = State#state{
                                circuit_state = open,
                                circuit_open_time = TimeNowMs,
                                last_state_change = TimeNowMs
                            },
                            log_state_transition(NewState, open);
                        false ->
                            State
                    end;

                open ->
                    % Check if cool-down time has elapsed
                    TimeNowMs = erlang:system_time(millisecond),
                    TimeSinceOpen = TimeNowMs - State#state.circuit_open_time,
                    case TimeSinceOpen >= CoolDownTimeMs of
                        true ->
                            emit_event(half_open, State),
                            logger:info("Circuit breaker transitioning to half_open (cool-down elapsed)", []),
                            NewState = State#state{
                                circuit_state = half_open,
                                circuit_half_open_time = TimeNowMs,
                                last_state_change = TimeNowMs,
                                success_count = 0,
                                failure_count = 0
                            },
                            log_state_transition(NewState, half_open);
                        false ->
                            State
                    end;

                half_open ->
                    % In half_open, succeed or fail based on operations
                    SuccessThreshold = 2,  % Need 2 successes to close
                    case State#state.success_count >= SuccessThreshold of
                        true ->
                            emit_event(closed, State),
                            logger:info("Circuit breaker closed after recovery", []),
                            TimeNowMs = erlang:system_time(millisecond),
                            NewState = State#state{
                                circuit_state = closed,
                                circuit_open_time = undefined,
                                circuit_half_open_time = undefined,
                                last_state_change = TimeNowMs,
                                success_count = 0,
                                failure_count = 0
                            },
                            log_state_transition(NewState, closed);
                        false ->
                            % Check if failed during half_open
                            case State#state.failure_count > 0 of
                                true ->
                                    emit_event(open, State),
                                    logger:error("Circuit breaker re-opened due to failure in half_open", []),
                                    TimeNowMs = erlang:system_time(millisecond),
                                    NewState = State#state{
                                        circuit_state = open,
                                        circuit_open_time = TimeNowMs,
                                        last_state_change = TimeNowMs,
                                        success_count = 0,
                                        failure_count = 0
                                    },
                                    log_state_transition(NewState, open);
                                false ->
                                    State
                            end
                    end
            end
    end.

-spec emit_event(circuit_state(), #state{}) -> ok.
emit_event(NewState, State) ->
    TimeNowMs = erlang:system_time(millisecond),
    Event = #{
        type => circuit_state_change,
        new_state => NewState,
        previous_state => State#state.circuit_state,
        timestamp => TimeNowMs,
        failure_count => State#state.failure_count,
        success_count => State#state.success_count
    },
    logger:notice("Circuit breaker state change: ~p", [Event]).

-spec log_state_transition(#state{}, circuit_state()) -> #state{}.
log_state_transition(State, NewState) ->
    TransitionLog = State#state.state_transition_log,
    TimeNowMs = erlang:system_time(millisecond),
    Entry = {TimeNowMs, NewState},
    NewLog = [Entry | TransitionLog],
    Metrics = State#state.metrics,
    [{state_transitions, Count}] = ets:lookup(Metrics, state_transitions),
    ets:insert(Metrics, {state_transitions, Count + 1}),
    State#state{state_transition_log = NewLog}.

-spec get_metrics_internal(#state{}) -> metrics().
get_metrics_internal(State) ->
    Metrics = State#state.metrics,
    [{p95_latency_ms, P95Latency}] = ets:lookup(Metrics, p95_latency_ms),
    [{error_rate_percent, ErrorRate}] = ets:lookup(Metrics, error_rate_percent),
    [{cpu_usage_percent, CpuUsage}] = ets:lookup(Metrics, cpu_usage_percent),
    [{memory_usage_percent, MemUsage}] = ets:lookup(Metrics, memory_usage_percent),
    [{total_requests, TotalRequests}] = ets:lookup(Metrics, total_requests),
    [{total_errors, TotalErrors}] = ets:lookup(Metrics, total_errors),
    [{requests_in_window, ReqsInWindow}] = ets:lookup(Metrics, requests_in_window),
    [{errors_in_window, ErrsInWindow}] = ets:lookup(Metrics, errors_in_window),
    [{retry_attempts_blocked, RetryBlocked}] = ets:lookup(Metrics, retry_attempts_blocked),
    [{state_transitions, StateTransitions}] = ets:lookup(Metrics, state_transitions),
    [{last_state_change, LastStateChange}] = ets:lookup(Metrics, last_state_change),
    [{last_update, LastUpdate}] = ets:lookup(Metrics, last_update),

    #{
        p95_latency_ms => P95Latency,
        error_rate_percent => ErrorRate,
        cpu_usage_percent => CpuUsage,
        memory_usage_percent => MemUsage,
        total_requests => TotalRequests,
        total_errors => TotalErrors,
        requests_in_window => ReqsInWindow,
        errors_in_window => ErrsInWindow,
        retry_attempts_blocked => RetryBlocked,
        state_transitions => StateTransitions,
        last_state_change => LastStateChange,
        last_update => LastUpdate
    }.

-spec reset_internal(#state{}) -> #state{}.
reset_internal(State) ->
    case State#state.recovery_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    TimeNowMs = erlang:system_time(millisecond),
    Metrics = State#state.metrics,
    init_metrics(Metrics),
    State#state{
        circuit_state = closed,
        circuit_open_time = undefined,
        circuit_half_open_time = undefined,
        last_state_change = TimeNowMs,
        request_window = queue:new(),
        error_window = queue:new(),
        retry_window = queue:new(),
        success_count = 0,
        failure_count = 0,
        recovery_timer = undefined,
        state_transition_log = []
    }.
