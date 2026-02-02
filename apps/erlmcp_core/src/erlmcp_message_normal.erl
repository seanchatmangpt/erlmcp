%%%-------------------------------------------------------------------
%%% @doc
%%% Normal Message Processing Implementation
%%%
%%% This module provides normal message processing for OTP versions
%%% that don't support priority messages (OTP < 28).
%%%
%%% Features:
%%%   - Normal message sending without priority
%%%   - FIFO message ordering guarantee
%%%   - Performance monitoring for message handling
%%%   - Graceful degradation when priority messages are available
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_message_normal).

%% API
-export([
    enable_normal_mode/0,
    disable_normal_mode/0,
    is_normal_mode_enabled/0,
    send_message/2,
    send_message_with_timeout/3,
    send_message_with_priority/3,
    monitor_message_performance/0,
    get_message_metrics/0
]).

%% Types
-type message() :: term().
-type timeout() :: pos_integer() | infinity.
-type message_metrics() :: #{
    sent_count => non_neg_integer(),
    received_count => non_neg_integer(),
    average_latency => integer(),
    max_latency => integer(),
    error_count => non_neg_integer(),
    method => normal | priority
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Enable normal message processing mode
-spec enable_normal_mode() -> ok.
enable_normal_mode() ->
    application:set_env(erlmcp, message_priority, false),
    application:set_env(erlmcp, normal_message_mode, true),
    logger:info("Normal message processing mode enabled"),
    start_message_monitoring(),
    ok.

%% @doc Disable normal message processing mode
-spec disable_normal_mode() -> ok.
disable_normal_mode() ->
    case erlang:function_exported(erlang, send, 3) of
        true ->
            application:set_env(erlmcp, message_priority, true),
            application:set_env(erlmcp, normal_message_mode, false),
            logger:info("Normal message processing mode disabled, using priority messages");
        false ->
            logger:warning("Cannot disable normal mode - priority messages not available"),
            enable_normal_mode()
    end,
    ok.

%% @doc Check if normal mode is enabled
-spec is_normal_mode_enabled() -> boolean().
is_normal_mode_enabled() ->
    application:get_env(erlmcp, normal_message_mode, true).

%% @doc Send message with normal priority
-spec send_message(pid() | atom(), message()) -> ok.
send_message(Dest, Msg) ->
    StartTime = erlang:monotonic_time(microsecond),

    Result = erlang:send(Dest, Msg, [nosuspend]),

    EndTime = erlang:monotonic_time(microsecond),
    Latency = EndTime - StartTime,

    % Record performance metrics
    record_message_send(Latency, Result),

    case Result of
        ok -> ok;
        {error, _} -> {error, message_send_failed}
    end.

%% @doc Send message with timeout
-spec send_message_with_timeout(pid() | atom(), message(), timeout()) -> ok | {error, timeout}.
send_message_with_timeout(Dest, Msg, Timeout) ->
    StartTime = erlang:monotonic_time(microsecond),

    receive
        after Timeout ->
            {error, timeout}
    end,

    Result = erlang:send(Dest, Msg, [nosuspend, {timeout, Timeout}]),

    EndTime = erlang:monotonic_time(microsecond),
    Latency = EndTime - StartTime,

    record_message_send(Latency, Result),

    case Result of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Send message with priority (falls back to normal priority)
-spec send_message_with_priority(pid() | atom(), message(), term()) -> ok.
send_message_with_priority(Dest, Msg, _Priority) ->
    % For OTP < 28, priority is not supported, use normal priority
    send_message(Dest, Msg).

%% @doc Start message performance monitoring
-spec monitor_message_performance() -> ok.
monitor_message_performance() ->
    case is_normal_mode_enabled() of
        true ->
            start_message_monitoring();
        false ->
            logger:info("Message monitoring not needed in priority mode")
    end,
    ok.

%% @doc Get message handling metrics
-spec get_message_metrics() -> message_metrics().
get_message_metrics() ->
    % Get metrics from application environment or process registry
    SentCount = get_metric(sent_count, 0),
    ReceivedCount = get_metric(received_count, 0),
    AvgLatency = get_metric(average_latency, 0),
    MaxLatency = get_metric(max_latency, 0),
    ErrorCount = get_metric(error_count, 0),

    #{
        sent_count => SentCount,
        received_count => ReceivedCount,
        average_latency => AvgLatency,
        max_latency => MaxLatency,
        error_count => ErrorCount,
        method => get_current_method()
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Record message send metrics
-spec record_message_send(integer(), term()) -> ok.
record_message_send(Latency, Result) ->
    % Increment sent count
    OldSent = get_metric(sent_count, 0),
    set_metric(sent_count, OldSent + 1),

    % Update latency metrics
    OldAvg = get_metric(average_latency, 0),
    NewAvg = if OldAvg =:= 0 -> Latency;
               true -> (OldAvg + Latency) div 2
            end,
    set_metric(average_latency, NewAvg),

    % Update max latency
    OldMax = get_metric(max_latency, 0),
    if Latency > OldMax ->
        set_metric(max_latency, Latency);
    true ->
        ok
    end,

    % Update error count
    case Result of
        ok -> ok;
        {error, _} ->
            OldErrors = get_metric(error_count, 0),
            set_metric(error_count, OldErrors + 1)
    end.

%% @private Get metric value
-spec get_metric(atom(), integer()) -> integer().
get_metric(Metric, Default) ->
    case get({message_metrics, Metric}) of
        undefined -> Default;
        Value -> Value
    end.

%% @private Set metric value
-spec set_metric(atom(), integer()) -> ok.
set_metric(Metric, Value) ->
    put({message_metrics, Metric}, Value).

%% @private Start message monitoring
-spec start_message_monitoring() -> ok.
start_message_monitoring() ->
    case erlang:whereis(message_monitor) of
        undefined ->
            spawn_link(fun message_monitor_loop/0);
        _ ->
            ok
    end.

%% @private Message monitoring loop
-spec message_monitor_loop() -> no_return().
message_monitor_loop() ->
    register(message_monitor, self()),

    message_monitor_loop(0).

%% @private Message monitoring loop with iteration
-spec message_monitor_loop(non_neg_integer()) -> no_return().
message_monitor_loop(Iteration) ->
    % Check message metrics every 30 seconds
    timer:sleep(30000),

    Metrics = get_message_metrics(),

    % Log warnings for poor performance
    case Metrics#{
        average_latency := AvgLatency,
        error_count := ErrorCount,
        sent_count := SentCount
    } of
        #{average_latency := Lat} when Lat > 1000 ->
            logger:warning("High message latency detected: ~p microseconds", [Lat]);
        #{error_count := Errors} when Errors > 100 and SentCount > 1000 ->
            logger:warning("High message error rate: ~p errors out of ~p messages",
                          [Errors, SentCount]);
        _ ->
            ok
    end,

    % Continue monitoring
    message_monitor_loop(Iteration + 1).

%% @private Get current method name
-spec get_current_method() -> normal | priority.
get_current_method() ->
    case is_normal_mode_enabled() of
        true -> normal;
        false -> priority
    end.

%% @private Monitor message queue length
-spec monitor_message_queues() -> ok.
monitor_message_queues() ->
    % Check message queue lengths for critical processes
    CriticalProcesses = [self()],  % Add critical processes here

    lists:foreach(fun(Pid) ->
        QueueInfo = process_info(Pid, message_queue_len),
        case QueueInfo of
            {message_queue_len, Length} when Length > 1000 ->
                logger:warning("Large message queue detected for ~p: ~p messages",
                              [Pid, Length]);
            _ ->
                ok
        end
    end, CriticalProcesses).

%% @private Get performance statistics
-spec get_performance_statistics() -> map().
get_performance_statistics() ->
    Metrics = get_message_metrics(),

    % Calculate performance indicators
    SuccessRate = case Metrics#{
        sent_count := Sent,
        error_count := Errors
    } of
        #{error_count := 0} -> 100.0;
        #{sent_count := S, error_count := E} ->
            ((S - E) / S) * 100.0
    end,

    #{
        metrics => Metrics,
        success_rate => SuccessRate,
        timestamp => erlang:system_time(millisecond)
    }.

%% @private Log performance statistics
-spec log_performance_statistics() -> ok.
log_performance_statistics() ->
    Stats = get_performance_statistics(),

    logger:info("Message Performance Statistics: ~p", [Stats]).

%% @private Test message delivery
-spec test_message_delivery() -> boolean().
test_message_delivery() ->
    TestPid = spawn(fun() -> receive _ -> ok end end),

    % Send test message
    Result = send_message(TestPid, test),

    % Clean up
    exit(TestPid, normal),

    case Result of
        ok -> true;
        {error, _} -> false
    end.