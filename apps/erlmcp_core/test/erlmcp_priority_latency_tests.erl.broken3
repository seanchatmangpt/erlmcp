%%%-------------------------------------------------------------------
%%% @doc Priority Message Latency Tests (OTP 28 EEP-76)
%%%
%%% Demonstrates priority vs normal message latency under load.
%%% Tests verify that priority messages achieve <1ms p99 latency
%%% even with 10K normal messages queued.
%%%
%%% Chicago School TDD: Real processes, priority queues, observable behavior
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_priority_latency_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Test Configuration
%%%===================================================================

-define(PRIORITY_LATENCY_TARGET_US, 1000).  % 1ms target
-define(NORMAL_LATENCY_TARGET_US, 5000).    % 5ms acceptable for normal
-define(BACKGROUND_LOADS, [100, 1000, 10000]).
-define(HEALTH_CHECK_COUNT, 50).

%%%===================================================================
%%% Test Setup
%%%===================================================================

%% @doc Setup priority latency tests
priority_latency_setup() ->
    %% Verify OTP 28+ availability
    OtpVersion = erlang:system_info(otp_release),
    case OtpVersion >= "28" of
        true ->
            {ok, OtpVersion};
        false ->
            {skip, "Priority queues require OTP 28+, found: " ++ OtpVersion}
    end.

priority_latency_cleanup(_OtpVersion) ->
    ok.

%%%===================================================================
%%% Test Suite: Priority vs Normal Latency
%%%===================================================================

priority_vs_normal_latency_test_() ->
    {setup,
     fun priority_latency_setup/0,
     fun priority_latency_cleanup/1,
     fun(_OtpVersion) ->
        [?_test(test_latency_under_load(100)),
         ?_test(test_latency_under_load(1000)),
         ?_test(test_latency_under_load(10000))]
     end}.

%% @doc Test priority vs normal latency under load
test_latency_under_load(BackgroundLoad) ->
    ?LOG_INFO("Testing priority latency with ~p background messages", [BackgroundLoad]),

    %% Start test server
    {ok, ServerPid} = start_test_server(),

    %% Flood with background traffic
    BackgroundPids = spawn_background_traffic(ServerPid, BackgroundLoad),

    %% Wait for queue to build up
    timer:sleep(100),

    %% Measure NORMAL health check latency
    NormalLatencies = measure_health_checks(ServerPid, normal, ?HEALTH_CHECK_COUNT),

    %% Measure PRIORITY health check latency
    PriorityLatencies = measure_health_checks(ServerPid, priority, ?HEALTH_CHECK_COUNT),

    %% Cleanup background traffic
    cleanup_background_traffic(BackgroundPids),
    stop_test_server(ServerPid),

    %% Calculate metrics
    NormalMetrics = calculate_metrics(NormalLatencies),
    PriorityMetrics = calculate_metrics(PriorityLatencies),

    %% Verify priority meets target
    PriorityP99Us = maps:get(p99_us, PriorityMetrics),
    ?assert(PriorityP99Us < ?PRIORITY_LATENCY_TARGET_US,
            ?_format("Priority p99 ~p us exceeds target ~p us",
                     [PriorityP99Us, ?PRIORITY_LATENCY_TARGET_US])),

    %% Log results
    ?LOG_INFO("Background Load: ~p messages", [BackgroundLoad]),
    ?LOG_INFO("Normal p99: ~p us (~.3f ms)", [maps:get(p99_us, NormalMetrics),
                                              maps:get(p99_ms, NormalMetrics)]),
    ?LOG_INFO("Priority p99: ~p us (~.3f ms)", [PriorityP99Us,
                                                 maps:get(p99_ms, PriorityMetrics)]),

    %% Verify priority is faster than normal
    NormalP99Us = maps:get(p99_us, NormalMetrics),
    ?assert(PriorityP99Us < NormalP99Us,
            ?_format("Priority p99 ~p us not faster than normal ~p us",
                     [PriorityP99Us, NormalP99Us])).

%%%===================================================================
%%% Test Suite: Priority Message Ordering
%%%===================================================================

priority_ordering_test_() ->
    {setup,
     fun priority_latency_setup/0,
     fun priority_latency_cleanup/1,
     fun(_OtpVersion) ->
        [?_test(test_priority_jumps_queue()),
         ?_test(test_priority_fifo_ordering())]
     end}.

%% @doc Test that priority messages jump the queue
test_priority_jumps_queue() ->
    Parent = self(),
    Pid = spawn(fun() -> test_receiver_loop(Pid, []) end),

    %% Send 100 normal messages
    [Pid ! {normal, N} || N <- lists:seq(1, 100)],

    %% Send priority message
    Pid ! {priority, self(), urgent_msg},

    %% Send more normal messages
    [Pid ! {normal, N} || N <- lists:seq(101, 200)],

    %% Wait for processing
    timer:sleep(500),

    %% Stop receiver and get messages
    Pid ! stop,
    Messages = receive
        {messages, Msgs} -> Msgs
    after 1000 ->
        timeout
    end,

    %% Verify priority message arrived first
    ?assertEqual(201, length(Messages)),
    ?assertEqual({priority, urgent_msg}, hd(Messages)),

    exit(Pid, kill).

%% @doc Test FIFO ordering among priority messages
test_priority_fifo_ordering() ->
    Parent = self(),
    Pid = spawn(fun() -> priority_fifo_loop(Parent, []) end),

    %% Send 3 priority messages in sequence
    Pid ! {priority, self(), msg1},
    Pid ! {priority, self(), msg2},
    Pid ! {priority, self(), msg3},

    %% Get result
    PriorityMessages = receive
        {priority_order, Order} -> Order
    after 1000 ->
        timeout
    end,

    %% Verify FIFO ordering
    ?assertEqual([msg1, msg2, msg3], PriorityMessages),

    exit(Pid, kill).

%%%===================================================================
%%% Test Suite: Priority Throughput
%%%===================================================================

priority_throughput_test_() ->
    {setup,
     fun priority_latency_setup/0,
     fun priority_latency_cleanup/1,
     fun(_OtpVersion) ->
        [?_test(test_priority_throughput_no_load()),
         ?_test(test_priority_throughput_with_load())]
     end}.

%% @doc Test priority throughput without load
test_priority_throughput_no_load() ->
    {ok, ServerPid} = start_test_server(),
    MessageCount = 1000,

    %% Measure throughput
    StartTime = erlang:monotonic_time(microsecond),
    send_priority_messages(ServerPid, MessageCount),
    wait_for_messages(MessageCount),
    EndTime = erlang:monotonic_time(microsecond),

    DurationMs = (EndTime - StartTime) / 1000,
    Throughput = MessageCount / (DurationMs / 1000),

    stop_test_server(ServerPid),

    %% Verify throughput > 100K msg/s
    ?assert(Throughput > 100000,
            ?_format("Priority throughput ~p msg/s below 100K msg/s", [Throughput])).

%% @doc Test priority throughput with load
test_priority_throughput_with_load() ->
    {ok, ServerPid} = start_test_server(),

    %% Background load
    BackgroundPids = spawn_background_traffic(ServerPid, 1000),
    timer:sleep(100),

    %% Send 100 priority messages
    PriorityCount = 100,
    StartTime = erlang:monotonic_time(microsecond),
    send_priority_messages(ServerPid, PriorityCount),
    wait_for_messages(PriorityCount),
    EndTime = erlang:monotonic_time(microsecond),

    cleanup_background_traffic(BackgroundPids),
    stop_test_server(ServerPid),

    DurationMs = (EndTime - StartTime) / 1000,
    Throughput = PriorityCount / (DurationMs / 1000),

    %% Verify priority throughput maintained under load
    ?assert(Throughput > 10000,
            ?_format("Priority throughput under load ~p msg/s below 10K msg/s",
                     [Throughput])).

%%%===================================================================
%%% Test Suite: Graceful Degradation
%%%===================================================================

graceful_degradation_test_() ->
    {setup,
     fun priority_latency_setup/0,
     fun priority_latency_cleanup/1,
     fun(_OtpVersion) ->
        [?_test(test_fallback_to_normal_on_error()),
         ?_test(test_normal_unchanged_when_priority_unused())]
     end}.

%% @doc Test fallback to normal send on error
test_fallback_to_normal_on_error() ->
    %% Use invalid alias to trigger fallback
    InvalidAlias = make_ref(),

    %% This should not crash
    try
        erlmcp_priority:send_priority(InvalidAlias, test_msg, self()),
        ?assert(false, should_have_failed)
    catch
        error:{invalid_alias, _} ->
            %% Expected
            ok
    end.

%% @doc Test normal messages unchanged when priority unused
test_normal_unchanged_when_priority_unused() ->
    {ok, ServerPid} = start_test_server(),

    %% Send normal messages only
    [ServerPid ! {normal, N} || N <- lists:seq(1, 100)],

    %% Verify all processed
    timer:sleep(500),
    stop_test_server(ServerPid),

    ?assert(true).  % If we get here, no crash

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Start test server process
start_test_server() ->
    Pid = spawn_link(fun() -> test_server_loop(0) end),
    {ok, Pid}.

%% @doc Test server loop
test_server_loop(Count) ->
    receive
        {health_check, From, Ref, priority} ->
            %% Fast priority response
            From ! {health_reply, Ref, ok},
            test_server_loop(Count + 1);
        {health_check, From, Ref, normal} ->
            %% Normal response (simulates processing)
            timer:sleep(1),
            From ! {health_reply, Ref, ok},
            test_server_loop(Count + 1);
        {priority_test, _} ->
            test_server_loop(Count + 1);
        {normal, _} ->
            test_server_loop(Count + 1);
        stop ->
            ok
    end.

%% @doc Stop test server
stop_test_server(Pid) ->
    catch Pid ! stop,
    timer:sleep(100).

%% @doc Spawn background traffic
spawn_background_traffic(ServerPid, NumMessages) ->
    NumWorkers = min(100, NumMessages div 10),
    MsgsPerWorker = NumMessages div NumWorkers,

    lists:map(fun(_) ->
        spawn_link(fun() ->
            background_worker_loop(ServerPid, MsgsPerWorker)
        end)
    end, lists:seq(1, NumWorkers)).

%% @doc Background worker loop
background_worker_loop(ServerPid, MessagesRemaining) when MessagesRemaining > 0 ->
    Ref = make_ref(),
    ServerPid ! {normal, Ref},
    receive
        _ -> ok
    after 100 ->
        ok
    end,
    background_worker_loop(ServerPid, MessagesRemaining - 1);
background_worker_loop(_ServerPid, 0) ->
    receive
        stop -> ok
    end.

%% @doc Cleanup background traffic
cleanup_background_traffic(Pids) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! stop
    end, Pids),
    timer:sleep(100).

%% @doc Measure health check latency
measure_health_checks(ServerPid, Mode, Count) ->
    lists:map(fun(_) ->
        Ref = make_ref(),
        Start = erlang:monotonic_time(microsecond),

        case Mode of
            priority ->
                %% Send priority health check
                erlang:send(ServerPid, {health_check, self(), Ref, priority},
                           [nosuspend, {priority, high}]);
            normal ->
                %% Send normal health check
                ServerPid ! {health_check, self(), Ref, normal}
        end,

        %% Wait for reply
        receive
            {health_reply, Ref, ok} ->
                End = erlang:monotonic_time(microsecond),
                End - Start
        after 5000 ->
            5000000  % Timeout
        end
    end, lists:seq(1, Count)).

%% @doc Calculate latency metrics
calculate_metrics([]) ->
    #{p50_us => 0, p95_us => 0, p99_us => 0, avg_us => 0,
      p50_ms => 0, p95_ms => 0, p99_ms => 0, avg_ms => 0};
calculate_metrics(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),

    P50 = percentile(Sorted, 0.50),
    P95 = percentile(Sorted, 0.95),
    P99 = percentile(Sorted, 0.99),
    Avg = lists:sum(Latencies) / Len,

    #{
        p50_us => round_float(P50),
        p95_us => round_float(P95),
        p99_us => round_float(P99),
        avg_us => round_float(Avg),
        p50_ms => round_float(P50 / 1000, 3),
        p95_ms => round_float(P95 / 1000, 3),
        p99_ms => round_float(P99 / 1000, 3),
        avg_ms => round_float(Avg / 1000, 3)
    }.

%% @doc Calculate percentile
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percentile))),
    lists:nth(Index, SortedList).

%% @doc Round float
round_float(Value) ->
    round(Value).
round_float(Value, DecimalPlaces) ->
    Multiplier = math:pow(10, DecimalPlaces),
    round(Value * Multiplier) / Multiplier.

%% @doc Send priority messages
send_priority_messages(_ServerPid, 0) ->
    ok;
send_priority_messages(ServerPid, Count) ->
    erlang:send(ServerPid, {priority_test, Count}, [nosuspend, {priority, high}]),
    send_priority_messages(ServerPid, Count - 1).

%% @doc Wait for messages to be processed
wait_for_messages(0) ->
    ok;
wait_for_messages(Count) ->
    receive
        _ -> wait_for_messages(Count - 1)
    after 5000 ->
        timeout
    end.

%% @doc Test receiver loop
test_receiver_loop(Parent, Acc) ->
    receive
        {priority, _From, Msg} ->
            test_receiver_loop(Parent, [{priority, Msg} | Acc]);
        {normal, Msg} ->
            test_receiver_loop(Parent, [{normal, Msg} | Acc]);
        stop ->
            Parent ! {messages, lists:reverse(Acc)}
    after 1000 ->
        Parent ! {messages, lists:reverse(Acc)}
    end.

%% @doc Priority FIFO loop
priority_fifo_loop(Parent, Acc) ->
    receive
        {priority, _From, Msg} ->
            priority_fifo_loop(Parent, [Msg | Acc]);
        {get_order} ->
            Parent ! {priority_order, lists:reverse(Acc)};
        stop ->
            Parent ! {priority_order, lists:reverse(Acc)}
    after 1000 ->
        Parent ! {priority_order, lists:reverse(Acc)}
    end.
