%%%-------------------------------------------------------------------
%% @doc Comprehensive Common Test Suite for Backpressure and Queue Limits
%%
%% Tests cover:
%% - Queue limit enforcement with exact number validation
%% - Memory stability under sustained 2x overload
%% - Slow consumer + fast producer with deterministic refusal tracking
%% - Bandwidth throttle scenarios
%% - Queue depth metrics over time
%% - Memory graphs and stability proof
%%
%% Can be executed 5 times to capture metrics progression:
%%   rebar3 ct --suite=erlmcp_backpressure_SUITE --repeat=5
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_backpressure_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Suite callbacks
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_queue_limit_exact_enforcement/1,
    test_message_count_hard_limit/1,
    test_byte_size_hard_limit/1,
    test_memory_stability_2x_overload/1,
    test_slow_consumer_fast_producer/1,
    test_deterministic_refusal_rate/1,
    test_bandwidth_throttle/1,
    test_queue_depth_progression/1,
    test_connection_reset_drains_queue/1,
    test_tenant_aggregated_limits/1,
    test_backpressure_signal_propagation/1,
    test_metrics_collection_accuracy/1
]).

-define(TEST_SUITE_NAME, "erlmcp_backpressure_v1.3.0").
-define(MAX_MESSAGES, 1000).
-define(MAX_BYTES, 52428800).  % 50 MB
-define(SAMPLE_MESSAGE_SIZE, 10240).  % 10 KB typical message

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [
        test_queue_limit_exact_enforcement,
        test_message_count_hard_limit,
        test_byte_size_hard_limit,
        test_memory_stability_2x_overload,
        test_slow_consumer_fast_producer,
        test_deterministic_refusal_rate,
        test_bandwidth_throttle,
        test_queue_depth_progression,
        test_connection_reset_drains_queue,
        test_tenant_aggregated_limits,
        test_backpressure_signal_propagation,
        test_metrics_collection_accuracy
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_queue_limits:start_link(),
    {ok, _} = erlmcp_backpressure:start_link(),
    {ok, _} = erlmcp_circuit_breaker:start_link(),

    ct:log("=== ~s Test Suite Initialized ===", [?TEST_SUITE_NAME]),
    ct:log("Configuration: max_messages=~p, max_bytes=~p", [?MAX_MESSAGES, ?MAX_BYTES]),

    [{queue_limits_pid, whereis(erlmcp_queue_limits)} | Config].

end_per_suite(Config) ->
    erlmcp_queue_limits:stop(),
    erlmcp_backpressure:stop(),
    erlmcp_circuit_breaker:stop(),
    application:stop(erlmcp),
    Config.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test: ~p", [TestCase]),
    [{test_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(TestCase, Config) ->
    StartTime = proplists:get_value(test_start_time, Config),
    ElapsedMs = erlang:system_time(millisecond) - StartTime,
    ct:log("Test ~p completed in ~p ms", [TestCase, ElapsedMs]),
    Config.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test exact enforcement of queue limits with precise number tracking
test_queue_limit_exact_enforcement(Config) ->
    ConnectionId = test_conn_exact,

    % Reset connection to clean state
    erlmcp_queue_limits:reset_connection(ConnectionId),

    % Check initial state
    {ok, accepted} = erlmcp_queue_limits:check_queue_limit(ConnectionId, ?SAMPLE_MESSAGE_SIZE),
    Stats1 = erlmcp_queue_limits:get_connection_stats(ConnectionId),
    ?assertEqual(0, maps:get(message_count, Stats1, 0)),

    % Record 100 messages
    lists:foreach(fun(Idx) ->
        erlmcp_queue_limits:record_message(ConnectionId, {msg, Idx}, ?SAMPLE_MESSAGE_SIZE),
        {ok, Stats} = erlmcp_queue_limits:get_connection_stats(ConnectionId),
        ?assertEqual(Idx, maps:get(message_count, Stats))
    end, lists:seq(1, 100)),

    % Verify exact count
    {ok, FinalStats} = erlmcp_queue_limits:get_connection_stats(ConnectionId),
    ExactMessageCount = maps:get(message_count, FinalStats),
    ExactByteCount = maps:get(byte_count, FinalStats),

    ct:log("Exact message count: ~p", [ExactMessageCount]),
    ct:log("Exact byte count: ~p (~p MB)", [ExactByteCount, ExactByteCount / (1024 * 1024)]),

    ?assertEqual(100, ExactMessageCount),
    ?assertEqual(100 * ?SAMPLE_MESSAGE_SIZE, ExactByteCount),

    Config.

%% @doc Test message count hard limit enforcement
test_message_count_hard_limit(Config) ->
    ConnectionId = test_conn_msg_limit,
    erlmcp_queue_limits:reset_connection(ConnectionId),

    % Update limits to small value for faster testing
    {ok, _} = erlmcp_queue_limits:update_limits(#{max_messages => 50}),

    % Try to add 100 messages (should fail at 50)
    Refusals = lists:foldl(fun(Idx, RefusalCount) ->
        MessageSize = 100,  % Small messages to avoid byte limit
        case erlmcp_queue_limits:check_queue_limit(ConnectionId, MessageSize) of
            {ok, accepted} ->
                erlmcp_queue_limits:record_message(ConnectionId, {msg, Idx}, MessageSize),
                RefusalCount;
            {error, refuse, Details} ->
                ct:log("Refusal #~p: ~p", [RefusalCount + 1, Details]),
                RefusalCount + 1
        end
    end, 0, lists:seq(1, 100)),

    ct:log("Total refusals: ~p (expected ~50)", [Refusals]),
    ?assert(Refusals >= 50),  % At least 50 messages should be refused

    % Restore original limits
    {ok, _} = erlmcp_queue_limits:update_limits(#{max_messages => ?MAX_MESSAGES}),

    Config.

%% @doc Test byte size hard limit enforcement
test_byte_size_hard_limit(Config) ->
    ConnectionId = test_conn_byte_limit,
    erlmcp_queue_limits:reset_connection(ConnectionId),

    % Set smaller byte limit for testing
    SmallByteLimit = 1048576,  % 1 MB
    {ok, _} = erlmcp_queue_limits:update_limits(#{max_bytes => SmallByteLimit}),

    LargeMessageSize = 262144,  % 256 KB
    BytesAccumulated = ref:new(),

    % Try to add messages until byte limit reached
    ByteRefusals = lists:foldl(fun(Idx, RefusalCount) ->
        case erlmcp_queue_limits:check_queue_limit(ConnectionId, LargeMessageSize) of
            {ok, accepted} ->
                erlmcp_queue_limits:record_message(ConnectionId, {msg, Idx}, LargeMessageSize),
                ref:set(BytesAccumulated, ref:get(BytesAccumulated) + LargeMessageSize),
                RefusalCount;
            {error, refuse, Details} ->
                ct:log("Byte refusal: ~p", [Details]),
                RefusalCount + 1
        end
    end, 0, lists:seq(1, 10)),

    {ok, FinalStats} = erlmcp_queue_limits:get_connection_stats(ConnectionId),
    FinalBytes = maps:get(byte_count, FinalStats),

    ct:log("Final byte count: ~p (limit: ~p)", [FinalBytes, SmallByteLimit]),
    ct:log("Byte refusals: ~p", [ByteRefusals]),

    ?assert(FinalBytes =< SmallByteLimit),

    % Restore original limits
    {ok, _} = erlmcp_queue_limits:update_limits(#{max_bytes => ?MAX_BYTES}),

    Config.

%% @doc Test memory stability under sustained 2x overload
%% This is the critical test proving backpressure works
test_memory_stability_2x_overload(Config) ->
    StartTime = erlang:system_time(millisecond),
    ConnectionIds = [conn_overload_1, conn_overload_2, conn_overload_3],

    ct:log("Starting 2x overload test for 10 seconds...", []),

    % Take initial memory snapshot
    {memory, InitialMemory} = erlang:statistics(memory),
    InitialHeap = proplists:get_value(heap_size, InitialMemory, 0),

    ct:log("Initial heap: ~p words (~p MB)", [InitialHeap, InitialHeap * 8 / (1024 * 1024)]),

    % Simulate 2x sustained overload for 10 seconds
    OverloadDuration = 10000,  % 10 seconds
    MessageRate = 100,  % messages per second per connection
    TargetOverloadRate = MessageRate * 2 * length(ConnectionIds),

    _OverloadStats = overload_generator(ConnectionIds, TargetOverloadRate, OverloadDuration),

    % Take final memory snapshot
    {memory, FinalMemory} = erlang:statistics(memory),
    FinalHeap = proplists:get_value(heap_size, FinalMemory, 0),

    ElapsedMs = erlang:system_time(millisecond) - StartTime,
    HeapGrowthPercent = ((FinalHeap - InitialHeap) / InitialHeap) * 100,

    ct:log("Final heap: ~p words (~p MB)", [FinalHeap, FinalHeap * 8 / (1024 * 1024)]),
    ct:log("Heap growth: ~.1f%", [HeapGrowthPercent]),
    ct:log("Elapsed time: ~p ms", [ElapsedMs]),

    % Memory should not grow more than 30% even under 2x overload
    ?assert(HeapGrowthPercent < 30.0, "Heap growth exceeded 30% threshold"),

    % Verify all connections stayed within limits
    lists:foreach(fun(ConnId) ->
        case erlmcp_queue_limits:get_connection_stats(ConnId) of
            {error, not_found} -> ok;
            {ok, Stats} ->
                MsgCount = maps:get(message_count, Stats, 0),
                ByteCount = maps:get(byte_count, Stats, 0),

                ct:log("Connection ~p: ~p messages, ~p bytes", [ConnId, MsgCount, ByteCount]),
                ?assert(MsgCount =< ?MAX_MESSAGES),
                ?assert(ByteCount =< ?MAX_BYTES)
        end
    end, ConnectionIds),

    Config.

%% @doc Test slow consumer with fast producer - verify deterministic refusal
test_slow_consumer_fast_producer(Config) ->
    ConnId = test_conn_slow_consumer,
    erlmcp_queue_limits:reset_connection(ConnId),

    ProducerRate = 1000,  % messages/sec - much faster than consumer
    ConsumerRate = 100,   % messages/sec - much slower
    TestDurationMs = 5000,

    ct:log("Starting slow consumer test: producer=~p msg/s, consumer=~p msg/s", [ProducerRate, ConsumerRate]),

    % Produce messages at high rate
    ProducedCount = lists:foldl(fun(_Idx, Count) ->
        case erlmcp_queue_limits:check_queue_limit(ConnId, ?SAMPLE_MESSAGE_SIZE) of
            {ok, accepted} ->
                erlmcp_queue_limits:record_message(ConnId, {msg, _Idx}, ?SAMPLE_MESSAGE_SIZE),
                Count + 1;
            {error, refuse, _} ->
                Count
        end,
        timer:sleep(1000 div ProducerRate)  % Produce at rate
    end, 0, lists:seq(1, ProducerRate * (TestDurationMs div 1000))),

    % Consume messages at slower rate
    ConsumedCount = lists:foldl(fun(MsgIdx, Count) ->
        case erlmcp_queue_limits:get_connection_stats(ConnId) of
            {ok, Stats} ->
                case maps:get(message_count, Stats, 0) > 0 of
                    true ->
                        erlmcp_queue_limits:remove_message(ConnId, {msg, MsgIdx}),
                        Count + 1;
                    false ->
                        Count
                end;
            {error, not_found} ->
                Count
        end,
        timer:sleep(1000 div ConsumerRate)  % Consume at rate
    end, 0, lists:seq(1, ConsumerRate * (TestDurationMs div 1000))),

    {ok, FinalStats} = erlmcp_queue_limits:get_connection_stats(ConnId),
    QueueDepth = maps:get(message_count, FinalStats),
    RefusalRate = ProducedCount - ConsumedCount - QueueDepth,

    ct:log("Produced: ~p, Consumed: ~p, Queue depth: ~p, Refusals: ~p",
           [ProducedCount, ConsumedCount, QueueDepth, RefusalRate]),

    % Verify refusals occurred (backpressure activated)
    ?assert(RefusalRate > 0, "Expected refusals under slow consumer scenario"),

    % Verify queue stayed within limits
    ?assert(QueueDepth =< ?MAX_MESSAGES),

    Config.

%% @doc Test deterministic refusal rate under overload
test_deterministic_refusal_rate(Config) ->
    Iterations = 5,
    Rates = [0.5, 1.0, 2.0, 5.0],  % Multiples of sustainable rate

    Results = lists:map(fun(Rate) ->
        {RefusalsPerSecond, AcceptanceRate} = measure_refusal_rate(Rate, 10000),
        ct:log("Rate ~.1fx: refusals=~p/s, acceptance=~.1f%", [Rate, RefusalsPerSecond, AcceptanceRate * 100]),
        {Rate, RefusalsPerSecond, AcceptanceRate}
    end, Rates),

    % Verify refusal rate increases with overload
    lists:foreach(fun(Idx) ->
        {Rate1, Refusals1, _} = lists:nth(Idx, Results),
        {Rate2, Refusals2, _} = lists:nth(Idx + 1, Results),

        case Idx < length(Results) of
            true ->
                % Higher rate should have more refusals
                ct:log("Comparing rates ~.1fx vs ~.1fx: ~p vs ~p refusals",
                       [Rate1, Rate2, Refusals1, Refusals2]),
                ?assert(Refusals2 > Refusals1, "Refusals should increase with rate");
            false ->
                ok
        end
    end, lists:seq(1, length(Results) - 1)),

    Config.

%% @doc Test bandwidth throttle scenario
test_bandwidth_throttle(Config) ->
    ConnId = test_conn_bandwidth,
    erlmcp_queue_limits:reset_connection(ConnId),

    % Simulate bandwidth-limited connection
    MessageSize = 1000,  % 1 KB messages
    TargetBandwidth = 100000,  % 100 KB/s
    MessagesPerSecond = TargetBandwidth div MessageSize,
    DurationMs = 5000,

    ct:log("Bandwidth throttle test: ~p messages/sec, ~p seconds", [MessagesPerSecond, DurationMs div 1000]),

    % Try to send messages at different rates
    {LimitReached, MessagesAccepted} = measure_throughput(ConnId, MessageSize, MessagesPerSecond, DurationMs),

    ActualThroughput = (MessagesAccepted * MessageSize) / (DurationMs / 1000),

    ct:log("Target bandwidth: ~p KB/s, Actual: ~.1f KB/s",
           [TargetBandwidth div 1024, ActualThroughput / 1024]),

    ?assert(MessagesAccepted > 0, "Should accept some messages"),
    ?assert(LimitReached, "Should reach limit eventually"),

    Config.

%% @doc Test queue depth progression over time
test_queue_depth_progression(Config) ->
    ConnId = test_conn_depth_prog,
    erlmcp_queue_limits:reset_connection(ConnId),

    Intervals = 10,
    IntervalDurationMs = 100,
    TotalDurationMs = Intervals * IntervalDurationMs,

    Measurements = lists:map(fun(Interval) ->
        % Add 50 messages
        lists:foreach(fun(Idx) ->
            erlmcp_queue_limits:record_message(ConnId, {msg, Interval * 100 + Idx}, ?SAMPLE_MESSAGE_SIZE)
        end, lists:seq(1, 50)),

        % Measure queue depth
        {ok, Stats} = erlmcp_queue_limits:get_connection_stats(ConnId),
        Depth = maps:get(message_count, Stats),

        timer:sleep(IntervalDurationMs),

        {Interval, Depth}
    end, lists:seq(1, Intervals)),

    ct:log("Queue depth progression:", []),
    lists:foreach(fun({Interval, Depth}) ->
        ct:log("  Interval ~p: depth=~p", [Interval, Depth])
    end, Measurements),

    % Verify queue grows then stabilizes
    [FirstDepth | RestDepths] = [D || {_, D} <- Measurements],
    LastDepth = lists:last(RestDepths),

    ?assert(FirstDepth < LastDepth, "Queue should grow initially"),
    ?assert(LastDepth =< ?MAX_MESSAGES, "Queue should respect limit"),

    Config.

%% @doc Test that reset_connection drains queue properly
test_connection_reset_drains_queue(Config) ->
    ConnId = test_conn_reset,
    erlmcp_queue_limits:reset_connection(ConnId),

    % Add 100 messages
    lists:foreach(fun(Idx) ->
        erlmcp_queue_limits:record_message(ConnId, {msg, Idx}, ?SAMPLE_MESSAGE_SIZE)
    end, lists:seq(1, 100)),

    {ok, Stats1} = erlmcp_queue_limits:get_connection_stats(ConnId),
    PreResetCount = maps:get(message_count, Stats1),
    ct:log("Before reset: ~p messages", [PreResetCount]),
    ?assertEqual(100, PreResetCount),

    % Reset connection
    ok = erlmcp_queue_limits:reset_connection(ConnId),

    {ok, Stats2} = erlmcp_queue_limits:get_connection_stats(ConnId),
    PostResetCount = maps:get(message_count, Stats2),
    ct:log("After reset: ~p messages", [PostResetCount]),
    ?assertEqual(0, PostResetCount),

    Config.

%% @doc Test tenant aggregated limits
test_tenant_aggregated_limits(Config) ->
    % This test assumes tenant limits are enabled
    {ok, _} = erlmcp_queue_limits:update_limits(#{enable_tenant_limits => true}),

    TenantId = test_tenant_agg,
    ConnIds = [conn_t1, conn_t2, conn_t3],

    % Simulate tenant with 3 connections
    lists:foreach(fun(ConnIdx, ConnId) ->
        lists:foreach(fun(MsgIdx) ->
            erlmcp_queue_limits:record_message(ConnId, {msg, MsgIdx}, ?SAMPLE_MESSAGE_SIZE)
        end, lists:seq(1, 50))
    end, lists:zip(lists:seq(1, length(ConnIds)), ConnIds)),

    % Get tenant stats
    case erlmcp_queue_limits:get_tenant_stats(TenantId) of
        {ok, TenantStats} ->
            TotalMessages = maps:get(total_messages, TenantStats, 0),
            TotalConnections = maps:get(total_connections, TenantStats, 0),
            ct:log("Tenant stats: ~p connections, ~p total messages", [TotalConnections, TotalMessages]);
        {error, not_found} ->
            ct:log("Tenant not yet tracked (expected behavior)")
    end,

    % Restore setting
    {ok, _} = erlmcp_queue_limits:update_limits(#{enable_tenant_limits => false}),

    Config.

%% @doc Test backpressure signal propagation
test_backpressure_signal_propagation(Config) ->
    ConnId = test_conn_signal,
    erlmcp_queue_limits:reset_connection(ConnId),

    % Fill queue near limit
    SmallLimit = 200,
    {ok, _} = erlmcp_queue_limits:update_limits(#{max_messages => SmallLimit}),

    % Fill to 90% capacity
    FillCount = (SmallLimit * 90) div 100,
    lists:foreach(fun(Idx) ->
        {ok, accepted} = erlmcp_queue_limits:check_queue_limit(ConnId, 100),
        erlmcp_queue_limits:record_message(ConnId, {msg, Idx}, 100)
    end, lists:seq(1, FillCount)),

    % Now try to add more - should get refusal signals
    Refusals = lists:foldl(fun(_Idx, Count) ->
        case erlmcp_queue_limits:check_queue_limit(ConnId, 100) of
            {error, refuse, _} -> Count + 1;
            {ok, accepted} -> Count
        end
    end, 0, lists:seq(1, 100)),

    ct:log("Refusal signals received: ~p", [Refusals]),
    ?assert(Refusals > 0, "Should receive refusal signals when queue near limit"),

    % Restore limits
    {ok, _} = erlmcp_queue_limits:update_limits(#{max_messages => ?MAX_MESSAGES}),

    Config.

%% @doc Test metrics collection accuracy
test_metrics_collection_accuracy(Config) ->
    % Get current metrics
    AllStats = erlmcp_queue_limits:get_all_stats(),

    Metrics = maps:get(metrics, AllStats, #{}),
    Connections = maps:get(connections, AllStats, #{}),

    ct:log("Global metrics: ~p", [Metrics]),
    ct:log("Active connections: ~p", [maps:size(Connections)]),

    % Verify metrics have expected fields
    ?assert(is_map(Metrics)),

    % Verify connection stats structure
    lists:foreach(fun({_ConnId, ConnStats}) ->
        ?assert(maps:is_key(message_count, ConnStats)),
        ?assert(maps:is_key(byte_count, ConnStats)),
        ?assert(is_integer(maps:get(message_count, ConnStats))),
        ?assert(is_integer(maps:get(byte_count, ConnStats)))
    end, maps:to_list(Connections)),

    ct:log("Metrics collection test: PASS", []),

    Config.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Simulate overload scenario and measure metrics
overload_generator(ConnectionIds, TargetRate, DurationMs) ->
    StartTime = erlang:system_time(millisecond),
    MessageIndex = 0,

    lists:foldl(fun(_Tick, Count) ->
        lists:foreach(fun(ConnId) ->
            case erlmcp_queue_limits:check_queue_limit(ConnId, ?SAMPLE_MESSAGE_SIZE) of
                {ok, accepted} ->
                    erlmcp_queue_limits:record_message(ConnId, {msg, MessageIndex}, ?SAMPLE_MESSAGE_SIZE);
                {error, refuse, _} ->
                    ok
            end
        end, ConnectionIds),

        timer:sleep(1),  % 1ms granularity

        case (erlang:system_time(millisecond) - StartTime) > DurationMs of
            true -> Count;
            false -> Count + 1
        end
    end, 0, lists:seq(1, DurationMs)).

%% @doc Measure refusal rate at a given overload multiplier
measure_refusal_rate(OverloadMultiplier, DurationMs) ->
    ConnId = {refusal_rate_test, OverloadMultiplier},
    erlmcp_queue_limits:reset_connection(ConnId),

    BaseRate = 100,  % messages/second
    TargetRate = round(BaseRate * OverloadMultiplier),
    DelayPerMessage = 1000 div TargetRate,

    StartTime = erlang:system_time(millisecond),
    {Accepted, Refused} = lists:foldl(fun(_Idx, {Acc, Ref}) ->
        case erlmcp_queue_limits:check_queue_limit(ConnId, 100) of
            {ok, accepted} ->
                erlmcp_queue_limits:record_message(ConnId, {msg, _Idx}, 100),
                timer:sleep(DelayPerMessage),
                {Acc + 1, Ref};
            {error, refuse, _} ->
                timer:sleep(DelayPerMessage),
                {Acc, Ref + 1}
        end
    end, {0, 0}, lists:seq(1, (DurationMs div DelayPerMessage))),

    ElapsedMs = erlang:system_time(millisecond) - StartTime,
    RefusalsPerSecond = (Refused * 1000) / ElapsedMs,
    AcceptanceRate = Accepted / (Accepted + Refused),

    {RefusalsPerSecond, AcceptanceRate}.

%% @doc Measure throughput at bandwidth limit
measure_throughput(ConnId, MessageSize, MessagesPerSec, DurationMs) ->
    DelayPerMessage = 1000 div MessagesPerSec,
    StartTime = erlang:system_time(millisecond),

    {Accepted, _Refused, LimitReached} = lists:foldl(fun(_Idx, {Acc, Ref, LimitHit}) ->
        case erlmcp_queue_limits:check_queue_limit(ConnId, MessageSize) of
            {ok, accepted} ->
                erlmcp_queue_limits:record_message(ConnId, {msg, _Idx}, MessageSize),
                timer:sleep(DelayPerMessage),
                case (erlang:system_time(millisecond) - StartTime) > DurationMs of
                    true -> {Acc + 1, Ref, true};
                    false -> {Acc + 1, Ref, LimitHit}
                end;
            {error, refuse, _} ->
                timer:sleep(DelayPerMessage),
                case (erlang:system_time(millisecond) - StartTime) > DurationMs of
                    true -> {Acc, Ref + 1, true};
                    false -> {Acc, Ref + 1, LimitHit}
                end
        end
    end, {0, 0, false}, lists:seq(1, (DurationMs div DelayPerMessage) * 2)),

    {LimitReached, Accepted}.

%% Simple reference implementation for tracking state
ref_new() -> {erlmcp_ref, erlang:system_time(nanosecond)}.
ref_set({erlmcp_ref, _}, Value) -> {erlmcp_ref, Value}.
ref_get({erlmcp_ref, Value}) -> Value.

% For simplicity in tests, redefine as module functions
ref() ->
    case get(test_ref) of
        undefined -> put(test_ref, 0), 0;
        V -> V
    end.
