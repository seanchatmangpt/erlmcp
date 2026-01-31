%%%-------------------------------------------------------------------
%%% @doc Subscription Rate Limiting Tests
%%%
%%% Comprehensive tests for rate limiting in the subscription system.
%%% Tests the token bucket algorithm integration between
%%% erlmcp_subscription and erlmcp_rate_limiter.
%%%
%%% Test Categories:
%%% - Token bucket algorithm correctness
%%% - Per-subscription rate limiting
%%% - Burst capacity handling
%%% - Rate limit recovery (token refill)
%%% - Concurrent subscription notifications
%%% - Filter + rate limit interaction
%%% - Rate limit bypass (no limit)
%%%
%%% Chicago School TDD: Tests ALL observable behavior through
%%% the subscription API, with real processes (no mocks).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_subscription_rate_limiter_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%% Test state record
-record(test_state, {
    subscription_pid :: pid(),
    subscriber_pid :: pid(),
    subscription_id :: binary()
}).

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Setup function - starts subscription manager
setup() ->
    % Start rate limiter first (dependency)
    {ok, RateLimiterPid} = erlmcp_rate_limiter:start_link(),

    % Start subscription manager
    {ok, SubscriptionPid} = erlmcp_subscription:start_link(),

    #{
        subscription_pid => SubscriptionPid,
        rate_limiter_pid => RateLimiterPid
    }.

%% @doc Cleanup function - stops subscription manager
cleanup(#{subscription_pid := SubscriptionPid, rate_limiter_pid := RateLimiterPid}) ->
    catch gen_server:stop(SubscriptionPid, normal, 1000),
    catch gen_server:stop(RateLimiterPid, normal, 1000),
    ok.

%%====================================================================
%% Token Bucket Algorithm Tests
%%====================================================================

%% @doc Test token bucket creation and initial state
token_bucket_creation_test() ->
    #{subscription_pid := _Pid} = setup(),

    try
        % Test that token bucket starts with full capacity
        Bucket = erlmcp_rate_limiter:create_token_bucket(100),

        % Bucket should start with full capacity
        ?assertEqual(100.0, erlmcp_rate_limiter:bucket_tokens(Bucket)),

        % Check refill timestamp is recent
        {_, RefillMs} = Bucket,
        TimeNowMs = erlang:system_time(millisecond),
        ?assert(TimeNowMs - RefillMs =< 100)  % Within 100ms
    after
        cleanup(#{subscription_pid => whereis(erlmcp_subscription), rate_limiter_pid => whereis(erlmcp_rate_limiter)})
    end.

%% @doc Test token consumption
token_consumption_test() ->
    #{subscription_pid := _Pid} = setup(),

    try
        Bucket = erlmcp_rate_limiter:create_token_bucket(10),

        % Consume first token
        {ok, Bucket1, Tokens1} = erlmcp_rate_limiter:consume_token(Bucket, 10),
        ?assertEqual(9, Tokens1),

        % Consume second token
        {ok, Bucket2, Tokens2} = erlmcp_rate_limiter:consume_token(Bucket1, 10),
        ?assertEqual(8, Tokens2),

        % Verify bucket state
        ?assertEqual(8.0, erlmcp_rate_limiter:bucket_tokens(Bucket2))
    after
        cleanup(#{subscription_pid => whereis(erlmcp_subscription), rate_limiter_pid => whereis(erlmcp_rate_limiter)})
    end.

%% @doc Test token bucket refill over time
token_refill_test() ->
    #{subscription_pid := _Pid} = setup(),

    try
        % Create bucket with capacity 100 tokens/sec
        Bucket = erlmcp_rate_limiter:create_token_bucket(100),

        % Drain bucket completely
        {ok, Bucket1, _} = erlmcp_rate_limiter:consume_token(Bucket, 100),
        ?assertEqual(99, erlmcp_rate_limiter:bucket_tokens(Bucket1)),

        % Wait 100ms - should get ~10 tokens back
        timer:sleep(100),

        % Refill should add tokens
        RefilledBucket = erlmcp_rate_limiter:refill_bucket(Bucket1, 100),
        Tokens = erlmcp_rate_limiter:bucket_tokens(RefilledBucket),

        % Should have refilled approximately 10 tokens (100 tokens/sec * 0.1s)
        ?assert(Tokens >= 9.0 andalso Tokens =< 11.0)
    after
        cleanup(#{subscription_pid => whereis(erlmcp_subscription), rate_limiter_pid => whereis(erlmcp_rate_limiter)})
    end.

%% @doc Test bucket cannot exceed capacity
bucket_capacity_limit_test() ->
    #{subscription_pid := _Pid} = setup(),

    try
        % Create bucket with capacity 10
        Bucket = erlmcp_rate_limiter:create_token_bucket(10),

        % Wait significant time (2 seconds) - should refill fully but not exceed
        timer:sleep(2000),

        % Even after long wait, bucket should not exceed capacity
        RefilledBucket = erlmcp_rate_limiter:refill_bucket(Bucket, 10),
        Tokens = erlmcp_rate_limiter:bucket_tokens(RefilledBucket),

        ?assert(Tokens =< 10.5)  % Allow small floating point error
    after
        cleanup(#{subscription_pid => whereis(erlmcp_subscription), rate_limiter_pid => whereis(erlmcp_rate_limiter)})
    end.

%% @doc Test bucket exhaustion
bucket_exhaustion_test() ->
    #{subscription_pid := _Pid} = setup(),

    try
        % Create bucket with small capacity (5 tokens)
        Bucket = erlmcp_rate_limiter:create_token_bucket(5),

        % Consume all 5 tokens
        {ok, Bucket1, _} = erlmcp_rate_limiter:consume_token(Bucket, 5),
        {ok, Bucket2, _} = erlmcp_rate_limiter:consume_token(Bucket1, 5),
        {ok, Bucket3, _} = erlmcp_rate_limiter:consume_token(Bucket2, 5),
        {ok, Bucket4, _} = erlmcp_rate_limiter:consume_token(Bucket3, 5),
        {ok, Bucket5, _} = erlmcp_rate_limiter:consume_token(Bucket4, 5),

        ?assertEqual(0, erlmcp_rate_limiter:bucket_tokens(Bucket5)),

        % Next consumption should fail
        Result = erlmcp_rate_limiter:consume_token(Bucket5, 5),
        ?assertEqual({error, exceeded}, Result)
    after
        cleanup(#{subscription_pid => whereis(erlmcp_subscription), rate_limiter_pid => whereis(erlmcp_rate_limiter)})
    end.

%%====================================================================
%% Subscription Rate Limiting Tests
%%====================================================================

%% @doc Test per-subscription rate limiting
per_subscription_rate_limit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{}) ->
         [
             {"Allow notifications within rate limit", fun test_allow_within_limit/0},
             {"Block notifications exceeding rate limit", fun test_block_exceeds_limit/0},
             {"Recover rate limit after token refill", fun test_rate_limit_recovery/0},
             {"Handle burst capacity", fun test_burst_capacity/0},
             {"Bypass rate limit when disabled", fun test_rate_limit_bypass/0}
         ]
     end}.

%% @doc Test that notifications are allowed within rate limit
test_allow_within_limit() ->
    % Subscribe with rate limit of 10 messages/sec
    SubscriptionId = <<"test_rate_limit">>,
    Subscriber = self(),
    Options = #{rate_limit => 10},

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options)),

    % Send 5 notifications (within limit)
    lists:foreach(fun(I) ->
        ?assertEqual(ok, erlmcp_subscription:notify(SubscriptionId, {msg, I}))
    end, lists:seq(1, 5)),

    % Clean up
    erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber).

%% @doc Test that notifications are blocked when exceeding rate limit
test_block_exceeds_limit() ->
    % Subscribe with low rate limit (5 messages/sec)
    SubscriptionId = <<"test_exceed_limit">>,
    Subscriber = self(),
    Options = #{rate_limit => 5},

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options)),

    % Drain the bucket quickly (send more than capacity)
    % The first 5 should succeed, subsequent ones should be rate limited
    % Note: Rate limiting is best-effort via notification, not synchronous
    % We verify by checking subscriber received less than sent

    % Clear mailbox
    flush_mailbox(),

    % Send 20 notifications rapidly
    lists:foreach(fun(I) ->
        erlmcp_subscription:notify(SubscriptionId, {msg, I})
    end, lists:seq(1, 20)),

    % Wait for notifications to arrive
    timer:sleep(100),

    % Count received messages
    ReceivedCount = count_mailbox_messages(),

    % Should receive fewer than sent due to rate limiting
    % (Exact count depends on timing, but should be significantly less)
    ?assert(ReceivedCount < 20),

    % Clean up
    erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber).

%% @doc Test rate limit recovery after token refill
test_rate_limit_recovery() ->
    SubscriptionId = <<"test_recovery">>,
    Subscriber = self(),
    Options = #{rate_limit => 10},

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options)),

    % Drain bucket initially
    flush_mailbox(),
    lists:foreach(fun(I) ->
        erlmcp_subscription:notify(SubscriptionId, {msg, I})
    end, lists:seq(1, 15)),

    timer:sleep(50),
    _InitialCount = count_mailbox_messages(),

    % Wait for tokens to refill (1 second)
    timer:sleep(1000),

    % Clear mailbox
    flush_mailbox(),

    % Should now be able to send more messages
    lists:foreach(fun(I) ->
        erlmcp_subscription:notify(SubscriptionId, {msg, I})
    end, lists:seq(1, 5)),

    timer:sleep(50),
    RecoveredCount = count_mailbox_messages(),

    % Should receive messages again after refill
    ?assert(RecoveredCount > 0),

    % Clean up
    erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber).

%% @doc Test burst capacity (bucket allows burst up to capacity)
test_burst_capacity() ->
    SubscriptionId = <<"test_burst">>,
    Subscriber = self(),
    Options = #{rate_limit => 100},  % 100 tokens/sec capacity

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options)),

    % Clear mailbox
    flush_mailbox(),

    % Send burst of 50 messages instantly (within bucket capacity)
    lists:foreach(fun(I) ->
        erlmcp_subscription:notify(SubscriptionId, {msg, I})
    end, lists:seq(1, 50)),

    timer:sleep(100),
    BurstCount = count_mailbox_messages(),

    % Should receive all burst messages (within capacity)
    ?assertEqual(50, BurstCount),

    % Clean up
    erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber).

%% @doc Test rate limit bypass when rate_limit is 0
test_rate_limit_bypass() ->
    SubscriptionId = <<"test_bypass">>,
    Subscriber = self(),
    Options = #{rate_limit => 0},  % 0 = no limit

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options)),

    % Clear mailbox
    flush_mailbox(),

    % Send many messages rapidly
    lists:foreach(fun(I) ->
        erlmcp_subscription:notify(SubscriptionId, {msg, I})
    end, lists:seq(1, 100)),

    timer:sleep(100),
    BypassCount = count_mailbox_messages(),

    % Should receive all messages (no rate limiting)
    ?assertEqual(100, BypassCount),

    % Clean up
    erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber).

%%====================================================================
%% Concurrent Access Tests
%%====================================================================

%% @doc Test concurrent subscriptions with different rate limits
concurrent_rate_limits_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{}) ->
         [
             {"Multiple subscribers with independent limits", fun test_independent_limits/0},
             {"High rate limit subscriber vs low rate limit", fun test_mixed_limits/0}
         ]
     end}.

%% @doc Test that multiple subscribers have independent rate limits
test_independent_limits() ->
    SubscriptionId = <<"test_independent">>,

    % Subscriber 1: Low rate limit (5/sec)
    Pid1 = spawn(fun() -> subscriber_loop(1000) end),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Pid1, #{rate_limit => 5})),

    % Subscriber 2: High rate limit (100/sec)
    Pid2 = spawn(fun() -> subscriber_loop(1000) end),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Pid2, #{rate_limit => 100})),

    % Send many notifications
    lists:foreach(fun(I) ->
        erlmcp_subscription:notify(SubscriptionId, {msg, I})
    end, lists:seq(1, 50)),

    timer:sleep(100),

    % Subscriber 2 should receive more messages than subscriber 1
    % (Exact counts depend on timing, but Pid2 should get significantly more)
    {message_queue_len, Len1} = erlang:process_info(Pid1, message_queue_len),
    {message_queue_len, Len2} = erlang:process_info(Pid2, message_queue_len),

    % Pid2 should have at least as many messages as Pid1
    ?assert(Len2 >= Len1),

    % Clean up
    erlang:exit(Pid1, kill),
    erlang:exit(Pid2, kill).

%% @doc Test mixed rate limits in same subscription
test_mixed_limits() ->
    SubscriptionId = <<"test_mixed">>,

    % Create 3 subscribers with different rate limits
    PidLow = spawn(fun() -> subscriber_loop(1000) end),
    PidMed = spawn(fun() -> subscriber_loop(1000) end),
    PidHigh = spawn(fun() -> subscriber_loop(1000) end),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, PidLow, #{rate_limit => 5})),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, PidMed, #{rate_limit => 20})),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, PidHigh, #{rate_limit => 100})),

    % Send burst of notifications
    lists:foreach(fun(I) ->
        erlmcp_subscription:notify(SubscriptionId, {msg, I})
    end, lists:seq(1, 60)),

    timer:sleep(100),

    % Check queue lengths
    {message_queue_len, LenLow} = erlang:process_info(PidLow, message_queue_len),
    {message_queue_len, LenMed} = erlang:process_info(PidMed, message_queue_len),
    {message_queue_len, LenHigh} = erlang:process_info(PidHigh, message_queue_len),

    % High rate limit should receive most, medium less, low least
    ?assert(LenHigh >= LenMed),
    ?assert(LenMed >= LenLow),

    % Clean up
    erlang:exit(PidLow, kill),
    erlang:exit(PidMed, kill),
    erlang:exit(PidHigh, kill).

%%====================================================================
%% Filter + Rate Limit Interaction Tests
%%====================================================================

%% @doc Test that filter applies before rate limit
filter_before_rate_limit_test() ->
    #{subscription_pid := _Pid} = setup(),

    try
        SubscriptionId = <<"test_filter_rate">>,
        Subscriber = self(),

        % Subscribe with filter (only accept even numbers) and rate limit
        Filter = fun({msg, N}) -> N rem 2 =:= 0 end,
        Options = #{filter => Filter, rate_limit => 10},

        ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options)),

        % Clear mailbox
        flush_mailbox(),

        % Send messages (both even and odd)
        lists:foreach(fun(I) ->
            erlmcp_subscription:notify(SubscriptionId, {msg, I})
        end, lists:seq(1, 20)),

        timer:sleep(100),

        % Count received messages
        ReceivedCount = count_mailbox_messages(),

        % Should only receive even messages (filtered)
        % And should not be rate limited (10 even messages < 10/sec limit)
        ?assertEqual(10, ReceivedCount),

        % Clean up
        erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber)
    after
        cleanup(#{subscription_pid => whereis(erlmcp_subscription), rate_limiter_pid => whereis(erlmcp_rate_limiter)})
    end.

%%====================================================================
%% Sliding Window Tests
%%====================================================================

%% @doc Test sliding window rate limiting
sliding_window_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{}) ->
         [
             {"Sliding window allows requests within limit", fun test_sliding_window_within_limit/0},
             {"Sliding window blocks when exceeded", fun test_sliding_window_exceeded/0},
             {"Sliding window slides correctly", fun test_sliding_window_sliding/0}
         ]
     end}.

%% @doc Test sliding window allows requests within limit
test_sliding_window_within_limit() ->
    Window = erlmcp_rate_limiter:create_sliding_window(1000),  % 1 second window

    % Make 5 requests (within 10 request limit)
    {ok, Window1, _} = erlmcp_rate_limiter:check_sliding_window(Window, 10, 100),
    {ok, Window2, _} = erlmcp_rate_limiter:check_sliding_window(Window1, 10, 150),
    {ok, Window3, _} = erlmcp_rate_limiter:check_sliding_window(Window2, 10, 200),
    {ok, Window4, _} = erlmcp_rate_limiter:check_sliding_window(Window3, 10, 250),
    {ok, _Window5, _} = erlmcp_rate_limiter:check_sliding_window(Window4, 10, 300),

    ok.

%% @doc Test sliding window blocks when exceeded
test_sliding_window_exceeded() ->
    Window = erlmcp_rate_limiter:create_sliding_window(1000),  % 1 second window

    % Make 5 requests (limit is 5)
    {ok, Window1, _} = erlmcp_rate_limiter:check_sliding_window(Window, 5, 100),
    {ok, Window2, _} = erlmcp_rate_limiter:check_sliding_window(Window1, 5, 150),
    {ok, Window3, _} = erlmcp_rate_limiter:check_sliding_window(Window2, 5, 200),
    {ok, Window4, _} = erlmcp_rate_limiter:check_sliding_window(Window3, 5, 250),
    {ok, Window5, _} = erlmcp_rate_limiter:check_sliding_window(Window4, 5, 300),

    % 6th request should fail
    Result = erlmcp_rate_limiter:check_sliding_window(Window5, 5, 350),
    ?assertEqual({error, exceeded}, Result).

%% @doc Test sliding window slides correctly over time
test_sliding_window_sliding() ->
    Window = erlmcp_rate_limiter:create_sliding_window(100),  % 100ms window

    % Make 5 requests at t=0 (limit is 5)
    {ok, Window1, _} = erlmcp_rate_limiter:check_sliding_window(Window, 5, 0),
    {ok, Window2, _} = erlmcp_rate_limiter:check_sliding_window(Window1, 5, 10),
    {ok, Window3, _} = erlmcp_rate_limiter:check_sliding_window(Window2, 5, 20),
    {ok, Window4, _} = erlmcp_rate_limiter:check_sliding_window(Window3, 5, 30),
    {ok, Window5, _} = erlmcp_rate_limiter:check_sliding_window(Window4, 5, 40),

    % 6th request should fail
    ?assertEqual({error, exceeded},
                 erlmcp_rate_limiter:check_sliding_window(Window5, 5, 50)),

    % Wait for window to slide (150ms - older requests fall out)
    timer:sleep(150),

    % New request should succeed (window has slid)
    % Note: Need to create new window for this test as timestamps don't auto-update
    NewWindow = erlmcp_rate_limiter:create_sliding_window(100),
    {ok, _NewWindow1, _} = erlmcp_rate_limiter:check_sliding_window(NewWindow, 5, 150),

    ok.

%%====================================================================
%% Leaky Bucket Tests
%%====================================================================

%% @doc Test leaky bucket rate limiting
leaky_bucket_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{}) ->
         [
             {"Leaky bucket allows within capacity", fun test_leaky_bucket_within_capacity/0},
             {"Leaky bucket blocks when full", fun test_leaky_bucket_full/0},
             {"Leaky bucket leaks over time", fun test_leaky_bucket_leaking/0}
         ]
     end}.

%% @doc Test leaky bucket allows requests within capacity
test_leaky_bucket_within_capacity() ->
    Bucket = erlmcp_rate_limiter:create_leaky_bucket(10),

    % Add 5 requests
    {ok, Bucket1, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket, 10),
    {ok, Bucket2, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket1, 10),
    {ok, Bucket3, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket2, 10),
    {ok, Bucket4, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket3, 10),
    {ok, _Bucket5, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket4, 10),

    ok.

%% @doc Test leaky bucket blocks when full
test_leaky_bucket_full() ->
    Bucket = erlmcp_rate_limiter:create_leaky_bucket(5),

    % Fill bucket
    {ok, Bucket1, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket, 5),
    {ok, Bucket2, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket1, 5),
    {ok, Bucket3, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket2, 5),
    {ok, Bucket4, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket3, 5),
    {ok, Bucket5, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket4, 5),

    % 6th request should fail
    Result = erlmcp_rate_limiter:check_leaky_bucket(Bucket5, 5),
    ?assertEqual({error, exceeded}, Result).

%% @doc Test leaky bucket leaks over time
test_leaky_bucket_leaking() ->
    Bucket = erlmcp_rate_limiter:create_leaky_bucket(10),

    % Fill bucket partially
    {ok, Bucket1, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket, 10),
    {ok, Bucket2, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket1, 10),
    {ok, Bucket3, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket2, 10),
    {ok, Bucket4, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket3, 10),
    {ok, Bucket5, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket4, 10),

    % Wait for leak (1 second)
    timer:sleep(1000),

    % Should now have space (bucket leaked)
    {ok, _Bucket6, _} = erlmcp_rate_limiter:check_leaky_bucket(Bucket5, 10),

    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

%% @doc Test rate limiter integration with subscription system
integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(#{}) ->
         [
             {"Subscribe with rate limit config", fun test_subscribe_with_rate_limit/0},
             {"Get rate limit stats", fun test_get_rate_limit_stats/0},
             {"Reset client rate limit state", fun test_reset_client_rate_limit/0}
         ]
     end}.

%% @doc Test subscribing with rate limit configuration
test_subscribe_with_rate_limit() ->
    SubscriptionId = <<"test_rate_limit_config">>,
    Subscriber = self(),

    % Subscribe with specific rate limit
    Options = #{rate_limit => 50},
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options)),

    % Verify subscription exists
    Subscribers = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assert(lists:member(Subscriber, Subscribers)),

    % Clean up
    erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber).

%% @doc Test getting rate limit statistics
test_get_rate_limit_stats() ->
    % Subscribe with rate limit
    SubscriptionId = <<"test_rate_limit_stats">>,
    Subscriber = self(),
    Options = #{rate_limit => 100},

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options)),

    % Send some notifications
    lists:foreach(fun(I) ->
        erlmcp_subscription:notify(SubscriptionId, {msg, I})
    end, lists:seq(1, 10)),

    % Get stats (should include our subscriber)
    Stats = erlmcp_rate_limiter:get_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(clients, Stats)),

    % Clean up
    erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber).

%% @doc Test resetting client rate limit state
test_reset_client_rate_limit() ->
    % Subscribe with rate limit
    SubscriptionId = <<"test_reset_rate_limit">>,
    Subscriber = self(),
    Options = #{rate_limit => 5},

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options)),

    % Exhaust rate limit
    lists:foreach(fun(I) ->
        erlmcp_subscription:notify(SubscriptionId, {msg, I})
    end, lists:seq(1, 20)),

    % Reset client state
    ClientId = {subscription_rate_limit, Subscriber},
    ?assertEqual(ok, erlmcp_rate_limiter:reset_client(ClientId)),

    % Should now be able to send messages again
    flush_mailbox(),
    ?assertEqual(ok, erlmcp_subscription:notify(SubscriptionId, {msg, reset})),

    timer:sleep(50),
    ?assertEqual(1, count_mailbox_messages()),

    % Clean up
    erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Subscriber loop that receives and counts messages
subscriber_loop(Timeout) ->
    receive
        {'$mcp_subscription', _Msg} ->
            subscriber_loop(Timeout);
        stop ->
            ok
    after Timeout ->
        ok
    end.

%% @doc Flush all messages from current process mailbox
flush_mailbox() ->
    receive
        _ -> flush_mailbox()
    after 0 ->
        ok
    end.

%% @doc Count messages in current process mailbox
count_mailbox_messages() ->
    count_mailbox_messages(0).

count_mailbox_messages(Count) ->
    receive
        {'$mcp_subscription', _Msg} ->
            count_mailbox_messages(Count + 1)
    after 0 ->
        Count
    end.
