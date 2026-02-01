%%%-------------------------------------------------------------------
%% @doc Edge Case Tests for Rate Limiter
%%
%% Tests for:
%% - Token bucket precision (floating point edge cases)
%% - Burst capacity handling
%% - Priority queue bypass with state updates
%% - Proper rounding for rate calculations
%% - High priority clients state tracking
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_rate_limit_edge_case_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    application:set_env(erlmcp,
                        rate_limiting,
                        #{max_messages_per_sec => 10,
                          max_connections_per_sec => 5,
                          global_max_messages_per_sec => 100,
                          max_tool_calls_per_sec => 20,
                          max_subscriptions_per_sec => 10,
                          bucket_refill_interval_ms => 100,
                          ddos_violation_threshold => 10,
                          ddos_block_duration_ms => 1000,
                          enabled => true}),
    {ok, _Pid} = erlmcp_rate_limiter:start_link(),
    ok.

cleanup(_) ->
    erlmcp_rate_limiter:stop(),
    application:unset_env(erlmcp, rate_limiting).

%%====================================================================
%% Token Bucket Precision Tests
%%====================================================================

token_bucket_precision_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_refill_precision()),
      ?_test(test_consume_at_threshold()),
      ?_test(test_partial_token_consumption()),
      ?_test(test_floating_point_accumulation())]}.

%% Test that refill maintains precision over multiple cycles
test_refill_precision() ->
    Bucket = erlmcp_rate_limiter:create_token_bucket(10),

    % Simulate multiple refills
    B1 = erlmcp_rate_limiter:refill_bucket(Bucket, 10),
    B2 = erlmcp_rate_limiter:refill_bucket(B1, 10),
    B3 = erlmcp_rate_limiter:refill_bucket(B2, 10),

    % Tokens should not exceed capacity due to floating point errors
    {FinalTokens, _} = B3,
    ?assert(FinalTokens =< 10.000001),
    ?assert(FinalTokens >= 10.0).

%% Test consumption at exactly 1.0 token threshold
test_consume_at_threshold() ->
    Bucket = {1.0, erlang:system_time(millisecond)},

    % Should succeed with exactly 1.0 token
    Result = erlmcp_rate_limiter:consume_token(Bucket, 10),
    ?assertMatch({ok, _, 0}, Result).

%% Test consumption with partial tokens (0.9, 0.5, etc.)
test_partial_token_consumption() ->
    Bucket = {0.9, erlang:system_time(millisecond)},

    % Should fail with less than 1.0 token
    Result = erlmcp_rate_limiter:consume_token(Bucket, 10),
    ?assertMatch({error, exceeded}, Result).

%% Test that floating point accumulation doesn't cause issues
test_floating_point_accumulation() ->
    Bucket = erlmcp_rate_limiter:create_token_bucket(10),

    % Consume and refill many times
    {ok, B1, _} = erlmcp_rate_limiter:consume_token(Bucket, 10),
    {ok, B2, _} = erlmcp_rate_limiter:consume_token(B1, 10),
    {ok, B3, _} = erlmcp_rate_limiter:consume_token(B2, 10),

    % Wait a bit for refill
    timer:sleep(150),

    B4 = erlmcp_rate_limiter:refill_bucket(B3, 10),
    {Tokens, _} = B4,

    % Should have some tokens back but not exceed capacity
    ?assert(Tokens > 0),
    ?assert(Tokens =< 10.0).

%%====================================================================
%% Burst Capacity Tests
%%====================================================================

burst_capacity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_burst_after_idle()),
      ?_test(test_burst_not_exceed_capacity()),
      ?_test(test_consecutive_bursts())]}.

%% Test that idle period allows burst up to capacity
test_burst_after_idle() ->
    ClientId = burst_client1,
    TimeNowMs = erlang:system_time(millisecond),

    % Wait for bucket to fully refill
    timer:sleep(200),

    % Should be able to consume up to capacity
    Results =
        [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 10)],

    % All should succeed (burst capacity)
    ?assert(lists:all(fun ({ok, _}) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)).

%% Test that burst doesn't exceed configured capacity
test_burst_not_exceed_capacity() ->
    ClientId = burst_client2,
    TimeNowMs = erlang:system_time(millisecond),

    % Try to consume more than capacity
    Results =
        [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 15)],

    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),
    FailureCount = length([R || R <- Results, element(1, R) =:= error]),

    % Should have exactly 10 successes, 5 failures
    ?assertEqual(10, SuccessCount),
    ?assertEqual(5, FailureCount).

%% Test consecutive bursts with partial refill
test_consecutive_bursts() ->
    ClientId = burst_client3,
    TimeNowMs = erlang:system_time(millisecond),

    % First burst
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 10)],

    % Wait for partial refill
    timer:sleep(150),

    TimeAfterMs = erlang:system_time(millisecond),

    % Should be able to consume some but not all
    Results =
        [erlmcp_rate_limiter:check_message_rate(ClientId, TimeAfterMs) || _ <- lists:seq(1, 5)],

    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),

    % Should have at least 1 success due to refill
    ?assert(SuccessCount >= 1).

%%====================================================================
%% Priority Queue Tests
%%====================================================================

priority_queue_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_high_priority_bypass_updates_state()),
      ?_test(test_high_priority_state_tracking()),
      ?_test(test_priority_switching())]}.

%% Test that high priority bypass updates bucket state
test_high_priority_bypass_updates_state() ->
    ClientId = priority_client1,
    TimeNowMs = erlang:system_time(millisecond),

    % Set high priority
    erlmcp_rate_limiter:set_client_priority(ClientId, high),

    % Make many requests (should all succeed)
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs, high)
         || _ <- lists:seq(1, 100)],

    % Check that state was updated
    {ok, ClientState} = erlmcp_rate_limiter:get_client_info(ClientId),
    ?assert(maps:is_key(message_bucket, ClientState)),

    % Bucket should be near empty (state was tracked)
    Bucket = maps:get(message_bucket, ClientState),
    Tokens = erlmcp_rate_limiter:bucket_tokens(Bucket),
    ?assert(Tokens < 10.0).

%% Test that high priority clients have state tracked properly
test_high_priority_state_tracking() ->
    ClientId = priority_client2,
    TimeNowMs = erlang:system_time(millisecond),

    % Set high priority
    erlmcp_rate_limiter:set_client_priority(ClientId, high),

    % Make requests
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs, high)
         || _ <- lists:seq(1, 50)],

    % Wait for refill
    timer:sleep(200),

    TimeAfterMs = erlang:system_time(millisecond),

    % Switch to normal priority - should be limited
    Result = erlmcp_rate_limiter:check_message_rate(ClientId, TimeAfterMs, normal),

    % Should succeed because bucket refilled
    ?assertMatch({ok, _}, Result).

%% Test switching between priorities
test_priority_switching() ->
    ClientId = priority_client3,
    TimeNowMs = erlang:system_time(millisecond),

    % Start as high priority
    erlmcp_rate_limiter:set_client_priority(ClientId, high),
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs, high)
         || _ <- lists:seq(1, 20)],

    % Switch to normal priority - should be limited
    Results =
        [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs, normal)
         || _ <- lists:seq(1, 15)],

    % Should have some failures
    Failures = [R || R <- Results, element(1, R) =:= error],
    ?assert(length(Failures) > 0).

%%====================================================================
%% Rounding and Precision Tests
%%====================================================================

rounding_precision_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_retry_after_rounding()),
      ?_test(test_tokens_remaining_rounding()),
      ?_test(test_rate_calculation_precision())]}.

%% Test that retry-after is properly rounded
test_retry_after_rounding( ) -> ClientId = rounding_client1 , TimeNowMs = erlang : system_time( millisecond ) , _ = [ erlmcp_rate_limiter : check_message_rate( ClientId , TimeNowMs ) || _ <- lists : seq( 1 , 10 ) ] , Result = erlmcp_rate_limiter : check_message_rate( ClientId , TimeNowMs ) , ?assertMatch( { error , rate_limited , RetryAfter } when RetryAfter > 0 , Result ) , { error , rate_limited , RetryAfter } = Result , ?assert( RetryAfter > 0 ) , ?assert( is_integer( RetryAfter ) ) .

    % Exhaust limit

    % Should get rate limited with proper retry-after

%% Test that tokens remaining is never negative
test_tokens_remaining_rounding() ->
    ClientId = rounding_client2,
    TimeNowMs = erlang:system_time(millisecond),

    % Consume tokens
    Results = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 9)],

    % All should have non-negative remaining tokens
    lists:foreach(fun({ok, Remaining}) ->
                     ?assert(Remaining >= 0),
                     ?assert(is_integer(Remaining))
                  end,
                  Results).

%% Test rate calculation precision under load
test_rate_calculation_precision() ->
    ClientId = rounding_client3,
    TimeNowMs = erlang:system_time(millisecond),

    % Rapid fire requests
    Results =
        [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 10)],

    % Exactly 10 should succeed
    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),
    ?assertEqual(10, SuccessCount).

%%====================================================================
%% Global Rate Limit Tests
%%====================================================================

global_rate_limit_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_global_limit_precision()),
      ?_test(test_global_limit_retry_after()),
      ?_test(test_global_multiple_clients())]}.

%% Test global limit precision
test_global_limit_precision() ->
    TimeNowMs = erlang:system_time(millisecond),

    % Consume exactly up to limit
    Results = [erlmcp_rate_limiter:check_global_rate(TimeNowMs) || _ <- lists:seq(1, 100)],

    % All should succeed
    ?assert(lists:all(fun ({ok, _}) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)).

%% Test global limit retry-after calculation
test_global_limit_retry_after( ) -> TimeNowMs = erlang : system_time( millisecond ) , _ = [ erlmcp_rate_limiter : check_global_rate( TimeNowMs ) || _ <- lists : seq( 1 , 100 ) ] , Result = erlmcp_rate_limiter : check_global_rate( TimeNowMs ) , ?assertMatch( { error , global_rate_limited , RetryAfter } when RetryAfter > 0 , Result ) , { error , global_rate_limited , RetryAfter } = Result , ?assert( RetryAfter > 0 ) , ?assert( is_integer( RetryAfter ) ) .

    % Exhaust global limit

    % Should get rate limited with proper retry-after

%% Test global limit with multiple clients
test_global_multiple_clients() ->
    TimeNowMs = erlang:system_time(millisecond),

    % Multiple clients sharing global limit
    Client1Results =
        [erlmcp_rate_limiter:check_message_rate(client_global1, TimeNowMs)
         || _ <- lists:seq(1, 50)],
    Client2Results =
        [erlmcp_rate_limiter:check_message_rate(client_global2, TimeNowMs)
         || _ <- lists:seq(1, 30)],

    % Combined should be under global limit
    AllResults = Client1Results ++ Client2Results,
    SuccessCount = length([R || R <- AllResults, element(1, R) =:= ok]),

    % Should be under global limit
    ?assert(SuccessCount =< 100).

%%====================================================================
%% Edge Case Stress Tests
%%====================================================================

stress_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_rapid_concurrent_requests()),
      ?_test(test_boundary_conditions()),
      ?_test(test_time_skew_handling())]}.

%% Test rapid concurrent requests
test_rapid_concurrent_requests() ->
    ClientId = stress_client1,
    TimeNowMs = erlang:system_time(millisecond),

    % Fire requests as fast as possible
    Results =
        [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 20)],

    % Should handle gracefully without crashing
    SuccessCount = length([R || R <- Results, element(1, R) =:= ok]),
    FailureCount = length([R || R <- Results, element(1, R) =:= error]),

    ?assert(SuccessCount > 0),
    ?assert(FailureCount > 0).

%% Test boundary conditions (0, 1, max)
test_boundary_conditions() ->
    ClientId = boundary_client,
    TimeNowMs = erlang:system_time(millisecond),

    % Single request
    Result1 = erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    ?assertMatch({ok, _}, Result1),

    % Request at limit
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 9)],

    % Request over limit
    ResultOver = erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, ResultOver).

%% Test handling of time skew (timestamps out of order)
test_time_skew_handling() ->
    ClientId = skew_client,
    BaseTime = erlang:system_time(millisecond),

    % Requests with decreasing timestamps (simulating clock skew)
    Results =
        [erlmcp_rate_limiter:check_message_rate(ClientId, BaseTime - N * 10)
         || N <- lists:seq(0, 9)],

    % Should handle gracefully (may all succeed due to time skew)
    ?assert(lists:all(fun ({ok, _}) ->
                              true;
                          ({error, _}) ->
                              true
                      end,
                      Results)).
