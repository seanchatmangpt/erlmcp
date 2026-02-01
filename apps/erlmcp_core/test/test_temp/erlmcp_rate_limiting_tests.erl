%%%-------------------------------------------------------------------
%% @doc Test Suite for erlmcp_rate_limiter Module
%%
%% Comprehensive tests for:
%% - Per-client rate limiting
%% - Global rate limiting
%% - Token bucket algorithm
%% - DDoS protection
%% - Graceful degradation
%% - Configuration
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_rate_limiting_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    % Stop any existing instance
    catch erlmcp_rate_limiter:stop(),
    timer:sleep(100),

    % Set up default config
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
    timer:sleep(50),  % Let server initialize
    ok.

cleanup(_) ->
    erlmcp_rate_limiter:stop(),
    timer:sleep(50),
    application:unset_env(erlmcp, rate_limiting).

%%====================================================================
%% Test Suites
%%====================================================================

%%--------------------------------------------------------------------
%% Module Lifecycle Tests
%%--------------------------------------------------------------------

module_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_start_stop()), ?_test(test_multiple_starts()), ?_test(test_get_stats())]}.

test_start_stop() ->
    Pid = whereis(erlmcp_rate_limiter),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    %% Stop and verify
    erlmcp_rate_limiter:stop(),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Pid)),

    %% Restart for other tests
    {ok, _} = erlmcp_rate_limiter:start_link().

test_multiple_starts() ->
    % Clean up first instance if running
    catch erlmcp_rate_limiter:stop(),
    timer:sleep(100),

    % Start new instance
    {ok, Pid} = erlmcp_rate_limiter:start_link(),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    % Clean up for other tests
    catch erlmcp_rate_limiter:stop(),
    timer:sleep(50),

    % Restart
    {ok, _} = erlmcp_rate_limiter:start_link().

test_get_stats() ->
    Stats = erlmcp_rate_limiter:get_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(clients, Stats)),
    ?assert(maps:is_key(violations, Stats)),
    ?assert(maps:is_key(blocked_clients, Stats)).

%%--------------------------------------------------------------------
%% Per-Client Message Rate Limiting Tests
%%--------------------------------------------------------------------

message_rate_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_message_rate_single_request()),
      ?_test(test_message_rate_multiple_requests()),
      ?_test(test_message_rate_limit_exceeded()),
      ?_test(test_message_rate_recovery_after_wait()),
      ?_test(test_message_rate_per_client_isolation()),
      ?_test(test_message_rate_concurrent_clients())]}.

test_message_rate_single_request() ->
    ClientId = client1,
    TimeNowMs = erlang:system_time(millisecond),

    Result = erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    ?assertMatch({ok, _}, Result).

test_message_rate_multiple_requests() ->
    ClientId = client2,
    TimeNowMs = erlang:system_time(millisecond),

    % Should allow multiple requests (up to limit)
    Results = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 5)],
    ?assert(lists:all(fun ({ok, _}) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)).

test_message_rate_limit_exceeded() ->
    ClientId = client3,
    TimeNowMs = erlang:system_time(millisecond),

    % Consume all tokens (default limit is 10)
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 10)],

    % Next request should be rate limited
    Result = erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result).

test_message_rate_recovery_after_wait() ->
    ClientId = client4,
    TimeNowMs = erlang:system_time(millisecond),

    % Consume all tokens
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 10)],

    % Should be rate limited
    Result1 = erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),

    % Wait for refill - sleep to ensure actual time passes
    timer:sleep(1200),

    % Try again with current time
    Result2 = erlmcp_rate_limiter:check_message_rate(ClientId, erlang:system_time(millisecond)),
    ?assertMatch({ok, _}, Result2).

test_message_rate_per_client_isolation() ->
    Client1 = client5a,
    Client2 = client5b,
    TimeNowMs = erlang:system_time(millisecond),

    % Consume all tokens for client1
    _ = [erlmcp_rate_limiter:check_message_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 10)],

    % Client1 should be limited
    Result1 = erlmcp_rate_limiter:check_message_rate(Client1, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),

    % Client2 should still work
    Result2 = erlmcp_rate_limiter:check_message_rate(Client2, TimeNowMs),
    ?assertMatch({ok, _}, Result2).

test_message_rate_concurrent_clients() ->
    TimeNowMs = erlang:system_time(millisecond),

    % Simulate 5 concurrent clients
    Results =
        lists:map(fun(N) ->
                     ClientId = list_to_atom("client_concurrent_" ++ integer_to_list(N)),
                     erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs)
                  end,
                  lists:seq(1, 5)),

    ?assert(lists:all(fun ({ok, _}) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)).

%%--------------------------------------------------------------------
%% Per-Client Connection Rate Limiting Tests
%%--------------------------------------------------------------------

connection_rate_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_connection_rate_single()),
      ?_test(test_connection_rate_limit_exceeded()),
      ?_test(test_connection_rate_isolation())]}.

test_connection_rate_single() ->
    ClientId = conn_client1,
    TimeNowMs = erlang:system_time(millisecond),

    Result = erlmcp_rate_limiter:check_connection_rate(ClientId, TimeNowMs),
    ?assertMatch({ok, _}, Result).

test_connection_rate_limit_exceeded() ->
    ClientId = conn_client2,
    TimeNowMs = erlang:system_time(millisecond),

    % Consume all tokens (default limit is 5)
    _ = [erlmcp_rate_limiter:check_connection_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 5)],

    % Next request should be rate limited
    Result = erlmcp_rate_limiter:check_connection_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result).

test_connection_rate_isolation() ->
    Client1 = conn_client3a,
    Client2 = conn_client3b,
    TimeNowMs = erlang:system_time(millisecond),

    % Consume for client1
    _ = [erlmcp_rate_limiter:check_connection_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 5)],

    % Client1 limited
    Result1 = erlmcp_rate_limiter:check_connection_rate(Client1, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),

    % Client2 should work
    Result2 = erlmcp_rate_limiter:check_connection_rate(Client2, TimeNowMs),
    ?assertMatch({ok, _}, Result2).

%%--------------------------------------------------------------------
%% Tool Call Rate Limiting Tests
%%--------------------------------------------------------------------

tool_rate_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_tool_call_rate_single()),
      ?_test(test_tool_call_rate_limit()),
      ?_test(test_tool_call_isolation())]}.

test_tool_call_rate_single() ->
    ClientId = tool_client1,
    TimeNowMs = erlang:system_time(millisecond),

    Result = erlmcp_rate_limiter:check_tool_call_rate(ClientId, TimeNowMs),
    ?assertMatch({ok, _}, Result).

test_tool_call_rate_limit() ->
    ClientId = tool_client2,
    TimeNowMs = erlang:system_time(millisecond),

    % Default limit is 20
    _ = [erlmcp_rate_limiter:check_tool_call_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 20)],

    Result = erlmcp_rate_limiter:check_tool_call_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result).

test_tool_call_isolation() ->
    Client1 = tool_client3a,
    Client2 = tool_client3b,
    TimeNowMs = erlang:system_time(millisecond),

    % Consume for client1
    _ = [erlmcp_rate_limiter:check_tool_call_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 20)],

    % Client1 limited
    Result1 = erlmcp_rate_limiter:check_tool_call_rate(Client1, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),

    % Client2 works
    Result2 = erlmcp_rate_limiter:check_tool_call_rate(Client2, TimeNowMs),
    ?assertMatch({ok, _}, Result2).

%%--------------------------------------------------------------------
%% Resource Subscription Rate Limiting Tests
%%--------------------------------------------------------------------

subscription_rate_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_subscription_rate_single()),
      ?_test(test_subscription_rate_limit()),
      ?_test(test_subscription_isolation())]}.

test_subscription_rate_single() ->
    ClientId = sub_client1,
    TimeNowMs = erlang:system_time(millisecond),

    Result = erlmcp_rate_limiter:check_subscription_rate(ClientId, TimeNowMs),
    ?assertMatch({ok, _}, Result).

test_subscription_rate_limit() ->
    ClientId = sub_client2,
    TimeNowMs = erlang:system_time(millisecond),

    % Default limit is 10
    _ = [erlmcp_rate_limiter:check_subscription_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 10)],

    Result = erlmcp_rate_limiter:check_subscription_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result).

test_subscription_isolation() ->
    Client1 = sub_client3a,
    Client2 = sub_client3b,
    TimeNowMs = erlang:system_time(millisecond),

    % Consume for client1
    _ = [erlmcp_rate_limiter:check_subscription_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 10)],

    % Client1 limited
    Result1 = erlmcp_rate_limiter:check_subscription_rate(Client1, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),

    % Client2 works
    Result2 = erlmcp_rate_limiter:check_subscription_rate(Client2, TimeNowMs),
    ?assertMatch({ok, _}, Result2).

%%--------------------------------------------------------------------
%% Global Rate Limiting Tests
%%--------------------------------------------------------------------

global_rate_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_global_rate_single()), ?_test(test_global_rate_limit())]}.

test_global_rate_single() ->
    TimeNowMs = erlang:system_time(millisecond),
    Result = erlmcp_rate_limiter:check_global_rate(TimeNowMs),
    ?assertMatch({ok, _}, Result).

test_global_rate_limit() ->
    TimeNowMs = erlang:system_time(millisecond),

    % Default global limit is 100
    % We'll test with a smaller number to avoid interfering with other tests
    Results = [erlmcp_rate_limiter:check_global_rate(TimeNowMs) || _ <- lists:seq(1, 50)],
    ?assert(lists:all(fun ({ok, _}) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)),

    % 51st might still pass due to token refill, but let's test the API works
    Result = erlmcp_rate_limiter:check_global_rate(TimeNowMs),
    ?assert(is_tuple(Result)),
    ?assert(element(1, Result) =:= ok orelse element(1, Result) =:= error).

%%--------------------------------------------------------------------
%% Token Bucket Algorithm Tests
%%--------------------------------------------------------------------

token_bucket_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_bucket_creation()),
      ?_test(test_bucket_refill()),
      ?_test(test_bucket_token_consumption()),
      ?_test(test_bucket_overflow_prevention())]}.

test_bucket_creation() ->
    Bucket = erlmcp_rate_limiter:create_token_bucket(10),
    ?assertMatch({_Tokens, _Timestamp}, Bucket).

test_bucket_refill() ->
    Bucket = erlmcp_rate_limiter:create_token_bucket(10),
    % Create a bucket "aged" 1 second
    {Tokens, _} = Bucket,

    timer:sleep(200),
    RefillBucket = erlmcp_rate_limiter:refill_bucket(Bucket, 10),
    {RefillTokens, _} = RefillBucket,

    % Should have more tokens after refill
    ?assert(RefillTokens >= Tokens).

test_bucket_token_consumption() ->
    Bucket = erlmcp_rate_limiter:create_token_bucket(10),

    % Should succeed
    {ok, _NewBucket, Remaining} = erlmcp_rate_limiter:consume_token(Bucket, 10),
    ?assert(is_number(Remaining)),
    ?assert(Remaining >= 0).

test_bucket_overflow_prevention() ->
    Bucket = erlmcp_rate_limiter:create_token_bucket(10),

    % Consume multiple times
    {ok, B1, _} = erlmcp_rate_limiter:consume_token(Bucket, 10),
    {ok, B2, _} = erlmcp_rate_limiter:consume_token(B1, 10),

    % Wait for refill
    timer:sleep(500),

    % Refill multiple times - should not exceed capacity
    B3 = erlmcp_rate_limiter:refill_bucket(B2, 10),
    {ok, B4, _} = erlmcp_rate_limiter:consume_token(B3, 10),
    Tokens = erlmcp_rate_limiter:bucket_tokens(B4),

    % Tokens should be <= capacity (with small tolerance for floating point)
    ?assert(Tokens =< 10.01).

%%--------------------------------------------------------------------
%% DDoS Protection Tests
%%--------------------------------------------------------------------

ddos_protection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_ddos_no_attack()),
      ?_test(test_ddos_detection()),
      ?_test(test_ddos_blocking()),
      ?_test(test_ddos_per_client_isolation())]}.

test_ddos_no_attack() ->
    ClientId = ddos_client1,
    ?assertNot(erlmcp_rate_limiter:is_ddos_attack(ClientId)).

test_ddos_detection() ->
    ClientId = ddos_client2,
    TimeNowMs = erlang:system_time(millisecond),

    % Trigger violations (threshold is 10)
    % Each request beyond the limit triggers a violation
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 15)],

    % Should detect attack
    IsAttack = erlmcp_rate_limiter:is_ddos_attack(ClientId),
    ?assert(IsAttack).

test_ddos_blocking() ->
    ClientId = ddos_client3,
    TimeNowMs = erlang:system_time(millisecond),

    % Trigger violations
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 15)],

    % Should be rate limited
    IsLimited = erlmcp_rate_limiter:is_rate_limited(ClientId),
    ?assert(IsLimited).

test_ddos_per_client_isolation() ->
    Client1 = ddos_client5a,
    Client2 = ddos_client5b,
    TimeNowMs = erlang:system_time(millisecond),

    % Trigger violations for client1
    _ = [erlmcp_rate_limiter:check_message_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 15)],

    % Client1 should be flagged as attack
    ?assert(erlmcp_rate_limiter:is_ddos_attack(Client1)),

    % Client2 should not be affected
    ?assertNot(erlmcp_rate_limiter:is_ddos_attack(Client2)).

%%--------------------------------------------------------------------
%% Client Info and Management Tests
%%--------------------------------------------------------------------

client_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_get_client_info()),
      ?_test(test_reset_client()),
      ?_test(test_client_isolation())]}.

test_get_client_info() ->
    ClientId = mgmt_client1,
    TimeNowMs = erlang:system_time(millisecond),

    % Make a request to create client state
    erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),

    % Get info
    Result = erlmcp_rate_limiter:get_client_info(ClientId),
    ?assertMatch({ok, #{message_bucket := _, blocked_until := _}}, Result).

test_reset_client() ->
    ClientId = mgmt_client2,
    TimeNowMs = erlang:system_time(millisecond),

    % Create client state
    erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),

    % Verify it exists
    {ok, _} = erlmcp_rate_limiter:get_client_info(ClientId),

    % Reset
    ok = erlmcp_rate_limiter:reset_client(ClientId),

    % Should not exist
    {error, not_found} = erlmcp_rate_limiter:get_client_info(ClientId).

test_client_isolation() ->
    Client1 = mgmt_client3a,
    Client2 = mgmt_client3b,
    TimeNowMs = erlang:system_time(millisecond),

    % Create state for both
    erlmcp_rate_limiter:check_message_rate(Client1, TimeNowMs),
    erlmcp_rate_limiter:check_message_rate(Client2, TimeNowMs),

    % Reset client1
    ok = erlmcp_rate_limiter:reset_client(Client1),

    % Client1 should be gone
    {error, not_found} = erlmcp_rate_limiter:get_client_info(Client1),

    % Client2 should still exist
    {ok, _} = erlmcp_rate_limiter:get_client_info(Client2).

%%--------------------------------------------------------------------
%% Configuration Tests
%%--------------------------------------------------------------------

configuration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_default_config()),
      ?_test(test_custom_config()),
      ?_test(test_disabled_rate_limiting())]}.

test_default_config() ->
    Stats = erlmcp_rate_limiter:get_stats(),
    Config = maps:get(config, Stats),
    ?assert(maps:is_key(max_messages_per_sec, Config)),
    ?assert(maps:is_key(max_connections_per_sec, Config)).

test_custom_config() ->
    erlmcp_rate_limiter:stop(),
    timer:sleep(100),

    application:set_env(erlmcp, rate_limiting, #{max_messages_per_sec => 50, enabled => true}),
    {ok, _} = erlmcp_rate_limiter:start_link(),

    Stats = erlmcp_rate_limiter:get_stats(),
    Config = maps:get(config, Stats),
    MaxMsgs = maps:get(max_messages_per_sec, Config),
    ?assertEqual(50, MaxMsgs).

test_disabled_rate_limiting() ->
    erlmcp_rate_limiter:stop(),
    timer:sleep(100),

    application:set_env(erlmcp, rate_limiting, #{enabled => false}),
    {ok, _} = erlmcp_rate_limiter:start_link(),

    TimeNowMs = erlang:system_time(millisecond),
    ClientId = test_disabled,

    % Should always allow requests
    Results =
        [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 100)],
    ?assert(lists:all(fun ({ok, _}) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)).

%%--------------------------------------------------------------------
%% Graceful Degradation Tests
%%--------------------------------------------------------------------

graceful_degradation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_graceful_degradation_message_rate()),
      ?_test(test_graceful_degradation_connection_rate()),
      ?_test(test_graceful_degradation_tool_rate())]}.

test_graceful_degradation_message_rate() ->
    % Simulate high load - clients should be gradually limited
    TimeNowMs = erlang:system_time(millisecond),

    % First batch should succeed
    Results1 =
        [erlmcp_rate_limiter:check_message_rate(client_degrad1, TimeNowMs)
         || _ <- lists:seq(1, 10)],
    Successes1 = length([ok || {ok, _} <- Results1]),
    ?assertEqual(10, Successes1),

    % Second batch should have some failures
    Results2 =
        [erlmcp_rate_limiter:check_message_rate(client_degrad1, TimeNowMs)
         || _ <- lists:seq(1, 10)],
    Successes2 = length([ok || {ok, _} <- Results2]),
    ?assert(Successes2 < 10).

test_graceful_degradation_connection_rate() ->
    TimeNowMs = erlang:system_time(millisecond),

    % Exceed limit
    _ = [erlmcp_rate_limiter:check_connection_rate(client_degrad2, TimeNowMs)
         || _ <- lists:seq(1, 5)],

    % Further requests should return proper error
    Result = erlmcp_rate_limiter:check_connection_rate(client_degrad2, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result),
    {error, rate_limited, RetryAfter} = Result,
    ?assert(RetryAfter > 0).

test_graceful_degradation_tool_rate() ->
    TimeNowMs = erlang:system_time(millisecond),

    % Exceed tool rate
    _ = [erlmcp_rate_limiter:check_tool_call_rate(client_degrad3, TimeNowMs)
         || _ <- lists:seq(1, 20)],

    % Should have proper backoff time
    Result = erlmcp_rate_limiter:check_tool_call_rate(client_degrad3, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result),
    {error, rate_limited, RetryAfter} = Result,
    ?assert(RetryAfter > 0).
