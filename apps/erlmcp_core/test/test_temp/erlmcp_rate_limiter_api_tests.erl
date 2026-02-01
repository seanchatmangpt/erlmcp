%%%-------------------------------------------------------------------
%% @doc Test Suite for erlmcp_rate_limiter API Boundary Tests
%%
%% API boundary tests for:
%% - Per-client rate limiting
%% - Global rate limiting
%% - Token bucket algorithm
%% - Client management
%%
%% Chicago School TDD: Real processes, observable behavior, no mocks
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_rate_limiter_api_tests).

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
    timer:sleep(50),
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
    {setup, fun setup/0, fun cleanup/1, [?_test(test_start_stop()), ?_test(test_get_stats())]}.

test_start_stop() ->
    Pid = whereis(erlmcp_rate_limiter),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    erlmcp_rate_limiter:stop(),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Pid)),
    {ok, _} = erlmcp_rate_limiter:start_link().

test_get_stats() ->
    Stats = erlmcp_rate_limiter:get_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(clients, Stats)),
    ?assert(maps:is_key(violations, Stats)).

%%--------------------------------------------------------------------
%% Per-Client Message Rate Limiting Tests
%%--------------------------------------------------------------------

message_rate_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_message_rate_single_request()),
      ?_test(test_message_rate_limit_exceeded()),
      ?_test(test_message_rate_recovery_after_wait()),
      ?_test(test_message_rate_per_client_isolation())]}.

test_message_rate_single_request() ->
    ClientId = client1,
    TimeNowMs = erlang:system_time(millisecond),
    Result = erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    ?assertMatch({ok, _}, Result).

test_message_rate_limit_exceeded() ->
    ClientId = client3,
    TimeNowMs = erlang:system_time(millisecond),
    % Consume all tokens
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 10)],
    Result = erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result).

test_message_rate_recovery_after_wait() ->
    ClientId = client4,
    TimeNowMs = erlang:system_time(millisecond),
    % Consume all tokens
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 10)],
    Result1 = erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),
    timer:sleep(1200),
    Result2 = erlmcp_rate_limiter:check_message_rate(ClientId, erlang:system_time(millisecond)),
    ?assertMatch({ok, _}, Result2).

test_message_rate_per_client_isolation() ->
    Client1 = client5a,
    Client2 = client5b,
    TimeNowMs = erlang:system_time(millisecond),
    % Consume for client1
    _ = [erlmcp_rate_limiter:check_message_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 10)],
    Result1 = erlmcp_rate_limiter:check_message_rate(Client1, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),
    Result2 = erlmcp_rate_limiter:check_message_rate(Client2, TimeNowMs),
    ?assertMatch({ok, _}, Result2).

%%--------------------------------------------------------------------
%% Token Bucket Algorithm Tests
%%--------------------------------------------------------------------

token_bucket_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_bucket_creation()),
      ?_test(test_bucket_refill()),
      ?_test(test_bucket_token_consumption())]}.

test_bucket_creation() ->
    Bucket = erlmcp_rate_limiter:create_token_bucket(10),
    ?assertMatch({_Tokens, _Timestamp}, Bucket).

test_bucket_refill() ->
    Bucket = erlmcp_rate_limiter:create_token_bucket(10),
    timer:sleep(200),
    RefillBucket = erlmcp_rate_limiter:refill_bucket(Bucket, 10),
    {RefillTokens, _} = RefillBucket,
    ?assert(RefillTokens >= 0).

test_bucket_token_consumption() ->
    Bucket = erlmcp_rate_limiter:create_token_bucket(10),
    {ok, _NewBucket, Remaining} = erlmcp_rate_limiter:consume_token(Bucket, 10),
    ?assert(is_number(Remaining)),
    ?assert(Remaining >= 0).

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
    erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    Result = erlmcp_rate_limiter:get_client_info(ClientId),
    ?assertMatch({ok, #{message_bucket := _, blocked_until := _}}, Result).

test_reset_client() ->
    ClientId = mgmt_client2,
    TimeNowMs = erlang:system_time(millisecond),
    erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs),
    {ok, _} = erlmcp_rate_limiter:get_client_info(ClientId),
    ok = erlmcp_rate_limiter:reset_client(ClientId),
    {error, not_found} = erlmcp_rate_limiter:get_client_info(ClientId).

test_client_isolation() ->
    Client1 = mgmt_client3a,
    Client2 = mgmt_client3b,
    TimeNowMs = erlang:system_time(millisecond),
    erlmcp_rate_limiter:check_message_rate(Client1, TimeNowMs),
    erlmcp_rate_limiter:check_message_rate(Client2, TimeNowMs),
    ok = erlmcp_rate_limiter:reset_client(Client1),
    {error, not_found} = erlmcp_rate_limiter:get_client_info(Client1),
    {ok, _} = erlmcp_rate_limiter:get_client_info(Client2).

%%--------------------------------------------------------------------
%% Configuration Tests
%%--------------------------------------------------------------------

configuration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_default_config()), ?_test(test_disabled_rate_limiting())]}.

test_default_config() ->
    Stats = erlmcp_rate_limiter:get_stats(),
    Config = maps:get(config, Stats),
    ?assert(maps:is_key(max_messages_per_sec, Config)),
    ?assert(maps:is_key(max_connections_per_sec, Config)).

test_disabled_rate_limiting() ->
    erlmcp_rate_limiter:stop(),
    timer:sleep(100),
    application:set_env(erlmcp, rate_limiting, #{enabled => false}),
    {ok, _} = erlmcp_rate_limiter:start_link(),
    TimeNowMs = erlang:system_time(millisecond),
    ClientId = test_disabled,
    Results =
        [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 100)],
    ?assert(lists:all(fun ({ok, _}) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)).
