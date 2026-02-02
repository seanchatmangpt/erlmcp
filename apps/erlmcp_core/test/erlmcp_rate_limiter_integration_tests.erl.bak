%%%-------------------------------------------------------------------
%% @doc Test Suite for erlmcp_rate_limiter Integration Tests
%%
%% Integration tests for:
%% - Connection rate limiting
%% - Tool call rate limiting
%% - Subscription rate limiting
%% - Global rate limiting
%% - DDoS protection
%% - Graceful degradation
%%
%% Chicago School TDD: Real processes, observable behavior, no mocks
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_rate_limiter_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    catch erlmcp_rate_limiter:stop(),
    timer:sleep(100),
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
    _ = [erlmcp_rate_limiter:check_connection_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 5)],
    Result = erlmcp_rate_limiter:check_connection_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result).

test_connection_rate_isolation() ->
    Client1 = conn_client3a,
    Client2 = conn_client3b,
    TimeNowMs = erlang:system_time(millisecond),
    _ = [erlmcp_rate_limiter:check_connection_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 5)],
    Result1 = erlmcp_rate_limiter:check_connection_rate(Client1, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),
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
    _ = [erlmcp_rate_limiter:check_tool_call_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 20)],
    Result = erlmcp_rate_limiter:check_tool_call_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result).

test_tool_call_isolation() ->
    Client1 = tool_client3a,
    Client2 = tool_client3b,
    TimeNowMs = erlang:system_time(millisecond),
    _ = [erlmcp_rate_limiter:check_tool_call_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 20)],
    Result1 = erlmcp_rate_limiter:check_tool_call_rate(Client1, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),
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
    _ = [erlmcp_rate_limiter:check_subscription_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 10)],
    Result = erlmcp_rate_limiter:check_subscription_rate(ClientId, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result).

test_subscription_isolation() ->
    Client1 = sub_client3a,
    Client2 = sub_client3b,
    TimeNowMs = erlang:system_time(millisecond),
    _ = [erlmcp_rate_limiter:check_subscription_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 10)],
    Result1 = erlmcp_rate_limiter:check_subscription_rate(Client1, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result1),
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
    Results = [erlmcp_rate_limiter:check_global_rate(TimeNowMs) || _ <- lists:seq(1, 50)],
    ?assert(lists:all(fun ({ok, _}) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)).

%%--------------------------------------------------------------------
%% DDoS Protection Tests
%%--------------------------------------------------------------------

ddos_protection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_ddos_no_attack()),
      ?_test(test_ddos_detection()),
      ?_test(test_ddos_per_client_isolation())]}.

test_ddos_no_attack() ->
    ClientId = ddos_client1,
    ?assertNot(erlmcp_rate_limiter:is_ddos_attack(ClientId)).

test_ddos_detection() ->
    ClientId = ddos_client2,
    TimeNowMs = erlang:system_time(millisecond),
    _ = [erlmcp_rate_limiter:check_message_rate(ClientId, TimeNowMs) || _ <- lists:seq(1, 15)],
    IsAttack = erlmcp_rate_limiter:is_ddos_attack(ClientId),
    ?assert(IsAttack).

test_ddos_per_client_isolation() ->
    Client1 = ddos_client5a,
    Client2 = ddos_client5b,
    TimeNowMs = erlang:system_time(millisecond),
    _ = [erlmcp_rate_limiter:check_message_rate(Client1, TimeNowMs) || _ <- lists:seq(1, 15)],
    ?assert(erlmcp_rate_limiter:is_ddos_attack(Client1)),
    ?assertNot(erlmcp_rate_limiter:is_ddos_attack(Client2)).

%%--------------------------------------------------------------------
%% Graceful Degradation Tests
%%--------------------------------------------------------------------

graceful_degradation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [?_test(test_graceful_degradation_message_rate()),
      ?_test(test_graceful_degradation_connection_rate())]}.

test_graceful_degradation_message_rate() ->
    TimeNowMs = erlang:system_time(millisecond),
    Results1 =
        [erlmcp_rate_limiter:check_message_rate(client_degrad1, TimeNowMs)
         || _ <- lists:seq(1, 10)],
    Successes1 = length([ok || {ok, _} <- Results1]),
    ?assertEqual(10, Successes1),
    Results2 =
        [erlmcp_rate_limiter:check_message_rate(client_degrad1, TimeNowMs)
         || _ <- lists:seq(1, 10)],
    Successes2 = length([ok || {ok, _} <- Results2]),
    ?assert(Successes2 < 10).

test_graceful_degradation_connection_rate() ->
    TimeNowMs = erlang:system_time(millisecond),
    _ = [erlmcp_rate_limiter:check_connection_rate(client_degrad2, TimeNowMs)
         || _ <- lists:seq(1, 5)],
    Result = erlmcp_rate_limiter:check_connection_rate(client_degrad2, TimeNowMs),
    ?assertMatch({error, rate_limited, _}, Result),
    {error, rate_limited, RetryAfter} = Result,
    ?assert(RetryAfter > 0).
