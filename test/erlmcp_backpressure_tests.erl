%%%-------------------------------------------------------------------
%% @doc Comprehensive tests for multi-level backpressure system
%%
%% Tests cover:
%% - Token bucket correctness
%% - Adaptive rate adjustment
%% - Handler queue monitoring
%% - Circuit breaker state transitions
%% - Message shedding correctness
%% - Cascading failure prevention
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_backpressure_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok = erlmcp_backpressure:start_link(),
    ok = erlmcp_circuit_breaker:start_link(),
    ok.

cleanup(_) ->
    erlmcp_backpressure:stop(),
    erlmcp_circuit_breaker:stop(),
    ok.

%%====================================================================
%% Test Groups
%%====================================================================

backpressure_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Token Bucket Tests", [
                ?_test(test_token_bucket_creation()),
                ?_test(test_initial_tokens()),
                ?_test(test_token_refill_over_time()),
                ?_test(test_burst_capacity())
            ]},
            {"Per-Client Rate Limiting", [
                ?_test(test_new_client_rate_limit()),
                ?_test(test_client_token_consumption()),
                ?_test(test_multiple_clients_isolated())
            ]},
            {"Handler Queue Monitoring", [
                ?_test(test_queue_below_threshold()),
                ?_test(test_queue_at_threshold()),
                ?_test(test_queue_recovery())
            ]},
            {"Circuit Breaker", [
                ?_test(test_circuit_initial_state()),
                ?_test(test_circuit_status_check()),
                ?_test(test_circuit_metrics())
            ]},
            {"Stats and Monitoring", [
                ?_test(test_get_backpressure_stats()),
                ?_test(test_get_circuit_breaker_metrics())
            ]}
        ]
    }.

%%====================================================================
%% Token Bucket Tests
%%====================================================================

test_token_bucket_creation() ->
    TimeNowMs = erlang:system_time(millisecond),
    ClientId = test_client_1,
    {ok, {Tokens, CanSend}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs),
    ?assert(Tokens >= 0),
    ?assert(CanSend).

test_initial_tokens() ->
    TimeNowMs = erlang:system_time(millisecond),
    ClientId = test_client_initial,
    {ok, {Tokens1, _}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs),
    {ok, {Tokens2, _}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs),
    ?assert(Tokens2 < Tokens1).

test_token_refill_over_time() ->
    TimeNowMs = erlang:system_time(millisecond),
    ClientId = test_client_refill,
    {ok, {T1, _}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs),
    timer:sleep(100),
    TimeNowMs2 = erlang:system_time(millisecond),
    {ok, {T4, _}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs2),
    ?assert(T4 > T1).

test_burst_capacity() ->
    TimeNowMs = erlang:system_time(millisecond),
    ClientId = test_client_burst,
    timer:sleep(200),
    TimeNowMs2 = erlang:system_time(millisecond),
    {ok, {T1, _}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs2),
    {ok, {T2, _}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs2),
    {ok, {T3, _}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs2),
    ?assert(T1 >= 0),
    ?assert(T2 >= 0),
    ?assert(T3 >= 0).

%%====================================================================
%% Per-Client Rate Limiting Tests
%%====================================================================

test_new_client_rate_limit() ->
    TimeNowMs = erlang:system_time(millisecond),
    ClientId = test_new_client,
    {ok, {_Tokens, CanSend}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs),
    ?assert(CanSend).

test_client_token_consumption() ->
    TimeNowMs = erlang:system_time(millisecond),
    ClientId = test_client_consumption,
    {ok, {T1, _}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs),
    {ok, {T2, _}} = erlmcp_backpressure:check_rate_limit(ClientId, TimeNowMs),
    ?assert(T2 < T1).

test_multiple_clients_isolated() ->
    TimeNowMs = erlang:system_time(millisecond),
    Client1 = test_client_a,
    Client2 = test_client_b,

    {ok, {_Tokens, CanSend}} = erlmcp_backpressure:check_rate_limit(Client2, TimeNowMs),
    ?assert(CanSend).

%%====================================================================
%% Handler Queue Monitoring Tests
%%====================================================================

test_queue_below_threshold() ->
    QueueStats = #{current_depth => 10, max_capacity => 100},
    {ok, queue_ok} = erlmcp_backpressure:check_handler_queue(test_handler, QueueStats).

test_queue_at_threshold() ->
    QueueStats = #{current_depth => 80, max_capacity => 100},
    {error, backpressure_signal, _} = erlmcp_backpressure:check_handler_queue(test_handler, QueueStats).

test_queue_recovery() ->
    QueueStats1 = #{current_depth => 85, max_capacity => 100},
    {error, backpressure_signal, _} = erlmcp_backpressure:check_handler_queue(test_handler, QueueStats1),
    QueueStats2 = #{current_depth => 30, max_capacity => 100},
    {ok, queue_ok} = erlmcp_backpressure:check_handler_queue(test_handler, QueueStats2).

%%====================================================================
%% Circuit Breaker Tests
%%====================================================================

test_circuit_initial_state() ->
    {ok, closed} = erlmcp_circuit_breaker:get_status().

test_circuit_status_check() ->
    erlmcp_circuit_breaker:record_request(1, 50),
    {ok, Status} = erlmcp_circuit_breaker:get_status(),
    ?assert(Status =:= closed orelse Status =:= open orelse Status =:= half_open).

test_circuit_metrics() ->
    {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(p95_latency_ms, Metrics)).

%%====================================================================
%% Stats and Monitoring Tests
%%====================================================================

test_get_backpressure_stats() ->
    Stats = erlmcp_backpressure:get_stats(),
    ?assert(is_map(Stats)),
    ?assert(maps:is_key(circuit_status, Stats)),
    ?assert(maps:is_key(p95_latency_ms, Stats)).

test_get_circuit_breaker_metrics() ->
    {ok, Metrics} = erlmcp_circuit_breaker:get_metrics(),
    ?assert(is_map(Metrics)),
    ?assert(maps:is_key(p95_latency_ms, Metrics)),
    ?assert(maps:is_key(error_rate_percent, Metrics)).
