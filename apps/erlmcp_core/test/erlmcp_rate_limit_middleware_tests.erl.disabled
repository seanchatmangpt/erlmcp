%%%-------------------------------------------------------------------
%% @doc Test Suite for erlmcp_rate_limit_middleware
%%
%% Tests for:
%% - Request interception
%% - Method-specific limits
%% - Priority-based limiting
%% - Client ID extraction
%% - Retry-After header injection
%% - Middleware configuration
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_rate_limit_middleware_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    application:set_env(erlmcp, rate_limiting, #{
        max_messages_per_sec => 10,
        enabled => true
    }),
    {ok, _} = erlmcp_rate_limiter:start_link(),
    {ok, _} = erlmcp_rate_limit_middleware:start_link(),
    ok.

cleanup(_) ->
    erlmcp_rate_limit_middleware:stop(),
    erlmcp_rate_limiter:stop().

%%====================================================================
%% Test Suites
%%====================================================================

middleware_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_start_stop()),
         ?_test(test_check_request()),
         ?_test(test_add_rate_limit()),
         ?_test(test_remove_rate_limit()),
         ?_test(test_get_rate_limits())
     ]}.

test_start_stop() ->
    Pid = whereis(erlmcp_rate_limit_middleware),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)).

test_check_request() ->
    ClientId = <<"test_client1">>,
    Method = <<"tools/call">>,
    TimeMs = erlang:system_time(millisecond),

    Result = erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs),
    ?assertMatch({ok, _}, Result).

test_add_rate_limit() ->
    Method = <<"custom/method">>,
    Config = #{
        max_rate => 5,
        scope => per_client
    },

    ok = erlmcp_rate_limit_middleware:add_rate_limit(Method, Config),

    Limits = erlmcp_rate_limit_middleware:get_rate_limits(),
    ?assert(maps:is_key(Method, Limits)).

test_remove_rate_limit() ->
    Method = <<"custom/method2">>,
    Config = #{max_rate => 5, scope => per_client},

    ok = erlmcp_rate_limit_middleware:add_rate_limit(Method, Config),
    ok = erlmcp_rate_limit_middleware:remove_rate_limit(Method),

    Limits = erlmcp_rate_limit_middleware:get_rate_limits(),
    ?assertNot(maps:is_key(Method, Limits)).

test_get_rate_limits() ->
    Limits = erlmcp_rate_limit_middleware:get_rate_limits(),
    ?assert(is_map(Limits)).

%%--------------------------------------------------------------------
%% Client ID Extraction Tests
%%--------------------------------------------------------------------

client_id_extraction_test_() ->
    [
        ?_test(test_extract_client_ip()),
        ?_test(test_extract_session_id()),
        ?_test(test_extract_unknown())
    ].

test_extract_client_ip() ->
    Request = #{<<"_client_ip">> => <<"192.168.1.1">>},
    ClientId = erlmcp_rate_limit_middleware:extract_client_id(Request),
    ?assertEqual(<<"192.168.1.1">>, ClientId).

test_extract_session_id() ->
    Request = #{<<"_session_id">> => <<"session_123">>},
    ClientId = erlmcp_rate_limit_middleware:extract_client_id(Request),
    ?assertEqual(<<"session_123">>, ClientId).

test_extract_unknown() ->
    Request = #{},
    ClientId = erlmcp_rate_limit_middleware:extract_client_id(Request),
    ?assertEqual(<<"unknown">>, ClientId).

%%--------------------------------------------------------------------
%% Retry-After Header Tests
%%--------------------------------------------------------------------

retry_after_injection_test_() ->
    [
        ?_test(test_inject_retry_after())
    ].

test_inject_retry_after() ->
    ErrorResponse = #{<<"error">> => <<"Rate limited">>},
    RetryAfterMs = 5000,

    Updated = erlmcp_rate_limit_middleware:inject_retry_after(ErrorResponse, RetryAfterMs),

    ?assert(maps:is_key(<<"_headers">>, Updated)),
    Headers = maps:get(<<"_headers">>, Updated),
    ?assertEqual(<<"5">>, maps:get(<<"Retry-After">>, Headers)).

%%--------------------------------------------------------------------
%% Priority-Based Limiting Tests
%%--------------------------------------------------------------------

priority_limiting_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_high_priority_bypass()),
         ?_test(test_normal_priority_limited()),
         ?_test(test_low_priority_limited())
     ]}.

test_high_priority_bypass() ->
    ClientId = <<"high_priority_client">>,
    Method = <<"tools/call">>,
    TimeMs = erlang:system_time(millisecond),

    % Set high priority
    erlmcp_rate_limiter:set_client_priority(ClientId, high),

    % Make many requests (should all succeed)
    Results = [erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs, high)
               || _ <- lists:seq(1, 100)],

    % All should succeed
    ?assert(lists:all(fun({ok, _}) -> true; (_) -> false end, Results)).

test_normal_priority_limited() ->
    ClientId = <<"normal_priority_client">>,
    Method = <<"tools/call">>,
    TimeMs = erlang:system_time(millisecond),

    % Make requests until limited
    Results = [erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs, normal)
               || _ <- lists:seq(1, 20)],

    % Some should fail
    Failures = [R || R <- Results, element(1, R) =:= error],
    ?assert(length(Failures) > 0).

test_low_priority_limited() ->
    ClientId = <<"low_priority_client">>,
    Method = <<"tools/call">>,
    TimeMs = erlang:system_time(millisecond),

    % Make requests until limited
    Results = [erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs, low)
               || _ <- lists:seq(1, 20)],

    % Should have failures
    Failures = [R || R <- Results, element(1, R) =:= error],
    ?assert(length(Failures) > 0).

%%--------------------------------------------------------------------
%% Method-Specific Limits Tests
%%--------------------------------------------------------------------

method_specific_limits_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         ?_test(test_different_methods_isolated()),
         ?_test(test_method_limit_override())
     ]}.

test_different_methods_isolated() ->
    ClientId = <<"method_test_client">>,
    Method1 = <<"tools/call">>,
    Method2 = <<"resources/read">>,
    TimeMs = erlang:system_time(millisecond),

    % Exhaust limit for Method1
    _ = [erlmcp_rate_limit_middleware:check_request(ClientId, Method1, TimeMs)
         || _ <- lists:seq(1, 20)],

    % Method2 should still work
    Result = erlmcp_rate_limit_middleware:check_request(ClientId, Method2, TimeMs),
    ?assertMatch({ok, _}, Result).

test_method_limit_override() ->
    Method = <<"custom/strict">>,
    Config = #{
        max_rate => 2,
        scope => per_client
    },

    ok = erlmcp_rate_limit_middleware:add_rate_limit(Method, Config),

    ClientId = <<"strict_client">>,
    TimeMs = erlang:system_time(millisecond),

    % First 2 should succeed
    R1 = erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs),
    R2 = erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2),

    % Third should fail
    R3 = erlmcp_rate_limit_middleware:check_request(ClientId, Method, TimeMs),
    ?assertMatch({error, rate_limited, _}, R3).
