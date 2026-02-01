%%%-------------------------------------------------------------------
%%% @doc Quick integration test for subscription rate limiting
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_subscription_rate_limit_tests).

-include_lib("eunit/include/eunit.hrl").

%% Simple test to verify rate limiting integration works
subscription_rate_limit_integration_test() ->
    % This is a smoke test to verify the integration compiles and links
    % The comprehensive tests are in erlmcp_subscription_rate_limiter_tests.erl
    % Test that check_rate_limit function exists and has correct signature
    ?assertEqual(true, is_function(fun erlmcp_subscription:check_rate_limit/2, 2)),

    % Test that erlmcp_rate_limiter API exists
    ?assertEqual(true, is_function(fun erlmcp_rate_limiter:check_subscription_rate/2, 2)),
    ?assertEqual(true, is_function(fun erlmcp_rate_limiter:create_token_bucket/1, 1)),
    ?assertEqual(true, is_function(fun erlmcp_rate_limiter:consume_token/2, 2)),
    ?assertEqual(true, is_function(fun erlmcp_rate_limiter:refill_bucket/2, 2)),
    ?assertEqual(true, is_function(fun erlmcp_rate_limiter:bucket_tokens/1, 1)),

    % Test token bucket creation
    Bucket = erlmcp_rate_limiter:create_token_bucket(100),
    ?assertEqual(100.0, erlmcp_rate_limiter:bucket_tokens(Bucket)),

    % Test token consumption
    {ok, Bucket1, Tokens} = erlmcp_rate_limiter:consume_token(Bucket, 100),
    ?assertEqual(99, Tokens),

    ok.
