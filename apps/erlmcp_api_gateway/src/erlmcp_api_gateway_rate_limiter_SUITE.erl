-module(erlmcp_api_gateway_rate_limiter_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [basic_rate_limiting, multiple_rate_limits, reset_rate_limit, rate_limiter_stats].

init_per_suite(Config) ->
    application:start(?MODULE),
    Config.

end_per_suite(_Config) ->
    application:stop(?MODULE).

basic_rate_limiting(_Config) ->
    ApiId = "test_api",
    ConsumerId = "test_consumer",
    Window = 1000,

    {ok, allowed} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, Window),
    {ok, allowed} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, Window),
    {error, rate_limited} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, Window).

multiple_rate_limits(_Config) ->
    ApiId1 = "api1",
    ApiId2 = "api2",
    ConsumerId = "consumer",
    Window = 1000,

    {ok, allowed} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId1, ConsumerId, Window),
    {ok, allowed} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId2, ConsumerId, Window),
    {error, rate_limited} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId1, ConsumerId, Window).

reset_rate_limit(_Config) ->
    ApiId = "test_api",
    ConsumerId = "test_consumer",
    Window = 1000,

    erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, Window),
    erlmcp_api_gateway_rate_limiter:reset_rate(ApiId, ConsumerId),
    {ok, allowed} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, Window).

rate_limiter_stats(_Config) ->
    ApiId = "stats_api",
    ConsumerId = "stats_consumer",
    Window = 1000,

    erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, Window),
    erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, Window),

    {ok, Stats} = erlmcp_api_gateway_rate_limiter:get_stats(ApiId),
    maps:is_key(active_consumers, Stats).