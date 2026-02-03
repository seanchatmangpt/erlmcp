-module(erlmcp_api_gateway_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [api_registry_operations, rate_limiting_flow, authentication_flow,
     health_check].

init_per_suite(Config) ->
    application:start(?MODULE),
    Config.

end_per_suite(_Config) ->
    application:stop(?MODULE).

api_registry_operations(_Config) ->
    Api = #{
        id => "test_api_123",
        name => "Test API",
        description => "API for testing",
        upstream_url => "http://localhost:8080",
        rate_limit => #{<<"requests">> => 1000, <<"window">> => <<"1m">>},
        enabled => true
    },

    {ok, CreatedApi} = erlmcp_api_gateway_registry:create_api(Api),
    CreatedApiId = maps:get(id, CreatedApi),
    CreatedApiId =:= Api#{id},

    {ok, RetrievedApi} = erlmcp_api_gateway_registry:get_api(CreatedApiId),
    maps:get(name, RetrievedApi) =:= "Test API",

    UpdatedApi = maps:put(<<"description">>, "Updated description", Api#{id}),
    {ok, UpdatedRetrievedApi} = erlmcp_api_gateway_registry:update_api(UpdatedApi),
    maps:get(<<"description">>, UpdatedRetrievedApi) =:= "Updated description",

    ok = erlmcp_api_gateway_registry:delete_api(CreatedApiId),
    {error, not_found} = erlmcp_api_gateway_registry:get_api(CreatedApiId).

rate_limiting_flow(_Config) ->
    Consumer = #{
        id => "consumer_123",
        username => "test_consumer",
        custom_id => "customer_123",
        tags => [<<"premium">>]
    },

    {ok, CreatedConsumer} = erlmcp_api_gateway_registry:create_consumer(Consumer),
    ConsumerId = maps:get(id, CreatedConsumer),

    ApiId = "api_123",
    {ok, allowed} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, 1000),
    {ok, allowed} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, 1000),
    {error, rate_limited} = erlmcp_api_gateway_rate_limiter:check_rate(ApiId, ConsumerId, 1000).

authentication_flow(_Config) ->
    Consumer = #{
        id => "auth_consumer_123",
        username => "auth_test",
        custom_id => "customer_auth",
        tags => [<<"enterprise">>]
    },

    {ok, CreatedConsumer} = erlmcp_api_gateway_registry:create_consumer(Consumer),
    ConsumerId = maps:get(id, CreatedConsumer),

    ApiId = "auth_api_123",
    Token = generate_test_token(ConsumerId, ApiId),

    {ok, authorized} = erlmcp_api_gateway_auth:authorize(ApiId, ConsumerId, Token).

health_check(_Config) ->
    {ok, Health} = erlmcp_api_gateway_monitor:get_health(),
    maps:is_key(status, Health),
    maps:is_key(score, Health).

generate_test_token(ConsumerId, ApiId) ->
    Payload = #{
        <<"consumer_id">> => ConsumerId,
        <<"api_id">> => ApiId,
        <<"exp">> => erlang:system_time(second) + 3600
    },
    erlmcp_api_gateway_auth:generate_token(Payload, "test_secret").