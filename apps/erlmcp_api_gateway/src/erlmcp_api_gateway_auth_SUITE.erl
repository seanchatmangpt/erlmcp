-module(erlmcp_api_gateway_auth_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [basic_auth, jwt_token_validation, oauth_flow, revoke_token].

init_per_suite(Config) ->
    application:start(?MODULE),
    Config.

end_per_suite(_Config) ->
    application:stop(?MODULE).

basic_auth(_Config) ->
    {ok, Claims} = erlmcp_api_gateway_auth:generate_token(#{<<"sub">> => "user123"}, "secret"),
    {ok, Verified} = erlmcp_api_gateway_auth:validate_token(Claims),
    true.

jwt_token_validation(_Config) ->
    Payload = #{<<"sub">> => "user123", <<"iat">> => erlang:system_time(second)},
    {ok, Token} = erlmcp_api_gateway_auth:generate_token(Payload, "secret"),
    {ok, Claims} = erlmcp_api_gateway_auth:validate_token(Token),
    maps:get(<<"sub">>, Claims) =:= "user123".

oauth_flow(_Config) ->
    ConsumerSpec = #{<<"custom_id">> => "customer_123", <<"tags">> => [<<"pro">>]},
    {ok, Consumer} = erlmcp_api_gateway_auth:generate_consumer(ConsumerSpec),
    maps:get(<<"custom_id">>, Consumer) =:= <<"customer_123">>.

revoke_token(_Config) ->
    Payload = #{<<"sub">> => "user123"},
    {ok, Token} = erlmcp_api_gateway_auth:generate_token(Payload, "secret"),
    {ok, _Claims} = erlmcp_api_gateway_auth:validate_token(Token),
    ok = erlmcp_api_gateway_auth:revoke_token(Token),
    {error, _} = erlmcp_api_gateway_auth:validate_token(Token).