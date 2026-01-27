%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_https_enforcer module
%%%
%%% Provides additional unit tests for HTTPS enforcement functionality
%%% complementing the CT suite.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_https_enforcer_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Configuration Tests
%%====================================================================

get_config_test() ->
    Config = erlmcp_https_enforcer:get_config(),
    ?assertMatch(#{enabled := _, certfile := _, keyfile := _}, Config).

get_config_specific_test() ->
    Enabled = erlmcp_https_enforcer:get_config(enabled),
    ?assertEqual(false, Enabled),
    CertFile = erlmcp_https_enforcer:get_config(certfile),
    ?assertEqual("priv/cert.pem", CertFile).

%%====================================================================
%% HTTPS Enforcement Tests
%%====================================================================

is_https_required_test() ->
    Result = erlmcp_https_enforcer:is_https_required(),
    ?assertEqual(false, Result).

is_https_enabled_test() ->
    Result = erlmcp_https_enforcer:is_https_enabled(),
    ?assertEqual(false, Result).

should_redirect_to_https_test() ->
    application:set_env(erlmcp, http_security, [{require_https, true}]),
    application:set_env(erlmcp, https_config, [{enabled, true}]),

    ?assertEqual(true, erlmcp_https_enforcer:should_redirect_to_https(<<"http">>)),
    ?assertEqual(true, erlmcp_https_enforcer:should_redirect_to_https("http")),
    ?assertEqual(false, erlmcp_https_enforcer:should_redirect_to_https(<<"https">>)),

    %% Reset
    application:set_env(erlmcp, http_security, [{require_https, false}]),
    application:set_env(erlmcp, https_config, [{enabled, false}]).

%%====================================================================
%% Response Tests
%%====================================================================

get_https_headers_test() ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    Headers = erlmcp_https_enforcer:get_https_headers(),
    ?assertMatch(#{}, Headers).

get_hsts_header_format_test() ->
    application:set_env(erlmcp, https_config, [{hsts_max_age, 31536000}]),
    Header = erlmcp_https_enforcer:get_hsts_header(),
    ?assertMatch(<<"max-age=31536000", _/binary>>, Header).

build_redirect_response_test() ->
    {Status, Headers, Body} = erlmcp_https_enforcer:build_redirect_response(
        http, <<"example.com">>),
    ?assertEqual(301, Status),
    ?assertMatch(#{<<"location">> := _, <<"content-type">> := _}, Headers),
    ?assertMatch(<<"301", _/binary>>, Body).

%%====================================================================
%% Certificate Tests
%%====================================================================

validate_config_when_disabled_test() ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    {ok, _} = erlmcp_https_enforcer:validate_config().

load_certificates_when_disabled_test() ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    {ok, Options} = erlmcp_https_enforcer:load_certificates(),
    ?assertEqual([], Options).

is_certificate_valid_when_disabled_test() ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    Result = erlmcp_https_enforcer:is_certificate_valid(),
    ?assertEqual(true, Result).

%%====================================================================
%% Utility Tests
%%====================================================================

is_secure_protocol_https_test() ->
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol(<<"https">>)),
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol("https")),
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol(https)).

is_secure_protocol_http_test() ->
    ?assertEqual(false, erlmcp_https_enforcer:is_secure_protocol(<<"http">>)),
    ?assertEqual(false, erlmcp_https_enforcer:is_secure_protocol("http")),
    ?assertEqual(false, erlmcp_https_enforcer:is_secure_protocol(http)).

is_secure_protocol_wss_test() ->
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol(<<"wss">>)),
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol("wss")).

is_secure_protocol_h2_test() ->
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol(<<"h2">>)).

is_secure_protocol_h2c_test() ->
    ?assertEqual(false, erlmcp_https_enforcer:is_secure_protocol(<<"h2c">>)).

extract_host_from_map_test() ->
    Req = #{host => "localhost:8080"},
    {ok, Host} = erlmcp_https_enforcer:extract_host_from_request(Req),
    ?assertEqual(<<"localhost:8080">>, Host).

%%====================================================================
%% Integration Tests
%%====================================================================

enforce_https_integration_test() ->
    %% Setup HTTPS enforcement
    application:set_env(erlmcp, http_security, [{require_https, true}]),
    application:set_env(erlmcp, https_config, [{enabled, true}]),

    %% Verify the chain
    ?assertEqual(true, erlmcp_https_enforcer:is_https_required()),
    ?assertEqual(true, erlmcp_https_enforcer:is_https_enabled()),

    %% HTTP should redirect
    ?assertEqual(true, erlmcp_https_enforcer:should_redirect_to_https(<<"http">>)),

    %% HTTPS should not redirect
    ?assertEqual(false, erlmcp_https_enforcer:should_redirect_to_https(<<"https">>)),

    %% Reset
    application:set_env(erlmcp, http_security, [{require_https, false}]),
    application:set_env(erlmcp, https_config, [{enabled, false}]).

redirect_response_contains_hsts_test() ->
    application:set_env(erlmcp, https_config, [{enable_hsts, true}]),
    {_Status, Headers, _Body} = erlmcp_https_enforcer:build_redirect_response(
        http, <<"example.com">>),
    ?assertEqual(true, maps:is_key(<<"strict-transport-security">>, Headers)).

%%====================================================================
%% Error Cases
%%====================================================================

validate_missing_cert_test() ->
    application:set_env(erlmcp, https_config, [
        {enabled, true},
        {certfile, "/nonexistent/cert.pem"},
        {keyfile, "/nonexistent/key.pem"}
    ]),
    Result = erlmcp_https_enforcer:validate_config(),
    ?assertMatch({error, _}, Result),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

%%====================================================================
%% Edge Cases
%%====================================================================

redirect_response_with_binary_host_test() ->
    {Status, Headers, _Body} = erlmcp_https_enforcer:build_redirect_response(
        http, <<"example.com:8080">>),
    ?assertEqual(301, Status),
    Location = maps:get(<<"location">>, Headers),
    ?assertMatch(<<"https://", _/binary>>, Location).

redirect_response_with_string_host_test() ->
    {Status, Headers, _Body} = erlmcp_https_enforcer:build_redirect_response(
        http, "example.com"),
    ?assertEqual(301, Status),
    Location = maps:get(<<"location">>, Headers),
    ?assertMatch(<<"https://", _/binary>>, Location).

hsts_header_without_subdomains_test() ->
    application:set_env(erlmcp, https_config, [
        {hsts_max_age, 3600},
        {hsts_include_subdomains, false}
    ]),
    Header = erlmcp_https_enforcer:get_hsts_header(),
    ?assertEqual(nomatch, binary:match(Header, <<"includeSubDomains">>)).

hsts_header_with_subdomains_test() ->
    application:set_env(erlmcp, https_config, [
        {hsts_max_age, 3600},
        {hsts_include_subdomains, true}
    ]),
    Header = erlmcp_https_enforcer:get_hsts_header(),
    ?assertNotEqual(nomatch, binary:match(Header, <<"includeSubDomains">>)).

%%====================================================================
%% Configuration Precedence Tests
%%====================================================================

config_merge_with_defaults_test() ->
    application:set_env(erlmcp, https_config, [
        {hsts_max_age, 7200}
    ]),
    Config = erlmcp_https_enforcer:get_config(),
    %% Custom value should be present
    ?assertEqual(7200, maps:get(hsts_max_age, Config)),
    %% Default values should be present too
    ?assertEqual("priv/cert.pem", maps:get(certfile, Config)).

config_empty_env_uses_defaults_test() ->
    application:unset_env(erlmcp, https_config),
    Config = erlmcp_https_enforcer:get_config(),
    %% All defaults should be present
    ?assertEqual(false, maps:get(enabled, Config)),
    ?assertEqual("priv/cert.pem", maps:get(certfile, Config)).
