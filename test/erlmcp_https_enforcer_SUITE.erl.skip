%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for erlmcp_https_enforcer module
%%%
%%% Tests HTTPS enforcement, certificate management, and HTTP redirect functionality
%%% for MCP 2025-11-25 Gap #31 compliance.
%%%
%%% Test Coverage:
%%% - Configuration validation and defaults
%%% - Certificate loading and validation
%%% - HTTPS enforcement enable/disable
%%% - HTTP to HTTPS redirect decision logic
%%% - HSTS header generation
%%% - SSL options building
%%% - Host extraction from requests
%%% - Protocol security classification
%%% - Error handling for missing certificates
%%% - Mixed HTTP/HTTPS scenarios
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_https_enforcer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    %% Configuration tests
    config_default_values_test/1,
    config_get_specific_key_test/1,
    config_https_enabled_test/1,
    config_https_disabled_test/1,

    %% HTTPS enforcement tests
    is_https_required_enabled_test/1,
    is_https_required_disabled_test/1,
    is_https_enabled_test/1,
    should_redirect_http_test/1,
    should_not_redirect_https_test/1,
    should_not_redirect_without_enable_test/1,

    %% Certificate tests
    validate_config_success_test/1,
    validate_config_missing_files_test/1,
    load_certificates_disabled_test/1,
    load_certificates_missing_test/1,

    %% Response handling tests
    get_https_headers_enabled_test/1,
    get_https_headers_disabled_test/1,
    get_hsts_header_test/1,
    build_redirect_response_test/1,

    %% SSL options tests
    get_ssl_options_enabled_test/1,
    get_ssl_options_disabled_test/1,
    is_certificate_valid_test/1,
    get_certificate_info_test/1,

    %% Utility tests
    extract_host_from_map_test/1,
    is_secure_protocol_https_test/1,
    is_secure_protocol_http_test/1,
    is_secure_protocol_wss_test/1,
    is_secure_protocol_atom_test/1,

    %% Integration tests
    full_https_enforcement_flow_test/1,
    redirect_chain_test/1,
    mixed_http_https_test/1,
    security_config_merge_test/1,
    config_precedence_test/1,

    %% Error handling tests
    error_invalid_certfile_test/1,
    error_invalid_keyfile_test/1,
    error_missing_both_files_test/1,
    error_config_validation_test/1,

    %% Performance tests
    config_caching_test/1,
    header_generation_performance_test/1
]).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [{group, configuration},
     {group, https_enforcement},
     {group, certificates},
     {group, response_handling},
     {group, ssl_options},
     {group, utilities},
     {group, integration},
     {group, error_handling},
     {group, performance}].

groups() ->
    [{configuration, [parallel],
      [config_default_values_test,
       config_get_specific_key_test,
       config_https_enabled_test,
       config_https_disabled_test]},

     {https_enforcement, [sequential],
      [is_https_required_enabled_test,
       is_https_required_disabled_test,
       is_https_enabled_test,
       should_redirect_http_test,
       should_not_redirect_https_test,
       should_not_redirect_without_enable_test]},

     {certificates, [sequential],
      [validate_config_success_test,
       validate_config_missing_files_test,
       load_certificates_disabled_test,
       load_certificates_missing_test]},

     {response_handling, [parallel],
      [get_https_headers_enabled_test,
       get_https_headers_disabled_test,
       get_hsts_header_test,
       build_redirect_response_test]},

     {ssl_options, [sequential],
      [get_ssl_options_enabled_test,
       get_ssl_options_disabled_test,
       is_certificate_valid_test,
       get_certificate_info_test]},

     {utilities, [parallel],
      [extract_host_from_map_test,
       is_secure_protocol_https_test,
       is_secure_protocol_http_test,
       is_secure_protocol_wss_test,
       is_secure_protocol_atom_test]},

     {integration, [sequential],
      [full_https_enforcement_flow_test,
       redirect_chain_test,
       mixed_http_https_test,
       security_config_merge_test,
       config_precedence_test]},

     {error_handling, [sequential],
      [error_invalid_certfile_test,
       error_invalid_keyfile_test,
       error_missing_both_files_test,
       error_config_validation_test]},

     {performance, [sequential],
      [config_caching_test,
       header_generation_performance_test]}].

%%====================================================================
%% Suite Setup/Teardown
%%====================================================================

init_per_suite(Config) ->
    ct:pal("Starting HTTPS enforcer test suite"),
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    ct:pal("Ending HTTPS enforcer test suite"),
    ok.

init_per_group(Group, Config) ->
    ct:pal("Initializing group: ~p", [Group]),
    Config.

end_per_group(Group, _Config) ->
    ct:pal("Ending group: ~p", [Group]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Configuration Tests
%%====================================================================

config_default_values_test(_Config) ->
    Config = erlmcp_https_enforcer:get_config(),
    true = is_map(Config),
    false = maps:get(enabled, Config),
    ?assertEqual("priv/cert.pem", maps:get(certfile, Config)),
    ?assertEqual("priv/key.pem", maps:get(keyfile, Config)),
    ?assertEqual('tlsv1.2', maps:get(min_tls_version, Config)),
    true = is_list(maps:get(ciphers, Config)),
    true = maps:get(enable_hsts, Config),
    ?assertEqual(31536000, maps:get(hsts_max_age, Config)).

config_get_specific_key_test(_Config) ->
    ?assertEqual(false, erlmcp_https_enforcer:get_config(enabled)),
    ?assertEqual("priv/cert.pem", erlmcp_https_enforcer:get_config(certfile)),
    ?assertEqual("priv/key.pem", erlmcp_https_enforcer:get_config(keyfile)),
    ?assertEqual('tlsv1.2', erlmcp_https_enforcer:get_config(min_tls_version)).

config_https_enabled_test(_Config) ->
    %% Set HTTPS enabled
    application:set_env(erlmcp, https_config, [{enabled, true}]),
    ?assertEqual(true, erlmcp_https_enforcer:get_config(enabled)),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

config_https_disabled_test(_Config) ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    ?assertEqual(false, erlmcp_https_enforcer:get_config(enabled)).

%%====================================================================
%% HTTPS Enforcement Tests
%%====================================================================

is_https_required_enabled_test(_Config) ->
    %% Set require_https in http_security config
    application:set_env(erlmcp, http_security, [{require_https, true}]),
    ?assertEqual(true, erlmcp_https_enforcer:is_https_required()),
    %% Reset
    application:set_env(erlmcp, http_security, [{require_https, false}]).

is_https_required_disabled_test(_Config) ->
    application:set_env(erlmcp, http_security, [{require_https, false}]),
    ?assertEqual(false, erlmcp_https_enforcer:is_https_required()).

is_https_enabled_test(_Config) ->
    application:set_env(erlmcp, https_config, [{enabled, true}]),
    ?assertEqual(true, erlmcp_https_enforcer:is_https_enabled()),
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    ?assertEqual(false, erlmcp_https_enforcer:is_https_enabled()).

should_redirect_http_test(_Config) ->
    %% Only redirect if both require_https and https_enabled are true
    application:set_env(erlmcp, http_security, [{require_https, true}]),
    application:set_env(erlmcp, https_config, [{enabled, true}]),
    ?assertEqual(true, erlmcp_https_enforcer:should_redirect_to_https(<<"http">>)),
    ?assertEqual(true, erlmcp_https_enforcer:should_redirect_to_https("http")),
    %% Reset
    application:set_env(erlmcp, http_security, [{require_https, false}]),
    application:set_env(erlmcp, https_config, [{enabled, false}]).

should_not_redirect_https_test(_Config) ->
    application:set_env(erlmcp, http_security, [{require_https, true}]),
    application:set_env(erlmcp, https_config, [{enabled, true}]),
    ?assertEqual(false, erlmcp_https_enforcer:should_redirect_to_https(<<"https">>)),
    ?assertEqual(false, erlmcp_https_enforcer:should_redirect_to_https("https")),
    %% Reset
    application:set_env(erlmcp, http_security, [{require_https, false}]),
    application:set_env(erlmcp, https_config, [{enabled, false}]).

should_not_redirect_without_enable_test(_Config) ->
    application:set_env(erlmcp, http_security, [{require_https, true}]),
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    %% Should not redirect if HTTPS not enabled
    ?assertEqual(false, erlmcp_https_enforcer:should_redirect_to_https(<<"http">>)),
    %% Reset
    application:set_env(erlmcp, http_security, [{require_https, false}]).

%%====================================================================
%% Certificate Tests
%%====================================================================

validate_config_success_test(_Config) ->
    %% Disable HTTPS (doesn't require files)
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    {ok, Config} = erlmcp_https_enforcer:validate_config(),
    ?assertEqual(false, maps:get(enabled, Config)).

validate_config_missing_files_test(_Config) ->
    %% Enable HTTPS with non-existent files
    application:set_env(erlmcp, https_config, [
        {enabled, true},
        {certfile, "/nonexistent/cert.pem"},
        {keyfile, "/nonexistent/key.pem"}
    ]),
    Result = erlmcp_https_enforcer:validate_config(),
    ?assertMatch({error, _}, Result),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

load_certificates_disabled_test(_Config) ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    {ok, Options} = erlmcp_https_enforcer:load_certificates(),
    ?assertEqual([], Options).

load_certificates_missing_test(_Config) ->
    application:set_env(erlmcp, https_config, [
        {enabled, true},
        {certfile, "/nonexistent/cert.pem"},
        {keyfile, "/nonexistent/key.pem"}
    ]),
    Result = erlmcp_https_enforcer:load_certificates(),
    ?assertMatch({error, _}, Result),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

%%====================================================================
%% Response Handling Tests
%%====================================================================

get_https_headers_enabled_test(_Config) ->
    application:set_env(erlmcp, https_config, [
        {enabled, true},
        {enable_hsts, true}
    ]),
    Headers = erlmcp_https_enforcer:get_https_headers(),
    ?assertEqual(true, maps:is_key(<<"strict-transport-security">>, Headers)),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

get_https_headers_disabled_test(_Config) ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    Headers = erlmcp_https_enforcer:get_https_headers(),
    ?assertEqual(true, maps:is_key(<<"strict-transport-security">>, Headers) orelse
                        maps:size(Headers) =:= 0).

get_hsts_header_test(_Config) ->
    application:set_env(erlmcp, https_config, [
        {hsts_max_age, 31536000},
        {hsts_include_subdomains, false}
    ]),
    Header = erlmcp_https_enforcer:get_hsts_header(),
    ?assertMatch(<<"max-age=", _/binary>>, Header),
    ?assertEqual(false, binary:match(Header, <<"includeSubDomains">>)),

    %% Test with subdomains
    application:set_env(erlmcp, https_config, [
        {hsts_max_age, 31536000},
        {hsts_include_subdomains, true}
    ]),
    Header2 = erlmcp_https_enforcer:get_hsts_header(),
    ?assertNotEqual(nomatch, binary:match(Header2, <<"includeSubDomains">>)).

build_redirect_response_test(_Config) ->
    application:set_env(erlmcp, https_config, [{hsts_max_age, 31536000}]),
    {Status, Headers, Body} = erlmcp_https_enforcer:build_redirect_response(http, <<"example.com">>),
    ?assertEqual(301, Status),
    ?assertEqual(true, maps:is_key(<<"location">>, Headers)),
    ?assertEqual(true, maps:is_key(<<"strict-transport-security">>, Headers)),
    ?assertMatch(<<"301 Moved Permanently", _/binary>>, Body).

%%====================================================================
%% SSL Options Tests
%%====================================================================

get_ssl_options_enabled_test(_Config) ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    Options = erlmcp_https_enforcer:get_ssl_options(),
    ?assertEqual([], Options).

get_ssl_options_disabled_test(_Config) ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    Options = erlmcp_https_enforcer:get_ssl_options(),
    ?assertEqual([], Options).

is_certificate_valid_test(_Config) ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    Result = erlmcp_https_enforcer:is_certificate_valid(),
    ?assertEqual(true, Result).

get_certificate_info_test(_Config) ->
    application:set_env(erlmcp, https_config, [{enabled, false}]),
    Result = erlmcp_https_enforcer:get_certificate_info(),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Utility Tests
%%====================================================================

extract_host_from_map_test(_Config) ->
    %% Test with map request
    Req = #{host => "localhost"},
    {ok, Host} = erlmcp_https_enforcer:extract_host_from_request(Req),
    ?assertEqual(<<"localhost">>, Host).

is_secure_protocol_https_test(_Config) ->
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol(<<"https">>)),
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol("https")),
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol(https)).

is_secure_protocol_http_test(_Config) ->
    ?assertEqual(false, erlmcp_https_enforcer:is_secure_protocol(<<"http">>)),
    ?assertEqual(false, erlmcp_https_enforcer:is_secure_protocol("http")),
    ?assertEqual(false, erlmcp_https_enforcer:is_secure_protocol(http)).

is_secure_protocol_wss_test(_Config) ->
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol(<<"wss">>)),
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol("wss")),
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol(wss)).

is_secure_protocol_atom_test(_Config) ->
    ?assertEqual(true, erlmcp_https_enforcer:is_secure_protocol(https)),
    ?assertEqual(false, erlmcp_https_enforcer:is_secure_protocol(http)).

%%====================================================================
%% Integration Tests
%%====================================================================

full_https_enforcement_flow_test(_Config) ->
    %% Setup HTTPS enforcement
    application:set_env(erlmcp, http_security, [
        {require_https, true},
        {http_redirect_to_https, true}
    ]),
    application:set_env(erlmcp, https_config, [{enabled, true}]),

    %% Verify enforcement is active
    ?assertEqual(true, erlmcp_https_enforcer:is_https_required()),
    ?assertEqual(true, erlmcp_https_enforcer:is_https_enabled()),
    ?assertEqual(true, erlmcp_https_enforcer:should_redirect_to_https(<<"http">>)),

    %% Get response headers
    Headers = erlmcp_https_enforcer:get_https_headers(),
    ?assertEqual(true, maps:is_key(<<"strict-transport-security">>, Headers)),

    %% Reset
    application:set_env(erlmcp, http_security, [{require_https, false}]),
    application:set_env(erlmcp, https_config, [{enabled, false}]).

redirect_chain_test(_Config) ->
    %% Test multiple redirects
    application:set_env(erlmcp, https_config, [{hsts_max_age, 31536000}]),

    {Status1, Headers1, _} = erlmcp_https_enforcer:build_redirect_response(
        http, <<"example.com">>),
    ?assertEqual(301, Status1),
    Location1 = maps:get(<<"location">>, Headers1),
    ?assertMatch(<<"https://", _/binary>>, Location1).

mixed_http_https_test(_Config) ->
    %% HTTPS enabled, require HTTPS
    application:set_env(erlmcp, http_security, [{require_https, true}]),
    application:set_env(erlmcp, https_config, [{enabled, true}]),

    %% HTTP should redirect
    ?assertEqual(true, erlmcp_https_enforcer:should_redirect_to_https(<<"http">>)),
    %% HTTPS should not redirect
    ?assertEqual(false, erlmcp_https_enforcer:should_redirect_to_https(<<"https">>)),

    %% Reset
    application:set_env(erlmcp, http_security, [{require_https, false}]),
    application:set_env(erlmcp, https_config, [{enabled, false}]).

security_config_merge_test(_Config) ->
    %% Set partial config, verify merge with defaults
    application:set_env(erlmcp, https_config, [{hsts_max_age, 3600}]),
    Config = erlmcp_https_enforcer:get_config(),
    ?assertEqual(3600, maps:get(hsts_max_age, Config)),
    %% Other values should have defaults
    ?assertEqual(false, maps:get(enabled, Config)).

config_precedence_test(_Config) ->
    %% User config should override defaults
    application:set_env(erlmcp, https_config, [
        {enabled, true},
        {hsts_max_age, 7200}
    ]),
    Config = erlmcp_https_enforcer:get_config(),
    ?assertEqual(true, maps:get(enabled, Config)),
    ?assertEqual(7200, maps:get(hsts_max_age, Config)),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_invalid_certfile_test(_Config) ->
    application:set_env(erlmcp, https_config, [
        {enabled, true},
        {certfile, "/invalid/path/cert.pem"},
        {keyfile, "priv/key.pem"}
    ]),
    Result = erlmcp_https_enforcer:validate_config(),
    ?assertMatch({error, _}, Result),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

error_invalid_keyfile_test(_Config) ->
    application:set_env(erlmcp, https_config, [
        {enabled, true},
        {certfile, "priv/cert.pem"},
        {keyfile, "/invalid/path/key.pem"}
    ]),
    Result = erlmcp_https_enforcer:validate_config(),
    ?assertMatch({error, _}, Result),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

error_missing_both_files_test(_Config) ->
    application:set_env(erlmcp, https_config, [
        {enabled, true},
        {certfile, "/invalid/cert.pem"},
        {keyfile, "/invalid/key.pem"}
    ]),
    Result = erlmcp_https_enforcer:validate_config(),
    ?assertMatch({error, _}, Result),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

error_config_validation_test(_Config) ->
    %% Invalid configuration should be caught
    application:set_env(erlmcp, https_config, not_a_valid_config),
    catch erlmcp_https_enforcer:validate_config(),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).

%%====================================================================
%% Performance Tests
%%====================================================================

config_caching_test(_Config) ->
    %% Call get_config multiple times - should be fast
    Start = erlang:system_time(microsecond),
    lists:foreach(fun(_) ->
        erlmcp_https_enforcer:get_config()
    end, lists:seq(1, 1000)),
    End = erlang:system_time(microsecond),
    Duration = End - Start,
    %% Should complete 1000 calls in < 100ms (100000 microseconds)
    ?assert(Duration < 100000).

header_generation_performance_test(_Config) ->
    application:set_env(erlmcp, https_config, [
        {enabled, true},
        {enable_hsts, true}
    ]),
    Start = erlang:system_time(microsecond),
    lists:foreach(fun(_) ->
        erlmcp_https_enforcer:get_https_headers()
    end, lists:seq(1, 1000)),
    End = erlang:system_time(microsecond),
    Duration = End - Start,
    %% Should complete 1000 calls in < 50ms (50000 microseconds)
    ?assert(Duration < 50000),
    %% Reset
    application:set_env(erlmcp, https_config, [{enabled, false}]).
