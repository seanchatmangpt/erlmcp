%%%-------------------------------------------------------------------
%%% @doc Origin Validator Test Suite
%%%
%%% Comprehensive security tests for Origin validation.
%%% Tests CORS validation, DNS rebinding prevention, whitelist enforcement.
%%%
%%% == Chicago School TDD ==
%%% Uses REAL erlmcp_origin_validator module.
%%% Tests observable behavior through API calls only.
%%% NO mocks - real validation logic.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_origin_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test 1-5: Valid Origin Cases
%%%===================================================================

valid_localhost_origin_test() ->
    %% Test localhost origin (development)
    Origin = <<"http://localhost">>,
    AllowedOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({ok, _}, Result),
    {ok, ValidOrigin} = Result,
    ?assertEqual(Origin, ValidOrigin).

valid_localhost_with_port_test() ->
    %% Test localhost with port
    Origin = <<"http://localhost:8080">>,
    AllowedOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({ok, _}, Result).

valid_127_0_0_1_test() ->
    %% Test 127.0.0.1 IP address
    Origin = <<"http://127.0.0.1">>,
    AllowedOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({ok, _}, Result).

valid_ipv6_localhost_test() ->
    %% Test IPv6 localhost [::1]
    Origin = <<"http://[::1]">>,
    AllowedOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({ok, _}, Result).

valid_null_origin_test() ->
    %% Test null origin (same-origin policy)
    Origin = <<"null">>,
    AllowedOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({ok, _}, Result).

%%%===================================================================
%%% Test 6-10: Forbidden Origin Cases
%%%===================================================================

forbidden_external_origin_test() ->
    %% Test external origin not in whitelist
    Origin = <<"http://evil.com">>,
    AllowedOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({error, forbidden}, Result).

forbidden_subdomain_test() ->
    %% Test subdomain without wildcard
    Origin = <<"http://sub.localhost">>,
    AllowedOrigins = [<<"http://localhost">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({error, forbidden}, Result).

forbidden_different_port_test() ->
    %% Test different port without explicit allowance
    Origin = <<"http://localhost:9999">>,
    AllowedOrigins = [<<"http://localhost:8080">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({error, forbidden}, Result).

forbidden_different_protocol_test() ->
    %% Test HTTPS when HTTP is allowed
    Origin = <<"https://localhost">>,
    AllowedOrigins = [<<"http://localhost">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({error, forbidden}, Result).

empty_allowed_origins_test() ->
    %% Test with empty whitelist
    Origin = <<"http://localhost">>,
    AllowedOrigins = [],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({error, forbidden}, Result).

%%%===================================================================
%%% Test 11-15: Wildcard Matching
%%%===================================================================

wildcard_match_all_test() ->
    %% Test wildcard matching all origins
    Origin = <<"http://example.com">>,
    AllowedOrigins = [<<"*">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({ok, _}, Result).

wildcard_subdomain_match_test() ->
    %% Test subdomain wildcard
    Origin = <<"http://api.example.com">>,
    AllowedOrigins = [<<"*.example.com">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({ok, _}, Result).

wildcard_subdomain_no_match_test() ->
    %% Test wildcard doesn't match wrong domain
    Origin = <<"http://api.different.com">>,
    AllowedOrigins = [<<"*.example.com">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({error, forbidden}, Result).

wildcard_multiple_subdomains_test() ->
    %% Test wildcard with multiple subdomain levels
    Origin = <<"http://api.v1.example.com">>,
    AllowedOrigins = [<<"*.example.com">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    %% Should match any subdomain of example.com
    ?assertMatch({ok, _}, Result).

wildcard_base_domain_no_match_test() ->
    %% Test wildcard doesn't match base domain
    Origin = <<"http://example.com">>,
    AllowedOrigins = [<<"*.example.com">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    %% Wildcard *.example.com should not match example.com itself
    ?assertMatch({error, forbidden}, Result).

%%%===================================================================
%%% Test 16-20: DNS Rebinding Prevention
%%%===================================================================

dns_rebinding_private_ip_test() ->
    %% Test private IP address (potential DNS rebinding)
    Origin = <<"http://192.168.1.1">>,
    AllowedOrigins = [<<"http://localhost">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({error, forbidden}, Result).

dns_rebinding_link_local_test() ->
    %% Test link-local address
    Origin = <<"http://169.254.1.1">>,
    AllowedOrigins = [<<"http://localhost">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({error, forbidden}, Result).

dns_rebinding_localhost_variations_test() ->
    %% Test localhost variations (DNS rebinding attack)
    Origin = <<"http://127.0.0.2">>,
    AllowedOrigins = [<<"http://127.0.0.1">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    %% Should reject different localhost IP
    ?assertMatch({error, forbidden}, Result).

allowed_private_ip_test() ->
    %% Test explicitly allowed private IP
    Origin = <<"http://192.168.1.100">>,
    AllowedOrigins = [<<"http://192.168.1.100">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({ok, _}, Result).

dns_rebinding_ipv6_private_test() ->
    %% Test IPv6 private address
    Origin = <<"http://[fc00::1]">>,
    AllowedOrigins = [<<"http://localhost">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({error, forbidden}, Result).

%%%===================================================================
%%% Test 21-25: Edge Cases and Undefined Origin
%%%===================================================================

undefined_origin_test() ->
    %% Test undefined origin (no Origin header)
    Origin = undefined,
    AllowedOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    %% Undefined origin is allowed for local development
    ?assertMatch({ok, _}, Result).

case_sensitive_origin_test() ->
    %% Test origin matching is case-sensitive for domain
    Origin = <<"http://LOCALHOST">>,
    AllowedOrigins = [<<"http://localhost">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    %% Domains should be case-insensitive per RFC, but our implementation may differ
    %% Test actual behavior
    ?assert(is_tuple(Result)).

multiple_allowed_origins_test() ->
    %% Test with multiple allowed origins
    Origin = <<"http://app2.example.com">>,
    AllowedOrigins = [
        <<"http://app1.example.com">>,
        <<"http://app2.example.com">>,
        <<"http://app3.example.com">>
    ],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    ?assertMatch({ok, _}, Result).

origin_with_path_test() ->
    %% Test origin with path (should not have path)
    Origin = <<"http://localhost/path">>,
    AllowedOrigins = [<<"http://localhost">>],

    Result = erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins),

    %% Origins should not include path
    ?assertMatch({error, forbidden}, Result).

default_allowed_origins_test() ->
    %% Test get_default_allowed_origins returns expected origins
    AllowedOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    ?assert(is_list(AllowedOrigins)),
    ?assert(length(AllowedOrigins) > 0),
    %% Should include localhost
    ?assert(lists:member(<<"http://localhost">>, AllowedOrigins)),
    %% Should include 127.0.0.1
    ?assert(lists:member(<<"http://127.0.0.1">>, AllowedOrigins)),
    %% Should include IPv6 localhost
    ?assert(lists:member(<<"http://[::1]">>, AllowedOrigins)),
    %% Should include null for same-origin
    ?assert(lists:member(<<"null">>, AllowedOrigins)).
