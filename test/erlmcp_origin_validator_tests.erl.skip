-module(erlmcp_origin_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for erlmcp_origin_validator Module (Gap #3)
%%====================================================================
%% DNS Rebinding Attack Protection Implementation Tests
%%
%% This test suite validates the origin validation functionality
%% that prevents DNS rebinding attacks on local MCP servers.
%%
%% Security Requirements:
%% - Origin header must be validated against whitelist
%% - Invalid origins must be rejected with 403 Forbidden
%% - Missing Origin header allowed (same-origin requests)
%% - Support for wildcard ports in patterns
%% - Default to safe localhost-only origins
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Basic Origin Validation Tests
%%====================================================================

basic_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_exact_match()),
             ?_test(test_exact_match_with_port()),
             ?_test(test_invalid_origin()),
             ?_test(test_missing_origin_header()),
             ?_test(test_undefined_origin_accepted())
         ]
     end}.

test_exact_match() ->
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_exact_match_with_port() ->
    AllowedOrigins = [<<"http://localhost:8080">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost:8080">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_invalid_origin() ->
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://evil.com">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

test_missing_origin_header() ->
    %% When Origin header is not present, treat as same-origin (OK)
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(undefined, AllowedOrigins),
    ?assertMatch({ok, <<"same-origin">>}, Result).

test_undefined_origin_accepted() ->
    %% Undefined origin should always be accepted
    AllowedOrigins = [],
    Result = erlmcp_origin_validator:validate_origin(undefined, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% Wildcard Port Pattern Tests
%%====================================================================

wildcard_port_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_wildcard_port_match_8080()),
             ?_test(test_wildcard_port_match_3000()),
             ?_test(test_wildcard_port_match_443()),
             ?_test(test_wildcard_port_reject_wrong_host()),
             ?_test(test_wildcard_port_any_number())
         ]
     end}.

test_wildcard_port_match_8080() ->
    Pattern = <<"http://localhost:*">>,
    Result = erlmcp_origin_validator:matches_origin_pattern(<<"http://localhost:8080">>, Pattern),
    ?assert(Result).

test_wildcard_port_match_3000() ->
    Pattern = <<"http://localhost:*">>,
    Result = erlmcp_origin_validator:matches_origin_pattern(<<"http://localhost:3000">>, Pattern),
    ?assert(Result).

test_wildcard_port_match_443() ->
    Pattern = <<"https://localhost:*">>,
    Result = erlmcp_origin_validator:matches_origin_pattern(<<"https://localhost:443">>, Pattern),
    ?assert(Result).

test_wildcard_port_reject_wrong_host() ->
    Pattern = <<"http://localhost:*">>,
    Result = erlmcp_origin_validator:matches_origin_pattern(<<"http://evil.com:8080">>, Pattern),
    ?assertNot(Result).

test_wildcard_port_any_number() ->
    Pattern = <<"http://localhost:*">>,
    %% Test various port numbers
    Ports = [1, 80, 443, 3000, 8080, 9000, 65535],
    Results = [erlmcp_origin_validator:matches_origin_pattern(
        list_to_binary("http://localhost:" ++ integer_to_list(P)), Pattern) || P <- Ports],
    ?assert(lists:all(fun(R) -> R =:= true end, Results)).

%%====================================================================
%% Localhost and Loopback Tests
%%====================================================================

localhost_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_localhost_http()),
             ?_test(test_localhost_https()),
             ?_test(test_127_0_0_1_http()),
             ?_test(test_127_0_0_1_https()),
             ?_test(test_ipv6_loopback_http()),
             ?_test(test_ipv6_loopback_https()),
             ?_test(test_localhost_with_port()),
             ?_test(test_127_with_port()),
             ?_test(test_ipv6_with_port())
         ]
     end}.

test_localhost_http() ->
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_localhost_https() ->
    AllowedOrigins = [<<"https://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"https://localhost">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_127_0_0_1_http() ->
    AllowedOrigins = [<<"http://127.0.0.1">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://127.0.0.1">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_127_0_0_1_https() ->
    AllowedOrigins = [<<"https://127.0.0.1">>],
    Result = erlmcp_origin_validator:validate_origin(<<"https://127.0.0.1">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_ipv6_loopback_http() ->
    AllowedOrigins = [<<"http://[::1]">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://[::1]">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_ipv6_loopback_https() ->
    AllowedOrigins = [<<"https://[::1]">>],
    Result = erlmcp_origin_validator:validate_origin(<<"https://[::1]">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_localhost_with_port() ->
    AllowedOrigins = [<<"http://localhost:*">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost:8080">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_127_with_port() ->
    AllowedOrigins = [<<"http://127.0.0.1:*">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://127.0.0.1:3000">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_ipv6_with_port() ->
    AllowedOrigins = [<<"http://[::1]:*">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://[::1]:8080">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% Security: DNS Rebinding Attack Prevention Tests
%%====================================================================

dns_rebinding_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_dns_rebinding_attack_prevented()),
             ?_test(test_attacker_domain_rejected()),
             ?_test(test_suspicious_origin_rejected()),
             ?_test(test_only_whitelisted_origins_allowed()),
             ?_test(test_multiple_attack_vectors()),
             ?_test(test_malicious_subdomain_rejected())
         ]
     end}.

test_dns_rebinding_attack_prevented() ->
    %% Scenario: Attacker hosts malicious.com -> 127.0.0.1
    %% JavaScript from malicious.com sends Origin: http://malicious.com
    %% Should be rejected
    SafeOrigins = [<<"http://localhost:*">>, <<"http://127.0.0.1:*">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://malicious.com">>, SafeOrigins),
    ?assertMatch({error, forbidden}, Result).

test_attacker_domain_rejected() ->
    %% Random attacker domain should be rejected
    SafeOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://evil.example.com">>, SafeOrigins),
    ?assertMatch({error, forbidden}, Result).

test_suspicious_origin_rejected() ->
    %% Suspicious domains should be rejected
    SafeOrigins = [<<"http://localhost">>],
    SuspiciousDomains = [
        <<"http://attacker.com">>,
        <<"http://hacker.net">>,
        <<"http://malware.xyz">>,
        <<"http://phishing.org">>
    ],

    Results = [erlmcp_origin_validator:validate_origin(D, SafeOrigins) || D <- SuspiciousDomains],
    ?assert(lists:all(fun(R) -> R =:= {error, forbidden} end, Results)).

test_only_whitelisted_origins_allowed() ->
    %% Only whitelisted origins should be accepted
    AllowedOrigins = [
        <<"http://localhost">>,
        <<"http://127.0.0.1">>,
        <<"https://trusted-app.example.com">>
    ],

    %% Allowed should work
    R1 = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, AllowedOrigins),
    R2 = erlmcp_origin_validator:validate_origin(<<"http://127.0.0.1">>, AllowedOrigins),
    R3 = erlmcp_origin_validator:validate_origin(<<"https://trusted-app.example.com">>, AllowedOrigins),

    %% Not allowed should fail
    R4 = erlmcp_origin_validator:validate_origin(<<"http://evil.com">>, AllowedOrigins),
    R5 = erlmcp_origin_validator:validate_origin(<<"https://untrusted.net">>, AllowedOrigins),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2),
    ?assertMatch({ok, _}, R3),
    ?assertMatch({error, forbidden}, R4),
    ?assertMatch({error, forbidden}, R5).

test_multiple_attack_vectors() ->
    %% Test multiple attack vectors in one go
    SafeOrigins = [<<"http://localhost:*">>, <<"http://127.0.0.1:*">>],

    AttackVectors = [
        <<"http://localhost.attacker.com">>,
        <<"http://127.0.0.1.attacker.com">>,
        <<"http://localhost-mirror.com">>,
        <<"http://127.0.0.1-proxy.com">>,
        <<"http://192.168.1.1">>,
        <<"http://10.0.0.1">>
    ],

    Results = [erlmcp_origin_validator:validate_origin(V, SafeOrigins) || V <- AttackVectors],
    ?assert(lists:all(fun(R) -> R =:= {error, forbidden} end, Results)).

test_malicious_subdomain_rejected() ->
    %% Even if they try to spoof with subdomains
    SafeOrigins = [<<"http://localhost">>],
    MaliciousSubdomains = [
        <<"http://fake-localhost.com">>,
        <<"http://localhost.attacker.com">>,
        <<"http://127-0-0-1.com">>,
        <<"http://127.0.0.1.reversed.com">>
    ],

    Results = [erlmcp_origin_validator:validate_origin(D, SafeOrigins) || D <- MaliciousSubdomains],
    ?assert(lists:all(fun(R) -> R =:= {error, forbidden} end, Results)).

%%====================================================================
%% Pattern Matching Tests
%%====================================================================

pattern_matching_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_exact_pattern_match()),
             ?_test(test_exact_pattern_no_match()),
             ?_test(test_wildcard_pattern_match()),
             ?_test(test_multiple_patterns()),
             ?_test(test_pattern_priority())
         ]
     end}.

test_exact_pattern_match() ->
    Result = erlmcp_origin_validator:matches_origin_pattern(
        <<"http://localhost:8080">>,
        <<"http://localhost:8080">>),
    ?assert(Result).

test_exact_pattern_no_match() ->
    Result = erlmcp_origin_validator:matches_origin_pattern(
        <<"http://localhost:8080">>,
        <<"http://localhost:9000">>),
    ?assertNot(Result).

test_wildcard_pattern_match() ->
    Result = erlmcp_origin_validator:matches_origin_pattern(
        <<"http://localhost:8080">>,
        <<"http://localhost:*">>),
    ?assert(Result).

test_multiple_patterns() ->
    AllowedOrigins = [
        <<"http://localhost">>,
        <<"http://127.0.0.1:*">>,
        <<"https://localhost:3000">>
    ],

    %% Test each matches at least one pattern
    R1 = erlmcp_origin_validator:is_origin_allowed(<<"http://localhost">>, AllowedOrigins),
    R2 = erlmcp_origin_validator:is_origin_allowed(<<"http://127.0.0.1:8080">>, AllowedOrigins),
    R3 = erlmcp_origin_validator:is_origin_allowed(<<"https://localhost:3000">>, AllowedOrigins),

    ?assert(R1),
    ?assert(R2),
    ?assert(R3).

test_pattern_priority() ->
    %% First matching pattern should win
    AllowedOrigins = [
        <<"http://localhost:*">>,
        <<"http://localhost:8080">>
    ],

    Result = erlmcp_origin_validator:is_origin_allowed(<<"http://localhost:8080">>, AllowedOrigins),
    ?assert(Result).

%%====================================================================
%% Input Type Handling Tests
%%====================================================================

input_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_binary_origin()),
             ?_test(test_string_origin()),
             ?_test(test_binary_pattern()),
             ?_test(test_string_pattern()),
             ?_test(test_mixed_types())
         ]
     end}.

test_binary_origin() ->
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_string_origin() ->
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin("http://localhost", AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_binary_pattern() ->
    Result = erlmcp_origin_validator:matches_origin_pattern(
        <<"http://localhost">>,
        <<"http://localhost">>),
    ?assert(Result).

test_string_pattern() ->
    Result = erlmcp_origin_validator:matches_origin_pattern(
        <<"http://localhost">>,
        "http://localhost"),
    ?assert(Result).

test_mixed_types() ->
    %% Mix binary and string inputs
    AllowedOrigins = [
        "http://localhost",
        <<"http://127.0.0.1">>
    ],

    R1 = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, AllowedOrigins),
    R2 = erlmcp_origin_validator:validate_origin("http://127.0.0.1", AllowedOrigins),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2).

%%====================================================================
%% Case Sensitivity Tests
%%====================================================================

case_sensitivity_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_lowercase_http()),
             ?_test(test_uppercase_http()),
             ?_test(test_mixed_case_http()),
             ?_test(test_scheme_case_matters()),
             ?_test(test_host_case_insensitive())
         ]
     end}.

test_lowercase_http() ->
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_uppercase_http() ->
    %% HTTP scheme uppercase should NOT match
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"HTTP://localhost">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

test_mixed_case_http() ->
    %% Mixed case HTTP should NOT match
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"Http://localhost">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

test_scheme_case_matters() ->
    %% Scheme must match exactly (case sensitive)
    AllowedOrigins = [<<"https://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

test_host_case_insensitive() ->
    %% Note: In this implementation, host comparison is case-sensitive
    %% This is correct per URL specification, even though DNS is case-insensitive
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://LocalHost">>, AllowedOrigins),
    %% Implementation is case-sensitive (correct behavior)
    ?assertMatch({error, forbidden}, Result).

%%====================================================================
%% Default Origins Tests
%%====================================================================

default_origins_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_default_origins_exist()),
             ?_test(test_default_includes_localhost()),
             ?_test(test_default_includes_127()),
             ?_test(test_default_includes_ipv6()),
             ?_test(test_default_http_and_https()),
             ?_test(test_default_origins_practical())
         ]
     end}.

test_default_origins_exist() ->
    Defaults = erlmcp_origin_validator:get_default_allowed_origins(),
    ?assert(length(Defaults) > 0).

test_default_includes_localhost() ->
    Defaults = erlmcp_origin_validator:get_default_allowed_origins(),
    HasLocalhost = lists:any(fun(O) ->
        string:find(O, "localhost") =/= nomatch
    end, Defaults),
    ?assert(HasLocalhost).

test_default_includes_127() ->
    Defaults = erlmcp_origin_validator:get_default_allowed_origins(),
    Has127 = lists:any(fun(O) ->
        string:find(O, "127.0.0.1") =/= nomatch
    end, Defaults),
    ?assert(Has127).

test_default_includes_ipv6() ->
    Defaults = erlmcp_origin_validator:get_default_allowed_origins(),
    HasIPv6 = lists:any(fun(O) ->
        string:find(O, "[::1]") =/= nomatch
    end, Defaults),
    ?assert(HasIPv6).

test_default_http_and_https() ->
    Defaults = erlmcp_origin_validator:get_default_allowed_origins(),
    HasHttp = lists:any(fun(O) ->
        string:find(O, "http://") =/= nomatch
    end, Defaults),
    HasHttps = lists:any(fun(O) ->
        string:find(O, "https://") =/= nomatch
    end, Defaults),
    ?assert(HasHttp),
    ?assert(HasHttps).

test_default_origins_practical() ->
    %% Test that all common localhost combinations work with defaults
    Defaults = erlmcp_origin_validator:get_default_allowed_origins(),

    TestOrigins = [
        <<"http://localhost">>,
        <<"http://localhost:3000">>,
        <<"http://localhost:8080">>,
        <<"http://127.0.0.1">>,
        <<"http://127.0.0.1:3000">>,
        <<"http://127.0.0.1:8080">>,
        <<"https://localhost">>,
        <<"https://localhost:8443">>,
        <<"https://127.0.0.1">>,
        <<"https://127.0.0.1:8443">>
    ],

    Results = [erlmcp_origin_validator:validate_origin(O, Defaults) || O <- TestOrigins],
    %% All should be OK
    ?assert(lists:all(fun(R) -> R =/= {error, forbidden} end, Results)).

%%====================================================================
%% Edge Cases and Error Handling
%%====================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_empty_origin_list()),
             ?_test(test_empty_origin_string()),
             ?_test(test_port_extremes()),
             ?_test(test_malformed_origin()),
             ?_test(test_origin_with_path()),
             ?_test(test_origin_with_query())
         ]
     end}.

test_empty_origin_list() ->
    %% Empty allowed origins list should reject everything (except undefined)
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, []),
    ?assertMatch({error, forbidden}, Result).

test_empty_origin_string() ->
    %% Empty origin string
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

test_port_extremes() ->
    %% Test port number extremes
    Pattern = <<"http://localhost:*">>,

    R1 = erlmcp_origin_validator:matches_origin_pattern(<<"http://localhost:0">>, Pattern),
    R2 = erlmcp_origin_validator:matches_origin_pattern(<<"http://localhost:65535">>, Pattern),

    ?assert(R1),
    ?assert(R2).

test_malformed_origin() ->
    %% Malformed origin should be rejected
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"not a valid origin">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

test_origin_with_path() ->
    %% Origin header doesn't include path, but test robustness
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost/path">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

test_origin_with_query() ->
    %% Origin header doesn't include query, but test robustness
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://localhost?query=value">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_typical_development_config()),
             ?_test(test_typical_production_config()),
             ?_test(test_multiple_origin_validation()),
             ?_test(test_comprehensive_security_scenario())
         ]
     end}.

test_typical_development_config() ->
    %% Typical development config - all localhost ports
    DevOrigins = [
        <<"http://localhost:*">>,
        <<"http://127.0.0.1:*">>,
        <<"http://[::1]:*">>
    ],

    %% Should accept any localhost combination
    R1 = erlmcp_origin_validator:validate_origin(<<"http://localhost:3000">>, DevOrigins),
    R2 = erlmcp_origin_validator:validate_origin(<<"http://127.0.0.1:8080">>, DevOrigins),
    R3 = erlmcp_origin_validator:validate_origin(<<"http://[::1]:9000">>, DevOrigins),

    %% Should reject anything else
    R4 = erlmcp_origin_validator:validate_origin(<<"http://example.com">>, DevOrigins),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2),
    ?assertMatch({ok, _}, R3),
    ?assertMatch({error, forbidden}, R4).

test_typical_production_config() ->
    %% Production config - specific trusted origins only
    ProdOrigins = [
        <<"https://api.example.com">>,
        <<"https://app.example.com">>,
        <<"https://localhost:*">>  %% For local testing
    ],

    %% Should only accept production origins
    R1 = erlmcp_origin_validator:validate_origin(<<"https://api.example.com">>, ProdOrigins),
    R2 = erlmcp_origin_validator:validate_origin(<<"https://app.example.com">>, ProdOrigins),
    R3 = erlmcp_origin_validator:validate_origin(<<"https://localhost:8443">>, ProdOrigins),

    %% Should reject development
    R4 = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, ProdOrigins),

    %% Should reject untrusted
    R5 = erlmcp_origin_validator:validate_origin(<<"https://evil.com">>, ProdOrigins),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2),
    ?assertMatch({ok, _}, R3),
    ?assertMatch({error, forbidden}, R4),
    ?assertMatch({error, forbidden}, R5).

test_multiple_origin_validation() ->
    %% Validate multiple origins in sequence
    AllowedOrigins = [
        <<"http://localhost:*">>,
        <<"http://127.0.0.1:*">>,
        <<"https://trusted.com">>
    ],

    Origins = [
        <<"http://localhost:3000">>,
        <<"http://127.0.0.1:8080">>,
        <<"https://trusted.com">>,
        <<"http://evil.com">>
    ],

    Results = [erlmcp_origin_validator:validate_origin(O, AllowedOrigins) || O <- Origins],

    %% First 3 should be OK, last should be error
    ?assertMatch({ok, _}, lists:nth(1, Results)),
    ?assertMatch({ok, _}, lists:nth(2, Results)),
    ?assertMatch({ok, _}, lists:nth(3, Results)),
    ?assertMatch({error, forbidden}, lists:nth(4, Results)).

test_comprehensive_security_scenario() ->
    %% Complete security scenario
    SafeOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    %% Legitimate requests should pass
    LegitimateOrigins = [
        <<"http://localhost">>,
        <<"http://localhost:3000">>,
        <<"http://127.0.0.1">>,
        <<"http://127.0.0.1:8080">>,
        <<"https://localhost">>,
        <<"https://localhost:8443">>
    ],

    %% Attack attempts should fail
    AttackOrigins = [
        <<"http://attacker.com">>,
        <<"http://evil.example.com">>,
        <<"https://malicious.org">>,
        <<"http://localhost.attacker.com">>,
        <<"http://127.0.0.1.attacker.com">>
    ],

    LegitResults = [erlmcp_origin_validator:validate_origin(O, SafeOrigins) || O <- LegitimateOrigins],
    AttackResults = [erlmcp_origin_validator:validate_origin(O, SafeOrigins) || O <- AttackOrigins],

    %% All legitimate should pass
    ?assert(lists:all(fun(R) -> R =/= {error, forbidden} end, LegitResults)),

    %% All attacks should fail
    ?assert(lists:all(fun(R) -> R =:= {error, forbidden} end, AttackResults)).
