%%%-------------------------------------------------------------------
%%% @doc URI Validator Test Suite
%%%
%%% Comprehensive tests for URI validation.
%%% Tests RFC 3986 compliance, injection prevention, SSRF prevention.
%%%
%%% == Chicago School TDD ==
%%% Uses REAL erlmcp_uri_validator module.
%%% Tests observable behavior through API calls only.
%%% NO mocks - real URI validation logic.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_uri_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test 1-5: Valid URI Cases (RFC 3986 Compliance)
%%%===================================================================

valid_http_uri_test() ->
    %% Test valid HTTP URI
    Uri = <<"http://example.com">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch(ok, Result).

valid_https_uri_test() ->
    %% Test valid HTTPS URI
    Uri = <<"https://example.com">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch(ok, Result).

valid_uri_with_port_test() ->
    %% Test URI with port number
    Uri = <<"https://example.com:8443">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch(ok, Result).

valid_uri_with_path_test() ->
    %% Test URI with path
    Uri = <<"https://example.com/api/v1/users">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch(ok, Result).

valid_uri_with_query_test() ->
    %% Test URI with query string
    Uri = <<"https://example.com/search?q=test&lang=en">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch(ok, Result).

%%%===================================================================
%%% Test 6-10: URI Parsing Tests
%%%===================================================================

parse_simple_uri_test() ->
    %% Test parsing simple URI
    Uri = <<"https://example.com">>,

    Result = erlmcp_uri_validator:parse_uri(Uri),

    ?assertMatch({ok, #{scheme := <<"https">>, host := <<"example.com">>}}, Result).

parse_uri_with_port_test() ->
    %% Test parsing URI with port
    Uri = <<"https://example.com:8443">>,

    Result = erlmcp_uri_validator:parse_uri(Uri),

    ?assertMatch({ok, #{port := 8443}}, Result).

parse_uri_with_path_test() ->
    %% Test parsing URI with path
    Uri = <<"https://example.com/api/v1">>,

    Result = erlmcp_uri_validator:parse_uri(Uri),

    ?assertMatch({ok, #{path := <<"/api/v1">>}}, Result).

parse_uri_with_query_test() ->
    %% Test parsing URI with query
    Uri = <<"https://example.com/search?q=test">>,

    Result = erlmcp_uri_validator:parse_uri(Uri),

    ?assertMatch({ok, #{query := <<"q=test">>}}, Result).

parse_uri_with_fragment_test() ->
    %% Test parsing URI with fragment
    Uri = <<"https://example.com/page#section1">>,

    Result = erlmcp_uri_validator:parse_uri(Uri),

    ?assertMatch({ok, #{fragment := <<"section1">>}}, Result).

%%%===================================================================
%%% Test 11-15: Injection Prevention Tests
%%%===================================================================

sql_injection_test() ->
    %% Test SQL injection attempt in URI
    Uri = <<"https://example.com/users?id=1' OR '1'='1">>,

    %% Should still parse, but app should sanitize params
    Result = erlmcp_uri_validator:validate_uri(Uri),

    %% URI itself is valid, SQL injection prevention is app-level
    ?assertMatch(ok, Result).

command_injection_test() ->
    %% Test command injection with backticks
    Uri = <<"https://example.com/file?name=`whoami`">>,

    %% Should parse as valid URI
    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch(ok, Result).

path_traversal_dotdot_test() ->
    %% Test path traversal with ../
    Uri = <<"https://example.com/../../etc/passwd">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    %% Should reject due to dangerous pattern
    ?assertMatch({error, dangerous_characters}, Result).

path_traversal_windows_test() ->
    %% Test Windows path traversal with ..\
    Uri = <<"https://example.com/..\\..\\windows\\system32">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    %% Should reject
    ?assertMatch({error, dangerous_characters}, Result).

null_byte_injection_test() ->
    %% Test null byte injection
    Uri = <<"https://example.com/file.txt", 0, ".exe">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    %% Should reject
    ?assertMatch({error, dangerous_characters}, Result).

%%%===================================================================
%%% Test 16-20: SSRF Prevention Tests
%%%===================================================================

ssrf_localhost_test() ->
    %% Test SSRF to localhost
    Uri = <<"http://localhost/admin">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    %% Should reject private IP by default
    ?assertMatch({error, private_ip_not_allowed}, Result).

ssrf_127_0_0_1_test() ->
    %% Test SSRF to 127.0.0.1
    Uri = <<"http://127.0.0.1/internal">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch({error, private_ip_not_allowed}, Result).

ssrf_private_ip_10_test() ->
    %% Test SSRF to private IP 10.0.0.0/8
    Uri = <<"http://10.0.0.1/internal">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch({error, private_ip_not_allowed}, Result).

ssrf_private_ip_192_168_test() ->
    %% Test SSRF to private IP 192.168.0.0/16
    Uri = <<"http://192.168.1.1/router">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch({error, private_ip_not_allowed}, Result).

ssrf_private_ip_172_16_test() ->
    %% Test SSRF to private IP 172.16.0.0/12
    Uri = <<"http://172.16.0.1/internal">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    ?assertMatch({error, private_ip_not_allowed}, Result).

%%%===================================================================
%%% Test 21-25: Private IP Detection Tests
%%%===================================================================

is_private_ip_localhost_test() ->
    %% Test localhost detection
    ?assert(erlmcp_uri_validator:is_private_ip(<<"localhost">>)).

is_private_ip_127_test() ->
    %% Test 127.0.0.1 detection
    ?assert(erlmcp_uri_validator:is_private_ip(<<"127.0.0.1">>)).

is_private_ip_10_test() ->
    %% Test 10.0.0.0/8 detection
    ?assert(erlmcp_uri_validator:is_private_ip(<<"10.0.0.1">>)).

is_private_ip_192_168_test() ->
    %% Test 192.168.0.0/16 detection
    ?assert(erlmcp_uri_validator:is_private_ip(<<"192.168.1.1">>)).

is_private_ip_public_test() ->
    %% Test public IP is not private
    ?assertNot(erlmcp_uri_validator:is_private_ip(<<"8.8.8.8">>)).

%%%===================================================================
%%% Test 26-30: Edge Cases and Options Tests
%%%===================================================================

uri_too_long_test() ->
    %% Test URI exceeds max length
    LongUri = binary:copy(<<"a">>, 3000),
    Uri = <<"https://example.com/", LongUri/binary>>,

    Result = erlmcp_uri_validator:validate_uri(Uri, #{max_length => 2048}),

    ?assertMatch({error, uri_too_long}, Result).

allowed_schemes_test() ->
    %% Test custom allowed schemes
    Uri = <<"ftp://example.com/file.txt">>,

    %% Default should reject FTP
    Result1 = erlmcp_uri_validator:validate_uri(Uri),
    ?assertMatch({error, {disallowed_scheme, _}}, Result1),

    %% Custom options should allow FTP
    Result2 = erlmcp_uri_validator:validate_uri(Uri, #{allowed_schemes => [<<"ftp">>]}),
    ?assertMatch(ok, Result2).

allow_private_ips_option_test() ->
    %% Test allowing private IPs via options
    Uri = <<"http://192.168.1.1/internal">>,

    %% Default should reject
    Result1 = erlmcp_uri_validator:validate_uri(Uri),
    ?assertMatch({error, private_ip_not_allowed}, Result1),

    %% With option should allow
    Result2 = erlmcp_uri_validator:validate_uri(Uri, #{allow_private_ips => true}),
    ?assertMatch(ok, Result2).

is_safe_uri_test() ->
    %% Test is_safe_uri helper
    SafeUri = <<"https://example.com">>,
    UnsafeUri = <<"http://localhost">>,

    ?assert(erlmcp_uri_validator:is_safe_uri(SafeUri)),
    ?assertNot(erlmcp_uri_validator:is_safe_uri(UnsafeUri)).

javascript_protocol_test() ->
    %% Test JavaScript protocol (XSS vector)
    Uri = <<"javascript:alert('XSS')">>,

    Result = erlmcp_uri_validator:validate_uri(Uri),

    %% Should reject due to dangerous pattern
    ?assertMatch({error, dangerous_characters}, Result).
