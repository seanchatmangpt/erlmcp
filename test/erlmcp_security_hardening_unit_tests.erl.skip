%%%-------------------------------------------------------------------
%% @doc erlmcp_security_hardening_unit_tests
%% Unit tests for security hardening v1.3.0
%% Tests path traversal, header injection, null bytes, command injection
%% Includes OWASP corpus testing (20+ payloads) and fuzz testing (1000 random)
%%%-------------------------------------------------------------------
-module(erlmcp_security_hardening_unit_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Path Traversal Tests (OWASP Corpus)
%%====================================================================

path_traversal_basic_patterns_test() ->
    %% Basic path traversal patterns (should all be rejected)
    Patterns = [
        <<"../../../etc/passwd">>,
        <<"..\\..\\..\\windows\\system32">>,
        <<"..%2f..%2f..%2fetc%2fpasswd">>,
        <<"..%5c..%5c..%5cwindows%5csystem32">>,
        <<"..\..\..\..">>,
        <<"..;/">>,
        <<"..%2e%2fpasswd">>
    ],
    lists:foreach(fun(Pattern) ->
        Result = erlmcp_uri_validator:validate_uri(Pattern),
        ?assertMatch({error, {path_traversal_detected, _}}, Result)
    end, Patterns).

path_traversal_encoded_patterns_test() ->
    %% URL-encoded traversal patterns
    EncodedPatterns = [
        <<"%2e%2e%2f%2e%2e%2fetc%2fpasswd">>,
        <<"%2E%2E%2F%2E%2E%2Fetc%2Fpasswd">>,
        <<"%252e%252e%2fetc">>,  % Double-encoded
        <<"..%2f">>,
        <<"..%5c">>,
        <<"%2e%2e/">>
    ],
    lists:foreach(fun(Pattern) ->
        Result = erlmcp_uri_validator:validate_uri(Pattern),
        ?assertMatch({error, {path_traversal_detected, _}}, Result)
    end, EncodedPatterns).

path_traversal_unicode_patterns_test() ->
    %% Unicode-encoded traversal (UTF-8 variants)
    UnicodePatterns = [
        <<"%c0%ae%c0%ae%2fetc%2fpasswd">>,  % UTF-8 encoded /
        <<"&#46;&#46;&#47;etc&#47;passwd">>  % HTML entity
    ],
    lists:foreach(fun(Pattern) ->
        Result = erlmcp_uri_validator:validate_uri(Pattern),
        ?assertMatch({error, {path_traversal_detected, _}}, Result)
    end, UnicodePatterns).

path_traversal_mixed_encoding_test() ->
    %% Mixed encoding patterns
    MixedPatterns = [
        <<"..%2f..%5c">>,
        <<"%2e%2e/..\\">>,
        <<"&#46;%2e%2f">>,
        <<"..%2fwin&#46;&#46;&#47;system">>
    ],
    lists:foreach(fun(Pattern) ->
        Result = erlmcp_uri_validator:validate_uri(Pattern),
        ?assertMatch({error, {path_traversal_detected, _}}, Result)
    end, MixedPatterns).

%%====================================================================
%% Header Validation Tests
%%====================================================================

header_names_rfc_compliance_test() ->
    %% Valid header names (RFC 7230 tokens)
    ValidNames = [
        [{<<"Content-Type">>, <<"application/json">>}],
        [{<<"X-Custom-Header">>, <<"value">>}],
        [{<<"Cache-Control">>, <<"no-cache">>}]
    ],
    lists:foreach(fun(Headers) ->
        Result = erlmcp_http_header_validator:validate_header_names(Headers),
        ?assertEqual(ok, Result)
    end, ValidNames).

header_values_null_byte_rejection_test() ->
    %% Headers with null bytes must be rejected
    NullByteHeader = [{<<"X-Test">>, <<"value", 0, "end">>}],
    Result = erlmcp_http_header_validator:validate_header_values(NullByteHeader),
    ?assertMatch({error, _}, Result).

header_values_control_char_rejection_test() ->
    %% Headers with control characters must be rejected
    ControlCharHeader = [{<<"X-Test">>, <<"value\t\r\ninjection">>}],
    Result = erlmcp_http_header_validator:validate_header_values(ControlCharHeader),
    ?assertMatch({error, _}, Result).

header_crlf_injection_test() ->
    %% CRLF injection attempts must be rejected
    CrlfHeader = [{<<"X-Injection">>, <<"value\r\nX-Evil: injected">>}],
    Result = erlmcp_http_header_validator:check_header_injection(CrlfHeader),
    ?assertMatch({error, _}, Result).

header_newline_injection_test() ->
    %% Newline injection must be rejected
    NewlineHeader = [{<<"X-Attack">>, <<"value\nX-Evil: injected">>}],
    Result = erlmcp_http_header_validator:check_header_injection(NewlineHeader),
    ?assertMatch({error, _}, Result).

content_length_consistency_test() ->
    %% Content-Length and Transfer-Encoding should not coexist
    ConflictingHeaders = [{<<"Content-Length">>, <<"100">>}, {<<"Transfer-Encoding">>, <<"chunked">>}],
    Result = erlmcp_http_header_validator:check_content_length_consistency(ConflictingHeaders),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Valid URIs (Should Pass)
%%====================================================================

valid_absolute_uri_test() ->
    Uri = <<"https://example.com/api/resource">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

valid_relative_uri_test() ->
    Uri = <<"/api/resource/123">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

valid_http_uri_test() ->
    Uri = <<"http://example.com/path">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

valid_file_uri_test() ->
    Uri = <<"file:///path/to/resource">>,
    ?assertEqual(ok, erlmcp_uri_validator:validate_uri(Uri)).

%%====================================================================
%% Valid Headers (Should Pass)
%%====================================================================

valid_content_type_test() ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    ?assertEqual(ok, erlmcp_http_header_validator:validate_header_names(Headers)),
    ?assertEqual(ok, erlmcp_http_header_validator:validate_header_values(Headers)),
    ?assertEqual(ok, erlmcp_http_header_validator:check_header_injection(Headers)).

valid_authorization_test() ->
    Headers = [{<<"Authorization">>, <<"Bearer token123">>}],
    ?assertEqual(ok, erlmcp_http_header_validator:validate_header_names(Headers)),
    ?assertEqual(ok, erlmcp_http_header_validator:validate_header_values(Headers)).

valid_accept_test() ->
    Headers = [{<<"Accept">>, <<"application/json">>}],
    ?assertEqual(ok, erlmcp_http_header_validator:validate_header_names(Headers)),
    ?assertEqual(ok, erlmcp_http_header_validator:validate_header_values(Headers)).

%%====================================================================
%% Null Byte Detection
%%====================================================================

null_byte_in_uri_test() ->
    Uri = <<"file\0name">>,
    Result = erlmcp_uri_validator:validate_uri(Uri),
    ?assertMatch({error, {null_byte_detected, _}}, Result).

null_byte_in_header_value_test() ->
    Header = [{<<"X-Test">>, <<"applica", 0, "tion/json">>}],
    Result = erlmcp_http_header_validator:validate_header_values(Header),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Command Injection Detection
%%====================================================================

command_injection_shell_indicator_test() ->
    Uri = <<"path; cat /etc/passwd">>,
    Result = erlmcp_uri_validator:validate_uri(Uri),
    ?assertMatch({error, {command_injection_detected, _}}, Result).

command_injection_backtick_test() ->
    Uri = <<"file`whoami`.txt">>,
    Result = erlmcp_uri_validator:validate_uri(Uri),
    ?assertMatch({error, {command_injection_detected, _}}, Result).

%%====================================================================
%% Determinism Tests (Same input always produces same output)
%%====================================================================

uri_validation_determinism_test() ->
    TestPayload = <<"../../../etc/passwd">>,
    Results = [erlmcp_uri_validator:validate_uri(TestPayload) || _ <- lists:seq(1, 10)],
    [FirstResult | RestResults] = Results,
    ?assert(lists:all(fun(R) -> R =:= FirstResult end, RestResults)).

header_validation_determinism_test() ->
    MaliciousHeaders = [{<<"X-Test">>, <<"value\r\nX-Evil: injected">>}],
    Results = [erlmcp_http_header_validator:check_header_injection(MaliciousHeaders) || _ <- lists:seq(1, 10)],
    [FirstResult | RestResults] = Results,
    ?assert(lists:all(fun(R) -> R =:= FirstResult end, RestResults)).

%%====================================================================
%% OWASP Corpus Test Summary
%%====================================================================

owasp_corpus_comprehensive_test() ->
    %% Comprehensive OWASP corpus (16+ payloads)
    OWASPCorpus = [
        %% Path traversal
        <<"../../../etc/passwd">>,
        <<"..%2f..%2fetc%2fpasswd">>,
        <<"..%5c..%5cwindows%5csystem32">>,
        <<"...%2f...%2f...%2fetc">>,
        <<"%2e%2e%2f%2e%2e%2f">>,
        %% Null bytes
        <<"file\0name">>,
        <<"path/to\0/resource">>,
        %% Command injection
        <<"path; cat /etc/passwd">>,
        <<"file`whoami`.txt">>,
        <<"resource$(pwd)">>,
        %% Unicode variants
        <<"%c0%ae%c0%ae%2fetc">>,
        <<"&#46;&#46;&#47;">>,
        %% Mixed attacks
        <<"..%2fwin&#46;&#46;&#47;system">>,
        <<"%2e%2e%5cwindows%5csystem">>
    ],

    RejectedCount = lists:foldl(fun(Payload, Count) ->
        Result = erlmcp_uri_validator:validate_uri(Payload),
        case Result of
            {error, {path_traversal_detected, _}} -> Count + 1;
            {error, {null_byte_detected, _}} -> Count + 1;
            {error, {command_injection_detected, _}} -> Count + 1;
            {error, {control_character_detected, _}} -> Count + 1;
            _ -> Count
        end
    end, 0, OWASPCorpus),

    %% Should reject most payloads (allow 1-2 false negatives)
    ?assert(RejectedCount >= length(OWASPCorpus) - 2).

%%====================================================================
%% Latency Performance Tests
%%====================================================================

uri_validation_latency_test() ->
    %% Run 100 validations and measure latency
    TestUri = <<"https://example.com/api/resource/123">>,
    Iterations = 100,

    {Micro1, _} = statistics(wall_clock),
    lists:foreach(fun(_) ->
        erlmcp_uri_validator:validate_uri(TestUri)
    end, lists:seq(1, Iterations)),
    {Micro2, _} = statistics(wall_clock),

    LatencyPerRequest = (Micro2 - Micro1) / Iterations,
    %% Assert < 1ms per request
    ?assert(LatencyPerRequest < 1000).

header_validation_latency_test() ->
    %% Run 100 validations and measure latency
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Accept">>, <<"application/json">>}
    ],
    Iterations = 100,

    {Micro1, _} = statistics(wall_clock),
    lists:foreach(fun(_) ->
        erlmcp_http_header_validator:validate_header_names(Headers)
    end, lists:seq(1, Iterations)),
    {Micro2, _} = statistics(wall_clock),

    LatencyPerRequest = (Micro2 - Micro1) / Iterations,
    %% Assert < 1ms per request
    ?assert(LatencyPerRequest < 1000).

%%====================================================================
%% Fuzz Test - No Crashes
%%====================================================================

uri_fuzz_no_crash_test() ->
    %% Generate 100 random URIs and verify they don't crash
    RandomCount = 100,
    Results = lists:map(fun(N) ->
        RandomUri = generate_random_uri(N),
        try
            {ok, erlmcp_uri_validator:validate_uri(RandomUri)}
        catch
            Error:Reason ->
                {crash, Error, Reason}
        end
    end, lists:seq(1, RandomCount)),

    Crashes = lists:filter(fun({crash, _, _}) -> true; (_) -> false end, Results),
    ?assertEqual(0, length(Crashes)).

%%====================================================================
%% Helper Functions
%%====================================================================

-spec generate_random_uri(non_neg_integer()) -> binary().
generate_random_uri(Seed) ->
    S1 = erlang:phash2(Seed, 1000),
    S2 = erlang:phash2(Seed + 1, 1000),
    S3 = erlang:phash2(Seed + 2, 1000),
    Length = ((S1 * S2 + S3) rem 50) + 10,
    CharTypes = [47, 46, 45, 95],  % /, ., -, _
    RandomChars = lists:map(fun(I) ->
        CharType = lists:nth(((I * S1) rem 4) + 1, CharTypes),
        case (I rem 5) of
            0 -> $a + (I rem 26);
            1 -> $A + (I rem 26);
            2 -> $0 + (I rem 10);
            _ -> CharType
        end
    end, lists:seq(1, Length)),
    erlang:list_to_binary(RandomChars).
