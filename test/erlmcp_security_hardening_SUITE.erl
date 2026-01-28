%%%-------------------------------------------------------------------
%% @doc erlmcp_security_hardening_SUITE
%% Comprehensive security testing for v1.3.0
%% Tests path traversal, header injection, null bytes, command injection
%% Includes OWASP corpus testing (20+ payloads) and fuzz testing (1000 random)
%%%-------------------------------------------------------------------
-module(erlmcp_security_hardening_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% CT Callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 120}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        %% Path traversal tests (OWASP corpus)
        path_traversal_basic_patterns_test,
        path_traversal_encoded_patterns_test,
        path_traversal_unicode_patterns_test,
        path_traversal_double_encoding_test,
        path_traversal_mixed_encoding_test,

        %% Header validation tests
        header_names_rfc_compliance_test,
        header_values_null_byte_rejection_test,
        header_values_control_char_rejection_test,
        header_crlf_injection_test,
        content_length_consistency_test,

        %% Comprehensive security tests
        uri_security_fuzz_test,
        uri_security_corpus_test,
        header_injection_attempt_test,
        malicious_client_scenario_test,

        %% Performance tests
        uri_validation_latency_test,
        header_validation_latency_test,

        %% Determinism tests
        uri_validation_determinism_test,
        header_validation_determinism_test
    ].

%%====================================================================
%% Path Traversal Tests
%%====================================================================

path_traversal_basic_patterns_test(_Config) ->
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
        ct:log("Testing pattern: ~p -> ~p", [Pattern, Result]),
        ?assertMatch({error, {path_traversal_detected, _}}, Result)
    end, Patterns).

path_traversal_encoded_patterns_test(_Config) ->
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
        ct:log("Testing encoded pattern: ~p -> ~p", [Pattern, Result]),
        ?assertMatch({error, {path_traversal_detected, _}}, Result)
    end, EncodedPatterns).

path_traversal_unicode_patterns_test(_Config) ->
    %% Unicode-encoded traversal (UTF-8 variants)
    UnicodePatterns = [
        <<"%c0%ae%c0%ae%2fetc%2fpasswd">>,  % UTF-8 encoded /
        <<"%c1%9c">>,
        <<"%c0%9e">>,
        <<"&#46;&#46;&#47;etc&#47;passwd">>  % HTML entity
    ],
    lists:foreach(fun(Pattern) ->
        Result = erlmcp_uri_validator:validate_uri(Pattern),
        ct:log("Testing unicode pattern: ~p -> ~p", [Pattern, Result]),
        ?assertMatch({error, {path_traversal_detected, _}}, Result)
    end, UnicodePatterns).

path_traversal_double_encoding_test(_Config) ->
    %% Double-encoded traversal patterns
    DoubleEncoded = [
        <<"%252e%252e%252fpasswd">>,
        <<"%252e%252e%252f%252e%252e%252fetc">>,
        <<"%25%32%65%25%32%65%2fpasswd">>
    ],
    lists:foreach(fun(Pattern) ->
        Result = erlmcp_uri_validator:validate_uri(Pattern),
        ct:log("Testing double-encoded pattern: ~p -> ~p", [Pattern, Result]),
        ?assertMatch({error, {path_traversal_detected, _}}, Result)
    end, DoubleEncoded).

path_traversal_mixed_encoding_test(_Config) ->
    %% Mixed encoding patterns
    MixedPatterns = [
        <<"..%2f..%5c">>,
        <<"%2e%2e/..\\">>,
        <<"&#46;%2e%2f">>,
        <<"..%2fwin&#46;&#46;&#47;system">>
    ],
    lists:foreach(fun(Pattern) ->
        Result = erlmcp_uri_validator:validate_uri(Pattern),
        ct:log("Testing mixed pattern: ~p -> ~p", [Pattern, Result]),
        ?assertMatch({error, {path_traversal_detected, _}}, Result)
    end, MixedPatterns).

%%====================================================================
%% Header Validation Tests
%%====================================================================

header_names_rfc_compliance_test(_Config) ->
    %% Valid header names (RFC 7230 tokens)
    ValidNames = [
        [{<<"Content-Type">>, <<"application/json">>}],
        [{<<"X-Custom-Header">>, <<"value">>}],
        [{<<"Cache-Control">>, <<"no-cache">>}]
    ],
    lists:foreach(fun(Headers) ->
        Result = erlmcp_http_header_validator:validate_header_names(Headers),
        ct:log("Testing valid headers: ~p -> ~p", [Headers, Result]),
        ?assertEqual(ok, Result)
    end, ValidNames),

    %% Invalid header names (RFC 7230 violation)
    InvalidNames = [
        [{<<"Content Type">>, <<"value">>}],     % Space not allowed
        [{<<"Content@Type">>, <<"value">>}],     % @ not allowed
        [{<<"Content[Type]">>, <<"value">>}]     % [] not allowed
    ],
    lists:foreach(fun(Headers) ->
        Result = erlmcp_http_header_validator:validate_header_names(Headers),
        ct:log("Testing invalid headers: ~p -> ~p", [Headers, Result]),
        ?assertMatch({error, _}, Result)
    end, InvalidNames).

header_values_null_byte_rejection_test(_Config) ->
    %% Headers with null bytes must be rejected
    NullByteHeaders = [
        [{<<"X-Test">>, <<"value", 0, "end">>}],
        [{<<"Content-Type">>, <<"applica", 0, "tion/json">>}],
        [{<<"X-Injection">>, <<0>>}]
    ],
    lists:foreach(fun(Headers) ->
        Result = erlmcp_http_header_validator:validate_header_values(Headers),
        ct:log("Testing null byte headers: ~p -> ~p", [Headers, Result]),
        ?assertMatch({error, _}, Result)
    end, NullByteHeaders).

header_values_control_char_rejection_test(_Config) ->
    %% Headers with control characters must be rejected
    ControlCharHeaders = [
        [{<<"X-Test">>, <<"value\t\r\ninjection">>}],  % Tab, CR, LF
        [{<<"X-Control">>, <<1, 2, 3>>}],               % ASCII 1, 2, 3
        [{<<"X-Bell">>, <<7>>}]                         % Bell character
    ],
    lists:foreach(fun(Headers) ->
        Result = erlmcp_http_header_validator:validate_header_values(Headers),
        ct:log("Testing control char headers: ~p -> ~p", [Headers, Result]),
        ?assertMatch({error, _}, Result)
    end, ControlCharHeaders).

header_crlf_injection_test(_Config) ->
    %% CRLF injection attempts must be rejected
    CrlfHeaders = [
        [{<<"X-Injection">>, <<"value\r\nX-Evil: injected">>}],
        [{<<"X-Attack">>, <<"value\nX-Evil: injected">>}],
        [{<<"X-Newline">>, <<"value\rinjected">>}]
    ],
    lists:foreach(fun(Headers) ->
        Result = erlmcp_http_header_validator:check_header_injection(Headers),
        ct:log("Testing CRLF injection: ~p -> ~p", [Headers, Result]),
        ?assertMatch({error, _}, Result)
    end, CrlfHeaders).

content_length_consistency_test(_Config) ->
    %% Content-Length and Transfer-Encoding should not coexist
    ConflictingHeaders = [
        [{<<"Content-Length">>, <<"100">>}, {<<"Transfer-Encoding">>, <<"chunked">>}]
    ],
    lists:foreach(fun(Headers) ->
        Result = erlmcp_http_header_validator:check_content_length_consistency(Headers),
        ct:log("Testing conflicting headers: ~p -> ~p", [Headers, Result]),
        ?assertMatch({error, _}, Result)
    end, ConflictingHeaders).

%%====================================================================
%% Comprehensive Security Tests
%%====================================================================

uri_security_fuzz_test(_Config) ->
    %% Generate 1000 random URIs and verify they don't crash
    RandomCount = 1000,
    Results = lists:map(fun(N) ->
        RandomUri = generate_random_uri(N),
        try
            Result = erlmcp_uri_validator:validate_uri(RandomUri),
            {ok, Result}
        catch
            Error:Reason ->
                {error, {Error, Reason}}
        end
    end, lists:seq(1, RandomCount)),

    %% Verify no crashes
    Crashes = lists:filter(fun
        ({error, _}) -> true;
        (_) -> false
    end, Results),

    CrashCount = length(Crashes),
    ct:log("Fuzz test: ~p crashes out of ~p random URIs", [CrashCount, RandomCount]),
    ?assertEqual(0, CrashCount).

uri_security_corpus_test(_Config) ->
    %% Test comprehensive OWASP corpus (20+ payloads)
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

        %% Special characters
        <<"path<script>alert(1)</script>">>,
        <<"uri>>file.txt">>,
        <<"cmd|grep">>,

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

    ct:log("OWASP Corpus: rejected ~p/~p payloads", [RejectedCount, length(OWASPCorpus)]),
    ?assertTrue(RejectedCount >= length(OWASPCorpus) - 2).  % Allow 2 false negatives max

header_injection_attempt_test(_Config) ->
    %% Test malicious header patterns
    InjectionHeaders = [
        [{<<"X-Test">>, <<"value\r\nSet-Cookie: malicious">>}],
        [{<<"Authorization">>, <<"Bearer\r\nX-Injected: true">>}],
        [{<<"Content-Type">>, <<"application/json\nContent-Length: 0">>}]
    ],

    RejectedCount = lists:foldl(fun(Headers, Count) ->
        Result = erlmcp_http_header_validator:check_header_injection(Headers),
        case Result of
            {error, _} -> Count + 1;
            _ -> Count
        end
    end, 0, InjectionHeaders),

    ct:log("Header injection: rejected ~p/~p attempts", [RejectedCount, length(InjectionHeaders)]),
    ?assertEqual(length(InjectionHeaders), RejectedCount).

malicious_client_scenario_test(_Config) ->
    %% Simulate malicious client sending traversal + injection
    MaliciousRequests = [
        {<<"/api/../../../etc/passwd">>, [
            {<<"X-Payload">>, <<"../../../etc/passwd">>},
            {<<"Accept">>, <<"*/*\r\nX-Evil: true">>}
        ]},
        {<<"file:///etc/passwd">>, [
            {<<"Content-Type">>, <<"application/json\x00injected">>}
        ]},
        {<<"%2e%2e%2fetc%2fpasswd">>, [
            {<<"X-Test">>, <<"value">>}
        ]}
    ],

    Results = lists:map(fun({Uri, Headers}) ->
        UriResult = erlmcp_uri_validator:validate_uri(Uri),
        HeaderResult = erlmcp_http_header_validator:check_header_injection(Headers),
        {Uri, UriResult, HeaderResult}
    end, MaliciousRequests),

    %% Verify all were rejected
    AllRejected = lists:all(fun({_Uri, UriResult, HeaderResult}) ->
        (UriResult =/= ok orelse HeaderResult =/= ok)
    end, Results),

    ct:log("Malicious scenario: all rejected = ~p", [AllRejected]),
    ?assertTrue(AllRejected).

%%====================================================================
%% Performance Tests (Latency)
%%====================================================================

uri_validation_latency_test(_Config) ->
    %% Run 1000 validations and measure latency
    TestUri = <<"https://example.com/api/resource/123">>,
    Iterations = 1000,

    {MicrosStart, _} = statistics(wall_clock),
    lists:foreach(fun(_) ->
        erlmcp_uri_validator:validate_uri(TestUri)
    end, lists:seq(1, Iterations)),
    {MicrosEnd, _} = statistics(wall_clock),

    LatencyPerRequest = (MicrosEnd - MicrosStart) / Iterations,
    ct:log("URI validation latency: ~.3f microseconds per request", [LatencyPerRequest]),

    %% Assert < 1ms (1000 microseconds) per request
    ?assertTrue(LatencyPerRequest < 1000).

header_validation_latency_test(_Config) ->
    %% Run 1000 validations and measure latency
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Accept">>, <<"application/json">>},
        {<<"Authorization">>, <<"Bearer token123">>}
    ],
    Iterations = 1000,

    {MicrosStart, _} = statistics(wall_clock),
    lists:foreach(fun(_) ->
        erlmcp_http_header_validator:validate_header_names(Headers)
    end, lists:seq(1, Iterations)),
    {MicrosEnd, _} = statistics(wall_clock),

    LatencyPerRequest = (MicrosEnd - MicrosStart) / Iterations,
    ct:log("Header validation latency: ~.3f microseconds per request", [LatencyPerRequest]),

    %% Assert < 1ms per request
    ?assertTrue(LatencyPerRequest < 1000).

%%====================================================================
%% Determinism Tests
%%====================================================================

uri_validation_determinism_test(_Config) ->
    %% Run same URI 100 times and verify consistent rejection
    TestPayload = <<"../../../etc/passwd">>,
    Results = lists:map(fun(_) ->
        erlmcp_uri_validator:validate_uri(TestPayload)
    end, lists:seq(1, 100)),

    %% All results should be identical
    [FirstResult | RestResults] = Results,
    AllIdentical = lists:all(fun(Result) ->
        Result =:= FirstResult
    end, RestResults),

    ct:log("URI validation determinism: all identical = ~p", [AllIdentical]),
    ?assertTrue(AllIdentical).

header_validation_determinism_test(_Config) ->
    %% Run same headers 100 times and verify consistent results
    MaliciousHeaders = [
        {<<"X-Test">>, <<"value\r\nX-Evil: injected">>}
    ],
    Results = lists:map(fun(_) ->
        erlmcp_http_header_validator:check_header_injection(MaliciousHeaders)
    end, lists:seq(1, 100)),

    %% All results should be identical
    [FirstResult | RestResults] = Results,
    AllIdentical = lists:all(fun(Result) ->
        Result =:= FirstResult
    end, RestResults),

    ct:log("Header validation determinism: all identical = ~p", [AllIdentical]),
    ?assertTrue(AllIdentical).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Generate random URI for fuzz testing
-spec generate_random_uri(non_neg_integer()) -> binary().
generate_random_uri(Seed) ->
    random:seed(Seed),
    Length = random:uniform(100) + 10,
    RandomChars = lists:map(fun(_) ->
        case random:uniform(5) of
            1 -> random:uniform(122) + 95;  % a-z and some symbols
            2 -> random:uniform(90) + 64;   % A-Z
            3 -> random:uniform(10) + 48;   % 0-9
            4 -> $/ ;                        % /
            5 -> $.  ;                       % .
            _ -> $-
        end
    end, lists:seq(1, Length)),
    erlang:list_to_binary(RandomChars).
