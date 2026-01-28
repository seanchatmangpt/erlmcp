%% Test runner for security hardening v1.3.0

-module(security_test_runner).
-compile(export_all).

main(_) ->
    run_tests().

run_tests() ->
    io:format("=== SECURITY HARDENING v1.3.0 TEST RUNNER ===~n~n", []),

    Tests = [
        {"Path Traversal: Basic Patterns", fun test_path_traversal_basic/0},
        {"Path Traversal: Encoded Patterns", fun test_path_traversal_encoded/0},
        {"Path Traversal: Unicode Patterns", fun test_path_traversal_unicode/0},
        {"Path Traversal: Mixed Encoding", fun test_path_traversal_mixed/0},
        {"Header Validation: RFC Compliance", fun test_header_rfc_compliance/0},
        {"Header Validation: Null Bytes", fun test_header_null_bytes/0},
        {"Header Validation: CRLF Injection", fun test_header_crlf_injection/0},
        {"Valid URIs Pass", fun test_valid_uris/0},
        {"Valid Headers Pass", fun test_valid_headers/0},
        {"URI Determinism", fun test_uri_determinism/0},
        {"Header Determinism", fun test_header_determinism/0},
        {"OWASP Corpus", fun test_owasp_corpus/0},
        {"URI Latency", fun test_uri_latency/0},
        {"Header Latency", fun test_header_latency/0},
        {"Fuzz No Crash", fun test_fuzz_no_crash/0}
    ],

    Results = lists:map(fun({Name, TestFun}) ->
        case catch TestFun() of
            ok ->
                io:format("[PASS] ~s~n", [Name]),
                pass;
            Error ->
                io:format("[FAIL] ~s: ~p~n", [Name, Error]),
                fail
        end
    end, Tests),

    PassCount = lists:count(fun(X) -> X =:= pass end, Results),
    FailCount = lists:count(fun(X) -> X =:= fail end, Results),

    io:format("~n=== RESULTS ===~n", []),
    io:format("PASS: ~p~n", [PassCount]),
    io:format("FAIL: ~p~n", [FailCount]),
    io:format("TOTAL: ~p~n~n", [PassCount + FailCount]),

    case FailCount of
        0 -> halt(0);
        _ -> halt(1)
    end.

%%====================================================================
%% Test Functions
%%====================================================================

test_path_traversal_basic() ->
    Patterns = [
        <<"../../../etc/passwd">>,
        <<"..\\..\\..\\windows\\system32">>,
        <<"..%2f..%2f..%2fetc%2fpasswd">>
    ],
    lists:foreach(fun(Pattern) ->
        case erlmcp_uri_validator:validate_uri(Pattern) of
            {error, {path_traversal_detected, _}} -> ok;
            Other -> throw({unexpected, Pattern, Other})
        end
    end, Patterns).

test_path_traversal_encoded() ->
    Patterns = [
        <<"%2e%2e%2f%2e%2e%2fetc%2fpasswd">>,
        <<"%2E%2E%2F%2E%2E%2Fetc%2Fpasswd">>
    ],
    lists:foreach(fun(Pattern) ->
        case erlmcp_uri_validator:validate_uri(Pattern) of
            {error, {path_traversal_detected, _}} -> ok;
            Other -> throw({unexpected, Pattern, Other})
        end
    end, Patterns).

test_path_traversal_unicode() ->
    Pattern = <<"&#46;&#46;&#47;etc&#47;passwd">>,
    case erlmcp_uri_validator:validate_uri(Pattern) of
        {error, {path_traversal_detected, _}} -> ok;
        Other -> throw({unexpected, Pattern, Other})
    end.

test_path_traversal_mixed() ->
    Pattern = <<"..%2f..%5c">>,
    case erlmcp_uri_validator:validate_uri(Pattern) of
        {error, {path_traversal_detected, _}} -> ok;
        Other -> throw({unexpected, Pattern, Other})
    end.

test_header_rfc_compliance() ->
    ValidHeaders = [{<<"Content-Type">>, <<"application/json">>}],
    case erlmcp_http_header_validator:validate_header_names(ValidHeaders) of
        ok -> ok;
        Other -> throw({failed_valid_headers, Other})
    end.

test_header_null_bytes() ->
    NullByteHeader = [{<<"X-Test">>, <<"value", 0, "end">>}],
    case erlmcp_http_header_validator:validate_header_values(NullByteHeader) of
        {error, _} -> ok;
        Other -> throw({expected_rejection, Other})
    end.

test_header_crlf_injection() ->
    CrlfHeader = [{<<"X-Injection">>, <<"value\r\nX-Evil: injected">>}],
    case erlmcp_http_header_validator:check_header_injection(CrlfHeader) of
        {error, _} -> ok;
        Other -> throw({expected_rejection, Other})
    end.

test_valid_uris() ->
    ValidURIs = [
        <<"https://example.com/api/resource">>,
        <<"/api/resource/123">>,
        <<"http://example.com/path">>
    ],
    lists:foreach(fun(Uri) ->
        case erlmcp_uri_validator:validate_uri(Uri) of
            ok -> ok;
            Other -> throw({unexpected_rejection, Uri, Other})
        end
    end, ValidURIs).

test_valid_headers() ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case erlmcp_http_header_validator:validate_header_names(Headers) of
        ok -> ok;
        Other -> throw({valid_header_failed, Other})
    end.

test_uri_determinism() ->
    TestPayload = <<"../../../etc/passwd">>,
    Results = [erlmcp_uri_validator:validate_uri(TestPayload) || _ <- lists:seq(1, 10)],
    case lists:all(fun(R) -> R =:= lists:nth(1, Results) end, Results) of
        true -> ok;
        false -> throw(non_deterministic_results)
    end.

test_header_determinism() ->
    Headers = [{<<"X-Test">>, <<"value\r\nX-Evil: injected">>}],
    Results = [erlmcp_http_header_validator:check_header_injection(Headers) || _ <- lists:seq(1, 10)],
    case lists:all(fun(R) -> R =:= lists:nth(1, Results) end, Results) of
        true -> ok;
        false -> throw(non_deterministic_results)
    end.

test_owasp_corpus() ->
    Corpus = [
        <<"../../../etc/passwd">>,
        <<"..%2f..%2fetc%2fpasswd">>,
        <<"file\0name">>,
        <<"path; cat /etc/passwd">>,
        <<"file`whoami`.txt">>,
        <<"&#46;&#46;&#47;">>
    ],
    RejectedCount = lists:foldl(fun(Payload, Count) ->
        case erlmcp_uri_validator:validate_uri(Payload) of
            {error, {path_traversal_detected, _}} -> Count + 1;
            {error, {null_byte_detected, _}} -> Count + 1;
            {error, {command_injection_detected, _}} -> Count + 1;
            _ -> Count
        end
    end, 0, Corpus),
    case RejectedCount >= length(Corpus) - 1 of
        true -> ok;
        false -> throw({insufficient_rejections, RejectedCount, length(Corpus)})
    end.

test_uri_latency() ->
    TestUri = <<"https://example.com/api/resource/123">>,
    Iterations = 100,
    {Micro1, _} = statistics(wall_clock),
    lists:foreach(fun(_) ->
        erlmcp_uri_validator:validate_uri(TestUri)
    end, lists:seq(1, Iterations)),
    {Micro2, _} = statistics(wall_clock),
    LatencyPerRequest = (Micro2 - Micro1) / Iterations,
    case LatencyPerRequest < 1000 of
        true -> ok;
        false -> throw({latency_exceeded, LatencyPerRequest})
    end.

test_header_latency() ->
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    Iterations = 100,
    {Micro1, _} = statistics(wall_clock),
    lists:foreach(fun(_) ->
        erlmcp_http_header_validator:validate_header_names(Headers)
    end, lists:seq(1, Iterations)),
    {Micro2, _} = statistics(wall_clock),
    LatencyPerRequest = (Micro2 - Micro1) / Iterations,
    case LatencyPerRequest < 1000 of
        true -> ok;
        false -> throw({latency_exceeded, LatencyPerRequest})
    end.

test_fuzz_no_crash() ->
    RandomCount = 50,
    Results = lists:map(fun(N) ->
        RandomUri = generate_random_uri(N),
        try
            {ok, erlmcp_uri_validator:validate_uri(RandomUri)}
        catch
            _:_ ->
                crash
        end
    end, lists:seq(1, RandomCount)),
    Crashes = lists:count(fun(X) -> X =:= crash end, Results),
    case Crashes of
        0 -> ok;
        _ -> throw({crashes_detected, Crashes})
    end.

%%====================================================================
%% Helpers
%%====================================================================

generate_random_uri(Seed) ->
    S1 = erlang:phash2(Seed, 1000),
    S2 = erlang:phash2(Seed + 1, 1000),
    S3 = erlang:phash2(Seed + 2, 1000),
    Length = ((S1 * S2 + S3) rem 50) + 10,
    CharTypes = [47, 46, 45, 95],
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
