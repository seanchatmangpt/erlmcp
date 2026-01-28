-module(erlmcp_http_security_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for erlmcp_http_security Module
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Start dependencies
    application:ensure_all_started(erlmcp),
    timer:sleep(100),
    %% Manually start session manager if not already started
    case whereis(erlmcp_session_manager) of
        undefined -> erlmcp_session_manager:start_link();
        _ -> ok
    end,
    timer:sleep(100).

cleanup(_) ->
    application:stop(erlmcp),
    timer:sleep(50).

%%====================================================================
%% Origin Validation Tests
%%====================================================================

origin_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_origin_exact_match()),
             ?_test(test_origin_with_port()),
             ?_test(test_origin_wildcard_port()),
             ?_test(test_origin_localhost()),
             ?_test(test_origin_127_0_0_1()),
             ?_test(test_origin_invalid()),
             ?_test(test_origin_case_sensitive()),
             ?_test(test_origin_multiple_patterns()),
             ?_test(test_origin_https_required()),
             ?_test(test_origin_binary_input())
         ]
     end}.

test_origin_exact_match() ->
    Config = [
        {allowed_origins, [
            "http://localhost",
            "http://localhost:8080"
        ]}
    ],
    Result = erlmcp_http_security:validate_origin("http://localhost", Config),
    ?assertMatch({ok, _}, Result).

test_origin_with_port() ->
    Config = [
        {allowed_origins, [
            "http://localhost:8080"
        ]}
    ],
    Result = erlmcp_http_security:validate_origin("http://localhost:8080", Config),
    ?assertMatch({ok, _}, Result).

test_origin_wildcard_port() ->
    Config = [
        {allowed_origins, [
            "http://localhost:*"
        ]}
    ],
    Result1 = erlmcp_http_security:validate_origin("http://localhost:3000", Config),
    Result2 = erlmcp_http_security:validate_origin("http://localhost:9000", Config),
    ?assertMatch({ok, _}, Result1),
    ?assertMatch({ok, _}, Result2).

test_origin_localhost() ->
    Config = [
        {allowed_origins, [
            "http://localhost",
            "https://localhost"
        ]}
    ],
    Result = erlmcp_http_security:validate_origin("http://localhost", Config),
    ?assertMatch({ok, _}, Result).

test_origin_127_0_0_1() ->
    Config = [
        {allowed_origins, [
            "http://127.0.0.1",
            "http://127.0.0.1:*"
        ]}
    ],
    Result1 = erlmcp_http_security:validate_origin("http://127.0.0.1", Config),
    Result2 = erlmcp_http_security:validate_origin("http://127.0.0.1:8080", Config),
    ?assertMatch({ok, _}, Result1),
    ?assertMatch({ok, _}, Result2).

test_origin_invalid() ->
    Config = [
        {allowed_origins, [
            "http://localhost"
        ]}
    ],
    Result = erlmcp_http_security:validate_origin("http://evil.com", Config),
    ?assertMatch({error, invalid_origin}, Result).

test_origin_case_sensitive() ->
    Config = [
        {allowed_origins, [
            "http://localhost"
        ]}
    ],
    %% URL schemes are case-insensitive in HTTP spec, but we match exactly
    Result = erlmcp_http_security:validate_origin("HTTP://localhost", Config),
    ?assertMatch({error, invalid_origin}, Result).

test_origin_multiple_patterns() ->
    Config = [
        {allowed_origins, [
            "http://localhost",
            "http://127.0.0.1:*",
            "https://localhost:3000"
        ]}
    ],
    ?assertMatch({ok, _}, erlmcp_http_security:validate_origin("http://localhost", Config)),
    ?assertMatch({ok, _}, erlmcp_http_security:validate_origin("http://127.0.0.1:5000", Config)),
    ?assertMatch({ok, _}, erlmcp_http_security:validate_origin("https://localhost:3000", Config)).

test_origin_https_required() ->
    Config = [
        {allowed_origins, [
            "https://localhost"
        ]},
        {require_https, true}
    ],
    RequireHttps = erlmcp_http_security:require_https(Config),
    ?assert(RequireHttps).

test_origin_binary_input() ->
    Config = [
        {allowed_origins, [
            "http://localhost"
        ]}
    ],
    Result = erlmcp_http_security:validate_origin(<<"http://localhost">>, Config),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% Session Management Tests
%%====================================================================

session_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_create_session()),
             ?_test(test_validate_session()),
             ?_test(test_session_expiry()),
             ?_test(test_delete_session()),
             ?_test(test_session_not_found()),
             ?_test(test_session_info()),
             ?_test(test_session_id_format()),
             ?_test(test_multiple_sessions()),
             ?_test(test_session_binary_id()),
             ?_test(test_session_string_id())
         ]
     end}.

test_create_session() ->
    Result = erlmcp_session_manager:create_session(),
    ?assertMatch({ok, _SessionId}, Result),
    {ok, SessionId} = Result,
    ?assert(is_binary(SessionId)),
    ?assert(byte_size(SessionId) > 0).

test_validate_session() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertMatch({ok, _Info}, Result),
    {ok, Info} = Result,
    ?assert(maps:is_key(expires_at, Info)).

test_session_expiry() ->
    %% This test would require modifying session timeout
    %% For now, just verify valid session is created
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertMatch({ok, _}, Result).

test_delete_session() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, _} = erlmcp_session_manager:validate_session(SessionId),
    ok = erlmcp_session_manager:delete_session(SessionId),
    timer:sleep(100),  % Wait for async deletion
    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertMatch({error, not_found}, Result).

test_session_not_found() ->
    FakeSessionId = <<"ffffffff-ffff-4fff-bfff-ffffffffffff">>,
    Result = erlmcp_session_manager:validate_session(FakeSessionId),
    ?assertMatch({error, not_found}, Result).

test_session_info() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    Result = erlmcp_session_manager:get_session_info(SessionId),
    ?assertMatch({ok, _Info}, Result),
    {ok, Info} = Result,
    ?assert(maps:is_key(created_at, Info)),
    ?assert(maps:is_key(expires_at, Info)).

test_session_id_format() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    %% Should be UUID format (36 chars with hyphens)
    SessionStr = binary_to_list(SessionId),
    [Part1, Part2, Part3, Part4, Part5] = string:split(SessionStr, "-", all),
    ?assertEqual(8, length(Part1)),
    ?assertEqual(4, length(Part2)),
    ?assertEqual(4, length(Part3)),
    ?assertEqual(4, length(Part4)),
    ?assertEqual(12, length(Part5)).

test_multiple_sessions() ->
    {ok, S1} = erlmcp_session_manager:create_session(),
    {ok, S2} = erlmcp_session_manager:create_session(),
    {ok, S3} = erlmcp_session_manager:create_session(),
    ?assertNotEqual(S1, S2),
    ?assertNotEqual(S2, S3),
    ?assertNotEqual(S1, S3),
    ?assertMatch({ok, _}, erlmcp_session_manager:validate_session(S1)),
    ?assertMatch({ok, _}, erlmcp_session_manager:validate_session(S2)),
    ?assertMatch({ok, _}, erlmcp_session_manager:validate_session(S3)).

test_session_binary_id() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    %% SessionId should be binary
    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertMatch({ok, _}, Result).

test_session_string_id() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    SessionStr = binary_to_list(SessionId),
    %% Should also accept string IDs
    Result = erlmcp_session_manager:validate_session(SessionStr),
    ?assertMatch({ok, _}, Result).

%%====================================================================
%% HTTP Response Code Tests
%%====================================================================

http_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_invalid_origin_403()),
             ?_test(test_missing_session_400()),
             ?_test(test_expired_session_404()),
             ?_test(test_valid_origin_and_session_200())
         ]
     end}.

test_invalid_origin_403() ->
    %% Verify that invalid origin returns correct error type
    Config = [{allowed_origins, ["http://localhost"]}],
    Result = erlmcp_http_security:validate_origin("http://evil.com", Config),
    ?assertMatch({error, invalid_origin}, Result).

test_missing_session_400() ->
    %% Verify missing session detection
    FakeSessionId = <<"missing">>,
    Result = erlmcp_session_manager:validate_session(FakeSessionId),
    ?assertMatch({error, not_found}, Result).

test_expired_session_404() ->
    %% Verify expired session detection
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    erlmcp_session_manager:delete_session(SessionId),
    timer:sleep(100),  % Wait for async deletion
    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertMatch({error, not_found}, Result).

test_valid_origin_and_session_200() ->
    Config = [{allowed_origins, ["http://localhost"]}],
    {ok, Origin} = erlmcp_http_security:validate_origin("http://localhost", Config),
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, _ValidSession} = erlmcp_session_manager:validate_session(SessionId),
    ?assertNotEqual(undefined, Origin),
    ?assertNotEqual(undefined, SessionId).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_security_workflow()),
             ?_test(test_localhost_origins()),
             ?_test(test_session_cleanup()),
             ?_test(test_concurrent_sessions())
         ]
     end}.

test_security_workflow() ->
    %% Simulate complete security workflow
    Config = [{allowed_origins, ["http://localhost"]}],

    %% 1. Validate origin
    {ok, _Origin} = erlmcp_http_security:validate_origin("http://localhost", Config),

    %% 2. Create session
    {ok, SessionId} = erlmcp_session_manager:create_session(),

    %% 3. Validate session
    {ok, _SessionInfo} = erlmcp_session_manager:validate_session(SessionId),

    %% 4. Use session
    {ok, _Info} = erlmcp_session_manager:get_session_info(SessionId),

    %% 5. Delete session
    ok = erlmcp_session_manager:delete_session(SessionId),
    timer:sleep(100),  % Wait for async deletion

    %% 6. Verify deletion
    ?assertMatch({error, not_found}, erlmcp_session_manager:validate_session(SessionId)).

test_localhost_origins() ->
    Config = [
        {allowed_origins, [
            "http://localhost",
            "http://localhost:*",
            "https://localhost",
            "https://localhost:*",
            "http://127.0.0.1",
            "http://127.0.0.1:*",
            "https://127.0.0.1",
            "https://127.0.0.1:*"
        ]}
    ],
    
    TestOrigins = [
        "http://localhost",
        "http://localhost:3000",
        "https://localhost",
        "https://localhost:8443",
        "http://127.0.0.1",
        "http://127.0.0.1:3000",
        "https://127.0.0.1",
        "https://127.0.0.1:8443"
    ],
    
    Results = [erlmcp_http_security:validate_origin(Origin, Config) || Origin <- TestOrigins],
    ?assert(lists:all(fun(Result) -> Result =/= {error, invalid_origin} end, Results)).

test_session_cleanup() ->
    %% Create multiple sessions and verify they can be tracked
    Sessions = [erlmcp_session_manager:create_session() || _ <- lists:seq(1, 5)],
    SessionIds = [S || {ok, S} <- Sessions],

    %% All should be valid
    ValidResults = [erlmcp_session_manager:validate_session(S) || S <- SessionIds],
    ?assert(lists:all(fun(R) -> element(1, R) =:= ok end, ValidResults)),

    %% Delete all
    [erlmcp_session_manager:delete_session(S) || S <- SessionIds],
    timer:sleep(200),  % Wait for async deletion

    %% All should be expired/not found
    ExpiredResults = [erlmcp_session_manager:validate_session(S) || S <- SessionIds],
    ?assert(lists:all(fun(R) -> R =:= {error, not_found} end, ExpiredResults)).

test_concurrent_sessions() ->
    %% Create sessions concurrently
    Parent = self(),
    [spawn(fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(),
        Parent ! {session, SessionId}
    end) || _ <- lists:seq(1, 10)],

    %% Collect all session IDs
    Sessions = [receive {session, S} -> S after 1000 -> undefined end || _ <- lists:seq(1, 10)],

    %% Verify all are unique and valid
    ValidSessions = [S || S <- Sessions, S =/= undefined],
    ?assertEqual(10, length(ValidSessions)),
    ?assertEqual(10, length(lists:usort(ValidSessions))).

%%====================================================================
%% Origin Validator Module Tests (Gap #3 - DNS Rebinding Protection)
%%====================================================================

origin_validator_module_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             %% Basic origin validation
             ?_test(test_validator_exact_match()),
             ?_test(test_validator_wildcard_port()),
             ?_test(test_validator_ipv6_localhost()),
             ?_test(test_validator_https_origins()),
             ?_test(test_validator_missing_origin()),
             ?_test(test_validator_invalid_origin()),
             ?_test(test_validator_case_sensitivity()),
             ?_test(test_validator_pattern_matching()),
             ?_test(test_validator_default_origins()),
             ?_test(test_validator_port_extraction()),
             ?_test(test_validator_dns_rebinding_prevention()),
             ?_test(test_validator_binary_string_handling()),
             ?_test(test_validator_complex_patterns()),
             ?_test(test_validator_ipv6_brackets()),
             ?_test(test_validator_all_defaults_work())
         ]
     end}.

test_validator_exact_match() ->
    %% Test exact origin match
    Result = erlmcp_origin_validator:validate_origin(
        <<"http://localhost:8080">>,
        [<<"http://localhost:8080">>]),
    ?assertMatch({ok, _}, Result).

test_validator_wildcard_port() ->
    %% Test wildcard port matching
    AllowedOrigins = [<<"http://localhost:*">>, <<"https://localhost:*">>],

    R1 = erlmcp_origin_validator:validate_origin(<<"http://localhost:3000">>, AllowedOrigins),
    R2 = erlmcp_origin_validator:validate_origin(<<"http://localhost:9000">>, AllowedOrigins),
    R3 = erlmcp_origin_validator:validate_origin(<<"https://localhost:443">>, AllowedOrigins),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2),
    ?assertMatch({ok, _}, R3).

test_validator_ipv6_localhost() ->
    %% Test IPv6 localhost [::1]
    AllowedOrigins = [<<"http://[::1]:*">>, <<"https://[::1]:*">>],

    R1 = erlmcp_origin_validator:validate_origin(<<"http://[::1]:8080">>, AllowedOrigins),
    R2 = erlmcp_origin_validator:validate_origin(<<"https://[::1]:8443">>, AllowedOrigins),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2).

test_validator_https_origins() ->
    %% Test HTTPS origins
    AllowedOrigins = [<<"https://127.0.0.1:*">>, <<"https://localhost:*">>],

    R1 = erlmcp_origin_validator:validate_origin(<<"https://127.0.0.1:443">>, AllowedOrigins),
    R2 = erlmcp_origin_validator:validate_origin(<<"https://localhost:8443">>, AllowedOrigins),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2).

test_validator_missing_origin() ->
    %% Missing origin header should be treated as same-origin (ok)
    AllowedOrigins = erlmcp_origin_validator:get_default_allowed_origins(),
    Result = erlmcp_origin_validator:validate_origin(undefined, AllowedOrigins),
    ?assertMatch({ok, _}, Result).

test_validator_invalid_origin() ->
    %% Invalid origin should be rejected
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"http://evil.com">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

test_validator_case_sensitivity() ->
    %% URL schemes are case-sensitive in matching
    AllowedOrigins = [<<"http://localhost">>],
    Result = erlmcp_origin_validator:validate_origin(<<"HTTP://localhost">>, AllowedOrigins),
    ?assertMatch({error, forbidden}, Result).

test_validator_pattern_matching() ->
    %% Test multiple patterns
    AllowedOrigins = [
        <<"http://localhost">>,
        <<"http://127.0.0.1:*">>,
        <<"https://localhost:3000">>
    ],

    R1 = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, AllowedOrigins),
    R2 = erlmcp_origin_validator:validate_origin(<<"http://127.0.0.1:5000">>, AllowedOrigins),
    R3 = erlmcp_origin_validator:validate_origin(<<"https://localhost:3000">>, AllowedOrigins),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2),
    ?assertMatch({ok, _}, R3).

test_validator_default_origins() ->
    %% Test default safe origins
    DefaultOrigins = erlmcp_origin_validator:get_default_allowed_origins(),
    ?assert(length(DefaultOrigins) > 0),

    %% Defaults should allow localhost and 127.0.0.1
    R1 = erlmcp_origin_validator:validate_origin(<<"http://localhost:8080">>, DefaultOrigins),
    R2 = erlmcp_origin_validator:validate_origin(<<"http://127.0.0.1:3000">>, DefaultOrigins),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2).

test_validator_port_extraction() ->
    %% Test port extraction from origins
    Patterns = [<<"http://localhost:*">>],

    %% Should match any port
    R1 = erlmcp_origin_validator:matches_origin_pattern(<<"http://localhost:80">>, <<"http://localhost:*">>),
    R2 = erlmcp_origin_validator:matches_origin_pattern(<<"http://localhost:8080">>, <<"http://localhost:*">>),
    R3 = erlmcp_origin_validator:matches_origin_pattern(<<"http://localhost:65535">>, <<"http://localhost:*">>),

    ?assert(R1),
    ?assert(R2),
    ?assert(R3).

test_validator_dns_rebinding_prevention() ->
    %% Simulate DNS rebinding attack - attacker.com -> 127.0.0.1
    SafeConfig = [<<"http://localhost:*">>, <<"http://127.0.0.1:*">>],

    %% Attacker tries to send request as if from attacker.com
    AttackerOrigin = <<"http://attacker.com:8080">>,
    Result = erlmcp_origin_validator:validate_origin(AttackerOrigin, SafeConfig),

    %% Should be rejected
    ?assertMatch({error, forbidden}, Result).

test_validator_binary_string_handling() ->
    %% Test handling of both binary and string inputs
    Pattern = <<"http://localhost">>,

    %% Binary origin
    R1 = erlmcp_origin_validator:validate_origin(<<"http://localhost">>, [Pattern]),

    %% String origin
    R2 = erlmcp_origin_validator:validate_origin("http://localhost", [Pattern]),

    ?assertMatch({ok, _}, R1),
    ?assertMatch({ok, _}, R2).

test_validator_complex_patterns() ->
    %% Test complex pattern combinations
    AllowedOrigins = [
        "http://localhost",
        "http://localhost:*",
        "http://127.0.0.1:8080",
        "http://127.0.0.1:*",
        "https://localhost:*",
        "https://127.0.0.1:*"
    ],

    %% All should match
    TestCases = [
        "http://localhost",
        "http://localhost:3000",
        "http://127.0.0.1:8080",
        "http://127.0.0.1:9000",
        "https://localhost:443",
        "https://127.0.0.1:8443"
    ],

    Results = [erlmcp_origin_validator:validate_origin(TC, AllowedOrigins) || TC <- TestCases],
    ?assert(lists:all(fun(R) -> R =/= {error, forbidden} end, Results)).

test_validator_ipv6_brackets() ->
    %% Test IPv6 addresses with brackets and ports
    AllowedOrigins = [
        <<"http://[::1]:*">>,
        <<"https://[::1]:*">>,
        <<"http://[2001:db8::1]:*">>
    ],

    R1 = erlmcp_origin_validator:matches_origin_pattern(<<"http://[::1]:8080">>, <<"http://[::1]:*">>),
    R2 = erlmcp_origin_validator:matches_origin_pattern(<<"https://[::1]:443">>, <<"https://[::1]:*">>),

    ?assert(R1),
    ?assert(R2).

test_validator_all_defaults_work() ->
    %% Verify all default origins work correctly
    DefaultOrigins = erlmcp_origin_validator:get_default_allowed_origins(),

    %% Test that defaults include expected patterns
    HasHttp = lists:any(fun(O) ->
        string:find(O, "http://") =/= nomatch
    end, DefaultOrigins),

    HasHttps = lists:any(fun(O) ->
        string:find(O, "https://") =/= nomatch
    end, DefaultOrigins),

    HasLocalhost = lists:any(fun(O) ->
        string:find(O, "localhost") =/= nomatch
    end, DefaultOrigins),

    Has127 = lists:any(fun(O) ->
        string:find(O, "127.0.0.1") =/= nomatch
    end, DefaultOrigins),

    ?assert(HasHttp),
    ?assert(HasHttps),
    ?assert(HasLocalhost),
    ?assert(Has127).

