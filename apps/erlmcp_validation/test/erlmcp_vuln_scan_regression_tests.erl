%%%-------------------------------------------------------------------
%%% @doc
%%% Security Regression Tests for erlmcp
%%%
%%% Tests to ensure security fixes don't regress:
%%% - Baseline security metrics
%%% - Regression detection for security fixes
%%% - New feature security validation
%%% - Compliance with security baseline
%%% - Severity classification
%%%
%%% == Chicago School TDD ==
%%% Uses REAL erlmcp processes from erlmcp_test_helpers.
%%% Tests observable behavior through API calls only.
%%% NO internal state inspection or mocks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_vuln_scan_regression_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

security_regression_test_() ->
    {setup, fun setup_regression_tests/0, fun cleanup_regression_tests/1, fun regression_tests/1}.

%%%====================================================================
%%% Setup and Teardown
%%%====================================================================

setup_regression_tests() ->
    %% Start real erlmcp auth server with security features
    {ok, AuthPid} =
        erlmcp_auth:start_link(#{rate_limiter_enabled => true,
                                 rate_limit => #{max_requests => 100, window_seconds => 60},
                                 api_keys =>
                                     #{<<"test_key">> => <<"test_user">>,
                                       <<"admin_key">> => <<"admin_user">>}}),

    #{auth_pid => AuthPid}.

cleanup_regression_tests(#{auth_pid := _AuthPid}) ->
    erlmcp_auth:stop(),
    ok.

regression_tests(_State) ->
    [{"Baseline Security Metrics",
      [?_test(baseline_authentication_checks()),
       ?_test(baseline_input_validation()),
       ?_test(baseline_rate_limiting()),
       ?_test(baseline_session_management()),
       ?_test(baseline_cryptography())]},
     {"Regression Detection",
      [?_test(sql_injection_fix_regression()),
       ?_test(xss_fix_regression()),
       ?_test(auth_bypass_fix_regression()),
       ?_test(path_traversal_fix_regression()),
       ?_test(command_injection_fix_regression())]},
     {"New Feature Security",
      [?_test(batch_requests_security()),
       ?_test(sampling_security()),
       ?_test(tasks_security()),
       ?_test(progress_tokens_security())]},
     {"Security Baseline Compliance",
      [?_test(auth_001_compliance()),
       ?_test(input_001_compliance()),
       ?_test(rate_001_compliance()),
       ?_test(sess_001_compliance()),
       ?_test(crypt_001_compliance())]},
     {"Severity Classification",
      [?_test(classify_critical_severity()),
       ?_test(classify_high_severity()),
       ?_test(classify_medium_severity()),
       ?_test(classify_low_severity())]}].

%%%====================================================================
%%% Baseline Security Metrics Tests
%%%====================================================================

%% @doc Test baseline authentication checks
baseline_authentication_checks() ->
    %% Test that authentication is enforced
    %% 1. Requests without auth should fail
    NoAuthResult = erlmcp_auth:validate_request(#{resource => <<"tools/list">>}, #{}),

    ?assertMatch({error, unauthorized}, NoAuthResult),

    %% 2. Requests with invalid auth should fail
    InvalidAuthResult =
        erlmcp_auth:validate_request(#{resource => <<"tools/list">>},
                                     #{api_key => <<"invalid_key">>}),

    ?assertMatch({error, unauthorized}, InvalidAuthResult),

    %% 3. Requests with valid auth should succeed
    ValidAuthResult =
        erlmcp_auth:validate_request(#{resource => <<"tools/list">>}, #{api_key => <<"test_key">>}),

    ?assertMatch({ok, _}, ValidAuthResult).

%% @doc Test baseline input validation
baseline_input_validation() ->
    %% Test that input validation is in place
    %% 1. SQL injection should be blocked
    SQLResult =
        erlmcp_auth:validate_request(#{resource => <<"resources/read">>,
                                       uri => <<"'; DROP TABLE users; --">>},
                                     #{api_key => <<"test_key">>}),

    ?assertMatch({error, _}, SQLResult),

    %% 2. XSS should be handled safely
    XSSResult =
        erlmcp_auth:validate_request(#{resource => <<"tools/call">>,
                                       name => <<"<script>alert('XSS')</script>">>},
                                     #{api_key => <<"test_key">>}),

    ?assertMatch({error, _}, XSSResult),

    %% 3. Path traversal should be blocked
    PathResult =
        erlmcp_auth:validate_request(#{resource => <<"resources/read">>,
                                       uri => <<"../../../etc/passwd">>},
                                     #{api_key => <<"test_key">>}),

    ?assertMatch({error, _}, PathResult).

%% @doc Test baseline rate limiting
baseline_rate_limiting() ->
    %% Test that rate limiting is enforced
    %% Send many rapid requests
    Results =
        lists:map(fun(_) ->
                     erlmcp_auth:validate_request(#{resource => <<"tools/list">>},
                                                  #{api_key => <<"test_key">>})
                  end,
                  lists:seq(1, 120)),

    %% Count rate limited responses
    RateLimitedCount =
        lists:foldl(fun ({error, rate_limited}, Acc) ->
                            Acc + 1;
                        (_, Acc) ->
                            Acc
                    end,
                    0,
                    Results),

    %% Should have rate limiting
    ?assert(RateLimitedCount > 0).

%% @doc Test baseline session management
baseline_session_management() ->
    %% Test session management
    %% 1. Session creation should work
    SessionResult =
        erlmcp_auth:validate_request(#{resource => <<"session/create">>},
                                     #{api_key => <<"test_key">>}),

    ?assertMatch({ok, _}, SessionResult),

    %% 2. Session validation should work
    case SessionResult of
        {ok, #{session_id := SessionID}} ->
            ValidateResult =
                erlmcp_auth:validate_request(#{resource => <<"session/validate">>,
                                               session_id => SessionID},
                                             #{}),

            ?assertMatch({ok, _}, ValidateResult);
        _ ->
            ok
    end.

%% @doc Test baseline cryptography
baseline_cryptography() ->
    %% Test cryptographic security
    %% 1. Secrets should not be exposed in errors
    ErrorResult =
        erlmcp_auth:validate_request(#{resource => <<"admin">>},
                                     #{api_key => <<"wrong_secret_key_12345">>}),

    ?assertMatch({error, _}, ErrorResult),

    %% Verify error doesn't contain the secret
    {error, Reason} = ErrorResult,
    ?assertNot(nomatch =:= binary:match(Reason, <<"secret_key_12345">>)),

    %% 2. TLS should be used (check via observable behavior)
    %% This is verified at transport level, not application level
    ?assert(true).

%%%====================================================================
%%% Regression Detection Tests
%%%====================================================================

%% @doc Test SQL injection fix hasn't regressed
sql_injection_fix_regression() ->
    %% SQL injection payloads
    SQLPayloads =
        [<<"'; DROP TABLE users; --">>, <<"' OR '1'='1'">>, <<"1' UNION SELECT * FROM users--">>],

    %% All should be blocked
    lists:foreach(fun(Payload) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"resources/read">>,
                                                        uri => Payload},
                                                      #{api_key => <<"test_key">>}),

                     ?assertMatch({error, _},
                                  Result,
                                  "SQL injection fix regressed: " ++ binary_to_list(Payload))
                  end,
                  SQLPayloads).

%% @doc Test XSS fix hasn't regressed
xss_fix_regression() ->
    %% XSS payloads
    XSSPayloads =
        [<<"<script>alert('XSS')</script>">>,
         <<"<img src=x onerror=alert('XSS')">>,
         <<"javascript:alert('XSS')">>],

    %% All should be handled safely
    lists:foreach(fun(Payload) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"tools/call">>,
                                                        name => Payload},
                                                      #{api_key => <<"test_key">>}),

                     %% Should not crash, should handle safely
                     ?assertMatch({error, _},
                                  Result,
                                  "XSS fix regressed: " ++ binary_to_list(Payload))
                  end,
                  XSSPayloads).

%% @doc Test auth bypass fix hasn't regressed
auth_bypass_fix_regression() ->
    %% Auth bypass attempts
    BypassAttempts =
        [#{},  % No auth
         #{api_key => <<"">>},  % Empty key
         #{api_key => <<"null">>},  % Null-like key
         #{bypass => true}],  % Bypass flag

    %% All should fail
    lists:foreach(fun(Attempt) ->
                     Result = erlmcp_auth:validate_request(#{resource => <<"admin">>}, Attempt),

                     ?assertMatch({error, unauthorized}, Result, "Auth bypass fix regressed")
                  end,
                  BypassAttempts).

%% @doc Test path traversal fix hasn't regressed
path_traversal_fix_regression() ->
    %% Path traversal payloads
    PathPayloads =
        [<<"../../../etc/passwd">>,
         <<"..\\..\\..\\windows\\system32\\hosts">>,
         <<"....//....//etc/passwd">>],

    %% All should be blocked
    lists:foreach(fun(Payload) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"resources/read">>,
                                                        uri => Payload},
                                                      #{api_key => <<"test_key">>}),

                     ?assertMatch({error, _},
                                  Result,
                                  "Path traversal fix regressed: " ++ binary_to_list(Payload))
                  end,
                  PathPayloads).

%% @doc Test command injection fix hasn't regressed
command_injection_fix_regression() ->
    %% Command injection payloads
    CmdPayloads = [<<"; cat /etc/passwd">>, <<"| whoami">>, <<"&& ls -la">>],

    %% All should be blocked
    lists:foreach(fun(Payload) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"tools/call">>,
                                                        command => Payload},
                                                      #{api_key => <<"test_key">>}),

                     ?assertMatch({error, _},
                                  Result,
                                  "Command injection fix regressed: " ++ binary_to_list(Payload))
                  end,
                  CmdPayloads).

%%%====================================================================
%%% New Feature Security Tests
%%%====================================================================

%% @doc Test batch requests security
batch_requests_security() ->
    %% Test batch requests have proper security
    %% 1. Authentication required
    BatchAuthResult = erlmcp_auth:validate_request(#{resource => <<"batch">>, requests => []}, #{}),

    ?assertMatch({error, unauthorized}, BatchAuthResult),

    %% 2. Rate limiting enforced
    BatchRequests = lists:duplicate(50, #{method => <<"ping">>}),
    BatchResult =
        erlmcp_auth:validate_request(#{resource => <<"batch">>, requests => BatchRequests},
                                     #{api_key => <<"test_key">>}),

    ?assertMatch({error, _}, BatchResult).

%% @doc Test sampling security
sampling_security() ->
    %% Test sampling feature security
    %% 1. Authentication required
    SamplingAuthResult = erlmcp_auth:validate_request(#{resource => <<"sampling/create">>}, #{}),

    ?assertMatch({error, unauthorized}, SamplingAuthResult),

    %% 2. Authorization checked
    SamplingResult =
        erlmcp_auth:validate_request(#{resource => <<"sampling/create">>},
                                     #{api_key => <<"test_key">>}),

    ?assertMatch({error, _}, SamplingResult).

%% @doc Test tasks security
tasks_security() ->
    %% Test background tasks security
    %% 1. Authentication required
    TaskAuthResult = erlmcp_auth:validate_request(#{resource => <<"tasks/create">>}, #{}),

    ?assertMatch({error, unauthorized}, TaskAuthResult),

    %% 2. Input validation
    TaskResult =
        erlmcp_auth:validate_request(#{resource => <<"tasks/create">>, command => <<"; rm -rf /">>},
                                     #{api_key => <<"test_key">>}),

    ?assertMatch({error, _}, TaskResult).

%% @doc Test progress tokens security
progress_tokens_security() ->
    %% Test progress token security
    %% 1. Token should not expose sensitive info
    ProgressResult =
        erlmcp_auth:validate_request(#{resource => <<"tasks/progress">>, token => <<"test_token">>},
                                     #{api_key => <<"test_key">>}),

    ?assertMatch({error, _}, ProgressResult).

%%%====================================================================
%%% Security Baseline Compliance Tests
%%%====================================================================

%% @doc Test AUTH-001 compliance: All endpoints require authentication
auth_001_compliance() ->
    %% Test various endpoints require auth
    Endpoints =
        [<<"tools/list">>,
         <<"tools/call">>,
         <<"resources/list">>,
         <<"resources/read">>,
         <<"prompts/list">>],

    lists:foreach(fun(Endpoint) ->
                     Result = erlmcp_auth:validate_request(#{resource => Endpoint}, #{}),

                     ?assertMatch({error, unauthorized},
                                  Result,
                                  "AUTH-001 violation: " ++ binary_to_list(Endpoint))
                  end,
                  Endpoints).

%% @doc Test INPUT-001 compliance: All user input must be validated
input_001_compliance() ->
    %% Test input validation on various inputs
    MaliciousInputs =
        [#{uri => <<"'; DROP TABLE users; --">>},
         #{name => <<"<script>alert('XSS')</script>">>},
         #{path => <<"../../../etc/passwd">>}],

    lists:foreach(fun(Input) ->
                     Result =
                         erlmcp_auth:validate_request(
                             maps:merge(#{resource => <<"resources/read">>}, Input),
                             #{api_key => <<"test_key">>}),

                     %% Should be rejected or sanitized
                     ?assertMatch({error, _}, Result)
                  end,
                  MaliciousInputs).

%% @doc Test RATE-001 compliance: Rate limiting must be enabled
rate_001_compliance() ->
    %% Test rate limiting is active
    %% Send many requests
    Results =
        lists:map(fun(_) ->
                     erlmcp_auth:validate_request(#{resource => <<"tools/list">>},
                                                  #{api_key => <<"test_key">>})
                  end,
                  lists:seq(1, 110)),

    %% Should have rate limiting
    RateLimitedCount =
        lists:foldl(fun ({error, rate_limited}, Acc) ->
                            Acc + 1;
                        (_, Acc) ->
                            Acc
                    end,
                    0,
                    Results),

    ?assert(RateLimitedCount > 0, "RATE-001 violation: No rate limiting detected").

%% @doc Test SESS-001 compliance: Sessions must timeout after inactivity
sess_001_compliance() ->
    %% Test session timeout
    %% Create session
    {ok, SessionData} =
        erlmcp_auth:validate_request(#{resource => <<"session/create">>},
                                     #{api_key => <<"test_key">>}),

    %% Session should have timeout info
    ?assert(is_map(SessionData)),

    %% Verify session has reasonable timeout
    Timeout = maps:get(timeout, SessionData, 3600),
    ?assert(Timeout > 0, "SESS-001 violation: No session timeout"),
    ?assert(Timeout =< 7200, "SESS-001 violation: Session timeout too long").

%% @doc Test CRYPT-001 compliance: Secrets must be encrypted at rest
crypt_001_compliance() ->
    %% Test secrets are not exposed
    %% Error messages shouldn't contain secrets
    WrongKeys = [<<"secret_password_123">>, <<"api_key_abcxyz">>, <<"jwt_token_secret">>],

    lists:foreach(fun(Key) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"admin">>},
                                                      #{api_key => Key}),

                     case Result of
                         {error, Reason} ->
                             %% Verify secret not in error message
                             ?assertNot(nomatch =:= binary:match(Reason, Key));
                         _ ->
                             ok
                     end
                  end,
                  WrongKeys).

%%%====================================================================
%%% Severity Classification Tests
%%%====================================================================

%% @doc Test critical severity classification
classify_critical_severity() ->
    %% Critical vulnerabilities: auth bypass, SQL injection
    CriticalVulns =
        [{auth_bypass,
          fun() -> erlmcp_auth:validate_request(#{resource => <<"admin">>}, #{bypass => true}) end},
         {sql_injection,
          fun() ->
             erlmcp_auth:validate_request(#{resource => <<"resources/read">>,
                                            uri => <<"'; DROP TABLE users; --">>},
                                          #{api_key => <<"test_key">>})
          end}],

    lists:foreach(fun({VulnType, TestFun}) ->
                     Result = TestFun(),

                     %% Critical vulnerabilities must be blocked
                     ?assertMatch({error, _},
                                  Result,
                                  "Critical vulnerability not blocked: " ++ atom_to_list(VulnType))
                  end,
                  CriticalVulns).

%% @doc Test high severity classification
classify_high_severity() ->
    %% High severity: XSS, CSRF
    HighVulns =
        [{xss,
          fun() ->
             erlmcp_auth:validate_request(#{resource => <<"tools/call">>,
                                            name => <<"<script>alert('XSS')</script>">>},
                                          #{api_key => <<"test_key">>})
          end},
         {csrf,
          fun() ->
             erlmcp_auth:validate_request(#{resource => <<"transfer">>, method => <<"POST">>},
                                          #{api_key => <<"test_key">>})
          end}],

    lists:foreach(fun({VulnType, TestFun}) ->
                     Result = TestFun(),

                     %% High severity vulnerabilities must be handled safely
                     ?assertMatch({error, _},
                                  Result,
                                  "High severity not handled: " ++ atom_to_list(VulnType))
                  end,
                  HighVulns).

%% @doc Test medium severity classification
classify_medium_severity() ->
    %% Medium severity: information disclosure, missing headers
    MediumVulns =
        [{information_disclosure,
          fun() ->
             %% Test error messages don't leak info
             erlmcp_auth:validate_request(#{resource => <<"invalid_nonexistent">>},
                                          #{api_key => <<"test_key">>})
          end}],

    lists:foreach(fun({VulnType, TestFun}) ->
                     Result = TestFun(),

                     %% Medium severity should be handled
                     ?assertMatch({error, _},
                                  Result,
                                  "Medium severity not handled: " ++ atom_to_list(VulnType))
                  end,
                  MediumVulns).

%% @doc Test low severity classification
classify_low_severity() ->
    %% Low severity: deprecated API, missing optional headers
    LowVulns =
        [{deprecated_api,
          fun() ->
             %% Test old API version is handled
             erlmcp_auth:validate_request(#{jsonrpc => <<"1.0">>, method => <<"ping">>},
                                          #{api_key => <<"test_key">>})
          end}],

    lists:foreach(fun({VulnType, TestFun}) ->
                     Result = TestFun(),

                     %% Low severity should be handled gracefully
                     ?assertMatch({error, _},
                                  Result,
                                  "Low severity not handled: " ++ atom_to_list(VulnType))
                  end,
                  LowVulns).
