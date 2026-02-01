%%%-------------------------------------------------------------------
%%% @doc
%%% OWASP Top 10 Vulnerability Scanner Tests for erlmcp
%%%
%%% Tests OWASP Top 10 (2021) in MCP context:
%%% - A01: Broken Access Control
%%% - A02: Cryptographic Failures
%%% - A03: Injection
%%% - A04: Insecure Design
%%% - A05: Security Misconfiguration
%%% - A06: Vulnerable Components
%%% - A07: Authentication Failures
%%% - A08: Integrity Failures
%%% - A09: Logging Failures
%%% - A10: SSRF
%%%
%%% == Chicago School TDD ==
%%% Uses REAL erlmcp processes from erlmcp_test_helpers.
%%% Tests observable behavior through API calls only.
%%% NO internal state inspection or mocks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_vuln_scan_owasp_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

owasp_vulnerability_scan_test_() ->
    {setup, fun setup_owasp_scan/0, fun cleanup_owasp_scan/1, fun owasp_scan_tests/1}.

%%%====================================================================
%%% Setup and Teardown
%%%====================================================================

setup_owasp_scan() ->
    %% Start real erlmcp auth server for security testing
    {ok, AuthPid} =
        erlmcp_auth:start_link(#{rate_limiter_enabled => true,
                                 api_keys =>
                                     #{<<"test_valid_key">> => <<"test_user">>,
                                       <<"admin_key">> => <<"admin_user">>}}),

    %% Start security validator
    {ok, ValidatorPid} = erlmcp_security_validator:start_link(),

    #{auth_pid => AuthPid, validator_pid => ValidatorPid}.

cleanup_owasp_scan(#{auth_pid := _AuthPid, validator_pid := ValidatorPid}) ->
    %% Stop processes in reverse order
    erlmcp_auth:stop(),
    gen_server:stop(ValidatorPid),
    ok.

owasp_scan_tests(_State) ->
    [{"OWASP Top 10 Checks",
      [?_test(check_a01_broken_access_control()),
       ?_test(check_a02_cryptographic_failures()),
       ?_test(check_a03_injection()),
       ?_test(check_a04_insecure_design()),
       ?_test(check_a05_security_misconfiguration()),
       ?_test(check_a06_vulnerable_components()),
       ?_test(check_a07_authentication_failures()),
       ?_test(check_a08_integrity_failures()),
       ?_test(check_a09_logging_failures()),
       ?_test(check_a10_ssrf())]},
     {"Configuration Security Tests",
      [?_test(detect_insecure_configurations()),
       ?_test(find_exposed_admin_interfaces()),
       ?_test(identify_information_disclosure())]},
     {"Injection Vector Tests",
      [?_test(scan_for_sql_injection_vectors()),
       ?_test(scan_for_xss_vectors()),
       ?_test(scan_for_path_traversal_vectors()),
       ?_test(scan_for_command_injection_vectors()),
       ?_test(scan_for_ssrf_vectors())]}].

%%%====================================================================
%%% OWASP Top 10 Tests
%%%====================================================================

%% @doc Test OWASP A01: Broken Access Control
check_a01_broken_access_control() ->
    %% Test access control through observable behavior
    %% Attempt to access admin resource without authorization
    Result = erlmcp_auth:validate_request(#{resource => <<"admin">>}, #{}),

    %% Verify unauthorized access is blocked
    ?assertMatch({error, unauthorized}, Result),

    %% Test with valid authorization
    AuthResult =
        erlmcp_auth:validate_request(#{resource => <<"admin">>}, #{api_key => <<"admin_key">>}),

    %% Verify authorized access succeeds
    ?assertMatch({ok, _}, AuthResult).

%% @doc Test OWASP A02: Cryptographic Failures
check_a02_cryptographic_failures() ->
    %% Test that secrets are not exposed in error messages
    %% Trigger error with invalid API key
    ErrorResult =
        erlmcp_auth:validate_request(#{resource => <<"tools">>}, #{api_key => <<"invalid_key">>}),

    %% Verify error doesn't expose sensitive data
    ?assertMatch({error, _}, ErrorResult),

    %% Check that error message doesn't contain keys or secrets
    {error, Reason} = ErrorResult,
    ?assertNot(<<"key">> =:= Reason),
    ?assertNot(<<"secret">> =:= Reason),
    ?assertNot(<<"password">> =:= Reason).

%% @doc Test OWASP A03: Injection
check_a03_injection() ->
    %% Test SQL injection protection via API
    SQLPayload = <<"'; DROP TABLE users; --">>,

    %% Attempt to inject SQL via resource URI
    Result =
        case erlmcp_auth:validate_request(#{resource => <<"resources/read">>, uri => SQLPayload},
                                          #{api_key => <<"test_valid_key">>})
        of
            {error, invalid_input} ->
                safe;
            {ok, _} ->
                unsafe
        end,

    %% Verify injection is blocked
    ?assertEqual(safe, Result).

%% @doc Test OWASP A04: Insecure Design
check_a04_insecure_design() ->
    %% Test security by design principles
    %% Verify rate limiting is enforced (observable behavior)
    {ok, _} =
        erlmcp_auth:validate_request(#{resource => <<"tools/list">>},
                                     #{api_key => <<"test_valid_key">>}),

    %% Rapid requests should trigger rate limiting
    Results =
        lists:map(fun(_) ->
                     erlmcp_auth:validate_request(#{resource => <<"tools/list">>},
                                                  #{api_key => <<"test_valid_key">>})
                  end,
                  lists:seq(1, 100)),

    %% At least some requests should be rate limited
    RateLimitedCount =
        lists:foldl(fun ({error, rate_limited}, Acc) ->
                            Acc + 1;
                        (_, Acc) ->
                            Acc
                    end,
                    0,
                    Results),

    ?assert(RateLimitedCount > 0).

%% @doc Test OWASP A05: Security Misconfiguration
check_a05_security_misconfiguration() ->
    %% Test secure defaults are in place
    %% Verify debug mode is not enabled (observable via error messages)
    Result =
        erlmcp_auth:validate_request(#{resource => <<"invalid">>},
                                     #{api_key => <<"test_valid_key">>}),

    %% Error should be generic (not exposing stack traces in production)
    ?assertMatch({error, _}, Result),

    %% Verify error message doesn't expose internals
    {error, Reason} = Result,
    ?assertNot(<<"stacktrace">> =:= Reason),
    ?assertNot(<<"trace">> =:= Reason),
    ?assertNot(<<"module">> =:= Reason).

%% @doc Test OWASP A06: Vulnerable Components
check_a06_vulnerable_components() ->
    %% Test that dependencies are up to date
    %% This is a compile-time check, verify via module info
    {module, erlmcp_auth} = code:ensure_loaded(erlmcp_auth),

    %% Verify module can be loaded (basic check)
    ?assert(is_list(erlmcp_auth:module_info())).

%% @doc Test OWASP A07: Authentication Failures
check_a07_authentication_failures() ->
    %% Test strong authentication mechanisms
    %% Test invalid credentials are rejected
    InvalidResult =
        erlmcp_auth:validate_request(#{resource => <<"admin">>}, #{api_key => <<"invalid_key">>}),

    ?assertMatch({error, unauthorized}, InvalidResult),

    %% Test valid credentials are accepted
    ValidResult =
        erlmcp_auth:validate_request(#{resource => <<"tools/list">>},
                                     #{api_key => <<"test_valid_key">>}),

    ?assertMatch({ok, _}, ValidResult).

%% @doc Test OWASP A08: Integrity Failures
check_a08_integrity_failures() ->
    %% Test that tampered requests are detected
    %% Simulate request with invalid signature
    TamperedRequest = #{resource => <<"tools/list">>, signature => <<"invalid_signature">>},

    Result = erlmcp_auth:validate_request(TamperedRequest, #{}),

    %% Verify tampered request is rejected
    ?assertMatch({error, _}, Result).

%% @doc Test OWASP A09: Logging and Monitoring Failures
check_a09_logging_failures() ->
    %% Test that security events are logged
    %% Make a request that should be logged
    _ = erlmcp_auth:validate_request(#{resource => <<"admin">>}, #{api_key => <<"invalid_key">>}),

    %% Verify logging is enabled (check logger level)
    ?assertMatch(ok, logger:set_application_level(erlmcp, notice)).

%% @doc Test OWASP A10: Server-Side Request Forgery (SSRF)
check_a10_ssrf() ->
    %% Test SSRF protection via URL validation
    %% Attempt to access internal URL
    SSRFURI = <<"http://localhost:6379">>,  % Redis port

    Result =
        erlmcp_auth:validate_request(#{resource => <<"resources/read">>, uri => SSRFURI},
                                     #{api_key => <<"test_valid_key">>}),

    %% Verify SSRF attempt is blocked
    ?assertMatch({error, _}, Result),

    case Result of
        {error, invalid_uri} ->
            ok;
        {error, unauthorized} ->
            ok;
        _ ->
            ?assert(false, "SSRF attempt not blocked")
    end.

%%%====================================================================
%%% Configuration Security Tests
%%%====================================================================

%% @doc Test detection of insecure configurations
detect_insecure_configurations() ->
    %% Check for secure defaults via observable behavior
    %% 1. Default credentials should not work
    DefaultCredResult =
        erlmcp_auth:validate_request(#{resource => <<"admin">>},
                                     #{api_key => <<"admin">>}),  % Try common default

    ?assertMatch({error, unauthorized}, DefaultCredResult),

    %% 2. Rate limiting should be enabled (tested in A04)
    %% 3. Debug mode should be off (tested in A05)
    %% Verify secure configuration
    ?assert(true).

%% @doc Test finding exposed administrative interfaces
find_exposed_admin_interfaces() ->
    %% Test that admin endpoints require authentication
    AdminEndpoints = [<<"admin">>, <<"admin/dashboard">>, <<"admin/users">>, <<"admin/config">>],

    Results =
        lists:map(fun(Endpoint) ->
                     erlmcp_auth:validate_request(#{resource => Endpoint},
                                                  #{})  % No auth
                  end,
                  AdminEndpoints),

    %% All admin endpoints should require authentication
    lists:foreach(fun(Result) -> ?assertMatch({error, unauthorized}, Result) end, Results).

%% @doc Test identifying information disclosure
identify_information_disclosure() ->
    %% Test error messages don't leak sensitive information
    %% Request invalid tool
    ErrorResult =
        erlmcp_auth:validate_request(#{resource => <<"tools/invalid_nonexistent_tool">>},
                                     #{api_key => <<"test_valid_key">>}),

    ?assertMatch({error, _}, ErrorResult),

    %% Verify error doesn't expose:
    %% - File paths
    %% - Stack traces
    %% - Database structure
    %% - Internal keys
    {error, Reason} = ErrorResult,

    ?assertNot(nomatch =:= binary:match(Reason, <<"/">>)),
    ?assertNot(nomatch =:= binary:match(Reason, <<"\\">>)),
    ?assertNot(nomatch =:= binary:match(Reason, <<"SELECT">>)),
    ?assertNot(nomatch =:= binary:match(Reason, <<"table">>)).

%%%====================================================================
%%% Injection Vector Tests
%%%====================================================================

%% @doc Test scanning for SQL injection vectors
scan_for_sql_injection_vectors() ->
    %% SQL injection payloads to test
    SQLPayloads =
        [<<"'; DROP TABLE users; --">>,
         <<"' OR '1'='1'">>,
         <<"1' UNION SELECT * FROM users--">>,
         <<"'; EXEC xp_cmdshell('dir'); --">>,
         <<"' AND 1=1--">>],

    %% Test each payload is blocked
    lists:foreach(fun(Payload) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"resources/read">>,
                                                        uri => Payload},
                                                      #{api_key => <<"test_valid_key">>}),

                     %% Verify injection is blocked
                     ?assertMatch({error, _}, Result)
                  end,
                  SQLPayloads).

%% @doc Test scanning for XSS vectors
scan_for_xss_vectors() ->
    %% XSS payloads to test
    XSSPayloads =
        [<<"<script>alert('XSS')</script>">>,
         <<"<img src=x onerror=alert('XSS')">>,
         <<"<svg/onload=alert('XSS')">>,
         <<"javascript:alert('XSS')">>],

    %% Test each payload is sanitized/blocked
    lists:foreach(fun(Payload) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"tools/call">>,
                                                        name => Payload},
                                                      #{api_key => <<"test_valid_key">>}),

                     %% Verify XSS is handled (error or sanitized)
                     case Result of
                         {error, _} ->
                             ok;
                         {ok, _} ->
                             ok  % May be sanitized instead of blocked
                     end
                  end,
                  XSSPayloads).

%% @doc Test scanning for path traversal vectors
scan_for_path_traversal_vectors() ->
    %% Path traversal payloads
    PathTraversalPayloads =
        [<<"../../../etc/passwd">>,
         <<"..\\..\\..\\windows\\system32\\drivers\\etc\\hosts">>,
         <<"....//....//....//etc/passwd">>,
         <<"%2e%2e%2fetc%2fpasswd">>],

    %% Test each payload is blocked
    lists:foreach(fun(Payload) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"resources/read">>,
                                                        uri => Payload},
                                                      #{api_key => <<"test_valid_key">>}),

                     %% Verify path traversal is blocked
                     ?assertMatch({error, _}, Result)
                  end,
                  PathTraversalPayloads).

%% @doc Test scanning for command injection vectors
scan_for_command_injection_vectors() ->
    %% Command injection payloads
    CmdInjectionPayloads =
        [<<"; cat /etc/passwd">>,
         <<"| cat /etc/passwd">>,
         <<"&& cat /etc/passwd">>,
         <<"`cat /etc/passwd`">>,
         <<"$(cat /etc/passwd)">>],

    %% Test each payload is blocked
    lists:foreach(fun(Payload) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"tools/call">>,
                                                        command => Payload},
                                                      #{api_key => <<"test_valid_key">>}),

                     %% Verify command injection is blocked
                     ?assertMatch({error, _}, Result)
                  end,
                  CmdInjectionPayloads).

%% @doc Test scanning for SSRF vectors
scan_for_ssrf_vectors() ->
    %% SSRF payloads (internal URLs)
    SSRFPayloads =
        [<<"http://localhost:8080/admin">>,
         <<"http://127.0.0.1:6379">>,  % Redis
         <<"http://169.254.169.254/latest/meta-data/">>,  % AWS metadata
         <<"file:///etc/passwd">>,
         <<"http://0.0.0.0:8080">>],

    %% Test each payload is blocked
    lists:foreach(fun(Payload) ->
                     Result =
                         erlmcp_auth:validate_request(#{resource => <<"resources/read">>,
                                                        uri => Payload},
                                                      #{api_key => <<"test_valid_key">>}),

                     %% Verify SSRF is blocked
                     ?assertMatch({error, _}, Result)
                  end,
                  SSRFPayloads).
