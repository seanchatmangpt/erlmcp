%%%-------------------------------------------------------------------
%%% @doc
%%% Authentication Mechanism Test Suite (EUnit)
%%%
%%% Tests for CLI authentication and authorization
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real auth processes
%%% - NO mocks, real authentication mechanisms
%%% - State-based verification (auth state, permissions)
%%%
%%% Coverage Target: ≥90%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_auth_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

auth_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Token validation - valid token", fun test_valid_token/0},
      {"Token validation - invalid token", fun test_invalid_token/0},
      {"Token validation - expired token", fun test_expired_token/0},
      {"Token validation - malformed token", fun test_malformed_token/0},
      {"Token refresh - before expiration", fun test_token_refresh_before_expiration/0},
      {"Token refresh - after expiration", fun test_token_refresh_after_expiration/0},
      {"mTLS authentication - valid certificate", fun test_mtls_valid_certificate/0},
      {"mTLS authentication - invalid certificate", fun test_mtls_invalid_certificate/0},
      {"mTLS authentication - missing certificate", fun test_mtls_missing_certificate/0},
      {"Rate limiting - under limit", fun test_rate_limiting_under_limit/0},
      {"Rate limiting - at limit", fun test_rate_limiting_at_limit/0},
      {"Rate limiting - over limit", fun test_rate_limiting_over_limit/0},
      {"Rate limiting - window reset", fun test_rate_limiting_window_reset/0},
      {"Session authentication - valid session", fun test_session_valid/0},
      {"Session authentication - invalid session", fun test_session_invalid/0},
      {"Session authentication - expired session", fun test_session_expired/0},
      {"Authorization - permission check", fun test_authorization_permission_check/0},
      {"Authorization - role-based access", fun test_authorization_role_based/0},
      {"Authorization - resource-based access", fun test_authorization_resource_based/0},
      {"Authentication failure - lockout", fun test_auth_failure_lockout/0},
      {"Authentication failure - recovery", fun test_auth_failure_recovery/0}]}.

setup() ->
    application:ensure_all_started(erlmcp_cli),
    %% Reset authentication state
    erlmcp_cli_secrets:reset(),
    ok.

cleanup(_Args) ->
    application:stop(erlmcp_cli),
    ok.

%%%====================================================================
%%% Token Validation Tests
%%%====================================================================

test_valid_token() ->
    %% Generate valid token
    {ok, Token} = erlmcp_cli_secrets:generate_token(#{user => <<"test_user">>},
                                                     3600),

    %% Validate token
    {ok, Claims} = erlmcp_cli_secrets:validate_token(Token),

    %% Verify claims
    ?assertEqual(<<"test_user">>, maps:get(<<"user">>, Claims)),
    ?assert(maps:get(<<"exp">>, Claims) > erlang:system_time(second)).

test_invalid_token() ->
    %% Invalid token
    InvalidToken = <<"invalid.token.here">>,

    %% Validate (should fail)
    {error, {invalid_token, _}} = erlmcp_cli_secrets:validate_token(InvalidToken).

test_expired_token() ->
    %% Generate token with very short expiration
    {ok, Token} = erlmcp_cli_secrets:generate_token(#{user => <<"test_user">>},
                                                     1),

    %% Wait for expiration
    timer:sleep(1100),

    %% Validate (should fail)
    {error, {token_expired, _}} = erlmcp_cli_secrets:validate_token(Token).

test_malformed_token() ->
    %% Malformed token
    MalformedToken = <<"not-a-jwt">>,

    %% Validate (should fail)
    {error, {malformed_token, _}} = erlmcp_cli_secrets:validate_token(MalformedToken).

%%%====================================================================
%%% Token Refresh Tests
%%%====================================================================

test_token_refresh_before_expiration() ->
    %% Generate token
    {ok, OldToken} = erlmcp_cli_secrets:generate_token(#{user => <<"test_user">>},
                                                         3600),

    %% Refresh token
    {ok, NewToken} = erlmcp_cli_secrets:refresh_token(OldToken),

    %% Verify new token is different
    ?assertNotEqual(OldToken, NewToken),

    %% Validate new token
    {ok, Claims} = erlmcp_cli_secrets:validate_token(NewToken),
    ?assertEqual(<<"test_user">>, maps:get(<<"user">>, Claims)).

test_token_refresh_after_expiration() ->
    %% Generate token with short expiration
    {ok, OldToken} = erlmcp_cli_secrets:generate_token(#{user => <<"test_user">>},
                                                         1),

    %% Wait for expiration
    timer:sleep(1100),

    %% Try to refresh (should fail)
    {error, {token_expired, _}} = erlmcp_cli_secrets:refresh_token(OldToken).

%%%====================================================================
%%% mTLS Authentication Tests
%%%====================================================================

test_mtls_valid_certificate() ->
    %% Valid mTLS certificate
    %% In real scenario, this would use actual certificate
    Cert = #{subject => <<"CN=test.example.com">>,
             issuer => <<"CN=ca.example.com">>,
             valid_from => erlang:system_time(second) - 3600,
             valid_to => erlang:system_time(second) + 3600},

    %% Authenticate with certificate
    {ok, Identity} = erlmcp_cli_secrets:authenticate_mtls(Cert),

    %% Verify identity
    ?assertEqual(<<"test.example.com">>, Identity).

test_mtls_invalid_certificate() ->
    %% Invalid mTLS certificate (expired)
    ExpiredCert = #{subject => <<"CN=test.example.com">>,
                    issuer => <<"CN=ca.example.com">>,
                    valid_from => erlang:system_time(second) - 7200,
                    valid_to => erlang:system_time(second) - 3600},

    %% Authenticate (should fail)
    {error, {certificate_expired, _}} = erlmcp_cli_secrets:authenticate_mtls(ExpiredCert).

test_mtls_missing_certificate() ->
    %% Missing certificate
    %% Authenticate without certificate (should fail)
    {error, {certificate_missing, _}} = erlmcp_cli_secrets:authenticate_mtls(undefined).

%%%====================================================================
%%% Rate Limiting Tests
%%%====================================================================

test_rate_limiting_under_limit() ->
    %% Reset rate limiter
    ClientId = <<"test_client_under">>,
    ok = erlmcp_cli_secrets:reset_rate_limit(ClientId),

    %% Make requests under limit
    lists:foreach(fun(_) ->
        ok = erlmcp_cli_secrets:check_rate_limit(ClientId, 100, 60)
    end, lists:seq(1, 50)),

    %% Still under limit
    ok = erlmcp_cli_secrets:check_rate_limit(ClientId, 100, 60).

test_rate_limiting_at_limit() ->
    %% Reset rate limiter
    ClientId = <<"test_client_at">>,
    ok = erlmcp_cli_secrets:reset_rate_limit(ClientId),

    %% Make requests up to limit
    lists:foreach(fun(_) ->
        ok = erlmcp_cli_secrets:check_rate_limit(ClientId, 10, 60)
    end, lists:seq(1, 10)),

    %% At limit (should still succeed)
    ok = erlmcp_cli_secrets:check_rate_limit(ClientId, 10, 60).

test_rate_limiting_over_limit() ->
    %% Reset rate limiter
    ClientId = <<"test_client_over">>,
    ok = erlmcp_cli_secrets:reset_rate_limit(ClientId),

    %% Exceed limit
    lists:foreach(fun(_) ->
        ok = erlmcp_cli_secrets:check_rate_limit(ClientId, 5, 60)
    end, lists:seq(1, 5)),

    %% Over limit (should fail)
    {error, {rate_limit_exceeded, _}} = erlmcp_cli_secrets:check_rate_limit(ClientId, 5, 60).

test_rate_limiting_window_reset() ->
    %% Reset rate limiter
    ClientId = <<"test_client_reset">>,
    ok = erlmcp_cli_secrets:reset_rate_limit(ClientId),

    %% Exceed limit
    lists:foreach(fun(_) ->
        ok = erlmcp_cli_secrets:check_rate_limit(ClientId, 5, 1)
    end, lists:seq(1, 5)),

    %% Over limit
    {error, {rate_limit_exceeded, _}} = erlmcp_cli_secrets:check_rate_limit(ClientId, 5, 1),

    %% Wait for window reset
    timer:sleep(1100),

    %% Window reset (should succeed)
    ok = erlmcp_cli_secrets:check_rate_limit(ClientId, 5, 1).

%%%====================================================================
%%% Session Authentication Tests
%%%====================================================================

test_session_valid() ->
    %% Create session
    SessionId = <<"auth_test_session">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId,
                                                     #{user => <<"test_user">>}),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Authenticate session
    {ok, State} = erlmcp_cli_session:get_state(SessionId),

    %% Verify session authenticated
    ?assertEqual(<<"test_user">>, maps:get(user, State)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_session_invalid() ->
    %% Invalid session ID
    InvalidSessionId = <<"invalid_session_id">>,

    %% Authenticate (should fail)
    {error, {session_not_found, _}} = erlmcp_cli_session:get_state(InvalidSessionId).

test_session_expired() ->
    %% Create session with short TTL
    SessionId = <<"expired_test_session">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId,
                                                     #{user => <<"test_user">>,
                                                       ttl => 1000}),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Wait for expiration
    timer:sleep(1100),

    %% Verify session expired
    {ok, State} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(expired, maps:get(status, State)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Authorization Tests
%%%====================================================================

test_authorization_permission_check() ->
    %% Check permission
    User = #{id => <<"user1">>, permissions => [read, write]},

    %% Has permission
    ?assertEqual(true, erlmcp_cli_secrets:check_permission(User, read)),

    %% Doesn't have permission
    ?assertEqual(false, erlmcp_cli_secrets:check_permission(User, admin)).

test_authorization_role_based() ->
    %% Role-based authorization
    AdminUser = #{id => <<"admin">>, roles => [admin]},
    RegularUser = #{id => <<"user">>, roles => [user]},

    %% Admin has all permissions
    ?assertEqual(true, erlmcp_cli_secrets:check_role_permission(AdminUser, any)),

    %% Regular user has limited permissions
    ?assertEqual(false, erlmcp_cli_secrets:check_role_permission(RegularUser, admin)).

test_authorization_resource_based() ->
    %% Resource-based authorization
    User = #{id => <<"user1">>,
             permissions => [{resource, <<"res1">>, read}]},

    %% Can read own resource
    ?assertEqual(true,
                 erlmcp_cli_secrets:check_resource_permission(User, <<"res1">>, read)),

    %% Cannot write to resource
    ?assertEqual(false,
                 erlmcp_cli_secrets:check_resource_permission(User, <<"res1">>, write)).

%%%====================================================================
%%% Authentication Failure Tests
%%%====================================================================

test_auth_failure_lockout() ->
    %% Multiple failed authentication attempts
    UserId = <<"lockout_user">>,

    %% Configure lockout (3 attempts, 60 second window)
    ok = erlmcp_cli_secrets:configure_lockout(UserId, 3, 60),

    %% Failed attempts
    lists:foreach(fun(_) ->
        {error, _} = erlmcp_cli_secrets:validate_token(<<"invalid">>)
    end, lists:seq(1, 3)),

    %% Check lockout status
    {locked, _RemainingTime} = erlmcp_cli_secrets:check_lockout_status(UserId).

test_auth_failure_recovery() ->
    %% Lockout recovery
    UserId = <<"recovery_user">>,

    %% Configure lockout (2 attempts, 1 second window)
    ok = erlmcp_cli_secrets:configure_lockout(UserId, 2, 1),

    %% Failed attempts
    lists:foreach(fun(_) ->
        {error, _} = erlmcp_cli_secrets:validate_token(<<"invalid">>)
    end, lists:seq(1, 2)),

    %% Verify locked
    {locked, _} = erlmcp_cli_secrets:check_lockout_status(UserId),

    %% Wait for lockout expiration
    timer:sleep(1100),

    %% Verify unlocked
    {not_locked, _} = erlmcp_cli_secrets:check_lockout_status(UserId).
