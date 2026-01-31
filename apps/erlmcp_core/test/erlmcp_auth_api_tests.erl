%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth_api_tests - API Boundary Tests for Authentication
%%%
%%% Tests authentication API boundaries following Chicago School TDD:
%%%   - Test observable behavior through API calls only
%%%   - Use REAL erlmcp_auth processes (NO mocks)
%%%   - NO internal state inspection
%%%   - NO record duplication
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_api_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

auth_api_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"API Key Authentication", fun test_api_key_auth/0},
        {"Invalid API Key", fun test_invalid_api_key/0},
        {"JWT Validation with Key ID", fun test_jwt_with_key_id/0},
        {"JWT Missing Key ID", fun test_jwt_missing_key_id/0},
        {"JWT Unknown Key ID", fun test_jwt_unknown_key_id/0},
        {"OAuth2 Token Validation", fun test_oauth2_validation/0},
        {"mTLS Validation", fun test_mtls_validation/0},
        {"Session Creation", fun test_session_creation/0},
        {"Session with Metadata", fun test_session_with_metadata/0},
        {"Session Destruction", fun test_session_destruction/0},
        {"Permission Check", fun test_permission_check/0},
        {"Permission Check Invalid Session", fun test_permission_invalid_session/0},
        {"RBAC Role Management", fun test_rbac_roles/0},
        {"Token Rotation", fun test_token_rotation/0},
        {"Token Revocation", fun test_token_revocation/0}
     ]}.

setup() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),

    % Start rate limiter first (dependency)
    {ok, _RateLimiterPid} = erlmcp_auth_rate_limiter:start_link(#{}),

    % Start auth server with test config
    Config = #{
        api_keys => #{
            <<"test_key_123">> => <<"user_alice">>,
            <<"test_key_456">> => <<"user_bob">>
        },
        jwt_keys => #{
            <<"test_kid">> => <<"test_public_key">>
        },
        oauth2 => #{
            enabled => true,
            introspection_url => <<"https://auth.example.com/introspect">>
        },
        mtls => #{
            enabled => true,
            ca_cert => <<"/path/to/ca.pem">>
        },
        rate_limiter_enabled => true
    },

    {ok, Pid} = erlmcp_auth:start_link(Config),

    % Set up test roles
    ok = erlmcp_auth:add_role(<<"user_alice">>, <<"admin">>),
    ok = erlmcp_auth:add_role(<<"user_bob">>, <<"user">>),

    % Set up test permissions
    ok = erlmcp_auth:add_permission(<<"/api/tools">>, <<"execute">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"/api/admin">>, <<"write">>, [<<"admin">>]),

    Pid.

cleanup(_Pid) ->
    erlmcp_auth:stop(),
    erlmcp_auth_rate_limiter:stop(),
    ok.

%%====================================================================
%% Authentication Method Tests
%%====================================================================

test_api_key_auth() ->
    % Valid API key returns session ID
    {ok, SessionId} = erlmcp_auth:authenticate(api_key, #{api_key => <<"test_key_123">>}),
    ?assert(is_binary(SessionId)),
    ?assert(byte_size(SessionId) > 0),

    % Verify session is valid through API call
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/tools">>, <<"execute">>),

    ok.

test_invalid_api_key() ->
    % Invalid API key returns error
    {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => <<"invalid_key">>}),

    % Empty API key returns error
    {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => <<>>}),

    ok.

test_jwt_with_key_id() ->
    % Create JWT with key ID
    Header = base64:encode(jsx:encode(#{
        <<"alg">> => <<"HS256">>,
        <<"typ">> => <<"JWT">>,
        <<"kid">> => <<"test_kid">>
    })),

    FutureExp = erlang:system_time(second) + 3600,
    Payload = base64:encode(jsx:encode(#{
        <<"sub">> => <<"user_test">>,
        <<"exp">> => FutureExp,
        <<"iat">> => erlang:system_time(second)
    })),

    Signature = base64:encode(<<"signature">>),
    Token = <<Header/binary, ".", Payload/binary, ".", Signature/binary>>,

    % Validation result (error through API)
    Result = erlmcp_auth:validate_jwt(Token),
    ?assertMatch({error, _}, Result),

    ok.

test_jwt_missing_key_id() ->
    % JWT without kid in header
    Header = base64:encode(jsx:encode(#{
        <<"alg">> => <<"HS256">>,
        <<"typ">> => <<"JWT">>
    })),

    Payload = base64:encode(jsx:encode(#{
        <<"sub">> => <<"user_test">>,
        <<"exp">> => erlang:system_time(second) + 3600
    })),

    Signature = base64:encode(<<"sig">>),
    Token = <<Header/binary, ".", Payload/binary, ".", Signature/binary>>,

    % Should return error (observable through API)
    {error, _} = erlmcp_auth:validate_jwt(Token),

    ok.

test_jwt_unknown_key_id() ->
    % JWT with unknown key ID
    Header = base64:encode(jsx:encode(#{
        <<"alg">> => <<"HS256">>,
        <<"typ">> => <<"JWT">>,
        <<"kid">> => <<"unknown_kid">>
    })),

    Payload = base64:encode(jsx:encode(#{
        <<"sub">> => <<"user_test">>,
        <<"exp">> => erlang:system_time(second) + 3600
    })),

    Signature = base64:encode(<<"sig">>),
    Token = <<Header/binary, ".", Payload/binary, ".", Signature/binary>>,

    % Should return unknown_key_id error
    {error, unknown_key_id} = erlmcp_auth:validate_jwt(Token),

    ok.

test_oauth2_validation() ->
    % OAuth2 token validation
    {ok, TokenInfo} = erlmcp_auth:validate_oauth2_token(<<"test_oauth2_token">>),
    ?assertEqual(<<"oauth2_user">>, maps:get(<<"user_id">>, TokenInfo)),
    ?assertEqual(<<"read write">>, maps:get(<<"scope">>, TokenInfo)),

    ok.

test_mtls_validation() ->
    % mTLS certificate validation
    CertInfo = #{
        subject => #{
            cn => <<"client.example.com">>,
            o => <<"Test Org">>
        },
        issuer => <<"Test CA">>
    },

    {ok, UserId} = erlmcp_auth:validate_mtls(CertInfo),
    ?assertEqual(<<"client.example.com">>, UserId),

    ok.

%%====================================================================
%% Session Management Tests
%%====================================================================

test_session_creation() ->
    % Create session
    {ok, SessionId} = erlmcp_auth:create_session(<<"user_alice">>, #{}),
    ?assert(is_binary(SessionId)),
    ?assert(byte_size(SessionId) > 0),

    % Verify session is valid through API
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/admin">>, <<"write">>),

    ok.

test_session_with_metadata() ->
    % Create session with metadata
    Metadata = #{
        client_ip => <<"192.168.1.100">>,
        user_agent => <<"test_client/1.0">>
    },

    {ok, SessionId} = erlmcp_auth:create_session(<<"user_bob">>, Metadata),
    ?assert(is_binary(SessionId)),

    % Verify session works
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/tools">>, <<"execute">>),

    ok.

test_session_destruction() ->
    % Create session
    {ok, SessionId} = erlmcp_auth:create_session(<<"user_alice">>, #{}),

    % Verify it's valid
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/admin">>, <<"write">>),

    % Destroy session
    ok = erlmcp_auth:destroy_session(SessionId),

    % Session should be invalid (observable through API)
    {error, invalid_session} = erlmcp_auth:check_permission(SessionId, <<"/api/admin">>, <<"write">>),

    ok.

%%====================================================================
%% Permission Checking Tests
%%====================================================================

test_permission_check() ->
    % Create sessions for alice (admin) and bob (user)
    {ok, AliceSession} = erlmcp_auth:create_session(<<"user_alice">>, #{}),
    {ok, BobSession} = erlmcp_auth:create_session(<<"user_bob">>, #{}),

    % Admin can access /api/admin write
    ok = erlmcp_auth:check_permission(AliceSession, <<"/api/admin">>, <<"write">>),

    % User cannot access /api/admin write
    {error, forbidden} = erlmcp_auth:check_permission(BobSession, <<"/api/admin">>, <<"write">>),

    % Both can execute tools
    ok = erlmcp_auth:check_permission(AliceSession, <<"/api/tools">>, <<"execute">>),
    ok = erlmcp_auth:check_permission(BobSession, <<"/api/tools">>, <<"execute">>),

    ok.

test_permission_invalid_session() ->
    % Check permission with invalid session
    {error, invalid_session} = erlmcp_auth:check_permission(
        <<"invalid_session_id">>, <<"/api/tools">>, <<"execute">>
    ),

    ok.

%%====================================================================
%% RBAC Tests
%%====================================================================

test_rbac_roles() ->
    % Get user roles
    {ok, AliceRoles} = erlmcp_auth:get_user_roles(<<"user_alice">>),
    ?assert(lists:member(<<"admin">>, AliceRoles)),

    {ok, BobRoles} = erlmcp_auth:get_user_roles(<<"user_bob">>),
    ?assert(lists:member(<<"user">>, BobRoles)),

    % Get role permissions
    {ok, AdminPerms} = erlmcp_auth:get_role_permissions(<<"admin">>),
    ?assert(lists:member(<<"write">>, AdminPerms)),
    ?assert(lists:member(<<"execute">>, AdminPerms)),

    ok.

%%====================================================================
%% Token Management Tests
%%====================================================================

test_token_rotation() ->
    % Create session
    {ok, OldSessionId} = erlmcp_auth:create_session(<<"user_alice">>, #{}),

    % Rotate token
    {ok, NewSessionId} = erlmcp_auth:rotate_token(OldSessionId),
    ?assertNotEqual(OldSessionId, NewSessionId),

    % Old session should be invalid (observable through API)
    {error, invalid_session} = erlmcp_auth:check_permission(OldSessionId, <<"/api/tools">>, <<"execute">>),

    % New session should work
    ok = erlmcp_auth:check_permission(NewSessionId, <<"/api/tools">>, <<"execute">>),

    ok.

test_token_revocation() ->
    % Revoke token
    Token = <<"test_token_to_revoke">>,
    ok = erlmcp_auth:revoke_token(Token),

    % Verify token is revoked (observable through API)
    {error, token_revoked} = erlmcp_auth:validate_jwt(Token),

    ok.

%%====================================================================
%% Error Handling Tests
%%====================================================================

unsupported_auth_method_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        fun() ->
            % Try unsupported authentication method
            {error, unsupported_auth_method} = erlmcp_auth:authenticate(
                unknown_method, #{credentials => <<"test">>}
            )
        end
     ]}.
