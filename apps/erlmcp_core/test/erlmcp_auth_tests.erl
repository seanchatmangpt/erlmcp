%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth_tests - Tests for Authentication Module
%%% Tests JWT validation, API keys, RBAC, and OAuth2.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

auth_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"API Key Authentication", fun test_api_key_auth/0},
         {"JWT Validation", fun test_jwt_validation/0},
         {"Session Management", fun test_session_management/0},
         {"RBAC Role Assignment", fun test_rbac_roles/0},
         {"Permission Checking", fun test_permission_checking/0},
         {"Token Rotation", fun test_token_rotation/0},
         {"Token Revocation", fun test_token_revocation/0},
         {"Expired Session Cleanup", fun test_session_cleanup/0}
     ]}.

setup() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),

    % Start auth server with test config
    Config = #{
        api_keys => #{
            <<"test_key_123">> => <<"user_alice">>,
            <<"test_key_456">> => <<"user_bob">>
        },
        jwt_keys => #{
            <<"test_kid">> => <<"test_public_key">>
        }
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
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_api_key_auth() ->
    % Valid API key
    {ok, SessionId1} = erlmcp_auth:authenticate(api_key, #{api_key => <<"test_key_123">>}),
    ?assert(is_binary(SessionId1)),

    % Invalid API key
    {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => <<"invalid_key">>}),

    ok.

test_jwt_validation() ->
    % Create test JWT (header.payload.signature)
    Header = base64:encode(jsx:encode(#{<<"alg">> => <<"HS256">>, <<"typ">> => <<"JWT">>})),

    % Payload with future expiration
    FutureExp = erlang:system_time(second) + 3600,
    Payload = base64:encode(jsx:encode(#{
        <<"sub">> => <<"user_charlie">>,
        <<"exp">> => FutureExp,
        <<"iat">> => erlang:system_time(second)
    })),

    Signature = base64:encode(<<"fake_signature">>),
    ValidToken = <<Header/binary, ".", Payload/binary, ".", Signature/binary>>,

    % Valid JWT
    {ok, Claims} = erlmcp_auth:validate_jwt(ValidToken),
    ?assertEqual(<<"user_charlie">>, maps:get(<<"sub">>, Claims)),

    % Expired JWT
    PastExp = erlang:system_time(second) - 3600,
    ExpiredPayload = base64:encode(jsx:encode(#{
        <<"sub">> => <<"user_charlie">>,
        <<"exp">> => PastExp
    })),
    ExpiredToken = <<Header/binary, ".", ExpiredPayload/binary, ".", Signature/binary>>,
    {error, token_expired} = erlmcp_auth:validate_jwt(ExpiredToken),

    % Invalid format
    {error, invalid_jwt_format} = erlmcp_auth:validate_jwt(<<"invalid.token">>),

    ok.

test_session_management() ->
    % Create session
    {ok, SessionId} = erlmcp_auth:create_session(<<"user_alice">>, #{client_ip => <<"127.0.0.1">>}),
    ?assert(is_binary(SessionId)),

    % Destroy session
    ok = erlmcp_auth:destroy_session(SessionId),

    % Session should be invalid now
    {error, invalid_session} = erlmcp_auth:check_permission(SessionId, <<"/api/tools">>, <<"execute">>),

    ok.

test_rbac_roles() ->
    % Get user roles
    {ok, AliceRoles} = erlmcp_auth:get_user_roles(<<"user_alice">>),
    ?assert(lists:member(<<"admin">>, AliceRoles)),

    {ok, BobRoles} = erlmcp_auth:get_user_roles(<<"user_bob">>),
    ?assert(lists:member(<<"user">>, BobRoles)),

    % Get role permissions
    {ok, AdminPerms} = erlmcp_auth:get_role_permissions(<<"admin">>),
    ?assert(lists:member(<<"execute">>, AdminPerms)),
    ?assert(lists:member(<<"write">>, AdminPerms)),

    {ok, UserPerms} = erlmcp_auth:get_role_permissions(<<"user">>),
    ?assert(lists:member(<<"read">>, UserPerms)),
    ?assertNot(lists:member(<<"delete">>, UserPerms)),

    % Non-existent user
    {error, not_found} = erlmcp_auth:get_user_roles(<<"user_unknown">>),

    ok.

test_permission_checking() ->
    % Create sessions
    {ok, AliceSession} = erlmcp_auth:create_session(<<"user_alice">>, #{}),
    {ok, BobSession} = erlmcp_auth:create_session(<<"user_bob">>, #{}),

    % Admin can access /api/admin
    ok = erlmcp_auth:check_permission(AliceSession, <<"/api/admin">>, <<"write">>),

    % User cannot access /api/admin
    {error, forbidden} = erlmcp_auth:check_permission(BobSession, <<"/api/admin">>, <<"write">>),

    % Both can execute tools
    ok = erlmcp_auth:check_permission(AliceSession, <<"/api/tools">>, <<"execute">>),
    ok = erlmcp_auth:check_permission(BobSession, <<"/api/tools">>, <<"execute">>),

    ok.

test_token_rotation() ->
    % Create session
    {ok, OldSessionId} = erlmcp_auth:create_session(<<"user_alice">>, #{}),

    % Rotate token
    {ok, NewSessionId} = erlmcp_auth:rotate_token(OldSessionId),
    ?assertNotEqual(OldSessionId, NewSessionId),

    % Old session should be invalid
    {error, invalid_session} = erlmcp_auth:check_permission(OldSessionId, <<"/api/tools">>, <<"execute">>),

    % New session should work
    ok = erlmcp_auth:check_permission(NewSessionId, <<"/api/tools">>, <<"execute">>),

    ok.

test_token_revocation() ->
    Token = <<"test_token_to_revoke">>,

    % Revoke token
    ok = erlmcp_auth:revoke_token(Token),

    % Validate token should fail
    {error, token_revoked} = erlmcp_auth:validate_jwt(Token),

    ok.

test_session_cleanup() ->
    % Create session (expires in 1 hour by default)
    {ok, SessionId} = erlmcp_auth:create_session(<<"user_alice">>, #{}),

    % Session should be valid
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/tools">>, <<"execute">>),

    % TODO: Test expiration - would need to mock time or wait
    % For now, just verify session exists
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

% Add helper functions as needed
