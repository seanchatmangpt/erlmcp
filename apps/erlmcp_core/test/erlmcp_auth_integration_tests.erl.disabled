%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth_integration_tests - Integration Tests for Authentication
%%%
%%% Tests authentication integration scenarios following Chicago School TDD:
%%%   - Test observable behavior through API calls only
%%%   - Use REAL erlmcp_auth processes (NO mocks)
%%%   - NO internal state inspection
%%%   - NO record duplication
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

auth_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"Full Authentication Flow", fun test_full_auth_flow/0},
        {"Multi-Role Permission Resolution", fun test_multi_role_resolution/0},
        {"Session Isolation", fun test_session_isolation/0},
        {"Rate Limiting Integration", fun test_rate_limiting_integration/0},
        {"Permission Management", fun test_permission_management/0},
        {"Cleanup Operations", fun test_cleanup_operations/0}
     ]}.

setup() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),

    % Start rate limiter first
    {ok, RateLimiterPid} = erlmcp_auth_rate_limiter:start_link(#{
        max_attempts_per_second => 10,
        window_ms => 1000,
        max_failures => 5,
        block_duration_ms => 5000
    }),

    % Start auth server with comprehensive config
    Config = #{
        api_keys => #{
            <<"test_key_123">> => <<"user_alice">>,
            <<"test_key_456">> => <<"user_bob">>,
            <<"test_key_789">> => <<"user_charlie">>
        },
        jwt_keys => #{
            <<"test_kid">> => <<"test_public_key">>
        },
        rate_limiter_enabled => true
    },

    {ok, AuthPid} = erlmcp_auth:start_link(Config),

    % Set up test roles
    ok = erlmcp_auth:add_role(<<"user_alice">>, <<"admin">>),
    ok = erlmcp_auth:add_role(<<"user_bob">>, <<"user">>),
    ok = erlmcp_auth:add_role(<<"user_charlie">>, <<"guest">>),
    ok = erlmcp_auth:add_role(<<"user_david">>, <<"admin">>),
    ok = erlmcp_auth:add_role(<<"user_david">>, <<"user">>),  % Multiple roles

    % Set up test permissions
    ok = erlmcp_auth:add_permission(<<"/api/tools">>, <<"execute">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"/api/admin">>, <<"write">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"/api/admin">>, <<"delete">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"/api/data">>, <<"read">>, [<<"admin">>, <<"user">>, <<"guest">>]),

    {AuthPid, RateLimiterPid}.

cleanup({_AuthPid, RateLimiterPid}) ->
    erlmcp_auth:stop(),
    erlmcp_auth_rate_limiter:stop(),
    ok.

%%====================================================================
%% Integration Test Scenarios
%%====================================================================

test_full_auth_flow() ->
    % Authenticate
    {ok, SessionId} = erlmcp_auth:authenticate(api_key, #{
        api_key => <<"test_key_123">>,
        ip_address => {192, 168, 1, 1}
    }),

    % Check permissions
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/admin">>, <<"write">>),

    % Rotate token
    {ok, NewSessionId} = erlmcp_auth:rotate_token(SessionId),

    % Verify new session works
    ok = erlmcp_auth:check_permission(NewSessionId, <<"/api/admin">>, <<"delete">>),

    % Cleanup
    ok = erlmcp_auth:destroy_session(NewSessionId),

    ok.

test_multi_role_resolution() ->
    % User with multiple roles (david has admin + user)
    {ok, SessionId} = erlmcp_auth:create_session(<<"user_david">>, #{}),

    % Should have admin permissions
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/admin">>, <<"delete">>),

    % Should have user permissions
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/tools">>, <<"execute">>),

    ok.

test_session_isolation() ->
    % Create two sessions for same user
    {ok, Session1} = erlmcp_auth:create_session(<<"user_alice">>, #{}),
    {ok, Session2} = erlmcp_auth:create_session(<<"user_alice">>, #{}),

    % Both should work
    ok = erlmcp_auth:check_permission(Session1, <<"/api/admin">>, <<"write">>),
    ok = erlmcp_auth:check_permission(Session2, <<"/api/admin">>, <<"write">>),

    % Destroy first session
    ok = erlmcp_auth:destroy_session(Session1),

    % First should fail, second should still work (observable behavior)
    {error, invalid_session} = erlmcp_auth:check_permission(Session1, <<"/api/admin">>, <<"write">>),
    ok = erlmcp_auth:check_permission(Session2, <<"/api/admin">>, <<"write">>),

    ok.

test_rate_limiting_integration() ->
    % Authenticate successfully
    {ok, _SessionId} = erlmcp_auth:authenticate(api_key, #{
        api_key => <<"test_key_456">>,
        ip_address => {127, 0, 0, 1}
    }),

    % Verify rate limiter is enabled through API behavior
    Enabled = erlmcp_auth:is_rate_limiter_enabled(),
    ?assertEqual(true, Enabled),

    ok.

test_permission_management() ->
    % Add permission for new resource
    ok = erlmcp_auth:add_permission(<<"/api/new_resource">>, <<"custom_action">>, [<<"admin">>]),

    % Create admin session and verify permission
    {ok, SessionId} = erlmcp_auth:create_session(<<"user_alice">>, #{}),
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/new_resource">>, <<"custom_action">>),

    % Remove permission
    ok = erlmcp_auth:remove_permission(<<"/api/admin">>, <<"write">>, [<<"user">>]),

    ok.

test_cleanup_operations() ->
    % Create session
    {ok, SessionId} = erlmcp_auth:create_session(<<"user_alice">>, #{}),

    % Session should be valid
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/tools">>, <<"execute">>),

    % Revoke token
    Token = <<"cleanup_test_token">>,
    ok = erlmcp_auth:revoke_token(Token),

    % Verify it's revoked
    {error, token_revoked} = erlmcp_auth:validate_jwt(Token),

    ok.

%%====================================================================
%% Error Scenarios Integration
%%====================================================================

error_scenarios_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"Rate Limiting on Failed Auth", fun test_failed_auth_rate_limiting/0},
        {"Multiple Failed Auth Attempts", fun test_multiple_failed_attempts/0},
        {"Token Rotation Invalid Session", fun test_token_rotation_invalid/0},
        {"Non-existent User Roles", fun test_nonexistent_user_roles/0},
        {"Non-existent Role Permissions", fun test_nonexistent_role_permissions/0}
     ]}.

test_failed_auth_rate_limiting() ->
    % Authenticate with invalid API key multiple times
    InvalidKey = <<"wrong_key">>,

    {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => InvalidKey}),
    {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => InvalidKey}),
    {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => InvalidKey}),

    % Verify behavior through API (may be rate limited)
    Result = erlmcp_auth:authenticate(api_key, #{api_key => InvalidKey}),
    ?assertMatch({error, _}, Result),

    ok.

test_multiple_failed_attempts() ->
    % Multiple failed auth attempts
    lists:foreach(fun(_) ->
        {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => <<"bad_key">>})
    end, lists:seq(1, 3)),

    ok.

test_token_rotation_invalid() ->
    % Rotate non-existent session
    {error, invalid_session} = erlmcp_auth:rotate_token(<<"invalid_session">>),

    ok.

test_nonexistent_user_roles() ->
    % Non-existent user
    {error, not_found} = erlmcp_auth:get_user_roles(<<"user_nonexistent">>),

    ok.

test_nonexistent_role_permissions() ->
    % Non-existent role
    {error, not_found} = erlmcp_auth:get_role_permissions(<<"nonexistent_role">>),

    ok.

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

gen_server_callbacks_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"Unknown Request", fun test_unknown_request/0},
        {"Handle Cast", fun test_handle_cast/0},
        {"Handle Info Unknown", fun test_handle_info_unknown/0}
     ]}.

test_unknown_request() ->
    % Test unknown gen_server call
    {error, unknown_request} = gen_server:call(erlmcp_auth, unknown_message),

    ok.

test_handle_cast() ->
    % Test handle_cast
    ok = gen_server:cast(erlmcp_auth, test_cast_message),

    ok.

test_handle_info_unknown() ->
    % Test handle_info with unknown message
    erlmcp_auth ! unknown_info_message,
    timer:sleep(100),

    ok.
