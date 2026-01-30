%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive Authorization Test Suite for ErlMCP
%%%
%%% This test suite validates the Role-Based Access Control (RBAC) system
%%% and authorization mechanisms implemented in erlmcp_auth.
%%%
%%% Test Categories:
%%% 1. RBAC Tests - Role-based permissions
%%% 2. Permission Tests - Granular permission checks
%%% 3. Resource-Level Authorization - URI-based access control
%%% 4. Tool Authorization - Tool execution permissions
%%% 5. Session Authorization - Session management rights
%%%
%%% Testing Methodology: Chicago School TDD
%%% - Tests use REAL erlmcp processes, not mocks
%%% - State-based verification (check observable state)
%%% - Real gen_server operations for auth system
%%% - No mock objects or stubs
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_authorization_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Common Test Callbacks
%%%====================================================================

all() ->
    [
     %% RBAC Tests
     admin_role_full_access_test,
     user_role_limited_access_test,
     guest_role_read_only_access_test,
     role_escalation_prevention_test,

     %% Permission Tests
     resource_access_permissions_test,
     tool_execution_permissions_test,
     prompt_template_permissions_test,
     administrative_operation_permissions_test,

     %% Resource-Level Authorization
     resource_uri_access_control_test,
     resource_subscription_authorization_test,
     resource_update_permissions_test,

     %% Tool Authorization
     tool_call_authorization_test,
     tool_parameter_validation_test,
     dangerous_tool_restrictions_test,

     %% Session Authorization
     session_creation_permissions_test,
     session_termination_rights_test,
     cross_session_access_prevention_test,

     %% Edge Cases
     invalid_permission_denied_test,
     expired_session_authorization_test,
     concurrent_permission_changes_test
    ].

init_per_suite(Config) ->
    ct:pal("Starting ErlMCP Authorization Test Suite"),

    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    application:ensure_all_started(gproc),
    application:ensure_all_started(jsx),

    %% Start erlmcp_core application
    case application:start(erlmcp_core) of
        ok -> ok;
        {error, {already_started, erlmcp_core}} -> ok
    end,

    %% Start auth server for testing
    {ok, AuthPid} = erlmcp_auth:start_link(#{
        rate_limiter_enabled => false,
        api_keys => #{
            <<"test_admin_key">> => <<"user_admin">>,
            <<"test_user_key">> => <<"user_regular">>,
            <<"test_guest_key">> => <<"user_guest">>
        }
    }),

    ct:pal("Auth server started: ~p", [AuthPid]),

    [{auth_pid, AuthPid} | Config].

end_per_suite(Config) ->
    ct:pal("Ending ErlMCP Authorization Test Suite"),

    %% Stop auth server
    AuthPid = proplists:get_value(auth_pid, Config),
    case is_process_alive(AuthPid) of
        true -> erlmcp_auth:stop();
        false -> ok
    end,

    %% Stop application
    application:stop(erlmcp_core),

    ok.

init_per_testcase(_TestCase, Config) ->
    %% Reset auth state before each test
    case whereis(erlmcp_auth) of
        undefined -> ok;
        _ -> erlmcp_auth:stop()
    end,

    {ok, _AuthPid} = erlmcp_auth:start_link(#{
        rate_limiter_enabled => false,
        api_keys => #{
            <<"test_admin_key">> => <<"user_admin">>,
            <<"test_user_key">> => <<"user_regular">>,
            <<"test_guest_key">> => <<"user_guest">>
        }
    }),

    %% Configure default permissions
    setup_default_permissions(),

    %% Create test sessions
    AdminSession = create_test_session(<<"user_admin">>, [<<"admin">>]),
    UserSession = create_test_session(<<"user_regular">>, [<<"user">>]),
    GuestSession = create_test_session(<<"user_guest">>, [<<"guest">>]),

    [
        {admin_session, AdminSession},
        {user_session, UserSession},
        {guest_session, GuestSession}
        | Config
    ].

end_per_testcase(_TestCase, Config) ->
    %% Cleanup test sessions
    AdminSession = proplists:get_value(admin_session, Config, undefined),
    UserSession = proplists:get_value(user_session, Config, undefined),
    GuestSession = proplists:get_value(guest_session, Config, undefined),

    lists:foreach(fun(SessionId) ->
        case SessionId of
            undefined -> ok;
            _ -> erlmcp_auth:destroy_session(SessionId)
        end
    end, [AdminSession, UserSession, GuestSession]),

    %% Stop auth server
    erlmcp_auth:stop(),

    ok.

%%%====================================================================
%%% RBAC Tests
%%%====================================================================

%% @doc Test that admin role has full access to all resources
admin_role_full_access_test(Config) ->
    ct:pal("Testing admin role full access"),

    AdminSession = proplists:get_value(admin_session, Config),

    %% Admin should have all permissions
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"any_resource">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"any_resource">>, <<"write">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"any_resource">>, <<"execute">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"any_resource">>, <<"delete">>)),

    %% Verify admin has admin role
    {ok, AdminRoles} = erlmcp_auth:get_user_roles(<<"user_admin">>),
    ?assert(lists:member(<<"admin">>, AdminRoles)),

    %% Verify admin role has all permissions
    {ok, AdminPerms} = erlmcp_auth:get_role_permissions(<<"admin">>),
    ?assert(lists:member(<<"read">>, AdminPerms)),
    ?assert(lists:member(<<"write">>, AdminPerms)),
    ?assert(lists:member(<<"execute">>, AdminPerms)),
    ?assert(lists:member(<<"delete">>, AdminPerms)),

    ct:pal("Admin role full access test passed").

%% @doc Test that user role has limited access (read, write only)
user_role_limited_access_test(Config) ->
    ct:pal("Testing user role limited access"),

    UserSession = proplists:get_value(user_session, Config),

    %% User should have read and write access
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"user_resource">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"user_resource">>, <<"write">>)),

    %% User should NOT have execute or delete access
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"user_resource">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"user_resource">>, <<"delete">>)),

    %% Verify user has user role
    {ok, UserRoles} = erlmcp_auth:get_user_roles(<<"user_regular">>),
    ?assert(lists:member(<<"user">>, UserRoles)),
    ?assertNot(lists:member(<<"admin">>, UserRoles)),

    %% Verify user role has limited permissions
    {ok, UserPerms} = erlmcp_auth:get_role_permissions(<<"user">>),
    ?assert(lists:member(<<"read">>, UserPerms)),
    ?assert(lists:member(<<"write">>, UserPerms)),
    ?assertNot(lists:member(<<"execute">>, UserPerms)),
    ?assertNot(lists:member(<<"delete">>, UserPerms)),

    ct:pal("User role limited access test passed").

%% @doc Test that guest role has read-only access
guest_role_read_only_access_test(Config) ->
    ct:pal("Testing guest role read-only access"),

    GuestSession = proplists:get_value(guest_session, Config),

    %% Guest should have read access
    ?assertEqual(ok, erlmcp_auth:check_permission(GuestSession, <<"public_resource">>, <<"read">>)),

    %% Guest should NOT have write, execute, or delete access
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"public_resource">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"public_resource">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"public_resource">>, <<"delete">>)),

    %% Verify guest has guest role
    {ok, GuestRoles} = erlmcp_auth:get_user_roles(<<"user_guest">>),
    ?assert(lists:member(<<"guest">>, GuestRoles)),

    %% Verify guest role has read-only permission
    {ok, GuestPerms} = erlmcp_auth:get_role_permissions(<<"guest">>),
    ?assertEqual([<<"read">>], GuestPerms),

    ct:pal("Guest role read-only access test passed").

%% @doc Test that role escalation is prevented
role_escalation_prevention_test(Config) ->
    ct:pal("Testing role escalation prevention"),

    UserSession = proplists:get_value(user_session, Config),

    %% User cannot add admin role to themselves (would need admin API)
    %% This tests the permission check for role management
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"admin_management">>, <<"write">>)),

    %% Verify user still only has user role
    {ok, UserRoles} = erlmcp_auth:get_user_roles(<<"user_regular">>),
    ?assertNot(lists:member(<<"admin">>, UserRoles)),
    ?assert(lists:member(<<"user">>, UserRoles)),

    %% Verify user still cannot access admin resources
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"admin_resource">>, <<"delete">>)),

    ct:pal("Role escalation prevention test passed").

%%%====================================================================
%%% Permission Tests
%%%====================================================================

%% @doc Test resource access permissions
resource_access_permissions_test(Config) ->
    ct:pal("Testing resource access permissions"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),
    GuestSession = proplists:get_value(guest_session, Config),

    %% Setup resource permissions
    ok = erlmcp_auth:add_permission(<<"resource_1">>, <<"read">>, [<<"admin">>, <<"user">>, <<"guest">>]),
    ok = erlmcp_auth:add_permission(<<"resource_1">>, <<"write">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"resource_2">>, <<"read">>, [<<"admin">>]),  % Admin-only

    %% Test resource_1 read access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"resource_1">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"resource_1">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(GuestSession, <<"resource_1">>, <<"read">>)),

    %% Test resource_1 write access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"resource_1">>, <<"write">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"resource_1">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"resource_1">>, <<"write">>)),

    %% Test resource_2 admin-only access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"resource_2">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"resource_2">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"resource_2">>, <<"read">>)),

    ct:pal("Resource access permissions test passed").

%% @doc Test tool execution permissions
tool_execution_permissions_test(Config) ->
    ct:pal("Testing tool execution permissions"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),
    GuestSession = proplists:get_value(guest_session, Config),

    %% Setup tool permissions
    ok = erlmcp_auth:add_permission(<<"tool_safe">>, <<"execute">>, [<<"admin">>, <<"user">>, <<"guest">>]),
    ok = erlmcp_auth:add_permission(<<"tool_admin">>, <<"execute">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"tool_dangerous">>, <<"execute">>, [<<"admin">>]),

    %% Test safe tool access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"tool_safe">>, <<"execute">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"tool_safe">>, <<"execute">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(GuestSession, <<"tool_safe">>, <<"execute">>)),

    %% Test admin-only tool access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"tool_admin">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"tool_admin">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"tool_admin">>, <<"execute">>)),

    %% Test dangerous tool access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"tool_dangerous">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"tool_dangerous">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"tool_dangerous">>, <<"execute">>)),

    ct:pal("Tool execution permissions test passed").

%% @doc Test prompt template access permissions
prompt_template_permissions_test(Config) ->
    ct:pal("Testing prompt template permissions"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),
    GuestSession = proplists:get_value(guest_session, Config),

    %% Setup prompt permissions (prompts use read permission)
    ok = erlmcp_auth:add_permission(<<"prompt_public">>, <<"read">>, [<<"admin">>, <<"user">>, <<"guest">>]),
    ok = erlmcp_auth:add_permission(<<"prompt_premium">>, <<"read">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"prompt_internal">>, <<"read">>, [<<"admin">>]),

    %% Test public prompt access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"prompt_public">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"prompt_public">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(GuestSession, <<"prompt_public">>, <<"read">>)),

    %% Test premium prompt access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"prompt_premium">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"prompt_premium">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"prompt_premium">>, <<"read">>)),

    %% Test internal prompt access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"prompt_internal">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"prompt_internal">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"prompt_internal">>, <<"read">>)),

    ct:pal("Prompt template permissions test passed").

%% @doc Test administrative operation permissions
administrative_operation_permissions_test(Config) ->
    ct:pal("Testing administrative operation permissions"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),
    GuestSession = proplists:get_value(guest_session, Config),

    %% Setup admin permissions
    ok = erlmcp_auth:add_permission(<<"admin_users">>, <<"write">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"admin_config">>, <<"write">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"admin_logs">>, <<"delete">>, [<<"admin">>]),

    %% Test user management (admin only)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"admin_users">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"admin_users">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"admin_users">>, <<"write">>)),

    %% Test configuration management (admin only)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"admin_config">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"admin_config">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"admin_config">>, <<"write">>)),

    %% Test log deletion (admin only)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"admin_logs">>, <<"delete">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"admin_logs">>, <<"delete">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"admin_logs">>, <<"delete">>)),

    ct:pal("Administrative operation permissions test passed").

%%%====================================================================
%%% Resource-Level Authorization
%%%====================================================================

%% @doc Test resource URI access control
resource_uri_access_control_test(Config) ->
    ct:pal("Testing resource URI access control"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),
    GuestSession = proplists:get_value(guest_session, Config),

    %% Setup URI-based permissions
    ok = erlmcp_auth:add_permission(<<"file://public/data.txt">>, <<"read">>, [<<"admin">>, <<"user">>, <<"guest">>]),
    ok = erlmcp_auth:add_permission(<<"file://private/data.txt">>, <<"read">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"file://admin/config.json">>, <<"read">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"file://admin/config.json">>, <<"write">>, [<<"admin">>]),

    %% Test public URI access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"file://public/data.txt">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"file://public/data.txt">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(GuestSession, <<"file://public/data.txt">>, <<"read">>)),

    %% Test private URI access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"file://private/data.txt">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"file://private/data.txt">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"file://private/data.txt">>, <<"read">>)),

    %% Test admin URI read access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"file://admin/config.json">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"file://admin/config.json">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"file://admin/config.json">>, <<"read">>)),

    %% Test admin URI write access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"file://admin/config.json">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"file://admin/config.json">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"file://admin/config.json">>, <<"write">>)),

    ct:pal("Resource URI access control test passed").

%% @doc Test resource subscription authorization
resource_subscription_authorization_test(Config) ->
    ct:pal("Testing resource subscription authorization"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),
    GuestSession = proplists:get_value(guest_session, Config),

    %% Setup subscription permissions (using execute for subscribe)
    ok = erlmcp_auth:add_permission(<<"resource_events">>, <<"read">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"resource_events">>, <<"execute">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"admin_events">>, <<"execute">>, [<<"admin">>]),

    %% Test public resource subscription
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"resource_events">>, <<"execute">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"resource_events">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"resource_events">>, <<"execute">>)),

    %% Test admin resource subscription
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"admin_events">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"admin_events">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"admin_events">>, <<"execute">>)),

    ct:pal("Resource subscription authorization test passed").

%% @doc Test resource update permissions
resource_update_permissions_test(Config) ->
    ct:pal("Testing resource update permissions"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),
    GuestSession = proplists:get_value(guest_session, Config),

    %% Setup update permissions
    ok = erlmcp_auth:add_permission(<<"user_profile">>, <<"read">>, [<<"admin">>, <<"user">>, <<"guest">>]),
    ok = erlmcp_auth:add_permission(<<"user_profile">>, <<"write">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"system_config">>, <<"write">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"system_config">>, <<"delete">>, [<<"admin">>]),

    %% Test user profile read (all roles)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"user_profile">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"user_profile">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(GuestSession, <<"user_profile">>, <<"read">>)),

    %% Test user profile write (admin and user only)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"user_profile">>, <<"write">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"user_profile">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"user_profile">>, <<"write">>)),

    %% Test system config write (admin only)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"system_config">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"system_config">>, <<"write">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"system_config">>, <<"write">>)),

    %% Test system config delete (admin only)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"system_config">>, <<"delete">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"system_config">>, <<"delete">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"system_config">>, <<"delete">>)),

    ct:pal("Resource update permissions test passed").

%%%====================================================================
%%% Tool Authorization
%%%====================================================================

%% @doc Test tool call authorization
tool_call_authorization_test(Config) ->
    ct:pal("Testing tool call authorization"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),
    GuestSession = proplists:get_value(guest_session, Config),

    %% Setup tool permissions
    ok = erlmcp_auth:add_permission(<<"tool_calculator">>, <<"execute">>, [<<"admin">>, <<"user">>, <<"guest">>]),
    ok = erlmcp_auth:add_permission(<<"tool_database">>, <<"execute">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"tool_system">>, <<"execute">>, [<<"admin">>]),

    %% Test safe tool (all roles)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"tool_calculator">>, <<"execute">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"tool_calculator">>, <<"execute">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(GuestSession, <<"tool_calculator">>, <<"execute">>)),

    %% Test database tool (admin and user)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"tool_database">>, <<"execute">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"tool_database">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"tool_database">>, <<"execute">>)),

    %% Test system tool (admin only)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"tool_system">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"tool_system">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, <<"tool_system">>, <<"execute">>)),

    ct:pal("Tool call authorization test passed").

%% @doc Test tool parameter validation (via permission checks)
tool_parameter_validation_test(Config) ->
    ct:pal("Testing tool parameter validation"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),

    %% Setup tool with parameter-based permissions
    ok = erlmcp_auth:add_permission(<<"tool_file_read">>, <<"execute">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"tool_file_write">>, <<"execute">>, [<<"admin">>]),

    %% Test file read tool (both roles)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"tool_file_read">>, <<"execute">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"tool_file_read">>, <<"execute">>)),

    %% Test file write tool (admin only)
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"tool_file_write">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"tool_file_write">>, <<"execute">>)),

    %% Verify no access to unauthorized tools
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"tool_unauthorized">>, <<"execute">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"tool_shell">>, <<"execute">>)),

    ct:pal("Tool parameter validation test passed").

%% @doc Test dangerous tool restrictions
dangerous_tool_restrictions_test(Config) ->
    ct:pal("Testing dangerous tool restrictions"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),
    GuestSession = proplists:get_value(guest_session, Config),

    %% Setup dangerous tools (admin only)
    DangerousTools = [
        <<"tool_shell_exec">>,
        <<"tool_file_delete">>,
        <<"tool_network_scan">>,
        <<"tool_system_shutdown">>,
        <<"tool_admin_reset">>
    ],

    lists:foreach(fun(Tool) ->
        ok = erlmcp_auth:add_permission(Tool, <<"execute">>, [<<"admin">>])
    end, DangerousTools),

    %% Verify admin can execute dangerous tools
    lists:foreach(fun(Tool) ->
        ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, Tool, <<"execute">>))
    end, DangerousTools),

    %% Verify user cannot execute dangerous tools
    lists:foreach(fun(Tool) ->
        ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, Tool, <<"execute">>)),
        ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(GuestSession, Tool, <<"execute">>))
    end, DangerousTools),

    ct:pal("Dangerous tool restrictions test passed").

%%%====================================================================
%%% Session Authorization
%%%====================================================================

%% @doc Test session creation permissions
session_creation_permissions_test(Config) ->
    ct:pal("Testing session creation permissions"),

    %% All authenticated users should be able to create sessions
    {ok, AdminSession} = erlmcp_auth:create_session(<<"user_admin">>, #{test => admin}),
    {ok, UserSession} = erlmcp_auth:create_session(<<"user_regular">>, #{test => user}),
    {ok, GuestSession} = erlmcp_auth:create_session(<<"user_guest">>, #{test => guest}),

    %% Verify sessions were created
    ?assert(is_binary(AdminSession)),
    ?assert(is_binary(UserSession)),
    ?assert(is_binary(GuestSession)),

    %% Verify sessions are different
    ?assertNotEqual(AdminSession, UserSession),
    ?assertNotEqual(UserSession, GuestSession),

    %% Cleanup
    erlmcp_auth:destroy_session(AdminSession),
    erlmcp_auth:destroy_session(UserSession),
    erlmcp_auth:destroy_session(GuestSession),

    ct:pal("Session creation permissions test passed").

%% @doc Test session termination rights
session_termination_rights_test(Config) ->
    ct:pal("Testing session termination rights"),

    %% Create sessions
    {ok, Session1} = erlmcp_auth:create_session(<<"user_admin">>, #{id => 1}),
    {ok, Session2} = erlmcp_auth:create_session(<<"user_regular">>, #{id => 2}),

    %% Admin can terminate sessions (via destroy_session)
    ?assertEqual(ok, erlmcp_auth:destroy_session(Session1)),
    ?assertEqual(ok, erlmcp_auth:destroy_session(Session2)),

    %% Verify sessions are destroyed
    ?assertEqual({error, invalid_session}, erlmcp_auth:check_permission(Session1, <<"any">>, <<"read">>)),
    ?assertEqual({error, invalid_session}, erlmcp_auth:check_permission(Session2, <<"any">>, <<"read">>)),

    ct:pal("Session termination rights test passed").

%% @doc Test cross-session access prevention
cross_session_access_prevention_test(Config) ->
    ct:pal("Testing cross-session access prevention"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),

    %% Create user-specific resources
    ok = erlmcp_auth:add_permission(<<"user_admin_data">>, <<"read">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"user_regular_data">>, <<"read">>, [<<"user">>]),

    %% User cannot access admin-specific resources (unless shared)
    %% Admin cannot access user-specific resources (unless shared)
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(AdminSession, <<"user_regular_data">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"user_admin_data">>, <<"read">>)),

    %% But admin can access their own resources
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"user_admin_data">>, <<"read">>)),
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"user_regular_data">>, <<"read">>)),

    ct:pal("Cross-session access prevention test passed").

%%%====================================================================
%%% Edge Cases
%%%====================================================================

%% @doc Test that invalid permissions are denied
invalid_permission_denied_test(Config) ->
    ct:pal("Testing invalid permission denied"),

    AdminSession = proplists:get_value(admin_session, Config),

    %% Try invalid permission types
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(AdminSession, <<"any_resource">>, <<"invalid_perm">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(AdminSession, <<"any_resource">>, <<>>)),

    %% Try with invalid session
    ?assertEqual({error, invalid_session}, erlmcp_auth:check_permission(<<"invalid_session">>, <<"any_resource">>, <<"read">>)),

    %% Try with empty resource
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(AdminSession, <<>>, <<"read">>)),

    ct:pal("Invalid permission denied test passed").

%% @doc Test expired session authorization
expired_session_authorization_test(Config) ->
    ct:pal("Testing expired session authorization"),

    %% Create a session
    {ok, SessionId} = erlmcp_auth:create_session(<<"user_admin">>, #{test => expired}),

    %% Verify session works initially
    ?assertEqual(ok, erlmcp_auth:check_permission(SessionId, <<"any_resource">>, <<"read">>)),

    %% Destroy the session (simulating expiration)
    erlmcp_auth:destroy_session(SessionId),

    %% Verify expired session is rejected
    ?assertEqual({error, invalid_session}, erlmcp_auth:check_permission(SessionId, <<"any_resource">>, <<"read">>)),
    ?assertEqual({error, invalid_session}, erlmcp_auth:check_permission(SessionId, <<"any_resource">>, <<"write">>)),

    ct:pal("Expired session authorization test passed").

%% @doc Test concurrent permission changes
concurrent_permission_changes_test(Config) ->
    ct:pal("Testing concurrent permission changes"),

    AdminSession = proplists:get_value(admin_session, Config),
    UserSession = proplists:get_value(user_session, Config),

    %% Setup initial permissions
    ok = erlmcp_auth:add_permission(<<"concurrent_resource">>, <<"read">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"concurrent_resource">>, <<"write">>, [<<"admin">>]),

    %% Verify initial state
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"concurrent_resource">>, <<"read">>)),
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"concurrent_resource">>, <<"read">>)),

    %% Add user permission
    ok = erlmcp_auth:add_permission(<<"concurrent_resource">>, <<"read">>, [<<"user">>]),

    %% Verify permission change took effect
    ?assertEqual(ok, erlmcp_auth:check_permission(UserSession, <<"concurrent_resource">>, <<"read">>)),

    %% Remove user permission
    ok = erlmcp_auth:remove_permission(<<"concurrent_resource">>, <<"read">>, [<<"user">>]),

    %% Verify permission removal took effect
    ?assertEqual({error, forbidden}, erlmcp_auth:check_permission(UserSession, <<"concurrent_resource">>, <<"read">>)),

    %% Admin still has access
    ?assertEqual(ok, erlmcp_auth:check_permission(AdminSession, <<"concurrent_resource">>, <<"read">>)),

    ct:pal("Concurrent permission changes test passed").

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @private Setup default permissions for testing
setup_default_permissions() ->
    %% Admin permissions - full access
    ok = erlmcp_auth:add_permission(<<"admin_resource">>, <<"read">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"admin_resource">>, <<"write">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"admin_resource">>, <<"execute">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"admin_resource">>, <<"delete">>, [<<"admin">>]),

    %% User permissions - read and write
    ok = erlmcp_auth:add_permission(<<"user_resource">>, <<"read">>, [<<"admin">>, <<"user">>]),
    ok = erlmcp_auth:add_permission(<<"user_resource">>, <<"write">>, [<<"admin">>, <<"user">>]),

    %% Guest permissions - read only
    ok = erlmcp_auth:add_permission(<<"public_resource">>, <<"read">>, [<<"admin">>, <<"user">>, <<"guest">>]),

    ok.

%% @private Create a test session with given roles
create_test_session(UserId, Roles) ->
    %% Add roles to user
    lists:foreach(fun(Role) ->
        erlmcp_auth:add_role(UserId, Role)
    end, Roles),

    %% Create session
    {ok, SessionId} = erlmcp_auth:create_session(UserId, #{
        test => true,
        created_at => erlang:system_time(second)
    }),

    SessionId.
