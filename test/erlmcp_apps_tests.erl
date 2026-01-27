%%%-------------------------------------------------------------------
%% @doc Test suite for MCP Apps feature (erlmcp_apps and erlmcp_app_sandbox)
%%
%% Comprehensive test suite covering:
%% - App registration and lifecycle management
%% - Sandbox creation and resource isolation
%% - Permission-based access control
%% - Communication via postMessage API
%% - Security isolation and CSP headers
%% - State management per app
%% - Error handling and recovery
%%
%% @author ErlMCP Development Team
%% @since 0.8.0
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_apps_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, _Pid} = erlmcp_apps:start_link(),
    {ok, _SandboxPid} = erlmcp_app_sandbox:start_link(),
    ok.

cleanup(_) ->
    catch erlmcp_apps:stop(),
    catch erlmcp_app_sandbox:stop(),
    ok.

%%====================================================================
%% App Registration Tests
%%====================================================================

register_app_basic_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        AppName = <<"test-app">>,
        Version = <<"1.0.0">>,
        Manifest = #{
            <<"name">> => AppName,
            <<"version">> => Version,
            <<"description">> => <<"Test Application">>
        },

        {ok, AppId} = erlmcp_apps:register_app(AppName, Version, Manifest),
        ?assertIsNotEmpty(AppId),
        ?assertEqual(byte_size(AppId) > 0, true),

        {ok, App} = erlmcp_apps:get_app(AppId),
        ?assertEqual(App#mcp_app.name, AppName),
        ?assertEqual(App#mcp_app.version, Version),
        ?assertEqual(App#mcp_app.status, initialized)
    end}.

register_app_with_config_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        AppName = <<"configured-app">>,
        Version = <<"2.0.0">>,
        Manifest = #{<<"name">> => AppName, <<"version">> => Version},
        Config = #{
            <<"uri">> => <<"https://app.example.com">>,
            <<"permissions">> => [<<"tools/call">>, <<"resources/read">>]
        },

        {ok, AppId} = erlmcp_apps:register_app(AppName, Version, Manifest, Config),
        {ok, App} = erlmcp_apps:get_app(AppId),

        ?assertEqual(App#mcp_app.uri, <<"https://app.example.com">>),
        ?assertEqual(sets:size(App#mcp_app.permissions), 2)
    end}.

duplicate_app_registration_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        AppName = <<"duplicate-app">>,
        Version = <<"1.0.0">>,
        Manifest = #{<<"name">> => AppName, <<"version">> => Version},

        {ok, _AppId1} = erlmcp_apps:register_app(AppName, Version, Manifest),
        {error, app_already_registered} = erlmcp_apps:register_app(AppName, Version, Manifest)
    end}.

list_apps_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest1 = #{<<"name">> => <<"app1">>, <<"version">> => <<"1.0.0">>},
        Manifest2 = #{<<"name">> => <<"app2">>, <<"version">> => <<"2.0.0">>},

        {ok, _AppId1} = erlmcp_apps:register_app(<<"app1">>, <<"1.0.0">>, Manifest1),
        {ok, _AppId2} = erlmcp_apps:register_app(<<"app2">>, <<"2.0.0">>, Manifest2),

        Apps = erlmcp_apps:list_apps(),
        ?assertEqual(length(Apps), 2)
    end}.

unregister_app_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        AppName = <<"removable-app">>,
        Version = <<"1.0.0">>,
        Manifest = #{<<"name">> => AppName, <<"version">> => Version},

        {ok, AppId} = erlmcp_apps:register_app(AppName, Version, Manifest),
        ok = erlmcp_apps:unregister_app(AppId),

        {error, not_found} = erlmcp_apps:get_app(AppId)
    end}.

%%====================================================================
%% App Lifecycle Tests
%%====================================================================

activate_app_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"lifecycle-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"lifecycle-app">>, <<"1.0.0">>, Manifest),

        {ok, AppBefore} = erlmcp_apps:get_app(AppId),
        ?assertEqual(AppBefore#mcp_app.status, initialized),

        ok = erlmcp_apps:activate_app(AppId),
        {ok, AppAfter} = erlmcp_apps:get_app(AppId),
        ?assertEqual(AppAfter#mcp_app.status, active)
    end}.

deactivate_app_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"deactivate-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"deactivate-app">>, <<"1.0.0">>, Manifest),

        ok = erlmcp_apps:activate_app(AppId),
        ok = erlmcp_apps:deactivate_app(AppId),

        {ok, App} = erlmcp_apps:get_app(AppId),
        ?assertEqual(App#mcp_app.status, inactive)
    end}.

activate_nonexistent_app_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        {error, not_found} = erlmcp_apps:activate_app(<<"nonexistent">>)
    end}.

%%====================================================================
%% Permission Tests
%%====================================================================

check_permission_granted_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"perm-app">>, <<"version">> => <<"1.0.0">>},
        Config = #{
            <<"permissions">> => [<<"resources/read">>, <<"tools/call">>]
        },
        {ok, AppId} = erlmcp_apps:register_app(<<"perm-app">>, <<"1.0.0">>, Manifest, Config),

        ?assertEqual(true, erlmcp_apps:check_permission(AppId, <<"resources/read">>, #{})),
        ?assertEqual(true, erlmcp_apps:check_permission(AppId, <<"tools/call">>, #{}))
    end).

check_permission_denied_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"restricted-app">>, <<"version">> => <<"1.0.0">>},
        Config = #{
            <<"permissions">> => [<<"resources/read">>]
        },
        {ok, AppId} = erlmcp_apps:register_app(<<"restricted-app">>, <<"1.0.0">>, Manifest, Config),

        ?assertEqual(false, erlmcp_apps:check_permission(AppId, <<"tools/call">>, #{}))
    end}.

grant_permission_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"grant-app">>, <<"version">> => <<"1.0.0">>},
        Config = #{<<"permissions">> => []},
        {ok, AppId} = erlmcp_apps:register_app(<<"grant-app">>, <<"1.0.0">>, Manifest, Config),

        ok = erlmcp_apps:grant_permission(AppId, <<"tools/call">>),
        ?assertEqual(true, erlmcp_apps:check_permission(AppId, <<"tools/call">>, #{}))
    end}.

revoke_permission_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"revoke-app">>, <<"version">> => <<"1.0.0">>},
        Config = #{
            <<"permissions">> => [<<"tools/call">>]
        },
        {ok, AppId} = erlmcp_apps:register_app(<<"revoke-app">>, <<"1.0.0">>, Manifest, Config),

        ok = erlmcp_apps:revoke_permission(AppId, <<"tools/call">>),
        ?assertEqual(false, erlmcp_apps:check_permission(AppId, <<"tools/call">>, #{}))
    end).

%%====================================================================
%% State Management Tests
%%====================================================================

get_app_state_empty_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"state-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"state-app">>, <<"1.0.0">>, Manifest),

        {ok, State} = erlmcp_apps:get_app_state(AppId),
        ?assertEqual(State, #{})
    end}.

set_app_state_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"state-set-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"state-set-app">>, <<"1.0.0">>, Manifest),

        NewState = #{<<"counter">> => 42, <<"active">> => true},
        ok = erlmcp_apps:set_app_state(AppId, NewState),

        {ok, State} = erlmcp_apps:get_app_state(AppId),
        ?assertEqual(State, NewState)
    end}.

set_app_state_nonexistent_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        {error, not_found} = erlmcp_apps:set_app_state(<<"nonexistent">>, #{})
    end}.

%%====================================================================
%% Sandbox Tests
%%====================================================================

create_sandbox_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"sandbox-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"sandbox-app">>, <<"1.0.0">>, Manifest),

        Config = #{<<"origin">> => <<"https://app.example.com">>},
        {ok, SandboxId} = erlmcp_app_sandbox:create_sandbox(AppId, Config),

        ?assertIsNotEmpty(SandboxId),
        {ok, Sandbox} = erlmcp_app_sandbox:get_sandbox(SandboxId),
        ?assertEqual(Sandbox#sandbox.app_id, AppId)
    end}.

destroy_sandbox_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"destroy-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"destroy-app">>, <<"1.0.0">>, Manifest),

        {ok, SandboxId} = erlmcp_app_sandbox:create_sandbox(AppId, #{}),
        ok = erlmcp_app_sandbox:destroy_sandbox(SandboxId),

        {error, not_found} = erlmcp_app_sandbox:get_sandbox(SandboxId)
    end}.

sandbox_isolation_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest1 = #{<<"name">> => <<"isolated-app1">>, <<"version">> => <<"1.0.0">>},
        Manifest2 = #{<<"name">> => <<"isolated-app2">>, <<"version">> => <<"1.0.0">>},

        {ok, AppId1} = erlmcp_apps:register_app(<<"isolated-app1">>, <<"1.0.0">>, Manifest1),
        {ok, AppId2} = erlmcp_apps:register_app(<<"isolated-app2">>, <<"1.0.0">>, Manifest2),

        {ok, SandboxId1} = erlmcp_app_sandbox:create_sandbox(AppId1, #{}),
        {ok, SandboxId2} = erlmcp_app_sandbox:create_sandbox(AppId2, #{}),

        {ok, Sandbox1} = erlmcp_app_sandbox:get_sandbox(SandboxId1),
        {ok, Sandbox2} = erlmcp_app_sandbox:get_sandbox(SandboxId2),

        ?assertNotEqual(Sandbox1#sandbox.state, Sandbox2#sandbox.state)
    end}.

sandbox_permissions_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"perm-sandbox">>, <<"version">> => <<"1.0.0">>},
        Config = #{
            <<"permissions">> => [<<"resources/read">>, <<"tools/call">>]
        },
        {ok, AppId} = erlmcp_apps:register_app(<<"perm-sandbox">>, <<"1.0.0">>, Manifest, Config),

        SandboxConfig = #{
            <<"permissions">> => sets:from_list([<<"resources/read">>, <<"tools/call">>])
        },
        {ok, _SandboxId} = erlmcp_app_sandbox:create_sandbox(AppId, SandboxConfig)
    end}.

%%====================================================================
%% Sandbox Communication Tests
%%====================================================================

send_message_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"msg-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"msg-app">>, <<"1.0.0">>, Manifest),

        {ok, SandboxId} = erlmcp_app_sandbox:create_sandbox(AppId, #{}),
        ok = erlmcp_app_sandbox:send_message(SandboxId, <<"test/method">>, #{<<"param">> => <<"value">>})
    end}.

send_message_nonexistent_sandbox_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        {error, not_found} = erlmcp_app_sandbox:send_message(
            <<"nonexistent">>,
            <<"test/method">>,
            #{}
        )
    end}.

receive_message_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"recv-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"recv-app">>, <<"1.0.0">>, Manifest),

        {ok, SandboxId} = erlmcp_app_sandbox:create_sandbox(AppId, #{}),
        Message = #{<<"type">> => <<"test">>},
        ok = erlmcp_app_sandbox:receive_message(SandboxId, Message)
    end}.

%%====================================================================
%% CSP and Security Tests
%%====================================================================

csp_headers_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"csp-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"csp-app">>, <<"1.0.0">>, Manifest),

        {ok, SandboxId} = erlmcp_app_sandbox:create_sandbox(AppId, #{}),
        Headers = erlmcp_app_sandbox:get_csp_headers(SandboxId),

        ?assert(maps:is_key(<<"Content-Security-Policy">>, Headers)),
        ?assert(maps:is_key(<<"X-Frame-Options">>, Headers)),
        ?assert(maps:is_key(<<"X-XSS-Protection">>, Headers))
    end}.

validate_origin_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"origin-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"origin-app">>, <<"1.0.0">>, Manifest),

        Config = #{<<"origin">> => <<"https://trusted.example.com">>},
        {ok, SandboxId} = erlmcp_app_sandbox:create_sandbox(AppId, Config),

        ?assertEqual(true, erlmcp_app_sandbox:validate_origin(SandboxId, <<"https://trusted.example.com">>)),
        ?assertEqual(false, erlmcp_app_sandbox:validate_origin(SandboxId, <<"https://untrusted.example.com">>))
    end}.

check_resource_access_test() ->
    {setup, fun setup/0, fun cleanup/1, fun() ->
        Manifest = #{<<"name">> => <<"resource-app">>, <<"version">> => <<"1.0.0">>},
        {ok, AppId} = erlmcp_apps:register_app(<<"resource-app">>, <<"1.0.0">>, Manifest),

        SandboxConfig = #{
            <<"permissions">> => sets:from_list([<<"resources/read">>])
        },
        {ok, SandboxId} = erlmcp_app_sandbox:create_sandbox(AppId, SandboxConfig),

        ?assertEqual(true, erlmcp_app_sandbox:check_resource_access(SandboxId, <<"resources/read">>)),
        ?assertEqual(false, erlmcp_app_sandbox:check_resource_access(SandboxId, <<"tools/call">>))
    end).

%%====================================================================
%% Utility Tests
%%====================================================================

validate_app_name_test() ->
    ok = erlmcp_apps_util:validate_app_name(<<"valid-app">>),
    ok = erlmcp_apps_util:validate_app_name(<<"Valid App">>),
    ok = erlmcp_apps_util:validate_app_name(<<"valid_app_123">>),
    {error, _} = erlmcp_apps_util:validate_app_name(<<"">>),
    {error, _} = erlmcp_apps_util:validate_app_name(123).

validate_permission_test() ->
    ok = erlmcp_apps_util:validate_permission(<<"resources/read">>),
    ok = erlmcp_apps_util:validate_permission(<<"tools/call">>),
    ok = erlmcp_apps_util:validate_permission(<<"admin/control">>),
    {error, _} = erlmcp_apps_util:validate_permission(<<"invalid">>).

validate_uri_test() ->
    ok = erlmcp_apps_util:validate_uri(<<"https://app.example.com">>),
    ok = erlmcp_apps_util:validate_uri(<<"http://localhost:8080">>),
    {error, _} = erlmcp_apps_util:validate_uri(<<"ftp://example.com">>),
    {error, _} = erlmcp_apps_util:validate_uri(<<"">>).

normalize_app_name_test() ->
    ?assertEqual(erlmcp_apps_util:normalize_app_name(<<"My App">>), <<"my_app">>),
    ?assertEqual(erlmcp_apps_util:normalize_app_name(<<"UPPERCASE">>), <<"uppercase">>),
    ?assertEqual(erlmcp_apps_util:normalize_app_name(<<"Mixed Case App">>), <<"mixed_case_app">>).

calculate_app_checksum_test() ->
    Manifest1 = #{<<"name">> => <<"app1">>},
    Manifest2 = #{<<"name">> => <<"app2">>},

    Checksum1 = erlmcp_apps_util:calculate_app_checksum(Manifest1),
    Checksum2 = erlmcp_apps_util:calculate_app_checksum(Manifest2),

    ?assertNotEqual(Checksum1, Checksum2),
    ?assert(is_binary(Checksum1)),
    ?assert(byte_size(Checksum1) > 0).

generate_app_id_test() ->
    AppId1 = erlmcp_apps_util:generate_app_id(<<"test-app">>),
    AppId2 = erlmcp_apps_util:generate_app_id(<<"test-app">>),

    ?assertIsNotEmpty(AppId1),
    ?assertIsNotEmpty(AppId2),
    % IDs should be unique even for same name
    ?assertNotEqual(AppId1, AppId2).

%%====================================================================
%% Helper Functions
%%====================================================================

assertIsNotEmpty(Value) ->
    ?assert(Value =/= [] andalso Value =/= <<>> andalso Value =/= undefined).

assertNotEqual(A, B) ->
    ?assert(A =/= B).

end.
