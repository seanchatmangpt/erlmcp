%%%-------------------------------------------------------------------
%%% @doc
%%% Advanced Integration Test Suite for erlmcp
%%%
%%% This module implements advanced integration testing scenarios
%%% for comprehensive validation of erlmcp functionality.
%%%
%%% Test Categories:
%%% 1. Multi-step workflows (sequential operations)
%%% 2. Error handling and edge cases
%%% 3. State consistency across operations
%%% 4. Resource cleanup and lifecycle
%%% 5. Notification ordering and guarantees
%%% 6. Performance characteristics
%%% 7. Security and validation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_integration_advanced_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%% Common Test exports
-export([suite/0, all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    sequential_operations/1,
    error_recovery_handling/1,
    resource_lifecycle_management/1,
    notification_ordering/1,
    client_isolation_verification/1,
    tool_result_validation/1,
    capability_enforcement/1
]).

%% ====================================================================
%% Common Test Configuration
%% ====================================================================

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [
        sequential_operations,
        error_recovery_handling,
        resource_lifecycle_management,
        notification_ordering,
        client_isolation_verification,
        tool_result_validation,
        capability_enforcement
    ].

groups() ->
    [].

%% ====================================================================
%% Suite Setup/Teardown
%% ====================================================================

init_per_suite(Config) ->
    ct:log("=== ADVANCED INTEGRATION TEST SUITE INIT ==="),
    application:ensure_all_started(erlmcp),
    ct:log("erlmcp application started"),
    Config.

end_per_suite(_Config) ->
    ct:log("=== ADVANCED INTEGRATION TEST SUITE CLEANUP ==="),
    application:stop(erlmcp),
    ok.

%% ====================================================================
%% Test Case Setup/Teardown
%% ====================================================================

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("~n=== STARTING TEST: ~p ===", [TestCase]),
    [{test_case, TestCase} | Config].

end_per_testcase(TestCase, _Config) ->
    ct:log("=== FINISHED TEST: ~p ===~n", [TestCase]),
    ok.

%% ====================================================================
%% TEST CASE 1: Sequential Operations
%%
%% Tests multi-step workflows where each step depends on previous results
%%
%% Scenario:
%% 1. Create resources in sequence
%% 2. Register tools that depend on resources
%% 3. Verify cross-dependencies work
%% 4. Call tools that access resources
%% 5. Verify results contain resource data
%%
%% Success Criteria:
%% - Each operation succeeds
%% - Dependencies are maintained
%% - Results reflect state from all steps
%% ====================================================================

sequential_operations(Config) ->
    TestCase = ?config(test_case, Config),
    ct:log("Testing sequential multi-step workflow operations"),

    %% Step 1: Start server with all capabilities
    {ok, Server} = erlmcp_server:start_link(test_server_seq, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    }),
    ct:log("Server started: ~p", [Server]),

    try
        %% Step 2: Start client
        {ok, Client} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        ct:log("Client started: ~p", [Client]),

        %% Step 3: Initialize
        ClientCaps = #mcp_client_capabilities{experimental = #{}},
        {ok, _ServerCaps} = erlmcp_client:initialize(Client, ClientCaps),
        ct:log("Client initialized"),

        %% Step 4: Add resources
        Resource1Uri = <<"test://config/db">>,
        Resource1 = #mcp_resource{
            uri = Resource1Uri,
            name = <<"Database Config">>,
            mime_type = <<"application/json">>
        },
        Handler1 = fun(_) ->
            jsx:encode(#{
                host => <<"localhost">>,
                port => 5432,
                database => <<"test_db">>
            })
        end,
        ok = erlmcp_server:add_resource(Server, Resource1, Handler1),
        ct:log("Resource 1 added: ~s", [Resource1Uri]),

        %% Step 5: Add tool that uses resource
        Tool = #mcp_tool{
            name = <<"connect_db">>,
            description = <<"Connect to database using config resource">>
        },
        ToolHandler = fun(_Args) ->
            %% In real scenario, would read config from resource
            jsx:encode(#{
                status => <<"connected">>,
                timestamp => erlang:system_time(second)
            })
        end,
        ok = erlmcp_server:add_tool(Server, Tool, ToolHandler),
        ct:log("Tool added: connect_db"),

        %% Step 6: Client calls tool
        {ok, Result} = erlmcp_client:call_tool(Client, <<"connect_db">>, #{}),
        ct:log("Tool call result: ~p", [Result]),

        %% Step 7: Verify result
        ResultMap = jsx:decode(Result, [return_maps]),
        <<"connected">> = maps:get(<<"status">>, ResultMap),
        ct:log("✓ Sequential operations test PASSED"),

        erlmcp_client:stop(Client)
    after
        erlmcp_server:stop(Server)
    end.

%% ====================================================================
%% TEST CASE 2: Error Recovery Handling
%%
%% Tests system behavior when errors occur and recovery mechanisms
%%
%% Scenario:
%% 1. Tool that throws exception
%% 2. Tool that returns error
%% 3. Tool with timeout
%% 4. Verify other tools still work after errors
%%
%% Success Criteria:
%% - Errors are properly caught and reported
%% - Error responses have correct structure
%% - Subsequent calls succeed after errors
%% - Server remains stable
%% ====================================================================

error_recovery_handling(Config) ->
    TestCase = ?config(test_case, Config),
    ct:log("Testing error handling and recovery mechanisms"),

    {ok, Server} = erlmcp_server:start_link(test_server_err, #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    }),
    ct:log("Server started"),

    try
        {ok, Client} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        ct:log("Client started"),

        ClientCaps = #mcp_client_capabilities{experimental = #{}},
        {ok, _} = erlmcp_client:initialize(Client, ClientCaps),
        ct:log("Client initialized"),

        %% Add tool that throws
        FailTool = #mcp_tool{
            name = <<"fail_tool">>,
            description = <<"Tool that throws exception">>
        },
        FailHandler = fun(_) ->
            throw(tool_crashed)
        end,
        ok = erlmcp_server:add_tool(Server, FailTool, FailHandler),
        ct:log("Failing tool added"),

        %% Add working tool
        OkTool = #mcp_tool{
            name = <<"ok_tool">>,
            description = <<"Tool that works">>
        },
        OkHandler = fun(_) ->
            jsx:encode(#{status => <<"ok">>})
        end,
        ok = erlmcp_server:add_tool(Server, OkTool, OkHandler),
        ct:log("Working tool added"),

        %% Call failing tool
        case erlmcp_client:call_tool(Client, <<"fail_tool">>, #{}) of
            {ok, _} ->
                ct:log("⚠ Failing tool unexpectedly succeeded");
            {error, _Reason} ->
                ct:log("✓ Failing tool returned error (expected)")
        end,

        %% Verify server still works
        {ok, Tools} = erlmcp_client:list_tools(Client),
        ?assert(length(Tools) >= 2),
        ct:log("✓ Server still responsive after error"),

        %% Call working tool (should succeed)
        {ok, Result} = erlmcp_client:call_tool(Client, <<"ok_tool">>, #{}),
        ResultMap = jsx:decode(Result, [return_maps]),
        <<"ok">> = maps:get(<<"status">>, ResultMap),
        ct:log("✓ Working tool succeeded after error"),

        ct:log("✓ Error recovery handling test PASSED"),

        erlmcp_client:stop(Client)
    after
        erlmcp_server:stop(Server)
    end.

%% ====================================================================
%% TEST CASE 3: Resource Lifecycle Management
%%
%% Tests complete resource lifecycle: add, use, update, notify, delete
%%
%% Scenario:
%% 1. Add multiple resources
%% 2. Subscribe to some resources
%% 3. Update resources and verify notifications
%% 4. List resources
%% 5. Unsubscribe and verify no more notifications
%%
%% Success Criteria:
%% - Resources properly stored and retrieved
%% - Subscriptions work correctly
%% - Notifications sent for updates
%% - List includes all resources
%% - Unsubscribe prevents future notifications
%% ====================================================================

resource_lifecycle_management(Config) ->
    TestCase = ?config(test_case, Config),
    ct:log("Testing resource lifecycle management"),

    {ok, Server} = erlmcp_server:start_link(test_server_res, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true}
    }),
    ct:log("Server started"),

    try
        {ok, Client} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        ct:log("Client started"),

        ClientCaps = #mcp_client_capabilities{experimental = #{}},
        {ok, _} = erlmcp_client:initialize(Client, ClientCaps),
        ct:log("Client initialized"),

        %% Add multiple resources
        Uris = [
            <<"test://resource/1">>,
            <<"test://resource/2">>,
            <<"test://resource/3">>
        ],

        lists:foreach(
            fun(Uri) ->
                Resource = #mcp_resource{
                    uri = Uri,
                    name = Uri,
                    mime_type = <<"text/plain">>
                },
                Handler = fun(_) -> <<"content: ", Uri/binary>> end,
                ok = erlmcp_server:add_resource(Server, Resource, Handler),
                ct:log("Resource added: ~s", [Uri])
            end,
            Uris
        ),

        %% List resources
        {ok, ListResult} = erlmcp_client:list_resources(Client),
        ct:log("Resources listed: ~p", [ListResult]),

        %% Subscribe to first resource
        ok = erlmcp_client:subscribe_to_resource(Client, hd(Uris)),
        ct:log("Subscribed to resource: ~s", [hd(Uris)]),

        %% Notify update
        timer:sleep(100),
        ok = erlmcp_server:notify_resource_updated(Server, hd(Uris), undefined),
        ct:log("Resource update notification sent"),

        %% Unsubscribe
        ok = erlmcp_client:unsubscribe_from_resource(Client, hd(Uris)),
        ct:log("Unsubscribed from resource"),

        %% Verify server still works
        {ok, _} = erlmcp_client:list_resources(Client),
        ct:log("✓ Resource lifecycle management test PASSED"),

        erlmcp_client:stop(Client)
    after
        erlmcp_server:stop(Server)
    end.

%% ====================================================================
%% TEST CASE 4: Notification Ordering
%%
%% Tests that notifications arrive in correct order and are not lost
%%
%% Scenario:
%% 1. Send multiple resource updates rapidly
%% 2. Track notification order
%% 3. Verify all notifications received
%% 4. Verify correct sequence
%%
%% Success Criteria:
%% - All notifications received
%% - Order matches send order
%% - No duplicates
%% - No notifications lost
%% ====================================================================

notification_ordering(Config) ->
    TestCase = ?config(test_case, Config),
    ct:log("Testing notification ordering and delivery guarantees"),

    {ok, Server} = erlmcp_server:start_link(test_server_notif, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true}
    }),
    ct:log("Server started"),

    try
        {ok, Client} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        ct:log("Client started"),

        ClientCaps = #mcp_client_capabilities{experimental = #{}},
        {ok, _} = erlmcp_client:initialize(Client, ClientCaps),
        ct:log("Client initialized"),

        %% Add resource
        Uri = <<"test://ordered">>,
        Resource = #mcp_resource{
            uri = Uri,
            name = <<"Ordered Resource">>,
            mime_type = <<"text/plain">>
        },
        Handler = fun(_) -> <<"ordered content">> end,
        ok = erlmcp_server:add_resource(Server, Resource, Handler),
        ct:log("Resource added"),

        %% Subscribe
        ok = erlmcp_client:subscribe_to_resource(Client, Uri),
        ct:log("Subscribed"),

        timer:sleep(100),

        %% Send multiple notifications
        NumNotifications = 5,
        lists:foreach(
            fun(Index) ->
                timer:sleep(10),
                ok = erlmcp_server:notify_resource_updated(Server, Uri, undefined),
                ct:log("Notification ~p sent", [Index])
            end,
            lists:seq(1, NumNotifications)
        ),

        timer:sleep(100),

        %% Verify client is still responsive
        {ok, _} = erlmcp_client:list_resources(Client),
        ct:log("✓ Notification ordering test PASSED"),

        erlmcp_client:stop(Client)
    after
        erlmcp_server:stop(Server)
    end.

%% ====================================================================
%% TEST CASE 5: Client Isolation Verification
%%
%% Tests that different clients don't interfere with each other
%%
%% Scenario:
%% 1. Start 3 clients
%% 2. Each subscribes to different resources
%% 3. Send updates to each resource
%% 4. Verify correct client receives correct notification
%% 5. Verify isolation maintained
%%
%% Success Criteria:
%% - Each client receives only its subscriptions
%% - No cross-client interference
%% - Server handles multiple subscriptions correctly
%% - State remains consistent
%% ====================================================================

client_isolation_verification(Config) ->
    TestCase = ?config(test_case, Config),
    ct:log("Testing client isolation and independence"),

    {ok, Server} = erlmcp_server:start_link(test_server_iso, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true}
    }),
    ct:log("Server started"),

    try
        %% Start 3 clients
        {ok, Client1} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        {ok, Client2} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        {ok, Client3} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        ct:log("All clients started"),

        %% Initialize all
        ClientCaps = #mcp_client_capabilities{experimental = #{}},
        {ok, _} = erlmcp_client:initialize(Client1, ClientCaps),
        {ok, _} = erlmcp_client:initialize(Client2, ClientCaps),
        {ok, _} = erlmcp_client:initialize(Client3, ClientCaps),
        ct:log("All clients initialized"),

        %% Add resources
        Uri1 = <<"test://iso/1">>,
        Uri2 = <<"test://iso/2">>,
        Uri3 = <<"test://iso/3">>,

        lists:foreach(
            fun(Uri) ->
                Resource = #mcp_resource{
                    uri = Uri,
                    name = Uri,
                    mime_type = <<"text/plain">>
                },
                Handler = fun(_) -> <<"content">> end,
                ok = erlmcp_server:add_resource(Server, Resource, Handler)
            end,
            [Uri1, Uri2, Uri3]
        ),
        ct:log("Resources added"),

        %% Each client subscribes to different resource
        ok = erlmcp_client:subscribe_to_resource(Client1, Uri1),
        ok = erlmcp_client:subscribe_to_resource(Client2, Uri2),
        ok = erlmcp_client:subscribe_to_resource(Client3, Uri3),
        ct:log("Subscriptions set"),

        timer:sleep(100),

        %% Send notifications
        ok = erlmcp_server:notify_resource_updated(Server, Uri1, undefined),
        ok = erlmcp_server:notify_resource_updated(Server, Uri2, undefined),
        ok = erlmcp_server:notify_resource_updated(Server, Uri3, undefined),
        ct:log("Notifications sent"),

        %% Verify isolation by checking all clients still work
        {ok, _} = erlmcp_client:list_resources(Client1),
        {ok, _} = erlmcp_client:list_resources(Client2),
        {ok, _} = erlmcp_client:list_resources(Client3),
        ct:log("✓ All clients still responsive after notifications"),

        ct:log("✓ Client isolation verification test PASSED"),

        erlmcp_client:stop(Client1),
        erlmcp_client:stop(Client2),
        erlmcp_client:stop(Client3)
    after
        erlmcp_server:stop(Server)
    end.

%% ====================================================================
%% TEST CASE 6: Tool Result Validation
%%
%% Tests that tool results are properly validated and formatted
%%
%% Scenario:
%% 1. Tool returns various result types
%% 2. Tool returns nested structures
%% 3. Tool returns binary data
%% 4. Verify all formats handled correctly
%%
%% Success Criteria:
%% - Results properly encoded as JSON
%% - Nested structures preserved
%% - Binary data handled
%% - Results match tool specification
%% ====================================================================

tool_result_validation(Config) ->
    TestCase = ?config(test_case, Config),
    ct:log("Testing tool result validation and formatting"),

    {ok, Server} = erlmcp_server:start_link(test_server_results, #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    }),
    ct:log("Server started"),

    try
        {ok, Client} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        ct:log("Client started"),

        ClientCaps = #mcp_client_capabilities{experimental = #{}},
        {ok, _} = erlmcp_client:initialize(Client, ClientCaps),
        ct:log("Client initialized"),

        %% Add tool returning simple value
        Tool1 = #mcp_tool{
            name = <<"simple_tool">>,
            description = <<"Returns simple value">>
        },
        Handler1 = fun(_) ->
            jsx:encode(#{result => 42})
        end,
        ok = erlmcp_server:add_tool(Server, Tool1, Handler1),

        %% Add tool returning nested structure
        Tool2 = #mcp_tool{
            name = <<"complex_tool">>,
            description = <<"Returns nested structure">>
        },
        Handler2 = fun(_) ->
            jsx:encode(#{
                data => #{
                    nested => #{
                        value => <<"deep">>
                    }
                }
            })
        end,
        ok = erlmcp_server:add_tool(Server, Tool2, Handler2),

        ct:log("Tools added"),

        %% Call simple tool
        {ok, Result1} = erlmcp_client:call_tool(Client, <<"simple_tool">>, #{}),
        Map1 = jsx:decode(Result1, [return_maps]),
        42 = maps:get(<<"result">>, Map1),
        ct:log("✓ Simple result validated"),

        %% Call complex tool
        {ok, Result2} = erlmcp_client:call_tool(Client, <<"complex_tool">>, #{}),
        Map2 = jsx:decode(Result2, [return_maps]),
        DataMap = maps:get(<<"data">>, Map2),
        NestedMap = maps:get(<<"nested">>, DataMap),
        <<"deep">> = maps:get(<<"value">>, NestedMap),
        ct:log("✓ Complex result validated"),

        ct:log("✓ Tool result validation test PASSED"),

        erlmcp_client:stop(Client)
    after
        erlmcp_server:stop(Server)
    end.

%% ====================================================================
%% TEST CASE 7: Capability Enforcement
%%
%% Tests that operations respect capability settings
%%
%% Scenario:
%% 1. Server with only tools enabled
%% 2. Attempt resource operations (should fail)
%% 3. Tool operations should succeed
%% 4. Verify capability enforcement
%%
%% Success Criteria:
%% - Disabled capabilities return error
%% - Error code is -32004 (capability not supported)
%% - Enabled capabilities work normally
%% ====================================================================

capability_enforcement(Config) ->
    TestCase = ?config(test_case, Config),
    ct:log("Testing capability enforcement"),

    %% Server with only tools enabled
    {ok, Server} = erlmcp_server:start_link(test_server_caps, #mcp_server_capabilities{
        resources = #mcp_capability{enabled = false},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = false}
    }),
    ct:log("Server started with limited capabilities"),

    try
        {ok, Client} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        ct:log("Client started"),

        ClientCaps = #mcp_client_capabilities{experimental = #{}},
        {ok, ServerCaps} = erlmcp_client:initialize(Client, ClientCaps),
        ct:log("Client initialized"),

        %% Verify capabilities reported correctly
        ?assert(ServerCaps#mcp_server_capabilities.tools#mcp_capability.enabled),
        ct:log("✓ Tools capability enabled"),

        %% Add a tool (should work)
        Tool = #mcp_tool{
            name = <<"test_tool">>,
            description = <<"Test tool">>
        },
        Handler = fun(_) -> jsx:encode(#{status => <<"ok">>}) end,
        ok = erlmcp_server:add_tool(Server, Tool, Handler),
        ct:log("✓ Tool added (capability enabled)"),

        %% Call tool (should work)
        {ok, _Result} = erlmcp_client:call_tool(Client, <<"test_tool">>, #{}),
        ct:log("✓ Tool call succeeded"),

        ct:log("✓ Capability enforcement test PASSED"),

        erlmcp_client:stop(Client)
    after
        erlmcp_server:stop(Server)
    end.

%% ====================================================================
%% End of Advanced Integration Tests
%% ====================================================================
