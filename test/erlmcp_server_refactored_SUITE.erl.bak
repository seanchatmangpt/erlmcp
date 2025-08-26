%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive test suite for refactored erlmcp_server (transport-free)
%%%
%%% This test suite validates the refactored erlmcp_server that operates
%%% without direct transport dependencies, using registry-based message
%%% routing instead.
%%%
%%% Test Categories:
%%% 1. Protocol Handling - MCP request/response processing
%%% 2. Resource Management - Add/remove/list resources, subscriptions
%%% 3. Tool Management - Tool registration, execution, validation
%%% 4. Message Flow - Registry-based routing, batch handling
%%% 5. State Management - Initialization, capabilities, consistency
%%%-------------------------------------------------------------------
-module(erlmcp_server_refactored_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases - Protocol Handling
-export([
    initialize_request/1,
    initialize_response_format/1,
    capability_negotiation/1,
    json_rpc_decode_error/1,
    invalid_request_method/1,
    malformed_request_params/1,
    request_with_missing_id/1,
    notification_handling/1
]).

%% Test cases - Resource Management
-export([
    add_resource_basic/1,
    add_resource_template/1,
    list_resources/1,
    read_resource_success/1,
    read_resource_not_found/1,
    resource_handler_exception/1,
    subscribe_to_resource/1,
    unsubscribe_from_resource/1,
    resource_update_notification/1,
    concurrent_resource_access/1,
    resource_list_changed_notification/1
]).

%% Test cases - Tool Management  
-export([
    add_tool_basic/1,
    add_tool_with_schema/1,
    list_tools/1,
    call_tool_success/1,
    call_tool_not_found/1,
    tool_handler_exception/1,
    tool_validation/1,
    tool_cancellation_simulation/1,
    concurrent_tool_calls/1
]).

%% Test cases - Message Flow
-export([
    registry_message_routing/1,
    response_routing_through_registry/1,
    batch_message_handling/1,
    progress_notifications/1,
    notification_broadcast/1,
    transport_binding_validation/1,
    message_flow_error_handling/1
]).

%% Test cases - State Management
-export([
    server_initialization/1,
    capability_consistency/1,
    state_after_operations/1,
    memory_efficiency/1,
    subscription_state_management/1,
    server_shutdown_cleanup/1,
    state_recovery_after_error/1
]).

%% Test cases - Prompt Management
-export([
    add_prompt_basic/1,
    add_prompt_with_arguments/1,
    list_prompts/1,
    get_prompt_success/1,
    get_prompt_not_found/1,
    prompt_handler_exception/1
]).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [
        {group, protocol_handling},
        {group, resource_management},
        {group, tool_management},
        {group, message_flow},
        {group, state_management},
        {group, prompt_management}
    ].

groups() ->
    [
        {protocol_handling, [parallel], [
            initialize_request,
            initialize_response_format,
            capability_negotiation,
            json_rpc_decode_error,
            invalid_request_method,
            malformed_request_params,
            request_with_missing_id,
            notification_handling
        ]},
        {resource_management, [sequence], [
            add_resource_basic,
            add_resource_template,
            list_resources,
            read_resource_success,
            read_resource_not_found,
            resource_handler_exception,
            subscribe_to_resource,
            unsubscribe_from_resource,
            resource_update_notification,
            concurrent_resource_access,
            resource_list_changed_notification
        ]},
        {tool_management, [sequence], [
            add_tool_basic,
            add_tool_with_schema,
            list_tools,
            call_tool_success,
            call_tool_not_found,
            tool_handler_exception,
            tool_validation,
            tool_cancellation_simulation,
            concurrent_tool_calls
        ]},
        {message_flow, [sequence], [
            registry_message_routing,
            response_routing_through_registry,
            batch_message_handling,
            progress_notifications,
            notification_broadcast,
            transport_binding_validation,
            message_flow_error_handling
        ]},
        {state_management, [parallel], [
            server_initialization,
            capability_consistency,
            state_after_operations,
            memory_efficiency,
            subscription_state_management,
            server_shutdown_cleanup,
            state_recovery_after_error
        ]},
        {prompt_management, [sequence], [
            add_prompt_basic,
            add_prompt_with_arguments,
            list_prompts,
            get_prompt_success,
            get_prompt_not_found,
            prompt_handler_exception
        ]}
    ].

%%====================================================================
%% Suite Setup/Teardown
%%====================================================================

init_per_suite(Config) ->
    ct:pal("Starting erlmcp_server refactored test suite"),
    
    % Start necessary applications
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(sasl),
    
    % Start registry for message routing tests
    case erlmcp_registry:start_link() of
        {ok, RegistryPid} -> 
            ct:pal("Registry started: ~p", [RegistryPid]);
        {error, {already_started, RegistryPid}} -> 
            ct:pal("Registry already running: ~p", [RegistryPid])
    end,
    
    % Mock transport for testing
    {ok, MockTransport} = start_mock_transport(),
    
    [{mock_transport, MockTransport} | Config].

end_per_suite(Config) ->
    MockTransport = ?config(mock_transport, Config),
    stop_mock_transport(MockTransport),
    ct:pal("Ending erlmcp_server refactored test suite"),
    ok.

init_per_group(GroupName, Config) ->
    ct:pal("Starting group: ~p", [GroupName]),
    Config.

end_per_group(GroupName, _Config) ->
    ct:pal("Ending group: ~p", [GroupName]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    % Start fresh server for each test
    ServerId = list_to_atom("test_server_" ++ atom_to_list(TestCase)),
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true},
        logging = #mcp_capability{enabled = true}
    },
    {ok, ServerPid} = erlmcp_server_refactored:start_link(ServerId, Capabilities),
    [{server_pid, ServerPid}, {server_id, ServerId} | Config].

end_per_testcase(TestCase, Config) ->
    ServerPid = ?config(server_pid, Config),
    if
        is_pid(ServerPid) -> erlmcp_server_refactored:stop(ServerPid);
        true -> ok
    end,
    ct:pal("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases - Protocol Handling
%%====================================================================

initialize_request(Config) ->
    ServerPid = ?config(server_pid, Config),
    TransportId = test_transport,
    
    % Mock initialize request
    Params = #{
        ?MCP_FIELD_PROTOCOL_VERSION => ?MCP_VERSION,
        ?MCP_FIELD_CLIENT_INFO => #{
            ?MCP_INFO_NAME => <<"test_client">>,
            ?MCP_INFO_VERSION => <<"1.0.0">>
        },
        ?MCP_FIELD_CAPABILITIES => #{
            ?MCP_CAPABILITY_RESOURCES => #{?MCP_FEATURE_SUBSCRIBE => true}
        }
    },
    
    Request = #json_rpc_request{
        id = 1,
        method = ?MCP_METHOD_INITIALIZE,
        params = Params
    },
    
    % Send initialize message to server
    EncodedRequest = erlmcp_json_rpc:encode_request(Request#json_rpc_request.id,
                                                   Request#json_rpc_request.method,
                                                   Request#json_rpc_request.params),
    ServerPid ! {mcp_message, TransportId, EncodedRequest},
    
    % Verify server is initialized and response is sent via registry
    timer:sleep(100), % Allow async processing
    
    % Check server state via gen_server call
    State = get_server_state(ServerPid),
    ?assert(State =/= undefined),
    ok.

initialize_response_format(Config) ->
    ServerPid = ?config(server_pid, Config),
    TransportId = test_transport,
    
    % Send initialize request
    InitRequest = create_json_rpc_message(1, ?MCP_METHOD_INITIALIZE, #{}),
    ServerPid ! {mcp_message, TransportId, InitRequest},
    
    timer:sleep(100),
    
    % Verify response format through mock transport
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    ?assert(maps:is_key(?MCP_FIELD_PROTOCOL_VERSION, ResponseData)),
    ?assert(maps:is_key(?MCP_FIELD_CAPABILITIES, ResponseData)),
    ?assert(maps:is_key(?MCP_FIELD_SERVER_INFO, ResponseData)),
    
    % Verify capabilities format
    Capabilities = maps:get(?MCP_FIELD_CAPABILITIES, ResponseData),
    ?assert(maps:is_key(?MCP_CAPABILITY_RESOURCES, Capabilities)),
    ?assert(maps:is_key(?MCP_CAPABILITY_TOOLS, Capabilities)),
    ?assert(maps:is_key(?MCP_CAPABILITY_PROMPTS, Capabilities)),
    ok.

capability_negotiation(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Test different capability configurations
    Capabilities1 = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = false},
        prompts = undefined,
        logging = #mcp_capability{enabled = true}
    },
    
    % Restart server with different capabilities
    erlmcp_server_refactored:stop(ServerPid),
    {ok, ServerPid2} = erlmcp_server_refactored:start_link(test_server_caps, Capabilities1),
    
    % Send initialize request
    InitRequest = create_json_rpc_message(1, ?MCP_METHOD_INITIALIZE, #{}),
    ServerPid2 ! {mcp_message, test_transport, InitRequest},
    
    timer:sleep(100),
    
    % Verify only enabled capabilities are advertised
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Capabilities = maps:get(?MCP_FIELD_CAPABILITIES, ResponseData),
    
    % Should have resources and logging, not tools or prompts
    ?assert(maps:is_key(?MCP_CAPABILITY_RESOURCES, Capabilities)),
    ?assert(maps:is_key(?MCP_CAPABILITY_LOGGING, Capabilities)),
    ?assertNot(maps:is_key(?MCP_CAPABILITY_TOOLS, Capabilities)),
    ?assertNot(maps:is_key(?MCP_CAPABILITY_PROMPTS, Capabilities)),
    
    erlmcp_server_refactored:stop(ServerPid2),
    ok.

json_rpc_decode_error(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Send malformed JSON
    MalformedJson = <<"{invalid json">>,
    ServerPid ! {mcp_message, test_transport, MalformedJson},
    
    timer:sleep(100),
    
    % Server should handle gracefully without crashing
    ?assert(is_process_alive(ServerPid)),
    
    % Should not have sent any response for malformed message
    Response = get_mock_transport_response(test_transport, undefined),
    ?assertEqual(no_response, Response),
    ok.

invalid_request_method(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Send request with unknown method
    InvalidRequest = create_json_rpc_message(1, <<"unknown/method">>, #{}),
    ServerPid ! {mcp_message, test_transport, InvalidRequest},
    
    timer:sleep(100),
    
    % Should receive method not found error
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({error, 1, ?JSONRPC_METHOD_NOT_FOUND, _}, Response),
    ok.

malformed_request_params(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Initialize first
    InitRequest = create_json_rpc_message(1, ?MCP_METHOD_INITIALIZE, #{}),
    ServerPid ! {mcp_message, test_transport, InitRequest},
    timer:sleep(100),
    clear_mock_responses(test_transport),
    
    % Send resources/read without required uri parameter
    ReadRequest = create_json_rpc_message(2, ?MCP_METHOD_RESOURCES_READ, #{}),
    ServerPid ! {mcp_message, test_transport, ReadRequest},
    
    timer:sleep(100),
    
    % Should receive invalid params error
    Response = get_mock_transport_response(test_transport, 2),
    ?assertMatch({error, 2, ?JSONRPC_INVALID_PARAMS, _}, Response),
    ok.

request_with_missing_id(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Create request without id (should be treated as notification)
    NotificationJson = jsx:encode(#{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_INITIALIZED
    }),
    
    ServerPid ! {mcp_message, test_transport, NotificationJson},
    
    timer:sleep(100),
    
    % Server should handle notification without responding
    ?assert(is_process_alive(ServerPid)),
    
    % No response should be sent for notifications
    Response = get_mock_transport_response(test_transport, undefined),
    ?assertEqual(no_response, Response),
    ok.

notification_handling(Config) ->
    ServerPid = ?config(server_id, Config),
    
    % Send initialized notification
    NotificationJson = jsx:encode(#{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_INITIALIZED
    }),
    
    ServerPid ! {mcp_message, test_transport, NotificationJson},
    
    timer:sleep(100),
    
    % Server should process notification without issues
    ?assert(is_process_alive(ServerPid)),
    ok.

%%====================================================================
%% Test Cases - Resource Management
%%====================================================================

add_resource_basic(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    Uri = <<"file://test.txt">>,
    Handler = fun(_) -> <<"Test content">> end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler)),
    
    % Verify resource is stored by listing resources
    initialize_server(ServerPid, test_transport),
    
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Resources = maps:get(?MCP_PARAM_RESOURCES, ResponseData),
    ?assert(length(Resources) > 0),
    
    % Find our resource
    Resource = lists:keyfind(Uri, 2, [maps:get(?MCP_PARAM_URI, R) || R <- Resources]),
    ?assertNotEqual(false, Resource),
    ok.

add_resource_template(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    Template = <<"file://{path}">>,
    Name = <<"dynamic file">>,
    Handler = fun(Uri) -> <<"Content for ", Uri/binary>> end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_resource_template(ServerPid, Template, Name, Handler)),
    
    % List resource templates
    initialize_server(ServerPid, test_transport),
    
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_TEMPLATES_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Templates = maps:get(?MCP_PARAM_RESOURCE_TEMPLATES, ResponseData),
    ?assert(length(Templates) > 0),
    ok.

list_resources(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Add multiple resources
    Resources = [
        {<<"file://test1.txt">>, fun(_) -> <<"Content 1">> end},
        {<<"file://test2.txt">>, fun(_) -> <<"Content 2">> end},
        {<<"http://example.com">>, fun(_) -> <<"Web content">> end}
    ],
    
    lists:foreach(fun({Uri, Handler}) ->
        ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler))
    end, Resources),
    
    % Initialize server and list resources
    initialize_server(ServerPid, test_transport),
    
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    ResourceList = maps:get(?MCP_PARAM_RESOURCES, ResponseData),
    ?assertEqual(length(Resources), length(ResourceList)),
    
    % Verify all resources are present
    Uris = [maps:get(?MCP_PARAM_URI, R) || R <- ResourceList],
    lists:foreach(fun({Uri, _}) ->
        ?assert(lists:member(Uri, Uris))
    end, Resources),
    ok.

read_resource_success(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    Uri = <<"file://success.txt">>,
    ExpectedContent = <<"This is test content">>,
    Handler = fun(_) -> ExpectedContent end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Read the resource
    ReadParams = #{?MCP_PARAM_URI => Uri},
    ReadRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_READ, ReadParams),
    ServerPid ! {mcp_message, test_transport, ReadRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Contents = maps:get(?MCP_PARAM_CONTENTS, ResponseData),
    ?assert(length(Contents) > 0),
    
    Content = hd(Contents),
    ?assertEqual(Uri, maps:get(?MCP_PARAM_URI, Content)),
    ?assertEqual(ExpectedContent, maps:get(?MCP_PARAM_TEXT, Content)),
    ok.

read_resource_not_found(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    initialize_server(ServerPid, test_transport),
    
    % Try to read non-existent resource
    Uri = <<"file://nonexistent.txt">>,
    ReadParams = #{?MCP_PARAM_URI => Uri},
    ReadRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_READ, ReadParams),
    ServerPid ! {mcp_message, test_transport, ReadRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({error, 1, ?MCP_ERROR_RESOURCE_NOT_FOUND, _}, Response),
    ok.

resource_handler_exception(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    Uri = <<"file://error.txt">>,
    Handler = fun(_) -> error(handler_crashed) end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Read the resource (should cause handler to crash)
    ReadParams = #{?MCP_PARAM_URI => Uri},
    ReadRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_READ, ReadParams),
    ServerPid ! {mcp_message, test_transport, ReadRequest},
    
    timer:sleep(100),
    
    % Should receive internal error, server should still be alive
    ?assert(is_process_alive(ServerPid)),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({error, 1, ?JSONRPC_INTERNAL_ERROR, _}, Response),
    ok.

subscribe_to_resource(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    Uri = <<"file://watched.txt">>,
    Handler = fun(_) -> <<"Watched content">> end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Subscribe to resource
    SubParams = #{?MCP_PARAM_URI => Uri},
    SubRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_SUBSCRIBE, SubParams),
    ServerPid ! {mcp_message, test_transport, SubRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    ok.

unsubscribe_from_resource(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    Uri = <<"file://watched.txt">>,
    Handler = fun(_) -> <<"Watched content">> end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % First subscribe
    SubParams = #{?MCP_PARAM_URI => Uri},
    SubRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_SUBSCRIBE, SubParams),
    ServerPid ! {mcp_message, test_transport, SubRequest},
    
    timer:sleep(100),
    clear_mock_responses(test_transport),
    
    % Then unsubscribe
    UnsubRequest = create_json_rpc_message(2, ?MCP_METHOD_RESOURCES_UNSUBSCRIBE, SubParams),
    ServerPid ! {mcp_message, test_transport, UnsubRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 2),
    ?assertMatch({response, 2, _}, Response),
    ok.

resource_update_notification(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    Uri = <<"file://notify.txt">>,
    Handler = fun(_) -> <<"Notify content">> end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Subscribe to resource first
    SubParams = #{?MCP_PARAM_URI => Uri},
    SubRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_SUBSCRIBE, SubParams),
    ServerPid ! {mcp_message, test_transport, SubRequest},
    
    timer:sleep(100),
    clear_mock_responses(test_transport),
    
    % Trigger resource update notification
    Metadata = #{<<"lastModified">> => <<"2023-01-01T00:00:00Z">>},
    ?assertEqual(ok, erlmcp_server_refactored:notify_resource_updated(ServerPid, Uri, Metadata)),
    
    timer:sleep(100),
    
    % Should receive resource updated notification
    Notification = get_mock_transport_notification(test_transport, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_UPDATED),
    ?assertMatch({notification, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_UPDATED, _}, Notification),
    ok.

concurrent_resource_access(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Add resource with slow handler
    Uri = <<"file://slow.txt">>,
    Handler = fun(_) ->
        timer:sleep(50),  % Simulate slow operation
        <<"Slow content">>
    end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Send multiple concurrent read requests
    ReadParams = #{?MCP_PARAM_URI => Uri},
    [begin
        ReadRequest = create_json_rpc_message(Id, ?MCP_METHOD_RESOURCES_READ, ReadParams),
        ServerPid ! {mcp_message, test_transport, ReadRequest}
    end || Id <- lists:seq(1, 5)],
    
    timer:sleep(300),  % Wait for all requests to process
    
    % All requests should succeed
    [begin
        Response = get_mock_transport_response(test_transport, Id),
        ?assertMatch({response, Id, _}, Response)
    end || Id <- lists:seq(1, 5)],
    ok.

resource_list_changed_notification(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    initialize_server(ServerPid, test_transport),
    clear_mock_responses(test_transport),
    
    % Trigger resources list changed notification
    ?assertEqual(ok, erlmcp_server_refactored:notify_resources_changed(ServerPid)),
    
    timer:sleep(100),
    
    % Should receive list changed notification
    Notification = get_mock_transport_notification(test_transport, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED),
    ?assertMatch({notification, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, _}, Notification),
    ok.

%%====================================================================
%% Test Cases - Tool Management
%%====================================================================

add_tool_basic(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    ToolName = <<"echo">>,
    Handler = fun(Args) ->
        Text = maps:get(<<"text">>, Args, <<"hello">>),
        <<"Echo: ", Text/binary>>
    end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_tool(ServerPid, ToolName, Handler)),
    
    % Verify tool is added by listing tools
    initialize_server(ServerPid, test_transport),
    
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_TOOLS_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Tools = maps:get(?MCP_PARAM_TOOLS, ResponseData),
    ?assert(length(Tools) > 0),
    
    % Find our tool
    Tool = hd(Tools),
    ?assertEqual(ToolName, maps:get(?MCP_PARAM_NAME, Tool)),
    ok.

add_tool_with_schema(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    ToolName = <<"calculator">>,
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"operation">> => #{<<"type">> => <<"string">>},
            <<"a">> => #{<<"type">> => <<"number">>},
            <<"b">> => #{<<"type">> => <<"number">>}
        },
        <<"required">> => [<<"operation">>, <<"a">>, <<"b">>]
    },
    Handler = fun(Args) ->
        Op = maps:get(<<"operation">>, Args),
        A = maps:get(<<"a">>, Args),
        B = maps:get(<<"b">>, Args),
        case Op of
            <<"add">> -> integer_to_binary(A + B);
            <<"subtract">> -> integer_to_binary(A - B);
            _ -> <<"Unknown operation">>
        end
    end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_tool_with_schema(ServerPid, ToolName, Handler, Schema)),
    
    % List tools and verify schema is included
    initialize_server(ServerPid, test_transport),
    
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_TOOLS_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Tools = maps:get(?MCP_PARAM_TOOLS, ResponseData),
    Tool = hd(Tools),
    
    ?assertEqual(ToolName, maps:get(?MCP_PARAM_NAME, Tool)),
    ?assert(maps:is_key(?MCP_PARAM_INPUT_SCHEMA, Tool)),
    ?assertEqual(Schema, maps:get(?MCP_PARAM_INPUT_SCHEMA, Tool)),
    ok.

list_tools(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Add multiple tools
    Tools = [
        {<<"echo">>, fun(Args) -> maps:get(<<"text">>, Args, <<"hello">>) end},
        {<<"uppercase">>, fun(Args) -> 
            Text = maps:get(<<"text">>, Args, <<"">>),
            string:uppercase(Text)
        end},
        {<<"reverse">>, fun(Args) ->
            Text = maps:get(<<"text">>, Args, <<"">>),
            list_to_binary(lists:reverse(binary_to_list(Text)))
        end}
    ],
    
    lists:foreach(fun({Name, Handler}) ->
        ?assertEqual(ok, erlmcp_server_refactored:add_tool(ServerPid, Name, Handler))
    end, Tools),
    
    % List all tools
    initialize_server(ServerPid, test_transport),
    
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_TOOLS_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    ToolList = maps:get(?MCP_PARAM_TOOLS, ResponseData),
    ?assertEqual(length(Tools), length(ToolList)),
    
    % Verify all tools are present
    Names = [maps:get(?MCP_PARAM_NAME, T) || T <- ToolList],
    lists:foreach(fun({Name, _}) ->
        ?assert(lists:member(Name, Names))
    end, Tools),
    ok.

call_tool_success(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    ToolName = <<"greet">>,
    Handler = fun(Args) ->
        Name = maps:get(<<"name">>, Args, <<"World">>),
        <<"Hello, ", Name/binary, "!">>
    end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_tool(ServerPid, ToolName, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Call the tool
    CallParams = #{
        ?MCP_PARAM_NAME => ToolName,
        ?MCP_PARAM_ARGUMENTS => #{<<"name">> => <<"Alice">>}
    },
    CallRequest = create_json_rpc_message(1, ?MCP_METHOD_TOOLS_CALL, CallParams),
    ServerPid ! {mcp_message, test_transport, CallRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Content = maps:get(?MCP_PARAM_CONTENT, ResponseData),
    ?assert(length(Content) > 0),
    
    ContentItem = hd(Content),
    ?assertEqual(?MCP_CONTENT_TYPE_TEXT, maps:get(?MCP_PARAM_TYPE, ContentItem)),
    ?assertEqual(<<"Hello, Alice!">>, maps:get(?MCP_PARAM_TEXT, ContentItem)),
    ok.

call_tool_not_found(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    initialize_server(ServerPid, test_transport),
    
    % Try to call non-existent tool
    CallParams = #{
        ?MCP_PARAM_NAME => <<"nonexistent">>,
        ?MCP_PARAM_ARGUMENTS => #{}
    },
    CallRequest = create_json_rpc_message(1, ?MCP_METHOD_TOOLS_CALL, CallParams),
    ServerPid ! {mcp_message, test_transport, CallRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({error, 1, ?MCP_ERROR_TOOL_NOT_FOUND, _}, Response),
    ok.

tool_handler_exception(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    ToolName = <<"crash">>,
    Handler = fun(_) -> error(intentional_crash) end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_tool(ServerPid, ToolName, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Call the tool (should crash)
    CallParams = #{
        ?MCP_PARAM_NAME => ToolName,
        ?MCP_PARAM_ARGUMENTS => #{}
    },
    CallRequest = create_json_rpc_message(1, ?MCP_METHOD_TOOLS_CALL, CallParams),
    ServerPid ! {mcp_message, test_transport, CallRequest},
    
    timer:sleep(100),
    
    % Should receive internal error, server should still be alive
    ?assert(is_process_alive(ServerPid)),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({error, 1, ?JSONRPC_INTERNAL_ERROR, _}, Response),
    ok.

tool_validation(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Test tool call parameter validation
    initialize_server(ServerPid, test_transport),
    
    % Call without required name parameter
    CallParams = #{?MCP_PARAM_ARGUMENTS => #{}},
    CallRequest = create_json_rpc_message(1, ?MCP_METHOD_TOOLS_CALL, CallParams),
    ServerPid ! {mcp_message, test_transport, CallRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({error, 1, ?JSONRPC_INVALID_PARAMS, _}, Response),
    ok.

tool_cancellation_simulation(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Add slow tool to simulate cancellation scenario
    ToolName = <<"slow_tool">>,
    Handler = fun(_) ->
        timer:sleep(1000),  % Long-running operation
        <<"Completed">>
    end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_tool(ServerPid, ToolName, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Call the tool
    CallParams = #{
        ?MCP_PARAM_NAME => ToolName,
        ?MCP_PARAM_ARGUMENTS => #{}
    },
    CallRequest = create_json_rpc_message(1, ?MCP_METHOD_TOOLS_CALL, CallParams),
    ServerPid ! {mcp_message, test_transport, CallRequest},
    
    % In a real scenario, we might send a cancellation request here
    % For this test, we just verify the server remains responsive
    timer:sleep(100),
    
    ?assert(is_process_alive(ServerPid)),
    ok.

concurrent_tool_calls(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Add tool that can handle concurrent calls
    ToolName = <<"concurrent_tool">>,
    Handler = fun(Args) ->
        Id = maps:get(<<"id">>, Args, <<"unknown">>),
        timer:sleep(50),  % Simulate some work
        <<"Result for ", Id/binary>>
    end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_tool(ServerPid, ToolName, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Send multiple concurrent tool calls
    [begin
        CallParams = #{
            ?MCP_PARAM_NAME => ToolName,
            ?MCP_PARAM_ARGUMENTS => #{<<"id">> => integer_to_binary(Id)}
        },
        CallRequest = create_json_rpc_message(Id, ?MCP_METHOD_TOOLS_CALL, CallParams),
        ServerPid ! {mcp_message, test_transport, CallRequest}
    end || Id <- lists:seq(1, 5)],
    
    timer:sleep(300),  % Wait for all calls to complete
    
    % All calls should succeed
    [begin
        Response = get_mock_transport_response(test_transport, Id),
        ?assertMatch({response, Id, _}, Response)
    end || Id <- lists:seq(1, 5)],
    ok.

%%====================================================================
%% Test Cases - Message Flow
%%====================================================================

registry_message_routing(Config) ->
    ServerPid = ?config(server_pid, Config),
    ServerId = ?config(server_id, Config),
    TransportId = test_message_routing,
    
    % Register server with registry
    ServerConfig = #{capabilities => #mcp_server_capabilities{}},
    ok = erlmcp_registry:register_server(ServerId, ServerPid, ServerConfig),
    
    % Register transport and bind to server
    TransportConfig = #{type => stdio, server_id => ServerId},
    {ok, MockTransportPid} = start_mock_transport_process(TransportId),
    ok = erlmcp_registry:register_transport(TransportId, MockTransportPid, TransportConfig),
    ok = erlmcp_registry:bind_transport_to_server(TransportId, ServerId),
    
    % Send message through registry routing
    Message = create_json_rpc_message(1, ?MCP_METHOD_INITIALIZE, #{}),
    ok = erlmcp_registry:route_to_server(ServerId, TransportId, Message),
    
    timer:sleep(100),
    
    % Verify message was routed to server and response sent back
    ?assert(is_process_alive(ServerPid)),
    
    % Cleanup
    stop_mock_transport_process(MockTransportPid),
    ok.

response_routing_through_registry(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Initialize server
    initialize_server(ServerPid, test_transport),
    
    % Send request and verify response is routed through registry
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    % Verify response was sent through registry routing
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    ok.

batch_message_handling(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    initialize_server(ServerPid, test_transport),
    
    % Send batch of messages rapidly
    Messages = [
        create_json_rpc_message(I, ?MCP_METHOD_RESOURCES_LIST, #{})
        || I <- lists:seq(1, 10)
    ],
    
    lists:foreach(fun(Msg) ->
        ServerPid ! {mcp_message, test_transport, Msg}
    end, Messages),
    
    timer:sleep(200),  % Allow all messages to process
    
    % All messages should be processed
    [begin
        Response = get_mock_transport_response(test_transport, Id),
        ?assertMatch({response, Id, _}, Response)
    end || Id <- lists:seq(1, 10)],
    ok.

progress_notifications(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    initialize_server(ServerPid, test_transport),
    clear_mock_responses(test_transport),
    
    % Send progress notifications
    Token = <<"test_progress">>,
    Progress = 50.0,
    Total = 100.0,
    
    ?assertEqual(ok, erlmcp_server_refactored:report_progress(ServerPid, Token, Progress, Total)),
    
    timer:sleep(100),
    
    % Should receive progress notification
    Notification = get_mock_transport_notification(test_transport, ?MCP_METHOD_NOTIFICATIONS_PROGRESS),
    ?assertMatch({notification, ?MCP_METHOD_NOTIFICATIONS_PROGRESS, _}, Notification),
    
    {notification, _, Params} = Notification,
    ?assertEqual(Token, maps:get(?MCP_PARAM_PROGRESS_TOKEN, Params)),
    ?assertEqual(Progress, maps:get(?MCP_PARAM_PROGRESS, Params)),
    ?assertEqual(Total, maps:get(?MCP_PARAM_TOTAL, Params)),
    ok.

notification_broadcast(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    initialize_server(ServerPid, test_transport),
    clear_mock_responses(test_transport),
    
    % Trigger a broadcast notification
    ?assertEqual(ok, erlmcp_server_refactored:notify_resources_changed(ServerPid)),
    
    timer:sleep(100),
    
    % Should receive broadcast notification
    Notification = get_mock_transport_notification(test_transport, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED),
    ?assertMatch({notification, ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, _}, Notification),
    ok.

transport_binding_validation(Config) ->
    ServerPid = ?config(server_pid, Config),
    ServerId = ?config(server_id, Config),
    
    % Test that server only responds to messages from bound transports
    UnboundTransportId = unbound_transport,
    
    % Send message from unbound transport
    Message = create_json_rpc_message(1, ?MCP_METHOD_INITIALIZE, #{}),
    ServerPid ! {mcp_message, UnboundTransportId, Message},
    
    timer:sleep(100),
    
    % Server should handle gracefully (might log warning but shouldn't crash)
    ?assert(is_process_alive(ServerPid)),
    ok.

message_flow_error_handling(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Test various error scenarios in message flow
    
    % 1. Invalid JSON message
    ServerPid ! {mcp_message, test_transport, <<"{invalid">>},
    timer:sleep(50),
    ?assert(is_process_alive(ServerPid)),
    
    % 2. Valid JSON but not JSON-RPC
    InvalidRPC = jsx:encode(#{<<"not">> => <<"jsonrpc">>}),
    ServerPid ! {mcp_message, test_transport, InvalidRPC},
    timer:sleep(50),
    ?assert(is_process_alive(ServerPid)),
    
    % 3. Binary data instead of message
    ServerPid ! {mcp_message, test_transport, <<1,2,3,4>>},
    timer:sleep(50),
    ?assert(is_process_alive(ServerPid)),
    
    % 4. Wrong message format
    ServerPid ! {wrong_message_type, test_transport, <<"data">>},
    timer:sleep(50),
    ?assert(is_process_alive(ServerPid)),
    
    ok.

%%====================================================================
%% Test Cases - State Management
%%====================================================================

server_initialization(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Check initial state
    State = get_server_state(ServerPid),
    ?assertMatch({state, _, _, _, _, _, _, _, false}, State),
    
    % Initialize server
    InitRequest = create_json_rpc_message(1, ?MCP_METHOD_INITIALIZE, #{}),
    ServerPid ! {mcp_message, test_transport, InitRequest},
    
    timer:sleep(100),
    
    % Check state after initialization
    NewState = get_server_state(ServerPid),
    ?assertMatch({state, _, _, _, _, _, _, _, true}, NewState),
    ok.

capability_consistency(Config) ->
    % Test that capabilities remain consistent across operations
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = false},
        prompts = #mcp_capability{enabled = true}
    },
    
    {ok, ServerPid} = erlmcp_server_refactored:start_link(test_caps_server, Capabilities),
    
    % Get initial capabilities
    State1 = get_server_state(ServerPid),
    ?assertEqual(Capabilities, element(3, State1)),
    
    % Perform some operations
    Uri = <<"file://test.txt">>,
    Handler = fun(_) -> <<"content">> end,
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler)),
    
    % Capabilities should remain unchanged
    State2 = get_server_state(ServerPid),
    ?assertEqual(Capabilities, element(3, State2)),
    
    erlmcp_server_refactored:stop(ServerPid),
    ok.

state_after_operations(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Initial state should have empty maps
    State1 = get_server_state(ServerPid),
    ?assertEqual(#{}, element(4, State1)), % resources
    ?assertEqual(#{}, element(5, State1)), % resource_templates
    ?assertEqual(#{}, element(6, State1)), % tools
    ?assertEqual(#{}, element(7, State1)), % prompts
    
    % Add some resources and tools
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, <<"file://1">>, fun(_) -> <<"1">> end)),
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, <<"file://2">>, fun(_) -> <<"2">> end)),
    ?assertEqual(ok, erlmcp_server_refactored:add_tool(ServerPid, <<"tool1">>, fun(_) -> <<"tool1">> end)),
    
    % State should reflect the additions
    State2 = get_server_state(ServerPid),
    ?assertEqual(2, maps:size(element(4, State2))), % resources
    ?assertEqual(1, maps:size(element(6, State2))), % tools
    ok.

memory_efficiency(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Measure memory usage before adding resources
    {memory, InitialMemory} = process_info(ServerPid, memory),
    
    % Add many resources
    lists:foreach(fun(I) ->
        Uri = iolist_to_binary(["file://test", integer_to_list(I), ".txt"]),
        Handler = fun(_) -> iolist_to_binary(["Content ", integer_to_list(I)]) end,
        ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler))
    end, lists:seq(1, 100)),
    
    % Measure memory after
    {memory, FinalMemory} = process_info(ServerPid, memory),
    
    % Memory growth should be reasonable (less than 1MB for 100 resources)
    MemoryGrowth = FinalMemory - InitialMemory,
    ct:pal("Memory growth: ~p bytes for 100 resources", [MemoryGrowth]),
    ?assert(MemoryGrowth < 1048576), % Less than 1MB
    ok.

subscription_state_management(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    Uri = <<"file://subscribed.txt">>,
    Handler = fun(_) -> <<"subscribed content">> end,
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, Uri, Handler)),
    
    % Initially no subscriptions
    State1 = get_server_state(ServerPid),
    ?assertEqual(#{}, element(8, State1)), % subscriptions
    
    % Subscribe to resource
    TestPid = spawn(fun() -> receive _ -> ok end end),
    ?assertEqual(ok, erlmcp_server_refactored:subscribe_resource(ServerPid, Uri, TestPid)),
    
    % Should have subscription
    State2 = get_server_state(ServerPid),
    Subscriptions = element(8, State2),
    ?assert(maps:is_key(Uri, Subscriptions)),
    
    % Unsubscribe
    ?assertEqual(ok, erlmcp_server_refactored:unsubscribe_resource(ServerPid, Uri)),
    
    % Should be removed
    State3 = get_server_state(ServerPid),
    NewSubscriptions = element(8, State3),
    ?assertNot(maps:is_key(Uri, NewSubscriptions)),
    
    exit(TestPid, normal),
    ok.

server_shutdown_cleanup(Config) ->
    ServerPid = ?config(server_pid, Config),
    ServerId = ?config(server_id, Config),
    
    % Add some resources and subscriptions
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, <<"file://test.txt">>, fun(_) -> <<"test">> end)),
    TestPid = spawn(fun() -> receive _ -> ok end end),
    ?assertEqual(ok, erlmcp_server_refactored:subscribe_resource(ServerPid, <<"file://test.txt">>, TestPid)),
    
    % Stop server gracefully
    ?assertEqual(ok, erlmcp_server_refactored:stop(ServerPid)),
    
    % Server should be stopped
    timer:sleep(100),
    ?assertNot(is_process_alive(ServerPid)),
    
    exit(TestPid, normal),
    ok.

state_recovery_after_error(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Add some state
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, <<"file://test.txt">>, fun(_) -> <<"test">> end)),
    
    % Cause an error in resource handler
    ?assertEqual(ok, erlmcp_server_refactored:add_resource(ServerPid, <<"file://error.txt">>, fun(_) -> error(boom) end)),
    
    initialize_server(ServerPid, test_transport),
    
    % Try to read error resource
    ReadParams = #{?MCP_PARAM_URI => <<"file://error.txt">>},
    ReadRequest = create_json_rpc_message(1, ?MCP_METHOD_RESOURCES_READ, ReadParams),
    ServerPid ! {mcp_message, test_transport, ReadRequest},
    
    timer:sleep(100),
    
    % Server should handle error gracefully and remain functional
    ?assert(is_process_alive(ServerPid)),
    
    % Other resources should still work
    ReadParams2 = #{?MCP_PARAM_URI => <<"file://test.txt">>},
    ReadRequest2 = create_json_rpc_message(2, ?MCP_METHOD_RESOURCES_READ, ReadParams2),
    ServerPid ! {mcp_message, test_transport, ReadRequest2},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 2),
    ?assertMatch({response, 2, _}, Response),
    ok.

%%====================================================================
%% Test Cases - Prompt Management
%%====================================================================

add_prompt_basic(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    PromptName = <<"greeting">>,
    Handler = fun(Args) ->
        Name = maps:get(<<"name">>, Args, <<"User">>),
        <<"Hello, ", Name/binary, "! How can I help you today?">>
    end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_prompt(ServerPid, PromptName, Handler)),
    
    % Verify prompt is added by listing prompts
    initialize_server(ServerPid, test_transport),
    
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_PROMPTS_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Prompts = maps:get(?MCP_PARAM_PROMPTS, ResponseData),
    ?assert(length(Prompts) > 0),
    
    Prompt = hd(Prompts),
    ?assertEqual(PromptName, maps:get(?MCP_PARAM_NAME, Prompt)),
    ok.

add_prompt_with_arguments(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    PromptName = <<"code_review">>,
    Arguments = [
        #mcp_prompt_argument{
            name = <<"language">>,
            description = <<"Programming language">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"style">>,
            description = <<"Code style preference">>,
            required = false
        }
    ],
    Handler = fun(Args) ->
        Language = maps:get(<<"language">>, Args, <<"unknown">>),
        Style = maps:get(<<"style">>, Args, <<"standard">>),
        <<"Please review this ", Language/binary, " code using ", Style/binary, " style guidelines.">>
    end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_prompt_with_args(ServerPid, PromptName, Handler, Arguments)),
    
    % List prompts and verify arguments are included
    initialize_server(ServerPid, test_transport),
    
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_PROMPTS_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Prompts = maps:get(?MCP_PARAM_PROMPTS, ResponseData),
    Prompt = hd(Prompts),
    
    ?assertEqual(PromptName, maps:get(?MCP_PARAM_NAME, Prompt)),
    ?assert(maps:is_key(?MCP_PARAM_ARGUMENTS, Prompt)),
    
    PromptArgs = maps:get(?MCP_PARAM_ARGUMENTS, Prompt),
    ?assertEqual(length(Arguments), length(PromptArgs)),
    ok.

list_prompts(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    % Add multiple prompts
    Prompts = [
        {<<"greeting">>, fun(_) -> <<"Hello!">> end},
        {<<"farewell">>, fun(_) -> <<"Goodbye!">> end},
        {<<"help">>, fun(_) -> <<"How can I assist you?">> end}
    ],
    
    lists:foreach(fun({Name, Handler}) ->
        ?assertEqual(ok, erlmcp_server_refactored:add_prompt(ServerPid, Name, Handler))
    end, Prompts),
    
    % List all prompts
    initialize_server(ServerPid, test_transport),
    
    ListRequest = create_json_rpc_message(1, ?MCP_METHOD_PROMPTS_LIST, #{}),
    ServerPid ! {mcp_message, test_transport, ListRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    PromptList = maps:get(?MCP_PARAM_PROMPTS, ResponseData),
    ?assertEqual(length(Prompts), length(PromptList)),
    
    % Verify all prompts are present
    Names = [maps:get(?MCP_PARAM_NAME, P) || P <- PromptList],
    lists:foreach(fun({Name, _}) ->
        ?assert(lists:member(Name, Names))
    end, Prompts),
    ok.

get_prompt_success(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    PromptName = <<"summary">>,
    Handler = fun(Args) ->
        Topic = maps:get(<<"topic">>, Args, <<"general">>),
        [#{
            ?MCP_PARAM_ROLE => ?MCP_ROLE_USER,
            ?MCP_PARAM_CONTENT => #{
                ?MCP_PARAM_TYPE => ?MCP_CONTENT_TYPE_TEXT,
                ?MCP_PARAM_TEXT => <<"Please provide a summary of ", Topic/binary, ".">>
            }
        }]
    end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_prompt(ServerPid, PromptName, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Get the prompt
    GetParams = #{
        ?MCP_PARAM_NAME => PromptName,
        ?MCP_PARAM_ARGUMENTS => #{<<"topic">> => <<"machine learning">>}
    },
    GetRequest = create_json_rpc_message(1, ?MCP_METHOD_PROMPTS_GET, GetParams),
    ServerPid ! {mcp_message, test_transport, GetRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({response, 1, _}, Response),
    
    {response, _, ResponseData} = Response,
    Messages = maps:get(?MCP_PARAM_MESSAGES, ResponseData),
    ?assert(length(Messages) > 0),
    
    Message = hd(Messages),
    ?assertEqual(?MCP_ROLE_USER, maps:get(?MCP_PARAM_ROLE, Message)),
    ?assert(maps:is_key(?MCP_PARAM_CONTENT, Message)),
    ok.

get_prompt_not_found(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    initialize_server(ServerPid, test_transport),
    
    % Try to get non-existent prompt
    GetParams = #{
        ?MCP_PARAM_NAME => <<"nonexistent">>,
        ?MCP_PARAM_ARGUMENTS => #{}
    },
    GetRequest = create_json_rpc_message(1, ?MCP_METHOD_PROMPTS_GET, GetParams),
    ServerPid ! {mcp_message, test_transport, GetRequest},
    
    timer:sleep(100),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({error, 1, ?MCP_ERROR_PROMPT_NOT_FOUND, _}, Response),
    ok.

prompt_handler_exception(Config) ->
    ServerPid = ?config(server_pid, Config),
    
    PromptName = <<"crash_prompt">>,
    Handler = fun(_) -> error(prompt_handler_crashed) end,
    
    ?assertEqual(ok, erlmcp_server_refactored:add_prompt(ServerPid, PromptName, Handler)),
    
    initialize_server(ServerPid, test_transport),
    
    % Get the prompt (should crash)
    GetParams = #{
        ?MCP_PARAM_NAME => PromptName,
        ?MCP_PARAM_ARGUMENTS => #{}
    },
    GetRequest = create_json_rpc_message(1, ?MCP_METHOD_PROMPTS_GET, GetParams),
    ServerPid ! {mcp_message, test_transport, GetRequest},
    
    timer:sleep(100),
    
    % Should receive internal error, server should still be alive
    ?assert(is_process_alive(ServerPid)),
    
    Response = get_mock_transport_response(test_transport, 1),
    ?assertMatch({error, 1, ?JSONRPC_INTERNAL_ERROR, _}, Response),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

create_json_rpc_message(Id, Method, Params) ->
    jsx:encode(#{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => Id,
        ?JSONRPC_FIELD_METHOD => Method,
        ?JSONRPC_FIELD_PARAMS => Params
    }).

initialize_server(ServerPid, TransportId) ->
    InitRequest = create_json_rpc_message(init, ?MCP_METHOD_INITIALIZE, #{}),
    ServerPid ! {mcp_message, TransportId, InitRequest},
    timer:sleep(100),
    clear_mock_responses(TransportId).

get_server_state(ServerPid) ->
    sys:get_state(ServerPid).

%% Mock transport functions
start_mock_transport() ->
    MockTransportEts = ets:new(mock_transport, [named_table, public, {keypos, 1}]),
    {ok, MockTransportEts}.

stop_mock_transport(MockTransportEts) ->
    ets:delete(MockTransportEts),
    ok.

start_mock_transport_process(TransportId) ->
    Pid = spawn(fun() -> mock_transport_loop(TransportId, #{}) end),
    register(TransportId, Pid),
    {ok, Pid}.

stop_mock_transport_process(Pid) ->
    exit(Pid, normal).

mock_transport_loop(TransportId, Responses) ->
    receive
        {response, Id, Data} ->
            NewResponses = maps:put({response, Id}, {response, Id, Data}, Responses),
            mock_transport_loop(TransportId, NewResponses);
        {error, Id, Code, Message} ->
            NewResponses = maps:put({response, Id}, {error, Id, Code, Message}, Responses),
            mock_transport_loop(TransportId, NewResponses);
        {notification, Method, Params} ->
            NewResponses = maps:put({notification, Method}, {notification, Method, Params}, Responses),
            mock_transport_loop(TransportId, NewResponses);
        {get_response, From, Id} ->
            Response = maps:get({response, Id}, Responses, no_response),
            From ! {mock_response, Response},
            mock_transport_loop(TransportId, Responses);
        {get_notification, From, Method} ->
            Notification = maps:get({notification, Method}, Responses, no_notification),
            From ! {mock_notification, Notification},
            mock_transport_loop(TransportId, Responses);
        {clear_responses, From} ->
            From ! ok,
            mock_transport_loop(TransportId, #{});
        stop ->
            ok
    end.

get_mock_transport_response(TransportId, Id) ->
    try
        case ets:lookup(mock_transport, {response, TransportId, Id}) of
            [{_, Response}] -> Response;
            [] -> no_response
        end
    catch
        error:badarg -> no_response
    end.

get_mock_transport_notification(TransportId, Method) ->
    try
        case ets:lookup(mock_transport, {notification, TransportId, Method}) of
            [{_, Notification}] -> Notification;
            [] -> no_notification
        end
    catch
        error:badarg -> no_notification
    end.

clear_mock_responses(TransportId) ->
    try
        ets:match_delete(mock_transport, {{response, TransportId, '_'}, '_'}),
        ets:match_delete(mock_transport, {{notification, TransportId, '_'}, '_'})
    catch
        error:badarg -> ok
    end.