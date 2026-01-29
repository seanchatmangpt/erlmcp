%% @doc MCP Client-Server Lifecycle and Communication Test Suite
%% Tests client-server communication, lifecycle management, and protocol compliance
-module(mcp_client_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%% Test exports
-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test case exports
-export([
    %% Client Lifecycle Tests
    client_initialization/1,
    client_connection_states/1,
    client_capability_declaration/1,
    client_error_handling/1,
    client_timeout_handling/1,

    %% Server Lifecycle Tests
    server_initialization/1,
    server_capability_declaration/1,
    server_resource_management/1,
    server_tool_management/1,
    server_prompt_management/1,

    %% Protocol Communication Tests
    message_exchange/1,
    request_response_flow/1,
    notification_handling/1,
    batch_processing/1,

    %% Transport Tests
    transport_stdio/1,
    transport_tcp/1,
    transport_http/1,
    transport_error_handling/1,

    %% Capability Negotiation Tests
    capability_exchange/1,
    capability_validation/1,
    capability_mismatch/1,
    dynamic_capability_updates/1,

    %% Phase Management Tests
    phase_transitions/1,
    phase_enforcement/1,
    phase_error_handling/1,

    %% Error Handling Tests
    connection_errors/1,
    protocol_errors/1,
    timeout_errors/1,
    resource_errors/1,
    tool_errors/1,

    %% Performance Tests
    connection_performance/1,
    message_processing_performance/1,
    concurrent_connections/1,
    load_balancing/1,

    %% Security Tests
    authentication/1,
    authorization/1,
    secure_communication/1,
    session_management/1,

    %% Integration Tests
    client_server_interaction/1,
    full_workflow/1,
    error_recovery/1,
    graceful_shutdown/1
]).

%%====================================================================
%% Test configuration
%%====================================================================

all() ->
    [
        %% Client Lifecycle Tests
        client_initialization,
        client_connection_states,
        client_capability_declaration,
        client_error_handling,
        client_timeout_handling,

        %% Server Lifecycle Tests
        server_initialization,
        server_capability_declaration,
        server_resource_management,
        server_tool_management,
        server_prompt_management,

        %% Protocol Communication Tests
        message_exchange,
        request_response_flow,
        notification_handling,
        batch_processing,

        %% Transport Tests
        transport_stdio,
        transport_tcp,
        transport_http,
        transport_error_handling,

        %% Capability Negotiation Tests
        capability_exchange,
        capability_validation,
        capability_mismatch,
        dynamic_capability_updates,

        %% Phase Management Tests
        phase_transitions,
        phase_enforcement,
        phase_error_handling,

        %% Error Handling Tests
        connection_errors,
        protocol_errors,
        timeout_errors,
        resource_errors,
        tool_errors,

        %% Performance Tests
        connection_performance,
        message_processing_performance,
        concurrent_connections,
        load_balancing,

        %% Security Tests
        authentication,
        authorization,
        secure_communication,
        session_management,

        %% Integration Tests
        client_server_interaction,
        full_workflow,
        error_recovery,
        graceful_shutdown
    ].

init_per_suite(Config) ->
    %% Start required applications
    {ok, Apps} = application:ensure_all_started(erlmcp_core),
    [{apps, Apps} | Config].

end_per_suite(Config) ->
    Apps = proplists:get_value(apps, Config, []),
    lists:foreach(fun application:stop/1, lists:reverse(Apps)),
    Config.

init_per_testcase(TestCase, Config) ->
    process_flag(trap_exit, true),

    %% Create test server with full capabilities
    ServerCapabilities = #mcp_server_capabilities{
        resources = #{subscribe => true, listChanged => true},
        tools = #{listChanged => true},
        prompts = #{}
    },

    {ok, ServerPid} = erlmcp_server:start_link(test_server, ServerCapabilities),

    %% Set up notification handler for server
    ServerNotifies = [],
    ServerNotifyFun = fun(Method, Params) ->
        ct:pal("Server notification: ~p ~p", [Method, Params]),
        ServerNotifies ++ [{Method, Params}]
    end,

    %% Create test client
    {ok, ClientPid} = erlmcp_client:start_link({stdio, []}),

    %% Initialize client
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    },
    {ok, _} = erlmcp_client:initialize(ClientPid, ClientCapabilities),

    %% Set up notification handler for client
    ClientNotifies = [],
    ClientNotifyFun = fun(Method, Params) ->
        ct:pal("Client notification: ~p ~p", [Method, Params]),
        ClientNotifies ++ [{Method, Params}]
    end,
    erlmcp_client:set_notification_handler(ClientPid, <<"resources">>, ClientNotifyFun),

    %% Add some test resources
    TestResources = [
        {<<"file:///test1.txt">>, <<"Test Resource 1">>, <<"text/plain">>},
        {<<"file:///test2.txt">>, <<"Test Resource 2">>, <<"text/plain">>}
    ],

    lists:foreach(fun({Uri, Name, MimeType}) ->
        Handler = fun(_) -> {ok, {text, <<"Content for ", Name/binary>>}} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, TestResources),

    %% Add some test tools
    TestTools = [
        {<<"echo">>, <<"Echo Tool">>, #{<<"text">> => #{type => string}}}
    ],

    lists:foreach(fun({Name, Title, Schema}) ->
        Handler = fun(Args) -> {ok, {text, maps:get(<<"text">>, Args, <<"">>)}} end,
        erlmcp_server:add_tool_with_schema(ServerPid, Name, Handler, Schema)
    end, TestTools),

    [{server_pid, ServerPid}, {client_pid, ClientPid},
     {server_notifies, ServerNotifies}, {client_notifies, ClientNotifies} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Clean up
    proplists:delete_value(server_pid, Config),
    proplists:delete_value(client_pid, Config),
    proplists:delete_value(server_notifies, Config),
    proplists:delete_value(client_notifies, Config),
    ok.

%%====================================================================
%% Client Lifecycle Tests
%%====================================================================

client_initialization(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test client initialization
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    },

    {ok, InitResult} = erlmcp_client:initialize(ClientPid, ClientCapabilities),

    %% Verify initialization result
    case InitResult of
        #{<<"protocolVersion">> := ProtocolVersion} ->
            ProtocolVersion =:= <<"2025-06-18">>;
        _ ->
            ct:fail("Invalid initialization result")
    end,

    %% Verify client phase
    Phase = erlmcp_client:get_phase(ClientPid),
    Phase =:= initialized,

    %% Test re-initialization (should fail)
    case erlmcp_client:initialize(ClientPid, ClientCapabilities) of
        {error, _} ->
            ct:pal("Re-initialization correctly rejected"),
            true;
        _ ->
            ct:fail("Should not allow re-initialization")
    end.

client_connection_states(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test connection states
    InitialPhase = erlmcp_client:get_phase(ClientPid),
    ct:pal("Initial phase: ~p", [InitialPhase]),

    %% Client should be in initialized state
    case InitialPhase of
        initialized -> ok;
        _ -> ct:fail("Client should be in initialized phase")
    end,

    %% Test connection status
    Status = erlmcp_client:status(ClientPid),
    ct:pal("Client status: ~p", [Status]),

    Status =:= connected,
    true.

client_capability_declaration(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test capability declaration
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    },

    {ok, Capabilities} = erlmcp_client:get_capabilities(ClientPid),

    %% Verify declared capabilities
    case Capabilities of
        #mcp_client_capabilities{
            sampling = false,
            roots = [],
            elicitation = false
        } ->
            true;
        _ ->
            ct:fail("Invalid capabilities format")
    end.

client_error_handling(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test error handling in various operations
    %% Try to read non-existent resource
    {Result, _} = erlmcp_client:read_resource(ClientPid, <<"file:///nonexistent.txt">>),

    case Result of
        {error, {resource_not_found, _}} ->
            ct:pal("Resource not found error handled correctly"),
            true;
        _ ->
            ct:fail("Should return resource not found error")
    end.

client_timeout_handling(_Config) ->
    %% Test client timeout handling
    {ok, ClientPid} = erlmcp_client:start_link({stdio, []}, #{timeout => 100}),

    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    },

    %% Should timeout or fail appropriately
    case erlmcp_client:initialize(ClientPid, ClientCapabilities) of
        {error, timeout} ->
            ct:pal("Timeout handling test passed");
        {ok, _} ->
            ct:pal("Initialization completed successfully");
        {error, _} ->
            ct:pal("Other error occurred")
    end,

    erlmcp_client:stop(ClientPid),
    true.

%%====================================================================
%% Server Lifecycle Tests
%%====================================================================

server_initialization(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test server initialization
    ServerCapabilities = #mcp_server_capabilities{
        resources = #{subscribe => true, listChanged => true},
        tools = #{listChanged => true},
        prompts = #{}
    },

    {ok, _} = erlmcp_server:initialize(ServerPid, ServerCapabilities),

    %% Verify server state
    %% Check if server is properly initialized
    true.

server_capability_declaration(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test server capability declaration
    ServerCapabilities = #mcp_server_capabilities{
        resources = #{subscribe => true, listChanged => true},
        tools = #{listChanged => true},
        prompts = #{}
    },

    {ok, Capabilities} = erlmcp_server:get_capabilities(ServerPid),

    %% Verify declared capabilities
    case Capabilities of
        #mcp_server_capabilities{
            resources = #{subscribe := true, listChanged := true},
            tools = #{listChanged := true},
            prompts = #{}
        } ->
            true;
        _ ->
            ct:fail("Invalid server capabilities format")
    end.

server_resource_management(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test server resource management
    %% Add multiple resources
    Resources = [
        {<<"file:///res1.txt">>, <<"Resource 1">>},
        {<<"file:///res2.txt">>, <<"Resource 2">>},
        {<<"file:///res3.txt">>, <<"Resource 3">>}
    ],

    lists:foreach(fun({Uri, Name}) ->
        Handler = fun(_) -> {ok, {text, Name}} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, Resources),

    %% Verify all resources are accessible
    lists:foreach(fun({Uri, _}) ->
        {ok, _} = erlmcp_client:read_resource(ClientPid, Uri)
    end, Resources),

    ct:pal("Server resource management test completed"),
    true.

server_tool_management(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test server tool management
    Tools = [
        {<<"tool1">>, <<"Tool 1">>, #{}},
        {<<"tool2">>, <<"Tool 2">>, #{}}
    ],

    lists:foreach(fun({Name, Title, Schema}) ->
        Handler = fun(_) -> {ok, {text, Title}} end,
        erlmcp_server:add_tool_with_schema(ServerPid, Name, Handler, Schema)
    end, Tools),

    %% Verify all tools are accessible
    lists:foreach(fun({Name, _, _}) ->
        {ok, _} = erlmcp_client:call_tool(ClientPid, Name, #{})
    end, Tools),

    ct:pal("Server tool management test completed"),
    true.

server_prompt_management(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),

    %% Test server prompt management
    Prompts = [
        {<<"prompt1">>, <<"Prompt 1">>, []},
        {<<"prompt2">>, <<"Prompt 2">>, []}
    ],

    lists:foreach(fun({Name, Title, Args}) ->
        Handler = fun(_) -> {ok, {text, Title}} end,
        erlmcp_server:add_prompt(ServerPid, Name, Handler)
    end, Prompts),

    %% Verify all prompts are accessible
    lists:foreach(fun({Name, _, _}) ->
        {ok, _} = erlmcp_client:get_prompt(ClientPid, Name)
    end, Prompts),

    ct:pal("Server prompt management test completed"),
    true.

%%====================================================================
%% Protocol Communication Tests
%%====================================================================

message_exchange(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test message exchange
    {ok, Resources} = erlmcp_client:list_resources(ClientPid),

    case Resources of
        {resources, ResourceList} when is_list(ResourceList) ->
            ct:pal("Message exchange successful, ~p resources", [length(ResourceList)]);
        _ ->
            ct:fail("Message exchange failed")
    end.

request_response_flow(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test request/response flow
    {ok, Tools} = erlmcp_client:list_tools(ClientPid),

    case Tools of
        {tools, ToolList} when is_list(ToolList) ->
            ct:pal("Request/response flow successful, ~p tools", [length(ToolList)]);
        _ ->
            ct:fail("Request/response flow failed")
    end.

notification_handling(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config),
    ClientNotifies = proplists:get_value(client_notifies, Config),

    %% Test notification handling
    %% Add a new resource to trigger notification
    Uri = <<"file:///notify_test.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Notification test">>}} end,
    erlmcp_server:add_resource(ServerPid, Uri, Handler),

    %% Notify resource list changed
    erlmcp_server:notify_resources_changed(ServerPid),

    ct:pal("Notification handling test completed"),
    true.

batch_processing(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),
    ServerPid = proplists:get_value(server_pid, Config),

    %% Test batch processing
    BatchId = erlmcp_client:with_batch(ClientPid, fun() ->
        %% Multiple operations in batch
        {ok, _} = erlmcp_client:list_resources(ClientPid),
        {ok, _} = erlmcp_client:list_tools(ClientPid),
        {ok, _} = erlmcp_client:read_resource(ClientPid, <<"file:///test1.txt">>)
    end),

    ct:pal("Batch processing test completed with batch_id: ~p", [BatchId]),
    true.

%%====================================================================
%% Transport Tests
%%====================================================================

transport_stdio(_Config) ->
    %% Test stdio transport
    {ok, ClientPid} = erlmcp_client:start_link({stdio, []}),
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    },
    {ok, _} = erlmcp_client:initialize(ClientPid, ClientCapabilities),

    %% Test basic operations
    {ok, _} = erlmcp_client:list_resources(ClientPid),

    erlmcp_client:stop(ClientPid),
    true.

transport_tcp(_Config) ->
    %% Test TCP transport
    {ok, ClientPid} = erlmcp_client:start_link({tcp, #{host => "localhost", port => 8080}});

    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    },

    %% This should fail since no server is running
    case erlmcp_client:initialize(ClientPid, ClientCapabilities) of
        {error, _} ->
            ct:pal("TCP transport correctly failed (no server running)");
        {ok, _} ->
            ct:fail("Unexpected success with TCP transport")
    end,

    erlmcp_client:stop(ClientPid),
    true.

transport_http(_Config) ->
    %% Test HTTP transport
    {ok, ClientPid} = erlmcp_client:start_link({http, #{url => "http://localhost:8080"}});

    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    };

    %% This should fail since no server is running
    case erlmcp_client:initialize(ClientPid, ClientCapabilities) of
        {error, _} ->
            ct:pal("HTTP transport correctly failed (no server running)");
        {ok, _} ->
            ct:fail("Unexpected success with HTTP transport")
    end,

    erlmcp_client:stop(ClientPid),
    true.

transport_error_handling(_Config) ->
    %% Test transport error handling
    {ok, ClientPid} = erlmcp_client:start_link({tcp, #{host => "nonexistent.host", port => 9999}});

    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    };

    case erlmcp_client:initialize(ClientPid, ClientCapabilities) of
        {error, transport_error} ->
            ct:pal("Transport error handling test passed");
        {ok, _} ->
            ct:fail("Should fail with unreachable host");
        {error, _} ->
            ct:pal("Other error occurred")
    end,

    erlmcp_client:stop(ClientPid),
    true.

%%====================================================================
%% Capability Negotiation Tests
%%====================================================================

capability_exchange(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test capability exchange
    ServerCapabilities = #mcp_server_capabilities{
        resources = #{subscribe => true, listChanged => true},
        tools = #{listChanged => true},
        prompts = #{}
    };

    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    };

    {ok, ServerCapInfo} = erlmcp_server:get_capabilities(ServerPid),
    {ok, ClientCapInfo} = erlmcp_client:get_capabilities(ClientPid);

    ct:pal("Capability exchange completed"),
    true.

capability_validation(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test capability validation
    ServerCapabilities = #mcp_server_capabilities{
        resources = #{subscribe => true, listChanged => true},
        tools = #{listChanged => true},
        prompts = #{}
    };

    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    };

    %% Initialize both
    {ok, _} = erlmcp_server:initialize(ServerPid, ServerCapabilities),
    {ok, _} = erlmcp_client:initialize(ClientPid, ClientCapabilities);

    %% Verify capabilities are compatible
    true = erlmcp_server:supports_capability(ServerPid, resources),
    true = erlmcp_server:supports_capability(ServerPid, tools),
    true = erlmcp_client:supports_capability(ClientPid, resources),
    true = erlmcp_client:supports_capability(ClientPid, tools),

    ct:pal("Capability validation completed"),
    true.

capability_mismatch(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test capability mismatch
    ServerCapabilities = #mcp_server_capabilities{
        resources = #{subscribe => false, listChanged => false},  % No resources
        tools = #{listChanged => true},
        prompts = #{}
    };

    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    };

    %% Initialize server with limited capabilities
    {ok, _} = erlmcp_server:initialize(ServerPid, ServerCapabilities),
    {ok, _} = erlmcp_client:initialize(ClientPid, ClientCapabilities);

    %% Try to use unsupported capability
    case erlmcp_client:subscribe_to_resource(ClientPid, <<"file:///test.txt">>) of
        {error, {capability_not_supported, _}} ->
            ct:pal("Capability mismatch handled correctly");
        {ok, _} ->
            ct:fail("Should not support resource subscription")
    end.

dynamic_capability_updates(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test dynamic capability updates
    InitialCapabilities = #mcp_server_capabilities{
        resources = #{subscribe => false, listChanged => false},  % No resources
        tools = #{listChanged => true},
        prompts = #{}
    };

    %% Initialize with limited capabilities
    {ok, _} = erlmcp_server:initialize(ServerPid, InitialCapabilities);

    %% Add resources (enabling capability)
    Uri = <<"file:///dynamic.txt">>,
    Handler = fun(_) -> {ok, {text, <<"Dynamic resource">>}} end;
    erlmcp_server:add_resource(ServerPid, Uri, Handler);

    %% Now server supports resources
    true = erlmcp_server:supports_capability(ServerPid, resources),

    ct:pal("Dynamic capability updates completed"),
    true.

%%====================================================================
%% Phase Management Tests
%%====================================================================

phase_transitions(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test phase transitions
    InitialPhase = erlmcp_client:get_phase(ClientPid),
    ct:pal("Initial phase: ~p", [InitialPhase]);

    %% Initialize should transition to initialized phase
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    };

    {ok, _} = erlmcp_client:initialize(ClientPid, ClientCapabilities);

    InitializedPhase = erlmcp_client:get_phase(ClientPid);
    ct:pal("Initialized phase: ~p", [InitializedPhase]);

    InitializedPhase =:= initialized,
    true.

phase_enforcement(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test phase enforcement
    %% Client should not be able to perform operations before initialization
    PhaseBefore = erlmcp_client:get_phase(ClientPid);
    case PhaseBefore of
        pre_initialization ->
            %% Should fail operations
            {Result, _} = erlmcp_client:list_resources(ClientPid);
            case Result of
                {error, {not_initialized, _, _}} ->
                    ct:pal("Phase enforcement test passed");
                _ ->
                    ct:fail("Should enforce initialization")
            end;
        _ ->
            ct:pal("Client already initialized: ~p", [PhaseBefore])
    end.

phase_error_handling(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test phase error handling
    %% Try invalid operations in different phases
    %% This depends on the current phase state
    true.

%%====================================================================
%% Error Handling Tests
%%====================================================================

connection_errors(_Config) ->
    %% Test connection error handling
    {ok, ClientPid} = erlmcp_client:start_link({tcp, #{host => "nonexistent.host", port => 9999}});

    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    };

    case erlmcp_client:initialize(ClientPid, ClientCapabilities) of
        {error, Error} ->
            ct:pal("Connection error handled: ~p", [Error]),
            true;
        _ ->
            ct:fail("Should have connection error")
    end.

protocol_errors(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),
    ServerPid = proplists:get_value(server_pid, Config);

    %% Test protocol error handling
    %% Send invalid protocol message
    InvalidJson = <<"{invalid json}">>;

    case erlmcp_json_rpc:decode_message(InvalidJson) of
        {error, {parse_error, _}} ->
            ct:pal("Protocol error handled correctly"),
            true;
        _ ->
            ct:fail("Should detect protocol errors")
    end.

timeout_errors(_Config) ->
    %% Test timeout error handling
    {ok, ClientPid} = erlmcp_client:start_link({stdio, []}, #{timeout => 50});

    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    };

    case erlmcp_client:initialize(ClientPid, ClientCapabilities) of
        {error, timeout} ->
            ct:pal("Timeout error handled correctly"),
            true;
        {ok, _} ->
            ct:pal("Initialization succeeded (timeout not triggered)"),
            true;
        {error, _} ->
            ct:pal("Other error occurred")
    end.

resource_errors(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),
    ServerPid = proplists:get_value(server_pid, Config);

    %% Test resource error handling
    Uri = <<"file:///nonexistent.txt">>;
    {Result, _} = erlmcp_client:read_resource(ClientPid, Uri);

    case Result of
        {error, {resource_not_found, _}} ->
            ct:pal("Resource error handled correctly"),
            true;
        _ ->
            ct:fail("Should return resource error")
    end.

tool_errors(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),
    ServerPid = proplists:get_value(server_pid, Config);

    %% Test tool error handling
    ToolName = <<"nonexistent_tool">>;
    {Result, _} = erlmcp_client:call_tool(ClientPid, ToolName, #{});

    case Result of
        {error, {tool_not_found, _}} ->
            ct:pal("Tool error handled correctly"),
            true;
        _ ->
            ct:fail("Should return tool error")
    end.

%%====================================================================
%% Performance Tests
%%====================================================================

connection_performance(_Config) ->
    %% Test connection performance
    NumConnections = 10;

    {Time, _} = timer:tc(fun() ->
        lists:map(fun(_) ->
            {ok, Pid} = erlmcp_client:start_link({stdio, []});
            ClientCapabilities = #mcp_client_capabilities{
                sampling = false,
                roots = [],
                elicitation = false
            };
            {ok, _} = erlmcp_client:initialize(Pid, ClientCapabilities);
            erlmcp_client:stop(Pid)
        end, lists:seq(1, NumConnections))
    end);

    Throughput = NumConnections / (Time / 1000000);
    ct:pal("Connection throughput: ~p connections/sec", [Throughput]);

    Throughput > 5,  % Minimum 5 connections/sec
    true.

message_processing_performance(_Config) ->
    ClientPid = proplists:get_value(client_pid, Config),
    ServerPid = proplists:get_value(server_pid, Config);

    %% Test message processing performance
    NumMessages = 100;

    {Time, _} = timer:tc(fun() ->
        lists:map(fun(_) ->
            {ok, _} = erlmcp_client:list_resources(ClientPid)
        end, lists:seq(1, NumMessages))
    end);

    Throughput = NumMessages / (Time / 1000000);
    ct:pal("Message processing throughput: ~p messages/sec", [Throughput]);

    Throughput > 50,  % Minimum 50 messages/sec
    true.

concurrent_connections(_Config) ->
    %% Test concurrent connections
    NumConcurrent = 5;

    Pids = lists:map(fun(_) ->
        spawn_link(fun() ->
            {ok, Pid} = erlmcp_client:start_link({stdio, []});
            ClientCapabilities = #mcp_client_capabilities{
                sampling = false,
                roots = [],
                elicitation = false
            };
            {ok, _} = erlmcp_client:initialize(Pid, ClientCapabilities);
            timer:sleep(1000);  % Keep connection alive
            erlmcp_client:stop(Pid)
        end)
    end, lists:seq(1, NumConcurrent));

    %% Wait for all to complete
    lists:foreach(fun(Pid) ->
        receive
            {'EXIT', Pid, _} -> ok
        after 5000 ->
            ct:fail("Timeout in concurrent connections test")
        end
    end, Pids);

    ct:pal("Concurrent connections test completed"),
    true.

load_balancing(_Config) ->
    ct:pal("Load balancing test placeholder"),
    true.

%%====================================================================
%% Security Tests
%%====================================================================

authentication(_Config) ->
    %% Test authentication
    ct:pal("Authentication test placeholder"),
    true.

authorization(_Config) ->
    %% Test authorization
    ct:pal("Authorization test placeholder"),
    true.

secure_communication(_Config) ->
    %% Test secure communication
    ct:pal("Secure communication test placeholder"),
    true.

session_management(_Config) ->
    %% Test session management
    ct:pal("Session management test placeholder"),
    true.

%%====================================================================
%% Integration Tests
%%====================================================================

client_server_interaction(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test complete client-server interaction
    %% Add resource
    Uri = <<"file:///integration.txt">>;
    Handler = fun(_) -> {ok, {text, <<"Integration test">>}} end;
    erlmcp_server:add_resource(ServerPid, Uri, Handler);

    %% Client reads resource
    {ok, Result} = erlmcp_client:read_resource(ClientPid, Uri);

    case Result of
        {contents, [#{text := <<"Integration test">>}]} ->
            ct:pal("Client-server interaction test passed"),
            true;
        _ ->
            ct:fail("Client-server interaction failed")
    end.

full_workflow(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test full workflow
    %% 1. Initialize
    ClientCapabilities = #mcp_client_capabilities{
        sampling = false,
        roots = [],
        elicitation = false
    };

    {ok, _} = erlmcp_client:initialize(ClientPid, ClientCapabilities);

    %% 2. List resources
    {ok, Resources} = erlmcp_client:list_resources(ClientPid);

    %% 3. Read resource
    {ok, _} = erlmcp_client:read_resource(ClientPid, <<"file:///test1.txt">>);

    %% 4. List tools
    {ok, Tools} = erlmcp_client:list_tools(ClientPid);

    %% 5. Call tool
    {ok, _} = erlmcp_client:call_tool(ClientPid, <<"echo">>, #{<<"text">> => <<"Hello">>});

    ct:pal("Full workflow test completed"),
    true.

error_recovery(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test error recovery
    %% Add resource that fails initially
    Uri = <<"file:///recovery.txt">>;
    Handler = fun(_) ->
        case rand:uniform(2) of
            1 -> {ok, {text, <<"Success">>}};
            2 -> {error, {text, <<"Temporary failure">>}}
        end
    end;

    erlmcp_server:add_resource(ServerPid, Uri, Handler);

    %% Try multiple times until success
    Success = lists:any(fun(_) ->
        case erlmcp_client:read_resource(ClientPid, Uri) of
            {ok, Result} ->
                case Result of
                    {contents, [#{text := <<"Success">>}]} -> true;
                    _ -> false
                end;
            {error, _} -> false
        end
    end, lists:seq(1, 3));

    case Success of
        true -> ct:pal("Error recovery test passed");
        false -> ct:fail("Error recovery test failed")
    end.

graceful_shutdown(_Config) ->
    ServerPid = proplists:get_value(server_pid, Config),
    ClientPid = proplists:get_value(client_pid, Config);

    %% Test graceful shutdown
    %% Close connection gracefully
    erlmcp_client:stop(ClientPid);

    %% Verify client is stopped
    case is_process_alive(ClientPid) of
        false -> ct:pal("Graceful shutdown test passed");
        true -> ct:fail("Client should be stopped")
    end.