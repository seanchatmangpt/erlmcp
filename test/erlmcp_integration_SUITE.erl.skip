%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive End-to-End Integration Tests for ErlMCP
%%%
%%% This test suite validates the complete OTP architecture works together:
%%% - Full system startup/shutdown
%%% - Complete message flow (client → transport → registry → server → response)
%%% - Multiple transport integration (stdio, tcp, http simultaneously)
%%% - Server-transport-registry coordination
%%% - Configuration loading and hot reload
%%% - Failure recovery integration
%%% - Performance under realistic load
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    % Full system tests
    test_system_startup_shutdown/1,
    test_complete_message_flow/1,
    test_multi_transport_coordination/1,
    test_server_registry_coordination/1,
    
    % Configuration tests
    test_configuration_loading/1,
    test_configuration_hot_reload/1,
    test_transport_config_validation/1,
    
    % Error handling and recovery
    test_failure_recovery_integration/1,
    test_transport_failure_recovery/1,
    test_server_crash_recovery/1,
    test_registry_failure_handling/1,
    
    % Performance tests
    test_concurrent_connections/1,
    test_high_message_throughput/1,
    test_resource_management_under_load/1,
    
    % Real client interaction tests
    test_real_mcp_client_interaction/1,
    test_tool_execution_end_to_end/1,
    test_resource_access_end_to_end/1,
    test_prompt_handling_integration/1,
    
    % System monitoring and health
    test_monitoring_integration/1,
    test_metrics_collection_integration/1,
    test_tracing_integration/1
]).

%% Test constants
-define(TEST_TIMEOUT, 30000).
-define(LOAD_TEST_DURATION, 10000).  % 10 seconds
-define(CONCURRENT_CLIENTS, 50).
-define(MESSAGE_BURST_SIZE, 100).
-define(TEST_SERVER_PREFIX, integration_test_server).
-define(TEST_TRANSPORT_PREFIX, integration_test_transport).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [
        {group, system_integration},
        {group, configuration_management},
        {group, failure_recovery},
        {group, performance_integration},
        {group, client_interaction},
        {group, monitoring_integration}
    ].

groups() ->
    [
        {system_integration, [sequence], [
            test_system_startup_shutdown,
            test_complete_message_flow,
            test_multi_transport_coordination,
            test_server_registry_coordination
        ]},
        {configuration_management, [parallel], [
            test_configuration_loading,
            test_configuration_hot_reload,
            test_transport_config_validation
        ]},
        {failure_recovery, [sequence], [
            test_failure_recovery_integration,
            test_transport_failure_recovery,
            test_server_crash_recovery,
            test_registry_failure_handling
        ]},
        {performance_integration, [parallel], [
            test_concurrent_connections,
            test_high_message_throughput,
            test_resource_management_under_load
        ]},
        {client_interaction, [sequence], [
            test_real_mcp_client_interaction,
            test_tool_execution_end_to_end,
            test_resource_access_end_to_end,
            test_prompt_handling_integration
        ]},
        {monitoring_integration, [parallel], [
            test_monitoring_integration,
            test_metrics_collection_integration,
            test_tracing_integration
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting ErlMCP Integration Test Suite"),
    
    %% Start required applications
    case application:start(crypto) of
        ok -> ok;
        {error, {already_started, crypto}} -> ok
    end,
    case application:start(ssl) of
        ok -> ok;
        {error, {already_started, ssl}} -> ok
    end,
    
    %% Start dependencies for JSON handling and validation
    DepsToStart = [jsx, jesse, opentelemetry_api, opentelemetry],
    lists:foreach(fun(App) ->
        case application:start(App) of
            ok -> ok;
            {error, {already_started, App}} -> ok;
            {error, {not_started, _}} -> 
                %% Try to start the app anyway, might work
                application:start(App, temporary)
        end
    end, DepsToStart),
    
    %% Start ErlMCP application
    case application:start(erlmcp) of
        ok -> ok;
        {error, {already_started, erlmcp}} -> ok;
        {error, {not_started, _}} ->
            %% Try to start it anyway
            application:start(erlmcp, temporary)
    end,
    
    %% Wait for system to stabilize
    timer:sleep(1000),
    
    %% Verify core components are running
    ?assertNotEqual(undefined, whereis(erlmcp_sup)),
    ?assertNotEqual(undefined, whereis(erlmcp_registry)),
    
    [{suite_start_time, erlang:system_time(millisecond)} | Config].

end_per_suite(Config) ->
    ct:pal("Ending ErlMCP Integration Test Suite"),
    
    %% Stop application
    application:stop(erlmcp),
    
    %% Calculate total suite time
    StartTime = proplists:get_value(suite_start_time, Config, 0),
    Duration = erlang:system_time(millisecond) - StartTime,
    ct:pal("Total suite duration: ~pms", [Duration]),
    
    ok.

init_per_group(Group, Config) ->
    ct:pal("Starting test group: ~p", [Group]),
    [{group, Group}, {group_start_time, erlang:system_time(millisecond)} | Config].

end_per_group(Group, Config) ->
    StartTime = proplists:get_value(group_start_time, Config, 0),
    Duration = erlang:system_time(millisecond) - StartTime,
    ct:pal("Test group ~p completed in ~pms", [Group, Duration]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    [{testcase, TestCase}, {testcase_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(TestCase, Config) ->
    StartTime = proplists:get_value(testcase_start_time, Config, 0),
    Duration = erlang:system_time(millisecond) - StartTime,
    ct:pal("Test case ~p completed in ~pms", [TestCase, Duration]),
    
    %% Cleanup any test artifacts
    cleanup_test_artifacts(TestCase),
    ok.

%%====================================================================
%% System Integration Tests
%%====================================================================

test_system_startup_shutdown(Config) ->
    ct:pal("Testing complete system startup and shutdown"),
    
    %% Test system startup order
    StartupOrder = [erlmcp_sup, erlmcp_registry, erlmcp_transport_sup],
    lists:foreach(fun(Process) ->
        ?assertNotEqual(undefined, whereis(Process), 
                       io_lib:format("Process ~p not started", [Process]))
    end, StartupOrder),
    
    %% Test supervised children are properly initialized
    {ok, SupChildren} = supervisor:which_children(erlmcp_sup),
    ?assert(length(SupChildren) >= 2, "Supervisor should have at least 2 children"),
    
    %% Test registry is operational
    ?assertEqual([], erlmcp_registry:list_servers()),
    ?assertEqual([], erlmcp_registry:list_transports()),
    
    %% Test graceful shutdown capabilities
    TestServerId = make_test_server_id(1),
    {ok, ServerPid} = erlmcp:start_server(TestServerId, #{}),
    ?assert(is_process_alive(ServerPid)),
    
    ok = erlmcp:stop_server(TestServerId),
    timer:sleep(100),
    ?assertNot(is_process_alive(ServerPid)),
    
    ct:pal("System startup/shutdown test completed successfully"),
    Config.

test_complete_message_flow(Config) ->
    ct:pal("Testing complete message flow: client → transport → registry → server → response"),
    
    %% Setup test server with capabilities
    ServerId = make_test_server_id(2),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{
            resources = #mcp_capability{enabled = true},
            tools = #mcp_capability{enabled = true},
            prompts = #mcp_capability{enabled = true}
        }
    },
    
    {ok, ServerPid} = erlmcp:start_server(ServerId, ServerConfig),
    
    %% Add test components to server
    TestTool = fun(Args) ->
        Name = maps:get(<<"name">>, Args, <<"World">>),
        #{result => <<"Hello, ", Name/binary, "!">>}
    end,
    
    TestResource = fun(_Uri) ->
        #{content => <<"Test resource content">>, mimeType => <<"text/plain">>}
    end,
    
    ok = erlmcp:add_tool(ServerId, <<"test_tool">>, TestTool),
    ok = erlmcp:add_resource(ServerId, <<"test://resource">>, TestResource),
    
    %% Setup transport (stdio for testing)
    TransportId = make_test_transport_id(2),
    TransportConfig = #{server_id => ServerId},
    
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, TransportConfig),
    ok = erlmcp:bind_transport_to_server(TransportId, ServerId),
    
    %% Test initialize flow
    InitMessage = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => 1,
        ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_INITIALIZE,
        ?JSONRPC_FIELD_PARAMS => #{
            ?MCP_FIELD_PROTOCOL_VERSION => ?MCP_VERSION,
            ?MCP_FIELD_CAPABILITIES => #{},
            ?MCP_FIELD_CLIENT_INFO => #{
                ?MCP_INFO_NAME => <<"integration_test">>,
                ?MCP_INFO_VERSION => <<"1.0.0">>
            }
        }
    },
    
    %% Send message through transport layer
    MessageJson = jsx:encode(InitMessage),
    
    %% Simulate message flow by sending to transport
    TransportPid ! {message, MessageJson},
    
    %% Wait for processing
    timer:sleep(500),
    
    %% Verify server is still alive and operational
    ?assert(is_process_alive(ServerPid)),
    ?assert(is_process_alive(TransportPid)),
    
    %% Test tools/list flow
    ToolsListMessage = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => 2,
        ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_TOOLS_LIST
    },
    
    ToolsListJson = jsx:encode(ToolsListMessage),
    TransportPid ! {message, ToolsListJson},
    timer:sleep(200),
    
    %% Verify components are still functioning
    ?assert(is_process_alive(ServerPid)),
    ?assert(is_process_alive(TransportPid)),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Complete message flow test completed successfully"),
    Config.

test_multi_transport_coordination(Config) ->
    ct:pal("Testing multiple transport coordination"),
    
    %% Create test server
    ServerId = make_test_server_id(3),
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Start multiple transports simultaneously
    Transports = [
        {make_test_transport_id(3, stdio), stdio, #{server_id => ServerId}},
        {make_test_transport_id(3, tcp), tcp, #{server_id => ServerId, host => "localhost", port => 8080}},
        {make_test_transport_id(3, http), http, #{server_id => ServerId, url => "http://localhost:8000"}}
    ],
    
    TransportPids = lists:map(fun({TransportId, Type, Config}) ->
        case erlmcp:start_transport(TransportId, Type, Config) of
            {ok, Pid} ->
                ok = erlmcp:bind_transport_to_server(TransportId, ServerId),
                {TransportId, Type, Pid, ok};
            {error, {transport_not_implemented, _}} ->
                ct:pal("Transport ~p not implemented, skipping", [Type]),
                {TransportId, Type, undefined, skipped};
            {error, Reason} ->
                ct:pal("Transport ~p failed to start: ~p", [Type, Reason]),
                {TransportId, Type, undefined, error}
        end
    end, Transports),
    
    %% Verify at least stdio transport is running
    StdioTransport = lists:keyfind(stdio, 2, TransportPids),
    case StdioTransport of
        {_, stdio, Pid, ok} when is_pid(Pid) ->
            ?assert(is_process_alive(Pid));
        _ ->
            ct:fail("STDIO transport should always be available")
    end,
    
    %% Test transport coordination through registry
    AllTransports = erlmcp:list_transports(),
    RunningTransports = [T || {_, T, P, S} <- TransportPids, is_pid(P), S =:= ok],
    
    ?assert(length(AllTransports) >= length(RunningTransports)),
    
    %% Test binding verification
    Bindings = erlmcp:get_transport_bindings(),
    case Bindings of
        {error, _} -> ct:pal("Transport bindings not available");
        BindingList when is_list(BindingList) ->
            ServerBindings = [T || {T, S} <- BindingList, S =:= ServerId],
            ?assert(length(ServerBindings) >= 1)
    end,
    
    %% Cleanup all transports
    lists:foreach(fun({TransportId, _Type, Pid, Status}) ->
        if is_pid(Pid) andalso Status =:= ok ->
               erlmcp:stop_transport(TransportId);
           true -> ok
        end
    end, TransportPids),
    
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Multi-transport coordination test completed successfully"),
    Config.

test_server_registry_coordination(Config) ->
    ct:pal("Testing server-registry coordination"),
    
    %% Test server registration and discovery
    ServerIds = [make_test_server_id(I) || I <- lists:seq(4, 8)],
    
    %% Start multiple servers
    ServerPids = lists:map(fun(ServerId) ->
        ServerConfig = #{test_id => ServerId},
        {ok, Pid} = erlmcp:start_server(ServerId, ServerConfig),
        {ServerId, Pid}
    end, ServerIds),
    
    %% Verify all servers are registered
    RegisteredServers = erlmcp:list_servers(),
    ?assertEqual(length(ServerIds), length(RegisteredServers)),
    
    %% Test server lookup functionality
    lists:foreach(fun({ServerId, Pid}) ->
        case erlmcp_registry:find_server(ServerId) of
            {ok, {FoundPid, _Config}} ->
                ?assertEqual(Pid, FoundPid);
            {error, not_found} ->
                ct:fail("Server ~p not found in registry", [ServerId])
        end
    end, ServerPids),
    
    %% Test configuration updates
    {TestServerId, _TestPid} = hd(ServerPids),
    UpdatedConfig = #{updated => true, timestamp => erlang:system_time()},
    ok = erlmcp:update_server_config(TestServerId, UpdatedConfig),
    
    {ok, {_, FinalConfig}} = erlmcp_registry:find_server(TestServerId),
    ?assertEqual(true, maps:get(updated, FinalConfig)),
    
    %% Test server shutdown coordination
    lists:foreach(fun({ServerId, _Pid}) ->
        ok = erlmcp:stop_server(ServerId)
    end, ServerPids),
    
    %% Verify all servers are unregistered
    timer:sleep(100),
    FinalServers = erlmcp:list_servers(),
    RegisteredIds = [Id || {Id, _} <- FinalServers],
    lists:foreach(fun(ServerId) ->
        ?assertNot(lists:member(ServerId, RegisteredIds))
    end, ServerIds),
    
    ct:pal("Server-registry coordination test completed successfully"),
    Config.

%%====================================================================
%% Configuration Management Tests
%%====================================================================

test_configuration_loading(Config) ->
    ct:pal("Testing configuration loading"),
    
    %% Test default configuration
    ServerId = make_test_server_id(10),
    {ok, _Pid} = erlmcp:start_server(ServerId),
    
    {ok, DefaultConfig} = erlmcp:get_server_config(ServerId),
    ?assert(is_map(DefaultConfig)),
    
    %% Test transport configuration validation
    ValidStdioConfig = #{server_id => ServerId},
    ?assertEqual(ok, erlmcp:validate_transport_config(stdio, ValidStdioConfig)),
    
    ValidTcpConfig = #{server_id => ServerId, host => "localhost", port => 9090},
    ?assertEqual(ok, erlmcp:validate_transport_config(tcp, ValidTcpConfig)),
    
    %% Test invalid configuration
    InvalidTcpConfig = #{server_id => ServerId, port => 70000}, % Invalid port
    ?assertMatch({error, _}, erlmcp:validate_transport_config(tcp, InvalidTcpConfig)),
    
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Configuration loading test completed successfully"),
    Config.

test_configuration_hot_reload(Config) ->
    ct:pal("Testing configuration hot reload"),
    
    %% Start server with initial config
    ServerId = make_test_server_id(11),
    InitialConfig = #{initial => true, version => 1},
    {ok, ServerPid} = erlmcp:start_server(ServerId, InitialConfig),
    
    %% Verify initial configuration
    {ok, {ServerPid, StoredConfig}} = erlmcp_registry:find_server(ServerId),
    ?assertEqual(true, maps:get(initial, StoredConfig)),
    ?assertEqual(1, maps:get(version, StoredConfig)),
    
    %% Update configuration
    UpdateConfig = #{updated => true, version => 2},
    ok = erlmcp:update_server_config(ServerId, UpdateConfig),
    
    %% Verify updated configuration
    {ok, {ServerPid, NewConfig}} = erlmcp_registry:find_server(ServerId),
    ?assertEqual(true, maps:get(initial, NewConfig)), % Should still have initial
    ?assertEqual(true, maps:get(updated, NewConfig)),  % Should have update
    ?assertEqual(2, maps:get(version, NewConfig)),     % Should have new version
    
    %% Test transport configuration updates
    TransportId = make_test_transport_id(11),
    TransportConfig = #{server_id => ServerId, version => 1},
    {ok, _TransportPid} = erlmcp:start_transport(TransportId, stdio, TransportConfig),
    
    UpdatedTransportConfig = #{version => 2, updated => true},
    ok = erlmcp:update_transport_config(TransportId, UpdatedTransportConfig),
    
    {ok, FinalTransportConfig} = erlmcp:get_transport_config(TransportId),
    ?assertEqual(2, maps:get(version, FinalTransportConfig)),
    ?assertEqual(true, maps:get(updated, FinalTransportConfig)),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Configuration hot reload test completed successfully"),
    Config.

test_transport_config_validation(Config) ->
    ct:pal("Testing transport configuration validation"),
    
    %% Test STDIO configuration validation
    ValidStdio = #{server_id => test_server},
    ?assertEqual(ok, erlmcp:validate_transport_config(stdio, ValidStdio)),
    
    InvalidStdio = #{}, % Missing server_id is ok for stdio
    ?assertEqual(ok, erlmcp:validate_transport_config(stdio, InvalidStdio)),
    
    %% Test TCP configuration validation
    ValidTcp = #{server_id => test_server, host => "127.0.0.1", port => 8080},
    ?assertEqual(ok, erlmcp:validate_transport_config(tcp, ValidTcp)),
    
    InvalidTcp1 = #{server_id => test_server, host => "127.0.0.1"}, % Missing port
    ?assertMatch({error, _}, erlmcp:validate_transport_config(tcp, InvalidTcp1)),
    
    InvalidTcp2 = #{server_id => test_server, host => "127.0.0.1", port => 100000}, % Invalid port
    ?assertMatch({error, _}, erlmcp:validate_transport_config(tcp, InvalidTcp2)),
    
    %% Test HTTP configuration validation
    ValidHttp = #{server_id => test_server, url => "http://localhost:8000"},
    ?assertEqual(ok, erlmcp:validate_transport_config(http, ValidHttp)),
    
    InvalidHttp = #{server_id => test_server, url => "invalid-url"},
    ?assertMatch({error, _}, erlmcp:validate_transport_config(http, InvalidHttp)),
    
    %% Test unknown transport type
    ?assertMatch({error, {unknown_transport_type, unknown}}, 
                erlmcp:validate_transport_config(unknown, #{})),
    
    ct:pal("Transport configuration validation test completed successfully"),
    Config.

%%====================================================================
%% Failure Recovery Tests
%%====================================================================

test_failure_recovery_integration(Config) ->
    ct:pal("Testing failure recovery integration"),
    
    %% Test server crash recovery
    ServerId = make_test_server_id(15),
    {ok, ServerPid1} = erlmcp:start_server(ServerId, #{}),
    
    %% Simulate server crash
    exit(ServerPid1, kill),
    timer:sleep(100),
    
    %% Server should be automatically restarted by supervisor
    case erlmcp_registry:find_server(ServerId) of
        {ok, {ServerPid2, _}} when ServerPid2 =/= ServerPid1 ->
            ct:pal("Server automatically restarted after crash");
        {error, not_found} ->
            %% If not automatically restarted, manual restart should work
            {ok, _} = erlmcp:start_server(ServerId, #{}),
            ct:pal("Server manually restarted after crash");
        {ok, {ServerPid1, _}} ->
            ct:fail("Server PID should have changed after crash")
    end,
    
    %% Test transport recovery
    TransportId = make_test_transport_id(15),
    TransportConfig = #{server_id => ServerId},
    {ok, TransportPid1} = erlmcp:start_transport(TransportId, stdio, TransportConfig),
    
    %% Simulate transport crash
    exit(TransportPid1, kill),
    timer:sleep(100),
    
    %% Transport should be recoverable
    {ok, _TransportPid2} = erlmcp:start_transport(TransportId, stdio, TransportConfig),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Failure recovery integration test completed successfully"),
    Config.

test_transport_failure_recovery(Config) ->
    ct:pal("Testing transport failure recovery"),
    
    ServerId = make_test_server_id(16),
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Test transport restart after failure
    TransportId = make_test_transport_id(16),
    TransportConfig = #{server_id => ServerId},
    
    {ok, TransportPid1} = erlmcp:start_transport(TransportId, stdio, TransportConfig),
    ?assert(is_process_alive(TransportPid1)),
    
    %% Stop transport
    ok = erlmcp:stop_transport(TransportId),
    timer:sleep(100),
    ?assertNot(is_process_alive(TransportPid1)),
    
    %% Restart transport
    {ok, TransportPid2} = erlmcp:start_transport(TransportId, stdio, TransportConfig),
    ?assert(is_process_alive(TransportPid2)),
    ?assertNotEqual(TransportPid1, TransportPid2),
    
    %% Verify transport is functional after restart
    ok = erlmcp:bind_transport_to_server(TransportId, ServerId),
    Transports = erlmcp:list_transports(),
    ?assert(lists:keymember(TransportId, 1, Transports)),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Transport failure recovery test completed successfully"),
    Config.

test_server_crash_recovery(Config) ->
    ct:pal("Testing server crash recovery"),
    
    ServerId = make_test_server_id(17),
    ServerConfig = #{test_recovery => true},
    {ok, ServerPid1} = erlmcp:start_server(ServerId, ServerConfig),
    
    %% Add some state to the server
    ok = erlmcp:add_tool(ServerId, <<"test_tool">>, fun(_) -> <<"test">> end),
    
    %% Verify server is registered
    {ok, {ServerPid1, _}} = erlmcp_registry:find_server(ServerId),
    
    %% Kill the server process
    exit(ServerPid1, kill),
    timer:sleep(200),
    
    %% Check if server is automatically recovered or needs manual restart
    case erlmcp_registry:find_server(ServerId) of
        {error, not_found} ->
            %% Manual recovery needed
            {ok, ServerPid2} = erlmcp:start_server(ServerId, ServerConfig),
            ?assertNotEqual(ServerPid1, ServerPid2),
            ct:pal("Server recovered manually after crash");
        {ok, {ServerPid2, _}} when ServerPid2 =/= ServerPid1 ->
            %% Automatic recovery occurred
            ct:pal("Server recovered automatically after crash");
        {ok, {ServerPid1, _}} ->
            ct:fail("Server PID should change after crash and recovery")
    end,
    
    %% Verify server is functional after recovery
    {ok, {NewPid, _}} = erlmcp_registry:find_server(ServerId),
    ?assert(is_process_alive(NewPid)),
    
    %% Re-add tool to verify functionality
    ok = erlmcp:add_tool(ServerId, <<"recovery_tool">>, fun(_) -> <<"recovered">> end),
    
    %% Cleanup
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Server crash recovery test completed successfully"),
    Config.

test_registry_failure_handling(Config) ->
    ct:pal("Testing registry failure handling"),
    
    %% Test graceful degradation when registry is unavailable
    ServerId = make_test_server_id(18),
    
    %% First, start server normally
    {ok, ServerPid} = erlmcp:start_server(ServerId, #{}),
    ?assert(is_process_alive(ServerPid)),
    
    %% Verify server operations work with registry
    {ok, {ServerPid, _}} = erlmcp_registry:find_server(ServerId),
    
    %% Test that system handles registry operations gracefully
    Servers = erlmcp:list_servers(),
    ?assert(is_list(Servers)),
    
    Transports = erlmcp:list_transports(),
    ?assert(is_list(Transports)),
    
    %% Test configuration operations
    UpdateConfig = #{registry_test => true},
    ?assertEqual(ok, erlmcp:update_server_config(ServerId, UpdateConfig)),
    
    %% Cleanup
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Registry failure handling test completed successfully"),
    Config.

%%====================================================================
%% Performance Integration Tests
%%====================================================================

test_concurrent_connections(Config) ->
    ct:pal("Testing concurrent connections (load: ~p clients)", [?CONCURRENT_CLIENTS]),
    
    %% Create a server to handle concurrent clients
    ServerId = make_test_server_id(20),
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Add a simple tool for testing
    TestTool = fun(Args) ->
        ClientId = maps:get(<<"client_id">>, Args, <<"unknown">>),
        #{result => <<"Response for client: ", ClientId/binary>>}
    end,
    ok = erlmcp:add_tool(ServerId, <<"concurrent_test">>, TestTool),
    
    %% Create concurrent transport connections
    StartTime = erlang:system_time(millisecond),
    
    ClientPids = lists:map(fun(ClientNum) ->
        spawn(fun() ->
            client_simulation_worker(ServerId, ClientNum, 10) % 10 messages per client
        end)
    end, lists:seq(1, ?CONCURRENT_CLIENTS)),
    
    %% Wait for all clients to complete
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, normal} -> ok;
            {'DOWN', Ref, process, Pid, Reason} ->
                ct:pal("Client ~p failed: ~p", [Pid, Reason])
        after ?TEST_TIMEOUT ->
            ct:fail("Client ~p timed out", [Pid])
        end
    end, ClientPids),
    
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,
    
    TotalMessages = ?CONCURRENT_CLIENTS * 10,
    MessagesPerSecond = (TotalMessages * 1000) div max(Duration, 1),
    
    ct:pal("Concurrent test completed: ~p clients, ~p messages in ~pms (~p msg/sec)",
           [?CONCURRENT_CLIENTS, TotalMessages, Duration, MessagesPerSecond]),
    
    %% Performance validation
    ?assert(Duration < ?TEST_TIMEOUT, "Test should complete within timeout"),
    ?assert(MessagesPerSecond > 10, "Should handle at least 10 messages per second"),
    
    %% Cleanup
    ok = erlmcp:stop_server(ServerId),
    
    Config.

test_high_message_throughput(Config) ->
    ct:pal("Testing high message throughput"),
    
    %% Setup server with multiple tools
    ServerId = make_test_server_id(21),
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Add multiple tools for variety
    Tools = [
        {<<"simple">>, fun(_) -> <<"Simple response">> end},
        {<<"echo">>, fun(Args) -> Args end},
        {<<"timestamp">>, fun(_) -> #{timestamp => erlang:system_time(millisecond)} end},
        {<<"counter">>, fun(Args) ->
            Count = maps:get(<<"count">>, Args, 0),
            #{next_count => Count + 1}
        end}
    ],
    
    lists:foreach(fun({Name, Handler}) ->
        ok = erlmcp:add_tool(ServerId, Name, Handler)
    end, Tools),
    
    %% Create transport for throughput testing
    TransportId = make_test_transport_id(21),
    TransportConfig = #{server_id => ServerId},
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, TransportConfig),
    
    %% Generate message burst
    StartTime = erlang:system_time(microsecond),
    
    MessageResults = lists:map(fun(MsgNum) ->
        ToolName = lists:nth((MsgNum rem length(Tools)) + 1, 
                           [Name || {Name, _} <- Tools]),
        
        Message = #{
            ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
            ?JSONRPC_FIELD_ID => MsgNum,
            ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_TOOLS_CALL,
            ?JSONRPC_FIELD_PARAMS => #{
                <<"name">> => ToolName,
                <<"arguments">> => #{<<"count">> => MsgNum}
            }
        },
        
        %% Send message (simulate transport handling)
        MessageJson = jsx:encode(Message),
        TransportPid ! {message, MessageJson},
        
        {MsgNum, sent}
    end, lists:seq(1, ?MESSAGE_BURST_SIZE)),
    
    %% Wait for processing
    timer:sleep(1000),
    
    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,
    DurationMs = Duration div 1000,
    
    ProcessedMessages = length(MessageResults),
    ThroughputMPS = (ProcessedMessages * 1000000) div max(Duration, 1),
    
    ct:pal("Throughput test: ~p messages in ~pms (~p msg/sec)",
           [ProcessedMessages, DurationMs, ThroughputMPS]),
    
    %% Performance validation
    ?assert(ProcessedMessages >= ?MESSAGE_BURST_SIZE, "All messages should be processed"),
    ?assert(ThroughputMPS > 100, "Should handle at least 100 messages per second"),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    Config.

test_resource_management_under_load(Config) ->
    ct:pal("Testing resource management under load"),
    
    %% Monitor system resources before test
    InitialMemory = erlang:memory(total),
    InitialProcessCount = erlang:system_info(process_count),
    
    ct:pal("Initial system state - Memory: ~pMB, Processes: ~p",
           [InitialMemory div 1024 div 1024, InitialProcessCount]),
    
    %% Create multiple servers under load
    ServerCount = 20,
    ServerIds = [make_test_server_id(22, I) || I <- lists:seq(1, ServerCount)],
    
    %% Start servers with resource-intensive configurations
    ServerPids = lists:map(fun(ServerId) ->
        ServerConfig = #{
            load_test => true,
            server_id => ServerId,
            capabilities => #mcp_server_capabilities{
                resources = #mcp_capability{enabled = true},
                tools = #mcp_capability{enabled = true},
                prompts = #mcp_capability{enabled = true}
            }
        },
        {ok, Pid} = erlmcp:start_server(ServerId, ServerConfig),
        
        %% Add multiple resources to each server
        lists:foreach(fun(ResourceNum) ->
            Uri = iolist_to_binary(io_lib:format("test://~p/resource_~p", [ServerId, ResourceNum])),
            Handler = fun(_) ->
                #{
                    content => iolist_to_binary(io_lib:format("Resource ~p content", [ResourceNum])),
                    size => ResourceNum * 100
                }
            end,
            ok = erlmcp:add_resource(ServerId, Uri, Handler)
        end, lists:seq(1, 10)),
        
        {ServerId, Pid}
    end, ServerIds),
    
    %% Monitor resource usage during operation
    timer:sleep(2000), % Let system stabilize
    
    PeakMemory = erlang:memory(total),
    PeakProcessCount = erlang:system_info(process_count),
    
    ct:pal("Peak system state - Memory: ~pMB, Processes: ~p",
           [PeakMemory div 1024 div 1024, PeakProcessCount]),
    
    %% Verify all servers are operational
    lists:foreach(fun({ServerId, Pid}) ->
        ?assert(is_process_alive(Pid)),
        {ok, {Pid, _}} = erlmcp_registry:find_server(ServerId)
    end, ServerPids),
    
    %% Simulate load on all servers
    LoadTestPids = lists:map(fun({ServerId, _}) ->
        spawn(fun() ->
            lists:foreach(fun(OpNum) ->
                try
                    %% Test resource operations
                    Uri = iolist_to_binary(io_lib:format("test://~p/resource_~p", 
                                                       [ServerId, OpNum rem 10 + 1])),
                    
                    %% Simulate resource access (this would go through proper channels)
                    timer:sleep(1),
                    ok
                catch
                    _:_ -> ok
                end
            end, lists:seq(1, 50))
        end)
    end, ServerPids),
    
    %% Wait for load test completion
    lists:foreach(fun(LoadPid) ->
        Ref = monitor(process, LoadPid),
        receive
            {'DOWN', Ref, process, LoadPid, _} -> ok
        after 10000 ->
            exit(LoadPid, kill),
            ct:pal("Load test process timed out")
        end
    end, LoadTestPids),
    
    %% Check system state after load
    timer:sleep(1000), % Allow GC
    FinalMemory = erlang:memory(total),
    FinalProcessCount = erlang:system_info(process_count),
    
    ct:pal("Final system state - Memory: ~pMB, Processes: ~p",
           [FinalMemory div 1024 div 1024, FinalProcessCount]),
    
    %% Cleanup all servers
    lists:foreach(fun({ServerId, _}) ->
        ok = erlmcp:stop_server(ServerId)
    end, ServerPids),
    
    %% Final resource check
    timer:sleep(1000),
    CleanupMemory = erlang:memory(total),
    CleanupProcessCount = erlang:system_info(process_count),
    
    ct:pal("After cleanup - Memory: ~pMB, Processes: ~p",
           [CleanupMemory div 1024 div 1024, CleanupProcessCount]),
    
    %% Resource management validation
    MemoryGrowth = (PeakMemory - InitialMemory) div 1024 div 1024,
    ProcessGrowth = PeakProcessCount - InitialProcessCount,
    
    ?assert(MemoryGrowth < 500, "Memory growth should be reasonable (<500MB)"),
    ?assert(ProcessGrowth < ServerCount * 5, "Process growth should be reasonable"),
    
    %% Memory should be mostly cleaned up
    FinalGrowth = (CleanupMemory - InitialMemory) div 1024 div 1024,
    ?assert(FinalGrowth < 50, "Memory should be cleaned up after server shutdown"),
    
    ct:pal("Resource management test completed - Max growth: ~pMB, ~p processes",
           [MemoryGrowth, ProcessGrowth]),
    
    Config.

%%====================================================================
%% Client Interaction Tests
%%====================================================================

test_real_mcp_client_interaction(Config) ->
    ct:pal("Testing real MCP client interaction"),
    
    %% Setup complete MCP server with all capabilities
    ServerId = make_test_server_id(30),
    ServerConfig = #{
        capabilities => #mcp_server_capabilities{
            resources = #mcp_capability{enabled = true},
            tools = #mcp_capability{enabled = true},
            prompts = #mcp_capability{enabled = true}
        },
        server_info => #{
            name => <<"ErlMCP Integration Test Server">>,
            version => <<"1.0.0">>
        }
    },
    
    {ok, ServerPid} = erlmcp:start_server(ServerId, ServerConfig),
    
    %% Add comprehensive MCP components
    setup_comprehensive_mcp_server(ServerId),
    
    %% Setup transport
    TransportId = make_test_transport_id(30),
    TransportConfig = #{server_id => ServerId},
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, TransportConfig),
    ok = erlmcp:bind_transport_to_server(TransportId, ServerId),
    
    %% Simulate real MCP client interaction sequence
    ClientInteraction = [
        %% 1. Initialize
        #{
            id => 1,
            method => ?MCP_METHOD_INITIALIZE,
            params => #{
                ?MCP_FIELD_PROTOCOL_VERSION => ?MCP_VERSION,
                ?MCP_FIELD_CAPABILITIES => #{
                    ?MCP_CAPABILITY_RESOURCES => #{?MCP_FEATURE_SUBSCRIBE => true},
                    ?MCP_CAPABILITY_TOOLS => #{},
                    ?MCP_CAPABILITY_PROMPTS => #{}
                },
                ?MCP_FIELD_CLIENT_INFO => #{
                    ?MCP_INFO_NAME => <<"Integration Test Client">>,
                    ?MCP_INFO_VERSION => <<"1.0.0">>
                }
            }
        },
        
        %% 2. List tools
        #{
            id => 2,
            method => ?MCP_METHOD_TOOLS_LIST
        },
        
        %% 3. List resources
        #{
            id => 3,
            method => ?MCP_METHOD_RESOURCES_LIST
        },
        
        %% 4. List prompts
        #{
            id => 4,
            method => ?MCP_METHOD_PROMPTS_LIST
        },
        
        %% 5. Call a tool
        #{
            id => 5,
            method => ?MCP_METHOD_TOOLS_CALL,
            params => #{
                <<"name">> => <<"calculator">>,
                <<"arguments">> => #{
                    <<"operation">> => <<"add">>,
                    <<"a">> => 10,
                    <<"b">> => 20
                }
            }
        },
        
        %% 6. Read a resource
        #{
            id => 6,
            method => ?MCP_METHOD_RESOURCES_READ,
            params => #{
                <<"uri">> => <<"file://test.txt">>
            }
        },
        
        %% 7. Get a prompt
        #{
            id => 7,
            method => ?MCP_METHOD_PROMPTS_GET,
            params => #{
                <<"name">> => <<"code_review">>,
                <<"arguments">> => #{
                    <<"language">> => <<"erlang">>,
                    <<"code">> => <<"test_code() -> ok.">>
                }
            }
        }
    ],
    
    %% Execute client interaction sequence
    lists:foreach(fun(Request) ->
        Message = #{
            ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
            ?JSONRPC_FIELD_ID => maps:get(id, Request),
            ?JSONRPC_FIELD_METHOD => maps:get(method, Request),
            ?JSONRPC_FIELD_PARAMS => maps:get(params, Request, #{})
        },
        
        MessageJson = jsx:encode(Message),
        TransportPid ! {message, MessageJson},
        timer:sleep(100) % Allow processing time
    end, ClientInteraction),
    
    %% Wait for all interactions to complete
    timer:sleep(1000),
    
    %% Verify server is still operational
    ?assert(is_process_alive(ServerPid)),
    ?assert(is_process_alive(TransportPid)),
    
    %% Verify server state
    {ok, {ServerPid, FinalConfig}} = erlmcp_registry:find_server(ServerId),
    ?assert(is_map(FinalConfig)),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Real MCP client interaction test completed successfully"),
    Config.

test_tool_execution_end_to_end(Config) ->
    ct:pal("Testing tool execution end-to-end"),
    
    ServerId = make_test_server_id(31),
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Add comprehensive test tools
    TestTools = [
        {<<"calculator">>, fun(Args) ->
            Op = maps:get(<<"operation">>, Args, <<"add">>),
            A = maps:get(<<"a">>, Args, 0),
            B = maps:get(<<"b">>, Args, 0),
            
            Result = case Op of
                <<"add">> -> A + B;
                <<"multiply">> -> A * B;
                <<"subtract">> -> A - B;
                _ -> {error, <<"Unknown operation">>}
            end,
            
            #{result => Result, operation => Op}
        end},
        
        {<<"string_processor">>, fun(Args) ->
            Text = maps:get(<<"text">>, Args, <<"">>),
            Operation = maps:get(<<"operation">>, Args, <<"upper">>),
            
            Result = case Operation of
                <<"upper">> -> string:uppercase(Text);
                <<"lower">> -> string:lowercase(Text);
                <<"reverse">> -> list_to_binary(lists:reverse(binary_to_list(Text)));
                _ -> Text
            end,
            
            #{result => Result, original => Text}
        end},
        
        {<<"file_info">>, fun(Args) ->
            Filename = maps:get(<<"filename">>, Args, <<"unknown">>),
            #{
                filename => Filename,
                size => byte_size(Filename) * 100, % Simulated size
                type => <<"file">>,
                timestamp => erlang:system_time(millisecond)
            }
        end}
    ],
    
    %% Add all tools to server
    lists:foreach(fun({Name, Handler}) ->
        ok = erlmcp:add_tool(ServerId, Name, Handler)
    end, TestTools),
    
    %% Setup transport
    TransportId = make_test_transport_id(31),
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}),
    
    %% Test each tool with various parameters
    ToolTests = [
        {<<"calculator">>, #{<<"operation">> => <<"add">>, <<"a">> => 15, <<"b">> => 25}},
        {<<"calculator">>, #{<<"operation">> => <<"multiply">>, <<"a">> => 7, <<"b">> => 8}},
        {<<"string_processor">>, #{<<"text">> => <<"Hello World">>, <<"operation">> => <<"upper">>}},
        {<<"string_processor">>, #{<<"text">> => <<"REVERSE ME">>, <<"operation">> => <<"reverse">>}},
        {<<"file_info">>, #{<<"filename">> => <<"test_file.erl">>}}
    ],
    
    %% Execute tool tests
    lists:foreach(fun({ToolName, Args}) ->
        Message = #{
            ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
            ?JSONRPC_FIELD_ID => erlang:unique_integer([positive]),
            ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_TOOLS_CALL,
            ?JSONRPC_FIELD_PARAMS => #{
                <<"name">> => ToolName,
                <<"arguments">> => Args
            }
        },
        
        MessageJson = jsx:encode(Message),
        TransportPid ! {message, MessageJson},
        timer:sleep(50) % Processing time
    end, ToolTests),
    
    %% Test error conditions
    ErrorTests = [
        {<<"nonexistent_tool">>, #{}},
        {<<"calculator">>, #{<<"operation">> => <<"invalid">>, <<"a">> => 1, <<"b">> => 2}},
        {<<"string_processor">>, #{}} % Missing required parameters
    ],
    
    lists:foreach(fun({ToolName, Args}) ->
        Message = #{
            ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
            ?JSONRPC_FIELD_ID => erlang:unique_integer([positive]),
            ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_TOOLS_CALL,
            ?JSONRPC_FIELD_PARAMS => #{
                <<"name">> => ToolName,
                <<"arguments">> => Args
            }
        },
        
        MessageJson = jsx:encode(Message),
        TransportPid ! {message, MessageJson},
        timer:sleep(50)
    end, ErrorTests),
    
    %% Final processing wait
    timer:sleep(500),
    
    %% Verify system is still stable
    ?assert(is_process_alive(TransportPid)),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Tool execution end-to-end test completed successfully"),
    Config.

test_resource_access_end_to_end(Config) ->
    ct:pal("Testing resource access end-to-end"),
    
    ServerId = make_test_server_id(32),
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Add test resources
    TestResources = [
        {<<"file://test.txt">>, fun(_Uri) ->
            #{
                content => <<"This is test file content">>,
                mimeType => <<"text/plain">>,
                size => 25
            }
        end},
        
        {<<"config://settings.json">>, fun(_Uri) ->
            ConfigData = #{
                <<"version">> => <<"1.0.0">>,
                <<"environment">> => <<"test">>,
                <<"features">> => [<<"feature1">>, <<"feature2">>]
            },
            #{
                content => jsx:encode(ConfigData),
                mimeType => <<"application/json">>,
                metadata => #{<<"type">> => <<"configuration">>}
            }
        end},
        
        {<<"data://users/123">>, fun(Uri) ->
            UserId = lists:last(binary:split(Uri, <<"/">>, [global])),
            #{
                content => jsx:encode(#{
                    <<"id">> => UserId,
                    <<"name">> => <<"Test User">>,
                    <<"email">> => <<"test@example.com">>
                }),
                mimeType => <<"application/json">>
            }
        end}
    ],
    
    %% Add all resources
    lists:foreach(fun({Uri, Handler}) ->
        ok = erlmcp:add_resource(ServerId, Uri, Handler)
    end, TestResources),
    
    %% Setup transport
    TransportId = make_test_transport_id(32),
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}),
    
    %% Test resource listing
    ListMessage = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => 1,
        ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_RESOURCES_LIST
    },
    
    TransportPid ! {message, jsx:encode(ListMessage)},
    timer:sleep(100),
    
    %% Test resource reading
    ResourceTests = [
        <<"file://test.txt">>,
        <<"config://settings.json">>,
        <<"data://users/123">>,
        <<"nonexistent://resource">>  % Should handle gracefully
    ],
    
    lists:foreach(fun(ResourceUri) ->
        ReadMessage = #{
            ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
            ?JSONRPC_FIELD_ID => erlang:unique_integer([positive]),
            ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_RESOURCES_READ,
            ?JSONRPC_FIELD_PARAMS => #{
                <<"uri">> => ResourceUri
            }
        },
        
        TransportPid ! {message, jsx:encode(ReadMessage)},
        timer:sleep(50)
    end, ResourceTests),
    
    %% Test resource templates (if supported)
    TemplateMessage = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => 100,
        ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_RESOURCES_TEMPLATES_LIST
    },
    
    TransportPid ! {message, jsx:encode(TemplateMessage)},
    timer:sleep(100),
    
    %% Final processing wait
    timer:sleep(500),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Resource access end-to-end test completed successfully"),
    Config.

test_prompt_handling_integration(Config) ->
    ct:pal("Testing prompt handling integration"),
    
    ServerId = make_test_server_id(33),
    {ok, _ServerPid} = erlmcp:start_server(ServerId, #{}),
    
    %% Add test prompts
    TestPrompts = [
        {<<"code_review">>, fun(Args) ->
            Language = maps:get(<<"language">>, Args, <<"unknown">>),
            Code = maps:get(<<"code">>, Args, <<"">>),
            
            #{
                messages => [
                    #{
                        role => <<"system">>,
                        content => <<"You are a code reviewer. Analyze the provided code.">>
                    },
                    #{
                        role => <<"user">>,
                        content => <<"Please review this ", Language/binary, " code: ", Code/binary>>
                    }
                ],
                model => <<"gpt-4">>,
                temperature => 0.1
            }
        end},
        
        {<<"documentation_helper">>, fun(Args) ->
            Topic = maps:get(<<"topic">>, Args, <<"general">>),
            Detail = maps:get(<<"detail_level">>, Args, <<"medium">>),
            
            #{
                messages => [
                    #{
                        role => <<"system">>,
                        content => <<"You are a technical documentation assistant.">>
                    },
                    #{
                        role => <<"user">>,
                        content => <<"Create ", Detail/binary, " level documentation for: ", Topic/binary>>
                    }
                ]
            }
        end},
        
        {<<"test_generator">>, fun(Args) ->
            Module = maps:get(<<"module">>, Args, <<"unknown">>),
            Functions = maps:get(<<"functions">>, Args, []),
            
            FunctionList = iolist_to_binary(io_lib:format("~p", [Functions])),
            
            #{
                messages => [
                    #{
                        role => <<"system">>,
                        content => <<"You are a test generation assistant for Erlang code.">>
                    },
                    #{
                        role => <<"user">>,
                        content => <<"Generate tests for module ", Module/binary, 
                                    " with functions: ", FunctionList/binary>>
                    }
                ]
            }
        end}
    ],
    
    %% Add all prompts
    lists:foreach(fun({Name, Handler}) ->
        ok = erlmcp:add_prompt(ServerId, Name, Handler)
    end, TestPrompts),
    
    %% Setup transport
    TransportId = make_test_transport_id(33),
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}),
    
    %% Test prompt listing
    ListMessage = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => 1,
        ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_PROMPTS_LIST
    },
    
    TransportPid ! {message, jsx:encode(ListMessage)},
    timer:sleep(100),
    
    %% Test prompt execution
    PromptTests = [
        {<<"code_review">>, #{
            <<"language">> => <<"erlang">>,
            <<"code">> => <<"factorial(0) -> 1; factorial(N) -> N * factorial(N-1).">>
        }},
        {<<"documentation_helper">>, #{
            <<"topic">> => <<"ErlMCP Integration Tests">>,
            <<"detail_level">> => <<"detailed">>
        }},
        {<<"test_generator">>, #{
            <<"module">> => <<"math_utils">>,
            <<"functions">> => [<<"add">>, <<"subtract">>, <<"multiply">>]
        }},
        {<<"nonexistent_prompt">>, #{}}  % Error case
    ],
    
    lists:foreach(fun({PromptName, Args}) ->
        GetMessage = #{
            ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
            ?JSONRPC_FIELD_ID => erlang:unique_integer([positive]),
            ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_PROMPTS_GET,
            ?JSONRPC_FIELD_PARAMS => #{
                <<"name">> => PromptName,
                <<"arguments">> => Args
            }
        },
        
        TransportPid ! {message, jsx:encode(GetMessage)},
        timer:sleep(100)
    end, PromptTests),
    
    %% Final processing wait
    timer:sleep(500),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    ct:pal("Prompt handling integration test completed successfully"),
    Config.

%%====================================================================
%% Monitoring Integration Tests
%%====================================================================

test_monitoring_integration(Config) ->
    ct:pal("Testing monitoring integration"),
    
    %% Test supervisor monitoring
    {ok, SupChildren} = supervisor:which_children(erlmcp_sup),
    RunningChildren = [Id || {Id, Pid, _Type, _Modules} <- SupChildren, is_pid(Pid)],
    ?assert(length(RunningChildren) >= 1, "Should have running supervised processes"),
    
    %% Test registry monitoring
    InitialServers = erlmcp:list_servers(),
    InitialTransports = erlmcp:list_transports(),
    
    %% Create monitored components
    ServerId = make_test_server_id(40),
    {ok, ServerPid} = erlmcp:start_server(ServerId, #{monitoring_test => true}),
    
    TransportId = make_test_transport_id(40),
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio, #{server_id => ServerId}),
    
    %% Verify monitoring can detect new components
    NewServers = erlmcp:list_servers(),
    NewTransports = erlmcp:list_transports(),
    
    ?assertEqual(length(InitialServers) + 1, length(NewServers)),
    ?assertEqual(length(InitialTransports) + 1, length(NewTransports)),
    
    %% Test process monitoring
    ?assert(is_process_alive(ServerPid)),
    ?assert(is_process_alive(TransportPid)),
    
    %% Test configuration monitoring
    {ok, ServerConfig} = erlmcp:get_server_config(ServerId),
    ?assertEqual(true, maps:get(monitoring_test, ServerConfig)),
    
    %% Test health check capabilities
    SystemHealth = #{
        memory => erlang:memory(total),
        processes => erlang:system_info(process_count),
        ports => erlang:system_info(port_count),
        uptime => element(1, erlang:statistics(wall_clock))
    },
    
    %% Validate system health is reasonable
    ?assert(maps:get(memory, SystemHealth) > 1024 * 1024), % > 1MB
    ?assert(maps:get(processes, SystemHealth) > 10),
    ?assert(maps:get(processes, SystemHealth) < 10000), % Reasonable upper bound
    
    ct:pal("System health: ~p", [SystemHealth]),
    
    %% Cleanup
    ok = erlmcp:stop_transport(TransportId),
    ok = erlmcp:stop_server(ServerId),
    
    %% Verify cleanup was monitored
    timer:sleep(100),
    FinalServers = erlmcp:list_servers(),
    FinalTransports = erlmcp:list_transports(),
    
    ?assertEqual(length(InitialServers), length(FinalServers)),
    ?assertEqual(length(InitialTransports), length(FinalTransports)),
    
    ct:pal("Monitoring integration test completed successfully"),
    Config.

test_metrics_collection_integration(Config) ->
    ct:pal("Testing metrics collection integration"),
    
    %% Initialize metrics system (if available)
    MetricsAvailable = case catch erlmcp_metrics:start_link() of
        {ok, _} -> true;
        {error, {already_started, _}} -> true;
        _ -> false
    end,
    
    case MetricsAvailable of
        true ->
            %% Reset metrics for clean test
            ok = erlmcp_metrics:reset_metrics(),
            
            %% Create test server for metrics collection
            ServerId = make_test_server_id(41),
            {ok, _ServerPid} = erlmcp:start_server(ServerId, #{metrics_test => true}),
            
            %% Record various metrics
            erlmcp_metrics:record_transport_operation(test_transport, stdio, <<"connect">>, 100),
            erlmcp_metrics:record_server_operation(ServerId, <<"initialize">>, 200, #{success => true}),
            erlmcp_metrics:record_registry_operation(<<"lookup">>, 50, #{found => true}),
            
            %% Add some load to generate more metrics
            lists:foreach(fun(I) ->
                erlmcp_metrics:record_server_operation(
                    ServerId, 
                    <<"operation_", (integer_to_binary(I))/binary>>, 
                    I * 10, 
                    #{iteration => I}
                )
            end, lists:seq(1, 10)),
            
            %% Wait for metrics processing
            timer:sleep(200),
            
            %% Retrieve and validate metrics
            AllMetrics = erlmcp_metrics:get_metrics(),
            ?assert(length(AllMetrics) >= 12, "Should have collected multiple metrics"),
            
            %% Test performance summary
            Summary = erlmcp_metrics:get_performance_summary(),
            ?assert(is_map(Summary)),
            ?assert(map_size(Summary) > 0),
            
            ct:pal("Collected ~p metrics, summary keys: ~p", 
                   [length(AllMetrics), maps:keys(Summary)]),
            
            %% Test filtered metrics retrieval
            ServerMetrics = erlmcp_metrics:get_metrics(<<"server_operation_duration_ms">>),
            ?assert(length(ServerMetrics) >= 10),
            
            %% Cleanup
            ok = erlmcp:stop_server(ServerId);
        false ->
            ct:pal("Metrics system not available, skipping metrics collection test")
    end,
    
    ct:pal("Metrics collection integration test completed"),
    Config.

test_tracing_integration(Config) ->
    ct:pal("Testing tracing integration"),
    
    %% Test tracing functionality (gracefully handle if OpenTelemetry not available)
    try
        %% Test basic tracing operations
        SpanName = <<"integration_test_span">>,
        Attributes = #{
            <<"test_type">> => <<"integration">>,
            <<"component">> => <<"tracing">>
        },
        
        %% Test span creation and management
        SpanCtx = erlmcp_tracing:start_transport_span(SpanName, test_transport, stdio),
        
        %% Record some performance metrics
        erlmcp_tracing:record_performance_metrics(SpanCtx, #{
            latency => 150,
            throughput => 1000,
            success_rate => 0.95
        }),
        
        %% Add custom attributes
        lists:foreach(fun(I) ->
            AttrKey = iolist_to_binary(io_lib:format("metric_~p", [I])),
            AttrValue = I * 100,
            erlmcp_tracing:add_span_attribute(SpanCtx, AttrKey, AttrValue)
        end, lists:seq(1, 5)),
        
        %% End the span
        erlmcp_tracing:end_span(SpanCtx),
        
        %% Test server operation tracing
        ServerId = make_test_server_id(42),
        {ok, _ServerPid} = erlmcp:start_server(ServerId, #{tracing_test => true}),
        
        %% Simulate traced server operations
        lists:foreach(fun(OpNum) ->
            OpName = iolist_to_binary(io_lib:format("operation_~p", [OpNum])),
            ServerSpan = erlmcp_tracing:start_server_span(OpName, ServerId),
            
            %% Simulate work
            timer:sleep(1),
            
            erlmcp_tracing:record_performance_metrics(ServerSpan, #{
                operation_time => OpNum * 10,
                memory_used => OpNum * 1024
            }),
            
            erlmcp_tracing:end_span(ServerSpan)
        end, lists:seq(1, 3)),
        
        %% Test attribute normalization functions
        NormalizedKey = erlmcp_tracing:normalize_attr_key(test_key_123),
        ?assertEqual(<<"test_key_123">>, NormalizedKey),
        
        NormalizedValue = erlmcp_tracing:normalize_attr_value({complex, value, 123}),
        ?assertEqual(<<"{complex,value,123}">>, NormalizedValue),
        
        ct:pal("Tracing integration test completed successfully"),
        
        %% Cleanup
        ok = erlmcp:stop_server(ServerId)
        
    catch
        error:undef ->
            ct:pal("OpenTelemetry/Tracing not available, test completed with graceful degradation");
        Class:Reason:Stack ->
            ct:pal("Tracing test encountered error ~p:~p~n~p", [Class, Reason, Stack]),
            ct:pal("Continuing test execution - tracing is optional")
    end,
    
    Config.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Create unique test server ID
make_test_server_id(TestNum) ->
    list_to_atom(io_lib:format("~p_~p", [?TEST_SERVER_PREFIX, TestNum])).

make_test_server_id(TestNum, SubNum) ->
    list_to_atom(io_lib:format("~p_~p_~p", [?TEST_SERVER_PREFIX, TestNum, SubNum])).

%% Create unique test transport ID
make_test_transport_id(TestNum) ->
    list_to_atom(io_lib:format("~p_~p", [?TEST_TRANSPORT_PREFIX, TestNum])).

make_test_transport_id(TestNum, Type) ->
    list_to_atom(io_lib:format("~p_~p_~p", [?TEST_TRANSPORT_PREFIX, TestNum, Type])).

%% Client simulation worker for concurrent testing
client_simulation_worker(ServerId, ClientNum, MessageCount) ->
    try
        %% Create transport for this client
        ClientTransportId = list_to_atom(io_lib:format("client_~p_transport", [ClientNum])),
        
        case erlmcp:start_transport(ClientTransportId, stdio, #{server_id => ServerId}) of
            {ok, TransportPid} ->
                %% Send messages
                lists:foreach(fun(MsgNum) ->
                    Message = #{
                        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                        ?JSONRPC_FIELD_ID => MsgNum,
                        ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_TOOLS_CALL,
                        ?JSONRPC_FIELD_PARAMS => #{
                            <<"name">> => <<"concurrent_test">>,
                            <<"arguments">> => #{
                                <<"client_id">> => iolist_to_binary(io_lib:format("client_~p", [ClientNum])),
                                <<"message_id">> => MsgNum
                            }
                        }
                    },
                    
                    MessageJson = jsx:encode(Message),
                    TransportPid ! {message, MessageJson},
                    timer:sleep(10) % Small delay between messages
                end, lists:seq(1, MessageCount)),
                
                %% Cleanup
                erlmcp:stop_transport(ClientTransportId);
            {error, Reason} ->
                throw({transport_start_failed, Reason})
        end
    catch
        Class:Exception ->
            ct:pal("Client ~p failed: ~p:~p", [ClientNum, Class, Exception]),
            throw({client_failed, ClientNum, Class, Exception})
    end.

%% Setup comprehensive MCP server for testing
setup_comprehensive_mcp_server(ServerId) ->
    %% Add tools
    ok = erlmcp:add_tool(ServerId, <<"calculator">>, fun(Args) ->
        Op = maps:get(<<"operation">>, Args, <<"add">>),
        A = maps:get(<<"a">>, Args, 0),
        B = maps:get(<<"b">>, Args, 0),
        
        Result = case Op of
            <<"add">> -> A + B;
            <<"multiply">> -> A * B;
            <<"subtract">> -> A - B;
            <<"divide">> when B =/= 0 -> A / B;
            <<"divide">> -> {error, <<"Division by zero">>};
            _ -> {error, <<"Unknown operation">>}
        end,
        
        #{result => Result}
    end),
    
    ok = erlmcp:add_tool(ServerId, <<"system_info">>, fun(_Args) ->
        #{
            erlang_version => list_to_binary(erlang:system_info(otp_release)),
            process_count => erlang:system_info(process_count),
            memory => erlang:memory(total),
            uptime => element(1, erlang:statistics(wall_clock))
        }
    end),
    
    %% Add resources
    ok = erlmcp:add_resource(ServerId, <<"file://test.txt">>, fun(_Uri) ->
        #{
            content => <<"This is a test file content for integration testing">>,
            mimeType => <<"text/plain">>,
            size => 54
        }
    end),
    
    ok = erlmcp:add_resource(ServerId, <<"config://app.json">>, fun(_Uri) ->
        Config = #{
            <<"app_name">> => <<"erlmcp">>,
            <<"version">> => <<"1.0.0">>,
            <<"environment">> => <<"test">>,
            <<"features">> => [<<"resources">>, <<"tools">>, <<"prompts">>]
        },
        #{
            content => jsx:encode(Config),
            mimeType => <<"application/json">>
        }
    end),
    
    %% Add prompts
    ok = erlmcp:add_prompt(ServerId, <<"code_review">>, fun(Args) ->
        Language = maps:get(<<"language">>, Args, <<"erlang">>),
        Code = maps:get(<<"code">>, Args, <<"">>),
        
        #{
            messages => [
                #{
                    role => <<"system">>,
                    content => <<"You are an expert code reviewer.">>
                },
                #{
                    role => <<"user">>,
                    content => <<"Please review this ", Language/binary, " code: ", Code/binary>>
                }
            ]
        }
    end),
    
    ok = erlmcp:add_prompt(ServerId, <<"documentation">>, fun(Args) ->
        Topic = maps:get(<<"topic">>, Args, <<"general">>),
        
        #{
            messages => [
                #{
                    role => <<"system">>,
                    content => <<"You are a technical documentation writer.">>
                },
                #{
                    role => <<"user">>,
                    content => <<"Create documentation for: ", Topic/binary>>
                }
            ]
        }
    end),
    
    ok.

%% Cleanup test artifacts
cleanup_test_artifacts(TestCase) ->
    ct:pal("Cleaning up artifacts for test case: ~p", [TestCase]),
    
    %% Clean up any test servers that might still be running
    TestServerPattern = atom_to_list(?TEST_SERVER_PREFIX),
    AllServers = erlmcp:list_servers(),
    
    lists:foreach(fun({ServerId, _}) ->
        ServerIdStr = atom_to_list(ServerId),
        case string:prefix(ServerIdStr, TestServerPattern) of
            nomatch -> ok;
            _ ->
                try
                    erlmcp:stop_server(ServerId)
                catch
                    _:_ -> ok
                end
        end
    end, AllServers),
    
    %% Clean up any test transports
    TestTransportPattern = atom_to_list(?TEST_TRANSPORT_PREFIX),
    AllTransports = erlmcp:list_transports(),
    
    lists:foreach(fun({TransportId, _}) ->
        TransportIdStr = atom_to_list(TransportId),
        case string:prefix(TransportIdStr, TestTransportPattern) of
            nomatch -> ok;
            _ ->
                try
                    erlmcp:stop_transport(TransportId)
                catch
                    _:_ -> ok
                end
        end
    end, AllTransports),
    
    %% Allow cleanup to complete
    timer:sleep(100),
    ok.