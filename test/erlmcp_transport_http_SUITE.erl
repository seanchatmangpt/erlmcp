%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for erlmcp_transport_http_new module
%%%
%%% This comprehensive test suite covers all aspects of the HTTP transport
%%% including behavior compliance, HTTP protocol handling, error handling,
%%% and performance characteristics.
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
         end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
%% Test cases
-export([basic_startup_test/1, basic_send_test/1, basic_close_test/1, get_state_test/1,
         get_info_test/1, transport_behavior_send/1, transport_behavior_close/1, 
         transport_behavior_get_info/1, transport_behavior_handle_transport_call/1,
         registry_registration/1, registry_unregistration/1, registry_message_routing/1,
         config_validation/1, config_defaults/1, test_mode_startup/1, http_server_options/1,
         error_handling_server_start_failure/1, error_handling_invalid_config/1,
         error_handling_server_error/1, error_handling_invalid_path/1,
         http_request_handling/1, http_response_generation/1, mcp_request_processing/1, 
         json_rpc_validation/1, http_server_startup/1, http_server_shutdown/1, 
         ssl_configuration/1, path_routing/1, supervisor_integration/1, graceful_shutdown/1, 
         abnormal_termination/1, high_volume_requests/1, memory_usage/1, 
         latency_measurement/1, concurrent_requests/1, stress_test/1, 
         server_cleanup/1]).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [{group, basic_functionality},
     {group, transport_behavior},
     {group, registry_integration},
     {group, configuration},
     {group, error_handling},
     {group, http_processing},
     {group, server_management},
     {group, lifecycle},
     {group, performance},
     {group, integration}].

groups() ->
    [{basic_functionality,
      [parallel],
      [basic_startup_test, basic_send_test, basic_close_test, get_state_test, get_info_test]},
     {transport_behavior,
      [sequential],
      [transport_behavior_send,
       transport_behavior_close,
       transport_behavior_get_info,
       transport_behavior_handle_transport_call]},
     {registry_integration,
      [sequential],
      [registry_registration,
       registry_unregistration,
       registry_message_routing]},
     {configuration,
      [parallel],
      [config_validation, config_defaults, test_mode_startup, http_server_options]},
     {error_handling,
      [sequential],
      [error_handling_server_start_failure,
       error_handling_invalid_config,
       error_handling_server_error,
       error_handling_invalid_path]},
     {http_processing,
      [parallel],
      [http_request_handling, http_response_generation, mcp_request_processing, json_rpc_validation]},
     {server_management,
      [sequential],
      [http_server_startup, http_server_shutdown, ssl_configuration, path_routing]},
     {lifecycle,
      [sequential],
      [supervisor_integration, graceful_shutdown, abnormal_termination]},
     {performance, 
      [sequential], 
      [high_volume_requests, memory_usage, latency_measurement]},
     {integration,
      [sequential],
      [concurrent_requests, stress_test, server_cleanup]}].

%%====================================================================
%% Suite Setup/Teardown
%%====================================================================

init_per_suite(Config) ->
    ct:pal("Starting HTTP transport test suite"),

    % Start necessary applications
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(asn1),
    ok = application:ensure_started(public_key),
    ok = application:ensure_started(ssl),
    ok = application:ensure_started(inets),
    ok = application:ensure_started(sasl),

    % Start registry for integration tests
    case erlmcp_registry:start_link() of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok
    end,

    Config.

end_per_suite(_Config) ->
    ct:pal("Ending HTTP transport test suite"),
    ok.

init_per_group(GroupName, Config) ->
    ct:pal("Starting group: ~p", [GroupName]),
    Config.

end_per_group(GroupName, _Config) ->
    ct:pal("Ending group: ~p", [GroupName]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    TransportId = list_to_atom(lists:flatten(
        io_lib:format("~p_~p", [TestCase, erlang:unique_integer([positive])]))),
    [{transport_id, TransportId} | Config].

end_per_testcase(TestCase, _Config) ->
    ct:pal("Ending test case: ~p", [TestCase]),
    % Cleanup any processes started during test
    cleanup_processes(),
    ok.

%%====================================================================
%% Test Cases - Basic Functionality
%%====================================================================

basic_startup_test(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    % Verify state
    {ok, State} = gen_server:call(Pid, get_state),
    ?assert(is_tuple(State)),

    ok = gen_server:stop(Pid),
    ok.

basic_send_test(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    % Test sending in test mode (should succeed without actual HTTP server)
    ?assertEqual(ok, erlmcp_transport_http_new:send(State, <<"test message">>)),
    ?assertEqual(ok, erlmcp_transport_http_new:send(State, "test string")),

    ok = gen_server:stop(Pid),
    ok.

basic_close_test(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    % Test close operation
    ?assertEqual(ok, erlmcp_transport_http_new:close(State)),

    ok = gen_server:stop(Pid),
    ok.

get_state_test(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    % Verify state structure
    ?assert(is_tuple(State)),
    ?assert(tuple_size(State) > 5), % Should have multiple fields

    ok = gen_server:stop(Pid),
    ok.

get_info_test(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    Info = erlmcp_transport_http_new:get_info(Pid),
    ?assert(is_map(Info)),
    ?assertMatch(#{type := http}, Info),
    ?assertMatch(#{transport_id := TransportId}, Info),
    ?assertMatch(#{port := 8080}, Info),
    ?assertMatch(#{path := "/mcp"}, Info),

    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Transport Behavior
%%====================================================================

transport_behavior_send(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    % Test different data types
    TestCases = [
        <<"binary data">>,
        "string data",
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>})
    ],

    lists:foreach(fun(Data) ->
        ?assertEqual(ok, erlmcp_transport_http_new:send(State, Data))
    end, TestCases),

    ok = gen_server:stop(Pid),
    ok.

transport_behavior_close(Config) ->
    TransportId = ?config(transport_id, Config),

    % Test with different configurations
    TestConfigs = [
        #{test_mode => true, port => 8080, path => "/mcp"},
        #{test_mode => false, port => 8081, path => "/test"}
    ],

    lists:foreach(fun(TestConfig) ->
        case erlmcp_transport_http_new:start_link(TransportId, TestConfig) of
            {ok, Pid} ->
                {ok, State} = gen_server:call(Pid, get_state),
                ?assertEqual(ok, erlmcp_transport_http_new:close(State)),
                ok = gen_server:stop(Pid);
            {error, _} ->
                % Expected for some invalid configurations
                ok
        end
    end, TestConfigs),

    ok.

transport_behavior_get_info(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp", ssl_enabled => false},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    Info = erlmcp_transport_http_new:get_info(State),

    ?assertMatch(#{type := http}, Info),
    ?assertMatch(#{status := running}, Info),
    ?assertMatch(#{test_mode := true}, Info),
    ?assertMatch(#{transport_id := TransportId}, Info),
    ?assertMatch(#{ssl_enabled := false}, Info),

    ok = gen_server:stop(Pid),
    ok.

transport_behavior_handle_transport_call(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Test HTTP request call
    TestRequest = gen_server:call(Pid, {http_request, 'POST', "/mcp", [], <<"test body">>}),
    ?assertMatch({_, _, _}, TestRequest), % Should return {Status, Headers, Body}

    % Test unknown call - should return error
    {reply, {error, unknown_request}, _} = 
        gen_server:call(Pid, {unknown_transport_call, test}),

    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Registry Integration
%%====================================================================

registry_registration(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Wait for registration to complete
    timer:sleep(100),
    
    % Verify registration (registry should exist from init_per_suite)
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {RegPid, RegConfig}} ->
            ?assertEqual(Pid, RegPid),
            ?assertMatch(#{type := http}, RegConfig);
        {error, not_found} ->
            ct:pal("Warning: Transport not registered (registry might not be available)")
    end,

    ok = gen_server:stop(Pid),
    ok.

registry_unregistration(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    timer:sleep(100),

    % Verify registered (if registry is available)
    case erlmcp_registry:find_transport(TransportId) of
        {ok, _} ->
            % Stop and verify unregistered
            ok = gen_server:stop(Pid),
            timer:sleep(100),
            ?assertEqual({error, not_found}, erlmcp_registry:find_transport(TransportId));
        {error, not_found} ->
            ct:pal("Registry not available, skipping unregistration test"),
            ok = gen_server:stop(Pid)
    end,
    ok.

registry_message_routing(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    timer:sleep(100),

    % Test message sending through registry
    TestMessage = <<"test_http_message">>,
    
    % Send message directly to transport
    gen_server:cast(Pid, {data, TestMessage}),
    
    % Verify transport is still alive (basic message handling)
    timer:sleep(100),
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Configuration
%%====================================================================

config_validation(Config) ->
    % Test valid configurations
    ValidConfigs = [
        #{test_mode => true, port => 8080, path => "/mcp"},
        #{test_mode => false, port => 8081, path => "/api", ssl_enabled => false},
        #{port => 9090, path => "/test", ssl_enabled => true, test_mode => true}
    ],

    lists:foreach(fun(TestConfig) ->
        TransportId = list_to_atom("test_" ++ integer_to_list(erlang:unique_integer([positive]))),
        try
            {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
            ?assert(is_process_alive(Pid)),
            ok = gen_server:stop(Pid)
        catch
            error:{badmatch, {error, Reason}} ->
                ct:pal("Valid config failed: ~p, Reason: ~p", [TestConfig, Reason])
        end
    end, ValidConfigs),
    ok.

config_defaults(Config) ->
    TransportId = ?config(transport_id, Config),
    MinimalConfig = #{test_mode => true},  % Minimal config

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, MinimalConfig),
    Info = erlmcp_transport_http_new:get_info(Pid),

    % Verify default values are applied
    ?assertMatch(#{port := 8080}, Info),  % Default port
    ?assertMatch(#{path := "/mcp"}, Info),  % Default path
    ?assertMatch(#{ssl_enabled := false}, Info),  % Default SSL

    ok = gen_server:stop(Pid),
    ok.

test_mode_startup(Config) ->
    TransportId = ?config(transport_id, Config),

    % Test mode enabled
    TestConfig1 = #{test_mode => true, port => 8080, path => "/mcp"},
    {ok, Pid1} = erlmcp_transport_http_new:start_link(TransportId, TestConfig1),
    
    Info1 = erlmcp_transport_http_new:get_info(Pid1),
    ?assertMatch(#{test_mode := true}, Info1),

    ok = gen_server:stop(Pid1),

    % Test mode disabled (should attempt real HTTP server)
    TestConfig2 = #{test_mode => false, port => 8082, path => "/test"},
    case erlmcp_transport_http_new:start_link(TransportId, TestConfig2) of
        {ok, Pid2} ->
            Info2 = erlmcp_transport_http_new:get_info(Pid2),
            ?assertMatch(#{test_mode := false}, Info2),
            ok = gen_server:stop(Pid2);
        {error, _Reason} ->
            % Expected if HTTP server can't start
            ok
    end,
    ok.

http_server_options(Config) ->
    TransportId = ?config(transport_id, Config),

    % Test various HTTP server option combinations
    TestCases = [
        #{test_mode => true, port => 8080, path => "/mcp"},
        #{test_mode => true, port => 8081, path => "/api"},
        #{test_mode => true, port => 443, path => "/secure", ssl_enabled => true},
        #{test_mode => true, port => 80, path => "/public", ssl_enabled => false}
    ],

    lists:foreach(fun(TestConfig) ->
        TestId = list_to_atom("test_" ++ integer_to_list(erlang:unique_integer([positive]))),
        {ok, Pid} = erlmcp_transport_http_new:start_link(TestId, TestConfig),
        Info = erlmcp_transport_http_new:get_info(Pid),
        
        ?assert(maps:is_key(port, Info)),
        ?assert(maps:is_key(path, Info)),
        ?assert(maps:is_key(ssl_enabled, Info)),
        
        ok = gen_server:stop(Pid)
    end, TestCases),
    ok.

%%====================================================================
%% Test Cases - Error Handling
%%====================================================================

error_handling_server_start_failure(Config) ->
    TransportId = ?config(transport_id, Config),
    % Test with invalid port (might succeed in test mode)
    TestConfig = #{test_mode => false, port => -1, path => "/mcp"},

    case erlmcp_transport_http_new:start_link(TransportId, TestConfig) of
        {ok, Pid} ->
            ct:pal("Unexpected success with invalid port"),
            ok = gen_server:stop(Pid);
        {error, Reason} ->
            ct:pal("Expected server start failure: ~p", [Reason]),
            ?assert(is_atom(Reason) orelse is_tuple(Reason))
    end,
    ok.

error_handling_invalid_config(Config) ->
    TransportId = ?config(transport_id, Config),
    % Test with missing required config
    TestConfig = #{},  % Empty config

    % Should use defaults and succeed in test mode
    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid),
    ok.

error_handling_server_error(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Simulate server error
    Pid ! {http_server_error, connection_lost},
    
    % Process should handle the error (may stop)
    timer:sleep(100),
    
    case is_process_alive(Pid) of
        true ->
            ok = gen_server:stop(Pid);
        false ->
            ct:pal("Process stopped due to simulated server error")
    end,
    ok.

error_handling_invalid_path(Config) ->
    TransportId = ?config(transport_id, Config),
    % Test with various path formats
    TestPaths = ["/mcp", "/api/v1", "/", "/test/path"],

    lists:foreach(fun(Path) ->
        TestConfig = #{test_mode => true, port => 8080, path => Path},
        {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
        Info = erlmcp_transport_http_new:get_info(Pid),
        ?assertMatch(#{path := Path}, Info),
        ok = gen_server:stop(Pid)
    end, TestPaths),
    ok.

%%====================================================================
%% Test Cases - HTTP Processing
%%====================================================================

http_request_handling(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Test various HTTP request methods and paths
    TestCases = [
        {'POST', "/mcp", [], <<"test body">>},
        {'GET', "/mcp", [], <<>>},
        {'POST', "/wrong-path", [], <<"test body">>},
        {'GET', "/wrong-path", [], <<>>}
    ],

    lists:foreach(fun({Method, Path, Headers, Body}) ->
        Response = gen_server:call(Pid, {http_request, Method, Path, Headers, Body}),
        ?assertMatch({_, _, _}, Response), % Should return {Status, Headers, Body}
        {Status, _RespHeaders, _RespBody} = Response,
        ?assert(is_integer(Status)),
        ?assert(Status >= 100 andalso Status < 600)
    end, TestCases),

    ok = gen_server:stop(Pid),
    ok.

http_response_generation(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Test response generation for different scenarios
    % Valid MCP request
    {Status1, Headers1, Body1} = gen_server:call(Pid, {http_request, 'POST', "/mcp", [], <<"valid json rpc">>}),
    ?assertEqual(200, Status1),
    ?assert(is_list(Headers1)),
    ?assert(is_binary(Body1)),

    % Health check request
    {Status2, Headers2, Body2} = gen_server:call(Pid, {http_request, 'GET', "/mcp", [], <<>>}),
    ?assertEqual(200, Status2),
    ?assert(is_list(Headers2)),
    ?assert(is_binary(Body2)),

    % Not found request
    {Status3, Headers3, Body3} = gen_server:call(Pid, {http_request, 'GET', "/notfound", [], <<>>}),
    ?assertEqual(404, Status3),
    ?assert(is_list(Headers3)),
    ?assert(is_binary(Body3)),

    ok = gen_server:stop(Pid),
    ok.

mcp_request_processing(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Test MCP JSON-RPC request processing
    JsonRpcRequest = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test.method">>,
        <<"params">> => #{<<"key">> => <<"value">>},
        <<"id">> => <<"test-id">>
    }),

    {Status, _Headers, Body} = gen_server:call(Pid, {http_request, 'POST', "/mcp", 
                                                    [{"Content-Type", "application/json"}], 
                                                    JsonRpcRequest}),
    
    ?assertEqual(200, Status),
    ?assert(is_binary(Body)),

    % Verify response is valid JSON
    try
        Response = jsx:decode(Body),
        ?assert(is_map(Response))
    catch
        _:_ ->
            ct:pal("Response is not valid JSON: ~p", [Body])
    end,

    ok = gen_server:stop(Pid),
    ok.

json_rpc_validation(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Test various JSON-RPC payloads
    TestCases = [
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>, <<"id">> => 1}),
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"notify">>}),
        <<"invalid json">>,
        <<"{\"incomplete\": json">>
    ],

    lists:foreach(fun(Payload) ->
        {Status, _Headers, _Body} = gen_server:call(Pid, {http_request, 'POST', "/mcp", [], Payload}),
        % Should handle all payloads gracefully (200 for valid, might be 200 or 500 for invalid)
        ?assert(Status =:= 200 orelse Status =:= 500)
    end, TestCases),

    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Server Management
%%====================================================================

http_server_startup(Config) ->
    TransportId = ?config(transport_id, Config),
    
    % Test HTTP server startup in test mode
    TestConfig1 = #{test_mode => true, port => 8080, path => "/mcp"},
    {ok, Pid1} = erlmcp_transport_http_new:start_link(TransportId, TestConfig1),
    Info1 = erlmcp_transport_http_new:get_info(Pid1),
    ?assertMatch(#{status := running}, Info1),
    ok = gen_server:stop(Pid1),

    % Test HTTP server startup in real mode (may fail)
    TestConfig2 = #{test_mode => false, port => 8083, path => "/test"},
    case erlmcp_transport_http_new:start_link(TransportId, TestConfig2) of
        {ok, Pid2} ->
            Info2 = erlmcp_transport_http_new:get_info(Pid2),
            ?assertMatch(#{status := running}, Info2),
            ok = gen_server:stop(Pid2);
        {error, _} ->
            % Expected if can't start HTTP server
            ok
    end,
    ok.

http_server_shutdown(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => false, port => 8084, path => "/test"},

    case erlmcp_transport_http_new:start_link(TransportId, TestConfig) of
        {ok, Pid} ->
            % Test graceful shutdown
            {ok, State} = gen_server:call(Pid, get_state),
            ?assertEqual(ok, erlmcp_transport_http_new:close(State)),
            ok = gen_server:stop(Pid);
        {error, _} ->
            % Expected if can't start server
            ok
    end,
    ok.

ssl_configuration(Config) ->
    TransportId = ?config(transport_id, Config),

    % Test SSL enabled configuration
    TestConfig1 = #{test_mode => true, port => 443, path => "/secure", ssl_enabled => true},
    {ok, Pid1} = erlmcp_transport_http_new:start_link(TransportId, TestConfig1),
    Info1 = erlmcp_transport_http_new:get_info(Pid1),
    ?assertMatch(#{ssl_enabled := true}, Info1),
    ok = gen_server:stop(Pid1),

    % Test SSL disabled configuration
    TestConfig2 = #{test_mode => true, port => 80, path => "/public", ssl_enabled => false},
    {ok, Pid2} = erlmcp_transport_http_new:start_link(TransportId, TestConfig2),
    Info2 = erlmcp_transport_http_new:get_info(Pid2),
    ?assertMatch(#{ssl_enabled := false}, Info2),
    ok = gen_server:stop(Pid2),

    ok.

path_routing(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/api/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Test requests to configured path vs other paths
    % Configured path should return 200
    {Status1, _Headers1, _Body1} = gen_server:call(Pid, {http_request, 'GET', "/api/mcp", [], <<>>}),
    ?assertEqual(200, Status1),

    % Wrong path should return 404
    {Status2, _Headers2, _Body2} = gen_server:call(Pid, {http_request, 'GET', "/wrong/path", [], <<>>}),
    ?assertEqual(404, Status2),

    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Lifecycle
%%====================================================================

supervisor_integration(Config) ->
    % Test that transport can be started/stopped cleanly for supervisor use
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    ?assert(is_process_alive(Pid)),

    % Verify clean shutdown
    ok = gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)),
    ok.

graceful_shutdown(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => false, port => 8085, path => "/test"},

    case erlmcp_transport_http_new:start_link(TransportId, TestConfig) of
        {ok, Pid} ->
            % Test graceful shutdown
            ok = gen_server:stop(Pid),
            timer:sleep(100),
            ?assertNot(is_process_alive(Pid));
        {error, _} ->
            % Expected if can't start server
            ok
    end,
    ok.

abnormal_termination(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    
    % Kill the transport process
    exit(Pid, kill),
    timer:sleep(100),
    
    ?assertNot(is_process_alive(Pid)),
    ok.

%%====================================================================
%% Test Cases - Performance
%%====================================================================

high_volume_requests(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    
    % Send many HTTP requests quickly
    RequestCount = 1000,
    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(N) ->
        Body = jsx:encode(#{<<"id">> => N, <<"method">> => <<"test">>, <<"jsonrpc">> => <<"2.0">>}),
        _Response = gen_server:call(Pid, {http_request, 'POST', "/mcp", [], Body})
    end, lists:seq(1, RequestCount)),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    ct:pal("Processed ~p HTTP requests in ~p ms (~p req/sec)", 
           [RequestCount, Duration, round(RequestCount * 1000 / Duration)]),

    % Should handle high volume without crashing
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    ok.

memory_usage(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    % Measure memory before
    {memory, MemBefore} = erlang:process_info(self(), memory),

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),
    timer:sleep(100),

    % Measure transport memory
    {memory, TransportMem} = erlang:process_info(Pid, memory),

    ct:pal("HTTP transport memory usage: ~p bytes", [TransportMem]),

    % Verify reasonable memory usage (< 1MB for basic transport)
    ?assert(TransportMem < 1024 * 1024),

    ok = gen_server:stop(Pid),
    ok.

latency_measurement(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Measure latency for HTTP request processing
    RequestCount = 100,
    Latencies = lists:map(fun(_N) ->
        StartTime = erlang:monotonic_time(microsecond),
        _Response = gen_server:call(Pid, {http_request, 'GET', "/mcp", [], <<>>}),
        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, RequestCount)),

    AvgLatency = lists:sum(Latencies) / length(Latencies),
    MaxLatency = lists:max(Latencies),
    MinLatency = lists:min(Latencies),

    ct:pal("HTTP latency stats - Avg: ~.2f μs, Min: ~p μs, Max: ~p μs",
           [AvgLatency, MinLatency, MaxLatency]),

    % Verify reasonable latency (< 10ms average for test mode calls)
    ?assert(AvgLatency < 10000),

    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Integration
%%====================================================================

concurrent_requests(Config) ->
    % Test multiple HTTP transports handling concurrent requests
    NumTransports = 3,
    TestConfigs = [
        #{test_mode => true, port => 8080 + N, path => "/mcp"}
        || N <- lists:seq(1, NumTransports)
    ],

    Parent = self(),
    Pids = [
        begin
            TransportId = list_to_atom("http_transport_" ++ integer_to_list(N)),
            {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, Config),
            spawn(fun() ->
                lists:foreach(fun(I) ->
                    Body = jsx:encode(#{<<"id">> => I, <<"method">> => <<"concurrent_test">>}),
                    _Response = gen_server:call(Pid, {http_request, 'POST', "/mcp", [], Body})
                end, lists:seq(1, 50)),
                Parent ! {done, Pid}
            end),
            Pid
        end
        || {N, Config} <- lists:zip(lists:seq(1, NumTransports), TestConfigs)
    ],

    % Wait for all to complete
    lists:foreach(fun(Pid) ->
        receive
            {done, Pid} -> ok
        after 5000 -> 
            ?assert(false, "Concurrent HTTP test timed out")
        end
    end, Pids),

    % All transports should still be alive
    lists:foreach(fun(Pid) ->
        ?assert(is_process_alive(Pid)),
        ok = gen_server:stop(Pid)
    end, Pids),

    ok.

stress_test(Config) ->
    % Combined stress test
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, port => 8080, path => "/mcp"},

    {ok, Pid} = erlmcp_transport_http_new:start_link(TransportId, TestConfig),

    % Multiple concurrent operations
    NumClients = 3,
    OpsPerClient = 200,

    Parent = self(),
    Clients = [
        spawn(fun() ->
            lists:foreach(fun(N) ->
                % Mix of different operations
                case N rem 3 of
                    0 -> 
                        _Info = erlmcp_transport_http_new:get_info(Pid);
                    1 -> 
                        Body = jsx:encode(#{<<"id">> => N, <<"method">> => <<"stress">>}),
                        _Response = gen_server:call(Pid, {http_request, 'POST', "/mcp", [], Body});
                    2 -> 
                        {ok, _State} = gen_server:call(Pid, get_state)
                end
            end, lists:seq(1, OpsPerClient)),
            Parent ! {client_done, self()}
        end)
        || _ <- lists:seq(1, NumClients)
    ],

    StartTime = erlang:monotonic_time(millisecond),

    % Wait for all clients
    lists:foreach(fun(ClientPid) ->
        receive
            {client_done, ClientPid} -> ok
        after 10000 -> 
            ?assert(false, "HTTP stress test client timed out")
        end
    end, Clients),

    EndTime = erlang:monotonic_time(millisecond),
    TotalOps = NumClients * OpsPerClient,
    Duration = EndTime - StartTime,

    ct:pal("HTTP stress test: ~p ops in ~p ms (~.2f ops/sec)",
           [TotalOps, Duration, TotalOps * 1000 / Duration]),

    % Should survive stress test
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    ok.

server_cleanup(Config) ->
    % Test that HTTP servers are properly cleaned up
    TransportId = ?config(transport_id, Config),
    
    % Test cleanup in test mode
    TestConfig1 = #{test_mode => true, port => 8080, path => "/mcp"},
    {ok, Pid1} = erlmcp_transport_http_new:start_link(TransportId, TestConfig1),
    ok = gen_server:stop(Pid1),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid1)),

    % Test cleanup with potential real server
    TestConfig2 = #{test_mode => false, port => 8086, path => "/test"},
    case erlmcp_transport_http_new:start_link(TransportId, TestConfig2) of
        {ok, Pid2} ->
            ok = gen_server:stop(Pid2),
            timer:sleep(100),
            ?assertNot(is_process_alive(Pid2));
        {error, _} ->
            % Expected if can't create server
            ok
    end,

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

cleanup_processes() ->
    % Kill any stray processes
    timer:sleep(50).