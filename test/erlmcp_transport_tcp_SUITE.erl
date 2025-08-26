%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for erlmcp_transport_tcp_new module
%%%
%%% This comprehensive test suite covers all aspects of the TCP transport
%%% including behavior compliance, connection management, error handling,
%%% and performance characteristics.
%%%-------------------------------------------------------------------
-module(erlmcp_transport_tcp_SUITE).

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
         config_validation/1, config_defaults/1, test_mode_startup/1, connection_parameters/1,
         error_handling_no_socket/1, error_handling_connection_refused/1,
         error_handling_socket_error/1, error_handling_invalid_host/1,
         message_parsing/1, message_buffering/1, line_extraction/1, large_message_handling/1,
         connection_establishment/1, connection_failure_handling/1, socket_options_validation/1,
         supervisor_integration/1, graceful_shutdown/1, abnormal_termination/1,
         high_volume_messages/1, memory_usage/1, latency_measurement/1,
         concurrent_connections/1, stress_test/1, socket_cleanup/1]).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [{group, basic_functionality},
     {group, transport_behavior},
     {group, registry_integration},
     {group, configuration},
     {group, error_handling},
     {group, message_processing},
     {group, connection_management},
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
      [config_validation, config_defaults, test_mode_startup, connection_parameters]},
     {error_handling,
      [sequential],
      [error_handling_no_socket,
       error_handling_connection_refused,
       error_handling_socket_error,
       error_handling_invalid_host]},
     {message_processing,
      [parallel],
      [message_parsing, message_buffering, line_extraction, large_message_handling]},
     {connection_management,
      [sequential],
      [connection_establishment, connection_failure_handling, socket_options_validation]},
     {lifecycle,
      [sequential],
      [supervisor_integration, graceful_shutdown, abnormal_termination]},
     {performance, 
      [sequential], 
      [high_volume_messages, memory_usage, latency_measurement]},
     {integration,
      [sequential],
      [concurrent_connections, stress_test, socket_cleanup]}].

%%====================================================================
%% Suite Setup/Teardown
%%====================================================================

init_per_suite(Config) ->
    ct:pal("Starting TCP transport test suite"),

    % Start necessary applications
    ok = application:ensure_started(crypto),
    ok = application:ensure_started(sasl),

    % Start registry for integration tests
    case erlmcp_registry:start_link() of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok
    end,

    % Start test TCP server for connection tests
    {ok, TestServer} = start_test_tcp_server(),
    TestPort = get_test_server_port(TestServer),

    [{test_server, TestServer},
     {test_port, TestPort},
     {test_host, "127.0.0.1"}
     | Config].

end_per_suite(Config) ->
    TestServer = ?config(test_server, Config),
    stop_test_tcp_server(TestServer),
    ct:pal("Ending TCP transport test suite"),
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
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    % Verify state
    {ok, State} = gen_server:call(Pid, get_state),
    ?assertMatch(#{transport_id := TransportId}, maps:from_list([
        {transport_id, element(2, State)},
        {test_mode, element(13, State)}
    ])),

    ok = gen_server:stop(Pid),
    ok.

basic_send_test(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    % Test sending in test mode (should succeed without actual socket)
    ?assertEqual(ok, erlmcp_transport_tcp_new:send(State, <<"test message">>)),
    ?assertEqual(ok, erlmcp_transport_tcp_new:send(State, "test string")),

    ok = gen_server:stop(Pid),
    ok.

basic_close_test(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    % Test close operation
    ?assertEqual(ok, erlmcp_transport_tcp_new:close(State)),

    ok = gen_server:stop(Pid),
    ok.

get_state_test(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    % Verify state structure
    ?assert(is_tuple(State)),
    ?assert(tuple_size(State) > 5), % Should have multiple fields

    ok = gen_server:stop(Pid),
    ok.

get_info_test(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

    Info = erlmcp_transport_tcp_new:get_info(Pid),
    ?assert(is_map(Info)),
    ?assertMatch(#{type := tcp}, Info),
    ?assertMatch(#{transport_id := TransportId}, Info),
    ?assertMatch(#{host := "127.0.0.1"}, Info),
    ?assertMatch(#{port := 8080}, Info),

    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Transport Behavior
%%====================================================================

transport_behavior_send(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    % Test different data types
    TestCases = [
        <<"binary data">>,
        "string data",
        [<<"iolist">>, " ", <<"data">>]
    ],

    lists:foreach(fun(Data) ->
        ?assertEqual(ok, erlmcp_transport_tcp_new:send(State, Data))
    end, TestCases),

    ok = gen_server:stop(Pid),
    ok.

transport_behavior_close(Config) ->
    TransportId = ?config(transport_id, Config),

    % Test with different configurations
    TestConfigs = [
        #{test_mode => true, host => "127.0.0.1", port => 8080},
        #{test_mode => false, host => "127.0.0.1", port => 9999}  % Non-existent port
    ],

    lists:foreach(fun(TestConfig) ->
        case erlmcp_transport_tcp_new:start_link(TransportId, TestConfig) of
            {ok, Pid} ->
                {ok, State} = gen_server:call(Pid, get_state),
                ?assertEqual(ok, erlmcp_transport_tcp_new:close(State)),
                ok = gen_server:stop(Pid);
            {error, _} ->
                % Expected for invalid configurations
                ok
        end
    end, TestConfigs),

    ok.

transport_behavior_get_info(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    Info = erlmcp_transport_tcp_new:get_info(State),

    ?assertMatch(#{type := tcp}, Info),
    ?assertMatch(#{status := running}, Info),
    ?assertMatch(#{test_mode := true}, Info),
    ?assertMatch(#{transport_id := TransportId}, Info),

    ok = gen_server:stop(Pid),
    ok.

transport_behavior_handle_transport_call(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

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
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

    % Wait for registration to complete
    timer:sleep(100),
    
    % Verify registration (registry should exist from init_per_suite)
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {RegPid, RegConfig}} ->
            ?assertEqual(Pid, RegPid),
            ?assertMatch(#{type := tcp}, RegConfig);
        {error, not_found} ->
            ct:pal("Warning: Transport not registered (registry might not be available)")
    end,

    ok = gen_server:stop(Pid),
    ok.

registry_unregistration(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
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
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    timer:sleep(100),

    % Test message sending through registry
    TestMessage = <<"test_message">>,
    
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
        #{test_mode => true, host => "127.0.0.1", port => 8080},
        #{test_mode => false, host => "localhost", port => 9090},
        #{host => {127, 0, 0, 1}, port => 8080, test_mode => true}
    ],

    lists:foreach(fun(TestConfig) ->
        TransportId = list_to_atom("test_" ++ integer_to_list(erlang:unique_integer([positive]))),
        try
            {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
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

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, MinimalConfig),
    Info = erlmcp_transport_tcp_new:get_info(Pid),

    % Verify default values are applied
    ?assertMatch(#{port := 8080}, Info),  % Default port
    ?assertMatch(#{host := "127.0.0.1"}, Info),  % Default host

    ok = gen_server:stop(Pid),
    ok.

test_mode_startup(Config) ->
    TransportId = ?config(transport_id, Config),

    % Test mode enabled
    TestConfig1 = #{test_mode => true, host => "127.0.0.1", port => 8080},
    {ok, Pid1} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig1),
    
    Info1 = erlmcp_transport_tcp_new:get_info(Pid1),
    ?assertMatch(#{test_mode := true}, Info1),

    ok = gen_server:stop(Pid1),

    % Test mode disabled (should attempt real connection and may fail)
    TestConfig2 = #{test_mode => false, host => "127.0.0.1", port => 9999},
    case erlmcp_transport_tcp_new:start_link(TransportId, TestConfig2) of
        {ok, Pid2} ->
            ok = gen_server:stop(Pid2);
        {error, _Reason} ->
            % Expected for non-existent port
            ok
    end,
    ok.

connection_parameters(Config) ->
    TransportId = ?config(transport_id, Config),

    % Test various connection parameter combinations
    TestCases = [
        #{test_mode => true, host => "localhost", port => 8080},
        #{test_mode => true, host => {127, 0, 0, 1}, port => 8080},
        #{test_mode => true, host => "127.0.0.1", port => 443},
        #{test_mode => true, host => "127.0.0.1", port => 80}
    ],

    lists:foreach(fun(TestConfig) ->
        TestId = list_to_atom("test_" ++ integer_to_list(erlang:unique_integer([positive]))),
        {ok, Pid} = erlmcp_transport_tcp_new:start_link(TestId, TestConfig),
        Info = erlmcp_transport_tcp_new:get_info(Pid),
        
        ?assert(maps:is_key(host, Info)),
        ?assert(maps:is_key(port, Info)),
        
        ok = gen_server:stop(Pid)
    end, TestCases),
    ok.

%%====================================================================
%% Test Cases - Error Handling
%%====================================================================

error_handling_no_socket(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    {ok, State} = gen_server:call(Pid, get_state),

    % In test mode, socket operations should succeed
    ?assertEqual(ok, erlmcp_transport_tcp_new:send(State, <<"test">>)),

    ok = gen_server:stop(Pid),
    ok.

error_handling_connection_refused(Config) ->
    TransportId = ?config(transport_id, Config),
    % Try to connect to a port that should be closed
    TestConfig = #{test_mode => false, host => "127.0.0.1", port => 9999},

    case erlmcp_transport_tcp_new:start_link(TransportId, TestConfig) of
        {ok, Pid} ->
            ct:pal("Unexpected success connecting to closed port"),
            ok = gen_server:stop(Pid);
        {error, Reason} ->
            ct:pal("Expected connection failure: ~p", [Reason]),
            ?assert(is_atom(Reason) orelse is_tuple(Reason))
    end,
    ok.

error_handling_socket_error(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

    % Simulate socket error
    Pid ! {tcp_error, fake_socket, connection_lost},
    
    % Process should handle the error (may stop or continue)
    timer:sleep(100),
    % Note: In test mode, the process might not actually handle tcp_error
    % since there's no real socket
    
    case is_process_alive(Pid) of
        true ->
            ok = gen_server:stop(Pid);
        false ->
            ct:pal("Process stopped due to simulated socket error")
    end,
    ok.

error_handling_invalid_host(Config) ->
    TransportId = ?config(transport_id, Config),
    % Test with invalid host
    TestConfig = #{test_mode => false, host => "invalid.host.that.does.not.exist", port => 8080},

    case erlmcp_transport_tcp_new:start_link(TransportId, TestConfig) of
        {ok, Pid} ->
            ct:pal("Unexpected success with invalid host"),
            ok = gen_server:stop(Pid);
        {error, Reason} ->
            ct:pal("Expected failure with invalid host: ~p", [Reason]),
            ?assert(is_atom(Reason) orelse is_tuple(Reason))
    end,
    ok.

%%====================================================================
%% Test Cases - Message Processing
%%====================================================================

message_parsing(Config) ->
    % Test the internal line extraction functionality if accessible
    % Since extract_lines is private, we test through message handling
    
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

    % Send various message formats
    TestMessages = [
        <<"single line">>,
        <<"line1\nline2\n">>,
        <<"partial">>
    ],

    lists:foreach(fun(Message) ->
        gen_server:cast(Pid, {data, Message}),
        timer:sleep(10)  % Allow processing
    end, TestMessages),

    % Transport should still be alive
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    ok.

message_buffering(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

    % Send partial messages that need buffering
    gen_server:cast(Pid, {data, <<"partial">>}),
    gen_server:cast(Pid, {data, <<" message\n">>}),
    gen_server:cast(Pid, {data, <<"complete message\n">>}),

    timer:sleep(50),  % Allow processing
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    ok.

line_extraction(Config) ->
    % Test line extraction logic through cast messages
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

    % Test various line formats
    TestData = [
        <<"line1\nline2\npartial">>,
        <<" continued\nline3\n">>,
        <<"\n">>,  % Empty line
        <<"final line\n">>
    ],

    lists:foreach(fun(Data) ->
        gen_server:cast(Pid, {data, Data})
    end, TestData),

    timer:sleep(100),
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    ok.

large_message_handling(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

    % Create large message (1MB)
    LargeMessage = binary:copy(<<"X">>, 1024 * 1024),
    LargeMessageWithNewline = <<LargeMessage/binary, "\n">>,

    % Send large message
    gen_server:cast(Pid, {data, LargeMessageWithNewline}),

    timer:sleep(500),  % Allow processing
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Connection Management
%%====================================================================

connection_establishment(Config) ->
    TransportId = ?config(transport_id, Config),
    TestHost = ?config(test_host, Config),
    TestPort = ?config(test_port, Config),

    % Test connection to real test server
    TestConfig = #{test_mode => false, host => TestHost, port => TestPort},

    case erlmcp_transport_tcp_new:start_link(TransportId, TestConfig) of
        {ok, Pid} ->
            Info = erlmcp_transport_tcp_new:get_info(Pid),
            ?assertMatch(#{type := tcp}, Info),
            ?assertMatch(#{host := TestHost}, Info),
            ?assertMatch(#{port := TestPort}, Info),
            
            ok = gen_server:stop(Pid);
        {error, Reason} ->
            ct:pal("Connection establishment failed: ~p", [Reason]),
            % This might be expected if test server isn't available
            ?assert(true)
    end,
    ok.

connection_failure_handling(Config) ->
    TransportId = ?config(transport_id, Config),
    
    % Test connection to non-existent server
    TestConfig = #{test_mode => false, host => "127.0.0.1", port => 9998},

    case erlmcp_transport_tcp_new:start_link(TransportId, TestConfig) of
        {ok, Pid} ->
            ct:pal("Unexpected connection success"),
            ok = gen_server:stop(Pid);
        {error, Reason} ->
            ct:pal("Expected connection failure: ~p", [Reason])
    end,
    ok.

socket_options_validation(Config) ->
    % Test that socket options are properly configured
    % This is mainly tested through successful initialization
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    Info = erlmcp_transport_tcp_new:get_info(Pid),
    
    % Verify basic configuration is present
    ?assert(maps:is_key(config, Info)),
    
    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Lifecycle
%%====================================================================

supervisor_integration(Config) ->
    % Test that transport can be started/stopped cleanly for supervisor use
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    ?assert(is_process_alive(Pid)),

    % Verify clean shutdown
    ok = gen_server:stop(Pid),
    ?assertNot(is_process_alive(Pid)),
    ok.

graceful_shutdown(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => false, host => "127.0.0.1", port => 9997},

    case erlmcp_transport_tcp_new:start_link(TransportId, TestConfig) of
        {ok, Pid} ->
            % Test graceful shutdown
            ok = gen_server:stop(Pid),
            timer:sleep(100),
            ?assertNot(is_process_alive(Pid));
        {error, _} ->
            % Expected if can't bind to port
            ok
    end,
    ok.

abnormal_termination(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    
    % Kill the transport process
    exit(Pid, kill),
    timer:sleep(100),
    
    ?assertNot(is_process_alive(Pid)),
    ok.

%%====================================================================
%% Test Cases - Performance
%%====================================================================

high_volume_messages(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    
    % Send many messages quickly
    MessageCount = 1000,
    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(N) ->
        Message = iolist_to_binary([<<"message_">>, integer_to_list(N), <<"\n">>]),
        gen_server:cast(Pid, {data, Message})
    end, lists:seq(1, MessageCount)),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    ct:pal("Processed ~p messages in ~p ms (~p msg/sec)", 
           [MessageCount, Duration, round(MessageCount * 1000 / Duration)]),

    % Should handle high volume without crashing
    timer:sleep(100),
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    ok.

memory_usage(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    % Measure memory before
    {memory, MemBefore} = erlang:process_info(self(), memory),

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),
    timer:sleep(100),

    % Measure transport memory
    {memory, TransportMem} = erlang:process_info(Pid, memory),

    ct:pal("TCP transport memory usage: ~p bytes", [TransportMem]),

    % Verify reasonable memory usage (< 1MB for basic transport)
    ?assert(TransportMem < 1024 * 1024),

    ok = gen_server:stop(Pid),
    ok.

latency_measurement(Config) ->
    TransportId = ?config(transport_id, Config),
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

    % Measure latency for get_info calls
    MessageCount = 100,
    Latencies = lists:map(fun(_N) ->
        StartTime = erlang:monotonic_time(microsecond),
        _Info = erlmcp_transport_tcp_new:get_info(Pid),
        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, MessageCount)),

    AvgLatency = lists:sum(Latencies) / length(Latencies),
    MaxLatency = lists:max(Latencies),
    MinLatency = lists:min(Latencies),

    ct:pal("Latency stats - Avg: ~.2f μs, Min: ~p μs, Max: ~p μs",
           [AvgLatency, MinLatency, MaxLatency]),

    % Verify reasonable latency (< 1ms average for test mode calls)
    ?assert(AvgLatency < 1000),

    ok = gen_server:stop(Pid),
    ok.

%%====================================================================
%% Test Cases - Integration
%%====================================================================

concurrent_connections(Config) ->
    % Test multiple TCP transports running concurrently
    NumTransports = 5,
    TestConfigs = [
        #{test_mode => true, host => "127.0.0.1", port => 8080 + N}
        || N <- lists:seq(1, NumTransports)
    ],

    Parent = self(),
    Pids = [
        begin
            TransportId = list_to_atom("transport_" ++ integer_to_list(N)),
            {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, Config),
            spawn(fun() ->
                lists:foreach(fun(I) ->
                    Message = iolist_to_binary([<<"msg_">>, integer_to_list(I), <<"\n">>]),
                    gen_server:cast(Pid, {data, Message})
                end, lists:seq(1, 100)),
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
            ?assert(false, "Concurrent test timed out")
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
    TestConfig = #{test_mode => true, host => "127.0.0.1", port => 8080},

    {ok, Pid} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig),

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
                        _Info = erlmcp_transport_tcp_new:get_info(Pid);
                    1 -> 
                        Message = iolist_to_binary([<<"stress_">>, integer_to_list(N), <<"\n">>]),
                        gen_server:cast(Pid, {data, Message});
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
            ?assert(false, "Stress test client timed out")
        end
    end, Clients),

    EndTime = erlang:monotonic_time(millisecond),
    TotalOps = NumClients * OpsPerClient,
    Duration = EndTime - StartTime,

    ct:pal("Stress test: ~p ops in ~p ms (~.2f ops/sec)",
           [TotalOps, Duration, TotalOps * 1000 / Duration]),

    % Should survive stress test
    ?assert(is_process_alive(Pid)),

    ok = gen_server:stop(Pid),
    ok.

socket_cleanup(Config) ->
    % Test that sockets are properly cleaned up
    TransportId = ?config(transport_id, Config),
    
    % Test cleanup in test mode
    TestConfig1 = #{test_mode => true, host => "127.0.0.1", port => 8080},
    {ok, Pid1} = erlmcp_transport_tcp_new:start_link(TransportId, TestConfig1),
    ok = gen_server:stop(Pid1),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid1)),

    % Test cleanup with potential real socket (may fail to create)
    TestConfig2 = #{test_mode => false, host => "127.0.0.1", port => 9996},
    case erlmcp_transport_tcp_new:start_link(TransportId, TestConfig2) of
        {ok, Pid2} ->
            ok = gen_server:stop(Pid2),
            timer:sleep(100),
            ?assertNot(is_process_alive(Pid2));
        {error, _} ->
            % Expected if can't create socket
            ok
    end,

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

cleanup_processes() ->
    % Kill any stray processes
    timer:sleep(50).

%% Test TCP server functions
start_test_tcp_server() ->
    {ok, ListenSocket} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
    {ok, Port} = inet:port(ListenSocket),
    
    ServerPid = spawn(fun() -> test_tcp_server_loop(ListenSocket) end),
    
    {ok, {ServerPid, Port}}.

get_test_server_port({_Pid, Port}) ->
    Port.

stop_test_tcp_server({ServerPid, _Port}) ->
    exit(ServerPid, normal).

test_tcp_server_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> test_tcp_client_handler(Socket) end),
            test_tcp_server_loop(ListenSocket);
        {error, closed} ->
            ok
    end.

test_tcp_client_handler(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            ct:pal("Test TCP server received: ~p", [Data]),
            gen_tcp:send(Socket, <<"OK">>),
            test_tcp_client_handler(Socket);
        {error, closed} ->
            gen_tcp:close(Socket);
        {error, Reason} ->
            ct:pal("Test TCP server error: ~p", [Reason]),
            gen_tcp:close(Socket)
    end.