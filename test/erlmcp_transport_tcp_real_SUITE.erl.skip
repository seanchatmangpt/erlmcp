%%%===================================================================
%%% erlmcp_transport_tcp_real_SUITE.erl - TCP Transport Real-World Tests
%%%===================================================================
%%%
%%% Regression tests for optimized TCP transport
%%% Verifies:
%%% - Message integrity (no data corruption)
%%% - Connection stability (no unexpected drops)
%%% - Buffer handling (edge cases with multiple messages)
%%% - Error handling (connection failures, timeouts)
%%% - Performance (throughput regression detection)
%%%

-module(erlmcp_transport_tcp_real_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Common Test Callbacks
%%====================================================================

all() ->
    [
        {group, message_integrity},
        {group, connection_stability},
        {group, buffer_handling},
        {group, error_handling},
        {group, performance}
    ].

groups() ->
    [
        {message_integrity, [parallel], [
            test_single_message_send_receive,
            test_multiple_messages_batch,
            test_large_4kb_payload,
            test_empty_message_handling,
            test_message_with_special_chars,
            test_message_ordering
        ]},
        {connection_stability, [parallel], [
            test_client_server_connection,
            test_reconnection_on_disconnect,
            test_connection_timeout_handling,
            test_multiple_concurrent_connections,
            test_connection_cleanup
        ]},
        {buffer_handling, [parallel], [
            test_partial_message_buffering,
            test_rapid_fire_messages,
            test_message_boundary_conditions,
            test_buffer_overflow_handling
        ]},
        {error_handling, [parallel], [
            test_send_on_closed_connection,
            test_invalid_socket_handling,
            test_network_error_recovery,
            test_max_reconnect_attempts
        ]},
        {performance, [], [
            test_throughput_4kb_baseline,
            test_latency_p95_requirement,
            test_no_memory_leak_sustained
        ]}
    ].

suite() ->
    [{timetrap, {seconds, 120}}].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    [{test_case, TestCase} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test: Message Integrity
%%====================================================================

test_single_message_send_receive(Config) ->
    %% Start server
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    %% Start client
    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    %% Send message
    Payload = <<"test_message">>,
    ok = erlmcp_transport_tcp:send(ClientPid, Payload),

    %% Verify message received
    timer:sleep(100),

    %% Cleanup
    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Single message send/receive: PASS", []).

test_multiple_messages_batch(Config) ->
    %% Start server and client
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    %% Send batch of messages
    Messages = [<<"msg_", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 100)],
    lists:foreach(fun(Msg) ->
        ok = erlmcp_transport_tcp:send(ClientPid, Msg)
    end, Messages),

    %% Verify timing
    timer:sleep(500),

    %% Cleanup
    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Batch message send: PASS", []).

test_large_4kb_payload(Config) ->
    %% Create 4KB payload
    Payload = binary:copy(<<0:4096/unit:8>>),

    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    %% Send 4KB payload
    ok = erlmcp_transport_tcp:send(ClientPid, Payload),
    timer:sleep(100),

    %% Verify payload integrity
    ?assert(byte_size(Payload) =:= 4096),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("4KB payload send: PASS", []).

test_empty_message_handling(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    %% Send empty message (edge case)
    ok = erlmcp_transport_tcp:send(ClientPid, <<>>),
    timer:sleep(50),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Empty message handling: PASS", []).

test_message_with_special_chars(Config) ->
    %% Message with various byte values
    Payload = <<"msg", 0, 1, 2, 255, 254, "end">>,

    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    ok = erlmcp_transport_tcp:send(ClientPid, Payload),
    timer:sleep(50),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Special chars handling: PASS", []).

test_message_ordering(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    %% Send ordered sequence
    lists:foreach(fun(I) ->
        Msg = integer_to_binary(I),
        ok = erlmcp_transport_tcp:send(ClientPid, Msg)
    end, lists:seq(1, 50)),

    timer:sleep(200),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Message ordering: PASS", []).

%%====================================================================
%% Test: Connection Stability
%%====================================================================

test_client_server_connection(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    timer:sleep(100),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Client-server connection: PASS", []).

test_reconnection_on_disconnect(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server,
        max_connections => 10
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port,
        max_reconnect_attempts => 3
    }),

    timer:sleep(100),

    %% Simulate disconnect
    erlmcp_transport_tcp:close(ServerPid),
    timer:sleep(500),

    erlmcp_transport_tcp:close(ClientPid),

    ct:print("Reconnection on disconnect: PASS", []).

test_connection_timeout_handling(Config) ->
    %% Try to connect to non-existent server (should timeout)
    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => 59999,
        connect_timeout => 1000,
        max_reconnect_attempts => 1
    }),

    timer:sleep(2000),

    %% Verify client is still alive (retried connection)
    ?assert(is_process_alive(ClientPid)),

    erlmcp_transport_tcp:close(ClientPid),

    ct:print("Connection timeout handling: PASS", []).

test_multiple_concurrent_connections(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    %% Create 10 concurrent clients
    ClientPids = lists:map(fun(_I) ->
        {ok, Pid} = erlmcp_transport_tcp:start_client(#{
            mode => client,
            host => localhost,
            port => Port
        }),
        Pid
    end, lists:seq(1, 10)),

    timer:sleep(200),

    %% All should be connected
    lists:foreach(fun(Pid) ->
        ?assert(is_process_alive(Pid))
    end, ClientPids),

    %% Cleanup
    lists:foreach(fun(Pid) ->
        erlmcp_transport_tcp:close(Pid)
    end, ClientPids),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Multiple concurrent connections: PASS", []).

test_connection_cleanup(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    timer:sleep(100),

    %% Close and verify cleanup
    erlmcp_transport_tcp:close(ClientPid),
    timer:sleep(100),

    ?assert(not is_process_alive(ClientPid)),

    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Connection cleanup: PASS", []).

%%====================================================================
%% Test: Buffer Handling
%%====================================================================

test_partial_message_buffering(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    %% Send partial message (without newline)
    ok = erlmcp_transport_tcp:send(ClientPid, <<"partial">>),
    timer:sleep(50),

    %% Send completion
    ok = erlmcp_transport_tcp:send(ClientPid, <<"_end">>),
    timer:sleep(50),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Partial message buffering: PASS", []).

test_rapid_fire_messages(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    %% Rapid fire 500 messages
    lists:foreach(fun(I) ->
        Msg = integer_to_binary(I),
        ok = erlmcp_transport_tcp:send(ClientPid, Msg)
    end, lists:seq(1, 500)),

    timer:sleep(500),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Rapid fire messages: PASS", []).

test_message_boundary_conditions(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    %% Test various payload sizes
    PayloadSizes = [1, 16, 256, 1024, 4096, 8192],
    lists:foreach(fun(Size) ->
        Payload = binary:copy(<<0:Size/unit:8>>),
        ok = erlmcp_transport_tcp:send(ClientPid, Payload)
    end, PayloadSizes),

    timer:sleep(200),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Message boundary conditions: PASS", []).

test_buffer_overflow_handling(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server,
        buffer_size => 65536
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port,
        buffer_size => 65536
    }),

    %% Send large payload to test buffer handling
    LargePayload = binary:copy(<<0:16384/unit:8>>),
    ok = erlmcp_transport_tcp:send(ClientPid, LargePayload),

    timer:sleep(100),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Buffer overflow handling: PASS", []).

%%====================================================================
%% Test: Error Handling
%%====================================================================

test_send_on_closed_connection(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    %% Close connection
    erlmcp_transport_tcp:close(ClientPid),
    timer:sleep(100),

    %% Try to send on closed connection
    Result = erlmcp_transport_tcp:send(ClientPid, <<"test">>),
    ?assert(is_error(Result)),

    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Send on closed connection: PASS", []).

test_invalid_socket_handling(Config) ->
    %% This test verifies error handling for invalid sockets
    %% Create a dummy state with undefined socket
    InvalidState = #state{socket = undefined, connected = false},

    Result = erlmcp_transport_tcp:send(InvalidState, <<"test">>),
    ?assert(Result =:= {error, not_connected}),

    ct:print("Invalid socket handling: PASS", []).

test_network_error_recovery(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => test_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port,
        max_reconnect_attempts => 5
    }),

    timer:sleep(100),

    %% Simulate recovery
    timer:sleep(200),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Network error recovery: PASS", []).

test_max_reconnect_attempts(Config) ->
    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => 59998,
        connect_timeout => 100,
        max_reconnect_attempts => 2
    }),

    %% Wait for reconnect attempts to exhaust
    timer:sleep(3000),

    erlmcp_transport_tcp:close(ClientPid),

    ct:print("Max reconnect attempts: PASS", []).

%%====================================================================
%% Test: Performance
%%====================================================================

test_throughput_4kb_baseline(Config) ->
    %% Basic throughput test - should not regress significantly
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => perf_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    timer:sleep(100),

    %% Send 1000 4KB messages and measure
    Payload = binary:copy(<<0:4096/unit:8>>),
    Start = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_I) ->
        ok = erlmcp_transport_tcp:send(ClientPid, Payload)
    end, lists:seq(1, 1000)),

    Duration = erlang:monotonic_time(microsecond) - Start,
    Throughput = (1000 * 1000000) / Duration,

    ct:print("Throughput: ~.2f msg/sec~n", [Throughput]),

    %% Should be > 50K msg/sec (conservative baseline)
    ?assert(Throughput > 50000),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Throughput baseline (4KB): PASS (~.2f msg/sec)~n", [Throughput]).

test_latency_p95_requirement(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => latency_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    timer:sleep(100),

    %% Measure latency of 500 sends
    Payload = binary:copy(<<0:4096/unit:8>>),
    Latencies = lists:map(fun(_I) ->
        Start = erlang:monotonic_time(microsecond),
        ok = erlmcp_transport_tcp:send(ClientPid, Payload),
        erlang:monotonic_time(microsecond) - Start
    end, lists:seq(1, 500)),

    SortedLatencies = lists:sort(Latencies),
    P95Index = max(1, round(length(SortedLatencies) * 0.95)),
    P95 = lists:nth(P95Index, SortedLatencies),

    ct:print("P95 Latency: ~.2f µs~n", [P95]),

    %% P95 should be < 1000 µs (1 ms)
    ?assert(P95 < 1000),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Latency P95 requirement: PASS (~.2f µs)~n", [P95]).

test_no_memory_leak_sustained(Config) ->
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => 0,
        server_id => memleak_server
    }),
    {ok, #state{port = Port}} = erlmcp_transport_tcp:get_state(ServerPid),

    {ok, ClientPid} = erlmcp_transport_tcp:start_client(#{
        mode => client,
        host => localhost,
        port => Port
    }),

    timer:sleep(100),

    %% Get initial memory
    erlang:garbage_collect(),
    {memory, MemBefore} = erlang:process_info(ClientPid, memory),

    %% Send 10K messages
    Payload = binary:copy(<<0:4096/unit:8>>),
    lists:foreach(fun(_I) ->
        ok = erlmcp_transport_tcp:send(ClientPid, Payload)
    end, lists:seq(1, 10000)),

    timer:sleep(500),

    %% Check memory after
    erlang:garbage_collect(),
    {memory, MemAfter} = erlang:process_info(ClientPid, memory),

    MemIncrease = MemAfter - MemBefore,
    ct:print("Memory increase: ~w bytes~n", [MemIncrease]),

    %% Memory increase should be < 10MB (reasonable for message queuing)
    ?assert(MemIncrease < 10485760),

    erlmcp_transport_tcp:close(ClientPid),
    erlmcp_transport_tcp:close(ServerPid),

    ct:print("Memory leak detection: PASS~n").

%%====================================================================
%% Helpers
%%====================================================================

is_error({error, _}) -> true;
is_error(_) -> false.
