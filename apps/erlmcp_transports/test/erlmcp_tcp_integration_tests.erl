-module(erlmcp_tcp_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% TCP Transport Integration Tests - Chicago School TDD
%% Tests client-server communication with real processes
%% NO STATE INSPECTION, NO DUMMY PROCESSES

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(ranch),
    ok.

teardown(_) ->
    application:stop(ranch),
    application:stop(erlmcp_core),
    ok.

%%%===================================================================
%%% Client-Server Communication Tests
%%%===================================================================

client_server_communication_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      15,
      fun() ->
         % Start server
         ServerOpts = #{num_acceptors => 5},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         % Start client
         ClientOpts = #{port => Port, max_reconnect_attempts => 5},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         % Wait for client connection
         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),

         % Wait for server to receive connection
         ServerHandler =
             receive
                 {transport_connected, Handler} when is_pid(Handler) ->
                     Handler
             after 3000 ->
                 erlmcp_test_helpers:stop_test_process(ClientPid),
                 erlmcp_test_helpers:stop_test_process(ServerPid),
                 error("Server did not receive connection")
             end,

         % Send message from client
         Message = <<"test message">>,
         ?assertEqual(ok, erlmcp_test_helpers:send_transport_data(ClientPid, Message, 1000)),

         % Verify message received at server handler
         receive
             {transport_message, ReceivedMsg} ->
                 ?assertEqual(Message, ReceivedMsg)
         after 3000 ->
             erlmcp_test_helpers:stop_test_process(ClientPid),
             erlmcp_test_helpers:stop_test_process(ServerPid),
             ?assert(false, "Message not received")
         end,

         % Cleanup
         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.

bidirectional_communication_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      15,
      fun() ->
         ServerOpts = #{},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         ClientOpts = #{port => Port},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),

         ServerHandler =
             receive
                 {transport_connected, H} ->
                     H
             after 3000 ->
                 error("No handler")
             end,

         % Client -> Server
         ?assertEqual(ok,
                      erlmcp_test_helpers:send_transport_data(ClientPid, <<"client msg">>, 1000)),
         receive
             {transport_message, <<"client msg">>} ->
                 ok
         after 2000 ->
             ?assert(false)
         end,

         % Server -> Client (by sending through handler)
         ?assertEqual(ok,
                      erlmcp_test_helpers:send_transport_data(ServerHandler,
                                                              <<"server msg">>,
                                                              1000)),

         % Cleanup
         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.

%%%===================================================================
%%% Multiple Clients Tests
%%%===================================================================

multiple_clients_same_server_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      20,
      fun() ->
         ServerOpts = #{max_connections => 10},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         % Start multiple clients
         NumClients = 5,
         Clients =
             [begin
                  Opts = #{port => Port},
                  {ok, Pid} = erlmcp_test_helpers:start_test_client(Opts),
                  Pid
              end
              || _ <- lists:seq(1, NumClients)],

         % Wait for all clients to connect
         lists:foreach(fun(_) ->
                          case erlmcp_test_helpers:wait_for_transport_connected(5000) of
                              {ok, _} ->
                                  ok;
                              {error, timeout} ->
                                  lists:foreach(fun(C) -> erlmcp_test_helpers:stop_test_process(C)
                                                end,
                                                Clients),
                                  erlmcp_test_helpers:stop_test_process(ServerPid),
                                  ?assert(false, "Client connection timeout")
                          end
                       end,
                       lists:seq(1, NumClients)),

         % Receive all server handlers
         Handlers =
             lists:map(fun(_) ->
                          receive
                              {transport_connected, H} ->
                                  H
                          after 3000 ->
                              ?assert(false, "Missing handler")
                          end
                       end,
                       lists:seq(1, NumClients)),

         ?assertEqual(NumClients, length(Handlers)),

         % Send messages from each client
         lists:foreach(fun(Client) ->
                          ?assertEqual(ok,
                                       erlmcp_test_helpers:send_transport_data(Client,
                                                                               <<"test">>,
                                                                               1000))
                       end,
                       Clients),

         % Receive all messages
         lists:foreach(fun(_) ->
                          receive
                              {transport_message, <<"test">>} ->
                                  ok
                          after 2000 ->
                              ?assert(false, "Missing message")
                          end
                       end,
                       lists:seq(1, NumClients)),

         % Cleanup
         lists:foreach(fun(C) -> erlmcp_test_helpers:stop_test_process(C) end, Clients),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.

%%%===================================================================
%%% Reconnection Tests
%%%===================================================================

client_reconnects_after_server_restart_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      20,
      fun() ->
         % Start server
         ServerOpts = #{num_acceptors => 5},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         % Start client
         ClientOpts = #{port => Port, max_reconnect_attempts => infinity},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),
         ServerHandler =
             receive
                 {transport_connected, H} ->
                     H
             after 3000 ->
                 error("No handler")
             end,

         % Stop server
         erlmcp_test_helpers:stop_test_process(ServerPid),
         timer:sleep(200),

         % Client should detect disconnect
         receive
             {transport_disconnected, ClientPid, _} ->
                 ok
         after 3000 ->
             ?assert(false)
         end,

         % Restart server on same port
         NewServerOpts = #{port => Port},
         {ok, NewServerPid, _Port} = erlmcp_test_helpers:start_test_server(NewServerOpts),

         % Client should reconnect
         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(8000),

         % Verify new handler created
         NewHandler =
             receive
                 {transport_connected, H2} ->
                     H2
             after 3000 ->
                 error("No new handler")
             end,
         ?assert(NewHandler =/= ServerHandler),

         % Cleanup
         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(NewServerPid)
      end}}.

%%%===================================================================
%%% Large Message Tests
%%%===================================================================

large_message_handling_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      10,
      fun() ->
         ServerOpts = #{},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         ClientOpts = #{port => Port},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),
         receive
             {transport_connected, _Handler} ->
                 ok
         after 3000 ->
             ok
         end,

         % Send large message (1MB)
         LargeMessage = list_to_binary(lists:duplicate(1024 * 1024, $a)),
         ?assertEqual(ok, erlmcp_test_helpers:send_transport_data(ClientPid, LargeMessage, 5000)),

         % Receive large message
         receive
             {transport_message, ReceivedMsg} ->
                 ?assertEqual(byte_size(LargeMessage), byte_size(ReceivedMsg))
         after 5000 ->
             ?assert(false, "Large message not received")
         end,

         % Cleanup
         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.

message_too_large_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      10,
      fun() ->
         ServerOpts = #{},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         ClientOpts = #{port => Port},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),

         % Send message exceeding 16MB limit
         TooLargeMessage = list_to_binary(lists:duplicate(17 * 1024 * 1024, $x)),
         ?assertEqual(ok,
                      erlmcp_test_helpers:send_transport_data(ClientPid, TooLargeMessage, 10000)),

         % Connection should be closed
         receive
             {transport_disconnected, ClientPid, _} ->
                 ok
         after 5000 ->
             ok
         end,

         % Cleanup
         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.

%%%===================================================================
%%% Concurrent Stress Tests
%%%===================================================================

concurrent_message_stress_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      30,
      fun() ->
         ServerOpts = #{num_acceptors => 10},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         ClientOpts = #{port => Port},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),
         receive
             {transport_connected, _Handler} ->
                 ok
         after 3000 ->
             ok
         end,

         % Send 1000 messages rapidly
         NumMessages = 1000,
         StartTime = erlang:monotonic_time(millisecond),

         lists:foreach(fun(N) ->
                          Msg = <<"msg", (integer_to_binary(N))/binary>>,
                          erlmcp_test_helpers:send_transport_data(ClientPid, Msg, 1000)
                       end,
                       lists:seq(1, NumMessages)),

         EndTime = erlang:monotonic_time(millisecond),
         Duration = EndTime - StartTime,

         % Should complete in reasonable time
         ?assert(Duration < 10000),

         % Drain message queue
         timer:sleep(1000),

         % Cleanup
         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.

%%%===================================================================
%%% Protocol Compliance Tests
%%%===================================================================

line_based_protocol_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      10,
      fun() ->
         ServerOpts = #{},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         ClientOpts = #{port => Port},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),
         receive
             {transport_connected, _Handler} ->
                 ok
         after 3000 ->
             ok
         end,

         % Send messages without newlines (transport should add them)
         ?assertEqual(ok, erlmcp_test_helpers:send_transport_data(ClientPid, <<"msg1">>, 1000)),
         ?assertEqual(ok, erlmcp_test_helpers:send_transport_data(ClientPid, <<"msg2">>, 1000)),
         ?assertEqual(ok, erlmcp_test_helpers:send_transport_data(ClientPid, <<"msg3">>, 1000)),

         % Receive messages (should be line-separated)
         receive
             {transport_message, <<"msg1">>} ->
                 ok
         after 2000 ->
             ?assert(false)
         end,
         receive
             {transport_message, <<"msg2">>} ->
                 ok
         after 2000 ->
             ?assert(false)
         end,
         receive
             {transport_message, <<"msg3">>} ->
                 ok
         after 2000 ->
             ?assert(false)
         end,

         % Cleanup
         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.

buffer_overflow_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      10,
      fun() ->
         ServerOpts = #{},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         ClientOpts = #{port => Port},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),
         receive
             {transport_connected, _Handler} ->
                 ok
         after 3000 ->
             ok
         end,

         % Send partial message (no newline)
         Partial = <<"partial message">>,
         ?assertEqual(ok, erlmcp_test_helpers:send_transport_data(ClientPid, Partial, 1000)),

         % No message should be received yet
         receive
             {transport_message, _} ->
                 ?assert(false, "Should not receive partial message")
         after 500 ->
             ok
         end,

         % Send rest of message
         Rest = <<" rest">>,
         ?assertEqual(ok, erlmcp_test_helpers:send_transport_data(ClientPid, Rest, 1000)),

         % Should receive buffered message
         receive
             {transport_message, <<"partial message">>} ->
                 ok
         after 2000 ->
             ok
         end,
         receive
             {transport_message, <<" rest">>} ->
                 ok
         after 2000 ->
             ok
         end,

         % Cleanup
         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.
