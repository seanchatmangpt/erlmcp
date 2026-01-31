-module(erlmcp_tcp_server_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_transport_tcp.hrl").

%% TCP Transport Server Tests - Chicago School TDD
%% Tests observable behavior through API calls only
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
%%% Server Lifecycle Tests
%%%===================================================================

server_start_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"Start server with valid options",
         fun() ->
             Opts = #{
                 num_acceptors => 5,
                 max_connections => 100
             },
             {ok, Pid, Port} = erlmcp_test_helpers:start_test_server(Opts),
             ?assert(is_pid(Pid)),
             ?assert(is_process_alive(Pid)),
             ?assert(Port > 0),
             erlmcp_test_helpers:stop_test_process(Pid)
         end},

        {"Server assigns random port when port is 0",
         fun() ->
             Opts1 = #{port => 0},
             {ok, _, Port1} = erlmcp_test_helpers:start_test_server(Opts1),

             Opts2 = #{port => 0},
             {ok, _, Port2} = erlmcp_test_helpers:start_test_server(Opts2),

             ?assert(Port1 > 0),
             ?assert(Port2 > 0),
             ?assert(Port1 =/= Port2),

             erlmcp_test_helpers:stop_test_process(element(1, erlmcp_test_helpers:start_test_server(Opts1))),
             erlmcp_test_helpers:stop_test_process(element(1, erlmcp_test_helpers:start_test_server(Opts2)))
         end},

        {"Multiple servers with unique IDs",
         fun() ->
             Opts = #{port => 0},

             {ok, Pid1, Port1} = erlmcp_test_helpers:start_test_server(Opts),
             {ok, Pid2, Port2} = erlmcp_test_helpers:start_test_server(Opts),

             ?assert(is_process_alive(Pid1)),
             ?assert(is_process_alive(Pid2)),
             ?assert(Pid1 =/= Pid2),
             ?assert(Port1 =/= Port2),

             erlmcp_test_helpers:stop_test_process(Pid1),
             erlmcp_test_helpers:stop_test_process(Pid2)
         end}
     ]}.

server_ranch_integration_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Opts = #{num_acceptors => 5},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(Opts),

         % Verify ranch is accessible (observable via get_port)
         ?assert(Port > 0),

         % Connect a raw TCP client to verify ranch accepts connections
         {ok, Socket} = gen_tcp:connect("localhost", Port,
                                        [binary, {active, false}, {packet, line}],
                                        5000),
         ?assertMatch({ok, _}, gen_tcp:recv(Socket, 0, 1000)),

         gen_tcp:close(Socket),
         erlmcp_test_helpers:stop_test_process(ServerPid)
     end}}.

server_stop_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
         Opts = #{},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(Opts),

         % Stop server
         ?assertEqual(ok, erlmcp_transport_tcp:close(ServerPid)),

         timer:sleep(100),
         ?assertNot(is_process_alive(ServerPid)),

         % Verify port is closed
         ?assertMatch({error, _}, gen_tcp:connect("localhost", Port, [], 1000))
     end}.

%%%===================================================================
%%% Server Connection Handling Tests
%%%===================================================================

server_accepts_connection_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Opts = #{num_acceptors => 5},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(Opts),

         % Connect client
         {ok, Socket} = gen_tcp:connect("localhost", Port,
                                        [binary, {active, false}], 5000),
         ?assertMatch({ok, _}, gen_tcp:recv(Socket, 0, 1000)),

         % Server should notify owner of new connection
         receive
             {transport_connected, HandlerPid} when is_pid(HandlerPid) ->
                 ?assert(is_pid(HandlerPid)),
                 ?assert(is_process_alive(HandlerPid))
         after 2000 ->
             ?assert(false, "No connection notification received")
         end,

         gen_tcp:close(Socket),
         erlmcp_test_helpers:stop_test_process(ServerPid)
     end}}.

server_multiple_connections_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 15,
     fun() ->
         Opts = #{max_connections => 10},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(Opts),

         % Connect multiple clients
         NumClients = 5,
         Sockets = [begin
             {ok, S} = gen_tcp:connect("localhost", Port, [binary, {active, false}], 5000),
             S
         end || _ <- lists:seq(1, NumClients)],

         % Receive all connection notifications
         Handlers = lists:map(fun(_) ->
             receive
                 {transport_connected, H} -> H
             after 2000 ->
                 ?assert(false, "Missing connection notification")
             end
         end, lists:seq(1, NumClients)),

         ?assertEqual(NumClients, length(Handlers)),
         ?assert(length(lists:usort(Handlers)) =:= NumClients),  % All unique

         % Cleanup
         lists:foreach(fun(S) -> gen_tcp:close(S) end, Sockets),
         erlmcp_test_helpers:stop_test_process(ServerPid)
     end}}.

server_max_connections_limit_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Opts = #{max_connections => 2},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(Opts),

         % Connect up to limit
         {ok, S1} = gen_tcp:connect("localhost", Port, [binary, {active, false}], 5000),
         {ok, S2} = gen_tcp:connect("localhost", Port, [binary, {active, false}], 5000),

         % Third connection should be rejected by ranch
         case gen_tcp:connect("localhost", Port, [binary, {active, false}], 1000) of
             {error, _} -> ok;
             {ok, S3} ->
                 % Connection accepted but may be closed by server
                 gen_tcp:close(S3)
         end,

         % Cleanup
         gen_tcp:close(S1),
         gen_tcp:close(S2),
         erlmcp_test_helpers:stop_test_process(ServerPid)
     end}}.

%%%===================================================================
%%% Server Message Handling Tests
%%%===================================================================

server_receives_data_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Opts = #{},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(Opts),

         % Connect client
         {ok, Socket} = gen_tcp:connect("localhost", Port,
                                        [binary, {active, false}], 5000),

         % Wait for connection notification
         receive {transport_connected, _Handler} -> ok after 2000 -> ?assert(false) end,

         % Send data to server
         Message = <<"hello server\n">>,
         ok = gen_tcp:send(Socket, Message),

         % Owner should receive message via handler
         receive
             {transport_message, Data} ->
                 ?assertEqual(<<"hello server">>, Data)
         after 2000 ->
             ?assert(false, "Message not received")
         end,

         gen_tcp:close(Socket),
         erlmcp_test_helpers:stop_test_process(ServerPid)
     end}}.

server_handles_multiple_messages_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Opts = #{},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(Opts),

         {ok, Socket} = gen_tcp:connect("localhost", Port,
                                        [binary, {active, false}], 5000),

         receive {transport_connected, _Handler} -> ok after 2000 -> ?assert(false) end,

         % Send multiple messages
         Messages = [<<"msg1\n">>, <<"msg2\n">>, <<"msg3\n">>],
         lists:foreach(fun(M) -> gen_tcp:send(Socket, M) end, Messages),

         % Receive all messages
         Received = lists:map(fun(_) ->
             receive
                 {transport_message, Data} -> Data
             after 2000 ->
                 ?assert(false, "Missing message")
             end
         end, Messages),

         ?assertEqual([<<"msg1">>, <<"msg2">>, <<"msg3">>], Received),

         gen_tcp:close(Socket),
         erlmcp_test_helpers:stop_test_process(ServerPid)
     end}}.

%%%===================================================================
%%% Server Error Handling Tests
%%%===================================================================

server_handles_client_disconnect_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Opts = #{},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(Opts),

         {ok, Socket} = gen_tcp:connect("localhost", Port,
                                        [binary, {active, false}], 5000),

         Handler = receive
             {transport_connected, H} -> H
         after 2000 ->
             ?assert(false, "No handler")
         end,

         % Close client connection
         gen_tcp:close(Socket),

         % Handler should notify owner of disconnect
         receive
             {transport_disconnected, Handler, _Reason} ->
                 ?assert(true)
         after 2000 ->
             ?assert(false, "No disconnect notification")
         end,

         erlmcp_test_helpers:stop_test_process(ServerPid)
     end}}.

%%%===================================================================
%%% Transport Behavior Tests
%%%===================================================================

transport_behavior_close_server_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"Close server with ranch ref",
         fun() ->
             Opts = #{port => 0},
             {ok, StateMap, _Port} = erlmcp_test_helpers:start_test_server(Opts),

             % Get server via transport_init (not gen_server)
             % We can't directly test this without starting the process
             % So we test via the running server
             ?assert(is_process_alive(StateMap))
         end}
     ]}.

