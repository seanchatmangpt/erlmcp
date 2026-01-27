-module(erlmcp_transport_tcp_tests).

-include_lib("eunit/include/eunit.hrl").

%% Include the state record definition for testing
-record(state, {
    mode :: client | server,
    transport_id :: atom() | undefined,
    server_id :: atom() | undefined,
    socket :: gen_tcp:socket() | undefined,
    ranch_ref :: ranch:ref() | undefined,
    owner :: pid() | undefined,
    host :: inet:hostname() | inet:ip_address() | undefined,
    port :: inet:port_number() | undefined,
    options :: [gen_tcp:connect_option()],
    buffer = <<>> :: binary(),
    connected = false :: boolean(),
    reconnect_timer :: reference() | undefined,
    reconnect_attempts = 0 :: non_neg_integer(),
    max_reconnect_attempts = infinity :: pos_integer() | infinity
}).

%%====================================================================
%% Test Fixtures
%%====================================================================

setup_client() ->
    application:ensure_all_started(ranch),
    Owner = self(),
    Opts = #{
        mode => client,
        host => "localhost",
        port => 9999,
        owner => Owner,
        transport_id => test_client_transport,
        server_id => test_server,
        max_reconnect_attempts => 3
    },
    {ok, Pid} = erlmcp_transport_tcp:start_client(Opts),
    {Pid, Owner}.

setup_server() ->
    application:ensure_all_started(ranch),
    Owner = self(),
    %% Use unique ID to avoid ranch ref conflicts
    UniqueId = erlang:unique_integer([positive]),
    Opts = #{
        mode => server,
        port => 0,  % Use random port
        owner => Owner,
        transport_id => list_to_atom("test_server_transport_" ++ integer_to_list(UniqueId)),
        server_id => list_to_atom("test_server_" ++ integer_to_list(UniqueId)),
        num_acceptors => 5,
        max_connections => 100
    },
    {ok, Pid} = erlmcp_transport_tcp:start_server(Opts),
    timer:sleep(100),  % Give server time to start
    {Pid, Owner}.

cleanup(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            unlink(Pid),
            exit(Pid, kill),
            timer:sleep(50);
        false ->
            ok
    end;
cleanup(_) ->
    ok.

%%====================================================================
%% Client Mode Tests
%%====================================================================

client_start_test_() ->
    {setup,
     fun() -> application:ensure_all_started(ranch) end,
     fun(_) -> ok end,
     [
      {"Start client with valid options",
       fun() ->
           Opts = #{
               mode => client,
               host => "localhost",
               port => 9999,
               owner => self(),
               transport_id => test_transport,
               server_id => test_server
           },
           {ok, Pid} = erlmcp_transport_tcp:start_client(Opts),
           ?assert(is_pid(Pid)),
           ?assert(erlang:is_process_alive(Pid)),
           cleanup(Pid)
       end},

      {"Client init creates proper state",
       fun() ->
           Opts = #{
               mode => client,
               host => "127.0.0.1",
               port => 8888,
               owner => self(),
               keepalive => true,
               nodelay => true,
               buffer_size => 32768
           },
           {ok, Pid} = erlmcp_transport_tcp:start_client(Opts),
           {ok, State} = gen_server:call(Pid, get_state),
           ?assertEqual(client, State#state.mode),
           ?assertEqual("127.0.0.1", State#state.host),
           ?assertEqual(8888, State#state.port),
           ?assertEqual(self(), State#state.owner),
           ?assertEqual(false, State#state.connected),
           cleanup(Pid)
       end}
     ]}.

client_connection_failure_test_() ->
    {timeout, 10,
     fun() ->
         {Pid, _Owner} = setup_client(),

         %% Client should attempt to connect to non-existent server
         %% and fail, then schedule reconnection
         timer:sleep(200),

         %% Verify process is still alive (handling reconnection)
         ?assert(erlang:is_process_alive(Pid)),

         %% Get state and verify connection failed
         {ok, State} = gen_server:call(Pid, get_state),
         ?assertEqual(false, State#state.connected),
         ?assert(State#state.reconnect_attempts > 0),

         cleanup(Pid)
     end}.

client_send_not_connected_test_() ->
    {setup,
     fun setup_client/0,
     fun({Pid, _}) -> cleanup(Pid) end,
     fun({Pid, _}) ->
         {"Send fails when not connected",
          fun() ->
              timer:sleep(100),  % Wait for initial connection attempt
              Result = gen_server:call(Pid, {send, <<"test">>}),
              ?assertEqual({error, not_connected}, Result)
          end}
     end}.

%%====================================================================
%% Server Mode Tests
%%====================================================================

server_start_test_() ->
    {setup,
     fun() -> application:ensure_all_started(ranch) end,
     fun(_) -> ok end,
     [
      {"Start server with valid options",
       fun() ->
           Opts = #{
               mode => server,
               port => 0,
               owner => self(),
               transport_id => test_server_transport,
               server_id => test_server
           },
           {ok, Pid} = erlmcp_transport_tcp:start_server(Opts),
           ?assert(is_pid(Pid)),
           ?assert(erlang:is_process_alive(Pid)),

           %% Verify ranch listener started
           {ok, State} = gen_server:call(Pid, get_state),
           RanchRef = State#state.ranch_ref,
           ?assertNotEqual(undefined, RanchRef),

           %% Verify we got a port assigned
           Port = State#state.port,
           ?assert(is_integer(Port)),
           ?assert(Port > 0),

           cleanup(Pid),
           timer:sleep(100)  % Give ranch time to cleanup
       end},

      {"Server state initialization",
       fun() ->
           Opts = #{
               mode => server,
               port => 0,
               owner => self(),
               transport_id => my_transport,
               server_id => my_server,
               num_acceptors => 15,
               max_connections => 500
           },
           {ok, Pid} = erlmcp_transport_tcp:start_server(Opts),
           {ok, State} = gen_server:call(Pid, get_state),

           ?assertEqual(server, State#state.mode),
           ?assertEqual(self(), State#state.owner),
           ?assertEqual(my_transport, State#state.transport_id),
           ?assertEqual(my_server, State#state.server_id),
           ?assertEqual(true, State#state.connected),
           ?assertNotEqual(undefined, State#state.ranch_ref),

           cleanup(Pid),
           timer:sleep(100)
       end}
     ]}.

server_ranch_integration_test_() ->
    {timeout, 15,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         Opts = #{
             mode => server,
             port => 0,
             owner => self(),
             transport_id => ranch_test_transport,
             server_id => ranch_test_server
         },
         {ok, ServerPid} = erlmcp_transport_tcp:start_server(Opts),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,
         RanchRef = ServerState#state.ranch_ref,

         %% Verify ranch listener info (ranch 2.x returns 'running' atom)
         Status = ranch:get_status(RanchRef),
         ?assert(Status =:= running orelse Status =:= {ok, listening}),
         ?assertEqual(Port, ranch:get_port(RanchRef)),

         %% Connect a client
         {ok, ClientSocket} = gen_tcp:connect("localhost", Port,
                                               [binary, {active, false},
                                                {packet, line}], 5000),

         %% Verify connection was accepted by ranch
         timer:sleep(200),  % Give ranch time to accept

         %% Send data from client
         ok = gen_tcp:send(ClientSocket, <<"hello server\n">>),

         %% Cleanup
         gen_tcp:close(ClientSocket),
         cleanup(ServerPid),
         timer:sleep(100)
     end}.

%%====================================================================
%% Client-Server Integration Tests
%%====================================================================

client_server_integration_test_() ->
    {timeout, 15,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         ServerOpts = #{
             mode => server,
             port => 0,
             owner => self(),
             transport_id => integration_server_transport,
             server_id => integration_server
         },
         {ok, ServerPid} = erlmcp_transport_tcp:start_server(ServerOpts),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,

         %% Start client pointing to server
         ClientOpts = #{
             mode => client,
             host => "localhost",
             port => Port,
             owner => self(),
             transport_id => integration_client_transport,
             server_id => integration_server,
             max_reconnect_attempts => 5
         },
         {ok, ClientPid} = erlmcp_transport_tcp:start_client(ClientOpts),

         %% Wait for connection
         receive
             {transport_connected, ClientPid} ->
                 ?assert(true)
         after 5000 ->
             ?assert(false, "Client connection timeout")
         end,

         %% Verify client is connected
         {ok, ClientState} = gen_server:call(ClientPid, get_state),
         ?assertEqual(true, ClientState#state.connected),

         %% Wait for server to receive connection (before sending message)
         ServerHandler = receive
             {transport_connected, Handler} when is_pid(Handler), Handler =/= ClientPid ->
                 Handler
         after 3000 ->
             error("Server did not receive connection")
         end,

         %% Now send message from client
         Message = <<"test message">>,
         ok = gen_server:call(ClientPid, {send, Message}),

         %% Verify message received at server handler
         receive
             {transport_message, ReceivedMsg} ->
                 ?assertEqual(Message, ReceivedMsg)
         after 3000 ->
             ?assert(false, "Message not received")
         end,

         %% Cleanup
         cleanup(ClientPid),
         cleanup(ServerHandler),
         cleanup(ServerPid),
         timer:sleep(100)
     end}.

%%====================================================================
%% Buffer Management Tests
%%====================================================================

%% Helper to test buffer message extraction
extract_messages_helper(Buffer) ->
    extract_messages_helper(Buffer, []).

extract_messages_helper(Buffer, Acc) ->
    case binary:split(Buffer, <<"\n">>) of
        [_] ->
            {lists:reverse(Acc), Buffer};
        [Message, Rest] ->
            extract_messages_helper(Rest, [Message | Acc])
    end.

message_extraction_test_() ->
    [
     {"Extract single complete message",
      fun() ->
          Buffer = <<"hello\n">>,
          {Messages, Remaining} = extract_messages_helper(Buffer),
          ?assertEqual([<<"hello">>], Messages),
          ?assertEqual(<<>>, Remaining)
      end},

     {"Extract multiple complete messages",
      fun() ->
          Buffer = <<"msg1\nmsg2\nmsg3\n">>,
          {Messages, Remaining} = extract_messages_helper(Buffer),
          ?assertEqual([<<"msg1">>, <<"msg2">>, <<"msg3">>], Messages),
          ?assertEqual(<<>>, Remaining)
      end},

     {"Incomplete message remains in buffer",
      fun() ->
          Buffer = <<"hello\nworld">>,
          {Messages, Remaining} = extract_messages_helper(Buffer),
          ?assertEqual([<<"hello">>], Messages),
          ?assertEqual(<<"world">>, Remaining)
      end},

     {"No complete message",
      fun() ->
          Buffer = <<"incomplete">>,
          {Messages, Remaining} = extract_messages_helper(Buffer),
          ?assertEqual([], Messages),
          ?assertEqual(<<"incomplete">>, Remaining)
      end},

     {"Empty buffer",
      fun() ->
          Buffer = <<>>,
          {Messages, Remaining} = extract_messages_helper(Buffer),
          ?assertEqual([], Messages),
          ?assertEqual(<<>>, Remaining)
      end}
    ].

%%====================================================================
%% Transport Behavior Tests
%%====================================================================

transport_behavior_init_test_() ->
    [
     {"Init with client mode",
      fun() ->
          Opts = #{
              mode => client,
              host => "localhost",
              port => 9999,
              owner => self()
          },
          {ok, State} = erlmcp_transport_tcp:transport_init(Opts),
          ?assertEqual(client, State#state.mode),
          ?assertEqual("localhost", State#state.host)
      end},

     {"Init with server mode",
      fun() ->
          application:ensure_all_started(ranch),
          Opts = #{
              mode => server,
              port => 0,
              owner => self(),
              transport_id => behavior_test_transport,
              server_id => behavior_test_server
          },
          {ok, State} = erlmcp_transport_tcp:transport_init(Opts),
          ?assertEqual(server, State#state.mode),
          ?assert(State#state.connected),

          %% Cleanup
          erlmcp_transport_tcp:close(State),
          timer:sleep(100)
      end}
    ].

transport_behavior_send_test_() ->
    [
     {"Send with undefined socket",
      fun() ->
          State = #state{socket = undefined},
          Result = erlmcp_transport_tcp:send(State, <<"test">>),
          ?assertEqual({error, not_connected}, Result)
      end},

     {"Send with socket but not connected",
      fun() ->
          %% Create a dummy socket state (won't actually send)
          State = #state{socket = make_ref(), connected = false},
          Result = erlmcp_transport_tcp:send(State, <<"test">>),
          ?assertEqual({error, not_connected}, Result)
      end}
    ].

transport_behavior_close_test_() ->
    [
     {"Close client connection",
      fun() ->
          State = #state{mode = client, socket = undefined},
          Result = erlmcp_transport_tcp:close(State),
          ?assertEqual(ok, Result)
      end},

     {"Close server with ranch",
      fun() ->
          application:ensure_all_started(ranch),

          %% Start a server
          Opts = #{
              mode => server,
              port => 0,
              owner => self(),
              transport_id => close_test_transport,
              server_id => close_test_server
          },
          {ok, State} = erlmcp_transport_tcp:transport_init(Opts),
          RanchRef = State#state.ranch_ref,

          %% Verify ranch listener is running (ranch 2.x returns 'running', not {ok, listening})
          Status = ranch:get_status(RanchRef),
          ?assert(Status =:= running orelse Status =:= {ok, listening}),

          %% Close the server
          ok = erlmcp_transport_tcp:close(State),

          timer:sleep(100),

          %% Verify ranch listener is stopped
          ?assertError(badarg, ranch:get_status(RanchRef))
      end}
    ].

%%====================================================================
%% Reconnection Tests
%%====================================================================

%% Helper to calculate backoff (mirrors internal implementation)
calculate_backoff_helper(Attempts) ->
    InitialDelay = 1000,
    MaxDelay = 60000,
    BaseDelay = min(InitialDelay * (1 bsl Attempts), MaxDelay),
    Jitter = rand:uniform(BaseDelay div 4),
    BaseDelay + Jitter.

reconnection_backoff_test_() ->
    [
     {"Backoff increases exponentially",
      fun() ->
          Delay0 = calculate_backoff_helper(0),
          Delay1 = calculate_backoff_helper(1),
          Delay2 = calculate_backoff_helper(2),
          Delay3 = calculate_backoff_helper(3),

          %% Delays should generally increase
          ?assert(Delay1 > Delay0),
          ?assert(Delay2 > Delay1),
          ?assert(Delay3 > Delay2),

          %% Should have reasonable bounds (with jitter)
          ?assert(Delay0 >= 1000),
          ?assert(Delay0 =< 1500),
          ?assert(Delay3 >= 8000),
          ?assert(Delay3 =< 12000)
      end},

     {"Backoff caps at max delay",
      fun() ->
          Delay10 = calculate_backoff_helper(10),
          Delay20 = calculate_backoff_helper(20),

          %% Both should be capped near max delay (60s + jitter)
          ?assert(Delay10 >= 60000),
          ?assert(Delay10 =< 75000),
          ?assert(Delay20 >= 60000),
          ?assert(Delay20 =< 75000)
      end}
    ].

reconnection_max_attempts_test_() ->
    {timeout, 15,
     fun() ->
         %% Start client with max 2 reconnection attempts
         Opts = #{
             mode => client,
             host => "localhost",
             port => 9999,  % Non-existent server
             owner => self(),
             max_reconnect_attempts => 2
         },
         {ok, Pid} = erlmcp_transport_tcp:start_client(Opts),

         %% Wait for initial connection attempt + 2 reconnections
         %% Initial delay ~1s, second ~2s = ~3s total + overhead
         timer:sleep(4000),

         %% Verify max attempts reached
         {ok, State} = gen_server:call(Pid, get_state),
         ?assert(State#state.reconnect_attempts >= 2),
         ?assertEqual(false, State#state.connected),

         cleanup(Pid)
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

tcp_error_handling_test_() ->
    {timeout, 10,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         {ServerPid, _} = setup_server(),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,

         %% Connect client
         ClientOpts = #{
             mode => client,
             host => "localhost",
             port => Port,
             owner => self()
         },
         {ok, ClientPid} = erlmcp_transport_tcp:start_client(ClientOpts),

         %% Wait for connection
         receive {transport_connected, ClientPid} -> ok
         after 2000 -> error("Connection timeout")
         end,

         %% Get server handler
         ServerHandler = receive
             {transport_connected, Handler} -> Handler
         after 2000 ->
             error("Server did not receive connection")
         end,

         %% Close server to trigger tcp_closed on client
         cleanup(ServerHandler),

         %% Client should receive disconnect notification
         receive
             {transport_disconnected, ClientPid, _Reason} -> ok
         after 2000 ->
             ?assert(false, "Client did not detect disconnection")
         end,

         %% Verify client starts reconnecting
         timer:sleep(200),
         {ok, ClientState} = gen_server:call(ClientPid, get_state),
         ?assertEqual(false, ClientState#state.connected),
         ?assert(ClientState#state.reconnect_attempts > 0),

         cleanup(ClientPid),
         cleanup(ServerPid),
         timer:sleep(100)
     end}.

%%====================================================================
%% Concurrency Tests
%%====================================================================

multiple_clients_test_() ->
    {timeout, 20,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         ServerOpts = #{
             mode => server,
             port => 0,
             owner => self(),
             max_connections => 10
         },
         {ok, ServerPid} = erlmcp_transport_tcp:start_server(ServerOpts),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,

         %% Start multiple clients
         NumClients = 5,
         Clients = [begin
             Opts = #{
                 mode => client,
                 host => "localhost",
                 port => Port,
                 owner => self(),
                 transport_id => list_to_atom("client_" ++ integer_to_list(N))
             },
             {ok, Pid} = erlmcp_transport_tcp:start_client(Opts),
             Pid
         end || N <- lists:seq(1, NumClients)],

         %% Wait for all clients to connect
         [receive {transport_connected, CPid} -> ok
          after 3000 -> error({client_timeout, CPid})
          end || CPid <- Clients],

         %% Verify all clients are connected
         [begin
             {ok, CState} = gen_server:call(CPid, get_state),
             ?assertEqual(true, CState#state.connected)
         end || CPid <- Clients],

         %% Cleanup
         [cleanup(CPid) || CPid <- Clients],
         cleanup(ServerPid),
         timer:sleep(100)
     end}.

%%====================================================================
%% Ranch Protocol Handler Tests
%%====================================================================

ranch_protocol_handler_test_() ->
    {timeout, 10,
     fun() ->
         application:ensure_all_started(ranch),

         %% Start server
         {ServerPid, _} = setup_server(),
         {ok, ServerState} = gen_server:call(ServerPid, get_state),
         Port = ServerState#state.port,
         RanchRef = ServerState#state.ranch_ref,

         %% Verify ranch protocol is registered (ranch 2.x returns 'running' atom)
         Status = ranch:get_status(RanchRef),
         ?assert(Status =:= running orelse Status =:= {ok, listening}),

         %% Connect a raw TCP client
         {ok, Socket} = gen_tcp:connect("localhost", Port,
                                        [binary, {active, false}, {packet, line}],
                                        5000),

         %% Wait for protocol handler to start
         timer:sleep(200),

         %% Server should notify owner of new connection
         Handler = receive
             {transport_connected, H} -> H
         after 2000 ->
             error("No handler notification received")
         end,

         ?assert(is_pid(Handler)),
         ?assert(erlang:is_process_alive(Handler)),

         %% Cleanup
         gen_tcp:close(Socket),
         timer:sleep(100),
         cleanup(ServerPid)
     end}.
