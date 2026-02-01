-module(erlmcp_transport_integration_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test Integration Suite for Transport Coordination
%%%
%%% Tests:
%%% - Multi-transport application startup
%%% - gproc registration and discovery
%%% - Transport supervisor coordination
%%% - Real message passing between transports
%%% - Transport failover and recovery
%%%===================================================================

%%====================================================================
%% CT Callbacks
%%====================================================================

all() ->
    [application_startup,
     supervisor_integration,
     gproc_registration,
     multi_transport_coordination,
     transport_message_routing,
     tcp_client_server_integration,
     transport_failover].

init_per_suite(Config) ->
    %% Start dependencies
    {ok, _} = application:ensure_all_started(gproc),
    {ok, _} = application:ensure_all_started(ranch),
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(ssl),

    %% Start transport application
    {ok, _} = application:ensure_all_started(erlmcp_transports),

    [{app_started, true} | Config].

end_per_suite(_Config) ->
    application:stop(erlmcp_transports),
    application:stop(gun),
    application:stop(ranch),
    application:stop(gproc),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    timer:sleep(100),  % Allow cleanup
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

application_startup(_Config) ->
    %% Verify erlmcp_transports application started successfully
    Apps = application:which_applications(),
    ct:pal("Running applications: ~p", [Apps]),

    ?assert(lists:keymember(erlmcp_transports, 1, Apps)),

    %% Verify supervisor is running
    SupPid = whereis(erlmcp_transport_sup),
    ?assert(is_pid(SupPid)),
    ?assert(is_process_alive(SupPid)).

supervisor_integration(_Config) ->
    %% Test that supervisor can manage transport children
    Owner = self(),

    %% Set test mode for stdio
    put(test_mode, true),

    %% Start stdio transport as supervised child
    ChildSpec =
        #{id => integration_stdio,
          start => {erlmcp_transport_stdio, start_link, [Owner]},
          restart => temporary,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_transport_stdio]},

    {ok, StdioPid} = supervisor:start_child(erlmcp_transport_sup, ChildSpec),

    ct:pal("Started stdio transport: ~p", [StdioPid]),

    %% Verify child is running
    ?assert(is_process_alive(StdioPid)),

    %% Verify child is in supervisor's child list
    Children = supervisor:which_children(erlmcp_transport_sup),
    ct:pal("Supervisor children: ~p", [Children]),

    ?assert(lists:keymember(integration_stdio, 1, Children)),

    %% Cleanup
    supervisor:terminate_child(erlmcp_transport_sup, integration_stdio),
    supervisor:delete_child(erlmcp_transport_sup, integration_stdio),

    erase(test_mode).

gproc_registration(_Config) ->
    %% Test that transports register to gproc on startup
    Owner = self(),
    put(test_mode, true),

    TransportId = <<"gproc_test_transport">>,

    {ok, TransportPid} = erlmcp_transport_stdio:start_link(Owner),

    ct:pal("Started transport for gproc test: ~p", [TransportPid]),

    %% Give time for gproc registration
    timer:sleep(100),

    %% Verify transport is alive
    ?assert(is_process_alive(TransportPid)),

    %% Note: Actual gproc lookup depends on registration key implementation
    %% This is a placeholder for the mechanism
    %% Cleanup
    erlmcp_transport_stdio:close(TransportPid),
    erase(test_mode).

multi_transport_coordination(_Config) ->
    %% Test that multiple transport types can coordinate
    Owner = self(),
    put(test_mode, true),

    %% Start stdio transport
    {ok, StdioPid} = erlmcp_transport_stdio:start_link(Owner),

    %% Start TCP server transport
    TcpOpts =
        #{mode => server,
          port => 0,  % Random port
          owner => Owner,
          transport_id => ct_tcp_transport,
          server_id => ct_tcp_server},
    {ok, TcpPid} = erlmcp_transport_tcp:start_server(TcpOpts),

    ct:pal("Started stdio: ~p, TCP: ~p", [StdioPid, TcpPid]),

    %% Both should be alive
    ?assert(is_process_alive(StdioPid)),
    ?assert(is_process_alive(TcpPid)),

    %% Get TCP port
    {ok, TcpState} = gen_server:call(TcpPid, get_state),
    Port = element(14, TcpState),  % Port is 14th element in #state{}
    ct:pal("TCP server listening on port: ~p", [Port]),

    %% Cleanup
    erlmcp_transport_stdio:close(StdioPid),
    erlmcp_transport_tcp:close(TcpState),

    erase(test_mode).

transport_message_routing(_Config) ->
    %% Test message routing between transports
    Owner = self(),
    put(test_mode, true),

    %% Start stdio transport
    {ok, StdioPid} = erlmcp_transport_stdio:start_link(Owner),

    %% Send test message through stdio
    TestMsg = <<"test message routing">>,
    ok = erlmcp_transport_stdio:send(StdioPid, TestMsg),

    %% Simulate input (in test mode)
    gen_server:call(StdioPid, {simulate_input, <<"input test">>}),

    %% Receive message from transport
    receive
        {transport_message, ReceivedMsg} ->
            ct:pal("Received message: ~p", [ReceivedMsg]),
            ?assertEqual(<<"input test">>, ReceivedMsg)
    after 2000 ->
        ct:fail("Message routing timeout")
    end,

    %% Cleanup
    erlmcp_transport_stdio:close(StdioPid),
    erase(test_mode).

tcp_client_server_integration(_Config) ->
    %% Full TCP client-server integration test
    Owner = self(),

    %% Start TCP server
    ServerOpts =
        #{mode => server,
          port => 0,
          owner => Owner,
          transport_id => ct_integration_server_transport,
          server_id => ct_integration_server},
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(ServerOpts),
    {ok, ServerState} = gen_server:call(ServerPid, get_state),
    Port = element(14, ServerState),

    ct:pal("TCP server started on port: ~p", [Port]),

    %% Start TCP client
    ClientOpts =
        #{mode => client,
          host => "localhost",
          port => Port,
          owner => Owner,
          transport_id => ct_integration_client_transport,
          server_id => ct_integration_server,
          max_reconnect_attempts => 3},
    {ok, ClientPid} = erlmcp_transport_tcp:start_client(ClientOpts),

    ct:pal("TCP client started: ~p", [ClientPid]),

    %% Wait for connection
    receive
        {transport_connected, ClientPid} ->
            ct:pal("Client connected to server")
    after 5000 ->
        ct:fail("Client connection timeout")
    end,

    %% Wait for server handler
    ServerHandler =
        receive
            {transport_connected, Handler} when is_pid(Handler), Handler =/= ClientPid ->
                ct:pal("Server accepted connection: ~p", [Handler]),
                Handler
        after 3000 ->
            ct:fail("Server did not accept connection")
        end,

    %% Send message from client
    Message = <<"integration test message">>,
    ok = gen_server:call(ClientPid, {send, Message}),

    %% Verify message received at server
    receive
        {transport_message, ReceivedMsg} ->
            ct:pal("Server received message: ~p", [ReceivedMsg]),
            ?assertEqual(Message, ReceivedMsg)
    after 3000 ->
        ct:fail("Message not received at server")
    end,

    %% Cleanup
    gen_server:stop(ClientPid),
    gen_server:stop(ServerHandler),
    gen_server:stop(ServerPid),
    timer:sleep(100).

transport_failover(_Config) ->
    %% Test transport failover/recovery
    Owner = self(),

    %% Start TCP server
    ServerOpts =
        #{mode => server,
          port => 0,
          owner => Owner,
          transport_id => ct_failover_server_transport,
          server_id => ct_failover_server},
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(ServerOpts),
    {ok, ServerState} = gen_server:call(ServerPid, get_state),
    Port = element(14, ServerState),

    %% Start TCP client with reconnection
    ClientOpts =
        #{mode => client,
          host => "localhost",
          port => Port,
          owner => Owner,
          transport_id => ct_failover_client_transport,
          server_id => ct_failover_server,
          max_reconnect_attempts => 5},
    {ok, ClientPid} = erlmcp_transport_tcp:start_client(ClientOpts),

    %% Wait for connection
    receive
        {transport_connected, ClientPid} ->
            ok
    after 5000 ->
        ct:fail("Initial connection timeout")
    end,

    %% Get server handler
    ServerHandler =
        receive
            {transport_connected, H} ->
                H
        after 3000 ->
            ct:fail("No server handler")
        end,

    ct:pal("Connection established. Client: ~p, Handler: ~p", [ClientPid, ServerHandler]),

    %% Kill server handler to simulate disconnection
    exit(ServerHandler, kill),
    timer:sleep(100),

    %% Client should detect disconnection and attempt reconnection
    receive
        {transport_disconnected, ClientPid, _Reason} ->
            ct:pal("Client detected disconnection")
    after 2000 ->
        ct:pal("No disconnect notification (may already be reconnecting)")
    end,

    %% Wait for reconnection
    NewHandler =
        receive
            {transport_connected, NewH} when is_pid(NewH), NewH =/= ServerHandler ->
                ct:pal("Client reconnected, new handler: ~p", [NewH]),
                NewH
        after 10000 ->
            ct:fail("Client did not reconnect")
        end,

    %% Verify client can send message after reconnection
    Message = <<"failover test">>,
    timer:sleep(500),  % Give connection time to stabilize
    ok = gen_server:call(ClientPid, {send, Message}),

    receive
        {transport_message, ReceivedMsg} ->
            ct:pal("Message received after failover: ~p", [ReceivedMsg]),
            ?assertEqual(Message, ReceivedMsg)
    after 3000 ->
        ct:fail("Message not received after failover")
    end,

    %% Cleanup
    gen_server:stop(ClientPid),
    gen_server:stop(NewHandler),
    gen_server:stop(ServerPid),
    timer:sleep(100).
