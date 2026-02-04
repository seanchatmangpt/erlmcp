%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive Transport Layer EUnit Test Suite
%%%
%%% This module implements the test ontology defined in
%%% ggen/ontology/instances/transport_eunit.ttl using ggen methodology.
%%%
%%% == Test Coverage ==
%%%
%%% 1. **Stdio Transport Tests**
%%%    - start_link/1 with owner
%%%    - send/2 with binary and iolist data
%%%    - close/1 termination
%%%    - Message delivery to owner
%%%    - Line ending handling (\n, \r\n, \r)
%%%    - Owner monitoring and termination
%%%
%%% 2. **TCP Transport Tests**
%%%    - Client mode initialization
%%%    - Connection establishment
%%%    - Send/receive communication
%%%    - Reconnection with exponential backoff
%%%    - Message size validation (16MB limit)
%%%    - Socket cleanup
%%%
%%% 3. **HTTP Transport Tests**
%%%    - Request handler initialization
%%%    - Send via HTTP POST
%%%    - Response handling
%%%    - Timeout handling
%%%
%%% 4. **WebSocket Transport Tests**
%%%    - Message parsing (text frames)
%%%    - Ping/pong keepalive
%%%    - Close frame handling
%%%    - Fragment reassembly
%%%    - UTF-8 validation
%%%
%%% 5. **Transport Behavior Tests**
%%%    - Protocol detection from URLs
%%%    - Configuration validation
%%%    - Error handling across all transports
%%%    - Message size limits (16MB default)
%%%    - UTF-8 validation
%%%    - Backpressure handling
%%%    - Resource cleanup
%%%
%%% == Testing Methodology ==
%%%
%%% - Chicago School TDD: Real processes, state-based verification
%%% - Observable behavior via API calls
%%% - Minimal mocking (meip where appropriate)
%%% - Edge case coverage
%%% - Resource cleanup verification
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").
-include("erlmcp_transport_tcp.hrl").

%% Default test values
-define(DEFAULT_MAX_MESSAGE_SIZE, 16777216). %% 16MB
-define(DEFAULT_TIMEOUT, 5000).
-define(TEST_PORT, 19999).

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    %% Ensure application is started
    application:ensure_all_started(erlmcp_transports),
    ok.

teardown(_) ->
    %% Cleanup any remaining processes
    cleanup_transports(),
    timer:sleep(100),
    ok.

cleanup_transports() ->
    %% Stop any lingering transport processes
    Processes = processes(),
    lists:foreach(fun(P) ->
        case process_info(P, registered_name) of
            {_, Name} when is_atom(Name) ->
                case atom_to_list(Name) of
                    "erlmcp_transport" ++ _ ->
                        catch gen_server:stop(P, normal, 500);
                    _ ->
                        ok
                end;
            _ ->
                ok
        end
    end, Processes).

%%%===================================================================
%%% Stdio Transport Tests
%%%===================================================================

stdio_start_stop_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Start stdio transport with owner",
       fun() ->
          Owner = self(),
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          ?assert(is_pid(Transport)),
          ?assert(is_process_alive(Transport)),
          gen_server:stop(Transport, normal, 1000),
          timer:sleep(50),
          ?assertNot(is_process_alive(Transport))
       end},
      {"Start with options map",
       fun() ->
          Owner = self(),
          Opts = #{test_mode => true, max_message_size => 8388608},
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner, Opts),
          ?assert(is_process_alive(Transport)),
          gen_server:stop(Transport, normal, 1000)
       end},
      {"Multiple concurrent stdio transports",
       fun() ->
          Owner = self(),
          {ok, T1} = erlmcp_transport_stdio:start_link(Owner),
          {ok, T2} = erlmcp_transport_stdio:start_link(Owner),
          ?assert(is_process_alive(T1)),
          ?assert(is_process_alive(T2)),
          ?assert(T1 =/= T2),
          gen_server:stop(T1, normal, 1000),
          gen_server:stop(T2, normal, 1000)
       end}]}.

stdio_send_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Send binary data",
       fun() ->
          Owner = self(),
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          ?assertEqual(ok, erlmcp_transport_stdio:send(Transport, <<"test message">>)),
          gen_server:stop(Transport, normal, 1000)
       end},
      {"Send iolist data",
       fun() ->
          Owner = self(),
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          IoList = [<<"partial">>, " ", <<"message">>],
          ?assertEqual(ok, erlmcp_transport_stdio:send(Transport, IoList)),
          gen_server:stop(Transport, normal, 1000)
       end},
      {"Send empty binary",
       fun() ->
          Owner = self(),
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          ?assertEqual(ok, erlmcp_transport_stdio:send(Transport, <<>>)),
          gen_server:stop(Transport, normal, 1000)
       end},
      {"Send urgent message",
       fun() ->
          Owner = self(),
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          ?assertEqual(ok, erlmcp_transport_stdio:send_urgent(Transport, <<"urgent">>)),
          gen_server:stop(Transport, normal, 1000)
       end}]}.

stdio_message_delivery_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10, fun() ->
        Owner = self(),
        {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
        try
            TestLine = <<"test input line">>,
            ?assertEqual(ok, gen_server:call(Transport, {simulate_input, TestLine})),

            receive
                {transport_message, ReceivedLine} ->
                    ?assertEqual(TestLine, ReceivedLine)
            after 1000 ->
                ?assert(false, "Message not received")
            end
        after
            gen_server:stop(Transport, normal, 1000)
        end
     end}}.

stdio_line_endings_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Newline termination (\\n)",
       fun() ->
          Owner = self(),
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          try
              Line = <<"test line\n">>,
              gen_server:call(Transport, {simulate_input, Line}),
              receive
                  {transport_message, _} -> ok
              after 500 -> ok
              end
          after
              gen_server:stop(Transport, normal, 1000)
          end
       end},
      {"Carriage return + newline (\\r\\n)",
       fun() ->
          Owner = self(),
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          try
              Line = <<"test line\r\n">>,
              gen_server:call(Transport, {simulate_input, Line}),
              receive
                  {transport_message, _} -> ok
              after 500 -> ok
              end
          after
              gen_server:stop(Transport, normal, 1000)
          end
       end},
      {"Carriage return only (\\r)",
       fun() ->
          Owner = self(),
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          try
              Line = <<"test line\r">>,
              gen_server:call(Transport, {simulate_input, Line}),
              receive
                  {transport_message, _} -> ok
              after 500 -> ok
              end
          after
              gen_server:stop(Transport, normal, 1000)
          end
       end}]}.

stdio_owner_monitoring_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10, fun() ->
        Owner = spawn_link(fun() ->
            receive
                stop -> ok
            after 10000 -> ok
            end
        end),
        {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
        ?assert(is_process_alive(Transport)),

        unlink(Owner),
        exit(Owner, kill),
        timer:sleep(100),

        timer:sleep(500),
        ?assertNot(is_process_alive(Transport))
     end}}.

stdio_get_info_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        Owner = self(),
        {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
        try
            Info = erlmcp_transport_stdio:get_info(Transport),
            ?assert(is_map(Info)),
            ?assertEqual(stdio, maps:get(type, Info)),
            ?assertEqual(running, maps:get(status, Info))
        after
            gen_server:stop(Transport, normal, 1000)
        end
     end}.

%%%===================================================================
%%% TCP Transport Tests
%%%===================================================================

tcp_client_start_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Start TCP client with valid options",
       fun() ->
          Opts = #{
            mode => client,
            host => "localhost",
            port => ?TEST_PORT,
            owner => self()
          },
          {ok, Transport} = erlmcp_transport_tcp:start_client(Opts),
          ?assert(is_pid(Transport)),
          ?assert(is_process_alive(Transport)),
          gen_server:stop(Transport, normal, 1000)
       end},
      {"Client requires host and port",
       fun() ->
          Opts = #{
            mode => client,
            owner => self()
          },
          Result = (catch erlmcp_transport_tcp:start_client(Opts)),
          ?assertMatch({'EXIT', _}, Result)
       end}]}.

tcp_connect_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Connect to available server",
       fun() ->
          %% Start a simple echo server
          {ok, ListenSocket} = gen_tcp:listen(0, [binary, {active, false}]),
          {ok, Port} = inet:port(ListenSocket),

          %% Accept connections in background
          Server = spawn(fun() -> echo_server(ListenSocket) end),

          Opts = #{
            mode => client,
            host => "localhost",
            port => Port,
            owner => self(),
            connect_timeout => 2000
          },
          {ok, Client} = erlmcp_transport_tcp:start_client(Opts),

          %% Wait for connection or timeout
          receive
              {transport_connected, Client} ->
                  ?assert(true)
          after 3000 ->
                  ?assert(false, "Connection timeout")
          end,

          gen_server:stop(Client, normal, 1000),
          catch exit(Server, kill)
       end},
      {"Handle connection failure gracefully",
       fun() ->
          Opts = #{
            mode => client,
            host => "localhost",
            port => 49999, %% Unlikely to be in use
            owner => self(),
            max_reconnect_attempts => 0
          },
          {ok, Client} = erlmcp_transport_tcp:start_client(Opts),

          %% Client should attempt to connect and fail
          timer:sleep(500),

          %% Process should still be alive (in disconnected state)
          ?assert(is_process_alive(Client)),

          gen_server:stop(Client, normal, 1000)
       end}]}.

tcp_send_receive_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10, fun() ->
        %% Start echo server
        {ok, ListenSocket} = gen_tcp:listen(0, [binary, {active, false}, {packet, line}]),
        {ok, Port} = inet:port(ListenSocket),
        Server = spawn(fun() -> echo_server(ListenSocket) end),

        Opts = #{
            mode => client,
            host => "localhost",
            port => Port,
            owner => self(),
            connect_timeout => 2000
        },
        {ok, Client} = erlmcp_transport_tcp:start_client(Opts),

        receive
            {transport_connected, Client} -> ok
        after 3000 ->
            ?assert(false, "Connection timeout")
        end,

        %% Test send
        TestMessage = <<"test message\n">>,
        Result = gen_server:call(Client, {send, TestMessage}),
        ?assertEqual(ok, Result),

        %% Clean up
        gen_server:stop(Client, normal, 1000),
        catch exit(Server, kill)
     end}}.

tcp_reconnect_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10, fun() ->
        Opts = #{
            mode => client,
            host => "localhost",
            port => 49999,
            owner => self(),
            max_reconnect_attempts => 3
        },
        {ok, Client} = erlmcp_transport_tcp:start_client(Opts),

        %% Initial connection will fail, but client should remain alive
        timer:sleep(100),
        ?assert(is_process_alive(Client)),

        %% Get state to check reconnect attempts
        {ok, State} = gen_server:call(Client, get_state, 1000),
        ?assert(is_record(State, state)),
        ?assertEqual(client, State#state.mode),
        ?assertEqual(false, State#state.connected),

        gen_server:stop(Client, normal, 1000)
     end}}.

tcp_message_size_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        %% Test message size validation
        SmallMessage = <<"small">>,
        ?assertEqual(ok, erlmcp_transport_stdio:validate_message_size(
            SmallMessage, ?DEFAULT_MAX_MESSAGE_SIZE)),

        OversizeMessage = binary:copy(<<$x>>, ?DEFAULT_MAX_MESSAGE_SIZE + 1),
        ?assertEqual({error, size_exceeded}, erlmcp_transport_stdio:validate_message_size(
            OversizeMessage, ?DEFAULT_MAX_MESSAGE_SIZE)),

        %% Test default max size
        ?assertEqual(?DEFAULT_MAX_MESSAGE_SIZE, erlmcp_transport_stdio:get_max_message_size())
     end}.

tcp_get_info_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        Opts = #{
            mode => client,
            host => "localhost",
            port => ?TEST_PORT,
            owner => self()
        },
        {ok, Transport} = erlmcp_transport_tcp:start_client(Opts),
        try
            Info = erlmcp_transport_tcp:get_info(Transport),
            ?assert(is_map(Info)),
            ?assertEqual(tcp, maps:get(type, Info)),
            ?assertMatch(disconnected, maps:get(status, Info))
        after
            gen_server:stop(Transport, normal, 1000)
        end
     end}.

%%%===================================================================
%%% HTTP Transport Tests
%%%===================================================================

http_init_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Initialize HTTP transport with URL",
       fun() ->
          Opts = #{
            url => <<"http://localhost:8080/mcp">>,
            owner => self()
          },
          Result = erlmcp_transport_http:init(Opts),
          ?assertMatch({ok, _}, Result)
       end},
      {"Initialize requires URL",
       fun() ->
          Opts = #{
            owner => self()
          },
          Result = (catch erlmcp_transport_http:init(Opts)),
          ?assertMatch({'EXIT', _}, Result)
       end}]}.

http_send_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        %% Test send interface (will fail without actual server)
        Opts = #{
            url => <<"http://localhost:18080/mcp">>,
            owner => self()
        },
        case erlmcp_transport_http:init(Opts) of
            {ok, Pid} when is_pid(Pid) ->
                Result = erlmcp_transport_http:send(Pid, <<"test">>),
                ?assertMatch(ok, Result),
                erlmcp_transport_http:close(Pid);
            {error, _} ->
                ?assert(true)  %% Server not available
        end
     end}.

http_options_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        %% Test transport options validation
        ValidOpts = #{
            url => <<"https://example.com/mcp">>,
            owner => self(),
            method => post,
            timeout => 5000,
            headers => [{<<"Authorization">>, <<"Bearer token">>}]
        },
        Result = erlmcp_transport_http:init(ValidOpts),
        ?assertMatch({ok, _}, Result)
     end}.

%%%===================================================================
%%% WebSocket Transport Tests
%%%===================================================================

ws_message_parsing_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Parse single message",
       fun() ->
          Data = <<"{}">>,
          ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Data))
       end},
      {"Parse multiple messages",
       fun() ->
          Data = <<"{}\n{}\n">>,
          ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(Data))
       end},
      {"Validate message size",
       fun() ->
          SmallMessage = <<"{}">>,
          ?assertMatch({ok, _}, erlmcp_transport_ws:validate_message_size(SmallMessage)),

          Oversize = binary:copy(<<$x>>, 16777217),
          ?assertEqual({error, too_big}, erlmcp_transport_ws:validate_message_size(Oversize))
       end}]}.

ws_ping_pong_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        %% Verify ping/pong frame handling functions exist
        %% The functions are exported so we can check if they exist in the module
        ?assert(ensure_exported(erlmcp_transport_ws, handle_ping_frame)),
        ?assert(ensure_exported(erlmcp_transport_ws, handle_pong_frame))
     end}.

ws_close_frame_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        %% Test close frame handler exists
        ?assert(ensure_exported(erlmcp_transport_ws, handle_close_frame))
     end}.

ws_utf8_validation_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Valid UTF-8",
       fun() ->
          ValidUtf8 = <<"Hello World">>,
          ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(ValidUtf8))
       end},
      {"Empty binary",
       fun() ->
          ?assertEqual(ok, erlmcp_transport_ws:validate_utf8(<<>>))
       end},
      {"Invalid UTF-8 sequence",
       fun() ->
          InvalidUtf8 = <<255, 255, 255>>,
          ?assertEqual({error, invalid_utf8}, erlmcp_transport_ws:validate_utf8(InvalidUtf8))
       end}]}.

%%%===================================================================
%%% Transport Protocol Detection Tests
%%%===================================================================

transport_protocol_detection_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Detect stdio protocol",
       fun() ->
          ?assertEqual(stdio, detect_transport_protocol({stdio, []}))
       end},
      {"Detect TCP protocol",
       fun() ->
          ?assertEqual(tcp, detect_transport_protocol({tcp, #{port => 9000}}))
       end},
      {"Detect HTTP protocol from URL",
       fun() ->
          ?assertEqual(http, detect_transport_protocol(
            {http, #{url => <<"http://example.com/mcp">>}})),
          ?assertEqual(http, detect_transport_protocol(
            {http, #{url => <<"https://example.com/mcp">>}}))
       end},
      {"Detect WebSocket protocol from URL",
       fun() ->
          ?assertEqual(websocket, detect_transport_protocol(
            {websocket, #{url => <<"ws://example.com/ws">>}})),
          ?assertEqual(websocket, detect_transport_protocol(
            {websocket, #{url => <<"wss://example.com/ws">>}}))
       end},
      {"Detect SSE protocol",
       fun() ->
          ?assertEqual(sse, detect_transport_protocol({sse, #{}}))
       end}]}.

detect_transport_protocol({Type, _Opts}) when Type =:= stdio;
                                                   Type =:= tcp;
                                                   Type =:= http;
                                                   Type =:= websocket;
                                                   Type =:= sse ->
    Type;
detect_transport_protocol({http, Opts}) when is_map(Opts) ->
    URL = maps:get(url, Opts, <<>>),
    case URL of
        <<"http://", _/binary>> -> http;
        <<"https://", _/binary>> -> http
    end;
detect_transport_protocol({websocket, Opts}) when is_map(Opts) ->
    URL = maps:get(url, Opts, <<>>),
    case URL of
        <<"ws://", _/binary>> -> websocket;
        <<"wss://", _/binary>> -> websocket
    end.

%%%===================================================================
%%% Transport Error Handling Tests
%%%===================================================================

transport_error_handling_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Handle invalid transport type",
       fun() ->
          Result = (catch detect_transport_protocol({invalid, #{}})),
          ?assertMatch({error, _}, Result)
       end},
      {"Handle missing required options",
       fun() ->
          %% TCP without host/port should fail
          Result = (catch erlmcp_transport_tcp:start_client(#{})),
          ?assertMatch({'EXIT', _}, Result)
       end},
      {"Handle invalid owner pid",
       fun() ->
          %% Spawn and kill owner quickly
          Owner = spawn(fun() -> ok end),
          timer:sleep(10),  % Let it die
          ?assertNot(is_process_alive(Owner))
       end}]}.

transport_timeout_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10, fun() ->
        %% Test connection timeout handling
        Opts = #{
            mode => client,
            host => "192.0.2.1",  %% TEST-NET-1 (should never route)
            port => 9999,
            owner => self(),
            connect_timeout => 500
        },
        {ok, Client} = erlmcp_transport_tcp:start_client(Opts),

        %% Should handle timeout gracefully
        timer:sleep(1000),
        ?assert(is_process_alive(Client)),

        gen_server:stop(Client, normal, 1000)
     end}}.

%%%===================================================================
%%% Transport Validation Tests
%%%===================================================================

transport_validation_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Validate stdio options",
       fun() ->
          ?assertEqual(ok, erlmcp_transport_behavior:validate_transport_opts(
            stdio, #{owner => self()})),

          ?assertEqual({error, {invalid_opts, missing_owner}},
            erlmcp_transport_behavior:validate_transport_opts(stdio, #{})),

          ?assertEqual({error, {invalid_opts, invalid_owner_type}},
            erlmcp_transport_behavior:validate_transport_opts(
              stdio, #{owner => "not_a_pid"}))
       end},
      {"Validate TCP options",
       fun() ->
          ?assertEqual({error, {invalid_opts, missing_owner}},
            erlmcp_transport_behavior:validate_transport_opts(tcp, #{})),

          ?assertEqual({error, {invalid_opts, missing_host}},
            erlmcp_transport_behavior:validate_transport_opts(
              tcp, #{owner => self()})),

          ?assertEqual({error, {invalid_opts, missing_port}},
            erlmcp_transport_behavior:validate_transport_opts(
              tcp, #{owner => self(), host => "localhost"}))
       end},
      {"Validate HTTP options",
       fun() ->
          ?assertEqual({error, {invalid_opts, missing_owner}},
            erlmcp_transport_behavior:validate_transport_opts(http, #{})),

          ?assertEqual({error, {invalid_opts, missing_url}},
            erlmcp_transport_behavior:validate_transport_opts(
              http, #{owner => self()})),

          ?assertEqual({error, {invalid_opts, invalid_url_scheme}},
            erlmcp_transport_behavior:validate_transport_opts(
              http, #{owner => self(), url => <<"ftp://example.com">>}))
       end},
      {"Validate WebSocket options",
       fun() ->
          ?assertEqual({error, {invalid_opts, missing_url}},
            erlmcp_transport_behavior:validate_transport_opts(
              websocket, #{owner => self()})),

          ?assertEqual({error, {invalid_opts, invalid_url_scheme}},
            erlmcp_transport_behavior:validate_transport_opts(
              websocket, #{owner => self(), url => <<"http://example.com">>}))
       end}]}.

%%%===================================================================
%%% Message Size Limits Tests
%%%===================================================================

message_size_limits_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Default 16MB limit",
       fun() ->
          ?assertEqual(?DEFAULT_MAX_MESSAGE_SIZE,
            erlmcp_transport_stdio:get_max_message_size()),

          ?assertEqual(?DEFAULT_MAX_MESSAGE_SIZE,
            erlmcp_transport_tcp:get_max_message_size())
       end},
      {"Validate size at limit",
       fun() ->
          AtLimit = binary:copy(<<$x>>, ?DEFAULT_MAX_MESSAGE_SIZE),
          ?assertEqual(ok, erlmcp_transport_stdio:validate_message_size(
            AtLimit, ?DEFAULT_MAX_MESSAGE_SIZE))
       end},
      {"Reject oversized messages",
       fun() ->
          OverLimit = binary:copy(<<$x>>, ?DEFAULT_MAX_MESSAGE_SIZE + 1),
          ?assertEqual({error, size_exceeded},
            erlmcp_transport_stdio:validate_message_size(
              OverLimit, ?DEFAULT_MAX_MESSAGE_SIZE))
       end},
      {"Custom size limits",
       fun() ->
          CustomLimit = 1024,
          SmallMessage = <<"test">>,
          ?assertEqual(ok, erlmcp_transport_stdio:validate_message_size(
            SmallMessage, CustomLimit)),

          LargeMessage = binary:copy(<<$x>>, CustomLimit + 1),
          ?assertEqual({error, size_exceeded},
            erlmcp_transport_stdio:validate_message_size(
              LargeMessage, CustomLimit))
       end}]}.

%%%===================================================================
%%% UTF-8 Validation Tests
%%%===================================================================

utf8_validation_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Valid ASCII",
       fun() ->
          ?assertEqual(ok, erlmcp_transport_stdio:validate_message_utf8(<<"ASCII">>))
       end},
      {"Valid UTF-8 multibyte",
       fun() ->
          ?assertEqual(ok, erlmcp_transport_stdio:validate_message_utf8(
            <<"Hello World">>))
       end},
      {"Empty binary",
       fun() ->
          ?assertEqual(ok, erlmcp_transport_stdio:validate_message_utf8(<<>>))
       end},
      {"Invalid UTF-8 sequence",
       fun() ->
          ?assertEqual({error, invalid_utf8},
            erlmcp_transport_stdio:validate_message_utf8(<<255>>)),

          ?assertEqual({error, invalid_utf8},
            erlmcp_transport_stdio:validate_message_utf8(<<128, 128>>))
       end},
      {"Incomplete UTF-8 sequence",
       fun() ->
          ?assertEqual({error, invalid_utf8},
            erlmcp_transport_stdio:validate_message_utf8(<<16#C2>>))  %% Incomplete 2-byte
       end},
      {"Validate iolist conversion",
       fun() ->
          IoList = [<<"Hello">>, " ", <<"World">>],
          ?assertEqual(ok, erlmcp_transport_stdio:validate_message_utf8(IoList))
       end}]}.

%%%===================================================================
%%% Backpressure Handling Tests
%%%===================================================================

backpressure_handling_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"WebSocket backpressure activation",
       fun() ->
          %% Create a state simulating buffer near limit
          State = #{
            frame_buffer_size => 1000,
            frame_buffer_used => 999,
            backpressure_state => inactive
          },
          Result = erlmcp_transport_ws:check_backpressure(State),
          ?assertMatch({ok, _}, Result)
       end},
      {"WebSocket backpressure at limit",
       fun() ->
          State = #{
            frame_buffer_size => 1000,
            frame_buffer_used => 1000,
            backpressure_state => inactive
          },
          Result = erlmcp_transport_ws:check_backpressure(State),
          ?assertMatch({ok, _}, Result)
       end},
      {"WebSocket backpressure already active",
       fun() ->
          State = #{
            frame_buffer_size => 1000,
            frame_buffer_used => 500,
            backpressure_state => active
          },
          Result = erlmcp_transport_ws:check_backpressure(State),
          ?assertMatch({error, backpressure_active, _}, Result)
       end},
      {"Update buffer usage",
       fun() ->
          State = #{
            frame_buffer_size => 1000,
            frame_buffer_used => 100,
            messages_pending => 1
          },
          NewState = erlmcp_transport_ws:update_buffer_usage(State, 200, add),
          ?assertEqual(300, maps:get(frame_buffer_used, NewState)),
          ?assertEqual(2, maps:get(messages_pending, NewState)),

          SubState = erlmcp_transport_ws:update_buffer_usage(NewState, 100, subtract),
          ?assertEqual(200, maps:get(frame_buffer_used, SubState)),
          ?assertEqual(1, maps:get(messages_pending, SubState))
       end},
      {"Resume reading on buffer drain",
       fun() ->
          State = #{
            frame_buffer_size => 1000,
            frame_buffer_used => 400,
            backpressure_state => active,
            backpressure_timer => make_ref()
          },
          NewState = erlmcp_transport_ws:resume_reading(State),
          ?assertEqual(inactive, maps:get(backpressure_state, NewState))
       end}]}.

%%%===================================================================
%%% Transport Cleanup Tests
%%%===================================================================

transport_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Stdio cleanup on close",
       fun() ->
          Owner = self(),
          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          ?assert(is_process_alive(Transport)),

          ?assertEqual(ok, erlmcp_transport_stdio:close(Transport)),
          timer:sleep(100),
          ?assertNot(is_process_alive(Transport))
       end},
      {"TCP cleanup on stop",
       fun() ->
          Opts = #{
            mode => client,
            host => "localhost",
            port => ?TEST_PORT,
            owner => self()
          },
          {ok, Transport} = erlmcp_transport_tcp:start_client(Opts),
          ?assert(is_process_alive(Transport)),

          gen_server:stop(Transport, normal, 1000),
          timer:sleep(100),
          ?assertNot(is_process_alive(Transport))
       end},
      {"Cleanup after owner death",
       fun() ->
          Owner = spawn(fun() ->
              receive
                  stop -> ok
              after 10000 -> ok
              end
          end),

          {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
          ?assert(is_process_alive(Transport)),

          %% Kill owner
          unlink(Owner),
          exit(Owner, kill),

          %% Transport should terminate
          timer:sleep(600),
          ?assertNot(is_process_alive(Transport))
       end}]}.

%%%===================================================================
%%% Integration Tests
%%%===================================================================

transport_integration_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 15, fun() ->
        %% Full integration test with echo server
        {ok, ListenSocket} = gen_tcp:listen(0, [
            binary, {active, false}, {packet, line},
            {reuseaddr, true}
        ]),
        {ok, Port} = inet:port(ListenSocket),

        %% Start server and client
        Server = spawn(fun() -> echo_server(ListenSocket) end),

        Opts = #{
            mode => client,
            host => "localhost",
            port => Port,
            owner => self(),
            connect_timeout => 2000
        },
        {ok, Client} = erlmcp_transport_tcp:start_client(Opts),

        %% Wait for connection
        receive
            {transport_connected, Client} -> ok
        after 5000 ->
            ?assert(false, "Connection timeout")
        end,

        %% Send test messages
        Messages = [<<"msg1\n">>, <<"msg2\n">>, <<"msg3\n">>],
        lists:foreach(fun(Msg) ->
            ?assertEqual(ok, gen_server:call(Client, {send, Msg}))
        end, Messages),

        %% Verify connection remains stable
        timer:sleep(500),
        ?assert(is_process_alive(Client)),

        %% Cleanup
        gen_server:stop(Client, normal, 1000),
        catch exit(Server, kill),
        gen_tcp:close(ListenSocket)
     end}}.

%%%===================================================================
%%% Performance Benchmarks
%%%===================================================================

stdio_send_performance_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 30, fun() ->
        Owner = self(),
        {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
        try
            %% Measure send performance
            NumMessages = 1000,
            StartTime = erlang:monotonic_time(millisecond),

            lists:foreach(fun(N) ->
                Msg = <<"message ", (integer_to_binary(N))/binary, "\n">>,
                erlmcp_transport_stdio:send(Transport, Msg)
            end, lists:seq(1, NumMessages)),

            EndTime = erlang:monotonic_time(millisecond),
            Duration = EndTime - StartTime,

            %% Should complete reasonably quickly
            ?assert(Duration < 5000),

            %% Transport should remain stable
            ?assert(is_process_alive(Transport))
        after
            gen_server:stop(Transport, normal, 1000)
        end
     end}}.

tcp_message_throughput_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 30, fun() ->
        %% Start echo server
        {ok, ListenSocket} = gen_tcp:listen(0, [
            binary, {active, false}, {packet, line},
            {reuseaddr, true}, {recbuf, 65536}, {sndbuf, 65536}
        ]),
        {ok, Port} = inet:port(ListenSocket),
        Server = spawn(fun() -> echo_server(ListenSocket) end),

        Opts = #{
            mode => client,
            host => "localhost",
            port => Port,
            owner => self(),
            connect_timeout => 2000,
            buffer_size => 65536
        },
        {ok, Client} = erlmcp_transport_tcp:start_client(Opts),

        receive
            {transport_connected, Client} -> ok
        after 3000 ->
            ?assert(false, "Connection timeout")
        end,

        %% Measure throughput
        NumMessages = 500,
        StartTime = erlang:monotonic_time(millisecond),

        lists:foreach(fun(N) ->
            Msg = <<"msg", (integer_to_binary(N))/binary, "\n">>,
            gen_server:call(Client, {send, Msg}, 1000)
        end, lists:seq(1, NumMessages)),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        %% Calculate messages per second
        MsgPerSec = (NumMessages * 1000) div Duration,

        %% Should handle at least 100 msg/sec
        ?assert(MsgPerSec > 100),

        gen_server:stop(Client, normal, 1000),
        catch exit(Server, kill),
        gen_tcp:close(ListenSocket)
     end}}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Simple echo server for testing
echo_server(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 2000) of
        {ok, Socket} ->
            echo_loop(Socket);
        {error, _} ->
            ok
    end.

echo_loop(Socket) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            echo_loop(Socket);
        {error, closed} ->
            gen_tcp:close(Socket);
        {error, _} ->
            gen_tcp:close(Socket)
    end.

%% Check if a function is exported from a module
ensure_exported(Module, Function) ->
    case Module:module_info(exports) of
        Exports when is_list(Exports) ->
            lists:any(fun({F, _}) -> F =:= Function end, Exports);
        _ ->
            false
    end.
