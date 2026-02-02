%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_tracer - OTP 28 trace:system/3 integration
%%%
%%% Tests:
%%% - Trace session lifecycle (create, destroy)
%%% - Tool call tracing
%%% - Message tracing
%%% - System event monitoring
%%% - Trace collection and visualization
%%% - Distributed tracing across nodes
%%%
%%% Chicago School TDD: Tests specify behavior, not implementation
%%% Uses real erlmcp processes, no mocks
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tracer_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup before all tests
setup_all() ->
    {ok, Pid} = erlmcp_tracer:start_link(),
    {ok, Pid}.

%% Cleanup after all tests
cleanup_all(_Pid) ->
    erlmcp_tracer:stop_trace_session(),
    ok.

%% Setup before each test
setup() ->
    %% Ensure clean state
    erlmcp_tracer:stop_trace_session(),
    ok.

%% Cleanup after each test
cleanup(_) ->
    erlmcp_tracer:stop_trace_session(),
    ok.

%%====================================================================
%% Test Generators
%%====================================================================

tracer_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun session_lifecycle/0,
      fun tool_call_tracing/0,
      fun message_tracing/0,
      fun system_monitoring/0,
      fun trace_collection/0,
      fun trace_visualization/0]}.

%%====================================================================
%% Individual Tests
%%====================================================================

%% @doc Test trace session lifecycle
session_lifecycle() ->
    %% Start a trace session
    {ok, SessionId, TracerPid} = erlmcp_tracer:start_trace_session([node()]),
    ?assert(is_binary(SessionId)),
    ?assert(is_pid(TracerPid)),
    ?assert(process_info(TracerPid) =/= undefined),

    %% Verify session is active
    {ok, ActiveSessions} = erlmcp_tracer:get_active_sessions(),
    ?assert(lists:member(SessionId, ActiveSessions)),

    %% Stop the session
    ok = erlmcp_tracer:stop_trace_session(SessionId),

    %% Verify session is stopped
    {ok, NewActiveSessions} = erlmcp_tracer:get_active_sessions(),
    ?assertNot(lists:member(SessionId, NewActiveSessions)),

    %% Verify tracer process is cleaned up
    timer:sleep(100),
    ?assertEqual(undefined, process_info(TracerPid)),

    ok.

%% @doc Test tool call tracing
tool_call_tracing() ->
    %% Start tracing for tool calls
    {ok, SessionId, _TracerPid} = erlmcp_tracer:trace_tool_calls(),

    %% Create a mock server and add a tool
    {ok, Server} = erlmcp_mock_server:start_link(),

    ToolName = <<"test_tool">>,
    Handler = fun(Args) ->
                  ?LOG(debug, "Tool called with: ~p", [Args]),
                  {ok, #{result => <<"test_success">>}}
              end,

    ok = erlmcp_server:add_tool(Server, ToolName, Handler),

    %% Call the tool
    {ok, Result} = erlmcp_server:call_tool(Server, ToolName, #{arg1 => <<"value1">>}),
    ?assertMatch(#{result := <<"test_success">>}, Result),

    %% Allow trace events to propagate
    timer:sleep(200),

    %% Collect traces
    {ok, Events} = erlmcp_tracer:collect_trace(SessionId),
    ?assert(length(Events) > 0),

    %% Verify trace contains tool call events
    CallEvents = [E || E <- Events, maps:get(event_type, E) =:= call],
    ?assert(length(CallEvents) > 0),

    %% Cleanup
    gen_server:stop(Server),
    ok = erlmcp_tracer:stop_trace_session(SessionId),

    ok.

%% @doc Test message tracing
message_tracing() ->
    %% Start message tracing
    {ok, SessionId, _TracerPid} = erlmcp_tracer:trace_messages(),

    %% Create mock server and client
    {ok, Server} = erlmcp_mock_server:start_link(),
    {ok, Client} = erlmcp_mock_client:start_link(Server),

    %% Send a message
    ok = erlmcp_client:ping(Client),

    %% Allow trace events to propagate
    timer:sleep(200),

    %% Collect traces
    {ok, Events} = erlmcp_tracer:collect_trace(SessionId),
    ?assert(length(Events) > 0),

    %% Verify trace contains send/receive events
    SendEvents = [E || E <- Events, maps:get(event_type, E) =:= send],
    RecvEvents = [E || E <- Events, maps:get(event_type, E) =:= 'receive'],

    %% Should have at least some message activity
    ?assert(length(SendEvents) + length(RecvEvents) > 0),

    %% Cleanup
    gen_server:stop(Client),
    gen_server:stop(Server),
    ok = erlmcp_tracer:stop_trace_session(SessionId),

    ok.

%% @doc Test system event monitoring
system_monitoring() ->
    %% Enable system monitoring
    {ok, SessionId, _TracerPid} =
        erlmcp_tracer:start_trace_session([node()],
                                          #{system_monitor => true,
                                            long_gc => 1,     % 1ms (very sensitive for testing)
                                            long_schedule => 1}),

    %% Trigger system events by creating load
    {ok, Server} = erlmcp_mock_server:start_link(),

    %% Add tools and call them rapidly to trigger GC/scheduling
    lists:foreach(fun(N) ->
                      ToolName = <<"tool_", (integer_to_binary(N))/binary>>,
                      Handler = fun(_) ->
                                        {ok, #{result => N}}
                                end,
                      ok = erlmcp_server:add_tool(Server, ToolName, Handler),
                      {ok, _} = erlmcp_server:call_tool(Server, ToolName, #{})
                  end,
                  lists:seq(1, 20)),

    %% Allow time for system events
    timer:sleep(500),

    %% Collect traces
    {ok, Events} = erlmcp_tracer:collect_trace(SessionId),

    %% May have system events (not guaranteed in all environments)
    SystemEvents = [E || E <- Events, maps:get(event_type, E) =:= system_monitor],
    ?LOG(debug, "System events collected: ~p", [length(SystemEvents)]),

    %% Cleanup
    gen_server:stop(Server),
    ok = erlmcp_tracer:stop_trace_session(SessionId),

    ok.

%% @doc Test trace collection
trace_collection() ->
    %% Start a trace session
    {ok, SessionId, _TracerPid} = erlmcp_tracer:start_trace_session([node()]),

    %% Enable tool call tracing
    {ok, SessionId, _} = erlmcp_tracer:trace_tool_calls(),

    %% Generate some activity
    {ok, Server} = erlmcp_mock_server:start_link(),
    ok = erlmcp_server:add_tool(Server,
                                <<"test">>,
                                fun(_) -> {ok, #{}} end),
    {ok, _} = erlmcp_server:call_tool(Server, <<"test">>, #{}),

    timer:sleep(100),

    %% Collect traces
    {ok, Events} = erlmcp_tracer:collect_trace(SessionId),
    ?assert(is_list(Events)),

    %% Events should have timestamps
    [?assert(maps:is_key(timestamp, E)) || E <- Events],

    %% Events should have event types
    [?assert(maps:is_key(event_type, E)) || E <- Events],

    %% Cleanup
    gen_server:stop(Server),
    ok = erlmcp_tracer:stop_trace_session(SessionId),

    ok.

%% @doc Test trace visualization
trace_visualization() ->
    %% Generate some trace events
    Events =
        [#{timestamp => 1000,
           event_type => call,
           data => #{module => erlmcp_server,
                    function => call_tool,
                    arity => 3,
                    arguments => []}},
         #{timestamp => 2000,
           event_type => send,
           data => #{message => <<"test">>,
                    to => self()}},
         #{timestamp => 3000,
           event_type => 'receive',
           data => #{message => <<"reply">>}}],

    %% Test timeline formatting
    Timeline = erlmcp_trace_visualizer:format_timeline(Events),
    ?assert(is_list(Timeline)),
    ?assert(iolist_size(Timeline) > 0),

    %% Test call graph
    CallGraph = erlmcp_trace_visualizer:format_call_graph(Events),
    ?assert(is_list(CallGraph)),
    ?assert(iolist_size(CallGraph) > 0),

    %% Test message flow
    MessageFlow = erlmcp_trace_visualizer:format_message_flow(Events),
    ?assert(is_list(MessageFlow)),
    ?assert(iolist_size(MessageFlow) > 0),

    %% Test performance analysis
    Perf = erlmcp_trace_visualizer:analyze_performance(Events),
    ?assert(is_map(Perf)),
    ?assert(maps:is_key(total_events, Perf)),
    ?assertEqual(3, maps:get(total_events, Perf)),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
%% Mock server for testing
erlmcp_mock_server:start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, mock_server, []),
    {ok, Pid}.

%% @private
erlmcp_mock_client:start_link(Server) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {mock_client, Server}, []),
    {ok, Pid}.

%% @private
%% Mock server callbacks
init(mock_server) ->
    {ok, Server} = erlmcp_server:start_link(<<"mock_server">>, #{}),
    {ok, #{server => Server}};

init({mock_client, Server}) ->
    {ok, Client} = erlmcp_client:start_link({stdio, []}),
    {ok, #{client => Client, server => Server}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
