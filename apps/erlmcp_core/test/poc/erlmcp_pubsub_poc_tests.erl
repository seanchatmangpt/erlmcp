%%%-----------------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_pubsub_poc
%%%
%%% Chicago School TDD - Tests observable behavior through public API.
%%% NO MOCKS - Uses real pg module and real processes.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_pubsub_poc_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

%% Setup/teardown for each test
setup() ->
    %% Start the pubsub server
    {ok, Pid} = erlmcp_pubsub_poc:start(),
    Pid.

cleanup(Pid) ->
    %% Stop the server
    try
        erlmcp_pubsub_poc:stop()
    catch
        _:_ -> ok
    end,

    %% Ensure process is dead
    case is_process_alive(Pid) of
        true -> exit(Pid, kill);
        false -> ok
    end,

    %% Clean up any registered processes
    lists:foreach(fun(Name) ->
        case whereis(Name) of
            undefined -> ok;
            P -> exit(P, kill)
        end
    end, [agent_1, agent_2, agent_3, agent_4, agent_5]),

    ok.

%%%=============================================================================
%%% Basic Functionality Tests
%%%=============================================================================

start_stop_test() ->
    %% Start server
    {ok, Pid} = erlmcp_pubsub_poc:start(),
    ?assert(is_process_alive(Pid)),

    %% Stop server
    ok = erlmcp_pubsub_poc:stop(),

    %% Wait for process to die
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

subscribe_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Topic = "test:subscribe",
                     ok = erlmcp_pubsub_poc:subscribe(Topic, self()),

                     %% Verify subscription by checking members
                     Subs = erlmcp_pubsub_poc:list_subscribers(Topic),
                     ?assert(lists:member(self(), Subs))
                 end)
         ]
     end}.

unsubscribe_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Topic = "test:unsubscribe",

                     %% Subscribe
                     ok = erlmcp_pubsub_poc:subscribe(Topic, self()),
                     ?assert(lists:member(self(), erlmcp_pubsub_poc:list_subscribers(Topic))),

                     %% Unsubscribe
                     ok = erlmcp_pubsub_poc:unsubscribe(Topic, self()),
                     ?assertNot(lists:member(self(), erlmcp_pubsub_poc:list_subscribers(Topic)))
                 end)
         ]
     end}.

broadcast_to_single_subscriber_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Topic = "test:single",
                     Message = #{type => test, data => "hello"},

                     %% Subscribe
                     ok = erlmcp_pubsub_poc:subscribe(Topic, self()),

                     %% Broadcast
                     ok = erlmcp_pubsub_poc:broadcast(Topic, Message),

                     %% Receive message
                     receive
                         {pubsub_message, RecvTopic, RecvMsg} ->
                             ?assertEqual(Topic, RecvTopic),
                             ?assertEqual(Message, RecvMsg)
                     after 1000 ->
                         ?assert(false)  % Timeout
                     end
                 end)
         ]
     end}.

broadcast_to_multiple_subscribers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Topic = "test:multiple",
                     Message = #{type => test, data => "broadcast"},

                     %% Create 3 subscriber processes
                     Parent = self(),
                     Subs = [spawn_link(fun() -> subscriber_helper(Parent) end) || _ <- lists:seq(1, 3)],

                     %% Subscribe all
                     lists:foreach(fun(Sub) ->
                         ok = erlmcp_pubsub_poc:subscribe(Topic, Sub)
                     end, Subs),

                     %% Broadcast
                     ok = erlmcp_pubsub_poc:broadcast(Topic, Message),

                     %% Wait for all 3 to receive
                     Received = receive_n_acks(3, 2000),
                     ?assertEqual(3, length(Received)),

                     %% Cleanup
                     lists:foreach(fun(Sub) -> exit(Sub, kill) end, Subs)
                 end)
         ]
     end}.

topic_isolation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     TopicA = "test:topic_a",
                     TopicB = "test:topic_b",

                     %% Subscribe only to topic A
                     ok = erlmcp_pubsub_poc:subscribe(TopicA, self()),

                     %% Broadcast to topic B
                     ok = erlmcp_pubsub_poc:broadcast(TopicB, #{data => "should not receive"}),

                     %% Should NOT receive anything
                     receive
                         {pubsub_message, _, _} ->
                             ?assert(false)  % Should not get here
                     after 200 ->
                         ok  % Expected timeout
                     end,

                     %% Broadcast to topic A
                     ok = erlmcp_pubsub_poc:broadcast(TopicA, #{data => "should receive"}),

                     %% Should receive from topic A
                     receive
                         {pubsub_message, TopicA, #{data := "should receive"}} ->
                             ok
                     after 1000 ->
                         ?assert(false)  % Timeout
                     end
                 end)
         ]
     end}.

%%%=============================================================================
%%% Metrics Tests
%%%=============================================================================

metrics_collection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Topic = "test:metrics",

                     %% Reset metrics
                     ok = erlmcp_pubsub_poc:reset_metrics(),

                     %% Subscribe 2 processes
                     ok = erlmcp_pubsub_poc:subscribe(Topic, self()),
                     Sub2 = spawn(fun() -> timer:sleep(5000) end),
                     ok = erlmcp_pubsub_poc:subscribe(Topic, Sub2),

                     %% Broadcast 5 times with metrics
                     lists:foreach(fun(_) ->
                         erlmcp_pubsub_poc:broadcast(Topic, #{seq => 1}, #{collect_metrics => true})
                     end, lists:seq(1, 5)),

                     %% Get metrics
                     Metrics = erlmcp_pubsub_poc:get_metrics(),

                     %% Verify
                     ?assertEqual(5, maps:get(total_broadcasts, Metrics)),
                     ?assertEqual(10, maps:get(total_messages_sent, Metrics)),  % 5 broadcasts * 2 subs

                     Latencies = maps:get(latencies_us, Metrics),
                     ?assertEqual(5, length(Latencies)),

                     %% Cleanup
                     exit(Sub2, kill)
                 end)
         ]
     end}.

reset_metrics_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Topic = "test:reset",

                     %% Do some broadcasts with metrics
                     ok = erlmcp_pubsub_poc:subscribe(Topic, self()),
                     erlmcp_pubsub_poc:broadcast(Topic, #{data => 1}, #{collect_metrics => true}),

                     %% Verify metrics exist
                     Metrics1 = erlmcp_pubsub_poc:get_metrics(),
                     ?assertEqual(1, maps:get(total_broadcasts, Metrics1)),

                     %% Reset
                     ok = erlmcp_pubsub_poc:reset_metrics(),

                     %% Verify metrics reset
                     Metrics2 = erlmcp_pubsub_poc:get_metrics(),
                     ?assertEqual(0, maps:get(total_broadcasts, Metrics2)),
                     ?assertEqual(0, maps:get(total_messages_sent, Metrics2)),
                     ?assertEqual([], maps:get(latencies_us, Metrics2))
                 end)
         ]
     end}.

%%%=============================================================================
%%% MCP Use Case Tests
%%%=============================================================================

resource_subscription_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Simulate MCP resource subscription
                     ResourceURI = "resource:weather:sf",

                     %% Client subscribes
                     ok = erlmcp_pubsub_poc:subscribe(ResourceURI, self()),

                     %% Resource provider broadcasts update
                     Update = #{
                         type => resource_update,
                         uri => "weather:sf",
                         data => #{
                             temperature => 72,
                             conditions => "sunny"
                         }
                     },
                     ok = erlmcp_pubsub_poc:broadcast(ResourceURI, Update),

                     %% Client receives update
                     receive
                         {pubsub_message, ResourceURI, RecvUpdate} ->
                             ?assertEqual(Update, RecvUpdate),
                             ?assertEqual(resource_update, maps:get(type, RecvUpdate))
                     after 1000 ->
                         ?assert(false)
                     end
                 end)
         ]
     end}.

tool_streaming_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Simulate MCP tool result streaming
                     ToolTopic = "tool:llm-generate:req-123",

                     %% Client subscribes to stream
                     ok = erlmcp_pubsub_poc:subscribe(ToolTopic, self()),

                     %% Tool streams 3 chunks
                     Chunks = [
                         #{type => tool_chunk, chunk_id => 1, data => "Hello ", is_final => false},
                         #{type => tool_chunk, chunk_id => 2, data => "World", is_final => false},
                         #{type => tool_chunk, chunk_id => 3, data => "!", is_final => true}
                     ],

                     lists:foreach(fun(Chunk) ->
                         ok = erlmcp_pubsub_poc:broadcast(ToolTopic, Chunk)
                     end, Chunks),

                     %% Client receives all 3 chunks in order
                     RecvChunks = receive_chunks(3, 2000),
                     ?assertEqual(3, length(RecvChunks)),

                     %% Verify final chunk
                     FinalChunk = lists:last(RecvChunks),
                     ?assertEqual(true, maps:get(is_final, FinalChunk))
                 end)
         ]
     end}.

multi_agent_broadcast_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     %% Simulate multiple AI agents subscribing to same resource
                     Topic = "resource:kb:updates",

                     %% Create 10 agent processes
                     Parent = self(),
                     Agents = [spawn_link(fun() -> subscriber_helper(Parent) end) || _ <- lists:seq(1, 10)],

                     %% All agents subscribe
                     lists:foreach(fun(Agent) ->
                         ok = erlmcp_pubsub_poc:subscribe(Topic, Agent)
                     end, Agents),

                     %% Broadcast knowledge base update
                     KBUpdate = #{
                         type => kb_update,
                         operation => insert,
                         doc_id => "doc-123",
                         content => "New knowledge"
                     },
                     ok = erlmcp_pubsub_poc:broadcast(Topic, KBUpdate),

                     %% All 10 agents should receive
                     Received = receive_n_acks(10, 3000),
                     ?assertEqual(10, length(Received)),

                     %% Cleanup
                     lists:foreach(fun(Agent) -> exit(Agent, kill) end, Agents)
                 end)
         ]
     end}.

%%%=============================================================================
%%% Performance Tests
%%%=============================================================================

fan_out_latency_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(begin
                     Topic = "test:perf",

                     %% Create 5 subscribers
                     Subs = [spawn(fun() -> timer:sleep(5000) end) || _ <- lists:seq(1, 5)],
                     lists:foreach(fun(Sub) ->
                         ok = erlmcp_pubsub_poc:subscribe(Topic, Sub)
                     end, Subs),

                     %% Measure 100 broadcasts
                     Latencies = lists:map(fun(_) ->
                         Start = erlang:monotonic_time(microsecond),
                         ok = erlmcp_pubsub_poc:broadcast(Topic, #{seq => 1}),
                         End = erlang:monotonic_time(microsecond),
                         End - Start
                     end, lists:seq(1, 100)),

                     %% Verify reasonable latencies (< 200μs for 5 subs)
                     AvgLatency = lists:sum(Latencies) / length(Latencies),
                     MaxLatency = lists:max(Latencies),

                     ?assert(AvgLatency < 200),  % Avg < 200μs
                     ?assert(MaxLatency < 1000), % Max < 1ms

                     %% Cleanup
                     lists:foreach(fun(Sub) -> exit(Sub, kill) end, Subs)
                 end)
         ]
     end}.

high_frequency_broadcasts_test_() ->
    {timeout, 60,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(_Pid) ->
          [
           ?_test(begin
                      Topic = "test:high_freq",

                      %% Subscribe
                      ok = erlmcp_pubsub_poc:subscribe(Topic, self()),

                      %% Broadcast 1000 messages rapidly
                      Start = erlang:monotonic_time(millisecond),
                      lists:foreach(fun(N) ->
                          ok = erlmcp_pubsub_poc:broadcast(Topic, #{seq => N})
                      end, lists:seq(1, 1000)),
                      End = erlang:monotonic_time(millisecond),

                      Duration = End - Start,
                      Throughput = 1000 * 1000 / Duration,  % msg/s

                      %% Should handle > 10K msg/s
                      ?assert(Throughput > 10000),

                      %% Drain mailbox (don't need to verify all received for this test)
                      flush_mailbox()
                  end)
          ]
      end}}.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% Helper process that acks back to parent when it receives a message
subscriber_helper(Parent) ->
    receive
        {pubsub_message, _Topic, _Msg} ->
            Parent ! {ack, self()},
            subscriber_helper(Parent);
        stop ->
            ok
    end.

%% Receive N acknowledgments from helper processes
receive_n_acks(N, Timeout) ->
    receive_n_acks(N, Timeout, []).

receive_n_acks(0, _Timeout, Acc) ->
    lists:reverse(Acc);
receive_n_acks(N, Timeout, Acc) ->
    receive
        {ack, Pid} ->
            receive_n_acks(N - 1, Timeout, [Pid | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

%% Receive N chunks (tool streaming)
receive_chunks(N, Timeout) ->
    receive_chunks(N, Timeout, []).

receive_chunks(0, _Timeout, Acc) ->
    lists:reverse(Acc);
receive_chunks(N, Timeout, Acc) ->
    receive
        {pubsub_message, _Topic, Chunk} ->
            receive_chunks(N - 1, Timeout, [Chunk | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

%% Flush mailbox
flush_mailbox() ->
    receive
        _ -> flush_mailbox()
    after 0 ->
        ok
    end.
