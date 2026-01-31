%%%-------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_streaming_poc
%%%
%%% Tests streaming tool results POC functionality including:
%%% - Basic streaming mechanics
%%% - Multiple subscribers
%%% - Backpressure handling
%%% - Completion signaling
%%% - Latency measurement
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_streaming_poc_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

erlmcp_streaming_poc_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_start_stop/1,
      fun test_execute_no_subscribers/1,
      fun test_single_subscriber/1,
      fun test_multiple_subscribers/1,
      fun test_unsubscribe/1,
      fun test_dead_subscriber/1,
      fun test_backpressure_drop/1,
      fun test_stream_complete/1,
      fun test_concurrent_executions/1,
      fun test_latency_measurement/1
     ]}.

setup() ->
    {ok, Server} = erlmcp_streaming_poc:start_link(),
    Server.

cleanup(Server) ->
    erlmcp_streaming_poc:stop(Server).

%%====================================================================
%% Test Cases
%%====================================================================

test_start_stop(Server) ->
    fun() ->
        ?assert(is_pid(Server)),
        ?assert(is_process_alive(Server))
    end.

test_execute_no_subscribers(Server) ->
    fun() ->
        %% Execute tool with no subscribers
        {ok, ExecutionId} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 3, delay_ms => 10, chunk_size => 10}
        ),
        ?assert(is_reference(ExecutionId)),

        %% Wait for execution to complete
        timer:sleep(100),

        %% Server should still be alive
        ?assert(is_process_alive(Server))
    end.

test_single_subscriber(Server) ->
    fun() ->
        %% Start subscriber
        Subscriber = spawn_test_subscriber(),

        %% Execute tool
        {ok, ExecutionId} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 5, delay_ms => 10, chunk_size => 10}
        ),

        %% Subscribe
        Subscriber ! {subscribe, Server, ExecutionId},

        %% Wait for completion
        timer:sleep(200),

        %% Check received chunks
        Chunks = get_subscriber_chunks(Subscriber),
        ?assertEqual(5, length(Chunks)),

        %% Verify chunk sequence
        ChunkNums = [maps:get(chunk_num, C) || {_ExecId, C, _Time} <- Chunks],
        ?assertEqual([1, 2, 3, 4, 5], ChunkNums),

        %% Cleanup
        stop_subscriber(Subscriber)
    end.

test_multiple_subscribers(Server) ->
    fun() ->
        %% Start 3 subscribers
        Subscribers = [spawn_test_subscriber() || _ <- lists:seq(1, 3)],

        %% Execute tool
        {ok, ExecutionId} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 4, delay_ms => 10, chunk_size => 10}
        ),

        %% Subscribe all
        lists:foreach(fun(Sub) ->
            Sub ! {subscribe, Server, ExecutionId}
        end, Subscribers),
        timer:sleep(50),

        %% Wait for completion
        timer:sleep(200),

        %% All subscribers should receive all chunks
        lists:foreach(fun(Sub) ->
            Chunks = get_subscriber_chunks(Sub),
            ?assertEqual(4, length(Chunks))
        end, Subscribers),

        %% Cleanup
        lists:foreach(fun stop_subscriber/1, Subscribers)
    end.

test_unsubscribe(Server) ->
    fun() ->
        %% Start subscriber
        Subscriber = spawn_test_subscriber(),

        %% Execute tool
        {ok, ExecutionId} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 20, delay_ms => 50, chunk_size => 10}
        ),

        %% Subscribe
        Subscriber ! {subscribe, Server, ExecutionId},
        timer:sleep(150),

        %% Unsubscribe after receiving some chunks
        %% Note: We tell the subscriber to unsubscribe itself
        Subscriber ! {unsubscribe, Server, ExecutionId},
        timer:sleep(50),

        %% Wait for execution to complete
        timer:sleep(1000),

        %% Should have received fewer than 20 chunks
        Chunks = get_subscriber_chunks(Subscriber),
        ?assert(length(Chunks) < 20),

        %% Cleanup
        stop_subscriber(Subscriber)
    end.

test_dead_subscriber(Server) ->
    fun() ->
        %% Start subscriber
        Subscriber = spawn_test_subscriber(),

        %% Execute tool
        {ok, ExecutionId} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 10, delay_ms => 20, chunk_size => 10}
        ),

        %% Subscribe
        Subscriber ! {subscribe, Server, ExecutionId},
        timer:sleep(50),

        %% Kill subscriber
        exit(Subscriber, kill),
        timer:sleep(50),

        %% Server should still be alive and handle remaining chunks
        ?assert(is_process_alive(Server)),

        %% Wait for execution to complete
        timer:sleep(300),

        %% Server should still be alive
        ?assert(is_process_alive(Server))
    end.

test_backpressure_drop(Server) ->
    fun() ->
        %% This test verifies backpressure handling
        %% Since we use the 'drop' strategy, slow subscribers
        %% should drop chunks when their mailbox is full

        %% Start a slow subscriber
        SlowSubscriber = spawn_slow_subscriber(100), % 100ms per chunk

        %% Execute fast tool (10ms per chunk)
        {ok, ExecutionId} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 20, delay_ms => 10, chunk_size => 10}
        ),

        %% Subscribe
        SlowSubscriber ! {subscribe, Server, ExecutionId},
        timer:sleep(50),

        %% Wait for execution to complete
        timer:sleep(500),

        %% Slow subscriber should have received fewer chunks than sent
        Chunks = get_subscriber_chunks(SlowSubscriber),
        ?assert(length(Chunks) < 20),

        %% Cleanup
        stop_subscriber(SlowSubscriber)
    end.

test_stream_complete(Server) ->
    fun() ->
        %% Start subscriber
        Subscriber = spawn_test_subscriber(),

        %% Execute tool
        {ok, ExecutionId} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 3, delay_ms => 10, chunk_size => 10}
        ),

        %% Subscribe
        Subscriber ! {subscribe, Server, ExecutionId},
        timer:sleep(50),

        %% Wait for completion
        timer:sleep(200),

        %% Check completion signal
        Subscriber ! {get_completion, self()},
        receive
            {completion, ReceivedExecutionId} ->
                ?assertEqual(ExecutionId, ReceivedExecutionId)
        after 1000 ->
            ?assert(false)
        end,

        %% Cleanup
        stop_subscriber(Subscriber)
    end.

test_concurrent_executions(Server) ->
    fun() ->
        %% Execute multiple tools concurrently
        {ok, ExecId1} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 3, delay_ms => 10, chunk_size => 10}
        ),
        {ok, ExecId2} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 3, delay_ms => 10, chunk_size => 10}
        ),

        %% Verify different execution IDs
        ?assertNotEqual(ExecId1, ExecId2),

        %% Start subscribers for each
        Sub1 = spawn_test_subscriber(),
        Sub2 = spawn_test_subscriber(),

        Sub1 ! {subscribe, Server, ExecId1},
        Sub2 ! {subscribe, Server, ExecId2},
        timer:sleep(50),

        %% Wait for both to complete
        timer:sleep(200),

        %% Both should receive their chunks
        Chunks1 = get_subscriber_chunks(Sub1),
        Chunks2 = get_subscriber_chunks(Sub2),

        ?assertEqual(3, length(Chunks1)),
        ?assertEqual(3, length(Chunks2)),

        %% Cleanup
        stop_subscriber(Sub1),
        stop_subscriber(Sub2)
    end.

test_latency_measurement(Server) ->
    fun() ->
        %% Start subscriber
        Subscriber = spawn_test_subscriber(),

        %% Execute tool
        {ok, ExecutionId} = erlmcp_streaming_poc:execute_tool(
            Server,
            <<"slow_data_processor">>,
            #{chunks => 5, delay_ms => 10, chunk_size => 10}
        ),

        %% Subscribe
        Subscriber ! {subscribe, Server, ExecutionId},
        timer:sleep(50),

        %% Wait for completion
        timer:sleep(200),

        %% Get latencies
        Chunks = get_subscriber_chunks(Subscriber),
        Latencies = [ReceiveTime - SendTime || {_ExecId, _Chunk, {SendTime, ReceiveTime}} <- Chunks],

        %% All latencies should be positive and reasonable (<100ms)
        lists:foreach(fun(Latency) ->
            ?assert(Latency > 0),
            ?assert(Latency < 100000) % Less than 100ms in microseconds
        end, Latencies),

        %% Cleanup
        stop_subscriber(Subscriber)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

spawn_test_subscriber() ->
    spawn(fun() -> test_subscriber_loop([], undefined) end).

spawn_slow_subscriber(DelayMs) ->
    spawn(fun() -> slow_subscriber_loop(DelayMs, [], undefined) end).

test_subscriber_loop(Chunks, Completion) ->
    receive
        {subscribe, Server, ExecutionId} ->
            ok = erlmcp_streaming_poc:subscribe(Server, ExecutionId),
            test_subscriber_loop(Chunks, Completion);

        {unsubscribe, Server, ExecutionId} ->
            ok = erlmcp_streaming_poc:unsubscribe(Server, ExecutionId),
            test_subscriber_loop(Chunks, Completion);

        {stream_chunk, ExecutionId, ChunkData, SendTime} ->
            ReceiveTime = erlang:monotonic_time(microsecond),
            NewChunks = [{ExecutionId, ChunkData, {SendTime, ReceiveTime}} | Chunks],
            test_subscriber_loop(NewChunks, Completion);

        {stream_complete, ExecutionId} ->
            test_subscriber_loop(Chunks, ExecutionId);

        {get_chunks, From} ->
            From ! {chunks, lists:reverse(Chunks)},
            test_subscriber_loop(Chunks, Completion);

        {get_completion, From} ->
            From ! {completion, Completion},
            test_subscriber_loop(Chunks, Completion);

        stop ->
            ok
    end.

slow_subscriber_loop(DelayMs, Chunks, Completion) ->
    receive
        {subscribe, Server, ExecutionId} ->
            ok = erlmcp_streaming_poc:subscribe(Server, ExecutionId),
            slow_subscriber_loop(DelayMs, Chunks, Completion);

        {stream_chunk, ExecutionId, ChunkData, SendTime} ->
            ReceiveTime = erlang:monotonic_time(microsecond),
            timer:sleep(DelayMs), % Simulate slow processing
            NewChunks = [{ExecutionId, ChunkData, {SendTime, ReceiveTime}} | Chunks],
            slow_subscriber_loop(DelayMs, NewChunks, Completion);

        {stream_complete, ExecutionId} ->
            slow_subscriber_loop(DelayMs, Chunks, ExecutionId);

        {get_chunks, From} ->
            From ! {chunks, lists:reverse(Chunks)},
            slow_subscriber_loop(DelayMs, Chunks, Completion);

        stop ->
            ok
    after 0 ->
        slow_subscriber_loop(DelayMs, Chunks, Completion)
    end.

get_subscriber_chunks(Subscriber) ->
    Subscriber ! {get_chunks, self()},
    receive
        {chunks, Chunks} -> Chunks
    after 1000 ->
        []
    end.

stop_subscriber(Subscriber) ->
    Subscriber ! stop,
    timer:sleep(10).
