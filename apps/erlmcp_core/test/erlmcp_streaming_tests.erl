%% @doc EUnit tests for erlmcp_streaming module
%% Tests streaming execution manager using real gen_server
%% Chicago School TDD: Real processes, real monitors, state-based verification
-module(erlmcp_streaming_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

streaming_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(start_stream_single_subscriber_test(Pid)),
          ?_test(start_stream_multiple_subscribers_test(Pid)),
          ?_test(start_stream_already_streaming_test(Pid)),
          ?_test(send_chunk_test(Pid)),
          ?_test(complete_stream_test(Pid)),
          ?_test(error_stream_test(Pid)),
          ?_test(cancel_stream_test(Pid)),
          ?_test(is_streaming_test(Pid)),
          ?_test(get_subscribers_test(Pid)),
          ?_test(subscriber_death_cleanup_test(Pid)),
          ?_test(multiple_subscribers_death_test(Pid)),
          ?_test(multiple_streams_concurrent_test(Pid)),
          ?_test(streaming_workflow_test(Pid))
         ]
     end}.

%%====================================================================
%% Setup/Cleanup
%%====================================================================

setup() ->
    %% Start real erlmcp_streaming gen_server
    {ok, Pid} = erlmcp_streaming:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop streaming server
    catch erlmcp_streaming:stop(Pid),
    ok.

%%====================================================================
%% Test Functions (Chicago School: verify observable behavior)
%%====================================================================

start_stream_single_subscriber_test(Pid) ->
    %% Setup: Create subscriber
    Subscriber = spawn_subscriber(),
    ExecId = make_ref(),

    %% Exercise: Start stream
    ok = erlmcp_streaming:start_stream(ExecId, Subscriber),

    %% Verify: Stream is active
    ?assert(erlmcp_streaming:is_streaming(ExecId)),

    %% Verify: Subscriber listed
    {ok, Subs} = erlmcp_streaming:get_subscribers(ExecId),
    ?assertEqual([Subscriber], Subs),

    %% Cleanup
    ok = erlmcp_streaming:cancel_stream(ExecId),
    exit(Subscriber, kill).

start_stream_multiple_subscribers_test(Pid) ->
    %% Setup: Create 5 subscribers
    Subscribers = [spawn_subscriber() || _ <- lists:seq(1, 5)],
    ExecId = make_ref(),

    %% Exercise: Start stream with multiple subscribers
    ok = erlmcp_streaming:start_stream(ExecId, Subscribers),

    %% Verify: All subscribers listed
    {ok, Subs} = erlmcp_streaming:get_subscribers(ExecId),
    ?assertEqual(5, length(Subs)),
    [?assert(lists:member(Sub, Subs)) || Sub <- Subscribers],

    %% Cleanup
    ok = erlmcp_streaming:cancel_stream(ExecId),
    [exit(Sub, kill) || Sub <- Subscribers].

start_stream_already_streaming_test(Pid) ->
    %% Setup: Start a stream
    Subscriber = spawn_subscriber(),
    ExecId = make_ref(),
    ok = erlmcp_streaming:start_stream(ExecId, Subscriber),

    %% Exercise: Attempt to start same execution ID again
    Result = erlmcp_streaming:start_stream(ExecId, spawn_subscriber()),

    %% Verify: Error returned (state verification)
    ?assertEqual({error, already_streaming}, Result),

    %% Cleanup
    ok = erlmcp_streaming:cancel_stream(ExecId),
    exit(Subscriber, kill).

send_chunk_test(Pid) ->
    %% Setup: Start stream with subscriber
    Self = self(),
    Subscriber = spawn(fun() ->
        receive
            {stream_chunk, ExecIdMsg, Chunk} ->
                Self ! {received_chunk, ExecIdMsg, Chunk},
                %% Keep subscriber alive for cleanup
                receive stop -> ok end
        after 2000 ->
            Self ! {chunk_timeout}
        end
    end),
    ExecId = make_ref(),
    ok = erlmcp_streaming:start_stream(ExecId, Subscriber),

    %% Exercise: Send chunk (cast is async, allow time for delivery)
    Chunk = {data, <<"chunk 1">>},
    ok = erlmcp_streaming:send_chunk(ExecId, Chunk),
    timer:sleep(50), %% Allow message delivery

    %% Verify: Subscriber received chunk (observable behavior)
    receive
        {received_chunk, ExecId, Chunk} ->
            ok;
        {chunk_timeout} ->
            ?assert(false, "Subscriber timeout waiting for chunk")
    after 1000 ->
        ?assert(false, "Test process timeout waiting for chunk confirmation")
    end,

    %% Cleanup
    ok = erlmcp_streaming:cancel_stream(ExecId),
    timer:sleep(50), %% Allow cancellation to process
    Subscriber ! stop.

complete_stream_test(Pid) ->
    %% Setup: Start stream
    Self = self(),
    Subscriber = spawn(fun() ->
        receive
            {stream_complete, ExecIdMsg, FinalResult} ->
                Self ! {completed, ExecIdMsg, FinalResult}
        after 2000 ->
            Self ! {complete_timeout}
        end
    end),
    ExecId = make_ref(),
    ok = erlmcp_streaming:start_stream(ExecId, Subscriber),

    %% Exercise: Complete stream (cast is async, allow time for delivery)
    FinalResult = {result, <<"done">>},
    ok = erlmcp_streaming:complete_stream(ExecId, FinalResult),
    timer:sleep(50), %% Allow message delivery and cleanup

    %% Verify: Subscriber received completion
    receive
        {completed, ExecId, FinalResult} ->
            ok;
        {complete_timeout} ->
            ?assert(false, "Subscriber timeout waiting for completion")
    after 1000 ->
        ?assert(false, "Test process timeout waiting for completion confirmation")
    end,

    %% Verify: Stream no longer active (state verification)
    ?assertNot(erlmcp_streaming:is_streaming(ExecId)),
    exit(Subscriber, kill).

error_stream_test(Pid) ->
    %% Setup: Start stream
    Self = self(),
    Subscriber = spawn(fun() ->
        receive
            {stream_error, ExecIdMsg, Error} ->
                Self ! {errored, ExecIdMsg, Error}
        after 2000 ->
            Self ! {error_timeout}
        end
    end),
    ExecId = make_ref(),
    ok = erlmcp_streaming:start_stream(ExecId, Subscriber),

    %% Exercise: Error stream (cast is async, allow time for delivery)
    Error = {timeout, 5000},
    ok = erlmcp_streaming:error_stream(ExecId, Error),
    timer:sleep(50), %% Allow message delivery and cleanup

    %% Verify: Subscriber received error
    receive
        {errored, ExecId, Error} ->
            ok;
        {error_timeout} ->
            ?assert(false, "Subscriber timeout waiting for error")
    after 1000 ->
        ?assert(false, "Test process timeout waiting for error confirmation")
    end,

    %% Verify: Stream cleaned up
    ?assertNot(erlmcp_streaming:is_streaming(ExecId)),
    exit(Subscriber, kill).

cancel_stream_test(Pid) ->
    %% Setup: Start stream
    Self = self(),
    Subscriber = spawn(fun() ->
        receive
            {stream_cancelled, ExecIdMsg} ->
                Self ! {cancelled, ExecIdMsg}
        after 2000 ->
            Self ! {cancel_timeout}
        end
    end),
    ExecId = make_ref(),
    ok = erlmcp_streaming:start_stream(ExecId, Subscriber),

    %% Exercise: Cancel stream (call is synchronous but message delivery is async)
    ok = erlmcp_streaming:cancel_stream(ExecId),
    timer:sleep(50), %% Allow message delivery to subscriber

    %% Verify: Subscriber notified of cancellation
    receive
        {cancelled, ExecId} ->
            ok;
        {cancel_timeout} ->
            ?assert(false, "Subscriber timeout waiting for cancellation")
    after 1000 ->
        ?assert(false, "Test process timeout waiting for cancellation confirmation")
    end,

    %% Verify: Stream no longer active
    ?assertNot(erlmcp_streaming:is_streaming(ExecId)),
    exit(Subscriber, kill).

is_streaming_test(Pid) ->
    %% Setup: Start stream
    Subscriber = spawn_subscriber(),
    ExecId = make_ref(),

    %% Verify: Initially not streaming
    ?assertNot(erlmcp_streaming:is_streaming(ExecId)),

    %% Exercise: Start stream
    ok = erlmcp_streaming:start_stream(ExecId, Subscriber),

    %% Verify: Now streaming
    ?assert(erlmcp_streaming:is_streaming(ExecId)),

    %% Exercise: Cancel stream
    ok = erlmcp_streaming:cancel_stream(ExecId),
    timer:sleep(50), %% Allow cleanup to complete

    %% Verify: No longer streaming
    ?assertNot(erlmcp_streaming:is_streaming(ExecId)),

    %% Cleanup
    exit(Subscriber, kill).

get_subscribers_test(Pid) ->
    %% Setup: Stream not started
    ExecId = make_ref(),

    %% Verify: Not found for non-existent stream
    ?assertEqual({error, not_found}, erlmcp_streaming:get_subscribers(ExecId)),

    %% Exercise: Start stream with subscribers
    Subs = [spawn_subscriber() || _ <- lists:seq(1, 3)],
    ok = erlmcp_streaming:start_stream(ExecId, Subs),

    %% Verify: Subscribers returned
    {ok, Retrieved} = erlmcp_streaming:get_subscribers(ExecId),
    ?assertEqual(3, length(Retrieved)),

    %% Cleanup
    ok = erlmcp_streaming:cancel_stream(ExecId),
    [exit(Sub, kill) || Sub <- Subs].

subscriber_death_cleanup_test(Pid) ->
    %% Setup: Start stream with single subscriber
    Subscriber = spawn_subscriber(),
    ExecId = make_ref(),
    ok = erlmcp_streaming:start_stream(ExecId, Subscriber),

    %% Verify: Stream active
    ?assert(erlmcp_streaming:is_streaming(ExecId)),

    %% Exercise: Kill subscriber (real process death, DOWN message async)
    exit(Subscriber, kill),
    timer:sleep(100), %% Allow monitor DOWN message to trigger cleanup

    %% Verify: Stream automatically cleaned up (no subscribers left)
    ?assertNot(erlmcp_streaming:is_streaming(ExecId)).

multiple_subscribers_death_test(Pid) ->
    %% Setup: Start stream with 3 subscribers
    [Sub1, Sub2, Sub3] = [spawn_subscriber() || _ <- lists:seq(1, 3)],
    ExecId = make_ref(),
    ok = erlmcp_streaming:start_stream(ExecId, [Sub1, Sub2, Sub3]),

    %% Verify: Initially 3 subscribers
    {ok, Subs1} = erlmcp_streaming:get_subscribers(ExecId),
    ?assertEqual(3, length(Subs1)),

    %% Exercise: Kill one subscriber (DOWN message async)
    exit(Sub1, kill),
    timer:sleep(50), %% Allow monitor DOWN message to process

    %% Verify: 2 subscribers remain
    {ok, Subs2} = erlmcp_streaming:get_subscribers(ExecId),
    ?assertEqual(2, length(Subs2)),
    ?assert(lists:member(Sub2, Subs2)),
    ?assert(lists:member(Sub3, Subs2)),

    %% Exercise: Kill remaining subscribers
    exit(Sub2, kill),
    exit(Sub3, kill),
    timer:sleep(100), %% Allow both DOWN messages to process

    %% Verify: Stream cleaned up when all subscribers dead
    ?assertNot(erlmcp_streaming:is_streaming(ExecId)).

multiple_streams_concurrent_test(Pid) ->
    %% Setup: Start 10 concurrent streams
    Streams = [{make_ref(), spawn_subscriber()} || _ <- lists:seq(1, 10)],

    %% Exercise: Start all streams
    [ok = erlmcp_streaming:start_stream(ExecId, Sub) || {ExecId, Sub} <- Streams],

    %% Verify: All streams active
    [?assert(erlmcp_streaming:is_streaming(ExecId)) || {ExecId, _} <- Streams],

    %% Exercise: Complete half, cancel half (casts are async)
    {ToComplete, ToCancel} = lists:split(5, Streams),
    [ok = erlmcp_streaming:complete_stream(ExecId, done) || {ExecId, _} <- ToComplete],
    [ok = erlmcp_streaming:cancel_stream(ExecId) || {ExecId, _} <- ToCancel],

    timer:sleep(100), %% Allow all completions/cancellations to process

    %% Verify: All streams cleaned up
    [?assertNot(erlmcp_streaming:is_streaming(ExecId)) || {ExecId, _} <- Streams],

    %% Cleanup
    [exit(Sub, kill) || {_, Sub} <- Streams].

streaming_workflow_test(Pid) ->
    %% Complete workflow: start -> chunks -> complete
    Self = self(),
    Subscriber = spawn(fun() -> streaming_receiver(Self, []) end),
    ExecId = make_ref(),

    %% Exercise: Full streaming workflow
    ok = erlmcp_streaming:start_stream(ExecId, Subscriber),
    ok = erlmcp_streaming:send_chunk(ExecId, chunk1),
    timer:sleep(10), %% Allow message delivery
    ok = erlmcp_streaming:send_chunk(ExecId, chunk2),
    timer:sleep(10), %% Allow message delivery
    ok = erlmcp_streaming:send_chunk(ExecId, chunk3),
    timer:sleep(10), %% Allow message delivery
    ok = erlmcp_streaming:complete_stream(ExecId, final),

    %% Verify: Subscriber received all chunks + completion
    receive
        {workflow_complete, Messages} ->
            ?assertEqual(4, length(Messages)),
            ?assert(lists:member({chunk, chunk1}, Messages)),
            ?assert(lists:member({chunk, chunk2}, Messages)),
            ?assert(lists:member({chunk, chunk3}, Messages)),
            ?assert(lists:member({complete, final}, Messages))
    after 2000 ->
        ?assert(false, "Workflow timeout - messages not received")
    end,
    exit(Subscriber, kill).

%%====================================================================
%% Test Helpers (Chicago School: Real processes)
%%====================================================================

%% @doc Spawn a basic subscriber
spawn_subscriber() ->
    spawn(fun() ->
        receive
            _ -> ok
        end
    end).

%% @doc Receiver for workflow test
streaming_receiver(Parent, Acc) ->
    receive
        {stream_chunk, _, Chunk} ->
            streaming_receiver(Parent, [{chunk, Chunk} | Acc]);
        {stream_complete, _, FinalResult} ->
            Parent ! {workflow_complete, lists:reverse([{complete, FinalResult} | Acc])};
        {stream_error, _, Error} ->
            Parent ! {workflow_error, lists:reverse([{error, Error} | Acc])};
        {stream_cancelled, _} ->
            Parent ! {workflow_cancelled, lists:reverse([cancelled | Acc])}
    end.
