%%%-------------------------------------------------------------------
%%% @doc Multi-Stream Isolation Tests for SSE
%%%
%%% FM-03 Fix: SSE Multi-Stream Isolation Validation (RPN 280)
%%%
%%% Chicago School TDD approach:
%%% - Real gen_server (no mocks)
%%% - Test observable behavior through API calls only
%%% - NO state inspection (sys:get_status prohibited)
%%% - NO record duplication (respect encapsulation)
%%%
%%% Stress test scenarios:
%%% - 10 concurrent SSE streams, each producing 100 events
%%% - Resume from different positions (event 50, 75, 99)
%%% - Verify no event leak between streams
%%% - Verify correct event sequence for each resume point
%%% - 100 concurrent streams resuming independently
%%% - Memory usage validation (no linear growth with resume attempts)
%%% - Cross-stream event bleed detection at all resume points
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sse_multi_stream_isolation_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Setup fixture - Start real gen_server
%%--------------------------------------------------------------------
setup() ->
    %% Ensure any previous instance is stopped
    case whereis(erlmcp_sse_event_store) of
        undefined -> ok;
        OldPid -> exit(OldPid, kill), timer:sleep(100)
    end,
    {ok, Pid} = erlmcp_sse_event_store:start_link(),
    Pid.

%%--------------------------------------------------------------------
%% @doc Cleanup fixture - Stop gen_server and verify cleanup
%%--------------------------------------------------------------------
cleanup(Pid) ->
    %% Stop server
    case is_process_alive(Pid) of
        true -> gen_server:stop(erlmcp_sse_event_store);
        false -> ok
    end,
    %% Verify all ETS tables are cleaned up
    Tables = ets:all(),
    lists:foreach(fun(Table) ->
        case ets:info(Table, name) of
            {erlmcp_sse_event_store, _} -> ets:delete(Table);
            _ -> ok
        end
    end, Tables).

%%%===================================================================
%%% Test 1: 10 Concurrent Streams with 100 Events Each
%%%===================================================================

ten_streams_stress_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_ten_concurrent_streams()),
          ?_test(test_ten_streams_resume_at_50()),
          ?_test(test_ten_streams_resume_at_75()),
          ?_test(test_ten_streams_resume_at_99())
         ]
     end}.

test_ten_concurrent_streams() ->
    %% Create 10 concurrent SSE streams
    NumStreams = 10,
    EventsPerStream = 100,

    %% Generate session IDs
    SessionIds = lists:map(fun(N) ->
        <<"session_stress_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Spawn producer for each stream
    ResultsPid = self(),
    lists:foreach(fun(SessionId) ->
        spawn(fun() ->
            %% Produce 100 events
            ok = add_events(SessionId, lists:map(fun(EventNum) ->
                {EventNum, <<"stream_", SessionId/binary, "_event_", (integer_to_binary(EventNum))/binary>>}
            end, lists:seq(1, EventsPerStream))),
            ResultsPid ! {stream_complete, SessionId}
        end)
    end, SessionIds),

    %% Wait for all streams to complete
    lists:foreach(fun(SessionId) ->
        receive
            {stream_complete, SessionId} -> ok
        after 10000 ->
            ?assert(false) %% Timeout
        end
    end, SessionIds),

    %% Verify: Each stream has exactly 100 events (no event leak)
    lists:foreach(fun(SessionId) ->
        {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
        EventCount = maps:get(event_count, Info),
        ?assertEqual(EventsPerStream, EventCount)
    end, SessionIds),

    %% Verify: No cross-stream contamination
    lists:foreach(fun(SessionId) ->
        {ok, AllEvents} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
        ?assertEqual(EventsPerStream, length(AllEvents)),
        %% Verify each event belongs to this stream only
        lists:foreach(fun(EventData) ->
            ?assert(binary:match(EventData, SessionId) =/= nomatch)
        end, AllEvents)
    end, SessionIds).

test_ten_streams_resume_at_50() ->
    %% Create 10 streams with 100 events each
    NumStreams = 10,
    EventsPerStream = 100,
    ResumePoint = 50,

    SessionIds = lists:map(fun(N) ->
        <<"session_resume50_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Add events to all streams
    lists:foreach(fun(SessionId) ->
        ok = add_events(SessionId, lists:map(fun(EventNum) ->
            {EventNum, <<"data_", (integer_to_binary(EventNum))/binary>>}
        end, lists:seq(1, EventsPerStream)))
    end, SessionIds),

    %% Resume each stream from event 50
    lists:foreach(fun(SessionId) ->
        LastEventId = <<SessionId/binary, "_", (integer_to_binary(ResumePoint))/binary>>,
        {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, LastEventId),

        %% Verify: Exactly 50 events (51-100)
        ?assertEqual(50, length(Events)),

        %% Verify: Events in correct sequence
        ExpectedEvents = lists:map(fun(N) ->
            <<"data_", (integer_to_binary(N))/binary>>
        end, lists:seq(51, 100)),
        ?assertEqual(ExpectedEvents, Events)
    end, SessionIds).

test_ten_streams_resume_at_75() ->
    %% Create 10 streams with 100 events each
    NumStreams = 10,
    EventsPerStream = 100,
    ResumePoint = 75,

    SessionIds = lists:map(fun(N) ->
        <<"session_resume75_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Add events to all streams
    lists:foreach(fun(SessionId) ->
        ok = add_events(SessionId, lists:map(fun(EventNum) ->
            {EventNum, <<"data_", (integer_to_binary(EventNum))/binary>>}
        end, lists:seq(1, EventsPerStream)))
    end, SessionIds),

    %% Resume each stream from event 75
    lists:foreach(fun(SessionId) ->
        LastEventId = <<SessionId/binary, "_", (integer_to_binary(ResumePoint))/binary>>,
        {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, LastEventId),

        %% Verify: Exactly 25 events (76-100)
        ?assertEqual(25, length(Events)),

        %% Verify: Events in correct sequence
        ExpectedEvents = lists:map(fun(N) ->
            <<"data_", (integer_to_binary(N))/binary>>
        end, lists:seq(76, 100)),
        ?assertEqual(ExpectedEvents, Events)
    end, SessionIds).

test_ten_streams_resume_at_99() ->
    %% Create 10 streams with 100 events each
    NumStreams = 10,
    EventsPerStream = 100,
    ResumePoint = 99,

    SessionIds = lists:map(fun(N) ->
        <<"session_resume99_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Add events to all streams
    lists:foreach(fun(SessionId) ->
        ok = add_events(SessionId, lists:map(fun(EventNum) ->
            {EventNum, <<"data_", (integer_to_binary(EventNum))/binary>>}
        end, lists:seq(1, EventsPerStream)))
    end, SessionIds),

    %% Resume each stream from event 99
    lists:foreach(fun(SessionId) ->
        LastEventId = <<SessionId/binary, "_", (integer_to_binary(ResumePoint))/binary>>,
        {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, LastEventId),

        %% Verify: Exactly 1 event (100)
        ?assertEqual(1, length(Events)),

        %% Verify: Correct event
        ?assertEqual(<<"data_100">>, lists:nth(1, Events))
    end, SessionIds).

%%%===================================================================
%%% Test 2: 100 Concurrent Streams Resuming Independently
%%%===================================================================

hundred_streams_stress_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {timeout, 60, ?_test(test_hundred_concurrent_streams())},
          {timeout, 60, ?_test(test_hundred_streams_independent_resume())},
          {timeout, 60, ?_test(test_hundred_streams_cross_bleed_detection())}
         ]
     end}.

test_hundred_concurrent_streams() ->
    %% Create 100 concurrent streams
    NumStreams = 100,
    EventsPerStream = 50,

    SessionIds = lists:map(fun(N) ->
        <<"session_100_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Add events concurrently
    ResultsPid = self(),
    lists:foreach(fun(SessionId) ->
        spawn(fun() ->
            ok = add_events(SessionId, lists:map(fun(EventNum) ->
                {EventNum, <<"data_", (integer_to_binary(EventNum))/binary>>}
            end, lists:seq(1, EventsPerStream))),
            ResultsPid ! {stream_ready, SessionId}
        end)
    end, SessionIds),

    %% Wait for all streams
    lists:foreach(fun(SessionId) ->
        receive
            {stream_ready, SessionId} -> ok
        after 30000 ->
            ?assert(false) %% Timeout
        end
    end, SessionIds),

    %% Verify: Each stream has correct event count
    lists:foreach(fun(SessionId) ->
        {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
        ?assertEqual(EventsPerStream, maps:get(event_count, Info))
    end, SessionIds).

test_hundred_streams_independent_resume() ->
    %% Create 100 streams
    NumStreams = 100,
    EventsPerStream = 50,

    SessionIds = lists:map(fun(N) ->
        <<"session_indep_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Add events to all streams
    lists:foreach(fun(SessionId) ->
        ok = add_events(SessionId, lists:map(fun(EventNum) ->
            {EventNum, <<"event_", (integer_to_binary(EventNum))/binary>>}
        end, lists:seq(1, EventsPerStream)))
    end, SessionIds),

    %% Resume each stream from different position (based on stream number)
    ResultsPid = self(),
    lists:foreach(fun({Idx, SessionId}) ->
        spawn(fun() ->
            %% Resume position varies: stream 1 from 10, stream 2 from 20, etc.
            ResumeFrom = (Idx rem EventsPerStream) + 1,
            LastEventId = <<SessionId/binary, "_", (integer_to_binary(ResumeFrom))/binary>>,
            {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, LastEventId),

            ExpectedCount = EventsPerStream - ResumeFrom,
            ResultsPid ! {resume_result, SessionId, length(Events), ExpectedCount}
        end)
    end, lists:zip(lists:seq(1, NumStreams), SessionIds)),

    %% Verify all resumes correct
    SuccessCount = lists:foldl(fun(_, Acc) ->
        receive
            {resume_result, _SessionId, ActualCount, ExpectedCount} ->
                case ActualCount =:= ExpectedCount of
                    true -> Acc + 1;
                    false -> Acc
                end
        after 30000 ->
            Acc
        end
    end, 0, lists:seq(1, NumStreams)),

    %% Verify: All 100 streams resumed correctly
    ?assertEqual(NumStreams, SuccessCount).

test_hundred_streams_cross_bleed_detection() ->
    %% CRITICAL TEST: Ensure no event bleed between any of 100 streams
    NumStreams = 100,
    EventsPerStream = 50,

    SessionIds = lists:map(fun(N) ->
        <<"session_bleed_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Add unique events to each stream
    lists:foreach(fun({Idx, SessionId}) ->
        ok = add_events(SessionId, lists:map(fun(EventNum) ->
            %% Event data contains stream index for bleed detection
            {EventNum, <<"stream", (integer_to_binary(Idx))/binary, "_event", (integer_to_binary(EventNum))/binary>>}
        end, lists:seq(1, EventsPerStream)))
    end, lists:zip(lists:seq(1, NumStreams), SessionIds)),

    %% Verify: Each stream only contains its own events (no bleed)
    BleedCount = lists:foldl(fun({Idx, SessionId}, Acc) ->
        {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),

        %% Check each event belongs to this stream only
        StreamPrefix = <<"stream", (integer_to_binary(Idx))/binary>>,
        BleedEvents = lists:filter(fun(EventData) ->
            binary:match(EventData, StreamPrefix) =:= nomatch
        end, Events),

        Acc + length(BleedEvents)
    end, 0, lists:zip(lists:seq(1, NumStreams), SessionIds)),

    %% Verify: Zero bleed events across all 100 streams
    ?assertEqual(0, BleedCount).

%%%===================================================================
%%% Test 3: Memory Usage Validation
%%%===================================================================

memory_usage_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {timeout, 60, ?_test(test_memory_stable_with_resumes())},
          {timeout, 60, ?_test(test_no_linear_growth_with_resume_attempts())}
         ]
     end}.

test_memory_stable_with_resumes() ->
    %% Create 10 streams with 100 events each
    NumStreams = 10,
    EventsPerStream = 100,

    SessionIds = lists:map(fun(N) ->
        <<"session_mem_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Add events to all streams
    lists:foreach(fun(SessionId) ->
        ok = add_events(SessionId, lists:map(fun(EventNum) ->
            {EventNum, <<"data_", (integer_to_binary(EventNum))/binary>>}
        end, lists:seq(1, EventsPerStream)))
    end, SessionIds),

    %% Measure initial memory
    InitialMemory = erlang:memory(total),

    %% Perform 1000 resume operations
    lists:foreach(fun(N) ->
        SessionId = lists:nth((N rem NumStreams) + 1, SessionIds),
        ResumeFrom = N rem EventsPerStream,
        LastEventId = <<SessionId/binary, "_", (integer_to_binary(ResumeFrom))/binary>>,
        {ok, _Events} = erlmcp_sse_event_store:get_events_since(SessionId, LastEventId)
    end, lists:seq(1, 1000)),

    %% Measure final memory
    erlang:garbage_collect(),
    timer:sleep(100),
    FinalMemory = erlang:memory(total),

    %% Memory growth should be negligible (< 10% increase)
    MemoryGrowth = FinalMemory - InitialMemory,
    MemoryGrowthPercent = (MemoryGrowth / InitialMemory) * 100,

    %% Verify: Memory growth < 10%
    ?assert(MemoryGrowthPercent < 10.0).

test_no_linear_growth_with_resume_attempts() ->
    %% Test that memory doesn't grow linearly with resume attempts
    SessionId = <<"session_linear_growth">>,

    %% Add 100 events
    ok = add_events(SessionId, lists:map(fun(EventNum) ->
        {EventNum, <<"data_", (integer_to_binary(EventNum))/binary>>}
    end, lists:seq(1, 100))),

    %% Measure memory after 100 resumes
    lists:foreach(fun(N) ->
        ResumeFrom = N rem 100,
        LastEventId = <<SessionId/binary, "_", (integer_to_binary(ResumeFrom))/binary>>,
        {ok, _Events} = erlmcp_sse_event_store:get_events_since(SessionId, LastEventId)
    end, lists:seq(1, 100)),

    erlang:garbage_collect(),
    timer:sleep(50),
    Memory100 = erlang:memory(total),

    %% Measure memory after 1000 resumes
    lists:foreach(fun(N) ->
        ResumeFrom = N rem 100,
        LastEventId = <<SessionId/binary, "_", (integer_to_binary(ResumeFrom))/binary>>,
        {ok, _Events} = erlmcp_sse_event_store:get_events_since(SessionId, LastEventId)
    end, lists:seq(1, 1000)),

    erlang:garbage_collect(),
    timer:sleep(50),
    Memory1000 = erlang:memory(total),

    %% Memory growth should be sublinear (10x resumes should not cause 10x memory)
    MemoryGrowth = Memory1000 - Memory100,
    MemoryGrowthRatio = MemoryGrowth / Memory100,

    %% Verify: Growth ratio < 0.5 (not linear)
    ?assert(MemoryGrowthRatio < 0.5).

%%%===================================================================
%%% Test 4: Cross-Stream Event Bleed at All Resume Points
%%%===================================================================

cross_bleed_all_resume_points_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          {timeout, 60, ?_test(test_no_bleed_at_all_resume_points())},
          {timeout, 60, ?_test(test_concurrent_resume_different_streams())}
         ]
     end}.

test_no_bleed_at_all_resume_points() ->
    %% Create 20 streams
    NumStreams = 20,
    EventsPerStream = 100,

    SessionIds = lists:map(fun(N) ->
        <<"session_bleed_all_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Add unique events to each stream
    lists:foreach(fun({Idx, SessionId}) ->
        ok = add_events(SessionId, lists:map(fun(EventNum) ->
            {EventNum, <<"stream", (integer_to_binary(Idx))/binary, "_e", (integer_to_binary(EventNum))/binary>>}
        end, lists:seq(1, EventsPerStream)))
    end, lists:zip(lists:seq(1, NumStreams), SessionIds)),

    %% Test resume at every 10th position (10, 20, 30, ..., 90)
    ResumePoints = lists:seq(10, 90, 10),

    %% Verify no bleed at any resume point
    lists:foreach(fun(ResumeFrom) ->
        lists:foreach(fun({Idx, SessionId}) ->
            LastEventId = <<SessionId/binary, "_", (integer_to_binary(ResumeFrom))/binary>>,
            {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, LastEventId),

            %% Verify all events belong to this stream
            StreamPrefix = <<"stream", (integer_to_binary(Idx))/binary>>,
            BleedEvents = lists:filter(fun(EventData) ->
                binary:match(EventData, StreamPrefix) =:= nomatch
            end, Events),

            ?assertEqual(0, length(BleedEvents))
        end, lists:zip(lists:seq(1, NumStreams), SessionIds))
    end, ResumePoints).

test_concurrent_resume_different_streams() ->
    %% 50 streams, resume concurrently from different positions
    NumStreams = 50,
    EventsPerStream = 100,

    SessionIds = lists:map(fun(N) ->
        <<"session_concurrent_diff_", (integer_to_binary(N))/binary>>
    end, lists:seq(1, NumStreams)),

    %% Add unique events to each stream
    lists:foreach(fun({Idx, SessionId}) ->
        ok = add_events(SessionId, lists:map(fun(EventNum) ->
            {EventNum, <<"stream", (integer_to_binary(Idx))/binary, "_data", (integer_to_binary(EventNum))/binary>>}
        end, lists:seq(1, EventsPerStream)))
    end, lists:zip(lists:seq(1, NumStreams), SessionIds)),

    %% Resume all streams concurrently from different positions
    ResultsPid = self(),
    lists:foreach(fun({Idx, SessionId}) ->
        spawn(fun() ->
            ResumeFrom = (Idx * 7) rem EventsPerStream,  %% Different position per stream
            LastEventId = <<SessionId/binary, "_", (integer_to_binary(ResumeFrom))/binary>>,
            {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, LastEventId),

            %% Check for bleed
            StreamPrefix = <<"stream", (integer_to_binary(Idx))/binary>>,
            BleedEvents = lists:filter(fun(EventData) ->
                binary:match(EventData, StreamPrefix) =:= nomatch
            end, Events),

            ResultsPid ! {bleed_check, SessionId, length(BleedEvents)}
        end)
    end, lists:zip(lists:seq(1, NumStreams), SessionIds)),

    %% Collect results
    TotalBleed = lists:foldl(fun(_, Acc) ->
        receive
            {bleed_check, _SessionId, BleedCount} -> Acc + BleedCount
        after 30000 ->
            Acc
        end
    end, 0, lists:seq(1, NumStreams)),

    %% Verify: Zero bleed across all concurrent resumes
    ?assertEqual(0, TotalBleed).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private
%% @doc Helper to add multiple events to a session
-spec add_events(binary(), [{pos_integer(), binary()}]) -> ok.
add_events(SessionId, Events) ->
    lists:foreach(fun({EventNumber, Data}) ->
        {ok, _Id} = erlmcp_sse_event_store:add_event(SessionId, EventNumber, Data)
    end, Events),
    ok.
