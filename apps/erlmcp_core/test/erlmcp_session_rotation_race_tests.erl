-module(erlmcp_session_rotation_race_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Session Rotation Race Condition Tests (FM-02)
%% Chicago School TDD - Real concurrent processes, no mocks
%% Target: Eliminate session hijack via race conditions (RPN 300)
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

session_rotation_race_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% Concurrent Rotation Tests (8 tests)
         fun test_concurrent_rotate_requests/1,
         fun test_rotate_while_reading/1,
         fun test_rotate_new_request_race/1,
         fun test_double_rotate_no_duplicate_ids/1,
         fun test_rotate_subscribe_race/1,
         fun test_old_session_id_invalid_after_rotate/1,
         fun test_high_frequency_rotation/1,
         fun test_rotation_under_load/1
     ]}.

setup() ->
    {ok, Pid} = erlmcp_session_manager:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            exit(Pid, shutdown),
            timer:sleep(10);
        false ->
            ok
    end.

%%====================================================================
%% Concurrent Rotation Race Condition Tests
%%====================================================================

%% Test 1: Concurrent rotate requests (10 clients, same session, simultaneous rotate)
test_concurrent_rotate_requests(_Pid) ->
    fun() ->
        %% Create initial session
        OriginalMetadata = #{user => <<"alice">>, counter => 0},
        {ok, OriginalSessionId} = erlmcp_session_manager:create_session(OriginalMetadata),

        Parent = self(),
        NumClients = 10,

        %% Spawn 10 concurrent rotation requests
        Pids = [spawn(fun() ->
            NewMetadata = #{user => <<"alice">>, counter => N},
            Result = erlmcp_session_manager:rotate_session(OriginalSessionId, NewMetadata),
            Parent ! {rotate_result, N, Result}
        end) || N <- lists:seq(1, NumClients)],

        %% Collect results
        Results = [receive
            {rotate_result, N, Result} -> {N, Result}
        after 5000 ->
            error({timeout, N})
        end || N <- lists:seq(1, NumClients)],

        %% Exactly ONE should succeed, others should fail with not_found
        SuccessResults = [{N, NewId} || {N, {ok, NewId}} <- Results],
        FailureResults = [N || {N, {error, not_found}} <- Results],

        ?assertEqual(1, length(SuccessResults)),
        ?assertEqual(NumClients - 1, length(FailureResults)),

        %% Original session should be gone
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(OriginalSessionId)),

        %% New session should exist and be accessible
        [{_SuccessN, NewSessionId}] = SuccessResults,
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(NewSessionId)),

        %% Verify isolation: All pids should be done
        lists:foreach(fun(Pid) ->
            ?assertNot(is_process_alive(Pid))
        end, Pids)
    end.

%% Test 2: Rotate while reading (one client rotates, another reads simultaneously)
test_rotate_while_reading(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{data => <<"test">>}),

        Parent = self(),
        NumReaders = 20,

        %% Spawn readers
        ReaderPids = [spawn(fun() ->
            %% Continuously read for 100ms
            Results = read_loop(SessionId, erlang:system_time(millisecond) + 100),
            Parent ! {read_complete, self(), Results}
        end) || _ <- lists:seq(1, NumReaders)],

        %% Wait a bit for readers to start
        timer:sleep(10),

        %% Perform rotation
        NewMetadata = #{data => <<"rotated">>},
        RotateResult = erlmcp_session_manager:rotate_session(SessionId, NewMetadata),

        %% Collect reader results
        ReadResults = [receive
            {read_complete, Pid, Results} -> {Pid, Results}
        after 5000 ->
            error(timeout)
        end || Pid <- ReaderPids],

        %% Rotation should succeed
        ?assertMatch({ok, _NewSessionId}, RotateResult),

        %% Readers should either:
        %% - Successfully read old session (before rotation)
        %% - Get not_found (after rotation)
        %% But no crashes or inconsistent state
        lists:foreach(fun({_Pid, Results}) ->
            %% All results should be either {ok, _} or {error, not_found}
            ?assert(lists:all(fun
                ({ok, _}) -> true;
                ({error, not_found}) -> true;
                (_) -> false
            end, Results))
        end, ReadResults),

        %% Old session ID should be gone
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(SessionId))
    end.

%% Helper: Read session in a loop until deadline
read_loop(SessionId, Deadline) ->
    case erlang:system_time(millisecond) < Deadline of
        true ->
            Result = erlmcp_session_manager:get_session(SessionId),
            [Result | read_loop(SessionId, Deadline)];
        false ->
            []
    end.

%% Test 3: Rotate + new-request race (rotate happens mid-request)
test_rotate_new_request_race(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{counter => 0}),

        Parent = self(),
        NumUpdaters = 10,

        %% Spawn updaters that will update counter
        UpdaterPids = [spawn(fun() ->
            UpdateFun = fun(S) ->
                Meta = maps:get(metadata, S),
                Counter = maps:get(counter, Meta),
                S#{metadata => Meta#{counter => Counter + 1}}
            end,
            Result = erlmcp_session_manager:update_session(SessionId, UpdateFun),
            Parent ! {update_result, self(), Result}
        end) || _ <- lists:seq(1, NumUpdaters)],

        %% Wait a bit for updaters to start
        timer:sleep(5),

        %% Rotate session mid-update
        {ok, NewSessionId} = erlmcp_session_manager:rotate_session(
            SessionId,
            #{counter => 999, rotated => true}
        ),

        %% Collect updater results
        UpdateResults = [receive
            {update_result, Pid, Result} -> {Pid, Result}
        after 5000 ->
            error(timeout)
        end || Pid <- UpdaterPids],

        %% Some updates should succeed (before rotation), others fail (after)
        SuccessCount = length([R || {_Pid, ok} <- UpdateResults]),
        FailureCount = length([R || {_Pid, {error, not_found}} <- UpdateResults]),

        ?assertEqual(NumUpdaters, SuccessCount + FailureCount),

        %% Old session should be gone
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(SessionId)),

        %% New session should exist with rotated metadata
        {ok, NewSession} = erlmcp_session_manager:get_session(NewSessionId),
        NewMeta = maps:get(metadata, NewSession),
        ?assertEqual(999, maps:get(counter, NewMeta)),
        ?assertEqual(true, maps:get(rotated, NewMeta))
    end.

%% Test 4: Double rotate (rotate twice, verify no dupe IDs)
test_double_rotate_no_duplicate_ids(_Pid) ->
    fun() ->
        {ok, Id0} = erlmcp_session_manager:create_session(#{index => 0}),

        Parent = self(),

        %% Spawn two concurrent rotators
        spawn(fun() ->
            Result = erlmcp_session_manager:rotate_session(Id0, #{index => 1}),
            Parent ! {rotate1, Result}
        end),

        spawn(fun() ->
            Result = erlmcp_session_manager:rotate_session(Id0, #{index => 2}),
            Parent ! {rotate2, Result}
        end),

        %% Collect results
        Result1 = receive {rotate1, R1} -> R1 after 5000 -> error(timeout) end,
        Result2 = receive {rotate2, R2} -> R2 after 5000 -> error(timeout) end,

        %% Exactly one should succeed
        case {Result1, Result2} of
            {{ok, NewId1}, {error, not_found}} ->
                %% First rotation succeeded
                ?assert(is_binary(NewId1)),
                ?assertNotEqual(Id0, NewId1),
                ?assertMatch({ok, _}, erlmcp_session_manager:get_session(NewId1));
            {{error, not_found}, {ok, NewId2}} ->
                %% Second rotation succeeded
                ?assert(is_binary(NewId2)),
                ?assertNotEqual(Id0, NewId2),
                ?assertMatch({ok, _}, erlmcp_session_manager:get_session(NewId2));
            _ ->
                ?assert(false)  % Both succeeded or both failed - invalid
        end,

        %% Original should be gone
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(Id0))
    end.

%% Test 5: Rotate + subscribe race (rotate while subscribed to resource)
test_rotate_subscribe_race(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(
            #{subscribed_resource => <<"file://test.txt">>}
        ),

        Parent = self(),
        NumReaders = 15,

        %% Spawn continuous readers simulating subscription polling
        ReaderPids = [spawn(fun() ->
            %% Read continuously for 100ms
            Count = subscription_poll_loop(SessionId, 0,
                                          erlang:system_time(millisecond) + 100),
            Parent ! {poll_complete, self(), Count}
        end) || _ <- lists:seq(1, NumReaders)],

        %% Wait for readers to start
        timer:sleep(10),

        %% Rotate session
        {ok, NewSessionId} = erlmcp_session_manager:rotate_session(
            SessionId,
            #{subscribed_resource => <<"file://test.txt">>, rotated => true}
        ),

        %% Collect poll counts
        PollCounts = [receive
            {poll_complete, Pid, Count} -> {Pid, Count}
        after 5000 ->
            error(timeout)
        end || Pid <- ReaderPids],

        %% All readers should have completed some polls
        ?assert(lists:all(fun({_Pid, Count}) -> Count > 0 end, PollCounts)),

        %% Old session gone, new session exists
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(SessionId)),
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(NewSessionId))
    end.

%% Helper: Poll session continuously
subscription_poll_loop(SessionId, Count, Deadline) ->
    case erlang:system_time(millisecond) < Deadline of
        true ->
            case erlmcp_session_manager:get_session(SessionId) of
                {ok, _} -> subscription_poll_loop(SessionId, Count + 1, Deadline);
                {error, not_found} -> Count  % Session rotated
            end;
        false ->
            Count
    end.

%% Test 6: Old session ID after rotate (old ID should be invalid immediately)
test_old_session_id_invalid_after_rotate(_Pid) ->
    fun() ->
        {ok, OldSessionId} = erlmcp_session_manager:create_session(#{data => <<"original">>}),

        %% Rotate session
        {ok, NewSessionId} = erlmcp_session_manager:rotate_session(
            OldSessionId,
            #{data => <<"rotated">>}
        ),

        %% Verify new session exists
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(NewSessionId)),

        %% Old session should be IMMEDIATELY invalid
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(OldSessionId)),

        %% Try to use old session ID for operations - all should fail
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:update_session(OldSessionId, fun(S) -> S end)),
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:touch_session(OldSessionId)),
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:set_timeout(OldSessionId, 5000)),
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:rotate_session(OldSessionId, #{})),

        %% Deleting old ID should be idempotent (no crash)
        ?assertEqual(ok, erlmcp_session_manager:delete_session(OldSessionId))
    end.

%% Test 7: High-frequency rotation (rotate 1000 times in 5 seconds, no collisions)
test_high_frequency_rotation(_Pid) ->
    fun() ->
        {ok, InitialSessionId} = erlmcp_session_manager:create_session(#{index => 0}),

        %% Perform 1000 sequential rotations
        NumRotations = 1000,
        FinalSessionId = lists:foldl(fun(Index, CurrentSessionId) ->
            {ok, NewSessionId} = erlmcp_session_manager:rotate_session(
                CurrentSessionId,
                #{index => Index}
            ),

            %% Verify old session deleted
            ?assertEqual({error, not_found},
                         erlmcp_session_manager:get_session(CurrentSessionId)),

            %% Verify new session exists
            ?assertMatch({ok, _}, erlmcp_session_manager:get_session(NewSessionId)),

            NewSessionId
        end, InitialSessionId, lists:seq(1, NumRotations)),

        %% Final session should exist
        {ok, FinalSession} = erlmcp_session_manager:get_session(FinalSessionId),
        FinalMeta = maps:get(metadata, FinalSession),
        ?assertEqual(NumRotations, maps:get(index, FinalMeta)),

        %% Initial session should be gone
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(InitialSessionId)),

        %% Total sessions should be exactly 1
        AllSessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(1, length(AllSessions))
    end.

%% Test 8: Rotation under load (100 concurrent clients, rotate every 100 ops)
test_rotation_under_load(_Pid) ->
    fun() ->
        NumClients = 100,
        OpsPerClient = 100,

        %% Create initial sessions for each client
        InitialSessions = [begin
            {ok, SessionId} = erlmcp_session_manager:create_session(
                #{client => N, op_count => 0}
            ),
            SessionId
        end || N <- lists:seq(1, NumClients)],

        Parent = self(),

        %% Spawn workers that perform ops and rotate
        WorkerPids = [spawn(fun() ->
            Result = worker_with_rotation(SessionId, ClientN, OpsPerClient),
            Parent ! {worker_done, ClientN, Result}
        end) || {ClientN, SessionId} <- lists:zip(lists:seq(1, NumClients), InitialSessions)],

        %% Collect results
        WorkerResults = [receive
            {worker_done, ClientN, Result} -> {ClientN, Result}
        after 30000 ->
            error({timeout, ClientN})
        end || ClientN <- lists:seq(1, NumClients)],

        %% All workers should succeed
        ?assertEqual(NumClients, length(WorkerResults)),

        %% Extract final session IDs
        FinalSessionIds = [FinalId || {_ClientN, {ok, FinalId}} <- WorkerResults],
        ?assertEqual(NumClients, length(FinalSessionIds)),

        %% All final session IDs should be unique (no collisions)
        UniqueIds = lists:usort(FinalSessionIds),
        ?assertEqual(NumClients, length(UniqueIds)),

        %% All initial sessions should be gone
        lists:foreach(fun(InitialId) ->
            ?assertEqual({error, not_found},
                         erlmcp_session_manager:get_session(InitialId))
        end, InitialSessions),

        %% All final sessions should exist
        lists:foreach(fun(FinalId) ->
            ?assertMatch({ok, _}, erlmcp_session_manager:get_session(FinalId))
        end, FinalSessionIds),

        %% Total sessions should be exactly NumClients
        AllSessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(NumClients, length(AllSessions)),

        %% Verify no orphaned sessions
        AllSessionIds = [maps:get(id, S) || S <- AllSessions],
        ?assertEqual(lists:sort(FinalSessionIds), lists:sort(AllSessionIds))
    end.

%% Helper: Worker that performs ops and rotates every 50 ops
worker_with_rotation(SessionId, ClientN, TotalOps) ->
    worker_with_rotation_loop(SessionId, ClientN, 0, TotalOps).

worker_with_rotation_loop(SessionId, ClientN, OpCount, TotalOps) when OpCount >= TotalOps ->
    {ok, SessionId};  % Done
worker_with_rotation_loop(SessionId, ClientN, OpCount, TotalOps) ->
    %% Perform an update operation
    UpdateFun = fun(S) ->
        Meta = maps:get(metadata, S),
        S#{metadata => Meta#{op_count => OpCount + 1}}
    end,

    case erlmcp_session_manager:update_session(SessionId, UpdateFun) of
        ok ->
            %% Rotate every 50 ops
            NewSessionId = case (OpCount + 1) rem 50 of
                0 ->
                    NewMeta = #{client => ClientN, op_count => OpCount + 1, rotated_at => OpCount + 1},
                    case erlmcp_session_manager:rotate_session(SessionId, NewMeta) of
                        {ok, RotatedId} -> RotatedId;
                        {error, not_found} -> SessionId  % Race condition, use current ID
                    end;
                _ ->
                    SessionId
            end,
            worker_with_rotation_loop(NewSessionId, ClientN, OpCount + 1, TotalOps);
        {error, not_found} ->
            %% Session was rotated by someone else (shouldn't happen in this test)
            {error, unexpected_not_found}
    end.
