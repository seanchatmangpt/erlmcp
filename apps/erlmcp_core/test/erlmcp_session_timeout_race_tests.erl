-module(erlmcp_session_timeout_race_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Session Timeout and Cleanup Race Condition Tests (FM-02)
%% Chicago School TDD - Real processes, real timing
%% Target: Verify rotation + cleanup interactions don't leak sessions
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

session_timeout_race_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% Timeout/Cleanup Race Tests (4 tests)
         fun test_rotation_during_session_cleanup/1,
         fun test_cleanup_during_rotation/1,
         fun test_no_orphaned_sessions_after_rotation/1,
         fun test_cleanup_order_correct/1
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
%% Timeout and Cleanup Race Condition Tests
%%====================================================================

%% Test: Rotation during session cleanup (old ID being deleted)
test_rotation_during_session_cleanup(_Pid) ->
    fun() ->
        %% Create short-lived session
        {ok, SessionId} = erlmcp_session_manager:create_session(
            #{data => <<"test">>},
            100  % 100ms timeout
        ),

        Parent = self(),

        %% Spawn cleanup watcher
        spawn(fun() ->
            %% Wait for expiration
            timer:sleep(110),

            %% Trigger cleanup
            Result = erlmcp_session_manager:cleanup_expired(),
            Parent ! {cleanup_done, Result}
        end),

        %% Spawn rotator that tries to rotate before cleanup
        spawn(fun() ->
            timer:sleep(50),  % Rotate before expiration
            Result = erlmcp_session_manager:rotate_session(
                SessionId,
                #{data => <<"rotated">>}
            ),
            Parent ! {rotate_done, Result}
        end),

        %% Collect results
        RotateResult = receive {rotate_done, R} -> R after 5000 -> error(timeout) end,
        CleanupResult = receive {cleanup_done, C} -> C after 5000 -> error(timeout) end,

        %% Rotation should succeed (happens before expiration)
        ?assertMatch({ok, _NewSessionId}, RotateResult),
        {ok, NewSessionId} = RotateResult,

        %% Cleanup might find 0 or 1 expired sessions depending on timing
        ?assertMatch({ok, _Count}, CleanupResult),
        {ok, CleanupCount} = CleanupResult,

        %% Old session should be gone
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(SessionId)),

        %% New session might also be expired if rotation happened early
        %% Verify either new session exists OR it was cleaned up
        case erlmcp_session_manager:get_session(NewSessionId) of
            {ok, _} ->
                %% New session still alive
                ?assert(true);
            {error, not_found} ->
                %% New session was cleaned up (rotation happened very early)
                ?assert(CleanupCount >= 1)
        end,

        %% No orphaned sessions: total sessions should be 0 or 1
        AllSessions = erlmcp_session_manager:list_sessions(),
        ?assert(length(AllSessions) =< 1)
    end.

%% Test: Cleanup during rotation (new ID before old deleted)
test_cleanup_during_rotation(_Pid) ->
    fun() ->
        NumSessions = 20,

        %% Create mix of short-lived and long-lived sessions
        SessionPairs = [begin
            ShortTimeout = 80,
            {ok, ShortId} = erlmcp_session_manager:create_session(
                #{index => N, type => short},
                ShortTimeout
            ),

            {ok, LongId} = erlmcp_session_manager:create_session(
                #{index => N, type => long},
                10000
            ),

            {ShortId, LongId}
        end || N <- lists:seq(1, NumSessions)],

        Parent = self(),

        %% Spawn rotators for short-lived sessions
        lists:foreach(fun({ShortId, _LongId}) ->
            spawn(fun() ->
                timer:sleep(40),  % Rotate before expiration
                Result = erlmcp_session_manager:rotate_session(
                    ShortId,
                    #{rotated => true}
                ),
                Parent ! {rotate_result, ShortId, Result}
            end)
        end, SessionPairs),

        %% Spawn cleanup that runs during rotation
        spawn(fun() ->
            timer:sleep(120),  % After expiration
            Result = erlmcp_session_manager:cleanup_expired(),
            Parent ! {cleanup_result, Result}
        end),

        %% Collect rotation results
        RotateResults = [receive
            {rotate_result, ShortId, Result} -> {ShortId, Result}
        after 5000 ->
            error(timeout)
        end || {ShortId, _} <- SessionPairs],

        %% Collect cleanup result
        CleanupResult = receive
            {cleanup_result, Result} -> Result
        after 5000 ->
            error(timeout)
        end,

        %% Most rotations should succeed
        SuccessCount = length([1 || {_Id, {ok, _}} <- RotateResults]),
        ?assert(SuccessCount >= NumSessions - 2),  % Allow 2 failures due to timing

        %% Cleanup should have run
        ?assertMatch({ok, _Count}, CleanupResult),

        %% All original short-lived sessions should be gone
        lists:foreach(fun({ShortId, _LongId}) ->
            ?assertEqual({error, not_found},
                         erlmcp_session_manager:get_session(ShortId))
        end, SessionPairs),

        %% Long-lived sessions should still exist
        lists:foreach(fun({_ShortId, LongId}) ->
            ?assertMatch({ok, _},
                         erlmcp_session_manager:get_session(LongId))
        end, SessionPairs),

        %% Total sessions should be: NumSessions long + some rotated short
        AllSessions = erlmcp_session_manager:list_sessions(),
        ?assert(length(AllSessions) >= NumSessions)
    end.

%% Test: No orphaned sessions after rotation
test_no_orphaned_sessions_after_rotation(_Pid) ->
    fun() ->
        NumSessions = 50,

        %% Create sessions and rotate them
        InitialIds = [begin
            {ok, Id} = erlmcp_session_manager:create_session(#{index => N}),
            Id
        end || N <- lists:seq(1, NumSessions)],

        %% Rotate all sessions
        RotatedIds = [begin
            {ok, NewId} = erlmcp_session_manager:rotate_session(
                OldId,
                #{index => N, rotated => true}
            ),
            NewId
        end || {N, OldId} <- lists:zip(lists:seq(1, NumSessions), InitialIds)],

        %% Verify all initial IDs are gone
        lists:foreach(fun(InitialId) ->
            ?assertEqual({error, not_found},
                         erlmcp_session_manager:get_session(InitialId))
        end, InitialIds),

        %% Verify all rotated IDs exist
        lists:foreach(fun(RotatedId) ->
            ?assertMatch({ok, _},
                         erlmcp_session_manager:get_session(RotatedId))
        end, RotatedIds),

        %% Verify total session count matches
        AllSessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(NumSessions, length(AllSessions)),

        %% Verify no orphaned sessions (all session IDs are accounted for)
        AllSessionIds = lists:sort([maps:get(id, S) || S <- AllSessions]),
        ExpectedIds = lists:sort(RotatedIds),
        ?assertEqual(ExpectedIds, AllSessionIds),

        %% Cleanup should find 0 expired sessions
        {ok, CleanupCount} = erlmcp_session_manager:cleanup_expired(),
        ?assertEqual(0, CleanupCount),

        %% Session count should still be NumSessions
        FinalSessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(NumSessions, length(FinalSessions))
    end.

%% Test: Cleanup order correct (new ID before old deleted)
test_cleanup_order_correct(_Pid) ->
    fun() ->
        %% This test verifies the rotation implementation:
        %% 1. Insert new session
        %% 2. Delete old session
        %% This order prevents race conditions where cleanup could
        %% delete the new session before old is deleted

        NumRotations = 100,

        %% Create initial session
        {ok, InitialId} = erlmcp_session_manager:create_session(
            #{index => 0, data => <<"initial">>}
        ),

        Parent = self(),

        %% Spawn background cleanup that runs continuously
        CleanupPid = spawn(fun() ->
            cleanup_loop(Parent, 0)
        end),

        %% Perform many rotations while cleanup is running
        FinalId = lists:foldl(fun(Index, CurrentId) ->
            {ok, NewId} = erlmcp_session_manager:rotate_session(
                CurrentId,
                #{index => Index, data => <<"rotated">>}
            ),

            %% Brief delay to allow cleanup to interleave
            timer:sleep(1),

            %% Verify current session gone, new session exists
            ?assertEqual({error, not_found},
                         erlmcp_session_manager:get_session(CurrentId)),
            ?assertMatch({ok, _},
                         erlmcp_session_manager:get_session(NewId)),

            NewId
        end, InitialId, lists:seq(1, NumRotations)),

        %% Stop cleanup
        CleanupPid ! stop,
        CleanupCounts = receive
            {cleanup_stats, Counts} -> Counts
        after 1000 ->
            error(timeout)
        end,

        %% Verify final state
        ?assertEqual({error, not_found},
                     erlmcp_session_manager:get_session(InitialId)),
        ?assertMatch({ok, _},
                     erlmcp_session_manager:get_session(FinalId)),

        %% Total sessions should be exactly 1
        AllSessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(1, length(AllSessions)),

        %% Cleanup should have run multiple times without causing issues
        ?assert(length(CleanupCounts) > 0),

        io:format("Cleanup ran ~p times during ~p rotations (no race conditions)~n",
                  [length(CleanupCounts), NumRotations])
    end.

%% Helper: Continuous cleanup loop
cleanup_loop(Parent, Count) ->
    receive
        stop ->
            Parent ! {cleanup_stats, lists:seq(1, Count)},
            ok
    after 10 ->
        %% Run cleanup
        {ok, _CleanedCount} = erlmcp_session_manager:cleanup_expired(),
        cleanup_loop(Parent, Count + 1)
    end.
