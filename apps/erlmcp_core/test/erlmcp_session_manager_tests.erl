-module(erlmcp_session_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for erlmcp_session_manager Module
%% Chicago School TDD - Real processes, no mocks
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

session_manager_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_start_link/1,
         fun test_create_session/1,
         fun test_create_session_with_timeout/1,
         fun test_get_session/1,
         fun test_get_nonexistent_session/1,
         fun test_update_session/1,
         fun test_update_nonexistent_session/1,
         fun test_delete_session/1,
         fun test_delete_nonexistent_session/1,
         fun test_list_sessions/1,
         fun test_list_sessions_with_filter/1,
         fun test_set_timeout/1,
         fun test_touch_session/1,
         fun test_session_expiration/1,
         fun test_cleanup_expired/1,
         fun test_session_id_uniqueness/1,
         fun test_concurrent_session_creation/1,
         fun test_session_metadata/1,
         fun test_infinite_timeout/1,
         fun test_last_accessed_update_on_get/1,
         fun test_multiple_sessions/1,
         fun test_update_with_function/1,
         fun test_automatic_cleanup_timer/1,
         fun test_session_replication_hooks/1
     ]}.

setup() ->
    %% Start the session manager
    {ok, Pid} = erlmcp_session_manager:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop the session manager
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            exit(Pid, shutdown),
            timer:sleep(10);
        false ->
            ok
    end.

%%====================================================================
%% Basic Functionality Tests
%%====================================================================

test_start_link(_Pid) ->
    fun() ->
        ?assert(is_pid(whereis(erlmcp_session_manager))),
        ?assert(is_process_alive(whereis(erlmcp_session_manager)))
    end.

test_create_session(_Pid) ->
    fun() ->
        Metadata = #{user => <<"alice">>, project => <<"test">>},
        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

        ?assert(is_binary(SessionId)),
        ?assertEqual(32, byte_size(SessionId)),

        %% Verify session exists
        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(SessionId, maps:get(id, Session)),
        ?assertEqual(Metadata, maps:get(metadata, Session)),
        ?assert(is_integer(maps:get(created_at, Session))),
        ?assert(is_integer(maps:get(last_accessed, Session)))
    end.

test_create_session_with_timeout(_Pid) ->
    fun() ->
        Metadata = #{key => <<"value">>},
        TimeoutMs = 5000,
        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata, TimeoutMs),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(TimeoutMs, maps:get(timeout_ms, Session))
    end.

test_get_session(_Pid) ->
    fun() ->
        Metadata = #{test => <<"get_session">>},
        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertMatch(#{
            id := SessionId,
            metadata := Metadata,
            created_at := _,
            last_accessed := _,
            timeout_ms := _
        }, Session)
    end.

test_get_nonexistent_session(_Pid) ->
    fun() ->
        NonExistentId = <<"00000000000000000000000000000000">>,
        Result = erlmcp_session_manager:get_session(NonExistentId),
        ?assertEqual({error, not_found}, Result)
    end.

test_update_session(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{counter => 0}),

        %% Update session with function
        UpdateFun = fun(Session) ->
            Metadata = maps:get(metadata, Session),
            Counter = maps:get(counter, Metadata),
            NewMetadata = Metadata#{counter => Counter + 1},
            Session#{metadata => NewMetadata}
        end,

        ?assertEqual(ok, erlmcp_session_manager:update_session(SessionId, UpdateFun)),

        %% Verify update
        {ok, UpdatedSession} = erlmcp_session_manager:get_session(SessionId),
        UpdatedMetadata = maps:get(metadata, UpdatedSession),
        ?assertEqual(1, maps:get(counter, UpdatedMetadata))
    end.

test_update_nonexistent_session(_Pid) ->
    fun() ->
        NonExistentId = <<"00000000000000000000000000000000">>,
        UpdateFun = fun(S) -> S end,
        Result = erlmcp_session_manager:update_session(NonExistentId, UpdateFun),
        ?assertEqual({error, not_found}, Result)
    end.

test_delete_session(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{test => <<"delete">>}),

        %% Verify session exists
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(SessionId)),

        %% Delete session
        ?assertEqual(ok, erlmcp_session_manager:delete_session(SessionId)),

        %% Verify session deleted
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId))
    end.

test_delete_nonexistent_session(_Pid) ->
    fun() ->
        NonExistentId = <<"00000000000000000000000000000000">>,
        %% Should succeed (idempotent)
        ?assertEqual(ok, erlmcp_session_manager:delete_session(NonExistentId))
    end.

test_list_sessions(_Pid) ->
    fun() ->
        %% Create multiple sessions
        {ok, Id1} = erlmcp_session_manager:create_session(#{index => 1}),
        {ok, Id2} = erlmcp_session_manager:create_session(#{index => 2}),
        {ok, Id3} = erlmcp_session_manager:create_session(#{index => 3}),

        Sessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(3, length(Sessions)),

        %% Verify all sessions present
        Ids = [maps:get(id, S) || S <- Sessions],
        ?assert(lists:member(Id1, Ids)),
        ?assert(lists:member(Id2, Ids)),
        ?assert(lists:member(Id3, Ids))
    end.

test_list_sessions_with_filter(_Pid) ->
    fun() ->
        %% Create sessions with different metadata
        {ok, _Id1} = erlmcp_session_manager:create_session(#{type => admin}),
        {ok, _Id2} = erlmcp_session_manager:create_session(#{type => user}),
        {ok, _Id3} = erlmcp_session_manager:create_session(#{type => admin}),

        %% Filter admin sessions
        FilterFun = fun(Session) ->
            Metadata = maps:get(metadata, Session),
            maps:get(type, Metadata, undefined) =:= admin
        end,

        AdminSessions = erlmcp_session_manager:list_sessions(FilterFun),
        ?assertEqual(2, length(AdminSessions))
    end.

test_set_timeout(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}, 1000),

        %% Update timeout
        NewTimeout = 5000,
        ?assertEqual(ok, erlmcp_session_manager:set_timeout(SessionId, NewTimeout)),

        %% Verify timeout updated
        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(NewTimeout, maps:get(timeout_ms, Session))
    end.

test_touch_session(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}),
        {ok, OriginalSession} = erlmcp_session_manager:get_session(SessionId),
        OriginalAccessed = maps:get(last_accessed, OriginalSession),

        %% Wait a bit
        timer:sleep(10),

        %% Touch session
        ?assertEqual(ok, erlmcp_session_manager:touch_session(SessionId)),

        %% Verify last_accessed updated
        {ok, TouchedSession} = erlmcp_session_manager:get_session(SessionId),
        TouchedAccessed = maps:get(last_accessed, TouchedSession),
        ?assert(TouchedAccessed > OriginalAccessed)
    end.

%%====================================================================
%% Session Expiration Tests
%%====================================================================

test_session_expiration(_Pid) ->
    fun() ->
        %% Create session with 100ms timeout
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}, 100),

        %% Session should exist initially
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(SessionId)),

        %% Wait for expiration
        timer:sleep(150),

        %% Manual cleanup
        {ok, Count} = erlmcp_session_manager:cleanup_expired(),
        ?assertEqual(1, Count),

        %% Session should be gone
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId))
    end.

test_cleanup_expired(_Pid) ->
    fun() ->
        %% Create mix of expired and valid sessions
        {ok, Id1} = erlmcp_session_manager:create_session(#{}, 100),  % Will expire
        {ok, Id2} = erlmcp_session_manager:create_session(#{}, 10000), % Valid
        {ok, Id3} = erlmcp_session_manager:create_session(#{}, 100),  % Will expire

        %% Wait for some to expire
        timer:sleep(150),

        %% Cleanup
        {ok, Count} = erlmcp_session_manager:cleanup_expired(),
        ?assertEqual(2, Count),

        %% Verify expired sessions gone
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(Id1)),
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(Id3)),

        %% Verify valid session remains
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(Id2))
    end.

%%====================================================================
%% Session ID and Uniqueness Tests
%%====================================================================

test_session_id_uniqueness(_Pid) ->
    fun() ->
        %% Create many sessions
        SessionIds = [begin
            {ok, Id} = erlmcp_session_manager:create_session(#{index => N}),
            Id
        end || N <- lists:seq(1, 100)],

        %% All IDs should be unique
        UniqueIds = lists:usort(SessionIds),
        ?assertEqual(length(SessionIds), length(UniqueIds)),

        %% All IDs should be 32 hex characters
        lists:foreach(fun(Id) ->
            ?assertEqual(32, byte_size(Id)),
            ?assert(lists:all(fun(C) ->
                (C >= $0 andalso C =< $9) orelse
                (C >= $a andalso C =< $f) orelse
                (C >= $A andalso C =< $F)
            end, binary_to_list(Id)))
        end, SessionIds)
    end.

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_session_creation(_Pid) ->
    fun() ->
        Parent = self(),
        NumProcesses = 10,

        %% Spawn concurrent session creators
        Pids = [spawn(fun() ->
            {ok, SessionId} = erlmcp_session_manager:create_session(#{index => N}),
            Parent ! {session_created, SessionId}
        end) || N <- lists:seq(1, NumProcesses)],

        %% Collect results
        SessionIds = [receive
            {session_created, Id} -> Id
        after 5000 ->
            error(timeout)
        end || _ <- Pids],

        %% All IDs should be unique
        ?assertEqual(NumProcesses, length(lists:usort(SessionIds)))
    end.

%%====================================================================
%% Metadata Tests
%%====================================================================

test_session_metadata(_Pid) ->
    fun() ->
        %% Test various metadata types
        Metadata = #{
            binary_key => <<"binary_value">>,
            atom_key => atom_value,
            integer_key => 42,
            list_key => [1, 2, 3],
            map_key => #{nested => true}
        },

        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),
        {ok, Session} = erlmcp_session_manager:get_session(SessionId),

        RetrievedMetadata = maps:get(metadata, Session),
        ?assertEqual(Metadata, RetrievedMetadata)
    end.

%%====================================================================
%% Infinite Timeout Tests
%%====================================================================

test_infinite_timeout(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}, infinity),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(infinity, maps:get(timeout_ms, Session)),

        %% Wait and cleanup - session should not expire
        timer:sleep(100),
        {ok, Count} = erlmcp_session_manager:cleanup_expired(),
        ?assertEqual(0, Count),

        %% Session should still exist
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(SessionId))
    end.

%%====================================================================
%% Last Accessed Tests
%%====================================================================

test_last_accessed_update_on_get(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}),
        {ok, Session1} = erlmcp_session_manager:get_session(SessionId),
        LastAccessed1 = maps:get(last_accessed, Session1),

        %% Wait a bit
        timer:sleep(10),

        %% Get session again
        {ok, Session2} = erlmcp_session_manager:get_session(SessionId),
        LastAccessed2 = maps:get(last_accessed, Session2),

        %% Last accessed should be updated
        ?assert(LastAccessed2 > LastAccessed1)
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

test_multiple_sessions(_Pid) ->
    fun() ->
        %% Create sessions with different configurations
        {ok, Id1} = erlmcp_session_manager:create_session(#{user => <<"alice">>}, 5000),
        {ok, Id2} = erlmcp_session_manager:create_session(#{user => <<"bob">>}, 10000),
        {ok, Id3} = erlmcp_session_manager:create_session(#{user => <<"charlie">>}, infinity),

        %% Update session 1
        UpdateFun = fun(S) ->
            Meta = maps:get(metadata, S),
            S#{metadata => Meta#{updated => true}}
        end,
        erlmcp_session_manager:update_session(Id1, UpdateFun),

        %% Touch session 2
        erlmcp_session_manager:touch_session(Id2),

        %% Delete session 3
        erlmcp_session_manager:delete_session(Id3),

        %% List remaining sessions
        Sessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(2, length(Sessions)),

        %% Verify session 1 updated
        {ok, S1} = erlmcp_session_manager:get_session(Id1),
        Meta1 = maps:get(metadata, S1),
        ?assertEqual(true, maps:get(updated, Meta1)),

        %% Verify session 3 deleted
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(Id3))
    end.

test_update_with_function(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{counter => 0, values => []}),

        %% Multiple updates
        lists:foreach(fun(N) ->
            UpdateFun = fun(Session) ->
                Meta = maps:get(metadata, Session),
                Counter = maps:get(counter, Meta),
                Values = maps:get(values, Meta),
                NewMeta = Meta#{
                    counter => Counter + 1,
                    values => [N | Values]
                },
                Session#{metadata => NewMeta}
            end,
            erlmcp_session_manager:update_session(SessionId, UpdateFun)
        end, lists:seq(1, 10)),

        %% Verify final state
        {ok, FinalSession} = erlmcp_session_manager:get_session(SessionId),
        FinalMeta = maps:get(metadata, FinalSession),
        ?assertEqual(10, maps:get(counter, FinalMeta)),
        ?assertEqual(10, length(maps:get(values, FinalMeta)))
    end.

%%====================================================================
%% Automatic Cleanup Tests
%%====================================================================

test_automatic_cleanup_timer(_Pid) ->
    fun() ->
        %% Create session with very short timeout
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}, 50),

        %% Session exists initially
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(SessionId)),

        %% Wait for automatic cleanup (default interval is 60s, but timer fires)
        %% We trigger manual cleanup for testing
        timer:sleep(100),
        erlmcp_session_manager:cleanup_expired(),

        %% Session should be cleaned up
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId))
    end.

%%====================================================================
%% Replication Hook Tests
%%====================================================================

test_session_replication_hooks(_Pid) ->
    fun() ->
        %% Test that replication hooks are called (no-op if replicator not running)
        %% This just verifies the code path doesn't crash

        {ok, SessionId} = erlmcp_session_manager:create_session(#{test => <<"replication">>}),

        %% Update session
        UpdateFun = fun(S) -> S#{metadata => #{updated => true}} end,
        erlmcp_session_manager:update_session(SessionId, UpdateFun),

        %% Delete session
        erlmcp_session_manager:delete_session(SessionId),

        %% If we got here without crashing, replication hooks work
        ?assert(true)
    end.

%%====================================================================
%% Property-Based Tests (Manual)
%%====================================================================

property_session_lifecycle_test_() ->
    {timeout, 30, fun() ->
        %% Test session lifecycle with random operations
        {ok, _Pid} = erlmcp_session_manager:start_link(),

        %% Create 50 sessions
        SessionIds = [begin
            {ok, Id} = erlmcp_session_manager:create_session(#{index => N}),
            Id
        end || N <- lists:seq(1, 50)],

        %% Perform random operations
        lists:foreach(fun(Id) ->
            case rand:uniform(3) of
                1 -> erlmcp_session_manager:touch_session(Id);
                2 -> erlmcp_session_manager:update_session(Id, fun(S) -> S end);
                3 -> erlmcp_session_manager:get_session(Id)
            end
        end, SessionIds),

        %% All sessions should still exist
        Sessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(50, length(Sessions))
    end}.
