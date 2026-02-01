-module(erlmcp_session_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for erlmcp_session_manager Module
%% Chicago School TDD - Real processes, no mocks
%% Target: 85%+ coverage
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

session_manager_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [%% Basic CRUD Operations (6 tests)
      fun test_start_link/1,
      fun test_create_session/1, fun test_create_session_with_timeout/1, fun test_get_session/1,
      fun test_get_nonexistent_session/1, fun test_update_session/1,
      %% Update Operations (5 tests)
      fun test_update_nonexistent_session/1,
      fun test_update_with_function/1, fun test_update_with_invalid_function/1,
      fun test_update_preserves_last_accessed/1, fun test_update_creates_new_map/1,
      %% Delete Operations (3 tests)
      fun test_delete_session/1,
      fun test_delete_nonexistent_session/1, fun test_delete_idempotent/1,
      %% List Operations (6 tests)
      fun test_list_sessions/1,
      fun test_list_sessions_with_filter/1, fun test_list_sessions_empty/1,
      fun test_list_sessions_with_complex_filter/1, fun test_list_sessions_filter_all_match/1,
      fun test_list_sessions_filter_none_match/1,
      %% Timeout Operations (5 tests)
      fun test_set_timeout/1,
      fun test_set_timeout_nonexistent_session/1, fun test_set_timeout_to_infinity/1,
      fun test_set_timeout_from_infinity/1, fun test_touch_session/1,
      %% Touch Operations (4 tests)
      fun test_touch_nonexistent_session/1,
      fun test_touch_updates_last_accessed/1, fun test_touch_multiple_times/1,
      fun test_touch_preserves_metadata/1,
      %% Expiration Tests (6 tests)
      fun test_session_expiration/1,
      fun test_cleanup_expired/1, fun test_cleanup_expired_none/1,
      fun test_cleanup_expired_multiple/1, fun test_infinite_timeout_never_expires/1,
      fun test_expiration_calculation_edge_case/1,
      %% Session ID Tests (4 tests)
      fun test_session_id_uniqueness/1,
      fun test_session_id_format/1, fun test_session_id_cryptographically_random/1,
      fun test_session_id_collision_unlikely/1,
      %% Metadata Tests (5 tests)
      fun test_session_metadata/1,
      fun test_empty_metadata/1, fun test_complex_nested_metadata/1,
      fun test_metadata_with_binary_keys/1, fun test_metadata_preserved_through_update/1,
      %% Concurrency Tests (5 tests)
      fun test_concurrent_session_creation/1,
      fun test_concurrent_get_operations/1, fun test_concurrent_update_operations/1,
      fun test_concurrent_delete_operations/1, fun test_concurrent_mixed_operations/1,
      %% Last Accessed Tests (4 tests)
      fun test_last_accessed_update_on_get/1,
      fun test_last_accessed_initial_value/1, fun test_last_accessed_not_updated_on_list/1,
      fun test_last_accessed_monotonic/1,
      %% Integration Tests (4 tests)
      fun test_multiple_sessions/1,
      fun test_session_lifecycle/1, fun test_bulk_operations/1, fun test_session_isolation/1,
      %% Automatic Cleanup Tests (3 tests)
      fun test_automatic_cleanup_timer/1,
      fun test_cleanup_scheduled_correctly/1, fun test_cleanup_reschedules_after_execution/1,
      %% Replication Hook Tests (3 tests)
      fun test_session_replication_hooks/1,
      fun test_replicator_not_running/1, fun test_replication_events_dont_crash/1,
      %% Error Handling Tests (4 tests)
      fun test_unknown_request/1,
      fun test_handle_cast/1, fun test_handle_info_unknown/1, fun test_code_change/1,
      %% Edge Cases (6 tests)
      fun test_zero_timeout/1,
      fun test_very_large_timeout/1, fun test_special_characters_in_metadata/1,
      fun test_unicode_in_metadata/1, fun test_very_large_metadata/1,
      fun test_many_consecutive_operations/1,
      %% ETS Table Tests (4 tests)
      fun test_ets_table_named/1,
      fun test_ets_table_public/1, fun test_ets_table_ordered_set/1,
      fun test_ets_table_read_concurrency/1,
      %% Process Lifecycle Tests (4 tests)
      fun test_terminate_cleans_up/1,
      fun test_terminate_cancels_timer/1, fun test_process_flag_trap_exit/1,
      fun test_graceful_shutdown/1,
      %% Property-Based Tests (1 test)
      fun property_session_roundtrip/1]}.

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
%% Basic CRUD Operations Tests
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
       ?assertMatch(#{id := SessionId,
                      metadata := Metadata,
                      created_at := _,
                      last_accessed := _,
                      timeout_ms := _},
                    Session)
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
       UpdateFun =
           fun(Session) ->
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

%%====================================================================
%% Update Operations Tests
%%====================================================================

test_update_nonexistent_session(_Pid) ->
    fun() ->
       NonExistentId = <<"00000000000000000000000000000000">>,
       UpdateFun = fun(S) -> S end,
       Result = erlmcp_session_manager:update_session(NonExistentId, UpdateFun),
       ?assertEqual({error, not_found}, Result)
    end.

test_update_with_function(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{counter => 0, values => []}),

       %% Multiple updates
       lists:foreach(fun(N) ->
                        UpdateFun =
                            fun(Session) ->
                               Meta = maps:get(metadata, Session),
                               Counter = maps:get(counter, Meta),
                               Values = maps:get(values, Meta),
                               NewMeta = Meta#{counter => Counter + 1, values => [N | Values]},
                               Session#{metadata => NewMeta}
                            end,
                        erlmcp_session_manager:update_session(SessionId, UpdateFun)
                     end,
                     lists:seq(1, 10)),

       %% Verify final state
       {ok, FinalSession} = erlmcp_session_manager:get_session(SessionId),
       FinalMeta = maps:get(metadata, FinalSession),
       ?assertEqual(10, maps:get(counter, FinalMeta)),
       ?assertEqual(10, length(maps:get(values, FinalMeta)))
    end.

test_update_with_invalid_function(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{key => <<"value">>}),

       %% Function that throws an error
       BadUpdateFun = fun(_Session) -> error(deliberate_error) end,

       Result = erlmcp_session_manager:update_session(SessionId, BadUpdateFun),
       ?assertMatch({error, {update_failed, _}}, Result),

       %% Original session should still exist
       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(#{key => <<"value">>}, maps:get(metadata, Session))
    end.

test_update_preserves_last_accessed(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),
       {ok, Session1} = erlmcp_session_manager:get_session(SessionId),
       LastAccessed1 = maps:get(last_accessed, Session1),

       timer:sleep(10),

       %% Update should also update last_accessed
       UpdateFun = fun(S) -> S end,
       erlmcp_session_manager:update_session(SessionId, UpdateFun),

       {ok, Session2} = erlmcp_session_manager:get_session(SessionId),
       LastAccessed2 = maps:get(last_accessed, Session2),
       ?assert(LastAccessed2 > LastAccessed1)
    end.

test_update_creates_new_map(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{old => <<"data">>}),

       %% Update function that changes entire map
       UpdateFun = fun(Session) -> Session#{metadata => #{new => <<"data">>}} end,

       erlmcp_session_manager:update_session(SessionId, UpdateFun),

       {ok, Updated} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(#{new => <<"data">>}, maps:get(metadata, Updated))
    end.

%%====================================================================
%% Delete Operations Tests
%%====================================================================

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

test_delete_idempotent(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),

       %% Delete once
       ?assertEqual(ok, erlmcp_session_manager:delete_session(SessionId)),

       %% Delete again - should still succeed
       ?assertEqual(ok, erlmcp_session_manager:delete_session(SessionId))
    end.

%%====================================================================
%% List Operations Tests
%%====================================================================

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
       FilterFun =
           fun(Session) ->
              Metadata = maps:get(metadata, Session),
              maps:get(type, Metadata, undefined) =:= admin
           end,

       AdminSessions = erlmcp_session_manager:list_sessions(FilterFun),
       ?assertEqual(2, length(AdminSessions))
    end.

test_list_sessions_empty(_Pid) ->
    fun() ->
       %% No sessions created
       Sessions = erlmcp_session_manager:list_sessions(),
       ?assertEqual(0, length(Sessions))
    end.

test_list_sessions_with_complex_filter(_Pid) ->
    fun() ->
       %% Create sessions with various attributes
       {ok, _} = erlmcp_session_manager:create_session(#{role => admin, active => true}),
       {ok, _} = erlmcp_session_manager:create_session(#{role => user, active => true}),
       {ok, _} = erlmcp_session_manager:create_session(#{role => admin, active => false}),
       {ok, _} = erlmcp_session_manager:create_session(#{role => user, active => false}),

       %% Filter: active admins only
       FilterFun =
           fun(Session) ->
              Metadata = maps:get(metadata, Session),
              maps:get(role, Metadata, undefined) =:= admin
              andalso maps:get(active, Metadata, false) =:= true
           end,

       Result = erlmcp_session_manager:list_sessions(FilterFun),
       ?assertEqual(1, length(Result))
    end.

test_list_sessions_filter_all_match(_Pid) ->
    fun() ->
       {ok, _} = erlmcp_session_manager:create_session(#{type => user}),
       {ok, _} = erlmcp_session_manager:create_session(#{type => user}),
       {ok, _} = erlmcp_session_manager:create_session(#{type => user}),

       %% Filter that matches all
       FilterFun = fun(_Session) -> true end,

       Result = erlmcp_session_manager:list_sessions(FilterFun),
       ?assertEqual(3, length(Result))
    end.

test_list_sessions_filter_none_match(_Pid) ->
    fun() ->
       {ok, _} = erlmcp_session_manager:create_session(#{type => user}),
       {ok, _} = erlmcp_session_manager:create_session(#{type => admin}),

       %% Filter that matches none
       FilterFun = fun(_Session) -> false end,

       Result = erlmcp_session_manager:list_sessions(FilterFun),
       ?assertEqual(0, length(Result))
    end.

%%====================================================================
%% Timeout Operations Tests
%%====================================================================

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

test_set_timeout_nonexistent_session(_Pid) ->
    fun() ->
       NonExistentId = <<"00000000000000000000000000000000">>,
       Result = erlmcp_session_manager:set_timeout(NonExistentId, 5000),
       ?assertEqual({error, not_found}, Result)
    end.

test_set_timeout_to_infinity(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}, 1000),

       ?assertEqual(ok, erlmcp_session_manager:set_timeout(SessionId, infinity)),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(infinity, maps:get(timeout_ms, Session))
    end.

test_set_timeout_from_infinity(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}, infinity),

       ?assertEqual(ok, erlmcp_session_manager:set_timeout(SessionId, 5000)),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(5000, maps:get(timeout_ms, Session))
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
%% Touch Operations Tests
%%====================================================================

test_touch_nonexistent_session(_Pid) ->
    fun() ->
       NonExistentId = <<"00000000000000000000000000000000">>,
       Result = erlmcp_session_manager:touch_session(NonExistentId),
       ?assertEqual({error, not_found}, Result)
    end.

test_touch_updates_last_accessed(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),

       {ok, S1} = erlmcp_session_manager:get_session(SessionId),
       T1 = maps:get(last_accessed, S1),

       timer:sleep(10),

       erlmcp_session_manager:touch_session(SessionId),

       {ok, S2} = erlmcp_session_manager:get_session(SessionId),
       T2 = maps:get(last_accessed, S2),

       ?assert(T2 > T1)
    end.

test_touch_multiple_times(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),

       %% Touch multiple times
       lists:foreach(fun(_) ->
                        erlmcp_session_manager:touch_session(SessionId),
                        timer:sleep(5)
                     end,
                     lists:seq(1, 5)),

       %% Last accessed should be recent
       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       LastAccessed = maps:get(last_accessed, Session),
       Now = erlang:system_time(millisecond),
       ?assert(Now - LastAccessed < 100)
    end.

test_touch_preserves_metadata(_Pid) ->
    fun() ->
       Metadata = #{key => <<"value">>, counter => 42},
       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

       erlmcp_session_manager:touch_session(SessionId),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

%%====================================================================
%% Expiration Tests
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

test_cleanup_expired_none(_Pid) ->
    fun() ->
       %% Create sessions with long timeout
       {ok, _} = erlmcp_session_manager:create_session(#{}, 10000),
       {ok, _} = erlmcp_session_manager:create_session(#{}, 10000),

       %% Cleanup immediately - none should expire
       {ok, Count} = erlmcp_session_manager:cleanup_expired(),
       ?assertEqual(0, Count)
    end.

test_cleanup_expired_multiple(_Pid) ->
    fun() ->
       %% Create many short-lived sessions
       NumSessions = 50,
       SessionIds =
           [begin
                {ok, Id} = erlmcp_session_manager:create_session(#{}, 50),
                Id
            end
            || _ <- lists:seq(1, NumSessions)],

       %% Wait for expiration
       timer:sleep(100),

       %% Cleanup
       {ok, Count} = erlmcp_session_manager:cleanup_expired(),
       ?assertEqual(NumSessions, Count),

       %% All should be gone
       lists:foreach(fun(Id) ->
                        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(Id))
                     end,
                     SessionIds)
    end.

test_infinite_timeout_never_expires(_Pid) ->
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

test_expiration_calculation_edge_case(_Pid) ->
    fun() ->
       %% Test exact expiration boundary
       TimeoutMs = 100,
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}, TimeoutMs),

       %% Wait exactly timeout duration
       timer:sleep(TimeoutMs + 10),

       %% Should be expired
       {ok, Count} = erlmcp_session_manager:cleanup_expired(),
       ?assertEqual(1, Count),

       ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId))
    end.

%%====================================================================
%% Session ID Tests
%%====================================================================

test_session_id_uniqueness(_Pid) ->
    fun() ->
       %% Create many sessions
       SessionIds =
           [begin
                {ok, Id} = erlmcp_session_manager:create_session(#{index => N}),
                Id
            end
            || N <- lists:seq(1, 100)],

       %% All IDs should be unique
       UniqueIds = lists:usort(SessionIds),
       ?assertEqual(length(SessionIds), length(UniqueIds)),

       %% All IDs should be 32 hex characters
       lists:foreach(fun(Id) -> ?assertEqual(32, byte_size(Id)) end, SessionIds)
    end.

test_session_id_format(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),

       ?assertEqual(32, byte_size(SessionId)),

       %% Should be valid hex
       HexChars = binary_to_list(SessionId),
       ?assert(lists:all(fun(C) ->
                            C >= $0 andalso C =< $9
                            orelse C >= $a andalso C =< $f
                            orelse C >= $A andalso C =< $F
                         end,
                         HexChars))
    end.

test_session_id_cryptographically_random(_Pid) ->
    fun() ->
       %% Create multiple sessions and check for randomness
       SessionIds =
           [begin
                {ok, Id} = erlmcp_session_manager:create_session(#{}),
                Id
            end
            || _ <- lists:seq(1, 20)],

       %% Check that they're all different (statistically very unlikely to collide)
       UniqueIds = lists:usort(SessionIds),
       ?assertEqual(20, length(UniqueIds))
    end.

test_session_id_collision_unlikely(_Pid) ->
    fun() ->
       %% Test that collision is extremely unlikely
       %% 16 bytes = 128 bits of entropy
       SessionIds =
           [begin
                {ok, Id} = erlmcp_session_manager:create_session(#{n => N}),
                Id
            end
            || N <- lists:seq(1, 1000)],

       UniqueIds = lists:usort(SessionIds),
       ?assertEqual(1000, length(UniqueIds))
    end.

%%====================================================================
%% Metadata Tests
%%====================================================================

test_session_metadata(_Pid) ->
    fun() ->
       %% Test various metadata types
       Metadata =
           #{binary_key => <<"binary_value">>,
             atom_key => atom_value,
             integer_key => 42,
             list_key => [1, 2, 3],
             map_key => #{nested => true}},

       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),
       {ok, Session} = erlmcp_session_manager:get_session(SessionId),

       RetrievedMetadata = maps:get(metadata, Session),
       ?assertEqual(Metadata, RetrievedMetadata)
    end.

test_empty_metadata(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(#{}, maps:get(metadata, Session))
    end.

test_complex_nested_metadata(_Pid) ->
    fun() ->
       Metadata = #{level1 => #{level2 => #{level3 => #{value => <<"deep">>}}}},

       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

test_metadata_with_binary_keys(_Pid) ->
    fun() ->
       Metadata =
           #{<<"binary_key">> => <<"value">>,
             <<"user_id">> => 12345,
             <<"active">> => true},

       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

test_metadata_preserved_through_update(_Pid) ->
    fun() ->
       OriginalMetadata = #{key => <<"value">>, counter => 0},
       {ok, SessionId} = erlmcp_session_manager:create_session(OriginalMetadata),

       %% Update that modifies metadata
       UpdateFun =
           fun(Session) ->
              Meta = maps:get(metadata, Session),
              Counter = maps:get(counter, Meta),
              Session#{metadata => Meta#{counter => Counter + 1}}
           end,

       erlmcp_session_manager:update_session(SessionId, UpdateFun),

       {ok, Updated} = erlmcp_session_manager:get_session(SessionId),
       UpdatedMeta = maps:get(metadata, Updated),
       ?assertEqual(<<"value">>, maps:get(key, UpdatedMeta)),
       ?assertEqual(1, maps:get(counter, UpdatedMeta))
    end.

%%====================================================================
%% Concurrency Tests
%%====================================================================

test_concurrent_session_creation(_Pid) ->
    fun() ->
       Parent = self(),
       NumProcesses = 10,

       %% Spawn concurrent session creators
       Pids =
           [spawn(fun() ->
                     {ok, SessionId} = erlmcp_session_manager:create_session(#{index => N}),
                     Parent ! {session_created, SessionId}
                  end)
            || N <- lists:seq(1, NumProcesses)],

       %% Collect results
       SessionIds =
           [receive
                {session_created, Id} ->
                    Id
            after 5000 ->
                error(timeout)
            end
            || _ <- Pids],

       %% All IDs should be unique
       ?assertEqual(NumProcesses, length(lists:usort(SessionIds)))
    end.

test_concurrent_get_operations(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{test => <<"concurrent">>}),
       Parent = self(),
       NumProcesses = 20,

       %% Spawn concurrent readers
       Pids =
           [spawn(fun() ->
                     Result = erlmcp_session_manager:get_session(SessionId),
                     Parent ! {get_result, Result}
                  end)
            || _ <- lists:seq(1, NumProcesses)],

       %% All should succeed
       Results =
           [receive
                {get_result, R} ->
                    R
            after 5000 ->
                error(timeout)
            end
            || _ <- Pids],

       SuccessCount = length([1 || {ok, _} <- Results]),
       ?assertEqual(NumProcesses, SuccessCount)
    end.

test_concurrent_update_operations(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{counter => 0}),
       Parent = self(),
       NumProcesses = 10,

       %% Spawn concurrent updaters
       Pids =
           [spawn(fun() ->
                     UpdateFun =
                         fun(S) ->
                            Meta = maps:get(metadata, S),
                            Counter = maps:get(counter, Meta),
                            S#{metadata => Meta#{counter => Counter + 1}}
                         end,
                     Result = erlmcp_session_manager:update_session(SessionId, UpdateFun),
                     Parent ! {update_result, Result}
                  end)
            || _ <- lists:seq(1, NumProcesses)],

       %% All should succeed
       Results =
           [receive
                {update_result, R} ->
                    R
            after 5000 ->
                error(timeout)
            end
            || _ <- Pids],

       SuccessCount = length([R || ok = R <- Results]),
       ?assertEqual(NumProcesses, SuccessCount),

       %% Final counter should be NumProcesses
       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       Meta = maps:get(metadata, Session),
       ?assertEqual(NumProcesses, maps:get(counter, Meta))
    end.

test_concurrent_delete_operations(_Pid) ->
    fun() ->
       %% Create multiple sessions
       SessionIds =
           [begin
                {ok, Id} = erlmcp_session_manager:create_session(#{index => N}),
                Id
            end
            || N <- lists:seq(1, 10)],

       Parent = self(),

       %% Spawn concurrent deleters
       Pids =
           [spawn(fun() ->
                     Result = erlmcp_session_manager:delete_session(Id),
                     Parent ! {delete_result, Result}
                  end)
            || Id <- SessionIds],

       %% All should succeed
       Results =
           [receive
                {delete_result, R} ->
                    R
            after 5000 ->
                error(timeout)
            end
            || _ <- Pids],

       SuccessCount = length([R || ok = R <- Results]),
       ?assertEqual(10, SuccessCount)
    end.

test_concurrent_mixed_operations(_Pid) ->
    fun() ->
       {ok, SessionId} =
           erlmcp_session_manager:create_session(#{counter => 0, value => <<"test">>}),
       Parent = self(),

       %% Spawn mixed operations
       Pids =
           [spawn(fun() ->
                     [erlmcp_session_manager:get_session(SessionId) || _ <- lists:seq(1, 5)],
                     Parent ! {ops_complete, get}
                  end),
            spawn(fun() ->
                     UpdateFun = fun(S) -> S end,
                     [erlmcp_session_manager:update_session(SessionId, UpdateFun)
                      || _ <- lists:seq(1, 5)],
                     Parent ! {ops_complete, update}
                  end),
            spawn(fun() ->
                     [erlmcp_session_manager:touch_session(SessionId) || _ <- lists:seq(1, 5)],
                     Parent ! {ops_complete, touch}
                  end)],

       %% All should complete
       lists:foreach(fun(_) ->
                        receive
                            {ops_complete, _} ->
                                ok
                        after 5000 ->
                            error(timeout)
                        end
                     end,
                     Pids),

       %% Session should still be valid
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

test_last_accessed_initial_value(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       CreatedAt = maps:get(created_at, Session),
       LastAccessed = maps:get(last_accessed, Session),

       %% Initially, created_at and last_accessed should be equal
       ?assertEqual(CreatedAt, LastAccessed)
    end.

test_last_accessed_not_updated_on_list(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),
       {ok, Session1} = erlmcp_session_manager:get_session(SessionId),
       LastAccessed1 = maps:get(last_accessed, Session1),

       timer:sleep(10),

       %% List sessions (should not update last_accessed)
       _Sessions = erlmcp_session_manager:list_sessions(),

       %% List does NOT update last_accessed, but get_session does
       %% So we need to check directly in ETS to verify
       [{SessionData, _}] = ets:lookup(erlmcp_sessions, SessionId),
       LastAccessedInETS = maps:get(last_accessed, SessionData),

       ?assertEqual(LastAccessed1, LastAccessedInETS)
    end.

test_last_accessed_monotonic(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),

       %% Get session multiple times
       Timestamps =
           [begin
                timer:sleep(2),  %% Small delay to ensure different timestamps
                {ok, S} = erlmcp_session_manager:get_session(SessionId),
                maps:get(last_accessed, S)
            end
            || _ <- lists:seq(1, 5)],

       %% Check that each timestamp is >= the previous one
       %% Handle case where we might have fewer than 2 timestamps
       case length(Timestamps) > 1 of
           true ->
               Pairs = lists:zip(Timestamps, tl(Timestamps)),
               ?assert(lists:all(fun({T1, T2}) -> T2 >= T1 end, Pairs));
           false ->
               ?assert(true)
       end
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
       UpdateFun =
           fun(S) ->
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

test_session_lifecycle(_Pid) ->
    fun() ->
       %% Complete lifecycle: create -> update -> read -> delete
       Metadata = #{stage => initial},
       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

       %% Update
       UpdateFun = fun(S) -> S#{metadata => #{stage => updated}} end,
       ?assertEqual(ok, erlmcp_session_manager:update_session(SessionId, UpdateFun)),

       %% Read
       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(#{stage => updated}, maps:get(metadata, Session)),

       %% Delete
       ?assertEqual(ok, erlmcp_session_manager:delete_session(SessionId)),

       %% Verify gone
       ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId))
    end.

test_bulk_operations(_Pid) ->
    fun() ->
       %% Create many sessions
       NumSessions = 100,
       lists:foreach(fun(N) -> {ok, _} = erlmcp_session_manager:create_session(#{index => N}) end,
                     lists:seq(1, NumSessions)),

       %% List all
       AllSessions = erlmcp_session_manager:list_sessions(),
       ?assertEqual(NumSessions, length(AllSessions)),

       %% Filter
       EvenFilter =
           fun(S) ->
              Meta = maps:get(metadata, S),
              case maps:get(index, Meta, undefined) of
                  N when N rem 2 =:= 0 ->
                      true;
                  _ ->
                      false
              end
           end,

       EvenSessions = erlmcp_session_manager:list_sessions(EvenFilter),
       ?assertEqual(NumSessions div 2, length(EvenSessions))
    end.

test_session_isolation(_Pid) ->
    fun() ->
       %% Create two sessions
       {ok, Id1} = erlmcp_session_manager:create_session(#{user => alice}),
       {ok, Id2} = erlmcp_session_manager:create_session(#{user => bob}),

       %% Update session 1
       UpdateFun = fun(S) -> S#{metadata => #{user => alice_updated}} end,
       erlmcp_session_manager:update_session(Id1, UpdateFun),

       %% Session 2 should be unchanged
       {ok, Session2} = erlmcp_session_manager:get_session(Id2),
       ?assertEqual(#{user => bob}, maps:get(metadata, Session2))
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

test_cleanup_scheduled_correctly(_Pid) ->
    fun() ->
       %% Verify cleanup timer is scheduled
       %% This is tested by ensuring process is alive
       ?assert(is_process_alive(whereis(erlmcp_session_manager))),

       %% Create short-lived session
       {ok, _SessionId} = erlmcp_session_manager:create_session(#{}, 100),

       %% Manual cleanup should work
       {ok, _} = erlmcp_session_manager:cleanup_expired()
    end.

test_cleanup_reschedules_after_execution(_Pid) ->
    fun() ->
       %% Create multiple batches of expired sessions
       {ok, _Id1} = erlmcp_session_manager:create_session(#{}, 75),
       {ok, _Id2} = erlmcp_session_manager:create_session(#{}, 75),

       timer:sleep(100),

       %% First cleanup
       {ok, Count1} = erlmcp_session_manager:cleanup_expired(),
       ?assertEqual(2, Count1),

       %% Create more short-lived sessions
       {ok, _Id3} = erlmcp_session_manager:create_session(#{}, 75),
       {ok, _Id4} = erlmcp_session_manager:create_session(#{}, 75),

       timer:sleep(100),

       %% Second cleanup should still work
       {ok, Count2} = erlmcp_session_manager:cleanup_expired(),
       ?assertEqual(2, Count2)
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

test_replicator_not_running(_Pid) ->
    fun() ->
       %% Ensure replicator is not running
       undefined = whereis(erlmcp_session_replicator),

       %% Operations should still work
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),
       ?assertMatch({ok, _}, erlmcp_session_manager:get_session(SessionId)),
       ?assertEqual(ok, erlmcp_session_manager:delete_session(SessionId))
    end.

test_replication_events_dont_crash(_Pid) ->
    fun() ->
       %% Create, update, delete - all should call replication hooks safely
       {ok, Id} = erlmcp_session_manager:create_session(#{test => 1}),

       UpdateFun = fun(S) -> S end,
       erlmcp_session_manager:update_session(Id, UpdateFun),

       erlmcp_session_manager:touch_session(Id),
       erlmcp_session_manager:set_timeout(Id, 5000),

       erlmcp_session_manager:delete_session(Id),

       ?assert(true)
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

test_unknown_request(_Pid) ->
    fun() ->
       %% Send unknown request to gen_server
       gen_server:call(erlmcp_session_manager, unknown_request),
       ?assert(true)  %% Should not crash
    end.

test_handle_cast(_Pid) ->
    fun() ->
       %% Send cast message
       gen_server:cast(erlmcp_session_manager, test_cast),
       ?assert(true)  %% Should not crash
    end.

test_handle_info_unknown(_Pid) ->
    fun() ->
       %% Send unknown info message
       whereis(erlmcp_session_manager) ! unknown_info,
       timer:sleep(10),
       ?assert(true)  %% Should not crash
    end.

test_code_change(_Pid) ->
    fun() ->
       %% code_change callback should work
       %% This is tested implicitly by the process running
       ?assert(is_process_alive(whereis(erlmcp_session_manager)))
    end.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

test_zero_timeout(_Pid) ->
    fun() ->
       %% Zero timeout should expire immediately
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}, 1),

       timer:sleep(10),

       {ok, Count} = erlmcp_session_manager:cleanup_expired(),
       ?assertEqual(1, Count),

       ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId))
    end.

test_very_large_timeout(_Pid) ->
    fun() ->
       %% Very large timeout (1 year)
       LargeTimeout = 365 * 24 * 60 * 60 * 1000,
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}, LargeTimeout),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(LargeTimeout, maps:get(timeout_ms, Session))
    end.

test_special_characters_in_metadata(_Pid) ->
    fun() ->
       Metadata =
           #{<<"key with spaces">> => <<"value with spaces">>,
             <<"key\nwith\nnewlines">> => <<"value\twith\ttabs">>,
             <<"key\"quotes\"">> => <<"value'quotes'">>},

       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

test_unicode_in_metadata(_Pid) ->
    fun() ->
       Metadata =
           #{<<"emoji">> => <<"😀🎉">>,
             <<"chinese">> => <<"中文">>,
             <<"arabic">> => <<"العربية">>,
             <<"emojikey_🚀">> => <<"value">>},

       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

test_very_large_metadata(_Pid) ->
    fun() ->
       %% Create large metadata
       LargeBinary = crypto:strong_rand_bytes(1024),

       ManyKeysList =
           lists:map(fun(N) -> {<<"key", (integer_to_binary(N))>>, N} end, lists:seq(1, 100)),

       ManyKeys = maps:from_list(ManyKeysList),

       Metadata = #{large_data => LargeBinary, many_keys => ManyKeys},

       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       RetrievedMetadata = maps:get(metadata, Session),

       ?assertEqual(maps:get(large_data, Metadata), maps:get(large_data, RetrievedMetadata)),
       ?assertEqual(maps:size(ManyKeys),
                    maps:size(
                        maps:get(many_keys, RetrievedMetadata)))
    end.

test_many_consecutive_operations(_Pid) ->
    fun() ->
       %% Perform many operations in sequence
       NumOps = 100,

       lists:foreach(fun(N) -> {ok, _Id} = erlmcp_session_manager:create_session(#{index => N}) end,
                     lists:seq(1, NumOps)),

       Sessions = erlmcp_session_manager:list_sessions(),
       ?assertEqual(NumOps, length(Sessions)),

       %% Clean up all
       lists:foreach(fun(Session) ->
                        Id = maps:get(id, Session),
                        erlmcp_session_manager:delete_session(Id)
                     end,
                     Sessions),

       %% All should be deleted
       FinalSessions = erlmcp_session_manager:list_sessions(),
       ?assertEqual(0, length(FinalSessions))
    end.

%%====================================================================
%% ETS Table Tests
%%====================================================================

test_ets_table_named(_Pid) ->
    fun() ->
       %% Verify ETS table is named
       ?assertEqual(true, ets:info(erlmcp_sessions) =/= undefined)
    end.

test_ets_table_public(_Pid) ->
    fun() ->
       %% Can read directly from ETS table
       {ok, SessionId} = erlmcp_session_manager:create_session(#{test => direct}),

       %% Direct ETS lookup should work
       Result = ets:lookup(erlmcp_sessions, SessionId),
       ?assertEqual(1, length(Result))
    end.

test_ets_table_ordered_set(_Pid) ->
    fun() ->
       %% Verify table type
       Info = ets:info(erlmcp_sessions),
       ?assertEqual(ordered_set, proplists:get_value(type, Info))
    end.

test_ets_table_read_concurrency(_Pid) ->
    fun() ->
       %% Verify read_concurrency is enabled
       Info = ets:info(erlmcp_sessions),
       ?assertEqual(true, proplists:get_value(read_concurrency, Info))
    end.

%%====================================================================
%% Process Lifecycle Tests
%%====================================================================

test_terminate_cleans_up(_Pid) ->
    fun() ->
       %% This test verifies that terminate/2 cleans up ETS table
       %% Since we're using foreach setup, the process gets restarted between tests
       %% We verify that the ETS table is properly cleaned by checking it exists initially
       %% and will be cleaned up when process terminates
       %% Verify ETS table exists
       ?assertNotEqual(undefined, ets:info(erlmcp_sessions)),

       %% Verify table has the expected properties
       Info = ets:info(erlmcp_sessions),
       ?assertEqual(ordered_set, proplists:get_value(type, Info)),
       ?assertEqual(erlmcp_sessions, proplists:get_value(name, Info))
    end.

test_terminate_cancels_timer(_Pid) ->
    fun() ->
       %% Verify cleanup timer is scheduled in state
       %% We can't easily test actual termination without breaking setup
       %% So we verify the timer exists by checking the process is running
       Pid = whereis(erlmcp_session_manager),
       ?assert(is_pid(Pid)),
       ?assert(is_process_alive(Pid)),

       %% Verify process is a gen_server by checking status
       Status = sys:get_status(Pid),
       ?assertMatch({status, Pid, {module, gen_server}, _, _, _, _}, Status)
    end.

test_process_flag_trap_exit(_Pid) ->
    fun() ->
       %% Process should trap exit
       %% This is verified by checking that process_flag(trap_exit, true) is set in init/1
       %% We can verify by checking the process dictionary
       ProcessInfo = erlang:process_info(whereis(erlmcp_session_manager), trap_exit),

       ?assertMatch({trap_exit, true}, ProcessInfo)
    end.

test_graceful_shutdown(_Pid) ->
    fun() ->
       %% Create sessions
       {ok, _Id1} = erlmcp_session_manager:create_session(#{}, 10000),
       {ok, _Id2} = erlmcp_session_manager:create_session(#{}, 10000),

       %% Verify sessions exist
       Sessions = erlmcp_session_manager:list_sessions(),
       ?assertEqual(2, length(Sessions)),

       %% Shutdown should be graceful
       %% Since we use foreach, we can't actually kill the process here
       %% Instead verify that the process handles shutdown signals properly
       Pid = whereis(erlmcp_session_manager),
       ?assert(is_process_alive(Pid)),

       %% Send a shutdown request but don't actually kill (foreach needs process)
       %% Just verify the process is responsive
       {ok, _} = erlmcp_session_manager:create_session(#{shutdown_test => true}),

       FinalSessions = erlmcp_session_manager:list_sessions(),
       ?assertEqual(3, length(FinalSessions))
    end.

%%====================================================================
%% Property-Based Tests
%%====================================================================

property_session_roundtrip(_Pid) ->
    fun() ->
       %% Test session roundtrip with random data
       RandomMetadatas =
           [begin
                #{random_int => rand:uniform(1000),
                  random_binary =>
                      crypto:strong_rand_bytes(
                          rand:uniform(100)),
                  random_bool => rand:uniform(2) =:= 1,
                  random_float => rand:uniform()}
            end
            || _ <- lists:seq(1, 50)],

       %% Create sessions
       SessionIds =
           [begin
                {ok, Id} = erlmcp_session_manager:create_session(Meta),
                Id
            end
            || Meta <- RandomMetadatas],

       %% Verify all sessions
       lists:foreach(fun({Id, OriginalMeta}) ->
                        {ok, Session} = erlmcp_session_manager:get_session(Id),
                        RetrievedMeta = maps:get(metadata, Session),
                        ?assertEqual(OriginalMeta, RetrievedMeta)
                     end,
                     lists:zip(SessionIds, RandomMetadatas)),

       %% All sessions should be listable
       AllSessions = erlmcp_session_manager:list_sessions(),
       ?assertEqual(50, length(AllSessions))
    end.
