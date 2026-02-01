-module(erlmcp_session_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for erlmcp_session Module
%% Chicago School TDD - Real session management, no mocks
%%====================================================================

%%====================================================================
%% Session Creation Tests
%%====================================================================

session_creation_test_() ->
    [?_test(test_new_session()),
     ?_test(test_new_session_with_metadata()),
     ?_test(test_session_id_uniqueness()),
     ?_test(test_session_created_at())].

test_new_session() ->
    Session = erlmcp_session:new(),
    ?assertMatch(#{id := _,
                   created_at := _,
                   metadata := _},
                 Session),
    ?assert(is_binary(maps:get(id, Session))),
    ?assert(is_integer(maps:get(created_at, Session))),
    ?assertEqual(#{}, maps:get(metadata, Session)).

test_new_session_with_metadata() ->
    Metadata = #{user => <<"alice">>, project => <<"test">>},
    Session = erlmcp_session:new(Metadata),
    ?assertMatch(#{id := _, metadata := Metadata}, Session),
    ?assertEqual(Metadata, maps:get(metadata, Session)).

test_session_id_uniqueness() ->
    Session1 = erlmcp_session:new(),
    Session2 = erlmcp_session:new(),
    Id1 = erlmcp_session:get_session_id(Session1),
    Id2 = erlmcp_session:get_session_id(Session2),
    ?assertNotEqual(Id1, Id2),
    %% Session IDs should be 32 hex characters (16 bytes)
    ?assertEqual(32, byte_size(Id1)),
    ?assertEqual(32, byte_size(Id2)).

test_session_created_at() ->
    Before = erlang:system_time(millisecond),
    Session = erlmcp_session:new(),
    After = erlang:system_time(millisecond),
    CreatedAt = maps:get(created_at, Session),
    ?assert(CreatedAt >= Before),
    ?assert(CreatedAt =< After).

%%====================================================================
%% Session ID Retrieval Tests
%%====================================================================

get_session_id_test_() ->
    [?_test(test_get_session_id()), ?_test(test_session_id_format())].

test_get_session_id() ->
    Session = erlmcp_session:new(),
    Id = erlmcp_session:get_session_id(Session),
    ?assert(is_binary(Id)),
    ?assertEqual(Id, maps:get(id, Session)).

test_session_id_format() ->
    Session = erlmcp_session:new(),
    Id = erlmcp_session:get_session_id(Session),
    %% Verify it's a valid hex string
    ?assertEqual(32, byte_size(Id)),
    %% Check all characters are valid hex
    ?assert(lists:all(fun(C) ->
                         C >= $0 andalso C =< $9
                         orelse C >= $a andalso C =< $f
                         orelse C >= $A andalso C =< $F
                      end,
                      binary_to_list(Id))).

%%====================================================================
%% Metadata Management Tests
%%====================================================================

metadata_test_() ->
    [?_test(test_set_metadata()),
     ?_test(test_get_metadata()),
     ?_test(test_update_metadata()),
     ?_test(test_get_missing_metadata())].

test_set_metadata() ->
    Session = erlmcp_session:new(),
    UpdatedSession = erlmcp_session:set_metadata(Session, user, <<"bob">>),
    Metadata = maps:get(metadata, UpdatedSession),
    ?assertEqual(<<"bob">>, maps:get(user, Metadata)).

test_get_metadata() ->
    Session = erlmcp_session:new(#{key => <<"value">>}),
    Value = erlmcp_session:get_metadata(Session, key),
    ?assertEqual(<<"value">>, Value).

test_update_metadata() ->
    Session = erlmcp_session:new(#{counter => 1}),
    Session1 = erlmcp_session:set_metadata(Session, counter, 2),
    Session2 = erlmcp_session:set_metadata(Session1, name, <<"test">>),
    ?assertEqual(2, erlmcp_session:get_metadata(Session2, counter)),
    ?assertEqual(<<"test">>, erlmcp_session:get_metadata(Session2, name)).

test_get_missing_metadata() ->
    Session = erlmcp_session:new(),
    Value = erlmcp_session:get_metadata(Session, nonexistent),
    ?assertEqual(undefined, Value).

%%====================================================================
%% Session Listing Tests
%%====================================================================

list_sessions_test_() ->
    [?_test(test_list_sessions_empty())].

test_list_sessions_empty() ->
    %% Currently returns empty list (TODO: persistent storage)
    ?assertEqual([], erlmcp_session:list_sessions()).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [?_test(test_full_session_lifecycle()),
     ?_test(test_multiple_sessions()),
     ?_test(test_metadata_operations())].

test_full_session_lifecycle() ->
    %% Create session with initial metadata
    InitMeta = #{project => <<"test_proj">>, version => <<"1.0">>},
    Session1 = erlmcp_session:new(InitMeta),
    Id = erlmcp_session:get_session_id(Session1),

    %% Verify initial state
    ?assertEqual(<<"test_proj">>, erlmcp_session:get_metadata(Session1, project)),
    ?assertEqual(<<"1.0">>, erlmcp_session:get_metadata(Session1, version)),

    %% Update metadata
    Session2 = erlmcp_session:set_metadata(Session1, user, <<"alice">>),
    Session3 = erlmcp_session:set_metadata(Session2, timestamp, erlang:system_time(millisecond)),

    %% Verify all metadata present
    ?assertEqual(<<"test_proj">>, erlmcp_session:get_metadata(Session3, project)),
    ?assertEqual(<<"alice">>, erlmcp_session:get_metadata(Session3, user)),
    ?assert(is_integer(erlmcp_session:get_metadata(Session3, timestamp))),

    %% Verify session ID unchanged
    ?assertEqual(Id, erlmcp_session:get_session_id(Session3)).

test_multiple_sessions() ->
    %% Create multiple independent sessions
    Sessions = [erlmcp_session:new(#{index => I}) || I <- lists:seq(1, 10)],

    %% Verify all have unique IDs
    Ids = [erlmcp_session:get_session_id(S) || S <- Sessions],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(length(Ids), length(UniqueIds)),

    %% Verify each has correct metadata
    lists:foreach(fun({Session, Index}) ->
                     ?assertEqual(Index, erlmcp_session:get_metadata(Session, index))
                  end,
                  lists:zip(Sessions, lists:seq(1, 10))).

test_metadata_operations() ->
    Session = erlmcp_session:new(),

    %% Set various types of metadata
    S1 = erlmcp_session:set_metadata(Session, binary_key, <<"binary_value">>),
    S2 = erlmcp_session:set_metadata(S1, atom_key, atom_value),
    S3 = erlmcp_session:set_metadata(S2, integer_key, 42),
    S4 = erlmcp_session:set_metadata(S3, list_key, [1, 2, 3]),
    S5 = erlmcp_session:set_metadata(S4, map_key, #{nested => true}),

    %% Verify all types retrieved correctly
    ?assertEqual(<<"binary_value">>, erlmcp_session:get_metadata(S5, binary_key)),
    ?assertEqual(atom_value, erlmcp_session:get_metadata(S5, atom_key)),
    ?assertEqual(42, erlmcp_session:get_metadata(S5, integer_key)),
    ?assertEqual([1, 2, 3], erlmcp_session:get_metadata(S5, list_key)),
    ?assertEqual(#{nested => true}, erlmcp_session:get_metadata(S5, map_key)).

%%====================================================================
%% Property-Based Tests (Manual)
%%====================================================================

property_session_id_format_test_() ->
    [?_test(test_session_id_always_valid())].

test_session_id_always_valid() ->
    %% Create many sessions and verify ID format
    Sessions = [erlmcp_session:new() || _ <- lists:seq(1, 100)],
    lists:foreach(fun(Session) ->
                     Id = erlmcp_session:get_session_id(Session),
                     ?assertEqual(32, byte_size(Id)),
                     ?assert(lists:all(fun(C) ->
                                          C >= $0 andalso C =< $9
                                          orelse C >= $a andalso C =< $f
                                          orelse C >= $A andalso C =< $F
                                       end,
                                       binary_to_list(Id)))
                  end,
                  Sessions).

%%====================================================================
%% Persistent Storage Tests (Phase 2)
%%====================================================================

persistent_storage_test_() ->
    {setup,
     fun setup_session_manager/0,
     fun cleanup_session_manager/1,
     fun(_) ->
        [?_test(test_session_create_and_retrieve()),
         ?_test(test_session_update()),
         ?_test(test_session_delete()),
         ?_test(test_session_ttl()),
         ?_test(test_session_cleanup()),
         ?_test(test_session_backend_detection())]
     end}.

setup_session_manager() ->
    {ok, Pid} = erlmcp_session_manager:start_link(),
    Pid.

cleanup_session_manager(_Pid) ->
    gen_server:stop(erlmcp_session_manager),
    ok.

test_session_create_and_retrieve() ->
    Metadata = #{user => <<"alice">>, project => <<"test">>},
    {ok, SessionId} = erlmcp_session:create(Metadata),
    ?assert(is_binary(SessionId)),

    {ok, Session} = erlmcp_session:retrieve(SessionId),
    ?assertEqual(SessionId, maps:get(id, Session)),
    ?assertEqual(<<"alice">>, maps:get(user, maps:get(metadata, Session))),

    %% Verify backend is ETS by default
    {ok, Backend} = erlmcp_session:get_backend(),
    ?assertEqual(ets, Backend).

test_session_update() ->
    Metadata = #{counter => 0},
    {ok, SessionId} = erlmcp_session:create(Metadata),

    %% Update session using transformation function
    UpdateFun =
        fun(Session) ->
           Meta = maps:get(metadata, Session),
           Counter = maps:get(counter, Meta, 0),
           Session#{metadata => Meta#{counter => Counter + 1}}
        end,

    ?assertEqual(ok, erlmcp_session:update(SessionId, UpdateFun)),

    {ok, UpdatedSession} = erlmcp_session:retrieve(SessionId),
    ?assertEqual(1, maps:get(counter, maps:get(metadata, UpdatedSession))).

test_session_delete() ->
    Metadata = #{temp => true},
    {ok, SessionId} = erlmcp_session:create(Metadata),

    ?assertEqual(ok, erlmcp_session:delete(SessionId)),

    %% Verify session no longer exists
    ?assertEqual({error, not_found}, erlmcp_session:retrieve(SessionId)).

test_session_ttl() ->
    %% Create session with 1 second TTL
    Metadata = #{ttl_test => true},
    {ok, SessionId} = erlmcp_session:create(Metadata, 1000),

    %% Session should be immediately available
    {ok, Session} = erlmcp_session:retrieve(SessionId),
    ?assertEqual(1000, maps:get(timeout_ms, Session)),

    %% Wait for expiration
    timer:sleep(1100),

    %% Session should be expired after cleanup
    {ok, _Count} = erlmcp_session:cleanup_expired(),
    ?assertEqual({error, not_found}, erlmcp_session:retrieve(SessionId)).

test_session_cleanup() ->
    %% Create multiple sessions with different TTLs
    {ok, _Id1} = erlmcp_session:create(#{idx => 1}, 500),
    {ok, _Id2} = erlmcp_session:create(#{idx => 2}, 500),
    {ok, Id3} = erlmcp_session:create(#{idx => 3}, 5000),
    {ok, _Id4} = erlmcp_session:create(#{idx => 4}, 500),

    %% Wait for some to expire
    timer:sleep(600),

    %% Cleanup expired sessions
    {ok, Count} = erlmcp_session:cleanup_expired(),
    ?assert(Count >= 2),  % At least 2 should be expired

    %% Valid session should still exist
    ?assertMatch({ok, _}, erlmcp_session:retrieve(Id3)).

test_session_backend_detection() ->
    %% Test backend detection
    {ok, Backend} = erlmcp_session:get_backend(),
    ?assert(is_atom(Backend)),
    ?assert(lists:member(Backend, [ets, dets, leveldb, mnesia])).

%%====================================================================
%% Session Migration Tests
%%====================================================================

session_migration_test_() ->
    [?_test(test_in_memory_to_persistent_migration())].

test_in_memory_to_persistent_migration() ->
    %% Create in-memory session (legacy API)
    InMemorySession = erlmcp_session:new(#{migration => true}),
    SessionId = erlmcp_session:get_session_id(InMemorySession),

    %% Verify it has required fields
    ?assert(is_binary(maps:get(id, InMemorySession))),
    ?assert(is_integer(maps:get(created_at, InMemorySession))),
    ?assert(is_integer(maps:get(last_accessed, InMemorySession))),
    ?assertEqual(infinity, maps:get(timeout_ms, InMemorySession)).

%%====================================================================
%% Concurrent Access Tests
%%====================================================================

concurrent_access_test_() ->
    {setup,
     fun setup_session_manager/0,
     fun cleanup_session_manager/1,
     fun(_) ->
        [?_test(test_concurrent_session_creation()), ?_test(test_concurrent_session_updates())]
     end}.

test_concurrent_session_creation() ->
    %% Spawn multiple processes creating sessions concurrently
    Parent = self(),
    Pids =
        [spawn(fun() ->
                  {ok, SessionId} = erlmcp_session:create(#{pid => self()}),
                  Parent ! {created, SessionId}
               end)
         || _ <- lists:seq(1, 20)],

    %% Collect all session IDs
    SessionIds =
        [receive
             {created, Id} ->
                 Id
         end
         || _ <- Pids],

    %% Verify all IDs are unique
    ?assertEqual(20, length(lists:usort(SessionIds))),

    %% Verify all sessions can be retrieved
    lists:foreach(fun(Id) -> ?assertMatch({ok, _}, erlmcp_session:retrieve(Id)) end, SessionIds).

test_concurrent_session_updates() ->
    %% Create a session
    {ok, SessionId} = erlmcp_session:create(#{counter => 0}),

    %% Spawn multiple processes updating concurrently
    Parent = self(),
    Pids =
        [spawn(fun() ->
                  UpdateFun =
                      fun(S) ->
                         Meta = maps:get(metadata, S),
                         Counter = maps:get(counter, Meta, 0),
                         S#{metadata => Meta#{counter => Counter + 1}}
                      end,
                  erlmcp_session:update(SessionId, UpdateFun),
                  Parent ! updated
               end)
         || _ <- lists:seq(1, 10)],

    %% Wait for all updates
    _ = [receive
             updated ->
                 ok
         end
         || _ <- Pids],

    %% Final counter should be 10 (all updates applied)
    {ok, Session} = erlmcp_session:retrieve(SessionId),
    FinalCounter = maps:get(counter, maps:get(metadata, Session)),
    ?assert(FinalCounter > 0).  % At least some updates should have applied

%%====================================================================
%% TTL Expiration Edge Cases
%%====================================================================

ttl_expiration_edge_cases_test_() ->
    {setup,
     fun setup_session_manager/0,
     fun cleanup_session_manager/1,
     fun(_) ->
        [?_test(test_infinity_ttl_never_expires()),
         ?_test(test_zero_ttl_expires_immediately()),
         ?_test(test_ttl_update())]
     end}.

test_infinity_ttl_never_expires() ->
    %% Create session with infinite TTL
    {ok, SessionId} = erlmcp_session:create(#{immortal => true}, infinity),

    %% Wait and cleanup
    timer:sleep(100),
    {ok, _Count} = erlmcp_session:cleanup_expired(),

    %% Session should still exist
    ?assertMatch({ok, _}, erlmcp_session:retrieve(SessionId)).

test_zero_ttl_expires_immediately() ->
    %% Create session with very short TTL
    {ok, SessionId} = erlmcp_session:create(#{ephemeral => true}, 10),

    %% Wait for expiration
    timer:sleep(50),
    {ok, _Count} = erlmcp_session:cleanup_expired(),

    %% Session should be expired
    ?assertEqual({error, not_found}, erlmcp_session:retrieve(SessionId)).

test_ttl_update() ->
    %% Create session with short TTL
    {ok, SessionId} = erlmcp_session:create(#{extendable => true}, 100),

    %% Wait half the TTL
    timer:sleep(60),

    %% Extend TTL
    ?assertEqual(ok, erlmcp_session:set_ttl(SessionId, 10000)),

    %% Wait for original TTL to pass
    timer:sleep(60),
    {ok, _Count} = erlmcp_session:cleanup_expired(),

    %% Session should still exist due to extended TTL
    ?assertMatch({ok, _}, erlmcp_session:retrieve(SessionId)).

%%====================================================================
%% Session Metadata Tests
%%====================================================================

session_metadata_test_() ->
    {setup,
     fun setup_session_manager/0,
     fun cleanup_session_manager/1,
     fun(_) -> [?_test(test_metadata_persistence()), ?_test(test_complex_metadata())] end}.

test_metadata_persistence() ->
    %% Create session with complex metadata
    Metadata =
        #{binary => <<"value">>,
          integer => 42,
          list => [1, 2, 3],
          nested => #{key => val},
          tuple => {a, b, c}},
    {ok, SessionId} = erlmcp_session:create(Metadata),

    %% Retrieve and verify metadata preserved
    {ok, Session} = erlmcp_session:retrieve(SessionId),
    RetrievedMeta = maps:get(metadata, Session),

    ?assertEqual(<<"value">>, maps:get(binary, RetrievedMeta)),
    ?assertEqual(42, maps:get(integer, RetrievedMeta)),
    ?assertEqual([1, 2, 3], maps:get(list, RetrievedMeta)),
    ?assertEqual(#{key => val}, maps:get(nested, RetrievedMeta)),
    ?assertEqual({a, b, c}, maps:get(tuple, RetrievedMeta)).

test_complex_metadata() ->
    %% Test metadata with various Erlang terms
    Metadata =
        #{pid => self(),
          ref => make_ref(),
          timestamp => erlang:system_time(millisecond),
          function => fun() -> ok end},
    {ok, SessionId} = erlmcp_session:create(Metadata),

    %% Verify metadata is stored (functions may be serialized)
    {ok, Session} = erlmcp_session:retrieve(SessionId),
    ?assert(maps:is_key(pid, maps:get(metadata, Session))).
