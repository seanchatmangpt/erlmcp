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
    [
        ?_test(test_new_session()),
        ?_test(test_new_session_with_metadata()),
        ?_test(test_session_id_uniqueness()),
        ?_test(test_session_created_at())
    ].

test_new_session() ->
    Session = erlmcp_session:new(),
    ?assertMatch(#{id := _, created_at := _, metadata := _}, Session),
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
    [
        ?_test(test_get_session_id()),
        ?_test(test_session_id_format())
    ].

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
        (C >= $0 andalso C =< $9) orelse
        (C >= $a andalso C =< $f) orelse
        (C >= $A andalso C =< $F)
    end, binary_to_list(Id))).

%%====================================================================
%% Metadata Management Tests
%%====================================================================

metadata_test_() ->
    [
        ?_test(test_set_metadata()),
        ?_test(test_get_metadata()),
        ?_test(test_update_metadata()),
        ?_test(test_get_missing_metadata())
    ].

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
    [
        ?_test(test_list_sessions_empty())
    ].

test_list_sessions_empty() ->
    %% Currently returns empty list (TODO: persistent storage)
    ?assertEqual([], erlmcp_session:list_sessions()).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_full_session_lifecycle()),
        ?_test(test_multiple_sessions()),
        ?_test(test_metadata_operations())
    ].

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
    end, lists:zip(Sessions, lists:seq(1, 10))).

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
    [
        ?_test(test_session_id_always_valid())
    ].

test_session_id_always_valid() ->
    %% Create many sessions and verify ID format
    Sessions = [erlmcp_session:new() || _ <- lists:seq(1, 100)],
    lists:foreach(fun(Session) ->
        Id = erlmcp_session:get_session_id(Session),
        ?assertEqual(32, byte_size(Id)),
        ?assert(lists:all(fun(C) ->
            (C >= $0 andalso C =< $9) orelse
            (C >= $a andalso C =< $f) orelse
            (C >= $A andalso C =< $F)
        end, binary_to_list(Id)))
    end, Sessions).
