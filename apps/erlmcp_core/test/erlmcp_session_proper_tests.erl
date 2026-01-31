%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for erlmcp_session Module
%%%
%%% Tests invariants:
%%% - Session ID uniqueness and format
%%% - Session metadata operations (idempotent, commutative)
%%% - Session creation and retrieval consistency
%%% - Multiple session independence
%%%
%%% Chicago School TDD: Real session creation, no mocks, state-based verification
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Generators
%%%====================================================================

%% Generate valid metadata keys (atoms or binaries)
metadata_key() ->
    proper_types:oneof([
        proper_types:atom(),
        proper_types:binary()
    ]).

%% Generate valid metadata values
metadata_value() ->
    proper_types:oneof([
        proper_types:binary(),
        proper_types:int(),
        proper_types:atom(),
        proper_types:bool(),
        proper_types:list(proper_types:int()),
        proper_types:map(proper_types:binary(), proper_types:int())
    ]).

%% Generate valid metadata maps
metadata_map() ->
    proper_types:map(metadata_key(), metadata_value()).

%% Generate session data
session_data() ->
    proper_types:tuple([
        metadata_map()
    ]).

%%%====================================================================
%%% Properties: Session Creation
%%%====================================================================

%% Property: All session IDs are unique (no collisions in 1000 attempts)
prop_session_ids_are_unique() ->
    ?FORALL(_Count, proper_types:range(100, 1000),
        begin
            Sessions = [erlmcp_session:new() || _ <- lists:seq(1, _Count)],
            Ids = [erlmcp_session:get_session_id(S) || S <- Sessions],
            length(Ids) =:= length(lists:usort(Ids))
        end).

%% Property: Session ID is always 32 bytes (16 random bytes in hex)
prop_session_id_format() ->
    ?FORALL(_Metadata, metadata_map(),
        begin
            Session = erlmcp_session:new(_Metadata),
            Id = erlmcp_session:get_session_id(Session),
            byte_size(Id) =:= 32 andalso is_valid_hex_string(Id)
        end).

%% Property: Session creation timestamp is monotonically increasing within small window
prop_session_creation_timestamp() ->
    ?FORALL({Metadata1, Metadata2}, {metadata_map(), metadata_map()},
        begin
            Session1 = erlmcp_session:new(Metadata1),
            timer:sleep(1),  % Ensure at least 1ms difference
            Session2 = erlmcp_session:new(Metadata2),
            CreatedAt1 = maps:get(created_at, Session1),
            CreatedAt2 = maps:get(created_at, Session2),
            CreatedAt2 > CreatedAt1
        end).

%% Property: Session with empty metadata starts with empty map
prop_session_empty_metadata() ->
    ?FORALL(_Ignored, metadata_map(),
        begin
            Session = erlmcp_session:new(),
            Metadata = maps:get(metadata, Session),
            Metadata =:= #{}
        end).

%% Property: Session with provided metadata preserves it
prop_session_preserves_metadata() ->
    ?FORALL(Metadata, metadata_map(),
        begin
            Session = erlmcp_session:new(Metadata),
            SessionMetadata = maps:get(metadata, Session),
            SessionMetadata =:= Metadata
        end).

%%%====================================================================
%%% Properties: Metadata Operations
%%%====================================================================

%% Property: Setting and getting metadata is idempotent
prop_metadata_set_get_idempotent() ->
    ?FORALL({Metadata, Key, Value}, {metadata_map(), metadata_key(), metadata_value()},
        begin
            Session0 = erlmcp_session:new(Metadata),
            Session1 = erlmcp_session:set_metadata(Session0, Key, Value),
            Session2 = erlmcp_session:set_metadata(Session1, Key, Value),
            Value1 = erlmcp_session:get_metadata(Session1, Key),
            Value2 = erlmcp_session:get_metadata(Session2, Key),
            Value1 =:= Value andalso Value2 =:= Value
        end).

%% Property: Setting metadata overrides previous value
prop_metadata_set_overwrites() ->
    ?FORALL({Metadata, Key, Value1, Value2},
            {metadata_map(), metadata_key(), metadata_value(), metadata_value()},
        ?IMPLIES(Value1 =/= Value2,
        begin
            Session0 = erlmcp_session:new(Metadata),
            Session1 = erlmcp_session:set_metadata(Session0, Key, Value1),
            Session2 = erlmcp_session:set_metadata(Session1, Key, Value2),
            erlmcp_session:get_metadata(Session2, Key) =:= Value2
        end)).

%% Property: Getting missing metadata returns undefined
prop_metadata_missing_returns_undefined() ->
    ?FORALL({Metadata, Key}, {metadata_map(), metadata_key()},
        begin
            Session = erlmcp_session:new(Metadata),
            %% Get metadata without setting it
            Result = erlmcp_session:get_metadata(Session, Key),
            %% If Key was in original metadata, remove it first
            SessionWithoutKey = maps:remove(Key, Metadata),
            SessionTest = erlmcp_session:new(SessionWithoutKey),
            Result2 = erlmcp_session:get_metadata(SessionTest, Key),
            Result2 =:= undefined
        end).

%% Property: Multiple metadata operations are commutative for different keys
prop_metadata_commutative() ->
    ?FORALL({Metadata, Key1, Key2, Value1, Value2},
            {metadata_map(), metadata_key(), metadata_key(), metadata_value(), metadata_value()},
        ?IMPLIES(Key1 =/= Key2,
        begin
            Session0 = erlmcp_session:new(Metadata),
            %% Order 1: Set Key1 then Key2
            Session1A = erlmcp_session:set_metadata(Session0, Key1, Value1),
            Session1 = erlmcp_session:set_metadata(Session1A, Key2, Value2),
            %% Order 2: Set Key2 then Key1
            Session2A = erlmcp_session:set_metadata(Session0, Key2, Value2),
            Session2 = erlmcp_session:set_metadata(Session2A, Key1, Value1),
            %% Both should have same metadata
            erlmcp_session:get_metadata(Session1, Key1) =:= erlmcp_session:get_metadata(Session2, Key1)
            andalso erlmcp_session:get_metadata(Session1, Key2) =:= erlmcp_session:get_metadata(Session2, Key2)
        end)).

%% Property: Setting metadata doesn't change session ID
prop_metadata_preserves_session_id() ->
    ?FORALL({Metadata, Key, Value}, {metadata_map(), metadata_key(), metadata_value()},
        begin
            Session0 = erlmcp_session:new(Metadata),
            OriginalId = erlmcp_session:get_session_id(Session0),
            Session1 = erlmcp_session:set_metadata(Session0, Key, Value),
            NewId = erlmcp_session:get_session_id(Session1),
            OriginalId =:= NewId
        end).

%% Property: Setting metadata doesn't change created_at timestamp
prop_metadata_preserves_created_at() ->
    ?FORALL({Metadata, Key, Value}, {metadata_map(), metadata_key(), metadata_value()},
        begin
            Session0 = erlmcp_session:new(Metadata),
            OriginalCreatedAt = maps:get(created_at, Session0),
            Session1 = erlmcp_session:set_metadata(Session0, Key, Value),
            NewCreatedAt = maps:get(created_at, Session1),
            OriginalCreatedAt =:= NewCreatedAt
        end).

%%%====================================================================
%%% Properties: Multiple Sessions
%%%====================================================================

%% Property: Multiple sessions are independent
prop_multiple_sessions_independent() ->
    ?FORALL({Metadata1, Metadata2, Key, Value},
            {metadata_map(), metadata_map(), metadata_key(), metadata_value()},
        begin
            Session1 = erlmcp_session:new(Metadata1),
            Session2 = erlmcp_session:new(Metadata2),
            %% Set different metadata in each session
            Session1Updated = erlmcp_session:set_metadata(Session1, Key, Value),
            Session2Updated = erlmcp_session:set_metadata(Session2, Key, <<"different">>),
            %% Verify independence
            Session1Value = erlmcp_session:get_metadata(Session1Updated, Key),
            Session2Value = erlmcp_session:get_metadata(Session2Updated, Key),
            Session1Value =:= Value andalso Session2Value =:= <<"different">>
        end).

%% Property: Session IDs from metadata map are not influenced by metadata content
prop_session_id_independent_of_metadata() ->
    ?FORALL({Metadata1, Metadata2}, {metadata_map(), metadata_map()},
        ?IMPLIES(Metadata1 =/= Metadata2,
        begin
            Session1 = erlmcp_session:new(Metadata1),
            Session2 = erlmcp_session:new(Metadata2),
            Id1 = erlmcp_session:get_session_id(Session1),
            Id2 = erlmcp_session:get_session_id(Session2),
            %% Different metadata should not cause ID collision
            Id1 =/= Id2
        end)).

%%%====================================================================
%%% Properties: List Sessions (when implemented)
%%%====================================================================

%% Property: list_sessions returns list (may be empty until persistent storage)
prop_list_sessions_returns_list() ->
    ?FORALL(_Count, proper_types:range(0, 10),
        begin
            %% Create sessions (not stored yet)
            _ = [erlmcp_session:new() || _ <- lists:seq(1, _Count)],
            %% list_sessions currently returns empty list (TODO: persistent storage)
            Listed = erlmcp_session:list_sessions(),
            is_list(Listed)
        end).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Check if binary is valid hex string (all characters are 0-9, a-f, A-F)
is_valid_hex_string(Bin) ->
    lists:all(fun(C) ->
        (C >= $0 andalso C =< $9) orelse
        (C >= $a andalso C =< $f) orelse
        (C >= $A andalso C =< $F)
    end, binary_to_list(Bin)).

%%%====================================================================
%%% EUnit Integration
%%%====================================================================

proper_test_() ->
    [
        {"Session IDs are unique", ?_assertEqual(true, proper:quickcheck(prop_session_ids_are_unique(), 50))},
        {"Session ID format is valid", ?_assertEqual(true, proper:quickcheck(prop_session_id_format(), 100))},
        {"Session creation timestamps", ?_assertEqual(true, proper:quickcheck(prop_session_creation_timestamp(), 10))},
        {"Session empty metadata", ?_assertEqual(true, proper:quickcheck(prop_session_empty_metadata(), 50))},
        {"Session preserves metadata", ?_assertEqual(true, proper:quickcheck(prop_session_preserves_metadata(), 100))},
        {"Metadata set/get idempotent", ?_assertEqual(true, proper:quickcheck(prop_metadata_set_get_idempotent(), 100))},
        {"Metadata set overwrites", ?_assertEqual(true, proper:quickcheck(prop_metadata_set_overwrites(), 50))},
        {"Metadata missing returns undefined", ?_assertEqual(true, proper:quickcheck(prop_metadata_missing_returns_undefined(), 50))},
        {"Metadata operations commutative", ?_assertEqual(true, proper:quickcheck(prop_metadata_commutative(), 50))},
        {"Metadata preserves session ID", ?_assertEqual(true, proper:quickcheck(prop_metadata_preserves_session_id(), 100))},
        {"Metadata preserves created_at", ?_assertEqual(true, proper:quickcheck(prop_metadata_preserves_created_at(), 100))},
        {"Multiple sessions independent", ?_assertEqual(true, proper:quickcheck(prop_multiple_sessions_independent(), 50))},
        {"Session ID independent of metadata", ?_assertEqual(true, proper:quickcheck(prop_session_id_independent_of_metadata(), 50))},
        {"List sessions returns list", ?_assertEqual(true, proper:quickcheck(prop_list_sessions_returns_list(), 20))}
    ].

%%%====================================================================
%%% Run All Properties
%%%====================================================================

run_all_properties() ->
    proper:module(?MODULE, [{numtests, 100}]).
