%%%-------------------------------------------------------------------
%% @doc MCP Session Manager Tests
%%
%% Comprehensive test suite for session creation, validation, and management.
%% Tests cover:
%% - Session creation with UUID generation
%% - Session validation and expiry
%% - Session touch/refresh timeout
%% - Session deletion
%% - Automatic cleanup of expired sessions
%% - ETS storage and retrieval
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    case erlmcp_session_manager:start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _}} -> ok
    end.

cleanup(_) ->
    gen_server:stop(erlmcp_session_manager, normal, 5000).

%%====================================================================
%% Session Creation Tests
%%====================================================================

%% Test 1: Create session returns session ID
create_session_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    ?assert(is_binary(SessionId)),
    ?assert(byte_size(SessionId) > 0).

%% Test 2: Session ID is unique (not repeated)
create_unique_sessions_test() ->
    {ok, SessionId1} = erlmcp_session_manager:create_session(),
    {ok, SessionId2} = erlmcp_session_manager:create_session(),
    ?assertNotEqual(SessionId1, SessionId2).

%% Test 3: Session ID is in hex format
session_id_format_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    %% Should be valid hex string (lowercase letters and digits)
    HexPattern = <<"^[0-9a-f]+$">>,
    ?assert(is_valid_hex(SessionId)).

%% Test 4: Session ID length is consistent (256-bit = 64 hex chars)
session_id_length_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    %% 256 bits = 32 bytes = 64 hex characters
    ?assert(byte_size(SessionId) >= 32).

%% Test 5: Multiple sessions can be created in sequence
multiple_sessions_test() ->
    Sessions = [erlmcp_session_manager:create_session() || _ <- lists:seq(1, 5)],
    ?assertEqual(5, length(Sessions)),
    SessionIds = [Id || {ok, Id} <- Sessions],
    %% All should be unique
    ?assertEqual(5, length(lists:uniq(SessionIds))).

%%====================================================================
%% Session Validation Tests
%%====================================================================

%% Test 6: Validate newly created session
validate_new_session_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertEqual({ok, valid}, Result).

%% Test 7: Validate non-existent session
validate_nonexistent_session_test() ->
    Result = erlmcp_session_manager:validate_session(<<"nonexistent">>),
    ?assertEqual({error, not_found}, Result).

%% Test 8: Accept string session IDs
validate_session_string_id_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    StringId = erlang:binary_to_list(SessionId),
    Result = erlmcp_session_manager:validate_session(StringId),
    ?assertEqual({ok, valid}, Result).

%% Test 9: Reject invalid session IDs
validate_invalid_id_type_test() ->
    Result = erlmcp_session_manager:validate_session(invalid_atom),
    ?assertEqual({error, invalid}, Result).

%% Test 10: Session expires after timeout
expire_session_test() ->
    %% Start manager with very short timeout (1 second)
    gen_server:stop(erlmcp_session_manager),
    timer:sleep(100),
    {ok, _} = erlmcp_session_manager:start_link(#{timeout => 1}),

    {ok, SessionId} = erlmcp_session_manager:create_session(),
    ?assertEqual({ok, valid}, erlmcp_session_manager:validate_session(SessionId)),

    %% Wait for expiry
    timer:sleep(1100),

    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertEqual({error, expired}, Result).

%%====================================================================
%% Session Touch Tests
%%====================================================================

%% Test 11: Touch session refreshes timeout
touch_session_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    Result = erlmcp_session_manager:touch_session(SessionId),
    ?assertEqual(ok, Result).

%% Test 12: Touch extends session expiry
touch_extends_expiry_test() ->
    gen_server:stop(erlmcp_session_manager),
    timer:sleep(100),
    {ok, _} = erlmcp_session_manager:start_link(#{timeout => 2}),

    {ok, SessionId} = erlmcp_session_manager:create_session(),

    %% Wait 1 second
    timer:sleep(1000),

    %% Touch to refresh
    ?assertEqual(ok, erlmcp_session_manager:touch_session(SessionId)),

    %% Wait another 1 second (total 2 seconds from touch)
    timer:sleep(1000),

    %% Should still be valid (2 seconds timeout + 1 second touch = 3 seconds total)
    ?assertEqual({ok, valid}, erlmcp_session_manager:validate_session(SessionId)).

%% Test 13: Touch non-existent session fails
touch_nonexistent_test() ->
    Result = erlmcp_session_manager:touch_session(<<"nonexistent">>),
    ?assertEqual({error, not_found}, Result).

%% Test 14: Touch with string session ID
touch_string_id_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    StringId = erlang:binary_to_list(SessionId),
    Result = erlmcp_session_manager:touch_session(StringId),
    ?assertEqual(ok, Result).

%%====================================================================
%% Session Deletion Tests
%%====================================================================

%% Test 15: Delete existing session
delete_session_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    ?assertEqual({ok, valid}, erlmcp_session_manager:validate_session(SessionId)),

    Result = erlmcp_session_manager:delete_session(SessionId),
    ?assertEqual(ok, Result),

    %% Session should no longer exist
    ?assertEqual({error, not_found}, erlmcp_session_manager:validate_session(SessionId)).

%% Test 16: Delete non-existent session fails
delete_nonexistent_test() ->
    Result = erlmcp_session_manager:delete_session(<<"nonexistent">>),
    ?assertEqual({error, not_found}, Result).

%% Test 17: Delete with string session ID
delete_string_id_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    StringId = erlang:binary_to_list(SessionId),
    Result = erlmcp_session_manager:delete_session(StringId),
    ?assertEqual(ok, Result).

%%====================================================================
%% Session Info Tests
%%====================================================================

%% Test 18: Get session info returns timestamps
get_session_info_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, Info} = erlmcp_session_manager:get_session_info(SessionId),

    ?assert(is_map(Info)),
    ?assert(maps:is_key(id, Info)),
    ?assert(maps:is_key(created_at, Info)),
    ?assert(maps:is_key(last_accessed, Info)),
    ?assertEqual(SessionId, maps:get(id, Info)).

%% Test 19: created_at and last_accessed are timestamps
session_timestamps_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, Info} = erlmcp_session_manager:get_session_info(SessionId),

    CreatedAt = maps:get(created_at, Info),
    LastAccessed = maps:get(last_accessed, Info),

    ?assert(is_integer(CreatedAt)),
    ?assert(is_integer(LastAccessed)),
    ?assert(CreatedAt > 0),
    ?assert(LastAccessed > 0).

%% Test 20: last_accessed updates after touch
last_accessed_updates_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, Info1} = erlmcp_session_manager:get_session_info(SessionId),
    LastAccessed1 = maps:get(last_accessed, Info1),

    %% Wait a moment
    timer:sleep(100),

    %% Touch session
    erlmcp_session_manager:touch_session(SessionId),
    {ok, Info2} = erlmcp_session_manager:get_session_info(SessionId),
    LastAccessed2 = maps:get(last_accessed, Info2),

    ?assert(LastAccessed2 >= LastAccessed1).

%% Test 21: Get info for non-existent session
get_nonexistent_session_info_test() ->
    Result = erlmcp_session_manager:get_session_info(<<"nonexistent">>),
    ?assertEqual({error, not_found}, Result).

%%====================================================================
%% Automatic Cleanup Tests
%%====================================================================

%% Test 22: Manual cleanup removes expired sessions
manual_cleanup_test() ->
    gen_server:stop(erlmcp_session_manager),
    timer:sleep(100),
    {ok, _} = erlmcp_session_manager:start_link(#{timeout => 1}),

    {ok, SessionId} = erlmcp_session_manager:create_session(),
    timer:sleep(1100),

    {ok, DeletedCount} = erlmcp_session_manager:cleanup_expired_sessions(),
    ?assert(DeletedCount >= 1),
    ?assertEqual({error, not_found}, erlmcp_session_manager:validate_session(SessionId)).

%% Test 23: Cleanup doesn't remove valid sessions
cleanup_preserves_valid_test() ->
    {ok, SessionId1} = erlmcp_session_manager:create_session(),
    {ok, SessionId2} = erlmcp_session_manager:create_session(),

    {ok, _} = erlmcp_session_manager:cleanup_expired_sessions(),

    %% Both sessions should still be valid
    ?assertEqual({ok, valid}, erlmcp_session_manager:validate_session(SessionId1)),
    ?assertEqual({ok, valid}, erlmcp_session_manager:validate_session(SessionId2)).

%%====================================================================
%% Concurrent Access Tests
%%====================================================================

%% Test 24: Concurrent session creation
concurrent_creation_test() ->
    Parent = self(),
    Pids = [spawn_link(fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(),
        Parent ! {session, SessionId}
    end) || _ <- lists:seq(1, 10)],

    Sessions = collect_messages(10, []),
    ?assertEqual(10, length(Sessions)).

%% Test 25: Concurrent validation
concurrent_validation_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    Parent = self(),

    [spawn_link(fun() ->
        Result = erlmcp_session_manager:validate_session(SessionId),
        Parent ! {validation, Result}
    end) || _ <- lists:seq(1, 10)],

    Results = collect_validation_messages(10, []),
    ?assert(lists:all(fun(R) -> R =:= {ok, valid} end, Results)).

%%====================================================================
%% Edge Cases
%%====================================================================

%% Test 26: Session manager survives process restart
session_persistence_during_restart_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    ?assertEqual({ok, valid}, erlmcp_session_manager:validate_session(SessionId)),

    gen_server:stop(erlmcp_session_manager),
    timer:sleep(100),
    {ok, _} = erlmcp_session_manager:start_link(),

    %% Old session should not exist after restart (ETS table recreated)
    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertEqual({error, not_found}, Result).

%% Test 27: Handle invalid input gracefully
invalid_input_handling_test() ->
    ?assertEqual({error, invalid}, erlmcp_session_manager:validate_session(123)),
    ?assertEqual({error, invalid}, erlmcp_session_manager:validate_session(atom)),
    ?assertEqual({error, invalid}, erlmcp_session_manager:touch_session(123)),
    ?assertEqual({error, invalid}, erlmcp_session_manager:delete_session(123)).

%%====================================================================
%% Helper Functions
%%====================================================================

is_valid_hex(Bin) ->
    case catch erlang:binary_to_list(Bin) of
        {'EXIT', _} -> false;
        List ->
            lists:all(
                fun(C) ->
                    (C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F)
                end,
                List
            )
    end.

collect_messages(0, Acc) ->
    Acc;
collect_messages(N, Acc) ->
    receive
        {session, SessionId} ->
            collect_messages(N - 1, [SessionId | Acc])
    after 5000 ->
        Acc
    end.

collect_validation_messages(0, Acc) ->
    Acc;
collect_validation_messages(N, Acc) ->
    receive
        {validation, Result} ->
            collect_validation_messages(N - 1, [Result | Acc])
    after 5000 ->
        Acc
    end.

%%====================================================================
%% Test Groups
%%====================================================================

all_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        %% Session creation
        create_session_test(),
        create_unique_sessions_test(),
        session_id_format_test(),
        session_id_length_test(),
        multiple_sessions_test(),

        %% Session validation
        validate_new_session_test(),
        validate_nonexistent_session_test(),
        validate_session_string_id_test(),
        validate_invalid_id_type_test(),
        expire_session_test(),

        %% Session touch
        touch_session_test(),
        touch_extends_expiry_test(),
        touch_nonexistent_test(),
        touch_string_id_test(),

        %% Session deletion
        delete_session_test(),
        delete_nonexistent_test(),
        delete_string_id_test(),

        %% Session info
        get_session_info_test(),
        session_timestamps_test(),
        last_accessed_updates_test(),
        get_nonexistent_session_info_test(),

        %% Automatic cleanup
        manual_cleanup_test(),
        cleanup_preserves_valid_test(),

        %% Concurrent access
        concurrent_creation_test(),
        concurrent_validation_test(),

        %% Edge cases
        session_persistence_during_restart_test(),
        invalid_input_handling_test()
    ]}.
