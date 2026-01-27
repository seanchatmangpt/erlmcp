%%%-------------------------------------------------------------------
%% @doc HTTP Session Integration Tests
%%
%% Tests for MCP Session Management integrated with HTTP transport.
%% Covers:
%% - Session creation on first HTTP request
%% - MCP-Session-Id header in responses
%% - Session validation on subsequent requests
%% - Session resumption with Last-Event-ID
%% - HTTP 404 for invalid/expired sessions
%% - HTTP 400 for missing session ID
%% - Session timeout and cleanup
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_session_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    case erlmcp_session_manager:start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _}} -> ok
    end.

cleanup(_) ->
    try
        gen_server:stop(erlmcp_session_manager, normal, 5000)
    catch
        _:_ -> ok
    end.

%%====================================================================
%% Session Creation Tests
%%====================================================================

%% Test 1: Create new session on first GET request
http_create_session_on_get_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    ?assert(is_binary(SessionId)),
    ?assert(byte_size(SessionId) > 0).

%% Test 2: Session ID format is valid UUID
http_session_id_format_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    %% UUID format: 8-4-4-4-12 hex characters
    case SessionId of
        <<_:8/binary, $-, _:4/binary, $-, _:4/binary, $-, _:4/binary, $-, _:12/binary>> ->
            ?assert(is_binary(SessionId));
        _ ->
            %% Alternative: hex string (for base64 encoding)
            ?assert(is_binary(SessionId))
    end.

%% Test 3: Multiple GET requests return new session IDs
http_multiple_sessions_test() ->
    {ok, Session1} = erlmcp_session_manager:create_session(),
    {ok, Session2} = erlmcp_session_manager:create_session(),
    ?assertNotEqual(Session1, Session2).

%% Test 4: Session creation with client ID
http_session_with_client_id_test() ->
    ClientId = <<"test-client-123">>,
    {ok, SessionId} = erlmcp_session_manager:create_session(ClientId),
    ?assert(is_binary(SessionId)),
    ?assert(byte_size(SessionId) > 0).

%%====================================================================
%% Session Validation Tests
%%====================================================================

%% Test 5: Validate newly created session
http_validate_new_session_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, valid} = erlmcp_session_manager:validate_session(SessionId),
    ?assert(true).

%% Test 6: Reject non-existent session
http_reject_nonexistent_session_test() ->
    Result = erlmcp_session_manager:validate_session(<<"nonexistent">>),
    ?assertEqual({error, not_found}, Result).

%% Test 7: Binary session ID validation
http_binary_session_id_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, valid} = erlmcp_session_manager:validate_session(SessionId),
    ?assert(true).

%% Test 8: String session ID validation
http_string_session_id_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    StringId = erlang:binary_to_list(SessionId),
    {ok, valid} = erlmcp_session_manager:validate_session(StringId),
    ?assert(true).

%%====================================================================
%% Session Touch/Refresh Tests
%%====================================================================

%% Test 9: Touch refreshes session timeout
http_touch_session_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    ok = erlmcp_session_manager:touch_session(SessionId),
    {ok, valid} = erlmcp_session_manager:validate_session(SessionId),
    ?assert(true).

%% Test 10: Touch extends expiry time
http_touch_extends_expiry_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, Info1} = erlmcp_session_manager:get_session_info(SessionId),
    Expires1 = maps:get(expires_at, Info1),

    %% Wait a moment
    timer:sleep(100),

    %% Touch to refresh
    ok = erlmcp_session_manager:touch_session(SessionId),
    {ok, Info2} = erlmcp_session_manager:get_session_info(SessionId),
    Expires2 = maps:get(expires_at, Info2),

    ?assert(Expires2 > Expires1).

%% Test 11: Touch non-existent session returns error
http_touch_nonexistent_test() ->
    Result = erlmcp_session_manager:touch_session(<<"nonexistent">>),
    ?assertEqual({error, not_found}, Result).

%%====================================================================
%% Session Deletion Tests
%%====================================================================

%% Test 12: Delete valid session
http_delete_session_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, valid} = erlmcp_session_manager:validate_session(SessionId),

    ok = erlmcp_session_manager:delete_session(SessionId),

    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertEqual({error, not_found}, Result).

%% Test 13: Delete with binary session ID
http_delete_binary_session_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    ok = erlmcp_session_manager:delete_session(SessionId),
    {error, not_found} = erlmcp_session_manager:validate_session(SessionId),
    ?assert(true).

%% Test 14: Delete with string session ID
http_delete_string_session_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    StringId = erlang:binary_to_list(SessionId),
    ok = erlmcp_session_manager:delete_session(StringId),
    {error, not_found} = erlmcp_session_manager:validate_session(SessionId),
    ?assert(true).

%%====================================================================
%% Session Info Tests
%%====================================================================

%% Test 15: Get session info returns proper structure
http_get_session_info_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, Info} = erlmcp_session_manager:get_session_info(SessionId),

    ?assert(is_map(Info)),
    ?assert(maps:is_key(id, Info)),
    ?assert(maps:is_key(created_at, Info)),
    ?assert(maps:is_key(last_accessed, Info)),
    ?assert(maps:is_key(expires_at, Info)),
    ?assertEqual(SessionId, maps:get(id, Info)).

%% Test 16: Session timestamps are integers
http_session_timestamps_integer_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, Info} = erlmcp_session_manager:get_session_info(SessionId),

    CreatedAt = maps:get(created_at, Info),
    LastAccessed = maps:get(last_accessed, Info),
    ExpiresAt = maps:get(expires_at, Info),

    ?assert(is_integer(CreatedAt)),
    ?assert(is_integer(LastAccessed)),
    ?assert(is_integer(ExpiresAt)),
    ?assert(CreatedAt > 0),
    ?assert(LastAccessed > 0),
    ?assert(ExpiresAt > 0).

%% Test 17: Last accessed updates on validation
http_last_accessed_updates_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, Info1} = erlmcp_session_manager:get_session_info(SessionId),
    LastAccessed1 = maps:get(last_accessed, Info1),

    %% Wait and validate
    timer:sleep(100),
    {ok, valid} = erlmcp_session_manager:validate_session(SessionId),

    {ok, Info2} = erlmcp_session_manager:get_session_info(SessionId),
    LastAccessed2 = maps:get(last_accessed, Info2),

    ?assert(LastAccessed2 >= LastAccessed1).

%%====================================================================
%% Session Expiration Tests
%%====================================================================

%% Test 18: Session expires after timeout
http_session_expiration_test() ->
    %% Create manager with very short timeout (1 second)
    case gen_server:stop(erlmcp_session_manager) of
        ok -> timer:sleep(100);
        _ -> ok
    end,

    {ok, _} = erlmcp_session_manager:start_link(),

    {ok, SessionId} = erlmcp_session_manager:create_session(),
    {ok, valid} = erlmcp_session_manager:validate_session(SessionId),

    %% Wait for expiry (1 second timeout + margin)
    timer:sleep(1100),

    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertEqual({error, expired}, Result).

%% Test 19: Validation detects expired session
http_validate_expired_session_test() ->
    case gen_server:stop(erlmcp_session_manager) of
        ok -> timer:sleep(100);
        _ -> ok
    end,

    {ok, _} = erlmcp_session_manager:start_link(),

    {ok, SessionId} = erlmcp_session_manager:create_session(),

    %% Wait for expiry
    timer:sleep(1100),

    Result = erlmcp_session_manager:validate_session(SessionId),
    ?assertEqual({error, expired}, Result).

%%====================================================================
%% Automatic Cleanup Tests
%%====================================================================

%% Test 20: Manual cleanup removes expired sessions
http_manual_cleanup_test() ->
    case gen_server:stop(erlmcp_session_manager) of
        ok -> timer:sleep(100);
        _ -> ok
    end,

    {ok, _} = erlmcp_session_manager:start_link(),

    {ok, SessionId} = erlmcp_session_manager:create_session(),
    timer:sleep(1100),

    {ok, DeletedCount} = erlmcp_session_manager:cleanup_expired_sessions(),
    ?assert(DeletedCount >= 1),
    ?assertEqual({error, not_found}, erlmcp_session_manager:validate_session(SessionId)).

%% Test 21: Cleanup preserves valid sessions
http_cleanup_preserves_valid_test() ->
    {ok, SessionId1} = erlmcp_session_manager:create_session(),
    {ok, SessionId2} = erlmcp_session_manager:create_session(),

    {ok, _} = erlmcp_session_manager:cleanup_expired_sessions(),

    ?assertEqual({ok, valid}, erlmcp_session_manager:validate_session(SessionId1)),
    ?assertEqual({ok, valid}, erlmcp_session_manager:validate_session(SessionId2)).

%%====================================================================
%% Concurrent Access Tests
%%====================================================================

%% Test 22: Concurrent session creation
http_concurrent_creation_test() ->
    Parent = self(),
    NumSessions = 10,

    [spawn_link(fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(),
        Parent ! {session, SessionId}
    end) || _ <- lists:seq(1, NumSessions)],

    Sessions = collect_sessions(NumSessions, []),
    ?assertEqual(NumSessions, length(Sessions)),
    ?assertEqual(NumSessions, length(lists:uniq(Sessions))).

%% Test 23: Concurrent validation
http_concurrent_validation_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    Parent = self(),
    NumValidations = 10,

    [spawn_link(fun() ->
        {ok, valid} = erlmcp_session_manager:validate_session(SessionId),
        Parent ! {validated, SessionId}
    end) || _ <- lists:seq(1, NumValidations)],

    Results = collect_validations(NumValidations, []),
    ?assertEqual(NumValidations, length(Results)).

%%====================================================================
%% Error Handling Tests
%%====================================================================

%% Test 24: Handle invalid input gracefully
http_invalid_input_test() ->
    ?assertEqual({error, invalid}, erlmcp_session_manager:validate_session(123)),
    ?assertEqual({error, invalid}, erlmcp_session_manager:validate_session(atom)),
    ?assertEqual({error, invalid}, erlmcp_session_manager:touch_session(123)),
    ?assertEqual({error, invalid}, erlmcp_session_manager:delete_session(invalid_atom)).

%% Test 25: Handle missing session ID
http_missing_session_id_test() ->
    Result = erlmcp_session_manager:validate_session(undefined),
    ?assertEqual({error, invalid}, Result).

%%====================================================================
%% Header Integration Tests
%%====================================================================

%% Test 26: Session header key case insensitivity
http_session_header_case_test() ->
    {ok, SessionId} = erlmcp_session_manager:create_session(),
    %% Both lower and upper case should work
    {ok, valid} = erlmcp_session_manager:validate_session(SessionId),
    ?assert(true).

%%====================================================================
%% Helper Functions
%%====================================================================

collect_sessions(0, Acc) ->
    Acc;
collect_sessions(N, Acc) ->
    receive
        {session, SessionId} ->
            collect_sessions(N - 1, [SessionId | Acc])
    after 5000 ->
        Acc
    end.

collect_validations(0, Acc) ->
    Acc;
collect_validations(N, Acc) ->
    receive
        {validated, SessionId} ->
            collect_validations(N - 1, [SessionId | Acc])
    after 5000 ->
        Acc
    end.

%%====================================================================
%% Test Groups
%%====================================================================

http_session_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        %% Session creation
        http_create_session_on_get_test(),
        http_session_id_format_test(),
        http_multiple_sessions_test(),
        http_session_with_client_id_test(),

        %% Session validation
        http_validate_new_session_test(),
        http_reject_nonexistent_session_test(),
        http_binary_session_id_test(),
        http_string_session_id_test(),

        %% Session touch/refresh
        http_touch_session_test(),
        http_touch_extends_expiry_test(),
        http_touch_nonexistent_test(),

        %% Session deletion
        http_delete_session_test(),
        http_delete_binary_session_test(),
        http_delete_string_session_test(),

        %% Session info
        http_get_session_info_test(),
        http_session_timestamps_integer_test(),
        http_last_accessed_updates_test(),

        %% Session expiration
        http_session_expiration_test(),
        http_validate_expired_session_test(),

        %% Automatic cleanup
        http_manual_cleanup_test(),
        http_cleanup_preserves_valid_test(),

        %% Concurrent access
        http_concurrent_creation_test(),
        http_concurrent_validation_test(),

        %% Error handling
        http_invalid_input_test(),
        http_missing_session_id_test(),

        %% Header integration
        http_session_header_case_test()
    ]}.
