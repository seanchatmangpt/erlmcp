-module(erlmcp_session_manager_basic_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Basic Session Lifecycle Tests
%% Chicago School TDD - Real processes, no state inspection, API testing
%%====================================================================

%%--------------------------------------------------------------------
%% Test Setup
%%--------------------------------------------------------------------

session_manager_basic_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [%% Session Creation (4 tests)
      fun test_create_session/1,
      fun test_create_session_with_timeout/1,
      fun test_session_id_format/1,
      fun test_session_id_uniqueness/1,
      %% Session Retrieval (2 tests)
      fun test_get_session/1,
      fun test_get_nonexistent_session/1,
      %% Session Deletion (3 tests)
      fun test_delete_session/1,
      fun test_delete_nonexistent_session/1,
      fun test_delete_idempotent/1,
      %% Session List (2 tests)
      fun test_list_sessions/1,
      fun test_list_sessions_empty/1]}.

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

%%--------------------------------------------------------------------
%% Session Creation Tests
%%--------------------------------------------------------------------

test_create_session(_Pid) ->
    fun() ->
       Metadata = #{user => <<"alice">>, project => <<"test">>},
       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

       ?assert(is_binary(SessionId)),
       ?assertEqual(32, byte_size(SessionId)),

       %% Verify session exists through API
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

test_session_id_uniqueness(_Pid) ->
    fun() ->
       %% Create many sessions
       SessionIds =
           [begin
                {ok, Id} = erlmcp_session_manager:create_session(#{index => N}),
                Id
            end
            || N <- lists:seq(1, 50)],

       %% All IDs should be unique
       UniqueIds = lists:usort(SessionIds),
       ?assertEqual(length(SessionIds), length(UniqueIds)),

       %% All IDs should be 32 hex characters
       lists:foreach(fun(Id) -> ?assertEqual(32, byte_size(Id)) end, SessionIds)
    end.

%%--------------------------------------------------------------------
%% Session Retrieval Tests
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% Session Deletion Tests
%%--------------------------------------------------------------------

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

%%--------------------------------------------------------------------
%% Session List Tests
%%--------------------------------------------------------------------

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

test_list_sessions_empty(_Pid) ->
    fun() ->
       %% No sessions created
       Sessions = erlmcp_session_manager:list_sessions(),
       ?assertEqual(0, length(Sessions))
    end.
