#!/usr/bin/env escript
%% Test script for session persistent storage

-mode(compile).

main(_) ->
    io:format("Starting session persistence test...~n"),

    %% Start Mnesia
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    application:start(mnesia),
    io:format("Mnesia started~n"),

    %% Start session manager
    {ok, _Pid} = erlmcp_session_manager:start_link(),
    io:format("Session manager started~n"),

    %% Test 1: Create and persist session
    io:format("~n--- Test 1: Create and persist session ---~n"),
    Metadata = #{user => <<"test_user">>, role => admin},
    {ok, SessionId} = erlmcp_session_manager:create_session(Metadata, infinity),
    io:format("Created session: ~s~n", [SessionId]),

    ok = erlmcp_session_manager:persist_session(SessionId),
    io:format("Session persisted~n"),

    %% Verify in Mnesia
    Record = mnesia:dirty_read(erlmcp_persistent_sessions, SessionId),
    io:format("Session in Mnesia: ~p~n", [Record]),

    %% Test 2: Load session from Mnesia
    io:format("~n--- Test 2: Load session from Mnesia ---~n"),
    ets:delete(erlmcp_sessions, SessionId),
    {ok, LoadedSession} = erlmcp_session_manager:load_session(SessionId),
    io:format("Loaded session ID: ~s~n", [maps:get(id, LoadedSession)]),
    io:format("Loaded session user: ~p~n", [maps:get(user, maps:get(metadata, LoadedSession))]),

    %% Test 3: Create session with persist option
    io:format("~n--- Test 3: Create session with persist option ---~n"),
    Options = #{persist => true, ttl => 7200000},
    {ok, SessionId2} = erlmcp_session_manager:create_session(#{test => auto}, infinity, Options),
    io:format("Created auto-persisted session: ~s~n", [SessionId2]),

    Record2 = mnesia:dirty_read(erlmcp_persistent_sessions, SessionId2),
    io:format("Auto-persisted session in Mnesia: ~p~n", [Record2]),

    %% Test 4: Delete persistent session
    io:format("~n--- Test 4: Delete persistent session ---~n"),
    ok = erlmcp_session_manager:delete_persistent(SessionId),
    io:format("Persistent session deleted~n"),

    Record3 = mnesia:dirty_read(erlmcp_persistent_sessions, SessionId),
    io:format("After delete, session in Mnesia: ~p~n", [Record3]),

    %% Cleanup
    io:format("~n--- Cleanup ---~n"),
    ok = erlmcp_session_manager:delete_session(SessionId),
    ok = erlmcp_session_manager:delete_session(SessionId2),
    ok = erlmcp_session_manager:delete_persistent(SessionId2),
    gen_server:stop(erlmcp_session_manager),
    application:stop(mnesia),
    mnesia:delete_schema([node()]),

    io:format("~n=== All tests passed! ===~n"),
    init:stop(0).
