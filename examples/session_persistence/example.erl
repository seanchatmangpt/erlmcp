#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc Session Persistence Integration Example
%%%
%%% Demonstrates DETS backend for session persistence:
%%% 1. Configure DETS backend in sys.config
%%% 2. Create session with metadata
%%% 3. Stop and restart server
%%% 4. Restore session from disk
%%%
%%% Prerequisites:
%%% - erlmcp_core application started
%%% - DETS backend configured
%%%
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).

-define(SESSION_FILE, "examples/session_persistence/sessions.DETS").

main(_) ->
    io:format("~n=== Session Persistence Integration Example ===~n~n"),

    %% Clean up any existing session file
    file:delete(?SESSION_FILE),

    %% Step 1: Configure DETS backend
    io:format("Step 1: Configuring DETS backend...~n"),
    application:set_env(erlmcp_core, session_backend, dets),
    application:set_env(erlmcp_core, session_file, ?SESSION_FILE),
    io:format("✓ DETS backend configured: ~s~n~n", [?SESSION_FILE]),

    %% Step 2: Start applications
    io:format("Step 2: Starting erlmcp applications...~n"),
    {ok, _} = application:ensure_all_started(erlmcp_core),
    io:format("✓ Applications started~n~n"),

    %% Step 3: Create sessions with metadata
    io:format("Step 3: Creating sessions with metadata...~n"),
    Session1Data = #{
        user_id => <<"user123">>,
        auth_method => api_key,
        connection_info => #{
            ip => <<"127.0.0.1">>,
            transport => stdio
        },
        preferences => #{
            theme => dark,
            language => <<"en">>
        }
    },

    {ok, SessionId1} = erlmcp_session:create(Session1Data, 3600),
    io:format("✓ Created session 1: ~s~n", [SessionId1]),

    Session2Data = #{
        user_id => <<"user456">>,
        auth_method => jwt,
        connection_info => #{
            ip => <<"192.168.1.100">>,
            transport => tcp
        },
        preferences => #{
            theme => light,
            language => <<"es">>
        }
    },

    {ok, SessionId2} = erlmcp_session:create(Session2Data, 7200),
    io:format("✓ Created session 2: ~s~n", [SessionId2]),
    io:format("✓ Total sessions: ~p~n~n", [length(erlmcp_session:list_sessions())]),

    %% Step 4: Retrieve and update sessions
    io:format("Step 4: Retrieving and updating sessions...~n"),
    {ok, Session1} = erlmcp_session:retrieve(SessionId1),
    io:format("✓ Retrieved session 1: ~p~n", [maps:get(user_id, Session1)]),

    %% Update session metadata
    ok = erlmcp_session:update(SessionId1, fun(S) ->
        maps:put(last_activity, erlang:system_time(second), S)
    end),
    io:format("✓ Updated session 1 with last_activity~n~n"),

    %% Step 5: Verify DETS file was created
    io:format("Step 5: Verifying DETS file...~n"),
    case file:read_file_info(?SESSION_FILE) of
        {ok, FileInfo} ->
            Size = FileInfo#file_info.size,
            io:format("✓ DETS file created: ~s (~p bytes)~n~n", [?SESSION_FILE, Size]);
        {error, Reason} ->
            io:format("✗ Failed to read DETS file: ~p~n~n", [Reason])
    end,

    %% Step 6: Stop applications (simulate shutdown)
    io:format("Step 6: Stopping applications (simulating shutdown)...~n"),
    application:stop(erlmcp_core),
    io:format("✓ Applications stopped~n"),
    io:format("✓ Sessions should be persisted to disk~n~n"),

    %% Step 7: Restart applications
    io:format("Step 7: Restarting applications...~n"),
    application:set_env(erlmcp_core, session_backend, dets),
    application:set_env(erlmcp_core, session_file, ?SESSION_FILE),
    {ok, _} = application:ensure_all_started(erlmcp_core),
    io:format("✓ Applications restarted~n~n"),

    %% Step 8: Restore sessions from disk
    io:format("Step 8: Restoring sessions from disk...~n"),
    RestoredSessions = erlmcp_session:list_sessions(),
    io:format("✓ Restored ~p sessions~n", [length(RestoredSessions)]),

    lists:foreach(fun(Session) ->
        SessionId = maps:get(id, Session),
        UserId = maps:get(metadata, Session, #{}) |> maps:get(user_id, undefined),
        io:format("  - Session: ~s (User: ~s)~n", [SessionId, UserId])
    end, RestoredSessions),
    io:format("~n"),

    %% Step 9: Verify restored session data
    io:format("Step 9: Verifying restored session data...~n"),
    case erlmcp_session:retrieve(SessionId1) of
        {ok, RestoredSession} ->
            Metadata = maps:get(metadata, RestoredSession),
            UserId = maps:get(user_id, Metadata),
            Preferences = maps:get(preferences, Metadata),
            io:format("✓ Session 1 restored successfully~n"),
            io:format("  User ID: ~s~n", [UserId]),
            io:format("  Theme: ~s~n", [maps:get(theme, Preferences)]),
            io:format("  Language: ~s~n", [maps:get(language, Preferences)]);
        {error, Reason} ->
            io:format("✗ Failed to restore session 1: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 10: Demonstrate session TTL (time-to-live)
    io:format("Step 10: Demonstrating session TTL...~n"),
    {ok, ShortLivedSession} = erlmcp_session:create(
        #{user_id => <<"temp_user">>},
        2  % 2 second TTL
    ),
    io:format("✓ Created short-lived session: ~s (TTL: 2s)~n", [ShortLivedSession]),

    timer:sleep(3000),  % Wait for expiry

    %% Check if session expired
    case erlmcp_session:retrieve(ShortLivedSession) of
        {ok, _} ->
            io:format("✗ Session should have expired but still exists~n");
        {error, not_found} ->
            io:format("✓ Session expired and removed from DETS~n")
    end,
    io:format("~n"),

    %% Step 11: Cleanup expired sessions
    io:format("Step 11: Cleaning up expired sessions...~n"),
    {ok, ExpiredCount} = erlmcp_session:cleanup_expired(),
    io:format("✓ Cleaned up ~p expired sessions~n~n", [ExpiredCount]),

    %% Step 12: Delete a session
    io:format("Step 12: Deleting a session...~n"),
    ok = erlmcp_session:delete(SessionId2),
    io:format("✓ Deleted session: ~s~n", [SessionId2]),

    RemainingSessions = erlmcp_session:list_sessions(),
    io:format("✓ Remaining sessions: ~p~n~n", [length(RemainingSessions)]),

    %% Step 13: Check backend info
    io:format("Step 13: Checking backend configuration...~n"),
    {ok, Backend} = erlmcp_session:get_backend(),
    io:format("✓ Current backend: ~p~n~n", [Backend]),

    %% Step 14: Final cleanup
    io:format("Step 14: Final cleanup...~n"),
    application:stop(erlmcp_core),
    file:delete(?SESSION_FILE),
    io:format("✓ Cleanup complete~n~n"),

    io:format("=== Example Complete ===~n"),
    init:stop().
