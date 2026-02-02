%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Session Test Suite (EUnit)
%%%
%%% Tests for erlmcp_cli_session module - Session lifecycle and management
%%%
%%% Chicago School TDD:
%%% - Tests FIRST, real gen_server for session state
%%% - NO mocks, real session processes
%%% - State-based verification (session state, transitions)
%%%
%%% Coverage Target: ≥90%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_session_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

session_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Session creation - create new session", fun test_create_new_session/0},
      {"Session creation - create with options", fun test_create_with_options/0},
      {"Session creation - create duplicate session ID", fun test_create_duplicate_session/0},
      {"Session start - start session", fun test_start_session/0},
      {"Session start - start already started session", fun test_start_already_started/0},
      {"Session stop - stop session", fun test_stop_session/0},
      {"Session stop - stop stopped session", fun test_stop_stopped_session/0},
      {"Session terminate - terminate session", fun test_terminate_session/0},
      {"Session state - initial state", fun test_initial_state/0},
      {"Session state - state transitions", fun test_state_transitions/0},
      {"Session timeout - session timeout", fun test_session_timeout/0},
      {"Session timeout - refresh timeout", fun test_refresh_timeout/0},
      {"Session authentication - authenticate session", fun test_authenticate_session/0},
      {"Session authentication - invalid credentials", fun test_invalid_credentials/0},
      {"Concurrent sessions - multiple concurrent sessions", fun test_concurrent_sessions/0},
      {"Session memory - memory cleanup", fun test_memory_cleanup/0},
      {"Session persistence - save session state", fun test_save_session_state/0},
      {"Session persistence - restore session state", fun test_restore_session_state/0},
      {"Multi-client - multiple clients same session", fun test_multiple_clients_same_session/0},
      {"Multi-client - session sharing", fun test_session_sharing/0}]}.

setup() ->
    %% Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(erlmcp_cli),
    ok.

cleanup(_Args) ->
    %% Clean up any remaining sessions
    case whereis(erlmcp_cli_session_sup) of
        undefined -> ok;
        _Pid ->
            %% Stop all sessions
            try
                erlmcp_cli_session:stop_all_sessions()
            catch
                _:_ -> ok
            end
    end,
    ok.

%%%====================================================================
%%% Session Creation Tests
%%%====================================================================

test_create_new_session() ->
    %% Create new session
    SessionId = <<"test-session-1">>,
    {ok, SessionPid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Verify session process created
    ?assert(is_pid(SessionPid)),
    ?assert(is_process_alive(SessionPid)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_create_with_options() ->
    %% Create session with options
    SessionId = <<"test-session-2">>,
    Options = #{
        timeout => 60000,
        auth_required => true,
        metadata => #{<<"client">> => <<"test">>}
    },

    {ok, SessionPid} = erlmcp_cli_session:create_session(SessionId, Options),

    %% Verify session created with options
    ?assert(is_pid(SessionPid)),

    %% Get session state to verify options
    {ok, State} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(60000, maps:get(timeout, State)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_create_duplicate_session() ->
    %% Create session
    SessionId = <<"test-session-dup">>,
    {ok, _Pid1} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Attempt to create duplicate
    {error, session_already_exists} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Session Start/Stop Tests
%%%====================================================================

test_start_session() ->
    %% Create and start session
    SessionId = <<"test-session-start">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    ok = erlmcp_cli_session:start_session(SessionId),

    %% Verify session started
    {ok, State} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(started, maps:get(status, State)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_start_already_started() ->
    %% Create and start session
    SessionId = <<"test-session-start-2">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Attempt to start again
    {error, already_started} = erlmcp_cli_session:start_session(SessionId),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_stop_session() ->
    %% Create and start session
    SessionId = <<"test-session-stop">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Stop session
    ok = erlmcp_cli_session:stop_session(SessionId),

    %% Verify session stopped
    {ok, State} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(stopped, maps:get(status, State)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_stop_stopped_session() ->
    %% Create and stop session
    SessionId = <<"test-session-stop-2">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Stop without starting (already in stopped state)
    ok = erlmcp_cli_session:stop_session(SessionId),

    %% Attempt to stop again
    {error, already_stopped} = erlmcp_cli_session:stop_session(SessionId),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_terminate_session() ->
    %% Create session
    SessionId = <<"test-session-terminate">>,
    {ok, Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Terminate session
    ok = erlmcp_cli_session:terminate_session(SessionId),

    %% Verify process terminated
    timer:sleep(50),
    ?assertNot(is_process_alive(Pid)).

%%%====================================================================
%%% Session State Tests
%%%====================================================================

test_initial_state() ->
    %% Create session
    SessionId = <<"test-session-state">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Get initial state
    {ok, State} = erlmcp_cli_session:get_state(SessionId),

    %% Verify initial state
    ?assertEqual(SessionId, maps:get(session_id, State)),
    ?assertEqual(initialized, maps:get(status, State)),
    ?assert(is_map(maps:get(metadata, State))),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_state_transitions() ->
    %% Create session
    SessionId = <<"test-session-transitions">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),

    %% Verify initial state
    {ok, State1} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(initialized, maps:get(status, State1)),

    %% Start session
    ok = erlmcp_cli_session:start_session(SessionId),
    {ok, State2} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(started, maps:get(status, State2)),

    %% Stop session
    ok = erlmcp_cli_session:stop_session(SessionId),
    {ok, State3} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(stopped, maps:get(status, State3)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Session Timeout Tests
%%%====================================================================

test_session_timeout() ->
    %% Create session with short timeout
    SessionId = <<"test-session-timeout">>,
    Options = #{timeout => 100},  %% 100ms timeout
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, Options),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Wait for timeout
    timer:sleep(150),

    %% Verify session expired
    {ok, State} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(expired, maps:get(status, State)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_refresh_timeout() ->
    %% Create session with timeout
    SessionId = <<"test-session-refresh">>,
    Options = #{timeout => 1000},
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, Options),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Wait half the timeout
    timer:sleep(500),

    %% Refresh timeout
    ok = erlmcp_cli_session:refresh_timeout(SessionId),

    %% Wait for original timeout to pass
    timer:sleep(600),

    %% Verify session still alive (refresh worked)
    {ok, State} = erlmcp_cli_session:get_state(SessionId),
    ?assertNotEqual(expired, maps:get(status, State)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Session Authentication Tests
%%%====================================================================

test_authenticate_session() ->
    %% Create session requiring auth
    SessionId = <<"test-session-auth">>,
    Options = #{auth_required => true},
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, Options),

    %% Authenticate with valid credentials
    Credentials = #{<<"api_key">> => <<"test-key">>},
    ok = erlmcp_cli_session:authenticate(SessionId, Credentials),

    %% Verify authenticated
    {ok, State} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(true, maps:get(authenticated, State)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_invalid_credentials() ->
    %% Create session requiring auth
    SessionId = <<"test-session-invalid-auth">>,
    Options = #{auth_required => true},
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, Options),

    %% Authenticate with invalid credentials
    Credentials = #{<<"api_key">> => <<"invalid-key">>},
    {error, authentication_failed} = erlmcp_cli_session:authenticate(SessionId, Credentials),

    %% Verify not authenticated
    {ok, State} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(false, maps:get(authenticated, State, false)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Concurrent Sessions Tests
%%%====================================================================

test_concurrent_sessions() ->
    %% Create multiple concurrent sessions
    NumSessions = 10,
    SessionIds = [<<"test-session-concurrent-", (integer_to_binary(N))/binary>> || N <- lists:seq(1, NumSessions)],

    %% Create all sessions
    Pids = [begin
        {ok, Pid} = erlmcp_cli_session:create_session(Id, #{}),
        Pid
    end || Id <- SessionIds],

    %% Verify all sessions created
    ?assertEqual(NumSessions, length(Pids)),
    lists:foreach(fun(Pid) -> ?assert(is_process_alive(Pid)) end, Pids),

    %% Start all sessions
    lists:foreach(fun(Id) -> ok = erlmcp_cli_session:start_session(Id) end, SessionIds),

    %% Verify all started
    States = [begin
        {ok, State} = erlmcp_cli_session:get_state(Id),
        State
    end || Id <- SessionIds],

    lists:foreach(fun(State) ->
        ?assertEqual(started, maps:get(status, State))
    end, States),

    %% Cleanup all sessions
    lists:foreach(fun(Id) -> ok = erlmcp_cli_session:terminate_session(Id) end, SessionIds).

%%%====================================================================
%%% Session Memory Tests
%%%====================================================================

test_memory_cleanup() ->
    %% Create session with data
    SessionId = <<"test-session-memory">>,
    Options = #{metadata => #{<<"data">> => <<"large data">>}},
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, Options),

    %% Add data to session
    ok = erlmcp_cli_session:set_data(SessionId, <<"key1">>, <<"value1">>),
    ok = erlmcp_cli_session:set_data(SessionId, <<"key2">>, <<"value2">>),

    %% Verify data present
    {ok, Data} = erlmcp_cli_session:get_data(SessionId),
    ?assertEqual(<<"value1">>, maps:get(<<"key1">>, Data)),
    ?assertEqual(<<"value2">>, maps:get(<<"key2">>, Data)),

    %% Terminate session
    ok = erlmcp_cli_session:terminate_session(SessionId),

    %% Verify memory cleaned up (process dictionary empty)
    timer:sleep(50),
    ?assertEqual(undefined, whereis(list_to_existing_atom("session_" ++ binary_to_list(SessionId)))).

%%%====================================================================
%%% Session Persistence Tests
%%%====================================================================

test_save_session_state() ->
    %% Create session
    SessionId = <<"test-session-save">>,
    Options = #{metadata => #{<<"client">> => <<"test">>}},
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, Options),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Add session data
    ok = erlmcp_cli_session:set_data(SessionId, <<"key1">>, <<"value1">>),

    %% Save session state
    {ok, SavedState} = erlmcp_cli_session:save_session_state(SessionId),

    %% Verify saved state
    ?assertEqual(SessionId, maps:get(session_id, SavedState)),
    ?assertEqual(started, maps:get(status, SavedState)),
    ?assert(is_map(maps:get(data, SavedState))),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_restore_session_state() ->
    %% Create and save session
    SessionId = <<"test-session-restore">>,
    Options = #{metadata => #{<<"client">> => <<"test">>}},
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, Options),
    ok = erlmcp_cli_session:start_session(SessionId),
    ok = erlmcp_cli_session:set_data(SessionId, <<"key1">>, <<"value1">>),

    {ok, SavedState} = erlmcp_cli_session:save_session_state(SessionId),

    %% Terminate original session
    ok = erlmcp_cli_session:terminate_session(SessionId),

    %% Restore session from saved state
    {ok, _RestoredPid} = erlmcp_cli_session:restore_session_state(SessionId, SavedState),

    %% Verify restored session
    {ok, RestoredState} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(started, maps:get(status, RestoredState)),
    {ok, RestoredData} = erlmcp_cli_session:get_data(SessionId),
    ?assertEqual(<<"value1">>, maps:get(<<"key1">>, RestoredData)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%%%====================================================================
%%% Multi-Client Tests
%%%====================================================================

test_multiple_clients_same_session() ->
    %% Create session
    SessionId = <<"test-session-multiclient">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Register multiple clients to same session
    Client1 = self(),
    Client2 = spawn(fun() -> receive stop -> ok end end),

    ok = erlmcp_cli_session:add_client(SessionId, Client1),
    ok = erlmcp_cli_session:add_client(SessionId, Client2),

    %% Verify both clients registered
    {ok, Clients} = erlmcp_cli_session:get_clients(SessionId),
    ?assertEqual(2, length(Clients)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

test_session_sharing() ->
    %% Create session
    SessionId = <<"test-session-sharing">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Set shared data
    ok = erlmcp_cli_session:set_data(SessionId, <<"shared">>, <<"value">>),

    %% Spawn multiple processes accessing same session
    Pids = [spawn(fun() ->
        {ok, Data} = erlmcp_cli_session:get_data(SessionId),
        self() ! {data, Data}
    end) || _ <- lists:seq(1, 5)],

    %% Wait for all processes
    Results = [receive
        {data, Data} -> Data
    after 1000 ->
        ct:fail("Session sharing timeout")
    end || _ <- Pids],

    %% Verify all processes see same data
    lists:foreach(fun(Data) ->
        ?assertEqual(<<"value">>, maps:get(<<"shared">>, Data))
    end, Results),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).
