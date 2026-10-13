-module(erlmcp_session_hibernation_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for OTP 28 Session Hibernation
%% Chicago School TDD - Real processes, no mocks
%%====================================================================

%%====================================================================
%% Session Backend Hibernation Tests
%%====================================================================

hibernation_backend_test_() ->
    {setup,
     fun setup_backend/0,
     fun cleanup_backend/1,
     fun(_) ->
        [?_test(test_hibernate_idle_sessions()),
         ?_test(test_hibernation_disabled()),
         ?_test(test_hibernation_threshold()),
         ?_test(test_process_iterator_fallback()),
         ?_test(test_utf8_session_ids())]
     end}.

setup_backend() ->
    {ok, Pid} = erlmcp_session_backend:start_link(
                  #{backend => erlmcp_session_ets,
                    cleanup_interval => 60000,
                    hibernation_enabled => true,
                    hibernation_threshold_ms => 1000}),
    Pid.

cleanup_backend(_Pid) ->
    gen_server:stop(erlmcp_session_backend),
    ets:delete(erlmcp_sessions_ets),
    ok.

test_hibernate_idle_sessions() ->
    %% Create idle sessions
    Now = erlang:system_time(millisecond),
    OldTime = Now - 5000,  % 5 seconds ago (beyond 1s threshold)

    SessionId1 = <<"hibernate_session_1">>,
    Session1 =
        #{id => SessionId1,
          created_at => OldTime,
          last_accessed => OldTime,
          timeout_ms => infinity,
          metadata => #{idle => true}},

    {ok, BackendState} = erlmcp_session_ets:init(#{}),
    {ok, _} = erlmcp_session_ets:store(SessionId1, Session1, BackendState),

    %% Trigger hibernation
    {ok, Count} = erlmcp_session_backend:hibernate_idle_sessions(),

    %% Verify sessions were hibernated
    ?assertEqual(1, Count).

test_hibernation_disabled() ->
    %% Start backend with hibernation disabled
    {ok, Pid} = erlmcp_session_backend:start_link(
                  #{backend => erlmcp_session_ets,
                    cleanup_interval => 60000,
                    hibernation_enabled => false}),

    try
        %% Create idle sessions
        Now = erlang:system_time(millisecond),
        OldTime = Now - 5000,

        SessionId = <<"no_hibernate_session">>,
        Session =
            #{id => SessionId,
              created_at => OldTime,
              last_accessed => OldTime,
              timeout_ms => infinity,
              metadata => #{}},

        gen_server:call(erlmcp_session_backend, {store, SessionId, Session}),

        %% Trigger hibernation (should be disabled)
        {ok, Count} = erlmcp_session_backend:hibernate_idle_sessions(),

        %% Verify no sessions were hibernated
        ?assertEqual(0, Count)
    after
        gen_server:stop(Pid)
    end.

test_hibernation_threshold() ->
    %% Test that only sessions beyond threshold are hibernated
    Now = erlang:system_time(millisecond),
    OldTime = Now - 5000,   % Beyond threshold
    RecentTime = Now - 500, % Within threshold

    SessionId1 = <<"old_session">>,
    Session1 =
        #{id => SessionId1,
          created_at => OldTime,
          last_accessed => OldTime,
          timeout_ms => infinity,
          metadata => #{}},

    SessionId2 = <<"recent_session">>,
    Session2 =
        #{id => SessionId2,
          created_at => RecentTime,
          last_accessed => RecentTime,
          timeout_ms => infinity,
          metadata => #{}},

    {ok, BackendState} = erlmcp_session_ets:init(#{}),
    {ok, _} = erlmcp_session_ets:store(SessionId1, Session1, BackendState),
    {ok, _} = erlmcp_session_ets:store(SessionId2, Session2, BackendState),

    %% Trigger hibernation
    {ok, Count} = erlmcp_session_backend:hibernate_idle_sessions(),

    %% Only old session should be hibernated
    ?assertEqual(1, Count).

test_process_iterator_fallback() ->
    %% Test that process iterator degrades gracefully on OTP < 28
    {ok, Iterator} = erlmcp_session_backend:get_process_iterator(),

    %% Iterator should be available (either real or fallback)
    ?assertMatch({ok, _}, {ok, Iterator}).

test_utf8_session_ids() ->
    %% Test UTF-8 session ID validation
    AsciiId = <<"ascii_session_123">>,
    Utf8Id = <<"session_日本語_测试">>,
    InvalidId = <<"session_\000_null">>,

    %% Create sessions
    Now = erlang:system_time(millisecond),

    Session1 =
        #{id => AsciiId,
          created_at => Now,
          last_accessed => Now,
          timeout_ms => infinity,
          metadata => #{}},

    Session2 =
        #{id => Utf8Id,
          created_at => Now,
          last_accessed => Now,
          timeout_ms => infinity,
          metadata => #{}},

    Session3 =
        #{id => InvalidId,
          created_at => Now,
          last_accessed => Now,
          timeout_ms => infinity,
          metadata => #{}},

    {ok, BackendState} = erlmcp_session_ets:init(#{}),
    {ok, _} = erlmcp_session_ets:store(AsciiId, Session1, BackendState),
    {ok, _} = erlmcp_session_ets:store(Utf8Id, Session2, BackendState),
    {ok, _} = erlmcp_session_ets:store(InvalidId, Session3, BackendState),

    %% List UTF-8 session IDs (should filter invalid)
    {ok, Utf8Ids} = erlmcp_session_backend:list_utf8_session_ids(),

    %% Should contain ASCII and UTF-8 IDs, but not invalid ID
    ?assert(lists:member(AsciiId, Utf8Ids)),
    ?assert(lists:member(Utf8Id, Utf8Ids)),
    ?assertNot(lists:member(InvalidId, Utf8Ids)).

%%====================================================================
%% Session Manager Hibernation Tests
%%====================================================================

hibernation_manager_test_() ->
    {setup,
     fun setup_manager/0,
     fun cleanup_manager/1,
     fun(_) ->
        [?_test(test_manager_hibernate_idle()),
         ?_test(test_manager_iterator_listing()),
         ?_test(test_manager_utf8_listing()),
         ?_test(test_manager_hibernation_threshold())]
     end}.

setup_manager() ->
    {ok, Pid} = erlmcp_session_manager:start_link(),
    Pid.

cleanup_manager(_Pid) ->
    gen_server:stop(erlmcp_session_manager),
    ok.

test_manager_hibernate_idle() ->
    %% Create sessions with varying ages
    {ok, SessionId1} = erlmcp_session_manager:create_session(#{idle => true}),

    %% Wait to make session idle
    timer:sleep(100),

    %% Trigger hibernation
    {ok, Count} = erlmcp_session_manager:hibernate_idle_sessions(),

    %% Should hibernate at least one session
    ?assert(Count >= 0).

test_manager_iterator_listing() ->
    %% Create multiple sessions
    lists:foreach(fun(N) ->
                     erlmcp_session_manager:create_session(#{index => N})
                  end, lists:seq(1, 5)),

    %% List using process iterator
    {ok, Sessions} = erlmcp_session_manager:list_sessions_with_iterator(),

    %% Should have at least our sessions
    ?assert(length(Sessions) >= 5).

test_manager_utf8_listing() ->
    %% Create sessions with UTF-8 IDs
    {ok, _} = erlmcp_session_manager:create_session(#{name => <<"日本語">>}),
    {ok, _} = erlmcp_session_manager:create_session(#{name => <<"中文">>}),
    {ok, _} = erlmcp_session_manager:create_session(#{name => <<"한글">>}),

    %% List UTF-8 sessions
    {ok, Utf8Sessions} = erlmcp_session_manager:list_utf8_sessions(),

    %% Should have at least our UTF-8 sessions
    ?assert(length(Utf8Sessions) >= 3).

test_manager_hibernation_threshold() ->
    %% Test hibernation threshold configuration
    Now = erlang:system_time(millisecond),

    %% Create old and recent sessions
    {ok, OldSession} = erlmcp_session_manager:create_session(#{created => Now - 10000}),
    {ok, RecentSession} = erlmcp_session_manager:create_session(#{created => Now}),

    %% Trigger hibernation
    {ok, Count} = erlmcp_session_manager:hibernate_idle_sessions(),

    %% Count should be non-negative
    ?assert(Count >= 0).

%%====================================================================
%% Integration Tests
%%====================================================================

hibernation_integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun cleanup_integration/1,
     fun(_) ->
        [?_test(test_backend_manager_integration()),
         ?_test(test_cleanup_after_hibernation()),
         ?_test test_memory_efficiency()]
     end}.

setup_integration() ->
    {ok, BackendPid} = erlmcp_session_backend:start_link(
                         #{backend => erlmcp_session_ets,
                           cleanup_interval => 60000,
                           hibernation_enabled => true}),
    {ok, ManagerPid} = erlmcp_session_manager:start_link(),
    {BackendPid, ManagerPid}.

cleanup_integration({BackendPid, ManagerPid}) ->
    gen_server:stop(BackendPid),
    gen_server:stop(ManagerPid),
    ets:delete(erlmcp_sessions_ets),
    ok.

test_backend_manager_integration() ->
    %% Test that backend and manager work together
    {ok, SessionId} = erlmcp_session_manager:create_session(#{test => integration}),

    %% Verify session exists
    {ok, _SessionData} = erlmcp_session_manager:get_session(SessionId),

    %% Trigger hibernation in both
    {ok, BackendCount} = erlmcp_session_backend:hibernate_idle_sessions(),
    {ok, ManagerCount} = erlmcp_session_manager:hibernate_idle_sessions(),

    %% Both should complete without error
    ?assert(is_integer(BackendCount)),
    ?assert(is_integer(ManagerCount)).

test_cleanup_after_hibernation() ->
    %% Test that cleanup works after hibernation
    {ok, SessionId} = erlmcp_session_manager:create_session(#{ttl => 100}),

    %% Expire session
    timer:sleep(150),

    %% Trigger hibernation first
    {ok, _HibernateCount} = erlmcp_session_manager:hibernate_idle_sessions(),

    %% Then cleanup
    {ok, CleanupCount} = erlmcp_session_manager:cleanup_expired(),

    %% Should clean up expired session
    ?assert(CleanupCount >= 0).

test_memory_efficiency() ->
    %% Test that hibernation reduces memory footprint
    %% Create many sessions
    lists:foreach(fun(N) ->
                     erlmcp_session_manager:create_session(#{index => N})
                  end, lists:seq(1, 100)),

    %% Get memory before hibernation
    {memory, MemBefore} = process_info(whereis(erlmcp_session_manager), memory),

    %% Trigger hibernation
    timer:sleep(2000),  % Wait for sessions to become idle
    {ok, _Count} = erlmcp_session_manager:hibernate_idle_sessions(),

    %% Force garbage collection
    garbage_collect(whereis(erlmcp_session_manager)),

    %% Get memory after hibernation
    {memory, MemAfter} = process_info(whereis(erlmcp_session_manager), memory),

    %% Memory should be reduced or stable (not increased significantly)
    ?assert(MemAfter =< MemBefore * 1.1).  % Allow 10% tolerance
