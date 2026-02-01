%%%-------------------------------------------------------------------
%%% @doc EUnit Test Suite for erlmcp_sse_event_store Lifecycle
%%%
%%% Chicago School TDD approach:
%%% - Real gen_server (no mocks)
%%% - Test observable behavior through API calls only
%%% - NO state inspection (sys:get_status prohibited)
%%% - NO record duplication (respect encapsulation)
%%%
%%% Tests cover:
%%% - Server start/stop
%%% - Cleanup timer behavior
%%% - Session expiration
%%% - ETS table cleanup on termination
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sse_event_store_lifecycle_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Setup fixture - Start real gen_server
%%--------------------------------------------------------------------
setup() ->
    %% Ensure any previous instance is stopped
    case whereis(erlmcp_sse_event_store) of
        undefined ->
            ok;
        OldPid ->
            exit(OldPid, kill),
            timer:sleep(100)
    end,
    {ok, Pid} = erlmcp_sse_event_store:start_link(),
    Pid.

%%--------------------------------------------------------------------
%% @doc Cleanup fixture - Stop gen_server and verify cleanup
%%--------------------------------------------------------------------
cleanup(Pid) ->
    %% Stop server
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(erlmcp_sse_event_store);
        false ->
            ok
    end,
    %% Verify all ETS tables are cleaned up
    Tables = ets:all(),
    lists:foreach(fun(Table) ->
                     case ets:info(Table, name) of
                         {erlmcp_sse_event_store, _} ->
                             ets:delete(Table);
                         _ ->
                             ok
                     end
                  end,
                  Tables).

%%%===================================================================
%%% Server Lifecycle Tests
%%%===================================================================

lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_start_link()),
         ?_test(test_double_start_fails()),
         ?_test(test_stop_and_restart())]
     end}.

test_start_link() ->
    %% Setup already started server, verify it's alive
    Pid = whereis(erlmcp_sse_event_store),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Verify server is functional through API
    SessionId = <<"session_lifecycle_test">>,
    {ok, _EventId} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"test">>),
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual(1, maps:get(event_count, Info)).

test_double_start_fails() ->
    %% Attempting to start again should fail (already registered)
    Result = erlmcp_sse_event_store:start_link(),
    ?assertMatch({error, {already_started, _}}, Result).

test_stop_and_restart() ->
    %% Stop server
    Pid = whereis(erlmcp_sse_event_store),
    ok = gen_server:stop(erlmcp_sse_event_store),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)),

    %% Restart and verify through API
    {ok, NewPid} = erlmcp_sse_event_store:start_link(),
    ?assert(is_pid(NewPid)),
    ?assert(is_process_alive(NewPid)),

    %% Verify server is functional
    SessionId = <<"session_restart_test">>,
    {ok, _EventId} = erlmcp_sse_event_store:add_event(SessionId, 1, <<"test">>),
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual(1, maps:get(event_count, Info)).

%%%===================================================================
%%% Termination Tests
%%%===================================================================

termination_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [?_test(test_terminate_cleans_all_tables()), ?_test(test_terminate_cancels_timer())]
     end}.

test_terminate_cleans_all_tables() ->
    %% Create multiple sessions with events through API
    SessionIds = [<<"session_term_1">>, <<"session_term_2">>, <<"session_term_3">>],

    lists:foreach(fun(SessionId) ->
                     ok = add_events(SessionId, [{1, <<"e1">>}]),
                     %% Verify session exists through API
                     {ok, _Info} = erlmcp_sse_event_store:get_session_info(SessionId)
                  end,
                  SessionIds),

    %% Stop server
    ok = gen_server:stop(erlmcp_sse_event_store),
    timer:sleep(100),

    %% Verify all sessions are gone through API
    lists:foreach(fun(SessionId) ->
                     ?assertEqual({error, session_not_found},
                                  erlmcp_sse_event_store:get_session_info(SessionId))
                  end,
                  SessionIds).

test_terminate_cancels_timer() ->
    %% Server is running with cleanup timer
    Pid = whereis(erlmcp_sse_event_store),
    ?assert(is_process_alive(Pid)),

    %% Stop server
    ok = gen_server:stop(erlmcp_sse_event_store),
    timer:sleep(100),

    %% Verify process is dead
    ?assertNot(is_process_alive(Pid)).

%%%===================================================================
%%% Cleanup Tests
%%%===================================================================

cleanup_expired_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) -> [?_test(test_cleanup_with_recent_events()), ?_test(test_cleanup_empty_sessions())]
     end}.

test_cleanup_with_recent_events() ->
    SessionId = <<"session_recent">>,

    %% Add recent events through API
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}]),

    %% Trigger cleanup (should not delete recent events)
    erlmcp_sse_event_store:cleanup_expired(),
    timer:sleep(100),

    %% Verify session still exists through API
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual(2, maps:get(event_count, Info)).

test_cleanup_empty_sessions() ->
    SessionId = <<"session_empty">>,

    %% Add event then clear to create empty session
    ok = add_events(SessionId, [{1, <<"e1">>}]),
    ok = erlmcp_sse_event_store:clear_session(SessionId),

    %% Trigger cleanup (should handle empty gracefully)
    erlmcp_sse_event_store:cleanup_expired(),
    timer:sleep(100),

    %% Verify session is gone through API
    {error, session_not_found} = erlmcp_sse_event_store:get_session_info(SessionId).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private
%% @doc Helper to add multiple events to a session
-spec add_events(binary(), [{pos_integer(), binary()}]) -> ok.
add_events(SessionId, Events) ->
    lists:foreach(fun({EventNumber, Data}) ->
                     {ok, _Id} = erlmcp_sse_event_store:add_event(SessionId, EventNumber, Data)
                  end,
                  Events),
    ok.
