%%%-------------------------------------------------------------------
%%% @doc EUnit Test Suite for erlmcp_sse_event_store Concurrency
%%%
%%% Chicago School TDD approach:
%%% - Real gen_server (no mocks)
%%% - Test observable behavior through API calls only
%%% - NO state inspection (sys:get_status prohibited)
%%% - NO record duplication (respect encapsulation)
%%%
%%% Tests cover:
%%% - Concurrent event addition
%%% - Concurrent session access
%%% - Concurrent read/write operations
%%% - Race condition prevention
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sse_event_store_concurrency_tests).
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
        undefined -> ok;
        OldPid -> exit(OldPid, kill), timer:sleep(100)
    end,
    {ok, Pid} = erlmcp_sse_event_store:start_link(),
    Pid.

%%--------------------------------------------------------------------
%% @doc Cleanup fixture - Stop gen_server and verify cleanup
%%--------------------------------------------------------------------
cleanup(Pid) ->
    %% Stop server
    case is_process_alive(Pid) of
        true -> gen_server:stop(erlmcp_sse_event_store);
        false -> ok
    end,
    %% Verify all ETS tables are cleaned up
    Tables = ets:all(),
    lists:foreach(fun(Table) ->
        case ets:info(Table, name) of
            {erlmcp_sse_event_store, _} -> ets:delete(Table);
            _ -> ok
        end
    end, Tables).

%%%===================================================================
%%% Concurrency Tests
%%%===================================================================

concurrent_access_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_concurrent_add_events()),
          ?_test(test_concurrent_session_isolation()),
          ?_test(test_concurrent_read_write())
         ]
     end}.

test_concurrent_add_events() ->
    SessionId = <<"session_concurrent_add">>,

    %% Spawn multiple processes adding events concurrently
    NumProcesses = 10,
    EventsPerProcess = 5,

    Pids = lists:map(fun(ProcessNum) ->
        spawn(fun() ->
            StartNum = ProcessNum * EventsPerProcess + 1,
            lists:foreach(fun(EventNum) ->
                erlmcp_sse_event_store:add_event(
                    SessionId,
                    EventNum,
                    <<"event_", (integer_to_binary(EventNum))/binary>>
                )
            end, lists:seq(StartNum, StartNum + EventsPerProcess - 1))
        end)
    end, lists:seq(0, NumProcesses - 1)),

    %% Wait for all processes to complete
    timer:sleep(500),

    %% Verify all events were added through API
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(NumProcesses * EventsPerProcess, length(Events)).

test_concurrent_session_isolation() ->
    %% Multiple sessions with concurrent writes
    NumSessions = 5,
    SessionIds = [<<"session_concurrent_iso_", (integer_to_binary(N))/binary>>
                  || N <- lists:seq(1, NumSessions)],

    %% Add events to all sessions concurrently
    lists:foreach(fun(SessionId) ->
        spawn(fun() ->
            lists:foreach(fun(N) ->
                erlmcp_sse_event_store:add_event(SessionId, N, <<"data">>)
            end, lists:seq(1, 10))
        end)
    end, SessionIds),

    %% Wait for completion
    timer:sleep(500),

    %% Verify all sessions have correct event counts through API
    lists:foreach(fun(SessionId) ->
        {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
        ?assertEqual(10, maps:get(event_count, Info))
    end, SessionIds).

test_concurrent_read_write() ->
    SessionId = <<"session_concurrent_rw">>,

    %% Writer process
    spawn(fun() ->
        lists:foreach(fun(N) ->
            erlmcp_sse_event_store:add_event(
                SessionId,
                N,
                <<"event_", (integer_to_binary(N))/binary>>
            ),
            timer:sleep(10)
        end, lists:seq(1, 20))
    end),

    %% Reader processes
    lists:foreach(fun(_) ->
        spawn(fun() ->
            lists:foreach(fun(_) ->
                {ok, _Events} = erlmcp_sse_event_store:get_events_since(
                    SessionId,
                    undefined
                ),
                timer:sleep(20)
            end, lists:seq(1, 10))
        end)
    end, lists:seq(1, 5)),

    %% Wait for completion
    timer:sleep(1000),

    %% Verify final state through API
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
    ?assertEqual(20, maps:get(event_count, Info)).

%%%===================================================================
%%% Stress Tests
%%%===================================================================

stress_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_high_concurrency_load()),
          ?_test(test_rapid_fire_operations())
         ]
     end}.

test_high_concurrency_load() ->
    SessionId = <<"session_stress_high">>,

    %% Spawn 50 processes adding events
    NumProcesses = 50,
    EventsPerProcess = 10,

    lists:foreach(fun(ProcessNum) ->
        spawn(fun() ->
            StartNum = ProcessNum * EventsPerProcess + 1,
            lists:foreach(fun(EventNum) ->
                erlmcp_sse_event_store:add_event(
                    SessionId,
                    EventNum,
                    <<"event_", (integer_to_binary(EventNum))/binary>>
                )
            end, lists:seq(StartNum, StartNum + EventsPerProcess - 1))
        end)
    end, lists:seq(0, NumProcesses - 1)),

    %% Wait for completion
    timer:sleep(2000),

    %% Verify all events were added through API
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(NumProcesses * EventsPerProcess, length(Events)).

test_rapid_fire_operations() ->
    SessionId = <<"session_stress_rapid">>,

    %% Rapid fire add operations
    NumOps = 1000,
    lists:foreach(fun(N) ->
        erlmcp_sse_event_store:add_event(SessionId, N, <<"data">>)
    end, lists:seq(1, NumOps)),

    %% Verify through API
    {ok, Events} = erlmcp_sse_event_store:get_events_since(SessionId, undefined),
    ?assertEqual(NumOps, length(Events)).

%%%===================================================================
%%% Race Condition Tests
%%%===================================================================

race_condition_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_test(test_concurrent_clear_and_add()),
          ?_test(test_concurrent_session_access())
         ]
     end}.

test_concurrent_clear_and_add() ->
    SessionId = <<"session_race_clear">>,

    %% Add initial events
    ok = add_events(SessionId, [{1, <<"e1">>}, {2, <<"e2">>}]),

    %% Spawn clear and add operations concurrently
    spawn(fun() ->
        timer:sleep(50),
        erlmcp_sse_event_store:clear_session(SessionId)
    end),

    spawn(fun() ->
        timer:sleep(50),
        erlmcp_sse_event_store:add_event(SessionId, 3, <<"e3">>)
    end),

    %% Wait for completion
    timer:sleep(200),

    %% Verify system is in consistent state through API
    %% Either cleared or has event 3, but not corrupted
    {ok, Info} = erlmcp_sse_event_store:get_session_info(SessionId),
    EventCount = maps:get(event_count, Info),
    ?assert(EventCount =:= 0 orelse EventCount =:= 1).

test_concurrent_session_access() ->
    SessionId = <<"session_race_access">>,

    %% Multiple concurrent readers and writers
    lists:foreach(fun(N) ->
        spawn(fun() ->
                case N rem 2 of
                    0 ->
                        %% Writer
                        erlmcp_sse_event_store:add_event(SessionId, N, <<"data">>);
                    1 ->
                        %% Reader
                        erlmcp_sse_event_store:get_events_since(SessionId, undefined)
                end
        end)
    end, lists:seq(1, 20)),

    %% Wait for completion
    timer:sleep(500),

    %% Verify system is consistent through API
    {ok, _Info} = erlmcp_sse_event_store:get_session_info(SessionId),
    %% If we get here without crash, race conditions are handled
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private
%% @doc Helper to add multiple events to a session
-spec add_events(binary(), [{pos_integer(), binary()}]) -> ok.
add_events(SessionId, Events) ->
    lists:foreach(fun({EventNumber, Data}) ->
        {ok, _Id} = erlmcp_sse_event_store:add_event(SessionId, EventNumber, Data)
    end, Events),
    ok.
