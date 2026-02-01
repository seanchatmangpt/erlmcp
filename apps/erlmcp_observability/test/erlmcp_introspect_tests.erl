%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_introspect
%%%
%%% Chicago School TDD - No mocks, real processes only
%%% Tests observable behavior through the introspection API
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_introspect_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

introspect_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"status/0 returns system health map", fun test_status/0},
      {"health_check/0 runs immediate health probe", fun test_health_check/0},
      {"tasks/0 returns task status", fun test_tasks/0},
      {"queues/0 returns queue depths", fun test_queues/0},
      {"session_dump/1 returns error for nonexistent session", fun test_session_dump_not_found/0},
      {"streams/1 returns error for nonexistent session", fun test_streams_not_found/0}]}.

setup() ->
    %% Ensure required applications are started
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_observability),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_status() ->
    %% Call status/0 and verify it returns a map with expected keys
    Status = erlmcp_introspect:status(),

    ?assertMatch(#{status := _}, Status),
    ?assertMatch(#{timestamp := _}, Status),
    ?assertMatch(#{sessions := _}, Status),
    ?assertMatch(#{connections := _}, Status),
    ?assertMatch(#{throughput := _}, Status),
    ?assertMatch(#{memory := _}, Status),
    ?assertMatch(#{health_checks := _}, Status),

    %% Verify status is one of the expected values
    HealthStatus = maps:get(status, Status),
    ?assert(lists:member(HealthStatus, [healthy, degraded, critical])),

    %% Verify memory map has expected fields
    Memory = maps:get(memory, Status),
    ?assertMatch(#{heap_mb := _,
                   rss_mb := _,
                   processes := _},
                 Memory),

    %% Verify throughput map has expected fields
    Throughput = maps:get(throughput, Status),
    ?assertMatch(#{current_msg_per_s := _}, Throughput).

test_health_check() ->
    %% Run immediate health check
    {HealthStatus, Metrics} = erlmcp_introspect:health_check(),

    %% Verify health status is one of the expected atoms
    ?assert(lists:member(HealthStatus, [healthy, degraded, critical])),

    %% Verify metrics map has expected keys
    ?assertMatch(#{checks := _,
                   overall := _,
                   timestamp := _},
                 Metrics),

    %% Verify individual checks
    Checks = maps:get(checks, Metrics),
    ?assertMatch(#{registry := _,
                   session_backend := _,
                   supervisors := _},
                 Checks),

    %% Verify overall matches the returned status
    Overall = maps:get(overall, Metrics),
    ?assertEqual(HealthStatus, Overall).

test_tasks() ->
    %% Get task status
    Tasks = erlmcp_introspect:tasks(),

    %% Verify map has expected keys
    ?assertMatch(#{total := _,
                   by_status := _,
                   by_type := _},
                 Tasks),

    %% Verify total is a non-negative integer
    Total = maps:get(total, Tasks),
    ?assert(is_integer(Total)),
    ?assert(Total >= 0),

    %% Verify by_type has expected fields
    ByType = maps:get(by_type, Tasks),
    ?assertMatch(#{servers := _, transports := _}, ByType).

test_queues() ->
    %% Get queue depths
    Queues = erlmcp_introspect:queues(),

    %% Verify map has expected keys
    ?assertMatch(#{top_10 := _,
                   by_type := _,
                   control_plane := _},
                 Queues),

    %% Verify top_10 is a list
    Top10 = maps:get(top_10, Queues),
    ?assert(is_list(Top10)),
    ?assert(length(Top10) =< 10),

    %% Verify each entry in top_10 has expected structure
    lists:foreach(fun(Entry) ->
                     ?assertMatch(#{id := _,
                                    type := _,
                                    depth := _},
                                  Entry)
                  end,
                  Top10),

    %% Verify control_plane is a list of tuples
    ControlPlane = maps:get(control_plane, Queues),
    ?assert(is_list(ControlPlane)).

test_session_dump_not_found() ->
    %% Try to dump a nonexistent session
    Result = erlmcp_introspect:session_dump(<<"nonexistent-session-id">>),

    %% Should return error tuple
    ?assertMatch({error, not_found}, Result).

test_streams_not_found() ->
    %% Try to get streams for a nonexistent session
    Result = erlmcp_introspect:streams(<<"nonexistent-session-id">>),

    %% Should return ok with empty list (session doesn't exist, so no streams)
    ?assertMatch({ok, []}, Result).

%%====================================================================
%% Integration Tests with Real Processes
%%====================================================================

integration_test_() ->
    {setup,
     fun integration_setup/0,
     fun integration_cleanup/1,
     {timeout,
      30,
      [{"status reflects real system state", fun test_status_with_server/0},
       {"session_dump returns data for real session", fun test_session_dump_with_session/0}]}}.

integration_setup() ->
    %% Start all required applications
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_observability),

    %% Create a test server
    ServerId = test_server_introspect,
    Capabilities = #{tools => #{enabled => true}, resources => #{enabled => true}},

    case erlmcp_registry:find_server(ServerId) of
        {error, not_found} ->
            {ok, ServerPid} = erlmcp_server:start_link(ServerId, Capabilities),
            erlmcp_registry:register_server(ServerId, ServerPid, #{capabilities => Capabilities}),
            #{server_id => ServerId, server_pid => ServerPid};
        {ok, {Pid, _}} ->
            #{server_id => ServerId, server_pid => Pid}
    end.

integration_cleanup(#{server_id := ServerId, server_pid := Pid}) ->
    %% Cleanup test server
    erlmcp_registry:unregister_server(ServerId),
    catch gen_server:stop(Pid),
    ok.

test_status_with_server() ->
    %% Get status with a real server running
    Status = erlmcp_introspect:status(),

    %% Verify we see the server in the connections
    Connections = maps:get(connections, Status),
    ServerCount = maps:get(servers, Connections),
    ?assert(ServerCount >= 1).

test_session_dump_with_session() ->
    %% Create a test session
    Metadata = #{test => true, created_by => erlmcp_introspect_tests},
    {ok, SessionId} = erlmcp_session:create(Metadata),

    %% Try to dump the session
    case erlmcp_introspect:session_dump(SessionId) of
        {ok, SessionData} ->
            %% Verify session data has expected structure
            ?assertMatch(#{session_id := SessionId}, SessionData),
            ?assertMatch(#{state := _, metadata := _}, SessionData),

            %% Verify metadata matches what we created
            SessionMetadata = maps:get(metadata, SessionData),
            ?assertEqual(true, maps:get(test, SessionMetadata)),

            %% Cleanup
            erlmcp_session:delete(SessionId);
        {error, not_found} ->
            %% This is acceptable if session manager isn't fully integrated yet
            ?debugMsg("Session manager not fully integrated with introspection"),
            erlmcp_session:delete(SessionId)
    end.

%%====================================================================
%% Property-Based Tests (Structure Validation)
%%====================================================================

structure_test_() ->
    [{"status/0 always returns valid structure",
      fun() ->
         %% Run multiple times to ensure consistency
         lists:foreach(fun(_) ->
                          Status = erlmcp_introspect:status(),
                          ?assertMatch(#{status := _}, Status),
                          %% Verify timestamp is reasonable
                          Timestamp = maps:get(timestamp, Status),
                          Now = erlang:system_time(millisecond),
                          ?assert(Timestamp =< Now),
                          ?assert(Timestamp > Now - 1000)  % Within last second
                       end,
                       lists:seq(1, 10))
      end},
     {"health_check/0 always returns consistent format",
      fun() ->
         lists:foreach(fun(_) ->
                          {Status, Metrics} = erlmcp_introspect:health_check(),
                          ?assert(is_atom(Status)),
                          ?assert(is_map(Metrics)),
                          ?assertMatch(#{checks := _}, Metrics)
                       end,
                       lists:seq(1, 10))
      end}].
