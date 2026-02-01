%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp shell convenience functions
%%%
%%% Chicago School TDD - Tests that shell shortcuts work correctly
%%% and delegate to erlmcp_introspect properly
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_shell_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

shell_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"status/0 delegates to introspect", fun test_status/0},
      {"health/0 delegates to introspect", fun test_health/0},
      {"tasks/0 delegates to introspect", fun test_tasks/0},
      {"queues/0 delegates to introspect", fun test_queues/0},
      {"session/1 delegates to introspect", fun test_session/0},
      {"streams/1 delegates to introspect", fun test_streams/0},
      {"help/0 displays help text", fun test_help/0},
      {"sessions/0 lists sessions", fun test_sessions/0},
      {"servers/0 lists servers", fun test_servers/0},
      {"transports/0 lists transports", fun test_transports/0}]}.

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
    %% Call erlmcp:status() and verify it works
    Result = erlmcp:status(),

    %% Should return a map (from erlmcp_introspect:status())
    ?assert(is_map(Result)),
    ?assertMatch(#{status := _}, Result).

test_health() ->
    %% Call erlmcp:health() and verify it works
    {Status, Metrics} = erlmcp:health(),

    %% Should return health tuple (from erlmcp_introspect:health_check())
    ?assert(is_atom(Status)),
    ?assert(lists:member(Status, [healthy, degraded, critical])),
    ?assert(is_map(Metrics)).

test_tasks() ->
    %% Call erlmcp:tasks() and verify it works
    Result = erlmcp:tasks(),

    %% Should return a map (from erlmcp_introspect:tasks())
    ?assert(is_map(Result)),
    ?assertMatch(#{total := _}, Result).

test_queues() ->
    %% Call erlmcp:queues() and verify it works
    Result = erlmcp:queues(),

    %% Should return a map (from erlmcp_introspect:queues())
    ?assert(is_map(Result)),
    ?assertMatch(#{top_10 := _}, Result).

test_session() ->
    %% Call erlmcp:session/1 with a nonexistent session
    Result = erlmcp:session(<<"test-session-id">>),

    %% Should return error tuple (from erlmcp_introspect:session_dump/1)
    ?assertMatch({error, not_found}, Result).

test_streams() ->
    %% Call erlmcp:streams/1 with a nonexistent session
    Result = erlmcp:streams(<<"test-session-id">>),

    %% Should return ok tuple with list (from erlmcp_introspect:streams/1)
    ?assertMatch({ok, _}, Result),
    {ok, Streams} = Result,
    ?assert(is_list(Streams)).

test_help() ->
    %% Call erlmcp:help() and verify it returns ok
    Result = erlmcp:help(),

    %% Should return ok after displaying help
    ?assertEqual(ok, Result).

test_sessions() ->
    %% Call erlmcp:sessions() and verify it works
    Result = erlmcp:sessions(),

    %% Should return a list
    ?assert(is_list(Result)).

test_servers() ->
    %% Call erlmcp:servers() and verify it works
    Result = erlmcp:servers(),

    %% Should return a list
    ?assert(is_list(Result)).

test_transports() ->
    %% Call erlmcp:transports() and verify it works
    Result = erlmcp:transports(),

    %% Should return a list
    ?assert(is_list(Result)).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup,
     fun integration_setup/0,
     fun integration_cleanup/1,
     {timeout, 30, [{"servers/0 shows registered server", fun test_servers_with_server/0}]}}.

integration_setup() ->
    %% Start all required applications
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_observability),

    %% Create a test server
    ServerId = test_server_shell,
    Capabilities = #{tools => #{enabled => true}},

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

test_servers_with_server() ->
    %% Get servers list
    Servers = erlmcp:servers(),

    %% Should have at least our test server
    ?assert(is_list(Servers)),
    ?assert(length(Servers) >= 1),

    %% Verify structure of server entries
    lists:foreach(fun({ServerId, {Pid, _Config}}) ->
                     ?assert(is_atom(ServerId) orelse is_binary(ServerId)),
                     ?assert(is_pid(Pid))
                  end,
                  Servers).
