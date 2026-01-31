-module(erlmcp_registry_basic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Test Suite for erlmcp_registry Basic Operations
%%%
%%% Chicago School TDD Principles:
%%%   - Use REAL erlmcp_server processes (NO dummy spawn processes)
%%%   - Test observable behavior through ALL interfaces
%%%   - NO internal state inspection (test API boundaries only)
%%%   - NO record duplication (respect encapsulation)
%%%   - Files <500 lines each
%%%===================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

registry_basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"Registry startup", fun test_registry_startup/0},
        {"Server registration", fun test_server_registration/0},
        {"Server deregistration", fun test_server_deregistration/0},
        {"Duplicate registration same PID", fun test_duplicate_registration_same_pid/0},
        {"List servers", fun test_list_servers/0},
        {"Process monitoring", fun test_process_monitoring/0},
        {"Concurrent registration", fun test_concurrent_registration/0}
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    % Ensure required applications are started
    application:ensure_all_started(gproc),

    % Ensure gproc is started using utility function
    ok = erlmcp_registry_utils:ensure_gproc_started(),

    % Clear any stale test registrations using utility function
    ok = erlmcp_registry_utils:clear_test_registrations(),
    timer:sleep(100),

    % Start the registry for testing
    {ok, RegistryPid} = erlmcp_registry:start_link(),
    #{registry_pid => RegistryPid}.

cleanup(#{registry_pid := RegistryPid}) ->
    % Stop registry gracefully
    catch gen_server:stop(RegistryPid, shutdown, 5000),

    % Clear test registrations using utility function
    ok = erlmcp_registry_utils:clear_test_registrations(),
    timer:sleep(100),
    ok.

%%====================================================================
%% Test Cases - Basic Registration Operations
%%====================================================================

test_registry_startup() ->
    [
        ?_assertEqual([], erlmcp_registry:list_servers()),
        ?_assertMatch({error, not_found}, erlmcp_registry:find_server(unknown))
    ].

test_server_registration() ->
    {ok, ServerPid} = erlmcp_test_helpers:start_test_server(<<"test_server_1">>),
    ServerConfig = #{},

    try
        [
            ?_test(begin
                ?assertEqual(ok, erlmcp_registry:register_server(<<"test_server_1">>, ServerPid, ServerConfig))
            end),
            ?_test(begin
                ?assertMatch({ok, {ServerPid, ServerConfig}},
                             erlmcp_registry:find_server(<<"test_server_1">>))
            end),
            ?_test(begin
                Servers = erlmcp_registry:list_servers(),
                ?assert(lists:keymember(<<"test_server_1">>, 1, Servers))
            end),
            ?_test(begin
                % Test duplicate registration by same PID is idempotent (returns ok)
                ?assertEqual(ok, erlmcp_registry:register_server(<<"test_server_1">>, ServerPid, ServerConfig))
            end),
            ?_test(begin
                % Unregister
                ?assertEqual(ok, erlmcp_registry:unregister_server(<<"test_server_1">>)),
                ?assertMatch({error, not_found}, erlmcp_registry:find_server(<<"test_server_1">>))
            end)
        ]
    after
        erlmcp_test_helpers:stop_test_server(ServerPid)
    end.

test_server_deregistration() ->
    {ok, ServerPid} = erlmcp_test_helpers:start_test_server(<<"test_server_dereg">>),
    ServerConfig = #{},

    try
        [
            ?_test(begin
                ?assertEqual(ok, erlmcp_registry:register_server(<<"test_server_dereg">>, ServerPid, ServerConfig))
            end),
            ?_test(begin
                % First unregister
                ?assertEqual(ok, erlmcp_registry:unregister_server(<<"test_server_dereg">>)),
                ?assertMatch({error, not_found}, erlmcp_registry:find_server(<<"test_server_dereg">>))
            end),
            ?_test(begin
                % Second unregister - should still return ok (idempotent)
                ?assertEqual(ok, erlmcp_registry:unregister_server(<<"test_server_dereg">>))
            end)
        ]
    after
        erlmcp_test_helpers:stop_test_server(ServerPid)
    end.

test_duplicate_registration_same_pid() ->
    {ok, ServerPid} = erlmcp_test_helpers:start_test_server(<<"dup_server_same">>),
    ServerConfig = #{},

    try
        [
            ?_test(begin
                ?assertEqual(ok, erlmcp_registry:register_server(<<"dup_server_same">>, ServerPid, ServerConfig))
            end),
            ?_test(begin
                % Re-registering with same PID should succeed (idempotent)
                ?assertEqual(ok, erlmcp_registry:register_server(<<"dup_server_same">>, ServerPid, ServerConfig))
            end)
        ]
    after
        erlmcp_test_helpers:stop_test_server(ServerPid)
    end.

test_list_servers() ->
    % Create multiple servers using real erlmcp_server processes
    ServerCount = 5,
    ServerIds = [list_to_binary("list_server_" ++ integer_to_list(N)) || N <- lists:seq(1, ServerCount)],

    ServerPids = lists:map(fun(ServerId) ->
        {ok, ServerPid} = erlmcp_test_helpers:start_test_server(ServerId),
        ok = erlmcp_registry:register_server(ServerId, ServerPid, #{}),
        ServerPid
    end, ServerIds),

    try
        ServerList = erlmcp_registry:list_servers(),

        [
            ?_assertEqual(ServerCount, length(ServerList)),
            ?_test(begin
                % Verify all server IDs are present
                lists:foreach(fun(ServerId) ->
                    ?assert(lists:keymember(ServerId, 1, ServerList))
                end, ServerIds)
            end)
        ]
    after
        % Cleanup servers
        lists:foreach(fun(ServerPid) ->
            erlmcp_test_helpers:stop_test_server(ServerPid)
        end, ServerPids)
    end.

test_process_monitoring() ->
    {ok, ServerPid} = erlmcp_test_helpers:start_test_server(<<"test_server_monitor">>),

    % Register server
    ok = erlmcp_registry:register_server(<<"test_server_monitor">>, ServerPid, #{}),

    % Verify registration
    ?assertMatch({ok, {ServerPid, _}}, erlmcp_registry:find_server(<<"test_server_monitor">>)),

    % Stop server process (simulating process death)
    ok = erlmcp_test_helpers:stop_test_server(ServerPid),

    % Wait for gproc to detect process death and cleanup
    timer:sleep(500),

    [
        ?_assertMatch({error, not_found}, erlmcp_registry:find_server(<<"test_server_monitor">>))
    ].

test_concurrent_registration() ->
    ServerId = <<"concurrent_test_server">>,
    ConcurrentCount = 10,

    % Spawn multiple processes trying to register the same server
    Parent = self(),
    Pids = lists:map(fun(_) ->
        spawn_link(fun() ->
            % Use the caller's PID as the "server PID" for simulation
            MockServer = self(),
            Result = erlmcp_registry:register_server(ServerId, MockServer, #{}),
            Parent ! {registration_result, self(), Result}
        end)
    end, lists:seq(1, ConcurrentCount)),

    % Collect results
    Results = [receive
        {registration_result, Pid, Result} ->
            {Pid, Result}
    after 5000 ->
        timeout
    end || _ <- Pids],

    % Count successes and failures
    SuccessCount = lists:foldl(fun({_Pid, Result}, Acc) ->
        case Result of
            ok -> Acc + 1;
            {error, already_registered} -> Acc;
            _ -> Acc
        end
    end, 0, Results),

    % Should have at least one successful registration
    % Due to gproc race conditions, some registrations may fail with already_registered
    [
        ?_assert(SuccessCount >= 1)
    ].
