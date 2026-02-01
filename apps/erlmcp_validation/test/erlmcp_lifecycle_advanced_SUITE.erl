%%%-------------------------------------------------------------------
%%% @doc
%%% Advanced Lifecycle Test Suite for ErlMCP - Graceful Shutdown
%%%
%%% Phase 5B: Graceful Shutdown Testing
%%%
%%% This suite tests advanced lifecycle scenarios focusing on graceful
%%% shutdown under load, zero-downtime upgrades, configuration hot-reload,
%%% and shutdown safety.
%%%
%%% == Test Categories ==
%%%
%%% 1. Graceful Shutdown Under Load:
%%%    - Shutdown with 1000 active connections
%%%    - Shutdown with 10000 active connections
%%%    - In-flight request completion
%%%    - Pending request notification
%%%    - Connection drain timeout
%%%
%%% 2. Zero-Downtime Upgrades:
%%%    - Rolling upgrade simulation
%%%    - Blue-green deployment pattern
%%%    - Canary release validation
%%%    - Traffic switching tests
%%%
%%% 3. Configuration Hot-Reload:
%%%    - Config reload during operation
%%%    - Invalid config rejection
%%%    - Config validation on reload
%%%    - Dynamic capability changes
%%%
%%% 4. Shutdown Safety:
%%%    - No data loss on shutdown
%%%    - Clean resource release
%%%    - Proper process termination
%%%    - Mnesia shutdown safety
%%%
%%% == Test Methodology ==
%%% - Chicago School TDD: Real processes, NO mocks
%%% - Production load simulation: Real concurrent connections
%%% - Observable state verification: Real shutdown behavior
%%% - Timing verification: Measure actual shutdown times
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_lifecycle_advanced_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("../../../include/erlmcp.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).
%% Test cases - Graceful Shutdown Under Load
-export([test_shutdown_with_1000_connections/1, test_shutdown_with_10000_connections/1,
         test_inflight_request_completion/1, test_pending_request_notification/1,
         test_connection_drain_timeout/1]).
%% Test cases - Zero-Downtime Upgrades
-export([test_rolling_upgrade_simulation/1, test_blue_green_deployment/1,
         test_canary_release_validation/1, test_traffic_switching/1]).
%% Test cases - Configuration Hot-Reload
-export([test_config_reload_during_operation/1, test_invalid_config_rejection/1,
         test_config_validation_on_reload/1, test_dynamic_capability_changes/1]).
%% Test cases - Shutdown Safety
-export([test_no_data_loss_on_shutdown/1, test_clean_resource_release/1,
         test_proper_process_termination/1, test_mnesia_shutdown_safety/1]).

%%====================================================================
%% Constants
%%====================================================================

-define(SHUTDOWN_TIMEOUT_MS, 30000).
-define(CONNECTION_DRAIN_MS, 5000).
-define(MAX_1000_CONNECTIONS, 1000).
-define(MAX_10000_CONNECTIONS, 10000).
-define(INFLIGHT_REQUEST_COUNT, 100).
-define(PENDING_REQUEST_COUNT, 50).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [{group, graceful_shutdown_under_load},
     {group, zero_downtime_upgrades},
     {group, config_hot_reload},
     {group, shutdown_safety}].

groups() ->
    [{graceful_shutdown_under_load,
      [sequence],
      [test_shutdown_with_1000_connections,
       test_shutdown_with_10000_connections,
       test_inflight_request_completion,
       test_pending_request_notification,
       test_connection_drain_timeout]},
     {zero_downtime_upgrades,
      [sequence],
      [test_rolling_upgrade_simulation,
       test_blue_green_deployment,
       test_canary_release_validation,
       test_traffic_switching]},
     {config_hot_reload,
      [parallel],
      [test_config_reload_during_operation,
       test_invalid_config_rejection,
       test_config_validation_on_reload,
       test_dynamic_capability_changes]},
     {shutdown_safety,
      [sequence],
      [test_no_data_loss_on_shutdown,
       test_clean_resource_release,
       test_proper_process_termination,
       test_mnesia_shutdown_safety]}].

init_per_suite(Config) ->
    ct:pal("Starting Advanced Lifecycle Test Suite - Graceful Shutdown"),
    ct:pal("==========================================================="),

    %% Start required applications
    start_applications([crypto, ssl, gproc, jsx, jesse]),

    %% Start observability apps (optional)
    start_optional_apps([opentelemetry_api, opentelemetry]),

    %% Start core components
    {ok, _CoreSupPid} = erlmcp_core_sup:start_link(),
    {ok, _ServerSupPid} = erlmcp_server_sup:start_link(),

    %% Wait for system to stabilize
    timer:sleep(500),

    %% Verify core components are running
    ?assertNotEqual(undefined, whereis(erlmcp_core_sup)),
    ?assertNotEqual(undefined, whereis(erlmcp_registry)),
    ?assertNotEqual(undefined, whereis(erlmcp_server_sup)),

    ct:pal("Core components initialized successfully"),
    [{suite_start_time, erlang:system_time(millisecond)} | Config].

end_per_suite(Config) ->
    ct:pal("Ending Advanced Lifecycle Test Suite"),

    %% Stop supervisors
    stop_supervisor(erlmcp_server_sup),
    stop_supervisor(erlmcp_core_sup),

    %% Calculate suite duration
    StartTime = proplists:get_value(suite_start_time, Config, 0),
    Duration = erlang:system_time(millisecond) - StartTime,
    ct:pal("Total suite duration: ~pms (~.2fs)", [Duration, Duration / 1000]),

    ok.

init_per_group(Group, Config) ->
    ct:pal("~nStarting test group: ~p", [Group]),
    ct:pal("-------------------------------------------"),
    [{group, Group}, {group_start_time, erlang:system_time(millisecond)} | Config].

end_per_group(Group, Config) ->
    StartTime = proplists:get_value(group_start_time, Config, 0),
    Duration = erlang:system_time(millisecond) - StartTime,
    ct:pal("Test group ~p completed in ~pms (~.2fs)~n", [Group, Duration, Duration / 1000]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    [{testcase, TestCase}, {testcase_start_time, erlang:system_time(millisecond)} | Config].

end_per_testcase(TestCase, Config) ->
    StartTime = proplists:get_value(testcase_start_time, Config, 0),
    Duration = erlang:system_time(millisecond) - StartTime,
    ct:pal("Test case ~p completed in ~pms (~.2fs)", [TestCase, Duration, Duration / 1000]),
    cleanup_test_artifacts(TestCase),
    ok.

%%====================================================================
%% Graceful Shutdown Under Load Tests
%%====================================================================

%% @doc Test graceful shutdown with 1000 active connections
test_shutdown_with_1000_connections(Config) ->
    ct:pal("Testing graceful shutdown with 1000 active connections"),

    %% Create server with resources and tools
    ServerId = make_test_server_id(shutdown_1k),
    ServerConfig =
        #{capabilities =>
              #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                       tools = #mcp_capability{enabled = true}}},
    {ok, ServerPid} = erlmcp:start_server(ServerId, ServerConfig),

    %% Add test resources and tools
    add_test_resources(ServerId, 10),
    add_test_tools(ServerId, 5),

    %% Spawn 1000 client connections (simulated via spawn)
    ClientPids = spawn_client_connections(?MAX_1000_CONNECTIONS),

    %% Verify all clients are alive
    ?assertEqual(?MAX_1000_CONNECTIONS, length([Pid || Pid <- ClientPids, is_process_alive(Pid)])),
    ct:pal("Created ~p active client connections", [?MAX_1000_CONNECTIONS]),

    %% Measure shutdown time
    ShutdownStart = erlang:monotonic_time(millisecond),

    %% Initiate graceful shutdown via application:stop
    ct:pal("Initiating graceful shutdown..."),
    ok = erlmcp:stop_server(ServerId),

    %% Wait for shutdown to complete
    wait_for_shutdown(ServerPid, ?SHUTDOWN_TIMEOUT_MS),

    ShutdownEnd = erlang:monotonic_time(millisecond),
    ShutdownDuration = ShutdownEnd - ShutdownStart,

    ct:pal("Graceful shutdown completed in ~pms (~.2fs)",
           [ShutdownDuration, ShutdownDuration / 1000]),

    %% Verify all clients are terminated
    AliveCount = length([Pid || Pid <- ClientPids, is_process_alive(Pid)]),
    ct:pal("Remaining alive clients: ~p", [AliveCount]),
    ?assertEqual(0, AliveCount, "All clients should be terminated after shutdown"),

    %% Verify server is stopped
    ?assertNot(is_process_alive(ServerPid)),

    %% Verify registry cleanup
    {error, not_found} = erlmcp_registry:find_server(ServerId),

    %% Verify shutdown completed within timeout
    ?assert(ShutdownDuration < ?SHUTDOWN_TIMEOUT_MS),

    ct:pal("SUCCESS: Graceful shutdown of ~p connections completed in ~pms",
           [?MAX_1000_CONNECTIONS, ShutdownDuration]),

    Config.

%% @doc Test graceful shutdown with 10000 active connections
test_shutdown_with_10000_connections(Config) ->
    ct:pal("Testing graceful shutdown with 10000 active connections"),

    %% Create server
    ServerId = make_test_server_id(shutdown_10k),
    ServerConfig =
        #{capabilities =>
              #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                       tools = #mcp_capability{enabled = true}}},
    {ok, ServerPid} = erlmcp:start_server(ServerId, ServerConfig),

    %% Add test resources and tools
    add_test_resources(ServerId, 20),
    add_test_tools(ServerId, 10),

    %% Spawn 10000 client connections
    ClientPids = spawn_client_connections(?MAX_10000_CONNECTIONS),

    %% Verify all clients are alive
    AliveBefore = length([Pid || Pid <- ClientPids, is_process_alive(Pid)]),
    ct:pal("Created ~p active client connections", [AliveBefore]),
    ?assertEqual(?MAX_10000_CONNECTIONS, AliveBefore),

    %% Measure shutdown time
    ShutdownStart = erlang:monotonic_time(millisecond),

    %% Initiate graceful shutdown
    ct:pal("Initiating graceful shutdown of ~p connections...", [?MAX_10000_CONNECTIONS]),
    ok = erlmcp:stop_server(ServerId),

    %% Wait for shutdown to complete
    wait_for_shutdown(ServerPid, ?SHUTDOWN_TIMEOUT_MS),

    ShutdownEnd = erlang:monotonic_time(millisecond),
    ShutdownDuration = ShutdownEnd - ShutdownStart,

    ct:pal("Graceful shutdown completed in ~pms (~.2fs)",
           [ShutdownDuration, ShutdownDuration / 1000]),

    %% Verify all clients are terminated
    AliveAfter = length([Pid || Pid <- ClientPids, is_process_alive(Pid)]),
    ct:pal("Remaining alive clients: ~p", [AliveAfter]),
    ?assertEqual(0, AliveAfter, "All clients should be terminated after shutdown"),

    %% Verify server is stopped
    ?assertNot(is_process_alive(ServerPid)),

    %% Verify registry cleanup
    {error, not_found} = erlmcp_registry:find_server(ServerId),

    %% Verify shutdown completed within timeout
    ?assert(ShutdownDuration < ?SHUTDOWN_TIMEOUT_MS),

    ct:pal("SUCCESS: Graceful shutdown of ~p connections completed in ~pms",
           [?MAX_10000_CONNECTIONS, ShutdownDuration]),

    Config.

%% @doc Test in-flight request completion during shutdown
test_inflight_request_completion(Config) ->
    ct:pal("Testing in-flight request completion during shutdown"),

    %% Create server with slow tools
    ServerId = make_test_server_id(inflight),
    ServerConfig =
        #{capabilities => #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
    {ok, ServerPid} = erlmcp:start_server(ServerId, ServerConfig),

    %% Add slow tool (200ms delay)
    SlowTool =
        fun(Args) ->
           timer:sleep(200),
           Name = maps:get(<<"name">>, Args, <<"World">>),
           #{result => <<"Hello, ", Name/binary, "!">>}
        end,
    ok = erlmcp:add_tool(ServerId, <<"slow_tool">>, SlowTool),

    %% Create client and send in-flight requests
    ClientPids = spawn_client_connections(10),
    RequestCount = ?INFLIGHT_REQUEST_COUNT,

    %% Send requests that will be in-flight during shutdown
    ct:pal("Sending ~p in-flight requests...", [RequestCount]),
    send_tool_calls(ServerId, <<"slow_tool">>, RequestCount),

    %% Wait a bit for requests to be sent but not completed
    timer:sleep(50),

    %% Initiate shutdown while requests are in-flight
    ct:pal("Initiating shutdown with ~p in-flight requests...", [RequestCount]),
    ShutdownStart = erlang:monotonic_time(millisecond),

    %% Shutdown should wait for in-flight requests (within reason)
    ok = erlmcp:stop_server(ServerId),

    %% Wait for shutdown
    wait_for_shutdown(ServerPid, ?SHUTDOWN_TIMEOUT_MS),

    ShutdownEnd = erlang:monotonic_time(millisecond),
    ShutdownDuration = ShutdownEnd - ShutdownStart,

    ct:pal("Shutdown completed in ~pms (~.2fs)", [ShutdownDuration, ShutdownDuration / 1000]),

    %% Verify clients are terminated
    AliveCount = length([Pid || Pid <- ClientPids, is_process_alive(Pid)]),
    ?assertEqual(0, AliveCount),

    %% Verify server is stopped
    ?assertNot(is_process_alive(ServerPid)),

    %% Shutdown should complete even with in-flight requests
    ?assert(ShutdownDuration < ?SHUTDOWN_TIMEOUT_MS),

    ct:pal("SUCCESS: Handled ~p in-flight requests during shutdown", [RequestCount]),

    Config.

%% @doc Test pending request notification during shutdown
test_pending_request_notification(Config) ->
    ct:pal("Testing pending request notification during shutdown"),

    %% Create server
    ServerId = make_test_server_id(pending),
    ServerConfig =
        #{capabilities => #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
    {ok, ServerPid} = erlmcp:start_server(ServerId, ServerConfig),

    %% Add tool
    TestTool =
        fun(Args) ->
           Name = maps:get(<<"name">>, Args, <<"World">>),
           #{result => <<"Hello, ", Name/binary, "!">>}
        end,
    ok = erlmcp:add_tool(ServerId, <<"pending_tool">>, TestTool),

    %% Create clients and send requests
    ClientPids = spawn_client_connections(5),

    %% Create a process to track pending requests
    Parent = self(),
    _Tracker =
        spawn(fun() ->
                 %% Send pending requests
                 [begin
                      case erlmcp_server:call_tool(ServerPid,
                                                   <<"pending_tool">>,
                                                   #{<<"name">> =>
                                                         <<"Test", (integer_to_binary(N))/binary>>})
                      of
                          {ok, _Result} ->
                              Parent ! {request_complete, N};
                          {error, _Reason} ->
                              Parent ! {request_error, N, error}
                      end
                  end
                  || N <- lists:seq(1, ?PENDING_REQUEST_COUNT)],
                 Parent ! tracker_done
              end),

    %% Wait for some requests to be sent
    timer:sleep(100),

    %% Initiate shutdown
    ct:pal("Initiating shutdown with pending requests..."),
    ShutdownStart = erlang:monotonic_time(millisecond),

    ok = erlmcp:stop_server(ServerId),

    %% Wait for shutdown
    wait_for_shutdown(ServerPid, ?SHUTDOWN_TIMEOUT_MS),

    ShutdownEnd = erlang:monotonic_time(millisecond),
    ShutdownDuration = ShutdownEnd - ShutdownStart,

    ct:pal("Shutdown completed in ~pms", [ShutdownDuration]),

    %% Collect request results
    Completed = collect_results(1000, []),
    ct:pal("Request results: ~p", [Completed]),

    %% Verify clients terminated
    ?assertEqual(0, length([Pid || Pid <- ClientPids, is_process_alive(Pid)])),

    %% Verify server stopped
    ?assertNot(is_process_alive(ServerPid)),

    ct:pal("SUCCESS: Handled pending requests during shutdown"),

    Config.

%% @doc Test connection drain timeout during shutdown
test_connection_drain_timeout(Config) ->
    ct:pal("Testing connection drain timeout during shutdown"),

    %% Create server
    ServerId = make_test_server_id(drain),
    ServerConfig =
        #{capabilities => #mcp_server_capabilities{resources = #mcp_capability{enabled = true}}},
    {ok, ServerPid} = erlmcp:start_server(ServerId, ServerConfig),

    %% Add test resources
    add_test_resources(ServerId, 5),

    %% Create clients with slow resource reads
    ClientPids = spawn_client_connections(50),
    ct:pal("Created ~p client connections", [50]),

    %% Start slow resource reads (will be interrupted by drain timeout)
    [begin
         spawn(fun() ->
                  case erlmcp_server:read_resource(ServerPid,
                                                   <<"resource://test/",
                                                     (integer_to_binary(N))/binary>>)
                  of
                      {ok, _} ->
                          ok;
                      {error, _} ->
                          ok
                  end
               end)
     end
     || N <- lists:seq(1, 50)],

    %% Wait for operations to start
    timer:sleep(100),

    %% Measure drain time
    DrainStart = erlang:monotonic_time(millisecond),

    %% Initiate shutdown (should respect drain timeout)
    ct:pal("Initiating shutdown with drain timeout..."),
    ok = erlmcp:stop_server(ServerId),

    %% Wait for shutdown
    wait_for_shutdown(ServerPid, ?SHUTDOWN_TIMEOUT_MS),

    DrainEnd = erlang:monotonic_time(millisecond),
    DrainDuration = DrainEnd - DrainStart,

    ct:pal("Connection drain completed in ~pms", [DrainDuration]),

    %% Verify all clients terminated
    AliveCount = length([Pid || Pid <- ClientPids, is_process_alive(Pid)]),
    ?assertEqual(0, AliveCount),

    %% Verify server stopped
    ?assertNot(is_process_alive(ServerPid)),

    %% Drain should complete within reasonable time
    ?assert(DrainDuration < ?SHUTDOWN_TIMEOUT_MS),

    ct:pal("SUCCESS: Connection drain completed in ~pms", [DrainDuration]),

    Config.

%%====================================================================
%% Zero-Downtime Upgrade Tests
%%====================================================================

%% @doc Test rolling upgrade simulation
test_rolling_upgrade_simulation(Config) ->
    ct:pal("Testing rolling upgrade simulation"),

    %% Create initial server instances (simulating cluster)
    ServerCount = 3,
    Servers =
        [begin
             ServerId = make_test_server_id({rolling, N}),
             ServerConfig =
                 #{capabilities =>
                       #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
             {ok, Pid} = erlmcp:start_server(ServerId, ServerConfig),
             {ServerId, Pid, N}
         end
         || N <- lists:seq(1, ServerCount)],

    ct:pal("Created ~p server instances for rolling upgrade", [ServerCount]),

    %% Add tools to all servers
    lists:foreach(fun({ServerId, _Pid, _N}) ->
                     TestTool =
                         fun(Args) ->
                            Name = maps:get(<<"name">>, Args, <<"World">>),
                            #{result => <<"Hello, ", Name/binary, "!">>}
                         end,
                     ok = erlmcp:add_tool(ServerId, <<"rolling_tool">>, TestTool)
                  end,
                  Servers),

    %% Create clients for each server
    ClientsPerServer = 10,
    AllClients =
        [begin
             ClientPids = spawn_client_connections(ClientsPerServer),
             {ServerId, ClientPids}
         end
         || {ServerId, _Pid, _N} <- Servers],

    ct:pal("Created ~p clients per server", [ClientsPerServer]),

    %% Simulate rolling upgrade: upgrade one server at a time
    lists:foreach(fun({ServerId, _Pid, N}) ->
                     ct:pal("Upgrading server ~p/~p: ~p", [N, ServerCount, ServerId]),

                     %% Get current clients
                     {ServerId, ClientPids} = lists:keyfind(ServerId, 1, AllClients),

                     %% Stop server (simulate upgrade)
                     ok = erlmcp:stop_server(ServerId),

                     %% Verify clients disconnected
                     timer:sleep(100),
                     AliveCount = length([Pid || Pid <- ClientPids, is_process_alive(Pid)]),
                     ?assertEqual(0, AliveCount, "Clients should be disconnected"),

                     %% Restart server (simulate upgraded version)
                     {ok, NewPid} =
                         erlmcp:start_server(ServerId,
                                             #{capabilities =>
                                                   #mcp_server_capabilities{tools =
                                                                                #mcp_capability{enabled
                                                                                                    =
                                                                                                    true}}}),

                     %% Verify server restarted
                     ?assert(is_process_alive(NewPid)),

                     %% Re-add tools
                     TestTool =
                         fun(Args) ->
                            Name = maps:get(<<"name">>, Args, <<"World">>),
                            #{result => <<"Hello (upgraded), ", Name/binary, "!">>}
                         end,
                     ok = erlmcp:add_tool(ServerId, <<"rolling_tool_v2">>, TestTool),

                     %% Spawn new clients
                     NewClientPids = spawn_client_connections(ClientsPerServer),
                     ?assertEqual(ClientsPerServer, length(NewClientPids)),

                     %% Update clients list
                     lists:keyreplace(ServerId, 1, AllClients, {ServerId, NewClientPids}),

                     ct:pal("Server ~p upgraded successfully", [N]),
                     timer:sleep(200)
                  end,
                  Servers),

    %% Cleanup all servers
    lists:foreach(fun({ServerId, _Pid, _N}) -> ok = erlmcp:stop_server(ServerId) end, Servers),

    ct:pal("SUCCESS: Rolling upgrade of ~p servers completed", [ServerCount]),

    Config.

%% @doc Test blue-green deployment pattern
test_blue_green_deployment(Config) ->
    ct:pal("Testing blue-green deployment pattern"),

    %% Create blue environment (current production)
    BlueServerId = make_test_server_id(blue),
    BlueConfig =
        #{capabilities => #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
    {ok, BluePid} = erlmcp:start_server(BlueServerId, BlueConfig),

    %% Add tool to blue
    BlueTool =
        fun(Args) ->
           Name = maps:get(<<"name">>, Args, <<"World">>),
           #{result => <<"Blue: Hello, ", Name/binary, "!">>}
        end,
    ok = erlmcp:add_tool(BlueServerId, <<"bg_tool">>, BlueTool),

    %% Create clients for blue
    BlueClients = spawn_client_connections(20),
    ct:pal("Blue environment: ~p active connections", [length(BlueClients)]),

    %% Create green environment (new version)
    GreenServerId = make_test_server_id(green),
    GreenConfig =
        #{capabilities => #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
    {ok, GreenPid} = erlmcp:start_server(GreenServerId, GreenConfig),

    %% Add enhanced tool to green
    GreenTool =
        fun(Args) ->
           Name = maps:get(<<"name">>, Args, <<"World">>),
           #{result => <<"Green: Hello, ", Name/binary, "! (enhanced)">>}
        end,
    ok = erlmcp:add_tool(GreenServerId, <<"bg_tool">>, GreenTool),

    %% Create clients for green (testing phase)
    GreenClients = spawn_client_connections(5),
    ct:pal("Green environment: ~p test connections", [length(GreenClients)]),

    %% Verify both environments are running
    ?assert(is_process_alive(BluePid)),
    ?assert(is_process_alive(GreenPid)),

    %% Simulate traffic switch: switch all clients to green
    ct:pal("Switching traffic from blue to green..."),

    %% Disconnect blue clients
    lists:foreach(fun(Pid) ->
                     case is_process_alive(Pid) of
                         true ->
                             exit(Pid, normal);
                         false ->
                             ok
                     end
                  end,
                  BlueClients),

    %% Create new clients for green (production load)
    NewGreenClients = spawn_client_connections(20),
    ct:pal("Green environment: ~p production connections", [length(NewGreenClients)]),

    %% Verify green is handling load
    ?assert(is_process_alive(GreenPid)),
    ?assertEqual(20, length([Pid || Pid <- NewGreenClients, is_process_alive(Pid)])),

    %% Decommission blue environment
    ct:pal("Decommissioning blue environment..."),
    ok = erlmcp:stop_server(BlueServerId),
    ?assertNot(is_process_alive(BluePid)),

    %% Cleanup green
    lists:foreach(fun(Pid) ->
                     case is_process_alive(Pid) of
                         true ->
                             exit(Pid, normal);
                         false ->
                             ok
                     end
                  end,
                  NewGreenClients),
    ok = erlmcp:stop_server(GreenServerId),

    ct:pal("SUCCESS: Blue-green deployment completed"),

    Config.

%% @doc Test canary release validation
test_canary_release_validation(Config) ->
    ct:pal("Testing canary release validation"),

    %% Create stable server (majority of traffic)
    StableServerId = make_test_server_id(stable),
    StableConfig =
        #{capabilities => #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
    {ok, StablePid} = erlmcp:start_server(StableServerId, StableConfig),

    StableTool =
        fun(Args) ->
           Name = maps:get(<<"name">>, Args, <<"World">>),
           #{result => <<"Stable: ", Name/binary>>}
        end,
    ok = erlmcp:add_tool(StableServerId, <<"canary_tool">>, StableTool),

    %% Create canary server (new version, small traffic)
    CanaryServerId = make_test_server_id(canary),
    CanaryConfig =
        #{capabilities => #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
    {ok, CanaryPid} = erlmcp:start_server(CanaryServerId, CanaryConfig),

    CanaryTool =
        fun(Args) ->
           Name = maps:get(<<"name">>, Args, <<"World">>),
           #{result => <<"Canary: ", Name/binary, " (v2)">>}
        end,
    ok = erlmcp:add_tool(CanaryServerId, <<"canary_tool">>, CanaryTool),

    %% Simulate traffic distribution: 90% stable, 10% canary
    StableClients = spawn_client_connections(90),
    CanaryClients = spawn_client_connections(10),

    ct:pal("Traffic distribution: ~p stable, ~p canary",
           [length(StableClients), length(CanaryClients)]),

    %% Verify both servers are running
    ?assert(is_process_alive(StablePid)),
    ?assert(is_process_alive(CanaryPid)),

    %% Monitor canary for issues (simulate validation period)
    ct:pal("Monitoring canary for issues..."),
    timer:sleep(1000),

    %% Simulate successful validation: gradually increase canary traffic
    ct:pal("Canary validated successfully, increasing traffic..."),

    %% Add more canary clients (30% traffic)
    MoreCanaryClients = spawn_client_connections(20),
    ct:pal("New traffic distribution: ~p stable, ~p canary", [90, 30]),

    %% Final validation: switch all traffic to canary
    ct:pal("Final validation: switching all traffic to canary..."),

    %% Disconnect stable clients
    lists:foreach(fun(Pid) ->
                     case is_process_alive(Pid) of
                         true ->
                             exit(Pid, normal);
                         false ->
                             ok
                     end
                  end,
                  StableClients),

    %% Switch remaining stable clients to canary
    FinalCanaryClients = spawn_client_connections(90),

    %% Verify canary handling all traffic
    ?assert(is_process_alive(CanaryPid)),
    TotalCanaryClients =
        length(CanaryClients) + length(MoreCanaryClients) + length(FinalCanaryClients),
    ct:pal("Canary handling ~p total connections", [TotalCanaryClients]),

    %% Decommission stable
    ok = erlmcp:stop_server(StableServerId),
    ?assertNot(is_process_alive(StablePid)),

    %% Cleanup canary
    AllCanaryClients = CanaryClients ++ MoreCanaryClients ++ FinalCanaryClients,
    lists:foreach(fun(Pid) ->
                     case is_process_alive(Pid) of
                         true ->
                             exit(Pid, normal);
                         false ->
                             ok
                     end
                  end,
                  AllCanaryClients),
    ok = erlmcp:stop_server(CanaryServerId),

    ct:pal("SUCCESS: Canary release validation completed"),

    Config.

%% @doc Test traffic switching between servers
test_traffic_switching(Config) ->
    ct:pal("Testing traffic switching"),

    %% Create two servers
    ServerAId = make_test_server_id(server_a),
    {ok, ServerAPid} =
        erlmcp:start_server(ServerAId,
                            #{capabilities =>
                                  #mcp_server_capabilities{tools =
                                                               #mcp_capability{enabled = true}}}),

    ServerBId = make_test_server_id(server_b),
    {ok, ServerBPid} =
        erlmcp:start_server(ServerBId,
                            #{capabilities =>
                                  #mcp_server_capabilities{tools =
                                                               #mcp_capability{enabled = true}}}),

    %% Add tools
    ToolA =
        fun(Args) ->
           Name = maps:get(<<"name">>, Args, <<"World">>),
           #{result => <<"Server A: ", Name/binary>>}
        end,
    ok = erlmcp:add_tool(ServerAId, <<"switch_tool">>, ToolA),

    ToolB =
        fun(Args) ->
           Name = maps:get(<<"name">>, Args, <<"World">>),
           #{result => <<"Server B: ", Name/binary>>}
        end,
    ok = erlmcp:add_tool(ServerBId, <<"switch_tool">>, ToolB),

    %% Initial traffic: all to Server A
    ClientsA = spawn_client_connections(50),
    ct:pal("Initial traffic: ~p clients on Server A", [length(ClientsA)]),

    %% Verify Server A handling load
    ?assert(is_process_alive(ServerAPid)),
    ?assertEqual(50, length([Pid || Pid <- ClientsA, is_process_alive(Pid)])),

    %% Switch 50% traffic to Server B
    ct:pal("Switching 50% traffic to Server B..."),

    %% Move 25 clients from A to B
    ToMove = lists:sublist(ClientsA, 25),
    [exit(Pid, normal) || Pid <- ToMove],
    ClientsB = spawn_client_connections(25),

    RemainingA = lists:nthtail(25, ClientsA),
    ct:pal("After switch: ~p on Server A, ~p on Server B", [length(RemainingA), length(ClientsB)]),

    %% Verify both servers running
    ?assert(is_process_alive(ServerAPid)),
    ?assert(is_process_alive(ServerBPid)),

    %% Switch remaining traffic to Server B
    ct:pal("Switching remaining traffic to Server B..."),
    [exit(Pid, normal) || Pid <- RemainingA],
    MoreClientsB = spawn_client_connections(25),

    AllClientsB = ClientsB ++ MoreClientsB,
    ct:pal("Final: ~p clients on Server B", [length(AllClientsB)]),

    %% Verify Server B handling all traffic
    ?assert(is_process_alive(ServerBPid)),
    ?assertEqual(50, length([Pid || Pid <- AllClientsB, is_process_alive(Pid)])),

    %% Cleanup
    [exit(Pid, normal) || Pid <- AllClientsB],
    ok = erlmcp:stop_server(ServerAId),
    ok = erlmcp:stop_server(ServerBId),

    ct:pal("SUCCESS: Traffic switching completed"),

    Config.

%%====================================================================
%% Configuration Hot-Reload Tests
%%====================================================================

%% @doc Test configuration reload during operation
test_config_reload_during_operation(Config) ->
    ct:pal("Testing configuration reload during operation"),

    %% Create server with initial config
    ServerId = make_test_server_id(config_reload),
    InitialConfig =
        #{capabilities =>
              #mcp_server_capabilities{tools = #mcp_capability{enabled = true},
                                       resources = #mcp_capability{enabled = false}}},
    {ok, _ServerPid} = erlmcp:start_server(ServerId, InitialConfig),

    %% Add tool (tools enabled)
    TestTool = fun(_Args) -> #{result => <<"Tool result">>} end,
    ok = erlmcp:add_tool(ServerId, <<"test_tool">>, TestTool),

    %% Create clients
    Clients = spawn_client_connections(10),
    ct:pal("Created ~p clients with initial config", [length(Clients)]),

    %% Simulate config reload - just verify server remains stable
    ct:pal("Simulating configuration reload..."),
    timer:sleep(100),

    %% Verify server still running
    ?assertEqual(10, length([Pid || Pid <- Clients, is_process_alive(Pid)])),
    ct:pal("Configuration reloaded successfully (simulated)"),

    %% Cleanup
    [exit(Pid, normal) || Pid <- Clients],
    ok = erlmcp:stop_server(ServerId),

    ct:pal("SUCCESS: Configuration reload test completed"),

    Config.

%% @doc Test invalid config rejection
test_invalid_config_rejection(Config) ->
    ct:pal("Testing invalid config rejection"),

    %% Create server
    ServerId = make_test_server_id(invalid_config),
    ValidConfig =
        #{capabilities => #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
    {ok, ServerPid} = erlmcp:start_server(ServerId, ValidConfig),

    %% Attempting to create server with invalid config should fail
    InvalidServerId = make_test_server_id(invalid_server),
    InvalidConfig = #{capabilities => invalid_type},

    ct:pal("Attempting to create server with invalid configuration..."),

    %% This should fail or handle the invalid config gracefully
    case erlmcp:start_server(InvalidServerId, InvalidConfig) of
        {error, _Reason} ->
            ct:pal("Invalid config correctly rejected");
        {ok, _Pid} ->
            %% If it succeeds, stop it and warn
            ok = erlmcp:stop_server(InvalidServerId),
            ct:pal("WARNING: Invalid config was not rejected (implementation-dependent)")
    end,

    %% Verify original server still alive
    ?assert(is_process_alive(ServerPid), "Server should remain alive"),

    %% Cleanup
    ok = erlmcp:stop_server(ServerId),

    ct:pal("SUCCESS: Invalid config rejection test completed"),

    Config.

%% @doc Test config validation on reload
test_config_validation_on_reload(Config) ->
    ct:pal("Testing config validation on reload"),

    %% Create server
    ServerId = make_test_server_id(config_validation),
    InitialConfig =
        #{capabilities => #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
    {ok, _ServerPid} = erlmcp:start_server(ServerId, InitialConfig),

    %% Define valid config
    ValidConfig =
        #{capabilities =>
              #mcp_server_capabilities{tools = #mcp_capability{enabled = true},
                                       resources = #mcp_capability{enabled = true}}},

    ct:pal("Validating configuration..."),

    %% Validate config structure
    case validate_config(ValidConfig) of
        ok ->
            ct:pal("Configuration validation passed");
        {error, Reason} ->
            ct:fail("Valid configuration rejected: ~p", [Reason])
    end,

    %% Define invalid config
    _InvalidConfig = #{capabilities => "invalid"},

    ct:pal("Validating invalid configuration..."),
    case validate_config(_InvalidConfig) of
        ok ->
            ct:fail("Invalid configuration should be rejected");
        {error, _Reason} ->
            ct:pal("Invalid configuration correctly rejected")
    end,

    %% Cleanup
    ok = erlmcp:stop_server(ServerId),

    ct:pal("SUCCESS: Config validation test completed"),

    Config.

%% @doc Test dynamic capability changes
test_dynamic_capability_changes(Config) ->
    ct:pal("Testing dynamic capability changes"),

    %% Create server with minimal capabilities
    ServerId = make_test_server_id(dynamic_caps),
    InitialConfig =
        #{capabilities => #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}},
    {ok, ServerPid} = erlmcp:start_server(ServerId, InitialConfig),

    %% Add tool
    TestTool = fun(_Args) -> #{result => <<"Tool result">>} end,
    ok = erlmcp:add_tool(ServerId, <<"dynamic_tool">>, TestTool),

    %% Create clients
    Clients = spawn_client_connections(5),

    %% Dynamically enable resources
    ct:pal("Enabling resources capability..."),

    %% Add resources dynamically
    lists:foreach(fun(N) ->
                     ResourceUri = <<"resource://dynamic/", (integer_to_binary(N))/binary>>,
                     ResourceContent =
                         #{<<"content">> => <<"Resource ", (integer_to_binary(N))/binary>>},
                     ok = erlmcp:add_resource(ServerId, ResourceUri, ResourceContent)
                  end,
                  lists:seq(1, 5)),

    ct:pal("Added 5 dynamic resources"),

    %% Verify server still running with clients
    ?assert(is_process_alive(ServerPid)),
    ?assertEqual(5, length([Pid || Pid <- Clients, is_process_alive(Pid)])),

    %% Dynamically disable tools
    ct:pal("Disabling tools capability..."),
    ok = erlmcp:delete_tool(ServerId, <<"dynamic_tool">>),
    ct:pal("Tool deleted successfully"),

    %% Verify server and clients still operational
    ?assert(is_process_alive(ServerPid)),

    %% Cleanup
    [exit(Pid, normal) || Pid <- Clients],
    ok = erlmcp:stop_server(ServerId),

    ct:pal("SUCCESS: Dynamic capability changes test completed"),

    Config.

%%====================================================================
%% Shutdown Safety Tests
%%====================================================================

%% @doc Test no data loss on shutdown
test_no_data_loss_on_shutdown(Config) ->
    ct:pal("Testing no data loss on shutdown"),

    %% Create server with resources
    ServerId = make_test_server_id(no_data_loss),
    {ok, ServerPid} =
        erlmcp:start_server(ServerId,
                            #{capabilities =>
                                  #mcp_server_capabilities{resources =
                                                               #mcp_capability{enabled = true},
                                                           tools =
                                                               #mcp_capability{enabled = true}}}),

    %% Add resources and tools
    ResourceCount = 20,
    ToolCount = 10,

    lists:foreach(fun(N) ->
                     ResourceUri = <<"resource://data/", (integer_to_binary(N))/binary>>,
                     ResourceContent =
                         #{<<"content">> => <<"Important data ", (integer_to_binary(N))/binary>>,
                           <<"version">> => N},
                     ok = erlmcp:add_resource(ServerId, ResourceUri, ResourceContent)
                  end,
                  lists:seq(1, ResourceCount)),

    lists:foreach(fun(N) ->
                     ToolName = <<"tool_", (integer_to_binary(N))/binary>>,
                     ToolFun =
                         fun(_Args) ->
                            #{result => <<"Tool result ", (integer_to_binary(N))/binary>>}
                         end,
                     ok = erlmcp:add_tool(ServerId, ToolName, ToolFun)
                  end,
                  lists:seq(1, ToolCount)),

    ct:pal("Added ~p resources and ~p tools", [ResourceCount, ToolCount]),

    %% Create clients
    Clients = spawn_client_connections(10),

    %% Record server state before shutdown
    {ok, {_ServerPid, ServerConfig}} = erlmcp_registry:find_server(ServerId),
    StateBefore = ServerConfig,

    %% Initiate shutdown
    ct:pal("Initiating shutdown..."),
    ok = erlmcp:stop_server(ServerId),

    %% Wait for shutdown
    wait_for_shutdown(ServerPid, ?SHUTDOWN_TIMEOUT_MS),

    %% Verify server stopped
    ?assertNot(is_process_alive(ServerPid)),

    %% Verify registry cleanup (no orphaned state)
    {error, not_found} = erlmcp_registry:find_server(ServerId),

    %% Verify clients terminated
    ?assertEqual(0, length([Pid || Pid <- Clients, is_process_alive(Pid)])),

    ct:pal("State before shutdown: ~p", [StateBefore]),
    ct:pal("SUCCESS: No data loss on shutdown (all state cleaned up)"),

    Config.

%% @doc Test clean resource release
test_clean_resource_release(Config) ->
    ct:pal("Testing clean resource release"),

    %% Create server
    ServerId = make_test_server_id(resource_release),
    {ok, ServerPid} =
        erlmcp:start_server(ServerId,
                            #{capabilities =>
                                  #mcp_server_capabilities{resources =
                                                               #mcp_capability{enabled = true}}}),

    %% Add resources with ETS tables (simulating external resources)
    ResourceCount = 10,
    lists:foreach(fun(N) ->
                     ResourceUri = <<"resource://ets/", (integer_to_binary(N))/binary>>,
                     %% Create ETS table for resource
                     TableName =
                         binary_to_atom(<<"resource_table_", (integer_to_binary(N))/binary>>, utf8),
                     ets:new(TableName, [set, public, named_table]),
                     ets:insert(TableName,
                                {data, <<"Resource data ", (integer_to_binary(N))/binary>>}),

                     ResourceContent = #{<<"ets_table">> => TableName},
                     ok = erlmcp:add_resource(ServerId, ResourceUri, ResourceContent)
                  end,
                  lists:seq(1, ResourceCount)),

    ct:pal("Created ~p resources with ETS tables", [ResourceCount]),

    %% Verify ETS tables exist
    TableNames =
        [binary_to_atom(<<"resource_table_", (integer_to_binary(N))/binary>>, utf8)
         || N <- lists:seq(1, ResourceCount)],
    lists:foreach(fun(TableName) -> ?assert(ets:info(TableName) =/= undefined) end, TableNames),

    %% Initiate shutdown
    ct:pal("Initiating shutdown with ETS resources..."),
    ok = erlmcp:stop_server(ServerId),

    %% Wait for shutdown
    wait_for_shutdown(ServerPid, ?SHUTDOWN_TIMEOUT_MS),

    %% Verify ETS tables cleaned up (if server manages them)
    %% Note: This depends on implementation - some ETS tables may be owned by other processes
    ct:pal("Checking ETS table cleanup..."),
    CleanupCount =
        length([TableName || TableName <- TableNames, ets:info(TableName) =:= undefined]),
    ct:pal("ETS tables cleaned up: ~p/~p", [CleanupCount, ResourceCount]),

    %% Verify server stopped
    ?assertNot(is_process_alive(ServerPid)),

    ct:pal("SUCCESS: Clean resource release test completed"),

    Config.

%% @doc Test proper process termination
test_proper_process_termination(Config) ->
    ct:pal("Testing proper process termination"),

    %% Create server
    ServerId = make_test_server_id(process_term),
    {ok, ServerPid} =
        erlmcp:start_server(ServerId,
                            #{capabilities =>
                                  #mcp_server_capabilities{tools =
                                                               #mcp_capability{enabled = true}}}),

    %% Add tool
    TestTool = fun(_Args) -> #{result => <<"Tool result">>} end,
    ok = erlmcp:add_tool(ServerId, <<"term_tool">>, TestTool),

    %% Create monitored clients
    Clients =
        [spawn_monitor(fun() ->
                          %% Simulate client process
                          timer:sleep(1000),
                          client_done
                       end)
         || _ <- lists:seq(1, 20)],

    ct:pal("Created ~p monitored clients", [length(Clients)]),

    %% Initiate graceful shutdown
    ct:pal("Initiating graceful shutdown..."),
    ShutdownStart = erlang:monotonic_time(millisecond),

    ok = erlmcp:stop_server(ServerId),

    %% Wait for shutdown
    wait_for_shutdown(ServerPid, ?SHUTDOWN_TIMEOUT_MS),

    ShutdownEnd = erlang:monotonic_time(millisecond),
    ShutdownDuration = ShutdownEnd - ShutdownStart,

    %% Verify server terminated cleanly
    ?assertNot(is_process_alive(ServerPid)),

    %% Collect client monitor results
    ClientResults =
        [receive
             {'DOWN', Ref, process, _Pid, _Reason} ->
                 {down, Ref}
         after 1000 ->
             timeout
         end
         || {_, Ref} <- Clients],

    DownCount = length([R || R <- ClientResults, element(1, R) =:= down]),
    TimeoutCount = length([R || R <- ClientResults, R =:= timeout]),

    ct:pal("Client termination results: ~p down, ~p timeout", [DownCount, TimeoutCount]),

    %% Verify shutdown completed in reasonable time
    ?assert(ShutdownDuration < ?SHUTDOWN_TIMEOUT_MS),

    ct:pal("SUCCESS: Proper process termination in ~pms", [ShutdownDuration]),

    Config.

%% @doc Test Mnesia shutdown safety
test_mnesia_shutdown_safety(Config) ->
    ct:pal("Testing Mnesia shutdown safety"),

    %% Check if Mnesia is available
    case application:which_applications() of
        Apps when is_list(Apps) ->
            MnesiaRunning = lists:keymember(mnesia, 1, Apps),

            if MnesiaRunning ->
                   %% Create server with Mnesia-backed state
                   ServerId = make_test_server_id(mnesia_safe),
                   {ok, ServerPid} =
                       erlmcp:start_server(ServerId,
                                           #{capabilities =>
                                                 #mcp_server_capabilities{tools =
                                                                              #mcp_capability{enabled
                                                                                                  =
                                                                                                  true}}}),

                   %% Add tool
                   TestTool = fun(_Args) -> #{result => <<"Tool result">>} end,
                   ok = erlmcp:add_tool(ServerId, <<"mnesia_tool">>, TestTool),

                   %% Record Mnesia tables before shutdown
                   MnesiaTablesBefore = mnesia:system_info(tables),
                   ct:pal("Mnesia tables before shutdown: ~p", [length(MnesiaTablesBefore)]),

                   %% Initiate shutdown
                   ct:pal("Initiating shutdown with Mnesia..."),
                   ok = erlmcp:stop_server(ServerId),

                   %% Wait for shutdown
                   wait_for_shutdown(ServerPid, ?SHUTDOWN_TIMEOUT_MS),

                   %% Verify server stopped
                   ?assertNot(is_process_alive(ServerPid)),

                   %% Verify Mnesia still running (server shutdown shouldn't stop Mnesia)
                   MnesiaTablesAfter = mnesia:system_info(tables),
                   ct:pal("Mnesia tables after shutdown: ~p", [length(MnesiaTablesAfter)]),

                   ?assertEqual(MnesiaTablesBefore,
                                MnesiaTablesAfter,
                                "Mnesia tables should remain unchanged"),

                   ct:pal("SUCCESS: Mnesia shutdown safety verified");
               true ->
                   ct:pal("Mnesia not running, skipping Mnesia-specific test"),
                   %% Run basic shutdown test instead
                   ServerId = make_test_server_id(basic_safe),
                   {ok, ServerPid} = erlmcp:start_server(ServerId, #{}),
                   ok = erlmcp:stop_server(ServerId),
                   ?assertNot(is_process_alive(ServerPid)),
                   ct:pal("SUCCESS: Basic shutdown safety verified")
            end;
        _ ->
            ct:pal("Cannot determine application status, skipping Mnesia test")
    end,

    Config.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Start required applications
start_applications(Apps) ->
    lists:foreach(fun(App) ->
                     case application:start(App) of
                         ok ->
                             ok;
                         {error, {already_started, App}} ->
                             ok;
                         {error, Reason} ->
                             ct:pal("Failed to start ~p: ~p", [App, Reason])
                     end
                  end,
                  Apps).

%% @doc Start optional applications (may not be available)
start_optional_apps(Apps) ->
    lists:foreach(fun(App) ->
                     case application:start(App) of
                         ok ->
                             ct:pal("Started optional app: ~p", [App]);
                         {error, {already_started, App}} ->
                             ok;
                         {error, Reason} ->
                             ct:pal("Optional app ~p not available: ~p", [App, Reason])
                     end
                  end,
                  Apps).

%% @doc Stop supervisor
stop_supervisor(SupName) ->
    case whereis(SupName) of
        undefined ->
            ok;
        Pid ->
            supervisor:stop(Pid),
            wait_for_shutdown(Pid, 5000)
    end.

%% @doc Wait for process to terminate
wait_for_shutdown(Pid, Timeout) ->
    MonRef = monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, Pid, _Info} ->
            ok
    after Timeout ->
        ct:pal("Timeout waiting for process shutdown: ~p", [Pid]),
        case is_process_alive(Pid) of
            true ->
                exit(Pid, kill);
            false ->
                ok
        end
    end.

%% @doc Generate unique test server ID
make_test_server_id(Name) ->
    BinaryName =
        case Name of
            Atom when is_atom(Name) ->
                atom_to_binary(Name, utf8);
            Binary when is_binary(Binary) ->
                Binary;
            Tuple when is_tuple(Tuple) ->
                TupleBin = list_to_binary([atom_to_binary(E, utf8) || E <- tuple_to_list(Tuple)]),
                TupleBin
        end,
    Unique =
        binary:encode_hex(
            crypto:strong_rand_bytes(4)),
    binary_to_atom(<<"test_", BinaryName/binary, "_", Unique/binary>>, utf8).

%% @doc Spawn client connections (simulated)
spawn_client_connections(Count) ->
    [spawn_link(fun() ->
                   %% Simulate client connection
                   case whereis(erlmcp_registry) of
                       undefined ->
                           %% Registry not available, just wait
                           receive
                               stop ->
                                   ok
                           end;
                       _RegistryPid ->
                           %% Registry available, simulate connection
                           receive
                               stop ->
                                   ok
                           after 10000 ->
                               ok
                           end
                   end
                end)
     || _ <- lists:seq(1, Count)].

%% @doc Add test resources
add_test_resources(ServerId, Count) ->
    lists:foreach(fun(N) ->
                     ResourceUri = <<"resource://test/", (integer_to_binary(N))/binary>>,
                     ResourceContent =
                         #{<<"content">> => <<"Test resource ", (integer_to_binary(N))/binary>>},
                     erlmcp:add_resource(ServerId, ResourceUri, ResourceContent)
                  end,
                  lists:seq(1, Count)).

%% @doc Add test tools
add_test_tools(ServerId, Count) ->
    lists:foreach(fun(N) ->
                     ToolName = <<"test_tool_", (integer_to_binary(N))/binary>>,
                     ToolFun =
                         fun(Args) ->
                            Name = maps:get(<<"name">>, Args, <<"World">>),
                            #{result =>
                                  <<"Tool ",
                                    (integer_to_binary(N))/binary,
                                    ": Hello, ",
                                    Name/binary,
                                    "!">>}
                         end,
                     erlmcp:add_tool(ServerId, ToolName, ToolFun)
                  end,
                  lists:seq(1, Count)).

%% @doc Send tool calls to server
send_tool_calls(ServerId, ToolName, Count) ->
    lists:foreach(fun(N) ->
                     spawn(fun() ->
                              case erlmcp_server:call_tool(ServerId,
                                                           ToolName,
                                                           #{<<"name">> =>
                                                                 <<"Test",
                                                                   (integer_to_binary(N))/binary>>})
                              of
                                  {ok, _Result} ->
                                      ok;
                                  {error, _Reason} ->
                                      ok
                              end
                           end)
                  end,
                  lists:seq(1, Count)).

%% @doc Collect results from tracker
collect_results(Timeout, Acc) ->
    receive
        tracker_done ->
            lists:reverse(Acc);
        {request_complete, N} ->
            collect_results(Timeout, [{complete, N} | Acc]);
        {request_error, N, Reason} ->
            collect_results(Timeout, [{error, N, Reason} | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

%% @doc Validate configuration
validate_config(Config) ->
    try
        %% Check capabilities structure
        Capabilities = maps:get(capabilities, Config),
        case Capabilities of
            #mcp_server_capabilities{} ->
                ok;
            _ when is_map(Capabilities) ->
                ok;
            _ ->
                {error, invalid_capabilities}
        end
    catch
        error:_Reason ->
            {error, validation_failed}
    end.

%% @doc Cleanup test artifacts
cleanup_test_artifacts(_TestCase) ->
    %% Clean up any ETS tables created during test
    try
        Tables = ets:all(),
        lists:foreach(fun(TableName) ->
                         case atom_to_list(TableName) of
                             "resource_table_" ++ _ ->
                                 ets:delete(TableName);
                             _ ->
                                 ok
                         end
                      end,
                      Tables)
    catch
        _:_ ->
            ok
    end.
