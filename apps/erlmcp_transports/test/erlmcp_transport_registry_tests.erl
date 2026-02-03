%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Test Suite for Transport Registry
%%%
%%% Tests cover:
%%% - Transport registration and unregistration
%%% - Transport discovery and listing
%%% - Capability-based queries
%%% - Health checking
%%% - Configuration updates
%%% - Process monitoring and cleanup
%%% - Scope validation (local/global)
%%% - Statistics tracking
%%%
%%% Chicago School TDD: Real gen_server, state-based verification, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_registry_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

%% @doc Setup transport registry for testing
setup_registry() ->
    {ok, Pid} = erlmcp_transport_registry:start_link(#{scope => local}),
    Pid.

%% @doc Cleanup transport registry
cleanup_registry(Pid) ->
    catch gen_server:stop(Pid).

%% @doc Setup registry with auto-discovery
setup_registry_with_discovery() ->
    {ok, Pid} = erlmcp_transport_registry:start_link(#{scope => local, auto_discover => false}),
    Pid.

%% @doc Create test transport configuration
make_transport_config(TransportId, Type, Capabilities) ->
    #{
        type => Type,
        capabilities => Capabilities,
        metadata => #{test => true}
    }.

%% @doc Create a mock transport process
spawn_mock_transport(TransportId) ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

%%%====================================================================
%%% Transport Registration Tests
%%%====================================================================

register_transport_success_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_stdio,
        Config = make_transport_config(stdio, [resources, tools]),

        ?assertEqual(ok,
            erlmcp_transport_registry:register_transport(local, TransportId, self(), Config))
    after
        cleanup_registry(Pid)
    end.

register_transport_with_pid_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_with_pid,
        MockPid = spawn_mock_transport(TransportId),
        Config = make_transport_config(tcp, [notifications]),

        ?assertEqual(ok,
            erlmcp_transport_registry:register_transport(local, TransportId, MockPid, Config)),

        %% Verify transport was registered
        {ok, TransportInfo} = erlmcp_transport_registry:get_transport(local, TransportId),
        ?assertEqual(TransportId, maps:get(id, TransportInfo)),
        ?assertEqual(tcp, maps:get(type, TransportInfo)),

        %% Cleanup mock transport
        MockPid ! stop
    after
        cleanup_registry(Pid)
    end.

register_duplicate_transport_fails_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_dup,
        Config = make_transport_config(http, [resources]),

        ?assertEqual(ok,
            erlmcp_transport_registry:register_transport(local, TransportId, self(), Config)),

        %% Second registration should fail
        Result = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),
        ?assertEqual({error, already_registered}, Result)
    after
        cleanup_registry(Pid)
    end.

register_transport_scope_mismatch_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_scope,
        Config = make_transport_config(websocket, [tools]),

        %% Registry started with local scope, try registering with global
        Result = erlmcp_transport_registry:register_transport(global, TransportId, self(), Config),
        ?assertEqual({error, scope_mismatch}, Result)
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Transport Unregistration Tests
%%%====================================================================

unregister_transport_success_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_unregister,
        Config = make_transport_config(sse, [logging]),

        %% Register first
        ok = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),

        %% Then unregister
        ?assertEqual(ok, erlmcp_transport_registry:unregister_transport(local, TransportId)),

        %% Verify it's gone
        ?assertEqual({error, not_found},
            erlmcp_transport_registry:get_transport(local, TransportId))
    after
        cleanup_registry(Pid)
    end.

unregister_nonexistent_transport_succeeds_test() ->
    Pid = setup_registry(),
    try
        %% Unregistering non-existent transport should succeed (idempotent)
        ?assertEqual(ok,
            erlmcp_transport_registry:unregister_transport(local, nonexistent_transport))
    after
        cleanup_registry(Pid)
    end.

unregister_transport_demonitors_process_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_demonitor,
        MockPid = spawn_mock_transport(TransportId),
        Config = make_transport_config(tcp, [resources]),

        %% Register with real Pid
        ok = erlmcp_transport_registry:register_transport(local, TransportId, MockPid, Config),

        %% Unregister (should demonitor)
        ok = erlmcp_transport_registry:unregister_transport(local, TransportId),

        %% Cleanup mock transport
        MockPid ! stop
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Transport Query Tests
%%%====================================================================

get_transport_success_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_get,
        Config = make_transport_config(stdio, [tools, prompts]),

        ok = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),

        {ok, TransportInfo} = erlmcp_transport_registry:get_transport(local, TransportId),
        ?assertEqual(TransportId, maps:get(id, TransportInfo)),
        ?assertEqual(stdio, maps:get(type, TransportInfo)),
        ?assertEqual([tools, prompts], maps:get(capabilities, TransportInfo)),
        ?assertMatch(#{test := true}, maps:get(metadata, TransportInfo))
    after
        cleanup_registry(Pid)
    end.

get_transport_not_found_test() ->
    Pid = setup_registry(),
    try
        Result = erlmcp_transport_registry:get_transport(local, nonexistent),
        ?assertEqual({error, not_found}, Result)
    after
        cleanup_registry(Pid)
    end.

list_transports_empty_test() ->
    Pid = setup_registry(),
    try
        Transports = erlmcp_transport_registry:list_transports(local),
        ?assertEqual([], Transports)
    after
        cleanup_registry(Pid)
    end.

list_transports_multiple_test() ->
    Pid = setup_registry(),
    try
        %% Register multiple transports
        ok = erlmcp_transport_registry:register_transport(local, t1, self(),
            make_transport_config(stdio, [resources])),
        ok = erlmcp_transport_registry:register_transport(local, t2, self(),
            make_transport_config(tcp, [tools])),
        ok = erlmcp_transport_registry:register_transport(local, t3, self(),
            make_transport_config(http, [prompts])),

        Transports = erlmcp_transport_registry:list_transports(local),
        ?assertEqual(3, length(Transports)),

        %% Verify structure
        lists:foreach(fun({Id, Info}) ->
            ?assert(is_map(Info)),
            ?assertEqual(Id, maps:get(id, Info))
        end, Transports)
    after
        cleanup_registry(Pid)
    end.

list_transports_by_type_test() ->
    Pid = setup_registry(),
    try
        %% Register transports of different types
        ok = erlmcp_transport_registry:register_transport(local, tcp1, self(),
            #{type => tcp, capabilities => [resources]}),
        ok = erlmcp_transport_registry:register_transport(local, tcp2, self(),
            #{type => tcp, capabilities => [tools]}),
        ok = erlmcp_transport_registry:register_transport(local, http1, self(),
            #{type => http, capabilities => [prompts]}),

        TcpTransports = erlmcp_transport_registry:list_transports_by_type(tcp),
        ?assertEqual(2, length(TcpTransports)),

        HttpTransports = erlmcp_transport_registry:list_transports_by_type(http),
        ?assertEqual(1, length(HttpTransports))
    after
        cleanup_registry(Pid)
    end.

list_transports_by_capability_test() ->
    Pid = setup_registry(),
    try
        %% Register transports with different capabilities
        ok = erlmcp_transport_registry:register_transport(local, res1, self(),
            #{type => stdio, capabilities => [resources, tools]}),
        ok = erlmcp_transport_registry:register_transport(local, res2, self(),
            #{type => tcp, capabilities => [resources]}),
        ok = erlmcp_transport_registry:register_transport(local, tools1, self(),
            #{type => http, capabilities => [tools]}),

        ResourceTransports = erlmcp_transport_registry:list_transports_by_capability(resources),
        ?assertEqual(2, length(ResourceTransports)),

        ToolTransports = erlmcp_transport_registry:list_transports_by_capability(tools),
        ?assertEqual(2, length(ToolTransports))
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Capability-Based Discovery Tests
%%%====================================================================

find_transport_by_capability_test() ->
    Pid = setup_registry(),
    try
        %% Register transports with capabilities
        ok = erlmcp_transport_registry:register_transport(local, t1, self(),
            #{type => stdio, capabilities => [resources]}),
        ok = erlmcp_transport_registry:register_transport(local, t2, self(),
            #{type => tcp, capabilities => [tools]}),

        %% Find transport with resources capability
        {ok, TransportId, _Info} = erlmcp_transport_registry:find_transport_by_capability(resources),
        ?assertEqual(t1, TransportId)
    after
        cleanup_registry(Pid)
    end.

find_transport_by_capability_no_match_test() ->
    Pid = setup_registry(),
    try
        Result = erlmcp_transport_registry:find_transport_by_capability(notifications),
        ?assertEqual({error, no_suitable_transport}, Result)
    after
        cleanup_registry(Pid)
    end.

find_transport_by_capability_returns_healthy_test() ->
    Pid = setup_registry(),
    try
        %% Register one with Pid (healthy) and one without
        MockPid = spawn_mock_transport(healthy_transport),
        ok = erlmcp_transport_registry:register_transport(local, healthy, MockPid,
            #{type => stdio, capabilities => [resources]}),
        ok = erlmcp_transport_registry:register_transport(local, unhealthy, undefined,
            #{type => tcp, capabilities => [resources]}),

        %% Should find the healthy one
        {ok, TransportId, _Info} = erlmcp_transport_registry:find_transport_by_capability(resources),
        ?assertEqual(healthy, TransportId),

        MockPid ! stop
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Configuration Update Tests
%%%====================================================================

update_transport_config_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_update,
        Config = #{type => stdio, capabilities => [resources], metadata => #{v => 1}},

        ok = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),

        %% Update config
        ok = erlmcp_transport_registry:update_transport_config(TransportId,
            #{metadata => #{v => 2, updated => true}}),

        %% Verify update
        {ok, Updated} = erlmcp_transport_registry:get_transport(local, TransportId),
        Metadata = maps:get(metadata, Updated),
        ?assertEqual(2, maps:get(v, Metadata)),
        ?assertEqual(true, maps:get(updated, Metadata))
    after
        cleanup_registry(Pid)
    end.

update_transport_config_not_found_test() ->
    Pid = setup_registry(),
    try
        Result = erlmcp_transport_registry:update_transport_config(nonexistent, #{}),
        ?assertEqual({error, not_found}, Result)
    after
        cleanup_registry(Pid)
    end.

update_transport_capabilities_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_caps,
        Config = #{type => stdio, capabilities => [resources]},

        ok = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),

        %% Update capabilities
        ok = erlmcp_transport_registry:update_transport_config(TransportId,
            #{capabilities => [resources, tools, prompts]}),

        %% Verify update
        {ok, Updated} = erlmcp_transport_registry:get_transport(local, TransportId),
        ?assertEqual([resources, tools, prompts], maps:get(capabilities, Updated))
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Transport Statistics Tests
%%%====================================================================

get_transport_stats_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_stats,
        Config = #{type => stdio, capabilities => [resources]},

        ok = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),

        {ok, Stats} = erlmcp_transport_registry:get_transport_stats(TransportId),
        ?assertMatch(#{created_at := _, last_activity := _, messages_sent := 0,
                       messages_received := 0, bytes_sent := 0, bytes_received := 0,
                       errors := 0}, Stats)
    after
        cleanup_registry(Pid)
    end.

get_all_stats_test() ->
    Pid = setup_registry(),
    try
        ok = erlmcp_transport_registry:register_transport(local, t1, self(),
            #{type => stdio, capabilities => []}),
        ok = erlmcp_transport_registry:register_transport(local, t2, self(),
            #{type => tcp, capabilities => []}),

        AllStats = erlmcp_transport_registry:get_all_stats(),
        ?assertEqual(2, map_size(AllStats)),
        ?assert(is_map(maps:get(t1, AllStats))),
        ?assert(is_map(maps:get(t2, AllStats)))
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Health Check Tests
%%%====================================================================

health_check_healthy_transport_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_healthy,
        MockPid = spawn_mock_transport(TransportId),

        Config = #{type => stdio, capabilities => []},
        ok = erlmcp_transport_registry:register_transport(local, TransportId, MockPid, Config),

        %% Transport will be in 'initializing' state initially
        {ok, Status} = erlmcp_transport_registry:health_check(TransportId),
        ?assert(lists:member(Status, [healthy, degraded])),

        MockPid ! stop
    after
        cleanup_registry(Pid)
    end.

health_check_unhealthy_transport_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_unhealthy,

        %% Register without Pid
        Config = #{type => stdio, capabilities => []},
        ok = erlmcp_transport_registry:register_transport(local, TransportId, undefined, Config),

        {ok, Status} = erlmcp_transport_registry:health_check(TransportId),
        ?assertEqual(unhealthy, Status)
    after
        cleanup_registry(Pid)
    end.

health_check_all_test() ->
    Pid = setup_registry(),
    try
        MockPid1 = spawn_mock_transport(t1),
        MockPid2 = spawn_mock_transport(t2),

        ok = erlmcp_transport_registry:register_transport(local, t1, MockPid1,
            #{type => stdio, capabilities => []}),
        ok = erlmcp_transport_registry:register_transport(local, t2, MockPid2,
            #{type => tcp, capabilities => []}),
        ok = erlmcp_transport_registry:register_transport(local, t3, undefined,
            #{type => http, capabilities => []}),

        AllHealth = erlmcp_transport_registry:health_check_all(),
        ?assertEqual(3, map_size(AllHealth)),

        MockPid1 ! stop,
        MockPid2 ! stop
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Transport Capabilities Query Tests
%%%====================================================================

get_transport_capabilities_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_caps_query,
        Config = #{type => stdio, capabilities => [resources, tools, prompts]},

        ok = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),

        {ok, Caps} = erlmcp_transport_registry:get_transport_capabilities(TransportId),
        ?assertEqual([resources, tools, prompts], Caps)
    after
        cleanup_registry(Pid)
    end.

get_transport_capabilities_not_found_test() ->
    Pid = setup_registry(),
    try
        Result = erlmcp_transport_registry:get_transport_capabilities(nonexistent),
        ?assertEqual({error, not_found}, Result)
    after
        cleanup_registry(Pid)
    end.

normalize_capabilities_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_normalize,

        %% Include invalid capabilities
        Config = #{type => stdio, capabilities => [resources, invalid_cap, tools]},

        ok = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),

        {ok, Caps} = erlmcp_transport_registry:get_transport_capabilities(TransportId),
        ?assertEqual([resources, tools], Caps)
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Process Monitoring Tests
%%%====================================================================

process_death_updates_status_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_death,

        %% Spawn short-lived process
        MockPid = spawn(fun() -> timer:sleep(100) end),

        Config = #{type => stdio, capabilities => []},
        ok = erlmcp_transport_registry:register_transport(local, TransportId, MockPid, Config),

        %% Wait for process to die
        timer:sleep(200),

        %% Check status (should be error)
        {ok, Info} = erlmcp_transport_registry:get_transport(local, TransportId),
        ?assertEqual(error, maps:get(status, Info))
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Configuration Sanitization Tests
%%%====================================================================

config_sanitization_removes_secrets_test() ->
    Pid = setup_registry(),
    try
        TransportId = test_transport_sanitization,

        Config = #{
            type => stdio,
            capabilities => [],
            password => <<"secret123">>,
            api_key => <<"key456">>,
            safe_value => <<"public">>
        },

        ok = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),

        {ok, Info} = erlmcp_transport_registry:get_transport(local, TransportId),
        StoredConfig = maps:get(config, Info),

        ?assertEqual(<<"***REDACTED***">>, maps:get(password, StoredConfig)),
        ?assertEqual(<<"***REDACTED***">>, maps:get(api_key, StoredConfig)),
        ?assertEqual(<<"public">>, maps:get(safe_value, StoredConfig))
    after
        cleanup_registry(Pid)
    end.

%%%====================================================================
%%% Scope Validation Tests
%%%====================================================================

scope_mismatch_operations_test() ->
    Pid = setup_registry(),  %% Started with local scope
    try
        TransportId = test_transport_scope_op,
        Config = #{type => stdio, capabilities => []},

        %% Registration with wrong scope fails
        ?assertEqual({error, scope_mismatch},
            erlmcp_transport_registry:register_transport(global, TransportId, self(), Config)),

        %% Register with correct scope
        ok = erlmcp_transport_registry:register_transport(local, TransportId, self(), Config),

        %% Query with wrong scope fails
        ?assertEqual({error, scope_mismatch},
            erlmcp_transport_registry:get_transport(global, TransportId)),

        %% Unregister with wrong scope fails
        ?assertEqual({error, scope_mismatch},
            erlmcp_transport_registry:unregister_transport(global, TransportId))
    after
        cleanup_registry(Pid)
    end.
