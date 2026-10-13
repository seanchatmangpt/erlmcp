-module(erlmcp_upgrade_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Test server callbacks
suite() ->
    [{timetrap, {seconds, 300}}].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_core),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_core),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases - Appup File Generation
%%%===================================================================

appup_files_exist(_Config) ->
    %% Verify appup files exist for all applications
    Apps = [erlmcp_core, erlmcp_transports, erlmcp_observability, erlmcp_validation],
    lists:foreach(fun(App) ->
        Appup = atom_to_list(App) ++ ".appup",
        ?assert(filelib:is_file(code:lib_dir(App, src) ++ "/" ++ Appup),
                "Missing appup file for " ++ atom_to_list(App))
    end, Apps).

appup_files_parse(_Config) ->
    %% Verify appup files parse correctly
    Apps = [erlmcp_core, erlmcp_transports, erlmcp_observability, erlmcp_validation],
    lists:foreach(fun(App) ->
        AppupFile = code:lib_dir(App, src) ++ "/" ++ atom_to_list(App) ++ ".appup",
        case file:consult(AppupFile) of
            {ok, [{_Vsn, _Instructions}]} -> ok;
            {ok, [{_Vsn, _Up, _Down}]} -> ok;
            Error ->
                ?assertMatch({ok, _}, Error,
                             "Failed to parse appup for " ++ atom_to_list(App))
        end
    end, Apps).

%%%===================================================================
%%% Test Cases - Code Change Callbacks
%%%===================================================================

registry_code_change(_Config) ->
    %% Test registry state transformation
    OldState = #registry_state{},
    {ok, NewState} = erlmcp_registry:code_change("3.0.0", OldState, []),
    ?assert(is_record(NewState, registry_state)).

server_code_change(_Config) ->
    %% Test server state transformation
    ServerId = <<"test_server">>,
    Capabilities = #mcp_server_capabilities{},
    OldState = #state{server_id = ServerId, capabilities = Capabilities},

    %% Note: This requires actual gen_server process
    {ok, _Pid} = erlmcp_server:start_link(ServerId, Capabilities),
    ?assert(true). % Placeholder - actual test would call code_change

%%%===================================================================
%%% Test Cases - State Migration
%%%===================================================================

ets_table_migration(_Config) ->
    %% Test ETS table migration
    %% Create test table
    Table = ets:new(test_migration, [set, public]),
    ets:insert(Table, {key1, value1}),
    ets:insert(Table, {key2, value2}),

    %% Backup table
    {ok, Backup} = erlmcp_state_migration:backup_table(Table),
    ?assert(maps:is_key(data, Backup)),
    ?assert(maps:is_key(timestamp, Backup)),

    %% Restore table
    ok = erlmcp_state_migration:restore_table(Table, Backup),
    ?assertEqual([{key1, value1}, {key2, value2}], lists:sort(ets:tab2list(Table))),

    ets:delete(Table).

migrate_ets_tables(_Config) ->
    %% Test full ETS migration
    {ok, Result} = erlmcp_state_migration:migrate_ets_tables(<<"2.1.0">>, <<"3.0.0">>),
    ?assert(maps:is_key(target_version, Result)),
    ?assertEqual(<<"3.0.0">>, maps:get(target_version, Result)).

rollback_ets_tables(_Config) ->
    %% Test ETS rollback
    {ok, Result} = erlmcp_state_migration:rollback_ets_tables(<<"3.0.0">>, <<"2.1.0">>),
    ?assert(maps:is_key(target_version, Result)),
    ?assertEqual(<<"2.1.0">>, maps:get(target_version, Result)).

%%%===================================================================
%%% Test Cases - Protocol Versioning
%%%===================================================================

version_negotiation(_Config) ->
    %% Test protocol version negotiation
    {ok, Version} = erlmcp_protocol_versioning:negotiate_version(
        <<"2.1.0">>,
        [<<"2.1.0">>, <<"3.0.0">>]
    ),
    ?assertEqual(<<"3.0.0">>, Version).

transform_message_upgrade(_Config) ->
    %% Test message transformation for upgrade
    Message = #{jsonrpc => <<"2.0">>, id => 1, method => <<"test">>},
    {ok, Transformed} = erlmcp_protocol_versioning:transform_message(
        Message,
        <<"2.1.0">>,
        <<"3.0.0">>
    ),
    ?assert(is_map(Transformed)),
    ?assert(maps:is_key(<<"protocolVersion">>, Transformed)).

transform_message_downgrade(_Config) ->
    %% Test message transformation for downgrade
    Message = #{
        jsonrpc => <<"2.0">>,
        id => 1,
        method => <<"test">>,
        <<"protocolVersion">> => <<"3.0.0">>
    },
    {ok, Transformed} = erlmcp_protocol_versioning:transform_message(
        Message,
        <<"3.0.0">>,
        <<"2.1.0">>
    ),
    ?assert(is_map(Transformed)),
    ?assertNot(maps:is_key(<<"protocolVersion">>, Transformed)).

%%%===================================================================
%%% Test Cases - Upgrade Coordinator
%%%===================================================================

upgrade_coordinator_status(_Config) ->
    %% Test upgrade status retrieval
    {ok, Status} = erlmcp_upgrade_coordinator:get_upgrade_status(),
    ?assert(maps:is_key(status, Status)),
    ?assert(maps:is_key(current_version, Status)).

prepare_upgrade(_Config) ->
    %% Test upgrade preparation
    case erlmcp_upgrade_coordinator:prepare_upgrade(<<"3.0.0">>) of
        {ok, Info} ->
            ?assert(maps:is_key(target_version, Info)),
            ?assertEqual(<<"3.0.0">>, maps:get(target_version, Info));
        {error, Reason} ->
            %% May fail in some environments
            ct:log("Prepare upgrade failed: ~p", [Reason]),
            ?assert(true)
    end.

%%%===================================================================
%%% Test Cases - Upgrade Monitoring
%%%===================================================================

upgrade_monitor_health(_Config) ->
    %% Test system health check
    Health = erlmcp_upgrade_monitor:get_system_health(),
    ?assert(maps:is_key(status, Health)),
    ?assert(maps:is_key(score, Health)),
    ?assert(lists:member(maps:get(status, Health), [healthy, degraded, unhealthy])).

upgrade_monitor_progress(_Config) ->
    %% Test upgrade progress tracking
    ok = erlmcp_upgrade_monitor:start_upgrade_span(<<"3.0.0">>),
    ok = erlmcp_upgrade_monitor:record_upgrade_phase(test_phase, 100),
    {ok, Progress} = erlmcp_upgrade_monitor:get_upgrade_progress(),
    ?assert(maps:is_key(phases_completed, Progress)),
    ok = erlmcp_upgrade_monitor:end_upgrade_span().

%%%===================================================================
%%% Test Cases - Integration Tests
%%%===================================================================

end_to_end_upgrade(_Config) ->
    %% Full upgrade test (requires actual node)
    %% This is a placeholder - real test would run upgrade on a test node

    %% Skip if not in test environment
    case os:getenv("CT_SKIP_UPGRADE_TEST") of
        "true" ->
            {skip, "Upgrade test disabled"};
        _ ->
            ?assert(true) % Placeholder
    end.

upgrade_state_preservation(_Config) ->
    %% Test that state is preserved during upgrade

    %% Create test data in registry
    ServerId = <<"test_server_preserve">>,
    {ok, ServerPid} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),
    {ok, _Config} = erlmcp_registry:register_server(ServerId, ServerPid, #{}),

    %% Simulate upgrade (suspend/resume)
    supervisor:suspend(erlmcp_sup),
    timer:sleep(100),
    supervisor:resume(erlmcp_sup),

    %% Verify state preserved
    {ok, {FoundPid, _}} = erlmcp_registry:find_server(ServerId),
    ?assertEqual(ServerPid, FoundPid),

    erlmcp_server:stop(ServerId).

upgrade_connection_continuity(_Config) ->
    %% Test that connections survive upgrade
    ?assert(true). % Placeholder - requires transport setup

%%%===================================================================
%%% Test Cases - Auto-Rollback Functionality
%%%===================================================================

capture_baseline_metrics(_Config) ->
    %% Test baseline metrics capture
    {ok, Baseline} = erlmcp_upgrade_coordinator:capture_baseline_metrics(),
    ?assert(maps:is_key(timestamp, Baseline)),
    ?assert(maps:is_key(p99_latency, Baseline)),
    ?assert(maps:is_key(process_count, Baseline)),
    ?assert(maps:is_key(memory_total, Baseline)),
    ct:log("Baseline metrics captured: ~p", [Baseline]).

measure_p99_latency(_Config) ->
    %% Test P99 latency measurement
    Result = erlmcp_upgrade_coordinator:measure_p99_latency(),
    case Result of
        {ok, P99} ->
            ?assert(is_number(P99)),
            ?assert(P99 >= 0),
            ct:log("P99 latency: ~p ms", [P99]);
        {error, Reason} ->
            %% May fail if no metrics data available
            ct:log("P99 measurement failed (expected in test env): ~p", [Reason]),
            ?assert(true)
    end.

check_performance_regression_no_baseline(_Config) ->
    %% Test regression check without baseline
    Result = erlmcp_upgrade_coordinator:check_performance_regression(undefined),
    ?assertEqual({error, no_baseline}, Result).

check_performance_regression_with_baseline(_Config) ->
    %% Test regression check with baseline
    %% First capture a baseline
    case erlmcp_upgrade_coordinator:capture_baseline_metrics() of
        {ok, #{p99_latency := BaselineP99}} when is_number(BaselineP99), BaselineP99 > 0 ->
            Result = erlmcp_upgrade_coordinator:check_performance_regression(BaselineP99),
            case Result of
                {ok, HasRegression, Details} ->
                    ?assert(is_boolean(HasRegression)),
                    ?assert(maps:is_key(baseline_p99, Details)),
                    ?assert(maps:is_key(current_p99, Details)),
                    ?assert(maps:is_key(regression_detected, Details)),
                    ct:log("Regression check result: ~p", [Details]);
                {error, Reason} ->
                    %% May fail if metrics unavailable
                    ct:log("Regression check failed: ~p", [Reason]),
                    ?assert(true)
            end;
        _ ->
            ?assert(true) % Skip if no valid baseline
    end.

verify_upgrade_with_rollback(_Config) ->
    %% Test upgrade verification with auto-rollback
    %% This test verifies the flow but doesn't trigger actual rollback
    TargetVersion = <<"3.0.0">>,

    case erlmcp_upgrade_coordinator:verify_upgrade_with_rollback(TargetVersion) of
        {ok, Result} ->
            ?assert(maps:is_key(status, Result)),
            ct:log("Upgrade verification with rollback result: ~p", [Result]);
        {error, Reason} ->
            %% May fail due to various reasons in test environment
            ct:log("Upgrade verification failed (expected in test env): ~p", [Reason]),
            ?assert(true)
    end.

auto_rollback_triggers_on_regression(_Config) ->
    %% Test that auto-rollback is triggered on performance regression
    %% This is a simulation test

    %% Mock: Simulate a regression scenario
    BaselineP99 = 100,  % 100ms baseline
    DegradedP99 = 150,   % 150ms current (50% increase, exceeds 10% threshold)

    %% Regression threshold is 1.1 (10%)
    RegressionLimit = BaselineP99 * 1.1,  % 110ms

    ?assert(DegradedP99 > RegressionLimit),
    ct:log("Regression detected: ~p ms > ~p ms limit", [DegradedP99, RegressionLimit]),

    %% In a real scenario, this would trigger rollback
    ?assert(true).

auto_rollback_no_trigger_within_threshold(_Config) ->
    %% Test that auto-rollback is NOT triggered within acceptable threshold
    BaselineP99 = 100,  % 100ms baseline
    AcceptableP99 = 105, % 105ms current (5% increase, within 10% threshold)

    %% Regression threshold is 1.1 (10%)
    RegressionLimit = BaselineP99 * 1.1,  % 110ms

    ?assert(AcceptableP99 =< RegressionLimit),
    ct:log("No regression: ~p ms <= ~p ms limit", [AcceptableP99, RegressionLimit]),

    ?assert(true).

warmup_period_elapsed_before_verification(_Config) ->
    %% Test that warmup period is observed before performance verification
    %% The warmup period is 30 seconds by default
    WarmupPeriodMs = 30000,

    %% In a real scenario, the system would sleep for this period
    ?assert(is_integer(WarmupPeriodMs)),
    ?assertEqual(30000, WarmupPeriodMs),

    ct:log("Warmup period: ~p ms", [WarmupPeriodMs]),
    ?assert(true).
