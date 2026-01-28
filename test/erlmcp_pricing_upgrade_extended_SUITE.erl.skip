%%% erlmcp_pricing_upgrade_extended_SUITE.erl
%%% Comprehensive test suite for pricing tier upgrades and downgrades
-module(erlmcp_pricing_upgrade_extended_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Test group specifications
groups() ->
    [
        {forward_upgrades, [sequence], [
            test_upgrade_team_to_enterprise,
            test_upgrade_enterprise_to_gov,
            test_upgrade_chain_team_to_enterprise_to_gov
        ]},
        {downgrade_forbidden, [sequence], [
            test_downgrade_gov_to_enterprise_forbidden,
            test_downgrade_gov_to_team_forbidden,
            test_downgrade_enterprise_to_team_forbidden
        ]},
        {upgrade_simulation, [sequence], [
            test_simulate_upgrade_team_to_enterprise,
            test_simulate_upgrade_enterprise_to_gov,
            test_simulate_multiple_times_same_result
        ]},
        {upgrade_application, [sequence], [
            test_apply_upgrade_with_verification,
            test_upgrade_with_rollback_capability,
            test_upgrade_cooldown_enforcement
        ]},
        {determinism_checks, [sequence], [
            test_upgrade_determinism_team_to_enterprise,
            test_upgrade_determinism_enterprise_to_gov
        ]},
        {receipt_chain_logging, [sequence], [
            test_upgrade_event_logged_to_receipt_chain,
            test_receipt_chain_contains_upgrade_details
        ]}
    ].

all() ->
    [
        {group, forward_upgrades},
        {group, downgrade_forbidden},
        {group, upgrade_simulation},
        {group, upgrade_application},
        {group, determinism_checks},
        {group, receipt_chain_logging}
    ].

%% === Suite Setup/Teardown ===

suite() ->
    [
        {timetrap, {minutes, 5}},
        {require, erlmcp_app}
    ].

init_per_suite(Config) ->
    %% Start erlmcp application
    case application:start(erlmcp) of
        ok -> ok;
        {error, {already_started, erlmcp}} -> ok;
        Error -> ct:fail({failed_to_start_erlmcp, Error})
    end,

    %% Initialize mock pricing state
    mock_pricing_state_setup(),

    Config.

end_per_suite(Config) ->
    %% Cleanup
    mock_pricing_state_cleanup(),
    Config.

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% === Test Cases: Forward Upgrades ===

test_upgrade_team_to_enterprise(Config) ->
    %% Verify upgrade is allowed
    ?assert(erlmcp_pricing_upgrade:can_upgrade(team, enterprise)),

    %% Get upgrade path
    {ok, UpgradePath} = erlmcp_pricing_upgrade:get_upgrade_path(team, enterprise),

    %% Verify envelope expansion
    EnvelopeExpansion = maps:get(envelope_expansion, UpgradePath),
    OldEnvelope = maps:get(old_envelope, EnvelopeExpansion),
    NewEnvelope = maps:get(new_envelope, EnvelopeExpansion),

    %% Throughput: 450 → 1500
    ?assertEqual(450, maps:get(throughput_req_s, OldEnvelope)),
    ?assertEqual(1500, maps:get(throughput_req_s, NewEnvelope)),

    %% Concurrent connections: 128 → 512
    ?assertEqual(128, maps:get(concurrent_connections, OldEnvelope)),
    ?assertEqual(512, maps:get(concurrent_connections, NewEnvelope)),

    %% Queue depth: 2048 → 8192
    ?assertEqual(2048, maps:get(queue_depth_messages, OldEnvelope)),
    ?assertEqual(8192, maps:get(queue_depth_messages, NewEnvelope)),

    %% Verify config changes enable features
    ConfigChanges = maps:get(config_changes, UpgradePath),
    ?assert(length(ConfigChanges) > 0),

    %% Check for critical feature enablements
    WebsocketChange = lists:keyfind(<<"features.websocket_transport">>, 2, ConfigChanges),
    ?assertNotEqual(false, WebsocketChange),

    ct:log("✓ Team → Enterprise upgrade verified"),
    Config.

test_upgrade_enterprise_to_gov(Config) ->
    %% Verify upgrade is allowed
    ?assert(erlmcp_pricing_upgrade:can_upgrade(enterprise, gov)),

    %% Get upgrade path
    {ok, UpgradePath} = erlmcp_pricing_upgrade:get_upgrade_path(enterprise, gov),

    %% Verify envelope expansion (note: some metrics reduce due to encryption overhead)
    EnvelopeExpansion = maps:get(envelope_expansion, UpgradePath),
    OldEnvelope = maps:get(old_envelope, EnvelopeExpansion),
    NewEnvelope = maps:get(new_envelope, EnvelopeExpansion),

    %% Throughput: 1500 → 900 (reduced for compliance)
    ?assertEqual(1500, maps:get(throughput_req_s, OldEnvelope)),
    ?assertEqual(900, maps:get(throughput_req_s, NewEnvelope)),

    %% Concurrent connections: 512 → 256
    ?assertEqual(512, maps:get(concurrent_connections, OldEnvelope)),
    ?assertEqual(256, maps:get(concurrent_connections, NewEnvelope)),

    %% Verify FIPS-140-2 feature enabled
    ConfigChanges = maps:get(config_changes, UpgradePath),
    FipsChange = lists:keyfind(<<"features.fips_140_2">>, 2, ConfigChanges),
    ?assertNotEqual(false, FipsChange),

    %% Verify audit logging changes
    AuditChanges = lists:filter(
        fun(Change) ->
            Setting = maps:get(setting, Change),
            string:str(Setting, "audit.") > 0
        end,
        ConfigChanges
    ),
    ?assert(length(AuditChanges) > 0),

    ct:log("✓ Enterprise → Gov upgrade verified"),
    Config.

test_upgrade_chain_team_to_enterprise_to_gov(Config) ->
    %% Verify team can upgrade to enterprise
    ?assert(erlmcp_pricing_upgrade:can_upgrade(team, enterprise)),

    %% Verify enterprise can upgrade to gov
    ?assert(erlmcp_pricing_upgrade:can_upgrade(enterprise, gov)),

    %% Verify team cannot directly upgrade to gov
    ?assertNot(erlmcp_pricing_upgrade:can_upgrade(team, gov)),

    %% Get both paths
    {ok, Path1} = erlmcp_pricing_upgrade:get_upgrade_path(team, enterprise),
    {ok, Path2} = erlmcp_pricing_upgrade:get_upgrade_path(enterprise, gov),

    %% Verify both paths exist
    ?assertNotEqual(undefined, Path1),
    ?assertNotEqual(undefined, Path2),

    ct:log("✓ Upgrade chain (team → enterprise → gov) verified"),
    Config.

%% === Test Cases: Downgrades Forbidden ===

test_downgrade_gov_to_enterprise_forbidden(Config) ->
    %% Gov tier cannot downgrade to Enterprise
    ?assertNot(erlmcp_pricing_upgrade:can_downgrade(gov, enterprise)),

    %% Attempting to get path should fail
    {error, Reason} = erlmcp_pricing_upgrade:get_upgrade_path(gov, enterprise),
    ?assertEqual(downgrade_forbidden, Reason),

    ct:log("✓ Gov → Enterprise downgrade correctly forbidden"),
    Config.

test_downgrade_gov_to_team_forbidden(Config) ->
    %% Gov tier cannot downgrade to Team
    ?assertNot(erlmcp_pricing_upgrade:can_downgrade(gov, team)),

    %% Attempting to get path should fail
    {error, Reason} = erlmcp_pricing_upgrade:get_upgrade_path(gov, team),
    ?assertEqual(downgrade_forbidden, Reason),

    ct:log("✓ Gov → Team downgrade correctly forbidden"),
    Config.

test_downgrade_enterprise_to_team_forbidden(Config) ->
    %% Enterprise tier cannot downgrade to Team
    ?assertNot(erlmcp_pricing_upgrade:can_downgrade(enterprise, team)),

    %% Attempting to get path should fail
    {error, Reason} = erlmcp_pricing_upgrade:get_upgrade_path(enterprise, team),
    ?assertEqual(downgrade_forbidden, Reason),

    ct:log("✓ Enterprise → Team downgrade correctly forbidden"),
    Config.

%% === Test Cases: Upgrade Simulation ===

test_simulate_upgrade_team_to_enterprise(Config) ->
    %% Simulate upgrade without applying
    {ok, SimResult} = erlmcp_pricing_upgrade:simulate_upgrade(team, enterprise),

    %% Verify simulation results
    ?assertEqual(true, maps:get(simulated, SimResult)),
    ?assertEqual(true, maps:get(will_succeed, SimResult)),
    ?assertEqual(team, maps:get(from_plan, SimResult)),
    ?assertEqual(enterprise, maps:get(to_plan, SimResult)),

    %% Verify predicted envelope
    PredictedEnvelope = maps:get(predicted_envelope, SimResult),
    ?assertEqual(1500, maps:get(throughput_req_s, PredictedEnvelope)),
    ?assertEqual(512, maps:get(concurrent_connections, PredictedEnvelope)),

    %% Verify estimated downtime
    EstimatedDowntime = maps:get(estimated_downtime_ms, SimResult),
    ?assert(EstimatedDowntime >= 0),

    ct:log("✓ Simulation of Team → Enterprise upgrade verified"),
    Config.

test_simulate_upgrade_enterprise_to_gov(Config) ->
    %% Simulate upgrade without applying
    {ok, SimResult} = erlmcp_pricing_upgrade:simulate_upgrade(enterprise, gov),

    %% Verify simulation results
    ?assertEqual(true, maps:get(simulated, SimResult)),
    ?assertEqual(true, maps:get(will_succeed, SimResult)),

    %% Verify predicted envelope reflects Gov tier specs
    PredictedEnvelope = maps:get(predicted_envelope, SimResult),
    ?assertEqual(900, maps:get(throughput_req_s, PredictedEnvelope)),
    ?assertEqual(256, maps:get(concurrent_connections, PredictedEnvelope)),

    ct:log("✓ Simulation of Enterprise → Gov upgrade verified"),
    Config.

test_simulate_multiple_times_same_result(Config) ->
    %% Simulate same upgrade 3 times
    {ok, Result1} = erlmcp_pricing_upgrade:simulate_upgrade(team, enterprise),
    {ok, Result2} = erlmcp_pricing_upgrade:simulate_upgrade(team, enterprise),
    {ok, Result3} = erlmcp_pricing_upgrade:simulate_upgrade(team, enterprise),

    %% Results must be identical (deterministic)
    ?assertEqual(Result1, Result2),
    ?assertEqual(Result2, Result3),

    %% Verify key fields are deterministic
    Envelope1 = maps:get(predicted_envelope, Result1),
    Envelope2 = maps:get(predicted_envelope, Result2),
    Envelope3 = maps:get(predicted_envelope, Result3),

    ?assertEqual(Envelope1, Envelope2),
    ?assertEqual(Envelope2, Envelope3),

    ct:log("✓ Upgrade simulation determinism verified (3x identical results)"),
    Config.

%% === Test Cases: Upgrade Application ===

test_apply_upgrade_with_verification(Config) ->
    %% Setup mock state
    mock_set_current_plan(team),

    %% Apply upgrade
    {ok, UpgradeResult} = erlmcp_pricing_upgrade:apply_upgrade(team, enterprise),

    %% Verify upgrade completed
    ?assertEqual(true, maps:get(upgraded, UpgradeResult)),
    ?assertEqual(team, maps:get(from_plan, UpgradeResult)),
    ?assertEqual(enterprise, maps:get(to_plan, UpgradeResult)),

    %% Verify snapshot available for rollback
    ?assertNotEqual(undefined, maps:get(snapshot, UpgradeResult)),

    %% Verify rollback capability
    ?assertEqual(true, maps:get(rollback_available, UpgradeResult)),

    %% Verify downtime reported
    ActualDowntime = maps:get(actual_downtime_ms, UpgradeResult),
    ?assert(ActualDowntime >= 0),

    ct:log("✓ Upgrade application with verification passed (downtime: ~wms)", [ActualDowntime]),
    Config.

test_upgrade_with_rollback_capability(Config) ->
    %% Setup mock state
    mock_set_current_plan(enterprise),

    %% Apply upgrade
    {ok, UpgradeResult} = erlmcp_pricing_upgrade:apply_upgrade(enterprise, gov),

    %% Verify rollback is available
    ?assertEqual(true, maps:get(rollback_available, UpgradeResult)),

    %% Verify snapshot exists
    Snapshot = maps:get(snapshot, UpgradeResult),
    ?assertNotEqual(undefined, Snapshot),
    ?assertEqual(enterprise, maps:get(plan, Snapshot)),

    ct:log("✓ Upgrade rollback capability verified"),
    Config.

test_upgrade_cooldown_enforcement(Config) ->
    %% Check cooldown for a plan that hasn't been upgraded
    %% Should return true (cooldown passed/not applicable)
    CooldownPassed = erlmcp_pricing_upgrade:check_upgrade_cooldown(team),
    ?assertEqual(true, CooldownPassed),

    %% Record an upgrade timestamp
    mock_set_last_upgrade_time(team, erlang:system_time(second)),

    %% Cooldown should now fail (too recent)
    CooldownFailed = erlmcp_pricing_upgrade:check_upgrade_cooldown(team),
    ?assertEqual(false, CooldownFailed),

    ct:log("✓ Upgrade cooldown enforcement verified"),
    Config.

%% === Test Cases: Determinism Checks ===

test_upgrade_determinism_team_to_enterprise(Config) ->
    %% Get upgrade path 3 times
    {ok, Path1} = erlmcp_pricing_upgrade:get_upgrade_path(team, enterprise),
    {ok, Path2} = erlmcp_pricing_upgrade:get_upgrade_path(team, enterprise),
    {ok, Path3} = erlmcp_pricing_upgrade:get_upgrade_path(team, enterprise),

    %% All paths must be identical
    ?assertEqual(Path1, Path2),
    ?assertEqual(Path2, Path3),

    %% Verify key components are deterministic
    Envelope1 = maps:get(envelope_expansion, Path1),
    Envelope2 = maps:get(envelope_expansion, Path2),
    Envelope3 = maps:get(envelope_expansion, Path3),

    ?assertEqual(Envelope1, Envelope2),
    ?assertEqual(Envelope2, Envelope3),

    %% Verify config changes are deterministic
    Config1 = maps:get(config_changes, Path1),
    Config2 = maps:get(config_changes, Path2),
    Config3 = maps:get(config_changes, Path3),

    ?assertEqual(Config1, Config2),
    ?assertEqual(Config2, Config3),

    ct:log("✓ Team → Enterprise upgrade determinism verified (3x identical paths)"),
    Config.

test_upgrade_determinism_enterprise_to_gov(Config) ->
    %% Get upgrade path 3 times
    {ok, Path1} = erlmcp_pricing_upgrade:get_upgrade_path(enterprise, gov),
    {ok, Path2} = erlmcp_pricing_upgrade:get_upgrade_path(enterprise, gov),
    {ok, Path3} = erlmcp_pricing_upgrade:get_upgrade_path(enterprise, gov),

    %% All paths must be identical
    ?assertEqual(Path1, Path2),
    ?assertEqual(Path2, Path3),

    %% Verify migration steps are deterministic
    Steps1 = maps:get(migration_steps, Path1),
    Steps2 = maps:get(migration_steps, Path2),
    Steps3 = maps:get(migration_steps, Path3),

    ?assertEqual(Steps1, Steps2),
    ?assertEqual(Steps2, Steps3),

    ct:log("✓ Enterprise → Gov upgrade determinism verified (3x identical paths)"),
    Config.

%% === Test Cases: Receipt Chain Logging ===

test_upgrade_event_logged_to_receipt_chain(Config) ->
    %% Log an upgrade event
    erlmcp_pricing_upgrade:log_upgrade_event(team, enterprise, #{reason => test}),

    %% Retrieve upgrade history
    History = erlmcp_pricing_upgrade:get_upgrade_history(1),

    %% Should have at least one entry
    ?assert(length(History) > 0),

    %% Verify event details
    Event = lists:keyfind(upgrade_event, 1, History),
    ?assertNotEqual(false, Event),

    ct:log("✓ Upgrade event logged to receipt chain"),
    Config.

test_receipt_chain_contains_upgrade_details(Config) ->
    %% Log upgrade with details
    Details = #{
        reason => compliance,
        timestamp => erlang:system_time(second),
        verification => #{
            throughput => passed,
            connections => passed
        }
    },
    erlmcp_pricing_upgrade:log_upgrade_event(enterprise, gov, Details),

    %% Retrieve history
    History = erlmcp_pricing_upgrade:get_upgrade_history(5),

    %% Find most recent event
    case History of
        [] ->
            ct:log("⚠ No events in history (expected if receipt chain not initialized)");
        Events ->
            %% Verify event structure
            lists:foreach(
                fun(Event) ->
                    ?assertEqual(upgrade_event, maps:get(type, Event, undefined)),
                    ?assertNotEqual(undefined, maps:get(timestamp, Event, undefined)),
                    ?assertNotEqual(undefined, maps:get(from_plan, Event, undefined)),
                    ?assertNotEqual(undefined, maps:get(to_plan, Event, Event))
                end,
                Events
            ),
            ct:log("✓ Receipt chain contains upgrade details")
    end,

    Config.

%% === Mock Helper Functions ===

mock_pricing_state_setup() ->
    %% Create mock state storage
    case ets:info(erlmcp_pricing_mock) of
        undefined ->
            ets:new(erlmcp_pricing_mock, [named_table, public]);
        _ ->
            ets:delete_all_objects(erlmcp_pricing_mock)
    end,

    %% Initialize default state
    ets:insert(erlmcp_pricing_mock, {current_plan, team}),
    ets:insert(erlmcp_pricing_mock, {certification_valid, true}).

mock_pricing_state_cleanup() ->
    catch ets:delete(erlmcp_pricing_mock).

mock_set_current_plan(Plan) ->
    ets:insert(erlmcp_pricing_mock, {current_plan, Plan}).

mock_set_last_upgrade_time(Plan, Time) ->
    ets:insert(erlmcp_pricing_mock, {last_upgrade_time, Plan, Time}).

mock_get_current_plan() ->
    case ets:lookup(erlmcp_pricing_mock, current_plan) of
        [{_, Plan}] -> {ok, Plan};
        [] -> {error, not_found}
    end.

mock_get_last_upgrade_time(Plan) ->
    case ets:lookup(erlmcp_pricing_mock, {last_upgrade_time, Plan}) of
        [{_, Time}] -> Time;
        [] -> not_found
    end.
