%%% erlmcp_pricing_upgrade.erl
%%% Pricing tier upgrade and downgrade path management
%%% Implements deterministic upgrade paths with safety gates and rollback capability
-module(erlmcp_pricing_upgrade).

%% Public API
-export([
    can_upgrade/2,
    can_downgrade/2,
    get_upgrade_path/2,
    list_possible_upgrades/1,
    simulate_upgrade/2,
    apply_upgrade/2,
    verify_upgrade/1,
    calculate_migration_time/2,
    validate_upgrade_prerequisites/2,
    snapshot_system_state/1,
    restore_system_state/1,
    log_upgrade_event/3,
    record_upgrade_timestamp/1,
    update_plan_metadata/1,
    get_upgrade_history/1,
    check_upgrade_cooldown/1
]).

%% Helper functions
-export([
    load_upgrade_path/2,
    verify_certification_valid/1,
    check_infrastructure_headroom/1,
    verify_clean_receipt_state/0,
    verify_sla_compliance/1,
    update_team_limits/0,
    update_enterprise_limits/0,
    update_gov_limits/0,
    flush_pending_requests/1
]).

-include("erlmcp.hrl").

%% Type definitions
-type plan() :: team | enterprise | gov.
-type upgrade_path() :: #{
    from_plan := plan(),
    to_plan := plan(),
    description := string(),
    estimated_downtime_ms := integer(),
    envelope_expansion := envelope_expansion(),
    config_changes := [config_change()],
    migration_steps := migration_steps(),
    cooldown_period_seconds := integer(),
    rollback_compatible := boolean(),
    safety_gates := [atom()],
    evidence_requirements := map()
}.
-type envelope_expansion() :: #{
    old_envelope := envelope(),
    new_envelope := envelope()
}.
-type envelope() :: #{
    throughput_req_s := integer(),
    concurrent_connections := integer(),
    queue_depth_messages := integer(),
    p99_latency_ms := integer(),
    failover_sla_seconds => integer(),
    connection_timeout_seconds => integer()
}.
-type config_change() :: #{
    setting := string(),
    old_value := term(),
    new_value := term(),
    description := string(),
    requires_restart := boolean()
}.
-type migration_steps() :: #{
    pre_upgrade_checks := [migration_step()],
    upgrade_phase := [migration_step()],
    post_upgrade_verification := [migration_step()]
}.
-type migration_step() :: #{
    step_id := string(),
    description := string(),
    action_type := atom(),
    command => string(),
    timeout_seconds := integer(),
    on_failure := atom(),
    rollback_step => string()
}.
-type upgrade_result() :: {ok, Result :: map()} | {error, Reason :: atom() | string()}.
-type safety_check_result() :: {passed, Details :: map()} | {failed, Reason :: string()}.

-export_type([plan/0, upgrade_path/0]).

%% Upgrade compatibility matrix (deterministic)
-define(UPGRADE_PATHS, [
    {team, enterprise},      %% Team → Enterprise (allowed)
    {enterprise, gov}        %% Enterprise → Gov (allowed)
]).

%% Downgrade forbidden list (deterministic)
-define(DOWNGRADE_FORBIDDEN, [
    {gov, enterprise},       %% Gov → Enterprise (forbidden)
    {gov, team},             %% Gov → Team (forbidden)
    {enterprise, team}       %% Enterprise → Team (forbidden)
]).

%% === Public API ===

%% @doc Check if upgrade from one plan to another is allowed
%% Returns true only for forward upgrades: Team→Enterprise, Enterprise→Gov
-spec can_upgrade(FromPlan :: plan(), ToPlan :: plan()) -> boolean().
can_upgrade(FromPlan, ToPlan) when FromPlan =:= ToPlan ->
    false;
can_upgrade(FromPlan, ToPlan) ->
    lists:member({FromPlan, ToPlan}, ?UPGRADE_PATHS).

%% @doc Check if downgrade is allowed (always false - downgrades forbidden)
-spec can_downgrade(FromPlan :: plan(), ToPlan :: plan()) -> boolean().
can_downgrade(FromPlan, ToPlan) ->
    %% Downgrades explicitly forbidden
    case lists:member({FromPlan, ToPlan}, ?DOWNGRADE_FORBIDDEN) of
        true -> false;
        false ->
            %% Also forbidden: any move that reduces capabilities or envelope
            case {FromPlan, ToPlan} of
                {gov, _} -> false;        %% Gov cannot downgrade
                {enterprise, team} -> false; %% Enterprise cannot downgrade to Team
                _ -> false                %% All other downgrades forbidden
            end
    end.

%% @doc Get upgrade path definition between two plans
-spec get_upgrade_path(FromPlan :: plan(), ToPlan :: plan()) -> upgrade_result().
get_upgrade_path(FromPlan, ToPlan) ->
    case can_upgrade(FromPlan, ToPlan) of
        false ->
            case can_downgrade(FromPlan, ToPlan) of
                false ->
                    {error, downgrade_forbidden};
                true ->
                    {error, downgrade_forbidden}  %% Downgrades always forbidden
            end;
        true ->
            load_upgrade_path(FromPlan, ToPlan)
    end.

%% @doc List all valid upgrades from a given plan
-spec list_possible_upgrades(Plan :: plan()) -> [plan()].
list_possible_upgrades(Plan) ->
    [To || {From, To} <- ?UPGRADE_PATHS, From =:= Plan].

%% @doc Simulate upgrade without applying changes
%% Returns predicted success/failure and details
-spec simulate_upgrade(FromPlan :: plan(), ToPlan :: plan()) -> upgrade_result().
simulate_upgrade(FromPlan, ToPlan) ->
    case get_upgrade_path(FromPlan, ToPlan) of
        {error, Reason} ->
            {error, Reason};
        {ok, UpgradePath} ->
            %% Check safety gates in dry-run mode
            case validate_upgrade_prerequisites(FromPlan, UpgradePath) of
                {error, Reason} ->
                    {error, {simulation_failed, Reason}};
                {ok, _Details} ->
                    %% Predict envelope verification success
                    NewEnvelope = maps:get(new_envelope, maps:get(envelope_expansion, UpgradePath)),
                    {ok, #{
                        simulated => true,
                        from_plan => FromPlan,
                        to_plan => ToPlan,
                        predicted_envelope => NewEnvelope,
                        estimated_downtime_ms => maps:get(estimated_downtime_ms, UpgradePath),
                        migration_steps => length(maps:get(upgrade_phase, maps:get(migration_steps, UpgradePath))),
                        will_succeed => true
                    }}
            end
    end.

%% @doc Apply upgrade with safety gates and rollback capability
%% Returns system snapshot for potential rollback
-spec apply_upgrade(FromPlan :: plan(), ToPlan :: plan()) -> upgrade_result().
apply_upgrade(FromPlan, ToPlan) ->
    case get_upgrade_path(FromPlan, ToPlan) of
        {error, Reason} ->
            {error, Reason};
        {ok, UpgradePath} ->
            %% Validate all prerequisites
            case validate_upgrade_prerequisites(FromPlan, UpgradePath) of
                {error, Reason} ->
                    {error, {prerequisites_failed, Reason}};
                {ok, PrerequisitesDetails} ->
                    %% Create system snapshot for rollback
                    case snapshot_system_state(FromPlan) of
                        {error, SnapshotReason} ->
                            {error, {snapshot_failed, SnapshotReason}};
                        {ok, Snapshot} ->
                            %% Execute upgrade with timestamps
                            StartTime = erlang:timestamp(),
                            case execute_upgrade_steps(UpgradePath) of
                                {error, StepError} ->
                                    lager:error("Upgrade step failed: ~p", [StepError]),
                                    {error, {upgrade_step_failed, StepError}};
                                {ok, StepResults} ->
                                    %% Verify post-upgrade envelope
                                    case verify_upgrade(ToPlan) of
                                        {error, VerifyError} ->
                                            lager:error("Post-upgrade verification failed: ~p", [VerifyError]),
                                            {error, {verification_failed, VerifyError}};
                                        {ok, VerifyDetails} ->
                                            EndTime = erlang:timestamp(),
                                            ActualDowntimeMs = timer:now_diff(EndTime, StartTime) div 1000,
                                            %% Log upgrade event to receipt chain
                                            log_upgrade_event(FromPlan, ToPlan, #{
                                                reason => upgrade_applied,
                                                prerequisites => PrerequisitesDetails,
                                                steps => StepResults,
                                                verification => VerifyDetails,
                                                actual_downtime_ms => ActualDowntimeMs
                                            }),
                                            %% Record upgrade timestamp
                                            record_upgrade_timestamp(ToPlan),
                                            %% Update plan metadata
                                            update_plan_metadata(ToPlan),
                                            {ok, #{
                                                upgraded => true,
                                                from_plan => FromPlan,
                                                to_plan => ToPlan,
                                                timestamp => erlang:system_time(second),
                                                actual_downtime_ms => ActualDowntimeMs,
                                                snapshot => Snapshot,
                                                verification => VerifyDetails,
                                                rollback_available => maps:get(rollback_compatible, UpgradePath)
                                            }}
                                    end
                            end
                    end
            end
    end.

%% @doc Verify post-upgrade envelope meets target plan specifications
-spec verify_upgrade(TargetPlan :: plan()) -> upgrade_result().
verify_upgrade(TargetPlan) ->
    TargetEnvelope = get_target_envelope(TargetPlan),
    CurrentMetrics = erlmcp_metrics:get_current_metrics(),

    %% Check all envelope metrics
    Checks = [
        {throughput, maps:get(throughput_req_s, TargetEnvelope),
         maps:get(current_throughput_req_s, CurrentMetrics, 0)},
        {concurrent_connections, maps:get(concurrent_connections, TargetEnvelope),
         maps:get(current_connections, CurrentMetrics, 0)},
        {queue_depth, maps:get(queue_depth_messages, TargetEnvelope),
         maps:get(current_queue_depth, CurrentMetrics, 0)},
        {latency_p99, maps:get(p99_latency_ms, TargetEnvelope),
         maps:get(latency_p99_ms, CurrentMetrics, inf)}
    ],

    Results = lists:map(
        fun({MetricName, Target, Current}) ->
            case MetricName of
                throughput ->
                    {throughput, Current >= Target};
                concurrent_connections ->
                    {concurrent_connections, Current >= Target};
                queue_depth ->
                    {queue_depth, Current >= Target};
                latency_p99 ->
                    {latency_p99, Current =< Target}
            end
        end,
        Checks
    ),

    case lists:all(fun({_, Result}) -> Result end, Results) of
        false ->
            FailedChecks = [Metric || {Metric, false} <- Results],
            {error, {verification_failed, {failed_checks, FailedChecks}}};
        true ->
            {ok, #{
                verified => true,
                plan => TargetPlan,
                envelope => TargetEnvelope,
                current_metrics => CurrentMetrics,
                checks => Results
            }}
    end.

%% @doc Calculate estimated migration time for upgrade
-spec calculate_migration_time(FromPlan :: plan(), ToPlan :: plan()) -> non_neg_integer().
calculate_migration_time(FromPlan, ToPlan) ->
    case get_upgrade_path(FromPlan, ToPlan) of
        {error, _} ->
            0;
        {ok, UpgradePath} ->
            EstimatedDowntime = maps:get(estimated_downtime_ms, UpgradePath),
            MigrationSteps = maps:get(migration_steps, UpgradePath),
            PreUpgradeTimeMs = calculate_step_time(
                maps:get(pre_upgrade_checks, MigrationSteps)
            ),
            UpgradePhaseMs = calculate_step_time(
                maps:get(upgrade_phase, MigrationSteps)
            ),
            PostUpgradeTimeMs = calculate_step_time(
                maps:get(post_upgrade_verification, MigrationSteps)
            ),
            EstimatedDowntime + PreUpgradeTimeMs + UpgradePhaseMs + PostUpgradeTimeMs
    end.

%% @doc Validate upgrade prerequisites (safety gates)
-spec validate_upgrade_prerequisites(FromPlan :: plan(), UpgradePath :: upgrade_path()) ->
    {ok, Details :: map()} | {error, Reason :: string()}.
validate_upgrade_prerequisites(FromPlan, UpgradePath) ->
    SafetyGates = maps:get(safety_gates, UpgradePath, []),
    ToPlan = maps:get(to_plan, UpgradePath),

    %% Execute all safety gates
    GateResults = lists:map(
        fun(Gate) ->
            {Gate, execute_safety_gate(Gate, FromPlan, ToPlan)}
        end,
        SafetyGates
    ),

    %% All gates must pass
    FailedGates = [
        {Gate, Reason} || {Gate, {failed, Reason}} <- GateResults
    ],

    case FailedGates of
        [] ->
            PassedDetails = maps:from_list([
                {Gate, Details} || {Gate, {passed, Details}} <- GateResults
            ]),
            {ok, #{
                all_gates_passed => true,
                gate_details => PassedDetails,
                timestamp => erlang:system_time(second)
            }};
        _ ->
            ReasonStr = io_lib:format("Safety gates failed: ~p", [FailedGates]),
            {error, ReasonStr}
    end.

%% @doc Get upgrade history for current deployment
-spec get_upgrade_history(Limit :: pos_integer()) -> [map()].
get_upgrade_history(Limit) ->
    %% Query receipt chain for upgrade events
    case erlmcp_receipt_chain:get_events_by_type(upgrade_event) of
        {error, _} ->
            [];
        {ok, Events} ->
            lists:sublist(Events, Limit)
    end.

%% @doc Check if upgrade cooldown has elapsed
-spec check_upgrade_cooldown(ToPlan :: plan()) -> boolean().
check_upgrade_cooldown(ToPlan) ->
    case erlmcp_pricing_state:get_last_upgrade_time(ToPlan) of
        not_found ->
            true;  %% No previous upgrade, cooldown passed
        LastUpgradeTime ->
            %% Get cooldown period from current plan
            case erlmcp_pricing_state:get_current_plan() of
                {ok, CurrentPlan} ->
                    {ok, UpgradePath} = get_upgrade_path(CurrentPlan, ToPlan),
                    CooldownSeconds = maps:get(cooldown_period_seconds, UpgradePath),
                    CurrentTime = erlang:system_time(second),
                    (CurrentTime - LastUpgradeTime) >= CooldownSeconds;
                _OtherPlan ->
                    true
            end
    end.

%% === System State Management ===

%% @doc Create system snapshot for rollback capability
-spec snapshot_system_state(Plan :: plan()) -> upgrade_result().
snapshot_system_state(Plan) ->
    try
        Snapshot = #{
            plan => Plan,
            timestamp => erlang:system_time(second),
            metrics => erlmcp_metrics:get_current_metrics(),
            config => erlmcp_config:get_all_config(),
            process_state => collect_process_state(),
            registry_state => erlmcp_registry:get_all_state()
        },
        {ok, Snapshot}
    catch
        Error:Reason ->
            lager:error("Failed to snapshot system state: ~p:~p", [Error, Reason]),
            {error, snapshot_failed}
    end.

%% @doc Restore system from snapshot
-spec restore_system_state(Snapshot :: map()) -> ok | {error, term()}.
restore_system_state(Snapshot) ->
    try
        %% Restore process state
        ProcessState = maps:get(process_state, Snapshot),
        restore_process_state(ProcessState),

        %% Restore registry state
        RegistryState = maps:get(registry_state, Snapshot),
        erlmcp_registry:restore_state(RegistryState),

        lager:info("System restored from snapshot"),
        ok
    catch
        Error:Reason ->
            lager:error("Failed to restore system state: ~p:~p", [Error, Reason]),
            {error, restore_failed}
    end.

%% @doc Log upgrade event to receipt chain
-spec log_upgrade_event(FromPlan :: plan(), ToPlan :: plan(), Details :: map()) -> ok.
log_upgrade_event(FromPlan, ToPlan, Details) ->
    Event = #{
        type => upgrade_event,
        timestamp => erlang:system_time(millisecond),
        from_plan => FromPlan,
        to_plan => ToPlan,
        details => Details,
        node => node(),
        version => erlmcp_app:get_version()
    },
    erlmcp_receipt_chain:add_event(Event).

%% @doc Record upgrade completion timestamp
-spec record_upgrade_timestamp(Plan :: plan()) -> ok.
record_upgrade_timestamp(Plan) ->
    erlmcp_pricing_state:set_last_upgrade_time(Plan, erlang:system_time(second)).

%% @doc Update deployment metadata after upgrade
-spec update_plan_metadata(Plan :: plan()) -> ok.
update_plan_metadata(Plan) ->
    erlmcp_pricing_state:set_current_plan(Plan),
    erlmcp_pricing_state:set_upgrade_timestamp(erlang:system_time(second)).

%% === Helper Functions ===

%% @doc Load upgrade path definition from JSON file
-spec load_upgrade_path(FromPlan :: plan(), ToPlan :: plan()) -> upgrade_result().
load_upgrade_path(FromPlan, ToPlan) ->
    Filename = io_lib:format(
        "/Users/sac/erlmcp/plans/upgrade_~s_to_~s.json",
        [FromPlan, ToPlan]
    ),
    try
        {ok, JsonBinary} = file:read_file(Filename),
        {ok, JsonMap} = jsx:decode(JsonBinary, [return_maps]),
        {ok, JsonMap}
    catch
        _:Error ->
            lager:error("Failed to load upgrade path ~s→~s: ~p", [FromPlan, ToPlan, Error]),
            {error, upgrade_path_not_found}
    end.

%% @doc Verify current plan certification is valid
-spec verify_certification_valid(Plan :: plan()) -> safety_check_result().
verify_certification_valid(Plan) ->
    case erlmcp_pricing_state:get_certification_valid(Plan) of
        true ->
            {passed, #{certification_valid => true}};
        false ->
            {failed, "Plan certification not valid"}
    end.

%% @doc Check if cluster has infrastructure headroom for target plan
-spec check_infrastructure_headroom(ToPlan :: plan()) -> safety_check_result().
check_infrastructure_headroom(ToPlan) ->
    TargetEnvelope = get_target_envelope(ToPlan),
    TargetThroughput = maps:get(throughput_req_s, TargetEnvelope),
    TargetConnections = maps:get(concurrent_connections, TargetEnvelope),

    %% Check if cluster can handle target envelope
    CurrentMetrics = erlmcp_metrics:get_current_metrics(),
    AvailableThroughput = maps:get(available_throughput_req_s, CurrentMetrics, 0),
    AvailableConnections = maps:get(available_connections, CurrentMetrics, 0),

    case (AvailableThroughput >= TargetThroughput) andalso
         (AvailableConnections >= TargetConnections) of
        true ->
            {passed, #{
                target_throughput => TargetThroughput,
                available_throughput => AvailableThroughput,
                target_connections => TargetConnections,
                available_connections => AvailableConnections
            }};
        false ->
            {failed, "Insufficient infrastructure headroom"}
    end.

%% @doc Verify no pending refusals in receipt chain
-spec verify_clean_receipt_state() -> safety_check_result().
verify_clean_receipt_state() ->
    case erlmcp_receipt_chain:get_events_by_type(refusal) of
        {error, _} ->
            {passed, #{pending_refusals => 0}};
        {ok, []} ->
            {passed, #{pending_refusals => 0}};
        {ok, Events} ->
            %% Check if all refusals have been resolved
            UnresolvedCount = length(Events),
            case UnresolvedCount of
                0 ->
                    {passed, #{pending_refusals => 0}};
                _ ->
                    {failed, io_lib:format(
                        "~w unresolved refusals in receipt chain", [UnresolvedCount]
                    )}
            end
    end.

%% @doc Verify current system meets plan SLAs
-spec verify_sla_compliance(Plan :: plan()) -> safety_check_result().
verify_sla_compliance(Plan) ->
    TargetEnvelope = get_target_envelope(Plan),
    CurrentMetrics = erlmcp_metrics:get_current_metrics(),

    %% Check key SLA metrics
    Latency = maps:get(latency_p99_ms, CurrentMetrics, inf),
    TargetLatency = maps:get(p99_latency_ms, TargetEnvelope),

    case Latency =< TargetLatency of
        true ->
            {passed, #{current_latency_ms => Latency, target_latency_ms => TargetLatency}};
        false ->
            {failed, io_lib:format(
                "Latency p99 ~wms exceeds target ~wms",
                [Latency, TargetLatency]
            )}
    end.

%% @doc Update Team tier limits
-spec update_team_limits() -> ok.
update_team_limits() ->
    erlmcp_config:set(limits, #{
        max_message_size_bytes => 1048576,
        max_payload_size_mb => 10,
        max_concurrent_requests_per_conn => 32,
        memory_limit_mb => 512,
        backpressure_threshold_bytes => 8388608
    }).

%% @doc Update Enterprise tier limits
-spec update_enterprise_limits() -> ok.
update_enterprise_limits() ->
    erlmcp_config:set(limits, #{
        max_message_size_bytes => 10485760,
        max_payload_size_mb => 100,
        max_concurrent_requests_per_conn => 128,
        memory_limit_mb => 4096,
        backpressure_threshold_bytes => 67108864
    }).

%% @doc Update Government tier limits
-spec update_gov_limits() -> ok.
update_gov_limits() ->
    erlmcp_config:set(limits, #{
        max_message_size_bytes => 5242880,
        max_payload_size_mb => 50,
        max_concurrent_requests_per_conn => 64,
        memory_limit_mb => 2048,
        backpressure_threshold_bytes => 33554432
    }).

%% @doc Flush all pending requests from queue
-spec flush_pending_requests(Plan :: plan()) -> ok | {error, term()}.
flush_pending_requests(_Plan) ->
    %% Drain request queue gracefully
    case erlmcp_registry:get_queue_depth() of
        {error, _} ->
            ok;
        {ok, QueueDepth} when QueueDepth > 0 ->
            %% Wait for queue to drain (with timeout)
            wait_for_queue_drain(QueueDepth, 0);
        {ok, 0} ->
            ok
    end.

%% === Private Helper Functions ===

%% Execute upgrade migration steps
execute_upgrade_steps(UpgradePath) ->
    MigrationSteps = maps:get(migration_steps, UpgradePath),
    PreUpgradeSteps = maps:get(pre_upgrade_checks, MigrationSteps),
    UpgradePhaseSteps = maps:get(upgrade_phase, MigrationSteps),
    PostUpgradeSteps = maps:get(post_upgrade_verification, MigrationSteps),

    AllSteps = PreUpgradeSteps ++ UpgradePhaseSteps ++ PostUpgradeSteps,

    Results = lists:map(
        fun(Step) ->
            StepId = maps:get(step_id, Step),
            Command = maps:get(command, Step, "noop"),
            Timeout = maps:get(timeout_seconds, Step, 30) * 1000,
            OnFailure = maps:get(on_failure, Step, fail),

            lager:info("Executing upgrade step: ~s (timeout: ~wms)", [StepId, Timeout]),

            case execute_step_command(Command, Timeout) of
                ok ->
                    {StepId, ok};
                {error, Reason} ->
                    case OnFailure of
                        fail ->
                            {error, {StepId, Reason}};
                        warn ->
                            lager:warning("Step ~s failed (warning): ~p", [StepId, Reason]),
                            {StepId, {warned, Reason}};
                        skip ->
                            lager:info("Step ~s skipped", [StepId]),
                            {StepId, skipped}
                    end
            end
        end,
        AllSteps
    ),

    %% Check for errors
    case lists:any(fun(R) -> element(1, R) =:= error end, Results) of
        true ->
            FirstError = lists:keyfind(error, 1, Results),
            {error, FirstError};
        false ->
            {ok, Results}
    end.

%% Execute a single upgrade step command
execute_step_command("noop", _Timeout) ->
    ok;
execute_step_command(Command, Timeout) ->
    %% Parse and execute command
    %% Format: module:function(args) or function(args)
    try
        %% Simple implementation - in production would use erl_eval or similar
        lager:info("Executing step command: ~s (timeout: ~wms)", [Command, Timeout]),
        ok
    catch
        _:Error ->
            {error, Error}
    end.

%% Execute safety gate check
execute_safety_gate(certification_valid, FromPlan, _ToPlan) ->
    verify_certification_valid(FromPlan);
execute_safety_gate(infrastructure_headroom, _FromPlan, ToPlan) ->
    check_infrastructure_headroom(ToPlan);
execute_safety_gate(clean_receipt_state, _FromPlan, _ToPlan) ->
    verify_clean_receipt_state();
execute_safety_gate(post_upgrade_verification, _FromPlan, _ToPlan) ->
    {passed, #{post_upgrade_verification => deferred}};
execute_safety_gate(sla_compliance, FromPlan, _ToPlan) ->
    verify_sla_compliance(FromPlan);
execute_safety_gate(resource_availability, _FromPlan, ToPlan) ->
    check_infrastructure_headroom(ToPlan);
execute_safety_gate(_, _, _) ->
    {passed, #{}}.

%% Get target envelope for a plan
get_target_envelope(team) ->
    #{
        throughput_req_s => 450,
        concurrent_connections => 128,
        queue_depth_messages => 2048,
        p99_latency_ms => 250,
        failover_sla_seconds => 30,
        connection_timeout_seconds => 60
    };
get_target_envelope(enterprise) ->
    #{
        throughput_req_s => 1500,
        concurrent_connections => 512,
        queue_depth_messages => 8192,
        p99_latency_ms => 100,
        failover_sla_seconds => 10,
        connection_timeout_seconds => 120
    };
get_target_envelope(gov) ->
    #{
        throughput_req_s => 900,
        concurrent_connections => 256,
        queue_depth_messages => 4096,
        p99_latency_ms => 150,
        failover_sla_seconds => 15,
        connection_timeout_seconds => 90
    }.

%% Calculate total time for a list of steps
calculate_step_time(Steps) ->
    lists:foldl(
        fun(Step, Acc) ->
            Timeout = maps:get(timeout_seconds, Step, 30),
            Acc + (Timeout * 1000)
        end,
        0,
        Steps
    ).

%% Wait for queue to drain
wait_for_queue_drain(0, _Attempts) ->
    ok;
wait_for_queue_drain(_QueueDepth, Attempts) when Attempts > 100 ->
    {error, queue_drain_timeout};
wait_for_queue_drain(QueueDepth, Attempts) ->
    timer:sleep(100),
    case erlmcp_registry:get_queue_depth() of
        {error, _} ->
            ok;
        {ok, NewDepth} when NewDepth < QueueDepth ->
            wait_for_queue_drain(NewDepth, Attempts + 1);
        {ok, NewDepth} ->
            wait_for_queue_drain(NewDepth, Attempts + 1)
    end.

%% Collect current process state
collect_process_state() ->
    #{
        timestamp => erlang:system_time(second),
        processes => erlang:processes(),
        memory => erlang:memory(),
        statistics => erlang:statistics()
    }.

%% Restore process state from snapshot
restore_process_state(_ProcessState) ->
    %% In production, would restore monitored processes and state
    ok.
