%%% erlmcp_cli_upgrade.erl
%%% CLI commands for pricing tier upgrades and downgrades
-module(erlmcp_cli_upgrade).

%% Public API
-export([
    command/2
]).

-include("erlmcp.hrl").

%% === CLI Command Dispatcher ===

-spec command(Command :: string(), Args :: [string()]) -> ok | {error, term()}.
command("show", Args) ->
    cmd_show_upgrade_path(Args);
command("plan", Args) ->
    cmd_plan_upgrade(Args);
command("verify", _Args) ->
    cmd_verify_upgrade();
command("apply", Args) ->
    cmd_apply_upgrade(Args);
command("status", _Args) ->
    cmd_show_upgrade_status();
command("rollback", Args) ->
    cmd_rollback_upgrade(Args);
command("history", Args) ->
    cmd_show_upgrade_history(Args);
command(_Unknown, _Args) ->
    show_help().

%% === Command Implementations ===

%% erlmcp upgrade show <current_plan> <target_plan>
%% Display upgrade path definition between two plans
cmd_show_upgrade_path(Args) ->
    case parse_plan_args(Args) of
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            show_plan_args_help();
        {ok, FromPlan, ToPlan} ->
            case erlmcp_pricing_upgrade:get_upgrade_path(FromPlan, ToPlan) of
                {error, Reason} ->
                    io:format("Error: Cannot show upgrade path: ~p~n", [Reason]);
                {ok, UpgradePath} ->
                    display_upgrade_path_details(FromPlan, ToPlan, UpgradePath)
            end
    end.

%% erlmcp upgrade plan <current_plan> <target_plan>
%% Show upgrade steps and warnings without applying
cmd_plan_upgrade(Args) ->
    case parse_plan_args(Args) of
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            show_plan_args_help();
        {ok, FromPlan, ToPlan} ->
            case erlmcp_pricing_upgrade:simulate_upgrade(FromPlan, ToPlan) of
                {error, Reason} ->
                    io:format("Error: Upgrade simulation failed: ~p~n", [Reason]);
                {ok, SimulationResult} ->
                    display_upgrade_plan(FromPlan, ToPlan, SimulationResult)
            end
    end.

%% erlmcp upgrade verify
%% Check if current system meets target plan envelope
cmd_verify_upgrade() ->
    case erlmcp_pricing_state:get_current_plan() of
        {error, _} ->
            io:format("Error: Could not determine current plan~n");
        {ok, CurrentPlan} ->
            case erlmcp_pricing_upgrade:verify_upgrade(CurrentPlan) of
                {error, {verification_failed, {failed_checks, FailedChecks}}} ->
                    io:format("Verification FAILED for plan ~w~n~n", [CurrentPlan]),
                    io:format("Failed checks:~n"),
                    lists:foreach(
                        fun(Check) ->
                            io:format("  - ~w~n", [Check])
                        end,
                        FailedChecks
                    );
                {error, Reason} ->
                    io:format("Error: Verification failed: ~p~n", [Reason]);
                {ok, VerifyDetails} ->
                    display_verification_results(CurrentPlan, VerifyDetails)
            end
    end.

%% erlmcp upgrade apply <target_plan>
%% Apply upgrade with safety checks
cmd_apply_upgrade(Args) ->
    case Args of
        [TargetPlanStr] ->
            TargetPlan = list_to_atom(TargetPlanStr),
            case erlmcp_pricing_state:get_current_plan() of
                {error, _} ->
                    io:format("Error: Could not determine current plan~n");
                {ok, CurrentPlan} ->
                    %% Check cooldown
                    case erlmcp_pricing_upgrade:check_upgrade_cooldown(TargetPlan) of
                        false ->
                            io:format("Error: Upgrade cooldown not elapsed~n");
                        true ->
                            %% Confirm before applying
                            case confirm_upgrade(CurrentPlan, TargetPlan) of
                                false ->
                                    io:format("Upgrade cancelled~n");
                                true ->
                                    apply_upgrade_confirmed(CurrentPlan, TargetPlan)
                            end
                    end
            end;
        _ ->
            io:format("Error: Invalid arguments~n"),
            show_apply_help()
    end.

%% erlmcp upgrade status
%% Show current plan and upgrade history
cmd_show_upgrade_status() ->
    case erlmcp_pricing_state:get_current_plan() of
        {error, _} ->
            io:format("Error: Could not determine current plan~n");
        {ok, CurrentPlan} ->
            io:format("Current Plan: ~w~n~n", [CurrentPlan]),

            %% Show last upgrade time
            case erlmcp_pricing_state:get_last_upgrade_time(CurrentPlan) of
                not_found ->
                    io:format("Last Upgrade: Never~n");
                Timestamp ->
                    DateTime = erlang:system_time(second) - Timestamp,
                    Hours = DateTime div 3600,
                    io:format("Last Upgrade: ~w hours ago~n", [Hours])
            end,

            io:format("~nPossible Upgrades:~n"),
            PossibleUpgrades = erlmcp_pricing_upgrade:list_possible_upgrades(CurrentPlan),
            lists:foreach(
                fun(TargetPlan) ->
                    io:format("  - ~w~n", [TargetPlan])
                end,
                PossibleUpgrades
            ),

            %% Show recent upgrade history
            io:format("~nRecent Upgrade History:~n"),
            History = erlmcp_pricing_upgrade:get_upgrade_history(5),
            case History of
                [] ->
                    io:format("  No upgrades recorded~n");
                _ ->
                    lists:foreach(
                        fun(Event) ->
                            display_history_entry(Event)
                        end,
                        History
                    )
            end
    end.

%% erlmcp upgrade rollback
%% Revert to previous plan (if within cooldown)
cmd_rollback_upgrade(Args) ->
    case Args of
        [] ->
            io:format("Error: No target plan specified~n"),
            show_rollback_help();
        [PreviousPlanStr] ->
            PreviousPlan = list_to_atom(PreviousPlanStr),
            case erlmcp_pricing_state:get_current_plan() of
                {error, _} ->
                    io:format("Error: Could not determine current plan~n");
                {ok, CurrentPlan} ->
                    case erlmcp_pricing_upgrade:can_downgrade(CurrentPlan, PreviousPlan) of
                        false ->
                            io:format("Error: Downgrade from ~w to ~w is not allowed~n",
                                [CurrentPlan, PreviousPlan]);
                        true ->
                            case confirm_rollback(CurrentPlan, PreviousPlan) of
                                false ->
                                    io:format("Rollback cancelled~n");
                                true ->
                                    io:format("Error: Rollback not yet implemented~n")
                            end
                    end
            end
    end.

%% erlmcp upgrade history [limit]
%% Show upgrade history
cmd_show_upgrade_history(Args) ->
    Limit = case Args of
        [LimitStr] ->
            try
                list_to_integer(LimitStr)
            catch
                _:_ -> 10
            end;
        _ ->
            10
    end,

    History = erlmcp_pricing_upgrade:get_upgrade_history(Limit),
    io:format("Upgrade History (last ~w entries):~n~n", [Limit]),

    case History of
        [] ->
            io:format("No upgrade history recorded~n");
        _ ->
            lists:foreach(
                fun(Event) ->
                    display_history_entry(Event)
                end,
                History
            )
    end.

%% === Display Functions ===

%% Display full upgrade path details
display_upgrade_path_details(FromPlan, ToPlan, UpgradePath) ->
    io:format("~n=== Upgrade Path: ~w → ~w ===~n", [FromPlan, ToPlan]),
    io:format("~n"),
    io:format("Description: ~s~n", [maps:get(description, UpgradePath, "N/A")]),
    io:format("~n"),

    %% Show envelope changes
    io:format("Envelope Changes:~n"),
    EnvelopeExpansion = maps:get(envelope_expansion, UpgradePath),
    OldEnvelope = maps:get(old_envelope, EnvelopeExpansion),
    NewEnvelope = maps:get(new_envelope, EnvelopeExpansion),

    io:format("  Throughput:          ~w → ~w req/s~n",
        [maps:get(throughput_req_s, OldEnvelope),
         maps:get(throughput_req_s, NewEnvelope)]),
    io:format("  Concurrent Conns:    ~w → ~w~n",
        [maps:get(concurrent_connections, OldEnvelope),
         maps:get(concurrent_connections, NewEnvelope)]),
    io:format("  Queue Depth:         ~w → ~w messages~n",
        [maps:get(queue_depth_messages, OldEnvelope),
         maps:get(queue_depth_messages, NewEnvelope)]),
    io:format("  P99 Latency:         ~w → ~w ms~n",
        [maps:get(p99_latency_ms, OldEnvelope),
         maps:get(p99_latency_ms, NewEnvelope)]),
    io:format("~n"),

    %% Show config changes
    io:format("Configuration Changes (~w total):~n",
        [length(maps:get(config_changes, UpgradePath))]),
    ConfigChanges = maps:get(config_changes, UpgradePath),
    lists:foreach(
        fun(Change) ->
            Setting = maps:get(setting, Change),
            OldValue = maps:get(old_value, Change),
            NewValue = maps:get(new_value, Change),
            Restart = maps:get(requires_restart, Change, false),
            RestartStr = case Restart of
                true -> " [REQUIRES RESTART]";
                false -> ""
            end,
            io:format("  ~s: ~w → ~w~s~n",
                [Setting, OldValue, NewValue, RestartStr])
        end,
        ConfigChanges
    ),
    io:format("~n"),

    %% Show safety gates
    io:format("Safety Gates:~n"),
    SafetyGates = maps:get(safety_gates, UpgradePath, []),
    lists:foreach(
        fun(Gate) ->
            io:format("  - ~w~n", [Gate])
        end,
        SafetyGates
    ),
    io:format("~n"),

    %% Show timing info
    io:format("Timing:~n"),
    io:format("  Estimated Downtime: ~w ms~n",
        [maps:get(estimated_downtime_ms, UpgradePath)]),
    io:format("  Cooldown Period:    ~w seconds (~w hours)~n",
        [maps:get(cooldown_period_seconds, UpgradePath),
         maps:get(cooldown_period_seconds, UpgradePath) div 3600]),
    io:format("  Rollback Available: ~w~n",
        [maps:get(rollback_compatible, UpgradePath)]),
    io:format("~n").

%% Display upgrade plan (simulation)
display_upgrade_plan(FromPlan, ToPlan, SimulationResult) ->
    io:format("~n=== Upgrade Plan: ~w → ~w (Simulation) ===~n", [FromPlan, ToPlan]),
    io:format("~n"),
    io:format("Simulated Upgrade: ~w~n",
        [maps:get(simulated, SimulationResult)]),
    io:format("Will Succeed: ~w~n",
        [maps:get(will_succeed, SimulationResult)]),
    io:format("~n"),
    io:format("Predicted Envelope:~n"),
    PredictedEnvelope = maps:get(predicted_envelope, SimulationResult),
    io:format("  Throughput: ~w req/s~n",
        [maps:get(throughput_req_s, PredictedEnvelope)]),
    io:format("  Concurrent Connections: ~w~n",
        [maps:get(concurrent_connections, PredictedEnvelope)]),
    io:format("  Queue Depth: ~w messages~n",
        [maps:get(queue_depth_messages, PredictedEnvelope)]),
    io:format("  P99 Latency: ~w ms~n",
        [maps:get(p99_latency_ms, PredictedEnvelope)]),
    io:format("~n"),
    io:format("Estimated Downtime: ~w ms~n",
        [maps:get(estimated_downtime_ms, SimulationResult)]),
    io:format("Migration Steps: ~w~n",
        [maps:get(migration_steps, SimulationResult)]),
    io:format("~n"),
    io:format("Use 'erlmcp upgrade apply ~w' to proceed with upgrade~n", [ToPlan]),
    io:format("~n").

%% Display verification results
display_verification_results(CurrentPlan, VerifyDetails) ->
    io:format("~n=== Verification Results for Plan: ~w ===~n", [CurrentPlan]),
    io:format("~n"),
    io:format("Verified: ~w~n", [maps:get(verified, VerifyDetails)]),
    io:format("~n"),
    io:format("Envelope Specification:~n"),
    Envelope = maps:get(envelope, VerifyDetails),
    io:format("  Throughput: ~w req/s~n",
        [maps:get(throughput_req_s, Envelope)]),
    io:format("  Concurrent Connections: ~w~n",
        [maps:get(concurrent_connections, Envelope)]),
    io:format("~n"),
    io:format("Current Metrics:~n"),
    Metrics = maps:get(current_metrics, VerifyDetails),
    io:format("  Throughput: ~w req/s~n",
        [maps:get(current_throughput_req_s, Metrics, 0)]),
    io:format("  Concurrent Connections: ~w~n",
        [maps:get(current_connections, Metrics, 0)]),
    io:format("~n"),
    io:format("Verification Passed: System meets plan envelope requirements~n"),
    io:format("~n").

%% Display history entry
display_history_entry(Event) ->
    Timestamp = maps:get(timestamp, Event),
    FromPlan = maps:get(from_plan, Event),
    ToPlan = maps:get(to_plan, Event),
    DateTime = calendar:system_time_to_universal_time(Timestamp div 1000, millisecond),
    FormattedTime = io_lib:format("~w", [DateTime]),
    io:format("  ~s: ~w → ~w~n", [FormattedTime, FromPlan, ToPlan]).

%% === Helper Functions ===

%% Parse plan arguments
parse_plan_args(Args) ->
    case Args of
        [FromPlanStr, ToPlanStr] ->
            try
                FromPlan = list_to_atom(FromPlanStr),
                ToPlan = list_to_atom(ToPlanStr),
                case valid_plan(FromPlan) andalso valid_plan(ToPlan) of
                    true ->
                        {ok, FromPlan, ToPlan};
                    false ->
                        {error, "Invalid plan names"}
                end
            catch
                _:_ ->
                    {error, "Invalid plan format"}
            end;
        _ ->
            {error, "Expected: <from_plan> <to_plan>"}
    end.

%% Check if plan name is valid
valid_plan(team) -> true;
valid_plan(enterprise) -> true;
valid_plan(gov) -> true;
valid_plan(_) -> false.

%% Confirm upgrade with user
confirm_upgrade(CurrentPlan, TargetPlan) ->
    io:format("~n=== Confirm Upgrade ===~n"),
    io:format("Current Plan: ~w~n", [CurrentPlan]),
    io:format("Target Plan: ~w~n", [TargetPlan]),
    io:format("~n"),
    case erlmcp_pricing_upgrade:calculate_migration_time(CurrentPlan, TargetPlan) of
        0 ->
            io:format("Estimated Time: Unknown~n");
        TimeMs ->
            Seconds = TimeMs div 1000,
            io:format("Estimated Time: ~w seconds~n", [Seconds])
    end,
    io:format("~n"),
    io:format("WARNING: This upgrade may cause service disruption~n"),
    io:format("~n"),
    Response = io:get_line("Proceed with upgrade? (yes/no): "),
    case string:strip(Response) of
        "yes" -> true;
        "YES" -> true;
        "y" -> true;
        _ -> false
    end.

%% Confirm rollback with user
confirm_rollback(CurrentPlan, PreviousPlan) ->
    io:format("~n=== Confirm Rollback ===~n"),
    io:format("Current Plan: ~w~n", [CurrentPlan]),
    io:format("Previous Plan: ~w~n", [PreviousPlan]),
    io:format("~n"),
    io:format("WARNING: Rolling back may cause service disruption~n"),
    io:format("~n"),
    Response = io:get_line("Proceed with rollback? (yes/no): "),
    case string:strip(Response) of
        "yes" -> true;
        "YES" -> true;
        "y" -> true;
        _ -> false
    end.

%% Apply confirmed upgrade
apply_upgrade_confirmed(CurrentPlan, TargetPlan) ->
    io:format("~nApplying upgrade...~n"),
    case erlmcp_pricing_upgrade:apply_upgrade(CurrentPlan, TargetPlan) of
        {error, Reason} ->
            io:format("Upgrade FAILED: ~p~n", [Reason]);
        {ok, Result} ->
            io:format("Upgrade completed successfully!~n~n"),
            io:format("Plan upgraded from ~w to ~w~n", [CurrentPlan, TargetPlan]),
            io:format("Actual Downtime: ~w ms~n",
                [maps:get(actual_downtime_ms, Result)]),
            case maps:get(rollback_available, Result) of
                true ->
                    io:format("Rollback Available: Yes~n");
                false ->
                    io:format("Rollback Available: No~n")
            end
    end.

%% === Help Text ===

show_help() ->
    io:format("~nerlmcp upgrade - Pricing tier upgrade management~n~n"),
    io:format("Usage:~n"),
    io:format("  erlmcp upgrade show <from_plan> <to_plan>   Display upgrade path~n"),
    io:format("  erlmcp upgrade plan <from_plan> <to_plan>   Simulate upgrade~n"),
    io:format("  erlmcp upgrade verify                       Check envelope compliance~n"),
    io:format("  erlmcp upgrade apply <target_plan>          Apply upgrade~n"),
    io:format("  erlmcp upgrade status                       Show current plan and history~n"),
    io:format("  erlmcp upgrade rollback <previous_plan>     Revert to previous plan~n"),
    io:format("  erlmcp upgrade history [limit]              Show upgrade history~n~n"),
    io:format("Examples:~n"),
    io:format("  erlmcp upgrade show team enterprise~n"),
    io:format("  erlmcp upgrade plan team enterprise~n"),
    io:format("  erlmcp upgrade apply enterprise~n~n").

show_plan_args_help() ->
    io:format("Valid plans: team, enterprise, gov~n"),
    io:format("Valid upgrades:~n"),
    io:format("  team → enterprise~n"),
    io:format("  enterprise → gov~n~n").

show_apply_help() ->
    io:format("Usage: erlmcp upgrade apply <target_plan>~n"),
    io:format("Valid target plans: enterprise, gov~n~n").

show_rollback_help() ->
    io:format("Usage: erlmcp upgrade rollback <previous_plan>~n"),
    io:format("Note: Downgrades are not allowed in normal operation~n~n").
