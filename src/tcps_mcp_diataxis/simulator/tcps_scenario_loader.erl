%%%-----------------------------------------------------------------------------
%%% @doc TCPS Scenario Loader and Definitions
%%%
%%% Production-grade scenario definitions for TCPS workflow simulation.
%%% Provides 6 comprehensive scenarios that demonstrate all TCPS capabilities.
%%%
%%% Scenario Types:
%%% 1. Ideal Workflow - Everything passes quality gates
%%% 2. Quality Gate Failure - Test failures trigger Andon
%%% 3. WIP Limit Overflow - Too many work orders in progress
%%% 4. Heijunka Leveling - Load balancing across buckets
%%% 5. Receipt Chain Audit - Verify complete receipt chain
%%% 6. Kaizen Cycle - Continuous improvement iteration
%%%
%%% Each scenario includes:
%%% - Initial work orders
%%% - Expected events
%%% - Success criteria
%%% - Validation rules
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_scenario_loader).

%% API exports
-export([
    load_scenario/1,
    list_scenarios/0,
    validate_scenario/1,
    get_scenario_metadata/1
]).

-export_type([scenario_id/0, scenario/0, scenario_step/0]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type scenario_id() :: ideal_workflow |
                       quality_gate_failure |
                       wip_limit_overflow |
                       heijunka_leveling |
                       receipt_chain_audit |
                       kaizen_cycle.

-type scenario_step() :: #{
    step_number := non_neg_integer(),
    action := atom(),
    params := map(),
    expected_outcome := map(),
    validation := fun((map()) -> boolean())
}.

-type scenario() :: #{
    id := scenario_id(),
    name := binary(),
    description := binary(),
    duration_seconds := non_neg_integer(),
    initial_work_orders := [map()],
    steps := [scenario_step()],
    success_criteria := [fun((map()) -> boolean())],
    config := map()
}.

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Load a scenario by ID.
%% @end
%%------------------------------------------------------------------------------
-spec load_scenario(ScenarioId :: scenario_id()) -> {ok, scenario()} | {error, term()}.
load_scenario(ideal_workflow) ->
    {ok, ideal_workflow_scenario()};
load_scenario(quality_gate_failure) ->
    {ok, quality_gate_failure_scenario()};
load_scenario(wip_limit_overflow) ->
    {ok, wip_limit_overflow_scenario()};
load_scenario(heijunka_leveling) ->
    {ok, heijunka_leveling_scenario()};
load_scenario(receipt_chain_audit) ->
    {ok, receipt_chain_audit_scenario()};
load_scenario(kaizen_cycle) ->
    {ok, kaizen_cycle_scenario()};
load_scenario(_) ->
    {error, unknown_scenario}.

%%------------------------------------------------------------------------------
%% @doc List all available scenarios.
%% @end
%%------------------------------------------------------------------------------
-spec list_scenarios() -> [scenario_id()].
list_scenarios() ->
    [
        ideal_workflow,
        quality_gate_failure,
        wip_limit_overflow,
        heijunka_leveling,
        receipt_chain_audit,
        kaizen_cycle
    ].

%%------------------------------------------------------------------------------
%% @doc Validate a scenario definition.
%% @end
%%------------------------------------------------------------------------------
-spec validate_scenario(Scenario :: scenario()) -> ok | {error, term()}.
validate_scenario(#{id := _, name := _, steps := Steps}) when is_list(Steps) ->
    ok;
validate_scenario(_) ->
    {error, invalid_scenario}.

%%------------------------------------------------------------------------------
%% @doc Get scenario metadata.
%% @end
%%------------------------------------------------------------------------------
-spec get_scenario_metadata(ScenarioId :: scenario_id()) -> {ok, map()} | {error, term()}.
get_scenario_metadata(ScenarioId) ->
    case load_scenario(ScenarioId) of
        {ok, #{id := Id, name := Name, description := Desc, duration_seconds := Duration}} ->
            {ok, #{
                id => Id,
                name => Name,
                description => Desc,
                duration => Duration
            }};
        Error ->
            Error
    end.

%%%=============================================================================
%%% Scenario Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Scenario 1: Ideal Workflow
%% Everything passes quality gates, no Andon events, complete receipts.
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec ideal_workflow_scenario() -> scenario().
ideal_workflow_scenario() ->
    #{
        id => ideal_workflow,
        name => <<"Ideal Workflow">>,
        description => <<"Demonstrates perfect TCPS workflow with all quality gates passing">>,
        duration_seconds => 60,
        initial_work_orders => [
            #{
                id => <<"wo_ideal_001">>,
                bucket => reliability,
                priority => 5,
                title => <<"Fix database connection pooling">>,
                payload => #{type => bug_fix}
            }
        ],
        steps => [
            #{
                step_number => 1,
                action => create_work_order,
                params => #{bucket => reliability},
                expected_outcome => #{status => created},
                validation => fun(Result) -> maps:get(status, Result) == created end
            },
            #{
                step_number => 2,
                action => run_quality_gates,
                params => #{gates => all},
                expected_outcome => #{all_passed => true},
                validation => fun(Result) -> maps:get(all_passed, Result, false) end
            },
            #{
                step_number => 3,
                action => complete_work_order,
                params => #{},
                expected_outcome => #{status => completed},
                validation => fun(Result) -> maps:get(status, Result) == completed end
            },
            #{
                step_number => 4,
                action => verify_receipts,
                params => #{},
                expected_outcome => #{chain_complete => true},
                validation => fun(Result) -> maps:get(chain_complete, Result, false) end
            }
        ],
        success_criteria => [
            fun(State) ->
                WorkOrders = maps:get(work_orders, State, #{}),
                maps:size(WorkOrders) > 0
            end,
            fun(State) ->
                AndonEvents = maps:get(andon_events, State, []),
                length(AndonEvents) == 0
            end,
            fun(State) ->
                Receipts = maps:get(receipts, State, []),
                length(Receipts) >= 8  % All 8 quality gates
            end
        ],
        config => #{
            wip_limit => 5,
            quality_gate_pass_rate => 1.0
        }
    }.

%%------------------------------------------------------------------------------
%% @doc Scenario 2: Quality Gate Failure
%% Test failures trigger Andon events and stop-the-line.
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec quality_gate_failure_scenario() -> scenario().
quality_gate_failure_scenario() ->
    #{
        id => quality_gate_failure,
        name => <<"Quality Gate Failure">>,
        description => <<"Demonstrates Andon stop-the-line on test failures">>,
        duration_seconds => 90,
        initial_work_orders => [
            #{
                id => <<"wo_fail_001">>,
                bucket => security,
                priority => 9,
                title => <<"Fix SQL injection vulnerability">>,
                payload => #{
                    type => security_fix,
                    inject_failure => #{
                        gate => test_execution,
                        reason => <<"Unit tests failing: 3 of 50 tests failed">>
                    }
                }
            }
        ],
        steps => [
            #{
                step_number => 1,
                action => create_work_order,
                params => #{bucket => security},
                expected_outcome => #{status => created},
                validation => fun(Result) -> maps:get(status, Result) == created end
            },
            #{
                step_number => 2,
                action => run_quality_gates,
                params => #{gates => [shacl_validation, compilation, test_execution]},
                expected_outcome => #{failed_gate => test_execution},
                validation => fun(Result) ->
                    maps:get(failed_gate, Result, undefined) == test_execution
                end
            },
            #{
                step_number => 3,
                action => trigger_andon,
                params => #{failure_type => test_failure},
                expected_outcome => #{andon_active => true},
                validation => fun(Result) -> maps:get(andon_active, Result, false) end
            },
            #{
                step_number => 4,
                action => perform_root_cause_analysis,
                params => #{},
                expected_outcome => #{root_cause_found => true},
                validation => fun(Result) -> maps:is_key(root_cause, Result) end
            },
            #{
                step_number => 5,
                action => resolve_andon,
                params => #{resolution => <<"Fixed test data setup">>},
                expected_outcome => #{andon_resolved => true},
                validation => fun(Result) -> maps:get(andon_resolved, Result, false) end
            },
            #{
                step_number => 6,
                action => retry_quality_gates,
                params => #{gates => [test_execution]},
                expected_outcome => #{all_passed => true},
                validation => fun(Result) -> maps:get(all_passed, Result, false) end
            }
        ],
        success_criteria => [
            fun(State) ->
                AndonEvents = maps:get(andon_events, State, []),
                length(AndonEvents) >= 1
            end,
            fun(State) ->
                AndonEvents = maps:get(andon_events, State, []),
                lists:any(fun(E) -> maps:get(status, E) == resolved end, AndonEvents)
            end,
            fun(State) ->
                Receipts = maps:get(receipts, State, []),
                lists:any(fun(R) ->
                    maps:get(type, R, undefined) == andon_resolution
                end, Receipts)
            end
        ],
        config => #{
            wip_limit => 5,
            quality_gate_pass_rate => 0.7,
            inject_failures => true
        }
    }.

%%------------------------------------------------------------------------------
%% @doc Scenario 3: WIP Limit Overflow
%% Demonstrates Kanban WIP limit enforcement.
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec wip_limit_overflow_scenario() -> scenario().
wip_limit_overflow_scenario() ->
    #{
        id => wip_limit_overflow,
        name => <<"WIP Limit Overflow">>,
        description => <<"Demonstrates Kanban WIP limit enforcement and pull signal refusal">>,
        duration_seconds => 120,
        initial_work_orders => [
            #{
                id => list_to_binary("wo_wip_" ++ integer_to_list(N)),
                bucket => reliability,
                priority => 5,
                title => list_to_binary("Work Order " ++ integer_to_list(N)),
                payload => #{type => feature}
            } || N <- lists:seq(1, 7)  % Exceeds WIP limit of 5
        ],
        steps => [
            #{
                step_number => 1,
                action => check_wip_limits,
                params => #{bucket => reliability},
                expected_outcome => #{available_capacity => 5},
                validation => fun(Result) -> maps:get(available_capacity, Result) == 5 end
            },
            #{
                step_number => 2,
                action => create_multiple_work_orders,
                params => #{count => 5, bucket => reliability},
                expected_outcome => #{created => 5, rejected => 0},
                validation => fun(Result) -> maps:get(created, Result) == 5 end
            },
            #{
                step_number => 3,
                action => attempt_overflow,
                params => #{count => 2, bucket => reliability},
                expected_outcome => #{created => 0, rejected => 2},
                validation => fun(Result) -> maps:get(rejected, Result) == 2 end
            },
            #{
                step_number => 4,
                action => complete_work_order,
                params => #{},
                expected_outcome => #{wip_reduced => true},
                validation => fun(Result) -> maps:get(wip_reduced, Result, false) end
            },
            #{
                step_number => 5,
                action => retry_rejected,
                params => #{},
                expected_outcome => #{created => 1},
                validation => fun(Result) -> maps:get(created, Result) >= 1 end
            }
        ],
        success_criteria => [
            fun(State) ->
                KanbanState = maps:get(kanban_state, State, #{}),
                WipLimits = maps:get(wip_limits, KanbanState, #{}),
                CurrentWip = maps:get(current_wip, KanbanState, #{}),
                maps:get(reliability, CurrentWip, 0) =< maps:get(reliability, WipLimits, 5)
            end,
            fun(State) ->
                Events = maps:get(events, State, []),
                lists:any(fun(E) ->
                    maps:get(type, E, undefined) == wip_limit_exceeded
                end, Events)
            end
        ],
        config => #{
            wip_limit => 5,
            enforce_wip => true
        }
    }.

%%------------------------------------------------------------------------------
%% @doc Scenario 4: Heijunka Leveling
%% Demonstrates load balancing across buckets.
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec heijunka_leveling_scenario() -> scenario().
heijunka_leveling_scenario() ->
    #{
        id => heijunka_leveling,
        name => <<"Heijunka Leveling">>,
        description => <<"Demonstrates production leveling across reliability, security, cost, compliance buckets">>,
        duration_seconds => 180,
        initial_work_orders => [
            #{id => <<"wo_rel_1">>, bucket => reliability, priority => 7},
            #{id => <<"wo_rel_2">>, bucket => reliability, priority => 6},
            #{id => <<"wo_sec_1">>, bucket => security, priority => 9},
            #{id => <<"wo_sec_2">>, bucket => security, priority => 8},
            #{id => <<"wo_cost_1">>, bucket => cost, priority => 5},
            #{id => <<"wo_cost_2">>, bucket => cost, priority => 4},
            #{id => <<"wo_comp_1">>, bucket => compliance, priority => 6},
            #{id => <<"wo_comp_2">>, bucket => compliance, priority => 5}
        ],
        steps => [
            #{
                step_number => 1,
                action => create_mixed_work_orders,
                params => #{buckets => [reliability, security, cost, compliance]},
                expected_outcome => #{created => 8},
                validation => fun(Result) -> maps:get(created, Result) == 8 end
            },
            #{
                step_number => 2,
                action => apply_heijunka_leveling,
                params => #{},
                expected_outcome => #{leveled => true},
                validation => fun(Result) -> maps:get(leveled, Result, false) end
            },
            #{
                step_number => 3,
                action => verify_distribution,
                params => #{},
                expected_outcome => #{balanced => true},
                validation => fun(Result) ->
                    Distribution = maps:get(distribution, Result, #{}),
                    Counts = maps:values(Distribution),
                    Max = lists:max(Counts),
                    Min = lists:min(Counts),
                    (Max - Min) =< 2  % Variance threshold
                end
            }
        ],
        success_criteria => [
            fun(State) ->
                KanbanState = maps:get(kanban_state, State, #{}),
                CurrentWip = maps:get(current_wip, KanbanState, #{}),
                Buckets = [reliability, security, cost, compliance],
                Counts = [maps:get(B, CurrentWip, 0) || B <- Buckets],
                Max = lists:max(Counts),
                Min = lists:min(Counts),
                (Max - Min) =< 2
            end
        ],
        config => #{
            wip_limit => 5,
            heijunka_enabled => true
        }
    }.

%%------------------------------------------------------------------------------
%% @doc Scenario 5: Receipt Chain Audit
%% Verifies complete receipt chain for audit trail.
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec receipt_chain_audit_scenario() -> scenario().
receipt_chain_audit_scenario() ->
    #{
        id => receipt_chain_audit,
        name => <<"Receipt Chain Audit">>,
        description => <<"Verifies complete receipt chain with cryptographic linking">>,
        duration_seconds => 90,
        initial_work_orders => [
            #{
                id => <<"wo_audit_001">>,
                bucket => compliance,
                priority => 8,
                title => <<"Implement GDPR compliance checks">>,
                payload => #{type => compliance}
            }
        ],
        steps => [
            #{
                step_number => 1,
                action => create_work_order,
                params => #{bucket => compliance},
                expected_outcome => #{receipt_generated => true},
                validation => fun(Result) -> maps:is_key(receipt_hash, Result) end
            },
            #{
                step_number => 2,
                action => run_all_quality_gates,
                params => #{},
                expected_outcome => #{receipts_per_gate => 8},
                validation => fun(Result) ->
                    maps:get(receipts_per_gate, Result, 0) == 8
                end
            },
            #{
                step_number => 3,
                action => verify_receipt_chain,
                params => #{},
                expected_outcome => #{chain_valid => true, breaks => 0},
                validation => fun(Result) ->
                    maps:get(chain_valid, Result, false) andalso
                    maps:get(breaks, Result, 1) == 0
                end
            },
            #{
                step_number => 4,
                action => audit_timestamps,
                params => #{},
                expected_outcome => #{monotonic => true},
                validation => fun(Result) -> maps:get(monotonic, Result, false) end
            }
        ],
        success_criteria => [
            fun(State) ->
                Receipts = maps:get(receipts, State, []),
                length(Receipts) >= 8
            end,
            fun(State) ->
                Receipts = maps:get(receipts, State, []),
                verify_receipt_chain_integrity(Receipts)
            end
        ],
        config => #{
            wip_limit => 5,
            strict_receipts => true
        }
    }.

%%------------------------------------------------------------------------------
%% @doc Scenario 6: Kaizen Cycle
%% Demonstrates continuous improvement iteration.
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec kaizen_cycle_scenario() -> scenario().
kaizen_cycle_scenario() ->
    #{
        id => kaizen_cycle,
        name => <<"Kaizen Cycle">>,
        description => <<"Demonstrates continuous improvement with metrics and optimization">>,
        duration_seconds => 300,
        initial_work_orders => [
            #{
                id => <<"wo_kaizen_001">>,
                bucket => reliability,
                priority => 6,
                title => <<"Optimize test execution time">>,
                payload => #{type => kaizen}
            }
        ],
        steps => [
            #{
                step_number => 1,
                action => baseline_metrics,
                params => #{},
                expected_outcome => #{baseline_captured => true},
                validation => fun(Result) -> maps:is_key(baseline, Result) end
            },
            #{
                step_number => 2,
                action => identify_waste,
                params => #{},
                expected_outcome => #{waste_found => true},
                validation => fun(Result) ->
                    length(maps:get(waste_items, Result, [])) > 0
                end
            },
            #{
                step_number => 3,
                action => implement_improvement,
                params => #{},
                expected_outcome => #{implemented => true},
                validation => fun(Result) -> maps:get(implemented, Result, false) end
            },
            #{
                step_number => 4,
                action => measure_improvement,
                params => #{},
                expected_outcome => #{improvement_percentage => positive},
                validation => fun(Result) ->
                    maps:get(improvement_percentage, Result, 0) > 0
                end
            },
            #{
                step_number => 5,
                action => standardize_change,
                params => #{},
                expected_outcome => #{standardized => true},
                validation => fun(Result) -> maps:get(standardized, Result, false) end
            }
        ],
        success_criteria => [
            fun(State) ->
                Events = maps:get(events, State, []),
                lists:any(fun(E) ->
                    maps:get(type, E, undefined) == kaizen_improvement
                end, Events)
            end,
            fun(State) ->
                Config = maps:get(config, State, #{}),
                maps:get(improvements_applied, Config, 0) > 0
            end
        ],
        config => #{
            wip_limit => 5,
            kaizen_enabled => true,
            improvements_applied => 0
        }
    }.

%%%=============================================================================
%%% Internal Helper Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Verify receipt chain integrity.
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec verify_receipt_chain_integrity([map()]) -> boolean().
verify_receipt_chain_integrity([]) ->
    true;
verify_receipt_chain_integrity([_Single]) ->
    true;
verify_receipt_chain_integrity([R1, R2 | Rest]) ->
    Hash1 = maps:get(hash, R1, undefined),
    PrevHash = maps:get(previous_hash, R2, undefined),
    case Hash1 == PrevHash of
        true -> verify_receipt_chain_integrity([R2 | Rest]);
        false -> false
    end.
