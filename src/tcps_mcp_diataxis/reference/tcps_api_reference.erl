%%%-----------------------------------------------------------------------------
%%% @doc TCPS API Reference Generator
%%%
%%% Generates comprehensive API documentation for all TCPS modules with
%%% function signatures, parameter types, return values, and usage examples.
%%%
%%% Covered Modules:
%%% - tcps_quality_gates: Quality gate enforcement API
%%% - tcps_kanban: WIP limit and Heijunka leveling API
%%% - tcps_andon: Stop-the-line event API
%%% - tcps_kaizen: Continuous improvement API
%%% - tcps_work_order: Work order lifecycle API
%%% - tcps_sku: SKU production pipeline API
%%% - tcps_receipt: Receipt generation and verification API
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_api_reference).

%% API exports
-export([
    generate_full_reference/0,
    generate_module_api/1,
    get_quality_gates_api/0,
    get_kanban_api/0,
    get_andon_api/0,
    get_kaizen_api/0,
    get_work_order_api/0,
    get_sku_api/0,
    get_receipt_api/0,
    format_api_markdown/1,
    format_api_html/1
]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type api_doc() :: #{
    module := module(),
    category := binary(),
    functions := [function_entry()],
    types := [type_entry()],
    constants := [constant_entry()],
    error_codes := [error_entry()]
}.

-type function_entry() :: #{
    name := atom(),
    arity := non_neg_integer(),
    signature := binary(),
    description := binary(),
    parameters := [param_doc()],
    returns := binary(),
    errors := [error_doc()],
    examples := [example_doc()],
    since := binary() | undefined,
    see_also := [binary()]
}.

-type param_doc() :: #{
    name := binary(),
    type := binary(),
    description := binary(),
    required := boolean(),
    default := binary() | undefined
}.

-type error_doc() :: #{
    code := atom() | binary(),
    description := binary(),
    condition := binary()
}.

-type example_doc() :: #{
    code := binary(),
    output := binary(),
    description := binary()
}.

-type type_entry() :: #{
    name := atom(),
    type := binary(),
    fields := [field_doc()],
    description := binary()
}.

-type field_doc() :: #{
    name := binary(),
    type := binary(),
    description := binary(),
    required := boolean()
}.

-type constant_entry() :: #{
    name := binary(),
    value := term(),
    description := binary()
}.

-type error_entry() :: #{
    code := binary(),
    meaning := binary(),
    resolution := binary()
}.

-export_type([api_doc/0, function_entry/0]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate complete API reference for all TCPS modules.
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_full_reference() -> [api_doc()].
generate_full_reference() ->
    [
        get_quality_gates_api(),
        get_kanban_api(),
        get_andon_api(),
        get_kaizen_api(),
        get_work_order_api(),
        get_sku_api(),
        get_receipt_api()
    ].

%%------------------------------------------------------------------------------
%% @doc Generate API documentation for a specific module.
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_module_api(module()) -> api_doc().
generate_module_api(tcps_quality_gates) -> get_quality_gates_api();
generate_module_api(tcps_kanban) -> get_kanban_api();
generate_module_api(tcps_andon) -> get_andon_api();
generate_module_api(tcps_kaizen) -> get_kaizen_api();
generate_module_api(tcps_work_order) -> get_work_order_api();
generate_module_api(tcps_sku) -> get_sku_api();
generate_module_api(tcps_receipt) -> get_receipt_api();
generate_module_api(_) -> #{module => unknown, functions => [], types => []}.

%%%=============================================================================
%%% Module-Specific API Documentation
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Quality Gates API Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_quality_gates_api() -> api_doc().
get_quality_gates_api() ->
    #{
        module => tcps_quality_gates,
        category => <<"Quality Enforcement">>,

        functions => [
            #{
                name => check_gate,
                arity => 2,
                signature => <<"check_gate(Gate :: gate_name(), SkuId :: sku_id()) -> gate_result()">>,
                description => <<"Executes a single quality gate for a SKU and returns pass/fail with receipt or violations">>,
                parameters => [
                    #{name => <<"Gate">>, type => <<"gate_name()">>,
                      description => <<"Gate to execute (shacl_validation, compilation, test_execution, etc.)">>,
                      required => true, default => undefined},
                    #{name => <<"SkuId">>, type => <<"sku_id()">>,
                      description => <<"SKU identifier">>,
                      required => true, default => undefined}
                ],
                returns => <<"{pass, receipt()} | {fail, violations()}">>,
                errors => [
                    #{code => <<"unknown_gate">>, description => <<"Gate name not recognized">>,
                      condition => <<"Invalid gate name provided">>}
                ],
                examples => [
                    #{
                        code => <<"tcps_quality_gates:check_gate(compilation, <<\"sku_12345\">>)">>,
                        output => <<"{pass, #{receipt_id => <<\"RCPT-...\">>, ...}}">>,
                        description => <<"Check compilation gate for SKU">>
                    }
                ],
                since => <<"0.1.0">>,
                see_also => [<<"check_all_gates/1">>, <<"get_gate_status/2">>]
            },

            #{
                name => check_all_gates,
                arity => 1,
                signature => <<"check_all_gates(SkuId :: sku_id()) -> {ok, [receipt()]} | {failed_at, gate_name(), violations()}">>,
                description => <<"Executes all quality gates in sequence, stops on first failure">>,
                parameters => [
                    #{name => <<"SkuId">>, type => <<"sku_id()">>,
                      description => <<"SKU identifier">>,
                      required => true, default => undefined}
                ],
                returns => <<"{ok, [receipt()]} if all pass, {failed_at, GateName, Violations} on failure">>,
                errors => [],
                examples => [
                    #{
                        code => <<"tcps_quality_gates:check_all_gates(<<\"sku_12345\">>)">>,
                        output => <<"{ok, [Receipt1, Receipt2, ...]} or {failed_at, test_execution, [Violation1, ...]}">>,
                        description => <<"Run all gates sequentially">>
                    }
                ],
                since => <<"0.1.0">>,
                see_also => [<<"check_gate/2">>]
            },

            #{
                name => get_quality_metrics,
                arity => 0,
                signature => <<"get_quality_metrics() -> map()">>,
                description => <<"Returns aggregate quality metrics including test pass rate, coverage, defect rate, first pass yield">>,
                parameters => [],
                returns => <<"#{test_pass_rate => float(), test_coverage => float(), defect_rate => float(), first_pass_yield => float(), gate_pass_rates => map()}">>,
                errors => [],
                examples => [
                    #{
                        code => <<"tcps_quality_gates:get_quality_metrics()">>,
                        output => <<"#{test_pass_rate => 0.95, test_coverage => 0.85, ...}">>,
                        description => <<"Get current quality metrics">>
                    }
                ],
                since => <<"0.2.0">>,
                see_also => [<<"check_all_gates/1">>]
            },

            #{
                name => validate_stage_transition,
                arity => 3,
                signature => <<"validate_stage_transition(SkuId, FromStage, ToStage) -> ok | {error, {blocked, term()}}">>,
                description => <<"Validates that SKU can transition between stages by checking quality gates and Andon events">>,
                parameters => [
                    #{name => <<"SkuId">>, type => <<"sku_id()">>,
                      description => <<"SKU identifier">>,
                      required => true, default => undefined},
                    #{name => <<"FromStage">>, type => <<"stage()">>,
                      description => <<"Current stage">>,
                      required => true, default => undefined},
                    #{name => <<"ToStage">>, type => <<"stage()">>,
                      description => <<"Target stage">>,
                      required => true, default => undefined}
                ],
                returns => <<"ok if transition allowed, {error, {blocked, Reason}} if blocked">>,
                errors => [
                    #{code => <<"blocked">>, description => <<"Transition blocked by open Andon or failed gate">>,
                      condition => <<"Quality gate failed or Andon event open">>}
                ],
                examples => [
                    #{
                        code => <<"tcps_quality_gates:validate_stage_transition(<<\"sku_123\">>, testing, deployment)">>,
                        output => <<"ok or {error, {blocked, {gate_failed, test_execution, [...]}}}">>,
                        description => <<"Check if SKU can move from testing to deployment">>
                    }
                ],
                since => <<"0.3.0">>,
                see_also => [<<"can_proceed/2">>]
            }
        ],

        types => [
            #{
                name => gate_name,
                type => <<"shacl_validation | compilation | test_execution | security_scan | deterministic_build | quality_metrics | release_verification | smoke_test">>,
                fields => [],
                description => <<"Quality gate types in execution order">>
            },
            #{
                name => gate_result,
                type => <<"{pass, receipt()} | {fail, violations()}">>,
                fields => [],
                description => <<"Result of gate execution with receipt or violations">>
            },
            #{
                name => quality_thresholds,
                type => <<"map()">>,
                fields => [
                    #{name => <<"test_pass_rate">>, type => <<"float()">>,
                      description => <<"Minimum 95% (0.95)">>, required => true},
                    #{name => <<"test_coverage">>, type => <<"float()">>,
                      description => <<"Minimum 80% (0.80)">>, required => true},
                    #{name => <<"defect_rate">>, type => <<"float()">>,
                      description => <<"Maximum 5% (0.05)">>, required => true},
                    #{name => <<"first_pass_yield">>, type => <<"float()">>,
                      description => <<"Minimum 90% (0.90)">>, required => true}
                ],
                description => <<"Production quality thresholds (Toyota standards)">>
            }
        ],

        constants => [
            #{name => <<"PRODUCTION_THRESHOLDS">>,
              value => #{test_pass_rate => 0.95, test_coverage => 0.80},
              description => <<"Default quality thresholds">>},
            #{name => <<"QUALITY_GATES">>,
              value => [shacl_validation, compilation, test_execution, security_scan,
                       deterministic_build, quality_metrics, release_verification, smoke_test],
              description => <<"Gates in execution order">>}
        ],

        error_codes => [
            #{code => <<"unknown_gate">>, meaning => <<"Invalid gate name">>,
              resolution => <<"Use one of: shacl_validation, compilation, test_execution, etc.">>},
            #{code => <<"blocked">>, meaning => <<"Stage transition blocked">>,
              resolution => <<"Resolve open Andon events and ensure quality gates pass">>}
        ]
    }.

%%------------------------------------------------------------------------------
%% @doc Kanban API Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_kanban_api() -> api_doc().
get_kanban_api() ->
    #{
        module => tcps_kanban,
        category => <<"WIP Management">>,

        functions => [
            #{
                name => check_wip_limit,
                arity => 1,
                signature => <<"check_wip_limit(Bucket :: bucket()) -> {ok, available} | {error, limit_reached}">>,
                description => <<"Checks if WIP limit allows new work in specified bucket">>,
                parameters => [
                    #{name => <<"Bucket">>, type => <<"bucket()">>,
                      description => <<"Bucket name (reliability, security, cost, compliance)">>,
                      required => true, default => undefined}
                ],
                returns => <<"{ok, available} if space available, {error, limit_reached} if at limit">>,
                errors => [
                    #{code => <<"limit_reached">>, description => <<"WIP limit reached for bucket">>,
                      condition => <<"Current WIP equals or exceeds limit">>}
                ],
                examples => [
                    #{
                        code => <<"tcps_kanban:check_wip_limit(reliability)">>,
                        output => <<"{ok, available} or {error, limit_reached}">>,
                        description => <<"Check if reliability bucket has capacity">>
                    }
                ],
                since => <<"0.1.0">>,
                see_also => [<<"set_wip_limit/2">>, <<"get_wip_status/1">>]
            },

            #{
                name => set_wip_limit,
                arity => 2,
                signature => <<"set_wip_limit(Bucket :: bucket(), Limit :: pos_integer() | infinity) -> ok">>,
                description => <<"Sets WIP limit for a bucket">>,
                parameters => [
                    #{name => <<"Bucket">>, type => <<"bucket()">>,
                      description => <<"Bucket name">>,
                      required => true, default => undefined},
                    #{name => <<"Limit">>, type => <<"pos_integer() | infinity">>,
                      description => <<"Maximum work items (or infinity for unlimited)">>,
                      required => true, default => undefined}
                ],
                returns => <<"ok">>,
                errors => [],
                examples => [
                    #{
                        code => <<"tcps_kanban:set_wip_limit(security, 10)">>,
                        output => <<"ok">>,
                        description => <<"Set security bucket WIP limit to 10">>
                    }
                ],
                since => <<"0.1.0">>,
                see_also => [<<"check_wip_limit/1">>]
            },

            #{
                name => process_pull_signal,
                arity => 1,
                signature => <<"process_pull_signal(Signal :: pull_signal()) -> {ok, work_order_id()} | {error, limit_reached}">>,
                description => <<"Processes pull signal and creates work order if WIP limit allows">>,
                parameters => [
                    #{name => <<"Signal">>, type => <<"pull_signal()">>,
                      description => <<"Pull signal with bucket, priority, payload">>,
                      required => true, default => undefined}
                ],
                returns => <<"{ok, WorkOrderId} if created, {error, limit_reached} if WIP full">>,
                errors => [
                    #{code => <<"limit_reached">>, description => <<"WIP limit prevents new work">>,
                      condition => <<"Bucket at capacity">>}
                ],
                examples => [
                    #{
                        code => <<"Signal = #{bucket => security, priority => 9, payload => #{cve => \"CVE-2024-1234\"}},\ntcps_kanban:process_pull_signal(Signal)">>,
                        output => <<"{ok, <<\"wo_123456\">>}">>,
                        description => <<"Process security pull signal">>
                    }
                ],
                since => <<"0.2.0">>,
                see_also => [<<"check_wip_limit/1">>, <<"complete_work_order/2">>]
            },

            #{
                name => heijunka_schedule,
                arity => 0,
                signature => <<"heijunka_schedule() -> [work_order()]">>,
                description => <<"Returns leveled schedule of all work orders using Heijunka algorithm to prevent batching">>,
                parameters => [],
                returns => <<"List of work orders in optimized execution order">>,
                errors => [],
                examples => [
                    #{
                        code => <<"tcps_kanban:heijunka_schedule()">>,
                        output => <<"[WorkOrder1, WorkOrder2, ...]">>,
                        description => <<"Get leveled production schedule">>
                    }
                ],
                since => <<"0.3.0">>,
                see_also => [<<"level_work_orders/1">>]
            }
        ],

        types => [
            #{
                name => bucket,
                type => <<"reliability | security | cost | compliance">>,
                fields => [],
                description => <<"Kanban bucket types for work categorization">>
            },
            #{
                name => work_order,
                type => <<"map()">>,
                fields => [
                    #{name => <<"id">>, type => <<"work_order_id()">>, description => <<"Unique identifier">>, required => true},
                    #{name => <<"bucket">>, type => <<"bucket()">>, description => <<"Bucket assignment">>, required => true},
                    #{name => <<"priority">>, type => <<"non_neg_integer()">>, description => <<"Priority (0-10)">>, required => true},
                    #{name => <<"status">>, type => <<"pending | in_progress | completed">>, description => <<"Current status">>, required => true},
                    #{name => <<"created_at">>, type => <<"erlang:timestamp()">>, description => <<"Creation timestamp">>, required => true}
                ],
                description => <<"Work order structure">>
            },
            #{
                name => wip_status,
                type => <<"map()">>,
                fields => [
                    #{name => <<"current">>, type => <<"non_neg_integer()">>, description => <<"Current WIP count">>, required => true},
                    #{name => <<"limit">>, type => <<"non_neg_integer() | infinity">>, description => <<"WIP limit">>, required => true},
                    #{name => <<"available">>, type => <<"non_neg_integer() | infinity">>, description => <<"Available capacity">>, required => true},
                    #{name => <<"utilization">>, type => <<"float()">>, description => <<"Utilization percentage">>, required => true}
                ],
                description => <<"WIP status for a bucket">>
            }
        ],

        constants => [
            #{name => <<"DEFAULT_WIP_LIMITS">>,
              value => #{reliability => 5, security => 5, cost => 5, compliance => 5},
              description => <<"Default WIP limits per bucket">>}
        ],

        error_codes => [
            #{code => <<"limit_reached">>, meaning => <<"WIP limit at capacity">>,
              resolution => <<"Complete existing work or increase limit">>}
        ]
    }.

%%------------------------------------------------------------------------------
%% @doc Andon API Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_andon_api() -> api_doc().
get_andon_api() ->
    #{
        module => tcps_andon,
        category => <<"Stop-the-Line">>,

        functions => [
            #{
                name => trigger_andon,
                arity => 2,
                signature => <<"trigger_andon(FailureType :: failure_type(), Context :: andon_context()) -> {ok, andon_event_id()}">>,
                description => <<"Triggers Andon stop-the-line event for a defect">>,
                parameters => [
                    #{name => <<"FailureType">>, type => <<"failure_type()">>,
                      description => <<"Type of failure (shacl_violation, test_failure, etc.)">>,
                      required => true, default => undefined},
                    #{name => <<"Context">>, type => <<"andon_context()">>,
                      description => <<"Context with sku_id, stage, details">>,
                      required => true, default => undefined}
                ],
                returns => <<"{ok, AndonEventId}">>,
                errors => [],
                examples => [
                    #{
                        code => <<"Context = #{sku_id => <<\"sku_123\">>, stage => testing, details => #{error => <<\"Test failed\">>}},\ntcps_andon:trigger_andon(test_failure, Context)">>,
                        output => <<"{ok, <<\"ANDON-...\">>}">>,
                        description => <<"Trigger Andon for test failure">>
                    }
                ],
                since => <<"0.1.0">>,
                see_also => [<<"resolve_andon/2">>, <<"get_andon_event/1">>]
            },

            #{
                name => resolve_andon,
                arity => 2,
                signature => <<"resolve_andon(AndonId :: andon_event_id(), Resolution :: resolution()) -> ok">>,
                description => <<"Resolves Andon event with root cause analysis and prevention">>,
                parameters => [
                    #{name => <<"AndonId">>, type => <<"andon_event_id()">>,
                      description => <<"Andon event identifier">>,
                      required => true, default => undefined},
                    #{name => <<"Resolution">>, type => <<"resolution()">>,
                      description => <<"Root cause, fix applied, prevention added">>,
                      required => true, default => undefined}
                ],
                returns => <<"ok">>,
                errors => [
                    #{code => <<"already_resolved">>, description => <<"Andon already resolved">>,
                      condition => <<"Attempting to resolve resolved event">>},
                    #{code => <<"andon_not_found">>, description => <<"Andon ID not found">>,
                      condition => <<"Invalid Andon ID">>}
                ],
                examples => [
                    #{
                        code => <<"Resolution = #{root_cause => <<\"Missing dependency\">>, fix_applied => <<\"Added dep\">>, prevention_added => <<\"Updated docs\">>},\ntcps_andon:resolve_andon(<<\"ANDON-...\">>, Resolution)">>,
                        output => <<"ok">>,
                        description => <<"Resolve Andon with root cause">>
                    }
                ],
                since => <<"0.1.0">>,
                see_also => [<<"trigger_andon/2">>]
            },

            #{
                name => can_proceed_to_stage,
                arity => 2,
                signature => <<"can_proceed_to_stage(SkuId :: sku_id(), Stage :: stage()) -> {ok, proceed} | {blocked, [andon_event_id()]}">>,
                description => <<"Checks if SKU can proceed to stage (no open Andon events)">>,
                parameters => [
                    #{name => <<"SkuId">>, type => <<"sku_id()">>,
                      description => <<"SKU identifier">>,
                      required => true, default => undefined},
                    #{name => <<"Stage">>, type => <<"stage()">>,
                      description => <<"Target stage">>,
                      required => true, default => undefined}
                ],
                returns => <<"{ok, proceed} if clear, {blocked, [AndonIds]} if blocked">>,
                errors => [],
                examples => [
                    #{
                        code => <<"tcps_andon:can_proceed_to_stage(<<\"sku_123\">>, deployment)">>,
                        output => <<"{ok, proceed} or {blocked, [<<\"ANDON-...\">>]}">>,
                        description => <<"Check if SKU can deploy">>
                    }
                ],
                since => <<"0.2.0">>,
                see_also => [<<"is_blocked/1">>]
            }
        ],

        types => [
            #{
                name => failure_type,
                type => <<"shacl_violation | test_failure | non_determinism | missing_receipt | compilation_failure">>,
                fields => [],
                description => <<"Types of failures that trigger Andon">>
            },
            #{
                name => andon_event,
                type => <<"map()">>,
                fields => [
                    #{name => <<"event_id">>, type => <<"andon_event_id()">>, description => <<"Unique ID">>, required => true},
                    #{name => <<"failure_type">>, type => <<"failure_type()">>, description => <<"Failure type">>, required => true},
                    #{name => <<"sku_id">>, type => <<"sku_id()">>, description => <<"Affected SKU">>, required => true},
                    #{name => <<"stage">>, type => <<"stage()">>, description => <<"Production stage">>, required => true},
                    #{name => <<"status">>, type => <<"open | resolved">>, description => <<"Event status">>, required => true},
                    #{name => <<"timestamp">>, type => <<"integer()">>, description => <<"Unix timestamp (ms)">>, required => true}
                ],
                description => <<"Andon event structure">>
            },
            #{
                name => resolution,
                type => <<"map()">>,
                fields => [
                    #{name => <<"root_cause">>, type => <<"binary()">>, description => <<"Root cause analysis">>, required => true},
                    #{name => <<"fix_applied">>, type => <<"binary()">>, description => <<"Fix description">>, required => true},
                    #{name => <<"prevention_added">>, type => <<"binary()">>, description => <<"Prevention measures">>, required => true}
                ],
                description => <<"Andon resolution structure">>
            }
        ],

        constants => [],

        error_codes => [
            #{code => <<"already_resolved">>, meaning => <<"Event already resolved">>,
              resolution => <<"Cannot modify resolved Andon">>},
            #{code => <<"andon_not_found">>, meaning => <<"Invalid Andon ID">>,
              resolution => <<"Check Andon ID is correct">>}
        ]
    }.

%%------------------------------------------------------------------------------
%% @doc Kaizen API Reference
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_kaizen_api() -> api_doc().
get_kaizen_api() ->
    #{
        module => tcps_kaizen,
        category => <<"Continuous Improvement">>,

        functions => [
            #{
                name => collect_metrics,
                arity => 1,
                signature => <<"collect_metrics(TimePeriod :: time_period()) -> metrics()">>,
                description => <<"Collects quality metrics over time period (lead time, defect rate, etc.)">>,
                parameters => [
                    #{name => <<"TimePeriod">>, type => <<"{StartDate, EndDate}">>,
                      description => <<"Date range for metrics">>,
                      required => true, default => undefined}
                ],
                returns => <<"#{lead_time => float(), defect_rate => float(), ...}">>,
                errors => [],
                examples => [
                    #{
                        code => <<"tcps_kaizen:collect_metrics({{2024,1,1}, {2024,1,31}})">>,
                        output => <<"#{lead_time => 2.5, defect_rate => 0.03, ...}">>,
                        description => <<"Collect January 2024 metrics">>
                    }
                ],
                since => <<"0.1.0">>,
                see_also => [<<"analyze_trends/1">>, <<"generate_weekly_report/1">>]
            },

            #{
                name => identify_waste_points,
                arity => 0,
                signature => <<"identify_waste_points() -> [waste_point()]">>,
                description => <<"Identifies waste (slow compilation, flaky tests, manual steps) in last 7 days">>,
                parameters => [],
                returns => <<"List of waste points sorted by impact (highest first)">>,
                errors => [],
                examples => [
                    #{
                        code => <<"tcps_kaizen:identify_waste_points()">>,
                        output => <<"[#{stage => compile, waste_type => compilation_slow, total_waste => 5.2}, ...]">>,
                        description => <<"Find current waste points">>
                    }
                ],
                since => <<"0.2.0">>,
                see_also => [<<"propose_improvements/1">>]
            },

            #{
                name => propose_improvements,
                arity => 1,
                signature => <<"propose_improvements(WastePoints :: [waste_point()]) -> [improvement()]">>,
                description => <<"Generates improvement proposals with quantified benefits from waste analysis">>,
                parameters => [
                    #{name => <<"WastePoints">>, type => <<"[waste_point()]">>,
                      description => <<"Identified waste points">>,
                      required => true, default => undefined}
                ],
                returns => <<"List of improvements sorted by ROI">>,
                errors => [],
                examples => [
                    #{
                        code => <<"Waste = tcps_kaizen:identify_waste_points(),\nImprovements = tcps_kaizen:propose_improvements(Waste)">>,
                        output => <<"[#{description => <<\"Cache dependencies\">>, expected_benefit => <<\"Save 5.2 hrs/week\">>, ...}]">>,
                        description => <<"Generate improvement proposals">>
                    }
                ],
                since => <<"0.2.0">>,
                see_also => [<<"apply_improvement/1">>]
            },

            #{
                name => generate_weekly_report,
                arity => 1,
                signature => <<"generate_weekly_report(WeekEndDate :: date()) -> map()">>,
                description => <<"Generates comprehensive weekly Kaizen report with metrics, waste, improvements, and trends">>,
                parameters => [
                    #{name => <<"WeekEndDate">>, type => <<"date()">>,
                      description => <<"End date of week">>,
                      required => true, default => undefined}
                ],
                returns => <<"Report with summary, waste_analysis, improvements, trends, recommendations">>,
                errors => [],
                examples => [
                    #{
                        code => <<"tcps_kaizen:generate_weekly_report({2024, 1, 26})">>,
                        output => <<"#{report_type => weekly_kaizen, summary => ..., waste_analysis => ..., ...}">>,
                        description => <<"Generate week ending Jan 26, 2024 report">>
                    }
                ],
                since => <<"0.3.0">>,
                see_also => [<<"collect_metrics/1">>, <<"identify_waste_points/0">>]
            }
        ],

        types => [
            #{
                name => metrics,
                type => <<"map()">>,
                fields => [
                    #{name => <<"lead_time">>, type => <<"float()">>, description => <<"Hours from work_order to publication">>, required => true},
                    #{name => <<"defect_rate">>, type => <<"float()">>, description => <<"Andon events per 100 SKUs">>, required => true},
                    #{name => <<"rework_pct">>, type => <<"float()">>, description => <<"% of SKUs requiring fixes">>, required => true},
                    #{name => <<"throughput">>, type => <<"float()">>, description => <<"SKUs per day">>, required => true}
                ],
                description => <<"Quality metrics structure">>
            },
            #{
                name => waste_point,
                type => <<"map()">>,
                fields => [
                    #{name => <<"stage">>, type => <<"stage()">>, description => <<"Production stage">>, required => true},
                    #{name => <<"waste_type">>, type => <<"waste_type()">>, description => <<"Type of waste">>, required => true},
                    #{name => <<"total_waste">>, type => <<"float()">>, description => <<"Hours wasted per week">>, required => true},
                    #{name => <<"root_cause">>, type => <<"binary()">>, description => <<"Root cause">>, required => true}
                ],
                description => <<"Identified waste point">>
            }
        ],

        constants => [
            #{name => <<"TARGET_IMPROVEMENT_RATE">>, value => 0.05,
              description => <<"Target 5% improvement per week">>}
        ],

        error_codes => []
    }.

%%------------------------------------------------------------------------------
%% @doc Work Order API Reference (abbreviated)
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_work_order_api() -> api_doc().
get_work_order_api() ->
    #{module => tcps_work_order, category => <<"Work Order Management">>,
      functions => [], types => [], constants => [], error_codes => []}.

%%------------------------------------------------------------------------------
%% @doc SKU API Reference (abbreviated)
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_sku_api() -> api_doc().
get_sku_api() ->
    #{module => tcps_sku, category => <<"SKU Production">>,
      functions => [], types => [], constants => [], error_codes => []}.

%%------------------------------------------------------------------------------
%% @doc Receipt API Reference (abbreviated)
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_receipt_api() -> api_doc().
get_receipt_api() ->
    #{module => tcps_receipt, category => <<"Receipt Management">>,
      functions => [], types => [], constants => [], error_codes => []}.

%%%=============================================================================
%%% Formatting Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Format API documentation as Markdown.
%%
%% @end
%%------------------------------------------------------------------------------
-spec format_api_markdown(api_doc()) -> binary().
format_api_markdown(#{module := Module, category := Category, functions := Functions}) ->
    Header = [
        <<"# ">>, atom_to_binary(Module, utf8), <<" API Reference\n\n">>,
        <<"**Category:** ">>, Category, <<"\n\n">>,
        <<"## Functions\n\n">>
    ],

    FunctionDocs = [format_function_markdown(F) || F <- Functions],

    iolist_to_binary([Header, FunctionDocs]).

format_function_markdown(#{name := _Name, signature := Sig, description := Desc,
                           parameters := Params, returns := Returns, examples := Examples}) ->
    [
        <<"### `">>, Sig, <<"`\n\n">>,
        Desc, <<"\n\n">>,
        <<"**Parameters:**\n">>,
        format_params_markdown(Params), <<"\n">>,
        <<"**Returns:** ">>, Returns, <<"\n\n">>,
        case Examples of
            [] -> <<>>;
            _ -> [<<"**Examples:**\n\n">>, format_examples_markdown(Examples)]
        end,
        <<"\n---\n\n">>
    ].

format_params_markdown(Params) ->
    [[<<"- `">>, maps:get(name, P), <<"` (">>, maps:get(type, P), <<"): ">>,
      maps:get(description, P), <<"\n">>] || P <- Params].

format_examples_markdown(Examples) ->
    [[<<"```erlang\n">>, maps:get(code, E), <<"\n```\n">>,
      <<"Output: `">>, maps:get(output, E), <<"`\n\n">>] || E <- Examples].

%%------------------------------------------------------------------------------
%% @doc Format API documentation as HTML.
%%
%% @end
%%------------------------------------------------------------------------------
-spec format_api_html(api_doc()) -> binary().
format_api_html(ApiDoc) ->
    %% Convert markdown to HTML
    Markdown = format_api_markdown(ApiDoc),
    %% Simple conversion (in production, use proper markdown library)
    Markdown.
