%%%-------------------------------------------------------------------
%%% @doc SwarmFlow OS Header
%%%
%%% Core data structures for the SwarmFlow OS autonomic workflow runtime.
%%% Implements YAWL/Petri-net semantics with gen_statem per workflow case.
%%%
%%% Architecture:
%%% - Each workflow instance runs as a supervised gen_statem process
%%% - Failures restart only the affected case, not the service
%%% - Event logs enable replay, conformance checking, and patch search
%%% - Process-mining swarm proposes improvements with deterministic promotion
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(SWARMFLOW_HRL).
-define(SWARMFLOW_HRL, 1).

%%% ==================================================================
%%% SwarmFlow OS Version
%%% ==================================================================
-define(SWARMFLOW_VERSION, <<"1.0.0">>).
-define(SWARMFLOW_PROTOCOL_VERSION, <<"2026.02">>).

%%% ==================================================================
%%% Workflow Definition Records (YAWL/Petri-net semantics)
%%% ==================================================================

%% Place in a Petri net - holds tokens representing state
-record(swf_place, {
    id :: binary(),                         % Unique place identifier
    name :: binary(),                       % Human-readable name
    tokens = 0 :: non_neg_integer(),        % Current token count
    capacity = infinity :: pos_integer() | infinity,  % Max tokens (bounded place)
    metadata :: map() | undefined
}).

%% Transition in a Petri net - represents an action/event
-record(swf_transition, {
    id :: binary(),                         % Unique transition identifier
    name :: binary(),                       % Human-readable name
    kind :: atom(),                         % automatic | manual | time | message | tool | resource
    guard :: fun((map()) -> boolean()) | undefined,  % Guard condition
    action :: fun((map()) -> {ok, map()} | {error, term()}) | undefined,
    timeout_ms :: pos_integer() | undefined,
    priority = 0 :: integer(),              % Higher = more priority
    metadata :: map() | undefined
}).

%% Arc connecting places and transitions
-record(swf_arc, {
    id :: binary(),
    source :: binary(),                     % Place or transition ID
    target :: binary(),                     % Place or transition ID
    weight = 1 :: pos_integer(),            % Token weight
    kind :: normal | inhibitor | reset | read,  % Arc type
    expression :: fun((map()) -> non_neg_integer()) | undefined  % Dynamic weight
}).

%% Workflow net definition (complete Petri net)
-record(swf_net, {
    id :: binary(),
    name :: binary(),
    version :: binary(),
    places :: #{binary() => #swf_place{}},
    transitions :: #{binary() => #swf_transition{}},
    arcs :: [#swf_arc{}],
    initial_marking :: #{binary() => non_neg_integer()},  % Initial tokens per place
    final_places :: [binary()],             % Places indicating completion
    metadata :: map() | undefined
}).

%%% ==================================================================
%%% YAWL-specific Constructs
%%% ==================================================================

%% YAWL split/join types
-type yawl_split_type() :: and_split | xor_split | or_split.
-type yawl_join_type() :: and_join | xor_join | or_join.

%% YAWL task (extends Petri net transition)
-record(swf_yawl_task, {
    id :: binary(),
    name :: binary(),
    split_type :: yawl_split_type() | undefined,
    join_type :: yawl_join_type() | undefined,
    mi_minimum = 1 :: pos_integer(),        % Multiple instance minimum
    mi_maximum = 1 :: pos_integer() | infinity,  % Multiple instance maximum
    mi_threshold = 1 :: pos_integer(),      % MI completion threshold
    mi_creation :: static | dynamic,        % MI creation mode
    cancellation_set :: [binary()],         % Tasks to cancel on completion
    decomposition :: binary() | undefined,  % Sub-workflow reference
    timer :: #swf_timer{} | undefined,
    resourcing :: #swf_resourcing{} | undefined
}).

%% Timer specification
-record(swf_timer, {
    trigger :: on_enabled | on_start,
    duration_ms :: pos_integer() | undefined,
    datetime :: calendar:datetime() | undefined,
    action :: skip | fail | escalate
}).

%% Resource allocation
-record(swf_resourcing, {
    offer_strategy :: direct | role_based | deferred,
    allocate_strategy :: random | round_robin | shortest_queue | user_choice,
    roles :: [binary()],
    participants :: [binary()],
    filters :: [fun((map()) -> boolean())]
}).

%%% ==================================================================
%%% Workflow Case (Instance) Records
%%% ==================================================================

%% Workflow case state
-type case_status() :: created | running | suspended | completed | failed | cancelled | compensating.

%% Workflow case - one instance of a workflow net
-record(swf_case, {
    id :: binary(),                         % Unique case identifier (UUID)
    net_id :: binary(),                     % Reference to workflow net definition
    net_version :: binary(),                % Version of net at case creation
    status = created :: case_status(),
    marking :: #{binary() => non_neg_integer()},  % Current token distribution
    variables :: map(),                     % Case data/variables
    parent_case_id :: binary() | undefined, % For sub-workflows
    root_case_id :: binary() | undefined,   % Root of case hierarchy
    created_at :: integer(),                % Unix timestamp ms
    started_at :: integer() | undefined,
    completed_at :: integer() | undefined,
    deadline :: integer() | undefined,      % Case deadline
    priority = 0 :: integer(),
    tenant_id :: binary() | undefined,      % Multi-tenant isolation
    context_id :: binary() | undefined,     % A2A context reference
    metadata :: map() | undefined
}).

%% Work item - enabled transition waiting for execution
-record(swf_work_item, {
    id :: binary(),
    case_id :: binary(),
    transition_id :: binary(),
    status :: enabled | allocated | started | completed | failed | cancelled,
    allocated_to :: binary() | undefined,   % Participant/agent ID
    started_at :: integer() | undefined,
    completed_at :: integer() | undefined,
    input_data :: map(),
    output_data :: map() | undefined,
    timer_ref :: reference() | undefined
}).

%%% ==================================================================
%%% Event Log Records (Process Mining Foundation)
%%% ==================================================================

%% Event types for the event log
-type event_type() ::
    case_created | case_started | case_completed | case_failed | case_cancelled |
    case_suspended | case_resumed | case_compensating |
    transition_enabled | transition_fired | transition_completed | transition_failed |
    token_produced | token_consumed |
    variable_set | variable_updated |
    timer_started | timer_fired | timer_cancelled |
    message_sent | message_received |
    tool_invoked | tool_completed | tool_failed |
    resource_accessed | resource_updated |
    compensation_started | compensation_completed |
    patch_proposed | patch_promoted | patch_rejected | patch_rolled_back.

%% Single event in the event log
-record(swf_event, {
    id :: binary(),                         % Unique event ID (ULID for ordering)
    case_id :: binary(),
    event_type :: event_type(),
    transition_id :: binary() | undefined,
    place_id :: binary() | undefined,
    timestamp :: integer(),                 % Unix timestamp microseconds
    sequence :: non_neg_integer(),          % Monotonic sequence within case
    data :: map(),                          % Event-specific data
    actor_id :: binary() | undefined,       % Who/what triggered the event
    correlation_id :: binary() | undefined, % For distributed tracing
    causation_id :: binary() | undefined    % Parent event ID
}).

%% Event log segment (for efficient storage/retrieval)
-record(swf_event_log_segment, {
    id :: binary(),
    case_id :: binary(),
    start_sequence :: non_neg_integer(),
    end_sequence :: non_neg_integer(),
    events :: [#swf_event{}],
    checksum :: binary(),                   % Integrity verification
    compressed :: boolean()
}).

%%% ==================================================================
%%% Process Mining Records
%%% ==================================================================

%% Conformance result
-record(swf_conformance_result, {
    case_id :: binary(),
    net_id :: binary(),
    fitness :: float(),                     % 0.0 to 1.0
    precision :: float(),                   % 0.0 to 1.0
    generalization :: float(),              % 0.0 to 1.0
    simplicity :: float(),                  % 0.0 to 1.0
    deviations :: [#swf_deviation{}],
    computed_at :: integer()
}).

%% Deviation from expected behavior
-record(swf_deviation, {
    type :: missing_token | remaining_token | wrong_transition | deadlock | livelock,
    location :: binary(),                   % Place or transition ID
    expected :: term(),
    actual :: term(),
    severity :: low | medium | high | critical,
    message :: binary()
}).

%% Patch proposal for workflow improvement
-record(swf_patch, {
    id :: binary(),
    net_id :: binary(),
    version :: binary(),                    % Target net version
    kind :: add_place | remove_place | add_transition | remove_transition |
            add_arc | remove_arc | modify_guard | modify_action |
            add_constraint | remove_constraint | restructure,
    changes :: [#swf_patch_change{}],
    rationale :: binary(),
    confidence :: float(),                  % 0.0 to 1.0
    risk_score :: float(),                  % 0.0 to 1.0
    expected_improvement :: float(),        % Expected fitness improvement
    proposed_by :: binary(),                % Swarm worker ID
    proposed_at :: integer(),
    status :: proposed | evaluating | promoted | rejected | rolled_back
}).

%% Individual change in a patch
-record(swf_patch_change, {
    operation :: add | remove | modify,
    target_type :: place | transition | arc | guard | action,
    target_id :: binary() | undefined,
    old_value :: term() | undefined,
    new_value :: term() | undefined
}).

%%% ==================================================================
%%% Swarm Worker Records
%%% ==================================================================

%% Swarm worker types
-type swarm_worker_type() ::
    conformance_checker |   % Checks case conformance to net
    replay_scorer |         % Scores replay of event logs
    patch_proposer |        % Proposes workflow improvements
    pattern_miner |         % Discovers patterns in event logs
    anomaly_detector |      % Detects anomalous behavior
    performance_analyzer |  % Analyzes workflow performance
    bottleneck_finder |     % Identifies bottlenecks
    prediction_engine.      % Predicts case outcomes

%% Swarm worker state
-record(swf_swarm_worker, {
    id :: binary(),
    type :: swarm_worker_type(),
    status :: idle | working | suspended,
    current_task :: binary() | undefined,
    metrics :: #swf_worker_metrics{},
    config :: map(),
    pid :: pid() | undefined
}).

%% Worker performance metrics
-record(swf_worker_metrics, {
    tasks_completed :: non_neg_integer(),
    tasks_failed :: non_neg_integer(),
    avg_duration_ms :: float(),
    proposals_accepted :: non_neg_integer(),
    proposals_rejected :: non_neg_integer(),
    last_active :: integer()
}).

%%% ==================================================================
%%% Promotion Policy Records
%%% ==================================================================

%% Promotion policy for autonomic updates
-record(swf_promotion_policy, {
    id :: binary(),
    name :: binary(),
    min_confidence :: float(),              % Minimum patch confidence
    max_risk_score :: float(),              % Maximum acceptable risk
    min_improvement :: float(),             % Minimum expected improvement
    evaluation_window_ms :: pos_integer(),  % Time to evaluate patch
    rollback_threshold :: float(),          % Fitness drop triggering rollback
    required_approvals :: non_neg_integer(), % Manual approvals needed (0 = auto)
    canary_percentage :: float(),           % % of cases for canary deployment
    cool_down_ms :: pos_integer()           % Time between promotions
}).

%% Promotion decision record
-record(swf_promotion_decision, {
    id :: binary(),
    patch_id :: binary(),
    policy_id :: binary(),
    decision :: promote | reject | defer,
    reason :: binary(),
    fitness_before :: float(),
    fitness_after :: float() | undefined,
    decided_at :: integer(),
    decided_by :: automatic | binary()      % automatic or approver ID
}).

%%% ==================================================================
%%% A2A/MCP Integration Records
%%% ==================================================================

%% Mapping between workflow transitions and MCP tools
-record(swf_tool_binding, {
    transition_id :: binary(),
    tool_name :: binary(),
    tool_server :: binary() | undefined,    % MCP server reference
    input_mapping :: fun((map()) -> map()), % Case vars -> tool args
    output_mapping :: fun((map()) -> map()), % Tool result -> case vars
    timeout_ms :: pos_integer(),
    retry_policy :: #swf_retry_policy{} | undefined
}).

%% Mapping between workflow and MCP resources
-record(swf_resource_binding, {
    place_id :: binary(),                   % Place representing resource state
    resource_uri :: binary(),
    read_mapping :: fun((map()) -> map()),
    write_mapping :: fun((map()) -> map()),
    subscribe :: boolean()
}).

%% Mapping between workflow and A2A tasks
-record(swf_a2a_binding, {
    case_id :: binary(),
    task_id :: binary(),                    % A2A task ID
    context_id :: binary(),                 % A2A context ID
    sync_mode :: bidirectional | case_to_task | task_to_case,
    status_mapping :: #{atom() => atom()}   % case_status -> a2a_task_state
}).

%% Retry policy
-record(swf_retry_policy, {
    max_attempts :: pos_integer(),
    initial_delay_ms :: pos_integer(),
    max_delay_ms :: pos_integer(),
    backoff_multiplier :: float(),
    retryable_errors :: [atom()] | all
}).

%%% ==================================================================
%%% Compensation Records
%%% ==================================================================

%% Compensation handler for a transition
-record(swf_compensation, {
    transition_id :: binary(),
    compensation_transition_id :: binary(), % Transition to run for compensation
    scope :: local | saga,                  % Compensation scope
    order :: fifo | lifo,                   % Compensation order
    condition :: fun((map()) -> boolean()) | undefined
}).

%% Compensation log entry
-record(swf_compensation_log, {
    id :: binary(),
    case_id :: binary(),
    transition_id :: binary(),
    compensation_data :: map(),             % Data needed for compensation
    status :: pending | completed | failed,
    logged_at :: integer(),
    compensated_at :: integer() | undefined
}).

%%% ==================================================================
%%% Performance & Metrics Records
%%% ==================================================================

%% Case performance metrics
-record(swf_case_metrics, {
    case_id :: binary(),
    duration_ms :: non_neg_integer(),
    transitions_fired :: non_neg_integer(),
    transitions_failed :: non_neg_integer(),
    tools_invoked :: non_neg_integer(),
    tool_time_ms :: non_neg_integer(),
    wait_time_ms :: non_neg_integer(),
    resource_accesses :: non_neg_integer()
}).

%% Net performance metrics (aggregated)
-record(swf_net_metrics, {
    net_id :: binary(),
    version :: binary(),
    period_start :: integer(),
    period_end :: integer(),
    cases_started :: non_neg_integer(),
    cases_completed :: non_neg_integer(),
    cases_failed :: non_neg_integer(),
    avg_duration_ms :: float(),
    p50_duration_ms :: float(),
    p95_duration_ms :: float(),
    p99_duration_ms :: float(),
    throughput :: float(),                  % Cases per second
    fitness_score :: float()
}).

%%% ==================================================================
%%% Error Codes
%%% ==================================================================

-define(SWF_ERROR_NET_NOT_FOUND, -34001).
-define(SWF_ERROR_CASE_NOT_FOUND, -34002).
-define(SWF_ERROR_TRANSITION_NOT_ENABLED, -34003).
-define(SWF_ERROR_GUARD_FAILED, -34004).
-define(SWF_ERROR_INSUFFICIENT_TOKENS, -34005).
-define(SWF_ERROR_CAPACITY_EXCEEDED, -34006).
-define(SWF_ERROR_DEADLOCK_DETECTED, -34007).
-define(SWF_ERROR_TIMEOUT, -34008).
-define(SWF_ERROR_COMPENSATION_FAILED, -34009).
-define(SWF_ERROR_INVALID_MARKING, -34010).
-define(SWF_ERROR_PATCH_INVALID, -34011).
-define(SWF_ERROR_PROMOTION_REJECTED, -34012).
-define(SWF_ERROR_REPLAY_FAILED, -34013).
-define(SWF_ERROR_TOOL_FAILED, -34014).
-define(SWF_ERROR_RESOURCE_UNAVAILABLE, -34015).

%%% ==================================================================
%%% Type Exports
%%% ==================================================================

-export_type([
    case_status/0,
    event_type/0,
    swarm_worker_type/0,
    yawl_split_type/0,
    yawl_join_type/0
]).

-endif.
