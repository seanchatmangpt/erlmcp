%% @private
%% @doc Recovery Coordinator for erlmcp v3
%% Manages multi-site recovery with RTO/RPO compliance
-module(erlmcp_recovery_coordinator).

-behaviour(gen_server).

%% API
-export([start_link/0, initiate_recovery/2, recovery_status/1,
         test_recovery/1, validate_recovery_plan/1, approve_recovery/2]).
-export([register_recovery_plan/2, get_recovery_history/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp_types.hrl").

-record(recovery_plan, {
    id :: recovery_id(),
    service :: service_id(),
    failure_scenario :: failure_type(),
    rto :: pos_integer(),
    rpo :: pos_integer(),
    steps :: [recovery_step()],
    rollback_plan :: [recovery_step()],
    dependencies :: [service_id()],
    manual_approvals :: [approval_step()],
    success_criteria :: [success_metric()],
    test_history :: [test_result()]
}).

-record(recovery_step, {
    id :: step_id(),
    description :: string(),
    action :: manual | automatic,
    timeout :: pos_integer(),  % milliseconds
    retries :: pos_integer(),
    rollback_action :: manual | automatic,
    dependencies :: [step_id()],
    preconditions :: [precondition()],
    postconditions :: [postcondition()]
}).

-record(approval_step, {
    id :: approval_id(),
    approver :: role(),
    required_approvals :: pos_integer(),
    current_approvals :: pos_integer(),
    timeout :: pos_integer()
}).

-record(recovery_state, {
    plans :: map(),  % recovery_id() => recovery_plan()
    active_recoveries :: map(),  % recovery_id() => active_recovery()
    recovery_history :: [recovery_record()],
    approval_queue :: [approval_request()],
    metrics :: map(),  % recovery metrics
    test_runner :: pid(),
    monitoring :: pid()  % Recovery monitoring
}).

-record(active_recovery, {
    id :: recovery_id(),
    start_time :: integer(),
    current_step :: step_id(),
    completed_steps :: [step_id()],
    failed_steps :: [step_id()],
    status :: pending | in_progress | completed | failed,
    target_site :: site_id(),
    actual_rto :: integer(),  % milliseconds
    actual_rpo :: integer(),  % milliseconds
    rollback_triggered :: boolean()
}).

-record(recovery_record, {
    id :: recovery_id(),
    service :: service_id(),
    start_time :: integer(),
    end_time :: integer(),
    status :: success | failure,
    rto_compliance :: boolean(),
    rpo_compliance :: boolean(),
    steps_completed :: pos_integer(),
    total_steps :: pos_integer(),
    rollback_triggered :: boolean()
}).

-record(approval_request, {
    id :: approval_id(),
    recovery_id :: recovery_id(),
    approver :: role(),
    timestamp :: integer(),
    request_data :: map(),
    status :: pending | approved | rejected,
    approval_history :: [approval_event()]
}).

-record(approval_event, {
    timestamp :: integer(),
    approver :: role(),
    decision :: approved | rejected,
    comments :: string()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec initiate_recovery(service_id(), failure_type()) -> {ok, recovery_id()} | {error, term()}.
initiate_recovery(ServiceId, FailureType) ->
    gen_server:call(?MODULE, {initiate_recovery, ServiceId, FailureType}, 30000).

-spec recovery_status(recovery_id()) -> {ok, active_recovery()} | {error, not_found}.
recovery_status(RecoveryId) ->
    gen_server:call(?MODULE, {recovery_status, RecoveryId}).

-spec test_recovery(recovery_id()) -> test_result().
test_recovery(RecoveryId) ->
    gen_server:call(?MODULE, {test_recovery, RecoveryId}, 60000).

-spec validate_recovery_plan(recovery_id()) -> validation_result().
validate_recovery_plan(RecoveryId) ->
    gen_server:call(?MODULE, {validate_recovery_plan, RecoveryId}, 30000).

-spec approve_recovery(recovery_id(), role()) -> ok | {error, term()}.
approve_recovery(RecoveryId, Approver) ->
    gen_server:call(?MODULE, {approve_recovery, RecoveryId, Approver}, 10000).

-spec register_recovery_plan(recovery_id(), recovery_plan()) -> ok.
register_recovery_plan(RecoveryId, Plan) ->
    gen_server:call(?MODULE, {register_recovery_plan, RecoveryId, Plan}).

-spec get_recovery_history(recovery_id()) -> [recovery_record()].
get_recovery_history(RecoveryId) ->
    gen_server:call(?MODULE, {get_recovery_history, RecoveryId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    % Initialize recovery state
    State = #recovery_state{
        plans = initialize_default_plans(),
        active_recoveries = #{},
        recovery_history = [],
        approval_queue = [],
        metrics = initialize_metrics(),
        test_runner = start_test_runner(),
        monitoring = start_recovery_monitoring()
    },

    % Start periodic monitoring
    erlang:send_after(5000, self(), health_check),

    % Initialize metrics
    erlmcp_metrics:register(recovery_metrics),

    {ok, State}.

handle_call({initiate_recovery, ServiceId, FailureType}, _From, State) ->
    case find_recovery_plan(ServiceId, FailureType, State) of
        {ok, RecoveryPlan} ->
            case check_preconditions(RecoveryPlan, State) of
                satisfied ->
                    RecoveryId = generate_recovery_id(),
                    Result = start_recovery(RecoveryId, RecoveryPlan, State),
                    {reply, Result, State};
                unsatisfied ->
                    {reply, {error, preconditions_not_met}, State}
            end;
        {error, not_found} ->
            {reply, {error, recovery_plan_not_found}, State}
    end;

handle_call({recovery_status, RecoveryId}, _From, State) ->
    case maps:get(RecoveryId, State#recovery_state.active_recoveries, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ActiveRecovery ->
            {reply, {ok, ActiveRecovery}, State}
    end;

handle_call({test_recovery, RecoveryId}, _From, State) ->
    case maps:get(RecoveryId, State#recovery_state.plans, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        RecoveryPlan ->
            TestResult = execute_recovery_test(RecoveryPlan, State),
            UpdatedPlan = update_test_history(RecoveryId, TestResult, State),
            {reply, TestResult, State}
    end;

handle_call({validate_recovery_plan, RecoveryId}, _From, State) ->
    case maps:get(RecoveryId, State#recovery_state.plans, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        RecoveryPlan ->
            ValidationResult = validate_plan_compliance(RecoveryPlan, State),
            {reply, ValidationResult, State}
    end;

handle_call({approve_recovery, RecoveryId, Approver}, _From, State) ->
    case process_approval(RecoveryId, Approver, State) of
        {ok, UpdatedState} ->
            {reply, ok, UpdatedState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({register_recovery_plan, RecoveryId, Plan}, _From, State) ->
    % Validate plan
    case validate_recovery_plan_structure(Plan) of
        ok ->
            UpdatedPlans = maps:put(RecoveryId, Plan, State#recovery_state.plans),
            UpdatedState = State#recovery_state{plans = UpdatedPlans},
            {reply, ok, UpdatedState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_recovery_history, RecoveryId}, _From, State) ->
    FilteredHistory = lists:filter(fun(Record) ->
        Record#recovery_record.id =:= RecoveryId
    end, State#recovery_state.recovery_history),
    {reply, FilteredHistory, State};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(health_check, State) ->
    % Monitor active recoveries
    HealthStatus = monitor_recovery_health(State),

    % Check for recoveries that need attention
    AttentionNeeded = identify_attention_needed_recoveries(State),

    lists:foreach(fun(RecoveryId) ->
        case RecoveryId of
            stale ->
                handle_stale_recovery(RecoveryId, State);
            timeout ->
                handle_timeout_recovery(RecoveryId, State);
            error ->
                handle_error_recovery(RecoveryId, State)
        end
    end, AttentionNeeded),

    % Schedule next health check
    erlang:send_after(5000, self(), health_check),
    {noreply, State};

handle_info({recovery_step_complete, RecoveryId, StepId, Result}, State) ->
    % Handle completed recovery step
    UpdatedState = handle_step_completion(RecoveryId, StepId, Result, State),

    % Check if recovery is complete
    case is_recovery_complete(RecoveryId, UpdatedState) of
        true ->
            handle_recovery_completion(RecoveryId, UpdatedState);
        false ->
            % Proceed to next step
            proceed_to_next_step(RecoveryId, UpdatedState)
    end,
    {noreply, UpdatedState};

handle_info({recovery_failed, RecoveryId, Error}, State) ->
    % Handle recovery failure
    UpdatedState = handle_recovery_failure(RecoveryId, Error, State),

    % Check if rollback is needed
    case should_trigger_rollback(RecoveryId, UpdatedState) of
        true ->
            trigger_rollback(RecoveryId, UpdatedState);
        false ->
            ok
    end,
    {noreply, UpdatedState};

handle_info(approval_timeout, State) ->
    % Handle approval timeouts
    UpdatedState = process_approval_timeouts(State),
    {noreply, UpdatedState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    % Cleanup all ongoing recoveries
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Recovery Plan Management
-spec find_recovery_plan(service_id(), failure_type(), state()) -> {ok, recovery_plan()} | {error, term()}.
find_recovery_plan(ServiceId, FailureType, State) ->
    % Search for matching recovery plan
    lists:foldl(fun(RecoveryId, RecoveryPlan, Acc) ->
        case RecoveryPlan#recovery_plan.service =:= ServiceId andalso
             RecoveryPlan#recovery_plan.failure_scenario =:= FailureType of
            true -> {ok, RecoveryPlan};
            false -> Acc
        end
    end, {error, not_found}, maps:values(State#recovery_state.plans)).

-spec start_recovery(recovery_id(), recovery_plan(), state()) -> {ok, recovery_id()} | {error, term()}.
start_recovery(RecoveryId, RecoveryPlan, State) ->
    % Create active recovery record
    ActiveRecovery = #active_recovery{
        id = RecoveryId,
        start_time = erlang:system_time(millisecond),
        current_step = RecoveryPlan#recovery_plan.steps,
        completed_steps = [],
        failed_steps = [],
        status = in_progress,
        target_site = select_target_site(RecoveryPlan),
        actual_rto = 0,
        actual_rpo = 0,
        rollback_triggered = false
    },

    % Check for required approvals
    case RecoveryPlan#recovery_plan.manual_approvals of
        [] ->
            % No approvals needed, start immediately
            start_recovery_steps(RecoveryId, RecoveryPlan, State);
        ApprovalSteps ->
            % Add to approval queue
            ApprovalRequest = #approval_request{
                id = generate_approval_id(),
                recovery_id = RecoveryId,
                approver = get_required_approver(ApprovalSteps),
                timestamp = erlang:system_time(millisecond),
                request_data = #{service => RecoveryPlan#recovery_plan.service},
                status = pending,
                approval_history = []
            },

            UpdatedState = State#recovery_state{
                active_recoveries = maps:put(RecoveryId, ActiveRecovery, State#recovery_state.active_recoveries),
                approval_queue = [ApprovalRequest | State#recovery_state.approval_queue]
            },

            {ok, RecoveryId, UpdatedState}
    end.

%% Step Execution
-spec execute_recovery_step(recovery_id(), recovery_step(), state()) -> ok | {error, term()}.
execute_recovery_step(RecoveryId, Step, State) ->
    case Step#recovery_step.action of
        automatic ->
            execute_automatic_step(RecoveryId, Step, State);
        manual ->
            execute_manual_step(RecoveryId, Step, State)
    end.

-spec execute_automatic_step(recovery_id(), recovery_step(), state()) -> ok | {error, term()}.
execute_automatic_step(RecoveryId, Step, State) ->
    % Execute step action
    StepId = Step#recovery_step.id,
    Action = Step#recovery_step.action,
    Timeout = Step#recovery_step.timeout,

    % Spawn process to execute step
    Pid = spawn_link(fun() ->
        StepResult = execute_action(Action, Step),
        gen_server:call(?MODULE, {recovery_step_complete, RecoveryId, StepId, StepResult}, Timeout)
    end),

    % Monitor step execution
    erlang:monitor(process, Pid, [{flush, true}]),
    ok.

-spec execute_manual_step(recovery_id(), recovery_step(), state()) -> ok | {error, term()}.
execute_manual_step(RecoveryId, Step, State) ->
    % Create manual execution request
    ManualRequest = #{
        recovery_id => RecoveryId,
        step_id => Step#recovery_step.id,
        description => Step#recovery_step.description,
        timeout => Step#recovery_step.timeout,
        timestamp => erlang:system_time(millisecond)
    },

    % Send to manual execution interface
    erlmcp_notification:send(manual_execution, ManualRequest),

    % Wait for manual completion
    receive
        {manual_execution_complete, RecoveryId, StepId, Result} ->
            gen_server:call(?MODULE, {recovery_step_complete, RecoveryId, StepId, Result}, 30000);
        {manual_execution_failed, RecoveryId, StepId, Error} ->
            gen_server:call(?MODULE, {recovery_failed, RecoveryId, Error}, 30000)
    after
        Step#recovery_step.timeout ->
            gen_server:call(?MODULE, {recovery_failed, RecoveryId, step_timeout}, 30000)
    end,
    ok.

%% Testing
-spec execute_recovery_test(recovery_plan(), state()) -> test_result().
execute_recovery_test(RecoveryPlan, State) ->
    % Create test environment
    TestId = generate_test_id(),
    TestStart = erlang:system_time(millisecond),

    % Execute recovery steps in test mode
    TestResults = lists:foldl(fun(Step, Acc) ->
        StepId = Step#recovery_step.id,
        TestResult = execute_test_step(Step, RecoveryPlan),
        Acc#{StepId => TestResult}
    end, #{}, RecoveryPlan#recovery_plan.steps),

    % Validate test results
    TestEnd = erlang:system_time(millisecond),
    Validation = validate_test_results(TestResults, RecoveryPlan),

    % Generate test result
    #test_result{
        test_id = TestId,
        start_time = TestStart,
        end_time = TestEnd,
        duration = TestEnd - TestStart,
        validation = Validation,
        steps = TestResults,
        success = Validation#validation.success
    }.

-spec validate_test_results(map(), recovery_plan()) -> validation_result().
validate_test_results(TestResults, RecoveryPlan) ->
    % Check all steps completed successfully
    AllSteps = RecoveryPlan#recovery_plan.steps,
    FailedSteps = lists:foldl(fun(Step, Acc) ->
        StepId = Step#recovery_step.id,
        case maps:get(StepId, TestResults, {error, not_executed}) of
            {error, _} -> [StepId | Acc];
            _ -> Acc
        end
    end, [], AllSteps),

    % Check RTO/RPO compliance
    TestDuration = calculate_test_duration(TestResults),
    RTOCompliance = TestDuration =< RecoveryPlan#recovery_plan.rto,
    RPOCompliance = validate_rpo_compliance(RecoveryPlan),

    % Generate validation result
    #validation{
        success = length(FailedSteps) =:= 0 andalso RTOCompliance andalso RPOCompliance,
        failed_steps = FailedSteps,
        rto_compliance = RTOCompliance,
        rpo_compliance = RPOCompliance,
        test_duration = TestDuration,
        timestamp = erlang:system_time(millisecond)
    }.

%% Approval Management
-spec process_approval(recovery_id(), role(), state()) -> {ok, state()} | {error, term()}.
process_approval(RecoveryId, Approver, State) ->
    % Find approval request
    case lists:keyfind(RecoveryId, #approval_request.recovery_id, State#recovery_state.approval_queue) of
        false ->
            {error, approval_not_found};
        ApprovalRequest ->
            % Validate approver
            case is_valid_approver(Approver, ApprovalRequest) of
                true ->
                    % Process approval
                    case ApprovalRequest#approval_request.status of
                        pending ->
                            ApprovalEvent = #approval_event{
                                timestamp = erlang:system_time(millisecond),
                                approver = Approver,
                                decision = approved,
                                comments = ""
                            },

                            UpdatedApprovalRequest = ApprovalRequest#approval_request{
                                status = approved,
                                approval_history = [ApprovalEvent | ApprovalRequest#approval_request.approval_history]
                            },

                            % Check if all approvals are satisfied
                            case check_approval_completion(UpdatedApprovalRequest) of
                                true ->
                                    % Remove from queue and start recovery
                                    RecoveryPlan = maps:get(RecoveryId, State#recovery_state.plans),
                                    start_recovery_steps(RecoveryId, RecoveryPlan, State);
                                false ->
                                    % Keep in queue
                                    {ok, update_approval_queue(State, UpdatedApprovalRequest)}
                            end;
                        _ ->
                            {error, already_processed}
                    end;
                false ->
                    {error, invalid_approver}
            end
    end.

%% Validation
-spec validate_plan_compliance(recovery_plan(), state()) -> validation_result().
validate_plan_compliance(RecoveryPlan, State) ->
    % Check RTO/RPO targets
    RTOValid = RecoveryPlan#recovery_plan.rto =< erlmcp_config:get(max_rto),
    RPOValid = RecoveryPlan#recovery_plan.rpo =< erlmcp_config:get(max_rpo),

    % Check dependency availability
    DependencyCheck = check_dependencies_availability(RecoveryPlan, State),

    % Check step dependencies
    StepDependenciesCheck = validate_step_dependencies(RecoveryPlan),

    % Check target site availability
    TargetSiteCheck = validate_target_site(RecoveryPlan, State),

    #validation{
        success = RTOValid andalso RPOValid andalso DependencyCheck andalso
                 StepDependenciesCheck andalso TargetSiteCheck,
        rto_compliance = RTOValid,
        rpo_compliance = RPOValid,
        dependencies_available = DependencyCheck,
        step_dependencies_valid = StepDependenciesCheck,
        target_site_available = TargetSiteCheck,
        validation_details = #{
            rto_target => RecoveryPlan#recovery_plan.rto,
            rpo_target => RecoveryPlan#recovery_plan.rpo,
            dependencies => RecoveryPlan#recovery_plan.dependencies
        }
    }.

%% State Management
-spec initialize_default_plans() -> map().
initialize_default_plans() ->
    #{
        session_failure => #recovery_plan{
            id = session_failure,
            service = session_management,
            failure_scenario = site_failure,
            rto = 900,  % 15 minutes
            rpo = 5,    % 5 seconds
            steps = [
                #recovery_step{
                    id = verify_site_failure,
                    description = "Verify primary site failure",
                    action = automatic,
                    timeout = 30000,
                    retries = 3,
                    dependencies = [],
                    preconditions = [site_unavailable],
                    postconditions = [site_confirmed_failed]
                },
                #recovery_step{
                    id = redirect_traffic,
                    description = "Redirect traffic to backup site",
                    action = automatic,
                    timeout = 60000,
                    retries = 3,
                    dependencies = [verify_site_failure],
                    preconditions = [backup_site_available],
                    postconditions = [traffic_redirected]
                },
                #recovery_step{
                    id = start_service,
                    description = "Start service on backup site",
                    action = automatic,
                    timeout = 300000,
                    retries = 5,
                    dependencies = [redirect_traffic],
                    preconditions = [service_not_running],
                    postconditions = [service_healthy]
                }
            ],
            rollback_plan = [
                #recovery_step{
                    id = rollback_redirect,
                    description = "Rollback traffic redirection",
                    action = automatic,
                    timeout = 60000,
                    retries = 3
                }
            ],
            dependencies = [registry_service],
            manual_approvals = [],
            success_criteria = [service_healthy, latency_below_200ms],
            test_history = []
        }
    }.

-spec initialize_metrics() -> map().
initialize_metrics() ->
    #{
        recovery_initiations => 0,
        successful_recoveries => 0,
        failed_recoveries => 0,
        average_recovery_time => 0.0,
        rto_compliance_rate => 0.0,
        rpo_compliance_rate => 0.0
    }.

%% Helper Functions
-spec select_target_site(recovery_plan()) -> site_id().
select_target_site(RecoveryPlan) ->
    % Implement logic to select best target site
    % Consider factors: distance, capacity, load, etc.
    site_london.

-spec check_preconditions(recovery_plan(), state()) -> satisfied | unsatisfied.
check_preconditions(RecoveryPlan, State) ->
    % Check all preconditions are met
    lists:foldl(fun(Step, Acc) ->
        case check_step_preconditions(Step, State) of
            true -> Acc;
            false -> unsatisfied
        end
    end, satisfied, RecoveryPlan#recovery_plan.steps).

-spec check_step_preconditions(recovery_step(), state()) -> boolean().
check_step_preconditions(Step, State) ->
    % Implement precondition checking
    % For each precondition in Step#recovery_step.preconditions
    true.

-spec generate_recovery_id() -> binary().
generate_recovery_id() ->
    list_to_binary("recovery-" + integer_to_list(erlang:system_time(millisecond))).

-spec generate_approval_id() -> binary().
generate_approval_id() ->
    list_to_binary("approval-" + integer_to_list(erlang:system_time(millisecond))).

%%====================================================================
%% Test Functions
%%====================================================================

-spec test_recovery_initiation() -> {ok, binary()} | {error, term()}.
test_recovery_initiation() ->
    % Test recovery initiation
    ServiceId = session_management,
    FailureType = site_failure,

    case erlmcp_recovery_coordinator:initiate_recovery(ServiceId, FailureType) of
        {ok, RecoveryId} ->
            % Verify recovery started
            case erlmcp_recovery_coordinator:recovery_status(RecoveryId) of
                {ok, _} -> {ok, RecoveryId};
                {error, _} -> {error, recovery_failed_to_start}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec test_recovery_test() -> test_result().
test_recovery_test() ->
    % Test recovery testing
    RecoveryId = test_recovery_initiation(),

    case RecoveryId of
        {ok, Id} ->
            erlmcp_recovery_coordinator:test_recovery(Id);
        {error, _} ->
            #test_result{
                success = false,
                reason = recovery_initiation_failed
            }
    end.

-spec test_approval_process() -> ok | {error, term()}.
test_approval_process() ->
    % Test approval process
    RecoveryId = generate_recovery_id(),
    Approver = emergency_coordinator,

    case erlmcp_recovery_coordinator:approve_recovery(RecoveryId, Approver) of
        ok ->
            % Verify approval processed
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec test_validation() -> validation_result().
test_validation() ->
    % Test recovery plan validation
    TestPlan = #recovery_plan{
        id = test_validation,
        service = session_management,
        failure_scenario = site_failure,
        rto = 900,
        rpo = 5,
        steps = [],
        rollback_plan = [],
        dependencies = [],
        manual_approvals = [],
        success_criteria = [],
        test_history = []
    },

    erlmcp_recovery_coordinator:register_recovery_plan(test_validation, TestPlan),
    erlmcp_recovery_coordinator:validate_recovery_plan(test_validation).