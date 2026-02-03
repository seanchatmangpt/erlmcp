%%%-------------------------------------------------------------------
%%% @doc SwarmFlow Promotion Engine
%%%
%%% Autonomic promotion/rollback policy engine for workflow patches.
%%% Implements canary deployments, A/B testing, automatic rollback on
%%% fitness degradation, and approval workflows.
%%%
%%% This module manages the lifecycle of workflow patches from proposal
%%% through evaluation, promotion, and potential rollback. It enforces
%%% promotion policies including cool-down periods and approval requirements.
%%%
%%% Key Features:
%%% - Policy-based promotion decisions
%%% - Canary deployment with gradual rollout
%%% - A/B testing of patches
%%% - Automatic rollback on fitness degradation
%%% - Cool-down period enforcement
%%% - Manual approval workflow
%%% - Complete decision audit trail
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swf_promotion_engine).
-behaviour(gen_server).

-include("swarmflow.hrl").

%% API
-export([start_link/0]).
-export([register_policy/1,
         update_policy/2,
         get_policy/1,
         delete_policy/1,
         list_policies/0]).
-export([evaluate_patch/2,
         promote_patch/1,
         rollback_patch/1,
         get_patch_status/1]).
-export([get_decisions/1,
         get_decision/1]).
-export([set_canary_percentage/2,
         get_canary_percentage/1,
         get_canary_status/1]).
-export([request_approval/2,
         approve/2,
         reject/2,
         get_pending_approvals/0,
         get_pending_approvals/1]).
-export([start_ab_test/3,
         stop_ab_test/1,
         get_ab_test_results/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal exports for testing
-export([generate_id/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type policy_id() :: binary().
-type patch_id() :: binary().
-type decision_id() :: binary().
-type net_id() :: binary().
-type approver_id() :: binary().

-record(approval_request, {
    id :: binary(),
    patch_id :: patch_id(),
    policy_id :: policy_id(),
    requested_by :: binary(),
    requested_at :: integer(),
    approvers :: [approver_id()],
    approvals :: #{approver_id() => approve | reject},
    required_count :: non_neg_integer(),
    status :: pending | approved | rejected | expired,
    expires_at :: integer() | undefined
}).

-record(canary_state, {
    net_id :: net_id(),
    patch_id :: patch_id(),
    percentage :: float(),
    started_at :: integer(),
    cases_total :: non_neg_integer(),
    cases_canary :: non_neg_integer(),
    fitness_control :: float(),
    fitness_canary :: float(),
    status :: running | completed | rolled_back
}).

-record(ab_test, {
    id :: binary(),
    net_id :: net_id(),
    patch_a :: patch_id(),
    patch_b :: patch_id(),
    traffic_split :: float(),           % Percentage going to variant B
    started_at :: integer(),
    cases_a :: non_neg_integer(),
    cases_b :: non_neg_integer(),
    fitness_a :: float(),
    fitness_b :: float(),
    status :: running | completed | stopped
}).

-record(state, {
    policies :: #{policy_id() => #swf_promotion_policy{}},
    patches :: #{patch_id() => #swf_patch{}},
    decisions :: #{decision_id() => #swf_promotion_decision{}},
    decisions_by_patch :: #{patch_id() => [decision_id()]},
    approval_requests :: #{binary() => #approval_request{}},
    canary_states :: #{net_id() => #canary_state{}},
    ab_tests :: #{binary() => #ab_test{}},
    cool_downs :: #{net_id() => integer()},  % Last promotion time per net
    fitness_monitors :: #{patch_id() => reference()}
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the promotion engine
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register a new promotion policy
-spec register_policy(#swf_promotion_policy{}) -> {ok, policy_id()} | {error, term()}.
register_policy(Policy) when is_record(Policy, swf_promotion_policy) ->
    gen_server:call(?MODULE, {register_policy, Policy}).

%% @doc Update an existing promotion policy
-spec update_policy(policy_id(), map()) -> ok | {error, term()}.
update_policy(PolicyId, Updates) when is_binary(PolicyId), is_map(Updates) ->
    gen_server:call(?MODULE, {update_policy, PolicyId, Updates}).

%% @doc Get a promotion policy by ID
-spec get_policy(policy_id()) -> {ok, #swf_promotion_policy{}} | {error, not_found}.
get_policy(PolicyId) when is_binary(PolicyId) ->
    gen_server:call(?MODULE, {get_policy, PolicyId}).

%% @doc Delete a promotion policy
-spec delete_policy(policy_id()) -> ok | {error, term()}.
delete_policy(PolicyId) when is_binary(PolicyId) ->
    gen_server:call(?MODULE, {delete_policy, PolicyId}).

%% @doc List all promotion policies
-spec list_policies() -> {ok, [#swf_promotion_policy{}]}.
list_policies() ->
    gen_server:call(?MODULE, list_policies).

%% @doc Evaluate a patch against a policy
-spec evaluate_patch(#swf_patch{}, policy_id()) ->
    {ok, #swf_promotion_decision{}} | {error, term()}.
evaluate_patch(Patch, PolicyId) when is_record(Patch, swf_patch), is_binary(PolicyId) ->
    gen_server:call(?MODULE, {evaluate_patch, Patch, PolicyId}).

%% @doc Promote a patch (apply to net)
-spec promote_patch(patch_id()) -> ok | {error, term()}.
promote_patch(PatchId) when is_binary(PatchId) ->
    gen_server:call(?MODULE, {promote_patch, PatchId}).

%% @doc Rollback a promoted patch
-spec rollback_patch(patch_id()) -> ok | {error, term()}.
rollback_patch(PatchId) when is_binary(PatchId) ->
    gen_server:call(?MODULE, {rollback_patch, PatchId}).

%% @doc Get the status of a patch
-spec get_patch_status(patch_id()) -> {ok, #swf_patch{}} | {error, not_found}.
get_patch_status(PatchId) when is_binary(PatchId) ->
    gen_server:call(?MODULE, {get_patch_status, PatchId}).

%% @doc Get all promotion decisions for a patch
-spec get_decisions(patch_id()) -> {ok, [#swf_promotion_decision{}]}.
get_decisions(PatchId) when is_binary(PatchId) ->
    gen_server:call(?MODULE, {get_decisions, PatchId}).

%% @doc Get a specific decision by ID
-spec get_decision(decision_id()) -> {ok, #swf_promotion_decision{}} | {error, not_found}.
get_decision(DecisionId) when is_binary(DecisionId) ->
    gen_server:call(?MODULE, {get_decision, DecisionId}).

%% @doc Set canary percentage for a net
-spec set_canary_percentage(net_id(), float()) -> ok | {error, term()}.
set_canary_percentage(NetId, Percentage)
  when is_binary(NetId), is_float(Percentage), Percentage >= 0.0, Percentage =< 1.0 ->
    gen_server:call(?MODULE, {set_canary_percentage, NetId, Percentage}).

%% @doc Get current canary percentage for a net
-spec get_canary_percentage(net_id()) -> {ok, float()} | {error, not_found}.
get_canary_percentage(NetId) when is_binary(NetId) ->
    gen_server:call(?MODULE, {get_canary_percentage, NetId}).

%% @doc Get full canary state for a net
-spec get_canary_status(net_id()) -> {ok, #canary_state{}} | {error, not_found}.
get_canary_status(NetId) when is_binary(NetId) ->
    gen_server:call(?MODULE, {get_canary_status, NetId}).

%% @doc Request manual approval for a patch
-spec request_approval(patch_id(), [approver_id()]) -> {ok, binary()} | {error, term()}.
request_approval(PatchId, Approvers) when is_binary(PatchId), is_list(Approvers) ->
    gen_server:call(?MODULE, {request_approval, PatchId, Approvers}).

%% @doc Approve a patch
-spec approve(patch_id(), approver_id()) -> ok | {error, term()}.
approve(PatchId, ApproverId) when is_binary(PatchId), is_binary(ApproverId) ->
    gen_server:call(?MODULE, {approve, PatchId, ApproverId}).

%% @doc Reject a patch
-spec reject(patch_id(), approver_id()) -> ok | {error, term()}.
reject(PatchId, ApproverId) when is_binary(PatchId), is_binary(ApproverId) ->
    gen_server:call(?MODULE, {reject, PatchId, ApproverId}).

%% @doc Get all pending approval requests
-spec get_pending_approvals() -> {ok, [#approval_request{}]}.
get_pending_approvals() ->
    gen_server:call(?MODULE, get_pending_approvals).

%% @doc Get pending approval requests for an approver
-spec get_pending_approvals(approver_id()) -> {ok, [#approval_request{}]}.
get_pending_approvals(ApproverId) when is_binary(ApproverId) ->
    gen_server:call(?MODULE, {get_pending_approvals, ApproverId}).

%% @doc Start an A/B test between two patches
-spec start_ab_test(net_id(), patch_id(), patch_id()) -> {ok, binary()} | {error, term()}.
start_ab_test(NetId, PatchA, PatchB)
  when is_binary(NetId), is_binary(PatchA), is_binary(PatchB) ->
    gen_server:call(?MODULE, {start_ab_test, NetId, PatchA, PatchB}).

%% @doc Stop an A/B test
-spec stop_ab_test(binary()) -> ok | {error, term()}.
stop_ab_test(TestId) when is_binary(TestId) ->
    gen_server:call(?MODULE, {stop_ab_test, TestId}).

%% @doc Get A/B test results
-spec get_ab_test_results(binary()) -> {ok, #ab_test{}} | {error, not_found}.
get_ab_test_results(TestId) when is_binary(TestId) ->
    gen_server:call(?MODULE, {get_ab_test_results, TestId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    State = #state{
        policies = #{},
        patches = #{},
        decisions = #{},
        decisions_by_patch = #{},
        approval_requests = #{},
        canary_states = #{},
        ab_tests = #{},
        cool_downs = #{},
        fitness_monitors = #{}
    },
    {ok, State}.

handle_call({register_policy, Policy}, _From, State) ->
    PolicyId = case Policy#swf_promotion_policy.id of
        undefined -> generate_id();
        Id -> Id
    end,
    PolicyWithId = Policy#swf_promotion_policy{id = PolicyId},
    case validate_policy(PolicyWithId) of
        ok ->
            NewPolicies = maps:put(PolicyId, PolicyWithId, State#state.policies),
            {reply, {ok, PolicyId}, State#state{policies = NewPolicies}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({update_policy, PolicyId, Updates}, _From, State) ->
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            UpdatedPolicy = apply_policy_updates(Policy, Updates),
            case validate_policy(UpdatedPolicy) of
                ok ->
                    NewPolicies = maps:put(PolicyId, UpdatedPolicy, State#state.policies),
                    {reply, ok, State#state{policies = NewPolicies}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_policy, PolicyId}, _From, State) ->
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} -> {reply, {ok, Policy}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({delete_policy, PolicyId}, _From, State) ->
    case maps:is_key(PolicyId, State#state.policies) of
        true ->
            NewPolicies = maps:remove(PolicyId, State#state.policies),
            {reply, ok, State#state{policies = NewPolicies}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_policies, _From, State) ->
    Policies = maps:values(State#state.policies),
    {reply, {ok, Policies}, State};

handle_call({evaluate_patch, Patch, PolicyId}, _From, State) ->
    case maps:find(PolicyId, State#state.policies) of
        {ok, Policy} ->
            {Decision, NewState} = do_evaluate_patch(Patch, Policy, State),
            {reply, {ok, Decision}, NewState};
        error ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call({promote_patch, PatchId}, _From, State) ->
    case maps:find(PatchId, State#state.patches) of
        {ok, Patch} ->
            case can_promote(Patch, State) of
                {ok, NewState} ->
                    PromotedPatch = Patch#swf_patch{status = promoted},
                    NewPatches = maps:put(PatchId, PromotedPatch, NewState#state.patches),

                    %% Record promotion decision
                    Decision = create_decision(PatchId, <<"default">>, promote,
                                               <<"Patch promoted">>, automatic),
                    NewState2 = store_decision(Decision, NewState#state{patches = NewPatches}),

                    %% Start fitness monitoring
                    NewState3 = start_fitness_monitoring(PatchId, NewState2),

                    %% Update cool-down
                    NetId = Patch#swf_patch.net_id,
                    Now = erlang:system_time(millisecond),
                    NewCoolDowns = maps:put(NetId, Now, NewState3#state.cool_downs),

                    {reply, ok, NewState3#state{cool_downs = NewCoolDowns}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({rollback_patch, PatchId}, _From, State) ->
    case maps:find(PatchId, State#state.patches) of
        {ok, Patch} when Patch#swf_patch.status =:= promoted ->
            RolledBackPatch = Patch#swf_patch{status = rolled_back},
            NewPatches = maps:put(PatchId, RolledBackPatch, State#state.patches),

            %% Record rollback decision
            Decision = create_decision(PatchId, <<"default">>, reject,
                                       <<"Patch rolled back">>, automatic),
            NewState = store_decision(Decision, State#state{patches = NewPatches}),

            %% Stop fitness monitoring
            NewState2 = stop_fitness_monitoring(PatchId, NewState),

            %% Clean up canary state if exists
            NetId = Patch#swf_patch.net_id,
            NewCanaryStates = maps:remove(NetId, NewState2#state.canary_states),

            {reply, ok, NewState2#state{canary_states = NewCanaryStates}};
        {ok, _Patch} ->
            {reply, {error, not_promoted}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_patch_status, PatchId}, _From, State) ->
    case maps:find(PatchId, State#state.patches) of
        {ok, Patch} -> {reply, {ok, Patch}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({get_decisions, PatchId}, _From, State) ->
    DecisionIds = maps:get(PatchId, State#state.decisions_by_patch, []),
    Decisions = lists:filtermap(
        fun(DecisionId) ->
            case maps:find(DecisionId, State#state.decisions) of
                {ok, D} -> {true, D};
                error -> false
            end
        end,
        DecisionIds),
    {reply, {ok, Decisions}, State};

handle_call({get_decision, DecisionId}, _From, State) ->
    case maps:find(DecisionId, State#state.decisions) of
        {ok, Decision} -> {reply, {ok, Decision}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({set_canary_percentage, NetId, Percentage}, _From, State) ->
    case maps:find(NetId, State#state.canary_states) of
        {ok, Canary} ->
            UpdatedCanary = Canary#canary_state{percentage = Percentage},
            NewCanaryStates = maps:put(NetId, UpdatedCanary, State#state.canary_states),
            {reply, ok, State#state{canary_states = NewCanaryStates}};
        error ->
            %% Create new canary state
            Canary = #canary_state{
                net_id = NetId,
                patch_id = undefined,
                percentage = Percentage,
                started_at = erlang:system_time(millisecond),
                cases_total = 0,
                cases_canary = 0,
                fitness_control = 0.0,
                fitness_canary = 0.0,
                status = running
            },
            NewCanaryStates = maps:put(NetId, Canary, State#state.canary_states),
            {reply, ok, State#state{canary_states = NewCanaryStates}}
    end;

handle_call({get_canary_percentage, NetId}, _From, State) ->
    case maps:find(NetId, State#state.canary_states) of
        {ok, Canary} -> {reply, {ok, Canary#canary_state.percentage}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({get_canary_status, NetId}, _From, State) ->
    case maps:find(NetId, State#state.canary_states) of
        {ok, Canary} -> {reply, {ok, Canary}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({request_approval, PatchId, Approvers}, _From, State) ->
    case maps:find(PatchId, State#state.patches) of
        {ok, Patch} when Patch#swf_patch.status =:= evaluating ->
            RequestId = generate_id(),
            Now = erlang:system_time(millisecond),
            Request = #approval_request{
                id = RequestId,
                patch_id = PatchId,
                policy_id = <<"default">>,
                requested_by = Patch#swf_patch.proposed_by,
                requested_at = Now,
                approvers = Approvers,
                approvals = #{},
                required_count = length(Approvers),
                status = pending,
                expires_at = Now + (24 * 60 * 60 * 1000)  % 24 hours
            },
            NewRequests = maps:put(RequestId, Request, State#state.approval_requests),
            {reply, {ok, RequestId}, State#state{approval_requests = NewRequests}};
        {ok, _Patch} ->
            {reply, {error, invalid_patch_status}, State};
        error ->
            {reply, {error, patch_not_found}, State}
    end;

handle_call({approve, PatchId, ApproverId}, _From, State) ->
    case find_approval_request(PatchId, State) of
        {ok, Request} ->
            case lists:member(ApproverId, Request#approval_request.approvers) of
                true ->
                    NewApprovals = maps:put(ApproverId, approve, Request#approval_request.approvals),
                    ApprovalCount = count_approvals(NewApprovals),
                    NewStatus = case ApprovalCount >= Request#approval_request.required_count of
                        true -> approved;
                        false -> pending
                    end,
                    UpdatedRequest = Request#approval_request{
                        approvals = NewApprovals,
                        status = NewStatus
                    },
                    NewRequests = maps:put(Request#approval_request.id, UpdatedRequest,
                                          State#state.approval_requests),
                    NewState = State#state{approval_requests = NewRequests},

                    %% If fully approved, record decision
                    FinalState = case NewStatus of
                        approved ->
                            Decision = create_decision(PatchId, Request#approval_request.policy_id,
                                                      promote, <<"Approved by reviewers">>, ApproverId),
                            store_decision(Decision, NewState);
                        pending ->
                            NewState
                    end,
                    {reply, ok, FinalState};
                false ->
                    {reply, {error, not_authorized}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({reject, PatchId, ApproverId}, _From, State) ->
    case find_approval_request(PatchId, State) of
        {ok, Request} ->
            case lists:member(ApproverId, Request#approval_request.approvers) of
                true ->
                    NewApprovals = maps:put(ApproverId, reject, Request#approval_request.approvals),
                    UpdatedRequest = Request#approval_request{
                        approvals = NewApprovals,
                        status = rejected
                    },
                    NewRequests = maps:put(Request#approval_request.id, UpdatedRequest,
                                          State#state.approval_requests),

                    %% Update patch status
                    {ok, Patch} = maps:find(PatchId, State#state.patches),
                    RejectedPatch = Patch#swf_patch{status = rejected},
                    NewPatches = maps:put(PatchId, RejectedPatch, State#state.patches),

                    %% Record decision
                    Decision = create_decision(PatchId, Request#approval_request.policy_id,
                                              reject, <<"Rejected by reviewer">>, ApproverId),
                    NewState = store_decision(Decision, State#state{
                        approval_requests = NewRequests,
                        patches = NewPatches
                    }),
                    {reply, ok, NewState};
                false ->
                    {reply, {error, not_authorized}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_pending_approvals, _From, State) ->
    PendingRequests = maps:fold(
        fun(_Id, Request, Acc) ->
            case Request#approval_request.status of
                pending -> [Request | Acc];
                _ -> Acc
            end
        end,
        [],
        State#state.approval_requests),
    {reply, {ok, PendingRequests}, State};

handle_call({get_pending_approvals, ApproverId}, _From, State) ->
    PendingRequests = maps:fold(
        fun(_Id, Request, Acc) ->
            IsPending = Request#approval_request.status =:= pending,
            IsApprover = lists:member(ApproverId, Request#approval_request.approvers),
            NotYetVoted = not maps:is_key(ApproverId, Request#approval_request.approvals),
            case IsPending andalso IsApprover andalso NotYetVoted of
                true -> [Request | Acc];
                false -> Acc
            end
        end,
        [],
        State#state.approval_requests),
    {reply, {ok, PendingRequests}, State};

handle_call({start_ab_test, NetId, PatchA, PatchB}, _From, State) ->
    TestId = generate_id(),
    Now = erlang:system_time(millisecond),
    Test = #ab_test{
        id = TestId,
        net_id = NetId,
        patch_a = PatchA,
        patch_b = PatchB,
        traffic_split = 0.5,
        started_at = Now,
        cases_a = 0,
        cases_b = 0,
        fitness_a = 0.0,
        fitness_b = 0.0,
        status = running
    },
    NewTests = maps:put(TestId, Test, State#state.ab_tests),
    {reply, {ok, TestId}, State#state{ab_tests = NewTests}};

handle_call({stop_ab_test, TestId}, _From, State) ->
    case maps:find(TestId, State#state.ab_tests) of
        {ok, Test} ->
            StoppedTest = Test#ab_test{status = stopped},
            NewTests = maps:put(TestId, StoppedTest, State#state.ab_tests),
            {reply, ok, State#state{ab_tests = NewTests}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_ab_test_results, TestId}, _From, State) ->
    case maps:find(TestId, State#state.ab_tests) of
        {ok, Test} -> {reply, {ok, Test}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({fitness_check, PatchId}, State) ->
    case maps:find(PatchId, State#state.patches) of
        {ok, Patch} when Patch#swf_patch.status =:= promoted ->
            case check_fitness_degradation(Patch, State) of
                {degraded, NewFitness} ->
                    %% Automatic rollback
                    RolledBackPatch = Patch#swf_patch{status = rolled_back},
                    NewPatches = maps:put(PatchId, RolledBackPatch, State#state.patches),

                    Reason = io_lib:format("Fitness degraded to ~.2f", [NewFitness]),
                    Decision = create_decision(PatchId, <<"default">>, reject,
                                              list_to_binary(Reason), automatic),
                    NewState = store_decision(Decision, State#state{patches = NewPatches}),
                    NewState2 = stop_fitness_monitoring(PatchId, NewState),
                    {noreply, NewState2};
                ok ->
                    %% Continue monitoring
                    {noreply, State}
            end;
        _ ->
            %% Patch no longer promoted, stop monitoring
            NewState = stop_fitness_monitoring(PatchId, State),
            {noreply, NewState}
    end;

handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel all fitness monitors
    maps:foreach(
        fun(_PatchId, TimerRef) ->
            erlang:cancel_timer(TimerRef)
        end,
        State#state.fitness_monitors),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Generate a unique ID
-spec generate_id() -> binary().
generate_id() ->
    Bytes = crypto:strong_rand_bytes(16),
    <<A:32, B:16, C:16, D:16, E:48>> = Bytes,
    list_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                  [A, B, C, D, E])).

%% @private Validate a promotion policy
-spec validate_policy(#swf_promotion_policy{}) -> ok | {error, term()}.
validate_policy(Policy) ->
    Checks = [
        {Policy#swf_promotion_policy.min_confidence >= 0.0 andalso
         Policy#swf_promotion_policy.min_confidence =< 1.0,
         invalid_min_confidence},
        {Policy#swf_promotion_policy.max_risk_score >= 0.0 andalso
         Policy#swf_promotion_policy.max_risk_score =< 1.0,
         invalid_max_risk_score},
        {Policy#swf_promotion_policy.min_improvement >= 0.0,
         invalid_min_improvement},
        {Policy#swf_promotion_policy.rollback_threshold >= 0.0 andalso
         Policy#swf_promotion_policy.rollback_threshold =< 1.0,
         invalid_rollback_threshold},
        {Policy#swf_promotion_policy.canary_percentage >= 0.0 andalso
         Policy#swf_promotion_policy.canary_percentage =< 1.0,
         invalid_canary_percentage},
        {Policy#swf_promotion_policy.evaluation_window_ms > 0,
         invalid_evaluation_window},
        {Policy#swf_promotion_policy.cool_down_ms > 0,
         invalid_cool_down}
    ],
    case lists:dropwhile(fun({true, _}) -> true; (_) -> false end, Checks) of
        [] -> ok;
        [{false, Reason} | _] -> {error, Reason}
    end.

%% @private Apply updates to a policy
-spec apply_policy_updates(#swf_promotion_policy{}, map()) -> #swf_promotion_policy{}.
apply_policy_updates(Policy, Updates) ->
    maps:fold(
        fun(min_confidence, V, P) -> P#swf_promotion_policy{min_confidence = V};
           (max_risk_score, V, P) -> P#swf_promotion_policy{max_risk_score = V};
           (min_improvement, V, P) -> P#swf_promotion_policy{min_improvement = V};
           (evaluation_window_ms, V, P) -> P#swf_promotion_policy{evaluation_window_ms = V};
           (rollback_threshold, V, P) -> P#swf_promotion_policy{rollback_threshold = V};
           (required_approvals, V, P) -> P#swf_promotion_policy{required_approvals = V};
           (canary_percentage, V, P) -> P#swf_promotion_policy{canary_percentage = V};
           (cool_down_ms, V, P) -> P#swf_promotion_policy{cool_down_ms = V};
           (name, V, P) -> P#swf_promotion_policy{name = V};
           (_, _, P) -> P
        end,
        Policy,
        Updates).

%% @private Evaluate a patch against a policy
-spec do_evaluate_patch(#swf_patch{}, #swf_promotion_policy{}, #state{}) ->
    {#swf_promotion_decision{}, #state{}}.
do_evaluate_patch(Patch, Policy, State) ->
    Now = erlang:system_time(millisecond),
    PatchId = Patch#swf_patch.id,
    NetId = Patch#swf_patch.net_id,

    %% Store patch if not already stored
    PatchWithStatus = Patch#swf_patch{status = evaluating},
    NewPatches = maps:put(PatchId, PatchWithStatus, State#state.patches),
    State1 = State#state{patches = NewPatches},

    %% Run evaluation checks
    {Decision, Reason} = evaluate_against_policy(Patch, Policy, State1),

    %% Get current fitness (would normally come from swf_conformance module)
    FitnessBefore = get_current_fitness(NetId),

    PromotionDecision = #swf_promotion_decision{
        id = generate_id(),
        patch_id = PatchId,
        policy_id = Policy#swf_promotion_policy.id,
        decision = Decision,
        reason = Reason,
        fitness_before = FitnessBefore,
        fitness_after = undefined,
        decided_at = Now,
        decided_by = automatic
    },

    %% Store decision
    State2 = store_decision(PromotionDecision, State1),

    %% Update patch status based on decision
    UpdatedPatch = case Decision of
        promote -> PatchWithStatus;  % Keep as evaluating until promoted
        reject -> PatchWithStatus#swf_patch{status = rejected};
        defer -> PatchWithStatus
    end,
    NewPatches2 = maps:put(PatchId, UpdatedPatch, State2#state.patches),

    {PromotionDecision, State2#state{patches = NewPatches2}}.

%% @private Evaluate patch against policy rules
-spec evaluate_against_policy(#swf_patch{}, #swf_promotion_policy{}, #state{}) ->
    {promote | reject | defer, binary()}.
evaluate_against_policy(Patch, Policy, State) ->
    Confidence = Patch#swf_patch.confidence,
    RiskScore = Patch#swf_patch.risk_score,
    ExpectedImprovement = Patch#swf_patch.expected_improvement,
    NetId = Patch#swf_patch.net_id,

    %% Check cool-down period
    CoolDownOk = check_cool_down(NetId, Policy#swf_promotion_policy.cool_down_ms, State),

    %% Check policy thresholds
    ConfidenceOk = Confidence >= Policy#swf_promotion_policy.min_confidence,
    RiskOk = RiskScore =< Policy#swf_promotion_policy.max_risk_score,
    ImprovementOk = ExpectedImprovement >= Policy#swf_promotion_policy.min_improvement,

    %% Check if manual approval is required
    RequiresApproval = Policy#swf_promotion_policy.required_approvals > 0,

    if
        not CoolDownOk ->
            {defer, <<"Cool-down period not elapsed">>};
        not ConfidenceOk ->
            {reject, list_to_binary(io_lib:format(
                "Confidence ~.2f below threshold ~.2f",
                [Confidence, Policy#swf_promotion_policy.min_confidence]))};
        not RiskOk ->
            {reject, list_to_binary(io_lib:format(
                "Risk score ~.2f exceeds threshold ~.2f",
                [RiskScore, Policy#swf_promotion_policy.max_risk_score]))};
        not ImprovementOk ->
            {reject, list_to_binary(io_lib:format(
                "Expected improvement ~.2f below threshold ~.2f",
                [ExpectedImprovement, Policy#swf_promotion_policy.min_improvement]))};
        RequiresApproval ->
            {defer, <<"Requires manual approval">>};
        true ->
            {promote, <<"All policy criteria met">>}
    end.

%% @private Check if cool-down period has elapsed
-spec check_cool_down(net_id(), pos_integer(), #state{}) -> boolean().
check_cool_down(NetId, CoolDownMs, State) ->
    case maps:find(NetId, State#state.cool_downs) of
        {ok, LastPromotion} ->
            Now = erlang:system_time(millisecond),
            (Now - LastPromotion) >= CoolDownMs;
        error ->
            true
    end.

%% @private Check if patch can be promoted
-spec can_promote(#swf_patch{}, #state{}) -> {ok, #state{}} | {error, term()}.
can_promote(Patch, State) ->
    case Patch#swf_patch.status of
        evaluating ->
            %% Check for pending approval requirements
            case find_approval_request(Patch#swf_patch.id, State) of
                {ok, Request} when Request#approval_request.status =:= approved ->
                    {ok, State};
                {ok, Request} when Request#approval_request.status =:= pending ->
                    {error, pending_approval};
                {ok, Request} when Request#approval_request.status =:= rejected ->
                    {error, rejected};
                error ->
                    %% No approval required
                    {ok, State}
            end;
        proposed ->
            {error, not_evaluated};
        promoted ->
            {error, already_promoted};
        rejected ->
            {error, rejected};
        rolled_back ->
            {error, rolled_back}
    end.

%% @private Create a promotion decision record
-spec create_decision(patch_id(), policy_id(), promote | reject | defer,
                     binary(), automatic | approver_id()) -> #swf_promotion_decision{}.
create_decision(PatchId, PolicyId, Decision, Reason, DecidedBy) ->
    #swf_promotion_decision{
        id = generate_id(),
        patch_id = PatchId,
        policy_id = PolicyId,
        decision = Decision,
        reason = Reason,
        fitness_before = 0.0,
        fitness_after = undefined,
        decided_at = erlang:system_time(millisecond),
        decided_by = DecidedBy
    }.

%% @private Store a decision in state
-spec store_decision(#swf_promotion_decision{}, #state{}) -> #state{}.
store_decision(Decision, State) ->
    DecisionId = Decision#swf_promotion_decision.id,
    PatchId = Decision#swf_promotion_decision.patch_id,

    NewDecisions = maps:put(DecisionId, Decision, State#state.decisions),

    ExistingIds = maps:get(PatchId, State#state.decisions_by_patch, []),
    NewDecisionsByPatch = maps:put(PatchId, [DecisionId | ExistingIds],
                                   State#state.decisions_by_patch),

    State#state{
        decisions = NewDecisions,
        decisions_by_patch = NewDecisionsByPatch
    }.

%% @private Find approval request for a patch
-spec find_approval_request(patch_id(), #state{}) -> {ok, #approval_request{}} | error.
find_approval_request(PatchId, State) ->
    Result = maps:fold(
        fun(_Id, Request, Acc) ->
            case Request#approval_request.patch_id =:= PatchId of
                true -> {ok, Request};
                false -> Acc
            end
        end,
        error,
        State#state.approval_requests),
    Result.

%% @private Count approvals in an approval map
-spec count_approvals(#{approver_id() => approve | reject}) -> non_neg_integer().
count_approvals(Approvals) ->
    maps:fold(
        fun(_Id, approve, Count) -> Count + 1;
           (_Id, reject, Count) -> Count
        end,
        0,
        Approvals).

%% @private Start fitness monitoring for a promoted patch
-spec start_fitness_monitoring(patch_id(), #state{}) -> #state{}.
start_fitness_monitoring(PatchId, State) ->
    %% Check fitness every 60 seconds
    TimerRef = erlang:send_after(60000, self(), {fitness_check, PatchId}),
    NewMonitors = maps:put(PatchId, TimerRef, State#state.fitness_monitors),
    State#state{fitness_monitors = NewMonitors}.

%% @private Stop fitness monitoring for a patch
-spec stop_fitness_monitoring(patch_id(), #state{}) -> #state{}.
stop_fitness_monitoring(PatchId, State) ->
    case maps:find(PatchId, State#state.fitness_monitors) of
        {ok, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            NewMonitors = maps:remove(PatchId, State#state.fitness_monitors),
            State#state{fitness_monitors = NewMonitors};
        error ->
            State
    end.

%% @private Check for fitness degradation
-spec check_fitness_degradation(#swf_patch{}, #state{}) -> ok | {degraded, float()}.
check_fitness_degradation(Patch, _State) ->
    NetId = Patch#swf_patch.net_id,
    CurrentFitness = get_current_fitness(NetId),

    %% Use a default rollback threshold of 0.1 (10% degradation)
    RollbackThreshold = 0.1,

    %% Compare to expected fitness (base + improvement)
    BaseFitness = 0.8,  % Would come from stored baseline
    ExpectedFitness = BaseFitness + Patch#swf_patch.expected_improvement,

    if
        CurrentFitness < ExpectedFitness - RollbackThreshold ->
            {degraded, CurrentFitness};
        true ->
            ok
    end.

%% @private Get current fitness for a net (placeholder)
-spec get_current_fitness(net_id()) -> float().
get_current_fitness(_NetId) ->
    %% Would normally query swf_conformance or metrics system
    0.85.
