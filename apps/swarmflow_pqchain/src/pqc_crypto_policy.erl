%%% @doc Post-Quantum Crypto Policy Engine
%%%
%%% Implements crypto-agility for PQChain - the KEY DIFFERENTIATOR.
%%% Allows cryptographic algorithm changes without hard forks through
%%% staged rollouts with canary testing and automatic rollback.
%%%
%%% Policy Lifecycle:
%%% - draft: Created but not active
%%% - canary: Testing on small cohort
%%% - active: Currently enforced
%%% - deprecated: Being phased out
%%% - sunset: No longer accepted
%%%
%%% Transition Phases:
%%% - announced: Policy change announced
%%% - canary: Testing on canary_percentage of addresses
%%% - ramping: Gradually increasing percentage
%%% - active: Fully deployed
%%% - completed: Transition finished
%%%
%%% This enables "crypto changes like workflow updates" - the operational superpower.
%%%
%%% @end
-module(pqc_crypto_policy).
-behaviour(gen_server).

-include("pqchain.hrl").

%% API
-export([
    start_link/0,
    get_active_policy/0,
    get_policy/1,
    create_policy/1,
    propose_transition/2,
    check_signature/2,
    check_kem/2,
    is_algorithm_allowed/2,
    get_canary_cohort/1,
    in_canary/2,
    record_metrics/2,
    evaluate_rollback/1,
    advance_transition/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%% ============================================================================
%%% Type Specifications
%%% ============================================================================

-type policy_id() :: binary().
-type policy_status() :: draft | canary | active | deprecated | sunset.
-type transition_phase() :: announced | canary | ramping | active | completed.

-type check_result() :: {ok, valid} |
                        {ok, {valid_with_warning, binary()}} |
                        {error, {policy_violation, term()}}.

-record(state, {
    policies_table :: ets:tid(),
    transitions_table :: ets:tid(),
    metrics_table :: ets:tid(),
    active_policy_id :: policy_id() | undefined,
    active_transition_id :: binary() | undefined
}).

-define(SERVER, ?MODULE).
-define(POLICIES_TABLE, pqc_crypto_policies).
-define(TRANSITIONS_TABLE, pqc_crypto_transitions).
-define(METRICS_TABLE, pqc_crypto_metrics).

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Start the crypto policy engine.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get the currently active crypto policy.
-spec get_active_policy() -> {ok, #crypto_policy{}} | {error, no_active_policy}.
get_active_policy() ->
    gen_server:call(?SERVER, get_active_policy, 5000).

%% @doc Get a specific policy by ID.
-spec get_policy(policy_id()) -> {ok, #crypto_policy{}} | {error, not_found}.
get_policy(PolicyId) ->
    gen_server:call(?SERVER, {get_policy, PolicyId}, 5000).

%% @doc Create a new policy in draft state.
-spec create_policy(#crypto_policy{}) -> {ok, policy_id()} | {error, term()}.
create_policy(Policy) ->
    gen_server:call(?SERVER, {create_policy, Policy}, 5000).

%% @doc Propose a transition from current policy to new policy.
-spec propose_transition(policy_id(), #policy_transition{}) ->
    {ok, binary()} | {error, term()}.
propose_transition(NewPolicyId, Transition) ->
    gen_server:call(?SERVER, {propose_transition, NewPolicyId, Transition}, 5000).

%% @doc Check if a signature meets the current policy requirements.
-spec check_signature(#pqc_signature{} | #hybrid_signature{}, binary()) ->
    check_result().
check_signature(Signature, Address) ->
    gen_server:call(?SERVER, {check_signature, Signature, Address}, 5000).

%% @doc Check if a KEM algorithm meets the current policy requirements.
-spec check_kem(pqc_kem_algorithm(), binary()) -> check_result().
check_kem(KemAlgorithm, Address) ->
    gen_server:call(?SERVER, {check_kem, KemAlgorithm, Address}, 5000).

%% @doc Check if an algorithm is allowed under current policy.
-spec is_algorithm_allowed(pqc_sig_algorithm() | pqc_kem_algorithm(), binary()) ->
    boolean().
is_algorithm_allowed(Algorithm, Address) ->
    gen_server:call(?SERVER, {is_algorithm_allowed, Algorithm, Address}, 5000).

%% @doc Get the list of addresses in the canary cohort for a policy.
-spec get_canary_cohort(policy_id()) -> {ok, [binary()]} | {error, term()}.
get_canary_cohort(PolicyId) ->
    gen_server:call(?SERVER, {get_canary_cohort, PolicyId}, 5000).

%% @doc Check if an address is in the canary group for a policy.
-spec in_canary(binary(), policy_id()) -> boolean().
in_canary(Address, PolicyId) ->
    gen_server:call(?SERVER, {in_canary, Address, PolicyId}, 5000).

%% @doc Record verification metrics for a policy.
-spec record_metrics(policy_id(), #{atom() => term()}) -> ok.
record_metrics(PolicyId, Metrics) ->
    gen_server:cast(?SERVER, {record_metrics, PolicyId, Metrics}).

%% @doc Evaluate if a rollback should be triggered for a transition.
-spec evaluate_rollback(binary()) -> {ok, boolean()} | {error, term()}.
evaluate_rollback(TransitionId) ->
    gen_server:call(?SERVER, {evaluate_rollback, TransitionId}, 5000).

%% @doc Advance a transition to the next phase.
-spec advance_transition(binary()) -> {ok, transition_phase()} | {error, term()}.
advance_transition(TransitionId) ->
    gen_server:call(?SERVER, {advance_transition, TransitionId}, 5000).

%%% ============================================================================
%%% gen_server Callbacks
%%% ============================================================================

%% @private
init([]) ->
    process_flag(trap_exit, true),

    %% Register with gproc
    try
        gproc:reg({n, l, {?MODULE, server}})
    catch
        error:badarg ->
            %% Already registered (restart case)
            ok
    end,

    %% Create ETS tables
    PoliciesTable = ets:new(?POLICIES_TABLE, [
        set,
        named_table,
        public,
        {read_concurrency, true},
        {keypos, #crypto_policy.id}
    ]),

    TransitionsTable = ets:new(?TRANSITIONS_TABLE, [
        set,
        named_table,
        public,
        {read_concurrency, true},
        {keypos, #policy_transition.id}
    ]),

    MetricsTable = ets:new(?METRICS_TABLE, [
        set,
        named_table,
        public,
        {write_concurrency, true}
    ]),

    %% Create default policy (for genesis)
    DefaultPolicy = create_default_policy(),
    ets:insert(PoliciesTable, DefaultPolicy),

    State = #state{
        policies_table = PoliciesTable,
        transitions_table = TransitionsTable,
        metrics_table = MetricsTable,
        active_policy_id = DefaultPolicy#crypto_policy.id,
        active_transition_id = undefined
    },

    {ok, State}.

%% @private
handle_call(get_active_policy, _From, #state{active_policy_id = undefined} = State) ->
    {reply, {error, no_active_policy}, State};

handle_call(get_active_policy, _From, #state{active_policy_id = PolicyId,
                                               policies_table = Table} = State) ->
    case ets:lookup(Table, PolicyId) of
        [Policy] ->
            {reply, {ok, Policy}, State};
        [] ->
            {reply, {error, no_active_policy}, State}
    end;

handle_call({get_policy, PolicyId}, _From, #state{policies_table = Table} = State) ->
    case ets:lookup(Table, PolicyId) of
        [Policy] ->
            {reply, {ok, Policy}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_policy, Policy}, _From, #state{policies_table = Table} = State) ->
    %% Generate ID if not provided
    PolicyId = case Policy#crypto_policy.id of
        undefined -> generate_policy_id();
        Id -> Id
    end,

    %% Validate policy
    case validate_policy(Policy#crypto_policy{id = PolicyId}) of
        ok ->
            %% Ensure status is draft
            DraftPolicy = Policy#crypto_policy{
                id = PolicyId,
                status = draft,
                created_at = erlang:system_time(millisecond)
            },
            ets:insert(Table, DraftPolicy),
            {reply, {ok, PolicyId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({propose_transition, NewPolicyId, Transition}, _From,
            #state{active_policy_id = FromPolicyId,
                   policies_table = PTable,
                   transitions_table = TTable} = State) ->
    %% Verify new policy exists
    case ets:lookup(PTable, NewPolicyId) of
        [NewPolicy] when NewPolicy#crypto_policy.status =:= draft ->
            %% Generate transition ID
            TransitionId = generate_transition_id(),

            %% Create transition record
            FullTransition = Transition#policy_transition{
                id = TransitionId,
                from_policy = FromPolicyId,
                to_policy = NewPolicyId,
                phase = announced,
                started_at = erlang:system_time(millisecond),
                metrics = #policy_metrics{
                    transactions_processed = 0,
                    verification_failures = 0,
                    avg_verification_time_us = 0.0,
                    p99_verification_time_us = 0.0,
                    rollback_count = 0
                }
            },

            %% Store transition
            ets:insert(TTable, FullTransition),

            %% Update new policy to canary status
            UpdatedPolicy = NewPolicy#crypto_policy{status = canary},
            ets:insert(PTable, UpdatedPolicy),

            NewState = State#state{active_transition_id = TransitionId},
            {reply, {ok, TransitionId}, NewState};
        [_Policy] ->
            {reply, {error, policy_not_draft}, State};
        [] ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call({check_signature, Signature, Address}, _From, State) ->
    Result = do_check_signature(Signature, Address, State),
    {reply, Result, State};

handle_call({check_kem, KemAlgorithm, Address}, _From, State) ->
    Result = do_check_kem(KemAlgorithm, Address, State),
    {reply, Result, State};

handle_call({is_algorithm_allowed, Algorithm, Address}, _From, State) ->
    Result = do_is_algorithm_allowed(Algorithm, Address, State),
    {reply, Result, State};

handle_call({get_canary_cohort, PolicyId}, _From,
            #state{policies_table = Table} = State) ->
    case ets:lookup(Table, PolicyId) of
        [#crypto_policy{canary_cohort = Cohort}] ->
            {reply, {ok, Cohort}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({in_canary, Address, PolicyId}, _From,
            #state{policies_table = PTable} = State) ->
    Result = case ets:lookup(PTable, PolicyId) of
        [Policy] ->
            is_in_canary_cohort(Address, Policy);
        [] ->
            false
    end,
    {reply, Result, State};

handle_call({evaluate_rollback, TransitionId}, _From,
            #state{transitions_table = TTable,
                   metrics_table = MTable} = State) ->
    Result = do_evaluate_rollback(TransitionId, TTable, MTable),
    {reply, Result, State};

handle_call({advance_transition, TransitionId}, _From,
            #state{transitions_table = TTable,
                   policies_table = PTable,
                   active_policy_id = ActiveId} = State) ->
    case ets:lookup(TTable, TransitionId) of
        [Transition] ->
            case advance_transition_phase(Transition, PTable) of
                {ok, NewPhase, UpdatedTransition} ->
                    ets:insert(TTable, UpdatedTransition),

                    %% If transition completed, update active policy
                    NewState = case NewPhase of
                        completed ->
                            ToPolicyId = Transition#policy_transition.to_policy,
                            %% Update old policy to deprecated
                            case ets:lookup(PTable, ActiveId) of
                                [OldPolicy] ->
                                    ets:insert(PTable, OldPolicy#crypto_policy{
                                        status = deprecated
                                    });
                                [] ->
                                    ok
                            end,
                            %% Update new policy to active
                            case ets:lookup(PTable, ToPolicyId) of
                                [NewPolicy] ->
                                    ets:insert(PTable, NewPolicy#crypto_policy{
                                        status = active
                                    });
                                [] ->
                                    ok
                            end,
                            State#state{
                                active_policy_id = ToPolicyId,
                                active_transition_id = undefined
                            };
                        _ ->
                            State
                    end,
                    {reply, {ok, NewPhase}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({record_metrics, PolicyId, Metrics}, #state{metrics_table = Table} = State) ->
    %% Get current metrics
    CurrentMetrics = case ets:lookup(Table, PolicyId) of
        [{PolicyId, M}] -> M;
        [] -> #policy_metrics{
            transactions_processed = 0,
            verification_failures = 0,
            avg_verification_time_us = 0.0,
            p99_verification_time_us = 0.0,
            rollback_count = 0
        }
    end,

    %% Update metrics
    UpdatedMetrics = update_metrics(CurrentMetrics, Metrics),
    ets:insert(Table, {PolicyId, UpdatedMetrics}),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Internal Functions - Policy Validation
%%% ============================================================================

%% @private
validate_policy(#crypto_policy{
    sig_algorithms = SigAlgs,
    kem_algorithms = KemAlgs,
    hash_algorithms = HashAlgs
}) ->
    %% Validate signature algorithms
    case lists:all(fun pqc_crypto:is_signature_algorithm/1, SigAlgs) of
        false ->
            {error, invalid_signature_algorithms};
        true ->
            %% Validate KEM algorithms
            case lists:all(fun pqc_crypto:is_kem_algorithm/1, KemAlgs) of
                false ->
                    {error, invalid_kem_algorithms};
                true ->
                    %% Validate hash algorithms
                    case lists:all(fun pqc_crypto:is_hash_algorithm/1, HashAlgs) of
                        false ->
                            {error, invalid_hash_algorithms};
                        true ->
                            ok
                    end
            end
    end.

%% @private
create_default_policy() ->
    #crypto_policy{
        id = <<"default-genesis-policy">>,
        name = <<"Genesis Policy">>,
        version = 1,
        status = active,

        %% Signature requirements - accept ML-DSA and hybrid
        sig_algorithms = [
            ?PQC_SIG_ML_DSA_65,  % Recommended default
            ?PQC_SIG_ML_DSA_44,
            ?PQC_SIG_ML_DSA_87,
            ?CLASSIC_SIG_ED25519  % For hybrid support
        ],
        sig_required = hybrid,  % Accept hybrid during transition
        sig_min_security_level = 2,

        %% KEM requirements
        kem_algorithms = [
            ?PQC_KEM_ML_KEM_768,  % Recommended default
            ?PQC_KEM_ML_KEM_512,
            ?PQC_KEM_ML_KEM_1024,
            ?CLASSIC_KEM_X25519   % For hybrid support
        ],
        kem_required = hybrid,
        kem_min_security_level = 3,

        %% Hash requirements
        hash_algorithms = [
            ?HASH_SHA3_256,
            ?HASH_SHA3_512,
            ?HASH_BLAKE3
        ],
        hash_min_bits = 256,

        %% No canary rollout for genesis
        canary_percentage = 0.0,
        canary_cohort = [],
        effective_height = 0,
        sunset_height = undefined,

        %% Metadata
        rationale = <<"Initial post-quantum cryptography policy with hybrid signatures">>,
        approved_by = [],
        created_at = erlang:system_time(millisecond)
    }.

%%% ============================================================================
%%% Internal Functions - Policy Checking
%%% ============================================================================

%% @private
do_check_signature(#pqc_signature{algorithm = Algorithm}, Address, State) ->
    case get_applicable_policy(Address, State) of
        {ok, Policy} ->
            check_signature_against_policy(Algorithm, Policy);
        {error, Reason} ->
            {error, Reason}
    end;

do_check_signature(#hybrid_signature{
    classical = #pqc_signature{algorithm = ClassicAlg},
    pqc = #pqc_signature{algorithm = PQCAlg}
}, Address, State) ->
    case get_applicable_policy(Address, State) of
        {ok, Policy} ->
            %% Check both components
            case check_signature_against_policy(ClassicAlg, Policy) of
                {ok, valid} ->
                    check_signature_against_policy(PQCAlg, Policy);
                Other ->
                    Other
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
check_signature_against_policy(Algorithm, #crypto_policy{
    sig_algorithms = AllowedAlgs,
    sig_required = Required,
    sig_min_security_level = MinLevel
}) ->
    %% Check if algorithm is in allowed list
    case lists:member(Algorithm, AllowedAlgs) of
        true ->
            %% Check security level
            case pqc_crypto:security_level(Algorithm) of
                {ok, Level} when Level >= MinLevel ->
                    %% Check type requirement
                    IsPQC = is_pqc_algorithm(Algorithm),
                    IsClassical = is_classical_algorithm(Algorithm),

                    case Required of
                        pqc_only when IsPQC ->
                            {ok, valid};
                        pqc_only ->
                            {error, {policy_violation, pqc_required}};
                        classical_allowed ->
                            {ok, valid};
                        hybrid when IsClassical ->
                            {ok, {valid_with_warning,
                                  <<"Classical signature detected, hybrid recommended">>}};
                        hybrid ->
                            {ok, valid};
                        _ ->
                            {error, {policy_violation, type_mismatch}}
                    end;
                {ok, _Level} ->
                    {error, {policy_violation, insufficient_security_level}};
                {error, _} ->
                    {error, {policy_violation, unknown_algorithm}}
            end;
        false ->
            {error, {policy_violation, algorithm_not_allowed}}
    end.

%% @private
do_check_kem(KemAlgorithm, Address, State) ->
    case get_applicable_policy(Address, State) of
        {ok, Policy} ->
            check_kem_against_policy(KemAlgorithm, Policy);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
check_kem_against_policy(Algorithm, #crypto_policy{
    kem_algorithms = AllowedAlgs,
    kem_required = Required,
    kem_min_security_level = MinLevel
}) ->
    case lists:member(Algorithm, AllowedAlgs) of
        true ->
            case pqc_crypto:security_level(Algorithm) of
                {ok, Level} when Level >= MinLevel ->
                    IsPQC = is_pqc_algorithm(Algorithm),
                    IsClassical = is_classical_algorithm(Algorithm),

                    case Required of
                        pqc_only when IsPQC ->
                            {ok, valid};
                        pqc_only ->
                            {error, {policy_violation, pqc_required}};
                        classical_allowed ->
                            {ok, valid};
                        hybrid when IsClassical ->
                            {ok, {valid_with_warning,
                                  <<"Classical KEM detected, hybrid recommended">>}};
                        hybrid ->
                            {ok, valid};
                        _ ->
                            {error, {policy_violation, type_mismatch}}
                    end;
                {ok, _Level} ->
                    {error, {policy_violation, insufficient_security_level}};
                {error, _} ->
                    {error, {policy_violation, unknown_algorithm}}
            end;
        false ->
            {error, {policy_violation, algorithm_not_allowed}}
    end.

%% @private
do_is_algorithm_allowed(Algorithm, Address, State) ->
    case get_applicable_policy(Address, State) of
        {ok, Policy} ->
            is_algorithm_in_policy(Algorithm, Policy);
        {error, _} ->
            false
    end.

%% @private
is_algorithm_in_policy(Algorithm, #crypto_policy{
    sig_algorithms = SigAlgs,
    kem_algorithms = KemAlgs
}) ->
    lists:member(Algorithm, SigAlgs) orelse lists:member(Algorithm, KemAlgs).

%%% ============================================================================
%%% Internal Functions - Policy Selection
%%% ============================================================================

%% @private
%% Get the applicable policy for an address (considering transitions)
get_applicable_policy(Address, #state{
    active_policy_id = ActiveId,
    active_transition_id = TransitionId,
    policies_table = PTable,
    transitions_table = TTable
}) ->
    case TransitionId of
        undefined ->
            %% No active transition, use active policy
            case ets:lookup(PTable, ActiveId) of
                [Policy] -> {ok, Policy};
                [] -> {error, no_active_policy}
            end;
        _ ->
            %% Active transition, check if address is in canary/ramp
            case ets:lookup(TTable, TransitionId) of
                [#policy_transition{
                    to_policy = ToPolicyId,
                    phase = Phase,
                    ramp_percentage = RampPct
                }] ->
                    case should_use_new_policy(Address, Phase, RampPct, ToPolicyId, PTable) of
                        true ->
                            case ets:lookup(PTable, ToPolicyId) of
                                [Policy] -> {ok, Policy};
                                [] -> {error, policy_not_found}
                            end;
                        false ->
                            case ets:lookup(PTable, ActiveId) of
                                [Policy] -> {ok, Policy};
                                [] -> {error, no_active_policy}
                            end
                    end;
                [] ->
                    %% Transition not found, use active
                    case ets:lookup(PTable, ActiveId) of
                        [Policy] -> {ok, Policy};
                        [] -> {error, no_active_policy}
                    end
            end
    end.

%% @private
should_use_new_policy(_Address, announced, _RampPct, _PolicyId, _Table) ->
    false;  % Announced but not rolled out yet

should_use_new_policy(Address, canary, _RampPct, PolicyId, Table) ->
    %% Check if address is in canary cohort
    case ets:lookup(Table, PolicyId) of
        [Policy] -> is_in_canary_cohort(Address, Policy);
        [] -> false
    end;

should_use_new_policy(Address, ramping, RampPct, _PolicyId, _Table) ->
    %% Use consistent hashing for ramp percentage
    is_in_ramp_percentage(Address, RampPct);

should_use_new_policy(_Address, active, _RampPct, _PolicyId, _Table) ->
    true;  % Fully rolled out

should_use_new_policy(_Address, completed, _RampPct, _PolicyId, _Table) ->
    true.

%%% ============================================================================
%%% Internal Functions - Canary Cohort
%%% ============================================================================

%% @private
%% Check if an address is in the canary cohort
is_in_canary_cohort(Address, #crypto_policy{
    canary_cohort = Cohort,
    canary_percentage = Percentage
}) when length(Cohort) > 0 ->
    %% Explicit cohort
    lists:member(Address, Cohort);

is_in_canary_cohort(Address, #crypto_policy{
    canary_percentage = Percentage,
    id = PolicyId
}) when Percentage > 0.0 ->
    %% Hash-based selection for deterministic canary
    is_in_percentage_cohort(Address, PolicyId, Percentage);

is_in_canary_cohort(_Address, _Policy) ->
    false.

%% @private
%% Use consistent hashing to determine if address is in percentage cohort
is_in_percentage_cohort(Address, PolicyId, Percentage) ->
    %% Hash address + policy ID for determinism
    Hash = crypto:hash(sha256, <<Address/binary, PolicyId/binary>>),
    <<HashInt:256>> = Hash,
    %% Map to 0.0-1.0 range
    Ratio = HashInt / (1 bsl 256),
    Ratio < Percentage.

%% @private
%% Check if address is in ramp percentage (for ramping phase)
is_in_ramp_percentage(Address, RampPct) ->
    Hash = crypto:hash(sha256, Address),
    <<HashInt:256>> = Hash,
    Ratio = HashInt / (1 bsl 256),
    Ratio < RampPct.

%%% ============================================================================
%%% Internal Functions - Metrics and Rollback
%%% ============================================================================

%% @private
update_metrics(#policy_metrics{
    transactions_processed = TxCount,
    verification_failures = Failures,
    avg_verification_time_us = AvgTime,
    rollback_count = RollbackCount
} = Current, NewData) ->
    %% Extract new metrics
    NewTxCount = maps:get(transactions_processed, NewData, 0),
    NewFailures = maps:get(verification_failures, NewData, 0),
    NewTime = maps:get(verification_time_us, NewData, 0.0),
    NewP99 = maps:get(p99_verification_time_us, NewData, 0.0),

    %% Update running averages
    TotalTx = TxCount + NewTxCount,
    UpdatedAvg = case TotalTx of
        0 -> 0.0;
        _ -> ((AvgTime * TxCount) + (NewTime * NewTxCount)) / TotalTx
    end,

    Current#policy_metrics{
        transactions_processed = TotalTx,
        verification_failures = Failures + NewFailures,
        avg_verification_time_us = UpdatedAvg,
        p99_verification_time_us = max(Current#policy_metrics.p99_verification_time_us, NewP99),
        rollback_count = RollbackCount
    }.

%% @private
do_evaluate_rollback(TransitionId, TransitionsTable, MetricsTable) ->
    case ets:lookup(TransitionsTable, TransitionId) of
        [#policy_transition{
            to_policy = PolicyId,
            rollback_trigger = Trigger,
            metrics = Metrics
        }] ->
            %% Get latest metrics
            LatestMetrics = case ets:lookup(MetricsTable, PolicyId) of
                [{PolicyId, M}] -> M;
                [] -> Metrics
            end,

            %% Check rollback conditions
            ShouldRollback = check_rollback_conditions(LatestMetrics, Trigger),
            {ok, ShouldRollback};
        [] ->
            {error, not_found}
    end.

%% @private
check_rollback_conditions(#policy_metrics{
    transactions_processed = TxCount,
    verification_failures = Failures,
    avg_verification_time_us = AvgTime
}, #rollback_trigger{
    failure_rate_threshold = FailureThreshold,
    latency_threshold_ms = LatencyThreshold,
    error_count_threshold = ErrorThreshold
}) ->
    %% Calculate failure rate
    FailureRate = case TxCount of
        0 -> 0.0;
        _ -> Failures / TxCount
    end,

    %% Convert latency threshold to microseconds
    LatencyThresholdUs = LatencyThreshold * 1000,

    %% Check conditions
    (FailureRate > FailureThreshold) orelse
    (AvgTime > LatencyThresholdUs) orelse
    (Failures > ErrorThreshold).

%%% ============================================================================
%%% Internal Functions - Transition Management
%%% ============================================================================

%% @private
advance_transition_phase(#policy_transition{phase = announced} = Transition, _Table) ->
    %% Move to canary phase
    UpdatedTransition = Transition#policy_transition{phase = canary},
    {ok, canary, UpdatedTransition};

advance_transition_phase(#policy_transition{phase = canary} = Transition, _Table) ->
    %% Move to ramping phase
    UpdatedTransition = Transition#policy_transition{
        phase = ramping,
        ramp_percentage = 0.1  % Start at 10%
    },
    {ok, ramping, UpdatedTransition};

advance_transition_phase(#policy_transition{
    phase = ramping,
    ramp_percentage = RampPct,
    ramp_increment = Increment
} = Transition, _Table) ->
    %% Increase ramp percentage
    NewPct = min(1.0, RampPct + Increment),
    case NewPct >= 1.0 of
        true ->
            %% Fully ramped, move to active
            UpdatedTransition = Transition#policy_transition{
                phase = active,
                ramp_percentage = 1.0
            },
            {ok, active, UpdatedTransition};
        false ->
            %% Continue ramping
            UpdatedTransition = Transition#policy_transition{
                ramp_percentage = NewPct
            },
            {ok, ramping, UpdatedTransition}
    end;

advance_transition_phase(#policy_transition{phase = active} = Transition, _Table) ->
    %% Move to completed
    UpdatedTransition = Transition#policy_transition{phase = completed},
    {ok, completed, UpdatedTransition};

advance_transition_phase(#policy_transition{phase = completed}, _Table) ->
    {error, already_completed}.

%%% ============================================================================
%%% Internal Functions - Utilities
%%% ============================================================================

%% @private
generate_policy_id() ->
    %% Generate unique ID using timestamp + random bytes
    Timestamp = erlang:system_time(microsecond),
    Random = crypto:strong_rand_bytes(8),
    iolist_to_binary(io_lib:format("policy-~b-~s", [
        Timestamp,
        binary:encode_hex(Random)
    ])).

%% @private
generate_transition_id() ->
    %% Generate unique ID using timestamp + random bytes
    Timestamp = erlang:system_time(microsecond),
    Random = crypto:strong_rand_bytes(8),
    iolist_to_binary(io_lib:format("transition-~b-~s", [
        Timestamp,
        binary:encode_hex(Random)
    ])).

%% @private
is_pqc_algorithm(?PQC_SIG_ML_DSA_44) -> true;
is_pqc_algorithm(?PQC_SIG_ML_DSA_65) -> true;
is_pqc_algorithm(?PQC_SIG_ML_DSA_87) -> true;
is_pqc_algorithm(?PQC_SIG_SLH_DSA_128S) -> true;
is_pqc_algorithm(?PQC_SIG_SLH_DSA_128F) -> true;
is_pqc_algorithm(?PQC_SIG_SLH_DSA_192S) -> true;
is_pqc_algorithm(?PQC_SIG_SLH_DSA_192F) -> true;
is_pqc_algorithm(?PQC_SIG_SLH_DSA_256S) -> true;
is_pqc_algorithm(?PQC_SIG_SLH_DSA_256F) -> true;
is_pqc_algorithm(?PQC_KEM_ML_KEM_512) -> true;
is_pqc_algorithm(?PQC_KEM_ML_KEM_768) -> true;
is_pqc_algorithm(?PQC_KEM_ML_KEM_1024) -> true;
is_pqc_algorithm(_) -> false.

%% @private
is_classical_algorithm(?CLASSIC_SIG_ED25519) -> true;
is_classical_algorithm(?CLASSIC_SIG_SECP256K1) -> true;
is_classical_algorithm(?CLASSIC_KEM_X25519) -> true;
is_classical_algorithm(_) -> false.
