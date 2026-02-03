%%%====================================================================
%%% @doc SwarmFlow Patch Proposer - Swarm Worker for Workflow Improvements
%%%
%%% A stateful gen_server worker that proposes workflow improvements based
%%% on event logs and conformance analysis. Part of the process-mining swarm
%%% that enables autonomic workflow optimization.
%%%
%%% == Patch Proposal Strategies ==
%%% - Deviation-based: Fix conformance issues (missing tokens, wrong transitions)
%%% - Performance-based: Optimize bottlenecks (slow transitions, resource contention)
%%% - Structural: Simplification (remove redundant paths) and parallelization
%%%
%%% == Scoring System ==
%%% - Confidence: How certain we are the patch will help (0.0 - 1.0)
%%% - Risk: Potential for negative impact (0.0 - 1.0)
%%% - Expected improvement: Estimated fitness gain (0.0 - 1.0)
%%%
%%% == Architecture ==
%%% - Maintains pattern cache for efficient repeated analysis
%%% - Coordinates with swf_conformance for deviation detection
%%% - Submits patches to swf_swarm_coordinator for evaluation
%%% - Learns from promotion/rejection outcomes to improve proposals
%%%
%%% @end
%%%====================================================================
-module(swf_patch_proposer).

-behaviour(gen_server).

-include("swarmflow.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    start_link/1,
    propose_patches/2,
    propose_patches/3,
    analyze_deviations/2,
    analyze_performance/2,
    mine_patterns/2,
    score_proposal/1,
    assess_risk/1,
    get_stats/0,
    get_patterns/0,
    clear_patterns/0
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

%%====================================================================
%% Types
%%====================================================================

-type net_id() :: binary().
-type pattern() :: #{
    type := atom(),
    frequency := non_neg_integer(),
    confidence := float(),
    elements := [binary()],
    metadata := map()
}.

-type performance_issue() :: #{
    type := bottleneck | slow_transition | resource_contention | deadlock_risk,
    location := binary(),
    severity := low | medium | high | critical,
    avg_duration_ms := float(),
    p95_duration_ms := float(),
    frequency := non_neg_integer()
}.

-type proposal_context() :: #{
    net := #swf_net{},
    events := [#swf_event{}],
    deviations => [#swf_deviation{}],
    performance => [performance_issue()],
    patterns => [pattern()]
}.

-export_type([pattern/0, performance_issue/0, proposal_context/0]).

%%====================================================================
%% State record
%%====================================================================

-record(state, {
    %% Worker identity
    worker_id :: binary(),

    %% Coordinator reference for submitting patches
    coordinator :: pid() | undefined,

    %% Cached patterns: #{net_id() => [pattern()]}
    pattern_cache :: #{net_id() => [pattern()]},

    %% Historical proposals for learning: #{patch_id() => outcome}
    proposal_history :: #{binary() => promoted | rejected},

    %% Configuration
    config :: #{
        min_confidence := float(),
        max_risk := float(),
        min_pattern_frequency := pos_integer(),
        bottleneck_threshold_ms := pos_integer(),
        parallelization_threshold := float(),
        enable_structural_patches := boolean()
    },

    %% Metrics
    stats :: #{
        patches_proposed := non_neg_integer(),
        patches_promoted := non_neg_integer(),
        patches_rejected := non_neg_integer(),
        patterns_mined := non_neg_integer(),
        deviations_analyzed := non_neg_integer(),
        avg_confidence := float(),
        avg_risk := float()
    }
}).

-type state() :: #state{}.

%%====================================================================
%% Macros
%%====================================================================

-define(DEFAULT_MIN_CONFIDENCE, 0.6).
-define(DEFAULT_MAX_RISK, 0.4).
-define(DEFAULT_MIN_PATTERN_FREQUENCY, 3).
-define(DEFAULT_BOTTLENECK_THRESHOLD_MS, 5000).
-define(DEFAULT_PARALLELIZATION_THRESHOLD, 0.7).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the patch proposer with configuration.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Propose patches for a workflow net based on analysis.
%% Combines deviation analysis, performance analysis, and pattern mining.
-spec propose_patches(net_id(), proposal_context()) -> {ok, [#swf_patch{}]}.
propose_patches(NetId, Context) ->
    propose_patches(NetId, Context, #{}).

%% @doc Propose patches with options.
-spec propose_patches(net_id(), proposal_context(), map()) -> {ok, [#swf_patch{}]}.
propose_patches(NetId, Context, Opts) ->
    gen_server:call(?MODULE, {propose_patches, NetId, Context, Opts}, 30000).

%% @doc Analyze deviations and suggest fixes.
-spec analyze_deviations([#swf_deviation{}], #swf_net{}) -> {ok, [#swf_patch{}]}.
analyze_deviations(Deviations, Net) ->
    gen_server:call(?MODULE, {analyze_deviations, Deviations, Net}, 15000).

%% @doc Analyze performance issues and suggest optimizations.
-spec analyze_performance([#swf_event{}], #swf_net{}) -> {ok, [#swf_patch{}]}.
analyze_performance(Events, Net) ->
    gen_server:call(?MODULE, {analyze_performance, Events, Net}, 15000).

%% @doc Mine patterns from event logs.
-spec mine_patterns([#swf_event{}], map()) -> {ok, [pattern()]}.
mine_patterns(Events, Opts) ->
    gen_server:call(?MODULE, {mine_patterns, Events, Opts}, 30000).

%% @doc Calculate confidence score for a proposal.
-spec score_proposal(#swf_patch{}) -> {ok, float()}.
score_proposal(Patch) ->
    gen_server:call(?MODULE, {score_proposal, Patch}, 5000).

%% @doc Calculate risk score for a proposal.
-spec assess_risk(#swf_patch{}) -> {ok, float()}.
assess_risk(Patch) ->
    gen_server:call(?MODULE, {assess_risk, Patch}, 5000).

%% @doc Get proposer statistics.
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?MODULE, get_stats, 5000).

%% @doc Get cached patterns.
-spec get_patterns() -> #{net_id() => [pattern()]}.
get_patterns() ->
    gen_server:call(?MODULE, get_patterns, 5000).

%% @doc Clear pattern cache.
-spec clear_patterns() -> ok.
clear_patterns() ->
    gen_server:call(?MODULE, clear_patterns, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, state()}.
init(Config) ->
    process_flag(trap_exit, true),

    WorkerId = generate_worker_id(),

    %% Extract configuration with defaults
    MinConfidence = maps:get(min_confidence, Config, ?DEFAULT_MIN_CONFIDENCE),
    MaxRisk = maps:get(max_risk, Config, ?DEFAULT_MAX_RISK),
    MinPatternFreq = maps:get(min_pattern_frequency, Config, ?DEFAULT_MIN_PATTERN_FREQUENCY),
    BottleneckThreshold = maps:get(bottleneck_threshold_ms, Config, ?DEFAULT_BOTTLENECK_THRESHOLD_MS),
    ParallelizationThreshold = maps:get(parallelization_threshold, Config, ?DEFAULT_PARALLELIZATION_THRESHOLD),
    EnableStructural = maps:get(enable_structural_patches, Config, true),

    %% Find coordinator if available
    Coordinator = case maps:get(coordinator, Config, undefined) of
        undefined -> erlang:whereis(swf_swarm_coordinator);
        Pid -> Pid
    end,

    State = #state{
        worker_id = WorkerId,
        coordinator = Coordinator,
        pattern_cache = #{},
        proposal_history = #{},
        config = #{
            min_confidence => MinConfidence,
            max_risk => MaxRisk,
            min_pattern_frequency => MinPatternFreq,
            bottleneck_threshold_ms => BottleneckThreshold,
            parallelization_threshold => ParallelizationThreshold,
            enable_structural_patches => EnableStructural
        },
        stats = #{
            patches_proposed => 0,
            patches_promoted => 0,
            patches_rejected => 0,
            patterns_mined => 0,
            deviations_analyzed => 0,
            avg_confidence => 0.0,
            avg_risk => 0.0
        }
    },

    ?LOG_INFO("swf_patch_proposer started: worker_id=~s", [WorkerId]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

%% Propose patches for a net
handle_call({propose_patches, NetId, Context, Opts}, _From, State) ->
    {Patches, NewState} = do_propose_patches(NetId, Context, Opts, State),
    {reply, {ok, Patches}, NewState};

%% Analyze deviations
handle_call({analyze_deviations, Deviations, Net}, _From, State) ->
    {Patches, NewState} = do_analyze_deviations(Deviations, Net, State),
    {reply, {ok, Patches}, NewState};

%% Analyze performance
handle_call({analyze_performance, Events, Net}, _From, State) ->
    {Patches, NewState} = do_analyze_performance(Events, Net, State),
    {reply, {ok, Patches}, NewState};

%% Mine patterns
handle_call({mine_patterns, Events, Opts}, _From, State) ->
    {Patterns, NewState} = do_mine_patterns(Events, Opts, State),
    {reply, {ok, Patterns}, NewState};

%% Score proposal
handle_call({score_proposal, Patch}, _From, State) ->
    Score = do_score_proposal(Patch, State),
    {reply, {ok, Score}, State};

%% Assess risk
handle_call({assess_risk, Patch}, _From, State) ->
    Risk = do_assess_risk(Patch, State),
    {reply, {ok, Risk}, State};

%% Get stats
handle_call(get_stats, _From, State) ->
    {reply, State#state.stats, State};

%% Get patterns
handle_call(get_patterns, _From, State) ->
    {reply, State#state.pattern_cache, State};

%% Clear patterns
handle_call(clear_patterns, _From, State) ->
    {reply, ok, State#state{pattern_cache = #{}}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

%% Patch was promoted
handle_cast({patch_promoted, PatchId}, State) ->
    NewHistory = maps:put(PatchId, promoted, State#state.proposal_history),
    Stats = State#state.stats,
    NewStats = Stats#{
        patches_promoted => maps:get(patches_promoted, Stats, 0) + 1
    },
    {noreply, State#state{proposal_history = NewHistory, stats = NewStats}};

%% Patch was rejected
handle_cast({patch_rejected, PatchId}, State) ->
    NewHistory = maps:put(PatchId, rejected, State#state.proposal_history),
    Stats = State#state.stats,
    NewStats = Stats#{
        patches_rejected => maps:get(patches_rejected, Stats, 0) + 1
    },
    {noreply, State#state{proposal_history = NewHistory, stats = NewStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    ?LOG_INFO("swf_patch_proposer ~s terminating: ~p", [State#state.worker_id, Reason]),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Patch Proposal
%%====================================================================

-spec do_propose_patches(net_id(), proposal_context(), map(), state()) ->
    {[#swf_patch{}], state()}.
do_propose_patches(NetId, Context, Opts, State) ->
    Net = maps:get(net, Context),
    Events = maps:get(events, Context, []),

    %% 1. Analyze deviations if provided or compute them
    Deviations = case maps:get(deviations, Context, undefined) of
        undefined -> compute_deviations(Events, Net);
        D -> D
    end,

    %% 2. Get deviation-based patches
    {DeviationPatches, State1} = do_analyze_deviations(Deviations, Net, State),

    %% 3. Get performance-based patches
    {PerfPatches, State2} = do_analyze_performance(Events, Net, State1),

    %% 4. Get structural improvement patches
    StructPatches = case maps:get(enable_structural_patches, State#state.config) of
        true -> propose_structural_patches(Events, Net, State2);
        false -> []
    end,

    %% 5. Combine all patches
    AllPatches = DeviationPatches ++ PerfPatches ++ StructPatches,

    %% 6. Filter by confidence and risk thresholds
    MinConfidence = maps:get(min_confidence, Opts,
                             maps:get(min_confidence, State#state.config)),
    MaxRisk = maps:get(max_risk, Opts,
                       maps:get(max_risk, State#state.config)),

    FilteredPatches = lists:filter(
        fun(P) ->
            P#swf_patch.confidence >= MinConfidence andalso
            P#swf_patch.risk_score =< MaxRisk
        end,
        AllPatches
    ),

    %% 7. Score and rank patches
    RankedPatches = rank_patches(FilteredPatches),

    %% 8. Apply limit if specified
    LimitedPatches = case maps:get(limit, Opts, undefined) of
        undefined -> RankedPatches;
        Limit -> lists:sublist(RankedPatches, Limit)
    end,

    %% 9. Submit to coordinator if available
    FinalPatches = lists:map(
        fun(Patch) ->
            FinalPatch = Patch#swf_patch{
                proposed_by = State2#state.worker_id,
                proposed_at = erlang:system_time(millisecond),
                status = proposed
            },
            maybe_submit_patch(FinalPatch, State2),
            FinalPatch
        end,
        LimitedPatches
    ),

    %% 10. Update stats
    Stats = State2#state.stats,
    NewStats = Stats#{
        patches_proposed => maps:get(patches_proposed, Stats, 0) + length(FinalPatches)
    },

    %% 11. Update pattern cache
    {Patterns, State3} = case Events of
        [] -> {[], State2};
        _ -> do_mine_patterns(Events, #{}, State2)
    end,
    NewCache = maps:put(NetId, Patterns, State3#state.pattern_cache),

    {FinalPatches, State3#state{stats = NewStats, pattern_cache = NewCache}}.

-spec compute_deviations([#swf_event{}], #swf_net{}) -> [#swf_deviation{}].
compute_deviations(Events, Net) ->
    %% Use swf_conformance if available
    case erlang:whereis(swf_conformance_cache) of
        undefined ->
            %% Fallback: basic deviation detection
            detect_basic_deviations(Events, Net);
        _ ->
            case swf_conformance:find_deviations(Events, Net) of
                {ok, Devs} -> Devs;
                {error, _} -> detect_basic_deviations(Events, Net)
            end
    end.

-spec detect_basic_deviations([#swf_event{}], #swf_net{}) -> [#swf_deviation{}].
detect_basic_deviations(Events, Net) ->
    %% Basic deviation detection without full conformance checking
    TransitionEvents = [E || E <- Events,
                        E#swf_event.event_type =:= transition_fired orelse
                        E#swf_event.event_type =:= transition_completed],

    %% Check for unknown transitions
    ModelTransitions = maps:keys(Net#swf_net.transitions),

    lists:filtermap(
        fun(Event) ->
            TId = Event#swf_event.transition_id,
            case TId of
                undefined -> false;
                _ ->
                    case lists:member(TId, ModelTransitions) of
                        true -> false;
                        false ->
                            {true, #swf_deviation{
                                type = wrong_transition,
                                location = TId,
                                expected = <<"model transition">>,
                                actual = TId,
                                severity = medium,
                                message = <<"Transition not in model: ", TId/binary>>
                            }}
                    end
            end
        end,
        TransitionEvents
    ).

%%====================================================================
%% Internal Functions - Deviation Analysis
%%====================================================================

-spec do_analyze_deviations([#swf_deviation{}], #swf_net{}, state()) ->
    {[#swf_patch{}], state()}.
do_analyze_deviations(Deviations, Net, State) ->
    Patches = lists:filtermap(
        fun(Deviation) ->
            case deviation_to_patch(Deviation, Net, State) of
                {ok, Patch} -> {true, Patch};
                skip -> false
            end
        end,
        Deviations
    ),

    %% Update stats
    Stats = State#state.stats,
    NewStats = Stats#{
        deviations_analyzed => maps:get(deviations_analyzed, Stats, 0) + length(Deviations)
    },

    {Patches, State#state{stats = NewStats}}.

-spec deviation_to_patch(#swf_deviation{}, #swf_net{}, state()) ->
    {ok, #swf_patch{}} | skip.
deviation_to_patch(#swf_deviation{type = missing_token, location = PlaceId} = Dev, Net, State) ->
    %% Missing token: suggest adding an arc or modifying initial marking
    NetId = Net#swf_net.id,
    PatchId = generate_patch_id(NetId, <<"missing_token">>),

    Change = #swf_patch_change{
        operation = modify,
        target_type = place,
        target_id = PlaceId,
        old_value = #{initial_tokens => 0},
        new_value = #{initial_tokens => 1}
    },

    Confidence = calculate_deviation_confidence(Dev, State),
    Risk = calculate_deviation_risk(Dev, Net),

    Patch = #swf_patch{
        id = PatchId,
        net_id = NetId,
        version = Net#swf_net.version,
        kind = add_constraint,
        changes = [Change],
        rationale = iolist_to_binary([
            <<"Fix missing token in place ">>, PlaceId,
            <<". Deviation severity: ">>, atom_to_binary(Dev#swf_deviation.severity, utf8)
        ]),
        confidence = Confidence,
        risk_score = Risk,
        expected_improvement = estimate_improvement(Dev),
        proposed_by = undefined,
        proposed_at = undefined,
        status = proposed
    },

    case Confidence >= maps:get(min_confidence, State#state.config) of
        true -> {ok, Patch};
        false -> skip
    end;

deviation_to_patch(#swf_deviation{type = remaining_token, location = PlaceId} = Dev, Net, State) ->
    %% Remaining token: suggest adding a consuming transition or cleaning arc
    NetId = Net#swf_net.id,
    PatchId = generate_patch_id(NetId, <<"remaining_token">>),

    %% Create a cleanup transition
    CleanupTransitionId = <<PlaceId/binary, "_cleanup">>,

    Change1 = #swf_patch_change{
        operation = add,
        target_type = transition,
        target_id = CleanupTransitionId,
        old_value = undefined,
        new_value = #{name => <<"Cleanup">>, kind => automatic}
    },

    Change2 = #swf_patch_change{
        operation = add,
        target_type = arc,
        target_id = <<PlaceId/binary, "_to_cleanup">>,
        old_value = undefined,
        new_value = #{source => PlaceId, target => CleanupTransitionId, weight => 1}
    },

    Confidence = calculate_deviation_confidence(Dev, State) * 0.8, % Lower confidence for structural changes
    Risk = calculate_deviation_risk(Dev, Net) * 1.2, % Higher risk for adding elements

    Patch = #swf_patch{
        id = PatchId,
        net_id = NetId,
        version = Net#swf_net.version,
        kind = add_transition,
        changes = [Change1, Change2],
        rationale = iolist_to_binary([
            <<"Add cleanup transition for remaining tokens in place ">>, PlaceId
        ]),
        confidence = min(1.0, Confidence),
        risk_score = min(1.0, Risk),
        expected_improvement = estimate_improvement(Dev) * 0.8,
        proposed_by = undefined,
        proposed_at = undefined,
        status = proposed
    },

    {ok, Patch};

deviation_to_patch(#swf_deviation{type = wrong_transition, location = TransitionId} = Dev, Net, State) ->
    %% Wrong transition: suggest adding it to the model or adjusting guards
    NetId = Net#swf_net.id,
    PatchId = generate_patch_id(NetId, <<"wrong_transition">>),

    %% Propose adding the missing transition
    Change = #swf_patch_change{
        operation = add,
        target_type = transition,
        target_id = TransitionId,
        old_value = undefined,
        new_value = #{name => TransitionId, kind => automatic}
    },

    Confidence = calculate_deviation_confidence(Dev, State) * 0.7,
    Risk = calculate_deviation_risk(Dev, Net) * 1.5, % High risk for adding new transitions

    Patch = #swf_patch{
        id = PatchId,
        net_id = NetId,
        version = Net#swf_net.version,
        kind = add_transition,
        changes = [Change],
        rationale = iolist_to_binary([
            <<"Add missing transition ">>, TransitionId,
            <<" observed in event log but not in model">>
        ]),
        confidence = min(1.0, Confidence),
        risk_score = min(1.0, Risk),
        expected_improvement = estimate_improvement(Dev) * 0.7,
        proposed_by = undefined,
        proposed_at = undefined,
        status = proposed
    },

    {ok, Patch};

deviation_to_patch(#swf_deviation{type = deadlock} = Dev, Net, State) ->
    %% Deadlock: suggest adding escape transitions or modifying guards
    NetId = Net#swf_net.id,
    PatchId = generate_patch_id(NetId, <<"deadlock_fix">>),
    Location = Dev#swf_deviation.location,

    %% Add escape transition
    EscapeId = <<Location/binary, "_escape">>,

    Change = #swf_patch_change{
        operation = add,
        target_type = transition,
        target_id = EscapeId,
        old_value = undefined,
        new_value = #{name => <<"Escape">>, kind => automatic,
                     metadata => #{escape_from => Location}}
    },

    Confidence = calculate_deviation_confidence(Dev, State) * 0.6,
    Risk = 0.6, % Moderate-high risk for deadlock fixes

    Patch = #swf_patch{
        id = PatchId,
        net_id = NetId,
        version = Net#swf_net.version,
        kind = add_transition,
        changes = [Change],
        rationale = iolist_to_binary([
            <<"Add escape transition to prevent deadlock at ">>, Location
        ]),
        confidence = Confidence,
        risk_score = Risk,
        expected_improvement = 0.8, % High improvement for deadlock fixes
        proposed_by = undefined,
        proposed_at = undefined,
        status = proposed
    },

    {ok, Patch};

deviation_to_patch(#swf_deviation{type = livelock} = Dev, Net, _State) ->
    %% Livelock: suggest adding counters or timeout guards
    NetId = Net#swf_net.id,
    PatchId = generate_patch_id(NetId, <<"livelock_fix">>),
    Location = Dev#swf_deviation.location,

    %% Add timeout guard to break livelock
    Change = #swf_patch_change{
        operation = modify,
        target_type = guard,
        target_id = Location,
        old_value = undefined,
        new_value = #{timeout_ms => 30000, max_iterations => 100}
    },

    Patch = #swf_patch{
        id = PatchId,
        net_id = NetId,
        version = Net#swf_net.version,
        kind = modify_guard,
        changes = [Change],
        rationale = iolist_to_binary([
            <<"Add timeout/iteration guard to prevent livelock at ">>, Location
        ]),
        confidence = 0.5, % Lower confidence for livelock fixes
        risk_score = 0.5,
        expected_improvement = 0.6,
        proposed_by = undefined,
        proposed_at = undefined,
        status = proposed
    },

    {ok, Patch};

deviation_to_patch(_Deviation, _Net, _State) ->
    skip.

-spec calculate_deviation_confidence(#swf_deviation{}, state()) -> float().
calculate_deviation_confidence(#swf_deviation{severity = Severity}, State) ->
    %% Base confidence from severity
    BaseConfidence = case Severity of
        critical -> 0.9;
        high -> 0.8;
        medium -> 0.7;
        low -> 0.5
    end,

    %% Adjust based on historical success rate
    History = State#state.proposal_history,
    {Promoted, Total} = maps:fold(
        fun(_PatchId, Outcome, {P, T}) ->
            case Outcome of
                promoted -> {P + 1, T + 1};
                rejected -> {P, T + 1}
            end
        end,
        {0, 0},
        History
    ),

    HistoryFactor = case Total of
        0 -> 1.0;
        _ -> 0.5 + (0.5 * Promoted / Total)
    end,

    min(1.0, BaseConfidence * HistoryFactor).

-spec calculate_deviation_risk(#swf_deviation{}, #swf_net{}) -> float().
calculate_deviation_risk(#swf_deviation{type = Type, severity = Severity}, Net) ->
    %% Base risk from deviation type
    TypeRisk = case Type of
        missing_token -> 0.2;
        remaining_token -> 0.3;
        wrong_transition -> 0.5;
        deadlock -> 0.4;
        livelock -> 0.5
    end,

    %% Adjust for net complexity
    Places = maps:size(Net#swf_net.places),
    Transitions = maps:size(Net#swf_net.transitions),
    Complexity = (Places + Transitions) / 100,
    ComplexityFactor = min(1.5, 1.0 + Complexity),

    %% Adjust for severity
    SeverityFactor = case Severity of
        critical -> 0.8;
        high -> 0.9;
        medium -> 1.0;
        low -> 1.2
    end,

    min(1.0, TypeRisk * ComplexityFactor * SeverityFactor).

-spec estimate_improvement(#swf_deviation{}) -> float().
estimate_improvement(#swf_deviation{severity = Severity}) ->
    case Severity of
        critical -> 0.3;
        high -> 0.2;
        medium -> 0.1;
        low -> 0.05
    end.

%%====================================================================
%% Internal Functions - Performance Analysis
%%====================================================================

-spec do_analyze_performance([#swf_event{}], #swf_net{}, state()) ->
    {[#swf_patch{}], state()}.
do_analyze_performance(Events, Net, State) ->
    %% 1. Calculate transition durations
    TransitionStats = calculate_transition_stats(Events),

    %% 2. Identify bottlenecks
    BottleneckThreshold = maps:get(bottleneck_threshold_ms, State#state.config),
    Bottlenecks = identify_bottlenecks(TransitionStats, BottleneckThreshold),

    %% 3. Identify resource contention
    Contentions = identify_resource_contention(Events, Net),

    %% 4. Generate patches for each issue
    BottleneckPatches = lists:filtermap(
        fun(Issue) ->
            case bottleneck_to_patch(Issue, Net, State) of
                {ok, Patch} -> {true, Patch};
                skip -> false
            end
        end,
        Bottlenecks
    ),

    ContentionPatches = lists:filtermap(
        fun(Issue) ->
            case contention_to_patch(Issue, Net, State) of
                {ok, Patch} -> {true, Patch};
                skip -> false
            end
        end,
        Contentions
    ),

    {BottleneckPatches ++ ContentionPatches, State}.

-spec calculate_transition_stats([#swf_event{}]) -> #{binary() => map()}.
calculate_transition_stats(Events) ->
    %% Group events by transition and calculate statistics
    TransitionEvents = [E || E <- Events,
                        E#swf_event.event_type =:= transition_fired orelse
                        E#swf_event.event_type =:= transition_completed],

    %% Build start/complete pairs
    Pairs = build_transition_pairs(TransitionEvents),

    %% Calculate stats per transition
    maps:fold(
        fun(TransitionId, Durations, Acc) ->
            case Durations of
                [] -> Acc;
                _ ->
                    Sorted = lists:sort(Durations),
                    Len = length(Sorted),
                    Avg = lists:sum(Sorted) / Len,
                    P50Index = max(1, round(Len * 0.5)),
                    P95Index = max(1, round(Len * 0.95)),
                    P99Index = max(1, round(Len * 0.99)),

                    Stats = #{
                        count => Len,
                        avg_ms => Avg,
                        min_ms => hd(Sorted),
                        max_ms => lists:last(Sorted),
                        p50_ms => lists:nth(P50Index, Sorted),
                        p95_ms => lists:nth(P95Index, Sorted),
                        p99_ms => lists:nth(P99Index, Sorted)
                    },
                    maps:put(TransitionId, Stats, Acc)
            end
        end,
        #{},
        Pairs
    ).

-spec build_transition_pairs([#swf_event{}]) -> #{binary() => [non_neg_integer()]}.
build_transition_pairs(Events) ->
    %% Track start times and compute durations
    {_, Durations} = lists:foldl(
        fun(Event, {Starts, Durs}) ->
            TId = Event#swf_event.transition_id,
            CaseId = Event#swf_event.case_id,
            Key = {CaseId, TId},

            case Event#swf_event.event_type of
                transition_fired ->
                    {maps:put(Key, Event#swf_event.timestamp, Starts), Durs};
                transition_completed ->
                    case maps:get(Key, Starts, undefined) of
                        undefined ->
                            {Starts, Durs};
                        StartTime ->
                            Duration = (Event#swf_event.timestamp - StartTime) div 1000, % ms
                            CurrentDurations = maps:get(TId, Durs, []),
                            {maps:remove(Key, Starts),
                             maps:put(TId, [Duration | CurrentDurations], Durs)}
                    end;
                _ ->
                    {Starts, Durs}
            end
        end,
        {#{}, #{}},
        Events
    ),
    Durations.

-spec identify_bottlenecks(#{binary() => map()}, pos_integer()) -> [performance_issue()].
identify_bottlenecks(TransitionStats, ThresholdMs) ->
    maps:fold(
        fun(TransitionId, Stats, Acc) ->
            AvgMs = maps:get(avg_ms, Stats, 0),
            P95Ms = maps:get(p95_ms, Stats, 0),

            case P95Ms > ThresholdMs of
                true ->
                    Severity = if
                        P95Ms > ThresholdMs * 4 -> critical;
                        P95Ms > ThresholdMs * 2 -> high;
                        P95Ms > ThresholdMs * 1.5 -> medium;
                        true -> low
                    end,

                    Issue = #{
                        type => bottleneck,
                        location => TransitionId,
                        severity => Severity,
                        avg_duration_ms => AvgMs,
                        p95_duration_ms => P95Ms,
                        frequency => maps:get(count, Stats, 0)
                    },
                    [Issue | Acc];
                false ->
                    Acc
            end
        end,
        [],
        TransitionStats
    ).

-spec identify_resource_contention([#swf_event{}], #swf_net{}) -> [performance_issue()].
identify_resource_contention(Events, Net) ->
    %% Look for transitions that frequently wait for resources
    ResourceEvents = [E || E <- Events,
                      E#swf_event.event_type =:= resource_accessed],

    %% Group by resource URI and count concurrent access attempts
    ResourceAccess = lists:foldl(
        fun(Event, Acc) ->
            ResourceUri = maps:get(resource_uri, Event#swf_event.data, undefined),
            case ResourceUri of
                undefined -> Acc;
                _ ->
                    Timestamp = Event#swf_event.timestamp,
                    Current = maps:get(ResourceUri, Acc, []),
                    maps:put(ResourceUri, [Timestamp | Current], Acc)
            end
        end,
        #{},
        ResourceEvents
    ),

    %% Identify contentious resources
    maps:fold(
        fun(ResourceUri, Timestamps, Acc) ->
            %% Count overlapping access within 100ms windows
            Overlaps = count_overlapping_access(lists:sort(Timestamps), 100000), % 100ms in microseconds

            case Overlaps > length(Timestamps) * 0.3 of % >30% overlap indicates contention
                true ->
                    %% Find related transition
                    RelatedTransition = find_resource_transition(ResourceUri, Net),

                    Issue = #{
                        type => resource_contention,
                        location => RelatedTransition,
                        severity => if Overlaps > length(Timestamps) * 0.5 -> high;
                                      true -> medium
                                   end,
                        avg_duration_ms => 0.0,
                        p95_duration_ms => 0.0,
                        frequency => length(Timestamps)
                    },
                    [Issue | Acc];
                false ->
                    Acc
            end
        end,
        [],
        ResourceAccess
    ).

-spec count_overlapping_access([integer()], integer()) -> non_neg_integer().
count_overlapping_access([], _Window) -> 0;
count_overlapping_access([_], _Window) -> 0;
count_overlapping_access([T1, T2 | Rest], Window) ->
    Overlap = if abs(T2 - T1) < Window -> 1; true -> 0 end,
    Overlap + count_overlapping_access([T2 | Rest], Window).

-spec find_resource_transition(binary(), #swf_net{}) -> binary().
find_resource_transition(_ResourceUri, _Net) ->
    %% Placeholder: would search Net for transitions bound to this resource
    <<"unknown_transition">>.

-spec bottleneck_to_patch(performance_issue(), #swf_net{}, state()) ->
    {ok, #swf_patch{}} | skip.
bottleneck_to_patch(#{type := bottleneck, location := TransitionId} = Issue, Net, _State) ->
    NetId = Net#swf_net.id,
    PatchId = generate_patch_id(NetId, <<"bottleneck_opt">>),

    %% Suggest timeout or async execution
    Change = #swf_patch_change{
        operation = modify,
        target_type = transition,
        target_id = TransitionId,
        old_value = #{},
        new_value = #{
            kind => async,
            timeout_ms => round(maps:get(p95_duration_ms, Issue) * 2),
            retry_policy => #{max_attempts => 3}
        }
    },

    Severity = maps:get(severity, Issue),
    Confidence = case Severity of
        critical -> 0.85;
        high -> 0.75;
        medium -> 0.65;
        low -> 0.55
    end,

    Risk = case Severity of
        critical -> 0.3;
        high -> 0.35;
        medium -> 0.4;
        low -> 0.45
    end,

    Patch = #swf_patch{
        id = PatchId,
        net_id = NetId,
        version = Net#swf_net.version,
        kind = modify_action,
        changes = [Change],
        rationale = iolist_to_binary([
            <<"Optimize bottleneck at ">>, TransitionId,
            <<". P95 duration: ">>,
            integer_to_binary(round(maps:get(p95_duration_ms, Issue))),
            <<"ms">>
        ]),
        confidence = Confidence,
        risk_score = Risk,
        expected_improvement = 0.15,
        proposed_by = undefined,
        proposed_at = undefined,
        status = proposed
    },

    {ok, Patch}.

-spec contention_to_patch(performance_issue(), #swf_net{}, state()) ->
    {ok, #swf_patch{}} | skip.
contention_to_patch(#{type := resource_contention, location := TransitionId} = Issue, Net, _State) ->
    NetId = Net#swf_net.id,
    PatchId = generate_patch_id(NetId, <<"contention_opt">>),

    %% Suggest adding a semaphore/pool pattern
    Change = #swf_patch_change{
        operation = modify,
        target_type = transition,
        target_id = TransitionId,
        old_value = #{},
        new_value = #{
            resourcing => #{
                allocate_strategy => shortest_queue,
                pool_size => 4
            }
        }
    },

    Severity = maps:get(severity, Issue),
    Confidence = case Severity of
        high -> 0.7;
        medium -> 0.6;
        _ -> 0.5
    end,

    Patch = #swf_patch{
        id = PatchId,
        net_id = NetId,
        version = Net#swf_net.version,
        kind = modify_action,
        changes = [Change],
        rationale = iolist_to_binary([
            <<"Reduce resource contention at ">>, TransitionId,
            <<" by adding resource pooling">>
        ]),
        confidence = Confidence,
        risk_score = 0.4,
        expected_improvement = 0.1,
        proposed_by = undefined,
        proposed_at = undefined,
        status = proposed
    },

    {ok, Patch}.

%%====================================================================
%% Internal Functions - Pattern Mining
%%====================================================================

-spec do_mine_patterns([#swf_event{}], map(), state()) -> {[pattern()], state()}.
do_mine_patterns(Events, Opts, State) ->
    MinFreq = maps:get(min_frequency, Opts,
                       maps:get(min_pattern_frequency, State#state.config)),

    %% Extract activity sequences
    Sequences = extract_sequences(Events),

    %% Mine frequent subsequences
    FrequentPatterns = mine_frequent_subsequences(Sequences, MinFreq),

    %% Mine direct follows patterns
    DirectFollows = mine_direct_follows(Events, MinFreq),

    %% Mine eventually follows patterns
    EventuallyFollows = mine_eventually_follows(Events, MinFreq),

    AllPatterns = FrequentPatterns ++ DirectFollows ++ EventuallyFollows,

    %% Update stats
    Stats = State#state.stats,
    NewStats = Stats#{
        patterns_mined => maps:get(patterns_mined, Stats, 0) + length(AllPatterns)
    },

    {AllPatterns, State#state{stats = NewStats}}.

-spec extract_sequences([#swf_event{}]) -> [[binary()]].
extract_sequences(Events) ->
    %% Group events by case and extract transition sequences
    ByCase = lists:foldl(
        fun(Event, Acc) ->
            CaseId = Event#swf_event.case_id,
            case Event#swf_event.transition_id of
                undefined -> Acc;
                TId ->
                    Current = maps:get(CaseId, Acc, []),
                    maps:put(CaseId, [{Event#swf_event.sequence, TId} | Current], Acc)
            end
        end,
        #{},
        Events
    ),

    %% Sort each case's events and extract just transition IDs
    maps:fold(
        fun(_CaseId, SeqEvents, Acc) ->
            Sorted = lists:keysort(1, SeqEvents),
            Sequence = [TId || {_, TId} <- Sorted],
            [Sequence | Acc]
        end,
        [],
        ByCase
    ).

-spec mine_frequent_subsequences([[binary()]], pos_integer()) -> [pattern()].
mine_frequent_subsequences(Sequences, MinFreq) ->
    %% Count 2-grams and 3-grams
    NGrams = lists:foldl(
        fun(Seq, Acc) ->
            Len = length(Seq),
            TwoGrams = if Len >= 2 ->
                           [[lists:nth(I, Seq), lists:nth(I+1, Seq)]
                            || I <- lists:seq(1, Len - 1)];
                         true -> []
                       end,
            ThreeGrams = if Len >= 3 ->
                            [[lists:nth(I, Seq), lists:nth(I+1, Seq), lists:nth(I+2, Seq)]
                             || I <- lists:seq(1, Len - 2)];
                          true -> []
                        end,
            lists:foldl(
                fun(NGram, A) ->
                    Count = maps:get(NGram, A, 0),
                    maps:put(NGram, Count + 1, A)
                end,
                Acc,
                TwoGrams ++ ThreeGrams
            )
        end,
        #{},
        Sequences
    ),

    %% Filter by frequency and convert to patterns
    TotalCases = length(Sequences),
    maps:fold(
        fun(NGram, Count, Acc) when Count >= MinFreq ->
            Pattern = #{
                type => frequent_subsequence,
                frequency => Count,
                confidence => Count / max(1, TotalCases),
                elements => NGram,
                metadata => #{length => length(NGram)}
            },
            [Pattern | Acc];
           (_, _, Acc) ->
            Acc
        end,
        [],
        NGrams
    ).

-spec mine_direct_follows([#swf_event{}], pos_integer()) -> [pattern()].
mine_direct_follows(Events, MinFreq) ->
    %% Count direct follows relationships (A directly followed by B)
    TransitionEvents = lists:sort(
        fun(E1, E2) ->
            {E1#swf_event.case_id, E1#swf_event.sequence} =<
            {E2#swf_event.case_id, E2#swf_event.sequence}
        end,
        [E || E <- Events, E#swf_event.transition_id =/= undefined]
    ),

    {_, Follows} = lists:foldl(
        fun(Event, {Prev, Acc}) ->
            case Prev of
                undefined ->
                    {Event, Acc};
                PrevEvent when PrevEvent#swf_event.case_id =:= Event#swf_event.case_id ->
                    Pair = {PrevEvent#swf_event.transition_id, Event#swf_event.transition_id},
                    Count = maps:get(Pair, Acc, 0),
                    {Event, maps:put(Pair, Count + 1, Acc)};
                _ ->
                    {Event, Acc}
            end
        end,
        {undefined, #{}},
        TransitionEvents
    ),

    %% Filter and convert to patterns
    maps:fold(
        fun({From, To}, Count, Acc) when Count >= MinFreq ->
            Pattern = #{
                type => direct_follows,
                frequency => Count,
                confidence => 1.0, % Would need more context for proper confidence
                elements => [From, To],
                metadata => #{from => From, to => To}
            },
            [Pattern | Acc];
           (_, _, Acc) ->
            Acc
        end,
        [],
        Follows
    ).

-spec mine_eventually_follows([#swf_event{}], pos_integer()) -> [pattern()].
mine_eventually_follows(Events, MinFreq) ->
    %% Count eventually follows relationships (A eventually followed by B)
    Sequences = extract_sequences(Events),

    Eventually = lists:foldl(
        fun(Seq, Acc) ->
            Len = length(Seq),
            Pairs = [{lists:nth(I, Seq), lists:nth(J, Seq)}
                     || I <- lists:seq(1, Len - 1),
                        J <- lists:seq(I + 1, Len),
                        J - I > 1], % Skip direct follows
            lists:foldl(
                fun(Pair, A) ->
                    Count = maps:get(Pair, A, 0),
                    maps:put(Pair, Count + 1, A)
                end,
                Acc,
                Pairs
            )
        end,
        #{},
        Sequences
    ),

    %% Filter and convert to patterns
    maps:fold(
        fun({From, To}, Count, Acc) when Count >= MinFreq ->
            Pattern = #{
                type => eventually_follows,
                frequency => Count,
                confidence => 0.8, % Lower confidence than direct follows
                elements => [From, To],
                metadata => #{from => From, to => To}
            },
            [Pattern | Acc];
           (_, _, Acc) ->
            Acc
        end,
        [],
        Eventually
    ).

%%====================================================================
%% Internal Functions - Structural Improvements
%%====================================================================

-spec propose_structural_patches([#swf_event{}], #swf_net{}, state()) -> [#swf_patch{}].
propose_structural_patches(Events, Net, State) ->
    %% 1. Look for parallelization opportunities
    ParallelPatches = find_parallelization_opportunities(Events, Net, State),

    %% 2. Look for simplification opportunities
    SimplifyPatches = find_simplification_opportunities(Net, State),

    ParallelPatches ++ SimplifyPatches.

-spec find_parallelization_opportunities([#swf_event{}], #swf_net{}, state()) -> [#swf_patch{}].
find_parallelization_opportunities(Events, Net, State) ->
    %% Find transitions that could execute in parallel
    Sequences = extract_sequences(Events),

    %% Look for independent subsequences
    ThresholdRatio = maps:get(parallelization_threshold, State#state.config),

    %% Find pairs that never have direct data dependencies
    Candidates = find_independent_pairs(Sequences, ThresholdRatio),

    lists:filtermap(
        fun({T1, T2, Confidence}) ->
            case Confidence >= ThresholdRatio of
                true ->
                    PatchId = generate_patch_id(Net#swf_net.id, <<"parallelize">>),

                    Change = #swf_patch_change{
                        operation = modify,
                        target_type = arc,
                        target_id = <<T1/binary, "_", T2/binary, "_parallel">>,
                        old_value = #{pattern => sequential},
                        new_value = #{pattern => parallel, tasks => [T1, T2]}
                    },

                    Patch = #swf_patch{
                        id = PatchId,
                        net_id = Net#swf_net.id,
                        version = Net#swf_net.version,
                        kind = restructure,
                        changes = [Change],
                        rationale = iolist_to_binary([
                            <<"Parallelize transitions ">>, T1, <<" and ">>, T2,
                            <<" (independence confidence: ">>,
                            float_to_binary(Confidence, [{decimals, 2}]), <<")">>
                        ]),
                        confidence = Confidence * 0.8,
                        risk_score = 0.5,
                        expected_improvement = 0.2,
                        proposed_by = undefined,
                        proposed_at = undefined,
                        status = proposed
                    },
                    {true, Patch};
                false ->
                    false
            end
        end,
        Candidates
    ).

-spec find_independent_pairs([[binary()]], float()) -> [{binary(), binary(), float()}].
find_independent_pairs(Sequences, _ThresholdRatio) ->
    %% Count order variations: if T1 before T2 ~50% and T2 before T1 ~50%, they're independent
    OrderCounts = lists:foldl(
        fun(Seq, Acc) ->
            Len = length(Seq),
            Pairs = [{lists:nth(I, Seq), lists:nth(J, Seq), before}
                     || I <- lists:seq(1, Len - 1),
                        J <- lists:seq(I + 1, Len)],
            lists:foldl(
                fun({T1, T2, before}, A) ->
                    Key = {T1, T2},
                    {Before, After} = maps:get(Key, A, {0, 0}),
                    maps:put(Key, {Before + 1, After}, A)
                end,
                Acc,
                Pairs
            )
        end,
        #{},
        Sequences
    ),

    %% Find pairs with balanced ordering (indicating independence)
    maps:fold(
        fun({T1, T2}, {Before, After}, Acc) ->
            Total = Before + After,
            case Total > 0 of
                true ->
                    Ratio = min(Before, After) / Total,
                    %% High ratio means both orderings occur, suggesting independence
                    case Ratio > 0.3 of
                        true -> [{T1, T2, Ratio * 2} | Acc]; % Scale to 0-1
                        false -> Acc
                    end;
                false ->
                    Acc
            end
        end,
        [],
        OrderCounts
    ).

-spec find_simplification_opportunities(#swf_net{}, state()) -> [#swf_patch{}].
find_simplification_opportunities(Net, _State) ->
    %% Find redundant places or transitions
    Places = maps:keys(Net#swf_net.places),
    Arcs = Net#swf_net.arcs,

    %% Find places with single input and single output (candidates for removal)
    RedundantPlaces = lists:filter(
        fun(PlaceId) ->
            InArcs = [A || A <- Arcs, A#swf_arc.target =:= PlaceId],
            OutArcs = [A || A <- Arcs, A#swf_arc.source =:= PlaceId],
            length(InArcs) =:= 1 andalso length(OutArcs) =:= 1
        end,
        Places
    ),

    lists:filtermap(
        fun(PlaceId) ->
            %% Only suggest removal if not initial or final
            IsInitial = maps:get(PlaceId, Net#swf_net.initial_marking, 0) > 0,
            IsFinal = lists:member(PlaceId, Net#swf_net.final_places),

            case not IsInitial andalso not IsFinal of
                true ->
                    PatchId = generate_patch_id(Net#swf_net.id, <<"simplify">>),

                    Change = #swf_patch_change{
                        operation = remove,
                        target_type = place,
                        target_id = PlaceId,
                        old_value = maps:get(PlaceId, Net#swf_net.places, undefined),
                        new_value = undefined
                    },

                    Patch = #swf_patch{
                        id = PatchId,
                        net_id = Net#swf_net.id,
                        version = Net#swf_net.version,
                        kind = remove_place,
                        changes = [Change],
                        rationale = iolist_to_binary([
                            <<"Remove redundant place ">>, PlaceId,
                            <<" (single input/output)">>
                        ]),
                        confidence = 0.5, % Lower confidence for structural changes
                        risk_score = 0.6, % Higher risk
                        expected_improvement = 0.05,
                        proposed_by = undefined,
                        proposed_at = undefined,
                        status = proposed
                    },
                    {true, Patch};
                false ->
                    false
            end
        end,
        RedundantPlaces
    ).

%%====================================================================
%% Internal Functions - Scoring
%%====================================================================

-spec do_score_proposal(#swf_patch{}, state()) -> float().
do_score_proposal(#swf_patch{changes = Changes, kind = Kind} = Patch, State) ->
    %% Base score from patch kind
    KindScore = case Kind of
        modify_guard -> 0.7;
        modify_action -> 0.65;
        add_constraint -> 0.6;
        add_transition -> 0.5;
        add_place -> 0.5;
        add_arc -> 0.55;
        remove_place -> 0.4;
        remove_transition -> 0.35;
        remove_arc -> 0.45;
        remove_constraint -> 0.5;
        restructure -> 0.4
    end,

    %% Adjust for number of changes
    NumChanges = length(Changes),
    ChangePenalty = if
        NumChanges =:= 1 -> 1.0;
        NumChanges =< 3 -> 0.9;
        NumChanges =< 5 -> 0.8;
        true -> 0.6
    end,

    %% Adjust for historical success
    History = State#state.proposal_history,
    HistoryBonus = calculate_history_bonus(Patch, History),

    min(1.0, KindScore * ChangePenalty * HistoryBonus).

-spec calculate_history_bonus(#swf_patch{}, #{binary() => promoted | rejected}) -> float().
calculate_history_bonus(#swf_patch{kind = Kind}, History) ->
    %% Count successes/failures for similar patch kinds
    {Promoted, Total} = maps:fold(
        fun(_PatchId, Outcome, {P, T}) ->
            %% In production, would track patch kind in history
            case Outcome of
                promoted -> {P + 1, T + 1};
                rejected -> {P, T + 1}
            end
        end,
        {0, 0},
        History
    ),

    case Total of
        0 -> 1.0;
        _ ->
            %% Bonus/penalty based on success rate
            SuccessRate = Promoted / Total,
            0.8 + (0.4 * SuccessRate) % Range: 0.8 to 1.2
    end.

-spec do_assess_risk(#swf_patch{}, state()) -> float().
do_assess_risk(#swf_patch{changes = Changes, kind = Kind}, _State) ->
    %% Base risk from patch kind
    KindRisk = case Kind of
        modify_guard -> 0.3;
        modify_action -> 0.35;
        add_constraint -> 0.25;
        add_transition -> 0.5;
        add_place -> 0.4;
        add_arc -> 0.35;
        remove_place -> 0.6;
        remove_transition -> 0.7;
        remove_arc -> 0.5;
        remove_constraint -> 0.4;
        restructure -> 0.65
    end,

    %% Adjust for number of changes
    NumChanges = length(Changes),
    ChangeRisk = if
        NumChanges =:= 1 -> 1.0;
        NumChanges =< 3 -> 1.2;
        NumChanges =< 5 -> 1.4;
        true -> 1.8
    end,

    %% Assess operation risks
    OperationRisk = lists:foldl(
        fun(#swf_patch_change{operation = Op}, Acc) ->
            OpRisk = case Op of
                add -> 0.0;
                modify -> 0.1;
                remove -> 0.2
            end,
            Acc + OpRisk
        end,
        0.0,
        Changes
    ),

    min(1.0, KindRisk * ChangeRisk + OperationRisk / max(1, NumChanges)).

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

-spec rank_patches([#swf_patch{}]) -> [#swf_patch{}].
rank_patches(Patches) ->
    %% Rank by composite score: confidence * improvement / (1 + risk)
    lists:sort(
        fun(P1, P2) ->
            Score1 = P1#swf_patch.confidence * P1#swf_patch.expected_improvement /
                     (1 + P1#swf_patch.risk_score),
            Score2 = P2#swf_patch.confidence * P2#swf_patch.expected_improvement /
                     (1 + P2#swf_patch.risk_score),
            Score1 >= Score2
        end,
        Patches
    ).

-spec maybe_submit_patch(#swf_patch{}, state()) -> ok.
maybe_submit_patch(Patch, State) ->
    case State#state.coordinator of
        undefined ->
            ok;
        Coordinator when is_pid(Coordinator) ->
            gen_server:cast(Coordinator, {propose_patch, Patch}),
            ok
    end.

-spec generate_worker_id() -> binary().
generate_worker_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFFFF),
    iolist_to_binary([
        <<"patch_proposer-">>,
        integer_to_binary(Timestamp, 16), <<"-">>,
        integer_to_binary(Random, 16)
    ]).

-spec generate_patch_id(net_id(), binary()) -> binary().
generate_patch_id(NetId, Type) ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFF),
    iolist_to_binary([
        <<"patch-">>,
        binary:part(NetId, 0, min(8, byte_size(NetId))), <<"-">>,
        Type, <<"-">>,
        integer_to_binary(Timestamp, 16), <<"-">>,
        integer_to_binary(Random, 16)
    ]).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test confidence calculation
confidence_calculation_test() ->
    State = #state{
        worker_id = <<"test">>,
        coordinator = undefined,
        pattern_cache = #{},
        proposal_history = #{},
        config = #{min_confidence => 0.5, max_risk => 0.5,
                   min_pattern_frequency => 2, bottleneck_threshold_ms => 1000,
                   parallelization_threshold => 0.7, enable_structural_patches => true},
        stats = #{patches_proposed => 0, patches_promoted => 0, patches_rejected => 0,
                  patterns_mined => 0, deviations_analyzed => 0, avg_confidence => 0.0,
                  avg_risk => 0.0}
    },

    Deviation = #swf_deviation{
        type = missing_token,
        location = <<"p1">>,
        expected = 1,
        actual = 0,
        severity = high,
        message = <<"Test">>
    },

    Confidence = calculate_deviation_confidence(Deviation, State),
    ?assert(Confidence >= 0.0),
    ?assert(Confidence =< 1.0).

%% Test risk assessment
risk_assessment_test() ->
    Net = #swf_net{
        id = <<"test-net">>,
        name = <<"Test">>,
        version = <<"1.0">>,
        places = #{<<"p1">> => #swf_place{id = <<"p1">>, name = <<"Start">>}},
        transitions = #{<<"t1">> => #swf_transition{id = <<"t1">>, name = <<"Task">>, kind = automatic}},
        arcs = [],
        initial_marking = #{<<"p1">> => 1},
        final_places = [],
        metadata = #{}
    },

    Deviation = #swf_deviation{
        type = wrong_transition,
        location = <<"t2">>,
        expected = <<"model">>,
        actual = <<"t2">>,
        severity = medium,
        message = <<"Test">>
    },

    Risk = calculate_deviation_risk(Deviation, Net),
    ?assert(Risk >= 0.0),
    ?assert(Risk =< 1.0).

%% Test pattern mining
pattern_mining_test() ->
    Events = [
        #swf_event{id = <<"e1">>, case_id = <<"c1">>, event_type = transition_fired,
                   transition_id = <<"t1">>, timestamp = 1000, sequence = 0, data = #{}},
        #swf_event{id = <<"e2">>, case_id = <<"c1">>, event_type = transition_fired,
                   transition_id = <<"t2">>, timestamp = 2000, sequence = 1, data = #{}},
        #swf_event{id = <<"e3">>, case_id = <<"c2">>, event_type = transition_fired,
                   transition_id = <<"t1">>, timestamp = 3000, sequence = 0, data = #{}},
        #swf_event{id = <<"e4">>, case_id = <<"c2">>, event_type = transition_fired,
                   transition_id = <<"t2">>, timestamp = 4000, sequence = 1, data = #{}}
    ],

    Sequences = extract_sequences(Events),
    ?assertEqual(2, length(Sequences)),

    DirectFollows = mine_direct_follows(Events, 1),
    ?assert(length(DirectFollows) > 0).

-endif.
