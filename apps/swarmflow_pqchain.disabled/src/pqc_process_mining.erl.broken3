%%%-------------------------------------------------------------------
%%% @doc PQC Process Mining - High-Level Integration API
%%%
%%% Van der Aalst-style process mining operations that integrate:
%%% - Alpha Miner (pqc_alpha_miner)
%%% - Heuristics Miner (pqc_heuristics_miner)
%%% - Alignment-based conformance checking (pqc_alignment)
%%%
%%% This module provides the high-level API for process discovery,
%%% conformance checking, and model enhancement following the process
%%% mining methodology described in:
%%%
%%% Van der Aalst, W.M.P. (2016). Process Mining: Data Science in Action.
%%% Springer, 2nd edition.
%%%
%%% == Algorithm Auto-Selection ==
%%%
%%% The discover/2 function with algorithm => auto uses these heuristics:
%%% - Log has high noise (>10% infrequent variants) -> Heuristics Miner
%%% - Log is clean and small (<1000 events) -> Alpha Miner
%%% - Log has complex loops -> Heuristics Miner with loop detection
%%% - Default -> Heuristics Miner (most robust)
%%%
%%% == Quality Metrics ==
%%%
%%% - Fitness: How well does the model replay the log?
%%% - Precision: How much extra behavior does the model allow?
%%% - Generalization: How well does the model generalize?
%%% - Simplicity: How simple is the model structure?
%%% - F-Measure: Harmonic mean of fitness and precision
%%%
%%% == Integration ==
%%%
%%% Designed for use with SwarmFlow PQChain blockchain where:
%%% - Event logs come from on-chain execution receipts
%%% - Discovered models become smart contract templates
%%% - Conformance checking validates transaction execution
%%%
%%% OTP 26+ compatible. All operations are pure functions.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_process_mining).

-include("pqchain.hrl").
-include_lib("swarmflow_os/include/swarmflow.hrl").

%% API exports
-export([
    discover/1,
    discover/2,,,
    filter_log/2,
    aggregate_stats/1,
    export_bpmn/1,
    export_pnml/1,
    visualize/2
]).

%% Utility exports
-export([
    default_options/0,
    select_algorithm/1,
    compute_log_statistics/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type event_log() :: [trace()] | [#swf_event{}].
-type trace() :: [atom()].
-type algorithm() :: alpha | heuristics | inductive | auto.
-type visualization_format() :: dot | mermaid | plantuml.

-record(mining_result, {
    algorithm :: algorithm(),
    net :: #swf_net{},
    quality :: quality_metrics(),
    execution_time_ms :: non_neg_integer(),
    trace_count :: non_neg_integer(),
    activity_count :: non_neg_integer()
}).

-record(quality_metrics, {
    fitness :: float(),
    precision :: float(),
    generalization :: float(),
    simplicity :: float(),
    f_measure :: float()
}).

-record(conformance_result, {
    trace_fitness :: [{trace(), float()}],
    overall_fitness :: float(),
    deviations :: [deviation()],
    alignments :: [pqc_alignment:alignment()]
}).

-record(deviation, {
    trace_id :: binary(),
    position :: non_neg_integer(),
    type :: log_only | model_only,
    expected :: atom() | undefined,
    observed :: atom() | undefined
}).

-record(log_statistics, {
    trace_count :: non_neg_integer(),
    event_count :: non_neg_integer(),
    activity_count :: non_neg_integer(),
    unique_activities :: ordsets:ordset(atom()),
    variant_count :: non_neg_integer(),
    most_common_variant :: trace() | undefined,
    avg_trace_length :: float(),
    min_trace_length :: non_neg_integer(),
    max_trace_length :: non_neg_integer(),
    noise_ratio :: float(),
    loop_indicators :: non_neg_integer()
}).

-type mining_options() :: #{
    algorithm => algorithm(),
    config => term(),
    evaluate => boolean(),
    timeout_ms => pos_integer()
}.

-type filter_spec() :: #{
    time_range => {Start :: integer(), End :: integer()},
    activities => [atom()],
    cases => [binary()],
    min_frequency => non_neg_integer(),
    max_frequency => non_neg_integer()
}.

-type comparison() :: #{
    model1_metrics => quality_metrics(),
    model2_metrics => quality_metrics(),
    winner => model1 | model2 | tie,
    reason => binary()
}.

-export_type([
    mining_result/0,
    quality_metrics/0,
    conformance_result/0,
    deviation/0,
    log_statistics/0,
    mining_options/0,
    filter_spec/0,
    comparison/0,
    algorithm/0,
    event_log/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Discover process model from event log with default options.
%%
%% Uses auto-selection to choose the best algorithm based on log
%% characteristics.
%%
%% Example:
%% ```
%% EventLog = [[a, b, c], [a, c, b], [a, b, c]],
%% {ok, Result} = pqc_process_mining:discover(EventLog),
%% #mining_result{algorithm = heuristics, quality = Metrics} = Result.
%% '''
%% @end
%%--------------------------------------------------------------------
-spec discover(EventLog :: event_log()) -> {ok, mining_result()} | {error, term()}.
discover(EventLog) ->
    discover(EventLog, default_options()).

%%--------------------------------------------------------------------
%% @doc Discover process model from event log with custom options.
%%
%% Options:
%% - algorithm: alpha | heuristics | auto (default: auto)
%% - config: Algorithm-specific configuration
%% - evaluate: Compute quality metrics (default: true)
%% - timeout_ms: Maximum execution time (default: 60000)
%%
%% @end
%%--------------------------------------------------------------------
-spec discover(EventLog :: event_log(), Options :: mining_options()) ->
    {ok, mining_result()} | {error, term()}.
discover([], _Options) ->
    {error, <<"empty_event_log">>};
discover(EventLog, Options) when is_list(EventLog), is_map(Options) ->
    StartTime = erlang:monotonic_time(millisecond),

    try
        %% Normalize event log to traces
        Traces = normalize_event_log(EventLog),

        %% Select algorithm
        Algorithm = case maps:get(algorithm, Options, auto) of
            auto -> select_algorithm(Traces);
            Algo -> Algo
        end,

        %% Get algorithm configuration
        Config = maps:get(config, Options, default_algorithm_config(Algorithm)),

        %% Run discovery
        {ok, Net} = case Algorithm of
            alpha ->
                pqc_alpha_miner:discover(Traces);
            heuristics ->
                pqc_heuristics_miner:discover(Traces, Config);
            inductive ->
                {error, <<"inductive_miner_not_implemented">>}
        end,

        %% Compute execution time
        EndTime = erlang:monotonic_time(millisecond),
        ExecutionTime = EndTime - StartTime,

        %% Compute statistics
        Stats = compute_log_statistics(Traces),

        %% Evaluate quality if requested
        Quality = case maps:get(evaluate, Options, true) of
            true -> evaluate_model_internal(Net, Traces);
            false -> undefined
        end,

        Result = #mining_result{
            algorithm = Algorithm,
            net = Net,
            quality = Quality,
            execution_time_ms = ExecutionTime,
            trace_count = Stats#log_statistics.trace_count,
            activity_count = Stats#log_statistics.activity_count
        },

        {ok, Result}
    catch
        error:Reason:Stack ->
            {error, {discovery_failed, Reason, Stack}};
        throw:Error ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Run full conformance checking with alignments.
%%
%% Computes optimal alignments for each trace in the event log and
%% aggregates conformance metrics.
%%
%% Returns overall fitness score and per-trace fitness scores.
%%
%% @end
%%--------------------------------------------------------------------
-spec conformance_check(Net :: #swf_net{}, EventLog :: event_log()) ->
    {ok, conformance_result()} | {error, term()}.
conformance_check(#swf_net{} = Net) ->
    try
        %% Normalize event log
        Traces = normalize_event_log(EventLog),

        %% Convert Net to alignment-compatible format
        AlignmentNet = convert_net_to_alignment_format(Net),

        %% Compute alignments for each trace
        AlignmentResults = lists:map(
            fun(Trace) ->
                case pqc_alignment:align(Trace, AlignmentNet) of
                    {ok, Alignment} ->
                        {Trace, Alignment};
                    {error, Reason} ->
                        throw({alignment_failed, Trace, Reason})
                end
            end,
            Traces
        ),

        %% Extract trace fitness scores
        TraceFitness = [
            {T, pqc_alignment:fitness([A], [T])}
            || {T, A} <- AlignmentResults
        ],

        %% Compute overall fitness
        Alignments = [A || {_T, A} <- AlignmentResults],
        OverallFitness = pqc_alignment:fitness(Alignments, Traces),

        %% Extract deviations
        Deviations = extract_deviations(AlignmentResults),

        Result = #conformance_result{
            trace_fitness = TraceFitness,
            overall_fitness = OverallFitness,
            deviations = Deviations,
            alignments = Alignments
        },

        {ok, Result}
    catch
        error:Reason:Stack ->
            {error, {conformance_check_failed, Reason, Stack}};
        throw:Error ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Compute all quality metrics for a model.
%%
%% Returns:
%% - Fitness: Replay fitness (1.0 = perfect)
%% - Precision: Model specificity
%% - Generalization: Model generalization capability
%% - Simplicity: Structural simplicity
%% - F-Measure: Harmonic mean of fitness and precision
%%
%% @end
%%--------------------------------------------------------------------
-spec evaluate_model(Net :: #swf_net{}, EventLog :: event_log()) -> quality_metrics().
evaluate_model(#swf_net{} = Net) ->
    Traces = normalize_event_log(EventLog),
    evaluate_model_internal(Net, Traces).

%%--------------------------------------------------------------------
%% @doc Compare two models on the same event log.
%%
%% Returns quality metrics for both models and determines which is
%% better based on F-measure.
%%
%% @end
%%--------------------------------------------------------------------
-spec compare_models(Net1 :: #swf_net{}, Net2 :: #swf_net{}, EventLog :: event_log()) ->
    comparison().
compare_models(Net1, Net2, EventLog)
  when #swf_net{} = Net1, #swf_net{} = Net2, is_list(EventLog) ->(#swf_net{} = Net1) ->

    %% Evaluate both models
    Metrics1 = evaluate_model(Net1, EventLog),
    Metrics2 = evaluate_model(Net2, EventLog),

    %% Compare F-measures
    FMeasure1 = Metrics1#quality_metrics.f_measure,
    FMeasure2 = Metrics2#quality_metrics.f_measure,

    {Winner, Reason} = if
        FMeasure1 > FMeasure2 + 0.01 ->
            {model1, <<"Model 1 has higher F-measure">>};
        FMeasure2 > FMeasure1 + 0.01 ->
            {model2, <<"Model 2 has higher F-measure">>};
        true ->
            {tie, <<"Models have similar F-measures">>}
    end,

    #{
        model1_metrics => Metrics1,
        model2_metrics => Metrics2,
        winner => Winner,
        reason => Reason
    }.

%%--------------------------------------------------------------------
%% @doc Enhance model with frequency and performance annotations.
%%
%% Adds metadata to transitions with:
%% - Execution frequency
%% - Average execution time
%% - Standard deviation
%%
%% Returns enhanced net with updated metadata.
%%
%% @end
%%--------------------------------------------------------------------
-spec enhance_model(Net :: #swf_net{}, EventLog :: event_log()) ->
    {ok, #swf_net{}} | {error, term()}.
enhance_model(#swf_net{} = Net) ->
    try
        Traces = normalize_event_log(EventLog),

        %% Compute transition frequencies
        Frequencies = compute_transition_frequencies(Traces),

        %% Update transitions with frequency metadata
        Transitions = Net#swf_net.transitions,
        EnhancedTransitions = maps:map(
            fun(TId, Transition) ->
                TIdAtom = binary_to_atom(TId, utf8),
                Freq = maps:get(TIdAtom, Frequencies, 0),
                CurrentMeta = case Transition#swf_transition.metadata of
                    undefined -> #{};
                    M -> M
                end,
                EnhancedMeta = CurrentMeta#{
                    frequency => Freq,
                    relative_frequency => calculate_relative_frequency(Freq, Traces)
                },
                Transition#swf_transition{metadata = EnhancedMeta}
            end,
            Transitions
        ),

        EnhancedNet = Net#swf_net{transitions = EnhancedTransitions},
        {ok, EnhancedNet}
    catch
        error:Reason:Stack ->
            {error, {enhancement_failed, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Filter event log by various criteria.
%%
%% FilterSpec options:
%% - time_range: {StartTime, EndTime}
%% - activities: List of activities to include
%% - cases: List of case IDs to include
%% - min_frequency: Minimum trace frequency
%% - max_frequency: Maximum trace frequency
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_log(EventLog :: event_log(), FilterSpec :: filter_spec()) -> event_log().
filter_log(EventLog, FilterSpec) when is_list(EventLog), is_map(FilterSpec) ->
    %% Apply filters in sequence
    Filtered1 = filter_by_time(EventLog, FilterSpec),
    Filtered2 = filter_by_activities(Filtered1, FilterSpec),
    Filtered3 = filter_by_cases(Filtered2, FilterSpec),
    Filtered4 = filter_by_frequency(Filtered3, FilterSpec),
    Filtered4.

%%--------------------------------------------------------------------
%% @doc Compute aggregate statistics for event log.
%%
%% Returns:
%% - Total traces and events
%% - Unique activities
%% - Variant statistics
%% - Trace length statistics
%% - Noise indicators
%% - Loop indicators
%%
%% @end
%%--------------------------------------------------------------------
-spec aggregate_stats(EventLog :: event_log()) -> log_statistics().
aggregate_stats(EventLog) when is_list(EventLog) ->
    Traces = normalize_event_log(EventLog),
    compute_log_statistics(Traces).

%%--------------------------------------------------------------------
%% @doc Export discovered model as BPMN 2.0 XML.
%%
%% Converts SwarmFlow net to BPMN 2.0 format suitable for import into
%% process modeling tools like Camunda, ARIS, or Signavio.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_bpmn(Net :: #swf_net{}) -> iolist().
export_bpmn(#swf_net{} = Net) ->
    NetId = Net#swf_net.id,
    NetName = Net#swf_net.name,

    [
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>,
        <<"<definitions xmlns=\"http://www.omg.org/spec/BPMN/20100524/MODEL\"\n">>,
        <<"             xmlns:bpmndi=\"http://www.omg.org/spec/BPMN/20100524/DI\"\n">>,
        <<"             id=\"">>, NetId, <<"\"\n">>,
        <<"             targetNamespace=\"http://erlmcp.org/bpmn\">\n">>,
        <<"  <process id=\"">>, NetId, <<"\" name=\"">>, NetName, <<"\">\n">>,
        export_bpmn_transitions(Net#swf_net.transitions),
        <<"  </process>\n">>,
        <<"</definitions>\n">>
    ].

%%--------------------------------------------------------------------
%% @doc Export discovered model as Petri Net Markup Language (PNML).
%%
%% PNML is the standard XML format for Petri nets, used by tools like
%% ProM, LoLA, and CPN Tools.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_pnml(Net :: #swf_net{}) -> iolist().
export_pnml(#swf_net{} = Net) ->
    NetId = Net#swf_net.id,
    NetName = Net#swf_net.name,

    [
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>,
        <<"<pnml xmlns=\"http://www.pnml.org/version-2009/grammar/pnml\">\n">>,
        <<"  <net id=\"">>, NetId, <<"\" type=\"http://www.pnml.org/version-2009/grammar/ptnet\">\n">>,
        <<"    <name>\n">>,
        <<"      <text>">>, NetName, <<"</text>\n">>,
        <<"    </name>\n">>,
        export_pnml_places(Net#swf_net.places),
        export_pnml_transitions(Net#swf_net.transitions),
        export_pnml_arcs(Net#swf_net.arcs),
        <<"  </net>\n">>,
        <<"</pnml>\n">>
    ].

%%--------------------------------------------------------------------
%% @doc Visualize model in various formats.
%%
%% Supported formats:
%% - dot: GraphViz DOT format
%% - mermaid: Mermaid diagram syntax
%% - plantuml: PlantUML activity diagram
%%
%% @end
%%--------------------------------------------------------------------
-spec visualize(Net :: #swf_net{}, Format :: visualization_format()) -> iolist().
visualize(Net, dot) ->
    export_dot(Net);
visualize(Net, mermaid) ->
    export_mermaid(Net);
visualize(Net, plantuml) ->
    export_plantuml(Net).

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Get default discovery options.
%% @end
%%--------------------------------------------------------------------
-spec default_options() -> mining_options().
default_options() ->
    #{
        algorithm => auto,
        evaluate => true,
        timeout_ms => 60000
    }.

%%--------------------------------------------------------------------
%% @doc Select best algorithm based on log characteristics.
%%
%% Heuristics:
%% - High noise ratio (>0.1) -> Heuristics Miner
%% - Complex loops (many self-loops) -> Heuristics Miner
%% - Clean, small log (<1000 events) -> Alpha Miner
%% - Default -> Heuristics Miner (most robust)
%%
%% @end
%%--------------------------------------------------------------------
-spec select_algorithm(Traces :: [trace()]) -> algorithm().
select_algorithm(Traces) ->
    Stats = compute_log_statistics(Traces),

    %% Decision rules
    HighNoise = Stats#log_statistics.noise_ratio > 0.1,
    ComplexLoops = Stats#log_statistics.loop_indicators > 5,
    SmallCleanLog = Stats#log_statistics.event_count < 1000
        andalso Stats#log_statistics.noise_ratio < 0.05,

    if
        HighNoise -> heuristics;
        ComplexLoops -> heuristics;
        SmallCleanLog -> alpha;
        true -> heuristics  % Most robust default
    end.

%%--------------------------------------------------------------------
%% @doc Compute comprehensive log statistics.
%% @end
%%--------------------------------------------------------------------
-spec compute_log_statistics(Traces :: [trace()]) -> log_statistics().
compute_log_statistics(Traces) ->
    TraceCount = length(Traces),
    EventCount = lists:sum([length(T) || T <- Traces]),

    %% Collect all activities
    AllActivities = ordsets:from_list(lists:flatten(Traces)),
    ActivityCount = ordsets:size(AllActivities),

    %% Compute variant statistics
    Variants = compute_variants(Traces),
    VariantCount = length(Variants),
    MostCommonVariant = case Variants of
        [] -> undefined;
        [{Variant, _Count} | _] -> Variant
    end,

    %% Trace length statistics
    Lengths = [length(T) || T <- Traces],
    AvgLength = case Lengths of
        [] -> 0.0;
        _ -> lists:sum(Lengths) / length(Lengths)
    end,
    MinLength = case Lengths of
        [] -> 0;
        _ -> lists:min(Lengths)
    end,
    MaxLength = case Lengths of
        [] -> 0;
        _ -> lists:max(Lengths)
    end,

    %% Noise ratio (infrequent variants)
    NoiseRatio = calculate_noise_ratio(Variants, TraceCount),

    %% Loop indicators (self-transitions)
    LoopIndicators = count_loop_indicators(Traces),

    #log_statistics{
        trace_count = TraceCount,
        event_count = EventCount,
        activity_count = ActivityCount,
        unique_activities = AllActivities,
        variant_count = VariantCount,
        most_common_variant = MostCommonVariant,
        avg_trace_length = AvgLength,
        min_trace_length = MinLength,
        max_trace_length = MaxLength,
        noise_ratio = NoiseRatio,
        loop_indicators = LoopIndicators
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Normalize event log to list of traces.
-spec normalize_event_log(EventLog :: event_log()) -> [trace()].
normalize_event_log([]) ->
    [];
normalize_event_log(#swf_event{} = First) ->
    %% Group by case_id and extract transition sequences
    pqc_heuristics_miner:extract_traces(EventLog);
normalize_event_log([First | _] = EventLog) when is_list(First) ->
    %% Already list of traces
    EventLog.

%% @private Get default configuration for algorithm.
-spec default_algorithm_config(algorithm()) -> term().
default_algorithm_config(alpha) ->
    #{};
default_algorithm_config(heuristics) ->
    #heuristics_config{};
default_algorithm_config(_) ->
    #{}.

%% @private Evaluate model quality metrics.
-spec evaluate_model_internal(Net :: #swf_net{}, Traces :: [trace()]) -> quality_metrics().
evaluate_model_internal(Net, Traces) ->
    %% Convert net to alignment format
    AlignmentNet = convert_net_to_alignment_format(Net),

    %% Compute alignments
    Alignments = lists:filtermap(
        fun(Trace) ->
            case pqc_alignment:align(Trace, AlignmentNet) of
                {ok, Alignment} -> {true, Alignment};
                {error, _} -> false
            end
        end,
        Traces
    ),

    %% Compute metrics
    Fitness = pqc_alignment:fitness(Alignments, Traces),
    Precision = pqc_alignment:precision(AlignmentNet, Traces),
    Generalization = pqc_alignment:generalization(AlignmentNet, Traces),
    Simplicity = pqc_alignment:simplicity(AlignmentNet),
    FMeasure = pqc_alignment:f_measure(Fitness, Precision, 1.0),

    #quality_metrics{
        fitness = Fitness,
        precision = Precision,
        generalization = Generalization,
        simplicity = Simplicity,
        f_measure = FMeasure
    }.

%% @private Convert SwarmFlow net to alignment-compatible format.
-spec convert_net_to_alignment_format(Net :: #swf_net{}) -> map().
convert_net_to_alignment_format(Net) ->
    %% Convert transitions to alignment format
    Transitions = maps:map(
        fun(_TId, T) ->
            #{
                inputs => [],
                outputs => [],
                metadata => T#swf_transition.metadata
            }
        end,
        Net#swf_net.transitions
    ),

    #{
        transitions => Transitions,
        places => [],
        initial_marking => Net#swf_net.initial_marking
    }.

%% @private Extract deviations from alignment results.
-spec extract_deviations([{trace(), pqc_alignment:alignment()}]) -> [deviation()].
extract_deviations(AlignmentResults) ->
    lists:flatmap(
        fun({Trace, Alignment}) ->
            TraceId = base64:encode(crypto:strong_rand_bytes(8)),
            extract_deviations_from_alignment(TraceId, Trace, Alignment)
        end,
        AlignmentResults
    ).

%% @private Extract deviations from single alignment.
-spec extract_deviations_from_alignment(binary(), trace(), pqc_alignment:alignment()) ->
    [deviation()].
extract_deviations_from_alignment(_TraceId, _Trace, _Alignment) ->
    %% Simplified for now - would extract from alignment moves
    [].

%% @private Compute transition frequencies.
-spec compute_transition_frequencies([trace()]) -> #{atom() => non_neg_integer()}.
compute_transition_frequencies(Traces) ->
    AllEvents = lists:flatten(Traces),
    lists:foldl(
        fun(Event, Acc) ->
            maps:update_with(Event, fun(C) -> C + 1 end, 1, Acc)
        end,
        #{},
        AllEvents
    ).

%% @private Calculate relative frequency.
-spec calculate_relative_frequency(non_neg_integer(), [trace()]) -> float().
calculate_relative_frequency(Freq, Traces) ->
    TotalEvents = lists:sum([length(T) || T <- Traces]),
    case TotalEvents of
        0 -> 0.0;
        _ -> Freq / TotalEvents
    end.

%% @private Filter by time range.
-spec filter_by_time(event_log(), filter_spec()) -> event_log().
filter_by_time(EventLog, FilterSpec) ->
    case maps:get(time_range, FilterSpec, undefined) of
        undefined -> EventLog;
        {_Start, _End} ->
            %% Would filter swf_event records by timestamp
            EventLog
    end.

%% @private Filter by activities.
-spec filter_by_activities(event_log(), filter_spec()) -> event_log().
filter_by_activities(EventLog, FilterSpec) ->
    case maps:get(activities, FilterSpec, undefined) of
        undefined -> EventLog;
        Activities when is_list(Activities) ->
            [filter_trace_activities(T, Activities) || T <- EventLog]
    end.

%% @private Filter trace activities.
-spec filter_trace_activities(trace(), [atom()]) -> trace().
filter_trace_activities(Trace, Activities) when is_list(Trace) ->
    [A || A <- Trace, lists:member(A, Activities)];
filter_trace_activities(Event, _Activities) ->
    Event.

%% @private Filter by case IDs.
-spec filter_by_cases(event_log(), filter_spec()) -> event_log().
filter_by_cases(EventLog, FilterSpec) ->
    case maps:get(cases, FilterSpec, undefined) of
        undefined -> EventLog;
        _CaseIds ->
            %% Would filter swf_event records by case_id
            EventLog
    end.

%% @private Filter by frequency.
-spec filter_by_frequency(event_log(), filter_spec()) -> event_log().
filter_by_frequency(EventLog, FilterSpec) ->
    case {maps:get(min_frequency, FilterSpec, undefined),
          maps:get(max_frequency, FilterSpec, undefined)} of
        {undefined, undefined} -> EventLog;
        {MinFreq, MaxFreq} ->
            Traces = normalize_event_log(EventLog),
            Variants = compute_variants(Traces),
            VariantMap = maps:from_list(Variants),

            lists:filter(
                fun(Trace) when is_list(Trace) ->
                    Freq = maps:get(Trace, VariantMap, 0),
                    (MinFreq =:= undefined orelse Freq >= MinFreq) andalso
                    (MaxFreq =:= undefined orelse Freq =< MaxFreq);
                   (_) -> true
                end,
                EventLog
            )
    end.

%% @private Compute trace variants with frequencies.
-spec compute_variants([trace()]) -> [{trace(), non_neg_integer()}].
compute_variants(Traces) ->
    VariantMap = lists:foldl(
        fun(Trace, Acc) ->
            maps:update_with(Trace, fun(C) -> C + 1 end, 1, Acc)
        end,
        #{},
        Traces
    ),
    %% Sort by frequency descending
    lists:reverse(lists:keysort(2, maps:to_list(VariantMap))).

%% @private Calculate noise ratio.
-spec calculate_noise_ratio([{trace(), non_neg_integer()}], non_neg_integer()) -> float().
calculate_noise_ratio([], _TraceCount) ->
    0.0;
calculate_noise_ratio(Variants, TraceCount) ->
    %% Noise = variants that appear only once
    SingletonCount = length([V || {_V, Count} <- Variants, Count =:= 1]),
    case TraceCount of
        0 -> 0.0;
        _ -> SingletonCount / TraceCount
    end.

%% @private Count loop indicators (self-transitions).
-spec count_loop_indicators([trace()]) -> non_neg_integer().
count_loop_indicators(Traces) ->
    lists:sum([count_self_transitions(T) || T <- Traces]).

%% @private Count self-transitions in trace.
-spec count_self_transitions(trace()) -> non_neg_integer().
count_self_transitions([]) -> 0;
count_self_transitions([_]) -> 0;
count_self_transitions([A, A | Rest]) -> 1 + count_self_transitions([A | Rest]);
count_self_transitions([_A, B | Rest]) -> count_self_transitions([B | Rest]).

%%====================================================================
%% Export Functions
%%====================================================================

%% @private Export BPMN transitions.
-spec export_bpmn_transitions(#{binary() => #swf_transition{}}) -> iolist().
export_bpmn_transitions(Transitions) ->
    [
        [
            <<"    <task id=\"">>, TId, <<"\" name=\"">>, T#swf_transition.name, <<"\"/>\n">>
        ]
        || {TId, T} <- maps:to_list(Transitions)
    ].

%% @private Export PNML places.
-spec export_pnml_places(#{binary() => #swf_place{}}) -> iolist().
export_pnml_places(Places) ->
    [
        [
            <<"    <place id=\"">>, PId, <<"\">\n">>,
            <<"      <name><text>">>, P#swf_place.name, <<"</text></name>\n">>,
            case P#swf_place.tokens of
                0 -> [];
                N -> [<<"      <initialMarking><text>">>, integer_to_binary(N), <<"</text></initialMarking>\n">>]
            end,
            <<"    </place>\n">>
        ]
        || {PId, P} <- maps:to_list(Places)
    ].

%% @private Export PNML transitions.
-spec export_pnml_transitions(#{binary() => #swf_transition{}}) -> iolist().
export_pnml_transitions(Transitions) ->
    [
        [
            <<"    <transition id=\"">>, TId, <<"\">\n">>,
            <<"      <name><text>">>, T#swf_transition.name, <<"</text></name>\n">>,
            <<"    </transition>\n">>
        ]
        || {TId, T} <- maps:to_list(Transitions)
    ].

%% @private Export PNML arcs.
-spec export_pnml_arcs([#swf_arc{}]) -> iolist().
export_pnml_arcs(Arcs) ->
    [
        [
            <<"    <arc id=\"">>, A#swf_arc.id, <<"\" ">>,
            <<"source=\"">>, A#swf_arc.source, <<"\" ">>,
            <<"target=\"">>, A#swf_arc.target, <<"\">\n">>,
            case A#swf_arc.weight of
                1 -> [];
                W -> [<<"      <inscription><text>">>, integer_to_binary(W), <<"</text></inscription>\n">>]
            end,
            <<"    </arc>\n">>
        ]
        || A <- Arcs
    ].

%% @private Export as GraphViz DOT.
-spec export_dot(#swf_net{}) -> iolist().
export_dot(Net) ->
    [
        <<"digraph ">>, Net#swf_net.id, <<" {\n">>,
        <<"  label=\"">>, Net#swf_net.name, <<"\";\n">>,
        <<"  rankdir=LR;\n">>,
        export_dot_places(Net#swf_net.places),
        export_dot_transitions(Net#swf_net.transitions),
        export_dot_arcs(Net#swf_net.arcs),
        <<"}\n">>
    ].

%% @private Export DOT places.
-spec export_dot_places(#{binary() => #swf_place{}}) -> iolist().
export_dot_places(Places) ->
    [
        [
            <<"  ">>, PId, <<" [shape=circle,label=\"">>, P#swf_place.name, <<"\"];\n">>
        ]
        || {PId, P} <- maps:to_list(Places)
    ].

%% @private Export DOT transitions.
-spec export_dot_transitions(#{binary() => #swf_transition{}}) -> iolist().
export_dot_transitions(Transitions) ->
    [
        [
            <<"  ">>, TId, <<" [shape=box,label=\"">>, T#swf_transition.name, <<"\"];\n">>
        ]
        || {TId, T} <- maps:to_list(Transitions)
    ].

%% @private Export DOT arcs.
-spec export_dot_arcs([#swf_arc{}]) -> iolist().
export_dot_arcs(Arcs) ->
    [
        [
            <<"  ">>, A#swf_arc.source, <<" -> ">>, A#swf_arc.target, <<";\n">>
        ]
        || A <- Arcs
    ].

%% @private Export as Mermaid diagram.
-spec export_mermaid(#swf_net{}) -> iolist().
export_mermaid(Net) ->
    [
        <<"graph LR\n">>,
        [
            [<<"  ">>, TId, <<"[">>, T#swf_transition.name, <<"]\n">>]
            || {TId, T} <- maps:to_list(Net#swf_net.transitions)
        ],
        [
            [<<"  ">>, A#swf_arc.source, <<" --> ">>, A#swf_arc.target, <<"\n">>]
            || A <- Net#swf_net.arcs
        ]
    ].

%% @private Export as PlantUML activity diagram.
-spec export_plantuml(#swf_net{}) -> iolist().
export_plantuml(Net) ->
    [
        <<"@startuml\n">>,
        <<"title ">>, Net#swf_net.name, <<"\n">>,
        [
            [<<":">>, T#swf_transition.name, <<";\n">>]
            || {_TId, T} <- maps:to_list(Net#swf_net.transitions)
        ],
        <<"@enduml\n">>
    ].

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test default options
default_options_test() ->
    Options = default_options(),
    ?assertEqual(auto, maps:get(algorithm, Options)),
    ?assertEqual(true, maps:get(evaluate, Options)),
    ?assertEqual(60000, maps:get(timeout_ms, Options)).

%% Test algorithm selection
select_algorithm_clean_log_test() ->
    %% Small, clean log
    Traces = [[a, b, c], [a, b, c], [a, b, c]],
    ?assertEqual(alpha, select_algorithm(Traces)).

select_algorithm_noisy_log_test() ->
    %% Noisy log with many variants
    Traces = [[a, b, c], [a, c, b], [b, a, c], [c, b, a],
              [a, b], [b, c], [a, c], [a], [b], [c]],
    ?assertEqual(heuristics, select_algorithm(Traces)).

%% Test log statistics
compute_log_statistics_test() ->
    Traces = [[a, b, c], [a, b, c], [a, c, b]],
    Stats = compute_log_statistics(Traces),
    ?assertEqual(3, Stats#log_statistics.trace_count),
    ?assertEqual(9, Stats#log_statistics.event_count),
    ?assertEqual(3, Stats#log_statistics.activity_count),
    ?assertEqual(2, Stats#log_statistics.variant_count).

%% Test filter by activities
filter_by_activities_test() ->
    Traces = [[a, b, c], [a, c, b]],
    FilterSpec = #{activities => [a, b]},
    Filtered = filter_by_activities(Traces, FilterSpec),
    ?assertEqual([[a, b], [a, b]], Filtered).

-endif.
