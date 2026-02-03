%%%-------------------------------------------------------------------
%%% @doc PQC Heuristics Miner - Noise-Tolerant Process Discovery
%%%
%%% Implements Van der Aalst's Heuristics Miner algorithm (Weijters et al., 2006)
%%% for discovering process models from noisy event logs.
%%%
%%% The Heuristics Miner is more robust than Alpha Miner, handling:
%%% - Noise and infrequent behavior
%%% - Incomplete event logs
%%% - Non-fitting traces
%%% - AND/XOR split/join detection
%%% - Length-1 and length-2 loop detection
%%%
%%% Algorithm Overview:
%%% 1. Build dependency graph from event log
%%% 2. Calculate dependency measures between activities
%%% 3. Filter dependencies based on thresholds
%%% 4. Detect split/join types (AND vs XOR)
%%% 5. Detect loops (length-1 and length-2)
%%% 6. Construct causal net (C-net)
%%% 7. Convert to Petri net
%%%
%%% Dependency Measure Formula:
%%% dependency(a,b) = (|a >_L b| - |b >_L a|) / (|a >_L b| + |b >_L a| + 1)
%%% where a >_L b means "a directly followed by b" in the event log
%%% Returns value in [-1, 1]
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_heuristics_miner).

-include("pqchain.hrl").
-include_lib("swarmflow_os/include/swarmflow.hrl").

%% API exports
-export([
    discover/1,
    discover/2,
    dependency_graph/1,
    dependency_graph/2,
    dependency_measure/3,
    split_mining/2,
    join_mining/2,
    detect_loops/2,
    to_causal_net/1,
    to_petri_net/1
]).

%% Internal exports for testing
-export([
    extract_traces/1,
    build_frequency_counts/1,
    filter_dependencies/2,
    detect_and_splits/2,
    detect_xor_splits/2
]).

%%====================================================================
%% Type definitions
%%====================================================================

-record(dependency_graph, {
    activities :: ordsets:ordset(atom()),
    dependencies :: #{atom() => #{atom() => float()}},  % a => {b => strength}
    start_activities :: #{atom() => non_neg_integer()},
    end_activities :: #{atom() => non_neg_integer()}
}).

-record(heuristics_config, {
    dependency_threshold = 0.9 :: float(),           % Minimum dependency strength
    positive_observations = 1 :: pos_integer(),       % Minimum occurrences
    relative_to_best = 0.05 :: float(),              % Relative strength to best
    and_threshold = 0.1 :: float(),                  % Co-occurrence threshold for AND
    loop_length_one = 0.9 :: float(),                % L1 loop threshold
    loop_length_two = 0.9 :: float()                 % L2 loop threshold
}).

-record(causal_net, {
    activities :: ordsets:ordset(atom()),
    input_bindings :: #{atom() => [{split_type(), [atom()]}]},
    output_bindings :: #{atom() => [{join_type(), [atom()]}]},
    start_activities :: ordsets:ordset(atom()),
    end_activities :: ordsets:ordset(atom()),
    loops :: [{atom(), loop_type()}]
}).

-type split_type() :: and | xor.
-type join_type() :: and | xor.
-type loop_type() :: length_one | {length_two, atom()}.
-type trace() :: [atom()].
-type event_log() :: [#swf_event{}] | [trace()].
-type frequency_counts() :: #{
    direct_succession => #{{atom(), atom()} => non_neg_integer()},
    causality => #{{atom(), atom()} => non_neg_integer()},
    parallel => #{{atom(), atom()} => non_neg_integer()},
    start => #{atom() => non_neg_integer()},
    end => #{atom() => non_neg_integer()}
}.

-export_type([
    dependency_graph/0,
    heuristics_config/0,
    causal_net/0,
    split_type/0,
    join_type/0,
    loop_type/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Discover process model from event log with default configuration.
%% Accepts either a list of swf_event records or a list of traces (activity lists).
-spec discover(EventLog :: event_log()) -> {ok, #swf_net{}} | {error, term()}.
discover(EventLog) ->
    discover(EventLog, #heuristics_config{}).

%% @doc Discover process model from event log with custom configuration.
-spec discover(EventLog :: event_log(), Config :: #heuristics_config{}) ->
    {ok, #swf_net{}} | {error, term()}.
discover(EventLog, Config) when is_list(EventLog), is_record(Config, heuristics_config) ->
    try
        %% Step 1: Build dependency graph
        DepGraph = dependency_graph(EventLog, Config),

        %% Step 2: Detect loops
        Loops = detect_loops(EventLog, Config),

        %% Step 3: Convert to causal net
        CNet = to_causal_net(DepGraph),

        %% Step 4: Add loops to causal net
        CNetWithLoops = CNet#causal_net{loops = Loops},

        %% Step 5: Convert to Petri net
        PetriNet = to_petri_net(CNetWithLoops),

        {ok, PetriNet}
    catch
        error:Reason:Stacktrace ->
            {error, {discovery_failed, Reason, Stacktrace}}
    end.

%% @doc Build dependency graph from event log with default config.
-spec dependency_graph(EventLog :: event_log()) -> #dependency_graph{}.
dependency_graph(EventLog) ->
    dependency_graph(EventLog, #heuristics_config{}).

%% @doc Build dependency graph from event log with custom config.
-spec dependency_graph(EventLog :: event_log(), Config :: #heuristics_config{}) ->
    #dependency_graph{}.
dependency_graph(EventLog, Config) when is_list(EventLog) ->
    %% Extract traces from event log
    Traces = extract_traces(EventLog),

    %% Build frequency counts
    Counts = build_frequency_counts(Traces),

    %% Extract activities
    DirectSuccession = maps:get(direct_succession, Counts),
    Activities = ordsets:from_list(
        lists:usort([A || {A, _B} <- maps:keys(DirectSuccession)] ++
                    [B || {_A, B} <- maps:keys(DirectSuccession)])
    ),

    %% Calculate dependency measures
    Dependencies = calculate_dependencies(Activities, Counts),

    %% Filter based on thresholds
    FilteredDeps = filter_dependencies(Dependencies, Config),

    %% Extract start and end activities
    StartActivities = maps:get(start, Counts),
    EndActivities = maps:get(end, Counts),

    #dependency_graph{
        activities = Activities,
        dependencies = FilteredDeps,
        start_activities = StartActivities,
        end_activities = EndActivities
    }.

%% @doc Calculate dependency measure between two activities.
%% Formula: (|a >_L b| - |b >_L a|) / (|a >_L b| + |b >_L a| + 1)
%% Returns value in [-1, 1]
-spec dependency_measure(A :: atom(), B :: atom(), Counts :: frequency_counts()) -> float().
dependency_measure(A, B, Counts) ->
    DirectSuccession = maps:get(direct_succession, Counts),

    %% Get frequencies
    AB_Freq = maps:get({A, B}, DirectSuccession, 0),
    BA_Freq = maps:get({B, A}, DirectSuccession, 0),

    %% Calculate dependency measure
    Numerator = AB_Freq - BA_Freq,
    Denominator = AB_Freq + BA_Freq + 1,

    Numerator / Denominator.

%% @doc Determine split type (AND/XOR) for outgoing edges of an activity.
-spec split_mining(DepGraph :: #dependency_graph{}, Activity :: atom()) ->
    {split_type(), [atom()]}.
split_mining(#dependency_graph{dependencies = Deps}, Activity) ->
    %% Get outgoing dependencies
    Outgoing = maps:get(Activity, Deps, #{}),
    Successors = maps:keys(Outgoing),

    case length(Successors) of
        0 -> {xor, []};
        1 -> {xor, Successors};
        _ ->
            %% Check co-occurrence patterns
            %% If successors frequently occur together -> AND
            %% If successors are mutually exclusive -> XOR
            case are_concurrent(Successors, Deps) of
                true -> {and, Successors};
                false -> {xor, Successors}
            end
    end.

%% @doc Determine join type (AND/XOR) for incoming edges of an activity.
-spec join_mining(DepGraph :: #dependency_graph{}, Activity :: atom()) ->
    {join_type(), [atom()]}.
join_mining(#dependency_graph{dependencies = Deps}, Activity) ->
    %% Get incoming dependencies
    Predecessors = [Source || {Source, Targets} <- maps:to_list(Deps),
                              maps:is_key(Activity, Targets)],

    case length(Predecessors) of
        0 -> {xor, []};
        1 -> {xor, Predecessors};
        _ ->
            %% Check co-occurrence patterns
            case are_concurrent(Predecessors, Deps) of
                true -> {and, Predecessors};
                false -> {xor, Predecessors}
            end
    end.

%% @doc Detect length-1 and length-2 loops in the event log.
-spec detect_loops(EventLog :: event_log(), Config :: #heuristics_config{}) ->
    [{atom(), loop_type()}].
detect_loops(EventLog, Config) ->
    Traces = extract_traces(EventLog),
    Counts = build_frequency_counts(Traces),

    L1Threshold = Config#heuristics_config.loop_length_one,
    L2Threshold = Config#heuristics_config.loop_length_two,

    %% Detect length-1 loops (a >_L a)
    L1Loops = detect_length_one_loops(Counts, L1Threshold),

    %% Detect length-2 loops (a >_L b >_L a)
    L2Loops = detect_length_two_loops(Counts, L2Threshold),

    L1Loops ++ L2Loops.

%% @doc Convert dependency graph to causal net representation.
-spec to_causal_net(DepGraph :: #dependency_graph{}) -> #causal_net{}.
to_causal_net(#dependency_graph{
    activities = Activities,
    dependencies = Deps,
    start_activities = StartActs,
    end_activities = EndActs
}) ->
    %% Build input and output bindings for each activity
    InputBindings = maps:from_list([
        {Act, [join_mining(#dependency_graph{dependencies = Deps}, Act)]}
        || Act <- ordsets:to_list(Activities)
    ]),

    OutputBindings = maps:from_list([
        {Act, [split_mining(#dependency_graph{dependencies = Deps}, Act)]}
        || Act <- ordsets:to_list(Activities)
    ]),

    %% Convert start/end activities to ordsets
    StartSet = ordsets:from_list(maps:keys(StartActs)),
    EndSet = ordsets:from_list(maps:keys(EndActs)),

    #causal_net{
        activities = Activities,
        input_bindings = InputBindings,
        output_bindings = OutputBindings,
        start_activities = StartSet,
        end_activities = EndSet,
        loops = []
    }.

%% @doc Convert causal net to Petri net (swf_net).
-spec to_petri_net(CNet :: #causal_net{}) -> #swf_net{}.
to_petri_net(#causal_net{
    activities = Activities,
    input_bindings = InputBindings,
    output_bindings = OutputBindings,
    start_activities = StartActs,
    end_activities = EndActs,
    loops = Loops
}) ->
    NetId = base64:encode(crypto:strong_rand_bytes(16)),

    %% Create transitions for each activity
    Transitions = maps:from_list([
        {atom_to_binary(Act, utf8),
         #swf_transition{
             id = atom_to_binary(Act, utf8),
             name = atom_to_binary(Act, utf8),
             kind = automatic,
             guard = undefined,
             action = undefined,
             timeout_ms = undefined,
             priority = 0,
             metadata = #{}
         }}
        || Act <- ordsets:to_list(Activities)
    ]),

    %% Create places and arcs based on input/output bindings
    {Places, Arcs} = create_places_and_arcs(
        Activities,
        InputBindings,
        OutputBindings,
        StartActs,
        EndActs,
        Loops
    ),

    %% Create initial marking (start place has one token)
    InitialMarking = #{<<"start">> => 1},

    %% Create final places list
    FinalPlaces = [<<"end">>],

    #swf_net{
        id = NetId,
        name = <<"Heuristics Miner Result">>,
        version = <<"1.0.0">>,
        places = Places,
        transitions = Transitions,
        arcs = Arcs,
        initial_marking = InitialMarking,
        final_places = FinalPlaces,
        metadata = #{
            algorithm => heuristics_miner,
            discovered_at => erlang:system_time(millisecond)
        }
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Extract traces from event log.
%% If already list of traces, return as-is.
%% If list of swf_event records, group by case_id and extract transition sequences.
-spec extract_traces(EventLog :: event_log()) -> [trace()].
extract_traces([]) ->
    [];
extract_traces([First | _] = EventLog) when is_record(First, swf_event) ->
    %% Group events by case_id
    Grouped = lists:foldl(
        fun(#swf_event{case_id = CaseId, event_type = EventType,
                       transition_id = TransId, sequence = Seq}, Acc) ->
            case EventType of
                transition_fired when TransId =/= undefined ->
                    Key = CaseId,
                    Events = maps:get(Key, Acc, []),
                    maps:put(Key, [{Seq, binary_to_atom(TransId, utf8)} | Events], Acc);
                _ ->
                    Acc
            end
        end,
        #{},
        EventLog
    ),

    %% Sort each case's events by sequence and extract activities
    [lists:map(fun({_Seq, Act}) -> Act end,
               lists:sort(Events))
     || Events <- maps:values(Grouped)];

extract_traces([First | _] = EventLog) when is_list(First) ->
    %% Already a list of traces
    EventLog.

%% @doc Build frequency counts for direct succession, causality, parallelism.
-spec build_frequency_counts(Traces :: [trace()]) -> frequency_counts().
build_frequency_counts(Traces) ->
    %% Initialize counters
    InitCounts = #{
        direct_succession => #{},
        causality => #{},
        parallel => #{},
        start => #{},
        end => #{}
    },

    %% Process each trace
    lists:foldl(fun process_trace/2, InitCounts, Traces).

%% @doc Process a single trace and update frequency counts.
-spec process_trace(Trace :: trace(), Counts :: frequency_counts()) -> frequency_counts().
process_trace([], Counts) ->
    Counts;
process_trace([Single], Counts) ->
    %% Single activity trace - it's both start and end
    update_start_count(Single,
        update_end_count(Single, Counts));
process_trace([First | Rest], Counts) ->
    %% Update start activity
    Counts1 = update_start_count(First, Counts),

    %% Process pairs for direct succession
    Counts2 = process_pairs([First | Rest], Counts1),

    %% Update end activity
    Last = lists:last(Rest),
    update_end_count(Last, Counts2).

%% @doc Process consecutive pairs in trace for direct succession.
-spec process_pairs(Trace :: trace(), Counts :: frequency_counts()) -> frequency_counts().
process_pairs([], Counts) ->
    Counts;
process_pairs([_], Counts) ->
    Counts;
process_pairs([A, B | Rest], Counts) ->
    Counts1 = update_direct_succession(A, B, Counts),
    process_pairs([B | Rest], Counts1).

%% @doc Update direct succession count.
-spec update_direct_succession(A :: atom(), B :: atom(), Counts :: frequency_counts()) ->
    frequency_counts().
update_direct_succession(A, B, Counts) ->
    DS = maps:get(direct_succession, Counts),
    Key = {A, B},
    NewCount = maps:get(Key, DS, 0) + 1,
    NewDS = maps:put(Key, NewCount, DS),
    maps:put(direct_succession, NewDS, Counts).

%% @doc Update start activity count.
-spec update_start_count(Activity :: atom(), Counts :: frequency_counts()) ->
    frequency_counts().
update_start_count(Activity, Counts) ->
    Start = maps:get(start, Counts),
    NewCount = maps:get(Activity, Start, 0) + 1,
    NewStart = maps:put(Activity, NewCount, Start),
    maps:put(start, NewStart, Counts).

%% @doc Update end activity count.
-spec update_end_count(Activity :: atom(), Counts :: frequency_counts()) ->
    frequency_counts().
update_end_count(Activity, Counts) ->
    End = maps:get(end, Counts),
    NewCount = maps:get(Activity, End, 0) + 1,
    NewEnd = maps:put(Activity, NewCount, End),
    maps:put(end, NewEnd, Counts).

%% @doc Calculate dependency measures for all activity pairs.
-spec calculate_dependencies(Activities :: ordsets:ordset(atom()),
                             Counts :: frequency_counts()) ->
    #{atom() => #{atom() => float()}}.
calculate_dependencies(Activities, Counts) ->
    ActList = ordsets:to_list(Activities),

    maps:from_list([
        {A, calculate_outgoing_dependencies(A, ActList, Counts)}
        || A <- ActList
    ]).

%% @doc Calculate outgoing dependencies for a single activity.
-spec calculate_outgoing_dependencies(Activity :: atom(), AllActivities :: [atom()],
                                      Counts :: frequency_counts()) ->
    #{atom() => float()}.
calculate_outgoing_dependencies(Activity, AllActivities, Counts) ->
    OutgoingDeps = [
        {B, dependency_measure(Activity, B, Counts)}
        || B <- AllActivities,
           B =/= Activity
    ],

    %% Only keep positive dependencies
    maps:from_list([{B, Dep} || {B, Dep} <- OutgoingDeps, Dep > 0]).

%% @doc Filter dependencies based on configuration thresholds.
-spec filter_dependencies(Dependencies :: #{atom() => #{atom() => float()}},
                          Config :: #heuristics_config{}) ->
    #{atom() => #{atom() => float()}}.
filter_dependencies(Dependencies, Config) ->
    DepThreshold = Config#heuristics_config.dependency_threshold,
    RelativeToBest = Config#heuristics_config.relative_to_best,

    maps:from_list([
        {A, filter_outgoing(Deps, DepThreshold, RelativeToBest)}
        || {A, Deps} <- maps:to_list(Dependencies)
    ]).

%% @doc Filter outgoing dependencies for a single activity.
-spec filter_outgoing(Dependencies :: #{atom() => float()}, Threshold :: float(),
                      RelativeToBest :: float()) ->
    #{atom() => float()}.
filter_outgoing(Dependencies, Threshold, RelativeToBest) ->
    case maps:size(Dependencies) of
        0 ->
            #{};
        _ ->
            %% Find best (maximum) dependency
            Best = lists:max(maps:values(Dependencies)),
            MinAllowed = max(Threshold, Best - RelativeToBest),

            %% Filter dependencies
            maps:filter(
                fun(_B, Dep) -> Dep >= MinAllowed end,
                Dependencies
            )
    end.

%% @doc Check if activities are concurrent (co-occur frequently).
-spec are_concurrent(Activities :: [atom()], Dependencies :: #{atom() => #{atom() => float()}}) ->
    boolean().
are_concurrent([], _Dependencies) ->
    false;
are_concurrent([_], _Dependencies) ->
    false;
are_concurrent(Activities, Dependencies) ->
    %% Check if activities have weak or no dependencies between them
    %% indicating they can occur in parallel
    Pairs = [{A, B} || A <- Activities, B <- Activities, A < B],

    ConcurrentCount = lists:foldl(
        fun({A, B}, Count) ->
            ADeps = maps:get(A, Dependencies, #{}),
            BDeps = maps:get(B, Dependencies, #{}),

            AtoB = maps:get(B, ADeps, 0.0),
            BtoA = maps:get(A, BDeps, 0.0),

            %% If both dependencies are weak (< 0.5), they're likely concurrent
            case (AtoB < 0.5) andalso (BtoA < 0.5) of
                true -> Count + 1;
                false -> Count
            end
        end,
        0,
        Pairs
    ),

    %% If more than half the pairs are concurrent, treat as AND
    ConcurrentCount > (length(Pairs) div 2).

%% @doc Detect length-1 loops (a >_L a).
-spec detect_length_one_loops(Counts :: frequency_counts(), Threshold :: float()) ->
    [{atom(), loop_type()}].
detect_length_one_loops(Counts, Threshold) ->
    DirectSuccession = maps:get(direct_succession, Counts),

    %% Find all self-loops
    SelfLoops = [
        {A, maps:get({A, A}, DirectSuccession, 0)}
        || A <- lists:usort([X || {X, Y} <- maps:keys(DirectSuccession), X =:= Y])
    ],

    %% Filter by threshold
    [
        {A, length_one}
        || {A, Count} <- SelfLoops,
           Count > 0,
           calculate_loop_measure(A, A, Counts) >= Threshold
    ].

%% @doc Detect length-2 loops (a >_L b >_L a).
-spec detect_length_two_loops(Counts :: frequency_counts(), Threshold :: float()) ->
    [{atom(), loop_type()}].
detect_length_two_loops(Counts, Threshold) ->
    DirectSuccession = maps:get(direct_succession, Counts),

    %% Find all potential length-2 loops
    Pairs = [
        {{A, B}, {maps:get({A, B}, DirectSuccession, 0),
                  maps:get({B, A}, DirectSuccession, 0)}}
        || {A, B} <- maps:keys(DirectSuccession),
           A =/= B
    ],

    %% Filter pairs where both directions exist and meet threshold
    L2Loops = [
        {A, {length_two, B}}
        || {{A, B}, {AB_Count, BA_Count}} <- Pairs,
           AB_Count > 0,
           BA_Count > 0,
           calculate_loop_measure(A, B, Counts) >= Threshold,
           calculate_loop_measure(B, A, Counts) >= Threshold
    ],

    %% Remove duplicates (keep only one direction)
    lists:usort(L2Loops).

%% @doc Calculate loop measure for potential loop.
-spec calculate_loop_measure(A :: atom(), B :: atom(), Counts :: frequency_counts()) -> float().
calculate_loop_measure(A, B, Counts) ->
    DirectSuccession = maps:get(direct_succession, Counts),

    AB_Count = maps:get({A, B}, DirectSuccession, 0),

    case AB_Count of
        0 -> 0.0;
        _ ->
            %% Simple measure: frequency of the loop
            %% Could be enhanced with more sophisticated metrics
            min(1.0, AB_Count / 10.0)
    end.

%% @doc Create places and arcs for Petri net from causal net bindings.
-spec create_places_and_arcs(
    Activities :: ordsets:ordset(atom()),
    InputBindings :: #{atom() => [{join_type(), [atom()]}]},
    OutputBindings :: #{atom() => [{split_type(), [atom()]}]},
    StartActs :: ordsets:ordset(atom()),
    EndActs :: ordsets:ordset(atom()),
    Loops :: [{atom(), loop_type()}]
) -> {#{binary() => #swf_place{}}, [#swf_arc{}]}.
create_places_and_arcs(Activities, InputBindings, OutputBindings,
                       StartActs, EndActs, _Loops) ->
    %% Create initial and final places
    StartPlace = #swf_place{
        id = <<"start">>,
        name = <<"Start">>,
        tokens = 1,
        capacity = infinity,
        metadata = #{}
    },

    EndPlace = #swf_place{
        id = <<"end">>,
        name = <<"End">>,
        tokens = 0,
        capacity = infinity,
        metadata = #{}
    },

    %% Create intermediate places for each dependency
    {IntermediatePlaces, Arcs} = create_intermediate_places_and_arcs(
        ordsets:to_list(Activities),
        InputBindings,
        OutputBindings
    ),

    %% Connect start place to start activities
    StartArcs = [
        #swf_arc{
            id = base64:encode(crypto:strong_rand_bytes(8)),
            source = <<"start">>,
            target = atom_to_binary(Act, utf8),
            weight = 1,
            kind = normal,
            expression = undefined
        }
        || Act <- ordsets:to_list(StartActs)
    ],

    %% Connect end activities to end place
    EndArcs = [
        #swf_arc{
            id = base64:encode(crypto:strong_rand_bytes(8)),
            source = atom_to_binary(Act, utf8),
            target = <<"end">>,
            weight = 1,
            kind = normal,
            expression = undefined
        }
        || Act <- ordsets:to_list(EndActs)
    ],

    AllPlaces = maps:merge(
        #{<<"start">> => StartPlace, <<"end">> => EndPlace},
        IntermediatePlaces
    ),

    AllArcs = StartArcs ++ EndArcs ++ Arcs,

    {AllPlaces, AllArcs}.

%% @doc Create intermediate places and arcs between transitions.
-spec create_intermediate_places_and_arcs(
    Activities :: [atom()],
    InputBindings :: #{atom() => [{join_type(), [atom()]}]},
    OutputBindings :: #{atom() => [{split_type(), [atom()]}]}
) -> {#{binary() => #swf_place{}}, [#swf_arc{}]}.
create_intermediate_places_and_arcs(Activities, _InputBindings, OutputBindings) ->
    %% For each activity, create places for its outgoing arcs
    lists:foldl(
        fun(Activity, {PlacesAcc, ArcsAcc}) ->
            case maps:get(Activity, OutputBindings, []) of
                [] ->
                    {PlacesAcc, ArcsAcc};
                [{_SplitType, Successors}] ->
                    %% Create a place for each outgoing edge
                    {NewPlaces, NewArcs} = lists:foldl(
                        fun(Successor, {PAcc, AAcc}) ->
                            PlaceId = iolist_to_binary([
                                atom_to_binary(Activity, utf8),
                                <<"_to_">>,
                                atom_to_binary(Successor, utf8)
                            ]),

                            Place = #swf_place{
                                id = PlaceId,
                                name = PlaceId,
                                tokens = 0,
                                capacity = infinity,
                                metadata = #{}
                            },

                            %% Arc from activity to place
                            Arc1 = #swf_arc{
                                id = base64:encode(crypto:strong_rand_bytes(8)),
                                source = atom_to_binary(Activity, utf8),
                                target = PlaceId,
                                weight = 1,
                                kind = normal,
                                expression = undefined
                            },

                            %% Arc from place to successor
                            Arc2 = #swf_arc{
                                id = base64:encode(crypto:strong_rand_bytes(8)),
                                source = PlaceId,
                                target = atom_to_binary(Successor, utf8),
                                weight = 1,
                                kind = normal,
                                expression = undefined
                            },

                            {maps:put(PlaceId, Place, PAcc), [Arc1, Arc2 | AAcc]}
                        end,
                        {PlacesAcc, ArcsAcc},
                        Successors
                    ),
                    {NewPlaces, NewArcs}
            end
        end,
        {#{}, []},
        Activities
    ).

%% @doc Detect AND splits based on co-occurrence patterns.
-spec detect_and_splits(DepGraph :: #dependency_graph{}, Activity :: atom()) -> [atom()].
detect_and_splits(DepGraph, Activity) ->
    case split_mining(DepGraph, Activity) of
        {and, Successors} -> Successors;
        _ -> []
    end.

%% @doc Detect XOR splits based on mutual exclusion patterns.
-spec detect_xor_splits(DepGraph :: #dependency_graph{}, Activity :: atom()) -> [atom()].
detect_xor_splits(DepGraph, Activity) ->
    case split_mining(DepGraph, Activity) of
        {xor, Successors} -> Successors;
        _ -> []
    end.
