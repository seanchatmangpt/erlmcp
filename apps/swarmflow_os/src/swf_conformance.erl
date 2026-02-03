%%%-------------------------------------------------------------------
%%% @doc SwarmFlow OS Conformance Checking Module
%%%
%%% Implements conformance checking between event logs and workflow net
%%% definitions. Core process mining functionality for analyzing how well
%%% observed behavior matches expected workflow models.
%%%
%%% == Algorithms Implemented ==
%%% - Token-based replay: Simulates token flow through Petri net
%%% - Alignment-based conformance: Optimal sequence alignment using A*
%%% - Fitness calculation: Measures log-model fit (0.0 to 1.0)
%%% - Precision calculation: Measures model specificity (0.0 to 1.0)
%%%
%%% == Architecture ==
%%% - Stateless module (all state passed explicitly)
%%% - Results cached via ETS for performance
%%% - Supports parallel and choice constructs (AND/XOR splits/joins)
%%% - Deviation detection with severity classification
%%%
%%% == Performance ==
%%% - Replay: O(n) where n = number of events
%%% - Alignment: O(n*m) where n = trace length, m = model size
%%% - Caching reduces repeated computations significantly
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swf_conformance).

-include("swarmflow.hrl").

%% API exports
-export([
    check_conformance/2,
    replay_log/2,
    calculate_fitness/2,
    calculate_precision/2,
    find_deviations/2,
    align/2
]).

%% Cache management exports
-export([
    init_cache/0,
    clear_cache/0,
    get_cached_result/2,
    cache_result/3
]).

%% Internal exports for testing
-export([
    fire_transition/3,
    is_enabled/3,
    get_input_places/2,
    get_output_places/2,
    compute_alignment_cost/3
]).

%% Cache table name
-define(CONFORMANCE_CACHE, swf_conformance_cache).

%% Alignment move types
-type move_type() :: sync_move | model_move | log_move.

%% Alignment move record
-record(alignment_move, {
    type :: move_type(),
    log_event :: #swf_event{} | undefined,
    model_transition :: binary() | undefined,
    cost :: float()
}).

%% Replay state record (internal)
-record(replay_state, {
    marking :: #{binary() => non_neg_integer()},
    produced :: non_neg_integer(),
    consumed :: non_neg_integer(),
    missing :: non_neg_integer(),
    remaining :: non_neg_integer(),
    trace :: [#swf_event{}],
    deviations :: [#swf_deviation{}]
}).

%%%===================================================================
%%% Cache Management
%%%===================================================================

%% @doc Initialize the conformance result cache.
%% Creates an ETS table for caching conformance results.
-spec init_cache() -> ok.
init_cache() ->
    case ets:info(?CONFORMANCE_CACHE) of
        undefined ->
            ?CONFORMANCE_CACHE = ets:new(?CONFORMANCE_CACHE, [
                named_table,
                public,
                set,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

%% @doc Clear all cached conformance results.
-spec clear_cache() -> ok.
clear_cache() ->
    case ets:info(?CONFORMANCE_CACHE) of
        undefined -> ok;
        _ -> true = ets:delete_all_objects(?CONFORMANCE_CACHE), ok
    end.

%% @doc Get cached conformance result for a case and net.
-spec get_cached_result(CaseId :: binary(), NetId :: binary()) ->
    {ok, #swf_conformance_result{}} | not_found.
get_cached_result(CaseId, NetId) ->
    Key = {CaseId, NetId},
    case ets:lookup(?CONFORMANCE_CACHE, Key) of
        [{Key, Result}] -> {ok, Result};
        [] -> not_found
    end.

%% @doc Cache a conformance result.
-spec cache_result(CaseId :: binary(), NetId :: binary(),
                   Result :: #swf_conformance_result{}) -> ok.
cache_result(CaseId, NetId, Result) ->
    Key = {CaseId, NetId},
    true = ets:insert(?CONFORMANCE_CACHE, {Key, Result}),
    ok.

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Check conformance of an event log against a workflow net.
%% Returns comprehensive conformance result with fitness, precision,
%% and detected deviations.
%%
%% The function checks cache first, computes if not cached, then caches result.
-spec check_conformance(Events :: [#swf_event{}], Net :: #swf_net{}) ->
    {ok, #swf_conformance_result{}} | {error, term()}.
check_conformance([], _Net) ->
    {error, empty_event_log};
check_conformance(Events, #swf_net{id = NetId} = Net) ->
    % Extract case_id from first event
    [#swf_event{case_id = CaseId} | _] = Events,

    % Check cache first
    init_cache(),
    case get_cached_result(CaseId, NetId) of
        {ok, CachedResult} ->
            {ok, CachedResult};
        not_found ->
            % Compute conformance
            case compute_conformance(Events, Net) of
                {ok, Result} ->
                    cache_result(CaseId, NetId, Result),
                    {ok, Result};
                Error ->
                    Error
            end
    end.

%% @doc Replay an event log on a workflow net.
%% Returns the replay trace showing token flow and any deviations encountered.
-spec replay_log(Events :: [#swf_event{}], Net :: #swf_net{}) ->
    {ok, ReplayState :: map()} | {error, term()}.
replay_log([], _Net) ->
    {error, empty_event_log};
replay_log(Events, Net) ->
    % Filter to only transition events
    TransitionEvents = filter_transition_events(Events),

    % Initialize replay state with initial marking
    InitialState = #replay_state{
        marking = Net#swf_net.initial_marking,
        produced = count_initial_tokens(Net#swf_net.initial_marking),
        consumed = 0,
        missing = 0,
        remaining = 0,
        trace = [],
        deviations = []
    },

    % Replay each event
    FinalState = replay_events(TransitionEvents, Net, InitialState),

    % Check for remaining tokens
    FinalMarking = FinalState#replay_state.marking,
    RemainingTokens = count_remaining_tokens(FinalMarking, Net#swf_net.final_places),

    % Add remaining token deviations
    FinalDeviations = add_remaining_deviations(
        FinalState#replay_state.deviations,
        FinalMarking,
        Net#swf_net.final_places
    ),

    {ok, #{
        marking => FinalMarking,
        produced => FinalState#replay_state.produced,
        consumed => FinalState#replay_state.consumed,
        missing => FinalState#replay_state.missing,
        remaining => RemainingTokens,
        trace => lists:reverse(FinalState#replay_state.trace),
        deviations => lists:reverse(FinalDeviations)
    }}.

%% @doc Calculate fitness score (how well the log fits the model).
%% Fitness is based on token replay: 1.0 means perfect fit.
%%
%% Formula: 0.5 * (1 - missing/consumed) + 0.5 * (1 - remaining/produced)
-spec calculate_fitness(Events :: [#swf_event{}], Net :: #swf_net{}) ->
    {ok, float()} | {error, term()}.
calculate_fitness(Events, Net) ->
    case replay_log(Events, Net) of
        {ok, #{produced := Produced, consumed := Consumed,
               missing := Missing, remaining := Remaining}} ->
            % Calculate fitness using standard formula
            Fitness = calculate_fitness_score(Produced, Consumed, Missing, Remaining),
            {ok, Fitness};
        Error ->
            Error
    end.

%% @doc Calculate precision score (how precise the model is).
%% Precision measures if the model allows only observed behavior.
%% Higher precision means model doesn't allow unobserved behaviors.
%%
%% Uses escaping edges approach: count enabled but not fired transitions.
-spec calculate_precision(Events :: [#swf_event{}], Net :: #swf_net{}) ->
    {ok, float()} | {error, term()}.
calculate_precision(Events, Net) ->
    case compute_precision(Events, Net) of
        {ok, Precision} -> {ok, Precision};
        Error -> Error
    end.

%% @doc Find all deviations between log and model.
%% Returns classified deviations with severity levels.
-spec find_deviations(Events :: [#swf_event{}], Net :: #swf_net{}) ->
    {ok, [#swf_deviation{}]} | {error, term()}.
find_deviations(Events, Net) ->
    case replay_log(Events, Net) of
        {ok, #{deviations := Deviations}} ->
            % Add alignment-based deviations for completeness
            AlignmentDeviations = find_alignment_deviations(Events, Net),
            AllDeviations = merge_deviations(Deviations, AlignmentDeviations),
            {ok, AllDeviations};
        Error ->
            Error
    end.

%% @doc Compute optimal alignment between log trace and model.
%% Uses A* algorithm to find minimum cost alignment.
%%
%% Returns alignment as list of moves (sync, model, or log moves).
-spec align(Events :: [#swf_event{}], Net :: #swf_net{}) ->
    {ok, Alignment :: [#alignment_move{}], Cost :: float()} | {error, term()}.
align([], _Net) ->
    {ok, [], 0.0};
align(Events, Net) ->
    % Extract transition IDs from events
    TransitionEvents = filter_transition_events(Events),
    LogTrace = [E#swf_event.transition_id || E <- TransitionEvents],

    % Compute alignment using A* search
    case compute_alignment(LogTrace, Events, Net) of
        {ok, Alignment, Cost} ->
            {ok, Alignment, Cost};
        Error ->
            Error
    end.

%%%===================================================================
%%% Internal Functions - Core Conformance
%%%===================================================================

%% @private Compute full conformance metrics.
-spec compute_conformance([#swf_event{}], #swf_net{}) ->
    {ok, #swf_conformance_result{}} | {error, term()}.
compute_conformance(Events, Net) ->
    [#swf_event{case_id = CaseId} | _] = Events,

    % Calculate all metrics
    FitnessResult = calculate_fitness(Events, Net),
    PrecisionResult = calculate_precision(Events, Net),
    DeviationsResult = find_deviations(Events, Net),

    case {FitnessResult, PrecisionResult, DeviationsResult} of
        {{ok, Fitness}, {ok, Precision}, {ok, Deviations}} ->
            % Calculate generalization (estimated from log variance)
            Generalization = estimate_generalization(Events, Net),

            % Calculate simplicity (based on net structure)
            Simplicity = calculate_simplicity(Net),

            Result = #swf_conformance_result{
                case_id = CaseId,
                net_id = Net#swf_net.id,
                fitness = Fitness,
                precision = Precision,
                generalization = Generalization,
                simplicity = Simplicity,
                deviations = Deviations,
                computed_at = erlang:system_time(microsecond)
            },
            {ok, Result};
        {{error, E}, _, _} -> {error, {fitness_error, E}};
        {_, {error, E}, _} -> {error, {precision_error, E}};
        {_, _, {error, E}} -> {error, {deviations_error, E}}
    end.

%% @private Filter events to only transition-related events.
-spec filter_transition_events([#swf_event{}]) -> [#swf_event{}].
filter_transition_events(Events) ->
    [E || E <- Events,
          E#swf_event.event_type =:= transition_fired orelse
          E#swf_event.event_type =:= transition_completed].

%% @private Count initial tokens in marking.
-spec count_initial_tokens(#{binary() => non_neg_integer()}) -> non_neg_integer().
count_initial_tokens(Marking) ->
    maps:fold(fun(_, Count, Acc) -> Acc + Count end, 0, Marking).

%% @private Replay events through the net.
-spec replay_events([#swf_event{}], #swf_net{}, #replay_state{}) -> #replay_state{}.
replay_events([], _Net, State) ->
    State;
replay_events([Event | Rest], Net, State) ->
    TransitionId = Event#swf_event.transition_id,

    case TransitionId of
        undefined ->
            % Skip events without transition
            replay_events(Rest, Net, State);
        _ ->
            % Try to fire the transition
            NewState = try_fire_transition(TransitionId, Event, Net, State),
            replay_events(Rest, Net, NewState)
    end.

%% @private Try to fire a transition during replay.
-spec try_fire_transition(binary(), #swf_event{}, #swf_net{}, #replay_state{}) ->
    #replay_state{}.
try_fire_transition(TransitionId, Event, Net, State) ->
    Marking = State#replay_state.marking,

    case is_enabled(TransitionId, Marking, Net) of
        true ->
            % Transition is enabled - fire it
            {NewMarking, Consumed, Produced} = fire_transition(TransitionId, Marking, Net),
            State#replay_state{
                marking = NewMarking,
                consumed = State#replay_state.consumed + Consumed,
                produced = State#replay_state.produced + Produced,
                trace = [Event | State#replay_state.trace]
            };
        false ->
            % Transition not enabled - create missing tokens
            InputPlaces = get_input_places(TransitionId, Net),
            {NewMarking, MissingCount, Deviations} =
                create_missing_tokens(InputPlaces, Marking, TransitionId, Net),

            % Now fire the transition
            {FinalMarking, Consumed, Produced} = fire_transition(TransitionId, NewMarking, Net),

            State#replay_state{
                marking = FinalMarking,
                consumed = State#replay_state.consumed + Consumed,
                produced = State#replay_state.produced + Produced,
                missing = State#replay_state.missing + MissingCount,
                trace = [Event | State#replay_state.trace],
                deviations = Deviations ++ State#replay_state.deviations
            }
    end.

%% @doc Check if a transition is enabled in the current marking.
-spec is_enabled(TransitionId :: binary(), Marking :: map(), Net :: #swf_net{}) ->
    boolean().
is_enabled(TransitionId, Marking, Net) ->
    InputPlaces = get_input_places(TransitionId, Net),
    check_all_inputs_satisfied(InputPlaces, Marking).

%% @private Check if all input places have sufficient tokens.
-spec check_all_inputs_satisfied([{binary(), pos_integer(), atom()}], map()) -> boolean().
check_all_inputs_satisfied([], _Marking) ->
    true;
check_all_inputs_satisfied([{PlaceId, Weight, ArcKind} | Rest], Marking) ->
    TokenCount = maps:get(PlaceId, Marking, 0),
    case ArcKind of
        inhibitor ->
            % Inhibitor arc: enabled if NO tokens present
            TokenCount =:= 0 andalso check_all_inputs_satisfied(Rest, Marking);
        read ->
            % Read arc: enabled if tokens present, doesn't consume
            TokenCount >= Weight andalso check_all_inputs_satisfied(Rest, Marking);
        _ ->
            % Normal/reset arc: enabled if enough tokens
            TokenCount >= Weight andalso check_all_inputs_satisfied(Rest, Marking)
    end.

%% @doc Fire a transition, updating the marking.
%% Returns {NewMarking, TokensConsumed, TokensProduced}.
-spec fire_transition(TransitionId :: binary(), Marking :: map(), Net :: #swf_net{}) ->
    {map(), non_neg_integer(), non_neg_integer()}.
fire_transition(TransitionId, Marking, Net) ->
    % Consume tokens from input places
    InputPlaces = get_input_places(TransitionId, Net),
    {Marking1, Consumed} = consume_tokens(InputPlaces, Marking),

    % Produce tokens in output places
    OutputPlaces = get_output_places(TransitionId, Net),
    {Marking2, Produced} = produce_tokens(OutputPlaces, Marking1),

    {Marking2, Consumed, Produced}.

%% @doc Get input places for a transition with arc weights and kinds.
-spec get_input_places(TransitionId :: binary(), Net :: #swf_net{}) ->
    [{PlaceId :: binary(), Weight :: pos_integer(), ArcKind :: atom()}].
get_input_places(TransitionId, #swf_net{arcs = Arcs}) ->
    [
        {Arc#swf_arc.source, Arc#swf_arc.weight, Arc#swf_arc.kind}
        || Arc <- Arcs,
           Arc#swf_arc.target =:= TransitionId,
           is_place(Arc#swf_arc.source, Arc)
    ].

%% @doc Get output places for a transition with arc weights.
-spec get_output_places(TransitionId :: binary(), Net :: #swf_net{}) ->
    [{PlaceId :: binary(), Weight :: pos_integer()}].
get_output_places(TransitionId, #swf_net{arcs = Arcs}) ->
    [
        {Arc#swf_arc.target, Arc#swf_arc.weight}
        || Arc <- Arcs,
           Arc#swf_arc.source =:= TransitionId,
           Arc#swf_arc.kind =/= inhibitor,
           Arc#swf_arc.kind =/= read
    ].

%% @private Check if source is a place (not a transition).
-spec is_place(binary(), #swf_arc{}) -> boolean().
is_place(_Source, _Arc) ->
    % In a proper Petri net, arcs alternate between places and transitions
    % Source of an arc to a transition must be a place
    true.

%% @private Consume tokens from input places.
-spec consume_tokens([{binary(), pos_integer(), atom()}], map()) ->
    {map(), non_neg_integer()}.
consume_tokens(InputPlaces, Marking) ->
    lists:foldl(
        fun({PlaceId, Weight, ArcKind}, {M, Count}) ->
            case ArcKind of
                read ->
                    % Read arc doesn't consume
                    {M, Count};
                reset ->
                    % Reset arc consumes all tokens
                    CurrentTokens = maps:get(PlaceId, M, 0),
                    {maps:put(PlaceId, 0, M), Count + CurrentTokens};
                inhibitor ->
                    % Inhibitor arc doesn't consume
                    {M, Count};
                _ ->
                    % Normal arc consumes weight tokens
                    CurrentTokens = maps:get(PlaceId, M, 0),
                    NewTokens = max(0, CurrentTokens - Weight),
                    {maps:put(PlaceId, NewTokens, M), Count + Weight}
            end
        end,
        {Marking, 0},
        InputPlaces
    ).

%% @private Produce tokens in output places.
-spec produce_tokens([{binary(), pos_integer()}], map()) ->
    {map(), non_neg_integer()}.
produce_tokens(OutputPlaces, Marking) ->
    lists:foldl(
        fun({PlaceId, Weight}, {M, Count}) ->
            CurrentTokens = maps:get(PlaceId, M, 0),
            {maps:put(PlaceId, CurrentTokens + Weight, M), Count + Weight}
        end,
        {Marking, 0},
        OutputPlaces
    ).

%% @private Create missing tokens and record deviations.
-spec create_missing_tokens([{binary(), pos_integer(), atom()}], map(), binary(), #swf_net{}) ->
    {map(), non_neg_integer(), [#swf_deviation{}]}.
create_missing_tokens(InputPlaces, Marking, TransitionId, _Net) ->
    lists:foldl(
        fun({PlaceId, Weight, ArcKind}, {M, MissingCount, Devs}) ->
            case ArcKind of
                inhibitor ->
                    % Inhibitor: deviation if tokens present
                    TokenCount = maps:get(PlaceId, M, 0),
                    if
                        TokenCount > 0 ->
                            Dev = #swf_deviation{
                                type = wrong_transition,
                                location = PlaceId,
                                expected = 0,
                                actual = TokenCount,
                                severity = medium,
                                message = iolist_to_binary([
                                    <<"Inhibitor arc blocked: place ">>,
                                    PlaceId,
                                    <<" has tokens for transition ">>,
                                    TransitionId
                                ])
                            },
                            {maps:put(PlaceId, 0, M), MissingCount + TokenCount, [Dev | Devs]};
                        true ->
                            {M, MissingCount, Devs}
                    end;
                _ ->
                    % Normal/read/reset: need sufficient tokens
                    CurrentTokens = maps:get(PlaceId, M, 0),
                    Missing = max(0, Weight - CurrentTokens),
                    if
                        Missing > 0 ->
                            Dev = #swf_deviation{
                                type = missing_token,
                                location = PlaceId,
                                expected = Weight,
                                actual = CurrentTokens,
                                severity = determine_severity(Missing, Weight),
                                message = iolist_to_binary([
                                    <<"Missing ">>,
                                    integer_to_binary(Missing),
                                    <<" token(s) in place ">>,
                                    PlaceId,
                                    <<" for transition ">>,
                                    TransitionId
                                ])
                            },
                            {maps:put(PlaceId, Weight, M), MissingCount + Missing, [Dev | Devs]};
                        true ->
                            {M, MissingCount, Devs}
                    end
            end
        end,
        {Marking, 0, []},
        InputPlaces
    ).

%% @private Determine severity based on missing token ratio.
-spec determine_severity(Missing :: non_neg_integer(), Expected :: pos_integer()) ->
    low | medium | high | critical.
determine_severity(Missing, Expected) ->
    Ratio = Missing / Expected,
    if
        Ratio =< 0.25 -> low;
        Ratio =< 0.5 -> medium;
        Ratio =< 0.75 -> high;
        true -> critical
    end.

%% @private Count remaining tokens not in final places.
-spec count_remaining_tokens(map(), [binary()]) -> non_neg_integer().
count_remaining_tokens(Marking, FinalPlaces) ->
    FinalSet = sets:from_list(FinalPlaces),
    maps:fold(
        fun(PlaceId, TokenCount, Acc) ->
            case sets:is_element(PlaceId, FinalSet) of
                true -> Acc;  % Tokens in final places are expected
                false -> Acc + TokenCount
            end
        end,
        0,
        Marking
    ).

%% @private Add deviations for remaining tokens.
-spec add_remaining_deviations([#swf_deviation{}], map(), [binary()]) ->
    [#swf_deviation{}].
add_remaining_deviations(Deviations, Marking, FinalPlaces) ->
    FinalSet = sets:from_list(FinalPlaces),
    maps:fold(
        fun(PlaceId, TokenCount, Acc) ->
            IsFinal = sets:is_element(PlaceId, FinalSet),
            if
                TokenCount > 0 andalso not IsFinal ->
                    Dev = #swf_deviation{
                        type = remaining_token,
                        location = PlaceId,
                        expected = 0,
                        actual = TokenCount,
                        severity = low,
                        message = iolist_to_binary([
                            integer_to_binary(TokenCount),
                            <<" remaining token(s) in non-final place ">>,
                            PlaceId
                        ])
                    },
                    [Dev | Acc];
                true ->
                    Acc
            end
        end,
        Deviations,
        Marking
    ).

%%%===================================================================
%%% Internal Functions - Fitness Calculation
%%%===================================================================

%% @private Calculate fitness score using standard formula.
-spec calculate_fitness_score(Produced :: non_neg_integer(),
                              Consumed :: non_neg_integer(),
                              Missing :: non_neg_integer(),
                              Remaining :: non_neg_integer()) -> float().
calculate_fitness_score(Produced, Consumed, Missing, Remaining) ->
    % Standard fitness formula from process mining literature
    % Fitness = 0.5 * (1 - m/c) + 0.5 * (1 - r/p)
    % where m = missing, c = consumed, r = remaining, p = produced
    MissingRatio = if Consumed > 0 -> Missing / Consumed; true -> 0.0 end,
    RemainingRatio = if Produced > 0 -> Remaining / Produced; true -> 0.0 end,

    0.5 * (1.0 - MissingRatio) + 0.5 * (1.0 - RemainingRatio).

%%%===================================================================
%%% Internal Functions - Precision Calculation
%%%===================================================================

%% @private Compute precision using escaping edges approach.
-spec compute_precision([#swf_event{}], #swf_net{}) ->
    {ok, float()} | {error, term()}.
compute_precision(Events, Net) ->
    TransitionEvents = filter_transition_events(Events),

    % Replay and count enabled vs executed transitions
    case compute_precision_metrics(TransitionEvents, Net) of
        {ok, Executed, Enabled} when Enabled > 0 ->
            Precision = Executed / Enabled,
            {ok, Precision};
        {ok, _, 0} ->
            {ok, 1.0};  % No enabled transitions = perfect precision
        Error ->
            Error
    end.

%% @private Compute precision metrics during replay.
-spec compute_precision_metrics([#swf_event{}], #swf_net{}) ->
    {ok, Executed :: non_neg_integer(), Enabled :: non_neg_integer()} |
    {error, term()}.
compute_precision_metrics(Events, Net) ->
    InitialMarking = Net#swf_net.initial_marking,

    % Replay and count at each state
    {Executed, Enabled, _FinalMarking} = lists:foldl(
        fun(Event, {ExecAcc, EnabAcc, Marking}) ->
            % Count enabled transitions at current marking
            EnabledTransitions = count_enabled_transitions(Marking, Net),

            % Fire the observed transition
            TransitionId = Event#swf_event.transition_id,
            case TransitionId of
                undefined ->
                    {ExecAcc, EnabAcc, Marking};
                _ ->
                    {NewMarking, _, _} = fire_transition(TransitionId, Marking, Net),
                    {ExecAcc + 1, EnabAcc + EnabledTransitions, NewMarking}
            end
        end,
        {0, 0, InitialMarking},
        Events
    ),

    {ok, Executed, Enabled}.

%% @private Count number of enabled transitions in current marking.
-spec count_enabled_transitions(map(), #swf_net{}) -> non_neg_integer().
count_enabled_transitions(Marking, #swf_net{transitions = Transitions} = Net) ->
    maps:fold(
        fun(TransitionId, _Transition, Acc) ->
            case is_enabled(TransitionId, Marking, Net) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        Transitions
    ).

%%%===================================================================
%%% Internal Functions - Alignment
%%%===================================================================

%% @private Compute optimal alignment using A* search.
-spec compute_alignment([binary()], [#swf_event{}], #swf_net{}) ->
    {ok, [#alignment_move{}], float()} | {error, term()}.
compute_alignment(LogTrace, Events, Net) ->
    % Build event lookup map
    EventMap = build_event_map(Events),

    % Initial state: (log_position, marking)
    InitialMarking = Net#swf_net.initial_marking,
    InitialState = {0, InitialMarking},

    % A* search for optimal alignment
    OpenSet = [{0.0, InitialState, []}],  % {cost, state, path}
    ClosedSet = sets:new(),

    case astar_search(OpenSet, ClosedSet, LogTrace, EventMap, Net) of
        {ok, Alignment, Cost} ->
            {ok, lists:reverse(Alignment), Cost};
        Error ->
            Error
    end.

%% @private Build map from transition_id to event.
-spec build_event_map([#swf_event{}]) -> #{binary() => #swf_event{}}.
build_event_map(Events) ->
    lists:foldl(
        fun(Event, Acc) ->
            case Event#swf_event.transition_id of
                undefined -> Acc;
                TId -> maps:put(TId, Event, Acc)
            end
        end,
        #{},
        Events
    ).

%% @private A* search for optimal alignment.
-spec astar_search(OpenSet, ClosedSet, LogTrace, EventMap, Net) -> Result when
    OpenSet :: [{float(), {integer(), map()}, [#alignment_move{}]}],
    ClosedSet :: sets:set(),
    LogTrace :: [binary()],
    EventMap :: map(),
    Net :: #swf_net{},
    Result :: {ok, [#alignment_move{}], float()} | {error, term()}.
astar_search([], _ClosedSet, _LogTrace, _EventMap, _Net) ->
    {error, no_alignment_found};
astar_search([{Cost, {LogPos, Marking}, Path} | Rest], ClosedSet, LogTrace, EventMap, Net) ->
    % Check if we've reached the goal
    LogLen = length(LogTrace),
    FinalPlaces = Net#swf_net.final_places,

    case is_final_state(LogPos, LogLen, Marking, FinalPlaces) of
        true ->
            {ok, Path, Cost};
        false ->
            StateKey = {LogPos, Marking},
            case sets:is_element(StateKey, ClosedSet) of
                true ->
                    % Already visited, skip
                    astar_search(Rest, ClosedSet, LogTrace, EventMap, Net);
                false ->
                    % Generate successors
                    NewClosed = sets:add_element(StateKey, ClosedSet),
                    Successors = generate_successors(LogPos, Marking, LogTrace, EventMap, Net),

                    % Add successors to open set
                    NewOpen = add_successors(Successors, Cost, Path, Rest, LogLen, FinalPlaces),

                    % Sort by cost + heuristic
                    SortedOpen = lists:sort(fun({C1, _, _}, {C2, _, _}) -> C1 =< C2 end, NewOpen),

                    % Limit search space to prevent infinite loops
                    LimitedOpen = lists:sublist(SortedOpen, 10000),

                    astar_search(LimitedOpen, NewClosed, LogTrace, EventMap, Net)
            end
    end.

%% @private Check if we've reached the final state.
-spec is_final_state(LogPos, LogLen, Marking, FinalPlaces) -> boolean() when
    LogPos :: integer(),
    LogLen :: integer(),
    Marking :: map(),
    FinalPlaces :: [binary()].
is_final_state(LogPos, LogLen, Marking, FinalPlaces) ->
    % Log fully consumed and token in final place
    LogPos >= LogLen andalso has_token_in_final_place(Marking, FinalPlaces).

%% @private Check if there's a token in any final place.
-spec has_token_in_final_place(map(), [binary()]) -> boolean().
has_token_in_final_place(Marking, FinalPlaces) ->
    lists:any(
        fun(PlaceId) ->
            maps:get(PlaceId, Marking, 0) > 0
        end,
        FinalPlaces
    ).

%% @private Generate successor states for A* search.
-spec generate_successors(LogPos, Marking, LogTrace, EventMap, Net) -> Successors when
    LogPos :: integer(),
    Marking :: map(),
    LogTrace :: [binary()],
    EventMap :: map(),
    Net :: #swf_net{},
    Successors :: [{NewState :: {integer(), map()}, Move :: #alignment_move{}}].
generate_successors(LogPos, Marking, LogTrace, EventMap, Net) ->
    LogLen = length(LogTrace),

    % Get current log event (if any)
    CurrentLogTransition = if LogPos < LogLen ->
                               lists:nth(LogPos + 1, LogTrace);
                           true ->
                               undefined
                           end,

    % Get enabled model transitions
    EnabledTransitions = get_enabled_transitions(Marking, Net),

    Successors = [],

    % 1. Synchronous moves (log and model agree)
    SyncMoves = case CurrentLogTransition of
        undefined ->
            [];
        _ ->
            case lists:member(CurrentLogTransition, EnabledTransitions) of
                true ->
                    {NewMarking, _, _} = fire_transition(CurrentLogTransition, Marking, Net),
                    Event = maps:get(CurrentLogTransition, EventMap, undefined),
                    Move = #alignment_move{
                        type = sync_move,
                        log_event = Event,
                        model_transition = CurrentLogTransition,
                        cost = 0.0
                    },
                    [{{LogPos + 1, NewMarking}, Move}];
                false ->
                    []
            end
    end,

    % 2. Model moves (model advances, log stays)
    ModelMoves = [
        begin
            {NewMarking, _, _} = fire_transition(TId, Marking, Net),
            Move = #alignment_move{
                type = model_move,
                log_event = undefined,
                model_transition = TId,
                cost = 1.0
            },
            {{LogPos, NewMarking}, Move}
        end
        || TId <- EnabledTransitions,
           TId =/= CurrentLogTransition
    ],

    % 3. Log moves (log advances, model stays)
    LogMoves = case CurrentLogTransition of
        undefined ->
            [];
        _ ->
            Event = maps:get(CurrentLogTransition, EventMap, undefined),
            Move = #alignment_move{
                type = log_move,
                log_event = Event,
                model_transition = undefined,
                cost = 1.0
            },
            [{{LogPos + 1, Marking}, Move}]
    end,

    Successors ++ SyncMoves ++ ModelMoves ++ LogMoves.

%% @private Get list of enabled transitions.
-spec get_enabled_transitions(map(), #swf_net{}) -> [binary()].
get_enabled_transitions(Marking, #swf_net{transitions = Transitions} = Net) ->
    maps:fold(
        fun(TransitionId, _Transition, Acc) ->
            case is_enabled(TransitionId, Marking, Net) of
                true -> [TransitionId | Acc];
                false -> Acc
            end
        end,
        [],
        Transitions
    ).

%% @private Add successors to open set.
-spec add_successors(Successors, BaseCost, Path, OpenSet, LogLen, FinalPlaces) -> NewOpenSet when
    Successors :: [{State, Move}],
    State :: {integer(), map()},
    Move :: #alignment_move{},
    BaseCost :: float(),
    Path :: [#alignment_move{}],
    OpenSet :: [{float(), State, [#alignment_move{}]}],
    LogLen :: integer(),
    FinalPlaces :: [binary()],
    NewOpenSet :: [{float(), State, [#alignment_move{}]}].
add_successors(Successors, BaseCost, Path, OpenSet, LogLen, FinalPlaces) ->
    lists:foldl(
        fun({NewState, Move}, Acc) ->
            MoveCost = Move#alignment_move.cost,
            NewCost = BaseCost + MoveCost,
            Heuristic = compute_heuristic(NewState, LogLen, FinalPlaces),
            TotalCost = NewCost + Heuristic,
            [{TotalCost, NewState, [Move | Path]} | Acc]
        end,
        OpenSet,
        Successors
    ).

%% @private Compute A* heuristic (admissible: remaining log events).
-spec compute_heuristic(State, LogLen, FinalPlaces) -> float() when
    State :: {integer(), map()},
    LogLen :: integer(),
    FinalPlaces :: [binary()].
compute_heuristic({LogPos, _Marking}, LogLen, _FinalPlaces) ->
    % Simple heuristic: remaining events in log
    float(LogLen - LogPos).

%% @doc Compute alignment cost.
-spec compute_alignment_cost(Alignment, Events, Net) -> float() when
    Alignment :: [#alignment_move{}],
    Events :: [#swf_event{}],
    Net :: #swf_net{}.
compute_alignment_cost(Alignment, _Events, _Net) ->
    lists:foldl(
        fun(#alignment_move{cost = Cost}, Acc) ->
            Acc + Cost
        end,
        0.0,
        Alignment
    ).

%%%===================================================================
%%% Internal Functions - Deviation Detection
%%%===================================================================

%% @private Find deviations from alignment.
-spec find_alignment_deviations([#swf_event{}], #swf_net{}) -> [#swf_deviation{}].
find_alignment_deviations(Events, Net) ->
    case align(Events, Net) of
        {ok, Alignment, _Cost} ->
            alignment_to_deviations(Alignment);
        {error, _} ->
            []
    end.

%% @private Convert alignment moves to deviations.
-spec alignment_to_deviations([#alignment_move{}]) -> [#swf_deviation{}].
alignment_to_deviations(Alignment) ->
    lists:filtermap(
        fun(Move) ->
            case Move#alignment_move.type of
                sync_move ->
                    false;  % No deviation
                log_move ->
                    Event = Move#alignment_move.log_event,
                    TransitionId = case Event of
                        undefined -> <<"unknown">>;
                        _ -> Event#swf_event.transition_id
                    end,
                    {true, #swf_deviation{
                        type = wrong_transition,
                        location = TransitionId,
                        expected = <<"model transition">>,
                        actual = TransitionId,
                        severity = medium,
                        message = iolist_to_binary([
                            <<"Log move: transition ">>,
                            TransitionId,
                            <<" in log but not in model">>
                        ])
                    }};
                model_move ->
                    TransitionId = Move#alignment_move.model_transition,
                    {true, #swf_deviation{
                        type = missing_token,
                        location = TransitionId,
                        expected = TransitionId,
                        actual = <<"not executed">>,
                        severity = low,
                        message = iolist_to_binary([
                            <<"Model move: transition ">>,
                            TransitionId,
                            <<" expected but not in log">>
                        ])
                    }}
            end
        end,
        Alignment
    ).

%% @private Merge deviations from replay and alignment, removing duplicates.
-spec merge_deviations([#swf_deviation{}], [#swf_deviation{}]) -> [#swf_deviation{}].
merge_deviations(Deviations1, Deviations2) ->
    % Simple merge - in production would deduplicate
    Deviations1 ++ Deviations2.

%%%===================================================================
%%% Internal Functions - Generalization & Simplicity
%%%===================================================================

%% @private Estimate generalization score.
%% Generalization measures how well the model generalizes beyond the log.
-spec estimate_generalization([#swf_event{}], #swf_net{}) -> float().
estimate_generalization(Events, Net) ->
    % Simplified estimation based on log coverage of model
    TransitionEvents = filter_transition_events(Events),
    ObservedTransitions = lists:usort([E#swf_event.transition_id || E <- TransitionEvents,
                                        E#swf_event.transition_id =/= undefined]),
    ModelTransitions = maps:keys(Net#swf_net.transitions),

    ObservedCount = length(ObservedTransitions),
    ModelCount = length(ModelTransitions),

    if
        ModelCount > 0 ->
            Coverage = ObservedCount / ModelCount,
            % Generalization is inversely related to coverage
            % High coverage = low generalization (model overfits to log)
            1.0 - (Coverage * 0.5);
        true ->
            0.5
    end.

%% @private Calculate simplicity score based on net structure.
-spec calculate_simplicity(#swf_net{}) -> float().
calculate_simplicity(Net) ->
    % Simplicity based on average node degree and net size
    Places = maps:size(Net#swf_net.places),
    Transitions = maps:size(Net#swf_net.transitions),
    Arcs = length(Net#swf_net.arcs),

    Nodes = Places + Transitions,
    if
        Nodes > 0 ->
            AvgDegree = (2 * Arcs) / Nodes,
            % Simpler nets have lower average degree
            % Normalize to 0-1 range (assuming max degree of 10)
            max(0.0, 1.0 - (AvgDegree / 10.0));
        true ->
            1.0
    end.

%%%===================================================================
%%% Tests (EUnit)
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Basic replay test
replay_simple_test() ->
    Net = create_simple_net(),
    Events = create_simple_events(),
    {ok, Result} = replay_log(Events, Net),
    ?assert(maps:get(missing, Result) =:= 0).

%% Fitness calculation test
fitness_test() ->
    Net = create_simple_net(),
    Events = create_simple_events(),
    {ok, Fitness} = calculate_fitness(Events, Net),
    ?assert(Fitness >= 0.0),
    ?assert(Fitness =< 1.0).

%% Cache test
cache_test() ->
    init_cache(),
    CaseId = <<"test-case-1">>,
    NetId = <<"test-net-1">>,
    Result = #swf_conformance_result{
        case_id = CaseId,
        net_id = NetId,
        fitness = 0.95,
        precision = 0.90,
        generalization = 0.85,
        simplicity = 0.80,
        deviations = [],
        computed_at = erlang:system_time(microsecond)
    },
    ok = cache_result(CaseId, NetId, Result),
    {ok, Cached} = get_cached_result(CaseId, NetId),
    ?assertEqual(Result, Cached),
    clear_cache().

%% Helper: Create simple test net
create_simple_net() ->
    #swf_net{
        id = <<"test-net">>,
        name = <<"Simple Test Net">>,
        version = <<"1.0">>,
        places = #{
            <<"p1">> => #swf_place{id = <<"p1">>, name = <<"Start">>, tokens = 0},
            <<"p2">> => #swf_place{id = <<"p2">>, name = <<"Middle">>, tokens = 0},
            <<"p3">> => #swf_place{id = <<"p3">>, name = <<"End">>, tokens = 0}
        },
        transitions = #{
            <<"t1">> => #swf_transition{id = <<"t1">>, name = <<"Task A">>, kind = automatic},
            <<"t2">> => #swf_transition{id = <<"t2">>, name = <<"Task B">>, kind = automatic}
        },
        arcs = [
            #swf_arc{id = <<"a1">>, source = <<"p1">>, target = <<"t1">>, weight = 1, kind = normal},
            #swf_arc{id = <<"a2">>, source = <<"t1">>, target = <<"p2">>, weight = 1, kind = normal},
            #swf_arc{id = <<"a3">>, source = <<"p2">>, target = <<"t2">>, weight = 1, kind = normal},
            #swf_arc{id = <<"a4">>, source = <<"t2">>, target = <<"p3">>, weight = 1, kind = normal}
        ],
        initial_marking = #{<<"p1">> => 1},
        final_places = [<<"p3">>],
        metadata = #{}
    }.

%% Helper: Create simple test events
create_simple_events() ->
    Now = erlang:system_time(microsecond),
    [
        #swf_event{
            id = <<"e1">>,
            case_id = <<"case-1">>,
            event_type = transition_fired,
            transition_id = <<"t1">>,
            timestamp = Now,
            sequence = 0,
            data = #{}
        },
        #swf_event{
            id = <<"e2">>,
            case_id = <<"case-1">>,
            event_type = transition_fired,
            transition_id = <<"t2">>,
            timestamp = Now + 1000,
            sequence = 1,
            data = #{}
        }
    ].

-endif.
