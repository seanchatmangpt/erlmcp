%%%-------------------------------------------------------------------
%%% @doc pqc_alpha_miner unit tests
%%%
%%% Tests for the Alpha Miner process discovery algorithm.
%%% Uses Chicago School TDD (real processes, no mocks).
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pqc_alpha_miner_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pqchain.hrl").

%%====================================================================
%% Test fixtures
%%====================================================================

%% Simple sequential log: a -> b -> c
simple_sequential_log() ->
    [[a, b, c], [a, b, c], [a, b, c]].

%% Parallel log: a -> (b || c) -> d
parallel_log() ->
    [[a, b, c, d], [a, c, b, d]].

%% Choice log: a -> (b XOR c) -> d
choice_log() ->
    [[a, b, d], [a, c, d]].

%% Complex log with parallel and choice
complex_log() ->
    [
        [a, b, c, d],
        [a, c, b, d],
        [a, e, d],
        [a, b, c, d],
        [a, c, b, d]
    ].

%%====================================================================
%% Basic function tests
%%====================================================================

all_activities_test() ->
    Log = [[a, b, c], [a, c], [b]],
    Activities = pqc_alpha_miner:all_activities(Log),
    ?assertEqual(ordsets:from_list([a, b, c]), Activities).

start_activities_test() ->
    Log = [[a, b, c], [a, c], [b, d]],
    Starts = pqc_alpha_miner:start_activities(Log),
    ?assertEqual(ordsets:from_list([a, b]), Starts).

end_activities_test() ->
    Log = [[a, b, c], [a, c], [b, d]],
    Ends = pqc_alpha_miner:end_activities(Log),
    ?assertEqual(ordsets:from_list([c, d]), Ends).

direct_succession_test() ->
    Log = [[a, b, c], [a, c]],
    DirectSucc = pqc_alpha_miner:direct_succession(Log),
    Expected = ordsets:from_list([{a, b}, {b, c}, {a, c}]),
    ?assertEqual(Expected, DirectSucc).

%%====================================================================
%% Footprint tests
%%====================================================================

footprint_simple_sequential_test() ->
    Log = simple_sequential_log(),
    Footprint = pqc_alpha_miner:footprint(Log),

    %% Check activities
    ?assertEqual(ordsets:from_list([a, b, c]), Footprint#alpha_footprint.activities),

    %% Check direct succession: a > b, b > c
    ?assert(ordsets:is_element({a, b}, Footprint#alpha_footprint.direct_succession)),
    ?assert(ordsets:is_element({b, c}, Footprint#alpha_footprint.direct_succession)),

    %% Check causality: a -> b, b -> c (no reverse)
    ?assert(ordsets:is_element({a, b}, Footprint#alpha_footprint.causality)),
    ?assert(ordsets:is_element({b, c}, Footprint#alpha_footprint.causality)),

    %% Check no parallel relations
    ?assertEqual(ordsets:new(), Footprint#alpha_footprint.parallel),

    %% Check choice: a # c (a and c are not directly related)
    ?assert(ordsets:is_element({a, c}, Footprint#alpha_footprint.choice)),

    %% Check start and end
    ?assertEqual(ordsets:from_list([a]), Footprint#alpha_footprint.start_activities),
    ?assertEqual(ordsets:from_list([c]), Footprint#alpha_footprint.end_activities).

footprint_parallel_test() ->
    Log = parallel_log(),
    Footprint = pqc_alpha_miner:footprint(Log),

    %% Check activities
    ?assertEqual(ordsets:from_list([a, b, c, d]), Footprint#alpha_footprint.activities),

    %% Check parallel: b || c (both orders appear)
    ?assert(ordsets:is_element({b, c}, Footprint#alpha_footprint.parallel)),

    %% Check causality: a -> b, a -> c, b -> d, c -> d
    ?assert(ordsets:is_element({a, b}, Footprint#alpha_footprint.causality)),
    ?assert(ordsets:is_element({a, c}, Footprint#alpha_footprint.causality)),
    ?assert(ordsets:is_element({b, d}, Footprint#alpha_footprint.causality)),
    ?assert(ordsets:is_element({c, d}, Footprint#alpha_footprint.causality)),

    %% b and c should not be in causality (they're parallel)
    ?assertNot(ordsets:is_element({b, c}, Footprint#alpha_footprint.causality)),
    ?assertNot(ordsets:is_element({c, b}, Footprint#alpha_footprint.causality)).

footprint_choice_test() ->
    Log = choice_log(),
    Footprint = pqc_alpha_miner:footprint(Log),

    %% Check activities
    ?assertEqual(ordsets:from_list([a, b, c, d]), Footprint#alpha_footprint.activities),

    %% Check choice: b # c (they never appear together)
    ?assert(ordsets:is_element({b, c}, Footprint#alpha_footprint.choice)),

    %% Check causality
    ?assert(ordsets:is_element({a, b}, Footprint#alpha_footprint.causality)),
    ?assert(ordsets:is_element({a, c}, Footprint#alpha_footprint.causality)),
    ?assert(ordsets:is_element({b, d}, Footprint#alpha_footprint.causality)),
    ?assert(ordsets:is_element({c, d}, Footprint#alpha_footprint.causality)),

    %% No parallel relations
    ?assertEqual(ordsets:new(), Footprint#alpha_footprint.parallel).

%%====================================================================
%% Place computation tests
%%====================================================================

compute_places_sequential_test() ->
    Log = simple_sequential_log(),
    Footprint = pqc_alpha_miner:footprint(Log),
    Places = pqc_alpha_miner:compute_places(Footprint),

    %% Should find places: ({a}, {b}), ({b}, {c})
    ?assert(length(Places) >= 2),

    %% Check that expected places are present
    ?assert(lists:member({ordsets:from_list([a]), ordsets:from_list([b])}, Places)),
    ?assert(lists:member({ordsets:from_list([b]), ordsets:from_list([c])}, Places)).

compute_places_parallel_test() ->
    Log = parallel_log(),
    Footprint = pqc_alpha_miner:footprint(Log),
    Places = pqc_alpha_miner:compute_places(Footprint),

    %% Should find: ({a}, {b,c}), ({b,c}, {d})
    %% because b and c are parallel (mutually exclusive in choice relation)
    ?assert(length(Places) >= 1),

    %% At least one place should have inputs from a and outputs to both b and c
    HasParallelSplit = lists:any(
        fun({Inputs, Outputs}) ->
            ordsets:is_element(a, Inputs) andalso
            ordsets:is_element(b, Outputs) andalso
            ordsets:is_element(c, Outputs)
        end,
        Places
    ),
    ?assert(HasParallelSplit).

compute_places_choice_test() ->
    Log = choice_log(),
    Footprint = pqc_alpha_miner:footprint(Log),
    Places = pqc_alpha_miner:compute_places(Footprint),

    %% Should find places showing XOR split from a to {b,c}
    %% and XOR join from {b,c} to d
    ?assert(length(Places) >= 2).

%%====================================================================
%% Full discovery tests
%%====================================================================

discover_empty_log_test() ->
    ?assertEqual({error, <<"empty_event_log">>}, pqc_alpha_miner:discover([])).

discover_invalid_format_test() ->
    ?assertMatch({error, <<"invalid_event_log_format">>}, pqc_alpha_miner:discover(not_a_list)).

discover_simple_sequential_test() ->
    Log = simple_sequential_log(),
    {ok, Net} = pqc_alpha_miner:discover(Log),

    %% Verify net structure
    ?assertMatch(#swf_net{}, Net),
    ?assertEqual(<<"Alpha Miner Discovered Net">>, Net#swf_net.name),

    %% Should have source place (i_L) and sink place (o_L)
    ?assert(maps:is_key(<<"i_L">>, Net#swf_net.places)),
    ?assert(maps:is_key(<<"o_L">>, Net#swf_net.places)),

    %% Should have transitions for a, b, c
    ?assert(maps:is_key(<<"a">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"b">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"c">>, Net#swf_net.transitions)),

    %% Should have initial marking with token in source
    ?assertEqual(#{<<"i_L">> => 1}, Net#swf_net.initial_marking),

    %% Should have sink as final place
    ?assertEqual([<<"o_L">>], Net#swf_net.final_places),

    %% Verify arcs exist
    ?assert(length(Net#swf_net.arcs) > 0),

    %% Verify metadata
    Metadata = Net#swf_net.metadata,
    ?assertEqual(<<"alpha_miner">>, maps:get(algorithm, Metadata)),
    ?assert(maps:get(transition_count, Metadata) >= 3).

discover_parallel_test() ->
    Log = parallel_log(),
    {ok, Net} = pqc_alpha_miner:discover(Log),

    %% Should discover parallel structure
    ?assertMatch(#swf_net{}, Net),

    %% Should have all activities as transitions
    ?assert(maps:is_key(<<"a">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"b">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"c">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"d">>, Net#swf_net.transitions)),

    %% Should have multiple places (source, sink, internal)
    ?assert(maps:size(Net#swf_net.places) >= 4).

discover_choice_test() ->
    Log = choice_log(),
    {ok, Net} = pqc_alpha_miner:discover(Log),

    %% Should discover XOR choice structure
    ?assertMatch(#swf_net{}, Net),

    %% Should have all activities
    ?assertEqual(4, maps:size(Net#swf_net.transitions)).

discover_complex_test() ->
    Log = complex_log(),
    {ok, Net} = pqc_alpha_miner:discover(Log),

    %% Should handle complex structure with both parallel and choice
    ?assertMatch(#swf_net{}, Net),
    ?assert(maps:size(Net#swf_net.places) >= 3),
    ?assert(maps:size(Net#swf_net.transitions) >= 4).

%%====================================================================
%% Footprint matrix tests
%%====================================================================

footprint_to_matrix_test() ->
    Log = simple_sequential_log(),
    Footprint = pqc_alpha_miner:footprint(Log),
    Matrix = pqc_alpha_miner:footprint_to_matrix(Footprint),

    %% Should have activities list
    ?assert(maps:is_key(activities, Matrix)),
    Activities = maps:get(activities, Matrix),
    ?assertEqual([a, b, c], Activities),

    %% Should have matrix
    ?assert(maps:is_key(matrix, Matrix)),
    MatrixMap = maps:get(matrix, Matrix),

    %% Check some relations
    ?assertEqual(causality, maps:get(b, maps:get(a, MatrixMap))),
    ?assertEqual(causality, maps:get(c, maps:get(b, MatrixMap))),
    ?assertEqual(choice, maps:get(c, maps:get(a, MatrixMap))).

%%====================================================================
%% Footprint comparison tests
%%====================================================================

compare_footprints_identical_test() ->
    Log = simple_sequential_log(),
    Footprint = pqc_alpha_miner:footprint(Log),
    Similarity = pqc_alpha_miner:compare_footprints(Footprint, Footprint),
    ?assertEqual(1.0, Similarity).

compare_footprints_different_test() ->
    Log1 = simple_sequential_log(),
    Log2 = parallel_log(),
    Footprint1 = pqc_alpha_miner:footprint(Log1),
    Footprint2 = pqc_alpha_miner:footprint(Log2),
    Similarity = pqc_alpha_miner:compare_footprints(Footprint1, Footprint2),
    ?assert(Similarity >= 0.0 andalso Similarity =< 1.0),
    ?assert(Similarity < 1.0).  % Should not be identical

compare_footprints_no_overlap_test() ->
    Log1 = [[a, b, c]],
    Log2 = [[x, y, z]],
    Footprint1 = pqc_alpha_miner:footprint(Log1),
    Footprint2 = pqc_alpha_miner:footprint(Log2),
    Similarity = pqc_alpha_miner:compare_footprints(Footprint1, Footprint2),
    ?assertEqual(0.0, Similarity).

%%====================================================================
%% Property-based tests (basic sanity checks)
%%====================================================================

discover_always_has_source_sink_test() ->
    Logs = [
        simple_sequential_log(),
        parallel_log(),
        choice_log(),
        complex_log()
    ],

    lists:foreach(
        fun(Log) ->
            {ok, Net} = pqc_alpha_miner:discover(Log),
            ?assert(maps:is_key(<<"i_L">>, Net#swf_net.places)),
            ?assert(maps:is_key(<<"o_L">>, Net#swf_net.places))
        end,
        Logs
    ).

discover_transitions_match_activities_test() ->
    Logs = [
        simple_sequential_log(),
        parallel_log(),
        choice_log(),
        complex_log()
    ],

    lists:foreach(
        fun(Log) ->
            {ok, Net} = pqc_alpha_miner:discover(Log),
            Activities = pqc_alpha_miner:all_activities(Log),
            TransitionIds = maps:keys(Net#swf_net.transitions),
            ActivitiesBin = [atom_to_binary(A, utf8) || A <- ordsets:to_list(Activities)],
            ?assertEqual(lists:sort(ActivitiesBin), lists:sort(TransitionIds))
        end,
        Logs
    ).

%%====================================================================
%% Edge case tests
%%====================================================================

single_activity_log_test() ->
    Log = [[a], [a]],
    {ok, Net} = pqc_alpha_miner:discover(Log),

    %% Should have source, sink, and transition for 'a'
    ?assert(maps:is_key(<<"a">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"i_L">>, Net#swf_net.places)),
    ?assert(maps:is_key(<<"o_L">>, Net#swf_net.places)).

single_trace_log_test() ->
    Log = [[a, b, c]],
    {ok, Net} = pqc_alpha_miner:discover(Log),

    %% Should still discover sequential net
    ?assertEqual(3, maps:size(Net#swf_net.transitions)).

binary_activities_test() ->
    Log = [[<<"start">>, <<"middle">>, <<"end">>]],
    {ok, Net} = pqc_alpha_miner:discover(Log),

    %% Should work with binary activities
    ?assert(maps:is_key(<<"start">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"middle">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"end">>, Net#swf_net.transitions)).

mixed_atom_binary_activities_test() ->
    Log = [[a, <<"b">>, c]],
    {ok, Net} = pqc_alpha_miner:discover(Log),

    %% Should handle mixed atom/binary
    ?assert(maps:is_key(<<"a">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"b">>, Net#swf_net.transitions)),
    ?assert(maps:is_key(<<"c">>, Net#swf_net.transitions)).
