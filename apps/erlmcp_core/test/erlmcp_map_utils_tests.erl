-module(erlmcp_map_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=====================================================================
%%% Test Data Generators
%%%=====================================================================

%% Generate test map with mixed prefixes
generate_test_map(Size) ->
    lists:foldl(fun(I, Acc) ->
        Prefix = case I rem 3 of
            0 -> <<"root_">>;
            1 -> <<"resource_">>;
            2 -> <<"other_">>
        end,
        Key = <<Prefix/binary, (integer_to_binary(I))/binary>>,
        maps:put(Key, I, Acc)
    end, #{}, lists:seq(1, Size)).

%%%=====================================================================
%%% Filter By Prefix Tests
%%%=====================================================================

filter_by_prefix_binary_test() ->
    Map = #{<<"root_1">> => a, <<"root_2">> => b, <<"other">> => c},
    Result = erlmcp_map_utils:filter_by_prefix(Map, <<"root_">>),
    ?assertEqual(#{<<"root_1">> => a, <<"root_2">> => b}, Result).

filter_by_prefix_atom_test() ->
    Map = #{<<"root_1">> => a, <<"root_2">> => b, <<"other">> => c},
    Result = erlmcp_map_utils:filter_by_prefix(Map, root_),
    ?assertEqual(#{<<"root_1">> => a, <<"root_2">> => b}, Result).

filter_by_prefix_empty_test() ->
    Map = #{<<"a">> => 1, <<"b">> => 2},
    Result = erlmcp_map_utils:filter_by_prefix(Map, <<"x">>),
    ?assertEqual(#{}, Result).

filter_by_prefix_large_map_test() ->
    Map = generate_test_map(1000),
    Result = erlmcp_map_utils:filter_by_prefix(Map, <<"root_">>),
    ?assertEqual(334, maps:size(Result)),  % ~1/3 of entries
    lists:foreach(fun(K) ->
        ?assertMatch(<<"root_", _/binary>>, K)
    end, maps:keys(Result)).

%%%=====================================================================
%%% Put Nested Tests
%%%=====================================================================

put_nested_single_test() ->
    Map = #{},
    Result = erlmcp_map_utils:put_nested(Map, [a], 1),
    ?assertEqual(#{a => 1}, Result).

put_nested_deep_test() ->
    Map = #{},
    Result = erlmcp_map_utils:put_nested(Map, [a, b, c], 42),
    ?assertEqual(#{a => #{b => #{c => 42}}}, Result).

put_nested_update_test() ->
    Map = #{a => #{b => #{c => 1}}},
    Result = erlmcp_map_utils:put_nested(Map, [a, b, c], 2),
    ?assertEqual(#{a => #{b => #{c => 2}}}, Result).

put_nested_create_intermediate_test() ->
    Map = #{a => #{}},
    Result = erlmcp_map_utils:put_nested(Map, [a, b, c], 42),
    ?assertEqual(#{a => #{b => #{c => 42}}}, Result).

%%%=====================================================================
%%% Get Nested Tests
%%%=====================================================================

get_nested_single_test() ->
    Map = #{a => 1},
    ?assertEqual(1, erlmcp_map_utils:get_nested(Map, [a])).

get_nested_deep_test() ->
    Map = #{a => #{b => #{c => 42}}},
    ?assertEqual(42, erlmcp_map_utils:get_nested(Map, [a, b, c])).

get_nested_default_test() ->
    Map = #{a => #{b => #{c => 42}}},
    ?assertEqual(0, erlmcp_map_utils:get_nested(Map, [x, y], 0)).

get_nested_missing_test() ->
    Map = #{a => #{b => #{c => 42}}},
    ?assertEqual(undefined, erlmcp_map_utils:get_nested(Map, [a, x])).

get_nested_empty_path_test() ->
    Map = #{a => 1},
    ?assertEqual(undefined, erlmcp_map_utils:get_nested(Map, [])).

%%%=====================================================================
%%% Update Nested Tests
%%%=====================================================================

update_nested_test() ->
    Map = #{a => #{b => 1}},
    Result = erlmcp_map_utils:update_nested(Map, [a, b], fun(V) -> V * 2 end, 0),
    ?assertEqual(#{a => #{b => 2}}, Result).

update_nested_create_test() ->
    Map = #{},
    Result = erlmcp_map_utils:update_nested(Map, [a, b], fun(_) -> 42 end, 0),
    ?assertEqual(#{a => #{b => 42}}, Result).

update_nested_default_test() ->
    Map = #{a => #{}},
    Result = erlmcp_map_utils:update_nested(Map, [a, b], fun(V) -> V + 1 end, 0),
    ?assertEqual(#{a => #{b => 1}}, Result).

%%%=====================================================================
%%% Remove Nested Tests
%%%=====================================================================

remove_nested_single_test() ->
    Map = #{a => 1},
    Result = erlmcp_map_utils:remove_nested(Map, [a]),
    ?assertEqual(#{}, Result).

remove_nested_deep_test() ->
    Map = #{a => #{b => #{c => 42}}},
    Result = erlmcp_map_utils:remove_nested(Map, [a, b, c]),
    ?assertEqual(#{}, Result).

remove_nested_partial_test() ->
    Map = #{a => #{b => #{c => 1, d => 2}}},
    Result = erlmcp_map_utils:remove_nested(Map, [a, b, c]),
    ?assertEqual(#{a => #{b => #{d => 2}}}, Result).

remove_nested_cleanup_test() ->
    Map = #{a => #{b => #{}}},
    Result = erlmcp_map_utils:remove_nested(Map, [a, b, c]),
    ?assertEqual(#{}, Result).  % Empty maps cleaned up

%%%=====================================================================
%%% Merge Capabilities Tests
%%%=====================================================================

merge_capabilities_single_test() ->
    Maps = [#{a => 1}],
    Result = erlmcp_map_utils:merge_capabilities(Maps),
    ?assertEqual(#{a => 1}, Result).

merge_capabilities_multiple_test() ->
    Maps = [#{a => 1}, #{b => 2}, #{a => 3}],
    Result = erlmcp_map_utils:merge_capabilities(Maps),
    ?assertEqual(#{a => 3, b => 2}, Result).

merge_capabilities_empty_test() ->
    Result = erlmcp_map_utils:merge_capabilities([]),
    ?assertEqual(#{}, Result).

merge_capabilities_large_test() ->
    Maps = [#{<<"cap", (integer_to_binary(I))/binary>> => I} || I <- lists:seq(1, 100)],
    Result = erlmcp_map_utils:merge_capabilities(Maps),
    ?assertEqual(100, maps:size(Result)).

%%%=====================================================================
%%% Deep Merge Tests
%%%=====================================================================

deep_merge_simple_test() ->
    Left = #{a => 1},
    Right = #{b => 2},
    Result = erlmcp_map_utils:deep_merge(Left, Right),
    ?assertEqual(#{a => 1, b => 2}, Result).

deep_merge_nested_test() ->
    Left = #{a => #{x => 1, y => 2}},
    Right = #{a => #{y => 10, z => 3}, b => 4},
    Result = erlmcp_map_utils:deep_merge(Left, Right),
    ?assertEqual(#{a => #{x => 1, y => 10, z => 3}, b => 4}, Result).

deep_merge_override_test() ->
    Left = #{a => #{b => 1}},
    Right = #{a => 5},
    Result = erlmcp_map_utils:deep_merge(Left, Right),
    ?assertEqual(#{a => 5}, Result).

deep_merge_complex_test() ->
    Left = #{
        a => #{b => #{c => 1, d => 2}},
        e => #{f => 3}
    },
    Right = #{
        a => #{b => #{c => 10}, g => 4},
        e => 5,
        h => #{i => 6}
    },
    Result = erlmcp_map_utils:deep_merge(Left, Right),
    Expected = #{
        a => #{b => #{c => 10, d => 2}, g => 4},
        e => 5,
        h => #{i => 6}
    },
    ?assertEqual(Expected, Result).

%%%=====================================================================
%%% Map Size Accumulator Tests
%%%=====================================================================

map_size_acc_flat_test() ->
    Map = #{a => 1, b => 2, c => 3},
    ?assertEqual(3, erlmcp_map_utils:map_size_acc(Map)).

map_size_acc_nested_test() ->
    Map = #{a => 1, b => #{c => 2, d => #{e => 3}}},
    ?assertEqual(6, erlmcp_map_utils:map_size_acc(Map)).

map_size_acc_deep_test() ->
    Map = #{
        a => #{b => #{c => #{d => #{e => 1}}}},
        f => 2
    },
    ?assertEqual(7, erlmcp_map_utils:map_size_acc(Map)).

%%%=====================================================================
%%% Keys With Prefix Tests
%%%=====================================================================

keys_with_prefix_test() ->
    Map = #{<<"root_a">> => 1, <<"root_b">> => 2, <<"other">> => 3},
    Result = erlmcp_map_utils:keys_with_prefix(Map, <<"root_">>),
    ?assertEqual(2, length(Result)),
    ?assert(lists:member(<<"root_a">>, Result)),
    ?assert(lists:member(<<"root_b">>, Result)).

keys_with_prefix_atom_test() ->
    Map = #{<<"test_a">> => 1, <<"test_b">> => 2},
    Result = erlmcp_map_utils:keys_with_prefix(Map, test_),
    ?assertEqual(2, length(Result)).

keys_with_prefix_empty_test() ->
    Map = #{<<"a">> => 1, <<"b">> => 2},
    Result = erlmcp_map_utils:keys_with_prefix(Map, <<"x">>),
    ?assertEqual([], Result).

%%%=====================================================================
%%% Transform Values Tests
%%%=====================================================================

transform_values_test() ->
    Map = #{a => 1, b => 2},
    Result = erlmcp_map_utils:transform_values(Map, fun(V) -> V * 2 end),
    ?assertEqual(#{a => 2, b => 4}, Result).

transform_values_complex_test() ->
    Map = #{a => #{x => 1}, b => #{y => 2}},
    Result = erlmcp_map_utils:transform_values(Map, fun(V) -> maps:size(V) end),
    ?assertEqual(#{a => 1, b => 1}, Result).

%%%=====================================================================
%%% Compact Tests
%%%=====================================================================

compact_undefined_test() ->
    Map = #{a => 1, b => undefined, c => 2},
    Result = erlmcp_map_utils:compact(Map),
    ?assertEqual(#{a => 1, c => 2}, Result).

compact_nested_test() ->
    Map = #{a => 1, b => undefined, c => #{d => null, e => 2}},
    Result = erlmcp_map_utils:compact(Map),
    ?assertEqual(#{a => 1, c => #{e => 2}}, Result).

compact_empty_nested_test() ->
    Map = #{a => 1, b => #{}, c => 2},
    Result = erlmcp_map_utils:compact(Map),
    ?assertEqual(#{a => 1, c => 2}, Result).

compact_all_undefined_test() ->
    Map = #{a => undefined, b => null, c => #{d => undefined}},
    Result = erlmcp_map_utils:compact(Map),
    ?assertEqual(#{}, Result).

%%%=====================================================================
%%% Paths To Values Tests
%%%=====================================================================

paths_to_values_flat_test() ->
    Map = #{a => 1, b => 2},
    Result = erlmcp_map_utils:paths_to_values(Map),
    ?assertEqual(2, length(Result)),
    ?assert(lists:keymember([a], 1, Result)),
    ?assert(lists:keymember([b], 1, Result)).

paths_to_values_nested_test() ->
    Map = #{a => #{b => 1}, c => 2},
    Result = erlmcp_map_utils:paths_to_values(Map),
    ?assertEqual(2, length(Result)),
    ?assertEqual({[a, b], 1}, lists:keyfind([a, b], 1, Result)),
    ?assertEqual({[c], 2}, lists:keyfind([c], 1, Result)).

paths_to_values_deep_test() ->
    Map = #{a => #{b => #{c => 1}}, d => 2},
    Result = erlmcp_map_utils:paths_to_values(Map),
    ?assertEqual(2, length(Result)),
    ?assertEqual({[a, b, c], 1}, lists:keyfind([a, b, c], 1, Result)),
    ?assertEqual({[d], 2}, lists:keyfind([d], 1, Result)).
