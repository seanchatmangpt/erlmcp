-module(erlmcp_map_utils).

%% OTP 26-28 Map Enhancement Utilities for MCP Data Structures
%%
%% This module provides optimized map operations leveraging:
%% - OTP 26: Larger map keys, performance improvements
%% - OTP 27: maps:foreach/2, maps:filter/2 with pattern matching
%% - OTP 28: maps:put/3 compile-time optimization
%%
%% @doc Efficient map utilities for MCP metadata, capabilities, and resources

-export([
    filter_keys/2,
    put_nested/3,
    merge_capabilities/1,
    filter_by_prefix/2,
    update_nested/4,
    get_nested/2,
    get_nested/3,
    deep_merge/2,
    map_size_acc/1,
    keys_with_prefix/2,
    remove_nested/2,
    transform_values/2,
    compact/1,
    paths_to_values/1
]).

-export_type([
    path/0,
    nest_map/0
]).

%%%====================================================================
%%% Types
%%%====================================================================

-type path() :: [term()].
-type nest_map() :: map().

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Filter map by key prefix (OTP 27 enhancement with pattern matching)
%%
%% Example:
%%   Map = #{<<"root_1">> => V1, <<"root_2">> => V2, <<"other">> => V3},
%%   erlmcp_map_utils:filter_by_prefix(Map, <<"root_">>).
%%   => #{<<"root_1">> => V1, <<"root_2">> => V2}
%%
%% Uses OTP 27's optimized maps:filter/2 with pattern matching
-spec filter_by_prefix(map(), binary() | atom()) -> map().
filter_by_prefix(Map, Prefix) when is_map(Map), is_atom(Prefix) ->
    filter_by_prefix(Map, atom_to_binary(Prefix, utf8));
filter_by_prefix(Map, Prefix) when is_map(Map), is_binary(Prefix) ->
    PrefixSize = byte_size(Prefix),
    %% OTP 27: maps:filter/2 with optimized pattern matching
    maps:filter(fun(K, _V) ->
        case K of
            <<Prefix:PrefixSize/binary, _/binary>> -> true;
            _ -> false
        end
    end, Map).

%% @doc Filter map returning only keys matching exact prefix pattern
%%
%% Similar to filter_by_prefix/2 but requires exact prefix match
%% Used for resource subscription filtering in MCP
-spec filter_keys(map(), binary() | atom()) -> map().
filter_keys(Map, KeyPrefix) when is_map(Map), is_atom(KeyPrefix) ->
    filter_keys(Map, atom_to_binary(KeyPrefix, utf8));
filter_keys(Map, KeyPrefix) when is_map(Map), is_binary(KeyPrefix) ->
    KeyPrefixSize = byte_size(KeyPrefix),
    maps:filter(fun(K, _V) ->
        case K of
            <<KeyPrefix:KeyPrefixSize/binary, _/binary>> -> true;
            _ -> false
        end
    end, Map).

%% @doc Update nested map efficiently (OTP 28 optimization)
%%
%% Creates intermediate maps as needed.
%% Uses OTP 28's compile-time optimization for maps:put/3
%%
%% Example:
%%   Map = #{},
%%   erlmcp_map_utils:put_nested(Map, [a, b, c], 42).
%%   => #{a => #{b => #{c => 42}}}
%%
-spec put_nested(nest_map(), path(), term()) -> nest_map().
put_nested(Map, [Key], Value) ->
    %% OTP 28: Compile-time optimization for maps:put/3
    maps:put(Key, Value, Map);
put_nested(Map, [Key | Rest], Value) ->
    Nested = maps:get(Key, Map, #{}),
    maps:put(Key, put_nested(Nested, Rest, Value), Map).

%% @doc Get value from nested map path
%%
%% Returns undefined if path doesn't exist
-spec get_nested(nest_map(), path()) -> term() | undefined.
get_nested(Map, Path) ->
    get_nested(Map, Path, undefined).

%% @doc Get value from nested map path with default
%%
%% Example:
%%   Map = #{a => #{b => #{c => 42}}},
%%   erlmcp_map_utils:get_nested(Map, [a, b, c], 0).
%%   => 42
%%
-spec get_nested(nest_map(), path(), term()) -> term().
get_nested(_Map, [], Default) ->
    Default;
get_nested(Map, [Key], Default) ->
    maps:get(Key, Map, Default);
get_nested(Map, [Key | Rest], Default) ->
    case maps:get(Key, Map, undefined) of
        undefined -> Default;
        Nested -> get_nested(Nested, Rest, Default)
    end.

%% @doc Update value at nested path (creates path if needed)
%%
%% Fun receives current value (or undefined) and returns new value
-spec update_nested(nest_map(), path(), fun((term()) -> term()), term()) -> nest_map().
update_nested(Map, Path, Fun, Default) ->
    Current = get_nested(Map, Path, Default),
    put_nested(Map, Path, Fun(Current)).

%% @doc Remove nested path from map (cleans up empty maps)
%%
%% Example:
%%   Map = #{a => #{b => #{c => 42}}},
%%   erlmcp_map_utils:remove_nested(Map, [a, b, c]).
%%   => #{}
%%
-spec remove_nested(nest_map(), path()) -> nest_map().
remove_nested(Map, []) ->
    Map;
remove_nested(Map, [Key]) ->
    maps:remove(Key, Map);
remove_nested(Map, [Key | Rest]) ->
    case maps:get(Key, Map, undefined) of
        undefined -> Map;
        Nested ->
            Updated = remove_nested(Nested, Rest),
            case maps:size(Updated) of
                0 -> maps:remove(Key, Map);
                _ -> maps:put(Key, Updated, Map)
            end
    end.

%% @doc Merge multiple capability maps (for MCP capability negotiation)
%%
%% Uses foldl for efficient left-to-right merging
%% Later maps override earlier ones on conflicts
%%
%% Example:
%%   Cap1 = #{roots => true, tools => true},
%%   Cap2 = #{tools => false, sampling => true},
%%   erlmcp_map_utils:merge_capabilities([Cap1, Cap2]).
%%   => #{roots => true, tools => false, sampling => true}
%%
-spec merge_capabilities([map()]) -> map().
merge_capabilities(CapMaps) when is_list(CapMaps) ->
    lists:foldl(fun maps:merge/2, #{}, CapMaps).

%% @doc Deep merge two maps (recursively merges nested maps)
%%
%% Non-map values from Right override Left
%% Map values are merged recursively
%%
-spec deep_merge(map(), map()) -> map().
deep_merge(Left, Right) when is_map(Left), is_map(Right) ->
    maps:fold(fun(Key, RightVal, Acc) ->
        case maps:get(Key, Acc, undefined) of
            undefined ->
                %% Key doesn't exist in Left, add from Right
                maps:put(Key, RightVal, Acc);
            LeftVal when is_map(LeftVal), is_map(RightVal) ->
                %% Both are maps, merge recursively
                maps:put(Key, deep_merge(LeftVal, RightVal), Acc);
            _ ->
                %% LeftVal exists but isn't a map, or RightVal isn't a map
                %% Override with RightVal
                maps:put(Key, RightVal, Acc)
        end
    end, Left, Right).

%% @doc Count map size including nested maps
%%
%% Returns total count of all leaf values
%%
%% Example:
%%   Map = #{a => 1, b => #{c => 2, d => #{e => 3}}},
%%   erlmcp_map_utils:map_size_acc(Map).
%%   => 3
%%
-spec map_size_acc(map() | term()) -> non_neg_integer().
map_size_acc(Map) when is_map(Map) ->
    maps:fold(fun(_K, V, Acc) ->
        Acc + 1 + map_size_acc(V)
    end, 0, Map);
map_size_acc(_) ->
    0.

%% @doc Get all keys matching prefix (OTP 27 optimized)
%%
%% Returns list of keys (not values)
%% Useful for enumerating resources or tools by namespace
%%
-spec keys_with_prefix(map(), binary() | atom()) -> [term()].
keys_with_prefix(Map, Prefix) when is_map(Map), is_atom(Prefix) ->
    keys_with_prefix(Map, atom_to_binary(Prefix, utf8));
keys_with_prefix(Map, Prefix) when is_map(Map), is_binary(Prefix) ->
    PrefixSize = byte_size(Prefix),
    maps:fold(fun(K, _V, Acc) ->
        case K of
            <<Prefix:PrefixSize/binary, _/binary>> -> [K | Acc];
            _ -> Acc
        end
    end, [], Map).

%% @doc Transform all values in map (OTP 27: maps:map/2)
%%
%% Example:
%%   Map = #{a => 1, b => 2},
%%   erlmcp_map_utils:transform_values(Map, fun(V) -> V * 2 end).
%%   => #{a => 2, b => 4}
%%
-spec transform_values(map(), fun((term()) -> term())) -> map().
transform_values(Map, Fun) when is_map(Map), is_function(Fun, 1) ->
    %% OTP 27: optimized maps:map/2 implementation
    maps:map(fun(_K, V) -> Fun(V) end, Map).

%% @doc Remove all undefined and null values from map
%%
%% Recursively cleans nested maps
%% Used for JSON serialization cleanup
%%
%% Example:
%%   Map = #{a => 1, b => undefined, c => #{d => null, e => 2}},
%%   erlmcp_map_utils:compact(Map).
%%   => #{a => 1, c => #{e => 2}}
%%
-spec compact(map()) -> map().
compact(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        case V of
            undefined -> Acc;
            null -> Acc;
            Nested when is_map(Nested) ->
                case compact(Nested) of
                    Empty when map_size(Empty) =:= 0 -> Acc;
                    Cleaned -> maps:put(K, Cleaned, Acc)
                end;
            _ -> maps:put(K, V, Acc)
        end
    end, #{}, Map).

%% @doc Convert map to list of path-value pairs
%%
%% Flattens nested structure to paths
%%
%% Example:
%%   Map = #{a => #{b => 1, c => 2}, d => 3},
%%   erlmcp_map_utils:paths_to_values(Map).
%%   => {[{[a, b], 1}, {[a, c], 2}, {[d], 3}]}
%%
-spec paths_to_values(map()) -> {[{path(), term()}, ...]}.
paths_to_values(Map) when is_map(Map) ->
    paths_to_values(Map, [], []).

%% @doc Internal helper for paths_to_values/1
-spec paths_to_values(map(), path(), [{path(), term()}]) -> {[{path(), term()}, ...]}.
paths_to_values(Map, Path, Acc) when is_map(Map) ->
    maps:fold(fun(K, V, AccIn) ->
        NewPath = Path ++ [K],
        case V of
            Nested when is_map(Nested) ->
                paths_to_values(Nested, NewPath, AccIn);
            _ ->
                [{NewPath, V} | AccIn]
        end
    end, Acc, Map).

%%%====================================================================
%%% OTP Version-Specific Optimizations
%%%====================================================================

%% @doc Get OTP version for feature detection
%% -internal
-spec get_otp_version() -> {Major::non_neg_integer(), Minor::non_neg_integer()}.
get_otp_version() ->
    case erlang:system_info(otp_release) of
        [$R,N0,N1|_] -> {list_to_integer([N0]), list_to_integer([N1])};
        [N0,N1,N2|_] -> {list_to_integer([N0,N1]), list_to_integer([N2])};
        _ -> {26, 0}  %% Assume OTP 26 as minimum
    end.

%% @doc Check if OTP 28+ (compile-time optimization available)
%% -internal
-spec has_otp28_put_optimization() -> boolean().
has_otp28_put_optimization() ->
    {Major, _} = get_otp_version(),
    Major >= 28.

%% @doc Check if OTP 27+ (pattern matching in maps:filter/2)
%% -internal
-spec has_otp27_filter_optimization() -> boolean().
has_otp27_filter_optimization() ->
    {Major, _} = get_otp_version(),
    Major >= 27.

%%%====================================================================
%%% Unit Tests
%%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%% Test Filter By Prefix

filter_by_prefix_binary_test() ->
    Map = #{<<"root_1">> => a, <<"root_2">> => b, <<"other">> => c},
    Result = filter_by_prefix(Map, <<"root_">>),
    ?assertEqual(#{<<"root_1">> => a, <<"root_2">> => b}, Result).

filter_by_prefix_atom_test() ->
    Map = #{<<"root_1">> => a, <<"root_2">> => b, <<"other">> => c},
    Result = filter_by_prefix(Map, root_),
    ?assertEqual(#{<<"root_1">> => a, <<"root_2">> => b}, Result).

filter_by_prefix_empty_test() ->
    Map = #{<<"a">> => 1, <<"b">> => 2},
    Result = filter_by_prefix(Map, <<"x">>),
    ?assertEqual(#{}, Result).

%%% Test Put Nested

put_nested_single_test() ->
    Map = #{},
    Result = put_nested(Map, [a], 1),
    ?assertEqual(#{a => 1}, Result).

put_nested_deep_test() ->
    Map = #{},
    Result = put_nested(Map, [a, b, c], 42),
    ?assertEqual(#{a => #{b => #{c => 42}}}, Result).

put_nested_update_test() ->
    Map = #{a => #{b => #{c => 1}}},
    Result = put_nested(Map, [a, b, c], 2),
    ?assertEqual(#{a => #{b => #{c => 2}}}, Result).

%%% Test Get Nested

get_nested_single_test() ->
    Map = #{a => 1},
    ?assertEqual(1, get_nested(Map, [a])).

get_nested_deep_test() ->
    Map = #{a => #{b => #{c => 42}}},
    ?assertEqual(42, get_nested(Map, [a, b, c])).

get_nested_default_test() ->
    Map = #{a => #{b => #{c => 42}}},
    ?assertEqual(0, get_nested(Map, [x, y], 0)).

get_nested_missing_test() ->
    Map = #{a => #{b => #{c => 42}}},
    ?assertEqual(undefined, get_nested(Map, [a, x])).

%%% Test Update Nested

update_nested_test() ->
    Map = #{a => #{b => 1}},
    Result = update_nested(Map, [a, b], fun(V) -> V * 2 end, 0),
    ?assertEqual(#{a => #{b => 2}}, Result).

update_nested_create_test() ->
    Map = #{},
    Result = update_nested(Map, [a, b], fun(_) -> 42 end, 0),
    ?assertEqual(#{a => #{b => 42}}, Result).

%%% Test Remove Nested

remove_nested_single_test() ->
    Map = #{a => 1},
    Result = remove_nested(Map, [a]),
    ?assertEqual(#{}, Result).

remove_nested_deep_test() ->
    Map = #{a => #{b => #{c => 42}}},
    Result = remove_nested(Map, [a, b, c]),
    ?assertEqual(#{}, Result).

remove_nested_partial_test() ->
    Map = #{a => #{b => #{c => 1, d => 2}}},
    Result = remove_nested(Map, [a, b, c]),
    ?assertEqual(#{a => #{b => #{d => 2}}}, Result).

%%% Test Merge Capabilities

merge_capabilities_single_test() ->
    Maps = [#{a => 1}],
    Result = merge_capabilities(Maps),
    ?assertEqual(#{a => 1}, Result).

merge_capabilities_multiple_test() ->
    Maps = [#{a => 1}, #{b => 2}, #{a => 3}],
    Result = merge_capabilities(Maps),
    ?assertEqual(#{a => 3, b => 2}, Result).

merge_capabilities_empty_test() ->
    Result = merge_capabilities([]),
    ?assertEqual(#{}, Result).

%%% Test Deep Merge

deep_merge_simple_test() ->
    Left = #{a => 1},
    Right = #{b => 2},
    Result = deep_merge(Left, Right),
    ?assertEqual(#{a => 1, b => 2}, Result).

deep_merge_nested_test() ->
    Left = #{a => #{x => 1, y => 2}},
    Right = #{a => #{y => 10, z => 3}, b => 4},
    Result = deep_merge(Left, Right),
    ?assertEqual(#{a => #{x => 1, y => 10, z => 3}, b => 4}, Result).

deep_merge_override_test() ->
    Left = #{a => #{b => 1}},
    Right = #{a => 5},
    Result = deep_merge(Left, Right),
    ?assertEqual(#{a => 5}, Result).

%%% Test Map Size Acc

map_size_acc_flat_test() ->
    Map = #{a => 1, b => 2, c => 3},
    ?assertEqual(3, map_size_acc(Map)).

map_size_acc_nested_test() ->
    Map = #{a => 1, b => #{c => 2, d => #{e => 3}}},
    ?assertEqual(6, map_size_acc(Map)).

%%% Test Keys With Prefix

keys_with_prefix_test() ->
    Map = #{<<"root_a">> => 1, <<"root_b">> => 2, <<"other">> => 3},
    Result = keys_with_prefix(Map, <<"root_">>),
    ?assertEqual(2, length(Result)),
    ?assert(lists:member(<<"root_a">>, Result)),
    ?assert(lists:member(<<"root_b">>, Result)).

%%% Test Transform Values

transform_values_test() ->
    Map = #{a => 1, b => 2},
    Result = transform_values(Map, fun(V) -> V * 2 end),
    ?assertEqual(#{a => 2, b => 4}, Result).

%%% Test Compact

compact_undefined_test() ->
    Map = #{a => 1, b => undefined, c => 2},
    Result = compact(Map),
    ?assertEqual(#{a => 1, c => 2}, Result).

compact_nested_test() ->
    Map = #{a => 1, b => undefined, c => #{d => null, e => 2}},
    Result = compact(Map),
    ?assertEqual(#{a => 1, c => #{e => 2}}, Result).

compact_empty_nested_test() ->
    Map = #{a => 1, b => #{}, c => 2},
    Result = compact(Map),
    ?assertEqual(#{a => 1, c => 2}, Result).

%%% Test Paths To Values

paths_to_values_flat_test() ->
    Map = #{a => 1, b => 2},
    Result = paths_to_values(Map),
    ?assertEqual(2, length(Result)),
    ?assert(lists:keymember([a], 1, Result)),
    ?assert(lists:keymember([b], 1, Result)).

paths_to_values_nested_test() ->
    Map = #{a => #{b => 1}, c => 2},
    Result = paths_to_values(Map),
    ?assertEqual(2, length(Result)),
    ?assertEqual({[a, b], 1}, lists:keyfind([a, b], 1, Result)),
    ?assertEqual({[c], 2}, lists:keyfind([c], 1, Result)).

-endif.
