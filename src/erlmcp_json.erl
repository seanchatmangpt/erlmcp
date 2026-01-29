%%%-------------------------------------------------------------------
%% @doc MCP+ JSON Utilities - Canonical JSON Encoding
%%
%% Provides canonical JSON encoding for consistent hashing.
%% Maps are sorted by key before encoding to ensure deterministic output.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_json).

%% API
-export([
    canonical_encode/1,
    sort_map/1
]).

%%====================================================================
%% API
%%====================================================================

%% @doc Encode a term to canonical JSON (sorted keys for maps).
-spec canonical_encode(term()) -> binary().
canonical_encode(Term) ->
    SortedTerm = sort_term(Term),
    jsx:encode(SortedTerm).

%% @doc Recursively sort a map by keys.
-spec sort_map(map()) -> [{binary(), term()}].
sort_map(Map) when is_map(Map) ->
    Pairs = maps:to_list(Map),
    SortedPairs = lists:sort(fun({K1, _}, {K2, _}) -> K1 =< K2 end, Pairs),
    [{K, sort_term(V)} || {K, V} <- SortedPairs].

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Sort a term recursively.
-spec sort_term(term()) -> term().
sort_term(Map) when is_map(Map) ->
    sort_map(Map);
sort_term(List) when is_list(List) ->
    [sort_term(Item) || Item <- List];
sort_term(Other) ->
    Other.
