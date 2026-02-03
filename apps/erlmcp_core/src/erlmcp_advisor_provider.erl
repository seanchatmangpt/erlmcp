%%% @doc MCP Advisor Provider Behavior
%%%
%%% Defines the interface that all search providers must implement.
%%% Providers are pluggable components that search for MCP servers
%%% from different sources (registry, external APIs, local cache).
%%%
%%% Providers should be stateless and return results quickly.
%%% Long-running operations should use caching internally.
%%%
%%% @end
-module(erlmcp_advisor_provider).

-include("erlmcp.hrl").

%% Behavior exports
-export([behaviour_info/1]).

%% Utility exports
-export([
    normalize_result/1,
    validate_result/1,
    compute_text_similarity/2,
    tokenize/1,
    normalize_text/1
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type search_options() :: #{
    limit => pos_integer(),
    min_similarity => float(),
    config => map()
}.

-type search_result() :: #{
    id => binary(),
    title => binary(),
    description => binary(),
    url => binary() | undefined,
    github_url => binary() | undefined,
    similarity => float(),
    metadata => map()
}.

-type provider_info() :: #{
    name => binary(),
    version => binary(),
    description => binary(),
    capabilities => [atom()]
}.

-export_type([search_options/0, search_result/0, provider_info/0]).

%%====================================================================
%% Behavior Definition
%%====================================================================

%% @doc Behavior callbacks
-spec behaviour_info(callbacks | optional_callbacks) -> [{atom(), non_neg_integer()}] | undefined.
behaviour_info(callbacks) ->
    [
        %% Search for MCP servers matching the query
        %% -spec search(binary(), search_options()) -> {ok, [search_result()]} | {error, term()}.
        {search, 2},

        %% Get provider information
        %% -spec info() -> provider_info().
        {info, 0}
    ];
behaviour_info(optional_callbacks) ->
    [
        %% Initialize provider (called once at startup)
        %% -spec init(map()) -> ok | {error, term()}.
        {init, 1},

        %% Health check
        %% -spec health() -> ok | {error, term()}.
        {health, 0},

        %% Cleanup (called at shutdown)
        %% -spec cleanup() -> ok.
        {cleanup, 0}
    ];
behaviour_info(_) ->
    undefined.

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Normalize a search result to ensure all required fields exist
-spec normalize_result(map()) -> search_result().
normalize_result(Result) when is_map(Result) ->
    #{
        id => maps:get(id, Result,
                       maps:get(title, Result, erlang:unique_integer([positive]))),
        title => ensure_binary(maps:get(title, Result, <<>>)),
        description => ensure_binary(maps:get(description, Result, <<>>)),
        url => maps:get(url, Result, undefined),
        github_url => maps:get(github_url, Result, undefined),
        similarity => maps:get(similarity, Result, 0.0),
        metadata => maps:get(metadata, Result, #{})
    }.

%% @doc Validate a search result
-spec validate_result(map()) -> ok | {error, term()}.
validate_result(Result) when is_map(Result) ->
    case maps:get(title, Result, undefined) of
        undefined -> {error, missing_title};
        Title when not is_binary(Title) -> {error, invalid_title};
        _ ->
            case maps:get(similarity, Result, undefined) of
                undefined -> {error, missing_similarity};
                Sim when not is_float(Sim) -> {error, invalid_similarity};
                Sim when Sim < 0.0; Sim > 1.0 -> {error, similarity_out_of_range};
                _ -> ok
            end
    end.

%% @doc Compute text similarity between query and text using TF-IDF inspired approach
-spec compute_text_similarity(binary(), binary()) -> float().
compute_text_similarity(Query, Text) when is_binary(Query), is_binary(Text) ->
    QueryTokens = tokenize(Query),
    TextTokens = tokenize(Text),

    case {QueryTokens, TextTokens} of
        {[], _} -> 0.0;
        {_, []} -> 0.0;
        _ ->
            %% Jaccard similarity with some modifications
            QuerySet = sets:from_list(QueryTokens),
            TextSet = sets:from_list(TextTokens),

            Intersection = sets:size(sets:intersection(QuerySet, TextSet)),
            Union = sets:size(sets:union(QuerySet, TextSet)),

            case Union of
                0 -> 0.0;
                _ ->
                    BaseSimilarity = Intersection / Union,

                    %% Boost for exact substring match
                    ExactBoost = case binary:match(normalize_text(Text),
                                                   normalize_text(Query)) of
                        nomatch -> 0.0;
                        _ -> 0.2
                    end,

                    %% Boost for query coverage (how many query terms are in text)
                    QueryCoverage = Intersection / sets:size(QuerySet),
                    CoverageBoost = QueryCoverage * 0.1,

                    min(1.0, BaseSimilarity + ExactBoost + CoverageBoost)
            end
    end.

%% @doc Tokenize text into lowercase words
-spec tokenize(binary()) -> [binary()].
tokenize(Text) when is_binary(Text) ->
    %% Convert to lowercase
    Lower = normalize_text(Text),

    %% Split on non-alphanumeric characters
    Tokens = binary:split(Lower, [<<" ">>, <<"\n">>, <<"\t">>,
                                   <<",">>, <<".">>, <<"-">>,
                                   <<"_">>, <<"/">>, <<"(">>,
                                   <<")">>, <<"[">>, <<"]">>],
                          [global, trim_all]),

    %% Filter empty and short tokens
    [T || T <- Tokens, byte_size(T) >= 2].

%% @doc Normalize text for comparison
-spec normalize_text(binary()) -> binary().
normalize_text(Text) when is_binary(Text) ->
    string:lowercase(Text).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Ensure value is binary
-spec ensure_binary(term()) -> binary().
ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_list(V) -> list_to_binary(V);
ensure_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
ensure_binary(V) when is_integer(V) -> integer_to_binary(V);
ensure_binary(_) -> <<>>.
