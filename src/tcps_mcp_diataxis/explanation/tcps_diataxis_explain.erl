%%%-------------------------------------------------------------------
%%% @doc TCPS Diataxis Explanation Engine
%%% Understanding-oriented content that clarifies concepts, provides
%%% context, explains design decisions, and makes connections.
%%%
%%% This module provides the "Explanation" quadrant of Diataxis:
%%% - Clarifies core concepts and philosophy
%%% - Explains the "why" behind design decisions
%%% - Provides context and comparisons
%%% - Uses analogies and trade-off analysis
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_diataxis_explain).

-export([
    get_explanation/1,
    list_explanations/0,
    list_by_category/1,
    search_explanations/1,
    get_related_explanations/1,
    get_explanation_path/1,
    validate_explanation/1
]).

-type explanation_id() :: binary().
-type category() :: core_concepts | design_decisions | comparisons.
-type section() :: #{
    title := binary(),
    content := binary(),
    examples => [binary()],
    diagrams => [binary()],
    key_points => [binary()]
}.
-type explanation() :: #{
    id := explanation_id(),
    title := binary(),
    category := category(),
    summary := binary(),
    sections := [section()],
    analogies := [binary()],
    trade_offs := #{pros := [binary()], cons := [binary()]},
    related := [explanation_id()],
    tags := [binary()],
    difficulty := beginner | intermediate | advanced,
    reading_time_minutes := pos_integer()
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Get a specific explanation by ID
-spec get_explanation(explanation_id()) -> {ok, explanation()} | {error, not_found}.
get_explanation(Id) ->
    case maps:find(Id, get_all_explanations()) of
        {ok, Explanation} -> {ok, Explanation};
        error -> {error, not_found}
    end.

%% @doc List all available explanations
-spec list_explanations() -> [explanation()].
list_explanations() ->
    maps:values(get_all_explanations()).

%% @doc List explanations by category
-spec list_by_category(category()) -> [explanation()].
list_by_category(Category) ->
    [E || E <- list_explanations(), maps:get(category, E) =:= Category].

%% @doc Search explanations by keyword
-spec search_explanations(binary()) -> [explanation()].
search_explanations(Keyword) ->
    LowerKeyword = string:lowercase(Keyword),
    lists:filter(
        fun(E) ->
            Title = string:lowercase(maps:get(title, E)),
            Summary = string:lowercase(maps:get(summary, E)),
            Tags = [string:lowercase(T) || T <- maps:get(tags, E)],
            string:find(Title, LowerKeyword) =/= nomatch orelse
            string:find(Summary, LowerKeyword) =/= nomatch orelse
            lists:any(fun(Tag) -> string:find(Tag, LowerKeyword) =/= nomatch end, Tags)
        end,
        list_explanations()
    ).

%% @doc Get related explanations for a given explanation ID
-spec get_related_explanations(explanation_id()) -> [explanation()].
get_related_explanations(Id) ->
    case get_explanation(Id) of
        {ok, Explanation} ->
            RelatedIds = maps:get(related, Explanation),
            lists:filtermap(
                fun(RelId) ->
                    case get_explanation(RelId) of
                        {ok, RelExpl} -> {true, RelExpl};
                        {error, _} -> false
                    end
                end,
                RelatedIds
            );
        {error, _} -> []
    end.

%% @doc Get the learning path for an explanation
-spec get_explanation_path(explanation_id()) -> [explanation()].
get_explanation_path(Id) ->
    case get_explanation(Id) of
        {ok, Explanation} ->
            Category = maps:get(category, Explanation),
            Difficulty = maps:get(difficulty, Explanation),

            % Get all explanations in same category with same or lower difficulty
            SameCategory = list_by_category(Category),
            Path = lists:filter(
                fun(E) ->
                    difficulty_level(maps:get(difficulty, E)) =< difficulty_level(Difficulty)
                end,
                SameCategory
            ),

            % Sort by difficulty
            lists:sort(
                fun(E1, E2) ->
                    difficulty_level(maps:get(difficulty, E1)) =<
                    difficulty_level(maps:get(difficulty, E2))
                end,
                Path
            );
        {error, _} -> []
    end.

%% @doc Validate an explanation structure
-spec validate_explanation(explanation()) -> ok | {error, term()}.
validate_explanation(Explanation) ->
    RequiredKeys = [id, title, category, summary, sections, analogies, trade_offs, related, tags, difficulty, reading_time_minutes],
    case lists:all(fun(Key) -> maps:is_key(Key, Explanation) end, RequiredKeys) of
        true ->
            validate_sections(maps:get(sections, Explanation));
        false ->
            {error, missing_required_keys}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
difficulty_level(beginner) -> 1;
difficulty_level(intermediate) -> 2;
difficulty_level(advanced) -> 3.

%% @private
validate_sections([]) -> ok;
validate_sections([Section | Rest]) ->
    case maps:is_key(title, Section) andalso maps:is_key(content, Section) of
        true -> validate_sections(Rest);
        false -> {error, invalid_section}
    end.

%% @private
get_all_explanations() ->
    #{
        % Core Concepts
        <<"why_tcps">> => tcps_concepts:why_tcps(),
        <<"jidoka_philosophy">> => tcps_concepts:jidoka_philosophy(),
        <<"pull_vs_push">> => tcps_concepts:pull_vs_push(),
        <<"andon_thinking">> => tcps_concepts:andon_thinking(),
        <<"heijunka_leveling">> => tcps_concepts:heijunka_leveling(),

        % Design Decisions
        <<"receipts_not_commits">> => tcps_principles:receipts_not_commits(),
        <<"quality_gates_vs_ci">> => tcps_principles:quality_gates_vs_ci(),
        <<"wip_limits_matter">> => tcps_principles:wip_limits_matter(),
        <<"dual_storage">> => tcps_principles:dual_storage(),
        <<"mcp_integration">> => tcps_principles:mcp_integration(),

        % Comparisons
        <<"tcps_vs_devops">> => tcps_principles:tcps_vs_devops(),
        <<"tcps_vs_lean">> => tcps_principles:tcps_vs_lean(),
        <<"tcps_vs_agile">> => tcps_principles:tcps_vs_agile(),
        <<"quality_gates_vs_static">> => tcps_principles:quality_gates_vs_static(),
        <<"andon_vs_monitoring">> => tcps_principles:andon_vs_monitoring()
    }.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_explanation_test() ->
    {ok, Explanation} = get_explanation(<<"why_tcps">>),
    ?assertEqual(<<"Why TCPS? Understanding Toyota Production System for Code">>,
                 maps:get(title, Explanation)),
    ?assertEqual(core_concepts, maps:get(category, Explanation)).

list_by_category_test() ->
    CoreConcepts = list_by_category(core_concepts),
    ?assertEqual(5, length(CoreConcepts)),
    DesignDecisions = list_by_category(design_decisions),
    ?assertEqual(5, length(DesignDecisions)),
    Comparisons = list_by_category(comparisons),
    ?assertEqual(5, length(Comparisons)).

search_explanations_test() ->
    Results = search_explanations(<<"quality">>),
    ?assert(length(Results) > 0).

validate_explanation_test() ->
    {ok, Explanation} = get_explanation(<<"why_tcps">>),
    ?assertEqual(ok, validate_explanation(Explanation)).

difficulty_level_test() ->
    ?assertEqual(1, difficulty_level(beginner)),
    ?assertEqual(2, difficulty_level(intermediate)),
    ?assertEqual(3, difficulty_level(advanced)).

-endif.
