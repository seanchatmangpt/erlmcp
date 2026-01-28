%%%-------------------------------------------------------------------
%%% @doc Unit tests for tcps_diataxis_explain module
%%% Comprehensive test coverage for explanation retrieval and navigation.
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_diataxis_explain_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

explain_test_() ->
    [
     {"Get explanation by ID", fun test_get_explanation/0},
     {"Get non-existent explanation", fun test_get_nonexistent_explanation/0},
     {"List all explanations", fun test_list_explanations/0},
     {"List explanations by category", fun test_list_by_category/0},
     {"Search explanations", fun test_search_explanations/0},
     {"Get related explanations", fun test_get_related_explanations/0},
     {"Get explanation path", fun test_get_explanation_path/0},
     {"Validate explanation structure", fun test_validate_explanation/0},
     {"Explanation has all required keys", fun test_explanation_keys/0},
     {"Sections are well-formed", fun test_sections/0},
     {"Analogies present", fun test_analogies/0},
     {"Trade-offs documented", fun test_trade_offs/0},
     {"Category coverage", fun test_category_coverage/0},
     {"Difficulty levels", fun test_difficulty_levels/0}
    ].

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_get_explanation() ->
    {ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
    ?assertEqual(<<"Why TCPS? Understanding Toyota Production System for Code">>,
                 maps:get(title, Explanation)),
    ?assertEqual(core_concepts, maps:get(category, Explanation)),
    ?assert(maps:is_key(sections, Explanation)),
    ?assert(maps:is_key(analogies, Explanation)),
    ?assert(maps:is_key(trade_offs, Explanation)).

test_get_nonexistent_explanation() ->
    ?assertEqual({error, not_found}, tcps_diataxis_explain:get_explanation(<<"nonexistent">>)).

test_list_explanations() ->
    Explanations = tcps_diataxis_explain:list_explanations(),
    ?assert(length(Explanations) >= 15),
    ?assert(lists:all(fun(E) -> maps:is_key(id, E) end, Explanations)),
    ?assert(lists:all(fun(E) -> maps:is_key(title, E) end, Explanations)),
    ?assert(lists:all(fun(E) -> maps:is_key(category, E) end, Explanations)).

test_list_by_category() ->
    CoreConcepts = tcps_diataxis_explain:list_by_category(core_concepts),
    DesignDecisions = tcps_diataxis_explain:list_by_category(design_decisions),
    Comparisons = tcps_diataxis_explain:list_by_category(comparisons),

    ?assertEqual(5, length(CoreConcepts)),
    ?assertEqual(5, length(DesignDecisions)),
    ?assertEqual(5, length(Comparisons)),

    ?assert(lists:all(fun(E) -> maps:get(category, E) =:= core_concepts end, CoreConcepts)),
    ?assert(lists:all(fun(E) -> maps:get(category, E) =:= design_decisions end, DesignDecisions)),
    ?assert(lists:all(fun(E) -> maps:get(category, E) =:= comparisons end, Comparisons)).

test_search_explanations() ->
    Results1 = tcps_diataxis_explain:search_explanations(<<"quality">>),
    ?assert(length(Results1) > 0),

    Results2 = tcps_diataxis_explain:search_explanations(<<"andon">>),
    ?assert(length(Results2) >= 1),

    Results3 = tcps_diataxis_explain:search_explanations(<<"jidoka">>),
    ?assert(length(Results3) >= 1),

    % Case insensitive search
    Results4 = tcps_diataxis_explain:search_explanations(<<"QUALITY">>),
    ?assertEqual(length(Results1), length(Results4)).

test_get_related_explanations() ->
    {ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
    RelatedIds = maps:get(related, Explanation),

    ?assert(length(RelatedIds) > 0),

    Related = tcps_diataxis_explain:get_related_explanations(<<"why_tcps">>),
    ?assertEqual(length(RelatedIds), length(Related)),

    % All related should be valid explanations
    lists:foreach(
        fun(RelExpl) ->
            ?assert(maps:is_key(id, RelExpl)),
            ?assert(maps:is_key(title, RelExpl))
        end,
        Related
    ).

test_get_explanation_path() ->
    Path = tcps_diataxis_explain:get_explanation_path(<<"why_tcps">>),

    ?assert(length(Path) > 0),

    % Path should be sorted by difficulty
    Difficulties = [maps:get(difficulty, E) || E <- Path],
    SortedDifficulties = lists:sort(
        fun(D1, D2) ->
            difficulty_to_int(D1) =< difficulty_to_int(D2)
        end,
        Difficulties
    ),
    ?assertEqual(SortedDifficulties, Difficulties).

test_validate_explanation() ->
    {ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
    ?assertEqual(ok, tcps_diataxis_explain:validate_explanation(Explanation)),

    InvalidExplanation = #{
        id => <<"test">>,
        title => <<"Test">>
        % Missing required keys
    },
    ?assertEqual({error, missing_required_keys},
                 tcps_diataxis_explain:validate_explanation(InvalidExplanation)).

test_explanation_keys() ->
    {ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
    RequiredKeys = [id, title, category, summary, sections, analogies, trade_offs,
                    related, tags, difficulty, reading_time_minutes],
    lists:foreach(
        fun(Key) ->
            ?assert(maps:is_key(Key, Explanation))
        end,
        RequiredKeys
    ).

test_sections() ->
    {ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
    Sections = maps:get(sections, Explanation),

    ?assert(is_list(Sections)),
    ?assert(length(Sections) > 0),

    lists:foreach(
        fun(Section) ->
            ?assert(maps:is_key(title, Section)),
            ?assert(maps:is_key(content, Section)),
            ?assert(is_binary(maps:get(title, Section))),
            ?assert(is_binary(maps:get(content, Section)))
        end,
        Sections
    ).

test_analogies() ->
    {ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
    Analogies = maps:get(analogies, Explanation),

    ?assert(is_list(Analogies)),
    ?assert(length(Analogies) > 0),
    ?assert(lists:all(fun(A) -> is_binary(A) andalso byte_size(A) > 0 end, Analogies)).

test_trade_offs() ->
    {ok, Explanation} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
    TradeOffs = maps:get(trade_offs, Explanation),

    ?assert(maps:is_key(pros, TradeOffs)),
    ?assert(maps:is_key(cons, TradeOffs)),

    Pros = maps:get(pros, TradeOffs),
    Cons = maps:get(cons, TradeOffs),

    ?assert(is_list(Pros)),
    ?assert(is_list(Cons)),
    ?assert(length(Pros) > 0),
    ?assert(length(Cons) > 0).

test_category_coverage() ->
    AllCategories = [core_concepts, design_decisions, comparisons],

    lists:foreach(
        fun(Category) ->
            Explanations = tcps_diataxis_explain:list_by_category(Category),
            ?assertEqual(5, length(Explanations)),
            ?assert(lists:all(fun(E) -> maps:get(category, E) =:= Category end, Explanations))
        end,
        AllCategories
    ).

test_difficulty_levels() ->
    AllDifficulties = [beginner, intermediate, advanced],

    Explanations = tcps_diataxis_explain:list_explanations(),

    lists:foreach(
        fun(Difficulty) ->
            WithDifficulty = lists:filter(
                fun(E) -> maps:get(difficulty, E) =:= Difficulty end,
                Explanations
            ),
            ?assert(length(WithDifficulty) > 0)
        end,
        AllDifficulties
    ).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

difficulty_to_int(beginner) -> 1;
difficulty_to_int(intermediate) -> 2;
difficulty_to_int(advanced) -> 3.

%%%===================================================================
%%% Complex Scenarios
%%%===================================================================

complex_test_() ->
    [
     {"Learning path progression", fun test_learning_path/0},
     {"Cross-category relationships", fun test_cross_category/0},
     {"Reading time accuracy", fun test_reading_time/0}
    ].

test_learning_path() ->
    % Get a beginner explanation
    {ok, BeginnerExpl} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
    ?assertEqual(beginner, maps:get(difficulty, BeginnerExpl)),

    Path = tcps_diataxis_explain:get_explanation_path(<<"why_tcps">>),

    % Path should start with beginner
    FirstDifficulty = maps:get(difficulty, hd(Path)),
    ?assertEqual(beginner, FirstDifficulty),

    % All explanations in path should be same category
    Category = maps:get(category, BeginnerExpl),
    ?assert(lists:all(fun(E) -> maps:get(category, E) =:= Category end, Path)).

test_cross_category() ->
    % Get related explanations from different categories
    {ok, CoreExpl} = tcps_diataxis_explain:get_explanation(<<"why_tcps">>),
    Related = tcps_diataxis_explain:get_related_explanations(<<"why_tcps">>),

    % Some related should be from different categories (cross-references)
    CoreCategory = maps:get(category, CoreExpl),
    OtherCategories = lists:any(
        fun(E) -> maps:get(category, E) =/= CoreCategory end,
        Related
    ),
    ?assert(OtherCategories orelse length(Related) =:= 0).

test_reading_time() ->
    Explanations = tcps_diataxis_explain:list_explanations(),

    lists:foreach(
        fun(Explanation) ->
            ReadingTime = maps:get(reading_time_minutes, Explanation),
            ?assert(is_integer(ReadingTime)),
            ?assert(ReadingTime > 0),
            ?assert(ReadingTime < 60) % Reasonable upper bound
        end,
        Explanations
    ).
