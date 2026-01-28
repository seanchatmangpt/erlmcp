%%%-------------------------------------------------------------------
%%% @doc Unit tests for tcps_diataxis_howto module
%%% Comprehensive test coverage for how-to guide retrieval and execution.
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_diataxis_howto_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

howto_test_() ->
    [
     {"Get how-to guide by ID", fun test_get_guide/0},
     {"Get non-existent guide", fun test_get_nonexistent_guide/0},
     {"List all guides", fun test_list_guides/0},
     {"List guides by category", fun test_list_by_category/0},
     {"Search guides by keyword", fun test_search_guides/0},
     {"Get related guides", fun test_get_related_guides/0},
     {"Get guide difficulty", fun test_get_guide_difficulty/0},
     {"Validate guide structure", fun test_validate_guide/0},
     {"Guide has all required keys", fun test_guide_keys/0},
     {"Guide steps are well-formed", fun test_guide_steps/0},
     {"Success criteria present", fun test_success_criteria/0},
     {"Common pitfalls documented", fun test_common_pitfalls/0},
     {"Category-specific guides", fun test_category_guides/0},
     {"Difficulty levels", fun test_difficulty_levels/0},
     {"Prerequisites documented", fun test_prerequisites/0}
    ].

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_get_guide() ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>),
    ?assertEqual(<<"How to Configure Quality Gates">>, maps:get(title, Guide)),
    ?assertEqual(configuration, maps:get(category, Guide)),
    ?assertEqual(easy, maps:get(difficulty, Guide)),
    ?assert(maps:is_key(steps, Guide)),
    ?assert(maps:is_key(success_criteria, Guide)).

test_get_nonexistent_guide() ->
    ?assertEqual({error, not_found}, tcps_diataxis_howto:get_guide(<<"nonexistent">>)).

test_list_guides() ->
    Guides = tcps_diataxis_howto:list_guides(),
    ?assert(length(Guides) >= 6),
    ?assert(lists:all(fun(G) -> maps:is_key(id, G) end, Guides)),
    ?assert(lists:all(fun(G) -> maps:is_key(title, G) end, Guides)),
    ?assert(lists:all(fun(G) -> maps:is_key(category, G) end, Guides)).

test_list_by_category() ->
    Config = tcps_diataxis_howto:list_by_category(configuration),
    Troubleshoot = tcps_diataxis_howto:list_by_category(troubleshooting),
    Integration = tcps_diataxis_howto:list_by_category(integration),
    Optimization = tcps_diataxis_howto:list_by_category(optimization),

    ?assert(length(Config) >= 2),
    ?assert(length(Troubleshoot) >= 2),
    ?assert(length(Integration) >= 1),
    ?assert(length(Optimization) >= 1),

    ?assert(lists:all(fun(G) -> maps:get(category, G) =:= configuration end, Config)),
    ?assert(lists:all(fun(G) -> maps:get(category, G) =:= troubleshooting end, Troubleshoot)).

test_search_guides() ->
    Results1 = tcps_diataxis_howto:search_guides(<<"quality">>),
    ?assert(length(Results1) >= 2),

    Results2 = tcps_diataxis_howto:search_guides(<<"mcp">>),
    ?assert(length(Results2) >= 1),

    Results3 = tcps_diataxis_howto:search_guides(<<"andon">>),
    ?assert(length(Results3) >= 1),

    % Case insensitive search
    Results4 = tcps_diataxis_howto:search_guides(<<"QUALITY">>),
    ?assertEqual(length(Results1), length(Results4)).

test_get_related_guides() ->
    Related = tcps_diataxis_howto:get_related_guides(<<"configure_quality_gates">>),
    ?assertEqual(2, length(Related)),

    % Related guides should be valid
    lists:foreach(
        fun(Guide) ->
            ?assert(maps:is_key(id, Guide)),
            ?assert(maps:is_key(title, Guide))
        end,
        Related
    ).

test_get_guide_difficulty() ->
    ?assertEqual({ok, easy}, tcps_diataxis_howto:get_guide_difficulty(<<"configure_quality_gates">>)),
    ?assertEqual({ok, moderate}, tcps_diataxis_howto:get_guide_difficulty(<<"troubleshoot_quality_gates">>)),
    ?assertEqual({ok, hard}, tcps_diataxis_howto:get_guide_difficulty(<<"root_cause_analysis">>)),
    ?assertEqual({error, not_found}, tcps_diataxis_howto:get_guide_difficulty(<<"nonexistent">>)).

test_validate_guide() ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>),
    ?assertEqual(ok, tcps_diataxis_howto:validate_guide(Guide)),

    InvalidGuide = #{
        id => <<"test">>,
        title => <<"Test">>
        % Missing required keys
    },
    ?assertEqual({error, missing_required_keys}, tcps_diataxis_howto:validate_guide(InvalidGuide)).

test_guide_keys() ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>),
    RequiredKeys = [id, title, category, difficulty, summary, prerequisites, steps,
                    success_criteria, common_pitfalls, see_also, tags, estimated_minutes],
    lists:foreach(
        fun(Key) ->
            ?assert(maps:is_key(Key, Guide))
        end,
        RequiredKeys
    ).

test_guide_steps() ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>),
    Steps = maps:get(steps, Guide),

    ?assert(length(Steps) > 0),

    lists:foreach(
        fun(Step) ->
            ?assert(maps:is_key(action, Step)),
            ?assert(maps:is_key(expected_result, Step)),
            ?assert(is_binary(maps:get(action, Step))),
            ?assert(is_binary(maps:get(expected_result, Step)))
        end,
        Steps
    ).

test_success_criteria() ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>),
    Criteria = maps:get(success_criteria, Guide),

    ?assert(is_list(Criteria)),
    ?assert(length(Criteria) > 0),
    ?assert(lists:all(fun(C) -> is_binary(C) andalso byte_size(C) > 0 end, Criteria)).

test_common_pitfalls() ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>),
    Pitfalls = maps:get(common_pitfalls, Guide),

    ?assert(is_list(Pitfalls)),
    ?assert(length(Pitfalls) > 0),
    ?assert(lists:all(fun(P) -> is_binary(P) end, Pitfalls)).

test_category_guides() ->
    AllCategories = [configuration, troubleshooting, optimization, integration],

    lists:foreach(
        fun(Category) ->
            Guides = tcps_diataxis_howto:list_by_category(Category),
            ?assert(length(Guides) > 0),
            ?assert(lists:all(fun(G) -> maps:get(category, G) =:= Category end, Guides))
        end,
        AllCategories
    ).

test_difficulty_levels() ->
    AllDifficulties = [easy, moderate, hard],

    lists:foreach(
        fun(Difficulty) ->
            Guides = tcps_diataxis_howto:list_guides(),
            GuidesWithDifficulty = lists:filter(
                fun(G) -> maps:get(difficulty, G) =:= Difficulty end,
                Guides
            ),
            ?assert(length(GuidesWithDifficulty) > 0)
        end,
        AllDifficulties
    ).

test_prerequisites() ->
    {ok, Guide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>),
    Prerequisites = maps:get(prerequisites, Guide),

    ?assert(is_list(Prerequisites)),
    ?assert(lists:all(fun(P) -> is_binary(P) end, Prerequisites)).

%%%===================================================================
%%% Integration and Complex Scenarios
%%%===================================================================

integration_test_() ->
    [
     {"Search and retrieve workflow", fun test_search_and_retrieve/0},
     {"Follow related guides", fun test_follow_related_guides/0},
     {"Progressive difficulty", fun test_progressive_difficulty/0}
    ].

test_search_and_retrieve() ->
    % Search for quality-related guides
    Results = tcps_diataxis_howto:search_guides(<<"quality">>),
    ?assert(length(Results) > 0),

    % Retrieve first result
    FirstResult = hd(Results),
    GuideId = maps:get(id, FirstResult),
    {ok, RetrievedGuide} = tcps_diataxis_howto:get_guide(GuideId),

    ?assertEqual(GuideId, maps:get(id, RetrievedGuide)),
    ?assertEqual(maps:get(title, FirstResult), maps:get(title, RetrievedGuide)).

test_follow_related_guides() ->
    % Start with configuration guide
    {ok, ConfigGuide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>),
    RelatedIds = maps:get(see_also, ConfigGuide),
    ?assert(length(RelatedIds) > 0),

    % Follow related guides
    Related = tcps_diataxis_howto:get_related_guides(<<"configure_quality_gates">>),
    ?assertEqual(length(RelatedIds), length(Related)),

    % All related guides should be retrievable
    lists:foreach(
        fun(RelId) ->
            ?assertMatch({ok, _}, tcps_diataxis_howto:get_guide(RelId))
        end,
        RelatedIds
    ).

test_progressive_difficulty() ->
    % Easy guides should have fewer steps than hard guides (generally)
    {ok, EasyGuide} = tcps_diataxis_howto:get_guide(<<"configure_quality_gates">>),
    {ok, HardGuide} = tcps_diataxis_howto:get_guide(<<"root_cause_analysis">>),

    EasySteps = length(maps:get(steps, EasyGuide)),
    HardSteps = length(maps:get(steps, HardGuide)),

    ?assert(HardSteps >= EasySteps),

    EasyDuration = maps:get(estimated_minutes, EasyGuide),
    HardDuration = maps:get(estimated_minutes, HardGuide),

    ?assert(HardDuration > EasyDuration).
