#!/usr/bin/env escript
%%% -*- erlang -*-
%%%-------------------------------------------------------------------
%%% @doc Quick verification test for TCPS How-to Guides module
%%% @end
%%%-------------------------------------------------------------------

main(_) ->
    %% Add code paths
    code:add_path("ebin"),

    %% Compile modules
    compile:file("src/tcps_mcp_diataxis/tcps_howto_recipes.erl", [{outdir, "ebin"}]),

    io:format("~n=== TCPS How-to Guides Verification ===~n~n"),

    %% Test 1: Load all guides
    io:format("Test 1: Loading all guides...~n"),
    Guides = tcps_howto_recipes:load_all_guides(),
    GuideCount = maps:size(Guides),
    io:format("  ✓ Loaded ~p guides~n", [GuideCount]),

    %% Test 2: Verify guide structure
    io:format("~nTest 2: Verifying guide structures...~n"),
    maps:foreach(
        fun(Id, Guide) ->
            verify_guide_structure(Id, Guide)
        end,
        Guides
    ),
    io:format("  ✓ All guides have valid structure~n"),

    %% Test 3: Check categories
    io:format("~nTest 3: Checking categories...~n"),
    Categories = lists:usort([maps:get(category, G) || G <- maps:values(Guides)]),
    io:format("  ✓ Found ~p categories: ~p~n", [length(Categories), Categories]),

    %% Test 4: Check difficulty levels
    io:format("~nTest 4: Checking difficulty distribution...~n"),
    Beginner = length([G || G <- maps:values(Guides), maps:get(difficulty, G) =:= beginner]),
    Intermediate = length([G || G <- maps:values(Guides), maps:get(difficulty, G) =:= intermediate]),
    Advanced = length([G || G <- maps:values(Guides), maps:get(difficulty, G) =:= advanced]),
    io:format("  ✓ Beginner: ~p, Intermediate: ~p, Advanced: ~p~n",
              [Beginner, Intermediate, Advanced]),

    %% Test 5: Verify step structure
    io:format("~nTest 5: Verifying step structures...~n"),
    TotalSteps = lists:sum([length(maps:get(steps, G)) || G <- maps:values(Guides)]),
    io:format("  ✓ Total steps across all guides: ~p~n", [TotalSteps]),

    %% Test 6: Check specific guide
    io:format("~nTest 6: Detailed check of 'quality_gates' guide...~n"),
    QGGuide = maps:get(quality_gates, Guides),
    io:format("  Title: ~s~n", [maps:get(title, QGGuide)]),
    io:format("  Category: ~p~n", [maps:get(category, QGGuide)]),
    io:format("  Difficulty: ~p~n", [maps:get(difficulty, QGGuide)]),
    io:format("  Duration: ~p minutes~n", [maps:get(duration, QGGuide)]),
    io:format("  Steps: ~p~n", [length(maps:get(steps, QGGuide))]),
    io:format("  Prerequisites: ~p~n", [length(maps:get(prerequisites, QGGuide))]),
    io:format("  Common pitfalls: ~p~n", [length(maps:get(common_pitfalls, QGGuide))]),
    io:format("  Related guides: ~p~n", [maps:get(related_guides, QGGuide)]),
    io:format("  Tags: ~p~n", [maps:get(tags, QGGuide)]),

    %% Test 7: List all guide titles
    io:format("~n=== All Available Guides ===~n"),
    SortedGuides = lists:sort(
        fun({_, #{title := T1}}, {_, #{title := T2}}) -> T1 =< T2 end,
        maps:to_list(Guides)
    ),
    lists:foreach(
        fun({Id, #{title := Title, difficulty := Diff, duration := Dur}}) ->
            io:format("  [~-20s] ~s (~p, ~pm)~n", [atom_to_list(Id), Title, Diff, Dur])
        end,
        SortedGuides
    ),

    io:format("~n=== Verification Complete ===~n"),
    io:format("✓ All tests passed~n"),
    io:format("✓ ~p production-ready how-to guides available~n~n", [GuideCount]),
    ok.

verify_guide_structure(Id, Guide) ->
    RequiredFields = [id, title, category, difficulty, duration, problem,
                      prerequisites, steps, verification, common_pitfalls,
                      related_guides, tags],
    lists:foreach(
        fun(Field) ->
            case maps:is_key(Field, Guide) of
                true -> ok;
                false -> error({missing_field, Id, Field})
            end
        end,
        RequiredFields
    ),

    %% Verify steps have correct structure
    Steps = maps:get(steps, Guide),
    lists:foreach(
        fun(Step) ->
            case maps:is_key(number, Step) andalso
                 maps:is_key(description, Step) of
                true -> ok;
                false -> error({invalid_step, Id, Step})
            end
        end,
        Steps
    ).
