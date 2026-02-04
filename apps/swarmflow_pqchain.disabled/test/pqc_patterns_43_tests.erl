%%%-------------------------------------------------------------------
%%% @doc pqc_patterns_43_tests - EUnit tests for Van der Aalst patterns
%%%
%%% Tests that all 43 workflow patterns:
%%% 1. Can be retrieved via pattern/1
%%% 2. Are valid net structures
%%% 3. Can be compiled by pqc_pattern_net:compile/1
%%% 4. Have proper metadata (name, category, description)
%%% 5. Can be executed (enabled/fire)
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pqc_patterns_43_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test Suite Setup
%%--------------------------------------------------------------------

all_patterns_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"All 43 patterns exist", fun test_all_patterns_exist/0},
         {"All patterns have valid structure", fun test_pattern_structure/0},
         {"All patterns compile", fun test_patterns_compile/0},
         {"All patterns have metadata", fun test_pattern_metadata/0},
         {"Pattern categories are correct", fun test_categories/0},
         {"Pattern descriptions exist", fun test_descriptions/0},
         {"Basic patterns are executable", fun test_basic_execution/0}
     ]}.

setup() ->
    ok.

cleanup(_) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

%% @doc Test that all 43 patterns can be retrieved.
test_all_patterns_exist() ->
    AllPatterns = pqc_patterns_43:all_patterns(),
    ?assertEqual(43, length(AllPatterns)),
    ?assertEqual(lists:seq(1, 43), AllPatterns),

    %% Test each pattern can be retrieved
    lists:foreach(
        fun(N) ->
            Net = pqc_patterns_43:pattern(N),
            ?assertMatch(#{places := _, transitions := _, order := _}, Net),
            ?assert(is_map(Net))
        end,
        AllPatterns
    ).

%% @doc Test that all patterns have valid structure.
test_pattern_structure() ->
    lists:foreach(
        fun(N) ->
            Net = pqc_patterns_43:pattern(N),

            %% Required fields
            ?assertMatch(#{places := _}, Net),
            ?assertMatch(#{transitions := _}, Net),
            ?assertMatch(#{order := _}, Net),

            Places = maps:get(places, Net),
            Transitions = maps:get(transitions, Net),
            Order = maps:get(order, Net),

            %% Types
            ?assert(is_list(Places)),
            ?assert(is_map(Transitions)),
            ?assert(is_list(Order)),

            %% Order contains valid transition IDs
            lists:foreach(
                fun(Tid) ->
                    ?assert(maps:is_key(Tid, Transitions))
                end,
                Order
            ),

            %% Each transition has inputs and outputs
            maps:foreach(
                fun(_Tid, T) ->
                    ?assertMatch(#{inputs := _, outputs := _}, T),
                    Inputs = maps:get(inputs, T),
                    Outputs = maps:get(outputs, T),
                    ?assert(is_list(Inputs)),
                    ?assert(is_list(Outputs)),

                    %% Each arc has place and weight
                    lists:foreach(
                        fun(Arc) ->
                            ?assertMatch(#{place := _, weight := _}, Arc)
                        end,
                        Inputs ++ Outputs
                    )
                end,
                Transitions
            )
        end,
        lists:seq(1, 43)
    ).

%% @doc Test that all patterns compile successfully.
test_patterns_compile() ->
    lists:foreach(
        fun(N) ->
            Net = pqc_patterns_43:pattern(N),

            %% Should compile without error
            try
                Compiled = pqc_pattern_net:compile(Net),
                ?assert(is_map(Compiled)),

                %% Should have reachability caches
                ?assertMatch(#{reach := _, reach_rev := _}, Compiled)
            catch
                error:{bad_net, Reason} ->
                    ?debugFmt("Pattern ~p compilation failed: ~p", [N, Reason]),
                    ?assert(false)
            end
        end,
        lists:seq(1, 43)
    ).

%% @doc Test that all patterns have proper metadata.
test_pattern_metadata() ->
    lists:foreach(
        fun(N) ->
            Net = pqc_patterns_43:pattern(N),
            Meta = maps:get(metadata, Net, #{}),

            %% Must have pattern_id, pattern_name, category
            ?assertEqual(N, maps:get(pattern_id, Meta)),
            ?assert(is_binary(maps:get(pattern_name, Meta))),
            ?assert(is_atom(maps:get(category, Meta))),

            %% Metadata functions should work
            Name = pqc_patterns_43:pattern_name(N),
            Category = pqc_patterns_43:pattern_category(N),
            Description = pqc_patterns_43:pattern_description(N),

            ?assert(is_binary(Name)),
            ?assert(is_atom(Category)),
            ?assert(is_binary(Description)),
            ?assert(byte_size(Description) > 0)
        end,
        lists:seq(1, 43)
    ).

%% @doc Test that pattern categories are correct.
test_categories() ->
    Categories = pqc_patterns_43:categories(),

    ?assertEqual(11, length(Categories)),
    ?assert(lists:member(basic_control, Categories)),
    ?assert(lists:member(advanced_branching, Categories)),
    ?assert(lists:member(structural, Categories)),
    ?assert(lists:member(multiple_instance, Categories)),
    ?assert(lists:member(state_based, Categories)),
    ?assert(lists:member(cancellation, Categories)),
    ?assert(lists:member(iteration, Categories)),
    ?assert(lists:member(trigger, Categories)),
    ?assert(lists:member(advanced_sync, Categories)),
    ?assert(lists:member(advanced_concurrency, Categories)),
    ?assert(lists:member(termination, Categories)),

    %% Test patterns_by_category
    lists:foreach(
        fun(Category) ->
            Patterns = pqc_patterns_43:patterns_by_category(Category),
            ?assert(is_list(Patterns)),
            ?assert(length(Patterns) > 0),

            %% Each pattern in category should have that category
            lists:foreach(
                fun(N) ->
                    ?assertEqual(Category, pqc_patterns_43:pattern_category(N))
                end,
                Patterns
            )
        end,
        Categories
    ),

    %% Expected pattern counts per category
    ?assertEqual(5, length(pqc_patterns_43:patterns_by_category(basic_control))),
    ?assertEqual(4, length(pqc_patterns_43:patterns_by_category(advanced_branching))),
    ?assertEqual(2, length(pqc_patterns_43:patterns_by_category(structural))).

%% @doc Test that all pattern descriptions exist and are non-empty.
test_descriptions() ->
    lists:foreach(
        fun(N) ->
            Description = pqc_patterns_43:pattern_description(N),
            ?assert(is_binary(Description)),
            ?assert(byte_size(Description) > 20),  % Reasonably long description

            %% Check for common pattern description keywords
            DescLower = string:lowercase(binary_to_list(Description)),
            ?assert(
                string:find(DescLower, "activity") =/= nomatch orelse
                string:find(DescLower, "process") =/= nomatch orelse
                string:find(DescLower, "thread") =/= nomatch orelse
                string:find(DescLower, "branch") =/= nomatch orelse
                string:find(DescLower, "instance") =/= nomatch
            )
        end,
        lists:seq(1, 43)
    ).

%% @doc Test that basic patterns can be executed.
test_basic_execution() ->
    %% P1: Sequence
    Net1 = pqc_pattern_net:compile(pqc_patterns_43:pattern(1)),
    M1_0 = pqc_pattern_net:initial_marking(Net1),

    %% Should have token in p1
    ?assertEqual(1, pqc_pattern_net:token_count(p1, M1_0)),

    %% Transition 'a' should be enabled
    Enabled1 = pqc_pattern_net:enabled(Net1, M1_0),
    ?assert(lists:member(a, Enabled1)),

    %% Fire 'a'
    {ok, M1_1, _Effects1} = pqc_pattern_net:fire(Net1, a, M1_0),
    ?assertEqual(0, pqc_pattern_net:token_count(p1, M1_1)),
    ?assertEqual(1, pqc_pattern_net:token_count(p2, M1_1)),

    %% P2: Parallel Split
    Net2 = pqc_pattern_net:compile(pqc_patterns_43:pattern(2)),
    M2_0 = pqc_pattern_net:initial_marking(Net2),

    {ok, M2_1, _Effects2} = pqc_pattern_net:fire(Net2, split, M2_0),

    %% Should have tokens in all output places
    ?assertEqual(1, pqc_pattern_net:token_count(p2, M2_1)),
    ?assertEqual(1, pqc_pattern_net:token_count(p3, M2_1)),
    ?assertEqual(1, pqc_pattern_net:token_count(p4, M2_1)),

    %% P3: Synchronization
    Net3 = pqc_pattern_net:compile(pqc_patterns_43:pattern(3)),
    M3_0 = pqc_pattern_net:initial_marking(Net3),

    %% Should have tokens in p1, p2, p3
    ?assertEqual(1, pqc_pattern_net:token_count(p1, M3_0)),
    ?assertEqual(1, pqc_pattern_net:token_count(p2, M3_0)),
    ?assertEqual(1, pqc_pattern_net:token_count(p3, M3_0)),

    %% Sync transition should be enabled
    Enabled3 = pqc_pattern_net:enabled(Net3, M3_0),
    ?assert(lists:member(sync, Enabled3)),

    {ok, M3_1, _Effects3} = pqc_pattern_net:fire(Net3, sync, M3_0),
    ?assertEqual(1, pqc_pattern_net:token_count(p4, M3_1)),

    %% P11: Implicit Termination
    Net11 = pqc_pattern_net:compile(pqc_patterns_43:pattern(11)),
    M11_0 = pqc_pattern_net:initial_marking(Net11),

    {ok, M11_1, _} = pqc_pattern_net:fire(Net11, a, M11_0),
    {ok, M11_2, _} = pqc_pattern_net:fire(Net11, b, M11_1),

    %% Should be implicitly terminated
    ?assert(pqc_pattern_net:is_implicitly_terminated(Net11, M11_2)).

%%--------------------------------------------------------------------
%% Specific Pattern Tests
%%--------------------------------------------------------------------

%% Test P4: Exclusive Choice
exclusive_choice_test() ->
    Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(4)),
    M0 = pqc_pattern_net:initial_marking(Net),

    %% Set choice to 'a'
    M1 = M0#{meta => #{choice => a}},
    Enabled = pqc_pattern_net:enabled(Net, M1),
    ?assert(lists:member(choice_a, Enabled)),

    {ok, M2, _} = pqc_pattern_net:fire(Net, choice_a, M1),
    ?assertEqual(1, pqc_pattern_net:token_count(p2, M2)).

%% Test P9: Structured Discriminator
discriminator_test() ->
    Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(9)),
    M0 = pqc_pattern_net:initial_marking(Net),

    %% All three branches have tokens
    ?assertEqual(1, pqc_pattern_net:token_count(p1, M0)),
    ?assertEqual(1, pqc_pattern_net:token_count(p2, M0)),
    ?assertEqual(1, pqc_pattern_net:token_count(p3, M0)),

    %% Discriminator should be enabled
    Enabled = pqc_pattern_net:enabled(Net, M0),
    ?assert(lists:member(discriminator, Enabled)),

    %% Fire discriminator - should take first token, ignore rest
    {ok, M1, Effects} = pqc_pattern_net:fire(Net, discriminator, M0),
    ?assertEqual(1, pqc_pattern_net:token_count(p4, M1)),

    %% Check discriminator effects
    ?assert(lists:any(fun({discriminator_closed, _}) -> true; (_) -> false end, Effects)).

%% Test P12: Multiple Instances without Synchronization
mi_without_sync_test() ->
    Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(12)),
    M0 = pqc_pattern_net:initial_marking(Net),

    {ok, M1, Effects} = pqc_pattern_net:fire(Net, mi_split, M0),

    %% Should have 3 tokens in p2 (3 instances)
    ?assertEqual(3, pqc_pattern_net:token_count(p2, M1)),

    %% Check MI effects
    ?assert(lists:any(fun({split, mi, 3, _, _}) -> true; (_) -> false end, Effects)).

%% Test P16: Deferred Choice
deferred_choice_test() ->
    Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(16)),
    M0 = pqc_pattern_net:initial_marking(Net),

    %% Inject transient event for choice_a
    M1 = pqc_pattern_net:inject_event(M0, transient, <<"event_a">>, true),

    Enabled = pqc_pattern_net:enabled(Net, M1),
    ?assert(lists:member(choice_a, Enabled)),

    {ok, M2, _} = pqc_pattern_net:fire(Net, choice_a, M1),
    ?assertEqual(1, pqc_pattern_net:token_count(p2, M2)).

%% Test P41: Thread Split
thread_split_test() ->
    Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(41)),
    M0 = pqc_pattern_net:initial_marking(Net),

    {ok, M1, Effects} = pqc_pattern_net:fire(Net, thread_split, M0),

    %% Should have 3 tokens in p2
    ?assertEqual(3, pqc_pattern_net:token_count(p2, M1)),

    %% Check thread split effects
    ?assert(lists:any(fun({split, thread, 3, _}) -> true; (_) -> false end, Effects)).

%%--------------------------------------------------------------------
%% Helper Construction Tests
%%--------------------------------------------------------------------

with_initial_marking_test() ->
    Net = pqc_patterns_43:pattern(1),
    Modified = pqc_patterns_43:with_initial_marking(Net, #{p2 => 1, p3 => 1}),

    ?assertEqual(#{p2 => 1, p3 => 1}, maps:get(initial_marking, Modified)).

with_metadata_test() ->
    Net = pqc_patterns_43:pattern(1),
    Modified = pqc_patterns_43:with_metadata(Net, #{custom_key => <<"custom_value">>}),

    Meta = maps:get(metadata, Modified),
    ?assertEqual(<<"custom_value">>, maps:get(custom_key, Meta)),

    %% Original metadata should still be present
    ?assertEqual(1, maps:get(pattern_id, Meta)).
