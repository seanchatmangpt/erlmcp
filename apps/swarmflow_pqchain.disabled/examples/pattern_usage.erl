%%%-------------------------------------------------------------------
%%% @doc pattern_usage - Examples of using Van der Aalst workflow patterns
%%%
%%% Demonstrates how to:
%%% 1. Retrieve and compile patterns
%%% 2. Execute patterns with pqc_pattern_net
%%% 3. Compose patterns into complete workflows
%%% 4. Customize patterns for specific use cases
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pattern_usage).

-export([
    example_sequence/0,
    example_parallel/0,
    example_choice/0,
    example_discriminator/0,
    example_multiple_instance/0,
    example_loop/0,
    example_composed_workflow/0
]).

%%--------------------------------------------------------------------
%% Basic Pattern Examples
%%--------------------------------------------------------------------

%% @doc Example: P1 Sequence pattern
example_sequence() ->
    %% 1. Retrieve pattern
    Net = pqc_patterns_43:pattern(1),

    %% 2. Compile (adds reachability caches for OR-join)
    Compiled = pqc_pattern_net:compile(Net),

    %% 3. Get initial marking
    M0 = pqc_pattern_net:initial_marking(Compiled),
    io:format("Initial marking: ~p~n", [M0]),

    %% 4. Check enabled transitions
    Enabled = pqc_pattern_net:enabled(Compiled, M0),
    io:format("Enabled transitions: ~p~n", [Enabled]),

    %% 5. Fire transition 'a'
    {ok, M1, Effects1} = pqc_pattern_net:fire(Compiled, a, M0),
    io:format("After firing 'a': ~p~nEffects: ~p~n", [M1, Effects1]),

    %% 6. Fire transition 'b'
    {ok, M2, Effects2} = pqc_pattern_net:fire(Compiled, b, M1),
    io:format("After firing 'b': ~p~nEffects: ~p~n", [M2, Effects2]),

    %% 7. Check termination
    IsTerminated = pqc_pattern_net:is_implicitly_terminated(Compiled, M2),
    io:format("Is terminated: ~p~n", [IsTerminated]),

    {ok, M2}.

%% @doc Example: P2 Parallel Split pattern
example_parallel() ->
    Net = pqc_patterns_43:pattern(2),
    Compiled = pqc_pattern_net:compile(Net),
    M0 = pqc_pattern_net:initial_marking(Compiled),

    io:format("=== P2: Parallel Split ===~n"),
    io:format("Initial: 1 token in p1~n"),

    %% Fire split transition - creates 3 parallel threads
    {ok, M1, Effects} = pqc_pattern_net:fire(Compiled, split, M0),
    io:format("After split: ~p~n", [Effects]),
    io:format("Tokens in p2: ~p~n", [pqc_pattern_net:token_count(p2, M1)]),
    io:format("Tokens in p3: ~p~n", [pqc_pattern_net:token_count(p3, M1)]),
    io:format("Tokens in p4: ~p~n", [pqc_pattern_net:token_count(p4, M1)]),

    {ok, M1}.

%% @doc Example: P4 Exclusive Choice pattern
example_choice() ->
    Net = pqc_patterns_43:pattern(4),
    Compiled = pqc_pattern_net:compile(Net),
    M0 = pqc_pattern_net:initial_marking(Compiled),

    io:format("=== P4: Exclusive Choice ===~n"),

    %% Set choice to 'b' via metadata
    M1 = M0#{meta => #{choice => b}},

    Enabled = pqc_pattern_net:enabled(Compiled, M1),
    io:format("Enabled transitions (choice=b): ~p~n", [Enabled]),

    %% Fire choice_b
    {ok, M2, Effects} = pqc_pattern_net:fire(Compiled, choice_b, M1),
    io:format("Chosen branch: b~nEffects: ~p~n", [Effects]),
    io:format("Token now in p3: ~p~n", [pqc_pattern_net:token_count(p3, M2)]),

    {ok, M2}.

%% @doc Example: P9 Structured Discriminator pattern
example_discriminator() ->
    Net = pqc_patterns_43:pattern(9),
    Compiled = pqc_pattern_net:compile(Net),
    M0 = pqc_pattern_net:initial_marking(Compiled),

    io:format("=== P9: Structured Discriminator ===~n"),
    io:format("Initial: tokens in p1, p2, p3~n"),

    %% Discriminator waits for first completion, ignores others
    {ok, M1, Effects} = pqc_pattern_net:fire(Compiled, discriminator, M0),
    io:format("First branch completed~nEffects: ~p~n", [Effects]),
    io:format("Token in p4: ~p~n", [pqc_pattern_net:token_count(p4, M1)]),

    {ok, M1}.

%% @doc Example: P12 Multiple Instances without Synchronization
example_multiple_instance() ->
    Net = pqc_patterns_43:pattern(12),
    Compiled = pqc_pattern_net:compile(Net),
    M0 = pqc_pattern_net:initial_marking(Compiled),

    io:format("=== P12: Multiple Instances without Sync ===~n"),

    %% Fire MI split - creates 3 instances
    {ok, M1, Effects} = pqc_pattern_net:fire(Compiled, mi_split, M0),
    io:format("Created multiple instances~nEffects: ~p~n", [Effects]),

    %% Check MI tokens
    Tokens = pqc_pattern_net:place_tokens(p2, M1),
    io:format("Number of instances: ~p~n", [length(Tokens)]),
    io:format("Instance tokens: ~p~n", [Tokens]),

    {ok, M1}.

%% @doc Example: P21 Structured Loop pattern
example_loop() ->
    Net = pqc_patterns_43:pattern(21),
    Compiled = pqc_pattern_net:compile(Net),
    M0 = pqc_pattern_net:initial_marking(Compiled),

    io:format("=== P21: Structured Loop ===~n"),

    %% Set max iterations in metadata
    M1 = M0#{meta => #{max_iterations => 3, iteration => 0}},

    %% Enter loop
    {ok, M2, _} = pqc_pattern_net:fire(Compiled, enter_loop, M1),
    io:format("Entered loop~n"),

    %% Execute loop body 3 times
    M3 = loop_iteration(Compiled, M2, 1),
    M4 = loop_iteration(Compiled, M3, 2),
    M5 = loop_iteration(Compiled, M4, 3),

    %% Exit loop
    {ok, M6, _} = pqc_pattern_net:fire(Compiled, exit_loop, M5),
    io:format("Exited loop after 3 iterations~n"),

    {ok, M6}.

loop_iteration(Net, M, Iter) ->
    {ok, M1, _} = pqc_pattern_net:fire(Net, loop_body, M),
    io:format("Loop iteration ~p completed~n", [Iter]),
    M1.

%%--------------------------------------------------------------------
%% Advanced Example: Composed Workflow
%%--------------------------------------------------------------------

%% @doc Example: Compose multiple patterns into a complete workflow
example_composed_workflow() ->
    io:format("=== Composed Workflow Example ===~n"),
    io:format("Workflow: Sequence -> Parallel -> Sync -> Choice~n~n"),

    %% This demonstrates conceptual composition
    %% In practice, you would build a single net with these patterns

    %% Step 1: Execute sequence (P1)
    io:format("Step 1: Sequence~n"),
    {ok, _M1} = example_sequence(),

    %% Step 2: Execute parallel split (P2)
    io:format("~nStep 2: Parallel Split~n"),
    {ok, _M2} = example_parallel(),

    %% Step 3: Execute synchronization (P3)
    io:format("~nStep 3: Synchronization~n"),
    Net3 = pqc_patterns_43:pattern(3),
    Compiled3 = pqc_pattern_net:compile(Net3),
    M3_0 = pqc_pattern_net:initial_marking(Compiled3),
    {ok, _M3} = pqc_pattern_net:fire(Compiled3, sync, M3_0),

    %% Step 4: Execute choice (P4)
    io:format("~nStep 4: Exclusive Choice~n"),
    {ok, _M4} = example_choice(),

    io:format("~nComposed workflow completed!~n"),
    ok.

%%--------------------------------------------------------------------
%% Pattern Customization Examples
%%--------------------------------------------------------------------

%% @doc Customize pattern with different initial marking
custom_initial_marking() ->
    %% Get base pattern
    Net = pqc_patterns_43:pattern(1),

    %% Customize initial marking (start at p2 instead of p1)
    CustomNet = pqc_patterns_43:with_initial_marking(Net, #{p2 => 1}),

    Compiled = pqc_pattern_net:compile(CustomNet),
    M0 = pqc_pattern_net:initial_marking(Compiled),

    io:format("Custom initial marking: ~p~n", [M0]),
    {ok, CustomNet}.

%% @doc Add custom metadata to pattern
custom_metadata() ->
    Net = pqc_patterns_43:pattern(1),

    %% Add custom metadata
    CustomNet = pqc_patterns_43:with_metadata(Net, #{
        workflow_id => <<"order_processing">>,
        version => <<"1.0.0">>,
        owner => <<"team_alpha">>
    }),

    Meta = maps:get(metadata, CustomNet),
    io:format("Custom metadata: ~p~n", [Meta]),
    {ok, CustomNet}.

%%--------------------------------------------------------------------
%% Pattern Discovery Examples
%%--------------------------------------------------------------------

%% @doc List all patterns in a category
list_by_category() ->
    io:format("=== Patterns by Category ===~n~n"),

    Categories = pqc_patterns_43:categories(),
    lists:foreach(
        fun(Category) ->
            Patterns = pqc_patterns_43:patterns_by_category(Category),
            io:format("~p: ~p patterns~n", [Category, length(Patterns)]),
            lists:foreach(
                fun(N) ->
                    Name = pqc_patterns_43:pattern_name(N),
                    io:format("  P~p: ~s~n", [N, Name])
                end,
                Patterns
            ),
            io:format("~n")
        end,
        Categories
    ).

%% @doc Show pattern descriptions
show_descriptions() ->
    io:format("=== Pattern Descriptions ===~n~n"),

    %% Show first 5 patterns as examples
    lists:foreach(
        fun(N) ->
            Name = pqc_patterns_43:pattern_name(N),
            Description = pqc_patterns_43:pattern_description(N),
            io:format("P~p: ~s~n~s~n~n", [N, Name, Description])
        end,
        lists:seq(1, 5)
    ).

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

%% @doc Run all examples
run_all_examples() ->
    io:format("~n=================================================~n"),
    io:format("Van der Aalst Workflow Patterns - Usage Examples~n"),
    io:format("=================================================~n~n"),

    example_sequence(),
    io:format("~n"),
    example_parallel(),
    io:format("~n"),
    example_choice(),
    io:format("~n"),
    example_discriminator(),
    io:format("~n"),
    example_multiple_instance(),
    io:format("~n"),
    example_loop(),
    io:format("~n"),
    example_composed_workflow(),
    io:format("~n"),
    list_by_category(),
    io:format("~n"),
    show_descriptions(),

    io:format("~nAll examples completed successfully!~n"),
    ok.
