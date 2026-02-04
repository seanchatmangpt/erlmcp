%%%-------------------------------------------------------------------
%%% @doc Heuristics Miner Usage Examples
%%%
%%% Practical examples demonstrating the Heuristics Miner for various
%%% real-world process discovery scenarios.
%%%
%%% Examples:
%%% - Order fulfillment process
%%% - Loan application process
%%% - Software deployment workflow
%%% - Healthcare patient journey
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(heuristics_miner_example).

-include("pqchain.hrl").
-include_lib("swarmflow_os/include/swarmflow.hrl").

%% API exports
-export([
    example_order_fulfillment/0,
    example_loan_application/0,
    example_deployment_workflow/0,
    example_with_noise/0,
    example_loop_detection/0,
    print_net_summary/1
]).

%%====================================================================
%% Example 1: Order Fulfillment Process
%%====================================================================

example_order_fulfillment() ->
    io:format("~n=== Order Fulfillment Process Discovery ===~n~n"),

    %% Event log from an e-commerce system
    %% Process: receive_order -> check_inventory -> (ship | backorder) -> invoice -> close
    Traces = [
        % Normal flow: in stock
        [receive_order, check_inventory, ship, invoice, close],
        [receive_order, check_inventory, ship, invoice, close],
        [receive_order, check_inventory, ship, invoice, close],

        % Out of stock: backorder
        [receive_order, check_inventory, backorder, ship, invoice, close],
        [receive_order, check_inventory, backorder, ship, invoice, close],

        % Express shipping
        [receive_order, check_inventory, ship, invoice, close],

        % Some noise (data quality issue)
        [receive_order, ship, invoice, close]  % Missing check_inventory (1 occurrence)
    ],

    io:format("Event log contains ~p traces~n", [length(Traces)]),

    %% Configure to handle the noise
    Config = #heuristics_config{
        dependency_threshold = 0.7,
        positive_observations = 2  % Filter single occurrences
    },

    %% Discover the process
    {ok, Net} = pqc_heuristics_miner:discover(Traces, Config),

    io:format("~nDiscovered process model:~n"),
    print_net_summary(Net),

    %% Analyze dependency graph
    DepGraph = pqc_heuristics_miner:dependency_graph(Traces, Config),
    io:format("~nDependency Analysis:~n"),
    print_dependency_graph(DepGraph),

    Net.

%%====================================================================
%% Example 2: Loan Application Process
%%====================================================================

example_loan_application() ->
    io:format("~n=== Loan Application Process Discovery ===~n~n"),

    %% Loan application process with multiple paths
    Traces = [
        % Approved quickly (good credit)
        [submit, credit_check, approve, disburse],
        [submit, credit_check, approve, disburse],
        [submit, credit_check, approve, disburse],

        % Requires manual review
        [submit, credit_check, manual_review, approve, disburse],
        [submit, credit_check, manual_review, approve, disburse],

        % Rejected
        [submit, credit_check, reject],
        [submit, credit_check, manual_review, reject],

        % Additional documentation requested
        [submit, credit_check, request_docs, credit_check, approve, disburse],
        [submit, credit_check, request_docs, credit_check, reject]
    ],

    io:format("Loan applications: ~p traces~n", [length(Traces)]),

    %% Discover with default config
    {ok, Net} = pqc_heuristics_miner:discover(Traces),

    io:format("~nDiscovered loan process:~n"),
    print_net_summary(Net),

    %% Detect loops (credit_check can repeat after request_docs)
    Loops = pqc_heuristics_miner:detect_loops(Traces, #heuristics_config{}),
    io:format("~nDetected loops: ~p~n", [Loops]),

    Net.

%%====================================================================
%% Example 3: Software Deployment Workflow
%%====================================================================

example_deployment_workflow() ->
    io:format("~n=== Software Deployment Workflow Discovery ===~n~n"),

    %% CI/CD deployment process
    Traces = [
        % Successful deployment
        [commit, build, test, deploy_staging, deploy_prod],
        [commit, build, test, deploy_staging, deploy_prod],

        % Failed tests, fixed, retry
        [commit, build, test, commit, build, test, deploy_staging, deploy_prod],

        % Hotfix (skip staging)
        [commit, build, test, deploy_prod],

        % Rollback scenario
        [commit, build, test, deploy_staging, deploy_prod, rollback],

        % Multiple environments
        [commit, build, test, deploy_dev, deploy_staging, deploy_prod],
        [commit, build, test, deploy_dev, deploy_staging, deploy_prod]
    ],

    io:format("Deployment traces: ~p~n", [length(Traces)]),

    %% More lenient config to capture variations
    Config = #heuristics_config{
        dependency_threshold = 0.6,
        positive_observations = 1,
        loop_length_one = 0.5
    },

    {ok, Net} = pqc_heuristics_miner:discover(Traces, Config),

    io:format("~nDiscovered deployment workflow:~n"),
    print_net_summary(Net),

    %% Check for AND/XOR splits
    DepGraph = pqc_heuristics_miner:dependency_graph(Traces, Config),
    io:format("~nSplit analysis for 'test':~n"),
    {SplitType, Successors} = pqc_heuristics_miner:split_mining(DepGraph, test),
    io:format("  Type: ~p~n", [SplitType]),
    io:format("  Successors: ~p~n", [Successors]),

    Net.

%%====================================================================
%% Example 4: Handling Noisy Event Logs
%%====================================================================

example_with_noise() ->
    io:format("~n=== Noise Handling Example ===~n~n"),

    %% Heavily noisy event log (20% noise)
    Traces = [
        % Main process (80%)
        [a, b, c, d],
        [a, b, c, d],
        [a, b, c, d],
        [a, b, c, d],
        [a, b, c, d],
        [a, b, c, d],
        [a, b, c, d],
        [a, b, c, d],

        % Noise: missing steps
        [a, c, d],
        [a, b, d],

        % Noise: extra steps
        [a, x, b, c, d],
        [a, b, y, c, d],

        % Noise: wrong order
        [a, c, b, d]
    ],

    io:format("Total traces: ~p (includes noise)~n", [length(Traces)]),

    %% Strict configuration
    StrictConfig = #heuristics_config{
        dependency_threshold = 0.85,
        positive_observations = 3,
        relative_to_best = 0.05
    },

    {ok, StrictNet} = pqc_heuristics_miner:discover(Traces, StrictConfig),
    io:format("~nStrict configuration result:~n"),
    print_net_summary(StrictNet),

    %% Lenient configuration
    LenientConfig = #heuristics_config{
        dependency_threshold = 0.3,
        positive_observations = 1,
        relative_to_best = 0.3
    },

    {ok, LenientNet} = pqc_heuristics_miner:discover(Traces, LenientConfig),
    io:format("~nLenient configuration result:~n"),
    print_net_summary(LenientNet),

    io:format("~nComparison:~n"),
    io:format("  Strict: ~p transitions~n",
              [maps:size(StrictNet#swf_net.transitions)]),
    io:format("  Lenient: ~p transitions~n",
              [maps:size(LenientNet#swf_net.transitions)]),

    StrictNet.

%%====================================================================
%% Example 5: Loop Detection
%%====================================================================

example_loop_detection() ->
    io:format("~n=== Loop Detection Example ===~n~n"),

    %% Process with various loop types
    Traces = [
        % Length-1 loop (retry)
        [start, process, retry, retry, retry, complete],
        [start, process, retry, complete],
        [start, process, complete],

        % Length-2 loop (back-and-forth)
        [start, process, review, revise, review, complete],
        [start, process, review, revise, review, revise, review, complete],
        [start, process, review, complete]
    ],

    io:format("Traces with loops: ~p~n", [length(Traces)]),

    Config = #heuristics_config{
        loop_length_one = 0.5,
        loop_length_two = 0.5
    },

    %% Detect loops
    Loops = pqc_heuristics_miner:detect_loops(Traces, Config),
    io:format("~nDetected loops:~n"),
    lists:foreach(
        fun({Activity, LoopType}) ->
            io:format("  ~p: ~p~n", [Activity, LoopType])
        end,
        Loops
    ),

    %% Discover full model
    {ok, Net} = pqc_heuristics_miner:discover(Traces, Config),
    io:format("~nDiscovered model with loops:~n"),
    print_net_summary(Net),

    Net.

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Print summary of discovered Petri net
print_net_summary(#swf_net{} = Net) ->
    io:format("  Network ID: ~s~n", [Net#swf_net.id]),
    io:format("  Name: ~s~n", [Net#swf_net.name]),
    io:format("  Version: ~s~n", [Net#swf_net.version]),

    %% Places
    Places = Net#swf_net.places,
    io:format("~n  Places (~p):~n", [maps:size(Places)]),
    maps:foreach(
        fun(PlaceId, Place) ->
            io:format("    - ~s (tokens: ~p)~n",
                     [PlaceId, Place#swf_place.tokens])
        end,
        Places
    ),

    %% Transitions
    Transitions = Net#swf_net.transitions,
    io:format("~n  Transitions (~p):~n", [maps:size(Transitions)]),
    maps:foreach(
        fun(TransId, Trans) ->
            io:format("    - ~s (~p)~n",
                     [TransId, Trans#swf_transition.kind])
        end,
        Transitions
    ),

    %% Arcs
    Arcs = Net#swf_net.arcs,
    io:format("~n  Arcs (~p):~n", [length(Arcs)]),
    lists:foreach(
        fun(Arc) ->
            io:format("    ~s -> ~s (weight: ~p)~n",
                     [Arc#swf_arc.source,
                      Arc#swf_arc.target,
                      Arc#swf_arc.weight])
        end,
        Arcs
    ),

    %% Initial marking
    InitialMarking = Net#swf_net.initial_marking,
    io:format("~n  Initial marking: ~p~n", [InitialMarking]),

    %% Final places
    FinalPlaces = Net#swf_net.final_places,
    io:format("  Final places: ~p~n", [FinalPlaces]),

    ok.

%% @doc Print dependency graph analysis
print_dependency_graph(#dependency_graph{} = DepGraph) ->
    Activities = DepGraph#dependency_graph.activities,
    io:format("  Activities: ~p~n", [ordsets:to_list(Activities)]),

    Dependencies = DepGraph#dependency_graph.dependencies,
    io:format("~n  Dependencies:~n"),
    maps:foreach(
        fun(Source, Targets) ->
            case maps:size(Targets) of
                0 ->
                    io:format("    ~p -> (none)~n", [Source]);
                _ ->
                    maps:foreach(
                        fun(Target, Strength) ->
                            io:format("    ~p -> ~p: ~.3f~n",
                                     [Source, Target, Strength])
                        end,
                        Targets
                    )
            end
        end,
        Dependencies
    ),

    StartActs = DepGraph#dependency_graph.start_activities,
    io:format("~n  Start activities: ~p~n", [maps:to_list(StartActs)]),

    EndActs = DepGraph#dependency_graph.end_activities,
    io:format("  End activities: ~p~n", [maps:to_list(EndActs)]),

    ok.

%%====================================================================
%% Running All Examples
%%====================================================================

%% @doc Run all examples (for demonstration)
run_all_examples() ->
    io:format("~n╔════════════════════════════════════════════════════╗~n"),
    io:format("║  Heuristics Miner - Practical Examples            ║~n"),
    io:format("╚════════════════════════════════════════════════════╝~n"),

    example_order_fulfillment(),
    timer:sleep(1000),

    example_loan_application(),
    timer:sleep(1000),

    example_deployment_workflow(),
    timer:sleep(1000),

    example_with_noise(),
    timer:sleep(1000),

    example_loop_detection(),

    io:format("~n~n=== All Examples Completed ===~n~n"),
    ok.
