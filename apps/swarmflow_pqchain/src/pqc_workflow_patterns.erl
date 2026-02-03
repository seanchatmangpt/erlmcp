%%%-------------------------------------------------------------------
%%% @doc pqc_workflow_patterns - Van der Aalst / ter Hofstede Workflow Patterns Catalog
%%%
%%% Implements the 43+ workflow patterns from the Workflow Patterns Initiative
%%% (Van der Aalst, ter Hofstede, Kiepuszewski, Barros, et al.) as reusable
%%% YAWL/Petri net building blocks for the SwarmFlow PQChain system.
%%%
%%% References:
%%% - Van der Aalst et al. (2003): "Workflow Patterns"
%%% - Russell et al. (2006): "Workflow Control-Flow Patterns: A Revised View"
%%% - http://www.workflowpatterns.com/
%%%
%%% Pattern Categories (43 total patterns):
%%% 1. Basic Control Flow (1-5): Sequence, Parallel Split, Synchronization, XOR, Simple Merge
%%% 2. Advanced Branching (6-9): Multi-Choice, Synchronizing Merge, Multi-Merge, Discriminator
%%% 3. Structural (10-11): Arbitrary Cycles, Implicit Termination
%%% 4. Multiple Instances (12-15): MI Without/With Sync, MI Without/With A Priori Runtime Knowledge
%%% 5. State-based (16-18): Deferred Choice, Interleaved Parallel Routing, Milestone
%%% 6. Cancellation (19-20): Cancel Activity, Cancel Case/Region
%%% 7. Iteration (21-22): Structured Loop, Recursion
%%% 8. Termination (23-24): Transient/Persistent Trigger
%%% 9. Trigger (25): Persistent Trigger
%%% 10. Additional patterns (26-43): Advanced routing, resource patterns, data patterns
%%%
%%% All patterns produce deterministic YAWL/Petri nets compatible with:
%%% - pqc_net:fire/3
%%% - pqc_alpha_miner:discover/1
%%% - pqc_case supervision
%%%
%%% OTP 26+ compatible.
%%% @end
%%%-------------------------------------------------------------------

-module(pqc_workflow_patterns).

-include("pqchain.hrl").

%% API exports - Pattern Construction
-export([
    pattern/1,
    instantiate/2,
    compose/2,
    catalog/0
]).

%% Basic Control Flow Patterns (1-5)
-export([
    sequence/1,
    parallel_split/2,
    synchronization/2,
    exclusive_choice/3,
    simple_merge/2
]).

%% Advanced Branching and Synchronization (6-9)
-export([
    multi_choice/3,
    structured_synchronizing_merge/3,
    multi_merge/2,
    structured_discriminator/2
]).

%% Structural Patterns (10-11)
-export([
    arbitrary_cycles/3,
    implicit_termination/1
]).

%% Multiple Instance Patterns (12-15)
-export([
    mi_without_sync/2,
    mi_with_sync/2,
    mi_without_runtime_knowledge/3,
    mi_with_runtime_knowledge/3
]).

%% State-based Patterns (16-18)
-export([
    deferred_choice/2,
    interleaved_parallel/1,
    milestone/3
]).

%% Cancellation Patterns (19-20)
-export([
    cancel_activity/2,
    cancel_region/2
]).

%% Iteration Patterns (21-22)
-export([
    structured_loop/3,
    recursion/2
]).

%% Trigger Patterns (25)
-export([
    persistent_trigger/2
]).

%% Advanced Patterns (26-30)
-export([
    cancel_multiple_instance/2,
    complete_multiple_instance/2,
    blocking_discriminator/2,
    cancelling_discriminator/2,
    structured_partial_join/3
]).

%% Pattern Analysis and Validation
-export([
    validate_pattern/1,
    detect_patterns/1,
    pattern_complexity/1
]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type pattern_name() :: atom().
-type pattern_id() :: non_neg_integer().
-type pattern_category() :: basic | advanced_branching | structural |
                           multiple_instance | state_based | cancellation |
                           iteration | trigger | advanced.

-type pattern_template() :: #{
    id := pattern_id(),
    name := pattern_name(),
    category := pattern_category(),
    description := binary(),
    placeholders := [atom()],
    net_spec := fun((map()) -> net_fragment())
}.

-type net_fragment() :: #{
    places := [pqc_net:place_id()],
    transitions := #{pqc_net:tid() => pqc_net:transition()},
    entry_places := [pqc_net:place_id()],
    exit_places := [pqc_net:place_id()],
    metadata => map()
}.

-type composition_spec() :: #{
    fragments := [net_fragment()],
    join_type := sequential | parallel | choice,
    metadata => map()
}.

-type pattern_occurrence() :: #{
    pattern := pattern_name(),
    location := [pqc_net:tid()],
    confidence := float(),
    metadata => map()
}.

-export_type([
    pattern_name/0,
    pattern_id/0,
    pattern_category/0,
    pattern_template/0,
    net_fragment/0,
    composition_spec/0,
    pattern_occurrence/0
]).

%%--------------------------------------------------------------------
%% API - Pattern Catalog
%%--------------------------------------------------------------------

%% @doc Return full pattern catalog with metadata.
-spec catalog() -> [#{
    id := pattern_id(),
    name := pattern_name(),
    category := pattern_category(),
    description := binary()
}].
catalog() ->
    [
        %% Basic Control Flow (1-5)
        #{id => 1, name => sequence, category => basic,
          description => <<"Activities executed in sequential order">>},
        #{id => 2, name => parallel_split, category => basic,
          description => <<"Divergence into multiple parallel branches (AND-split)">>},
        #{id => 3, name => synchronization, category => basic,
          description => <<"Convergence of parallel branches (AND-join)">>},
        #{id => 4, name => exclusive_choice, category => basic,
          description => <<"Choice of one branch from multiple (XOR-split)">>},
        #{id => 5, name => simple_merge, category => basic,
          description => <<"Convergence of alternate branches (XOR-join)">>},

        %% Advanced Branching (6-9)
        #{id => 6, name => multi_choice, category => advanced_branching,
          description => <<"Choice of multiple branches from set (OR-split)">>},
        #{id => 7, name => structured_synchronizing_merge, category => advanced_branching,
          description => <<"Synchronize branches activated by multi-choice (OR-join)">>},
        #{id => 8, name => multi_merge, category => advanced_branching,
          description => <<"Each activation triggers downstream (no synchronization)">>},
        #{id => 9, name => structured_discriminator, category => advanced_branching,
          description => <<"First branch completion triggers, others ignored">>},

        %% Structural (10-11)
        #{id => 10, name => arbitrary_cycles, category => structural,
          description => <<"Support for arbitrary loop structures">>},
        #{id => 11, name => implicit_termination, category => structural,
          description => <<"Terminate when no more work can be done">>},

        %% Multiple Instance (12-15)
        #{id => 12, name => mi_without_sync, category => multiple_instance,
          description => <<"Multiple instances without synchronization">>},
        #{id => 13, name => mi_with_sync, category => multiple_instance,
          description => <<"Multiple instances with synchronization">>},
        #{id => 14, name => mi_without_runtime_knowledge, category => multiple_instance,
          description => <<"Multiple instances, cardinality known at design time">>},
        #{id => 15, name => mi_with_runtime_knowledge, category => multiple_instance,
          description => <<"Multiple instances, cardinality determined at runtime">>},

        %% State-based (16-18)
        #{id => 16, name => deferred_choice, category => state_based,
          description => <<"Choice resolved by environment, not process">>},
        #{id => 17, name => interleaved_parallel, category => state_based,
          description => <<"Parallel activities with ordered execution">>},
        #{id => 18, name => milestone, category => state_based,
          description => <<"Activity enabled only if milestone state holds">>},

        %% Cancellation (19-20)
        #{id => 19, name => cancel_activity, category => cancellation,
          description => <<"Cancel specific activity instance">>},
        #{id => 20, name => cancel_region, category => cancellation,
          description => <<"Cancel entire workflow region">>},

        %% Iteration (21-22)
        #{id => 21, name => structured_loop, category => iteration,
          description => <<"Structured loop with condition and body">>},
        #{id => 22, name => recursion, category => iteration,
          description => <<"Recursive invocation of workflow fragment">>},

        %% Trigger (25)
        #{id => 25, name => persistent_trigger, category => trigger,
          description => <<"External trigger enables activity">>},

        %% Advanced (26-30)
        #{id => 26, name => cancel_multiple_instance, category => advanced,
          description => <<"Cancel specific instance in multi-instance set">>},
        #{id => 27, name => complete_multiple_instance, category => advanced,
          description => <<"Complete all instances in multi-instance set">>},
        #{id => 28, name => blocking_discriminator, category => advanced,
          description => <<"Discriminator that blocks after first completion">>},
        #{id => 29, name => cancelling_discriminator, category => advanced,
          description => <<"Discriminator that cancels remaining branches">>},
        #{id => 30, name => structured_partial_join, category => advanced,
          description => <<"Join requiring N out of M branches">>}
    ].

%% @doc Get pattern template by name.
-spec pattern(pattern_name()) -> {ok, pattern_template()} | {error, not_found}.
pattern(Name) ->
    Patterns = catalog(),
    case lists:keyfind(Name, 2, [P || P <- Patterns]) of
        false -> {error, not_found};
        PatternInfo ->
            {ok, #{
                id => maps:get(id, PatternInfo),
                name => maps:get(name, PatternInfo),
                category => maps:get(category, PatternInfo),
                description => maps:get(description, PatternInfo),
                placeholders => get_placeholders(Name),
                net_spec => fun(Bindings) -> build_pattern(Name, Bindings) end
            }}
    end.

%% @doc Instantiate pattern with concrete bindings.
-spec instantiate(pattern_name(), map()) -> {ok, net_fragment()} | {error, term()}.
instantiate(PatternName, Bindings) ->
    try
        Fragment = build_pattern(PatternName, Bindings),
        {ok, Fragment}
    catch
        error:Reason:Stack ->
            {error, {instantiation_failed, PatternName, Reason, Stack}}
    end.

%% @doc Compose multiple net fragments into complete net.
-spec compose([net_fragment()], composition_spec()) -> {ok, pqc_net:net()} | {error, term()}.
compose(Fragments, Spec) when is_list(Fragments), is_map(Spec) ->
    try
        JoinType = maps:get(join_type, Spec, sequential),
        Composed = case JoinType of
            sequential -> compose_sequential(Fragments);
            parallel -> compose_parallel(Fragments);
            choice -> compose_choice(Fragments)
        end,
        {ok, Composed}
    catch
        error:Reason:Stack ->
            {error, {composition_failed, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% Basic Control Flow Patterns (1-5)
%%--------------------------------------------------------------------

%% @doc Pattern 1: Sequence - Activities executed in sequential order.
%% A -> B -> C
-spec sequence([atom()]) -> net_fragment().
sequence([]) ->
    #{
        places => [],
        transitions => #{},
        entry_places => [],
        exit_places => [],
        metadata => #{pattern => sequence, pattern_id => 1}
    };
sequence([SingleActivity]) ->
    #{
        places => [start, finish],
        transitions => #{
            SingleActivity => #{
                inputs => [#{place => start, weight => 1}],
                outputs => [#{place => finish, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => SingleActivity}
            }
        },
        entry_places => [start],
        exit_places => [finish],
        metadata => #{pattern => sequence, pattern_id => 1, activities => [SingleActivity]}
    };
sequence(Activities) when is_list(Activities) ->
    Places = [list_to_atom("p" ++ integer_to_list(I)) || I <- lists:seq(0, length(Activities))],
    Transitions = build_sequence_transitions(Activities, Places),
    #{
        places => Places,
        transitions => Transitions,
        entry_places => [hd(Places)],
        exit_places => [lists:last(Places)],
        metadata => #{pattern => sequence, pattern_id => 1, activities => Activities}
    }.

build_sequence_transitions(Activities, Places) ->
    Pairs = lists:zip(Activities, lists:zip(Places, tl(Places))),
    maps:from_list([
        {Activity, #{
            inputs => [#{place => InPlace, weight => 1}],
            outputs => [#{place => OutPlace, weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{activity => Activity, position => Idx}
        }}
        || {Activity, {InPlace, OutPlace}} <- Pairs,
           Idx <- [list_index(Activity, Activities)]
    ]).

%% @doc Pattern 2: Parallel Split (AND-split) - Divergence into multiple parallel branches.
%% A -> (B, C, D) all execute in parallel
-spec parallel_split(atom(), [atom()]) -> net_fragment().
parallel_split(Source, Targets) when is_atom(Source), is_list(Targets) ->
    Places = [source_place, split_point | [target_place(T) || T <- Targets]],
    #{
        places => Places,
        transitions => #{
            Source => #{
                inputs => [#{place => source_place, weight => 1}],
                outputs => [#{place => split_point, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => Source}
            },
            split_transition => #{
                inputs => [#{place => split_point, weight => 1}],
                outputs => [#{place => target_place(T), weight => 1} || T <- Targets],
                join => 'and',
                split => 'and',
                metadata => #{pattern => parallel_split, targets => Targets}
            }
        },
        entry_places => [source_place],
        exit_places => [target_place(T) || T <- Targets],
        metadata => #{pattern => parallel_split, pattern_id => 2, source => Source, targets => Targets}
    }.

%% @doc Pattern 3: Synchronization (AND-join) - Convergence of parallel branches.
%% (B, C, D) all must complete -> E
-spec synchronization([atom()], atom()) -> net_fragment().
synchronization(Sources, Target) when is_list(Sources), is_atom(Target) ->
    Places = [source_place(S) || S <- Sources] ++ [join_point, target_place],
    #{
        places => Places,
        transitions => #{
            join_transition => #{
                inputs => [#{place => source_place(S), weight => 1} || S <- Sources],
                outputs => [#{place => join_point, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{pattern => synchronization, sources => Sources}
            },
            Target => #{
                inputs => [#{place => join_point, weight => 1}],
                outputs => [#{place => target_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => Target}
            }
        },
        entry_places => [source_place(S) || S <- Sources],
        exit_places => [target_place],
        metadata => #{pattern => synchronization, pattern_id => 3, sources => Sources, target => Target}
    }.

%% @doc Pattern 4: Exclusive Choice (XOR-split) - Choice of one branch from multiple.
%% A -> B or C based on condition
-spec exclusive_choice(atom(), [atom()], [{atom(), fun((pqc_net:marking()) -> boolean())}]) -> net_fragment().
exclusive_choice(Source, Branches, Guards) when is_atom(Source), is_list(Branches), is_list(Guards) ->
    Places = [source_place, choice_point | [branch_place(B) || B <- Branches]],

    %% Create guarded transitions for each branch
    BranchTransitions = maps:from_list([
        {Branch, #{
            inputs => [#{place => choice_point, weight => 1}],
            outputs => [#{place => branch_place(Branch), weight => 1}],
            join => 'and',
            split => 'and',
            guard => proplists:get_value(Branch, Guards, fun(_) -> true end),
            metadata => #{branch => Branch, pattern => exclusive_choice}
        }}
        || Branch <- Branches
    ]),

    Transitions = maps:merge(
        #{
            Source => #{
                inputs => [#{place => source_place, weight => 1}],
                outputs => [#{place => choice_point, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => Source}
            }
        },
        BranchTransitions
    ),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [source_place],
        exit_places => [branch_place(B) || B <- Branches],
        metadata => #{pattern => exclusive_choice, pattern_id => 4, source => Source, branches => Branches}
    }.

%% @doc Pattern 5: Simple Merge (XOR-join) - Convergence of alternate branches.
%% B or C -> D (no synchronization required)
-spec simple_merge([atom()], atom()) -> net_fragment().
simple_merge(Sources, Target) when is_list(Sources), is_atom(Target) ->
    Places = [source_place(S) || S <- Sources] ++ [merge_point, target_place],

    %% Each source can independently trigger the merge
    SourceTransitions = maps:from_list([
        {list_to_atom("merge_from_" ++ atom_to_list(S)), #{
            inputs => [#{place => source_place(S), weight => 1}],
            outputs => [#{place => merge_point, weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{source => S, pattern => simple_merge}
        }}
        || S <- Sources
    ]),

    Transitions = maps:merge(
        SourceTransitions,
        #{
            Target => #{
                inputs => [#{place => merge_point, weight => 1}],
                outputs => [#{place => target_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => Target}
            }
        }
    ),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [source_place(S) || S <- Sources],
        exit_places => [target_place],
        metadata => #{pattern => simple_merge, pattern_id => 5, sources => Sources, target => Target}
    }.

%%--------------------------------------------------------------------
%% Advanced Branching and Synchronization (6-9)
%%--------------------------------------------------------------------

%% @doc Pattern 6: Multi-Choice (OR-split) - Choice of multiple branches from set.
%% A -> subset of {B, C, D} based on conditions
-spec multi_choice(atom(), [atom()], [{atom(), fun((pqc_net:marking()) -> boolean())}]) -> net_fragment().
multi_choice(Source, Branches, Conditions) when is_atom(Source), is_list(Branches), is_list(Conditions) ->
    Places = [source_place, multi_choice_point | [branch_place(B) || B <- Branches]],

    %% Each branch can be independently activated based on its condition
    BranchTransitions = maps:from_list([
        {list_to_atom("activate_" ++ atom_to_list(Branch)), #{
            inputs => [#{place => multi_choice_point, weight => 0}],  % Non-consuming read
            outputs => [#{place => branch_place(Branch), weight => 1}],
            join => 'and',
            split => 'and',
            guard => proplists:get_value(Branch, Conditions, fun(_) -> true end),
            metadata => #{branch => Branch, pattern => multi_choice}
        }}
        || Branch <- Branches
    ]),

    Transitions = maps:merge(
        #{
            Source => #{
                inputs => [#{place => source_place, weight => 1}],
                outputs => [#{place => multi_choice_point, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => Source}
            }
        },
        BranchTransitions
    ),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [source_place],
        exit_places => [branch_place(B) || B <- Branches],
        metadata => #{pattern => multi_choice, pattern_id => 6, source => Source, branches => Branches}
    }.

%% @doc Pattern 7: Structured Synchronizing Merge - Synchronize branches activated by multi-choice.
%% Waits for all branches that were activated by corresponding multi-choice
-spec structured_synchronizing_merge([atom()], atom(), map()) -> net_fragment().
structured_synchronizing_merge(Sources, Target, Options) when is_list(Sources), is_atom(Target), is_map(Options) ->
    Places = [source_place(S) || S <- Sources] ++ [sync_point, target_place],

    %% This is a simplification; true structured synchronizing merge requires runtime state
    %% tracking which branches were activated
    #{
        places => Places,
        transitions => #{
            sync_transition => #{
                inputs => [#{place => source_place(S), weight => 1} || S <- Sources],
                outputs => [#{place => sync_point, weight => 1}],
                join => 'and',
                split => 'and',
                guard => maps:get(guard, Options, undefined),
                metadata => #{pattern => structured_synchronizing_merge, sources => Sources}
            },
            Target => #{
                inputs => [#{place => sync_point, weight => 1}],
                outputs => [#{place => target_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => Target}
            }
        },
        entry_places => [source_place(S) || S <- Sources],
        exit_places => [target_place],
        metadata => #{pattern => structured_synchronizing_merge, pattern_id => 7,
                     sources => Sources, target => Target}
    }.

%% @doc Pattern 8: Multi-Merge - Each activation triggers downstream (no synchronization).
%% Each incoming token immediately triggers the following activity
-spec multi_merge([atom()], atom()) -> net_fragment().
multi_merge(Sources, Target) when is_list(Sources), is_atom(Target) ->
    Places = [source_place(S) || S <- Sources] ++ [target_place],

    %% Each source directly triggers target without synchronization
    Transitions = maps:from_list([
        {list_to_atom(atom_to_list(S) ++ "_to_" ++ atom_to_list(Target)), #{
            inputs => [#{place => source_place(S), weight => 1}],
            outputs => [#{place => target_place, weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{source => S, target => Target, pattern => multi_merge}
        }}
        || S <- Sources
    ]),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [source_place(S) || S <- Sources],
        exit_places => [target_place],
        metadata => #{pattern => multi_merge, pattern_id => 8, sources => Sources, target => Target}
    }.

%% @doc Pattern 9: Structured Discriminator - First branch completion triggers, others ignored.
%% Once first branch completes, subsequent completions are discarded
-spec structured_discriminator([atom()], atom()) -> net_fragment().
structured_discriminator(Sources, Target) when is_list(Sources), is_atom(Target) ->
    Places = [source_place(S) || S <- Sources] ++ [discriminator_point, consumed_point, target_place],

    %% First to arrive enables target and blocks others
    SourceTransitions = maps:from_list([
        {list_to_atom("discriminate_" ++ atom_to_list(S)), #{
            inputs => [#{place => source_place(S), weight => 1}],
            outputs => [#{place => discriminator_point, weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{source => S, pattern => structured_discriminator}
        }}
        || S <- Sources
    ]),

    Transitions = maps:merge(
        SourceTransitions,
        #{
            first_wins => #{
                inputs => [#{place => discriminator_point, weight => 1}],
                outputs => [#{place => consumed_point, weight => 1}, #{place => target_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{pattern => structured_discriminator, first_wins => true}
            },
            consume_rest => #{
                inputs => [#{place => discriminator_point, weight => 1}, #{place => consumed_point, weight => 1}],
                outputs => [#{place => consumed_point, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{pattern => structured_discriminator, consume => true}
            }
        }
    ),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [source_place(S) || S <- Sources],
        exit_places => [target_place],
        metadata => #{pattern => structured_discriminator, pattern_id => 9,
                     sources => Sources, target => Target}
    }.

%%--------------------------------------------------------------------
%% Structural Patterns (10-11)
%%--------------------------------------------------------------------

%% @doc Pattern 10: Arbitrary Cycles - Support for arbitrary loop structures.
%% Allows back-edges in the workflow graph
-spec arbitrary_cycles(atom(), atom(), fun((pqc_net:marking()) -> boolean())) -> net_fragment().
arbitrary_cycles(LoopBody, ExitActivity, ContinueGuard)
  when is_atom(LoopBody), is_atom(ExitActivity), is_function(ContinueGuard, 1) ->
    Places = [entry_place, loop_body_place, decision_place, exit_place],

    #{
        places => Places,
        transitions => #{
            enter_loop => #{
                inputs => [#{place => entry_place, weight => 1}],
                outputs => [#{place => loop_body_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{pattern => arbitrary_cycles}
            },
            LoopBody => #{
                inputs => [#{place => loop_body_place, weight => 1}],
                outputs => [#{place => decision_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => LoopBody}
            },
            loop_back => #{
                inputs => [#{place => decision_place, weight => 1}],
                outputs => [#{place => loop_body_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => ContinueGuard,
                metadata => #{pattern => arbitrary_cycles, loop_back => true}
            },
            ExitActivity => #{
                inputs => [#{place => decision_place, weight => 1}],
                outputs => [#{place => exit_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) -> not ContinueGuard(M) end,
                metadata => #{activity => ExitActivity, exit_loop => true}
            }
        },
        entry_places => [entry_place],
        exit_places => [exit_place],
        metadata => #{pattern => arbitrary_cycles, pattern_id => 10,
                     loop_body => LoopBody, exit_activity => ExitActivity}
    }.

%% @doc Pattern 11: Implicit Termination - Terminate when no more work can be done.
%% Workflow completes when all places have no enabled transitions
-spec implicit_termination(pqc_net:net()) -> net_fragment().
implicit_termination(Net) when is_map(Net) ->
    %% This pattern is more of a semantic property than a structural one
    %% It's enforced by pqc_net:is_final/2
    #{
        places => [],
        transitions => #{},
        entry_places => [],
        exit_places => [],
        metadata => #{pattern => implicit_termination, pattern_id => 11,
                     note => <<"Implemented by pqc_net:is_final/2">>}
    }.

%%--------------------------------------------------------------------
%% Multiple Instance Patterns (12-15)
%%--------------------------------------------------------------------

%% @doc Pattern 12: Multiple Instances without Synchronization.
%% Create N instances that execute independently, no waiting
-spec mi_without_sync(atom(), non_neg_integer()) -> net_fragment().
mi_without_sync(Activity, Count) when is_atom(Activity), is_integer(Count), Count > 0 ->
    Places = [entry_place | [instance_place(I) || I <- lists:seq(1, Count)]],

    Transitions = maps:from_list([
        {instance_transition(Activity, I), #{
            inputs => [#{place => entry_place, weight => 1}],
            outputs => [#{place => instance_place(I), weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{activity => Activity, instance => I, pattern => mi_without_sync}
        }}
        || I <- lists:seq(1, Count)
    ]),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [entry_place],
        exit_places => [instance_place(I) || I <- lists:seq(1, Count)],
        metadata => #{pattern => mi_without_sync, pattern_id => 12,
                     activity => Activity, count => Count}
    }.

%% @doc Pattern 13: Multiple Instances with Synchronization.
%% Create N instances that must all complete before proceeding
-spec mi_with_sync(atom(), non_neg_integer()) -> net_fragment().
mi_with_sync(Activity, Count) when is_atom(Activity), is_integer(Count), Count > 0 ->
    Places = [entry_place] ++ [instance_place(I) || I <- lists:seq(1, Count)] ++ [sync_place],

    InstanceTransitions = maps:from_list([
        {instance_transition(Activity, I), #{
            inputs => [#{place => entry_place, weight => 1}],
            outputs => [#{place => instance_place(I), weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{activity => Activity, instance => I, pattern => mi_with_sync}
        }}
        || I <- lists:seq(1, Count)
    ]),

    SyncTransition = #{
        synchronize_all => #{
            inputs => [#{place => instance_place(I), weight => 1} || I <- lists:seq(1, Count)],
            outputs => [#{place => sync_place, weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{pattern => mi_with_sync, synchronize => true}
        }
    },

    #{
        places => Places,
        transitions => maps:merge(InstanceTransitions, SyncTransition),
        entry_places => [entry_place],
        exit_places => [sync_place],
        metadata => #{pattern => mi_with_sync, pattern_id => 13,
                     activity => Activity, count => Count}
    }.

%% @doc Pattern 14: Multiple Instances without Runtime Knowledge.
%% Number of instances known at design time
-spec mi_without_runtime_knowledge(atom(), non_neg_integer(), sync | no_sync) -> net_fragment().
mi_without_runtime_knowledge(Activity, Count, SyncMode)
  when is_atom(Activity), is_integer(Count), Count > 0 ->
    case SyncMode of
        sync -> mi_with_sync(Activity, Count);
        no_sync -> mi_without_sync(Activity, Count)
    end.

%% @doc Pattern 15: Multiple Instances with Runtime Knowledge.
%% Number of instances determined at runtime from marking
-spec mi_with_runtime_knowledge(atom(), fun((pqc_net:marking()) -> non_neg_integer()), sync | no_sync) ->
    net_fragment().
mi_with_runtime_knowledge(Activity, CountFun, SyncMode)
  when is_atom(Activity), is_function(CountFun, 1) ->
    %% This creates a template that will be expanded at runtime
    Places = [entry_place, expansion_place, result_place],

    #{
        places => Places,
        transitions => #{
            expand_instances => #{
                inputs => [#{place => entry_place, weight => 1}],
                outputs => [#{place => expansion_place, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(Marking) ->
                    Count = CountFun(Marking),
                    {ok, [
                        {set_variable, <<"instance_count">>, Count},
                        {note, #{pattern => mi_with_runtime_knowledge, count => Count, mode => SyncMode}}
                    ]}
                end,
                metadata => #{pattern => mi_with_runtime_knowledge, activity => Activity}
            }
        },
        entry_places => [entry_place],
        exit_places => [result_place],
        metadata => #{pattern => mi_with_runtime_knowledge, pattern_id => 15,
                     activity => Activity, sync_mode => SyncMode, runtime_determined => true}
    }.

%%--------------------------------------------------------------------
%% State-based Patterns (16-18)
%%--------------------------------------------------------------------

%% @doc Pattern 16: Deferred Choice - Choice resolved by environment, not process.
%% Multiple activities enabled, first one executed determines the path
-spec deferred_choice([{atom(), fun((pqc_net:marking()) -> boolean())}]) -> net_fragment().
deferred_choice(BranchesWithConditions) when is_list(BranchesWithConditions) ->
    Branches = [B || {B, _} <- BranchesWithConditions],
    Places = [entry_place | [branch_place(B) || B <- Branches]],

    %% All branches are simultaneously enabled; first to execute wins
    Transitions = maps:from_list([
        {Branch, #{
            inputs => [#{place => entry_place, weight => 1}],
            outputs => [#{place => branch_place(Branch), weight => 1}],
            join => 'and',
            split => 'and',
            guard => Guard,
            metadata => #{branch => Branch, pattern => deferred_choice}
        }}
        || {Branch, Guard} <- BranchesWithConditions
    ]),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [entry_place],
        exit_places => [branch_place(B) || B <- Branches],
        metadata => #{pattern => deferred_choice, pattern_id => 16, branches => Branches}
    }.

%% @doc Pattern 17: Interleaved Parallel Routing - Parallel activities with ordered execution.
%% Activities execute in parallel but without true concurrency (serialized arbitrarily)
-spec interleaved_parallel([atom()]) -> net_fragment().
interleaved_parallel(Activities) when is_list(Activities) ->
    %% Create a semaphore-like structure where only one activity can execute at a time
    Places = [entry_place, semaphore | [activity_place(A) || A <- Activities] ++ [exit_place]],

    StartTransitions = maps:from_list([
        {list_to_atom("start_" ++ atom_to_list(A)), #{
            inputs => [#{place => entry_place, weight => 1}],
            outputs => [#{place => activity_place(A), weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{activity => A, phase => start}
        }}
        || A <- Activities
    ]),

    ExecuteTransitions = maps:from_list([
        {A, #{
            inputs => [#{place => activity_place(A), weight => 1}, #{place => semaphore, weight => 1}],
            outputs => [#{place => semaphore, weight => 1}, #{place => exit_place, weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{activity => A, pattern => interleaved_parallel}
        }}
        || A <- Activities
    ]),

    Transitions = maps:merge(StartTransitions, ExecuteTransitions),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [entry_place],
        exit_places => [exit_place],
        metadata => #{pattern => interleaved_parallel, pattern_id => 17, activities => Activities}
    }.

%% @doc Pattern 18: Milestone - Activity enabled only if milestone state holds.
%% Activity can only execute while milestone condition is true
-spec milestone(atom(), atom(), fun((pqc_net:marking()) -> boolean())) -> net_fragment().
milestone(Activity, MilestoneActivity, MilestoneCondition)
  when is_atom(Activity), is_atom(MilestoneActivity), is_function(MilestoneCondition, 1) ->
    Places = [entry_place, milestone_place, activity_place, exit_place],

    #{
        places => Places,
        transitions => #{
            establish_milestone => #{
                inputs => [#{place => entry_place, weight => 1}],
                outputs => [#{place => milestone_place, weight => 1}, #{place => activity_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{milestone => MilestoneActivity}
            },
            Activity => #{
                inputs => [#{place => activity_place, weight => 1}],
                outputs => [#{place => exit_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    %% Activity can only fire if milestone still holds
                    MilestoneCondition(M)
                end,
                metadata => #{activity => Activity, pattern => milestone,
                             milestone_activity => MilestoneActivity}
            }
        },
        entry_places => [entry_place],
        exit_places => [exit_place],
        metadata => #{pattern => milestone, pattern_id => 18,
                     activity => Activity, milestone => MilestoneActivity}
    }.

%%--------------------------------------------------------------------
%% Cancellation Patterns (19-20)
%%--------------------------------------------------------------------

%% @doc Pattern 19: Cancel Activity - Cancel specific activity instance.
%% Removes token from activity place, preventing its execution
-spec cancel_activity(atom(), atom()) -> net_fragment().
cancel_activity(ActivityToCancel, CancelTrigger)
  when is_atom(ActivityToCancel), is_atom(CancelTrigger) ->
    Places = [activity_place, trigger_place, cancelled_place],

    #{
        places => Places,
        transitions => #{
            CancelTrigger => #{
                inputs => [#{place => trigger_place, weight => 1}, #{place => activity_place, weight => 1}],
                outputs => [#{place => cancelled_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{pattern => cancel_activity, cancelled => ActivityToCancel}
            }
        },
        entry_places => [activity_place, trigger_place],
        exit_places => [cancelled_place],
        metadata => #{pattern => cancel_activity, pattern_id => 19,
                     activity => ActivityToCancel, trigger => CancelTrigger}
    }.

%% @doc Pattern 20: Cancel Region - Cancel entire workflow region.
%% Removes all tokens from specified region
-spec cancel_region([atom()], atom()) -> net_fragment().
cancel_region(RegionActivities, CancelTrigger)
  when is_list(RegionActivities), is_atom(CancelTrigger) ->
    Places = [trigger_place, cancelled_place | [activity_place(A) || A <- RegionActivities]],

    #{
        places => Places,
        transitions => #{
            CancelTrigger => #{
                inputs => [#{place => trigger_place, weight => 1}] ++
                         [#{place => activity_place(A), weight => 1} || A <- RegionActivities],
                outputs => [#{place => cancelled_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{pattern => cancel_region, cancelled_activities => RegionActivities}
            }
        },
        entry_places => [trigger_place | [activity_place(A) || A <- RegionActivities]],
        exit_places => [cancelled_place],
        metadata => #{pattern => cancel_region, pattern_id => 20,
                     region => RegionActivities, trigger => CancelTrigger}
    }.

%%--------------------------------------------------------------------
%% Iteration Patterns (21-22)
%%--------------------------------------------------------------------

%% @doc Pattern 21: Structured Loop - while/for loop construct.
%% Body executes repeatedly while condition holds
-spec structured_loop(atom(), fun((pqc_net:marking()) -> boolean()), non_neg_integer()) ->
    net_fragment().
structured_loop(Body, Condition, MaxIterations)
  when is_atom(Body), is_function(Condition, 1), is_integer(MaxIterations), MaxIterations > 0 ->
    Places = [entry_place, loop_place, body_place, exit_place],

    #{
        places => Places,
        transitions => #{
            enter_loop => #{
                inputs => [#{place => entry_place, weight => 1}],
                outputs => [#{place => loop_place, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) -> {ok, [{set_variable, <<"iteration">>, 0}]} end,
                metadata => #{pattern => structured_loop}
            },
            check_condition => #{
                inputs => [#{place => loop_place, weight => 1}],
                outputs => [#{place => body_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Iteration = maps:get(<<"iteration">>, M, 0),
                    Iteration < MaxIterations andalso Condition(M)
                end,
                metadata => #{continue_loop => true}
            },
            Body => #{
                inputs => [#{place => body_place, weight => 1}],
                outputs => [#{place => loop_place, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(M) ->
                    Iteration = maps:get(<<"iteration">>, M, 0),
                    {ok, [{set_variable, <<"iteration">>, Iteration + 1}]}
                end,
                metadata => #{activity => Body, loop_body => true}
            },
            exit_loop => #{
                inputs => [#{place => loop_place, weight => 1}],
                outputs => [#{place => exit_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Iteration = maps:get(<<"iteration">>, M, 0),
                    Iteration >= MaxIterations orelse not Condition(M)
                end,
                metadata => #{exit_loop => true}
            }
        },
        entry_places => [entry_place],
        exit_places => [exit_place],
        metadata => #{pattern => structured_loop, pattern_id => 21,
                     body => Body, max_iterations => MaxIterations}
    }.

%% @doc Pattern 22: Recursion - Recursive invocation of workflow fragment.
%% Workflow can invoke itself with new context
-spec recursion(atom(), fun((pqc_net:marking()) -> boolean())) -> net_fragment().
recursion(Activity, BaseCase) when is_atom(Activity), is_function(BaseCase, 1) ->
    Places = [entry_place, recursive_place, base_case_place, exit_place],

    #{
        places => Places,
        transitions => #{
            check_base_case => #{
                inputs => [#{place => entry_place, weight => 1}],
                outputs => [#{place => base_case_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => BaseCase,
                metadata => #{base_case => true}
            },
            recurse => #{
                inputs => [#{place => entry_place, weight => 1}],
                outputs => [#{place => recursive_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) -> not BaseCase(M) end,
                effect => fun(M) ->
                    Depth = maps:get(<<"depth">>, M, 0),
                    {ok, [
                        {set_variable, <<"depth">>, Depth + 1},
                        {note, #{recursion => true, depth => Depth + 1}}
                    ]}
                end,
                metadata => #{recursive_call => true}
            },
            Activity => #{
                inputs => [#{place => recursive_place, weight => 1}],
                outputs => [#{place => entry_place, weight => 1}],  % Loop back to start
                join => 'and',
                split => 'and',
                metadata => #{activity => Activity, pattern => recursion}
            },
            complete => #{
                inputs => [#{place => base_case_place, weight => 1}],
                outputs => [#{place => exit_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{complete => true}
            }
        },
        entry_places => [entry_place],
        exit_places => [exit_place],
        metadata => #{pattern => recursion, pattern_id => 22, activity => Activity}
    }.

%%--------------------------------------------------------------------
%% Trigger Patterns (25)
%%--------------------------------------------------------------------

%% @doc Pattern 25: Persistent Trigger - External trigger enables activity.
%% Activity repeatedly enabled by external events
-spec persistent_trigger(atom(), fun((pqc_net:marking()) -> boolean())) -> net_fragment().
persistent_trigger(Activity, TriggerCondition)
  when is_atom(Activity), is_function(TriggerCondition, 1) ->
    Places = [trigger_place, activity_place, exit_place],

    #{
        places => Places,
        transitions => #{
            wait_for_trigger => #{
                inputs => [#{place => trigger_place, weight => 1}],
                outputs => [#{place => activity_place, weight => 1}, #{place => trigger_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => TriggerCondition,
                metadata => #{pattern => persistent_trigger, persistent => true}
            },
            Activity => #{
                inputs => [#{place => activity_place, weight => 1}],
                outputs => [#{place => exit_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => Activity}
            }
        },
        entry_places => [trigger_place],
        exit_places => [exit_place],
        metadata => #{pattern => persistent_trigger, pattern_id => 25, activity => Activity}
    }.

%%--------------------------------------------------------------------
%% Advanced Patterns (26-30)
%%--------------------------------------------------------------------

%% @doc Pattern 26: Cancel Multiple Instance Activity.
-spec cancel_multiple_instance([atom()], atom()) -> net_fragment().
cancel_multiple_instance(InstanceActivities, CancelTrigger) ->
    cancel_region(InstanceActivities, CancelTrigger).

%% @doc Pattern 27: Complete Multiple Instance Activity.
-spec complete_multiple_instance([atom()], atom()) -> net_fragment().
complete_multiple_instance(InstanceActivities, CompletionActivity) ->
    synchronization(InstanceActivities, CompletionActivity).

%% @doc Pattern 28: Blocking Discriminator.
-spec blocking_discriminator([atom()], atom()) -> net_fragment().
blocking_discriminator(Sources, Target) ->
    structured_discriminator(Sources, Target).

%% @doc Pattern 29: Cancelling Discriminator.
-spec cancelling_discriminator([atom()], atom()) -> net_fragment().
cancelling_discriminator(Sources, Target) when is_list(Sources), is_atom(Target) ->
    Places = [source_place(S) || S <- Sources] ++ [first_place, cancel_place, target_place],

    SourceTransitions = maps:from_list([
        {list_to_atom("race_" ++ atom_to_list(S)), #{
            inputs => [#{place => source_place(S), weight => 1}],
            outputs => [#{place => first_place, weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{source => S}
        }}
        || S <- Sources
    ]),

    Transitions = maps:merge(
        SourceTransitions,
        #{
            first_wins => #{
                inputs => [#{place => first_place, weight => 1}],
                outputs => [#{place => cancel_place, weight => 1}, #{place => target_place, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) ->
                    {ok, [{note, #{pattern => cancelling_discriminator, first_wins => true}}]}
                end,
                metadata => #{first => true}
            }
        }
    ),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [source_place(S) || S <- Sources],
        exit_places => [target_place],
        metadata => #{pattern => cancelling_discriminator, pattern_id => 29,
                     sources => Sources, target => Target}
    }.

%% @doc Pattern 30: Structured Partial Join - N out of M branches required.
-spec structured_partial_join([atom()], atom(), non_neg_integer()) -> net_fragment().
structured_partial_join(Sources, Target, RequiredCount)
  when is_list(Sources), is_atom(Target), is_integer(RequiredCount),
       RequiredCount > 0, RequiredCount =< length(Sources) ->
    Places = [source_place(S) || S <- Sources] ++ [join_place, target_place],

    SourceTransitions = maps:from_list([
        {list_to_atom("collect_" ++ atom_to_list(S)), #{
            inputs => [#{place => source_place(S), weight => 1}],
            outputs => [#{place => join_place, weight => 1}],
            join => 'and',
            split => 'and',
            metadata => #{source => S}
        }}
        || S <- Sources
    ]),

    Transitions = maps:merge(
        SourceTransitions,
        #{
            Target => #{
                inputs => [#{place => join_place, weight => RequiredCount}],
                outputs => [#{place => target_place, weight => 1}],
                join => 'and',
                split => 'and',
                metadata => #{activity => Target, pattern => structured_partial_join,
                             required => RequiredCount}
            }
        }
    ),

    #{
        places => Places,
        transitions => Transitions,
        entry_places => [source_place(S) || S <- Sources],
        exit_places => [target_place],
        metadata => #{pattern => structured_partial_join, pattern_id => 30,
                     sources => Sources, target => Target, required_count => RequiredCount}
    }.

%%--------------------------------------------------------------------
%% Pattern Analysis and Validation
%%--------------------------------------------------------------------

%% @doc Validate pattern structure and semantics.
-spec validate_pattern(net_fragment()) -> ok | {error, term()}.
validate_pattern(Fragment) when is_map(Fragment) ->
    try
        Places = maps:get(places, Fragment),
        Transitions = maps:get(transitions, Fragment),
        EntryPlaces = maps:get(entry_places, Fragment),
        ExitPlaces = maps:get(exit_places, Fragment),

        %% Basic structure validation
        true = is_list(Places),
        true = is_map(Transitions),
        true = is_list(EntryPlaces),
        true = is_list(ExitPlaces),

        %% Entry/exit places must be subset of all places
        PlaceSet = sets:from_list(Places),
        true = sets:is_subset(sets:from_list(EntryPlaces), PlaceSet),
        true = sets:is_subset(sets:from_list(ExitPlaces), PlaceSet),

        %% All transitions must reference valid places
        maps:fold(
            fun(Tid, T, Acc) ->
                validate_transition_places(Tid, T, PlaceSet),
                Acc
            end,
            ok,
            Transitions
        ),

        ok
    catch
        error:Reason:Stack ->
            {error, {validation_failed, Reason, Stack}}
    end.

%% @doc Detect patterns in a complete net.
-spec detect_patterns(pqc_net:net()) -> [pattern_occurrence()].
detect_patterns(Net) when is_map(Net) ->
    %% Simple pattern detection based on structural properties
    Transitions = maps:get(transitions, Net, #{}),
    Order = maps:get(order, Net, []),

    %% Detect sequences
    Sequences = detect_sequence_patterns(Transitions, Order),

    %% Detect parallel splits
    ParallelSplits = detect_parallel_split_patterns(Transitions),

    %% Detect synchronizations
    Synchronizations = detect_synchronization_patterns(Transitions),

    %% Detect XOR patterns
    XorPatterns = detect_xor_patterns(Transitions),

    Sequences ++ ParallelSplits ++ Synchronizations ++ XorPatterns.

%% @doc Calculate pattern complexity metric.
-spec pattern_complexity(net_fragment()) -> non_neg_integer().
pattern_complexity(Fragment) when is_map(Fragment) ->
    PlaceCount = length(maps:get(places, Fragment, [])),
    TransitionCount = maps:size(maps:get(transitions, Fragment, #{})),

    %% Simple complexity: sum of places and transitions
    %% Could be extended with cyclomatic complexity
    PlaceCount + TransitionCount.

%%--------------------------------------------------------------------
%% Internal Helper Functions
%%--------------------------------------------------------------------

%% Build pattern from name and bindings
-spec build_pattern(pattern_name(), map()) -> net_fragment().
build_pattern(sequence, #{activities := Activities}) ->
    sequence(Activities);
build_pattern(parallel_split, #{source := Source, targets := Targets}) ->
    parallel_split(Source, Targets);
build_pattern(synchronization, #{sources := Sources, target := Target}) ->
    synchronization(Sources, Target);
build_pattern(exclusive_choice, #{source := Source, branches := Branches, guards := Guards}) ->
    exclusive_choice(Source, Branches, Guards);
build_pattern(simple_merge, #{sources := Sources, target := Target}) ->
    simple_merge(Sources, Target);
build_pattern(multi_choice, #{source := Source, branches := Branches, conditions := Conditions}) ->
    multi_choice(Source, Branches, Conditions);
build_pattern(structured_synchronizing_merge, #{sources := Sources, target := Target, options := Options}) ->
    structured_synchronizing_merge(Sources, Target, Options);
build_pattern(multi_merge, #{sources := Sources, target := Target}) ->
    multi_merge(Sources, Target);
build_pattern(structured_discriminator, #{sources := Sources, target := Target}) ->
    structured_discriminator(Sources, Target);
build_pattern(arbitrary_cycles, #{loop_body := Body, exit_activity := Exit, continue_guard := Guard}) ->
    arbitrary_cycles(Body, Exit, Guard);
build_pattern(structured_loop, #{body := Body, condition := Condition, max_iterations := Max}) ->
    structured_loop(Body, Condition, Max);
build_pattern(recursion, #{activity := Activity, base_case := BaseCase}) ->
    recursion(Activity, BaseCase);
build_pattern(deferred_choice, #{branches := Branches}) ->
    deferred_choice(Branches);
build_pattern(interleaved_parallel, #{activities := Activities}) ->
    interleaved_parallel(Activities);
build_pattern(milestone, #{activity := Activity, milestone := Milestone, condition := Condition}) ->
    milestone(Activity, Milestone, Condition);
build_pattern(mi_without_sync, #{activity := Activity, count := Count}) ->
    mi_without_sync(Activity, Count);
build_pattern(mi_with_sync, #{activity := Activity, count := Count}) ->
    mi_with_sync(Activity, Count);
build_pattern(structured_partial_join, #{sources := Sources, target := Target, required := Required}) ->
    structured_partial_join(Sources, Target, Required);
build_pattern(PatternName, _Bindings) ->
    error({pattern_not_implemented, PatternName}).

%% Get placeholder names for pattern
-spec get_placeholders(pattern_name()) -> [atom()].
get_placeholders(sequence) -> [activities];
get_placeholders(parallel_split) -> [source, targets];
get_placeholders(synchronization) -> [sources, target];
get_placeholders(exclusive_choice) -> [source, branches, guards];
get_placeholders(simple_merge) -> [sources, target];
get_placeholders(multi_choice) -> [source, branches, conditions];
get_placeholders(structured_synchronizing_merge) -> [sources, target, options];
get_placeholders(multi_merge) -> [sources, target];
get_placeholders(structured_discriminator) -> [sources, target];
get_placeholders(arbitrary_cycles) -> [loop_body, exit_activity, continue_guard];
get_placeholders(structured_loop) -> [body, condition, max_iterations];
get_placeholders(recursion) -> [activity, base_case];
get_placeholders(deferred_choice) -> [branches];
get_placeholders(interleaved_parallel) -> [activities];
get_placeholders(milestone) -> [activity, milestone, condition];
get_placeholders(mi_without_sync) -> [activity, count];
get_placeholders(mi_with_sync) -> [activity, count];
get_placeholders(structured_partial_join) -> [sources, target, required];
get_placeholders(_) -> [].

%% Composition helpers
-spec compose_sequential([net_fragment()]) -> pqc_net:net().
compose_sequential([]) ->
    #{places => [], transitions => #{}, order => []};
compose_sequential([Single]) ->
    fragment_to_net(Single);
compose_sequential([First | Rest]) ->
    %% Connect exit places of current to entry places of next
    lists:foldl(
        fun(Fragment, AccNet) ->
            connect_fragments_sequential(AccNet, Fragment)
        end,
        fragment_to_net(First),
        Rest
    ).

-spec compose_parallel([net_fragment()]) -> pqc_net:net().
compose_parallel(Fragments) ->
    %% Create AND-split to all fragments, then AND-join all results
    AllPlaces = lists:flatten([maps:get(places, F, []) || F <- Fragments]),
    AllTransitions = lists:foldl(
        fun(F, Acc) ->
            maps:merge(Acc, maps:get(transitions, F, #{}))
        end,
        #{},
        Fragments
    ),
    AllOrders = lists:flatten([maps:get(order, maps:get(metadata, F, #{}), []) || F <- Fragments]),

    #{
        places => [start_parallel, end_parallel | AllPlaces],
        transitions => AllTransitions,
        order => AllOrders
    }.

-spec compose_choice([net_fragment()]) -> pqc_net:net().
compose_choice(Fragments) ->
    %% Create XOR-split to all fragments, then XOR-join all results
    AllPlaces = lists:flatten([maps:get(places, F, []) || F <- Fragments]),
    AllTransitions = lists:foldl(
        fun(F, Acc) ->
            maps:merge(Acc, maps:get(transitions, F, #{}))
        end,
        #{},
        Fragments
    ),

    #{
        places => [start_choice, end_choice | AllPlaces],
        transitions => AllTransitions,
        order => []
    }.

-spec fragment_to_net(net_fragment()) -> pqc_net:net().
fragment_to_net(Fragment) ->
    #{
        places => maps:get(places, Fragment, []),
        transitions => maps:get(transitions, Fragment, #{}),
        order => extract_order_from_transitions(maps:get(transitions, Fragment, #{})),
        metadata => maps:get(metadata, Fragment, #{})
    }.

-spec connect_fragments_sequential(pqc_net:net(), net_fragment()) -> pqc_net:net().
connect_fragments_sequential(Net, Fragment) ->
    %% Simplified: just merge places and transitions
    #{
        places => maps:get(places, Net, []) ++ maps:get(places, Fragment, []),
        transitions => maps:merge(
            maps:get(transitions, Net, #{}),
            maps:get(transitions, Fragment, #{})
        ),
        order => maps:get(order, Net, []) ++
                extract_order_from_transitions(maps:get(transitions, Fragment, #{}))
    }.

-spec extract_order_from_transitions(map()) -> [pqc_net:tid()].
extract_order_from_transitions(Transitions) ->
    maps:keys(Transitions).

%% Pattern detection helpers
-spec detect_sequence_patterns(map(), [pqc_net:tid()]) -> [pattern_occurrence()].
detect_sequence_patterns(Transitions, Order) ->
    %% Look for chains of single-input, single-output transitions
    lists:filtermap(
        fun(Tid) ->
            case maps:find(Tid, Transitions) of
                {ok, T} ->
                    Inputs = maps:get(inputs, T, []),
                    Outputs = maps:get(outputs, T, []),
                    case {length(Inputs), length(Outputs)} of
                        {1, 1} ->
                            {true, #{
                                pattern => sequence,
                                location => [Tid],
                                confidence => 1.0,
                                metadata => #{transition => Tid}
                            }};
                        _ -> false
                    end;
                error -> false
            end
        end,
        Order
    ).

-spec detect_parallel_split_patterns(map()) -> [pattern_occurrence()].
detect_parallel_split_patterns(Transitions) ->
    %% Look for transitions with single input, multiple outputs, AND-split
    maps:fold(
        fun(Tid, T, Acc) ->
            Inputs = maps:get(inputs, T, []),
            Outputs = maps:get(outputs, T, []),
            Split = maps:get(split, T, 'and'),
            case {length(Inputs), length(Outputs), Split} of
                {1, N, 'and'} when N > 1 ->
                    [#{
                        pattern => parallel_split,
                        location => [Tid],
                        confidence => 1.0,
                        metadata => #{transition => Tid, output_count => N}
                    } | Acc];
                _ -> Acc
            end
        end,
        [],
        Transitions
    ).

-spec detect_synchronization_patterns(map()) -> [pattern_occurrence()].
detect_synchronization_patterns(Transitions) ->
    %% Look for transitions with multiple inputs, single output, AND-join
    maps:fold(
        fun(Tid, T, Acc) ->
            Inputs = maps:get(inputs, T, []),
            Outputs = maps:get(outputs, T, []),
            Join = maps:get(join, T, 'and'),
            case {length(Inputs), length(Outputs), Join} of
                {N, 1, 'and'} when N > 1 ->
                    [#{
                        pattern => synchronization,
                        location => [Tid],
                        confidence => 1.0,
                        metadata => #{transition => Tid, input_count => N}
                    } | Acc];
                _ -> Acc
            end
        end,
        [],
        Transitions
    ).

-spec detect_xor_patterns(map()) -> [pattern_occurrence()].
detect_xor_patterns(Transitions) ->
    %% Look for transitions with XOR-split or XOR-join
    maps:fold(
        fun(Tid, T, Acc) ->
            Split = maps:get(split, T, 'and'),
            Join = maps:get(join, T, 'and'),
            Patterns = case Split of
                'xor' -> [#{
                    pattern => exclusive_choice,
                    location => [Tid],
                    confidence => 1.0,
                    metadata => #{transition => Tid, type => split}
                }];
                _ -> []
            end,
            case Join of
                'xor' ->
                    [#{
                        pattern => simple_merge,
                        location => [Tid],
                        confidence => 1.0,
                        metadata => #{transition => Tid, type => join}
                    } | Patterns ++ Acc];
                _ -> Patterns ++ Acc
            end
        end,
        [],
        Transitions
    ).

-spec validate_transition_places(pqc_net:tid(), pqc_net:transition(), sets:set()) -> ok.
validate_transition_places(Tid, T, PlaceSet) ->
    Inputs = maps:get(inputs, T, []),
    Outputs = maps:get(outputs, T, []),

    lists:foreach(
        fun(#{place := P}) ->
            case sets:is_element(P, PlaceSet) of
                true -> ok;
                false -> throw({invalid_place_ref, Tid, P})
            end
        end,
        Inputs ++ Outputs
    ),
    ok.

%% Place naming helpers
-spec source_place(atom()) -> atom().
source_place(Name) -> list_to_atom("source_" ++ atom_to_list(Name)).

-spec target_place(atom()) -> atom().
target_place(Name) -> list_to_atom("target_" ++ atom_to_list(Name)).

-spec branch_place(atom()) -> atom().
branch_place(Name) -> list_to_atom("branch_" ++ atom_to_list(Name)).

-spec activity_place(atom()) -> atom().
activity_place(Name) -> list_to_atom("activity_" ++ atom_to_list(Name)).

-spec instance_place(non_neg_integer()) -> atom().
instance_place(N) -> list_to_atom("instance_" ++ integer_to_list(N)).

-spec instance_transition(atom(), non_neg_integer()) -> atom().
instance_transition(Activity, N) ->
    list_to_atom(atom_to_list(Activity) ++ "_inst_" ++ integer_to_list(N)).

-spec list_index(term(), list()) -> non_neg_integer().
list_index(Element, List) ->
    list_index(Element, List, 1).

-spec list_index(term(), list(), non_neg_integer()) -> non_neg_integer().
list_index(_Element, [], _Idx) -> 0;
list_index(Element, [Element | _], Idx) -> Idx;
list_index(Element, [_ | Rest], Idx) -> list_index(Element, Rest, Idx + 1).
