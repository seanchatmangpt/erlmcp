%%%-------------------------------------------------------------------
%%% @doc pqc_patterns_43 - Complete Van der Aalst/ter Hofstede Workflow Patterns
%%%
%%% Enumerates ALL 43 workflow patterns from the Workflow Patterns Initiative
%%% as executable Petri net fragments compatible with pqc_pattern_net.erl.
%%%
%%% References:
%%% - Van der Aalst, W.M.P., ter Hofstede, A.H.M., Kiepuszewski, B., Barros, A.P. (2003):
%%%   "Workflow Patterns", Distributed and Parallel Databases, 14(1), 5-51.
%%% - Russell, N., ter Hofstede, A.H.M., van der Aalst, W.M.P., Mulyar, N. (2006):
%%%   "Workflow Control-Flow Patterns: A Revised View", BPM Center Report BPM-06-22.
%%% - http://www.workflowpatterns.com/
%%%
%%% Pattern Categories:
%%% 1. Basic Control Flow (P1-P5): Sequence, Parallel Split, Synchronization,
%%%    Exclusive Choice, Simple Merge
%%% 2. Advanced Branching and Synchronization (P6-P9): Multi-Choice, Structured
%%%    Synchronizing Merge, Multi-Merge, Structured Discriminator
%%% 3. Structural (P10-P11): Arbitrary Cycles, Implicit Termination
%%% 4. Multiple Instance Patterns (P12-P15, P34-P36): MI without sync, MI with
%%%    design-time knowledge, MI with runtime knowledge, MI without prior knowledge,
%%%    Static/Dynamic/Cancelling Partial Join for MI
%%% 5. State-Based (P16-P18): Deferred Choice, Interleaved Parallel Routing, Milestone
%%% 6. Cancellation (P19-P20, P25-P27): Cancel Task, Cancel Case, Cancel Region,
%%%    Cancel MI Activity, Complete MI Activity
%%% 7. Iteration (P10, P21-P22): Arbitrary Cycles, Structured Loop, Recursion
%%% 8. Trigger (P23-P24): Transient Trigger, Persistent Trigger
%%% 9. Advanced Synchronization (P28-P33, P37-P38): Blocking/Cancelling Discriminator,
%%%    Structured/Blocking/Cancelling Partial Join, Generalized AND-Join,
%%%    Acyclic/General Synchronizing Merge
%%% 10. Advanced Concurrency (P39-P42): Critical Section, Interleaved Routing,
%%%     Thread Split, Thread Merge
%%% 11. Termination (P11, P43): Implicit Termination, Explicit Termination
%%%
%%% All patterns return net() structures compilable by pqc_pattern_net:compile/1.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pqc_patterns_43).

-include("pqchain.hrl").

%% API - Pattern Retrieval
-export([
    pattern/1,
    pattern_name/1,
    pattern_category/1,
    pattern_description/1,
    all_patterns/0,
    patterns_by_category/1,
    categories/0
]).

%% Pattern Construction Helpers
-export([
    with_initial_marking/2,
    with_metadata/2
]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type pattern_id() :: 1..43.
-type category() :: basic_control | advanced_branching | structural |
                   multiple_instance | state_based | cancellation |
                   iteration | trigger | advanced_sync | advanced_concurrency |
                   termination.

-export_type([pattern_id/0, category/0]).

%%--------------------------------------------------------------------
%% API - Pattern Retrieval
%%--------------------------------------------------------------------

%% @doc Return canonical net fragment for pattern N (1-43).
-spec pattern(pattern_id()) -> pqc_pattern_net:net().

%% ===================================================================
%% BASIC CONTROL FLOW PATTERNS (P1-P5)
%% ===================================================================

%% P1: Sequence - Activities executed in sequential order
pattern(1) ->
    #{
        places => [p1, p2, p3],
        transitions => #{
            a => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and'
            },
            b => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and'
            }
        },
        order => [a, b],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 1,
            pattern_name => <<"Sequence">>,
            category => basic_control
        }
    };

%% P2: Parallel Split (AND-split) - Divergence into multiple parallel branches
pattern(2) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1},
                    #{place => p4, weight => 1}
                ],
                join => 'and',
                split => 'and'  % AND-split
            }
        },
        order => [split],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 2,
            pattern_name => <<"Parallel Split (AND-split)">>,
            category => basic_control
        }
    };

%% P3: Synchronization (AND-join) - Convergence of parallel branches
pattern(3) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            sync => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => 'and',  % AND-join
                split => 'and'
            }
        },
        order => [sync],
        initial_marking => #{p1 => 1, p2 => 1, p3 => 1},
        metadata => #{
            pattern_id => 3,
            pattern_name => <<"Synchronization (AND-join)">>,
            category => basic_control
        }
    };

%% P4: Exclusive Choice (XOR-split) - Choice of one branch from multiple
pattern(4) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            choice_a => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'xor',  % XOR-split
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    maps:get(choice, Meta, a) =:= a
                end
            },
            choice_b => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'xor',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    maps:get(choice, Meta, a) =:= b
                end
            },
            choice_c => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'and',
                split => 'xor',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    maps:get(choice, Meta, a) =:= c
                end
            }
        },
        order => [choice_a, choice_b, choice_c],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 4,
            pattern_name => <<"Exclusive Choice (XOR-split)">>,
            category => basic_control
        }
    };

%% P5: Simple Merge (XOR-join) - Convergence of alternate branches
pattern(5) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            merge_a => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'xor',  % XOR-join
                split => 'and'
            },
            merge_b => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'xor',
                split => 'and'
            },
            merge_c => #{
                inputs => [#{place => p3, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'xor',
                split => 'and'
            }
        },
        order => [merge_a, merge_b, merge_c],
        initial_marking => #{p1 => 1},  % Start with token in one place
        metadata => #{
            pattern_id => 5,
            pattern_name => <<"Simple Merge (XOR-join)">>,
            category => basic_control
        }
    };

%% ===================================================================
%% ADVANCED BRANCHING AND SYNCHRONIZATION (P6-P9)
%% ===================================================================

%% P6: Multi-Choice (OR-split) - Choice of multiple branches from set
pattern(6) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            or_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1},
                    #{place => p4, weight => 1}
                ],
                join => 'and',
                split => 'or',  % OR-split
                split_policy => {choice, fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    maps:get(branches, Meta, [p2, p3])  % Default: branches 1 and 2
                end}
            }
        },
        order => [or_split],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 6,
            pattern_name => <<"Multi-Choice (OR-split)">>,
            category => advanced_branching
        }
    };

%% P7: Structured Synchronizing Merge (Structured OR-join)
%% Synchronizes branches activated by corresponding OR-split
pattern(7) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            or_join => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => 'or',  % OR-join
                or_join_kind => structured,  % Structured variant
                split => 'and'
            }
        },
        order => [or_join],
        initial_marking => #{p1 => 1, p2 => 1},  % Two branches active
        metadata => #{
            pattern_id => 7,
            pattern_name => <<"Structured Synchronizing Merge (OR-join)">>,
            category => advanced_branching
        }
    };

%% P8: Multi-Merge - Each activation triggers downstream without synchronization
pattern(8) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            multi_merge_a => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'and',
                split => 'and'
            },
            multi_merge_b => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'and',
                split => 'and'
            },
            multi_merge_c => #{
                inputs => [#{place => p3, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'and',
                split => 'and'
            }
        },
        order => [multi_merge_a, multi_merge_b, multi_merge_c],
        initial_marking => #{p1 => 1, p2 => 1, p3 => 1},
        metadata => #{
            pattern_id => 8,
            pattern_name => <<"Multi-Merge">>,
            category => advanced_branching,
            note => <<"Each incoming token immediately triggers output">>
        }
    };

%% P9: Structured Discriminator - First branch completion triggers, others ignored
pattern(9) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            discriminator => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => {partial, 1},  % Partial join, k=1
                join_policy => {discriminator, normal},  % Structured discriminator
                split => 'and'
            }
        },
        order => [discriminator],
        initial_marking => #{p1 => 1, p2 => 1, p3 => 1},
        metadata => #{
            pattern_id => 9,
            pattern_name => <<"Structured Discriminator">>,
            category => advanced_branching,
            note => <<"First arriving token triggers, subsequent tokens consumed">>
        }
    };

%% ===================================================================
%% STRUCTURAL PATTERNS (P10-P11)
%% ===================================================================

%% P10: Arbitrary Cycles - Support for arbitrary loop structures
pattern(10) ->
    #{
        places => [p1, p2, p3],
        transitions => #{
            body => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and'
            },
            loop_back => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p1, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    maps:get(continue, Meta, false)
                end
            },
            exit_loop => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    not maps:get(continue, Meta, false)
                end
            }
        },
        order => [body, loop_back, exit_loop],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 10,
            pattern_name => <<"Arbitrary Cycles">>,
            category => structural
        }
    };

%% P11: Implicit Termination - Terminate when no more work can be done
pattern(11) ->
    #{
        places => [p1, p2, p3],
        transitions => #{
            a => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and'
            },
            b => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and'
            }
        },
        order => [a, b],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 11,
            pattern_name => <<"Implicit Termination">>,
            category => structural,
            note => <<"Workflow completes when no transitions are enabled">>
        }
    };

%% ===================================================================
%% MULTIPLE INSTANCE PATTERNS (P12-P15, P34-P36)
%% ===================================================================

%% P12: Multiple Instances without Synchronization
pattern(12) ->
    #{
        places => [p1, p2],
        transitions => #{
            mi_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => {mi, 3}  % Create 3 instances
            }
        },
        order => [mi_split],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 12,
            pattern_name => <<"Multiple Instances without Synchronization">>,
            category => multiple_instance,
            note => <<"Creates N instances that execute independently">>
        }
    };

%% P13: Multiple Instances with A Priori Design-Time Knowledge
pattern(13) ->
    #{
        places => [p1, p2, p3],
        transitions => #{
            mi_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => {mi, {design, 3}}  % Design-time count
            },
            mi_join => #{
                inputs => [#{place => p2, weight => 3}],  % Wait for all 3
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and'
            }
        },
        order => [mi_split, mi_join],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 13,
            pattern_name => <<"Multiple Instances with A Priori Design-Time Knowledge">>,
            category => multiple_instance
        }
    };

%% P14: Multiple Instances with A Priori Run-Time Knowledge
pattern(14) ->
    #{
        places => [p1, p2, p3],
        transitions => #{
            mi_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => {mi, {runtime, fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    maps:get(instance_count, Meta, 1)
                end}}
            },
            mi_join => #{
                inputs => [#{place => p2, weight => 1}],  % Runtime-determined weight
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    Expected = maps:get(instance_count, Meta, 1),
                    pqc_pattern_net:token_count(p2, M) >= Expected
                end
            }
        },
        order => [mi_split, mi_join],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 14,
            pattern_name => <<"Multiple Instances with A Priori Run-Time Knowledge">>,
            category => multiple_instance
        }
    };

%% P15: Multiple Instances without A Priori Run-Time Knowledge
pattern(15) ->
    #{
        places => [p1, p2, p3],
        transitions => #{
            create_instance => #{
                inputs => [#{place => p1, weight => 0}],  % Non-consuming read
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    maps:get(create_more, Meta, false)
                end
            },
            complete_all => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    not maps:get(create_more, Meta, false) andalso
                    pqc_pattern_net:token_count(p2, M) =:= 0
                end
            }
        },
        order => [create_instance, complete_all],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 15,
            pattern_name => <<"Multiple Instances without A Priori Run-Time Knowledge">>,
            category => multiple_instance
        }
    };

%% P34: Static Partial Join for Multiple Instances
pattern(34) ->
    #{
        places => [p1, p2, p3],
        transitions => #{
            mi_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => {mi, 5}  % Create 5 instances
            },
            partial_join => #{
                inputs => [#{place => p2, weight => 3}],  % Wait for 3 out of 5
                outputs => [#{place => p3, weight => 1}],
                join => {partial, 3},  % Static partial join
                split => 'and'
            }
        },
        order => [mi_split, partial_join],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 34,
            pattern_name => <<"Static Partial Join for Multiple Instances">>,
            category => multiple_instance
        }
    };

%% P35: Cancelling Partial Join for Multiple Instances
pattern(35) ->
    #{
        places => [p1, p2, p3],
        transitions => #{
            mi_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => {mi, 5}
            },
            partial_join => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => {partial, 3},
                join_policy => {partial_policy, canceling},  % Cancel remaining
                split => 'and'
            }
        },
        order => [mi_split, partial_join],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 35,
            pattern_name => <<"Cancelling Partial Join for Multiple Instances">>,
            category => multiple_instance
        }
    };

%% P36: Dynamic Partial Join for Multiple Instances
pattern(36) ->
    #{
        places => [p1, p2, p3],
        transitions => #{
            mi_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => {mi, {runtime, fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    maps:get(total_instances, Meta, 5)
                end}}
            },
            dynamic_partial_join => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => {partial, 1},  % Will be adjusted dynamically
                join_policy => {partial_policy, structured},
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    Total = maps:get(total_instances, Meta, 5),
                    Required = maps:get(required_instances, Meta, 3),
                    pqc_pattern_net:token_count(p2, M) >= Required
                end
            }
        },
        order => [mi_split, dynamic_partial_join],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 36,
            pattern_name => <<"Dynamic Partial Join for Multiple Instances">>,
            category => multiple_instance
        }
    };

%% ===================================================================
%% STATE-BASED PATTERNS (P16-P18)
%% ===================================================================

%% P16: Deferred Choice - Choice resolved by environment, not process
pattern(16) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            choice_a => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and',
                trigger => {transient, <<"event_a">>}
            },
            choice_b => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and',
                trigger => {transient, <<"event_b">>}
            },
            choice_c => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'and',
                split => 'and',
                trigger => {transient, <<"event_c">>}
            }
        },
        order => [choice_a, choice_b, choice_c],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 16,
            pattern_name => <<"Deferred Choice">>,
            category => state_based,
            note => <<"First external event to arrive determines the path">>
        }
    };

%% P17: Interleaved Parallel Routing - Parallel activities with ordered execution
pattern(17) ->
    #{
        places => [p1, p2, p3, p4, mutex],
        transitions => #{
            a => #{
                inputs => [#{place => p1, weight => 1}, #{place => mutex, weight => 1}],
                outputs => [#{place => p2, weight => 1}, #{place => mutex, weight => 1}],
                join => 'and',
                split => 'and'
            },
            b => #{
                inputs => [#{place => p3, weight => 1}, #{place => mutex, weight => 1}],
                outputs => [#{place => p4, weight => 1}, #{place => mutex, weight => 1}],
                join => 'and',
                split => 'and'
            }
        },
        order => [a, b],
        initial_marking => #{p1 => 1, p3 => 1, mutex => 1},
        metadata => #{
            pattern_id => 17,
            pattern_name => <<"Interleaved Parallel Routing">>,
            category => state_based,
            note => <<"Mutual exclusion via mutex place">>
        }
    };

%% P18: Milestone - Activity enabled only if milestone state holds
pattern(18) ->
    #{
        places => [p1, p2, p3, milestone_state],
        transitions => #{
            establish_milestone => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}, #{place => milestone_state, weight => 1}],
                join => 'and',
                split => 'and'
            },
            activity_with_milestone => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    pqc_pattern_net:token_count(milestone_state, M) > 0
                end
            },
            invalidate_milestone => #{
                inputs => [#{place => milestone_state, weight => 1}],
                outputs => [],
                join => 'and',
                split => 'and'
            }
        },
        order => [establish_milestone, activity_with_milestone, invalidate_milestone],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 18,
            pattern_name => <<"Milestone">>,
            category => state_based
        }
    };

%% ===================================================================
%% CANCELLATION PATTERNS (P19-P20, P25-P27)
%% ===================================================================

%% P19: Cancel Activity - Cancel specific activity instance
pattern(19) ->
    #{
        places => [p1, p2, p_activity, p_cancelled],
        transitions => #{
            start_activity => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p_activity, weight => 1}],
                join => 'and',
                split => 'and'
            },
            normal_completion => #{
                inputs => [#{place => p_activity, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and'
            },
            cancel_activity => #{
                inputs => [#{place => p_activity, weight => 1}],
                outputs => [#{place => p_cancelled, weight => 1}],
                join => 'and',
                split => 'and',
                cancel => {cancel_task, normal_completion}
            }
        },
        order => [start_activity, normal_completion, cancel_activity],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 19,
            pattern_name => <<"Cancel Activity">>,
            category => cancellation
        }
    };

%% P20: Cancel Case - Cancel entire case instance
pattern(20) ->
    #{
        places => [p1, p2, p3, p_cancel_trigger],
        transitions => #{
            a => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and'
            },
            b => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and'
            },
            cancel_case => #{
                inputs => [#{place => p_cancel_trigger, weight => 1}],
                outputs => [],
                join => 'and',
                split => 'and',
                cancel => explicit_terminate  % Terminate entire case
            }
        },
        order => [a, b, cancel_case],
        initial_marking => #{p1 => 1, p_cancel_trigger => 0},
        metadata => #{
            pattern_id => 20,
            pattern_name => <<"Cancel Case">>,
            category => cancellation
        }
    };

%% P25: Cancel Region - Cancel workflow region
pattern(25) ->
    #{
        places => [p1, p2, p3, p4, p_cancel],
        transitions => #{
            enter_region => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and'
            },
            inside_region => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and'
            },
            cancel_region => #{
                inputs => [#{place => p_cancel, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'and',
                split => 'and',
                cancel => {cancel_region, <<"region_1">>}
            }
        },
        order => [enter_region, inside_region, cancel_region],
        initial_marking => #{p1 => 1, p_cancel => 0},
        metadata => #{
            pattern_id => 25,
            pattern_name => <<"Cancel Region">>,
            category => cancellation
        }
    };

%% P26: Cancel Multiple Instance Activity
pattern(26) ->
    #{
        places => [p1, p2, p3, p_cancel],
        transitions => #{
            mi_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => {mi, 5}
            },
            mi_activity => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and'
            },
            cancel_mi => #{
                inputs => [#{place => p_cancel, weight => 1}],
                outputs => [],
                join => 'and',
                split => 'and',
                cancel => {cancel_mi_task, mi_activity}
            }
        },
        order => [mi_split, mi_activity, cancel_mi],
        initial_marking => #{p1 => 1, p_cancel => 0},
        metadata => #{
            pattern_id => 26,
            pattern_name => <<"Cancel Multiple Instance Activity">>,
            category => cancellation
        }
    };

%% P27: Complete Multiple Instance Activity
pattern(27) ->
    #{
        places => [p1, p2, p3, p_complete],
        transitions => #{
            mi_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => {mi, 5}
            },
            mi_activity => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}],
                join => 'and',
                split => 'and'
            },
            complete_mi => #{
                inputs => [#{place => p_complete, weight => 1}],
                outputs => [],
                join => 'and',
                split => 'and',
                cancel => {complete_mi_task, mi_activity}
            }
        },
        order => [mi_split, mi_activity, complete_mi],
        initial_marking => #{p1 => 1, p_complete => 0},
        metadata => #{
            pattern_id => 27,
            pattern_name => <<"Complete Multiple Instance Activity">>,
            category => cancellation
        }
    };

%% ===================================================================
%% ITERATION PATTERNS (P21-P22)
%% ===================================================================

%% P21: Structured Loop - While/for loop construct
pattern(21) ->
    #{
        places => [p1, p_loop_body, p_exit],
        transitions => #{
            enter_loop => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p_loop_body, weight => 1}],
                join => 'and',
                split => 'and'
            },
            loop_body => #{
                inputs => [#{place => p_loop_body, weight => 1}],
                outputs => [#{place => p_loop_body, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    Iteration = maps:get(iteration, Meta, 0),
                    MaxIter = maps:get(max_iterations, Meta, 10),
                    Iteration < MaxIter
                end,
                effect => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    Iteration = maps:get(iteration, Meta, 0),
                    {ok, [], Meta#{iteration => Iteration + 1}}
                end
            },
            exit_loop => #{
                inputs => [#{place => p_loop_body, weight => 1}],
                outputs => [#{place => p_exit, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    Iteration = maps:get(iteration, Meta, 0),
                    MaxIter = maps:get(max_iterations, Meta, 10),
                    Iteration >= MaxIter
                end
            }
        },
        order => [enter_loop, loop_body, exit_loop],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 21,
            pattern_name => <<"Structured Loop">>,
            category => iteration
        }
    };

%% P22: Recursion - Recursive invocation of workflow fragment
pattern(22) ->
    #{
        places => [p_entry, p_recurse, p_base_case, p_exit],
        transitions => #{
            check_base_case => #{
                inputs => [#{place => p_entry, weight => 1}],
                outputs => [#{place => p_base_case, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    Depth = maps:get(depth, Meta, 0),
                    Depth >= 3  % Base case: depth 3
                end
            },
            recurse => #{
                inputs => [#{place => p_entry, weight => 1}],
                outputs => [#{place => p_recurse, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    Depth = maps:get(depth, Meta, 0),
                    Depth < 3
                end,
                effect => fun(M) ->
                    Meta = maps:get(meta, M, #{}),
                    Depth = maps:get(depth, Meta, 0),
                    {ok, [], Meta#{depth => Depth + 1}}
                end
            },
            recursive_call => #{
                inputs => [#{place => p_recurse, weight => 1}],
                outputs => [#{place => p_entry, weight => 1}],  % Loop back
                join => 'and',
                split => 'and'
            },
            complete => #{
                inputs => [#{place => p_base_case, weight => 1}],
                outputs => [#{place => p_exit, weight => 1}],
                join => 'and',
                split => 'and'
            }
        },
        order => [check_base_case, recurse, recursive_call, complete],
        initial_marking => #{p_entry => 1},
        metadata => #{
            pattern_id => 22,
            pattern_name => <<"Recursion">>,
            category => iteration
        }
    };

%% ===================================================================
%% TRIGGER PATTERNS (P23-P24)
%% ===================================================================

%% P23: Transient Trigger - External trigger enables activity temporarily
pattern(23) ->
    #{
        places => [p1, p2],
        transitions => #{
            triggered_activity => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and',
                trigger => {transient, <<"external_event">>}
            }
        },
        order => [triggered_activity],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 23,
            pattern_name => <<"Transient Trigger">>,
            category => trigger,
            note => <<"Trigger consumed after use">>
        }
    };

%% P24: Persistent Trigger - External trigger enables activity repeatedly
pattern(24) ->
    #{
        places => [p1, p2],
        transitions => #{
            persistent_triggered => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and',
                trigger => {persistent, <<"persistent_event">>}
            }
        },
        order => [persistent_triggered],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 24,
            pattern_name => <<"Persistent Trigger">>,
            category => trigger,
            note => <<"Trigger remains available after use">>
        }
    };

%% ===================================================================
%% ADVANCED SYNCHRONIZATION PATTERNS (P28-P33, P37-P38)
%% ===================================================================

%% P28: Blocking Discriminator
pattern(28) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            discriminator => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => {partial, 1},
                join_policy => {discriminator, blocking},  % Blocking variant
                split => 'and'
            }
        },
        order => [discriminator],
        initial_marking => #{p1 => 1, p2 => 1, p3 => 1},
        metadata => #{
            pattern_id => 28,
            pattern_name => <<"Blocking Discriminator">>,
            category => advanced_sync
        }
    };

%% P29: Cancelling Discriminator
pattern(29) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            discriminator => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => {partial, 1},
                join_policy => {discriminator, canceling},  % Canceling variant
                split => 'and'
            }
        },
        order => [discriminator],
        initial_marking => #{p1 => 1, p2 => 1, p3 => 1},
        metadata => #{
            pattern_id => 29,
            pattern_name => <<"Cancelling Discriminator">>,
            category => advanced_sync
        }
    };

%% P30: Structured Partial Join - N out of M branches required
pattern(30) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            partial_join => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => {partial, 2},  % 2 out of 3
                join_policy => {partial_policy, structured},
                split => 'and'
            }
        },
        order => [partial_join],
        initial_marking => #{p1 => 1, p2 => 1, p3 => 1},
        metadata => #{
            pattern_id => 30,
            pattern_name => <<"Structured Partial Join">>,
            category => advanced_sync
        }
    };

%% P31: Blocking Partial Join
pattern(31) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            blocking_partial_join => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => {partial, 2},
                join_policy => {partial_policy, blocking},
                split => 'and'
            }
        },
        order => [blocking_partial_join],
        initial_marking => #{p1 => 1, p2 => 1, p3 => 1},
        metadata => #{
            pattern_id => 31,
            pattern_name => <<"Blocking Partial Join">>,
            category => advanced_sync
        }
    };

%% P32: Cancelling Partial Join
pattern(32) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            cancelling_partial_join => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => {partial, 2},
                join_policy => {partial_policy, canceling},
                split => 'and'
            }
        },
        order => [cancelling_partial_join],
        initial_marking => #{p1 => 1, p2 => 1, p3 => 1},
        metadata => #{
            pattern_id => 32,
            pattern_name => <<"Cancelling Partial Join">>,
            category => advanced_sync
        }
    };

%% P33: Generalized AND-Join - Awaits all tokens from all active paths
pattern(33) ->
    #{
        places => [p1, p2, p3, p4, p5],
        transitions => #{
            split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}, #{place => p3, weight => 1}],
                join => 'and',
                split => 'and'
            },
            generalized_and_join => #{
                inputs => [#{place => p2, weight => 1}, #{place => p3, weight => 1}],
                outputs => [#{place => p4, weight => 1}],
                join => 'and',  % Synchronizes all active paths
                split => 'and'
            }
        },
        order => [split, generalized_and_join],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 33,
            pattern_name => <<"Generalized AND-Join">>,
            category => advanced_sync
        }
    };

%% P37: Acyclic Synchronizing Merge (Local OR-join)
pattern(37) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            or_join => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => 'or',
                or_join_kind => local,  % Local (acyclic) OR-join
                split => 'and'
            }
        },
        order => [or_join],
        initial_marking => #{p1 => 1, p2 => 1},
        metadata => #{
            pattern_id => 37,
            pattern_name => <<"Acyclic Synchronizing Merge">>,
            category => advanced_sync
        }
    };

%% P38: General Synchronizing Merge (General OR-join)
pattern(38) ->
    #{
        places => [p1, p2, p3, p4],
        transitions => #{
            general_or_join => #{
                inputs => [
                    #{place => p1, weight => 1},
                    #{place => p2, weight => 1},
                    #{place => p3, weight => 1}
                ],
                outputs => [#{place => p4, weight => 1}],
                join => 'or',
                or_join_kind => general,  % General OR-join
                split => 'and'
            }
        },
        order => [general_or_join],
        initial_marking => #{p1 => 1, p2 => 1},
        metadata => #{
            pattern_id => 38,
            pattern_name => <<"General Synchronizing Merge">>,
            category => advanced_sync
        }
    };

%% ===================================================================
%% ADVANCED CONCURRENCY PATTERNS (P39-P42)
%% ===================================================================

%% P39: Critical Section - Mutual exclusion for resource access
pattern(39) ->
    #{
        places => [p1, p2, p3, p_critical, p_mutex],
        transitions => #{
            enter_critical => #{
                inputs => [#{place => p1, weight => 1}, #{place => p_mutex, weight => 1}],
                outputs => [#{place => p_critical, weight => 1}],
                join => 'and',
                split => 'and',
                mutex => <<"critical_resource">>
            },
            critical_work => #{
                inputs => [#{place => p_critical, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and'
            },
            exit_critical => #{
                inputs => [#{place => p2, weight => 1}],
                outputs => [#{place => p3, weight => 1}, #{place => p_mutex, weight => 1}],
                join => 'and',
                split => 'and'
            }
        },
        order => [enter_critical, critical_work, exit_critical],
        initial_marking => #{p1 => 1, p_mutex => 1},
        metadata => #{
            pattern_id => 39,
            pattern_name => <<"Critical Section">>,
            category => advanced_concurrency
        }
    };

%% P40: Interleaved Routing - Multiple paths with serialized execution
pattern(40) ->
    #{
        places => [p1, p2, p3, p4, p_sem],
        transitions => #{
            path_a => #{
                inputs => [#{place => p1, weight => 1}, #{place => p_sem, weight => 1}],
                outputs => [#{place => p2, weight => 1}, #{place => p_sem, weight => 1}],
                join => 'and',
                split => 'and',
                mutex => <<"interleaved_resource">>
            },
            path_b => #{
                inputs => [#{place => p3, weight => 1}, #{place => p_sem, weight => 1}],
                outputs => [#{place => p4, weight => 1}, #{place => p_sem, weight => 1}],
                join => 'and',
                split => 'and',
                mutex => <<"interleaved_resource">>
            }
        },
        order => [path_a, path_b],
        initial_marking => #{p1 => 1, p3 => 1, p_sem => 1},
        metadata => #{
            pattern_id => 40,
            pattern_name => <<"Interleaved Routing">>,
            category => advanced_concurrency
        }
    };

%% P41: Thread Split - Split into N threads
pattern(41) ->
    #{
        places => [p1, p2],
        transitions => #{
            thread_split => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => {thread, 3}  % Create 3 threads
            }
        },
        order => [thread_split],
        initial_marking => #{p1 => 1},
        metadata => #{
            pattern_id => 41,
            pattern_name => <<"Thread Split">>,
            category => advanced_concurrency,
            note => <<"Creates N identical threads from single token">>
        }
    };

%% P42: Thread Merge - Merge N threads
pattern(42) ->
    #{
        places => [p1, p2],
        transitions => #{
            thread_merge => #{
                inputs => [#{place => p1, weight => 3}],  % Wait for 3 threads
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and'
            }
        },
        order => [thread_merge],
        initial_marking => #{p1 => 3},  % Start with 3 threads
        metadata => #{
            pattern_id => 42,
            pattern_name => <<"Thread Merge">>,
            category => advanced_concurrency,
            note => <<"Merges N threads into single token">>
        }
    };

%% ===================================================================
%% TERMINATION PATTERNS (P43)
%% ===================================================================

%% P43: Explicit Termination - Explicit termination signal
pattern(43) ->
    #{
        places => [p1, p2, p_terminate],
        transitions => #{
            work => #{
                inputs => [#{place => p1, weight => 1}],
                outputs => [#{place => p2, weight => 1}],
                join => 'and',
                split => 'and'
            },
            explicit_terminate => #{
                inputs => [#{place => p_terminate, weight => 1}],
                outputs => [],
                join => 'and',
                split => 'and',
                cancel => explicit_terminate  % Explicit termination
            }
        },
        order => [work, explicit_terminate],
        initial_marking => #{p1 => 1, p_terminate => 0},
        metadata => #{
            pattern_id => 43,
            pattern_name => <<"Explicit Termination">>,
            category => termination,
            note => <<"Terminate with explicit signal, clearing all tokens">>
        }
    }.

%%--------------------------------------------------------------------
%% Pattern Metadata API
%%--------------------------------------------------------------------

%% @doc Return human-readable name for pattern N.
-spec pattern_name(pattern_id()) -> binary().
pattern_name(N) ->
    Net = pattern(N),
    Meta = maps:get(metadata, Net, #{}),
    maps:get(pattern_name, Meta, <<"Unknown">>).

%% @doc Return category atom for pattern N.
-spec pattern_category(pattern_id()) -> category().
pattern_category(N) ->
    Net = pattern(N),
    Meta = maps:get(metadata, Net, #{}),
    maps:get(category, Meta, undefined).

%% @doc Return description from the book for pattern N.
-spec pattern_description(pattern_id()) -> binary().
pattern_description(1) -> <<"An activity in a process is enabled after the completion of another activity in the same process.">>;
pattern_description(2) -> <<"A point in the process where a single thread of control splits into multiple threads of control which can be executed in parallel.">>;
pattern_description(3) -> <<"A point in the process where multiple parallel subprocesses/activities converge to a single thread of control, thus synchronizing multiple threads.">>;
pattern_description(4) -> <<"A point in the process where, based on a decision or workflow control data, one of several branches is chosen.">>;
pattern_description(5) -> <<"A point in the process where two or more alternative branches come together without synchronization.">>;
pattern_description(6) -> <<"A point in a process where, based on a decision or workflow control data, a number of branches are chosen.">>;
pattern_description(7) -> <<"A point in the process where multiple paths converge into a single thread. The thread should wait until all active paths (i.e., selected by a preceding OR-split) have completed.">>;
pattern_description(8) -> <<"The convergence of two or more branches into a single subsequent branch such that each enablement of an incoming branch results in the thread of control being passed to the subsequent branch.">>;
pattern_description(9) -> <<"The convergence of two or more branches into a single subsequent branch. The thread of control is passed to the subsequent branch when the first incoming branch is enabled. Subsequent enablements do not result in the thread of control being passed on.">>;
pattern_description(10) -> <<"A point in the process where an activity can loop back to an earlier point in the process allowing it to be re-executed, possibly more than once.">>;
pattern_description(11) -> <<"A given subprocess should be terminated when there is nothing else to be done. In other words, there are no active activities in the process and no other activity can be made active (and at the same time there are no active subprocesses).">>;
pattern_description(12) -> <<"Within a given process instance, multiple instances of an activity can be created, i.e., there is a facility to spawn off new threads of control. These threads of control are independent of the main thread of control.">>;
pattern_description(13) -> <<"Within a given process instance, multiple instances of an activity can be created and are known at design time. Once all of the instances are completed, some other activity needs to be started.">>;
pattern_description(14) -> <<"Within a given process instance, multiple instances of an activity can be created. The number of instances of a given activity that are required is known at runtime before the instances must be created. Once all of the instances are completed, some other activity needs to be started.">>;
pattern_description(15) -> <<"Within a given process instance, multiple instances of an activity can be created. The required number of instances may depend on a number of runtime factors, including state data, resource availability and inter-process communications, and is not known until the final instance has completed. After the final instance is completed, some other activity needs to be started.">>;
pattern_description(16) -> <<"A point in a process where one of several branches is chosen. In contrast to the XOR-split, the choice is not made explicitly (e.g., based on data or a workflow control decision) but several alternatives are offered to the environment. The choice is made by the environment.">>;
pattern_description(17) -> <<"A set of activities is executed in an arbitrary order: each activity in the set needs to be executed, the order is decided at run-time and no two activities are executed at the same moment (i.e., no two activities are active for the same process instance at the same time).">>;
pattern_description(18) -> <<"An activity is only enabled if a certain milestone is reached in the process. The semantics of milestone is: the activity is enabled after the milestone has been reached and can be done any time after, even after the milestone has expired.">>;
pattern_description(19) -> <<"An enabled activity is disabled and removed from the execution environment. Once the activity is cancelled, a thread of control may not pass through it.">>;
pattern_description(20) -> <<"A complete process instance is removed. If the process instance has subprocesses, these are cancelled as well. If the process instance is part of a higher level process, the higher level process continues.">>;
pattern_description(21) -> <<"The ability to execute an activity or subprocess repeatedly under the control of a structured loop construct (e.g., while, repeat-until, for loops).">>;
pattern_description(22) -> <<"The ability to recursively invoke a subprocess fragment from within itself.">>;
pattern_description(23) -> <<"The ability for a task to be triggered by a signal from the external environment. The signal is transient and if not acted upon immediately it is lost.">>;
pattern_description(24) -> <<"The ability for a task to be triggered by a signal from the external environment. The signal is persistent and remains available until it is acted upon.">>;
pattern_description(25) -> <<"The ability to cancel a complete region of a process instance. The region usually consists of several activities which are removed together.">>;
pattern_description(26) -> <<"The ability to cancel individual instances of a multi-instance activity during execution. The remaining instances can continue.">>;
pattern_description(27) -> <<"The ability to force all remaining instances of a multi-instance activity to complete immediately.">>;
pattern_description(28) -> <<"Like the discriminator pattern, but after the first branch completes, the discriminator is reset and can accept another completion.">>;
pattern_description(29) -> <<"Like the discriminator pattern, but after the first branch completes, all remaining active branches are cancelled.">>;
pattern_description(30) -> <<"The convergence of N out of M incoming branches. After N incoming branches have been enabled, the outgoing activity is triggered and the discriminator is reset.">>;
pattern_description(31) -> <<"Like the structured partial join, but after N completions, the join blocks and waits for all M branches to complete before resetting.">>;
pattern_description(32) -> <<"Like the structured partial join, but after N completions, all remaining branches are cancelled.">>;
pattern_description(33) -> <<"The general form of AND-join which synchronizes multiple threads converging on a single point. It extends the basic AND-join to handle more complex scenarios.">>;
pattern_description(34) -> <<"Multiple instances are spawned and a fixed subset (known at design time) must complete before proceeding.">>;
pattern_description(35) -> <<"Multiple instances are spawned and a fixed subset must complete. When the subset completes, the remaining instances are cancelled.">>;
pattern_description(36) -> <<"Multiple instances are spawned and a dynamically determined subset must complete before proceeding.">>;
pattern_description(37) -> <<"The synchronization of multiple threads without cycles. Similar to structured OR-join but applicable to acyclic process structures.">>;
pattern_description(38) -> <<"The most general form of OR-join which can synchronize multiple threads even in the presence of cycles.">>;
pattern_description(39) -> <<"The execution of an activity by a single thread at a time. No other instances can execute the activity until the current instance completes.">>;
pattern_description(40) -> <<"Multiple activities can proceed in parallel but their execution is interleaved (i.e., serialized) through a common resource or control mechanism.">>;
pattern_description(41) -> <<"A single thread of control is split into multiple concurrent threads that are identical copies.">>;
pattern_description(42) -> <<"Multiple identical threads converge into a single thread.">>;
pattern_description(43) -> <<"The explicit termination of a process instance via a special termination activity or signal.">>.

%% @doc Return list of all pattern IDs (1-43).
-spec all_patterns() -> [pattern_id()].
all_patterns() ->
    lists:seq(1, 43).

%% @doc Return list of pattern IDs in given category.
-spec patterns_by_category(category()) -> [pattern_id()].
patterns_by_category(Category) ->
    [N || N <- all_patterns(), pattern_category(N) =:= Category].

%% @doc Return list of all categories.
-spec categories() -> [category()].
categories() ->
    [
        basic_control,
        advanced_branching,
        structural,
        multiple_instance,
        state_based,
        cancellation,
        iteration,
        trigger,
        advanced_sync,
        advanced_concurrency,
        termination
    ].

%%--------------------------------------------------------------------
%% Pattern Construction Helpers
%%--------------------------------------------------------------------

%% @doc Override initial marking for pattern.
-spec with_initial_marking(pqc_pattern_net:net(), #{atom() => non_neg_integer()}) ->
    pqc_pattern_net:net().
with_initial_marking(Net, Marking) ->
    Net#{initial_marking => Marking}.

%% @doc Add/override metadata for pattern.
-spec with_metadata(pqc_pattern_net:net(), map()) -> pqc_pattern_net:net().
with_metadata(Net, Meta) ->
    Existing = maps:get(metadata, Net, #{}),
    Net#{metadata => maps:merge(Existing, Meta)}.
