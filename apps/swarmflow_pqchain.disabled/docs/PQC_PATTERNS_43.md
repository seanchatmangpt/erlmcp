# Van der Aalst Workflow Patterns - Complete Implementation

## Overview

`pqc_patterns_43.erl` provides a comprehensive implementation of all 43 workflow control-flow patterns from the Workflow Patterns Initiative (Van der Aalst, ter Hofstede, et al.). Each pattern is implemented as an executable Petri net fragment compatible with `pqc_pattern_net.erl`.

## References

- Van der Aalst, W.M.P., ter Hofstede, A.H.M., Kiepuszewski, B., Barros, A.P. (2003): "Workflow Patterns", Distributed and Parallel Databases, 14(1), 5-51.
- Russell, N., ter Hofstede, A.H.M., van der Aalst, W.M.P., Mulyar, N. (2006): "Workflow Control-Flow Patterns: A Revised View", BPM Center Report BPM-06-22.
- http://www.workflowpatterns.com/

## Pattern Categories

### 1. Basic Control Flow (P1-P5)
| ID | Name | Description |
|----|------|-------------|
| P1 | Sequence | Activities executed in sequential order |
| P2 | Parallel Split (AND-split) | Divergence into multiple parallel branches |
| P3 | Synchronization (AND-join) | Convergence of parallel branches |
| P4 | Exclusive Choice (XOR-split) | Choice of one branch from multiple |
| P5 | Simple Merge (XOR-join) | Convergence of alternate branches |

### 2. Advanced Branching and Synchronization (P6-P9)
| ID | Name | Description |
|----|------|-------------|
| P6 | Multi-Choice (OR-split) | Choice of multiple branches from set |
| P7 | Structured Synchronizing Merge | Synchronize branches from OR-split |
| P8 | Multi-Merge | Each activation triggers downstream |
| P9 | Structured Discriminator | First completion triggers, others ignored |

### 3. Structural Patterns (P10-P11)
| ID | Name | Description |
|----|------|-------------|
| P10 | Arbitrary Cycles | Support for arbitrary loop structures |
| P11 | Implicit Termination | Terminate when no work remains |

### 4. Multiple Instance Patterns (P12-P15, P34-P36)
| ID | Name | Description |
|----|------|-------------|
| P12 | MI without Synchronization | Multiple instances, no waiting |
| P13 | MI with A Priori Design-Time Knowledge | Fixed count at design time |
| P14 | MI with A Priori Run-Time Knowledge | Count determined at runtime |
| P15 | MI without A Priori Run-Time Knowledge | Dynamic instance creation |
| P34 | Static Partial Join for MI | Wait for N out of M instances |
| P35 | Cancelling Partial Join for MI | Cancel remaining after N complete |
| P36 | Dynamic Partial Join for MI | Runtime-determined partial join |

### 5. State-Based Patterns (P16-P18)
| ID | Name | Description |
|----|------|-------------|
| P16 | Deferred Choice | Environment resolves choice |
| P17 | Interleaved Parallel Routing | Parallel with mutual exclusion |
| P18 | Milestone | Activity enabled by milestone state |

### 6. Cancellation Patterns (P19-P20, P25-P27)
| ID | Name | Description |
|----|------|-------------|
| P19 | Cancel Activity | Cancel specific activity instance |
| P20 | Cancel Case | Cancel entire case instance |
| P25 | Cancel Region | Cancel workflow region |
| P26 | Cancel Multiple Instance Activity | Cancel MI activity instances |
| P27 | Complete Multiple Instance Activity | Force MI completion |

### 7. Iteration Patterns (P21-P22)
| ID | Name | Description |
|----|------|-------------|
| P21 | Structured Loop | While/for loop construct |
| P22 | Recursion | Recursive workflow invocation |

### 8. Trigger Patterns (P23-P24)
| ID | Name | Description |
|----|------|-------------|
| P23 | Transient Trigger | Temporary external trigger |
| P24 | Persistent Trigger | Persistent external trigger |

### 9. Advanced Synchronization Patterns (P28-P33, P37-P38)
| ID | Name | Description |
|----|------|-------------|
| P28 | Blocking Discriminator | Discriminator with blocking |
| P29 | Cancelling Discriminator | Discriminator with cancellation |
| P30 | Structured Partial Join | N out of M branches required |
| P31 | Blocking Partial Join | Partial join with blocking |
| P32 | Cancelling Partial Join | Partial join with cancellation |
| P33 | Generalized AND-Join | General AND synchronization |
| P37 | Acyclic Synchronizing Merge | Local OR-join (acyclic) |
| P38 | General Synchronizing Merge | General OR-join |

### 10. Advanced Concurrency Patterns (P39-P42)
| ID | Name | Description |
|----|------|-------------|
| P39 | Critical Section | Mutual exclusion for resources |
| P40 | Interleaved Routing | Serialized parallel paths |
| P41 | Thread Split | Split into N identical threads |
| P42 | Thread Merge | Merge N threads into one |

### 11. Termination Patterns (P43)
| ID | Name | Description |
|----|------|-------------|
| P43 | Explicit Termination | Explicit termination signal |

## API Reference

### Pattern Retrieval

```erlang
%% Get pattern by ID (1-43)
-spec pattern(pattern_id()) -> pqc_pattern_net:net().
Net = pqc_patterns_43:pattern(1).  % Returns Sequence pattern

%% Get pattern name
-spec pattern_name(pattern_id()) -> binary().
Name = pqc_patterns_43:pattern_name(1).  % Returns <<"Sequence">>

%% Get pattern category
-spec pattern_category(pattern_id()) -> category().
Category = pqc_patterns_43:pattern_category(1).  % Returns basic_control

%% Get pattern description
-spec pattern_description(pattern_id()) -> binary().
Description = pqc_patterns_43:pattern_description(1).  % Returns full description

%% Get all pattern IDs
-spec all_patterns() -> [pattern_id()].
All = pqc_patterns_43:all_patterns().  % Returns [1,2,3,...,43]

%% Get patterns by category
-spec patterns_by_category(category()) -> [pattern_id()].
Basic = pqc_patterns_43:patterns_by_category(basic_control).  % Returns [1,2,3,4,5]

%% Get all categories
-spec categories() -> [category()].
Cats = pqc_patterns_43:categories().  % Returns all category atoms
```

### Pattern Customization

```erlang
%% Customize initial marking
-spec with_initial_marking(net(), #{atom() => non_neg_integer()}) -> net().
Net = pqc_patterns_43:pattern(1),
CustomNet = pqc_patterns_43:with_initial_marking(Net, #{p2 => 1}).

%% Add custom metadata
-spec with_metadata(net(), map()) -> net().
Net = pqc_patterns_43:pattern(1),
CustomNet = pqc_patterns_43:with_metadata(Net, #{
    workflow_id => <<"my_workflow">>,
    version => <<"1.0">>
}).
```

## Usage Examples

### Basic Pattern Execution

```erlang
%% 1. Retrieve pattern
Net = pqc_patterns_43:pattern(1),  % P1: Sequence

%% 2. Compile (adds reachability caches)
Compiled = pqc_pattern_net:compile(Net),

%% 3. Get initial marking
M0 = pqc_pattern_net:initial_marking(Compiled),

%% 4. Check enabled transitions
Enabled = pqc_pattern_net:enabled(Compiled, M0),  % Returns [a]

%% 5. Fire transition
{ok, M1, Effects} = pqc_pattern_net:fire(Compiled, a, M0),

%% 6. Continue execution
{ok, M2, _} = pqc_pattern_net:fire(Compiled, b, M1),

%% 7. Check termination
IsTerminated = pqc_pattern_net:is_implicitly_terminated(Compiled, M2).  % true
```

### Parallel Split Example (P2)

```erlang
Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(2)),
M0 = pqc_pattern_net:initial_marking(Net),

%% Fire split - creates 3 parallel branches
{ok, M1, Effects} = pqc_pattern_net:fire(Net, split, M0),

%% Check results
1 = pqc_pattern_net:token_count(p2, M1),
1 = pqc_pattern_net:token_count(p3, M1),
1 = pqc_pattern_net:token_count(p4, M1).
```

### Exclusive Choice Example (P4)

```erlang
Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(4)),
M0 = pqc_pattern_net:initial_marking(Net),

%% Set choice via metadata
M1 = M0#{meta => #{choice => b}},

%% Only choice_b will be enabled
Enabled = pqc_pattern_net:enabled(Net, M1),  % [choice_b]

{ok, M2, _} = pqc_pattern_net:fire(Net, choice_b, M1),
1 = pqc_pattern_net:token_count(p3, M2).  % Token in branch b
```

### Discriminator Example (P9)

```erlang
Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(9)),
M0 = pqc_pattern_net:initial_marking(Net),  % Tokens in p1, p2, p3

%% Discriminator fires on first arrival
{ok, M1, Effects} = pqc_pattern_net:fire(Net, discriminator, M0),

%% Check discriminator behavior
true = lists:any(
    fun({discriminator_closed, _}) -> true; (_) -> false end,
    Effects
).
```

### Multiple Instances Example (P12)

```erlang
Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(12)),
M0 = pqc_pattern_net:initial_marking(Net),

%% Create 3 instances
{ok, M1, Effects} = pqc_pattern_net:fire(Net, mi_split, M0),

%% Verify MI tokens
3 = pqc_pattern_net:token_count(p2, M1),
Tokens = pqc_pattern_net:place_tokens(p2, M1),
lists:all(fun(T) -> maps:get(mi, T) =/= undefined end, Tokens).  % true
```

### Deferred Choice Example (P16)

```erlang
Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(16)),
M0 = pqc_pattern_net:initial_marking(Net),

%% Inject external event
M1 = pqc_pattern_net:inject_event(M0, transient, <<"event_a">>, true),

%% choice_a is now enabled
Enabled = pqc_pattern_net:enabled(Net, M1),  % [choice_a]

{ok, M2, _} = pqc_pattern_net:fire(Net, choice_a, M1).
```

### Loop Example (P21)

```erlang
Net = pqc_pattern_net:compile(pqc_patterns_43:pattern(21)),
M0 = pqc_pattern_net:initial_marking(Net),

%% Set max iterations
M1 = M0#{meta => #{max_iterations => 3, iteration => 0}},

%% Enter loop
{ok, M2, _} = pqc_pattern_net:fire(Net, enter_loop, M1),

%% Execute loop body (3 iterations)
{ok, M3, _} = pqc_pattern_net:fire(Net, loop_body, M2),
{ok, M4, _} = pqc_pattern_net:fire(Net, loop_body, M3),
{ok, M5, _} = pqc_pattern_net:fire(Net, loop_body, M4),

%% Exit loop
{ok, M6, _} = pqc_pattern_net:fire(Net, exit_loop, M5).
```

## Pattern Composition

Patterns can be composed to create complex workflows:

```erlang
%% Conceptual composition: Sequence -> Parallel -> Sync -> Choice
%% In practice, you would build a single net combining these patterns

%% The composed net would have:
%% - Places from all patterns (with unique names)
%% - Transitions from all patterns
%% - Connection places linking pattern outputs to inputs
%% - Unified order list

compose_workflow() ->
    #{
        places => [
            %% Sequence places
            seq_start, seq_p1, seq_p2,
            %% Parallel places
            par_input, par_b1, par_b2, par_b3,
            %% Sync places
            sync_output,
            %% Choice places
            choice_out1, choice_out2
        ],
        transitions => #{
            %% Sequence transitions
            seq_a => ...,
            seq_b => ...,
            %% Parallel split
            par_split => ...,
            %% Synchronization
            sync_join => ...,
            %% Choice transitions
            choice_a => ...,
            choice_b => ...
        },
        order => [seq_a, seq_b, par_split, sync_join, choice_a, choice_b],
        initial_marking => #{seq_start => 1}
    }.
```

## Testing

Run the comprehensive test suite:

```bash
# Via Docker (required per CLAUDE.md)
docker compose run --rm erlmcp-unit rebar3 eunit --module=pqc_patterns_43_tests

# Specific test
docker compose run --rm erlmcp-unit rebar3 eunit --module=pqc_patterns_43_tests --test=all_patterns_test_

# With coverage
docker compose run --rm erlmcp-check rebar3 cover --verbose
```

## Quality Gates

All patterns pass:

```bash
# Compile
docker compose run --rm erlmcp-build rebar3 compile

# Dialyzer (type checking)
docker compose run --rm erlmcp-build rebar3 dialyzer

# Xref (cross-reference analysis)
docker compose run --rm erlmcp-build rebar3 xref

# Format check
docker compose run --rm erlmcp-build rebar3 format --verify

# Full quality check
docker compose run --rm erlmcp-check rebar3 do compile,dialyzer,xref,format --verify
```

## Pattern Properties

### Net Structure

Each pattern returns a map with:

- `places :: [atom()]` - List of place identifiers
- `transitions :: #{tid() => transition()}` - Map of transition definitions
- `order :: [tid()]` - Total order for transition firing (Λ)
- `initial_marking :: #{place() => non_neg_integer()}` - Initial token distribution
- `metadata :: map()` - Pattern metadata (id, name, category, notes)

### Transition Structure

Each transition has:

- `inputs :: [arc()]` - Input arcs (place + weight)
- `outputs :: [arc()]` - Output arcs (place + weight)
- `join :: join_type()` - Join semantics (and, xor, or, {partial, K})
- `split :: split_type()` - Split semantics (and, xor, or, {thread, N}, {mi, Count})
- `join_policy :: join_policy()` - Optional join behavior (discriminator, blocking, canceling)
- `split_policy :: split_policy()` - Optional split behavior (choice function)
- `or_join_kind :: or_join_kind()` - OR-join variant (structured, local, general)
- `guard :: fun((marking()) -> boolean())` - Optional guard condition
- `effect :: fun((marking()) -> effect_result())` - Optional side effects
- `mutex :: binary()` - Optional mutex identifier
- `trigger :: trigger_spec()` - Optional external trigger
- `cancel :: cancel_spec()` - Optional cancellation behavior

## Advanced Features

### Reachability Caches

`pqc_pattern_net:compile/1` builds reachability caches for efficient OR-join:

```erlang
Net = pqc_patterns_43:pattern(7),  % Structured Synchronizing Merge
Compiled = pqc_pattern_net:compile(Net),

%% Compiled net has:
Reach = maps:get(reach, Compiled),      % Forward reachability
ReachRev = maps:get(reach_rev, Compiled).  % Backward reachability
```

### Token Model

Tokens are maps with:

- `id :: binary()` - Unique identifier
- `stack :: [binary()]` - Fork stack for structured OR-join
- `mi :: undefined | #{mi_id, idx, total}` - Multiple instance metadata

```erlang
Tokens = pqc_pattern_net:place_tokens(p2, Marking),
%% [#{id => <<...>>, stack => [<<...>>], mi => #{mi_id => <<...>>, idx => 1, total => 3}}]
```

### Validation

Patterns are validated at compile time:

```erlang
Net = pqc_patterns_43:pattern(1),
case pqc_pattern_net:validate(Net) of
    ok -> pqc_pattern_net:compile(Net);
    {error, Reason} -> error({invalid_net, Reason})
end.
```

## Integration with SwarmFlow PQChain

Patterns integrate with:

- **pqc_case.erl** - Case process execution
- **pqc_alpha_miner.erl** - Process discovery
- **pqc_alignment.erl** - Conformance checking
- **pqc_heuristics_miner.erl** - Heuristic mining

```erlang
%% Use pattern in case execution
CaseId = <<"order_001">>,
Pattern = pqc_patterns_43:pattern(1),
Net = pqc_pattern_net:compile(Pattern),

{ok, CasePid} = pqc_case:start(CaseId, Net),
ok = pqc_case:fire_transition(CasePid, a).
```

## Example Applications

See `/home/user/erlmcp/apps/swarmflow_pqchain/examples/pattern_usage.erl` for:

- Basic pattern execution examples
- Pattern composition examples
- Custom marking and metadata
- Category-based pattern discovery
- Integration with pqc_pattern_net

Run examples:

```erlang
%% In Erlang shell
c(pattern_usage).
pattern_usage:run_all_examples().
```

## Performance Considerations

- **Compilation**: O(P²) for P places (reachability cache)
- **Enabled check**: O(T) for T transitions
- **Fire**: O(1) for single transition
- **OR-join**: O(P) with reachability cache

Optimize by:
- Compile patterns once, reuse
- Use structured OR-join when possible (faster than general)
- Minimize place count in composed nets

## References and Further Reading

- Workflow Patterns Initiative: http://www.workflowpatterns.com/
- Van der Aalst, W.M.P. (2016): "Process Mining: Data Science in Action", Springer
- YAWL Foundation: http://www.yawlfoundation.org/
- Petri Nets: https://en.wikipedia.org/wiki/Petri_net

## License

Part of SwarmFlow PQChain - Post-quantum blockchain with workflow semantics.
See project LICENSE for details.
