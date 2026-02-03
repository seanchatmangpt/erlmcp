# Heuristics Miner - Noise-Tolerant Process Discovery

## Overview

The Heuristics Miner is an advanced process discovery algorithm developed by Weijters, van der Aalst, and others (2006). It discovers process models from event logs while handling noise, incomplete data, and infrequent behavior - making it significantly more robust than the classic Alpha Miner algorithm.

## Algorithm Description

### Core Principle

The Heuristics Miner builds a **dependency graph** by calculating dependency measures between activities, then converts this graph into a **causal net** (C-net), and finally generates a **Petri net** (workflow net).

### Dependency Measure

The key innovation is the dependency measure formula:

```
dependency(a,b) = (|a >_L b| - |b >_L a|) / (|a >_L b| + |b >_L a| + 1)
```

Where:
- `|a >_L b|` = frequency of activity `a` directly followed by activity `b` in the event log
- Returns a value in `[-1, 1]`
- **Positive values**: `a` likely precedes `b`
- **Negative values**: `b` likely precedes `a`
- **Near zero**: No clear dependency or parallel activities

### Algorithm Steps

1. **Extract Traces**: Convert event log into activity sequences
2. **Build Frequency Counts**: Count direct successions, starts, and ends
3. **Calculate Dependencies**: Compute dependency measures for all activity pairs
4. **Filter Dependencies**: Apply thresholds to remove noise
5. **Detect Split/Join Types**: Determine AND vs XOR semantics
6. **Detect Loops**: Find length-1 and length-2 loops
7. **Construct Causal Net**: Build C-net representation
8. **Convert to Petri Net**: Generate executable workflow net

## Configuration Parameters

The `heuristics_config` record controls the discovery behavior:

```erlang
-record(heuristics_config, {
    dependency_threshold = 0.9 :: float(),           % Minimum dependency strength
    positive_observations = 1 :: pos_integer(),       % Minimum occurrences
    relative_to_best = 0.05 :: float(),              % Relative strength to best
    and_threshold = 0.1 :: float(),                  % Co-occurrence threshold for AND
    loop_length_one = 0.9 :: float(),                % L1 loop threshold
    loop_length_two = 0.9 :: float()                 % L2 loop threshold
}).
```

### Parameter Meanings

- **dependency_threshold**: Minimum dependency measure value to include an edge (0.0 to 1.0)
  - Higher = stricter filtering (more noise removal)
  - Lower = more permissive (captures more behavior)
  - Default: 0.9 (very strict)

- **positive_observations**: Minimum number of times a dependency must occur
  - Filters out very infrequent behavior
  - Default: 1 (include all observed dependencies)

- **relative_to_best**: Maximum difference from the strongest dependency
  - For activity `a`, if best outgoing dependency = 0.95, minimum = 0.95 - 0.05 = 0.90
  - Ensures only strong relationships are kept
  - Default: 0.05

- **and_threshold**: Threshold for detecting AND splits/joins
  - Lower = more likely to detect AND (parallel) behavior
  - Higher = more likely to detect XOR (choice) behavior
  - Default: 0.1

- **loop_length_one**: Minimum measure for detecting self-loops (a → a)
  - Default: 0.9

- **loop_length_two**: Minimum measure for detecting two-step loops (a → b → a)
  - Default: 0.9

## Usage Examples

### Example 1: Simple Linear Process

```erlang
%% Event log with linear process: a -> b -> c
Traces = [
    [a, b, c],
    [a, b, c],
    [a, b, c]
],

%% Discover with default configuration
{ok, Net} = pqc_heuristics_miner:discover(Traces).

%% Result: Petri net with transitions a, b, c in sequence
```

### Example 2: Process with Choice (XOR)

```erlang
%% Process: a -> (b | c) -> d
Traces = [
    [a, b, d],
    [a, c, d],
    [a, b, d],
    [a, c, d]
],

{ok, Net} = pqc_heuristics_miner:discover(Traces).

%% Result: Petri net with XOR split after a and XOR join before d
```

### Example 3: Process with Parallelism (AND)

```erlang
%% Process: a -> (b || c) -> d
Traces = [
    [a, b, c, d],
    [a, c, b, d],
    [a, b, c, d],
    [a, c, b, d]
],

{ok, Net} = pqc_heuristics_miner:discover(Traces).

%% Result: Petri net with AND split after a and AND join before d
```

### Example 4: Handling Noise

```erlang
%% Event log with noise (infrequent behavior)
Traces = [
    [a, b, c],
    [a, b, c],
    [a, b, c],
    [a, b, c],
    [a, x, c],  % Noise: x occurs only once
    [a, b, c],
    [a, b, c]
],

%% Use strict configuration to filter noise
Config = #heuristics_config{
    dependency_threshold = 0.7,
    positive_observations = 2  % Require at least 2 occurrences
},

{ok, Net} = pqc_heuristics_miner:discover(Traces, Config).

%% Result: Petri net filtering out the infrequent 'x' activity
```

### Example 5: Loop Detection

```erlang
%% Process with self-loop
Traces = [
    [a, b, b, b, c],
    [a, b, c],
    [a, b, b, c]
],

Config = #heuristics_config{
    loop_length_one = 0.5
},

Loops = pqc_heuristics_miner:detect_loops(Traces, Config).

%% Result: [{b, length_one}]
```

### Example 6: Working with swf_event Records

```erlang
%% Event log as swf_event records
Events = [
    #swf_event{
        id = <<"e1">>,
        case_id = <<"case1">>,
        event_type = transition_fired,
        transition_id = <<"register">>,
        sequence = 1,
        timestamp = 1000,
        data = #{}
    },
    #swf_event{
        id = <<"e2">>,
        case_id = <<"case1">>,
        event_type = transition_fired,
        transition_id = <<"approve">>,
        sequence = 2,
        timestamp = 2000,
        data = #{}
    },
    #swf_event{
        id = <<"e3">>,
        case_id = <<"case1">>,
        event_type = transition_fired,
        transition_id = <<"complete">>,
        sequence = 3,
        timestamp = 3000,
        data = #{}
    }
],

{ok, Net} = pqc_heuristics_miner:discover(Events).

%% Events are automatically grouped by case_id and sorted by sequence
```

## API Reference

### Main Functions

#### `discover/1`

```erlang
-spec discover(EventLog :: event_log()) -> {ok, #swf_net{}} | {error, term()}.
```

Discover process model from event log with default configuration.

- **Input**: Event log (list of traces or list of swf_event records)
- **Output**: `{ok, SwarmFlow Petri net}` or `{error, Reason}`

#### `discover/2`

```erlang
-spec discover(EventLog :: event_log(), Config :: #heuristics_config{}) ->
    {ok, #swf_net{}} | {error, term()}.
```

Discover process model with custom configuration.

#### `dependency_graph/1,2`

```erlang
-spec dependency_graph(EventLog :: event_log()) -> #dependency_graph{}.
-spec dependency_graph(EventLog :: event_log(), Config :: #heuristics_config{}) ->
    #dependency_graph{}.
```

Build dependency graph from event log.

Returns a `#dependency_graph{}` record containing:
- `activities`: Set of all discovered activities
- `dependencies`: Map of activity pairs to dependency strengths
- `start_activities`: Frequencies of starting activities
- `end_activities`: Frequencies of ending activities

#### `dependency_measure/3`

```erlang
-spec dependency_measure(A :: atom(), B :: atom(), Counts :: frequency_counts()) -> float().
```

Calculate dependency measure between two activities.

- **Returns**: Value in `[-1, 1]`
- **Positive**: `a` likely precedes `b`
- **Negative**: `b` likely precedes `a`

#### `split_mining/2`

```erlang
-spec split_mining(DepGraph :: #dependency_graph{}, Activity :: atom()) ->
    {split_type(), [atom()]}.
```

Determine split type (AND/XOR) for outgoing edges of an activity.

- **Returns**: `{and, Successors}` or `{xor, Successors}`

#### `join_mining/2`

```erlang
-spec join_mining(DepGraph :: #dependency_graph{}, Activity :: atom()) ->
    {join_type(), [atom()]}.
```

Determine join type (AND/XOR) for incoming edges of an activity.

#### `detect_loops/2`

```erlang
-spec detect_loops(EventLog :: event_log(), Config :: #heuristics_config{}) ->
    [{atom(), loop_type()}].
```

Detect length-1 and length-2 loops in the event log.

- **Returns**: List of `{Activity, LoopType}` tuples
- **LoopType**: `length_one` or `{length_two, OtherActivity}`

#### `to_causal_net/1`

```erlang
-spec to_causal_net(DepGraph :: #dependency_graph{}) -> #causal_net{}.
```

Convert dependency graph to causal net (C-net) representation.

#### `to_petri_net/1`

```erlang
-spec to_petri_net(CNet :: #causal_net{}) -> #swf_net{}.
```

Convert causal net to SwarmFlow Petri net.

## Data Structures

### dependency_graph

```erlang
-record(dependency_graph, {
    activities :: ordsets:ordset(atom()),
    dependencies :: #{atom() => #{atom() => float()}},
    start_activities :: #{atom() => non_neg_integer()},
    end_activities :: #{atom() => non_neg_integer()}
}).
```

### causal_net

```erlang
-record(causal_net, {
    activities :: ordsets:ordset(atom()),
    input_bindings :: #{atom() => [{join_type(), [atom()]}]},
    output_bindings :: #{atom() => [{split_type(), [atom()]}]},
    start_activities :: ordsets:ordset(atom()),
    end_activities :: ordsets:ordset(atom()),
    loops :: [{atom(), loop_type()}]
}).
```

## Comparison with Alpha Miner

| Feature | Alpha Miner | Heuristics Miner |
|---------|------------|------------------|
| **Noise Handling** | ❌ Poor | ✅ Excellent |
| **Incomplete Logs** | ❌ Fails | ✅ Handles gracefully |
| **Loop Detection** | ❌ No | ✅ Length-1 and length-2 |
| **Frequency Info** | ❌ Ignored | ✅ Used for filtering |
| **Configurability** | ❌ Limited | ✅ Highly configurable |
| **Split/Join Detection** | ⚠️ Basic | ✅ Advanced (AND/XOR) |
| **Performance** | ✅ Fast | ⚠️ Moderate |
| **Use Case** | Perfect logs | Real-world logs |

## Best Practices

### 1. Start with Default Configuration

```erlang
{ok, Net} = pqc_heuristics_miner:discover(Traces).
```

The defaults are conservative and work well for most cases.

### 2. Adjust for Noise

If your log has noise:

```erlang
Config = #heuristics_config{
    dependency_threshold = 0.8,      % Lower threshold
    positive_observations = 3        % Require more occurrences
},
{ok, Net} = pqc_heuristics_miner:discover(Traces, Config).
```

### 3. Inspect Dependency Graph

```erlang
DepGraph = pqc_heuristics_miner:dependency_graph(Traces),
Dependencies = DepGraph#dependency_graph.dependencies.

%% Inspect dependency strengths
maps:map(
    fun(Activity, Deps) ->
        io:format("~p -> ~p~n", [Activity, Deps])
    end,
    Dependencies
).
```

### 4. Handle Large Event Logs

For large logs, filter events before discovery:

```erlang
%% Filter only transition_fired events
RelevantEvents = [E || E <- Events,
                       E#swf_event.event_type =:= transition_fired],

{ok, Net} = pqc_heuristics_miner:discover(RelevantEvents).
```

### 5. Validate Results

```erlang
%% Use conformance checking to validate discovered model
{ok, Net} = pqc_heuristics_miner:discover(Traces),

%% Check fitness, precision, etc. using process mining tools
Fitness = calculate_fitness(Net, Traces).
```

## Algorithm Complexity

- **Time Complexity**: O(n × m²)
  - n = number of traces
  - m = number of unique activities

- **Space Complexity**: O(m²)
  - For storing dependency matrix

## References

1. Weijters, A.J.M.M., van der Aalst, W.M.P., and De Medeiros, A.K.A. (2006).
   "Process Mining with the Heuristics Miner Algorithm".
   BETA Working Paper Series, WP 166.

2. van der Aalst, W.M.P. (2016).
   "Process Mining: Data Science in Action" (2nd ed.).
   Springer.

3. De Medeiros, A.K.A., Weijters, A.J.M.M., and van der Aalst, W.M.P. (2007).
   "Genetic process mining: An experimental evaluation".
   Data Mining and Knowledge Discovery, 14(2), 245-304.

## Integration with SwarmFlow PQChain

The Heuristics Miner is integrated into the SwarmFlow PQChain system:

- **Event Logs**: Stored as `#swf_event{}` records in the blockchain
- **Process Discovery**: Can run as a workflow transition or swarm worker
- **Model Evolution**: Discovered models can be proposed as patches
- **Consensus**: Discovery results are deterministic and verifiable
- **Post-Quantum**: All cryptographic operations use NIST PQC standards

## Future Enhancements

- [ ] Support for longer loops (length > 2)
- [ ] Incremental discovery (streaming event logs)
- [ ] Multi-perspective mining (incorporating resources, data)
- [ ] Fuzzy mining integration
- [ ] Genetic algorithm optimization
- [ ] Distributed discovery across cluster
- [ ] Machine learning-enhanced thresholds
- [ ] Interactive threshold tuning UI

## License

Part of erlmcp v3 - Erlang/OTP MCP SDK.
