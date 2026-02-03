# Adaptive Learning Architecture Design

## System Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         ERLMCP V3 ADAPTIVE LEARNING                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                        TELEMENTRY LAYER                              │   │
│  │  erlmcp_tracing | erlmcp_telemetry | erlmcp_metrics | erlmcp_otel   │   │
│  └───────────────────────────┬─────────────────────────────────────────┘   │
│                              │                                              │
│                              ▼                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                     ADAPTIVE LEARNING CORE                           │   │
│  │  erlmcp_adaptive_learning (gen_server)                              │   │
│  │  ┌────────────┐ ┌────────────┐ ┌────────────┐ ┌────────────┐       │   │
│  │  │ RETRIEVE   │ │   JUDGE    │ │  DISTILL   │ │ CONSOLIDATE│       │   │
│  │  │            │ │            │ │            │ │            │       │   │
│  │  │ HNSW Index │ │  Verdicts  │ │ Patterns   │ │   EWC++    │       │   │
│  │  └─────┬──────┘ └─────┬──────┘ └─────┬──────┘ └─────┬──────┘       │   │
│  └────────┼───────────────┼───────────────┼───────────────┼──────────────┘   │
│           │               │               │               │                  │
│  ┌────────▼────────┐ ┌───▼──────────┐ ┌──▼──────────┐ ┌──▼────────────┐   │
│  │ erlmcp_hnsw     │ │ trajectory   │ │ erlmcp_     │ │ erlmcp_      │   │
│  │ (HNSW Index)    │ │ _tracking    │ │ experience_ │ │ consolidation │   │
│  └─────────────────┘ │ (state)      │ │ replay      │ │ (EWC++)       │   │
│                      └───────────────┘ │ _buffer     │ └───────────────┘   │
│                                       └─────────────┘                      │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                        DETECTION LAYER                              │   │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐             │   │
│  │  │ erlmcp_      │  │ erlmcp_      │  │ erlmcp_      │             │   │
│  │  │ anomaly_     │  │ replay_      │  │ recommenda-  │             │   │
│  │  │ detector     │  │ buffer       │  │ tion_engine │             │   │
│  │  └──────────────┘  └──────────────┘  └──────────────┘             │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                              │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                      PERSISTENCE LAYER                               │   │
│  │  AgentDB | SQLite | File Export/Import                              │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Module Specifications

### erlmcp_adaptive_learning

Main coordinator implementing the 4-step pipeline.

**API:**
```erlang
start_link() -> {ok, pid()}
start_trajectory(SessionId, AgentType, TaskDesc) -> {ok, TrajectoryId}
record_step(SessionId, Operation, Outcome, DurationMs) -> ok
end_trajectory(SessionId, Verdict, Reward) -> ok
find_similar_patterns(TaskDesc, AgentType, K) -> {ok, [#pattern_match{}]}
get_recommendations() -> {ok, [#recommendation{}]}
```

**State:**
```erlang
#learning_state{
    trajectories        :: #{session_id() => #trajectory{}},
    patterns            :: #{pattern_id() => #pattern{}},
    hnsw_index          :: #hnsw_index{},
    replay_buffer       :: #replay_buffer{},
    anomaly_detector    :: #anomaly_detector{},
    optimization_state  :: #optimization_state{},
    consolidation_state :: #consolidation_state{},
    recommendation_engine:: #recommendation_engine{},
    metrics             :: map()
}
```

### erlmcp_hnsw

HNSW index for fast approximate nearest neighbor search.

**API:**
```erlang
new(Dim, Metric, Config) -> #hnsw_index{}
insert(Index, NodeId, Vector) -> #hnsw_index{}
search(Index, Query, K) -> {ok, [{NodeId, Distance}]}
size(Index) -> non_neg_integer()
clear(Index) -> #hnsw_index{}
```

**Algorithm:**
- Multi-layer graph structure
- O(log N) search complexity
- 150-12,500x faster than brute force

### erlmcp_anomaly_detector

Statistical anomaly detection using sigma thresholds.

**API:**
```erlang
observe(MetricName, Value) -> ok
check(Detector, Features) -> {[#anomaly{}], #anomaly_detector{}}
get_baseline(MetricName) -> {ok, #statistical_baseline{}}
set_threshold(MetricName, Sigma) -> ok
```

**Detection:**
```
anomaly if: |observed - mean| / std_dev > sigma_threshold
```

### erlmcp_replay_buffer

Prioritized experience replay buffer.

**API:**
```erlang
new(Capacity, Strategy) -> #replay_buffer{}
add(Buffer, Experience) -> #replay_buffer{}
sample(Buffer, BatchSize) -> {ok, [#experience{}], #replay_buffer{}}
update_priorities(Buffer, Updates) -> #replay_buffer{}
```

**Strategies:**
- uniform: Random sampling
- prioritized: By TD-error or reward
- diversity: Maximize coverage
- recency: Favor recent experiences

## Data Flow

### Learning Pipeline

```
┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐
│ Request │───▶│ Execute │───▶│ Track   │───▶│ Judge   │───▶│ Store   │
│         │    │         │    │ Steps   │    │ Verdict │    │ Pattern │
└─────────┘    └─────────┘    └─────────┘    └─────────┘    └─────────┘
     │                            │              │              │
     │                            │              ▼              ▼
     │                            │         ┌─────────┐  ┌─────────┐
     │                            │         │ Detect  │  │ Extract │
     │                            │         │ Anomaly │  │ Pattern │
     │                            │         └─────────┘  └─────────┘
     │                            │              │              │
     ▼                            ▼              ▼              ▼
┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐
│Similar  │───▶│Select   │───▶│Apply    │───▶│Measure │───▶│Update   │
│Patterns │    │Best     │    │Policy   │    │Result  │    │Baseline │
└─────────┘    └─────────┘    └─────────┘    └─────────┘    └─────────┘
```

### Retrieval Flow (HNSW)

```
Query Embedding
       │
       ▼
┌─────────────┐
│ Start at    │
│ Entry Point │
└──────┬──────┘
       │
       ▼
┌─────────────────────────┐
│ For each layer (top to │
│ bottom):               │
│  - Greedy search to    │
│    nearest neighbor    │
│  - Move down to next   │
│    layer               │
└──────────┬──────────────┘
           │
           ▼
┌─────────────────────────┐
│ Layer 0 (bottom):       │
│  - Beam search (ef=50)  │
│  - Find k nearest       │
└──────────┬──────────────┘
           │
           ▼
┌─────────────────────────┐
│ Return k closest        │
│ patterns with distances │
└─────────────────────────┘
```

## Pattern Schema

### Pattern Record

```erlang
-record(pattern, {
    id              :: binary(),           % Unique identifier
    task_signature  :: binary(),           % Normalized task
    agent_type      :: atom() | binary(),  % Agent type
    approach        :: binary(),           % Strategy name
    steps           :: [binary()],         % Operation sequence
    success_count   :: non_neg_integer(),  % Successes
    failure_count   :: non_neg_integer(),  % Failures
    avg_reward      :: float(),            % Mean reward
    reward_variance :: float(),            % Reward variance
    last_used       :: integer(),          % Last use timestamp
    created_at      :: integer(),          % Creation timestamp
    embedding       :: binary(),           % Vector embedding
    metadata        :: map()               % Additional data
}).
```

### Trajectory Record

```erlang
-record(trajectory, {
    id               :: binary(),
    session_id       :: binary(),
    agent_type       :: atom() | binary(),
    task_description :: binary(),
    start_time       :: integer(),
    end_time         :: integer() | undefined,
    steps            :: [#trajectory_step{}],
    verdict          :: verdict() | undefined,
    reward           :: float() | undefined,
    metadata         :: map(),
    embedding        :: binary() | undefined
}).
```

## Configuration

### sys.config

```erlang
{erlmcp_adaptive_learning, #{
    %% Embedding configuration
    embedding_dim => 256,
    distance_metric => cosine,

    %% Replay buffer
    replay_buffer_size => 10000,
    replay_strategy => prioritized,

    %% HNSW index
    hnsw_m => 16,
    hnsw_ef_construction => 200,
    hnsw_ef_search => 50,
    hnsw_max_elements => 1000000,

    %% Learning thresholds
    pattern_min_reward => 0.5,
    promotion_threshold => 0.8,
    demotion_threshold => 0.3,

    %% Consolidation
    consolidation_interval_ms => 300000,  % 5 minutes
    ewc_lambda => 1000,

    %% Optimization
    optimization_interval_ms => 60000,     % 1 minute
    learning_rate => 0.01,
    convergence_threshold => 0.001,

    %% Anomaly detection
    anomaly_window_size => 100,
    anomaly_sigma_threshold => 3.0,
    anomaly_min_observations => 30
}}.
```

## Integration with Observability

### Telemetry Events

```erlang
%% Trajectory events
telemetry:execute(
    [erlmcp, learning, trajectory_start],
    #{},
    #{session_id => SessionId, agent_type => AgentType}
).

telemetry:execute(
    [erlmcp, learning, trajectory_complete],
    #{duration_ms => Duration, reward => Reward},
    #{verdict => Verdict, agent_type => AgentType}
).

%% Pattern events
telemetry:execute(
    [erlmcp, learning, pattern_learned],
    #{},
    #{pattern_id => PatternId, reward => Reward}
).

telemetry:execute(
    [erlmcp, learning, pattern_match],
    #{similarity => Similarity},
    #{pattern_id => PatternId}
).

%% Anomaly events
telemetry:execute(
    [erlmcp, learning, anomaly_detected],
    #{sigma_score => Score},
    #{metric_name => Metric, severity => Severity}
).
```

### OTEL Attributes

```erlang
%% Add learning attributes to spans
erlmcp_tracing:set_attributes(SpanCtx, #{
    <<"learning.pattern_id">> => PatternId,
    <<"learning.similarity">> => Similarity,
    <<"learning.reward">> => Reward,
    <<"learning.recommendation">> => RecommendationId
}).
```

### Dashboard Metrics

```
# Learning System Metrics
erlmcp_learning_patterns_total
erlmcp_learning_trajectories_total
erlmcp_learning_avg_reward
erlmcp_learning_anomalies_total
erlmcp_learning_recommendations_active
erlmcp_learning_hnsw_size
erlmcp_learning_replay_buffer_size
```

## Performance Targets

| Operation | Target | Max |
|-----------|--------|-----|
| Pattern search | <5ms | 10ms |
| Pattern insert | <10ms | 50ms |
| Anomaly check | <1ms | 5ms |
| Replay sample | <50ms | 100ms |
| Consolidation | <500ms | 2000ms |

## Deployment Considerations

1. **Memory**: HNSW index requires ~4KB per pattern (256-dim float32)
2. **CPU**: HNSW insert is O(log N), search is O(log N)
3. **Storage**: Patterns persist to AgentDB for sharing
4. **Network**: Distributed pattern sync via AgentDB gossip
5. **Scaling**: Partition patterns by agent_type/hashing

## Future Enhancements

1. **Deep Learning Integration**
   - External embedding models (OpenAI, local LLM)
   - Neural pattern recognition
   - Reinforcement learning (DQN, PPO)

2. **Multi-Objective Optimization**
   - Pareto frontier for competing objectives
   - Constraint satisfaction
   - Trade-off analysis

3. **Causal Inference**
   - Root cause analysis
   - Intervention recommendations
   - Counterfactual predictions

4. **Distributed Learning**
   - Federated pattern learning
   - Cross-node pattern sync
   - Consensus on recommendations
