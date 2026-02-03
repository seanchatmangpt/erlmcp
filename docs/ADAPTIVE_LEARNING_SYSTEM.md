# Adaptive Learning System for erlmcp v3

## Overview

The Adaptive Learning System implements a 4-step intelligence pipeline for continuous operational improvement:

```
┌─────────────────────────────────────────────────────────────────────┐
│                  ADAPTIVE LEARNING PIPELINE                         │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│   ┌──────────┐    ┌──────────┐    ┌──────────┐    ┌──────────┐    │
│   │ RETRIEVE │───▶│  JUDGE   │───▶│ DISTILL  │───▶│CONSOLIDATE│   │
│   │          │    │          │    │          │    │          │    │
│   │ HNSW     │    │ Verdicts │    │ LoRA     │    │ EWC++    │    │
│   │ 150x     │    │ Success/ │    │ Extract  │    │ Prevent  │    │
│   │ faster   │    │ Failure  │    │ Learnings│    │ Forget   │    │
│   └──────────┘    └──────────┘    └──────────┘    └──────────┘    │
│        │               │               │               │           │
│        ▼               ▼               ▼               ▼           │
│   ┌─────────────────────────────────────────────────────────────┐ │
│   │                    PATTERN MEMORY                           │ │
│   │  AgentDB + HNSW Index + SQLite Persistence                  │ │
│   └─────────────────────────────────────────────────────────────┘ │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Components

### 1. Trajectory Tracking

Tracks complete operational sessions from start to finish:

```erlang
%% Start tracking
{ok, TrajectoryId} = erlmcp_adaptive_learning:start_trajectory(
    SessionId,
    AgentType,
    TaskDescription
).

%% Record steps
erlmcp_adaptive_learning:record_step(
    SessionId,
    Operation,
    Outcome,
    DurationMs,
    Metrics
).

%% End with verdict
erlmcp_adaptive_learning:end_trajectory(
    SessionId,
    Verdict,     % success | failure | partial
    Reward       % 0.0 - 1.0
).
```

### 2. Verdict Judgment

Assigns success/failure verdicts based on:

- **Outcome**: Operation success/failure
- **Duration**: Within acceptable bounds
- **Resource Usage**: Memory, CPU efficiency
- **Error Rate**: Number and severity of errors

```erlang
-record(verdict, {
    outcome       :: success | failure | partial,
    confidence    :: float(),              % 0.0 - 1.0
    reason        :: binary() | undefined,
    reward        :: float(),              % 0.0 - 1.0
    metadata      :: map()
}).
```

### 3. Pattern Distillation

Extracts reusable patterns from successful trajectories:

```erlang
-record(pattern, {
    id              :: pattern_id(),
    task_signature  :: binary(),
    agent_type      :: agent_type(),
    approach        :: binary(),
    steps           :: [binary()],
    success_count   :: non_neg_integer(),
    failure_count   :: non_neg_integer(),
    avg_reward      :: float(),
    reward_variance :: float(),
    last_used       :: integer(),
    created_at      :: integer(),
    embedding       :: embedding_vector(),
    metadata        :: map()
}).
```

### 4. HNSW Indexing

Fast similarity search using Hierarchical Navigable Small World graphs:

| N Elements | Brute Force | HNSW (ef=50) | Speedup |
|------------|-------------|--------------|---------|
| 1K         | 2ms         | 0.1ms        | 20x     |
| 10K        | 20ms        | 0.3ms        | 67x     |
| 100K       | 200ms       | 0.8ms        | 250x    |
| 1M         | 2000ms      | 3ms          | 667x    |

```erlang
%% Find similar patterns
{ok, Matches} = erlmcp_adaptive_learning:find_similar_patterns(
    TaskDescription,
    AgentType,
    K  % Number of results
).

%% Match result
-record(pattern_match, {
    pattern_id  :: pattern_id(),
    similarity  :: float(),    % 0.0 - 1.0
    reward      :: float(),
    confidence  :: float(),
    pattern     :: #pattern{} | undefined
}).
```

### 5. Experience Replay

Prioritized experience replay for training:

```erlang
-record(replay_buffer, {
    capacity               :: pos_integer(),
    experiences            :: [#experience{}],
    priorities             :: #{binary() => float()},
    sampling_distribution  :: uniform | prioritized | diversity | recency,
    total_samples          :: non_neg_integer()
}).
```

Sampling strategies:
- **uniform**: Random sampling (baseline)
- **prioritized**: Sample by priority (TD-error or reward magnitude)
- **diversity**: Maximize coverage of experience space
- **recency**: Weight recent experiences higher

### 6. Adaptive Optimization

Continuously optimizes operational parameters:

```erlang
-record(optimization_policy, {
    name             :: binary(),
    parameters       :: map(),
    performance_score:: float(),
    last_updated     :: integer(),
    update_count     :: non_neg_integer()
}).

-record(optimization_recommendation, {
    id                 :: binary(),
    type               :: parameter_tuning | resource_allocation | strategy_change,
    target             :: binary(),
    current_value      :: term(),
    recommended_value  :: term(),
    expected_improvement:: float(),
    confidence         :: float(),
    reason             :: binary(),
    estimated_impact   :: map()
}).
```

### 7. Anomaly Detection

Statistical anomaly detection using sigma thresholds:

```erlang
-record(anomaly, {
    id              :: binary(),
    severity        :: low | medium | high | critical,
    metric_name     :: binary(),
    observed_value  :: float(),
    expected_value  :: float(),
    deviation       :: float(),
    sigma_score     :: float(),
    timestamp       :: integer(),
    context         :: map()
}).
```

Detection algorithm:
```
Anomaly if: |observed - mean| / std_dev > sigma_threshold

Default: 3 sigma (99.7% confidence interval)
```

### 8. Recommendation Engine

Generates actionable recommendations:

```erlang
-record(recommendation, {
    id              :: binary(),
    category        :: pattern_selection | parameter_optimization | error_prevention,
    priority        :: low | medium | high | critical,
    title           :: binary(),
    description     :: binary(),
    action          :: binary() | map(),
    expected_outcome:: binary(),
    confidence      :: float(),
    evidence        :: [binary()],  % Supporting pattern IDs
    expires_at      :: integer() | undefined
}).
```

## Integration Points

### Telemetry Integration

```erlang
%% In erlmcp_telemetry, emit learning events
-telemetry_event(
    [erlmcp, learning, trajectory_complete],
    #{duration_ms => integer(), reward => float()},
    #{verdict => atom(), agent_type => atom()}
).
```

### OTEL Span Integration

```erlang
%% Attach learning attributes to spans
erlmcp_tracing:set_attributes(SpanCtx, #{
    <<"learning.pattern_id">> => PatternId,
    <<"learning.similarity">> => Similarity,
    <<"learning.recommendation">> => RecommendationId
}).
```

### Dashboard Integration

```erlang
%% Query learning metrics for dashboard
erlmcp_adaptive_learning:get_metrics().
%% Returns: #{
%%   trajectories_completed => 1234,
%%   patterns_learned => 567,
%%   avg_reward => 0.78,
%%   anomalies_detected => 12,
%%   ...
%% }
```

## Configuration

```erlang
%% sys.config
{erlmcp_adaptive_learning, #{
    embedding_dim => 256,
    distance_metric => cosine,
    replay_buffer_size => 10000,
    consolidation_interval_ms => 300000,  % 5 minutes
    optimization_interval_ms => 60000,     % 1 minute
    anomaly_sigma_threshold => 3.0
}}.
```

## Performance Characteristics

| Operation | Complexity | Performance |
|-----------|------------|-------------|
| Pattern search (HNSW) | O(log N) | <5ms for 1M patterns |
| Pattern insertion | O(log N) | <10ms |
| Anomaly check | O(1) | <1ms |
| Experience replay sample | O(B log N) | <50ms for batch=32 |

## ML Integration Points

### External Embedding Model

Replace the built-in embedding generation with a real model:

```erlang
%% Hook for external embedding service
callback(generate_embedding, Task, AgentType) ->
    %% Call external service (e.g., OpenAI, local LLM)
    llm_client:embed(Task#{agent_type => AgentType}).
```

### External Training

Export experiences for external training:

```erlang
%% Export replay buffer
erlmcp_replay_buffer:serialize(Buffer).

%% Import trained model
erlmcp_adaptive_learning:import_patterns("trained_patterns.json").
```

### A/B Testing

Compare pattern recommendations against baselines:

```erlang
%% Split traffic between learned patterns and baseline
case rand:uniform() of
    N when N < 0.5 -> use_learned_pattern();
    _ -> use_baseline_approach()
end.
```

## Persistence

Patterns are persisted via:

1. **AgentDB**: For distributed pattern sharing
2. **SQLite**: For local pattern cache
3. **File Export/Import**: For backup and transfer

```erlang
%% Export to file
erlmcp_adaptive_learning:export_patterns("patterns.json").

%% Import from file
erlmcp_adaptive_learning:import_patterns("patterns.json").
```

## Monitoring

Key metrics to monitor:

```erlang
%% Learning system health
#{
    patterns_learned => 567,
    patterns_promoted => 123,
    avg_reward => 0.78,
    anomaly_rate => 0.02,
    recommendation_accuracy => 0.85
}
```

Alert on:
- Pattern learning rate < threshold
- Average reward degradation
- Anomaly rate spike
- Recommendation accuracy drop

## Best Practices

1. **Start with High Rewards**: Only store patterns with reward >= 0.5
2. **Regular Consolidation**: Run consolidation every 5-10 minutes
3. **Monitor Anomalies**: Set up alerts for high-severity anomalies
4. **Validate Recommendations**: A/B test before fully adopting
5. **Periodic Review**: Clean up low-performing patterns weekly

## Future Enhancements

- [ ] Reinforcement learning integration (DQN, PPO)
- [ ] Multi-objective optimization
- [ ] Distributed pattern learning across nodes
- [ ] Automatic hyperparameter tuning
- [ ] Causal inference for root cause analysis
- [ ] Transfer learning between agent types
