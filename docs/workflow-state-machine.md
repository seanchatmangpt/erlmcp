# erlmcp v3 Workflow State Machine - Complete Documentation

## Architecture Overview

The workflow state machine system provides enterprise-grade orchestration for distributed workflows in erlmcp v3.

### Component Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                    erlmcp_workflow_orchestrator             │
│              (Workflow Lifecycle Management)                 │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  • Workflow creation and execution                    │   │
│  │  • Resource allocation and scheduling                 │   │
│  │  • Priority queue management                         │   │
│  │  • Executor coordination                             │   │
│  └──────────────────────────────────────────────────────┘   │
└────────────┬─────────────────────────────────────────────────┘
             │
             ├──► erlmcp_workflow_statemachine (per workflow)
             │    ┌───────────────────────────────────────┐
             │    │ States: pending → running → paused    │
             │    │         → completed/failed/cancelled  │
             │    └───────────────────────────────────────┘
             │
             ├──► erlmcp_workflow_persistence
             │    ┌───────────────────────────────────────┐
             │    │ • ETS: Fast in-memory lookup (O(1))   │
             │    │ • DETS: Durable disk backup           │
             │    │ • WAL: Write-ahead log for recovery   │
             │    │ • Snapshots: Point-in-time saves      │
             │    └───────────────────────────────────────┘
             │
             ├──► erlmcp_workflow_monitor
             │    ┌───────────────────────────────────────┐
             │    │ • Real-time progress tracking         │
             │    │ • OTEL integration                    │
             │    │ • Metrics collection                  │
             │    │ • Alert thresholds                    │
             │    └───────────────────────────────────────┘
             │
             ├──► erlmcp_workflow_policy
             │    ┌───────────────────────────────────────┐
             │    │ • Timeout strategies                  │
             │    │ • Exponential backoff retry           │
             │    │ • Circuit breaker pattern             │
             │    │ • Dead letter queue                   │
             │    └───────────────────────────────────────┘
             │
             ├──► erlmcp_workflow_compensation
             │    ┌───────────────────────────────────────┐
             │    │ • Saga transaction coordination       │
             │    │ • Compensation actions                 │
             │    │ • Rollback orchestration              │
             │    │ • State restoration                   │
             │    └───────────────────────────────────────┘
             │
             └──► erlmcp_workflow_governance
                  ┌───────────────────────────────────────┐
                  │ • Validation rules                    │
                  │ • Resource quotas                     │
                  │ • Approval workflows                  │
                  │ • Compliance checking                 │
                  └───────────────────────────────────────┘
```

## State Machine Specification

### States

| State | Description | Transitions |
|-------|-------------|-------------|
| **pending** | Workflow created, awaiting start | → running |
| **running** | Workflow executing tasks | → paused, completed, failed, cancelled |
| **paused** | Workflow temporarily suspended | → running, cancelled |
| **completed** | Workflow finished successfully | Terminal |
| **failed** | Workflow failed with error | Terminal |
| **cancelled** | Workflow cancelled by user | Terminal |

### State Transitions

```
pending ──start_workflow──> running
                             │
                    pause_workflow
                             ↓
                           paused
                             │
                    resume_workflow
                             ↓
                           running ──all_tasks_complete──> completed
                             │
                         failure
                             ↓
                          failed
                             │
                    cancel_workflow
                             ↓
                         cancelled
```

## API Reference

### Workflow Orchestration

#### Create Workflow

```erlang
%% Simple workflow
{ok, WorkflowId} = erlmcp_workflow_orchestrator:create_workflow(
    <<"data-pipeline">>,
    #{
        <<"extract">> => #{
            name => <<"Extract Data">>,
            module => data_extractor,
            function => extract,
            args => [<<"source">>],
            dependencies => []
        },
        <<"transform">> => #{
            name => <<"Transform Data">>,
            module => data_transformer,
            function => transform,
            args => [],
            dependencies => [<<"extract">>]
        },
        <<"load">> => #{
            name => <<"Load Data">>,
            module => data_loader,
            function => load,
            args => [<<"destination">>],
            dependencies => [<<"transform">>]
        }
    },
    #{priority => normal}
).

%% Execute workflow
ok = erlmcp_workflow_orchestrator:execute_workflow(WorkflowId).
```

#### Parallel Task Execution

```erlang
%% Independent tasks execute in parallel
{ok, WorkflowId} = erlmcp_workflow_orchestrator:create_workflow(
    <<"parallel-tasks">>,
    #{
        <<"task1">> => #{
            module => worker,
            function => process,
            args => [1],
            dependencies => []
        },
        <<"task2">> => #{
            module => worker,
            function => process,
            args => [2],
            dependencies => []
        },
        <<"task3">> => #{
            module => worker,
            function => process,
            args => [3],
            dependencies => []
        },
        <<"aggregate">> => #{
            module => aggregator,
            function => aggregate,
            args => [],
            dependencies => [<<"task1">>, <<"task2">>, <<"task3">>]
        }
    }
).
```

### Workflow Control

```erlang
%% Pause running workflow
ok = erlmcp_workflow_orchestrator:pause_workflow(WorkflowId).

%% Resume paused workflow
ok = erlmcp_workflow_orchestrator:resume_workflow(WorkflowId).

%% Cancel workflow
ok = erlmcp_workflow_orchestrator:cancel_workflow(WorkflowId).

%% Get workflow status
{ok, Status} = erlmcp_workflow_orchestrator:get_workflow_status(WorkflowId).
```

### Persistence

```erlang
%% Enable persistence
ok = erlmcp_workflow_persistence:enable_persistence().

%% Save workflow state
ok = erlmcp_workflow_persistence:save_workflow(WorkflowId, WorkflowData).

%% Load workflow state
{ok, WorkflowData} = erlmcp_workflow_persistence:load_workflow(WorkflowId).

%% Create snapshot
{ok, SnapshotId} = erlmcp_workflow_persistence:create_snapshot(WorkflowId).

%% Restore from snapshot
{ok, RestoredData} = erlmcp_workflow_persistence:restore_snapshot(SnapshotId).
```

### Monitoring

```erlang
%% Track workflow for monitoring
ok = erlmcp_workflow_monitor:track_workflow(WorkflowId, #{
    tasks_total => 10
}).

%% Update progress
ok = erlmcp_workflow_monitor:update_progress(WorkflowId, 0.5, #{
    tasks_completed => 5,
    tasks_failed => 0
}).

%% Record metric
ok = erlmcp_workflow_monitor:record_metric(WorkflowId, <<"duration_ms">>, 12345).

%% Get workflow metrics
{ok, Metrics} = erlmcp_workflow_monitor:get_workflow_metrics(WorkflowId).

%% Get system metrics
{ok, SystemMetrics} = erlmcp_workflow_monitor:get_system_metrics().
```

### Policy Management

```erlang
%% Define timeout policy
{ok, PolicyId} = erlmcp_workflow_policy:define_policy(<<"timeout">>, #{
    default_timeout => 30000,
    max_timeout => 300000,
    per_task_timeouts => #{
        <<"slow_task">> => 120000
    }
}).

%% Define retry policy with exponential backoff
{ok, PolicyId} = erlmcp_workflow_policy:define_policy(<<"retry">>, #{
    max_attempts => 5,
    backoff => exponential,
    base_delay => 1000,
    max_delay => 60000,
    retryable_errors => [timeout, network_error]
}).

%% Define circuit breaker
{ok, PolicyId} = erlmcp_workflow_policy:define_policy(<<"circuit_breaker">>, #{
    failure_threshold => 5,
    success_threshold => 2,
    timeout => 60000
}).
```

### Compensation (Saga Pattern)

```erlang
%% Register compensation actions
ok = erlmcp_workflow_compensation:register_compensation(
    WorkflowId,
    <<"reserve_inventory">>,
    #{
        module => inventory_service,
        function => release_reservation,
        args => [<<"order-123">>]
    }
).

ok = erlmcp_workflow_compensation:register_compensation(
    WorkflowId,
    <<"charge_payment">>,
    #{
        module => payment_service,
        function => refund,
        args => [<<"order-123">>]
    }
).

%% Execute compensation on failure
ok = erlmcp_workflow_compensation:compensate_workflow(
    WorkflowId,
    [<<"charge_payment">>, <<"reserve_inventory">>]
).
```

### Governance

```erlang
%% Validate workflow before execution
{ok, ValidationResults} = erlmcp_workflow_governance:validate_workflow(WorkflowSpec).

%% Set resource limits
ok = erlmcp_workflow_governance:set_resource_limit(<<"concurrent_workflows">>, 100).

%% Check resource usage
{ok, Usage} = erlmcp_workflow_governance:get_resource_usage().

%% Request approval for critical workflow
{ok, pending} = erlmcp_workflow_governance:request_approval(
    WorkflowId,
    <<"Critical production deployment">>
).

%% Approve workflow
ok = erlmcp_workflow_governance:approve_workflow(
    WorkflowId,
    <<"Approved by John Doe">>
).
```

## Dependency Resolution

The workflow state machine uses topological sorting to resolve task dependencies:

```erlang
%% Workflow with complex dependencies
WorkflowTasks = #{
    <<"task_a">> => #{dependencies => []},
    <<"task_b">> => #{dependencies => [<<"task_a">>]},
    <<"task_c">> => #{dependencies => [<<"task_a">>]},
    <<"task_d">> => #{dependencies => [<<"task_b">>, <<"task_c">>]},
    <<"task_e">> => #{dependencies => [<<"task_d">>]}
}.

%% Execution order:
%% 1. task_a (no dependencies)
%% 2. task_b, task_c (parallel, both depend on task_a)
%% 3. task_d (depends on task_b and task_c)
%% 4. task_e (depends on task_d)
```

## Error Handling

### Automatic Retry

```erlang
%% Configure retry policy
RetryPolicy = #{
    max_attempts => 3,
    backoff => exponential,
    base_delay => 1000,
    max_delay => 30000
},

Task = #{
    id => <<"flaky_task">>,
    module => network_client,
    function => call_api,
    args => [],
    dependencies => [],
    retry_policy => RetryPolicy
}.
```

### Circuit Breaker

```erlang
%% Register circuit breaker for external service
{ok, CBPolicyId} = erlmcp_workflow_policy:define_policy(
    <<"circuit_breaker">>,
    #{
        failure_threshold => 5,
        success_threshold => 2,
        timeout => 60000
    }
).

%% Check circuit breaker state before execution
{ok, closed} = erlmcp_workflow_policy:check_circuit_breaker(CBPolicyId).
```

## Observability

### OTEL Integration

```erlang
%% Workflow events are automatically emitted to OTEL
%% Events include:
%% - created
%% - started
%% - completed
%% - failed
%% - paused
%% - resumed
%% - cancelled

%% Metrics include:
%% - workflow_duration_ms
%% - task_duration_ms
%% - workflow_success_rate
%% - workflow_error_count
```

### Custom Metrics

```erlang
%% Record custom metrics
ok = erlmcp_workflow_monitor:record_metric(
    WorkflowId,
    <<"custom_metric">>,
    Value
).

%% Subscribe to workflow events
ok = erlmcp_workflow_monitor:subscribe_to_updates(
    self(),
    [started, completed, failed]
).

%% Receive events in your process
receive
    {workflow_monitor_event, Event} ->
        logger:info("Workflow event: ~p", [Event])
end.
```

## Performance Optimization

### Parallel Execution

Independent tasks execute in parallel automatically:

```erlang
%% These 10 tasks will execute concurrently
Tasks = lists:map(fun(I) ->
    TaskId = list_to_binary(["task_", integer_to_list(I)]),
    {TaskId, #{
        module => worker,
        function => process,
        args => [I],
        dependencies => []
    }}
end, lists:seq(1, 10)).
```

### Resource Limits

```erlang
%% Configure executor capacity
{ok, ExecutorPid} = erlmcp_workflow_orchestrator:register_executor(
    <<"executor_1">>,
    10  % Max 10 concurrent workflows
).
```

## Best Practices

1. **Keep Tasks Idempotent**: Tasks should be safe to retry
2. **Use Compensation**: Register compensations for side effects
3. **Set Timeouts**: Always configure reasonable timeouts
4. **Monitor Progress**: Track workflow execution with monitoring
5. **Validate First**: Use governance to validate before execution
6. **Handle Failures**: Implement proper error handling and retries
7. **Use Dependencies**: Model task dependencies explicitly
8. **Enable Persistence**: Use persistence for long-running workflows
9. **Set Alerts**: Configure alert thresholds for monitoring
10. **Audit Everything**: Use audit logs for compliance

## Supervision Tree

```
erlmcp_sup
└── erlmcp_workflow_orchestrator_sup
    ├── erlmcp_workflow_orchestrator
    ├── erlmcp_workflow_persistence
    ├── erlmcp_workflow_monitor
    ├── erlmcp_workflow_policy
    ├── erlmcp_workflow_compensation
    ├── erlmcp_workflow_governance
    └── erlmcp_workflow_sup (dynamic)
        └── erlmcp_workflow_statemachine (per workflow)
```

## Testing

### EUnit Tests

```bash
# Run workflow state machine tests
rebar3 eunit --module=erlmcp_workflow_statemachine_tests

# Run all workflow tests
rebar3 eunit --module='*_workflow_*_tests'
```

### Property-Based Tests

```bash
# Run Proper tests
rebar3 proper --module=erlmcp_workflow_statemachine_tests
```

## Examples

See `examples/workflow/` for complete examples:

- `simple_workflow.erl` - Basic workflow with 3 tasks
- `parallel_workflow.erl` - Parallel task execution
- `data_pipeline.erl` - ETL pipeline with dependencies
- `saga_transaction.erl` - Distributed transaction with compensation
- `error_handling.erl` - Retry and circuit breaker patterns

## Performance Characteristics

| Metric | Value |
|--------|-------|
| State transition latency | < 1ms |
| Task spawn overhead | < 100μs |
| Dependency resolution | O(V + E) |
| Parallel execution | Unlimited (by resources) |
| Memory per workflow | ~10KB base + tasks |
| Max concurrent workflows | Configurable (default: 10) |

## License

Apache 2.0 - See LICENSE file for details.
