# Workflow Examples from erlmcp Project

This document provides concrete examples of task orchestration patterns implemented in the erlmcp project, demonstrating how to use Claude Code CLI for complex workflow management.

## 1. Full SPARC Development Workflow

### Example 1: Implementing a New Cache Server

```erlang
% Complete SPARC workflow for cache server implementation
% Execute in single message with proper coordination

% Phase 1: Research (Context Preservation)
Task("Research OTP Patterns",
  "Analyze existing erlmcp gen_server patterns in src/, understand #state{} records and supervision",
  "erlang-researcher")

% Phase 2: Planning (Architecture Design)
Task("Plan Cache Server Implementation",
  "Design gen_server with ETS backend, supervised by erlmcp_sup, API functions and state management",
  "plan-designer")

% Phase 3: Architecture Review
Task("Review Cache Architecture",
  "Validate OTP behavior choices, error handling, supervision strategy, and integration points",
  "erlang-architect")

% Phase 4: Implementation (Parallel)
Task("Implement Cache Server",
  "Create cache_server.erl following erlmcp patterns with proper gen_server callbacks",
  "erlang-otp-developer")

Task("Write Cache Tests",
  "Comprehensive EUnit + CT tests covering normal operation, error cases, and edge conditions",
  "erlang-test-engineer")

% Phase 5: Quality Validation
Task("Review Cache Implementation",
  "Validate code quality, OTP patterns, and performance characteristics",
  "code-reviewer")
```

### Example 2: Adding New Transport Layer

```erlang
% Multi-agent workflow for transport development
% Demonstrates subagent pipeline pattern

% 1. Research existing patterns
Task("Research Transport Architecture",
  "Analyze erlmcp_transport.erl behavior and existing stdio/tcp/http implementations",
  "erlang-researcher")

% 2. Design new transport
Task("Design WebSocket Transport",
  "Plan WebSocket transport with ranch integration, message framing, and connection management",
  "plan-designer")

% 3. Architecture review (gatekeeper)
Task("Review WebSocket Design",
  "Validate behavior callbacks, supervision strategy, error handling, and integration points",
  "erlang-architect")

% 4. Parallel implementation
Task("Implement WebSocket Transport",
  "Create erlmcp_transport_websocket.erl with proper callbacks and ranch integration",
  "erlang-transport-builder")

Task("Write Transport Tests",
  "EUnit + CT tests for WebSocket transport including connection, messaging, and error scenarios",
  "erlang-test-engineer")

Task("Benchmark Transport Performance",
  "Measure throughput, latency, and memory usage for WebSocket vs existing transports",
  "erlang-performance")

% 5. Quality gates
Task("Validate Transport Implementation",
  "Ensure all quality gates pass (tests, dialyzer, xref, benchmarks)",
  "code-reviewer")
```

## 2. TCPS Manufacturing Workflow Examples

### Example 1: Security Patch Deployment

```erlang
% Complete TCPS workflow for security vulnerability patching

% Stage 1: Pull (Security Advisory)
/tcps-pull security-advisory CVE-2026-1234

% Stage 2: Level (Critical Priority)
/tcps-heijunka critical  % Auto-levels security work to front of queue

% Stage 3: WIP Management
/tcps-kanban check        % Verify WIP limits allow critical work
/tcps-kanban move WO-SEC-123 to-code  % Move to development bucket

% Stage 4-5: Build & Implementation
/tcps-build WO-SEC-123   % Security patch implementation

% Stage 6: Quality Checks (Jidoka)
/tcps-jidoka WO-SEC-123  % Comprehensive security testing
/tcps-jidoka WO-SEC-123 --full  % Include penetration testing

% Stage 7: Stop-the-Line (if needed)
/tcps-andon trigger WO-SEC-123 "security-test-failure"  % Auto-quarantine

% Stage 8: Release
/tcps-receipt WO-SEC-123  % Generate security receipt with audit trail
```

### Example 2: Feature Development with Dependencies

```erlang
% Feature development with dependency management

% Step 1: Create foundation work order
{ok, AuthLibWO} = tcps_work_order:create_work_order(#{
    type => github_issue,
    source => "https://github.com/erlmcp/erlmcp/issues/200",
    description => "Build authentication library",
    labels => ["enhancement"],
    metadata => #{}
}),
tcps_work_order:start_work_order(AuthLibWO).

% Step 2: Create dependent feature
{ok, OAuthWO} = tcps_work_order:create_work_order(#{
    type => github_issue,
    source => "https://github.com/erlmcp/erlmcp/issues/201",
    description => "Add OAuth support",
    labels => ["enhancement"],
    metadata => #{}
}),
tcps_work_order:start_work_order(OAuthWO).

% Step 3: Establish dependency
tcps_work_order:add_dependency(OAuthWO, AuthLibWO).

% Step 4: Work automatically unblocks when foundation completes
% OAuthWO will automatically progress to "ready" when AuthLibWO completes

% Step 5: Execute parallel development (WIP permitting)
/tcps-kanban move WO-200 to-code   % Authentication library
/tcps-kanban move WO-201 to-wait   % OAuth waits for dependency

% Step 6: Complete both work orders
/tcps-build WO-200                 % Build auth library
/tcps-build WO-201                 % OAuth implementation (now unblocked)
/tcps-jidoka WO-200 WO-201        % Run quality checks
/tcps-receipt WO-200 WO-201       % Generate release receipts
```

## 3. Swarm Coordination Examples

### Example 1: Full-Stack Development Swarm

```bash
% Complex multi-agent swarm for full-stack development
./claude-flow swarm "Build e-commerce REST API with React frontend" \
  --strategy development \
  --mode mesh \
  --max-agents 8 \
  --parallel \
  --monitor \
  --review \
  --testing \
  --background \
  --distributed

% Monitor swarm progress
./claude-flow status --verbose

% Access real-time dashboard
open http://localhost:8080/dashboard

% Check agent activity
./claude-flow agent list
```

### Example 2: Performance Optimization Swarm

```bash
% Background swarm for continuous performance optimization
./claude-flow swarm "Optimize database queries and API performance" \
  --strategy optimization \
  --mode hierarchical \
  --max-agents 6 \
  --background \
  --monitor \
  --ui \
  --testing \
  --verbose

% Schedule periodic optimization runs
*/30 * * * * /path/to/claude-flow swarm "System health check" --strategy optimization --background

% View optimization results
./claude-flow memory query "optimization_results" --namespace swarm
```

## 4. Real-Time Monitoring Examples

### Example 1: Dashboard-Driven Development

```erlang
% Simulate work flow through dashboard (for development/testing)
-module(dashboard_simulation).

-export([run/0]).

run() ->
    % Start dashboard
    {ok, _} = tcps_dashboard:start_dashboard(8080),

    % Simulate continuous work flow
    spawn_link(fun() -> simulate_work_flow() end),
    spawn_link(fun() -> simulate_quality_alerts() end),
    spawn_link(fun() -> simulate_improvements() end),

    ok.

simulate_work_flow() ->
    WorkOrders = [wo1, wo2, wo3, wo4, wo5],
    lists:foreach(fun(WO) ->
        % Create work order
        tcps_dashboard:notify_event(work_order_created, #{
            id => WO,
            sku_id => generate_sku(),
            bucket => backlog,
            priority => random_priority()
        }),

        % Progress through buckets
        Buckets = [backlog, ready, in_progress, review, done],
        lists:foreach(fun(Bucket) ->
            timer:sleep(2000),
            tcps_dashboard:notify_event(work_order_completed, #{
                id => WO,
                sku_id => generate_sku(),
                bucket => Bucket,
                priority => random_priority()
            })
        end, Buckets)
    end, WorkOrders).

simulate_quality_alerts() ->
    timer:sleep(15000), % Wait for some work to start
    Alerts = [
        {critical, "Test failure in auth module"},
        {warning, "Code coverage below 80%"},
        {info, "New dependency available"}
    ],
    lists:foreach(fun({Severity, Message}) ->
        tcps_dashboard:notify_event(andon_triggered, #{
            id => generate_alert_id(),
            severity => Severity,
            title => list_to_binary(Message),
            affected_skus => [generate_sku()],
            triggered_at => erlang:timestamp()
        }),
        timer:sleep(30000)
    end, Alerts).

simulate_improvements() ->
    timer:sleep(30000),
    Improvements = [
        {"Automated code review", 0.25},
        {"Parallel test execution", 0.30},
        {"Build pipeline optimization", 0.20}
    ],
    lists:foreach(fun({Title, ROI}) ->
        tcps_dashboard:notify_event(kaizen_improvement, #{
            id => generate_improvement_id(),
            title => list_to_binary(Title),
            estimated_roi => ROI,
            status => in_progress,
            category => reduce_waste
        }),
        timer:sleep(45000)
    end, Improvements).
```

### Example 2: Agent Health Monitoring

```bash
% Monitor agent health and performance
./claude-flow monitor --focus agents

% Check swarm resource utilization
./claude-flow memory query "resource_usage" --namespace swarm

% View agent-specific metrics
./claude-flow agent info erlang-otp-developer

% Detect and handle agent failures
./claude-flow swarm "Failed agent recovery" --strategy maintenance --distributed
```

## 5. Error Recovery Examples

### Example 1: 5 Whys Root Cause Analysis

```erlang
% Example: Test failure in TCP transport
/5-whys-analyze "test failure in tcp transport"

% Analysis flow:
% 1. Problem: TCP transport tests failing
% 2. Why: Connection timeout during stress test
% 3. Why: Too many concurrent connections overwhelming the process
% 4. Why: No connection pool management implemented
% 5. Why: Assumed single-threaded operation was sufficient
% 6. Root cause: Missing connection pooling infrastructure

% Create fix work order
{ok, FixWO} = tcps_work_order:create_work_order(#{
    type => defect,
    source => "test-failure-analysis",
    description => "Implement connection pooling for TCP transport",
    labels => ["bug", "performance"],
    metadata => #{root_cause => "missing_connection_pooling"}
}).
```

### Example 2: Automatic Retry Circuit Breaker

```erlang
% Circuit breaker implementation for external dependencies
-module(erlmcp_circuit_breaker).

-export([call_service/2, call_service/3]).

call_service(Service, Request) ->
    call_service(Service, Request, 5000).

call_service(Service, Request, Timeout) ->
    case circuit_breaker_state(Service) of
        closed ->
            try_call_service(Service, Request, Timeout);
        open ->
            {error, service_unavailable};
        half_open ->
            maybe_try_service(Service, Request, Timeout)
    end.

try_call_service(Service, Request, Timeout) ->
    try
        Result = erlmcp_http_client:call(Service, Request, Timeout),
        circuit_breaker_success(Service),
        Result
    catch
        _:Reason ->
            circuit_breaker_failure(Service),
            {error, Reason}
    end.
```

## 6. Performance Optimization Examples

### Example 1: Benchmark-Driven Development

```erlang
% Performance benchmark suite
benchmark_all() ->
    % Core operations benchmark
    erlmcp_bench_core_ops:run(<<"core_ops_100k">>),        % Target: >2M ops/sec
    erlmcp_bench_core_ops:run(<<"registry_throughput">>), % Target: >500K msg/sec
    erlmcp_bench_core_ops:run(<<"memory_usage">>),        % Target: <1GB/10K ops

    % Network benchmark
    erlmcp_bench_network_real:run(<<"tcp_sustained_1k">>),  % Target: 10K+ conn/sec
    erlmcp_bench_network_real:run(<<"http_throughput">>),    % Target: 5K+ req/sec

    % Stress testing
    erlmcp_bench_stress:run(<<"stress_30s_10k_ops">>),      % Target: No failures
    erlmcp_bench_stress:run(<<"memory_exhaustion">>),       % Target: Graceful degradation

    % Integration testing
    erlmcp_bench_integration:run(<<"mcp_tool_sequence">>). % Target: <100ms avg latency
```

### Example 2: Adaptive Resource Allocation

```erlang
% Dynamic resource allocation based on workload
allocate_resources() ->
    SystemLoad = erlmcp_monitor:get_system_load(),
    ActiveConnections = erlmcp_registry:count_connections(),

    % Adaptive strategy selection
    Strategy = case {SystemLoad, ActiveConnections} of
        {high, _} when CPU > 80 -> shortest_job_first;
        {high, _} -> deadline_driven;
        {normal, > 10000} -> resource_aware;
        _ -> priority_based
    end,

    % Apply strategy
    erlmcp_scheduler:set_strategy(Strategy),
    erlmcp_scheduler:rebalance_queue().

% Example: CPU threshold-based scaling
scale_workers() ->
    CPU = erlmcp_monitor:get_cpu_usage(),
    CurrentWorkers = erlmcp_poolboy:worker_count(),

    if
        CPU > 85 andalso CurrentWorkers < Max ->
            erlmcp_poolboy:expand(2);
        CPU < 60 andalso CurrentWorkers > Min ->
            erlmcp_poolboy:shrink(1);
        true ->
            ok
    end.
```

These examples demonstrate the practical application of task orchestration patterns in real-world scenarios, showing how to coordinate complex workflows while maintaining quality, performance, and reliability standards.