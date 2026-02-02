# MCP Observability for erlmcp - Executive Summary

**Date:** 2026-02-01  
**Version:** 1.0.0  
**Author:** Erlang Performance Agent + Architecture Team

---

## Overview

This comprehensive observability design addresses all 7 requirements for erlmcp's MCP implementation:

1. ✅ **OpenTelemetry Integration** - Distributed tracing for all MCP operations
2. ✅ **MCP-Specific Metrics** - 50+ custom metrics for resources, tools, prompts, subscriptions
3. ✅ **Performance Profiling** - fprof/eprof integration + flame graphs
4. ✅ **Health Monitoring** - MCP-specific health checks + alerting
5. ✅ **claude-flow Integration** - Hybrid observability for SONA requirements
6. ✅ **Compliance Dashboards** - Real-time MCP 2025-11-25 spec compliance tracking
7. ✅ **Chaos Engineering** - 8 MCP-specific chaos scenarios

---

## Deliverables

### Documentation (4 files)

| File | Purpose | Location |
|------|---------|----------|
| `MCP_OBSERVABILITY_DESIGN.md` | High-level design specification | `/home/user/erlmcp/docs/` |
| `MCP_OBSERVABILITY_IMPLEMENTATION_STRATEGY.md` | Detailed implementation with code examples | `/home/user/erlmcp/docs/` |
| `MCP_OBSERVABILITY_INSTRUMENTATION_GUIDE.md` | Quick reference for developers | `/home/user/erlmcp/docs/` |
| `MCP_OBSERVABILITY_SUMMARY.md` | Executive summary (this file) | `/home/user/erlmcp/docs/` |

### Code Modules to Implement (3 new modules)

| Module | Purpose | Estimated LOC |
|--------|---------|---------------|
| `erlmcp_mcp_metrics.erl` | MCP-specific metrics collection | ~500 |
| `erlmcp_mcp_chaos.erl` | MCP chaos engineering scenarios | ~400 |
| `erlmcp_mcp_benchmarks.erl` | Automated performance benchmarks | ~600 |

### Code Instrumentation (1 existing module)

| Module | Changes | Impact |
|--------|---------|--------|
| `erlmcp_server.erl` | Add OTEL spans + metrics to 8 operations | ~200 LOC added |

### Dashboard (1 new view)

| File | Purpose | Technology |
|------|---------|------------|
| `mcp_compliance.html` | Real-time compliance dashboard | HTML + WebSocket + Plotly.js |

---

## Key Features

### 1. OpenTelemetry Integration

**Comprehensive Span Coverage:**
- `mcp.initialize` - Protocol handshake
- `mcp.resources.{list,read,subscribe,notify}` - Resource operations
- `mcp.tools.{list,call,validate_schema,execute_handler}` - Tool operations
- `mcp.prompts.{list,get}` - Prompt operations
- `mcp.transport.{send,receive}` - Transport layer
- `mcp.jsonrpc.{encode,decode}` - JSON-RPC processing

**Trace Context Propagation:**
- Across transport boundaries (stdio, TCP, HTTP, WebSocket)
- Between Rust (claude-flow) and Erlang (erlmcp)
- Nested spans for detailed operation breakdown

**Example Instrumentation:**
```erlang
handle_call({call_tool, Name, Args}, From, State) ->
    Span = erlmcp_otel:start_span(<<"mcp.tools.call">>, #{
        <<"tool.name">> => Name
    }),
    try
        Result = execute_tool(Name, Args, State),
        erlmcp_otel:end_span(Span),
        {reply, Result, State}
    catch
        Class:Reason:Stack ->
            erlmcp_otel:record_error(Span, {Class, Reason, Stack}),
            erlmcp_otel:end_span(Span),
            {reply, {error, Reason}, State}
    end.
```

### 2. MCP-Specific Metrics (50+ metrics)

**Metric Categories:**

| Category | Metrics | Examples |
|----------|---------|----------|
| Resources | 8 metrics | `erlmcp.mcp.resources.list.latency_ms`, `erlmcp.mcp.resources.notify.fan_out` |
| Tools | 8 metrics | `erlmcp.mcp.tools.call.latency_ms`, `erlmcp.mcp.tools.call.validation_time_ms` |
| Prompts | 3 metrics | `erlmcp.mcp.prompts.get.latency_ms` |
| Transport | 5 metrics | `erlmcp.mcp.transport.send.latency_ms`, `erlmcp.mcp.jsonrpc.encode_latency_ms` |
| Session | 4 metrics | `erlmcp.mcp.session.active_count`, `erlmcp.mcp.session.init_latency_ms` |

**Metric Types:**
- **Histograms** - Latency percentiles (P50/P95/P99)
- **Counters** - Throughput, error counts
- **Gauges** - Active sessions, subscription counts

**Performance Baseline Tracking:**
```
Baseline (v2.1.0):
- P50 latency: 5ms
- P95 latency: 20ms
- P99 latency: 50ms
- Throughput: 1K-10K req/s
```

### 3. Performance Profiling & Flame Graphs

**Three-Tier Profiling:**

1. **Continuous Lightweight** (Always On)
   - Sample-based tracing (1% sample rate)
   - Metrics histograms
   - Process info snapshots every 60s

2. **On-Demand Deep Profiling** (Manual)
   - fprof for function-level CPU profiling
   - eprof for time-based profiling
   - recon_trace for live message tracing

3. **Auto-Triggered** (Incident Response)
   - Latency spike detection (P99 > 100ms)
   - Memory leak detection (heap growth > 20%/min)
   - Message queue buildup (>1000 messages)

**Flame Graph Generation:**
```erlang
%% Profile tool_call operation
erlmcp_profiler:profile(erlmcp_server, handle_call, 3, #{
    duration => 60000,
    output => "/tmp/tool_call.fprof",
    mode => fprof
}),

%% Generate flame graph
erlmcp_profiler:flame_graph("/tmp/tool_call.fprof", "/tmp/flame.svg").
```

**Automated Regression Detection:**
- Run benchmarks on every PR (CI)
- Compare against baseline (10% regression threshold)
- Fail CI if performance degrades

### 4. Health Monitoring & Alerting

**MCP-Specific Health Checks:**

| Check | Condition | Action |
|-------|-----------|--------|
| Resource List Latency | P95 < 20ms | Alert if degraded |
| Tool Call Availability | All tools callable | Alert if unavailable |
| Subscription Fan-out | P95 < 50ms | Alert if slow |

**Alert Definitions:**

| Alert | Severity | Trigger | Action |
|-------|----------|---------|--------|
| `MCP_ResourceListLatencyHigh` | Warning | P95 > 20ms for 5 min | Log, notify ops |
| `MCP_ToolCallFailureRate` | Warning | Error rate > 5% | Investigate |
| `MCP_SessionInitFailure` | Critical | Init timeout > 10% | Page oncall |

**Integration with erlmcp_health_monitor:**
```erlang
erlmcp_health_monitor:register_component(
    {mcp_resources, ServerId},
    Pid,
    fun() -> check_resource_list_latency(ServerId) end
).
```

### 5. Integration with claude-flow Observability

**Hybrid Architecture:**

```
┌─────────────────────────────┐
│  claude-flow (Rust)         │
│  SONA Layer (<0.05ms)       │
│  - Local metrics            │
│  - 1% trace sampling        │
└──────────┬──────────────────┘
           │ Trace Context
           ▼
┌─────────────────────────────┐
│  erlmcp (Erlang)            │
│  MCP Layer (1-5ms)          │
│  - Full OTEL traces         │
│  - MCP-specific metrics     │
└─────────────────────────────┘
```

**Shared Metrics Export:**
- Common Prometheus endpoint: `http://localhost:9090/metrics`
- Standard MCP metric naming: `mcp_operation_duration_seconds`
- Compatible labels for cross-layer analysis

**Trace Context Bridging:**
- Serialize trace context in JSON-RPC messages
- Restore parent span in erlmcp
- Create child spans for MCP operations

### 6. MCP Compliance Tracking Dashboard

**Real-time Dashboard:** `http://localhost:9090/mcp-compliance`

**Sections:**

1. **Protocol Compliance**
   - MCP spec version: 2025-11-25
   - Initialize handshake success rate
   - Capabilities coverage

2. **Performance Compliance**
   - Latency budgets per operation
   - Throughput targets
   - Memory per connection

3. **Feature Compliance**
   - Resources: Static ✓, Templates ✓, Subscriptions ✓
   - Tools: Schema validation ✓, Cancellation ✓
   - Prompts: Arguments ✓, Schema ✓

4. **Real-time Violations**
   - Latency violations (P95 > threshold)
   - Error rate violations (>5%)
   - Schema validation failures

**Weekly Compliance Report:**
```
MCP Compliance Report - Week of 2026-02-01
==========================================

Protocol Compliance: ✓ PASS (99.8% success rate)
Performance Compliance: ⚠ WARNING (tools/call P95=52ms, 3 violations)
Feature Compliance: ✓ PASS (100% implementation)

Recommendations:
1. Investigate tools/call P95 violations (tool: "complex_analysis")
2. Consider schema validation caching (75% improvement expected)
```

### 7. Chaos Engineering Tests

**8 MCP-Specific Chaos Scenarios:**

| Scenario | Fault Type | Expected Behavior |
|----------|-----------|-------------------|
| Resource Handler Timeout | 20% handlers timeout | P95 < 100ms, error rate < 10% |
| Tool Handler Crash | 10% tools crash | Supervisor restart < 1s |
| Subscription Flood | 1000 subscribers, 10 Hz | P95 notify < 50ms, 0 drops |
| JSON Corruption | 5% message corruption | 100% detection, reject invalid |
| Transport Jitter | +100ms network latency | Maintain SLA (P95 < 150ms) |
| Schema Validation Failure | 10% invalid inputs | 100% rejection, no crashes |
| Memory Leak | Gradual heap growth | Alert < 5min, auto-restart |
| Message Queue Buildup | >1000 queued messages | Apply backpressure, no OOM |

**Automated GameDay:**
```erlang
%% Run chaos suite every Monday at 2 AM UTC
schedule_chaos_gameday() ->
    erlcron:cron({weekly, monday, {2, 0, 0}}, fun() ->
        Results = erlmcp_mcp_chaos:run_all_scenarios(),
        notify_ops_team(Results)
    end).
```

**Safety Controls:**
- Blast radius limits (max 30% affected)
- SLA threshold enforcement
- Auto-rollback on violations
- Dry-run mode for testing

---

## Implementation Plan

### Phase 1: Foundation (Week 1-2)
- [ ] Create `erlmcp_mcp_metrics.erl`
- [ ] Instrument `erlmcp_server.erl` (OTEL spans)
- [ ] Add MCP health checks
- [ ] Create compliance dashboard view

### Phase 2: Profiling & Benchmarks (Week 3-4)
- [ ] Create `erlmcp_mcp_benchmarks.erl`
- [ ] Integrate flame graph generation
- [ ] CI performance regression detection
- [ ] Profile critical paths

### Phase 3: Chaos Engineering (Week 5-6)
- [ ] Create `erlmcp_mcp_chaos.erl`
- [ ] Implement 8 chaos scenarios
- [ ] GameDay automation
- [ ] Chaos report generation

### Phase 4: Integration & Documentation (Week 7-8)
- [ ] claude-flow observability integration
- [ ] Prometheus exporter
- [ ] Trace context bridging
- [ ] Observability runbook

**Total Implementation Time:** 6-8 weeks  
**Team Size:** 2-3 engineers  
**Risk Level:** Low (builds on existing infrastructure)

---

## Performance Impact Analysis

### Overhead Estimates

| Component | Overhead | Mitigation |
|-----------|----------|------------|
| OTEL Spans | 0.1-0.5ms per operation | Sample-based (1% for low-priority ops) |
| Metrics Collection | 0.01-0.05ms per metric | Async gen_server:cast |
| Health Checks | 30s interval | Dedicated process, non-blocking |
| Profiling | <5% CPU | On-demand only, disabled in production |

**Total Expected Overhead:** <5% latency increase, <2% CPU increase

### Benefits

| Benefit | Impact |
|---------|--------|
| Performance Regression Detection | Prevent 10%+ performance degradation |
| Incident Response Time | Reduce MTTR from hours to minutes |
| Chaos Resilience | Validate fault tolerance before production |
| Compliance Assurance | Maintain MCP spec compliance |

**ROI:** High (prevents performance regressions worth >100x overhead cost)

---

## Dependencies

### Existing erlmcp Modules (Leverage)
- `erlmcp_otel.erl` - OpenTelemetry integration
- `erlmcp_metrics.erl` - Metrics collector
- `erlmcp_profiler.erl` - CPU/memory profiling
- `erlmcp_health_monitor.erl` - Health checks
- `erlmcp_chaos.erl` - Chaos engineering
- `erlmcp_dashboard_server.erl` - WebSocket dashboard

### External Dependencies (Required)
- `opentelemetry_api` 1.5.0+
- `opentelemetry` 1.7.0+
- `opentelemetry_exporter` 1.10.0+
- `prometheus_client` 4.10.0+ (optional, for Prometheus export)

### Tools (Optional)
- FlameGraph scripts (Brendan Gregg)
- Jaeger/Zipkin for trace visualization
- Prometheus + Grafana for metrics visualization

---

## Success Criteria

| Metric | Current | Target | Method |
|--------|---------|--------|--------|
| MCP Operation Coverage | 60% | 100% | OTEL span count |
| Metric Cardinality | 20 | 50+ | Prometheus labels |
| Profiling Overhead | N/A | <5% | CPU benchmarks |
| Chaos Test Coverage | 0 | 8 scenarios | Test suite |
| Alert Response Time | Manual | <5 min | Auto-alerts |
| Compliance Violations | Unknown | 0 | Weekly report |

**Gate for Production:**
- [ ] All 8 chaos scenarios pass
- [ ] <5% performance overhead
- [ ] 100% MCP operation coverage
- [ ] 0 compliance violations for 1 week

---

## Next Steps

1. **Review & Approval** - Stakeholder review of design docs
2. **Resource Allocation** - Assign 2-3 engineers
3. **Sprint Planning** - Break into 2-week sprints (4 phases)
4. **Kickoff** - Begin Phase 1 implementation
5. **Weekly Reviews** - Track progress, adjust plan

---

## Questions & Contact

**Design Reviewers:**
- Erlang Performance Agent
- Erlang Architect
- Erlang OTP Developer
- Code Reviewer

**Approval Required From:**
- Tech Lead (Architecture)
- Product Manager (Priorities)
- DevOps Lead (Monitoring integration)

---

**END OF SUMMARY**

See detailed design in:
- `/home/user/erlmcp/docs/MCP_OBSERVABILITY_DESIGN.md`
- `/home/user/erlmcp/docs/MCP_OBSERVABILITY_IMPLEMENTATION_STRATEGY.md`
- `/home/user/erlmcp/docs/MCP_OBSERVABILITY_INSTRUMENTATION_GUIDE.md`
