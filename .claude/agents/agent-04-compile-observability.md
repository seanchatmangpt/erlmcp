---
name: agent-04-compile-observability
description: Observability stack compilation - OTEL, metrics, tracing, dashboard
model: haiku
erlang_otp_context: true
phase: build
depends_on: agent-01-compile-gate
---

# Agent: Observability Compilation (agent-04)

## Purpose

Compiles the erlmcp_observability application containing OTEL integration, metrics, tracing, and dashboard server.

## Scope

**App**: `apps/erlmcp_observability`
**Modules**: 31 modules covering:
- OTEL (erlmcp_otel, tracer, metrics)
- Dashboard (erlmcp_dashboard_server)
- Monitoring (erlmcp_health_monitor)
- Chaos (erlmcp_chaos)
- Audit (erlmcp_audit)

## Success Criteria

- [ ] All observability modules compiled
- [ ] OTEL API integration validated
- [ ] Dashboard server compiles

## Commands

```bash
cd apps/erlmcp_observability
rebar3 compile 2>&1 | tee ../../.erlmcp/compile-observability.log
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🏭 AGENT 04: OBSERVABILITY COMPILATION                     ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Modules: 31 compiled

Components:
  ✓ OTEL Integration
  ✓ Metrics Collection
  ✓ Tracing
  ✓ Dashboard Server
  ✓ Health Monitor
  ✓ Chaos Engine
  ✓ Audit Logger
```

## Integration

**Depends on**: agent-01
**Parallel with**: agents 02, 03, 05
**Blocks**: agent-09 (test-smoke - health checks)
