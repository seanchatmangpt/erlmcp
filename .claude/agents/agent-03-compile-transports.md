---
name: agent-03-compile-transports
description: Transport layer compilation - stdio, tcp, http, websocket, sse
model: haiku
erlang_otp_context: true
phase: build
depends_on: agent-01-compile-gate
---

# Agent: Transport Compilation (agent-03)

## Purpose

Compiles the erlmcp_transports application containing all transport implementations (stdio, tcp, http, websocket, sse).

## Scope

**App**: `apps/erlmcp_transports`
**Modules**: 23 modules covering:
- Transport behavior (`erlmcp_transport`)
- Implementations (stdio, tcp, http, ws, sse)
- Infrastructure (pool, pipeline, registry)
- Security (header_validator, tls)

## Success Criteria

- [ ] All transport modules compiled
- [ ] Transport behavior contracts validated
- [ ] No missing dependencies (gun, ranch, cowboy)

## Commands

```bash
cd apps/erlmcp_transports
rebar3 compile 2>&1 | tee ../../.erlmcp/compile-transports.log
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🏭 AGENT 03: TRANSPORT COMPILATION                         ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Modules: 23 compiled

Transports:
  ✓ stdio
  ✓ tcp
  ✓ http
  ✓ websocket
  ✓ sse

Dependencies:
  ✓ gun (HTTP client)
  ✓ ranch (TCP acceptor pool)
  ✓ cowboy (HTTP server)
```

## Integration

**Depends on**: agent-01
**Parallel with**: agents 02, 04, 05
**Blocks**: agent-07 (test-ct - transport tests)
