---
name: agent-02-compile-core
description: Core application compilation specialist - erlmcp_core (JSON-RPC, registry, client/server)
model: haiku
erlang_otp_context: true
phase: build
depends_on: agent-01-compile-gate
---

# Agent: Core Compilation (agent-02)

## Purpose

Compiles the erlmcp_core application containing the MCP protocol implementation, client/server, registry, and session management.

## Scope

**App**: `apps/erlmcp_core`
**Modules**: 97 modules covering:
- Protocol (erlmcp_json_rpc, erlmcp_protocol)
- Client/Server (erlmcp_client, erlmcp_server)
- Registry (erlmcp_registry using gproc)
- Session (erlmcp_session_backend, manager)
- Security (erlmcp_auth, erlmcp_secrets)
- MCP (resources, tools, prompts)

## Success Criteria

- [ ] All core modules compiled to `.beam` files
- [ ] No undefined function calls within core
- [ ] Type specs validated (where present)
- [ ] Compile time < 30 seconds

## Commands

```bash
cd apps/erlmcp_core
rebar3 compile 2>&1 | tee ../../.erlmcp/compile-core.log

# Verify beam files exist
ls -la _build/default/lib/erlmcp_core/ebin/ | wc -l
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🏭 AGENT 02: CORE COMPILATION                             ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Modules: 97 compiled
Duration: Xs

Key Modules:
  ✓ erlmcp_json_rpc
  ✓ erlmcp_client
  ✓ erlmcp_server
  ✓ erlmcp_registry
  ✓ erlmcp_session_backend
  ✓ erlmcp_auth
  ✓ erlmcp_secrets
```

## Critical Files

- `src/erlmcp_json_rpc.erl` - JSON-RPC 2.0 protocol
- `src/erlmcp_client.erl` - MCP client implementation
- `src/erlmcp_server.erl` - MCP server implementation
- `src/erlmcp_registry.erl` - Process registry (gproc)

## Integration

**Depends on**: agent-01 (compile-gate)
**Blocks**: agent-06 (test-eunit - core tests)
**Parallel with**: agents 03, 04, 05

## Error Recovery

- Clean recompile: `rebar3 clean && rebar3 compile`
- Check for circular dependencies
- Verify include files are present
