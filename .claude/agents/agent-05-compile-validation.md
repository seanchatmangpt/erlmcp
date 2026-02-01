---
name: agent-05-compile-validation
description: Validation app compilation - spec compliance, transport validators, CLI
model: haiku
erlang_otp_context: true
phase: build
depends_on: agent-01-compile-gate
---

# Agent: Validation Compilation (agent-05)

## Purpose

Compiles the erlmcp_validation application containing spec validators, transport validators, and the CLI tool.

## Scope

**App**: `apps/erlmcp_validation`
**Modules**: 13 modules covering:
- Validators (protocol, transport, security)
- Report generator (erlmcp_compliance_report)
- CLI (erlmcp_validate_cli)
- Spec parser

## Success Criteria

- [ ] All validation modules compiled
- [ ] CLI escript buildable
- [ ] Schema validators loaded (jesse)

## Commands

```bash
cd apps/erlmcp_validation
rebar3 compile 2>&1 | tee ../../.erlmcp/compile-validation.log
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🏭 AGENT 05: VALIDATION COMPILATION                        ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS
Modules: 13 compiled

Validators:
  ✓ Protocol Validator
  ✓ Transport Validator
  ✓ Security Validator

Tools:
  ✓ Compliance Reporter
  ✓ CLI (escript ready)
```

## Integration

**Depends on**: agent-01
**Parallel with**: agents 02, 03, 04
**Blocks**: agent-14 (format - validation reports)
