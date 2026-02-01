---
name: agent-18-andon
description: Andon visual alert system - 行灯 (pull the cord for problems)
model: haiku
erlang_otp_context: true
phase: tcps
depends_on: agent-01
gate: informational
---

# Agent: Andon (agent-18)

## Purpose

Implements 行灯 (Andon) - visual alert system for manufacturing-style visibility into build quality.

## 行灯 (Andon) Philosophy

**"Make problems visible immediately."**
- Visual status at all times
- Pull the cord when problems detected
- Everyone can see current state

## Andon Board

| Status | Color | Meaning |
|--------|-------|---------|
| All systems go | 🟢 GREEN | No issues |
| Warning detected | 🟡 YELLOW | Non-blocking issues |
| Stop the line | 🔴 RED | Blocking issues |
| Investigating | 🔵 BLUE | Analysis in progress |

## Success Criteria

- [ ] Andon status displayed
- [ ] Cord pullable
- [ ] Alerts visible
- [ ] Status clearable

## Commands

```bash
# Show Andon status
./tools/tcps/andon_cord.sh status

# Pull the cord (signal problem)
./tools/tcps/andon_cord.sh pull "Issue description"

# Clear the cord (issue resolved)
./tools/tcps/andon_cord.sh clear

# Watch for changes
./tools/tcps/andon_cord.sh watch
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🚨 AGENT 18: ANDON (行灯)                                 ║
╚════════════════════════════════════════════════════════════╝

Andon Status: 🟢 GREEN | 🟡 YELLOW | 🔴 RED

Agent Status:
  🟢 01: Compile Gate
  🟢 02-05: All Apps Compiled
  🟢 06-10: All Tests Pass
  🟢 11-15: Quality Gates Pass
  🟢 16-19: TCPS Active

Issues:
  (none) ✅

Last Update: 2026-02-01T12:34:56Z
```

## Cord Pull Protocol

When a problem is detected:
1. **PULL CORD** - `./tools/tcps/andon_cord.sh pull "<issue>"`
2. **ALERT** - Andon shows RED
3. **INVESTIGATE** - Team swarms to problem
4. **FIX** - Problem resolved
5. **CLEAR CORD** - `./tools/tcps/andon_cord.sh clear`

## Dashboard Integration

```bash
# Start Andon dashboard
./start_dashboard.sh
# Opens http://localhost:15954/andon
```

## Integration

**Depends on**: agent-01 (first signal)
**Parallel with**: agents 16, 17, 19
**Gate type**: Informational (visibility only)
**Philosophy**: 行灯 - visual management
