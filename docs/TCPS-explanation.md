# TCPS Explanation & Rationale

This document explains why the Toyota Code Production System exists, the problems it solves, and how it changes MCP operations.

## Why TCPS?
Traditional support models rely on human heroics, tribal knowledge, and retrospective postmortems. TCPS replaces that with:
- **Deterministic manufacturing:** ontology → templates → artifacts.
- **Receipts over trust:** every stage yields evidence.
- **Autonomous agents:** MCP units execute standard work without waiting for humans.
- **Continuous improvement:** failures immediately feed 5-Whys + spec/test deltas.

## Core Principles Revisited
1. **Pull, don’t push:** demand drives production; prevents waste.
2. **Built-in quality (Jidoka):** abnormality detection halts publishing instantly.
3. **Level flow (Heijunka):** balanced workload maintains predictability.
4. **Error-proofing (Poka-yoke):** SHACL + templates enforce correctness upfront.
5. **Visible stoppage (Andon):** red events are public, actionable, and auditable.

## Impact on Teams
- **Infra:** ensures templates/ontology stay authoritative; no drift.
- **Product:** sees predictable release cadence with leveled buckets.
- **Support:** transforms into refusal receipts and regenerated fixes, not queues.
- **Security/compliance:** every remediation carries receipts, enabling audits.

## Relation to Other Docs
- Tutorial (`docs/TCPS.md`) shows the entire line.
- How-to (`docs/TCPS-howto.md`) provides step-by-step execution.
- Reference (`docs/TCPS-reference.md`) lists commands/artifacts.
- Checklist (`docs/TCPS-checklist.md`) enforces the gate.
- Certification (`docs/TCPS-certification.md`) records subsystem status.

## Future Evolution
- Integrate telemetry feedback to auto-adjust WIP limits.
- Expand SMED profiles for additional clouds/verticals.
- Automate Kaizen proposals based on receipt analytics.

TCPS is a cultural shift: proof replaces anecdotes, and MCP agents run the factory with stop-the-line power.
