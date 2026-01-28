---
name: tcps-andon
description: Andon (行灯) - Visible stop-the-line signaling
category: tcps
invokes_agent: code-reviewer
uses_rules: rules-security-review
japanese_term: 行灯 (Andon - Lamp/Lantern)
manufacturing_stage: "Cross-stage: Problem Signaling"
consolidates: [sparc/review, sparc/security-review]
---

# Command: /tcps-andon

## Purpose
**Japanese**: 行灯 (Andon - Lamp/Lantern)
**Manufacturing Stage**: Cross-stage - Visible stop-the-line signaling
**TCPS Pillar**: Andon (visible problems, stop production immediately)

Trigger stop-the-line events, quarantine SKUs, make problems visible. Every Andon triggers 5 Whys analysis for permanent improvement.

## Usage
```bash
/tcps-andon [trigger|analyze|resolve]
```

## Andon Triggers

- SHACL validation failure
- Test failure (unit, integration, property)
- Build timeout
- Security vulnerability
- Missing receipt in chain
- Performance regression

## Andon Response Flow

```
Detect Abnormality → Stop Line → Issue Andon Receipt → Quarantine SKU → Run 5 Whys → Create Delta → Test Fix → Update Standard Work → Resume Production
```

## Examples

### Trigger Andon
```bash
/tcps-andon trigger test-failure "Integration test failed: erlmcp_transport_tcp:connect/2"
```

### Analyze Andon Event
```bash
/tcps-andon analyze andon-20260127-001
```

Runs 5 Whys analysis, generates root cause report.

### Resolve Andon
```bash
/tcps-andon resolve andon-20260127-001 --kaizen KZ-2026-003
```

Links resolution to Kaizen proposal, resumes production.

## See Also
- Related: `/5-whys-analyze` - Root cause analysis
- Related: `/tcps-kaizen` - Continuous improvement
- Docs: `docs/tcps/ANDON_RUNBOOK.md`

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /sparc review, /sparc security-review
**Manufacturing Stage**: Cross-stage
