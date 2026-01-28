---
name: poka-yoke-validate
description: Poka-yoke (ポカヨケ) - Error-proofing validation
category: poka-yoke
invokes_agent: erlang-performance
japanese_term: ポカヨケ (Poka-yoke - Mistake-Proofing)
consolidates: [perf/analyze, analysis/bottleneck-detect, analysis/performance-report]
---

# Command: /poka-yoke-validate

## Purpose
**Japanese**: ポカヨケ (Poka-yoke - Mistake-Proofing)
**TCPS Usage**: Prevent errors before they happen

Validate plans, schemas, envelopes. Error-proof the system so defects cannot occur.

## Usage
```bash
/poka-yoke-validate [plan|schema|envelope]
```

## Validation Gates
1. Schema validation (required fields)
2. Envelope consistency (bounds realistic)
3. Refusal codes exist (1001-1089)
4. Evidence requirements met

## Examples
```bash
/poka-yoke-validate team-plan
```

## See Also
- Docs: `docs/PRICING_POKA_YOKE.md`
- Related: `/tcps-receipt` - Evidence bundles

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /perf analyze, /analysis/bottleneck-detect
