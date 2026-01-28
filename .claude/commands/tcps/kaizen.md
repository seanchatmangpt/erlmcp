---
name: tcps-kaizen
description: Kaizen (改善) - Continuous improvement proposals
category: tcps
invokes_agent: erlang-otp-developer
uses_rules: rules-docs-writer
japanese_term: 改善 (Kaizen - Change for Better)
manufacturing_stage: "Cross-stage: Improvement"
consolidates: [sparc/docs, sparc/docs-writer]
---

# Command: /tcps-kaizen

## Purpose
**Japanese**: 改善 (Kaizen - Change for Better)
**Manufacturing Stage**: Cross-stage - Continuous incremental improvement
**TCPS Pillar**: Kaizen (continuously reduce waste, improve quality)

Document improvements from Andon events, propose waste elimination, update standard work. Every failure becomes permanent improvement.

## Usage
```bash
/tcps-kaizen [propose|implement|document]
```

## Kaizen Proposal Process

1. **Identify waste**: Where is time/effort wasted?
2. **Propose improvement**: How can we eliminate waste?
3. **Test hypothesis**: Run experiment with metrics
4. **Measure results**: Did it improve lead time/defects?
5. **Standardize**: Update standard work if successful
6. **Share**: Document for other teams

## Examples

### Propose Kaizen
```bash
/tcps-kaizen propose KZ-2026-003 "Reduce test stage time from 30min to 15min"
```

### Implement Kaizen
```bash
/tcps-kaizen implement KZ-2026-003
```

Updates standard work, generates receipt.

## Tracked Metrics

- **Lead time**: Work order creation → release
- **Cycle time**: Build start → build complete
- **Defect rate**: Andon events per 100 builds
- **Rework rate**: Builds requiring retry
- **Test coverage**: % of code covered by tests
- **Build determinism**: % of builds with identical checksums

## See Also
- Related: `/tcps-andon` - Problem detection
- Related: `/5-whys-analyze` - Root cause analysis
- Docs: `docs/tcps/KAIZEN_GUIDE.md`

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /sparc docs, /sparc docs-writer
**Manufacturing Stage**: Cross-stage
