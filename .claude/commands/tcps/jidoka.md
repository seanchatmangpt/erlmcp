---
name: tcps-jidoka
description: Jidoka (自働化) - Built-in quality, stop-the-line authority
category: tcps
invokes_agent: erlang-test-engineer
uses_rules: rules-tdd
japanese_term: 自働化 (Jidoka - Automation with Human Touch)
manufacturing_stage: "Stage 7: Quality Checks"
consolidates: [sparc/test, sparc/tdd]
---

# Command: /tcps-jidoka

## Purpose
**Japanese**: 自働化 (Jidoka - Automation with Human Touch)
**Manufacturing Stage**: Stage 7 - Built-in quality checks with stop-the-line authority
**TCPS Pillar**: Jidoka (built-in quality, stop production on abnormalities)

Run quality checks with automatic line-stop authority. Don't inspect quality in—build it in. Every test failure triggers Andon.

## Usage
```bash
/tcps-jidoka [run|verify]
```

## Stop-the-Line Triggers

1. SHACL validation failure
2. Compilation error
3. Test failure (unit, integration, property)
4. Coverage drop below 80%
5. Security scan failure (Bandit/Dialyzer)
6. Missing receipt in chain
7. Non-deterministic build output
8. Performance regression

## Examples

```bash
/tcps-jidoka run
```

**On success**: Emits `receipts/test/results-*.json`, proceeds to release
**On failure**: Triggers Andon, quarantines SKU, blocks publishing

## See Also
- Related: `/tcps-andon` - Stop-the-line signaling
- Next: `/tcps-receipt` - Release with evidence
- Docs: `docs/tcps/TCPS.md` - Pillar 2: Jidoka

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /sparc test, /sparc tdd
**Manufacturing Stage**: Stage 7 of 8
