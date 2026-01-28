---
name: tcps-build
description: Standard Work - Follow documented build process
category: tcps
invokes_agent: erlang-otp-developer
uses_rules: rules-code + rules-tdd
japanese_term: 標準作業 (Hyōjun Sagyō - Standard Work)
manufacturing_stage: "Stages 6-7: Build & Test"
consolidates: [sparc/code]
---

# Command: /tcps-build

## Purpose
**Japanese**: 標準作業 (Hyōjun Sagyō - Standard Work)
**Manufacturing Stage**: Stages 6-7 - Build & Compile
**TCPS Pillar**: Standard Work (documented processes for every stage)

Execute standard build process following documented procedures. Every stage has defined inputs, outputs, SLO budgets, receipts, and failure modes.

## Usage
```bash
/tcps-build [module]
```

## Standard Work Document

**Stage 6: Build**
- Inputs: Rendered source files in `src/`, `rebar.lock`, `rebar.config`
- Process: Verify deps, compile (rebar3 compile), capture output, verify BEAM files, calculate checksums
- Outputs: BEAM files in `_build/default/lib/erlmcp/ebin/`, compiler warnings, build receipt
- SLO: 10 minutes maximum
- Receipt: `receipts/build/compile-*.json`

## See Also
- Next: `/tcps-jidoka` - Built-in quality
- Docs: `docs/tcps/STANDARD_WORK.md`

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /sparc code
**Manufacturing Stage**: Stages 6-7 of 8
