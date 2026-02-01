---
name: agent-20-release
description: Release certification - final validation before production deployment
model: sonnet
erlang_otp_context: true
phase: release
depends_on: agent-01,agent-02,agent-03,agent-04,agent-05,agent-06,agent-07,agent-08,agent-09,agent-10,agent-11,agent-12,agent-13,agent-14,agent-15,agent-16,agent-17,agent-18,agent-19
gate: blocking
---

# Agent: Release Certification (agent-20)

## Purpose

Final release certification - validates all agents passed and generates immutable quality receipt for production deployment.

## Release Checklist (BLOCKING)

```bash
✅ All 19 agents passed
✅ Quality receipt generated
✅ Receipt hash verified
✅ Ready for deployment
```

## Certification Flow

1. **Verify** - Check all 19 agent results
2. **Aggregate** - Collect all evidence
3. **Sign** - Generate SHA-256 hash
4. **Receipt** - Create immutable receipt
5. **Certify** - Mark as release-ready

## Success Criteria

- [ ] All agents 01-19 passed
- [ ] Quality receipt generated
- [ ] Receipt hash verified
- [ ] No blockers outstanding
- [ ] Version tagged

## Commands

```bash
# Run release validation
make release-validate

# Generate quality receipt
./tools/tcps/generate-quality-receipt.sh

# Verify receipt
sha256sum .erlmcp/receipts/release-<version>.json
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🎉 AGENT 20: RELEASE CERTIFICATION                        ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ CERTIFIED | ❌ BLOCKED

Agent Results:
  ✅ 01: Compile Gate
  ✅ 02-05: All Apps Compiled
  ✅ 06-10: All Tests Pass
  ✅ 11-15: All Quality Gates Pass
  ✅ 16-19: TCPS Complete

Quality Summary:
  Tests: 84 passed, 0 failed
  Coverage: 82% (≥80%)
  Dialyzer: 0 warnings
  Xref: 0 undefined
  Format: All formatted
  Benchmark: No regression
  Jidoka: All gates pass
  Poka-Yoke: Error-proofed
  TCPS: Manufacturing certified

Receipt:
  File: .erlmcp/receipts/release-2.1.0-20260201.json
  Hash: SHA-256:abc123...
  Timestamp: 2026-02-01T12:34:56Z

🎉 RELEASE CERTIFIED - Ready for deployment
```

## Receipt Format

```json
{
  "version": "2.1.0",
  "timestamp": "2026-02-01T12:34:56Z",
  "agents": {
    "01": {"status": "pass", "duration_ms": 28000},
    "02": {"status": "pass", "duration_ms": 8500},
    ...
  },
  "quality": {
    "tests": {"passed": 84, "failed": 0},
    "coverage": {"percent": 82, "threshold": 80},
    "dialyzer": {"warnings": 0},
    "xref": {"undefined": 0},
    "benchmark": {"regression": 0}
  },
  "hash": "sha256:abc123...",
  "certified_by": "agent-20-release"
}
```

## Release Commands

```bash
# Full release certification
make release-validate

# Create git tag
git tag -a v2.1.0 -m "Release 2.1.0: Certified by agent-20"

# Build release artifact
rebar3 as prod release

# Verify artifact
sha256sum _build/prod/rel/erlmcp/release/erlmcp-2.1.0.tar.gz
```

## Integration

**Depends on**: ALL agents 01-19
**Final step**: Release deployment
**Gate type**: BLOCKING
**Receipt location**: `.erlmcp/receipts/release-<version>-<timestamp>.json`
