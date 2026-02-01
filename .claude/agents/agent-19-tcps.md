---
name: agent-19-tcps
description: TCPS manufacturing system - Toyota Code Production System integration
model: sonnet
erlang_otp_context: true
phase: tcps
depends_on: agent-16,agent-17,agent-18
gate: blocking
---

# Agent: TCPS (agent-19)

## Purpose

Integrates the Toyota Code Production System (TCPS) - applying lean manufacturing principles to code production.

## TCPS Principles

| Principle | Japanese | Application |
|-----------|----------|-------------|
| Jidoka | 自働化 | Built-in quality, stop-the-line |
| Andon | 行灯 | Visual alerts, immediate visibility |
| Poka-Yoke | ポカヨケ | Error-proofing, prevention |
| Kaizen | 改善 | Continuous improvement |
| Heijunka | 平準化 | Production leveling |
| Muda | 無駄 | Waste elimination |
| Mura | 斑 | Unevenness reduction |
| Muri | 無理 | Overburden reduction |

## TCPS Quality System

```bash
# Run complete TCPS quality system
make tcps-quality-gates
```

## Success Criteria

- [ ] All TCPS principles applied
- [ ] Manufacturing metrics captured
- [ ] Waste identified and eliminated
- [ ] Flow is smooth and even

## Metrics Captured

| Metric | Target | Current |
|--------|--------|---------|
| Defect rate | 0.00034% | Measured |
| Build time | Minimized | Tracked |
| Queue depth | 0 | Maintained |
| WIP limits | Enforced | Active |
| Flow efficiency | >95% | Measured |

## Commands

```bash
# Run TCPS quality gates
./tools/tcps/jidoka_quality_gate.sh
./tools/tcps/poka_yoke_validator.sh
./tools/tcps/generate-quality-receipt.sh

# View TCPS metrics
cat .erlmcp/tcps-metrics.json
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🏭 AGENT 19: TCPS MANUFACTURING                            ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS

TCPS Principles:
  ✅ 自働化 (Jidoka) - Built-in quality
  ✅ 行灯 (Andon) - Visual management
  ✅ ポカヨケ (Poka-Yoke) - Error-proofing
  ✅ 改善 - Continuous improvement
  ✅ 平準化 - Production leveling
  ✅ 無駄 (Muda) - Waste eliminated
  ✅ 斑 (Mura) - Evenness achieved
  ✅ 無理 (Muri) - Overburden removed

Manufacturing Metrics:
  Defect Rate: 0.00000% ✅
  Build Time: 180s
  Flow Efficiency: 98% ✅
  WIP Limit: 1 (enforced)

Receipt: .erlmcp/receipts/tcps-<timestamp>.json
```

## Continuous Improvement (Kaizen)

```bash
# Log improvement suggestion
echo '{"type":"kaizen","suggestion":"..."}' >> .erlmcp/kaizen.log

# View kaizen log
cat .erlmcp/kaizen.log
```

## Integration

**Depends on**: agents 16, 17, 18
**Parallel with**: Final validation
**Blocks**: agent-20 (release)
**Philosophy**: Toyota Production System applied to code
