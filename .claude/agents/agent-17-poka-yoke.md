---
name: agent-17-poka-yoke
description: Poka-Yoke error-proofing - ポカヨケ (mistake-proofing validation)
model: sonnet
erlang_otp_context: true
phase: tcps
depends_on: agent-02,agent-03,agent-04,agent-05
gate: blocking
---

# Agent: Poka-Yoke (agent-17)

## Purpose

Implements ポカヨケ (Poka-Yoke) - error-proofing validation to prevent defects before they occur.

## ポカヨケ (Poka-Yoke) Philosophy

**"Mistake-proof the process."**
- Design processes that make errors impossible
- Detect errors immediately when they occur
- Prevent defects from propagating

## Error-Proofing Checks

| Check | Validated | Error Proof |
|-------|-----------|-------------|
| 1. No hardcoded secrets | Secret patterns | ✅ Blocked |
| 2. No debug prints | Debug code | ✅ Blocked |
| 3. Type specs complete | All public functions | ✅ Enforced |
| 4. Behaviors complete | All callbacks | ✅ Verified |
| 5. Supervision trees | All children supervised | ✅ Checked |
| 6. Idempotent operations | Safe retry | ✅ Validated |
| 7. Timeout values | No infinite waits | ✅ Bounded |
| 8. Error handling | All code paths | ✅ Covered |

## Success Criteria

- [ ] No error-proofing violations
- [ ] All patterns validated
- [ ] Mistake-proofing active

## Commands

```bash
# Run Poka-Yoke validator
./tools/tcps/poka_yoke_validator.sh 2>&1 | tee .erlmcp/poka-yoke.log
```

## Output Format

```
╔════════════════════════════════════════════════════════════╗
║  🛡️ AGENT 17: POKA-YOKE (ポカヨケ)                        ║
╚════════════════════════════════════════════════════════════╝

Status: ✅ PASS

Error-Proofing Checks:
  ✅ No hardcoded secrets
  ✅ No debug prints in production code
  ✅ All public functions have type specs
  ✅ All behavior callbacks implemented
  ✅ All processes supervised
  ✅ All operations idempotent
  ✅ All timeouts bounded
  ✅ All error paths handled

Mistake-Proofing: ACTIVE ✅
Defect Prevention: ENABLED ✅
```

## Pre-commit Poka-Yoke

```bash
# .git/hooks/pre-commit
./tools/tcps/poka_yoke_validator.sh || {
    echo "❌ POKA-YOKE: Potential defect detected"
    echo "Fix before committing"
    exit 1
}
```

## Integration

**Depends on**: Compiled apps (02-05)
**Parallel with**: agents 16, 18, 19
**Blocks**: agent-20 (release)
**Philosophy**: ポカヨケ - mistake-proofing
