# 🚀 Handoff to Implementation Agents

**From**: Design Phase (Complete)
**To**: Implementation Phase (Ready to Launch)
**Status**: All specifications ready, work orders prepared, branch ready

---

## What You're Taking Over

### Completed Deliverables

✅ **CLAUDE.md** — Updated with cloud execution, SessionStart hook, network requirements, cost estimates
✅ **CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md** — 697 lines of concrete implementation guidance
✅ **CLAUDE_CODE_WEB_STRATEGY_SUMMARY.md** — 360 lines executive overview
✅ **AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md** — 10 work orders, fully detailed, no ambiguity

### Supporting Specs (Reference)

- `DX_QOL_SPECIFICATION.md` — Pain points + solutions
- `PAIN_POINTS_RESEARCH.md` — Root cause analysis
- `INCREMENTAL_VALIDATION_DESIGN.md` — Cost optimization (80% savings)
- `WEB_AGENT_COORDINATION_DESIGN.md` — Multi-agent orchestration
- `WEB_OBSERVABILITY_DESIGN.md` — Metrics/dashboards

### Specs Not Implemented (By Design)

These are in the branch but are **not** needed for Phase 1. Skip them:
- Protocol FSM implementations (WO-001..003 are done, but these are design-only for now)
- Nine-nines chaos test suite (advanced, comes later)
- Metrics dashboard server (comes after hooks work)

### Your Job

Implement WO-001 through WO-010 exactly as specified in `AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md`.

**No deviations. No "improvements." Just build what's in the spec.**

---

## Critical Constraints (Non-Negotiable)

### 1. Chicago TDD (No Exceptions)
- Every feature gets a test
- Tests use real Erlang processes (no mocks, no stubs)
- All tests must pass before commit
- Coverage target: ≥80%

### 2. Armstrong Principles
- Illegal states unrepresentable
- Idempotent operations
- Let-it-crash friendly
- Observable behavior (logs, receipts)

### 3. File Locations
**Hooks** go in `.claude/hooks/`:
```
.claude/hooks/
├── SessionStart.sh       (OTP bootstrap)
├── policy-bash.sh        (network governance)
├── policy-websearch.sh   (search filtering)
├── post-write-ci.sh      (async CI trigger)
└── receipt.sh            (audit trail)
```

**Skills** go in `.claude/skills/<name>/`:
```
.claude/skills/otp-manager/
├── SKILL.md              (frontmatter + docs)
├── otp_fetch_build.sh
├── otp_verify.sh
└── otp_clean.sh
```

**Agents** go in `.claude/agents/`:
```
.claude/agents/
├── verifier.md           (verification subagent)
├── build-engineer.md     (code editing subagent)
└── release-scout.md      (read-only subagent)
```

**Config** goes in `.claude/`:
```
.claude/
├── settings.json         (hook registry + policy)
└── settings.local.json   (gitignored personal overrides)
```

### 4. Commits

Each agent commits per work order:
```bash
git commit -m "feat(WO-NNN): <title>

<description from work order>

Spec: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-NNN
Coverage: XX%
Tests: N passed, 0 failed"
```

### 5. Testing

Each work order must have:
- Unit tests (test the feature in isolation)
- Integration tests (test the feature in context)
- Real process tests (no mocks)
- Error cases (what happens when things fail)

---

## The Execution Model

### Parallel Execution (10 Agents)

All agents work simultaneously on independent work orders:
```
Agent1 (erlang-otp-developer)    → WO-001 (SessionStart)
Agent2 (erlang-transport-builder) → WO-002 (policy-bash)
Agent3 (code-reviewer)            → WO-003 (settings.json)
Agent4 (erlang-test-engineer)     → WO-004 (post-write-ci)
Agent5 (erlang-github-ops)        → WO-005 (receipt)
Agent6 (erlang-architect)         → WO-007 (verifier subagent)
Agent7 (erlang-otp-developer)     → WO-008 (build-engineer subagent)
Agent8 (erlang-researcher)        → WO-009 (release-scout subagent)
Agent9 (erlang-otp-developer)     → WO-006 (OTP manager skill)
Agent10 (erlang-github-ops)       → WO-010 (Makefile integration)
```

### No Blocking

Dependencies are minimal:
- WO-003 (settings.json) depends on WO-001, WO-002 (hook scripts exist)
- WO-010 (Makefile) depends on all others (final integration)

Everyone else is independent.

### Coordination

Just push your commits to the branch. They'll merge naturally in the final PR.

---

## Definition of Done

A work order is **done** when:
1. ✅ File(s) created (at correct path)
2. ✅ Test suite written (≥5 test cases per feature)
3. ✅ All tests pass locally
4. ✅ Code reviewed for Armstrong compliance
5. ✅ Documentation complete (comments + examples)
6. ✅ Committed to branch with proper message
7. ✅ Ready for `make check` (all gates)

---

## Success Criteria (Exit Condition)

After all 10 agents finish:
```bash
cd /home/user/erlmcp
git log --oneline | head -10  # Should show 10+ new commits

make hooks-validate           # All hooks exist + executable
make settings-validate        # settings.json valid JSON + schema
make governance-test          # All hook tests pass
make check                    # Compile + eunit + ct + coverage

# Should see:
# ✅ compile: 0 errors
# ✅ eunit: all pass
# ✅ ct: all pass
# ✅ coverage: ≥80%
# ✅ dialyzer: warnings = 0 (or advisory)
# ✅ xref: undefined = ∅ (or advisory)
```

If all gates pass, **implementation is complete**.

---

## Troubleshooting Guide

### "SessionStart hook can't find erl"
- Check: `which erl` exists
- Solution: Use absolute path `/usr/bin/erl` or ensure PATH set

### "Bash hook script has syntax errors"
- Check: `bash -n script.sh` (no -n = no syntax check)
- Solution: Run through shellcheck before committing

### "Tests expect OTP 28 but we have 25"
- Expected in current environment (SessionStart will fix in cloud)
- For local testing, mock or skip OTP-version-specific tests

### "Permission denied on hook script"
- Solution: `chmod +x .claude/hooks/script.sh`
- Make sure: All shell scripts are executable

### "JSON parsing fails in policy hook"
- Solution: Use `jq` (standard in most environments)
- Alternative: Use bash pure `grep`/`sed` (no dependencies)

### "Subagent tool access validation confusing"
- Reference: CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md (Part 3)
- Ask: What tools does the subagent need? (allow list)
- Ask: What tools should it NOT have? (deny list)

---

## What NOT to Do

❌ **Don't invent new features**
- Stick to spec in AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md

❌ **Don't add documentation beyond the spec**
- Comments + examples inside code OK
- New markdown files only if absolutely necessary

❌ **Don't modify existing modules**
- Only add new files under `.claude/`
- Don't touch `apps/erlmcp_core/src/` unless specified

❌ **Don't skip tests**
- Every feature gets a test
- No "we'll test it in cloud" excuses

❌ **Don't use mocks/fakes/stubs**
- Real Erlang processes only
- Use real files, real network (within allowlist)

❌ **Don't commit half-finished work**
- Commit only when "done" (all criteria met)
- Incomplete work blocks other agents' testing

---

## Reference Materials (Use Liberally)

| Document | Use For |
|----------|---------|
| `AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md` | **PRIMARY SPEC** — detailed per-WO requirements |
| `CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md` | Implementation guidance (concrete examples) |
| `CLAUDE.md` | Armstrong principles, OTP patterns, cloud semantics |
| `CLAUDE_CODE_WEB_STRATEGY_SUMMARY.md` | High-level "why" and architecture decisions |

---

## Timeline

- **Phase 1**: WO-001..009 parallel (2-3 hours)
- **Phase 2**: WO-010 integration (30 minutes)
- **Phase 3**: Final gates + quality check (10 minutes)
- **Total**: 3-4 hours to production-ready code

No blocking. No waiting. Just work.

---

## When You're Done

1. All 10 commits pushed to branch
2. `make check` passes (all gates)
3. Tests passing, coverage ≥80%
4. Ready for final PR review

At that point, you're **completely done**. The governance system is ready for:
- Cloud VM testing (SessionStart in real cloud)
- Agent execution (real agents using hooks)
- Deployment (merge to main, distribute via plugin)

---

## Questions?

Refer to:
- **WHAT to build?** → AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md
- **HOW to build?** → CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md (Part 3)
- **WHY this way?** → CLAUDE_CODE_WEB_STRATEGY_SUMMARY.md
- **What about Erlang patterns?** → CLAUDE.md

If specs are unclear, **ask for clarification** before starting. No guessing.

---

## Final Thought

This is boring infrastructure. It should be. It works because:

1. **Rules are enforced by the platform** (hooks called by Claude Code runtime)
2. **Illegal behavior is unrepresentable** (tool access, sessionStart idempotence)
3. **Verification is automatic** (Stop hook blocks bad state)
4. **Auditability is built-in** (SessionEnd hook + transcripts)

No agents need to "remember to do X." The environment forces it.

**That's the Armstrong principle in action.**

---

## GO!

You have everything you need. No ambiguity. No guessing. Just execute.

**Code like a Joe Armstrong AGI swarm.** 🧠⚙️

---

**Branch**: `claude/erlmcp-armstrong-innovations-DNaeK`
**Spec**: `AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md`
**Expected Completion**: 3-4 hours
**Human Intervention Required**: NONE
