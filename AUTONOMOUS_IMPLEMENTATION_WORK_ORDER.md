# Autonomous Implementation Work Order

**Master Task**: Implement complete Claude Code Web governance system for erlmcp.

**Execution Mode**: 10 agents in parallel, full autonomy, no human intervention.

**Success Criterion**: All hooks working, Makefile integrated, CLI updated, tests passing.

---

## The 10 Work Orders (Execute in Parallel)

### WO-001: SessionStart Hook Implementation
**Agent**: `erlang-otp-developer`
**Task**: Implement `.claude/hooks/SessionStart.sh` (OTP 28 bootstrap)

**Specification**:
- File: `.claude/hooks/SessionStart.sh`
- Logic: Detect OTP version, install if missing, persist env vars
- Testing: Mock cloud env (`CLAUDE_CODE_REMOTE=true`), verify OTP available
- Integration: Make sure subsequent Bash commands inherit env vars

**Deliverables**:
- ✅ Executable script (200 lines)
- ✅ Unit test harness
- ✅ Integration test (real OTP bootstrap)
- ✅ Error recovery (if OTP install fails, clear cache + retry)

**Dependencies**: None

---

### WO-002: Policy-Bash Hook Implementation
**Agent**: `erlang-transport-builder`
**Task**: Implement `.claude/hooks/policy-bash.sh` (network + FS governance)

**Specification**:
- File: `.claude/hooks/policy-bash.sh`
- Input: PreToolUse event JSON (Bash tool, command)
- Output: `{permissionDecision: "allow"|"ask"|"deny", reason: "...", updatedInput: {...}}`
- Logic: Block dangerous commands, ask for unknown domains, allow build commands

**Allowlist**:
- github.com, hex.pm, erlang-solutions.com
- erl, rebar3, make, git (to allowlisted hosts)

**Deny**:
- sudo, rm -rf, non-deterministic commands

**Deliverables**:
- ✅ Bash script with JSON output (150 lines)
- ✅ Test suite (25+ test cases)
- ✅ Integration with PreToolUse event format

**Dependencies**: None

---

### WO-003: Settings.json Configuration
**Agent**: `code-reviewer`
**Task**: Create `.claude/settings.json` (hook registry + policy)

**Specification**:
- File: `.claude/settings.json`
- Sections:
  - permissions (deny .env, secrets/)
  - hooks (SessionStart, PreToolUse, PostToolUse, Stop, SessionEnd)
  - subagents (verifier, build-engineer, release-scout)
- Validation: JSON schema compliance, hook paths exist, subagent configs valid

**Deliverables**:
- ✅ Valid settings.json (500 lines)
- ✅ JSON schema + documentation
- ✅ Validation script (verify all hooks exist, all subagents defined)
- ✅ Example `.claude/settings.local.json` (for personal overrides)

**Dependencies**: WO-001, WO-002, WO-004, WO-005, WO-006

---

### WO-004: Post-Write CI Hook
**Agent**: `erlang-test-engineer`
**Task**: Implement `.claude/hooks/post-write-ci.sh` (async CI triggering)

**Specification**:
- File: `.claude/hooks/post-write-ci.sh`
- Trigger: PostToolUse event (Write or Edit tool)
- Logic: If .erl/.hrl file modified, async compile + test
- Constraints:
  - Non-blocking (async: true)
  - Logs to `.erlmcp/build.log` and `.erlmcp/test.log`
  - Timeout: 120 seconds
  - Doesn't interrupt agent (returns immediately)

**Deliverables**:
- ✅ Bash script (100 lines)
- ✅ Log management (rotate .erlmcp/build.log)
- ✅ Test suite (verify async execution, log output)

**Dependencies**: None (but expect WO-001 complete for env vars)

---

### WO-005: Receipt/Audit Hook
**Agent**: `erlang-github-ops`
**Task**: Implement `.claude/hooks/receipt.sh` (SessionEnd audit trail)

**Specification**:
- File: `.claude/hooks/receipt.sh`
- Trigger: SessionEnd event
- Logic:
  1. Capture OTP version, erlmcp version, build hash
  2. Write JSON receipt to `.erlmcp/receipts/<timestamp>.json`
  3. Archive transcript if available (TRANSCRIPT_PATH env var)
  4. Generate summary (cost estimate, time, pass/fail gates)

**Receipt Format**:
```json
{
  "session_id": "session_...",
  "timestamp": "ISO-8601",
  "otp_version": "28.3.1",
  "erlmcp_version": "v3.0.0",
  "build_hash": "abc123...",
  "quality_gates": {
    "compile": "pass|fail",
    "eunit": "pass|fail",
    "ct": "pass|fail"
  },
  "cost_estimate": "$0.XX",
  "time_seconds": 480
}
```

**Deliverables**:
- ✅ Bash script (150 lines)
- ✅ JSON schema for receipt
- ✅ Archival logic (transcript + receipt)
- ✅ Test suite (verify JSON validity, receipt structure)

**Dependencies**: None

---

### WO-006: OTP Manager Skill
**Agent**: `erlang-otp-developer`
**Task**: Create `.claude/skills/otp-manager/` (reusable OTP procedures)

**Specification**:
- File: `.claude/skills/otp-manager/SKILL.md`
- Slash command: `/otp-manager <fetch-build|verify|clean>`
- Scripts:
  - `otp_fetch_build.sh` — Download + build OTP 28.3.1
  - `otp_verify.sh` — Check version, compilation
  - `otp_clean.sh` — Cleanup artifacts

**Deliverables**:
- ✅ SKILL.md (markdown frontmatter + docs)
- ✅ 3 supporting bash scripts (200 lines total)
- ✅ Test suite (verify slash command invocation)
- ✅ Integration (can be preloaded by subagents)

**Dependencies**: WO-001 (reuse SessionStart logic)

---

### WO-007: Verifier Subagent
**Agent**: `erlang-architect`
**Task**: Create `.claude/agents/verifier.md` (locked-down verification)

**Specification**:
- File: `.claude/agents/verifier.md`
- Tool access:
  - Allow: Bash, Read
  - Deny: Write, Edit, Delete, WebSearch
- Bash constraints: Only allow erl/rebar3 verify commands
- Behavior: Run OTP check + compile + unit tests, return JSON decision
- Used by: Stop hook (blocks agent completion)

**Deliverables**:
- ✅ verifier.md (markdown + YAML frontmatter)
- ✅ Subagent prompt + logic
- ✅ Test suite (verify tool access restrictions)
- ✅ Integration test (verify with Stop hook)

**Dependencies**: WO-001 (needs OTP available)

---

### WO-008: Build Engineer Subagent
**Agent**: `erlang-otp-developer`
**Task**: Create `.claude/agents/build-engineer.md` (code editing role)

**Specification**:
- File: `.claude/agents/build-engineer.md`
- Tool access:
  - Allow: Bash, Read, Write
  - Constraint: Write only in `apps/erlmcp_*/src`
  - Deny: Delete, WebSearch
- Behavior: Can implement features, write tests, run compile/test
- Used by: Feature implementation tasks

**Deliverables**:
- ✅ build-engineer.md (markdown + YAML)
- ✅ Subagent prompt + constraints
- ✅ Test suite (verify Write constraints enforced)

**Dependencies**: None

---

### WO-009: Release Scout Subagent
**Agent**: `erlang-researcher`
**Task**: Create `.claude/agents/release-scout.md` (read-only specialist)

**Specification**:
- File: `.claude/agents/release-scout.md`
- Tool access:
  - Allow: Read, WebSearch
  - Deny: Bash, Write, Edit, Delete
- Behavior: Search GitHub releases, find new OTP versions, generate reports
- Used by: Release tracking tasks

**Deliverables**:
- ✅ release-scout.md (markdown + YAML)
- ✅ Subagent prompt (search strategies)
- ✅ Test suite (verify WebSearch allowed, Write blocked)

**Dependencies**: None

---

### WO-010: Makefile Integration + CLI Updates
**Agent**: `erlang-github-ops`
**Task**: Update Makefile and CLI for governance system

**Specification**:
- Makefile targets:
  - `make hooks-validate` — Verify all hooks exist, are executable
  - `make settings-validate` — Validate settings.json schema
  - `make governance-test` — Run hook test suites
  - `make receipts-list` — Show recent session receipts

- CLI script `.claude/commands/governance.sh`:
  - `governance hooks` — List active hooks
  - `governance receipts` — Show recent receipts
  - `governance verify` — Run verification subagent manually

- Documentation:
  - Update DEVELOPMENT.md with hook lifecycle
  - Add troubleshooting guide
  - Add examples

**Deliverables**:
- ✅ Makefile targets (50 lines)
- ✅ CLI script (150 lines)
- ✅ Documentation updates
- ✅ Integration tests

**Dependencies**: WO-001 through WO-009 (needs all pieces in place)

---

## Execution Strategy

### Phase 1: Parallel Implementation (WO-001 through WO-009)
- All 10 agents work simultaneously
- No blockers (WO-001–009 are independent)
- Each agent:
  1. Creates file(s)
  2. Writes test suite
  3. Verifies locally (no external deps needed)
  4. Commits to branch

### Phase 2: Integration (WO-010)
- Runs after all others complete
- Integrates hooks into Makefile
- Updates CLI
- Runs comprehensive test

### Phase 3: Quality Gate
- All agents run: `make check`
- All hooks validated
- All tests pass
- All receipts generated

---

## Success Criteria

### Functional
- ✅ SessionStart hook runs on cloud VM startup
- ✅ OTP 28.3.1+ available after SessionStart
- ✅ PreToolUse blocks dangerous commands
- ✅ PostToolUse triggers async CI
- ✅ Stop hook blocks if tests fail
- ✅ SessionEnd writes receipt
- ✅ All skills invokable (/otp-manager, etc.)
- ✅ All subagents have tool constraints enforced

### Testing
- ✅ Each hook has ≥5 test cases
- ✅ Each skill has ≥3 invocation tests
- ✅ Each subagent tool access validated
- ✅ Integration test (full flow end-to-end)
- ✅ Coverage ≥80% for new code

### Quality
- ✅ No mocks (real processes only)
- ✅ Idempotent (hooks safe to call multiple times)
- ✅ Documented (comments + examples)
- ✅ Committed to branch (ready for PR)

---

## Coordination Notes

- **No human intervention**: Agents work autonomously
- **No blocking**: Use independent work orders, parallel execution
- **Real tests**: No fakes, no stubs, use real Erlang processes
- **Chicago TDD**: Code + tests committed together
- **Armstrong compliance**: Illegal states unrepresentable

---

## Timeline

- **Phase 1 (Parallel)**: All 10 agents, ~2-3 hours total
- **Phase 2 (Integration)**: WO-010, ~30 minutes
- **Phase 3 (Quality)**: Final gates, ~10 minutes
- **Total**: ~3-4 hours to production-ready code

---

## Branch Management

- **Create branch**: `claude/governance-implementation-<timestamp>`
- **Commits**: Each agent commits per WO completion
- **Final PR**: Merge all commits to master branch
- **Quality**: All gates pass before merge

---

## After Implementation

- [ ] Run `make check` (compile + tests + coverage)
- [ ] Verify hook lifecycle:
  - [ ] SessionStart fires (OTP ready)
  - [ ] PreToolUse fires (policy enforced)
  - [ ] PostToolUse fires (async CI)
  - [ ] Stop fires (verification blocks bad state)
  - [ ] SessionEnd fires (receipt written)
- [ ] Test in cloud (real cloud session, not mock)
- [ ] Measure metrics (time to first build, cost, etc.)
- [ ] Document results
- [ ] Celebrate 🎉

---

**Ready to execute?**

Let agents go. No human babysitting. Full autonomy.
