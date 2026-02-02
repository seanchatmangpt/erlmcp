# OTP Compliance for erlmcp-flow - Summary

**Version:** 1.0.0
**Date:** 2026-02-01
**Status:** ACTIVE

---

## Overview

This document summarizes the OTP compliance requirements and tooling for erlmcp-flow development.

### Key Principles

1. **All stateful components MUST be gen_server or gen_statem**
2. **init/1 MUST NEVER block** (< 100ms)
3. **All processes MUST be supervised** (no bare spawn)
4. **Messages MUST be typed** (records or tagged tuples)
5. **Timeouts MUST be ≥ 5000ms**
6. **Let-it-crash** (no defensive programming for bugs)
7. **Real processes in tests** (no mocks)
8. **Observable behavior testing** (no state inspection)

---

## Documentation Structure

### 1. Complete Checklist (Comprehensive)

**File:** `docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md`

**Contents:**
- Detailed explanations of each OTP principle
- Code patterns (correct vs incorrect)
- Real examples from erlmcp codebase
- Complete review checklist
- Anti-patterns and fixes
- Testing guidelines

**When to use:**
- Learning OTP patterns
- Implementing new gen_server/supervisor
- Code review deep dive
- Troubleshooting OTP violations

### 2. Quick Reference Card (Day-to-Day)

**File:** `docs/OTP_COMPLIANCE_QUICK_REFERENCE.md`

**Contents:**
- TL;DR checklist
- Code templates (copy-paste ready)
- Quick pattern reference
- Common violations table
- Emergency fixes

**When to use:**
- Daily development
- Quick lookup during coding
- PR description template
- Pre-commit checklist

### 3. Automated Scanner

**File:** `scripts/otp-compliance-scan.sh`

**Checks:**
1. Unsupervised spawn patterns
2. Blocking init/1 operations
3. Missing gen_server callbacks
4. Short timeouts (< 5000ms)
5. Mock usage in tests
6. State inspection (sys:get_status)
7. Missing type specs
8. Improper supervisor child specs
9. Large test files (> 500 lines)

**Usage:**
```bash
# Run manually
./scripts/otp-compliance-scan.sh

# CI integration
make validate-otp
```

---

## Integration Points

### Pre-Commit Hook

OTP compliance checks run automatically on commit via:

**File:** `.git/hooks/pre-commit`

```bash
# Check for OTP violations
./scripts/otp-compliance-scan.sh || exit 1
```

**Install:**
```bash
./scripts/install-quality-hooks.sh
```

### CI/CD Pipeline

**File:** `.github/workflows/quality-gate.yml`

```yaml
- name: OTP Compliance Check
  run: ./scripts/otp-compliance-scan.sh
```

### Make Targets

```bash
make validate-otp          # Run OTP compliance scanner
make check-supervision     # Verify supervision trees
make check-blocking-init   # Find blocking init/1
```

---

## Quality Standards Integration

OTP compliance is part of erlmcp-flow's comprehensive quality system:

```
ERLMCP_FLOW_QUALITY_STANDARDS.md
├── 1. Chicago School TDD ──────┐
├── 2. OTP Design Patterns ─────┼──> ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md
├── 3. Joe Armstrong Principles │
├── 4. Quality Checklist ────────┘
├── 5. CI/CD Integration
└── 6. Quality Metrics
```

**Relationship:**
- Quality Standards: Overall framework and gates
- OTP Compliance: Detailed OTP-specific requirements
- Quick Reference: Developer day-to-day guide
- Scanner: Automated enforcement

---

## Code Review Workflow

### Step 1: Developer Self-Check

Before creating PR:

```bash
# Run OTP compliance scanner
./scripts/otp-compliance-scan.sh

# Run full quality gates
make validate
```

### Step 2: PR Description Template

Include in PR description:

```markdown
## OTP Compliance Checklist

### gen_server/gen_statem
- [ ] All callbacks exported
- [ ] Non-blocking init/1
- [ ] Proper message handling

### Supervision
- [ ] All processes supervised
- [ ] Restart strategies defined

### Testing
- [ ] Real processes (no mocks)
- [ ] Observable behavior testing

### Documentation
- [ ] Type specs complete
- [ ] Module docs updated
```

(Copy from `docs/OTP_COMPLIANCE_QUICK_REFERENCE.md`)

### Step 3: Code Reviewer Check

Use detailed checklist from `docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md` Section 7.

### Step 4: CI Validation

GitHub Actions runs:
1. Compilation
2. OTP compliance scan
3. Chicago TDD scan
4. Full test suite
5. Coverage check
6. Dialyzer + Xref

---

## Common Workflows

### 1. New gen_server Implementation

```bash
# 1. Copy template from quick reference
cat docs/OTP_COMPLIANCE_QUICK_REFERENCE.md | grep -A 50 "gen_server Template"

# 2. Implement module
vim apps/erlmcp_core/src/my_server.erl

# 3. Run compliance check
./scripts/otp-compliance-scan.sh

# 4. Run tests
rebar3 eunit --module=my_server_tests

# 5. Full validation
make validate
```

### 2. Fixing OTP Violations

```bash
# 1. Run scanner to identify violations
./scripts/otp-compliance-scan.sh

# 2. Look up fix in quick reference
cat docs/OTP_COMPLIANCE_QUICK_REFERENCE.md | grep -A 10 "Blocking init/1"

# 3. Apply fix from checklist
cat docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md | grep -A 30 "Non-Blocking init"

# 4. Re-run scanner
./scripts/otp-compliance-scan.sh

# 5. Verify tests pass
rebar3 eunit
```

### 3. Code Review Process

```bash
# 1. Reviewer: Run scanner on PR branch
git checkout pr-branch
./scripts/otp-compliance-scan.sh

# 2. Reviewer: Use detailed checklist
cat docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md | grep -A 100 "Code Review Checklist"

# 3. Developer: Address violations
# ... fix code ...

# 4. Developer: Re-validate
make validate

# 5. Merge when green
```

---

## Enforcement Levels

### BLOCKING (Must Fix Before Merge)

- Unsupervised spawn
- Blocking init/1
- Missing gen_server callbacks
- Mock usage in tests
- State inspection in tests
- Short timeouts (< 5000ms)
- No type specs for exported functions

**Enforced by:** CI + pre-commit hooks

### WARNING (Should Fix, Non-Blocking)

- Large test files (> 500 lines)
- Old-style supervisor child specs
- spawn_link without supervision
- Missing documentation

**Enforced by:** Code review

---

## Metrics & Monitoring

### Compliance Metrics

Track over time:

```bash
# Generate compliance report
./scripts/otp-compliance-scan.sh > otp-compliance-report.txt

# Metrics:
# - Violations per module
# - Warnings per module
# - Compliance percentage
# - Trend over time
```

### Quality Dashboard

```bash
# View quality metrics including OTP compliance
make metrics-snapshot
make metrics-report
```

---

## Training & Resources

### Internal Resources

1. **Comprehensive Guide:**
   - File: `docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md`
   - Sections: 10
   - Code examples: 50+
   - Review checklist: Complete

2. **Quick Reference:**
   - File: `docs/OTP_COMPLIANCE_QUICK_REFERENCE.md`
   - Templates: 7
   - Anti-patterns: 8
   - Quick fixes: 12

3. **Codebase Examples:**
   - `apps/erlmcp_core/src/erlmcp_server.erl` - gen_server
   - `apps/erlmcp_core/src/erlmcp_sup.erl` - supervisor
   - `apps/erlmcp_core/test/erlmcp_server_tests.erl` - testing

### External Resources

1. [Erlang OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
2. [Learn You Some Erlang - OTP](http://learnyousomeerlang.com/what-is-otp)
3. [Joe Armstrong's Thesis](https://erlang.org/download/armstrong_thesis_2003.pdf)
4. [Designing for Scalability with Erlang/OTP](https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/)

---

## Roadmap

### Phase 1: Foundation (COMPLETE)
- [x] OTP compliance checklist
- [x] Quick reference card
- [x] Automated scanner
- [x] CI integration

### Phase 2: Enhancement (Planned)
- [ ] Dialyzer integration for OTP pattern detection
- [ ] Custom Xref callbacks for supervision validation
- [ ] Metrics dashboard for compliance trends
- [ ] Auto-fix suggestions (e.g., convert blocking init)

### Phase 3: Advanced (Future)
- [ ] Property-based testing for OTP invariants
- [ ] Supervision tree visualization
- [ ] Performance profiling for gen_server
- [ ] Hot code upgrade testing automation

---

## FAQ

### Q: What if I need to violate OTP rules?

**A:** Document the exception and get approval. Add suppression comment:

```erlang
%% OTP_COMPLIANCE_EXCEPTION: Reason for violation
%% Approved-by: @reviewer
%% Date: 2026-02-01
spawn(fun() -> special_case() end).
```

### Q: How do I fix blocking init/1?

**A:** See Section 1.2 in `docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md` or quick reference:

```erlang
%% Use {continue, ...} for async init
init(Opts) ->
    {ok, #state{opts = Opts}, {continue, initialize}}.

handle_continue(initialize, State) ->
    %% Safe to block here
    {ok, Conn} = connect_db(State#state.opts),
    {noreply, State#state{conn = Conn}}.
```

### Q: Why no mocks in tests?

**A:** Chicago School TDD (Section 5 of checklist). Use real processes to test real behavior.

### Q: Can I use sys:get_status for debugging?

**A:** Yes, in development/REPL. NO in automated tests.

---

## Support

**Questions?**
- Read: `docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md`
- Quick help: `docs/OTP_COMPLIANCE_QUICK_REFERENCE.md`
- Run scanner: `./scripts/otp-compliance-scan.sh`
- Ask team: #erlmcp-flow channel

**Found a bug in scanner?**
- File: `scripts/otp-compliance-scan.sh`
- Report: Open issue with "otp-compliance" label

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2026-02-01 | Initial release |

---

**Maintained by:** erlmcp-flow Core Team
**Review Cycle:** Quarterly
**Last Updated:** 2026-02-01

**CODE LIKE A JOE ARMSTRONG AGI SWARM!!!**
