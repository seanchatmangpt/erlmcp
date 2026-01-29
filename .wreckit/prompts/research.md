# Research Phase - TCPS Quality Standards

You are tasked with conducting comprehensive research for **Toyota Code Production System (TCPS)** manufacturing quality. Apply **Lean Six Sigma** principles: **99.99966% defect-free delivery** (3.4 defects per million opportunities).

## TCPS Manufacturing Principles

**MANDATORY QUALITY STANDARDS:**

1. **Jidoka (自働化)** - Built-in quality, stop-the-line authority
   - Every defect MUST stop production
   - No "assume pass" - verify everything
   - Fail-closed behavior (A ≠ μ(O) ⇒ REFUSE)

2. **Poka-yoke (ポカヨケ)** - Mistake-proofing
   - Design errors out of the system
   - Compile-time type safety (Dialyzer)
   - Runtime validation (contracts, guards)

3. **Kaizen (改善)** - Continuous improvement
   - Measure everything (metrics, traces, logs)
   - Root cause analysis (5 Whys)
   - Receipt-based audit trail

4. **Heijunka (平準化)** - Production leveling
   - Small, incremental changes
   - Testable phases
   - No big-bang rewrites

5. **Andon (行灯)** - Visible problem signaling
   - Quality gate failures are visible
   - No silent failures
   - Clear error messages

## Item Details
- **ID:** {{id}}
- **Title:** {{title}}
- **Section:** {{section}}
- **Overview:** {{overview}}
- **Working Directory:** {{item_path}}
- **Language:** Erlang/OTP 25+
- **Build:** rebar3
- **Test:** EUnit, Common Test, Proper

## Research Process

### Step 1: Initial Analysis

1. **Understand the manufacturing requirement:**
   - What SPECIFIC quality gate is this addressing?
   - What is the CRITICAL acceptance criterion?
   - What are the NON-NEGOTIABLE constraints?

2. **Read relevant files COMPLETELY:**
   - Do NOT skim - read every line
   - Start with CLAUDE.md for project rules
   - Read the actual source code files
   - Read existing tests (if any)

3. **Identify OTP patterns:**
   - Is this a gen_server? supervisor? application?
   - What behavior modules must be implemented?
   - What are the supervision tree requirements?

### Step 2: Deep Investigation

1. **Explore the Erlang/OTP codebase:**
   - Use `Glob` to find ALL related .erl files
   - Use `Grep` to find function references
   - Use `Read` to examine COMPLETE files

2. **Document what you find:**
   - Include specific file paths and LINE NUMBERS
   - Note OTP patterns (gen_server callbacks, supervisor children)
   - Identify existing test patterns (Chicago School TDD)
   - Find quality benchmarks (performance, coverage)

3. **Understand TCPS integration:**
   - Is this a quality gate? (compilation, tests, coverage, dialyzer, xref)
   - Does this generate receipts? (SHA-256 hash chain)
   - Does this trigger Andon events? (quality failures)
   - Is this Poka-yoke? (error-proofing)

### Step 3: Quality Gate Analysis

**For EACH quality gate, document:**

1. **Current State:**
   - What is the ACTUAL measurement? (coverage %, test pass rate, warning count)
   - What is the TARGET? (≥80% coverage, 100% pass rate, 0 warnings)
   - What is the GAP? (percentage points, specific failures)

2. **Root Cause Analysis (5 Whys):**
   - Why is the quality gate failing?
   - What is the ACTUAL root cause? (not symptoms)
   - What MUST be fixed to pass?

3. **Manufacturing Constraints:**
   - What are the ZERO TOLERANCE items? (compilation errors, type safety)
   - What are the QUALITY THRESHOLDS? (80% coverage, 100% pass rate)
   - What are the PERFORMANCE BASELINES? (2.52M msg/sec core_ops)

### Step 4: Risk Assessment

**Identify RISKS with TCPS perspective:**

| Risk | Severity (P0/P1/P2/P3) | Impact | Mitigation |
|------|------------------------|--------|------------|
| [Risk 1] | [P0=Critical, P1=High, P2=Medium, P3=Low] | [What breaks?] | [How to prevent?] |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix immediately
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

## Output

Create a file at: `{{item_path}}/research.md`

Use this EXACT structure:

```markdown
# Research: {{title}}

**Date**: [Current date]
**Item**: {{id}}
**Section**: {{section}}
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
{{overview}}

### Quality Gate Status
- **Gate Type**: [Compilation/Test/Coverage/Dialyzer/Xref/Performance/Security]
- **Current State**: [ACTUAL measurement with numbers]
- **Target State**: [REQUIRED threshold]
- **Gap**: [Quantified gap]

## Summary
[2-3 paragraphs answering:
1. What needs to be done (manufacturing objective)
2. How to do it (technical approach)
3. Why this approach (TCPS justification)]

## Current State Analysis

### Existing Implementation
- **Files**: [Complete list with paths and line numbers]
- **Patterns**: [OTP patterns observed]
- **Tests**: [Existing test coverage %]
- **Quality**: [Current gate status - pass/fail with data]

### Key Files
- `path/to/module.erl:123-456` - [Specific function/pattern]
- `path/to/test.erl:789` - [Test pattern to follow]
- `docs/reference.md:45` - [Documentation reference]

### OTP Patterns Observed
- **Behavior**: [gen_server/supervisor/application/gen_fsm]
- **Supervision**: [one_for_one/one_for_all/simple_one_for_one]
- **Process Pattern**: [Process-per-connection/Pool/Registry]
- **Test Pattern**: [Chicago School TDD - real processes, no mocks]

## Technical Considerations

### Dependencies
- **Internal Modules**: [Specific modules with file paths]
- **External Libraries**: [rebar.config deps with versions]
- **OTP Applications**: [kernel/stdlib/sasl/mnesia/etc]

### TCPS Quality Gates to Pass
- [ ] Compilation: 0 errors
- [ ] EUnit: 100% pass rate
- [ ] Common Test: 100% pass rate
- [ ] Coverage: ≥80%
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined function calls
- [ ] Performance: <10% regression from baseline

### Patterns to Follow
- **Gen Server Pattern**: [Reference file:line]
- **Test Pattern**: [Reference test file:line]
- **Error Handling**: [Error handling pattern]
- **Type Specs**: [Dialyzer spec pattern]

## Root Cause Analysis (5 Whys)

**Problem**: [Quality gate failure]

1. Why? [First-level reason]
2. Why? [Second-level reason]
3. Why? [Third-level reason]
4. Why? [Fourth-level reason]
5. Why? [ROOT CAUSE]

**Solution**: [Fix root cause, not symptoms]

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| [Specific risk] | P0/P1/P2/P3 | [What breaks] | [How to prevent] |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria
2. **Pseudocode** - Algorithm design BEFORE coding
3. **Architecture** - Integration points and dependencies
4. **Refinement** - Chicago School TDD (tests FIRST)
5. **Completion** - All quality gates passing

**Implementation Strategy:**
[High-level approach following TCPS phases]

**Quality Validation:**
- Automated: [rebar3 commands to run]
- Manual: [What to verify manually]
- Metrics: [What to measure]

## Open Questions
[NONE - research until all questions answered]

## Manufacturing Checklist
- [ ] Root cause identified (not symptoms)
- [ ] Quality gates defined (specific thresholds)
- [ ] OTP patterns understood (behaviors, supervision)
- [ ] Test strategy clear (Chicago School TDD)
- [ ] Risk assessment complete (severity P0-P3)
- [ ] No open questions (all research complete)
```

## TCPS Research Guidelines

**MANDATORY QUALITY STANDARDS:**

1. **Be COMPLETE:**
   - Read ENTIRE files, not snippets
   - Use `Read` tool for every file mentioned
   - Verify with actual code, not assumptions

2. **Be SKEPTICAL:**
   - Question ALL assumptions
   - Verify patterns actually exist
   - Look for edge cases and error paths
   - Don't trust comments - trust code

3. **Be QUANTIFIED:**
   - Use SPECIFIC numbers (coverage %, test counts, timing)
   - Measure ACTUAL state, not guesses
   - Quantify the gap (current → target)

4. **Apply TCPS PRINCIPLES:**
   - **Jidoka**: Where can we add stop-the-line quality checks?
   - **Poka-yoke**: How can we error-proof this?
   - **Kaizen**: What metrics should we track?
   - **Heijunka**: How can we break this into small phases?
   - **Andon**: How will failures be visible?

5. **Focus on ZERO DEFECTS:**
   - 99.99966% defect-free delivery
   - 3.4 defects per million opportunities
   - NO tolerance for "good enough"
   - EVERY gate must pass

## Completion

When you have:
1. Read ALL relevant files COMPLETELY
2. Identified the ACTUAL root cause (not symptoms)
3. Documented OTP patterns with file:line references
4. Quantified the quality gate gap
5. Assessed all risks with severity (P0-P3)
6. Created `{{item_path}}/research.md` with the EXACT structure above
7. ZERO open questions (all research complete)

Output the following signal:
{{completion_signal}}

**REMEMBER**: This is manufacturing. We ship ZERO DEFECTS. Research thoroughly, measure everything, verify with actual code.
