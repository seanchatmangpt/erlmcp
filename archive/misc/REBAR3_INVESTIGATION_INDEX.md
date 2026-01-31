# REBAR3 EUnit Investigation - Document Index

## Investigation Complete ✅

This investigation examined why `rebar3 eunit` incorrectly loads Common Test SUITE modules, causing build failures with "Module not found" errors.

---

## Documents Generated

### 1. REBAR3_EUNIT_FIX_GUIDE.md (START HERE - ACTIONABLE)
**Purpose:** Step-by-step guide to fix the issue

**Contents:**
- 30-second problem summary
- Root causes explained (3 issues)
- Best fix approach (directory separation)
- Step-by-step migration guide
- Bash script for automation
- Verification commands
- Q&A section

**When to Use:** You want to implement the fix immediately

**Reading Time:** 10-15 minutes

---

### 2. REBAR3_EUNIT_SUITE_ANALYSIS.md (COMPREHENSIVE)
**Purpose:** Deep technical analysis and documentation

**Contents:**
- Executive summary
- Problem analysis (detailed)
- Root causes with code examples
- Rebar3 EUnit discovery algorithm explanation
- Why CT modules fail with EUnit
- File reference map (20+ files)
- 3 fix options (best to worst)
- Metrology and technical details
- Summary table

**When to Use:** You want to understand the issue deeply

**Reading Time:** 20-30 minutes

---

### 3. REBAR3_INVESTIGATION_REFERENCE.md (TECHNICAL REFERENCE)
**Purpose:** Code snippets and technical details

**Contents:**
- Error analysis
- 4-phase processing explanation with code
- Why exclusion patterns fail
- Before/after file organization
- Helper module issue details
- Configuration file analysis
- Verification commands
- Migration script (ready to run)
- Impact summary
- Key takeaways
- Timeline

**When to Use:** You need code examples or specific technical details

**Reading Time:** 15-20 minutes

---

## Quick Navigation

### I want to...

**Fix the issue immediately**
→ Read: `REBAR3_EUNIT_FIX_GUIDE.md`
→ Section: "The Fix" (Step 1-3)

**Understand what's wrong**
→ Read: `REBAR3_EUNIT_SUITE_ANALYSIS.md`
→ Section: "Problem Analysis"

**See code examples**
→ Read: `REBAR3_INVESTIGATION_REFERENCE.md`
→ Section: "Root Cause: Rebar3 Processing Phases"

**Know which files to change**
→ Read: `REBAR3_INVESTIGATION_REFERENCE.md`
→ Section: "Rebar3 Configuration Files"

**Run the migration script**
→ Read: `REBAR3_INVESTIGATION_REFERENCE.md`
→ Section: "Migration Script"

**Verify the fix worked**
→ Read: `REBAR3_EUNIT_FIX_GUIDE.md`
→ Section: "Verification"

---

## Key Findings Summary

### Root Cause
Rebar3's `{exclude, ".*_SUITE$"}` pattern is applied ONLY during source discovery, not during compilation or test execution. CT modules are compiled to beam files and discovered by EUnit, which fails because it can't find the test interface.

### The Problem
- **20+ CT and helper modules** mixed with EUnit modules in same directories
- **Exclusion pattern ineffective** at runtime
- **Helper module tcps_ct_hooks** treated as test (it's infrastructure)
- **EUnit can't execute CT modules** (different test interface)

### The Solution
Organize tests into separate `unit/` and `integration/` subdirectories:
- EUnit scans `test/unit/` only
- CT scans `test/integration/` only
- No pattern matching needed
- Clear, maintainable structure

### Impact
- **Fix Time:** ~15 minutes
- **Severity:** CRITICAL (blocks build)
- **Files Affected:** 20+ test modules + 4 rebar.config files

---

## File Organization Changes

### Before Fix
```
test/
├── erlmcp_buffer_pool_tests.erl (EUnit)
├── auto_fix_SUITE.erl (CT) ← PROBLEM
├── hooks_integration_SUITE.erl (CT) ← PROBLEM
└── quality_gates_SUITE.erl (CT) ← PROBLEM

apps/tcps_erlmcp/test/
├── erlmcp_pricing_receipt_extended_SUITE.erl (CT) ← PROBLEM
├── tcps_ct_hooks.erl (Helper) ← PROBLEM
└── integration/
    └── 9 CT SUITE modules ← PROBLEM
```

### After Fix
```
test/
├── unit/
│   ├── erlmcp_buffer_pool_tests.erl (EUnit) ✅
│   └── ... (8 more)
└── integration/
    ├── auto_fix_SUITE.erl (CT) ✅
    ├── hooks_integration_SUITE.erl (CT) ✅
    └── quality_gates_SUITE.erl (CT) ✅

apps/tcps_erlmcp/test/
├── unit/
│   ├── ... (EUnit tests)
└── integration/
    ├── erlmcp_pricing_receipt_extended_SUITE.erl (CT) ✅
    ├── tcps_ct_hooks.erl (Helper) ✅
    └── ... (9 CT SUITE modules) ✅
```

---

## Configuration Changes

### Rebar.config

**Before:**
```erlang
{test_dirs, ["test"]}.
{eunit_opts, [
    {exclude, ".*_SUITE$"},
    verbose
]}.
```

**After:**
```erlang
{test_dirs, ["test/unit"]}.
{ct_opts, [{ct_dir, ["test/integration"]}]}.
{eunit_opts, [verbose]}.
```

---

## Error Messages Explained

| Error | Root Cause | Solution |
|-------|-----------|----------|
| `Module 'tcps_andon_integration_SUITE' not found` | EUnit tries to execute CT module (no test interface) | Move to test/integration/ |
| `Module 'tcps_ct_hooks' not found` | Helper module treated as test | Move to test/integration/ |
| `Module 'hooks_integration_SUITE' not found` | CT SUITE in same directory as EUnit | Move to test/integration/ |

---

## Rebar3 Processing Phases

```
Phase 1: Source Discovery
  ✅ Exclusion pattern applied
  Files marked "exclude"

Phase 2: Compilation
  ❌ Pattern ignored, ALL files compiled
  
Phase 3: Beam Discovery
  ❌ Pattern not re-applied, ALL beams found
  
Phase 4: Test Execution
  ❌ EUnit tries to execute CT modules
  FAILURE: "Module not found"
```

**Why this happens:**
- Source discovery applies patterns
- Compilation ignores patterns
- Beam discovery doesn't know about patterns
- Test execution has no exclusion logic

**How to fix it:**
- Use DIRECTORY SEPARATION instead of patterns
- EUnit only sees `test/unit/`
- CT only sees `test/integration/`

---

## Affected Files

### Test Directories
- `/Users/sac/erlmcp/test/` (11 files - mixed)
- `/Users/sac/erlmcp/apps/erlmcp_core/test/` (2 CT SUITE)
- `/Users/sac/erlmcp/apps/erlmcp_observability/test/` (1 CT SUITE)
- `/Users/sac/erlmcp/apps/erlmcp_transports/test/` (2 CT SUITE)
- `/Users/sac/erlmcp/apps/tcps_erlmcp/test/` (3 CT SUITE + helper)
- `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/` (9 CT SUITE)

### Configuration Files
- `/Users/sac/erlmcp/rebar.config` (Line 34, 38-41)
- `/Users/sac/erlmcp/apps/erlmcp_core/rebar.config` (add config)
- `/Users/sac/erlmcp/apps/erlmcp_observability/rebar.config` (add config)
- `/Users/sac/erlmcp/apps/erlmcp_transports/rebar.config` (add config)
- `/Users/sac/erlmcp/apps/tcps_erlmcp/rebar.config` (add config)

### Key Helper Module
- `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/tcps_ct_hooks.erl`

---

## Implementation Checklist

- [ ] **Read:** REBAR3_EUNIT_FIX_GUIDE.md (Section: "The Fix")
- [ ] **Create:** test/unit/ and test/integration/ directories
- [ ] **Migrate:** Move EUnit tests to test/unit/
- [ ] **Migrate:** Move CT tests to test/integration/
- [ ] **Repeat:** For each apps/*/test/ directory
- [ ] **Update:** /Users/sac/erlmcp/rebar.config (test_dirs and ct_opts)
- [ ] **Update:** Per-app rebar.configs (optional but recommended)
- [ ] **Verify:** Run `rebar3 eunit` (should pass)
- [ ] **Verify:** Run `rebar3 ct` (should pass)
- [ ] **Verify:** Run `rebar3 check` (should pass)
- [ ] **Update:** CI/CD pipeline (if needed)
- [ ] **Document:** Test structure for team

---

## Verification Commands

After implementing the fix:

```bash
# EUnit should pass (unit tests only)
TERM=dumb rebar3 eunit

# CT should pass (integration tests only)
TERM=dumb rebar3 ct

# Full check should pass
TERM=dumb rebar3 check
```

**Expected Results:**
- No "Module not found" errors
- All tests pass
- Clear test output showing which tests ran

---

## Timeline

| Step | Duration | Task |
|------|----------|------|
| 1. Create directories | 2 min | mkdir unit/ integration/ |
| 2. Move files | 5 min | Move tests to subdirectories |
| 3. Update config | 2 min | Update rebar.config |
| 4. Verify | 3 min | Run tests |
| **Total** | ~12 min | Complete fix |

---

## Document Relationship

```
REBAR3_INVESTIGATION_INDEX.md (this file)
├── REBAR3_EUNIT_FIX_GUIDE.md (actionable steps)
├── REBAR3_EUNIT_SUITE_ANALYSIS.md (deep analysis)
└── REBAR3_INVESTIGATION_REFERENCE.md (technical details)
```

---

## FAQ

**Q: Do I need to read all three documents?**
A: No. Read REBAR3_EUNIT_FIX_GUIDE.md to implement the fix. Read others if you want more details.

**Q: Will this break anything?**
A: No. Moving test files doesn't change module names or behavior. Only directory structure changes.

**Q: Can I run both EUnit and CT with one command?**
A: Yes. Use `rebar3 check` which runs both.

**Q: How long does the fix take?**
A: About 15 minutes (create directories, move files, update config, verify).

**Q: What if I have custom test directories?**
A: Apply the same principle: separate EUnit tests from CT tests.

---

## Next Steps

1. **Start Here:** Read `REBAR3_EUNIT_FIX_GUIDE.md`
2. **Implement:** Follow the step-by-step migration guide
3. **Verify:** Run verification commands
4. **Document:** Update team documentation (optional)

---

**Investigation Status:** COMPLETE ✅

For questions about specific findings, refer to the appropriate document above.
