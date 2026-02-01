# Quick Fix Guide: rebar3 eunit CT SUITE Module Loading

## The Problem in 30 Seconds

```
Error: Module `tcps_andon_integration_SUITE' not found in project.
```

**Why:** CT SUITE modules are being compiled and discovered by EUnit, but EUnit can't execute them because they don't have the `test/0` or `test/1` functions that EUnit expects.

**What's happening:**
1. All `.erl` files in `test/` directories get compiled (including CT SUITE modules)
2. EUnit scans compiled `.beam` files in `_build/test/lib/*/ebin/`
3. EUnit tries to execute test functions on ALL `.beam` files
4. CT SUITE modules have `all/0` not `test/0` → EUnit fails

---

## Root Causes (3 Issues)

### Issue 1: Incomplete Exclusion Pattern
**File:** `/Users/sac/erlmcp/rebar.config` line 39

```erlang
{exclude, ".*_SUITE$"}  % Applied only at source discovery, not beam loading
```

**Problem:** Pattern is applied ONLY when discovering source files, not when scanning compiled beam files.

---

### Issue 2: Mixed Test Types in Same Directory
**Locations:**
- `/Users/sac/erlmcp/test/` - Mix of EUnit tests and CT SUITE modules
- `/Users/sac/erlmcp/apps/*/test/` - Mix of EUnit tests and CT SUITE modules

**Problem:** Rebar3 treats the entire `test/` directory as "tests to run with EUnit"

---

### Issue 3: Helper Modules Treated as Tests
**File:** `/Users/sac/erlmcp/apps/tcps_erlmcp/test/integration/tcps_ct_hooks.erl`

**Problem:** CT hook modules (infrastructure) are compiled and EUnit tries to execute them as tests.

---

## The Fix (Best Approach)

### Step 1: Organize Tests into Subdirectories

Create this structure:

```
test/
├── unit/                    ← EUnit tests only
│   ├── erlmcp_buffer_pool_tests.erl
│   ├── erlmcp_capability_cache_tests.erl
│   ├── erlmcp_codegen_tests.erl
│   ├── erlmcp_connection_pool_tests.erl
│   ├── erlmcp_hot_reload_tests.erl
│   ├── erlmcp_metrics_dashboard_tests.erl
│   ├── erlmcp_registry_distributed_tests.erl
│   └── erlmcp_trace_propagation_tests.erl
└── integration/             ← CT SUITE modules
    ├── auto_fix_SUITE.erl
    ├── hooks_integration_SUITE.erl
    └── quality_gates_SUITE.erl

apps/erlmcp_core/test/
├── unit/
│   └── ... (EUnit tests)
└── integration/
    ├── erlmcp_integration_SUITE.erl
    └── erlmcp_registry_dist_SUITE.erl

apps/erlmcp_observability/test/
├── unit/
│   └── ... (EUnit tests)
└── integration/
    └── erlmcp_observability_SUITE.erl

apps/erlmcp_transports/test/
├── unit/
│   └── ... (EUnit tests)
└── integration/
    ├── erlmcp_transport_behavior_SUITE.erl
    └── erlmcp_transport_integration_SUITE.erl

apps/tcps_erlmcp/test/
├── unit/
│   └── ... (EUnit tests)
└── integration/
    ├── erlmcp_pricing_poka_yoke_SUITE.erl
    ├── erlmcp_pricing_receipt_extended_SUITE.erl
    ├── erlmcp_receipt_cli_SUITE.erl
    ├── tcps_ct_hooks.erl
    ├── tcps_andon_integration_SUITE.erl
    ├── tcps_concurrent_SUITE.erl
    ├── tcps_heijunka_SUITE.erl
    ├── tcps_mcp_diataxis_SUITE.erl
    ├── tcps_performance_SUITE.erl
    ├── tcps_persistence_SUITE.erl
    ├── tcps_pipeline_SUITE.erl
    ├── tcps_quality_gates_SUITE.erl
    └── tcps_simulator_integration_SUITE.erl
```

### Step 2: Update Root rebar.config

**File:** `/Users/sac/erlmcp/rebar.config`

**Change:**
```erlang
%% OLD (Line 34):
{test_dirs, ["test"]}.

%% NEW:
{test_dirs, ["test/unit"]}.

%% Add CT configuration:
{ct_opts, [
    {ct_dir, ["test/integration"]},
    {auto_compile, true}
]}.
```

And remove or simplify the eunit exclusion pattern:

```erlang
%% OLD (Line 38-41):
{eunit_opts, [
    {exclude, ".*_SUITE$"},
    verbose
]}.

%% NEW (can remove exclusion now):
{eunit_opts, [
    verbose
]}.
```

### Step 3: Add Per-App Configurations (Optional but Recommended)

If apps have their own rebar.config, add:

**File:** `/Users/sac/erlmcp/apps/erlmcp_core/rebar.config`

```erlang
{test_dirs, ["test/unit"]}.
{ct_opts, [{ct_dir, ["test/integration"]}]}.
```

Repeat for all apps: `erlmcp_core`, `erlmcp_observability`, `erlmcp_transports`, `tcps_erlmcp`.

---

## Migration Steps

### Option A: Command-Line Migration (Bash Script)

```bash
#!/bin/bash

# Create unit/integration subdirectories
create_subdirs() {
    local test_dir=$1
    mkdir -p "$test_dir/unit"
    mkdir -p "$test_dir/integration"
}

# Move EUnit tests to unit/
move_eunit_tests() {
    local test_dir=$1
    find "$test_dir" -maxdepth 1 -name "*_tests.erl" -exec mv {} "$test_dir/unit/" \;
}

# Move CT SUITE and helper modules to integration/
move_ct_tests() {
    local test_dir=$1
    find "$test_dir" -maxdepth 1 -name "*_SUITE.erl" -exec mv {} "$test_dir/integration/" \;
    find "$test_dir" -maxdepth 1 -name "*_ct_hooks.erl" -exec mv {} "$test_dir/integration/" \;
}

# Top-level test directory
create_subdirs "/Users/sac/erlmcp/test"
move_eunit_tests "/Users/sac/erlmcp/test"
move_ct_tests "/Users/sac/erlmcp/test"

# Per-app test directories
for app_dir in /Users/sac/erlmcp/apps/*/test; do
    if [ -d "$app_dir" ]; then
        create_subdirs "$app_dir"
        move_eunit_tests "$app_dir"
        move_ct_tests "$app_dir"
    fi
done

# Handle integration/ subdirectories (move all CT modules)
for integration_dir in /Users/sac/erlmcp/apps/*/test/integration; do
    if [ -d "$integration_dir" ]; then
        # Move all .erl files from integration/ subfolder to parent integration/
        # (they should already be in integration/)
        true
    fi
done

echo "Migration complete!"
```

### Option B: Manual Migration

```bash
# Create directories
mkdir -p /Users/sac/erlmcp/test/unit
mkdir -p /Users/sac/erlmcp/test/integration

# Move EUnit tests
mv /Users/sac/erlmcp/test/erlmcp_buffer_pool_tests.erl /Users/sac/erlmcp/test/unit/
mv /Users/sac/erlmcp/test/erlmcp_capability_cache_tests.erl /Users/sac/erlmcp/test/unit/
# ... repeat for all 8 EUnit tests

# Move CT SUITE tests
mv /Users/sac/erlmcp/test/auto_fix_SUITE.erl /Users/sac/erlmcp/test/integration/
mv /Users/sac/erlmcp/test/hooks_integration_SUITE.erl /Users/sac/erlmcp/test/integration/
mv /Users/sac/erlmcp/test/quality_gates_SUITE.erl /Users/sac/erlmcp/test/integration/

# Repeat for apps/*/test directories
```

---

## Verification

### After Migration, Run:

```bash
# Should run only EUnit tests (unit/)
TERM=dumb rebar3 eunit

# Should succeed with NO "Module not found" errors
# Expected output: "X/X tests passed" (no CT modules)

# Then run CT tests
TERM=dumb rebar3 ct

# Should run only CT tests (integration/)
# Expected output: "X passed, 0 failed" from CT

# Full quality check
TERM=dumb rebar3 check
```

### Success Indicators:

- `rebar3 eunit` completes without errors
- `rebar3 ct` completes without errors
- No "Module not found" messages
- Test counts are accurate (EUnit runs unit tests only)

---

## Why This Fix Works

| Problem | How It's Solved |
|---------|-----------------|
| EUnit loads CT modules | CT modules moved to different directory not scanned by EUnit |
| Exclusion pattern doesn't work | No need for pattern - CT modules physically separated |
| Helper modules treated as tests | Helper modules moved to `test/integration/` (not scanned by EUnit) |
| Configuration confusion | Clear directory structure: `test/unit/` for EUnit, `test/integration/` for CT |
| Future maintenance issues | New developers understand: tests in `unit/` run with EUnit, tests in `integration/` run with CT |

---

## Alternative Fix (If Directory Migration Not Feasible)

Use more comprehensive exclusion patterns in rebar.config:

```erlang
{eunit_opts, [
    {exclude, ".*_SUITE\.erl$"},
    {exclude, ".*_SUITE$"},
    {exclude, ".*_ct_hooks\.erl$"},
    {exclude, ".*_ct_hooks$"},
    {exclude, ".*/integration/.*\.erl$"},
    verbose
]}.
```

**Limitations:**
- Still doesn't prevent compilation
- Beam files still created
- Only masks the issue at test discovery
- Not recommended (incomplete solution)

---

## Files to Modify

### Configuration Files
- `/Users/sac/erlmcp/rebar.config` - Update test_dirs and add ct_opts

### Per-App Configs (Optional)
- `/Users/sac/erlmcp/apps/erlmcp_core/rebar.config`
- `/Users/sac/erlmcp/apps/erlmcp_observability/rebar.config`
- `/Users/sac/erlmcp/apps/erlmcp_transports/rebar.config`
- `/Users/sac/erlmcp/apps/tcps_erlmcp/rebar.config`

### Directory Structure (File Moves)
- `/Users/sac/erlmcp/test/` - Create unit/ and integration/ subdirectories
- `/Users/sac/erlmcp/apps/*/test/` - Create unit/ and integration/ subdirectories

---

## Questions & Answers

**Q: Will this break any imports or references?**
A: No. Module imports use module names, not file paths. Moving `erlmcp_buffer_pool_tests.erl` to `test/unit/` doesn't change the module name `erlmcp_buffer_pool_tests`.

**Q: Do I need to update any includes?**
A: No. Common Test and EUnit still work the same way with the same module names.

**Q: What about CI/CD pipelines?**
A: Update CI commands:
- Old: `rebar3 eunit && rebar3 ct`
- New: Same (rebar3 will find tests in their subdirectories automatically)

**Q: Can I run both EUnit and CT with one command?**
A: Yes: `rebar3 check` or create an alias combining both.

**Q: What if I have a different test directory structure?**
A: Apply the same principle: EUnit tests in one directory (e.g., `unit/`), CT tests in another (e.g., `integration/`).

---

## Timeline

- **Quick fix:** 5-10 minutes (create directories, move files, update rebar.config)
- **Testing:** 2-3 minutes (verify EUnit and CT both work)
- **Total:** ~15 minutes

---

## Reference Documents

- Full Analysis: `/Users/sac/erlmcp/REBAR3_EUNIT_SUITE_ANALYSIS.md`
- Rebar3 Testing Guide: https://rebar3.org/docs/testing/
- EUnit Documentation: https://www.erlang.org/doc/man/eunit.html
- Common Test Documentation: https://www.erlang.org/doc/man/ct.html
