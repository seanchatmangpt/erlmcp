# Common Test Blocking Issues - Quick Reference
**Date**: 2026-01-29

## Critical Issues Requiring Immediate Fixes

### 1. Arithmetic Error in erlmcp_process_monitor (BLOCKING 7 tests)
**File**: `apps/erlmcp_observability/src/erlmcp_process_monitor.erl:282`
**Error**: `{badarith, [{erlang,'div',[858993459.2,3072]}, ...]}`

**Problem**: Using float result in integer division
```erlang
%% WRONG - / returns float, div requires integer
MemoryCapacity = (TargetMemory * (1.0 - SafetyMargin)) div PerConnOverhead,
```

**Solution**:
```erlang
%% RIGHT - Use integer arithmetic
MemoryCapacity = trunc((TargetMemory * (100 - SafetyMargin)) div (PerConnOverhead * 100)),
```

**Tests Blocked**:
- erlmcp_transport_integration_SUITE (7 tests)

---

### 2. gproc Application Fails to Start (BLOCKING 4 tests)
**Error**: `{undef, [{gproc,start_link,[],[]}, ...]}`

**Problem**: gproc not starting correctly
**Check**:
```bash
# Verify gproc is in dependencies
grep gproc apps/erlmcp_core/rebar.config

# Rebuild
rebar3 clean
rm -rf _build
rebar3 compile
```

**Tests Blocked**:
- erlmcp_observability_SUITE (4 tests)

---

### 3. erlmcp.app File Not Found (BLOCKING 21 tests)
**File**: `apps/erlmcp_core/test/erlmcp_integration_SUITE.erl:148`
**Error**: `{error, {"no such file or directory", "erlmcp.app"}}`

**Problem**: Application startup using wrong app name
```erlang
%% WRONG - erlmcp app doesn't exist (should be erlmcp_core)
case application:start(erlmcp) of
```

**Solution**:
```erlang
%% RIGHT - Use erlmcp_core or ensure_all_started
case application:ensure_all_started(erlmcp_core) of
```

**Tests Blocked**:
- erlmcp_integration_SUITE (21 tests)

---

## Secondary Issues

### 4. Missing Beam File in Cover Compilation
**Error**: `{file_error, "erlmcp_pricing_util.beam", enoent}`
**Impact**: erlmcp_registry_dist_SUITE blocked
**Fix**: Exclude from cover or add module to build

### 5. Unbound Variable in Test
**File**: `tests/erlmcp_monitor_test.erl:83-84`
**Error**: `variable 'Components' is unbound`
**Fix**: Define variable or fix test logic

### 6. Undefined Record in Test
**File**: `tests/erlmcp_trace_analyzer_tests.erl:99`
**Error**: `record trace_analysis undefined`
**Fix**: Add record definition or fix test

---

## Quick Fix Commands

```bash
# 1. Fix process monitor (manual edit required)
# Edit apps/erlmcp_observability/src/erlmcp_process_monitor.erl line 282

# 2. Rebuild everything
rebar3 clean
rm -rf _build/test
rebar3 compile

# 3. Test gproc
erl -noshell -eval "application:ensure_all_started(gproc), halt()."

# 4. Fix integration suite (manual edit required)
# Edit apps/erlmcp_core/test/erlmcp_integration_SUITE.erl line 148

# 5. Re-run tests
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_integration_SUITE
```

---

## Test Statistics

- **Total Suites**: 7
- **Passed**: 0 (0%)
- **Failed**: 7 (100%)
- **Total Tests**: 36+
- **Blocked**: 32 (due to init_per_suite failures)
- **Direct Failures**: 4

---

**Priority**: Fix issues 1-3 immediately to unblock 32 tests.
