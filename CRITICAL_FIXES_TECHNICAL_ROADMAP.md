# Critical Fixes Technical Roadmap

**Prepared for**: erlmcp v0.7.0 Production Release
**Date**: 2026-01-27
**Priority**: URGENT - Must complete before GA

---

## Critical Issue #1: Dialyzer Cannot Complete

### Problem

```
===> Error in dialyzing apps: Analysis failed with error:
Could not scan the following file(s):
  Could not get Core Erlang code for: erlmcp_progress.beam
  Could not get Core Erlang code for: erlmcp_localhost_binding_tests.beam
  Could not get Core Erlang code for: erlmcp_localhost_binding.beam
  Could not get Core Erlang code for: erlmcp_gap38_timeout_validation_tests.beam
  Could not get Core Erlang code for: gap32_verification.beam
```

### Root Cause

These 5 modules compiled without `debug_info` flag, preventing Dialyzer from extracting Core Erlang code.

### Solution

#### Step 1: Identify source files
```bash
# Find source files for affected beams
find src test -name "*progress*" -o -name "*localhost_binding*" -o -name "*gap32*"
```

#### Step 2: Check compilation flags
```bash
# Current rebar.config includes debug_info globally:
# {erl_opts, [debug_info, ...]}
```

#### Step 3: Force rebuild with debug_info
```bash
rebar3 clean
rebar3 compile
```

#### Step 4: Verify debug_info present
```bash
# Check if beam has debug_info
erlang:beam_lib:chunks("ebin/erlmcp_progress.beam", [debug_info])
```

#### Step 5: Run dialyzer again
```bash
rebar3 dialyzer
```

### Expected Outcome

- ✓ Dialyzer completes successfully
- ✓ Type errors identified and fixed
- ✓ Confidence in type safety restored

### Estimated Time

**2 hours** (including investigation and any type error fixes)

---

## Critical Issue #2: Tests Cannot Execute

### Problem

```
===> Error Running EUnit Tests:
  Module `tcps_andon_integration_SUITE' not found in project.
  Module `tcps_concurrent_SUITE' not found in project.
  Module `tcps_heijunka_SUITE' not found in project.
  Module `tcps_mcp_diataxis_SUITE' not found in project.
  Module `tcps_performance_SUITE' not found in project.
  Module `tcps_persistence_SUITE' not found in project.
  Module `tcps_pipeline_SUITE' not found in project.
  Module `tcps_quality_gates_SUITE' not found in project.
  Module `tcps_simulator_integration_SUITE' not found in project.
  Module `tcps_ct_hooks' not found in project.
  Module `tcps_mock_services' not found in project.
  Module `tcps_test_utils' not found in project.
  Module `tcps_rebar3_providers_tests' not found in project.
```

### Root Cause

13 test modules are referenced in rebar.config or test discovery but files don't exist in test/ directory.

### Investigation Steps

```bash
# Find which files reference these missing modules
grep -r "tcps_andon_integration_SUITE" . --include="*.config" --include="*.erl"
grep -r "tcps_concurrent_SUITE" . --include="*.config" --include="*.erl"

# List all TCPS test files in test/
ls test/tcps_*.erl | wc -l

# List what TCPS test modules exist
ls test/tcps_*.erl | sort
```

### Solution Options

#### Option A: Find Missing Test Files
```bash
# Search entire git history
git log --diff-filter=D --summary | grep "create mode.*tcps_.*SUITE"

# Check if files deleted recently
git log --oneline test/ | head -20
```

#### Option B: Implement Missing Tests
If files were deleted intentionally:

1. Create stub SUITE files for missing modules:
   ```erlang
   -module(tcps_andon_integration_SUITE).
   -include_lib("common_test/include/ct.hrl").

   all() -> [].
   ```

2. Or implement actual tests if scope allows

#### Option C: Remove References
If tests no longer needed:

1. Remove from rebar.config
2. Remove from .eunit file
3. Comment out in test orchestrator modules

### Recommended Action

**Option A First**: Search git history for deleted files
- If found: Restore from git
- If not: Implement Option B (stub files) or Option C (remove references)

### Expected Outcome

- ✓ All test modules present in test/ directory
- ✓ Test suite executes completely
- ✓ Pass/fail rates measured
- ✓ Coverage percentage calculated

### Estimated Time

**3-4 hours** (investigation + implementation/restoration)

---

## Critical Issue #3: Module Size Violations

### Problem

37 of 159 modules (23%) exceed 500 LOC limit:

| Module | LOC | Excess |
|--------|-----|--------|
| tcps_work_order.erl | 2,202 | +1,702 |
| tcps_receipt_verifier.erl | 1,706 | +1,206 |
| tcps_persistence.erl | 1,633 | +1,133 |
| erlmcp_server.erl | 1,520 | +1,020 |
| tcps_sku.erl | 1,457 | +957 |
| erlmcp_report_visualizer.erl | 1,359 | +859 |
| tcps_kaizen.erl | 1,354 | +854 |
| tcps_quality_gates.erl | 1,318 | +818 |
| erlmcp.erl | 1,088 | +588 |
| [+ 28 more modules] | | |

### Root Cause

Modules grew with feature additions without refactoring into smaller pieces.

### Refactoring Strategy

#### Priority 1: erlmcp_server.erl (1,520 LOC)

**Current structure**:
- Initialization and state management
- Resource handling
- Tool registration and execution
- Prompt management
- Task management
- Progress notifications

**Split into**:

1. `erlmcp_server_core.erl` (400 LOC)
   - Initialization
   - State management
   - Exports API

2. `erlmcp_server_resources.erl` (350 LOC)
   - add_resource/3
   - delete_resource/2
   - subscribe_resource/3
   - Resource handling

3. `erlmcp_server_tools.erl` (400 LOC)
   - add_tool/3
   - add_tool_with_schema/4
   - Tool execution
   - Tool notifications

4. `erlmcp_server_prompts.erl` (300 LOC)
   - add_prompt/3, /4, /5
   - Prompt handling
   - Argument validation

```erlang
% erlmcp_server.erl becomes thin dispatcher:
-module(erlmcp_server).
-export([
    start_link/2,
    add_resource/3,
    add_tool/3,
    add_prompt/3,
    % ... all API exports
]).

add_resource(Server, Uri, Handler) ->
    erlmcp_server_resources:add_resource(Server, Uri, Handler).

add_tool(Server, Name, Handler) ->
    erlmcp_server_tools:add_tool(Server, Name, Handler).

add_prompt(Server, Name, Handler) ->
    erlmcp_server_prompts:add_prompt(Server, Name, Handler).
```

#### Priority 2: tcps_work_order.erl (2,202 LOC)

**Suggested split**:
- tcps_work_order.erl (400 LOC) - Core data structures
- tcps_work_order_lifecycle.erl (500 LOC) - State transitions
- tcps_work_order_validation.erl (400 LOC) - Validation logic
- tcps_work_order_serialization.erl (400 LOC) - JSON/serialization
- tcps_work_order_metrics.erl (300 LOC) - Metrics collection

#### Priority 3: tcps_receipt_verifier.erl (1,706 LOC)

**Suggested split**:
- tcps_receipt_verifier.erl (400 LOC) - Core verification
- tcps_receipt_parser.erl (400 LOC) - Receipt parsing
- tcps_receipt_validator.erl (450 LOC) - Validation rules
- tcps_receipt_signatures.erl (400 LOC) - Signature handling

### Implementation Plan

```bash
# Week 1: erlmcp_server.erl split
Day 1-2:
  ├─ Create new modules (stubs)
  ├─ Move resource code to erlmcp_server_resources.erl
  └─ Add internal API calls

Day 3-4:
  ├─ Move tool code to erlmcp_server_tools.erl
  ├─ Move prompt code to erlmcp_server_prompts.erl
  └─ Update main erlmcp_server.erl to dispatch

Day 5:
  ├─ Run tests for all split modules
  ├─ Verify all API still works
  └─ Update documentation

# Week 2: TCPS module splits
Similar process for tcps_work_order.erl, tcps_receipt_verifier.erl, etc.
```

### Verification

```bash
# Verify size after split
for f in src/erlmcp_server*.erl; do
  wc -l "$f"
done

# Verify all modules < 500 LOC
find src -name "*.erl" -exec wc -l {} \; | awk '$1 > 500 {print}'
```

### Expected Outcome

- ✓ All modules <500 LOC
- ✓ Single responsibility principle followed
- ✓ Easier to test and maintain
- ✓ Passes Lean Six Sigma quality gate

### Estimated Time

**8-10 days** for all refactoring

---

## Critical Issue #4: Xref Undefined Functions

### Problem

46 functions defined in code but not exported/implemented:

```
erlmcp.erl: 11 undefined
erlmcp_registry.erl: 3 undefined
erlmcp_transport_http.erl: 4 undefined
erlmcp_transport_tcp.erl: 2 undefined
[... 26 more]
```

### Investigation Steps

```bash
# Get full xref report
rebar3 xref 2>&1 | grep "Warning: " | head -50

# Check specific module
rebar3 xref 2>&1 | grep "erlmcp_server.erl"

# Get count
rebar3 xref 2>&1 | grep "Warning:" | wc -l
```

### Solution Approach

#### For each undefined function:

1. **Check if it's called anywhere**:
   ```bash
   grep -r "erlmcp:get_transport_binding_info" src/ test/
   ```

2. **If called**:
   - Implement the function
   - Add to exports
   - Verify logic

3. **If not called**:
   - Remove the function call
   - Or move to internal helper

### Example Fix

**Issue**: erlmcp:get_transport_binding_info/1 undefined

**Investigation**:
```bash
grep -r "get_transport_binding_info" .
# Result: No matches found
```

**Solution**:
- Search for references to this in code
- If none: Remove the function call
- If some: Implement the function

```erlang
%% In erlmcp.erl - add:
-spec get_transport_binding_info(atom()) -> map() | {error, term()}.
get_transport_binding_info(TransportId) ->
    erlmcp_registry:lookup_transport(TransportId).
```

### Batch Fix Script

```bash
#!/bin/bash
# xref_fixes.sh

UNDEFINED=$(rebar3 xref 2>&1 | grep "Warning: " | awk '{print $NF}' | sort -u)

for FUNC in $UNDEFINED; do
  echo "Checking $FUNC"

  if grep -q "$FUNC" src/*.erl test/*.erl 2>/dev/null; then
    echo "  ✓ Function is called - needs implementation"
    echo "  TODO: Add -spec and implement $FUNC"
  else
    echo "  ✗ Function not called - can remove"
    echo "  TODO: Check why $FUNC exists without use"
  fi
done
```

### Expected Outcome

- ✓ All functions either implemented or removed
- ✓ Xref reports 0 undefined functions
- ✓ No runtime call failures

### Estimated Time

**3-4 hours** (investigation + implementation)

---

## High Priority Fix: Type Coverage

### Problem

30 modules without -spec declarations (19% of codebase untyped)

### Solution

#### Step 1: Identify untyped modules
```bash
for f in src/*.erl; do
  if ! grep -q "^-spec" "$f"; then
    echo "$f"
  fi
done
```

#### Step 2: Add specs systematically

For each untyped module:

```erlang
% Add -spec for each exported function
-spec function_name(Type1, Type2) -> ReturnType.
function_name(Arg1, Arg2) -> ...
```

#### Step 3: Use Dialyzer to verify

```bash
rebar3 dialyzer
```

#### Step 4: Fix any type errors

```erlang
% Example: Fix type error
% Before: might_fail(X) -> integer when is_list(X)  % Wrong!
% After:
-spec might_fail(list()) -> {ok, integer()} | {error, term()}.
might_fail(X) when is_list(X) ->
    % Proper type handling
```

### Checklist for Each Module

- [ ] All exported functions have -spec
- [ ] All internal helpers typed if public
- [ ] Return types cover all branches
- [ ] Error cases represented in type
- [ ] Dialyzer passes for this module

### Example Spec Patterns

```erlang
% Simple function
-spec add(integer(), integer()) -> integer().

% With maps
-spec process_config(map()) -> {ok, term()} | {error, Reason} when Reason :: term().

% Callback function
-spec resource_handler(binary()) -> binary() | #mcp_content{}.

% With lists
-spec validate_list([Item]) -> {ok, [Item]} | {error, string()} when Item :: term().

% With atoms (state machines)
-spec transition(atom()) -> {ok, atom()} | {error, invalid_transition}.
```

### Expected Outcome

- ✓ 100% type coverage
- ✓ All functions have -spec
- ✓ Dialyzer can verify all code
- ✓ Better IDE support and documentation

### Estimated Time

**2-3 days** (adding specs, fixing type errors)

---

## High Priority Fix: Hardcoded Credentials

### Problem

sys.config contains example credentials that could be deployed:

```erlang
{email_password, "changeme"},
{pagerduty_integration_key, "changeme"},
{webhook_auth_header, "Bearer changeme"},
{datadog_api_key, "changeme"},
{newrelic_api_key, "changeme"},
{grafana_cloud_password, "changeme"},
```

### Solution

#### Step 1: Move to Environment Variables

```erlang
% sys.config BEFORE:
{email_password, "changeme"}

% sys.config AFTER:
{email_password, {env, "ERLMCP_EMAIL_PASSWORD"}}
```

#### Step 2: Update Code to Read Environment

```erlang
% In config loading module:
-spec get_email_password() -> binary().
get_email_password() ->
    case os:getenv("ERLMCP_EMAIL_PASSWORD") of
        false ->
            error({missing_env, "ERLMCP_EMAIL_PASSWORD"});
        Password when is_list(Password) ->
            list_to_binary(Password)
    end.
```

#### Step 3: Update Paths

```erlang
% BEFORE (hardcoded):
{allowed_paths, [
    "/Users/sac/projects",
    "/tmp"
]}

% AFTER (configurable):
{allowed_paths, {env, "ERLMCP_ALLOWED_PATHS"}}
```

#### Step 4: Create Environment Template

```bash
# .env.example
ERLMCP_EMAIL_PASSWORD=your_password_here
ERLMCP_EMAIL_FROM=alerts@example.com
ERLMCP_PAGERDUTY_KEY=your_key_here
ERLMCP_DATADOG_API_KEY=your_key_here
ERLMCP_NEWRELIC_API_KEY=your_key_here
ERLMCP_ALLOWED_PATHS=/var/mcp,/tmp

# Production deployment loads from actual env
export ERLMCP_EMAIL_PASSWORD=$(aws secretsmanager get-secret-value --secret-id erlmcp/email/password)
```

#### Step 5: Validation on Startup

```erlang
%% erlmcp_app.erl start function
start(normal, []) ->
    case validate_required_env_vars() of
        ok ->
            erlmcp_sup:start_link();
        {error, Missing} ->
            {error, {missing_environment_variables, Missing}}
    end.

validate_required_env_vars() ->
    Required = [
        "ERLMCP_EMAIL_PASSWORD",
        "ERLMCP_PAGERDUTY_KEY"
    ],

    Missing = [Var || Var <- Required, os:getenv(Var) =:= false],

    case Missing of
        [] -> ok;
        _ -> {error, Missing}
    end.
```

### Checklist

- [ ] All credentials moved to environment variables
- [ ] No "changeme" placeholder values in config
- [ ] No hardcoded API keys in code
- [ ] .env.example created
- [ ] Startup validation added
- [ ] Documentation updated
- [ ] CI/CD secrets configured

### Expected Outcome

- ✓ No credentials in version control
- ✓ Safe to share config template
- ✓ Easy to deploy to different environments
- ✓ Secrets stored in secure vaults

### Estimated Time

**2-4 hours**

---

## High Priority Fix: Hardcoded Paths

### Problem

System-specific paths in config that prevent deployment:

```erlang
% sys.config
{allowed_paths, [
    "/Users/sac/projects",  % Developer-specific
    "/tmp"
]},

% Root config
{root, "erlmcp"},  % In logs/erlmcp.log
```

### Solution

#### Step 1: Use Relative/Dynamic Paths

```erlang
% Before:
{log_file, "logs/erlmcp.log"}

% After:
{log_file, {env, "ERLMCP_LOG_DIR", "log/erlmcp.log"}}
```

#### Step 2: Helper Function for Path Resolution

```erlang
-spec get_log_dir() -> file:filename().
get_log_dir() ->
    case os:getenv("ERLMCP_LOG_DIR") of
        false ->
            filename:join(erlang:system_info(root_dir), "log");
        Dir ->
            Dir
    end.

-spec get_base_dir() -> file:filename().
get_base_dir() ->
    case os:getenv("ERLMCP_BASE_DIR") of
        false ->
            filename:basedir(user, "erlmcp");
        Dir ->
            Dir
    end.
```

#### Step 3: Update Configuration

```erlang
% sys.config
{erlmcp, [
    %% Use environment variables with sensible defaults
    {log_dir, {env, "ERLMCP_LOG_DIR"}},
    {data_dir, {env, "ERLMCP_DATA_DIR"}},
    {roots, [
        {allowed_paths, {env, "ERLMCP_ALLOWED_PATHS"}}
    ]}
]},
```

#### Step 4: Deployment Configuration

```bash
#!/bin/bash
# deploy.sh

export ERLMCP_LOG_DIR=/var/log/erlmcp
export ERLMCP_DATA_DIR=/var/lib/erlmcp
export ERLMCP_ALLOWED_PATHS=/var/mcp:/tmp:/home/user/projects

./erlmcp/bin/erlmcp foreground
```

### Checklist

- [ ] Remove /Users/sac paths from config
- [ ] Use environment variables
- [ ] Provide sensible defaults
- [ ] Document all path environment variables
- [ ] Test on different systems
- [ ] Update deployment guide

### Expected Outcome

- ✓ Config portable across systems
- ✓ Easy multi-environment deployment
- ✓ No developer-specific paths

### Estimated Time

**1-2 hours**

---

## Implementation Order

### Week 1 (4 days)

```
Monday:
  ├─ Fix Dialyzer (2 hours)
  ├─ Locate missing test modules (1 hour)
  └─ Start test module investigation (3 hours)

Tuesday:
  ├─ Resolve missing test modules (2 hours)
  ├─ Fix xref undefined functions (3 hours)
  └─ Verify tests can execute (1 hour)

Wednesday:
  ├─ Remove hardcoded credentials (2 hours)
  ├─ Remove hardcoded paths (1 hour)
  └─ Create environment template (1 hour)

Thursday:
  ├─ Plan module refactoring (1 hour)
  ├─ Start erlmcp_server.erl split
  └─ Buffer for unexpected issues
```

### Week 2-3 (10 days)

```
Monday-Thursday:
  ├─ Complete erlmcp_server.erl split
  ├─ Refactor tcps_work_order.erl
  ├─ Refactor tcps_receipt_verifier.erl
  └─ Start other large modules

Friday:
  ├─ Refactor smaller modules
  ├─ Run test suite
  └─ Fix any test failures
```

### Week 3 (3 days)

```
Monday-Tuesday:
  ├─ Add type annotations (30 modules)
  ├─ Run Dialyzer
  └─ Fix type errors

Wednesday:
  ├─ Final verification
  ├─ Performance baseline
  └─ Security audit
```

---

## Success Criteria

### All Blockers Resolved ✓

- [ ] Dialyzer completes successfully
- [ ] All test modules present, tests execute
- [ ] 0 xref undefined functions
- [ ] All modules <500 LOC
- [ ] 100% type coverage
- [ ] No hardcoded credentials
- [ ] No hardcoded paths

### Quality Gates Pass ✓

- [ ] Compilation: 0 errors
- [ ] Tests: 100% pass rate
- [ ] Coverage: 80%+
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined functions

### Production Readiness ✓

- [ ] Security audit passed
- [ ] Configuration validated
- [ ] Deployment guide written
- [ ] Changelog created
- [ ] All documentation updated

---

## Sign-Off

| Item | Status |
|------|--------|
| Blocker Count | 4 Critical |
| Estimated Effort | 19-29 days |
| Target Completion | 2026-02-17 |
| Ready for Release | 🔴 When all fixes complete |
