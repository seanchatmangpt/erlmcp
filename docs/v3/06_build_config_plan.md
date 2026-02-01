# erlmcp v3 OSS Release - Build Configuration Plan

**Status**: IMPLEMENTATION PLAN
**Author**: Erlang OTP Developer
**Created**: 2026-01-31
**Target**: v3.0.0 OSS Release

---

## Executive Summary

This plan addresses critical build configuration issues for the v3 OSS release:

1. **Observability as Optional**: erlmcp_observability should NOT be required for basic MCP operations
2. **jsx Removal**: Complete elimination of jsx dependency (OTP 28+ has native `json`)
3. **tcps_erlmcp Cleanup**: Non-existent app referenced in release config
4. **Type Definition Conflicts**: Duplicate type definitions in erlmcp_apps_server.erl
5. **Minimal OSS Release**: Clean dependency tree for community adoption

**Impact**: Unblock v3.0.0 release, reduce dependencies by 30%, improve compilation success rate from 0% to 100%

---

## Current State Analysis

### Critical Issues

#### Issue #1: Duplicate Type Definitions (BLOCKING COMPILATION)

**File**: `apps/erlmcp_core/src/erlmcp_apps_server.erl`

**Problem**: Types defined both in header file and module source

```
Line 21: -type app_id() :: binary().        % Duplicate of erlmcp.hrl:888
Line 22: -type app_manifest() :: map().     % Duplicate of erlmcp.hrl:889
Line 24: -type app_status() :: ...          % Duplicate of erlmcp.hrl:891
```

**Compilation Error**:
```
===> Compiling apps/erlmcp_core/src/erlmcp_apps_server.erl failed
    ┌─ apps/erlmcp_core/src/erlmcp_apps_server.erl:
    │
 21 │  -type app_id() :: binary().
    │   ╰── type app_id() already defined
```

**Solution**: Remove duplicate type definitions from module, keep only in erlmcp.hrl

---

#### Issue #2: jsx Dependency Inconsistency (BROKEN PROMISE)

**Claim**: "jsx removed - OTP 28.3.1+ provides native json module"

**Reality**: jsx still referenced in multiple locations:

| File | Line | Reference |
|------|------|-----------|
| `apps/erlmcp_core/src/erlmcp_core.app.src` | 15 | `jsx` in applications list |
| `apps/erlmcp_validation/src/erlmcp_validation.app.src` | 8 | `jsx` in applications list |
| `apps/erlmcp_core/src/erlmcp_json_codec.erl` | 46, 50, 61 | `jsx:encode/decode` calls |
| `rebar.config` | 56, 260 | Comments claim removed, but still pulled as transitive dep |

**Root Cause**: erlmcp_json_codec.erl uses adaptive codec (jiffy + jsx fallback)
- Attempts jiffy first (3x faster for large payloads)
- Falls back to jsx if jiffy unavailable
- **BUT**: Neither jiffy nor jsx needed in OTP 28+ (native `json` module)

**Impact**: Unnecessary dependency bloat, confusion about OTP 28+ compatibility

---

#### Issue #3: tcps_erlmcp Ghost App (BUILD BREAKING)

**Reference**: `rebar.config:285` includes `tcps_erlmcp` in release

**Problem**: App directory does not exist:
```bash
$ ls apps/
erlmcp_core/        erlmcp_transports/   erlmcp_observability/  erlmcp_validation/
```

**Impact**: Release generation fails with "app not found" error

**History**: tcps_erlmcp was a separate Toyota Code Production System app for v2.1.0, removed for v3 OSS

---

#### Issue #4: Observability as Mandatory (WRONG FOR OSS)

**Current State**: erlmcp_observability required by default

**Problem**: OpenTelemetry dependencies (opentelemetry, opentelemetry_api, opentelemetry_exporter) add heavy overhead for users who only need basic MCP protocol

**User Impact**:
- Forced dependency on 3 OpenTelemetry packages (~15MB)
- Complex configuration for simple use cases
- Slower compilation for development

**Should Be**: Optional add-on for enterprise monitoring scenarios

---

## Dependency Cleanup Strategy

### Phase 1: Critical Fixes (Unblock Compilation)

#### Fix #1: Remove Duplicate Type Definitions

**File**: `apps/erlmcp_core/src/erlmcp_apps_server.erl`

**Action**: Delete lines 21-24 (duplicate type definitions)

**Before**:
```erlang
%% Types
-type app_id() :: binary().
-type app_manifest() :: map().
-type permissions() :: [binary()].
-type app_status() :: stopped | starting | running | stopping | crashed.
```

**After**:
```erlang
%% Types (defined in erlmcp.hrl - included via -include("erlmcp.hrl"))
```

**Verification**:
```bash
TERM=dumb rebar3 compile --apps=erlmcp_core
# Expected: clean compilation with 0 errors
```

---

#### Fix #2: Remove jsx Completely

**Strategy**: Replace with conditional OTP 28+ native json module

**Files to Modify**:

1. **Create**: `apps/erlmcp_core/src/erlmcp_json.erl` (unified JSON codec)

```erlang
-module(erlmcp_json).
-export([encode/1, decode/1]).

-ifdef(OTP_28_PLUS).
%% OTP 28.3.1+: Use native json module
encode(Data) -> json:encode(Data).
decode(Bin) -> json:decode(Bin, [return_maps]).
-else.
%% Fallback for OTP 25-27: Use jsx (transitive dependency via jesse)
encode(Data) -> jsx:encode(Data).
decode(Bin) -> jsx:decode(Bin, [return_maps]).
-endif.
```

2. **Delete**: `apps/erlmcp_core/src/erlmcp_json_codec.erl` (obsolete adaptive codec)

3. **Update**: `apps/erlmcp_core/src/erlmcp_core.app.src`

**Before**:
```erlang
{applications, [
    kernel,
    stdlib,
    crypto,
    public_key,
    jsx,              % REMOVE THIS LINE
    jesse,
    gproc,
    jose,
    bbmustache
]},
```

**After**:
```erlang
{applications, [
    kernel,
    stdlib,
    crypto,
    public_key,
    jesse,            % Keep jesse for JSON Schema validation
    gproc,
    jose,
    bbmustache
]},
```

4. **Update**: `apps/erlmcp_validation/src/erlmcp_validation.app.src`

Same change as erlmcp_core.app.src (remove jsx from applications list)

5. **Global Search**: Replace all `jsx:encode/decode` calls with `erlmcp_json:encode/decode`

```bash
# Find all direct jsx usage
grep -r "jsx:encode\|jsx:decode" apps/ --include="*.erl" | grep -v "_build"
```

**Expected Results**:
- 0 direct jsx references in production code
- jsx only pulled as transitive dependency via jesse (acceptable)
- OTP 28+ builds use native json module (zero jsx runtime calls)

---

#### Fix #3: Remove tcps_erlmcp References

**File**: `rebar.config`

**Line 10**: Update comment
```erlang
%% OLD:
%%   - tcps_erlmcp: Toyota Code Production System (optional)

%% NEW:
%%   - tcps_erlmcp: REMOVED in v3.0.0 (moved to separate repository)
```

**Line 285**: Remove from release config
```erlang
{release, {erlmcp, "3.0.0"}, [
    erlmcp_core,
    erlmcp_transports,
    erlmcp_observability,  % Still optional (see Phase 2)
    erlmcp_validation,
    %% tcps_erlmcp REMOVED - non-existent app
    gproc,
    ...
]},
```

**Line 79**: Remove from shell config
```erlang
{shell, [
    {config, "config/sys.config"},
    {apps, [erlmcp_core, erlmcp_transports, erlmcp_observability, erlmcp_validation]}
    %% tcps_erlmcp already not referenced here
]}.
```

---

### Phase 2: Optional Apps Configuration

#### Goal: Make Observability Truly Optional

**Current Problem**: erlmcp_observability is always compiled even if not needed

**Solution**: Conditional compilation based on environment variable

**Step 1**: Update `rebar.config` profiles

```erlang
{profiles, [
    %% Minimal profile (core + transports + validation only)
    {minimal, [
        {erl_opts, [
            debug_info,
            {d, 'MINIMAL_BUILD'}
        ]},
        {relx, [
            {release, {erlmcp_minimal, "3.0.0"}, [
                erlmcp_core,
                erlmcp_transports,
                erlmcp_validation,
                gproc,
                gun,
                ranch,
                poolboy,
                jesse,
                jose,
                bbmustache,
                cowboy,
                mnesia,
                sasl
            ]}
        ]}
    ]},

    %% Observability-enabled profile (add OpenTelemetry)
    {observability, [
        {deps, [
            {opentelemetry_api, "1.5.0"},
            {opentelemetry, "1.7.0"},
            {opentelemetry_exporter, "1.10.0"}
        ]},
        {relx, [
            {release, {erlmcp_full, "3.0.0"}, [
                erlmcp_core,
                erlmcp_transports,
                erlmcp_observability,  % Include observability app
                erlmcp_validation,
                %% ... all dependencies including OTEL
            ]}
        ]}
    ]},

    %% Default profile (observability included for backward compatibility)
    {prod, [
        {erl_opts, [
            no_debug_info,
            {d, 'PROD'}
        ]},
        {deps, [
            %% OTEL deps at top level for backward compatibility
            {opentelemetry_api, "1.5.0"},
            {opentelemetry, "1.7.0"},
            {opentelemetry_exporter, "1.10.0"}
        ]},
        {compiler_warnings_as_errors, true},
        {relx, [
            {dev_mode, false},
            {include_erts, true},
            {include_src, false}
        ]}
    ]}
]}.
```

**Step 2**: Conditional application dependencies

**File**: `apps/erlmcp_core/src/erlmcp_core.app.src`

```erlang
{applications, [
    kernel,
    stdlib,
    crypto,
    public_key,
    jesse,
    gproc,
    jose,
    bbmustache
    %% NOTE: erlmcp_observability NOT listed (optional dependency)
]},

%% Optional applications (loaded if available, but not required)
{optional_applications, [
    erlmcp_observability  % May or may not be available
]},
```

**Step 3**: Runtime OTEL feature detection

**File**: `apps/erlmcp_core/src/erlmcp_otel.erl` (modify existing)

```erlang
%% @doc Check if OpenTelemetry is available
-spec is_available() -> boolean().
is_available() ->
    case whereis(otel_tracer_server) of
        undefined -> false;
        _Pid -> true
    end.

%% @doc Safe span creation (no-op if OTEL unavailable)
-spec start_span(binary()) -> opentelemetry:span_ctx() | undefined.
start_span(Name) ->
    case is_available() of
        true  -> otel_tracer:start_span(Name);
        false -> undefined
    end.
```

**Usage Patterns**:

```erlang
%% In production code with optional telemetry:
handle_call(Request, From, State) ->
    Span = erlmcp_otel:start_span("handle_call"),
    Result = do_handle_call(Request, State),
    case Span of
        undefined -> ok;
        _ -> otel_span:end_span(Span)
    end,
    {reply, Result, State}.
```

---

### Phase 3: Minimal OSS Release Profile

#### Goal: Zero-friction community adoption

**Release Variants**:

| Variant | Includes | Use Case | Size |
|---------|----------|----------|------|
| **erlmcp_minimal** | core + transports + validation | Basic MCP protocol | ~5MB |
| **erlmcp_standard** | + OpenTelemetry | Production monitoring | ~8MB |
| **erlmcp_dev** | + dev tools (recon, observer_cli) | Development | ~10MB |

**Build Commands**:

```bash
# Minimal release (OSS users)
rebar3 as minimal release

# Standard release (production with monitoring)
rebar3 as prod release

# Development release
rebar3 as dev release
```

**Release Verification**:

```bash
# Test minimal release works without OTEL
./_build/minimal/rel/erlmcp_minimal/bin/erlmcp_minimal console
# Should start even if opentelemetry apps not available

# Test standard release has OTEL
./_build/prod/rel/erlmcp/bin/erlmcp console
1> application:which_applications().
% Should include opentelemetry, opentelemetry_api, opentelemetry_exporter
```

---

## Dependency Classification

### Essential Dependencies (Core Functionality)

| Dependency | Version | Purpose | Removal Cost |
|------------|---------|---------|--------------|
| **jesse** | 1.8.1 | JSON Schema validation | HIGH (rewrite required) |
| **gproc** | 0.9.0 | Process registry | HIGH (core architecture) |
| **jose** | 1.11.1 | JWT validation | MEDIUM (security critical) |
| **bbmustache** | 1.12.2 | Template rendering | LOW (simple templates) |

### Transport Dependencies (erlmcp_transports)

| Dependency | Version | Purpose | Optional |
|------------|---------|---------|----------|
| **gun** | 2.0.1 | HTTP client | NO (HTTP transport) |
| **ranch** | 2.1.0 | TCP acceptor pool | NO (TCP transport) |
| **cowboy** | 2.10.0 | HTTP server | NO (HTTP server) |
| **poolboy** | 1.5.2 | Connection pooling | NO (connection mgmt) |

### Optional Dependencies (Observability)

| Dependency | Version | Purpose | Profile |
|------------|---------|---------|---------|
| **opentelemetry_api** | 1.5.0 | OTEL API | observability, prod |
| **opentelemetry** | 1.7.0 | OTEL SDK | observability, prod |
| **opentelemetry_exporter** | 1.10.0 | OTEL exporters | observability, prod |

### Development Dependencies (Dev Only)

| Dependency | Version | Purpose | Profile |
|------------|---------|---------|---------|
| **recon** | 2.5.3 | Runtime debugging | dev, test |
| **observer_cli** | 1.7.4 | CLI observer | dev, test |
| **proper** | 1.4.0 | Property-based testing | test |
| **meck** | 0.9.2 | Mocking (limited) | test |
| **coveralls** | 2.2.0 | Coverage reporting | test |
| **rebar3_lint** | 3.0.1 | Linting | dev |

### Transitive Dependencies (Indirect)

| Dependency | Brought By | Action |
|------------|------------|--------|
| **jsx** | jesse (via JSON Schema) | Accept as transitive |
| **jiffy** | NONE (remove erlmcp_json_codec) | Remove from codebase |
| **telemetry** | opentelemetry | Only if OTEL enabled |

---

## Build Verification Plan

### Step 1: Pre-Build Validation

```bash
# 1. Check for type definition conflicts
grep -n "^-type app_id\(\)" apps/*/src/*.erl
# Expected: Only 1 match in erlmcp.hrl

# 2. Check for direct jsx usage (should be 0 after fix)
grep -r "jsx:encode\|jsx:decode" apps/ --include="*.erl" | grep -v "_build" | wc -l
# Expected: 0

# 3. Check for tcps_erlmcp references
grep -r "tcps_erlmcp" rebar.config
# Expected: Only in comments, not in code

# 4. Verify OpenTelemetry is optional
grep -A 5 "optional_applications" apps/*/src/*.app.src
# Expected: erlmcp_observability in optional_applications list
```

### Step 2: Compilation Tests

```bash
# Test 1: Minimal profile (no OTEL)
TERM=dumb rebar3 as minimal compile
# Expected: Clean compilation, 0 errors

# Test 2: Standard profile (with OTEL)
TERM=dumb rebar3 as prod compile
# Expected: Clean compilation, 0 errors

# Test 3: All apps
TERM=dumb rebar3 compile
# Expected: Clean compilation, 0 errors
```

### Step 3: Release Generation

```bash
# Test 1: Minimal release
rebar3 as minimal release
# Verify: _build/minimal/rel/erlmcp_minimal/ exists

# Test 2: Standard release
rebar3 as prod release
# Verify: _build/prod/rel/erlmcp/ exists

# Test 3: Check release sizes
du -sh _build/minimal/rel/erlmcp_minimal/
du -sh _build/prod/rel/erlmcp/
# Expected: minimal < 8MB, prod < 15MB
```

### Step 4: Runtime Verification

```bash
# Test 1: Minimal release starts without OTEL
_build/minimal/rel/erlmcp_minimal/bin/erlmcp_minimal console
1> application:which_applications().
% Should NOT include opentelemetry*
2> l(erlmcp_json).
% Should load successfully (using native json or jsx fallback)
3> erlmcp_json:encode(#{test => <<"value">>}).
% Should return encoded JSON

# Test 2: Standard release has OTEL
_build/prod/rel/erlmcp/bin/erlmcp console
1> application:which_applications().
% SHOULD include opentelemetry, opentelemetry_api, opentelemetry_exporter
2> erlmcp_otel:is_available().
% Should return true
```

### Step 5: Test Suite Execution

```bash
# Test 1: Unit tests (minimal profile)
rebar3 as minimal eunit
# Expected: All core tests pass (skip OTEL-dependent tests)

# Test 2: Integration tests (standard profile)
rebar3 as test ct
# Expected: All integration tests pass

# Test 3: Coverage verification
rebar3 as test cover
# Expected: Coverage >= 80% for all apps
```

---

## Migration Guide for Users

### For OSS Users (Minimal Setup)

**Before (v2.1.0)**:
```bash
# Forced to install all dependencies including OTEL
rebar3 compile
rebar3 release
# Result: 15MB release with OpenTelemetry overhead
```

**After (v3.0.0)**:
```bash
# Minimal build (core + transports + validation)
rebar3 as minimal compile
rebar3 as minimal release
# Result: 5MB release, zero overhead

# Add OpenTelemetry only if needed
rebar3 as observability compile
rebar3 as observability release
# Result: 8MB release with monitoring
```

### For Enterprise Users (Full Stack)

**Configuration** (`config/sys.config`):
```erlang
{erlmcp_core, [
    {observability_enabled, true},  % Enable OTEL integration
    {otel_exporter, otlp_http},     % Export to OTLP collector
    {otel_endpoint, "http://otel-collector:4318"}
]},
{erlmcp_observability, [
    {metrics_enabled, true},
    {tracing_enabled, true},
    {dashboard_port, 8080}
]}.
```

---

## Implementation Checklist

### Phase 1: Critical Fixes (Priority: CRITICAL)

- [ ] **Fix #1.1**: Remove duplicate type definitions from erlmcp_apps_server.erl
- [ ] **Fix #2.1**: Create erlmcp_json.erl with conditional OTP 28+ support
- [ ] **Fix #2.2**: Delete erlmcp_json_codec.erl
- [ ] **Fix #2.3**: Remove jsx from erlmcp_core.app.src
- [ ] **Fix #2.4**: Remove jsx from erlmcp_validation.app.src
- [ ] **Fix #2.5**: Replace all direct jsx usage with erlmcp_json calls
- [ ] **Fix #3.1**: Remove tcps_erlmcp from rebar.config release
- [ ] **Fix #3.2**: Update tcps_erlmcp comments in rebar.config
- [ ] **Verify**: Compilation succeeds with `TERM=dumb rebar3 compile`
- [ ] **Verify**: 0 compilation errors, 0 warnings

### Phase 2: Optional Apps (Priority: HIGH)

- [ ] **Config #1**: Add `minimal` profile to rebar.config
- [ ] **Config #2**: Add `observability` profile to rebar.config
- [ ] **Config #3**: Update `prod` profile to include OTEL deps
- [ ] **App #1**: Add erlmcp_observability to optional_applications
- [ ] **Code #1**: Add is_available/0 check to erlmcp_otel.erl
- [ ] **Code #2**: Update all OTEL calls to check availability first
- [ ] **Test**: Minimal release works without OTEL
- [ ] **Test**: Standard release has OTEL available

### Phase 3: Documentation (Priority: MEDIUM)

- [ ] **Doc #1**: Update README.md with build profiles
- [ ] **Doc #2**: Create MIGRATION.md for v2 -> v3
- [ ] **Doc #3**: Update CLAUDE.md with optional apps guidance
- [ ] **Doc #4**: Create DEPENDENCIES.md listing all deps
- [ ] **Doc #5**: Update RELEASE_NOTES.md for v3.0.0

### Phase 4: Validation (Priority: HIGH)

- [ ] **Test #1**: All unit tests pass (minimal profile)
- [ ] **Test #2**: All integration tests pass (standard profile)
- [ ] **Test #3**: Coverage >= 80% (all apps)
- [ ] **Test #4**: Minimal release < 8MB
- [ ] **Test #5**: Standard release < 15MB
- [ ] **Test #6**: Dialyzer warnings = 0
- [ ] **Test #7**: Xref checks pass
- [ ] **Test #8**: Escript builds correctly

---

## Success Criteria

### Build Quality Gates

| Gate | Criterion | Status |
|------|-----------|--------|
| **Compilation** | errors = 0 | ❌ Failing (duplicate types) |
| **Warnings** | warnings < 10 | ⚠️ Check after fixes |
| **Tests** | pass_rate = 1.0 | ✅ Expected after fixes |
| **Coverage** | coverage >= 0.8 | ✅ Maintain current |
| **Release Size** | minimal < 8MB | 🔄 Measure after fix |
| **Dialyzer** | warnings = 0 | 🔄 Verify after fix |
| **Xref** | undefined = ∅ | ✅ Maintain current |

### Dependency Targets

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| **Direct deps** | 12 | 9 | -25% |
| **Transitive deps** | 45 | 35 | -22% |
| **Release size (min)** | N/A | < 8MB | New |
| **Release size (std)** | ~15MB | < 15MB | Maintain |
| **Compile time (min)** | N/A | < 30s | Target |
| **Compile time (std)** | ~45s | < 60s | Maintain |

---

## Risk Assessment

### High Risk Items

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Breaking existing deployments** | HIGH | Keep prod profile backward compatible |
| **jesse still depends on jsx** | MEDIUM | Accept as transitive dep, no direct usage |
| **OTP 25-27 compatibility** | LOW | Conditional compilation via -ifdef(OTP_28_PLUS) |
| **OpenTelemetry API changes** | LOW | Use stable 1.5.0+ API version |

### Rollback Plan

If critical issues found after merging:

1. **Revert rebar.config** to v2.1.0 state
2. **Keep erlmcp_json_codec.erl** as fallback
3. **Restore jsx in .app.src files** (as transitive dep)
4. **Document v3.0.1** as bugfix release

---

## Open Questions

1. **Q**: Should we support OTP 25-27 in v3.0.0?
   - **A**: Yes, via conditional compilation. OTP 28+ is recommended but not required.

2. **Q**: Should jesse be replaced with lighter-weight JSON Schema validator?
   - **A**: No, jesse is battle-tested. Rewriting is high-risk for v3.0.0.

3. **Q**: Should OpenTelemetry be removed from default dev experience?
   - **A**: Yes, use `minimal` profile for development, `observability` profile for production.

4. **Q**: What about existing v2.1.0 users upgrading to v3.0.0?
   - **A**: Provide migration guide. `prod` profile remains backward compatible.

---

## Next Steps

1. **Immediate**: Fix duplicate type definitions (5 minutes)
2. **Today**: Implement erlmcp_json.erl and remove jsx direct usage (2 hours)
3. **Tomorrow**: Implement optional apps configuration (3 hours)
4. **This Week**: Complete build verification and testing (4 hours)
5. **Next Week**: Update documentation and create release artifacts (2 hours)

**Total Effort**: ~11 hours (1-2 developer-days)

---

## References

- **OTP Patterns**: `/Users/sac/erlmcp/docs/otp-patterns.md`
- **Current rebar.config**: `/Users/sac/erlmcp/rebar.config`
- **Duplicate Types Issue**: `apps/erlmcp_core/src/erlmcp_apps_server.erl:21-24`
- **Header File**: `apps/erlmcp_core/include/erlmcp.hrl:888-891`
- **JSON Codec**: `apps/erlmcp_core/src/erlmcp_json_codec.erl`
- **App Metadata**: `apps/*/src/*.app.src`

---

**Document Status**: READY FOR IMPLEMENTATION
**Next Action**: Execute Phase 1 fixes (duplicate types + jsx removal)
**Owner**: Erlang OTP Developer Agent
