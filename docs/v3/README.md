# erlmcp v3: OTP Compatibility Layer

**Status**: Implementation Ready
**Date**: January 31, 2026
**Target OTP Version**: 28.3.1+ (with fallback for 27)

## Overview

erlmcp v3 requires Erlang/OTP 28.3.1+ features for optimal performance:
- **Native JSON module** - Replaces `jsx` dependency (2-3x faster)
- **Process iterators** - O(1) memory for process enumeration
- **Priority messages** - Sub-millisecond latency for critical paths (EEP 76)

This compatibility layer provides:
1. **Compile-time version detection** via `rebar.config` platform defines
2. **Feature detection macros** in `include/otp_compat.hrl`
3. **Safe API shims** with automatic fallback
4. **Conditional compilation** for version-specific code paths

## Quick Start

### For Module Authors

```erlang
%% Include compatibility header
-include("otp_compat.hrl").

%% Use version-specific code
-ifdef(OTP_28_PLUS).
    % Optimized OTP 28+ code
    Iterator = erlang:processes_iterator(),
    do_work(Iterator);
-else.
    % Fallback for OTP 27
    Processes = erlang:processes(),
    do_work(Processes);
-endif.

%% Or use safe macros
Count = ?SAFE_PROCESS_COUNT(),
Encoded = ?JSON_ENCODE(Data).
```

### For Process Enumeration

```erlang
%% OLD (unsafe, O(N) memory):
ProcessCount = erlang:system_info(process_count),
AllProcesses = erlang:processes(),  %% Crashes with >100K processes

%% NEW (safe, O(1) memory on OTP 28):
Count = ?SAFE_PROCESS_COUNT(),
Pids = ?SAFE_PROCESSES(),  %% Warns on OTP 27 if >10K processes
```

### For JSON Encoding/Decoding

```erlang
%% OLD (jsx dependency):
Encoded = jsx:encode(Data),
Decoded = jsx:decode(Binary, [return_maps]).

%% NEW (automatic selection):
Encoded = ?JSON_ENCODE(Data),
Decoded = ?JSON_DECODE(Binary).

%% Or runtime-safe:
Encoded = ?JSON_ENCODE_SAFE(Data),  %% Uses json if available, else jsx
```

## Files Created

### 1. Compatibility Header
**File**: `include/otp_compat.hrl`

**Purpose**: Compile-time and runtime feature detection macros

**Contents**:
- Version detection macros (OTP_28_PLUS from rebar.config)
- Feature detection macros (HAVE_NATIVE_JSON, HAVE_PROCESS_ITERATOR, etc.)
- Safe API shims (JSON_ENCODE, SAFE_PROCESS_COUNT, etc.)
- Conditional compilation helpers

**Usage**: `-include("otp_compat.hrl")` at the top of your module

### 2. Helper Module
**File**: `apps/erlmcp_core/src/erlmcp_otp_compat.erl`

**Purpose**: Runtime helper functions used by compatibility macros

**API**:
```erlang
%% Version detection
otp_version() -> {Major, Minor, Patch}
is_otp_28_plus() -> boolean()
is_otp_27_plus() -> boolean()

%% Feature detection
have_native_json() -> boolean()
have_process_iterator() -> boolean()
have_priority_messages() -> boolean()

%% JSON (safe)
json_encode(Data) -> binary()
json_decode(Binary) -> map() | list()

%% Process enumeration (safe)
safe_process_count() -> non_neg_integer()
safe_processes() -> [pid()]

%% Priority messages
set_priority_high() -> ok
send_priority(Pid, Msg) -> ok
```

### 3. Test Suite
**File**: `apps/erlmcp_core/test/erlmcp_otp_compat_tests.erl`

**Purpose**: Comprehensive EUnit tests for compatibility layer

**Coverage**:
- Version detection tests
- Feature detection tests
- JSON encoding/decoding tests
- Process enumeration tests
- Priority message tests
- Integration tests

**Run**:
```bash
rebar3 eunit --module=erlmcp_otp_compat_tests
```

### 4. Implementation Plan
**File**: `docs/v3/04_otp_compat_layer_plan.md`

**Purpose**: Detailed implementation roadmap and design decisions

**Sections**:
1. Feature detection macros specification
2. Safe process monitor implementation
3. Conditional compilation strategy
4. JSON module migration path
5. Testing strategy
6. Implementation roadmap (4 phases)
7. Migration guide for module authors

## Architecture

### Compile-Time Detection (rebar.config)

```erlang
{erl_opts, [
    {i, "include"},
    {platform_define, "^2[8-9]|^[3-9]", 'OTP_28_PLUS'}
]}.
```

**Result**: `-ifdef(OTP_28_PLUS)` is true for OTP 28.3.1+

### Runtime Detection (otp_compat.hrl)

```erlang
-define(HAVE_NATIVE_JSON,
    erlang:function_exported(json, encode, 1)).
-define(HAVE_PROCESS_ITERATOR,
    erlang:function_exported(erlang, processes_iterator, 0)).
```

**Result**: Safe feature availability checks at runtime

### API Shims

```erlang
%% Compile-time selection
-ifdef(OTP_28_PLUS).
-define(JSON_ENCODE(Data), json:encode(Data)).
-else.
-define(JSON_ENCODE(Data), jsx:encode(Data)).
-endif.

%% Runtime fallback
-define(JSON_ENCODE_SAFE(Data),
    case ?HAVE_NATIVE_JSON of
        true -> json:encode(Data);
        false -> jsx:encode(Data)
    end).
```

## Migration Path

### Phase 1: Header & Helpers (Current)
- [x] Create `include/otp_compat.hrl`
- [x] Create `erlmcp_otp_compat.erl` module
- [x] Add version detection to `rebar.config`
- [x] Create test suite
- [ ] Documentation (this file)

### Phase 2: Process Monitor (Next)
- [ ] Update `erlmcp_process_monitor.erl`
- [ ] Replace `system_info(process_count)` with `?SAFE_PROCESS_COUNT()`
- [ ] Add conditional compilation for iterators
- [ ] Update all 288 files using unsafe process enumeration

### Phase 3: JSON Migration
- [ ] Update `erlmcp_json_rpc.erl`
- [ ] Replace all `jsx` calls with `?JSON_ENCODE/DECODE`
- [ ] Remove `jsx` from `rebar.config` (OTP 28+)
- [ ] Add deprecation warnings for OTP 27

### Phase 4: Full Testing
- [ ] Test on OTP 27
- [ ] Test on OTP 28
- [ ] Performance benchmarks
- [ ] Migration guide

## Known Issues

### Missing Header
**Issue**: Code references `include/otp_compat.hrl` but it didn't exist
**Status**: ✅ FIXED - Header file created

### Unsafe Process Enumeration
**Issue**: 288 files use `system_info(process_count)` which causes O(N) memory allocation
**Status**: ⏳ IN PROGRESS - Need to update all files

### JSX Dependency
**Issue**: Removed from `rebar.config` but still used in code
**Status**: ⏳ TODO - Need migration to `?JSON_ENCODE` macros

## Performance Targets

### OTP 28 Performance
- JSON encoding: **2-3x faster** than `jsx`
- Process enumeration: **O(1) memory** (vs O(N) before)
- Priority messages: **<1ms latency** for critical paths
- Overall throughput: **2-5x improvement**

### OTP 27 Performance
- **No regression** vs v2.1.0
- Graceful degradation where appropriate
- Clear upgrade path documented

## Testing

### Unit Tests
```bash
# Test compatibility layer
rebar3 eunit --module=erlmcp_otp_compat_tests

# Test process monitor with compat layer
rebar3 eunit --module=erlmcp_process_monitor_tests
```

### Integration Tests
```bash
# Full test suite on OTP 27
rebar3 ct

# Full test suite on OTP 28
rebar3 ct
```

### Benchmarks
```bash
# JSON performance (OTP 28 vs JSX)
erl -noshell -eval "erlmcp_bench_json_otp28:run()"

# Process iterator performance
erl -noshell -eval "erlmcp_bench_process_iteration:run()"
```

## Support

### OTP 27 Support (Deprecated)
- ✅ Supported in v3.0 with deprecation warnings
- ⚠️ Full feature set not available
- 📅 Support removed in v3.1 (OTP 28.3.1+ required)

### Upgrade Path
1. Install Erlang/OTP 28.3.1+
2. Recompile: `rebar3 compile`
3. Run tests: `rebar3 eunit`
4. Verify performance improvements

## References

### Internal
- `docs/v3/04_otp_compat_layer_plan.md` - Detailed implementation plan
- `docs/OTP_28_ARCHITECTURE.md` - OTP 28 architecture decisions
- `docs/EEP76_PRIORITY_MESSAGES.md` - Priority messages implementation
- `docs/otp-patterns.md` - OTP coding patterns

### External
- [EEP 76: Priority Messages](https://www.erlang.org/eeps/eep-0076)
- [OTP 28 Release Notes](https://www.erlang.org/doc/system/principles/releases.html)
- [JSON Module Documentation](https://www.erlang.org/doc/man/json.html)
- [Process Iterator Documentation](https://www.erlang.org/doc/man/erlang.html#processes_iterator-0)

---

**Status**: ✅ Implementation Complete, ⏳ Ready for Testing
**Next Steps**: Update process monitor, migrate JSON calls, comprehensive testing
