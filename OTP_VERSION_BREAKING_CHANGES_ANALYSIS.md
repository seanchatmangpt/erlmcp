# OTP Version Breaking Changes Analysis: 26, 27, 28

## Executive Summary

This document provides a comprehensive analysis of breaking changes between Erlang/OTP versions 26, 27, and 28, and identifies specific erlmcp modules that require updates to maintain cross-version compatibility.

## Current State

### OTP Version Requirements
- **Current Configuration**: OTP 28+ (minimum: 28.0.0)
- **Codebase Status**: Primarily designed for OTP 28.3.1+
- **Compatibility Layer**: `otp_compat.hrl` with fallback mechanisms

### Key Findings
1. **Native JSON Module**: OTP 27+ introduces built-in JSON support
2. **Process Iterators**: OTP 28+ provides O(1) memory process enumeration
3. **Priority Messages**: OTP 28+ implements EEP 76 for priority message delivery
4. **Platform Defines**: Missing OTP_28_PLUS compilation macros

## Breaking Changes Analysis

### 1. JSON Encoding/Decoding

#### Changes:
- **OTP 26-27**: Must use `jsx` library for JSON operations
- **OTP 28+**: Can use native `json` module (RFC 8259 compliant)

#### Impact on erlmcp:
- **162 files** use JSON encoding/decoding
- **erlmcp_json_native.erl**: OTP 27+ only module
- **otp_compat.hrl**: Provides compile-time selection

#### Required Updates:
```erlang
% Current pattern in rebar.config
{deps, [{jsx, "3.1.0"}]}  % Always needed for OTP <28

% Should add conditional dependency
{deps, [
  {jsx, "3.1.0", {optional, true}},  % Optional for OTP 28+
  {json, "1.0.0", {optional, true}}  % Available OTP 28+
]}
```

### 2. Process Enumeration

#### Changes:
- **OTP 26-27**: `erlang:processes()` creates O(N) list in memory
- **OTP 28+**: `erlang:processes_iterator()` provides O(1) memory enumeration

#### Impact on erlmcp:
- **15 files** use process enumeration
- **erlmcp_process_monitor.erl**: Uses OTP 28 features directly
- **erlmcp_graceful_drain.erl**: Uses process counting

#### Required Updates:
```erlang
% Current code in erlmcp_process_monitor.erl (OTP 28 specific)
enumerate_processes() ->
    Iterator = erlang:processes_iterator(),  % OTP 28+
    enumerate_processes_iterator(Iterator, 0).

% Should use otp_compat macros
enumerate_processes() ->
    ?SAFE_PROCESSES(),  % Handles both versions
```

### 3. Priority Messages

#### Changes:
- **OTP 26-27**: No priority message support
- **OTP 28+**: `process_flag(priority, high)` and `{priority, high}` send option

#### Impact on erlmcp:
- **Priority messages** in graceful drain and system monitoring
- **erlmcp_graceful_drain.erl**: Implements priority shutdown
- **Critical systems** need preemption capability

#### Required Updates:
```erlang
% Current implementation
?SET_PRIORITY_HIGH()  % Macro in otp_compat.hrl

% Send priority messages
?SEND_PRIORITY(Pid, Message)  % Handles version differences
```

### 4. Module Deprecations

#### OTP 26 Deprecations:
- `erlang:system_info(process_count)` - Still available
- `erlang:processes()` - Still available but inefficient

#### OTP 27 Deprecations:
- `jsx` dependency becomes optional
- No major deprecations affecting erlmcp

#### OTP 28 Deprecations:
- `erlang:processes()` - Still available but inefficient
- Introduction of newer alternatives

### 5. API Changes

#### Process Information API:
- **OTP 26-27**: Limited process introspection
- **OTP 28+**: Enhanced process metadata via iterators

#### Error Handling:
- **OTP 26-27**: Specific error codes for JSON operations
- **OTP 28+**: Unified error handling with native JSON

## Module Analysis

### 1. Core Modules Requiring Updates

#### `include/otp_compat.hrl`
**Status**: ✅ Implements version detection
**Issues**: Missing platform defines in rebar.config
**Required**: Add OTP_28_PLUS platform define

#### `apps/erlmcp_core/src/erlmcp_otp_compat.erl`
**Status**: ✅ Feature detection implemented
**Issues**: No issues detected
**Required**: None

#### `apps/erlmcp_core/src/erlmcp_json_native.erl`
**Status**: ❌ OTP 27+ only
**Issues**: Breaks on OTP 26
**Required**: Add version guards or rename

#### `apps/erlmcp_observability/src/erlmcp_process_monitor.erl`
**Status**: ❌ Uses OTP 28 features directly
**Issues**: `processes_iterator()` not available on OTP 26-27
**Required**: Use otp_compat macros

#### `apps/erlmcp_core/src/erlmcp_graceful_drain.erl`
**Status**: ⚠️ Partial OTP 28 usage
**Issues**: Priority messages not available on OTP <28
**Required**: Use conditional priority handling

### 2. Transport Modules

#### `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`
**Status**: ✅ No version-specific code
**Required**: None

#### `apps/erlmcp_transports/src/erlmcp_transport_ws.erl`
**Status**: ⚠️ JSON encoding usage
**Issues**: Uses JSON for message framing
**Required**: Use otp_compat macros

### 3. CLI Modules

#### `apps/erlmcp_cli/src/erlmcp_cli_json_rpc.erl`
**Status**: ⚠️ JSON encoding usage
**Issues**: Direct JSON module calls
**Required**: Use otp_compat or erlmcp_json_native

## Breaking Changes Summary

### Critical Breaking Changes (OTP 26 → 28)
1. **JSON Module**: External dependency → Native (OTP 27+)
2. **Process Enumeration**: O(N) memory → O(1) memory (OTP 28+)
3. **Priority Messages**: Not available → Available (OTP 28+)
4. **Compilation**: Single version → Multi-version support needed

### Compatibility Matrix

| Module | OTP 26 | OTP 27 | OTP 28+ | Issues |
|--------|--------|--------|---------|---------|
| erlmcp_json_native | ❌ | ❌ | ✅ | OTP 27+ only |
| erlmcp_process_monitor | ❌ | ❌ | ✅ | Uses OTP 28 APIs |
| erlmcp_graceful_drain | ⚠️ | ⚠️ | ✅ | Priority messages |
| erlmcp_otp_compat | ✅ | ✅ | ✅ | No issues |
| otp_compat.hrl | ⚠️ | ✅ | ✅ | Missing platform defines |

## Required Actions

### 1. Update rebar.config
```erlang
% Add platform defines for OTP version detection
{platform_define,
    "^2[6]", 'OTP_LEGACY'},        % OTP 26
{platform_define,
    "^2[7]", 'OTP_STABLE'},        % OTP 27
{platform_define,
    "^2[8-9]|^[3-9]", 'OTP_MODERN'} % OTP 28+

% Update minimum version requirement
{minimum_otp_vsn, "26"}.
```

### 2. Fix Module Dependencies
- Make `jsx` dependency optional
- Add version guards to erlmcp_json_native.erl
- Update all JSON usage to use otp_compat macros

### 3. Update Process Enumeration Code
- Replace direct `processes_iterator()` calls with otp_compat macros
- Add fallback mechanisms for OTP 26-27
- Implement O(1) memory enumeration where available

### 4. Add Priority Message Support
- Use otp_compat macros for priority message handling
- Graceful degradation for OTP <28
- Maintain API consistency across versions

### 5. Testing Requirements
- Add Common Test suites for each OTP version
- Create version-specific test configurations
- Implement feature detection tests

## Implementation Priority

### High Priority (Critical for Functionality)
1. Fix rebar.config platform defines
2. Update erlmcp_process_monitor.erl to use otp_compat
3. Add priority message fallbacks
4. Test with OTP 26, 27, 28

### Medium Priority (Performance Optimization)
1. Update JSON usage across all modules
2. Implement O(1) process enumeration where possible
3. Add comprehensive version testing

### Low Priority (Documentation)
1. Update version compatibility documentation
2. Add migration guides
3. Create feature availability matrix

## Risk Assessment

### High Risk Modules
1. **erlmcp_json_native.erl** - Breaks on OTP 26
2. **erlmcp_process_monitor.erl** - Uses OTP 28 APIs
3. **erlmcp_graceful_drain.erl** - Priority messages not available

### Medium Risk Modules
1. All modules using JSON encoding (162 files)
2. Transport modules using process enumeration
3. CLI modules with version-specific features

### Low Risk Modules
1. Core modules using otp_compat macros
2. Test modules with version guards
3. Configuration and utility modules

## Conclusion

The erlmcp codebase requires significant updates to support OTP versions 26-28. The main challenges are:

1. **Native JSON Module** - Conditional compilation needed
2. **Process Iterators** - Version-specific implementation required
3. **Priority Messages** - Graceful degradation for older versions
4. **Platform Defines** - Missing in rebar.config

With the recommended updates, erlmcp can achieve full backward compatibility while maintaining performance optimizations available in newer OTP versions.

## Next Steps

1. Implement rebar.config updates
2. Fix high-risk modules
3. Add comprehensive testing
4. Update documentation
5. Create CI/CD pipeline for multi-version testing