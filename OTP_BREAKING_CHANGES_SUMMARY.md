# OTP Breaking Changes Summary: 26, 27, 28

## Quick Reference

This document provides a concise overview of breaking changes between OTP versions and specific module requirements for erlmcp compatibility.

## Critical Breaking Changes

### 1. JSON Module (OTP 27+)

**Change**: Native `json` module introduced in OTP 27
**Impact**: 162 files use JSON encoding/decoding
**Solution**: Use `otp_compat` macros for automatic selection

**Code Pattern**:
```erlang
% Before (OTP specific)
json:encode(Data)  % OTP 27+
jsx:encode(Data)   % OTP 26

% After (version agnostic)
?JSON_ENCODE(Data)  % Automatically selects appropriate module
```

### 2. Process Iterators (OTP 28+)

**Change**: `erlang:processes_iterator()` for O(1) memory enumeration
**Impact**: 15 files use process enumeration
**Solution**: Use `otp_compat` macros with fallback

**Code Pattern**:
```erlang
% Before (OTP 28 specific)
Iterator = erlang:processes_iterator()
enumerate_processes_iterator(Iterator, 0)

% After (version agnostic)
?SAFE_PROCESSES()  % Handles both O(1) and O(N) methods
```

### 3. Priority Messages (OTP 28+)

**Change**: `process_flag(priority, high)` and `{priority, high}` send option
**Impact**: Critical system components need preemption
**Solution**: Use `otp_compat` macros with graceful degradation

**Code Pattern**:
```erlang
% Before (OTP 28 specific)
process_flag(priority, high)
erlang:send(Pid, Msg, [nosuspend, {priority, high}])

% After (version agnostic)
?SET_PRIORITY_HIGH()
?SEND_PRIORITY(Pid, Msg)
```

## Module Update Requirements

### High Priority (Break Without Updates)

#### 1. `include/otp_compat.hrl`
**Status**: ✅ Has features but missing platform defines
**Issue**: `OTP_28_PLUS` macro not defined in rebar.config
**Fix**: Add platform defines to rebar.config

#### 2. `apps/erlmcp_core/src/erlmcp_json_native.erl`
**Status**: ❌ OTP 27+ only
**Issue**: Will not compile on OTP 26
**Fix**: Add version guards and conditional compilation

#### 3. `apps/erlmcp_observability/src/erlmcp_process_monitor.erl`
**Status**: ❌ Uses OTP 28 features directly
**Issue**: `processes_iterator()` not available on OTP 26-27
**Fix**: Use `otp_compat` macros for process enumeration

#### 4. `apps/erlmcp_core/src/erlmcp_graceful_drain.erl`
**Status**: ⚠️ Partial priority message support
**Issue**: Priority messages not available on OTP <28
**Fix**: Use `otp_compat` macros for graceful degradation

### Medium Priority (Performance Impact)

#### All JSON Usage (162 files)
**Pattern**: Direct `json:` or `jsx:` calls
**Fix**: Use `?JSON_ENCODE()` and `?JSON_DECODE()` macros

#### Process Enumeration (15 files)
**Pattern**: Direct `erlang:processes()` calls
**Fix**: Use `?SAFE_PROCESSES()` and `?SAFE_PROCESS_COUNT()` macros

### Low Priority (Documentation)

#### Transport and CLI modules
**Pattern**: Version-specific hardcoding
**Fix**: Use version detection and conditional logic

## Version Compatibility Matrix

| Feature | OTP 26 | OTP 27 | OTP 28+ | Implementation Status |
|---------|--------|--------|---------|----------------------|
| Native JSON | ❌ | ✅ | ✅ | ⚠️ Partial |
| Process Iterator | ❌ | ❌ | ✅ | ❌ Missing |
| Priority Messages | ❌ | ❌ | ✅ | ⚠️ Partial |
| OTP Compilability | ✅ | ✅ | ✅ | ❌ Not Ready |
| Graceful Degradation | ✅ | ✅ | ✅ | ❌ Not Ready |

## Immediate Actions Required

### 1. Fix rebar.config (30 minutes)
```erlang
% Add to rebar.config
{platform_define,
    "^2[6-9]|^[3-9]", 'OTP_28_PLUS'}.
```

### 2. Update erlmcp_json_native.erl (1 hour)
```erlang
% Add version guards and use otp_compat
-include("otp_compat.hrl").

encode(Term) ->
    ?JSON_ENCODE(Term).

decode(Binary) ->
    ?JSON_DECODE(Binary).
```

### 3. Update erlmcp_process_monitor.erl (2 hours)
```erlang
% Replace direct iterator calls
enumerate_processes() ->
    ?SAFE_PROCESSES().
```

### 4. Update erlmcp_graceful_drain.erl (1 hour)
```erlang
% Use priority message macros
?SET_PRIORITY_HIGH()
?SEND_PRIORITY(Pid, Msg)
```

## Testing Requirements

### Before Implementation
1. **Current State**: Test compilation on OTP 28
2. **Baseline**: Establish performance benchmarks
3. **Feature Detection**: Verify `otp_compat` features work

### After Implementation
1. **Multi-Version Testing**: Test on OTP 26, 27, 28
2. **Performance Testing**: Compare process enumeration performance
3. **Regression Testing**: Ensure no functionality loss

### Test Commands
```bash
# Test current OTP 28 support
make check

# Test after implementation
rebar3 compile -DOTP_26
rebar3 compile -DOTP_27
rebar3 compile -DOTP_28_PLUS

# Run comprehensive tests
rebar3 ct --suite=erlmcp_cross_otp_compat_SUITE
```

## Risk Assessment

### High Risk Items
1. **JSON Native Module**: Breaks on OTP 26
2. **Process Iterator**: Memory issues on OTP 26-27
3. **Priority Messages**: System behavior changes

### Mitigation Strategies
1. **Gradual Rollout**: Implement features with version detection
2. **Fallback Systems**: Maintain backward compatibility
3. **Comprehensive Testing**: Multi-version test matrix
4. **Performance Monitoring**: Continuous benchmarks

## Success Metrics

### Short Term (1 week)
- [ ] All modules compile on OTP 26, 27, 28
- [ ] Basic functionality works on all versions
- [ ] No breaking changes introduced

### Medium Term (2 weeks)
- [ ] Performance optimizations working on OTP 28
- [ ] Graceful degradation on OTP 26-27
- [ ] Comprehensive test coverage

### Long Term (1 month)
- [ ] Production-ready multi-version support
- [ ] Complete documentation
- [ ] CI/CD pipeline for multi-version testing

## Common Issues and Solutions

### Issue 1: Compilation Failures on OTP 26
**Solution**: Add version guards around OTP 28+ features
```erlang
-ifdef(OTP_28_PLUS).
use_process_iterator() ->
    erlang:processes_iterator().
-else.
use_process_iterator() ->
    erlang:processes().
-endif.
```

### Issue 2: Performance Degradation on OTP 26-27
**Solution**: Document performance impact and provide optimization guidance
- Process enumeration: O(N) vs O(1) memory
- JSON encoding: Native vs external library

### Issue 3: API Inconsistency
**Solution**: Use otp_compat macros consistently
- Never call OTP-specific functions directly
- Always use version detection macros

## Resources

### Documentation
- `OTP_VERSION_BREAKING_CHANGES_ANALYSIS.md` - Detailed analysis
- `MODULE_UPDATE_REQUIREMENTS.md` - Implementation guide
- `docs/VERSION_COMPATIBILITY.md` - User guide

### Code References
- `include/otp_compat.hrl` - Compatibility layer
- `apps/erlmcp_core/src/erlmcp_otp_compat.erl` - Feature detection
- Test suites in `apps/*/test/`

### Tools
- `rebar3` with platform defines
- Common Test for multi-version testing
- Dialyzer for type safety

## Conclusion

The erlmcp codebase requires updates to support OTP versions 26-28. The main challenges are:

1. **JSON Module**: Conditional compilation needed
2. **Process Iterators**: Version-specific implementation required
3. **Priority Messages**: Graceful degradation needed

With the recommended updates, erlmcp can achieve full backward compatibility while maintaining performance optimizations available in newer OTP versions.

**Next Steps**: Start with rebar.config updates and high-priority module fixes.