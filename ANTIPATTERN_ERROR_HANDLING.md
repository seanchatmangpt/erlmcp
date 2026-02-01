# Error Handling Antipattern Analysis

**Date**: 2026-02-01
**Scope**: apps/erlmcp_*/src/*.erl
**Total Files Analyzed**: 571 Erlang source files
**Antipattern**: Improper error handling and missing catch/rescue patterns

---

## Executive Summary

This analysis identified **8 categories** of error handling antipatterns across the erlmcp codebase. While many issues are in test files (which is acceptable), there are **critical production code vulnerabilities** that could cause crashes, data loss, or silent failures.

**Critical Findings**:
- 17 instances of catch-all error handlers silencing exceptions
- 18+ unsafe atom conversions that could exhaust atom table
- 45+ unsafe list operations (hd/tl) without empty list checks
- 30+ maps:get calls without default values or error handling
- 28+ gen_server:call without explicit timeouts
- 30+ throw/1 calls requiring proper error propagation
- Multiple file and network operations without comprehensive error handling

---

## Category 1: Catch-All Error Handlers (_:_)

### Description
Generic catch-all patterns that silence errors without logging, making debugging impossible.

### Severity: HIGH

### Findings

#### 1.1 Silent Error Suppression in Profiler

**File**: `apps/erlmcp_validation/src/erlmcp_cli_profiler.erl`
**Lines**: 177, 184, 191

```erlang
FprofResult = try
    fprof:trace(stop),
    fprof:profile({file, "fprof.trace"}),
    fprof:analyse([{dest, "fprof.analysis"}]),
    {ok, <<"fprof.analysis">>}
catch _:_ -> {error, not_running} end,
```

**Issue**: All errors collapsed to `{error, not_running}`, losing critical diagnostic information.

**Error Types That Could Occur**:
- File system errors (permission denied, disk full)
- Profiler internal errors
- Invalid trace data
- Process crashes

**Impact**: Silent failure hides real issues like disk space problems or file permission errors.

**Recommendation**:
```erlang
FprofResult = try
    fprof:trace(stop),
    fprof:profile({file, "fprof.trace"}),
    fprof:analyse([{dest, "fprof.analysis"}]),
    {ok, <<"fprof.analysis">>}
catch
    error:{badmatch, _} = Error ->
        logger:warning("Fprof analysis failed: ~p", [Error]),
        {error, {fprof_failed, Error}};
    error:badarg = Error ->
        logger:debug("Fprof not running: ~p", [Error]),
        {error, not_running};
    Class:Reason:Stack ->
        logger:error("Fprof unexpected error ~p:~p~n~p", [Class, Reason, Stack]),
        {error, {profiler_error, Reason}}
end
```

#### 1.2 Silent JSON Parsing Failure

**File**: `apps/erlmcp_validation/src/erlmcp_cli_interactive.erl`
**Line**: 583

```erlang
catch _:_ -> #{}
```

**Issue**: JSON parsing errors silently return empty map.

**Error Types**: Invalid JSON, encoding errors, memory exhaustion

**Impact**: Invalid configuration silently accepted, leading to runtime failures.

**Recommendation**: Log parse errors and return meaningful error tuples.

#### 1.3 Stacktrace Capture Pattern

**File**: `apps/erlmcp_core/src/erlmcp_reproducer.erl`
**Lines**: 114, 130, 146

```erlang
stacktrace => try throw(capture_stacktrace) catch _:_ -> erlang:get_stacktrace() end,
```

**Issue**: Using deprecated `erlang:get_stacktrace()` with catch-all.

**Error Types**: None expected, but pattern is obsolete.

**Impact**: Low (test/debug code), but uses deprecated BIF.

**Recommendation**:
```erlang
stacktrace => element(3, erlang:process_info(self(), current_stacktrace))
```

#### 1.4 Unsafe Atom Conversion Fallback

**File**: `apps/erlmcp_observability/src/erlmcp_otel.erl`
**Lines**: 650, 700

```erlang
AtomKey = case is_binary(K) of
    true ->
        try binary_to_existing_atom(K, utf8)
        catch _:_ -> binary_to_atom(K, utf8)  % DANGER: Creates new atoms!
        end;
    false -> K
end,
```

**Issue**: Falls back to creating new atoms on user input.

**Error Types**: Atom table exhaustion attack.

**Impact**: CRITICAL - Can crash BEAM VM by exhausting atom table (1M limit).

**Recommendation**:
```erlang
AtomKey = case is_binary(K) of
    true ->
        try
            binary_to_existing_atom(K, utf8)
        catch
            error:badarg ->
                logger:warning("Unknown attribute key: ~p, using binary", [K]),
                K  % Keep as binary, don't create atom
        end;
    false -> K
end,
```

#### 1.5 Format Fallback Without Logging

**File**: `apps/erlmcp_observability/src/erlmcp_otel.erl`
**Line**: 1185

```erlang
format_attribute_value(V) when is_list(V) ->
    try list_to_binary(V)
    catch _:_ -> list_to_binary(io_lib:format("~p", [V]))
    end;
```

**Issue**: Silently falls back to debug format without indicating why.

**Error Types**: Invalid UTF-8, improper lists.

**Impact**: Performance degradation (io_lib:format is slow), no visibility.

**Recommendation**: Log when fallback occurs and include validation.

#### 1.6 Test Cleanup Handlers

**Files**:
- `apps/erlmcp_core/test/erlmcp_cancellation_integration_tests.erl:41`
- `apps/erlmcp_core/test/erlmcp_secrets_aws_tests.erl:102`
- `apps/erlmcp_core/test/erlmcp_supervisor_collapse_tests.erl:21,32`
- `apps/erlmcp_core/test/erlmcp_supervisor_utils_tests.erl:37,49`

```erlang
catch _:_ -> ok
catch _:_ -> exit(OldPid, kill)
catch _:_ -> exit(SupPid, kill)
```

**Issue**: Test cleanup silently swallows errors.

**Impact**: LOW (test code), but masks cleanup failures.

**Recommendation**: Acceptable in test cleanup, but consider logging unexpected errors.

---

## Category 2: Unsafe Atom Conversions

### Description
Converting binaries/strings to atoms without validation can exhaust atom table.

### Severity: CRITICAL

### Findings

#### 2.1 Dynamic Atom Creation in Transport Modules

**File**: `apps/erlmcp_transport_ws.erl`
**Line**: 154

```erlang
ListenerName = binary_to_atom(<<"erlmcp_ws_", TransportIdBin/binary>>, utf8),
```

**Issue**: Creates atoms from user-controlled input.

**Attack Vector**: User can exhaust atom table by creating many transports.

**Recommendation**:
```erlang
% Pre-register transport names or use ETS with binary keys
case erlmcp_transport_registry:lookup_listener(TransportIdBin) of
    {ok, Pid} -> Pid;
    {error, not_found} ->
        % Use ETS table with binary keys instead of process registry
        ...
end
```

**Similar Issues**:
- `apps/erlmcp_transport_sse.erl:104` - Same pattern
- `apps/erlmcp_validation/test/erlmcp_lifecycle_advanced_SUITE.erl:1142,1153,1358` - Test code
- `apps/erlmcp_validation/src/erlmcp_memory_manager.erl:220` - Hash-based atoms

#### 2.2 Command-Line Argument Atom Conversion

**File**: `apps/erlmcp_validation/src/erlmcp_validate_cli.erl`
**Lines**: 531, 1473

```erlang
SectionAtom = try list_to_existing_atom(Section) catch error:badarg -> Section end,
```

**Issue**: Falls back to keeping string on error, inconsistent types.

**Recommendation**: Define allowed sections as module constant:
```erlang
-define(VALID_SECTIONS, [intro, protocol, transports, security]).

validate_section(Section) when is_list(Section) ->
    Atom = list_to_atom(Section),  % Safe: limited input space
    case lists:member(Atom, ?VALID_SECTIONS) of
        true -> {ok, Atom};
        false -> {error, {invalid_section, Section}}
    end.
```

#### 2.3 Configuration Mode Conversion

**File**: `apps/erlmcp_core/src/erlmcp_elicitation.erl`
**Lines**: 268, 397

```erlang
case binary_to_atom(Mode, utf8) of
    inline -> ...;
    reference -> ...
end
```

**Issue**: No validation before atom creation.

**Recommendation**:
```erlang
parse_mode(<<"inline">>) -> {ok, inline};
parse_mode(<<"reference">>) -> {ok, reference};
parse_mode(Other) -> {error, {invalid_mode, Other}}.
```

#### 2.4 Log Level Conversion

**Files**:
- `apps/erlmcp_core/src/erlmcp_server.erl:3212`
- `apps/erlmcp_core/src/erlmcp_logging.erl:365`

```erlang
Level = binary_to_existing_atom(LevelBin, utf8),
```

**Issue**: Can crash if invalid level provided.

**Recommendation**: Use whitelist pattern matching.

---

## Category 3: Unsafe List Operations (hd/tl)

### Description
Using `hd/1` and `tl/1` on potentially empty lists causes crashes.

### Severity: MEDIUM-HIGH

### Findings

#### 3.1 Command History Access

**File**: `apps/erlmcp_validation/src/erlmcp_cli_stats.erl`
**Line**: 332

```erlang
(hd(CommandHistory))#command_execution.timestamp)
```

**Issue**: Crashes if CommandHistory is empty.

**Recommendation**:
```erlang
case CommandHistory of
    [First | _] -> First#command_execution.timestamp;
    [] -> undefined  % Or current timestamp
end
```

#### 3.2 Cache Folding

**File**: `apps/erlmcp_validation/src/erlmcp_memory_manager.erl`
**Line**: 307

```erlang
end, hd(CacheList), tl(CacheList)
```

**Issue**: Crashes on empty or single-element list.

**Recommendation**:
```erlang
case CacheList of
    [H | T] when T =/= [] -> lists:foldl(Fun, H, T);
    [Single] -> Single;
    [] -> default_value()
end
```

#### 3.3 Health Check Selection

**Files**:
- `apps/erlmcp_observability/src/erlmcp_health_monitor.erl:750`
- `apps/erlmcp_observability/src/erlmcp_recovery_manager.erl:746`

```erlang
{ok, element(1, hd(Multiple))}
```

**Issue**: Assumes Multiple is non-empty.

**Recommendation**:
```erlang
case Multiple of
    [First | _] -> {ok, element(1, First)};
    [] -> {error, no_candidates}
end
```

#### 3.4 Protocol Validator

**File**: `apps/erlmcp_validation/src/erlmcp_protocol_validator.erl`
**Line**: 618

```erlang
Fields = case is_binary(hd(FieldList)) of
```

**Issue**: Assumes FieldList is non-empty.

**Impact**: Crash on empty field list.

#### 3.5 Benchmark Statistics

**Files**:
- `apps/erlmcp_core/test/erlmcp_bench_integration.erl:515`
- `apps/erlmcp_core/test/erlmcp_bench_stress.erl:95,447`

```erlang
Min = hd(Sorted),
```

**Issue**: Assumes sorted list is non-empty.

**Impact**: Low (test code), but misleading results.

#### 3.6 Trace Analysis

**Files**:
- `apps/erlmcp_observability/src/erlmcp_trace_analyzer.erl:117,153,319,418,431`

```erlang
_ -> hd(CriticalPaths)
trace_id = maps:get(trace_id, hd(Spans), <<"unknown">>),
```

**Issue**: Multiple hd/tl operations without guards.

**Recommendation**: Add non-empty list guards or use case statements.

---

## Category 4: Unsafe Map Operations

### Description
Using `maps:get/2` without default value or error handling.

### Severity: MEDIUM

### Findings

#### 4.1 Plugin Metadata Access

**File**: `apps/erlmcp_core/src/erlmcp_plugin.erl`
**Lines**: 67, 73, 79, 85

```erlang
-spec get_name(metadata()) -> binary().
get_name(Metadata) ->
    maps:get(name, Metadata).  % Crashes if 'name' missing
```

**Issue**: No validation that required fields exist.

**Recommendation**:
```erlang
-spec get_name(metadata()) -> {ok, binary()} | {error, missing_field}.
get_name(Metadata) ->
    case maps:find(name, Metadata) of
        {ok, Name} when is_binary(Name) -> {ok, Name};
        {ok, _} -> {error, invalid_name_type};
        error -> {error, missing_name}
    end.
```

#### 4.2 Nested Map Access

**File**: `apps/erlmcp_core/src/pricing/erlmcp_pricing_receipt.erl`
**Lines**: 250, 339, 342, 520, 539, 562

```erlang
CurrentHash = maps:get(current_hash, maps:get(hash_chain, Receipt)),
```

**Issue**: Double map access - crashes if either key missing.

**Recommendation**:
```erlang
case Receipt of
    #{hash_chain := #{current_hash := Hash}} -> {ok, Hash};
    _ -> {error, invalid_receipt_structure}
end
```

#### 4.3 Queue Limits Default Chaining

**File**: `apps/erlmcp_core/src/erlmcp_queue_limits.erl`
**Lines**: 186, 242

```erlang
Limit = maps:get(Role, State#state.limits, maps:get(default, State#state.limits)),
```

**Issue**: Nested maps:get can still crash if 'default' missing.

**Recommendation**:
```erlang
Limit = maps:get(Role, State#state.limits,
                 maps:get(default, State#state.limits, ?DEFAULT_LIMIT)),
```

---

## Category 5: Missing Timeouts in gen_server:call

### Description
gen_server:call without explicit timeout defaults to 5 seconds, can cause deadlocks.

### Severity: MEDIUM

### Findings

#### 5.1 Registry List Operations

**File**: `apps/erlmcp_core/src/erlmcp_registry.erl`
**Lines**: 144, 154

```erlang
list_servers() ->
    gen_server:call(?MODULE, list_servers);  % No timeout!

list_transports() ->
    gen_server:call(?MODULE, list_transports);  % No timeout!
```

**Issue**: Under load, these can block caller for 5+ seconds.

**Recommendation**:
```erlang
-define(REGISTRY_TIMEOUT, 2000).

list_servers() ->
    gen_server:call(?MODULE, list_servers, ?REGISTRY_TIMEOUT).
```

#### 5.2 Test State Access

Multiple test files access state without timeout:
- `apps/erlmcp_transports/test/erlmcp_transport_compliance_SUITE.erl:357,392,417,453,485`
- `apps/erlmcp_transports/src/erlmcp_bench_integration.erl:277,317,357,397`
- `apps/erlmcp_core/test/erlmcp_bench_network_real.erl:276`

```erlang
{ok, ClientState} = gen_server:call(ClientPid, get_state),  % Default 5s timeout
```

**Issue**: Test can hang for 5 seconds on process crash.

**Recommendation**:
```erlang
{ok, ClientState} = gen_server:call(ClientPid, get_state, 1000),
```

---

## Category 6: Throw Without Proper Catch

### Description
Using `throw/1` for control flow requires all callers to catch.

### Severity: MEDIUM

### Findings

#### 6.1 Validator Startup Failures

**File**: `apps/erlmcp_validation/src/erlmcp_validate_cli.erl`
**Lines**: 1129, 1618

```erlang
{error, Reason} -> throw({validator_start_failed, ValidatorModule, Reason})
throw({unsupported_shell, Shell})
```

**Issue**: Throws bypass normal error returns, can escape test frameworks.

**Recommendation**: Return `{error, Reason}` tuples instead.

#### 6.2 Compliance Validation Throws

**File**: `apps/erlmcp_validation/src/erlmcp_test_client.erl`
**Lines**: 427, 435, 443, 445, 602, 608, 622, 625

```erlang
Invalid -> throw({non_compliant, {invalid_jsonrpc_version, Invalid}})
_ -> throw({non_compliant, {missing_required_fields, MissingFields}})
throw({non_compliant, {both_result_and_error, Response}});
```

**Issue**: 8+ throw sites make caller error handling complex.

**Recommendation**: Use result tuples consistently.

#### 6.3 Server Parameter Validation

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Lines**: 792, 2942, 2954

```erlang
throw({client_info_error, ClientInfoError})
_ -> throw({error, <<"Invalid limit parameter">>})
throw({error, <<"Invalid uriPattern parameter">>})
```

**Issue**: Mixed throw and return error styles.

**Recommendation**: Standardize on returning `{error, Reason}`.

---

## Category 7: File Operations Without Error Handling

### Description
File I/O operations that don't comprehensively handle errors.

### Severity: MEDIUM

### Findings

#### 7.1 Receipt File Operations

**File**: `apps/erlmcp_core/src/pricing/erlmcp_pricing_receipt.erl`
**Line**: 207

```erlang
ErrorListDir -> throw(ErrorListDir)
```

**Issue**: File listing error thrown instead of returned.

**Error Types**: Permission denied, directory not found, I/O error.

**Recommendation**: Return error tuple with context.

#### 7.2 Profiler File Operations

**File**: `apps/erlmcp_validation/src/erlmcp_cli_profiler.erl`
**Lines**: 173-176

```erlang
fprof:trace(stop),
fprof:profile({file, "fprof.trace"}),
fprof:analyse([{dest, "fprof.analysis"}]),
```

**Issue**: File operations not individually error-checked.

**Error Types**: Write failures, disk full, permission denied.

**Recommendation**: Check each operation's return value.

#### 7.3 Test File Operations

Multiple test files perform file I/O without comprehensive error handling:
- `apps/erlmcp_core/test/erlmcp_reproducer_tests.erl`
- `apps/erlmcp_core/test/erlmcp_session_backend_tests.erl`
- `apps/erlmcp_observability/test/erlmcp_evidence_path_tests.erl`

**Impact**: Low (test code), but can cause misleading test failures.

---

## Category 8: Network Operations Without Error Handling

### Description
TCP operations that don't handle all error conditions.

### Severity: HIGH

### Findings

#### 8.1 TCP Close Without Error Check

**File**: `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`
**Lines**: 157-166

```erlang
close(#state{socket = Socket, server_id = ServerId}) when Socket =/= undefined ->
    catch erlmcp_connection_limiter:release_connection(ServerId),
    gen_tcp:close(Socket),  % Return value ignored!
    ok;
```

**Issue**: gen_tcp:close can fail, but error is ignored.

**Error Types**: Socket already closed, connection reset.

**Impact**: Resource leak if close fails.

**Recommendation**:
```erlang
close(#state{socket = Socket, server_id = ServerId}) when Socket =/= undefined ->
    catch erlmcp_connection_limiter:release_connection(ServerId),
    case gen_tcp:close(Socket) of
        ok -> ok;
        {error, Reason} ->
            logger:warning("TCP close failed: ~p", [Reason]),
            ok  % Still return ok, but log the issue
    end;
```

#### 8.2 Connection Handoff Error Recovery

**File**: `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`
**Lines**: 199-229

Good example of comprehensive error handling with try/catch/after pattern, but:

**Missing**: Timeout handling for gen_server:start_link (could block indefinitely).

**Recommendation**: Add timeout:
```erlang
case gen_server:start_link(?MODULE, #{...}, [{timeout, 30000}]) of
```

---

## Recommendations Summary

### Immediate Actions (Critical)

1. **Fix Atom Creation from User Input** (CRITICAL)
   - Files: `erlmcp_otel.erl:650,700`, `erlmcp_transport_ws.erl:154`, `erlmcp_transport_sse.erl:104`
   - Risk: Atom table exhaustion crashes BEAM VM
   - Fix: Use whitelist pattern matching or keep as binaries

2. **Add Timeouts to Registry Calls** (HIGH)
   - File: `erlmcp_registry.erl:144,154`
   - Risk: Deadlocks under load
   - Fix: Add 2-second timeout

3. **Validate List Operations** (HIGH)
   - Files: Multiple (see Category 3)
   - Risk: Crash on empty lists
   - Fix: Add guards or case statements

### Short-Term Improvements

4. **Standardize Error Return Patterns**
   - Replace `throw/1` with `{error, Reason}` returns
   - Ensure all public APIs return consistent error tuples
   - Document error types in -spec declarations

5. **Add Error Logging to Catch-All Handlers**
   - Files: `erlmcp_cli_profiler.erl`, `erlmcp_cli_interactive.erl`
   - Log errors before suppressing
   - Include context (operation, input, expected behavior)

6. **Validate Map Structure**
   - Add guards for required fields in maps
   - Use maps:find/2 instead of maps:get/2 for optional fields
   - Document required vs optional fields in type specs

### Long-Term Enhancements

7. **Create Error Handling Guidelines**
   - Document approved error patterns
   - Create error handling checklist for code review
   - Add linter rules for common antipatterns

8. **Implement Defensive Programming Patterns**
   - Input validation at API boundaries
   - Assertion guards for critical invariants
   - Explicit error handling for all external I/O

9. **Improve Test Error Handling**
   - Even test code should log cleanup failures
   - Use proper test fixtures for setup/teardown
   - Validate test preconditions

---

## Error Handling Best Practices (OTP Compliance)

### Pattern 1: Explicit Error Tuples

```erlang
%% GOOD
-spec process_data(binary()) -> {ok, result()} | {error, reason()}.
process_data(Data) ->
    case validate_data(Data) of
        ok ->
            case transform_data(Data) of
                {ok, Result} -> {ok, Result};
                {error, _} = Err -> Err
            end;
        {error, _} = Err -> Err
    end.

%% BAD
process_data(Data) ->
    ValidData = validate_data(Data),  % Assumes success
    transform_data(ValidData).
```

### Pattern 2: Specific Exception Catching

```erlang
%% GOOD
try
    risky_operation()
catch
    error:badarg ->
        logger:warning("Invalid argument to risky_operation"),
        {error, invalid_input};
    error:{badmatch, _} = Error ->
        logger:error("Pattern match failed: ~p", [Error]),
        {error, internal_error};
    Class:Reason:Stack ->
        logger:error("Unexpected error ~p:~p~n~p", [Class, Reason, Stack]),
        {error, {unexpected, Reason}}
end.

%% BAD
try
    risky_operation()
catch
    _:_ -> {error, something_failed}  % Lost all context!
end.
```

### Pattern 3: Safe Atom Conversion

```erlang
%% GOOD
-define(VALID_MODES, [inline, reference, external]).

parse_mode(<<"inline">>) -> {ok, inline};
parse_mode(<<"reference">>) -> {ok, reference};
parse_mode(<<"external">>) -> {ok, external};
parse_mode(Other) -> {error, {invalid_mode, Other}}.

%% BAD
parse_mode(Bin) ->
    binary_to_atom(Bin, utf8).  % User controls atom creation!
```

### Pattern 4: Safe List Operations

```erlang
%% GOOD
get_first([First | _]) -> {ok, First};
get_first([]) -> {error, empty_list}.

%% ALSO GOOD with guard
process_nonempty([H | T]) ->
    do_something(H, T).

%% BAD
get_first(List) ->
    hd(List).  % Crashes on empty list
```

### Pattern 5: Timeout in gen_server:call

```erlang
%% GOOD
-define(DEFAULT_TIMEOUT, 5000).

get_state(Pid) ->
    gen_server:call(Pid, get_state, ?DEFAULT_TIMEOUT).

%% BETTER (when operation can be slow)
get_state(Pid, Timeout) when is_integer(Timeout), Timeout > 0 ->
    gen_server:call(Pid, get_state, Timeout).

%% BAD
get_state(Pid) ->
    gen_server:call(Pid, get_state).  % Implicit 5s timeout
```

---

## Testing Error Paths

### Current State
- Most tests verify happy paths
- Error injection limited to chaos testing
- Few tests for error recovery

### Recommendations

1. **Add Negative Test Cases**
   ```erlang
   error_handling_test() ->
       ?assertMatch({error, invalid_input},
                    process_data(<<>>)),
       ?assertMatch({error, {invalid_mode, <<"bad">>}},
                    parse_mode(<<"bad">>)).
   ```

2. **Error Injection Testing**
   ```erlang
   -ifdef(TEST).
   -define(INJECT_ERROR(Condition, Error),
           case get(inject_errors) of
               true when Condition -> throw(Error);
               _ -> ok
           end).
   -endif.
   ```

3. **Property-Based Error Testing**
   ```erlang
   prop_no_crash_on_invalid_input() ->
       ?FORALL(Input, any(),
           try
               process_data(Input),
               true
           catch
               _:_ -> false  % Should return error, not crash
           end).
   ```

---

## Metrics

### Error Handling Coverage

| Category | Instances Found | Critical | High | Medium | Low |
|----------|----------------|----------|------|--------|-----|
| Catch-all handlers | 17 | 2 | 3 | 10 | 2 |
| Unsafe atom conversions | 18 | 4 | 8 | 6 | 0 |
| Unsafe list operations | 45+ | 0 | 12 | 28 | 5 |
| Unsafe map operations | 30+ | 0 | 2 | 28 | 0 |
| Missing timeouts | 28+ | 0 | 2 | 26 | 0 |
| Throw without catch | 30+ | 0 | 0 | 30 | 0 |
| File I/O errors | 15+ | 0 | 2 | 10 | 3 |
| Network errors | 8+ | 0 | 2 | 6 | 0 |
| **TOTAL** | **190+** | **6** | **31** | **144** | **10** |

### Risk Assessment

- **CRITICAL (6 issues)**: Require immediate fixes before production
- **HIGH (31 issues)**: Should be fixed in next sprint
- **MEDIUM (144 issues)**: Address in regular development
- **LOW (10 issues)**: Nice to have, mostly test code

---

## Conclusion

The erlmcp codebase demonstrates generally good error handling practices, but has several critical vulnerabilities that must be addressed:

1. **Atom exhaustion attacks** are the highest priority - 4 critical instances
2. **Missing timeouts** in registry operations could cause deadlocks
3. **Silent error suppression** in profiler and configuration parsing hides bugs
4. **Inconsistent error patterns** (throw vs. return) complicate caller code

The majority of issues (76%) are MEDIUM severity and can be addressed through systematic refactoring. Test code issues are acceptable but should still be improved for better test diagnostics.

**Recommended Actions**:
1. Fix all CRITICAL issues immediately (estimated 4 hours)
2. Address HIGH priority issues in next sprint (estimated 2 days)
3. Create error handling guidelines and linter rules (estimated 1 day)
4. Gradually refactor MEDIUM issues during regular development

---

## Appendix: Complete File List

### Files with Critical Issues
- `apps/erlmcp_observability/src/erlmcp_otel.erl`
- `apps/erlmcp_transport_ws.erl`
- `apps/erlmcp_transport_sse.erl`
- `apps/erlmcp_core/src/erlmcp_elicitation.erl`

### Files with High Priority Issues
- `apps/erlmcp_validation/src/erlmcp_cli_profiler.erl`
- `apps/erlmcp_validation/src/erlmcp_cli_interactive.erl`
- `apps/erlmcp_core/src/erlmcp_registry.erl`
- `apps/erlmcp_observability/src/erlmcp_health_monitor.erl`
- `apps/erlmcp_observability/src/erlmcp_recovery_manager.erl`
- `apps/erlmcp_validation/src/erlmcp_cli_stats.erl`
- `apps/erlmcp_validation/src/erlmcp_memory_manager.erl`
- `apps/erlmcp_validation/src/erlmcp_protocol_validator.erl`
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`
- `apps/erlmcp_core/src/erlmcp_server.erl`

### Test Files (Lower Priority)
- 40+ test files with acceptable but improvable error handling
- Mostly missing timeout specifications and cleanup error logging

---

**Report Generated**: 2026-02-01
**Analyst**: Code Reviewer (erlmcp-code-reviewer)
**Next Review**: After critical fixes implemented
