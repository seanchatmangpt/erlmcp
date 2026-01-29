# Research: Fix Transport Behavior Suite Validation Failure

**Date**: 2025-01-21
**Item**: 041-fix-transport-behavior-suite-validation-failure
**Section**: ct-fixes
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
Transport behavior validation test failing - expected error but got ok. Need to determine if validation logic is wrong or test expectation is wrong.

**Motivation:** Transport behavior compliance tests ensure all transports implement the behavior correctly. This validation failure indicates either broken validation or wrong test.

**Success criteria:**
- Root cause documented (validation logic or test expectation)
- Fix applied (validation or test)
- stdio_opts_validation test passes
- All transport behavior tests pass
- rebar3 ct --suite=erlmcp_transport_behavior_SUITE executes

**Technical constraints:**
- Option A: Fix the test (validation is correct)
- Option B: Fix the validation (test is correct)

**Signals:** priority: critical, urgency: P0 - BLOCKS INTEGRATION TESTING

### Quality Gate Status
- **Gate Type**: Test (Common Test)
- **Current State**: Test failure - stdio_opts_validation expects error but gets ok for invalid options
- **Target State**: 100% test pass rate (0 failures)
- **Gap**: 1 test case failing out of 30+ test cases in the suite

## Summary

**Manufacturing Objective**: Fix the transport behavior validation logic to properly reject invalid/unknown configuration options while accepting valid optional options. This is a **Poka-yoke** (mistake-proofing) issue - the validation gate should catch configuration errors early.

**Technical Approach**: The root cause is that `validate_transport_opts/2` in `erlmcp_transport_behavior.erl` only checks for required fields but does NOT validate that optional fields are known/allowed. The test `stdio_opts_validation` expects strict validation (reject unknown options), but the implementation uses loose validation (ignore unknown options).

**TCPS Justification**: This follows the **Jidoka** principle - built-in quality with stop-the-line authority. When a transport is configured with invalid options, it MUST fail fast with a clear error message rather than silently accepting incorrect configuration. The validation is a quality gate that prevents misconfigured transports from entering the system.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_behavior.erl:515-538` - Validation logic
  - `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl:309-331` - Failing test
  - `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_stdio.erl:28-35` - stdio state record
  - `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:22-42` - TCP type specs
- **Patterns**:
  - Transport modules use maps for configuration options
  - Required fields use `:=` in type specs
  - Optional fields use `=>` in type specs
  - Validation currently only checks required fields
- **Tests**:
  - 30+ test cases in erlmcp_transport_behavior_SUITE
  - 1 failing: stdio_opts_validation (line 309-331)
  - Test expects: `#{owner => self(), invalid_option => true}` should be rejected
  - Actual: Validation returns `ok` (ignores unknown option)
- **Quality**:
  - Gate Status: FAIL - test expects error but gets ok
  - Root cause: Missing strict validation for unknown options

### Key Files
- `apps/erlmcp_transports/src/erlmcp_transport_behavior.erl:515-538` - **Current validation logic** (loose validation)
  ```erlang
  validate_transport_opts(stdio, Opts) when is_map(Opts) ->
      case maps:get(owner, Opts, undefined) of
          Pid when is_pid(Pid) -> ok;  % Missing strict check for unknown options
          undefined -> {error, {invalid_opts, missing_owner}};
          _ -> {error, {invalid_opts, invalid_owner_type}}
      end;
  ```
- `apps/erlmcp_transports/test/erlmcp_transport_behavior_SUITE.erl:309-331` - **Failing test expectation**
  - Line 313-314: Expects `test_mode` to be accepted (valid optional option)
  - Line 325: Expects `invalid_option => true` to be rejected (unknown option)
- `apps/erlmcp_transports/src/erlmcp_transport_stdio.erl:28-35` - **stdio state record**
  - `owner :: pid()` - Required (from init/1 parameter)
  - `test_mode = false :: boolean()` - Optional
  - `transport_id :: atom() | binary() | undefined` - Optional
  - `max_message_size :: pos_integer()` - Computed from app config
- `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl:22-42` - **TCP type specs** (example of comprehensive optional fields)
  - Required: `mode := mode()`
  - Optional: host, port, owner, server_id, transport_id, connect_timeout, keepalive, nodelay, buffer_size, num_acceptors, max_connections, max_reconnect_attempts, use_pool, pool_name, pool_min_size, pool_max_size, pool_strategy

### OTP Patterns Observed
- **Behavior**: erlmcp_transport_behavior (callback module, not a gen_server)
- **Supervision**: N/A (behavior module, not a process)
- **Process Pattern**: Transport modules are gen_servers that implement the behavior
- **Test Pattern**: Chicago School TDD - uses real processes, validates behavior compliance
- **Type Safety**: Uses -type specs with `:=` for required fields and `=>` for optional fields

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - erlmcp_transport_behavior (behavior definition and validation)
  - erlmcp_transport_stdio (stdio implementation)
  - erlmcp_transport_tcp (TCP implementation)
  - erlmcp_transport_http (HTTP implementation)
  - erlmcp_transport_ws (WebSocket implementation)
  - erlmcp_transport_sse (SSE implementation)
- **External Libraries**:
  - No external deps for validation logic (pure Erlang)
- **OTP Applications**:
  - kernel (maps, lists)
  - stdlib (stdlib)

### TCPS Quality Gates to Pass
- [ ] Compilation: 0 errors
- [ ] EUnit: 100% pass rate
- [ ] Common Test: 100% pass rate (currently FAILING)
- [ ] Coverage: ≥80%
- [ ] Dialyzer: 0 warnings
- [ ] Xref: 0 undefined function calls
- [ ] Performance: N/A (validation is negligible overhead)

### Patterns to Follow
- **Validation Pattern**: Whitelist-based validation
  - Define known optional fields per transport type
  - Check that all keys in Opts map are either required or known optional
  - Reject any unknown/invalid keys
- **Type Spec Pattern**: Use `-type` specs to document valid options
  - Required: `Key := Type`
  - Optional: `Key => Type`
  - Validation should match the type spec
- **Error Handling Pattern**: Return `{error, {invalid_opts, Reason}}`
  - `{invalid_opts, missing_owner}` - Missing required field
  - `{invalid_opts, invalid_owner_type}` - Wrong type for required field
  - `{invalid_opts, unknown_option, Key}` - Unknown option (NEEDS TO BE ADDED)

## Root Cause Analysis (5 Whys)

**Problem**: Test `stdio_opts_validation` at line 325 fails - expects `{error, _}` but gets `ok` for options with `invalid_option => true`.

1. **Why?** The validation function `validate_transport_opts(stdio, Opts)` only checks for the `owner` field but does not check for unknown/invalid options.

2. **Why?** The validation logic was designed with loose validation (ignore unknown options) rather than strict validation (reject unknown options).

3. **Why?** The validation logic does not have a whitelist of known optional options for each transport type.

4. **Why?** The transport behavior module documents optional options in type specs (e.g., `test_mode => boolean()`) but the validation logic doesn't use these specs to enforce strict validation.

5. **ROOT CAUSE**: Missing strict validation implementation - `validate_transport_opts/2` needs to check that ALL keys in the Opts map are either required or known optional fields, and reject any unknown keys.

**Solution**: Implement strict validation by:
1. Define whitelist of known optional options for each transport type
2. After checking required fields, verify no unknown keys exist
3. Return `{error, {invalid_opts, {unknown_option, Key}}}` for unknown keys

**Implementation locations**:
- `erlmcp_transport_behavior.erl:516-521` - Add strict check for stdio
- `erlmcp_transport_behavior.erl:682-723` - Add strict check for TCP
- `erlmcp_transport_behavior.erl:728-750` - Add strict check for HTTP
- `erlmcp_transport_behavior.erl:755-777` - Add strict check for WebSocket

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| Breaking existing valid code that uses unknown options | P1 | Transports that previously accepted unknown options will now fail | Audit all transport usages to ensure they only use documented options; add deprecation period if needed |
| Incomplete whitelist causes false positives | P2 | Valid optional options get rejected | Carefully review each transport's implementation to find ALL accepted options (use grep for `maps:get` in init functions) |
| Performance regression from validation overhead | P3 | Increased validation time on transport startup | Validation is O(N) where N is number of options; typical options < 10, negligible overhead |
| Test expectations wrong (loose vs strict validation) | P2 | If tests are wrong, fixing validation will require test updates | Review test expectations against TCPS principles (Jidoka = fail fast); current tests are correct expecting strict validation |

**Severity Definitions:**
- **P0 (Critical):** BLOCKS production - MUST fix immediately
- **P1 (High):** Major quality gap - MUST fix before release
- **P2 (Medium):** Important but not blocking
- **P3 (Low):** Nice-to-have

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Requirements with acceptance criteria
   - Validation must accept required fields: `owner` (stdio), `host+port` (tcp), `url` (http/ws)
   - Validation must accept known optional fields (whitelist)
   - Validation must reject unknown fields (strict validation)
   - Return `{error, {invalid_opts, {unknown_option, Key}}}` for unknown keys

2. **Pseudocode** - Algorithm design BEFORE coding
   ```erlang
   validate_transport_opts(stdio, Opts) ->
       Required = [owner],
       Optional = [test_mode, transport_id],
       case validate_required(Opts, Required) of
           ok -> validate_optional(Opts, Optional);
           Error -> Error
       end.

   validate_optional(Opts, AllowedKeys) ->
       UnknownKeys = maps:keys(Opts) -- AllowedKeys -- [owner],
       case UnknownKeys of
           [] -> ok;
           _ -> {error, {invalid_opts, {unknown_options, UnknownKeys}}}
       end.
   ```

3. **Architecture** - Integration points and dependencies
   - Modify `erlmcp_transport_behavior.erl` validation functions
   - No changes to transport implementations needed
   - Tests already expect strict validation (test expectations are correct)

4. **Refinement** - Chicago School TDD (tests FIRST)
   - Tests are already written and correct (line 309-331)
   - Fix validation to match test expectations
   - Run `rebar3 ct --suite=erlmcp_transport_behavior_SUITE` to verify

5. **Completion** - All quality gates passing
   - Validation logic updated
   - All transport behavior tests pass
   - Coverage remains ≥80%
   - Dialyzer clean (type specs match implementation)

**Implementation Strategy:**

**Phase 1: Add strict validation to stdio** (5 min)
- Define known optional keys: `[test_mode, transport_id]`
- After checking `owner`, verify all keys are in `[owner, test_mode, transport_id]`
- Return error for unknown keys

**Phase 2: Add strict validation to TCP** (10 min)
- Extract known optional keys from type spec (lines 22-42)
- Known optional: `[host, port, owner, server_id, transport_id, connect_timeout, keepalive, nodelay, buffer_size, num_acceptors, max_connections, max_reconnect_attempts, use_pool, pool_name, pool_min_size, pool_max_size, pool_strategy]`
- Note: `mode` has special handling (defaults to client)

**Phase 3: Add strict validation to HTTP/WebSocket** (5 min)
- HTTP: Known optional from type spec (lines 11-22 of erlmcp_transport_http.erl)
- WebSocket: Same as HTTP (both use URL-based connection)

**Phase 4: Verify and document** (5 min)
- Run full test suite
- Update documentation to reflect strict validation
- Commit with TCPS compliance notes

**Quality Validation:**
- Automated: `rebar3 ct --suite=erlmcp_transport_behavior_SUITE`
- Manual: Review test output for 100% pass rate
- Metrics: All 30+ test cases passing, 0 failures

## Open Questions
**NONE** - Research is complete:

1. ✅ Root cause identified: Missing strict validation for unknown options
2. ✅ Quality gates defined: 100% test pass rate, 0 failures
3. ✅ OTP patterns understood: Behavior module with validation callbacks
4. ✅ Test strategy clear: Tests are correct, fix validation logic
5. ✅ Risk assessment complete: P1 risk of breaking existing code, mitigated by audit
6. ✅ Implementation strategy defined: 4-phase approach with whitelisted options

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Missing strict validation
- [x] Quality gates defined (specific thresholds) - 100% test pass rate
- [x] OTP patterns understood (behaviors, supervision) - Behavior module pattern
- [x] Test strategy clear (Chicago School TDD) - Tests are correct, fix implementation
- [x] Risk assessment complete (severity P0-P3) - P1 risk documented with mitigation
- [x] No open questions (all research complete) - All unknowns resolved
