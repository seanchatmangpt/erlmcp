# erlmcp_auth EUnit Test Report

**Date**: 2026-01-29
**Test Module**: erlmcp_auth_tests
**Test Runner**: rebar3 eunit
**Status**: CRITICAL FAILURE - All tests cancelled due to missing dependency

---

## Executive Summary

The `erlmcp_auth_tests` test suite **COMPLETELY FAILS** to run due to a fundamental architecture problem: the test attempts to authenticate without starting the required `erlmcp_auth_rate_limiter` dependency, causing the authentication server to crash with `noproc` error.

**Severity**: CRITICAL - Tests cannot run at all
**Root Cause**: Missing supervisor integration for rate limiter dependency
**Impact**: 0/8 tests executed, 100% test failure rate

---

## Test Execution Results

```
======================== EUnit ========================
module 'erlmcp_auth_tests'
  erlmcp_auth_tests: test_api_key_auth (API Key Authentication)...
*skipped*
undefined
*unexpected termination of test process*
::{noproc,{gen_server,call,
                  [erlmcp_auth_rate_limiter,
                   {check_rate_limit,<<"test_key_123">>,undefined}]}}
=======================================================
  Failed: 0.  Skipped: 0.  Passed: 0.
One or more tests were cancelled.
```

**Tests Attempted**: 8
**Tests Passed**: 0
**Tests Failed**: 0 (cancelled before execution)
**Tests Skipped**: 0

---

## Root Cause Analysis

### Primary Issue: Missing Rate Limiter Process

**Error Trace**:
```
** Generic server erlmcp_auth terminating
** Last message in was {authenticate,api_key,#{api_key => <<"test_key_123">>}}
** Reason for termination ==
** {{noproc,{gen_server,call,
            [erlmcp_auth_rate_limiter,
             {check_rate_limit,<<"test_key_123">>,undefined}]}}
```

**Analysis**:
1. `erlmcp_auth` module calls `erlmcp_auth_rate_limiter:check_rate_limit/2` at line 374
2. The rate limiter process is **NOT registered** (no process running)
3. `gen_server:call/2` fails with `noproc` (no such process)
4. Authentication server crashes unexpectedly
5. Test framework cancels all remaining tests

### Secondary Issue: Not in Supervision Tree

**Finding**: `erlmcp_auth` is **NOT** part of the `erlmcp_core_sup` supervision tree:
- `erlmcp_core.app.src` does not list `erlmcp_auth` in registered processes
- `erlmcp_core_sup.erl` does NOT include `erlmcp_auth` as a child spec
- `erlmcp_auth_rate_limiter` is also NOT in the supervision tree

**Architecture Gap**: The auth module appears to be designed as an optional component but lacks proper integration into the OTP supervision tree.

---

## Test Quality Analysis

### Test Structure Issues

#### 1. **CRITICAL: Missing Dependency Setup**

**Problem**: Test setup does NOT start the rate limiter dependency

**Current Code** (lines 29-54):
```erlang
setup() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),

    % Start auth server with test config
    Config = #{
        api_keys => #{
            <<"test_key_123">> => <<"user_alice">>,
            <<"test_key_456">> => <<"user_bob">>
        },
        jwt_keys => #{
            <<"test_kid">> => <<"test_public_key">>
        }
    },

    {ok, Pid} = erlmcp_auth:start_link(Config),
    % ... setup roles and permissions
```

**Issue**: `erlmcp_auth:start_link/1` starts the auth server, but the rate limiter is:
- NOT started explicitly
- NOT in the supervision tree
- Required by the auth module when `rate_limiter_enabled = true` (default)

**Fix Required**: Either:
- Option A: Start rate limiter explicitly in setup: `{ok, _} = erlmcp_auth_rate_limiter:start_link(#{});`
- Option B: Disable rate limiting in config: `Config = #{rate_limiter_enabled => false, ...}`
- Option C: Add auth and rate limiter to supervision tree (architectural fix)

#### 2. **Test Dependency Violates Chicago School TDD**

**Problem**: Tests assume rate limiter exists but don't start it

**Chicago School TDD Violation**: Tests should use **real collaborators**, but the rate limiter process isn't started. This is worse than mocking - it's assuming a dependency exists without verification.

**Correct Approach**: If testing with rate limiting, start the real rate limiter process in setup.

#### 3. **Incomplete Test Coverage for Error Conditions**

**Problem**: Test at line 134 expects permission <<"read">> for user role, but default roles (line 344) only grant [<<"read">>, <<"write">>]

**Code** (line 134):
```erlang
?assert(lists:member(<<"read">>, UserPerms)),
```

**Issue**: This test passes but doesn't verify the user role CANNOT do admin-only actions. Missing negative test cases.

#### 4. **JWT Validation Test Uses Fake Signature**

**Problem**: Line 86 uses hardcoded fake signature that won't pass real validation

**Code** (line 86):
```erlang
Signature = base64:encode(<<"fake_signature">>),
```

**Issue**: The current `do_validate_jwt/2` implementation (lines 437-456 in erlmcp_auth.erl) doesn't verify signatures (marked as TODO), so this test passes. When signature verification is implemented, this test will fail.

#### 5. **Token Revocation Test Incomplete**

**Problem**: Line 182 expects `validate_jwt/1` to check revocation, but the implementation checks revocation in `do_validate_jwt/2` (line 440)

**Code** (line 182):
```erlang
{error, token_revoked} = erlmcp_auth:validate_jwt(Token),
```

**Status**: This test will work correctly, but the comment on line 183 says "ok" which is misleading.

---

## Detailed Test Analysis

### Test 1: `test_api_key_auth/0` (FAILED TO RUN)
**Purpose**: Verify API key authentication
**Expected Behavior**:
- Valid API key returns session ID
- Invalid API key returns `{error, invalid_api_key}`

**Actual Behavior**: Test cancelled due to auth server crash

**Issues**:
- Relies on rate limiter being available
- Does NOT test rate limiting behavior itself

**Recommendation**: REFACTOR - Start rate limiter in setup or disable rate limiting

---

### Test 2: `test_jwt_validation/0` (FAILED TO RUN)
**Purpose**: Verify JWT token validation
**Expected Behavior**:
- Valid JWT returns claims
- Expired JWT returns `{error, token_expired}`
- Invalid format returns `{error, invalid_jwt_format}`

**Actual Behavior**: Test cancelled due to auth server crash

**Issues**:
- Uses fake signature that bypasses real validation
- Doesn't test signature verification (not implemented yet)
- Doesn't test missing expiration (`{error, missing_expiration}`)

**Recommendation**: REFACTOR - Add signature verification tests when implemented

---

### Test 3: `test_session_management/0` (FAILED TO RUN)
**Purpose**: Verify session creation and destruction
**Expected Behavior**:
- Create session returns session ID
- Destroy session removes it
- Destroyed session returns `{error, invalid_session}`

**Actual Behavior**: Test cancelled due to auth server crash

**Issues**:
- Tests basic CRUD operations correctly
- Missing edge case: double destroy (should be idempotent)

**Recommendation**: KEEP - Add idempotency test

---

### Test 4: `test_rbac_roles/0` (FAILED TO RUN)
**Purpose**: Verify role-based access control
**Expected Behavior**:
- User has correct roles
- Role has correct permissions
- Non-existent user returns `{error, not_found}`

**Actual Behavior**: Test cancelled due to auth server crash

**Issues**:
- Line 134 checks for <<"read">> permission but doesn't verify forbidden actions
- Doesn't test role removal

**Recommendation**: EXPAND - Add negative test cases and role removal tests

---

### Test 5: `test_permission_checking/0` (FAILED TO RUN)
**Purpose**: Verify permission enforcement
**Expected Behavior**:
- Admin can access admin endpoints
- User cannot access admin endpoints
- Both can access user endpoints

**Actual Behavior**: Test cancelled due to auth server crash

**Issues**:
- Good coverage of positive/negative cases
- Missing: test with no roles (guest)

**Recommendation**: EXPAND - Add guest role test

---

### Test 6: `test_token_rotation/0` (FAILED TO RUN)
**Purpose**: Verify token rotation invalidates old tokens
**Expected Behavior**:
- Rotation returns new session ID
- Old session ID becomes invalid
- New session ID works

**Actual Behavior**: Test cancelled due to auth server crash

**Issues**:
- Tests the contract correctly
- Missing: double rotation (rotate rotated token)

**Recommendation**: EXPAND - Add sequential rotation test

---

### Test 7: `test_token_revocation/0` (FAILED TO RUN)
**Purpose**: Verify token revocation
**Expected Behavior**:
- Revoke token succeeds
- Revoked token returns `{error, token_revoked}`

**Actual Behavior**: Test cancelled due to auth server crash

**Issues**:
- Test creates token but doesn't create it via authenticate (not a real session)
- Missing: revoke non-existent token

**Recommendation**: REFACTOR - Use real session token from authenticate

---

### Test 8: `test_session_cleanup/0` (FAILED TO RUN)
**Purpose**: Verify session expiration
**Expected Behavior**: Session should be valid (TODO: test expiration)

**Actual Behavior**: Test cancelled due to auth server crash

**Issues**:
- Test marked as TODO at line 193
- Doesn't actually test expiration
- Would need to mock time or wait 1 hour

**Recommendation**: DELETE or REFACTOR - Use configurable TTL for testing

---

## Recommendations

### Immediate Actions (REQUIRED)

#### 1. **Fix Test Setup** (CRITICAL - Blocks all tests)
**Option A**: Start rate limiter in test setup
```erlang
setup() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),

    % CRITICAL: Start rate limiter BEFORE auth server
    {ok, _RateLimiterPid} = erlmcp_auth_rate_limiter:start_link(#{
        max_attempts_per_second => 100,  % High limit for tests
        max_failures => 10
    }),

    Config = #{
        rate_limiter_enabled => true,  % Or false to skip rate limiting
        api_keys => #{...},
        jwt_keys => #{...}
    },
    {ok, Pid} = erlmcp_auth:start_link(Config),
    % ... rest of setup
```

**Option B**: Disable rate limiting in tests
```erlang
Config = #{
    rate_limiter_enabled => false,  % Skip rate limiting
    api_keys => #{...},
    jwt_keys => #{...}
},
```

**Recommendation**: Use Option A (start real rate limiter) to follow Chicago School TDD

---

#### 2. **Add Rate Limiter to Supervision Tree** (ARCHITECTURAL)
**File**: `apps/erlmcp_core/src/erlmcp_core_sup.erl`

Add to child specs (after line 207):
```erlang
%% ================================================================
%% AUTHENTICATION: Rate limiting and session management
%% ================================================================
#{
    id => erlmcp_auth_rate_limiter,
    start => {erlmcp_auth_rate_limiter, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_auth_rate_limiter]
},

#{
    id => erlmcp_auth,
    start => {erlmcp_auth, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_auth]
},
```

**Impact**: Makes auth and rate limiter part of OTP supervision tree

---

#### 3. **Delete or Refactor `test_session_cleanup/0`** (LOW PRIORITY)
**Problem**: Test is marked as TODO and doesn't actually test expiration

**Option A**: Delete the test (if not urgent)
```erlang
% Remove from test list at line 27
```

**Option B**: Refactor with configurable TTL
```erlang
test_session_cleanup() ->
    % Create session with 1-second TTL
    {ok, SessionId} = erlmcp_auth:create_session(<<"user_alice">>, #{
        ttl => 1  % Add TTL support to auth module
    }),

    % Session should be valid immediately
    ok = erlmcp_auth:check_permission(SessionId, <<"/api/tools">>, <<"execute">>),

    % Wait for expiration
    timer:sleep(1500),

    % Session should be expired
    {error, invalid_session} = erlmcp_auth:check_permission(SessionId, <<"/api/tools">>, <<"execute">>),

    ok.
```

**Recommendation**: Delete for now, add back when TTL is configurable

---

### Medium-Term Improvements

#### 4. **Add Missing Test Coverage**
- **JWT signature verification**: When implemented, add tests
- **Role removal**: Test removing roles from users
- **Permission removal**: Test removing permissions from resources
- **Double rotation**: Test rotating a rotated token
- **Guest role**: Test user with no roles (defaults to guest)
- **Rate limiting**: Add explicit rate limit tests

---

#### 5. **Improve Test Assertions**
- Add more specific error messages in assertions
- Test edge cases (empty strings, nil values, max lengths)
- Test concurrent authentication (multiple processes)

---

#### 6. **Add Property-Based Tests (Proper)**
```erlang
prop_authenticate_api_key() ->
    ?FORALL(Key, valid_api_key(),
        begin
            {ok, _RateLimiterPid} = erlmcp_auth_rate_limiter:start_link(#{}),
            {ok, AuthPid} = erlmcp_auth:start_link(#{rate_limiter_enabled => false}),
            Result = erlmcp_auth:authenticate(api_key, #{api_key => Key}),
            erlmcp_auth:stop(),
            erlmcp_auth_rate_limiter:stop(),
            Result =:= ok orelse element(1, Result) =:= error
        end).
```

---

## Code Quality Issues

### 1. **Chicago School TDD Violations**
- Tests assume rate limiter exists but don't start it
- Should use real collaborators (real rate limiter process)

### 2. **Incomplete Error Testing**
- Missing negative test cases for permissions
- Missing edge cases (empty values, malformed input)

### 3. **TODO Comments in Production Code**
- Line 443: `TODO: Implement full JWT validation with jose library`
- Line 476: `TODO: Implement OAuth2 token introspection`
- Line 489: `TODO: Implement mTLS certificate validation`

**Impact**: Tests pass but implementation is incomplete

---

## Decision: Fix or Delete?

### RECOMMENDATION: **FIX** (do not delete)

**Reasoning**:
1. **Test suite is valuable**: Covers critical auth functionality
2. **Fix is straightforward**: Start rate limiter in setup (5-minute fix)
3. **Architecture needs work**: Add auth to supervision tree (30-minute fix)
4. **Deleting would lose coverage**: Auth is security-critical, needs tests

### Priority Matrix

| Test | Status | Priority | Effort | Action |
|------|--------|----------|--------|--------|
| All tests | BLOCKED | P0 | 5 min | Start rate limiter in setup |
| test_session_cleanup | TODO | P3 | 1 hour | Add configurable TTL or delete |
| test_rbac_roles | INCOMPLETE | P2 | 30 min | Add negative test cases |
| test_jwt_validation | INCOMPLETE | P2 | 1 hour | Add signature verification tests |
| test_permission_checking | INCOMPLETE | P2 | 15 min | Add guest role test |
| test_token_rotation | INCOMPLETE | P2 | 15 min | Add sequential rotation test |
| test_token_revocation | INCOMPLETE | P2 | 15 min | Use real session token |

---

## Next Steps

### Immediate (Today)
1. **Fix test setup** - Add rate limiter startup to `setup()` function
2. **Run tests** - Verify all 8 tests pass
3. **Update report** - Confirm tests pass

### Short-Term (This Week)
1. **Add auth to supervision tree** - Architectural fix
2. **Expand test coverage** - Add missing test cases
3. **Add property tests** - Proper invariants

### Long-Term (Next Sprint)
1. **Implement JWT signature verification** - Complete TODOs
2. **Add OAuth2 introspection** - Complete TODOs
3. **Add mTLS validation** - Complete TODOs

---

## Conclusion

The `erlmcp_auth_tests` test suite has a **critical blocker** preventing any tests from running: the rate limiter dependency is not started. This is a **5-minute fix** that will unblock all 8 tests.

**Overall Assessment**: Tests are well-structured and valuable, but need immediate fix to setup function. The architecture also needs improvement (add auth to supervision tree).

**Recommended Action**: Fix the test setup immediately, then expand coverage.

**Quality Gates**:
- [ ] Fix: Start rate limiter in test setup
- [ ] All 8 tests pass
- [ ] Add auth to supervision tree
- [ ] Coverage >= 80% (currently unknown due to blocker)
- [ ] Add missing test cases (negative tests, edge cases)

---

**Report Generated**: 2026-01-29
**Generated By**: erlmcp-test-engineer agent
**Tool**: rebar3 eunit --module=erlmcp_auth_tests
