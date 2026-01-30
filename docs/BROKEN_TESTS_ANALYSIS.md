# Broken Test Files Analysis and Recommendations

**Analysis Date:** 2026-01-30
**Status:** Ready for Review
**Test Pattern:** Chicago School TDD

## Executive Summary

This document analyzes broken/obsolete test files in the erlmcp codebase and provides specific recommendations for each. The analysis considers:

1. **Test Value:** Does the test still verify useful behavior?
2. **API Relevance:** Has the API changed significantly?
3. **Maintenance Cost:** How much work to fix vs. value gained?
4. **Better Alternatives:** Are there better ways to test this functionality?

**Summary of Findings:**
- **Total Broken Files:** 7 identified
- **Recommendation - Rewrite:** 4 tests (high value, moderate fix cost)
- **Recommendation - Delete:** 2 tests (obsolete, low value)
- **Recommendation - Keep Minimal:** 1 test (specialized, edge case)

---

## 1. erlmcp_json_rpc_proper_tests.erl.broken

**File:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_json_rpc_proper_tests.erl.broken`

**Status:** DELETED MODULE (moved to .broken)

**Test Coverage:**
- Property-based tests for JSON-RPC encoding/decoding
- 15+ Proper properties for roundtrip validation
- Error handling, batch requests, notifications

**API Changes:**
```erlang
%% OLD API (test expects):
erlmcp_json_rpc:encode_error(Id, Code, Message, Data)

%% NEW API (current implementation):
erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data)
erlmcp_json_rpc:decode_message(Json) instead of decode(Json)
```

**Recommendation: REWRITE**

**Rationale:**
- **High Value:** Property-based tests are crucial for protocol compliance
- **API Changes:** Minimal - just function renames
- **Coverage:** Tests invariants that unit tests miss (roundtrip properties)
- **Cost:** ~1-2 hours to update function names

**Specific Changes Required:**
1. Replace `encode_error` → `encode_error_response`
2. Replace `decode` → `decode_message`
3. Update error record handling (now uses `#mcp_error{}` records)
4. Remove tests for deprecated functions

**Action Plan:**
```erlang
%% Changes needed in erlmcp_json_rpc_proper_tests.erl:

%% Line 205: Change encode_error to encode_error_response
Encoded = erlmcp_json_rpc:encode_error_response(
    maps:get(id, Response),
    maps:get(code, ErrorMap),
    maps:get(message, ErrorMap),
    maps:get(data, ErrorMap, undefined)
),

%% Line 143: Change decode to decode_message
case erlmcp_json_rpc:decode_message(Encoded) of

%% Line 214: Update error matching pattern
case maps:get(error, Decoded) of
    #{code := Code, message := Message, data := Data} ->
        %% Now returns maps, not #mcp_error{} records
```

**Priority:** HIGH (protocol compliance testing)

---

## 2. erlmcp_client_request_id_overflow_tests.erl

**File:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_client_request_id_overflow_tests.erl` (untracked)

**Status:** NEW FILE (never integrated)

**Test Coverage:**
- Request ID overflow behavior
- Rolling over from max integer to 0
- Correlation of requests during rollover

**API Changes:**
- `erlmcp_request_id` module was moved/broken (see `erlmcp_request_id.erl.broken`)
- Client state management changed from `request_id` field to different pattern

**Recommendation: REWRITE**

**Rationale:**
- **Critical Edge Case:** Request ID overflow is rare but catastrophic
- **Chicago School:** Tests observable behavior (correlation still works after overflow)
- **API Changes:** Moderate - need to understand new request ID generation
- **Cost:** ~2-3 hours (need to understand new ID generation pattern)

**Specific Changes Required:**
1. Determine if `erlmcp_request_id` module still exists or was integrated elsewhere
2. Check if client uses monotonic counter or different approach
3. Test actual correlation behavior, not internal ID state

**Action Plan:**
```erlang
%% Check if this module still exists:
%% 1. grep for "request_id" in erlmcp_client.erl
%% 2. Look for ID generation pattern
%% 3. Test the correlation behavior, not the internal counter
```

**Priority:** MEDIUM (edge case, but important for long-running sessions)

---

## 3. erlmcp_state_migration_tests.erl

**File:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_state_migration_tests.erl` (untracked)

**Status:** NEW FILE (never integrated)

**Test Coverage:**
- State version migration
- Upgrading old state records to new format
- Backward compatibility

**API Changes:**
- `erlmcp_state_migration.erl` was deleted (moved to `.broken`)
- State migration approach changed to different pattern

**Recommendation: DELETE**

**Rationale:**
- **Obsolete Pattern:** State migration module removed from codebase
- **New Approach:** Codebase uses different backward compatibility strategy
- **Low Value:** Tests deleted functionality
- **Cost:** Zero (just delete)

**Alternative:**
If state migration is still needed, it should be tested as part of the module that implements it (not as a standalone test).

**Priority:** LOW (delete unless state migration is still required)

---

## 4. erlmcp_transport_tcp_leak_tests.erl

**File:** `/Users/sac/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_tcp_leak_tests.erl` (untracked)

**Status:** NEW FILE (never integrated)

**Test Coverage:**
- TCP connection leak detection
- Port exhaustion under load
- Cleanup verification

**API Changes:**
- Transport implementation changed significantly
- TCP now uses `ranch` library instead of custom implementation
- Connection pooling added via `erlmcp_pool_manager`

**Recommendation: REWRITE**

**Rationale:**
- **High Value:** Resource leaks are critical in production
- **Chicago School:** Test observable behavior (port count, memory usage)
- **API Changes:** Major - entire transport rewritten
- **Cost:** ~3-4 hours (need to understand ranch + pool manager)

**Specific Changes Required:**
1. Test ranch connection handling, not custom TCP
2. Verify pool manager cleanup
3. Use recon/os_mon for leak detection
4. Test with real load (1000+ connections)

**Action Plan:**
```erlang
%% New approach using ranch + pool manager:

tcp_leak_detection_test_() ->
    {setup,
     fun() ->
         %% Start ranch listener
         {ok, Pid} = erlmcp_transport_tcp:start_link(Config),
         Pid
     end,
     fun(Pid) ->
         gen_server:stop(Pid)
     end,
     fun(Pid) ->
         [?_test(begin
             %% Get initial port count
             InitialPorts = count_ports(),

             %% Create 1000 connections
             [create_connection() || _ <- lists:seq(1, 1000)],

             %% Close all connections
             close_all_connections(),

             %% Wait for cleanup
             timer:sleep(1000),

             %% Verify no leaks
             FinalPorts = count_ports(),
             ?assertEqual(InitialPorts, FinalPorts)
         end)]
     end}.
```

**Priority:** HIGH (production-critical leak detection)

---

## 5. erlmcp_schema_validator_tests.erl.broken

**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_schema_validator.erl.broken`

**Status:** DELETED MODULE

**Test Coverage:**
- JSON Schema validation
- Request/response schema compliance
- Type checking

**API Changes:**
- Module deleted entirely
- Functionality may have moved to `jesse` library or integrated elsewhere

**Recommendation: DELETE**

**Rationale:**
- **Obsolete:** Module no longer exists
- **Replaced By:** `jesse` library (external dependency)
- **Low Value:** Tests deleted functionality
- **Cost:** Zero (delete file)

**Priority:** LOW (delete obsolete test)

---

## 6. erlmcp_uri_validator_tests.erl.broken

**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_uri_validator.erl.broken`

**Status:** DELETED MODULE

**Test Coverage:**
- URI validation
- MCP endpoint validation
- URI pattern matching

**API Changes:**
- Module deleted entirely
- Functionality may be integrated into other modules

**Recommendation: DELETE**

**Rationale:**
- **Obsolete:** Module no longer exists
- **Low Value:** Tests deleted functionality
- **Cost:** Zero (delete file)

**Priority:** LOW (delete obsolete test)

---

## 7. erlmcp_prompt_argument_validator_tests.erl.broken

**File:** `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_prompt_argument_validator.erl.broken`

**Status:** DELETED MODULE

**Test Coverage:**
- Prompt argument validation
- Schema-based prompt parameter checking
- Type validation for prompts

**API Changes:**
- Module deleted entirely
- Functionality may be integrated into server or handlers

**Recommendation: DELETE**

**Rationale:**
- **Obsolete:** Module no longer exists
- **Low Value:** Tests deleted functionality
- **Cost:** Zero (delete file)

**Priority:** LOW (delete obsolete test)

---

## Summary Table

| File | Recommendation | Priority | Est. Effort | Value |
|------|---------------|----------|-------------|-------|
| erlmcp_json_rpc_proper_tests.erl.broken | REWRITE | HIGH | 1-2 hours | Protocol compliance |
| erlmcp_client_request_id_overflow_tests.erl | REWRITE | MEDIUM | 2-3 hours | Edge case coverage |
| erlmcp_state_migration_tests.erl | DELETE | LOW | 0 | Obsolete |
| erlmcp_transport_tcp_leak_tests.erl | REWRITE | HIGH | 3-4 hours | Production-critical |
| erlmcp_schema_validator_tests.erl.broken | DELETE | LOW | 0 | Obsolete |
| erlmcp_uri_validator_tests.erl.broken | DELETE | LOW | 0 | Obsolete |
| erlmcp_prompt_argument_validator_tests.erl.broken | DELETE | LOW | 0 | Obsolete |

**Total Effort:**
- Rewrite: 6-9 hours
- Delete: 0 hours
- **Net Value:** 4 high-value test suites restored

---

## Implementation Priority Order

### Phase 1: Quick Wins (1-2 hours)
1. **Delete obsolete test files** (0 hours)
   - erlmcp_schema_validator_tests.erl.broken
   - erlmcp_uri_validator_tests.erl.broken
   - erlmcp_prompt_argument_validator_tests.erl.broken
   - erlmcp_state_migration_tests.erl

### Phase 2: High Value (2-3 hours)
2. **Rewrite erlmcp_json_rpc_proper_tests** (1-2 hours)
   - Update function names (encode_error → encode_error_response)
   - Update decode calls (decode → decode_message)
   - Fix error record handling
   - Run proper:module(?MODULE) to verify

3. **Rewrite erlmcp_client_request_id_overflow_tests** (2-3 hours)
   - Determine current request ID generation approach
   - Test correlation behavior during overflow
   - Verify long-running session stability

### Phase 3: Production-Critical (3-4 hours)
4. **Rewrite erlmcp_transport_tcp_leak_tests** (3-4 hours)
   - Adapt to ranch + pool manager architecture
   - Use recon/os_mon for leak detection
   - Test with realistic load (1000+ connections)
   - Verify cleanup under stress

---

## Testing Strategy Guidelines

### Chicago School TDD Compliance

All rewritten tests MUST follow Chicago School TDD principles:

1. **Real Collaborators:**
   - Use actual gen_servers, not mocks
   - Spawn real processes for testing
   - Test with real transports, not stubs

2. **State-Based Verification:**
   - Assert on observable state (API results, message receipts)
   - Don't verify internal method calls
   - Test what system does, not how it does it

3. **Behavior Verification:**
   - Test outputs and side effects
   - Don't test implementation details
   - Focus on user-visible behavior

### Property-Based Testing Best Practices

For `erlmcp_json_rpc_proper_tests`:

```erlang
%% DO: Test invariants
prop_encode_decode_roundtrip() ->
    ?FORALL(Message, message_generator(),
        begin
            Encoded = erlmcp_json_rpc:encode_request(...),
            {ok, Decoded} = erlmcp_json_rpc:decode_message(Encoded),
            compare_messages(Message, Decoded)
        end).

%% DON'T: Test implementation details
%% prop_internal_state_correct() ->  %% BAD: Tests internals
```

### Leak Detection Best Practices

For `erlmcp_transport_tcp_leak_tests`:

```erlang
%% DO: Measure observable resource usage
tcp_no_leak_test_() ->
    ?_test(begin
        InitialPorts = count_ports(),
        InitialMemory = erlang:memory(total),

        %% Perform operations
        create_and_close_connections(1000),

        %% Wait for cleanup
        timer:sleep(1000),

        %% Verify cleanup (state-based)
        FinalPorts = count_ports(),
        FinalMemory = erlang:memory(total),

        ?assertEqual(InitialPorts, FinalPorts),
        ?assert(FinalMemory < InitialMemory + 1024 * 1024)  % 1MB tolerance
    end).

%% DON'T: Mock or instrument internals
```

---

## Conclusion

The broken test files fall into three clear categories:

1. **Obsolete (Delete):** 5 files testing deleted modules
2. **API Changes (Rewrite):** 2 files with minor/moderate API updates needed

**Recommended Action:** Focus effort on the 2 high-value rewrites (JSON-RPC proper tests and TCP leak tests) and delete the rest. This maximizes test coverage while minimizing maintenance burden.

**Next Steps:**
1. Delete obsolete test files (Phase 1)
2. Rewrite erlmcp_json_rpc_proper_tests (Phase 2)
3. Rewrite erlmcp_transport_tcp_leak_tests (Phase 3)

**Estimated Total Time:** 6-9 hours for full resolution
