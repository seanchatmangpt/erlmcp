# MCP Server Testing - Quick Start Guide

## Running the Tests

### Compile All Tests
```bash
TERM=dumb rebar3 compile
```

### Run Initialization Tests (Area 1)
```bash
rebar3 eunit --module=erlmcp_server_init_tests
```

### Run All Server Tests
```bash
rebar3 eunit --module=erlmcp_server_*_tests
```

### Run with Coverage
```bash
rebar3 eunit --module=erlmcp_server_init_tests --cover
rebar3 cover
```

### Run Integration Tests
```bash
rebar3 ct --suite=erlmcp_server_init_SUITE
```

## Test Files Created

### Documentation (1,136 lines)
1. **MCP_SERVER_TESTING_STRATEGY.md** (853 lines)
   - Complete testing strategy
   - 8 testing areas
   - 200+ test scenarios
   - Quality gates and success criteria

2. **MCP_SERVER_TESTING_SUMMARY.md** (283 lines)
   - Executive summary
   - Deliverables checklist
   - Next steps and timeline

### Test Code (1,017 lines)
3. **erlmcp_server_test_helpers.erl** (605 lines)
   - Common test utilities
   - Server setup/teardown
   - Sample data builders
   - Custom assertions
   - Transport simulation

4. **erlmcp_server_init_tests.erl** (412 lines)
   - 25+ test cases for initialization
   - Lifecycle tests
   - Capability negotiation
   - Phase tracking
   - Security enforcement

## Testing Areas Overview

| Area | Module | Tests | Status |
|------|--------|-------|--------|
| 1. Initialization | `erlmcp_server_init_tests.erl` | 25 | ✅ Stub Created |
| 2. Tool Execution | `erlmcp_server_tools_tests.erl` | 35 | ⏳ Planned |
| 3. Resources | `erlmcp_server_resources_tests.erl` | 40 | ⏳ Planned |
| 4. Prompts | `erlmcp_server_prompts_tests.erl` | 30 | ⏳ Planned |
| 5. Batch | `erlmcp_server_batch_tests.erl` | 20 | ⏳ Planned |
| 6. State | `erlmcp_server_state_tests.erl` | 20 | ⏳ Planned |
| 7. Cleanup | `erlmcp_server_cleanup_tests.erl` | 20 | ⏳ Planned |
| 8. Performance | `erlmcp_server_performance_tests.erl` | 10 | ⏳ Planned |

## Quick Test Examples

### Using Test Helpers
```erlang
%% Setup a server
Server = erlmcp_server_test_helpers:setup_server(),

%% Add a resource
Resource = erlmcp_server_test_helpers:sample_resource(),
Handler = erlmcp_server_test_helpers:simple_resource_handler(<<"content">>),
ok = erlmcp_server:add_resource(Server, Resource, Handler),

%% Verify resource exists
?assert(erlmcp_server_test_helpers:assert_resource_exists(Server, <<"test://resource/1">>)),

%% Cleanup
erlmcp_server_test_helpers:cleanup_server(Server).
```

### Building Messages
```erlang
%% Initialize request
InitReq = erlmcp_server_test_helpers:build_initialize_request(),

%% Tool call request
ToolReq = erlmcp_server_test_helpers:build_tools_call_request(
    <<"my_tool">>,
    #{<<"input">> => <<"test">>}
),

%% Resource read request
ReadReq = erlmcp_server_test_helpers:build_resources_read_request(
    <<"test://resource/1">>
).
```

### Custom Assertions
```erlang
%% Assert server phase
erlmcp_server_test_helpers:assert_server_phase(Server, ?MCP_PHASE_INITIALIZED),

%% Assert initialized
erlmcp_server_test_helpers:assert_server_initialized(Server),

%% Assert resource exists
erlmcp_server_test_helpers:assert_resource_exists(Server, Uri).
```

## Quality Gates Checklist

Before marking tests as "done", ensure:

- [ ] **Compilation:** 0 errors (`TERM=dumb rebar3 compile`)
- [ ] **Tests:** 100% pass rate (`rebar3 eunit`)
- [ ] **Coverage:** ≥80% (`rebar3 cover`)
- [ ] **Dialyzer:** 0 warnings (`rebar3 dialyzer`)
- [ ] **Xref:** 0 issues (`rebar3 xref`)
- [ ] **Benchmarks:** No regression (<10% degradation)

## Test Philosophy

### Chicago School TDD
- ✅ Real processes (no mocks)
- ✅ State-based verification
- ✅ Async operation handling
- ✅ Production-like conditions

### Key Principles
1. **Test behavior, not implementation**
2. **Use real gen_servers, not mocks**
3. **Verify actual state changes**
4. **Test error paths and edge cases**

## Next Steps

### Week 1: Foundation
1. ✅ Create testing strategy
2. ✅ Create test helpers
3. ✅ Create initialization tests
4. ⏳ Run and validate Area 1 tests

### Week 2-4: Core Functionality
1. Implement Area 2 (Tools)
2. Implement Area 3 (Resources)
3. Implement Area 4 (Prompts)
4. Achieve ≥60% coverage

### Week 5-8: Advanced Features
1. Implement Area 5 (Batch)
2. Implement Area 6 (State)
3. Implement Area 7 (Cleanup)
4. Implement Area 8 (Performance)
5. Achieve ≥80% coverage

### Week 9-12: Production Readiness
1. Fix coverage gaps
2. Performance benchmarking
3. CI/CD integration
4. Documentation

## Troubleshooting

### Test Won't Compile
```bash
# Check syntax
TERM=dumb rebar3 compile

# Check for warnings
rebar3 compile -W
```

### Test Fails
```bash
# Run with verbose output
rebar3 eunit --module=erlmcp_server_init_tests --verbose

# Check for specific test
rebar3 eunit --test=test_server_start_link_success
```

### Coverage Issues
```bash
# Generate coverage report
rebar3 cover

# View in browser
open _build/test/cover/index.html
```

## Resources

- **Full Strategy:** `/Users/sac/erlmcp/docs/testing/MCP_SERVER_TESTING_STRATEGY.md`
- **Test Helpers:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_test_helpers.erl`
- **Example Tests:** `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_init_tests.erl`
- **EUnit Docs:** http://erlang.org/doc/apps/eunit/chapter.html
- **CT Docs:** http://erlang.org/doc/apps/common_test/chapter.html

## Summary

**Files Created:** 4 (2,153 total lines)
- 2 documentation files (1,136 lines)
- 2 test modules (1,017 lines)

**Test Coverage Plan:**
- 8 testing areas
- 200+ test scenarios
- Target: ≥80% coverage

**Status:** ✅ Strategy complete, implementation in progress

**Next:** Implement Areas 2-8 test modules
