# Integration Testing Quick Reference Guide

**Last Updated**: 2026-01-27
**Project**: erlmcp

---

## Quick Start

### Run All Integration Tests

```bash
# Using make
make test-int

# Using rebar3 directly
rebar3 ct

# Verbose output
rebar3 ct --verbose
```

### Run Specific Module

```bash
# Run v1 basic tests
rebar3 ct --test erlmcp_integration_tests_v1

# Run advanced tests
rebar3 ct --test erlmcp_integration_advanced_SUITE
```

### Run Specific Test Case

```bash
# Run one test from v1
rebar3 ct --test erlmcp_integration_tests_v1 --case client_initialization_flow

# Run one test from advanced
rebar3 ct --test erlmcp_integration_advanced_SUITE --case sequential_operations
```

---

## Test Files Location

| File | Type | Tests | Location |
|------|------|-------|----------|
| **erlmcp_integration_tests_v1.erl** | EUnit/CT | 7 basic | `/Users/sac/erlmcp/test/erlmcp_integration_tests_v1.erl` |
| **erlmcp_integration_advanced_SUITE.erl** | Common Test | 7 advanced | `/Users/sac/erlmcp/test/erlmcp_integration_advanced_SUITE.erl` |

---

## Available Test Cases

### V1 Tests (Basic Scenarios)

```
erlmcp_integration_tests_v1:
  1. client_initialization_flow
     → Tests client startup, initialization, capability negotiation

  2. resource_subscription_flow
     → Tests resource registration, subscription, notifications

  3. tool_management_flow
     → Tests tool registration, listing, execution

  4. concurrent_clients_flow
     → Tests multiple clients operating independently

  5. connection_failure_recovery
     → Tests reconnection and session recovery

  6. rate_limiting_flow
     → Tests rate limiting trigger and recovery

  7. circuit_breaker_flow
     → Tests circuit breaker state transitions
```

### Advanced Tests (Extended Scenarios)

```
erlmcp_integration_advanced_SUITE:
  1. sequential_operations
     → Tests multi-step workflows with dependencies

  2. error_recovery_handling
     → Tests error catching and recovery

  3. resource_lifecycle_management
     → Tests complete resource lifecycle

  4. notification_ordering
     → Tests notification delivery guarantees

  5. client_isolation_verification
     → Tests client isolation and independence

  6. tool_result_validation
     → Tests result formatting and validation

  7. capability_enforcement
     → Tests capability-based access control
```

---

## Common Test Commands

### With Coverage Report

```bash
# Generate coverage
rebar3 do ct --test erlmcp_integration_advanced_SUITE, cover

# View coverage HTML
open _build/default/cover/index.html
```

### With Custom Timeout

```bash
# Increase timeout to 60 seconds (default 30s)
rebar3 ct --test erlmcp_integration_advanced_SUITE --timetrap 60
```

### With Specific Log Level

```bash
# Very verbose output
rebar3 ct --test erlmcp_integration_tests_v1 --vvv

# Summary only
rebar3 ct --test erlmcp_integration_tests_v1 --quiet
```

### Parallel Execution

```bash
# Run multiple test suites in parallel
rebar3 ct --test erlmcp_integration_tests_v1 --test erlmcp_integration_advanced_SUITE --cover
```

---

## Understanding Test Output

### Successful Test Run

```
======================== TEST RESULTS ========================
Test: erlmcp_integration_tests_v1:client_initialization_flow
Status: OK (elapsed time: 0.234 seconds)

Test: erlmcp_integration_tests_v1:tool_management_flow
Status: OK (elapsed time: 1.123 seconds)

======================== Summary ========================
Total: 14 tests
Passed: 14 (100%)
Failed: 0
Skipped: 0
Time: 18.5 seconds
==========================================================
```

### Failed Test Example

```
======================== TEST FAILURE ========================
Test: erlmcp_integration_tests_v1:connection_failure_recovery
Status: FAILED

Failure Reason:
  {assertion_failed,[{module,erlmcp_integration_tests_v1},
                     {line,123},
                     {expression,"is_pid(Client)"},
                     {expected,true},
                     {value,false}]}

Location: erlmcp_integration_tests_v1.erl:123
Context: Client initialization failed
==========================================================
```

---

## Debugging Failed Tests

### Step 1: Read Error Message
- Check what assertion failed
- Note line number and file
- Review expected vs actual value

### Step 2: Run with Verbose Logging

```bash
rebar3 ct --test erlmcp_integration_advanced_SUITE --case sequential_operations --verbose
```

### Step 3: Check Log Files

```bash
# CT generates detailed logs in _build/default/ct
ls -la _build/default/ct/

# View test run log
cat _build/default/ct/tests/erlmcp_integration_tests_v1/erlmcp_integration_tests_v1.txt

# View stdout/stderr from test
cat _build/default/ct/tests/erlmcp_integration_tests_v1/stdout
```

### Step 4: Add Debug Output

Edit test and add more logging:

```erlang
test_case(Config) ->
    ct:log("Starting test: ~p", [?config(test_case, Config)]),

    {ok, Server} = erlmcp_server:start_link(...),
    ct:log("Server started: ~p", [Server]),
    ct:log("Server state: ~p", [sys:get_state(Server)]),

    % ... rest of test
end.
```

### Step 5: Run Single Test in Isolation

```bash
rebar3 ct --test erlmcp_integration_advanced_SUITE --case sequential_operations --vvv
```

---

## Common Issues & Solutions

### Issue: "Test timed out"
**Cause**: Test taking longer than expected
**Solution**:
```bash
# Increase timeout
rebar3 ct --test erlmcp_integration_tests_v1 --timetrap 60
```

### Issue: "Port already in use"
**Cause**: Previous test didn't cleanup properly
**Solution**:
```bash
# Kill erlang processes
pkill -9 erl

# Clean build
make clean
```

### Issue: "Application erlmcp not started"
**Cause**: Application initialization failed
**Solution**: Check `init_per_suite` runs successfully:
```bash
rebar3 shell
> application:ensure_all_started(erlmcp).
```

### Issue: "undefined module erlmcp_client"
**Cause**: Code not compiled
**Solution**:
```bash
rebar3 compile
rebar3 ct --test erlmcp_integration_tests_v1
```

### Issue: "Test assertion failed"
**Cause**: Test logic issue or unexpected behavior
**Solution**:
1. Read assertion error message carefully
2. Run with verbose logging
3. Check if API changed
4. Review test comment for expected behavior

---

## Analyzing Test Results

### Generate Report

```bash
# Run tests with coverage and generate report
rebar3 do ct --test erlmcp_integration_advanced_SUITE, cover

# View HTML coverage report
open _build/default/cover/index.html
```

### Check Coverage by Module

In coverage HTML report:
1. Click module name to see line-by-line coverage
2. Red = not covered
3. Green = covered
4. Gray = no code

### Performance Analysis

```bash
# Extract timing from logs
grep "elapsed time" _build/default/ct/tests/erlmcp_integration_tests_v1/erlmcp_integration_tests_v1.txt

# Expected total: ~20 seconds for all 14 tests
```

---

## Test Dependencies

### Required Erlang Modules
- `erlmcp_server` - MCP server implementation
- `erlmcp_client` - MCP client implementation
- `erlmcp_json_rpc` - JSON-RPC encoding/decoding
- `erlmcp_registry` - Message routing

### Required Applications
- `erlmcp` - Main application
- `jsx` - JSON library (dependency)
- `jesse` - JSON Schema validation (dependency)

### Dependencies to Verify

```bash
# Check rebar.config has all dependencies
grep -A 5 "^{deps" rebar.config

# Verify applications start
rebar3 shell
> application:ensure_all_started(erlmcp).
{ok,[erlmcp]}
```

---

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Integration Tests
on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: [24, 25, 26]
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
          rebar3-version: latest

      - run: rebar3 compile
      - run: rebar3 ct --test erlmcp_integration_tests_v1
      - run: rebar3 ct --test erlmcp_integration_advanced_SUITE
      - run: rebar3 do ct, cover
      - run: bash <(curl -s https://codecov.io/bash)
```

---

## Performance Baselines

### Expected Test Timings

| Test | Expected | Actual | Notes |
|------|----------|--------|-------|
| client_initialization_flow | < 1s | TBD | Basic startup |
| tool_management_flow | < 2s | TBD | Tool call roundtrip |
| concurrent_clients_flow | < 3s | TBD | 5 parallel clients |
| sequential_operations | < 1s | TBD | Multi-step workflow |
| error_recovery_handling | < 1s | TBD | Exception handling |
| **Total** | **~20s** | TBD | All 14 tests |

### Recording Results

After running tests, document actual timings:

```bash
# Extract and save timings
grep "elapsed time" _build/default/ct/tests/*/erlmcp_integration*.txt > timings.txt
date >> timings.txt
```

---

## Extending Tests

### Adding New Test Case

1. Choose module (v1 for basic, advanced_SUITE for advanced)
2. Copy existing test case template
3. Update name and description
4. Implement test body
5. Add assertions
6. Run: `rebar3 ct --test MODULE --case new_test`

### Adding New Test Module

1. Create `erlmcp_new_integration_SUITE.erl`
2. Use Common Test template (include `ct.hrl`)
3. Implement suite/0, all/0, init/end callbacks
4. Add test cases as functions
5. Run: `rebar3 ct --test erlmcp_new_integration_SUITE`

### Example New Test

```erlang
-module(erlmcp_my_integration_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

suite() -> [{timetrap, {seconds, 30}}].
all() -> [my_test_case].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    ok.

my_test_case(Config) ->
    ct:log("My new test"),

    {ok, Server} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{
        tools = #mcp_capability{enabled = true}
    }),

    try
        % Test implementation
        ct:log("✓ Test PASSED"),
        ok
    after
        erlmcp_server:stop(Server)
    end.
```

---

## Useful Make Targets

```bash
# Run all erlmcp tests
make test

# Run integration tests only
make test-int

# Run unit tests (fast)
make test-unit

# Full validation (compile + lint + test)
make check

# Coverage report
make test-coverage

# View results
make test-report
```

---

## Documentation References

| Document | Purpose | Location |
|----------|---------|----------|
| **INTEGRATION_TESTS_V1_REPORT.md** | Detailed test report | `/Users/sac/erlmcp/docs/INTEGRATION_TESTS_V1_REPORT.md` |
| **INTEGRATION_TESTING_SUMMARY.md** | Overview & architecture | `/Users/sac/erlmcp/docs/INTEGRATION_TESTING_SUMMARY.md` |
| **INTEGRATION_TESTING_GUIDE.md** | This quick reference | `/Users/sac/erlmcp/docs/INTEGRATION_TESTING_GUIDE.md` |
| **MCP Specification** | Protocol details | https://modelcontextprotocol.io |
| **CLAUDE.md** | Project guidelines | `/Users/sac/erlmcp/CLAUDE.md` |

---

## Quick Debugging Checklist

- [ ] Test runs in isolation: `rebar3 ct --test MODULE --case TEST`
- [ ] Server starts without errors: Check `init_per_testcase`
- [ ] Client initializes successfully: Check initialization flow
- [ ] Resources/tools registered properly: Check add_* calls
- [ ] Cleanup happens: Check `end_per_testcase`
- [ ] No port conflicts: Check if erlang processes running
- [ ] Application started: Check `application:ensure_all_started(erlmcp)`
- [ ] Dependencies installed: Check `rebar3 get-deps`
- [ ] Code compiled: Check `rebar3 compile`

---

## Support & Issues

### Getting Help

1. **Read test code** - It's well-documented with comments
2. **Check test logs** - In `_build/default/ct/`
3. **Review INTEGRATION_TESTS_V1_REPORT.md** - Detailed scenario descriptions
4. **Check CLAUDE.md** - Project conventions and patterns

### Reporting Issues

Include:
- Test name that failed
- Erlang/OTP version: `erl -version`
- Full error message from logs
- Steps to reproduce
- Expected vs actual behavior

---

## Version Information

- **Created**: 2026-01-27
- **Erlang/OTP**: 25+ required
- **rebar3**: Latest
- **erlmcp**: 0.6.0+

---

**Happy Testing! 🎉**

For detailed information about specific test scenarios, see **INTEGRATION_TESTS_V1_REPORT.md**.
