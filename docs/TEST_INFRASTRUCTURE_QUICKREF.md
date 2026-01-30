# Test Infrastructure Quick Reference Guide

**Last Updated**: 2026-01-29
**Project**: erlmcp (Erlang/OTP MCP SDK)
**Status**: Production-Ready (80% Health Score)

---

## Quick Start: Running Tests

### Local Development
```bash
# Quick test (EUnit only, ~30 seconds)
make eunit

# Full test suite (EUnit + CT, ~2 minutes)
make test

# With coverage report
make coverage

# Quality gates (blocking, ~5 minutes)
make validate
```

### Before Committing
```bash
# Run all quality checks
make validate

# Or run individual checks
make validate-compile    # Compilation check
make validate-test       # Test execution
make validate-coverage   # 80% coverage threshold
make validate-quality    # Dialyzer + Xref
```

---

## Test Infrastructure Status

### ✅ Working Components

| Component | Version/Status | Notes |
|-----------|----------------|-------|
| **rebar3** | 3.24.0 | Build tool operational |
| **Erlang/OTP** | OTP 27 (ERTS 15.2.7.1) | Latest stable version |
| **EPMD** | Installed | Port mapper daemon operational |
| **EUnit** | Built-in | Unit testing framework |
| **Common Test** | Built-in | Integration testing framework |
| **Proper** | 1.4.0 | Property-based testing configured |
| **Dialyzer** | Configured | Type checking (warnings expected) |
| **Docker** | Installed | Multi-stage builds working |

### Test Files Inventory

- **Unit Tests**: 46 files (`*_tests.erl`)
- **Integration Suites**: 5 files (`*_SUITE.erl`)
- **Total Test Files**: 51 files

### CI/CD Workflows

- **GitHub Actions**: 18 workflow files
- **Multi-Version Testing**: OTP 25, 26, 27, 28
- **Quality Gates**: Compilation, Tests, Coverage, Dialyzer

---

## External Dependencies

### Runtime Dependencies (Auto-Started)

1. **EPMD** (Erlang Port Mapper Daemon)
   - **Port**: 4369
   - **Purpose**: Distributed Erlang node discovery
   - **Status**: Auto-started by Erlang
   - **Cleanup**: `epmd -kill` (if conflicts occur)

2. **Mnesia** (Distributed Database)
   - **Location**: `./Mnesia.nonode@nohost/`
   - **Purpose**: Session storage, cache, receipts
   - **Status**: Auto-created by OTP applications
   - **Cleanup**: `mnesia:delete_schema([node()])`

### Network Ports (Test Configuration)

All test servers use **OS-assigned ports** (`port = 0`):
- HTTP: 8080-8090 (random)
- WebSocket: 8080-8090 (random)
- TCP: Dynamic (random)
- Metrics: 9090 (Prometheus)
- Distribution: 9100-9200 (Erlang nodes)

---

## Test Framework Usage

### EUnit (Unit Testing)

**Test Files**: `apps/*/test/*_tests.erl`

```erlang
-module(erlmcp_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% Simple test
basic_test() ->
    ?assertEqual(1, 1).

%% Test with setup/teardown
server_test_() ->
    {setup,
     fun() -> {ok, Pid} = erlmcp_server:start_link(), Pid end,
     fun(Pid) -> erlmcp_server:stop(Pid) end,
     fun(Pid) ->
         [
          ?_test(?assert(is_pid(Pid)))
         ]
     end}.
```

**Run**:
```bash
rebar3 eunit
rebar3 eunit --module=erlmcp_server_tests
```

### Common Test (Integration Testing)

**Test Files**: `test/*_SUITE.erl`

```erlang
-module(erlmcp_integration_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() -> [test_case_1, test_case_2].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp).

test_case_1(_Config) ->
    {ok, Pid} = erlmcp_server:start_link(),
    ?assert(is_pid(Pid)),
    erlmcp_server:stop(Pid).
```

**Run**:
```bash
rebar3 ct
rebar3 ct --suite=erlmcp_integration_SUITE
rebar3 ct --dir=test/integration
```

### Proper (Property-Based Testing)

**Test Files**: `apps/*/test/*_tests.erl`

```erlang
-module(erlmcp_json_rpc_tests).
-include_lib("proper/include/proper.hrl").

prop_encode_decode_roundtrip() ->
    ?FORALL(Message, message_generator(),
        begin
            Encoded = erlmcp_json_rpc:encode(Message),
            {ok, Decoded} = erlmcp_json_rpc:decode(Encoded),
            Decoded =:= Message
        end).

message_generator() ->
    ?LET({Id, Method, Params},
        {integer(), binary(), proper_types:list(any())},
        #{jsonrpc => <<"2.0">>, id => Id, method => Method, params => Params}).
```

**Run**:
```bash
rebar3 proper
rebar3 proper -c  # With coverage
```

---

## Configuration Files

### System Configuration

- **Development**: `config/sys.config`
- **Testing**: `config/test.config`
- **Production**: `config/prod.config`

### VM Arguments

- **Main**: `vm.args`
- **Test-specific**: See `config/test.config` (vm_args section)

### Rebar Configuration

- **Root**: `rebar.config` (umbrella project)
- **App-specific**: `apps/*/rebar.config`

---

## Docker Testing

### Build Test Image
```bash
docker build -f Dockerfile.dev -t erlmcp:test .
```

### Run Tests in Container
```bash
# Quick test
docker run --rm erlmcp:test make test

# With coverage
docker run --rm -v $(pwd)/_build:/app/_build erlmcp:test make coverage

# Interactive shell
docker run --rm -it erlmcp:test /bin/sh
```

### Docker Compose
```bash
# Start all services
docker-compose up -d

# Run tests
docker-compose exec erlmcp make test

# View logs
docker-compose logs -f erlmcp
```

---

## Troubleshooting

### Common Issues

**Issue**: Tests fail with "connection refused"
```bash
# Solution: Check EPMD
epmd -names
epmd -daemon
```

**Issue**: Tests fail with "port already in use"
```bash
# Solution: Kill process using port
lsof -ti:8080 | xargs kill -9

# Better: Use port 0 (OS-assigned) in tests
```

**Issue**: Mnesia startup fails
```bash
# Solution: Clean Mnesia schema
rm -rf Mnesia.* test_mnesia
erl -sname test -eval "mnesia:delete_schema([node()]), init:stop()."
```

**Issue**: Tests timeout in CI
```bash
# Solution: Increase timeout in .github/workflows/*.yml
timeout-minutes: 60
```

### Debugging Tips

**Enable verbose logging**:
```bash
# In test config
{kernel, [{logger_level, debug}]}.
```

**Run single test**:
```bash
rebar3 eunit --module=erlmcp_server_tests
```

**Run test in shell**:
```bash
rebar3 shell --config config/test.config
> erlmcp_server_tests:basic_test().
```

**Check test coverage**:
```bash
rebar3 cover
open _build/test/cover/index.html
```

---

## Quality Gates

### Pre-Commit Checklist

- [ ] Compilation passes: `make compile`
- [ ] Tests pass: `make test`
- [ ] Coverage ≥80%: `make coverage`
- [ ] Dialyzer passes: `make dialyzer`
- [ ] Xref passes: `make xref`

### CI/CD Gates (Blocking)

1. **Compilation**: 0 errors required
2. **EUnit Tests**: 0 failures required
3. **Coverage**: ≥80% required
4. **Dialyzer**: Type checking (warnings allowed)
5. **Xref**: Cross-reference (warnings allowed)

### Quality Metrics

- **Test Health**: 85% (some flakiness due to EPMD/Mnesia)
- **Documentation**: 60% (needs TESTING.md)
- **Infrastructure**: 80% (production-ready)

---

## Best Practices

### Chicago School TDD (Recommended)

✅ **DO**:
- Use real collaborators (spawn real gen_servers)
- Test observable state (API results, message receipts)
- Test behaviors and outputs, not internal calls
- Integrate components together when practical

❌ **DON'T**:
- Use mock objects (meck, mocking frameworks)
- Verify internal method calls
- Test implementation details
- Use stubs of collaborators

### Test Isolation

- **Use setup/teardown**: Clean state before/after each test
- **Random ports**: Avoid port conflicts with `port = 0`
- **EPMD cleanup**: Kill EPMD before distributed tests
- **Mnesia cleanup**: Delete schema before database tests

### Test Organization

- **Unit tests**: `apps/*/test/*_tests.erl` (one per module)
- **Integration tests**: `test/*_SUITE.erl` (multi-process scenarios)
- **Property tests**: Add `prop_*` functions to test files
- **Test helpers**: `test/tcps_test_helper.erl` (shared fixtures)

---

## Resources

### Documentation

- **Full Analysis**: `docs/TEST_DEPENDENCIES_ANALYSIS.md`
- **OTP Patterns**: `docs/otp-patterns.md`
- **API Reference**: `docs/api-reference.md`
- **Architecture**: `docs/architecture.md`

### Test Scripts

- **Build & Test**: `scripts/build_and_test.sh`
- **Coverage Check**: `scripts/check_coverage_threshold.sh`
- **Smoke Tests**: `scripts/smoke_tests.sh`
- **Integration**: `scripts/run_integration_tests.sh`

### Make Targets

```bash
make help              # Show all targets
make test              # Run all tests
make eunit             # EUnit only
make ct                # Common Test only
make coverage          # Coverage report
make validate          # All quality gates
make dialyzer          # Type checking
make xref              # Cross-reference
```

---

## Summary

The erlmcp test infrastructure is **PRODUCTION-READY** with:

- ✅ **Solid foundation**: rebar3, OTP 27, EUnit, CT, Proper
- ✅ **CI/CD integration**: GitHub Actions with quality gates
- ✅ **Docker support**: Multi-stage builds for dev/prod
- ⚠️ **Minor gaps**: EPMD/Mnesia cleanup, documentation

**Recommended workflow**:
```bash
# Before committing
make validate

# In CI/CD (automatic)
make test-strict
make coverage-strict
make quality-strict
```

**Overall Health**: 80% (Production-ready with improvements needed)

---

**For detailed analysis**, see: `docs/TEST_DEPENDENCIES_ANALYSIS.md`

**Last Updated**: 2026-01-29
**Next Review**: 2026-02-28 (monthly review recommended)
