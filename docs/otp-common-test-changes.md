# Common Test Changes Across OTP 26-28

## Executive Summary

This document consolidates Common Test framework changes across Erlang/OTP versions 26, 27, and 28, providing guidance for cross-OTP compatibility in erlmcp test suites.

## Table of Contents

1. [OTP 26 Changes (May 2023)](#otp-26-changes)
2. [OTP 27 Changes (2024)](#otp-27-changes)
3. [OTP 28 Changes (May 2025)](#otp-28-changes)
4. [Compatibility Matrix](#compatibility-matrix)
5. [Migration Guide](#migration-guide)
6. [Best Practices](#best-practices)

---

## OTP 26 Changes (May 2023)

### Common Test 1.23.x

#### Improvements

**1. Robust Release Fetching (OTP-18340)**
- **What Changed**: Updated common_test with a more robust mechanism for fetching old releases while properly ignoring the current release.
- **Impact**: Improved reliability when testing against multiple OTP versions
- **Compatibility**: Fully backward compatible
- **Migration**: No action required

**2. Release Handling**
- Better handling of release packages during test execution
- Improved error messages for missing dependencies

#### Breaking Changes

**None** - OTP 26 Common Test is fully backward compatible with OTP 25.

---

## OTP 27 Changes (2024)

### Common Test 1.27.x

#### Major Improvements

**1. Colored Failing Lines (OTP-18898/PR-7917)**
- **What Changed**: Failing lines in test source code are now colored to make them easier to identify on the screen.
- **Impact**: Improved developer experience when debugging failing tests
- **Compatibility**: Fully backward compatible (colors are optional)
- **Configuration**:
  ```erlang
  % Enable/disable colors in suite configuration
  {ct_colors, true}.
  ```
- **Migration**: No action required - automatic on supported terminals

**2. Native Code Coverage Support**
- **What Changed**: JIT-based native code coverage collection
- **Benefits**:
  - More efficient code coverage collection
  - Support for previously uncoverable code
  - Minimal performance impact: *"When running the test suites for most OTP applications, there is no noticeable difference in execution time running with and without Cover."*
- **Requirements**:
  - Code must be compiled with `line_coverage` option
  - Native coverage must be enabled in the runtime system
- **Compilation**:
  ```erlang
  % In rebar.config:
  {erl_opts, [debug_info, line_coverage]}.
  ```
- **Runtime**:
  ```erlang
  % Enable native coverage
  cover:compile_module(Module, [{native, true}]).
  ```
- **Migration**: Optional feature - existing cover continues to work

**3. Backward Compatibility Fix (OTP-19385)**
- **What Changed**: Common Test no longer crashes when running tests with OTP 26 and earlier while having previous test results from OTP 27.
- **Impact**: Better cross-version testing support
- **Migration**: No action required

**4. Documentation Updates**
- Function specifications and types added/updated for Common Test APIs
- Improved type safety with dialyzer

#### Breaking Changes

**None** - OTP 27 Common Test is fully backward compatible with OTP 26.

---

## OTP 28 Changes (May 2025)

### Common Test 1.29.x

#### Improvements

**1. Pre-End Hooks Config Delivery (OTP-28)**
- **What Changed**: Config data from `pre_end_per_testcase` hook is now delivered to `post_end_per_testcase` callback in case of testcase timetrap or linked process crash.
- **Impact**: Better hooks for test cleanup and error handling
- **Compatibility**: New feature, no breaking changes
- **Example**:
  ```erlang
  %% In pre_end_per_testcase
  pre_end_per_testcase(TestCase, Config) ->
      {config, Config, #{extra_info => data}}.

  %% In post_end_per_testcase
  post_end_per_testcase(TestCase, Config, Extra) ->
      ct:log("Extra data: ~p", [Extra]).
  ```

**2. SBOM Improvements**
- Updated vendor dependencies SHA to improve accuracy of the source SBOM with purl pointing to the repository
- Better supply chain security for Common Test itself

**3. Enhanced Error Reporting**
- Better stack traces and error context in test logs
- Improved HTML test report formatting

#### Breaking Changes

**None** - OTP 28 Common Test is fully backward compatible with OTP 27.

---

## Compatibility Matrix

### Callback Support

| Callback | OTP 25 | OTP 26 | OTP 27 | OTP 28 | Notes |
|----------|--------|--------|--------|--------|-------|
| `all/0` | ✅ | ✅ | ✅ | ✅ | Required |
| `suite/0` | ✅ | ✅ | ✅ | ✅ | Optional |
| `groups/0` | ✅ | ✅ | ✅ | ✅ | Optional |
| `init_per_suite/1` | ✅ | ✅ | ✅ | ✅ | Optional |
| `end_per_suite/1` | ✅ | ✅ | ✅ | ✅ | Optional |
| `init_per_group/2` | ✅ | ✅ | ✅ | ✅ | Optional |
| `end_per_group/2` | ✅ | ✅ | ✅ | ✅ | Optional |
| `init_per_testcase/2` | ✅ | ✅ | ✅ | ✅ | Optional |
| `end_per_testcase/2` | ✅ | ✅ | ✅ | ✅ | Optional |
| `pre_end_per_testcase/2` | ✅ | ✅ | ✅ | ✅ | Optional |
| `post_end_per_testcase/3` | ✅ | ✅ | ✅ | ✅ | Enhanced in OTP 28 |

### Features

| Feature | OTP 25 | OTP 26 | OTP 27 | OTP 28 | Notes |
|---------|--------|--------|--------|--------|-------|
| Colored Output | ❌ | ❌ | ✅ | ✅ | Automatic in OTP 27+ |
| Native Coverage | ❌ | ❌ | ✅ | ✅ | Requires `line_coverage` compile |
| SBOM Support | ❌ | ❌ | ❌ | ✅ | Supply chain security |
| Hook Config Pass-through | ❌ | ❌ | ❌ | ✅ | Enhanced error handling |
| HTML Reports | ✅ | ✅ | ✅ | ✅ | Improved in OTP 28 |

---

## Migration Guide

### From OTP 25/26 to OTP 27

**Step 1: Enable Line Coverage (Optional)**
```erlang
% In rebar.config:
{erl_opts, [debug_info, line_coverage]}.
```

**Step 2: Verify Test Suites**
- All existing test suites continue to work without changes
- Run full test suite to verify compatibility:
  ```bash
  rebar3 ct --suite apps/*/test/*_SUITE.erl
  ```

**Step 3: Upgrade to Native Coverage (Optional)**
```erlang
% In test suite:
init_per_suite(Config) ->
    {ok, _} = cover:compile_beam(directory, [{native, true}]),
    Config.
```

### From OTP 27 to OTP 28

**Step 1: No Breaking Changes**
- All OTP 27 test suites work on OTP 28 without modification
- Run full test suite to verify:
  ```bash
  rebar3 ct
  ```

**Step 2: Leverage Enhanced Hooks (Optional)**
```erlang
% Use enhanced hook config pass-through
pre_end_per_testcase(_TestCase, Config) ->
    {config, Config, #{cleanup_info => important_data}}.

post_end_per_testcase(_TestCase, _Config, Extra) ->
    ct:log("Cleanup: ~p", [Extra]),
    ok.
```

**Step 3: Verify SBOM (Optional)**
```bash
% Check SBOM generation
rebar3 tree | grep common_test
```

---

## Best Practices

### 1. Cross-OTP Compatibility

**Use Feature Detection, Not Version Checks**
```erlang
% ❌ BAD: Hard-coded version check
init_per_suite(Config) ->
    case erlang:system_info(otp_release) of
        "27" -> setup_otp27();
        "28" -> setup_otp28()
    end.

% ✅ GOOD: Feature detection
init_per_suite(Config) ->
    case has_native_coverage() of
        true  -> setup_native_coverage();
        false -> setup_regular_coverage()
    end.

has_native_coverage() ->
    try
        {ok, _} = cover:compile_module(dummy, [{native, true}]),
        true
    catch
        _:_ -> false
    end.
```

**Provide Fallback Implementations**
```erlang
% ✅ GOOD: Feature with fallback
get_processes() ->
    case has_process_iterator() of
        true  -> erlang:processes_iterator();  % OTP 28+
        false -> erlang:processes()           % OTP 25-27
    end.

has_process_iterator() ->
    try
        _ = erlang:processes_iterator(),
        true
    catch
        error:undef -> false
    end.
```

### 2. Test Suite Structure

**Standard Callback Order**
```erlang
-module(my_SUITE).
-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

all() ->
    [{group, feature_x}, {group, feature_y}].

groups() ->
    [{feature_x, [parallel], [test1, test2]},
     {feature_y, [sequence], [test3, test4]}].

init_per_suite(Config) ->
    application:ensure_all_started(my_app),
    Config.

end_per_suite(_Config) ->
    application:stop(my_app),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.
```

**Use Groups for Organization**
```erlang
% ✅ GOOD: Organized test groups
groups() ->
    [{lifecycle, [sequence],
      [init, connect, authenticate, disconnect, cleanup]},
     {api, [parallel],
      [list_tools, call_tool, list_resources]},
     {error_handling, [parallel],
      [timeout, invalid_args, server_error]}].
```

### 3. Configuration Management

**Use Proplists for Config**
```erlang
% ✅ GOOD: Standard config pattern
init_per_suite(Config) ->
    {ok, Server} = my_server:start_link(),
    [{server_pid, Server} | Config].

end_per_suite(Config) ->
    Server = proplists:get_value(server_pid, Config),
    gen_server:stop(Server),
    ok.
```

**Clean Up in Reverse Order**
```erlang
% ✅ GOOD: LIFO cleanup
end_per_suite(Config) ->
    % Stop in reverse order of startup
    application:stop(transports),
    application:stop(core),
    application:stop(dependencies),
    ok.
```

### 4. Error Handling

**Let Tests Fail Clearly**
```erlang
% ❌ BAD: Silent failures
test_something(_Config) ->
    case risky_operation() of
        {ok, _} -> ok;
        _ -> ok  % Swallows errors
    end.

% ✅ GOOD: Explicit failures
test_something(_Config) ->
    ?assertEqual({ok, expected}, risky_operation()).
```

**Use Timeouts Appropriately**
```erlang
% ✅ GOOD: Configurable timeouts
init_per_suite(Config) ->
    Timeout = proplists:get_value(test_timeout, Config, 5000),
    [{test_timeout, Timeout} | Config].

test_async_operation(Config) ->
    Timeout = proplists:get_value(test_timeout, Config),
    receive
        {result, Result} -> Result
    after Timeout ->
        ct:fail("Timeout waiting for result")
    end.
```

### 5. Performance

**Use Parallel Test Execution**
```erlang
% ✅ GOOD: Parallel tests for independence
groups() ->
    [{fast_tests, [parallel],
      [test1, test2, test3, test4, test5]}].
```

**Avoid Expensive Setup**
```erlang
% ❌ BAD: Expensive per-testcase setup
init_per_testcase(_TestCase, Config) ->
    {ok, Db} = connect_to_database(),  % Expensive!
    [{db, Db} | Config].

% ✅ GOOD: Suite-level setup
init_per_suite(Config) ->
    {ok, Db} = connect_to_database(),
    [{db, Db} | Config].

init_per_testcase(_TestCase, Config) ->
    Db = proplists:get_value(db, Config),
    {ok, Tx} = db:start_transaction(Db),
    [{tx, Tx} | Config].

end_per_testcase(_TestCase, Config) ->
    Tx = proplists:get_value(tx, Config),
    db:rollback_transaction(Tx),
    ok.
```

### 6. Coverage

**Target 80%+ Coverage**
```erlang
% In rebar.config:
{plugins, [rebar3_cover]}.
{cover_opts, [verbose]}.

% Set coverage goal:
{cover_opts, [{verbose, true}, {min_coverage, 80}]}.
```

**Use Native Coverage in OTP 27+**
```erlang
init_per_suite(Config) ->
    % Try native coverage first (OTP 27+)
    case has_native_coverage() of
        true ->
            ct:log("Using native coverage (OTP 27+)"),
            {ok, _} = cover:compile_beam(directory, [{native, true}]),
            Config;
        false ->
            ct:log("Using regular coverage"),
            Config
    end.
```

---

## Testing Recommendations

### 1. Run Tests on All Target OTP Versions

```bash
# OTP 26
kerl install 26.2.5 26
kerl use 26
rebar3 ct

# OTP 27
kerl install 27.2 27
kerl use 27
rebar3 ct

# OTP 28
kerl install 28.0 28
kerl use 28
rebar3 ct
```

### 2. Use Version-Specific Test Suites

```erlang
% otp_compat_SUITE.erl
-module(otp_compat_SUITE).

all() ->
    [test_otp26_features,
     test_otp27_features,
     test_otp28_features].

test_otp27_features(Config) ->
    OTPVersion = proplists:get_value(otp_version, Config),
    case OTPVersion >= 27 of
        true ->
            % Test native coverage
            ?assert(has_native_coverage());
        false ->
            {skip, "Native coverage requires OTP 27+"}
    end.

test_otp28_features(Config) ->
    OTPVersion = proplists:get_value(otp_version, Config),
    case OTPVersion >= 28 of
        true ->
            % Test enhanced hooks
            ?assert(has_enhanced_hooks());
        false ->
            {skip, "Enhanced hooks require OTP 28+"}
    end.
```

### 3. Continuous Integration Matrix

```yaml
# .github/workflows/test.yml
jobs:
  test:
    strategy:
      matrix:
        otp: [26, 27, 28]
        os: [ubuntu-latest]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v3
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
      - run: rebar3 ct
      - run: rebar3 cover
```

---

## Troubleshooting

### Common Issues

**Issue 1: Colored Output Not Working**
```
Solution: Check terminal support
% Force enable
{ct_colors, true}.

% Or disable
{ct_colors, false}.
```

**Issue 2: Native Coverage Not Available**
```
Error: cover:compile_module fails with {error, not_supported}
Solution: Ensure code compiled with line_coverage
{erl_opts, [debug_info, line_coverage]}.
```

**Issue 3: Test Results Incompatible Across Versions**
```
Error: Common Test crashes reading old test results
Solution: Clean test results between OTP versions
rm -rf _build/test/logs/
```

**Issue 4: Hook Config Not Passed in OTP 28**
```
Error: post_end_per_testcase missing config
Solution: Ensure pre_end_per_testcase returns {config, Config, Extra}
pre_end_per_testcase(TestCase, Config) ->
    {config, Config, #{extra => data}}.
```

---

## References

### Official Documentation

- [Common Test User's Guide](https://www.erlang.org/doc/apps/common_test/users_guide.html)
- [Common Test Release Notes](https://www.erlang.org/doc/apps/common_test/notes.html)
- [Erlang/OTP 26 Release Notes](https://www.erlang.org/news/164)
- [Erlang/OTP 27 Highlights](https://www.erlang.org/blog/highlights-otp-27/)
- [Erlang/OTP 28 Highlights](https://www.erlang.org/blog/highlights-otp-28/)

### erlmcp-Specific

- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_otp28_compat_SUITE.erl` - OTP 28 compatibility test suite
- `/Users/sac/erlmcp/test/CT_COMPATIBILITY_REPORT.md` - Compatibility analysis
- `/Users/sac/erlmcp/docs/architecture/ct-compatibility-design.md` - Compatibility architecture

---

## Summary

**Key Takeaways**:

1. **No Breaking Changes**: All Common Test changes across OTP 26-28 are backward compatible
2. **Optional Features**: New features (colored output, native coverage) are opt-in
3. **Feature Detection**: Use runtime feature detection, not hard-coded version checks
4. **Test Thoroughly**: Verify compatibility on all target OTP versions
5. **Leverage New Features**: Native coverage in OTP 27+ improves performance

**Compatibility Status**: ✅ All erlmcp CT suites are compatible with OTP 26, 27, and 28

---

*Last Updated: 2026-02-01*
*OTP Versions Covered: 26.0-28.3.1*
