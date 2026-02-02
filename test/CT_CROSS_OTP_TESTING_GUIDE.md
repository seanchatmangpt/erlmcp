# Cross-OTP Testing Guide for erlmcp

## Quick Start

```bash
# Test on all supported OTP versions
for otp in 26 27 28; do
    kerl use $otp
    rebar3 ct
    rebar3 cover
done

# Or use the compat test suite
rebar3 ct --suite erlmcp_ct_compat
```

## Overview

This guide provides comprehensive instructions for testing erlmcp across Erlang/OTP versions 26, 27, and 28. All 37 Common Test suites are fully compatible with no breaking changes.

## Prerequisites

### Install Multiple OTP Versions

Using [kerl](https://github.com/kerl/kerl):

```bash
# Install kerl
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod +x kerl
mv kerl /usr/local/bin/

# Build and install OTP versions
kerl build 26.2.5 26
kerl build 27.2 27
kerl build 28.0 28

# Install
kerl install 26 26
kerl install 27 27
kerl install 28 28
```

### Verify Installation

```bash
# Test each version
kerl use 26
erl -version
% Erlang/OTP 26 [erts-13.0]

kerl use 27
erl -version
% Erlang/OTP 27 [erts-14.0]

kerl use 28
erl -version
% Erlang/OTP 28 [erts-15.0]
```

## Running Tests

### Basic Test Execution

```bash
# Run all CT suites
rebar3 ct

# Run specific suite
rebar3 ct --suite apps/erlmcp_core/test/erlmcp_e2e_SUITE

# Run specific test case
rebar3 ct --suite erlmcp_e2e_SUITE --case e2e_test_full_mcp_lifecycle

# Run with verbose output
rebar3 ct --verbose

# Run with cover
rebar3 ct --cover
```

### Cross-Version Testing

```bash
# Test on all OTP versions
for otp in 26 27 28; do
    echo "Testing on OTP $otp"
    kerl use $otp
    rebar3 compile
    rebar3 ct
    rebar3 cover
done

# Or use make (if target exists)
make test-all-otp
```

### Compatibility Test Suite

```bash
# Run the compatibility test suite
rebar3 ct --suite apps/erlmcp_core/test/erlmcp_ct_compat_SUITE

# This suite tests:
# - OTP version detection
# - Feature detection (native coverage, process iterator, etc.)
# - Callback compatibility
# - Config management
# - Timeout handling
# - Parallel execution
```

## Using the Compat Module

### Start the Compat Helper

```erlang
% In init_per_suite/1
init_per_suite(Config) ->
    {ok, _} = erlmcp_ct_compat:start_link(),
    Config.
```

### Detect Features

```erlang
% Check if native coverage is available (OTP 27+)
case erlmcp_ct_compat:supports_native_coverage() of
    true ->
        {ok, _} = cover:compile_beam(directory, [{native, true}]),
        Config;
    false ->
        Config
end.

% Check if process iterator is available (OTP 28+)
case erlmcp_ct_compat:supports_process_iterator() of
    true ->
        Processes = erlmcp_ct_compat:get_processes(),
        ct:log("Using process iterator: ~p processes", [length(Processes)]);
    false ->
        Processes = erlang:processes(),
        ct:log("Using processes/0: ~p processes", [length(Processes)])
end.
```

### Use JSON Compatibility

```erlang
% Encode JSON (uses json module in OTP 28+, jsx otherwise)
Data = #{key => <<"value">>},
JSON = erlmcp_ct_compat:encode_json(Data),

% Decode JSON
Decoded = erlmcp_ct_compat:decode_json(JSON).
```

### Timeout Handling

```erlang
% Execute with timeout (works on all OTP versions)
Fun = fun() -> long_running_operation() end,
case erlmcp_ct_compat:with_timeout(my_test, 5000, Fun) of
    {ok, Result} ->
        ct:log("Operation completed: ~p", [Result]);
    {error, timeout} ->
        ct:fail("Operation timed out")
end.
```

## Test Suite Structure

### Standard Callbacks

```erlang
-module(my_SUITE).
-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

all() ->
    [{group, my_group}].

groups() ->
    [{my_group, [parallel], [test1, test2, test3]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(my_app),
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

### Config Management

```erlang
% Add config data
Config1 = [{key1, value1} | Config],

% Retrieve config data
Value = proplists:get_value(key1, Config1),

% Pass through callbacks
init_per_suite(Config) ->
    Config1 = [{suite_data, data} | Config],
    Config1.

init_per_testcase(TestCase, Config) ->
    SuiteData = proplists:get_value(suite_data, Config),
    Config2 = [{test_data, SuiteData} | Config],
    Config2.
```

### Cleanup Patterns

```erlang
% LIFO cleanup (reverse order of startup)
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(dep3),
    {ok, _} = application:ensure_all_started(dep2),
    {ok, _} = application:ensure_all_started(dep1),
    Config.

end_per_suite(_Config) ->
    application:stop(dep1),  % Stopped first
    application:stop(dep2),  % Stopped second
    application:stop(dep3),  % Stopped last
    ok.
```

## Feature Detection Patterns

### OTP Version Detection

```erlang
% ❌ BAD: Hard-coded version check
case erlang:system_info(otp_release) of
    "28" -> use_otp28_feature();
    "27" -> use_otp27_feature();
    _ -> use_legacy()
end.

% ✅ GOOD: Feature detection
case erlmcp_ct_compat:has_feature(json_module) of
    true -> use_json_module();
    false -> use_jsx()
end.
```

### Conditional Testing

```erlang
% Skip test if feature not available
test_new_feature(Config) ->
    case erlmcp_ct_compat:supports_process_iterator() of
        true ->
            % Test the feature
            Iterator = erlang:processes_iterator(),
            ?assert(is_function(Iterator));
        false ->
            % Skip with reason
            {skip, "Process iterator requires OTP 28+"}
    end.
```

### Graceful Degradation

```erlang
% Use best available implementation
get_all_processes() ->
    case erlmcp_ct_compat:supports_process_iterator() of
        true ->
            % Use efficient iterator (OTP 28+)
            erlmcp_ct_compat:get_processes();
        false ->
            % Use legacy method (OTP 26-27)
            erlang:processes()
    end.
```

## Native Coverage (OTP 27+)

### Enable Native Coverage

Add to `rebar.config`:

```erlang
{erl_opts, [debug_info, line_coverage]}.
```

### Use in Test Suite

```erlang
init_per_suite(Config) ->
    case erlmcp_ct_compat:supports_native_coverage() of
        true ->
            ct:log("Enabling native code coverage (OTP 27+)"),
            {ok, _} = cover:compile_beam(directory, [{native, true}]),
            Config;
        false ->
            ct:log("Using regular code coverage (OTP 26)"),
            Config
    end.
```

### Run with Coverage

```bash
# Basic coverage
rebar3 ct --cover

# Detailed coverage report
rebar3 cover
open _build/test/cover/index.html
```

## Continuous Integration

### GitHub Actions Matrix

```yaml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: [26, 27, 28]
        rebar3: 3.22.1

    steps:
      - uses: actions/checkout@v3

      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
          rebar3-version: ${{ matrix.rebar3 }}

      - name: Compile
        run: rebar3 compile

      - name: Run Tests
        run: rebar3 ct --cover

      - name: Upload Coverage
        uses: actions/upload-artifact@v3
        with:
          name: coverage-${{ matrix.otp }}
          path: _build/test/cover/
```

### GitLab CI Matrix

```yaml
test:
  image: erlang:27
  parallel:
    matrix:
      - OTP: ['26', '27', '28']
  script:
    - kerl use $OTP
    - rebar3 ct --cover
  artifacts:
    paths:
      - _build/test/cover/
```

## Troubleshooting

### Common Issues

**Issue**: Test fails on OTP 26 but passes on OTP 27-28

```
Solution: Check for OTP 27+ features
grep -r "processes_iterator\|json:encode\|native.*coverage" apps/*/test/
```

**Issue**: Coverage compilation fails

```
Solution: Ensure line_coverage is enabled
{erl_opts, [debug_info, line_coverage]}.
```

**Issue**: Timeout in test execution

```
Solution: Increase timeout or use compat module
erlmcp_ct_compat:with_timeout(test_name, TimeoutMs, Fun)
```

### Debug Mode

```bash
# Run with verbose output
rebar3 ct --verbose

# Run with shell for debugging
rebar3 ct --shell

# Keep test results
rebar3 ct --keep_logs
```

## Best Practices

### 1. Use Feature Detection

```erlang
% ✅ GOOD
case erlmcp_ct_compat:has_feature(Feature) of
    true -> use_feature();
    false -> use_fallback()
end.

% ❌ BAD
case erlang:system_info(otp_release) of
    "28" -> use_feature();
    _ -> skip
end.
```

### 2. Clean Up in Reverse Order

```erlang
% ✅ GOOD: LIFO cleanup
end_per_suite(_Config) ->
    application:stop(app1),  % Started last
    application:stop(app2),  % Started first
    ok.
```

### 3. Use Real Processes

```erlang
% ✅ GOOD: Real gen_server
{ok, Pid} = my_server:start_link(),
?assert(is_process_alive(Pid)),
my_server:stop(Pid).

% ❌ BAD: Mock
meck:new(my_server),
meck:expect(my_server, start_link, fun() -> {ok, fake_pid} end).
```

### 4. Test Observable Behavior

```erlang
% ✅ GOOD: State verification
ok = my_server:set_value(Key, Value),
{ok, Value} = my_server:get_value(Key).

% ❌ BAD: Implementation testing
?assert(called, my_server, handle_call, [_, _, _]).
```

### 5. Use Groups for Organization

```erlang
% ✅ GOOD: Logical grouping
groups() ->
    [{lifecycle, [sequence],
      [init, connect, authenticate, disconnect, cleanup]},
     {api_tests, [parallel],
      [list_tools, call_tool, list_resources]}].
```

## Resources

### Documentation

- [Common Test User's Guide](https://www.erlang.org/doc/apps/common_test/users_guide.html)
- [OTP Changes Across Versions](/Users/sac/erlmcp/docs/otp-common-test-changes.md)
- [Compatibility Report](/Users/sac/erlmcp/test/CT_COMPATIBILITY_REPORT.md)

### Test Suites

- [CT Compat Suite](/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_ct_compat_SUITE.erl)
- [OTP 28 Compat Suite](/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_otp28_compat_SUITE.erl)
- [E2E Suite](/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_e2e_SUITE.erl)

### Helper Modules

- [CT Compat Module](/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_ct_compat.erl)

## Summary

✅ **All 37 CT suites compatible with OTP 26, 27, and 28**
✅ **Zero breaking changes**
✅ **Optional enhancements available (native coverage, enhanced hooks)**
✅ **Comprehensive compatibility utilities provided**

---

*Last Updated: 2026-02-01*
