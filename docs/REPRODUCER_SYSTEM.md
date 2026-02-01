# Armstrong-Style Reproducer System

## Overview

The Armstrong-Style Reproducer System captures every interop failure as a self-contained, executable test case. Following Joe Armstrong's philosophy of "make every failure a gift," each failure produces a reproducer that:

1. **Is executable** - `reproducer_<id>:run()` replays the exact failure
2. **Is self-contained** - Includes all inputs, expected outputs, actual outputs
3. **Is traceable** - Links to violated rule IDs for root cause analysis
4. **Is persistent** - Stored as Erlang modules in `test/reproducers/`
5. **Is testable** - Integrates with EUnit/CT for regression testing

## Philosophy

> "Make every failure a gift" - Joe Armstrong

**Core Principles:**
- Every failure is an opportunity to improve the system
- Reproducers are first-class artifacts, not disposable logs
- Self-contained reproducers enable asynchronous debugging
- Executable reproducers serve as regression tests
- Reproducers capture exact failure conditions, not approximations

## Architecture

### Core Module: `erlmcp_reproducer.erl`

**Location:** `apps/erlmcp_core/src/erlmcp_reproducer.erl`

**Responsibilities:**
- Capture failure scenarios
- Generate reproducer modules
- Store reproducers to disk
- Track fixed/unfixed status
- Generate audit reports

**API:**
```erlang
%% Capture failures
erlmcp_reproducer:capture_protocol_failure(RuleId, Input, Expected, Actual)
erlmcp_reproducer:capture_transport_failure(RuleId, Input, Expected, Actual)
erlmcp_reproducer:capture_sse_failure(RuleId, Input, Expected, Actual)

%% Query reproducers
erlmcp_reproducer:list_all()
erlmcp_reproducer:list_unfixed()
erlmcp_reproducer:get_reproducer(Id)

%% Replay
erlmcp_reproducer:replay(Scenario)

%% Track fixes
erlmcp_reproducer:mark_fixed(Id)

%% Audit
erlmcp_reproducer:audit_report()
```

### Validator Integration: `erlmcp_validator_hooks.erl`

**Location:** `apps/erlmcp_validation/src/erlmcp_validator_hooks.erl`

**Responsibilities:**
- Provide hooks for validators to emit reproducers
- Control global capture enable/disable
- Gracefully handle reproducer system absence

**API:**
```erlang
%% Enable/disable capture
erlmcp_validator_hooks:enable_reproducer_capture()
erlmcp_validator_hooks:disable_reproducer_capture()

%% Emit failures
erlmcp_validator_hooks:emit_protocol_failure(RuleId, Expected, Actual)
erlmcp_validator_hooks:emit_transport_failure(RuleId, Expected, Actual)
erlmcp_validator_hooks:emit_sse_failure(RuleId, Expected, Actual)
```

## Reproducer Format

Each reproducer is a self-contained Erlang module:

```erlang
-module(reproducer_20260201_120000_001).
-export([run/0, scenario/0]).

scenario() ->
    #{
        rule_id => <<"SSE_INVALID_RESUME_ID">>,
        description => <<"SSE client sends non-string resume-id">>,
        category => sse,
        input => [
            {sse_connect, #{session_id => <<"session_123">>}},
            {sse_message, #{resume_id => 12345}}  % Invalid: should be binary
        ],
        expected => {error, invalid_resume_id},
        actual => {error, {bad_resume_id, 12345}}
    }.

run() ->
    S = scenario(),
    Result = erlmcp_reproducer:replay(S),
    case Result of
        {ok, Expected} -> {ok, fixed};
        {error, {mismatch, _}} -> {error, still_failing};
        _ -> {error, unexpected_result}
    end.
```

## Example Reproducers

### 1. SSE Invalid Resume-ID
**File:** `test/reproducers/reproducer_20260201_120000_001.erl`
**Rule:** `SSE_INVALID_RESUME_ID`
**Issue:** SSE client sends integer resume-id instead of binary

### 2. JSON-RPC Wrong Version
**File:** `test/reproducers/reproducer_20260201_120000_002.erl`
**Rule:** `JSONRPC_WRONG_VERSION`
**Issue:** Client sends JSON-RPC 1.0 instead of 2.0 (FIXED)

### 3. Initialize Missing ClientInfo
**File:** `test/reproducers/reproducer_20260201_120000_003.erl`
**Rule:** `INITIALIZE_MISSING_CLIENT_INFO`
**Issue:** Initialize request without required clientInfo field

### 4. Invalid Error Code
**File:** `test/reproducers/reproducer_20260201_120000_004.erl`
**Rule:** `INVALID_ERROR_CODE`
**Issue:** Server returns error code -99999 outside valid range

### 5. Message Too Large
**File:** `test/reproducers/reproducer_20260201_120000_005.erl`
**Rule:** `MESSAGE_TOO_LARGE`
**Issue:** Message exceeds 16MB size limit without rejection

## Testing

### EUnit Tests
**File:** `apps/erlmcp_core/test/erlmcp_reproducer_tests.erl`

Tests core reproducer functionality:
- Capture protocol/transport/SSE failures
- List all/unfixed reproducers
- Mark reproducers as fixed
- Generate reproducer modules
- Replay scenarios
- Generate audit reports

**Run:** `rebar3 eunit --module=erlmcp_reproducer_tests`

### Common Test Suite
**File:** `test/reproducers/reproducer_SUITE.erl`

Runs all reproducer modules and tracks fixed/unfixed status:
- Individual reproducer tests
- Reproducer discoverability tests
- Audit report tests

**Run:** `rebar3 ct --suite=test/reproducers/reproducer_SUITE`

### Validator Hooks Tests
**File:** `apps/erlmcp_validation/test/erlmcp_validator_hooks_tests.erl`

Tests validator integration:
- Emit failures when enabled
- No-op when disabled
- Graceful handling when reproducer system unavailable

**Run:** `rebar3 eunit --module=erlmcp_validator_hooks_tests`

## CI Integration

### Pre-Merge Checks
Reproducers are automatically run in CI:

```bash
#!/bin/bash
# .github/workflows/reproducers.yml

rebar3 ct --suite=test/reproducers/reproducer_SUITE

# Check unfixed count
UNFIXED=$(erl -noshell -eval "
  {ok, _} = application:ensure_all_started(erlmcp_core),
  {ok, _} = erlmcp_reproducer:start_link(),
  {ok, Report} = erlmcp_reproducer:audit_report(),
  Unfixed = maps:get(unfixed, Report),
  io:format(\"~p\", [Unfixed]),
  halt().")

if [ "$UNFIXED" -gt 10 ]; then
  echo "ERROR: Too many unfixed reproducers ($UNFIXED > 10)"
  echo "Please fix existing issues before adding new code"
  exit 1
fi
```

### Weekly Audit
Generate and publish reproducer audit report:

```bash
#!/bin/bash
# .github/workflows/weekly-audit.yml

rebar3 shell -eval "
  {ok, Report} = erlmcp_reproducer:audit_report(),
  file:write_file('reproducer_audit.json', jsx:encode(Report)),
  halt()."

# Email report to team
cat reproducer_audit.json | mail -s "Weekly Reproducer Audit" team@example.com
```

## Usage Workflow

### For Developers

1. **When you find a bug:**
   ```erlang
   %% Enable reproducer capture
   erlmcp_validator_hooks:enable_reproducer_capture(),

   %% Run failing code
   erlmcp_protocol_validator:validate_json_rpc(BadJson),

   %% Reproducer automatically created
   ```

2. **List unfixed reproducers:**
   ```erlang
   {ok, Unfixed} = erlmcp_reproducer:list_unfixed(),
   io:format("Unfixed: ~p~n", [length(Unfixed)]).
   ```

3. **Fix a bug and mark reproducer:**
   ```erlang
   %% After fixing the bug
   erlmcp_reproducer:mark_fixed(<<"20260201_120000_003">>).
   ```

4. **Run specific reproducer:**
   ```erlang
   reproducer_20260201_120000_001:run().
   % {error, still_failing} - Bug still present
   % {ok, fixed} - Bug is fixed
   ```

### For QA

1. **Run all reproducers:**
   ```bash
   rebar3 ct --suite=test/reproducers/reproducer_SUITE
   ```

2. **Generate audit report:**
   ```erlang
   {ok, Report} = erlmcp_reproducer:audit_report(),
   io:format("Fix Rate: ~.2f%~n", [maps:get(fix_rate_percent, Report)]).
   ```

3. **Review unfixed reproducers:**
   ```erlang
   {ok, Unfixed} = erlmcp_reproducer:list_unfixed(),
   [io:format("~s: ~s~n", [R#reproducer.id, R#reproducer.description])
    || R <- Unfixed].
   ```

## Integration with Validators

### Protocol Validator Integration

```erlang
%% In erlmcp_protocol_validator.erl
validate_json_rpc(Json) ->
    try jsx:decode(Json) of
        Data ->
            case validate_jsonrpc_structure(Data) of
                ok -> {ok, Data};
                {error, {wrong_version, Version}} = Error ->
                    %% Emit reproducer
                    erlmcp_validator_hooks:emit_protocol_failure(
                        <<"JSONRPC_WRONG_VERSION">>,
                        {error, {invalid_request, {wrong_version, <<"2.0">>}}},
                        Error
                    ),
                    Error
            end
    catch
        error:_ -> {error, {parse_error, invalid_json}}
    end.
```

### Transport Validator Integration

```erlang
%% In erlmcp_transport_validator.erl
validate_message_format(Transport, Data) ->
    case byte_size(Data) > 16777216 of
        true ->
            %% Emit reproducer for oversized message
            erlmcp_validator_hooks:emit_transport_failure(
                <<"MESSAGE_TOO_LARGE">>,
                {error, {message_too_large, 16777216}},
                {ok, accepted_oversized_message}
            ),
            {error, message_too_large};
        false ->
            validate_message_structure(Transport, Data)
    end.
```

## Metrics

The audit report provides key metrics:

```erlang
#{
    timestamp => 1738459200000,
    total_reproducers => 50,
    fixed => 35,
    unfixed => 15,
    fix_rate_percent => 70.0,
    reproducers_by_rule => #{
        <<"SSE_INVALID_RESUME_ID">> => 5,
        <<"JSONRPC_WRONG_VERSION">> => 2,
        <<"MESSAGE_TOO_LARGE">> => 8,
        %% ...
    }
}
```

**Key Metrics:**
- **Fix Rate:** Percentage of reproducers marked as fixed
- **Unfixed Count:** Number of outstanding issues
- **Top Failure Rules:** Most common failure categories

## Benefits

1. **Asynchronous Debugging**
   - Capture failure, debug later
   - Share reproducer with team members
   - No need to reproduce manually

2. **Regression Testing**
   - Each fixed bug becomes a regression test
   - Prevents reintroduction of bugs
   - Builds comprehensive edge case coverage

3. **Root Cause Analysis**
   - Rule IDs link to spec sections
   - System state snapshot aids debugging
   - Stack traces capture failure context

4. **Quality Metrics**
   - Fix rate tracks quality improvements
   - Unfixed count shows outstanding issues
   - Rule frequency identifies problem areas

5. **Compliance**
   - Reproducers document spec violations
   - Audit trail for certification
   - Evidence-based quality reporting

## Future Enhancements

1. **Automatic Triage**
   - Classify reproducers by severity
   - Prioritize based on frequency
   - Auto-assign to owners

2. **Similarity Detection**
   - Group similar failures
   - Detect duplicate issues
   - Suggest related fixes

3. **Visual Debugger**
   - Web UI for browsing reproducers
   - Timeline view of failures
   - Interactive replay

4. **Machine Learning**
   - Predict fix difficulty
   - Recommend similar solved issues
   - Auto-suggest fixes

## References

- Joe Armstrong: "Let It Crash" philosophy
- TPS (Toyota Production System): Andon, Poka-Yoke, Jidoka
- Chicago School TDD: State-based testing, real collaborators
- MCP 2025-11-25 Specification
- JSON-RPC 2.0 Specification
