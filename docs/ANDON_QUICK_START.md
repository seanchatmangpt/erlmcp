# TCPS Andon System - Quick Start Guide

## 30-Second Overview

The Andon system stops your CI/CD pipeline when quality issues are detected and requires root cause analysis before proceeding. Think of it as an emergency stop button for your build system.

## Installation

```erlang
% Add to your rebar.config deps
{deps, [
    {jsx, "3.1.0"}  % Required for JSON receipt generation
]}.
```

## Basic Usage

### 1. Trigger an Andon (Stop the Line)

```erlang
% When you detect a problem:
{ok, AndonId} = tcps_andon:trigger_andon(test_failure, #{
    sku_id => <<"BUILD-123">>,
    stage => testing,
    details => #{
        test_module => my_tests,
        test_case => test_feature_x,
        failure_reason => <<"Expected 42, got 43">>
    }
}).
% → Andon triggered, build is now BLOCKED
```

### 2. Check if Blocked

```erlang
% Before deploying:
case tcps_andon:is_blocked(<<"BUILD-123">>) of
    true -> {error, blocked_by_andon};
    false -> deploy()
end.
```

### 3. Resolve (Restart the Line)

```erlang
% After fixing the issue:
ok = tcps_andon:resolve_andon(AndonId, #{
    root_cause => <<"Race condition in cache">>,
    fix_applied => <<"Added gen_server serialization">>,
    prevention_added => <<"Added property-based tests">>
}).
% → Build is now UNBLOCKED
```

## Integration Points

### Compilation Failure Hook

```erlang
% In your build script:
case compile:file("src/mymodule.erl") of
    {error, Errors, _} ->
        lists:foreach(fun(Error) ->
            tcps_andon:hook_compilation_failure(#{
                error_type => syntax_error,
                file => <<"src/mymodule.erl">>,
                line => 42,
                message => format_error(Error),
                sku_id => get_build_id()
            })
        end, Errors);
    _ -> ok
end.
```

### Test Failure Hook

```erlang
% In your test suite:
my_test() ->
    try
        ?assertEqual(Expected, Actual)
    catch
        error:{assertEqual, _} ->
            tcps_andon:hook_test_failure(#{
                test_module => ?MODULE,
                test_function => my_test,
                failure_type => assertion_failed,
                expected => Expected,
                actual => Actual,
                sku_id => <<"current_build">>
            }),
            error(test_failed)
    end.
```

### SHACL Validation Hook

```erlang
% After SHACL validation:
case shacl:validate(Graph, Shapes) of
    {ok, #{conforms := false} = Report} ->
        tcps_andon:hook_shacl_failure(#{
            validation_report => Report,
            sku_id => get_build_id()
        });
    _ -> ok
end.
```

## CLI Integration Example

```bash
#!/bin/bash
# build.sh

BUILD_ID="BUILD-$(date +%s)"

# Compile
if ! rebar3 compile; then
    echo "Compilation failed - Andon triggered"
    # Andon already triggered by hook
    exit 1
fi

# Check if blocked before testing
if erl -noshell -eval "
    tcps_andon:start(),
    case tcps_andon:is_blocked(<<\"$BUILD_ID\">>) of
        true -> halt(1);
        false -> halt(0)
    end.
"; then
    echo "✓ Clear to proceed"
else
    echo "✗ Blocked by Andon - resolve issues first"
    exit 1
fi

# Run tests
rebar3 eunit

# Deploy (only if not blocked)
if erl -noshell -eval "
    tcps_andon:start(),
    case tcps_andon:can_proceed_to_stage(<<\"$BUILD_ID\">>, deployment) of
        {ok, proceed} -> halt(0);
        {blocked, _} -> halt(1)
    end.
"; then
    echo "✓ Deploying..."
    ./deploy.sh
else
    echo "✗ Deployment blocked by Andon"
    exit 1
fi
```

## Failure Types

| Type | When to Use | Stage |
|------|------------|-------|
| `shacl_violation` | RDF/OWL constraint violations | compilation |
| `test_failure` | Unit/integration test failures | testing |
| `non_determinism` | Flaky tests, race conditions | execution |
| `missing_receipt` | Verification artifacts missing | validation |
| `compilation_failure` | Syntax, type errors | compilation |

## Required Resolution Fields

When resolving an Andon, you MUST provide:

1. **root_cause**: Why did it happen?
2. **fix_applied**: What did you change?
3. **prevention_added**: How will you prevent recurrence?

## Receipts

Every Andon event generates JSON receipts stored in `priv/receipts/`:

```json
{
  "receipt_id": "RCPT-1769477330250123-456789-1234",
  "andon_event_id": "ANDON-1769477330238967-297049-2372",
  "timestamp": 1769477330250,
  "timestamp_iso": "2024-01-26T12:34:56.250Z",
  "failure_type": "test_failure",
  "sku_id": "BUILD-123",
  "stage": "testing",
  "status": "open",
  "receipt_type": "andon_event",
  "ontology_refs": [
    "http://example.org/tcps/ontology#AndonEvent",
    "http://example.org/tcps/ontology#test_failure"
  ]
}
```

## Common Patterns

### Pattern 1: CI/CD Pipeline

```erlang
run_pipeline(SkuId) ->
    tcps_andon:start(),

    % Each stage checks if blocked
    case tcps_andon:can_proceed_to_stage(SkuId, compilation) of
        {ok, proceed} -> compile_stage(SkuId);
        {blocked, _} -> {error, blocked}
    end,

    case tcps_andon:can_proceed_to_stage(SkuId, testing) of
        {ok, proceed} -> test_stage(SkuId);
        {blocked, _} -> {error, blocked}
    end,

    % ... etc
    ok.
```

### Pattern 2: Resolution Automation

```erlang
% Auto-resolve for specific failure types
auto_resolve_if_safe(AndonId) ->
    Event = tcps_andon:get_andon_event(AndonId),

    case maps:get(failure_type, Event) of
        non_determinism ->
            % Auto-rerun and resolve if passes
            case rerun_test(Event) of
                ok ->
                    tcps_andon:resolve_andon(AndonId, #{
                        root_cause => <<"Transient timing issue">>,
                        fix_applied => <<"Test passed on retry">>,
                        prevention_added => <<"Added to flaky test watchlist">>
                    });
                error ->
                    require_manual_resolution
            end;
        _ ->
            require_manual_resolution
    end.
```

### Pattern 3: Metrics Dashboard

```erlang
get_andon_metrics(TimeWindow) ->
    AllEvents = ets:tab2list(tcps_andon_events),

    #{
        total_andons => length(AllEvents),
        open_andons => length([E || {_, #{status := open}} <- AllEvents]),
        by_type => count_by_type(AllEvents),
        avg_resolution_time => calculate_mttr(AllEvents),
        top_failing_skus => get_frequent_failures(AllEvents)
    }.
```

## Testing

```bash
# Run the test suite
erl -noshell \
    -pa _build/test/lib/erlmcp/ebin \
    -pa _build/test/lib/erlmcp/test \
    -pa _build/test/lib/jsx/ebin \
    -eval 'eunit:test(tcps_andon_tests, [verbose])' \
    -s init stop

# Expected: All 35 tests passed.
```

## Troubleshooting

### "Andon won't resolve"

Check that your resolution includes all three required fields:
- `root_cause`
- `fix_applied`
- `prevention_added`

### "SKU still blocked after resolution"

Check if there are multiple Andons for the same SKU:

```erlang
History = tcps_andon:get_andon_history(SkuId),
OpenAndons = [E || E = #{status := open} <- History],
io:format("Open Andons: ~p~n", [OpenAndons]).
```

### "Receipt not found"

Receipts are stored in `priv/receipts/` by default. Check:
1. Directory exists and is writable
2. JSX library is available for JSON encoding
3. Disk space available

## Next Steps

- Read full documentation: `/Users/sac/erlmcp/docs/TCPS_ANDON_SYSTEM.md`
- See examples: `/Users/sac/erlmcp/examples/andon_example.erl`
- Integration patterns: See "Integration Examples" in main docs

## Support

- Module: `/Users/sac/erlmcp/src/tcps/tcps_andon.erl`
- Tests: `/Users/sac/erlmcp/test/tcps/tcps_andon_tests.erl`
- Issues: GitHub issues for erlmcp project
