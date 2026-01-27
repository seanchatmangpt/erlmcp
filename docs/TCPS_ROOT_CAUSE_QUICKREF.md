# TCPS Root Cause Analysis Quick Reference

## Quick Start

```erlang
%% 1. Start analysis
{ok, AnalysisId} = tcps_root_cause:start_analysis(AndonEventId, Problem).

%% 2. Add 5 whys
ok = tcps_root_cause:add_why(AnalysisId, 1, Answer1).
ok = tcps_root_cause:add_why(AnalysisId, 2, Answer2).
ok = tcps_root_cause:add_why(AnalysisId, 3, Answer3).
ok = tcps_root_cause:add_why(AnalysisId, 4, Answer4).
ok = tcps_root_cause:add_why(AnalysisId, 5, Answer5).

%% 3. Finalize with root cause and prevention
{ok, Result} = tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention).

%% 4. Extract prevention delta
Delta = maps:get(prevention_delta, Result).
#{shacl_additions := SHACL,
  test_additions := Tests,
  template_improvements := Templates,
  dependency_pins := Deps} = Delta.

%% 5. Generate receipt
Receipt = maps:get(receipt, Result).
```

## API Reference

| Function | Purpose |
|----------|---------|
| `start_analysis/2` | Begin new 5 Whys analysis |
| `add_why/3` | Add why answer (1-5) |
| `finalize_analysis/3` | Complete with root cause + prevention |
| `get_analysis/1` | Retrieve analysis by ID |
| `list_analyses/0` | Get all analyses |
| `get_analyses_by_andon/1` | Get analyses for Andon event |
| `generate_prevention_actions/1` | Get automated suggestions |

## Pattern Detection

### SHACL Triggers
- `"shacl"` → Add SHACL shape for validation
- `"validation"` → Add required property validation
- `"invalid data"` → Add datatype constraint
- `"type mismatch"` → Add class/datatype constraint
- `"cardinality"` → Add count constraints

### Test Triggers
- `"race condition"` → Add concurrency test
- `"edge case"` → Add boundary value tests
- `"error handling"` → Add negative test cases
- `"timeout"` → Add timeout test
- `"non-deterministic"` → Add property-based test

### Template Triggers
- `"boilerplate"` → Add template helper
- `"hard-coded"` → Extract to template variables
- `"duplicated"` → Create reusable component

### Dependency Triggers
- Regex: `"dependency ([a-z_]+) version ([0-9.]+)"`
- Auto-extracts name and version for pinning

## Common Scenarios

### Test Failure
```erlang
RootCause = <<"Missing concurrency specification in ontology">>,
Prevention = <<"Add tcps:concurrencyMode property">>.
%% Suggests: concurrency tests
```

### Compilation Error
```erlang
RootCause = <<"Missing SHACL validation for record structure">>,
Prevention = <<"Add SHACL shape enforcing tcps:recordField">>.
%% Suggests: SHACL shapes, template improvements
```

### Non-Determinism
```erlang
RootCause = <<"Dependency cowboy version 2.10.0 not pinned">>,
Prevention = <<"Pin all dependencies with exact versions">>.
%% Extracts: #{<<"cowboy">> => <<"2.10.0">>}
```

## Receipt Structure

```json
{
  "receipt_type": "root_cause_analysis",
  "analysis_id": "...",
  "andon_event_id": "...",
  "problem_statement": "...",
  "five_whys_chain": { ... },
  "root_cause": "...",
  "prevention_action": "...",
  "prevention_delta": {
    "shacl_additions": [...],
    "test_additions": [...],
    "template_improvements": [...],
    "dependency_pins": {...}
  },
  "timeline": { ... },
  "status": "finalized",
  "ontology_ref": "tcps:RootCauseAnalysis",
  "andon_event_ref": "tcps:AndonEvent/..."
}
```

## Testing

```bash
# Run tests
rebar3 eunit --module=tcps_root_cause_tests

# Run demo
escript examples/tcps_root_cause_demo.erl
```

## Files

| File | Purpose |
|------|---------|
| `src/tcps_root_cause.erl` | Main implementation |
| `include/tcps_root_cause.hrl` | Record definitions |
| `test/tcps_root_cause_tests.erl` | Test suite (20 tests) |
| `ontology/tcps_root_cause.ttl` | Ontology integration |
| `examples/tcps_root_cause_demo.erl` | Interactive demo |
| `docs/TCPS_ROOT_CAUSE_ANALYSIS.md` | Full documentation |
| `docs/examples/tcps_root_cause_examples.md` | Detailed examples |

## Best Practices

1. ✓ Be specific in whys - drill deeper, not sideways
2. ✓ Root cause must be actionable
3. ✓ Prevention actions are mandatory
4. ✓ Leverage automated detection
5. ✓ Track effectiveness over time

## Integration Points

```
Andon Stop → 5 Whys → Prevention Delta → Resume Flow
                ↓
          Receipt & Ontology
```

Prevention delta applies to:
- SHACL shapes (`shapes/`)
- Test cases (`test/`)
- Templates (`templates/`)
- Dependencies (`ontology/dependencies.ttl`)

---

**Full docs:** `/Users/sac/erlmcp/docs/TCPS_ROOT_CAUSE_ANALYSIS.md`
