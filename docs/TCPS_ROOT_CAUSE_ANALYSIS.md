# TCPS 5 Whys Root Cause Analysis Framework

## Overview

The TCPS Root Cause Analysis framework implements Toyota Production System's **5 Whys methodology** for systematic investigation of Andon events (test failures, compilation errors, non-determinism) in software manufacturing.

This is the **Jidoka** (built-in quality) pillar of TCPS - ensuring every failure becomes a permanent improvement to the production system.

## Key Features

### 1. Structured 5 Whys Analysis
- Sequential "why" questions drilling from symptom to root cause
- Evidence-based reasoning chain
- Validation ensuring all 5 whys are completed
- Status tracking (pending → in_progress → finalized)

### 2. Automated Prevention Action Generation
Pattern detection for automatic prevention suggestions:
- **SHACL Shape Additions** - Validation constraints to prevent recurrence
- **Test Case Additions** - New tests derived from failure scenarios
- **Template Improvements** - Template modifications for better code generation
- **Dependency Pinning** - Version constraints for deterministic builds

### 3. Receipt-Based Audit Trail
Every analysis generates a comprehensive receipt containing:
- Full 5 Whys chain
- Root cause and prevention actions
- Applied deltas (SHACL shapes, tests, templates, dependencies)
- Timeline and duration metrics
- Linkage to Andon events and TCPS ontology

### 4. Ontology Integration
- All analyses stored in TCPS ontology (`ontology/tcps_root_cause.ttl`)
- SHACL validation for data quality
- Linked to Andon events for traceability
- Effectiveness tracking over time

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    TCPS Root Cause Analysis                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌─────────────┐        ┌──────────────┐        ┌────────────┐  │
│  │ Andon Event │───────▶│ 5 Whys Chain │───────▶│ Root Cause │  │
│  └─────────────┘        └──────────────┘        └────────────┘  │
│         │                                              │          │
│         │                                              ▼          │
│         │                                   ┌──────────────────┐ │
│         │                                   │ Prevention Delta │ │
│         │                                   └──────────────────┘ │
│         │                                              │          │
│         │                                              ▼          │
│         │       ┌──────────────────────────────────────────────┐ │
│         └──────▶│            Receipt Generation                │ │
│                 └──────────────────────────────────────────────┘ │
│                                      │                            │
│                                      ▼                            │
│                 ┌──────────────────────────────────────────────┐ │
│                 │   TCPS Ontology (Audit Trail & Learning)     │ │
│                 └──────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

## Module: `tcps_root_cause.erl`

### API Functions

#### Start Analysis
```erlang
-spec start_analysis(AndonEventId :: binary(), Problem :: binary()) ->
    {ok, AnalysisId :: binary()} | {error, term()}.
```

Start a new root cause analysis for an Andon event.

**Example:**
```erlang
{ok, AnalysisId} = tcps_root_cause:start_analysis(
    <<"andon_test_failure_001">>,
    <<"Unit test test_concurrent_access fails intermittently">>
).
```

#### Add Why
```erlang
-spec add_why(AnalysisId :: binary(), WhyNumber :: 1..5, Answer :: binary()) ->
    ok | {error, term()}.
```

Add a sequential "why" answer (1 through 5).

**Example:**
```erlang
ok = tcps_root_cause:add_why(AnalysisId, 1,
    <<"Test shows race condition in data access">>).
ok = tcps_root_cause:add_why(AnalysisId, 2,
    <<"Two processes modify shared ETS table simultaneously">>).
%% ... continue through why 5
```

#### Finalize Analysis
```erlang
-spec finalize_analysis(AnalysisId :: binary(), RootCause :: binary(),
                        Prevention :: binary()) ->
    {ok, #{receipt => map(), prevention_delta => map(), analysis => map()}} |
    {error, term()}.
```

Finalize the analysis with root cause and prevention actions. Returns complete receipt and prevention delta.

**Example:**
```erlang
{ok, Result} = tcps_root_cause:finalize_analysis(
    AnalysisId,
    <<"Missing concurrency specification in ontology">>,
    <<"Add tcps:concurrencyMode property to ontology">>
).

Receipt = maps:get(receipt, Result),
Delta = maps:get(prevention_delta, Result).
```

#### Query Functions
```erlang
%% Get specific analysis
-spec get_analysis(AnalysisId :: binary()) ->
    {ok, #five_whys{}} | {error, not_found}.

%% List all analyses
-spec list_analyses() -> [#five_whys{}].

%% Get analyses for specific Andon event
-spec get_analyses_by_andon(AndonEventId :: binary()) -> [#five_whys{}].
```

### Pattern Detection

#### SHACL Shape Patterns
Detects root causes requiring SHACL validation:
- `"shacl"` → Add SHACL shape for validation
- `"validation"` → Add SHACL shape for required property validation
- `"invalid data"` → Add SHACL datatype constraint
- `"type mismatch"` → Add SHACL sh:class or sh:datatype constraint
- `"cardinality"` → Add SHACL sh:minCount and sh:maxCount constraints

#### Test Case Patterns
Detects root causes requiring test additions:
- `"race condition"` → Add concurrency test with multiple processes
- `"edge case"` → Add boundary value test cases
- `"error handling"` → Add negative test cases for error paths
- `"timeout"` → Add timeout handling test
- `"non-deterministic"` → Add property-based test with random inputs

#### Template Improvement Patterns
Detects root causes requiring template changes:
- `"boilerplate"` → Add template helper for repeated code patterns
- `"hard-coded"` → Extract hard-coded values to template variables
- `"duplicated"` → Create reusable template component

#### Dependency Pinning
Automatically extracts dependency names and versions from root causes:
- Regex: `"dependency ([a-z_]+) version ([0-9.]+)"`
- Example: `"dependency cowboy version 2.10.0"` → `#{<<"cowboy">> => <<"2.10.0">>}`

## Usage Examples

### Example 1: Test Failure Analysis

```erlang
%% 1. Andon event triggered
AndonId = <<"andon_test_failure_001">>,
Problem = <<"Unit test test_concurrent_access fails intermittently">>,

%% 2. Start analysis
{ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),

%% 3. Conduct 5 Whys
ok = tcps_root_cause:add_why(AnalysisId, 1,
    <<"Test shows race condition in data access">>),
ok = tcps_root_cause:add_why(AnalysisId, 2,
    <<"Two processes modify shared ETS table simultaneously">>),
ok = tcps_root_cause:add_why(AnalysisId, 3,
    <<"No synchronization mechanism for concurrent writes">>),
ok = tcps_root_cause:add_why(AnalysisId, 4,
    <<"Template assumes single-process access pattern">>),
ok = tcps_root_cause:add_why(AnalysisId, 5,
    <<"Ontology doesn't specify concurrency requirements">>),

%% 4. Finalize with root cause and prevention
{ok, Result} = tcps_root_cause:finalize_analysis(
    AnalysisId,
    <<"Missing concurrency specification in ontology causing race condition">>,
    <<"Add tcps:concurrencyMode property to ontology and generate synchronization">>
),

%% 5. Apply prevention delta
Delta = maps:get(prevention_delta, Result),
#{test_additions := TestCases} = Delta,
%% Returns: ["Add concurrency test with multiple processes"]
```

### Example 2: Compilation Error Analysis

```erlang
{ok, AnalysisId} = tcps_root_cause:start_analysis(
    <<"andon_compile_error_001">>,
    <<"Compilation error: undefined record info">>
),

ok = tcps_root_cause:add_why(AnalysisId, 1,
    <<"Record definition not included in generated module">>),
ok = tcps_root_cause:add_why(AnalysisId, 2,
    <<"Template filter didn't extract record from ontology">>),
ok = tcps_root_cause:add_why(AnalysisId, 3,
    <<"SPARQL query for records was incorrect">>),
ok = tcps_root_cause:add_why(AnalysisId, 4,
    <<"Record ontology structure changed without updating query">>),
ok = tcps_root_cause:add_why(AnalysisId, 5,
    <<"No SHACL validation ensuring record structure consistency">>),

{ok, Result} = tcps_root_cause:finalize_analysis(
    AnalysisId,
    <<"Missing SHACL validation for record structure consistency">>,
    <<"Add SHACL shape enforcing tcps:recordField constraints">>
),

%% Automated detection suggests:
%% - SHACL shape additions for validation
%% - Template improvements for record generation
```

### Example 3: Non-Determinism Analysis

```erlang
{ok, AnalysisId} = tcps_root_cause:start_analysis(
    <<"andon_nondeterminism_001">>,
    <<"Generated code differs between runs">>
),

ok = tcps_root_cause:add_why(AnalysisId, 1,
    <<"Dependency cowboy version changed between builds">>),
ok = tcps_root_cause:add_why(AnalysisId, 2,
    <<"rebar.lock not checked into repository">>),
ok = tcps_root_cause:add_why(AnalysisId, 3,
    <<"Template generation script didn't verify lock file">>),
ok = tcps_root_cause:add_why(AnalysisId, 4,
    <<"No policy requiring dependency pinning">>),
ok = tcps_root_cause:add_why(AnalysisId, 5,
    <<"Ontology doesn't model dependency version constraints">>),

{ok, Result} = tcps_root_cause:finalize_analysis(
    AnalysisId,
    <<"Dependency cowboy version 2.10.0 not pinned in ontology">>,
    <<"Pin all dependencies in ontology with exact versions">>
),

%% Automatically extracts:
%% #{dependency_pins => #{<<"cowboy">> => <<"2.10.0">>}}
```

## Receipt Structure

Generated receipts follow this structure:

```json
{
  "receipt_type": "root_cause_analysis",
  "receipt_version": "1.0.0",
  "analysis_id": "analysis_1706284800000_123456",
  "andon_event_id": "andon_test_failure_001",
  "timestamp": "2026-01-26T10:15:00Z",

  "problem_statement": "Unit test test_concurrent_access fails intermittently",

  "five_whys_chain": {
    "why_1": {
      "question": "Why did this problem occur?",
      "answer": "Test shows race condition in data access"
    },
    ...
  },

  "root_cause": "Missing concurrency specification in ontology causing race condition",
  "prevention_action": "Add tcps:concurrencyMode property to ontology",

  "prevention_delta": {
    "shacl_additions": [...],
    "test_additions": [...],
    "template_improvements": [...],
    "dependency_pins": {...}
  },

  "timeline": {
    "created_at": "2026-01-26T10:00:00Z",
    "updated_at": "2026-01-26T10:15:00Z",
    "finalized_at": "2026-01-26T10:15:00Z",
    "duration_ms": 900000
  },

  "status": "finalized",
  "ontology_ref": "tcps:RootCauseAnalysis",
  "andon_event_ref": "tcps:AndonEvent/andon_test_failure_001"
}
```

## Ontology Integration

### Classes
- `tcps:RootCauseAnalysis` - Main analysis entity
- `tcps:FiveWhysChain` - Sequential chain of whys
- `tcps:WhyQuestion` - Individual why question/answer
- `tcps:PreventionDelta` - Prevention actions
- `tcps:ShaclAddition` - SHACL constraint to add
- `tcps:TestAddition` - Test case to add
- `tcps:TemplateImprovement` - Template modification
- `tcps:DependencyPin` - Dependency version constraint

### Properties
- `tcps:analysisId` - Unique identifier
- `tcps:andonEventId` - Link to originating Andon event
- `tcps:problemStatement` - Problem description
- `tcps:hasFiveWhysChain` - Link to why chain
- `tcps:rootCause` - Identified root cause
- `tcps:preventionAction` - Prevention action statement
- `tcps:hasPreventionDelta` - Link to prevention delta

### SHACL Validation
```turtle
tcps:RootCauseAnalysisShape a sh:NodeShape ;
    sh:targetClass tcps:RootCauseAnalysis ;
    sh:property [
        sh:path tcps:analysisId ;
        sh:minCount 1 ;
        sh:maxCount 1
    ] ;
    sh:property [
        sh:path tcps:hasFiveWhysChain ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:class tcps:FiveWhysChain
    ] .

tcps:FiveWhysChainShape a sh:NodeShape ;
    sh:targetClass tcps:FiveWhysChain ;
    sh:property [
        sh:path tcps:hasWhyQuestion ;
        sh:minCount 5 ;
        sh:maxCount 5  # Must have exactly 5 whys
    ] .
```

## Testing

Comprehensive test suite at `/Users/sac/erlmcp/test/tcps_root_cause_tests.erl`:

```bash
rebar3 eunit --module=tcps_root_cause_tests
```

**Test Coverage:**
- Basic analysis workflow (start, add whys, finalize)
- Error handling (incomplete analyses, missing whys)
- Multiple concurrent analyses
- Query by Andon event
- Real-world scenarios (test failures, compilation errors, non-determinism)
- Prevention action pattern detection (SHACL, tests, templates, dependencies)
- Receipt generation and validation

**Results:**
```
20 tests, 0 failures
```

## Demo

Run the interactive demo:

```bash
escript examples/tcps_root_cause_demo.erl
```

The demo shows:
1. Test failure → concurrency test addition
2. Compilation error → SHACL shape + template improvement
3. Non-determinism → dependency pinning
4. Analysis listing and querying

## Integration with TCPS Production Line

The 5 Whys framework integrates at these points:

```
┌─────────────┐
│ Andon Stop  │──┐
└─────────────┘  │
                 ▼
        ┌────────────────┐
        │  5 Whys        │
        │  Analysis      │
        └────────────────┘
                 │
                 ▼
        ┌────────────────┐
        │ Prevention     │
        │ Delta          │
        └────────────────┘
                 │
                 ├─────▶ Add SHACL shapes
                 ├─────▶ Generate test cases
                 ├─────▶ Improve templates
                 └─────▶ Pin dependencies
                 │
                 ▼
        ┌────────────────┐
        │ Resume Flow    │──▶ Production continues with improvements
        └────────────────┘
                 │
                 ▼
        ┌────────────────┐
        │ Receipt        │──▶ Audit trail for compliance
        └────────────────┘
```

## Best Practices

### 1. Be Specific in Whys
Each why should drill deeper, not repeat the same level:
- ❌ Bad: "Code didn't work" → "Bug in implementation"
- ✅ Good: "Loop counter incremented twice" → "Template generated both pre- and post-increment"

### 2. Root Cause Must Be Actionable
- ❌ Bad: "Code quality issues"
- ✅ Good: "Missing SHACL validation for function parameter types"

### 3. Prevention Actions Are Mandatory
Every finalized analysis must include concrete prevention actions.

### 4. Leverage Automated Detection
Review and apply the automatically suggested prevention actions.

### 5. Track Effectiveness
Monitor whether prevention actions prevent recurrence by tracking Andon event trends.

## Kaizen (Continuous Improvement)

Every root cause analysis contributes to continuous improvement:

1. **Specification Improvement** - SHACL shapes strengthen validation
2. **Test Improvement** - New test cases prevent regression
3. **Template Improvement** - Better code generation
4. **Determinism** - Dependency pinning ensures reproducibility

This is the **Kaizen** pillar of TCPS - systematic, incremental improvement driven by evidence.

## Files

### Implementation
- `/Users/sac/erlmcp/src/tcps_root_cause.erl` - Main module
- `/Users/sac/erlmcp/include/tcps_root_cause.hrl` - Record definitions

### Tests
- `/Users/sac/erlmcp/test/tcps_root_cause_tests.erl` - Test suite (20 tests, 100% passing)

### Ontology
- `/Users/sac/erlmcp/ontology/tcps_root_cause.ttl` - TCPS ontology integration
- Examples: Test failure, compilation error, non-determinism analyses

### Documentation
- `/Users/sac/erlmcp/docs/TCPS_ROOT_CAUSE_ANALYSIS.md` - This file
- `/Users/sac/erlmcp/docs/examples/tcps_root_cause_examples.md` - Detailed examples

### Demo
- `/Users/sac/erlmcp/examples/tcps_root_cause_demo.erl` - Interactive demo script

## Summary

The TCPS 5 Whys Root Cause Analysis framework provides:

✓ **Systematic Investigation** - Structured methodology prevents superficial fixes
✓ **Automated Prevention** - Pattern detection suggests concrete improvements
✓ **Receipt-Based Audit** - Complete traceability for compliance and learning
✓ **Ontology Integration** - Root causes feed back into specification
✓ **Continuous Improvement** - Every failure strengthens the system

This is Toyota Production System's **Jidoka** (built-in quality) and **Kaizen** (continuous improvement) applied to software manufacturing.

---

**Related Documentation:**
- [TCPS Overview](/Users/sac/erlmcp/docs/TCPS.md)
- [TCPS Checklist](/Users/sac/erlmcp/docs/TCPS-checklist.md)
- [TCPS Certification](/Users/sac/erlmcp/docs/TCPS-certification.md)
