# TCPS 5 Whys Root Cause Analysis Examples

Comprehensive examples demonstrating the TCPS root cause analysis framework in action, following Toyota Production System methodology.

## Overview

The 5 Whys framework provides systematic root cause analysis for Andon events (test failures, compilation errors, non-determinism) with automated prevention action generation.

## Example 1: Test Failure → Root Cause → SHACL Shape Addition

### Scenario

Unit test `test_concurrent_access` fails intermittently with race condition errors.

### Analysis

```erlang
%% Start root cause analysis for test failure
{ok, AnalysisId} = tcps_root_cause:start_analysis(
    <<"andon_test_failure_001">>,
    <<"Unit test test_concurrent_access fails intermittently">>
).

%% Add sequential whys
ok = tcps_root_cause:add_why(AnalysisId, 1,
    <<"Test shows race condition in data access">>).

ok = tcps_root_cause:add_why(AnalysisId, 2,
    <<"Two processes modify shared ETS table simultaneously">>).

ok = tcps_root_cause:add_why(AnalysisId, 3,
    <<"No synchronization mechanism for concurrent writes">>).

ok = tcps_root_cause:add_why(AnalysisId, 4,
    <<"Template assumes single-process access pattern">>).

ok = tcps_root_cause:add_why(AnalysisId, 5,
    <<"Ontology doesn't specify concurrency requirements">>).

%% Finalize with root cause and prevention
{ok, Result} = tcps_root_cause:finalize_analysis(
    AnalysisId,
    <<"Missing concurrency specification in ontology causing race condition">>,
    <<"Add tcps:concurrencyMode property to ontology and generate appropriate synchronization">>
).
```

### Generated Receipt

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
    "why_2": {
      "question": "Why did that happen?",
      "answer": "Two processes modify shared ETS table simultaneously"
    },
    "why_3": {
      "question": "Why did that happen?",
      "answer": "No synchronization mechanism for concurrent writes"
    },
    "why_4": {
      "question": "Why did that happen?",
      "answer": "Template assumes single-process access pattern"
    },
    "why_5": {
      "question": "Why did that happen?",
      "answer": "Ontology doesn't specify concurrency requirements"
    }
  },

  "root_cause": "Missing concurrency specification in ontology causing race condition",
  "prevention_action": "Add tcps:concurrencyMode property to ontology and generate appropriate synchronization",

  "prevention_delta": {
    "shacl_additions": [
      "Add SHACL shape for required property validation"
    ],
    "test_additions": [
      "Add concurrency test with multiple processes"
    ],
    "template_improvements": [],
    "dependency_pins": {}
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

### SHACL Shape Addition

```turtle
# Add to shapes/concurrency.shacl.ttl
tcps:ConcurrencySpecificationShape a sh:NodeShape ;
    sh:targetClass tcps:DataStore ;
    sh:property [
        sh:path tcps:concurrencyMode ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:in ("single_process" "multi_process_synchronized" "lock_free")
    ] ;
    sh:property [
        sh:path tcps:synchronizationMechanism ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
        sh:message "Must specify synchronization mechanism for multi-process access"
    ] .
```

### Test Addition

```erlang
%% Add to test/concurrency_tests.erl
test_concurrent_ets_writes() ->
    TableName = test_table,
    ets:new(TableName, [named_table, public]),

    %% Spawn 100 concurrent writers
    Pids = [spawn(fun() ->
        lists:foreach(
            fun(I) ->
                ets:insert(TableName, {self(), I})
            end,
            lists:seq(1, 100)
        )
    end) || _ <- lists:seq(1, 100)],

    %% Wait for all writers
    lists:foreach(fun(Pid) ->
        MRef = monitor(process, Pid),
        receive
            {'DOWN', MRef, _, _, _} -> ok
        end
    end, Pids),

    %% Verify no data corruption
    Size = ets:info(TableName, size),
    ?assertEqual(10000, Size).
```

---

## Example 2: Compilation Error → Root Cause → Template Improvement

### Scenario

Build fails with `undefined record info` error.

### Analysis

```erlang
{ok, AnalysisId} = tcps_root_cause:start_analysis(
    <<"andon_compile_error_001">>,
    <<"Compilation error: undefined record info">>
).

ok = tcps_root_cause:add_why(AnalysisId, 1,
    <<"Record definition not included in generated module">>).

ok = tcps_root_cause:add_why(AnalysisId, 2,
    <<"Template filter didn't extract record from ontology">>).

ok = tcps_root_cause:add_why(AnalysisId, 3,
    <<"SPARQL query for records was incorrect">>).

ok = tcps_root_cause:add_why(AnalysisId, 4,
    <<"Record ontology structure changed without updating query">>).

ok = tcps_root_cause:add_why(AnalysisId, 5,
    <<"No SHACL validation ensuring record structure consistency">>).

{ok, Result} = tcps_root_cause:finalize_analysis(
    AnalysisId,
    <<"Missing SHACL validation for record structure consistency">>,
    <<"Add SHACL shape enforcing tcps:recordField constraints">>
).
```

### Prevention Delta

```json
{
  "prevention_delta": {
    "shacl_additions": [
      "Add SHACL datatype constraint"
    ],
    "test_additions": [],
    "template_improvements": [
      "Add template helper for repeated code patterns"
    ],
    "dependency_pins": {}
  }
}
```

### SHACL Shape Addition

```turtle
# Add to shapes/records.shacl.ttl
tcps:RecordDefinitionShape a sh:NodeShape ;
    sh:targetClass tcps:RecordDefinition ;
    sh:property [
        sh:path tcps:recordName ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:pattern "^[a-z][a-z0-9_]*$" ;
        sh:message "Record name must be valid Erlang atom"
    ] ;
    sh:property [
        sh:path tcps:hasRecordField ;
        sh:minCount 1 ;
        sh:class tcps:RecordField
    ] .

tcps:RecordFieldShape a sh:NodeShape ;
    sh:targetClass tcps:RecordField ;
    sh:property [
        sh:path tcps:fieldName ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string
    ] ;
    sh:property [
        sh:path tcps:fieldType ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:in ("atom" "binary" "integer" "list" "map" "tuple" "pid")
    ] .
```

### Template Improvement

```erlang
%% templates/erlang/record_helpers.tera
{%- macro generate_record_definition(record) -%}
-record({{ record.name }}, {
    {%- for field in record.fields %}
    {{ field.name }} :: {{ field.type }}{{ field.default | default("") }}
    {%- if not loop.last %},{% endif %}
    {%- endfor %}
}).
{%- endmacro -%}

{%- macro validate_record_consistency(record) -%}
%% SHACL validation for {{ record.name }}
{%- if record.fields | length < 1 %}
{{ error("Record must have at least one field") }}
{%- endif %}
{%- for field in record.fields %}
{%- if not field.type %}
{{ error("Field " ~ field.name ~ " missing type specification") }}
{%- endif %}
{%- endfor %}
{%- endmacro -%}
```

---

## Example 3: Non-Determinism → Root Cause → Dependency Pinning

### Scenario

Generated code differs between CI runs due to dependency version changes.

### Analysis

```erlang
{ok, AnalysisId} = tcps_root_cause:start_analysis(
    <<"andon_nondeterminism_001">>,
    <<"Generated code differs between runs">>
).

ok = tcps_root_cause:add_why(AnalysisId, 1,
    <<"Dependency cowboy version changed between builds">>).

ok = tcps_root_cause:add_why(AnalysisId, 2,
    <<"rebar.lock not checked into repository">>).

ok = tcps_root_cause:add_why(AnalysisId, 3,
    <<"Template generation script didn't verify lock file">>).

ok = tcps_root_cause:add_why(AnalysisId, 4,
    <<"No policy requiring dependency pinning">>).

ok = tcps_root_cause:add_why(AnalysisId, 5,
    <<"Ontology doesn't model dependency version constraints">>).

{ok, Result} = tcps_root_cause:finalize_analysis(
    AnalysisId,
    <<"Dependency cowboy version 2.10.0 not pinned in ontology">>,
    <<"Pin all dependencies in ontology with exact versions">>
).
```

### Prevention Delta

The framework automatically extracts dependency information from the root cause:

```json
{
  "prevention_delta": {
    "shacl_additions": [],
    "test_additions": [],
    "template_improvements": [],
    "dependency_pins": {
      "cowboy": "2.10.0"
    }
  }
}
```

### Ontology Update

```turtle
# Add to ontology/dependencies.ttl
tcps:CowboyDependency a tcps:Dependency ;
    tcps:dependencyName "cowboy" ;
    tcps:pinnedVersion "2.10.0" ;
    tcps:versionConstraint "== 2.10.0" ;
    tcps:lockfileRequired true ;
    tcps:deterministicBuild true .
```

### SHACL Validation

```turtle
# Add to shapes/dependencies.shacl.ttl
tcps:DependencyShape a sh:NodeShape ;
    sh:targetClass tcps:Dependency ;
    sh:property [
        sh:path tcps:dependencyName ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string
    ] ;
    sh:property [
        sh:path tcps:pinnedVersion ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:pattern "^[0-9]+\\.[0-9]+\\.[0-9]+$" ;
        sh:message "Version must be semantic version (X.Y.Z)"
    ] ;
    sh:property [
        sh:path tcps:lockfileRequired ;
        sh:hasValue true ;
        sh:message "All dependencies must require lockfile for determinism"
    ] .
```

---

## Example 4: Integration with Andon Events

### Full Workflow

```erlang
%% 1. Test fails, Andon event triggered
AndonEventId = tcps_andon:trigger_event(#{
    type => test_failure,
    test_module => <<"user_service_tests">>,
    test_case => <<"test_create_user">>,
    error => <<"Expected {ok, UserId} but got {error, validation_failed}">>,
    timestamp => erlang:system_time(millisecond)
}).

%% 2. Start root cause analysis
{ok, AnalysisId} = tcps_root_cause:start_analysis(
    AndonEventId,
    <<"User creation test fails with validation error">>
).

%% 3. Conduct 5 Whys analysis
ok = tcps_root_cause:add_why(AnalysisId, 1,
    <<"Email validation rejects valid email format">>).

ok = tcps_root_cause:add_why(AnalysisId, 2,
    <<"Email regex pattern is overly restrictive">>).

ok = tcps_root_cause:add_why(AnalysisId, 3,
    <<"Template used outdated RFC 5322 subset">>).

ok = tcps_root_cause:add_why(AnalysisId, 4,
    <<"No test coverage for modern email formats">>).

ok = tcps_root_cause:add_why(AnalysisId, 5,
    <<"Ontology email specification references obsolete RFC">>).

%% 4. Finalize and get prevention actions
{ok, Result} = tcps_root_cause:finalize_analysis(
    AnalysisId,
    <<"Ontology references obsolete RFC 5322 subset for email validation">>,
    <<"Update ontology to RFC 5322 complete spec and add edge case tests">>
).

%% 5. Extract prevention delta
PreventionDelta = maps:get(prevention_delta, Result),
#{
    shacl_additions := ShaclShapes,
    test_additions := TestCases,
    template_improvements := TemplateChanges,
    dependency_pins := DepPins
} = PreventionDelta.

%% 6. Apply prevention actions
lists:foreach(fun(Shape) ->
    tcps_shacl:add_shape(Shape)
end, ShaclShapes).

lists:foreach(fun(TestCase) ->
    tcps_testing:generate_test_case(TestCase)
end, TestCases).

%% 7. Generate receipt for audit trail
Receipt = maps:get(receipt, Result),
tcps_receipts:store_receipt(Receipt).

%% 8. Close Andon event
tcps_andon:close_event(AndonEventId, #{
    root_cause_analysis => AnalysisId,
    resolution => <<"Prevention actions applied">>,
    receipt => Receipt
}).
```

---

## Example 5: Querying Analysis History

### Get All Analyses for Specific Andon Event

```erlang
%% Get all root cause analyses for a recurring issue
AndonEventId = <<"andon_timeout_001">>,
Analyses = tcps_root_cause:get_analyses_by_andon(AndonEventId),

%% Analyze trends
lists:foreach(fun(Analysis) ->
    io:format("Analysis ~p:~n", [Analysis#five_whys.analysis_id]),
    io:format("  Root Cause: ~p~n", [Analysis#five_whys.root_cause]),
    io:format("  Status: ~p~n", [Analysis#five_whys.status]),
    io:format("  Duration: ~p ms~n~n", [
        Analysis#five_whys.finalized_at - Analysis#five_whys.created_at
    ])
end, Analyses).
```

### List All Active Analyses

```erlang
%% Get all analyses currently in progress
AllAnalyses = tcps_root_cause:list_analyses(),
ActiveAnalyses = lists:filter(
    fun(A) -> A#five_whys.status =:= in_progress end,
    AllAnalyses
).

io:format("Active analyses: ~p~n", [length(ActiveAnalyses)]).
```

---

## Example 6: Automated Prevention Action Generation

### Pattern Detection

```erlang
%% Test SHACL detection
RootCause1 = <<"Missing validation for required properties">>,
Delta1 = tcps_root_cause:generate_prevention_actions(RootCause1),
#prevention_delta{shacl_additions = Shapes1} = Delta1.
%% Returns: ["Add SHACL shape for required property validation"]

%% Test race condition detection
RootCause2 = <<"Race condition in concurrent access">>,
Delta2 = tcps_root_cause:generate_prevention_actions(RootCause2),
#prevention_delta{test_additions = Tests2} = Delta2.
%% Returns: ["Add concurrency test with multiple processes"]

%% Test dependency detection
RootCause3 = <<"Dependency ranch version 1.8.0 broke API compatibility">>,
Delta3 = tcps_root_cause:generate_prevention_actions(RootCause3),
#prevention_delta{dependency_pins = Deps3} = Delta3.
%% Returns: #{"ranch" => "1.8.0"}
```

---

## Example 7: Receipt-Based Audit Trail

### Receipt Linkage

Every root cause analysis generates a receipt that links:

1. **Original Andon Event** - The failure that triggered analysis
2. **Full 5 Whys Chain** - Complete reasoning path to root cause
3. **Root Cause** - Identified fundamental issue
4. **Prevention Actions** - Concrete steps to prevent recurrence
5. **Applied Deltas** - SHACL shapes, tests, templates, dependencies added
6. **Timeline** - Creation, updates, finalization timestamps
7. **Ontology References** - TCPS ontology integration

```erlang
%% Query receipts for compliance audit
Receipts = tcps_receipts:query(#{
    receipt_type => <<"root_cause_analysis">>,
    date_range => {
        <<"2026-01-01T00:00:00Z">>,
        <<"2026-01-31T23:59:59Z">>
    }
}).

%% Generate compliance report
Report = #{
    total_analyses => length(Receipts),
    avg_duration_ms => calculate_avg_duration(Receipts),
    prevention_actions_applied => count_prevention_actions(Receipts),
    shacl_shapes_added => count_shacl_additions(Receipts),
    tests_added => count_test_additions(Receipts)
}.
```

---

## Best Practices

### 1. Start Analysis Immediately

When an Andon event occurs, start the root cause analysis immediately to capture fresh context.

### 2. Be Specific in Whys

Each why should drill deeper into the underlying cause, not repeat the same level of abstraction.

**Bad:**
```
Why 1: Test failed
Why 2: Code didn't work
Why 3: Bug in implementation
```

**Good:**
```
Why 1: Assertion expected 5 but got 3
Why 2: Loop counter incremented twice per iteration
Why 3: Template generated both pre- and post-increment
Why 4: SPARQL query extracted duplicate increment operations
Why 5: Ontology allowed multiple increment specifications
```

### 3. Root Cause Should Be Actionable

The root cause should point to a specific fix, not a vague problem.

**Bad:** "Code quality issues"
**Good:** "Missing SHACL validation for function parameter types"

### 4. Prevention Actions Are Mandatory

Every finalized analysis must include concrete prevention actions that prevent recurrence.

### 5. Leverage Automated Detection

The framework automatically suggests prevention actions based on root cause patterns. Review and apply these suggestions.

### 6. Track Effectiveness

Monitor whether prevention actions actually prevent recurrence by tracking Andon event trends over time.

---

## Integration with TCPS Production Line

The 5 Whys framework integrates into the TCPS production line at these points:

1. **Andon Stop** → Root cause analysis triggered
2. **5 Whys Analysis** → Systematic investigation
3. **Prevention Delta** → SHACL shapes, tests, templates updated
4. **Resume Flow** → Production line continues with improvements
5. **Receipts** → Audit trail for continuous improvement

This ensures every failure becomes a permanent improvement to the production system.

---

## Summary

The TCPS 5 Whys root cause analysis framework provides:

- **Systematic Investigation** - Structured methodology prevents superficial fixes
- **Automated Prevention** - Pattern detection suggests concrete improvements
- **Receipt-Based Audit** - Complete traceability for compliance and learning
- **Ontology Integration** - Root causes feed back into specification
- **Continuous Improvement** - Every failure strengthens the system

This is Toyota Production System's Jidoka pillar (built-in quality) applied to software manufacturing.
