# MCP Specification Compliance Validation Framework v2.0
**Design Document**
**Date:** 2026-02-02
**Target:** 100% MCP Spec Adherence
**Baseline:** 65% (v2.1.0) → 95%+ (v3.0.0)

---

## Executive Summary

This framework defines a **comprehensive, automated compliance validation system** for the erlmcp implementation to achieve 100% MCP specification adherence. The framework establishes:

- **Black-box spec conformance testing** (behavioral verification)
- **Feature matrix validation** (65%→95% progression)
- **Automated compliance reporting** (JSON/HTML/Markdown)
- **Spec version tracking** (change detection)
- **Breaking change detection** (semantic versioning)
- **Regression prevention** (baseline comparisons)
- **MCP compatibility test suite** (cross-version)

**Expected Outcome:** Zero ambiguity in spec compliance verification.

---

## 1. BLACK-BOX SPEC CONFORMANCE TESTING

### 1.1 Principle: Observable Behavior Only

Black-box testing validates **external behavior** against MCP specification without examining internal implementation details.

```erlang
% Module: erlmcp_spec_conformance_SUITE.erl
% Validates ONLY observable behaviors, not internals

-module(erlmcp_spec_conformance_SUITE).

-type spec_requirement() :: #{
    req_id => binary(),           % e.g., <<"MCP-CORE-001">>
    spec_section => binary(),     % e.g., <<"Protocol/JSON-RPC">>
    description => binary(),      % Requirement in human terms
    observable_behavior => [binary()],  % What the system should do
    non_observable => [binary()]  % What we DON'T test (implementation)
}.

%% Test Categories:
%% 1. JSON-RPC 2.0 Protocol Compliance
%% 2. MCP Message Semantics
%% 3. Error Handling & Codes
%% 4. Lifecycle (initialize→ready→close)
%% 5. Resource Operations (list, read, subscribe)
%% 6. Tool Operations (list, call, validation)
%% 7. Prompt Operations (list, get, templates)
%% 8. Sampling/LLM Integration
%% 9. Logging & Notifications
%% 10. Cancellation & Progress Tracking
```

### 1.2 Test Structure: Spec Section Mapping

Each spec section maps to isolated test suites:

```
MCP Spec 2025-11-25
├── Section 1: Protocol [erlmcp_protocol_conformance_SUITE]
│   ├── JSON-RPC 2.0 validation
│   ├── Version negotiation
│   ├── Capability exchange
│   └── Error codes (11 standard codes)
│
├── Section 2: Resources [erlmcp_resources_conformance_SUITE]
│   ├── List operation (static + templates)
│   ├── Read operation (all URI schemes)
│   ├── Subscribe operation (fan-out)
│   ├── Unsubscribe + cleanup
│   └── Resource metadata
│
├── Section 3: Tools [erlmcp_tools_conformance_SUITE]
│   ├── List operation
│   ├── Call operation (schema validation)
│   ├── Input validation errors
│   └── Deprecation handling
│
└── [Remaining Sections...]
```

### 1.3 Conformance Test Template

```erlang
%% File: apps/erlmcp_validation/test/erlmcp_protocol_conformance_SUITE.erl

%% Requirement Mapping
-define(REQ_JSONRPC_VERSION, #{
    req_id => <<"MCP-CORE-001">>,
    spec_section => <<"Protocol/Basic">>,
    description => <<"Server MUST respond with jsonrpc: '2.0' in all responses">>,
    spec_reference => <<"https://github.com/modelcontextprotocol/spec#json-rpc-20">>,
    observable_behavior => [
        <<"Response contains 'jsonrpc' field">>,
        <<"Value is string '2.0'">>,
        <<"Present in all message types">>
    ]
}).

%% Test Suite Structure
-export([
    %% Lifecycle
    all/0, init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,

    %% Conformance Tests
    spec_req_001_jsonrpc_version/1,
    spec_req_002_protocol_version_negotiation/1,
    spec_req_003_capability_exchange/1,
    spec_req_004_error_code_standard/1,
    spec_req_005_error_code_mcp_custom/1,
    spec_req_006_batch_requests/1,
    spec_req_007_notifications/1
]).

%% Test Implementation
spec_req_001_jsonrpc_version(_Config) ->
    %% BEHAVIOR: Send request, verify response has "jsonrpc": "2.0"
    {ok, Client} = erlmcp_test_utils:connect_client(),

    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"ping">>,
        <<"id">> => 1
    },

    {ok, Response} = erlmcp_test_utils:send_request(Client, Request, 1000),

    %% Verify observable behavior
    <<"2.0">> = maps:get(<<"jsonrpc">>, Response),
    <<"2.0">> = maps:get(<<"jsonrpc">>,
        erlmcp_test_utils:message_to_map(Response)),

    %% Verify ALL response types have jsonrpc field
    ct:log("Response structure: ~p", [Response]),
    ok.
```

### 1.4 Test Assertion Levels

```erlang
%% Level 1: MUST (RFC 2119) - Test FAILS if violated
assert_must(Condition, ErrorMsg) ->
    case Condition of
        true -> ok;
        false -> ct:fail({"MUST assertion failed", ErrorMsg})
    end.

%% Level 2: SHOULD (RFC 2119) - Test WARNS if violated
assert_should(Condition, WarningMsg) ->
    case Condition of
        true -> ok;
        false -> ct:log("⚠️ SHOULD assertion: ~s", [WarningMsg])
    end.

%% Level 3: MAY (RFC 2119) - Test IGNORES if violated
assert_may(Condition, InfoMsg) ->
    case Condition of
        true -> ok;
        false -> ct:log("ℹ️ MAY assertion: ~s", [InfoMsg])
    end.
```

---

## 2. FEATURE MATRIX VALIDATION (65% → 95%)

### 2.1 Compliance Score Calculation

```erlang
%% Module: erlmcp_compliance_matrix.erl

-type compliance_level() :: fully | partial | missing | experimental.

-type feature_spec() :: #{
    feature_id => binary(),           % e.g., <<"tools/call">>
    spec_section => binary(),         % e.g., <<"Tools">>
    compliance_level => compliance_level(),
    percentage => float(),            % 0.0 to 100.0
    dependencies => [binary()],       % Required features
    priority => critical | high | medium | low,
    phase => 1 | 2 | 3,              % v2.2 | v2.3 | v3.0
    tests => [binary()],              % Test identifiers
    regression_priority => boolean()   % Must track regressions?
}.

%% Compliance Matrix (65 total features)
-define(COMPLIANCE_MATRIX, [
    %% CORE PROTOCOL (7 features)
    #{feature_id => <<"jsonrpc_2_0">>,
      percentage => 100.0,
      tests => [<<"spec_req_001_jsonrpc_version">>,
                <<"spec_req_002_request_structure">>]},

    %% RESOURCES (10 features)
    #{feature_id => <<"resources/list">>,
      percentage => 100.0},
    #{feature_id => <<"resources/read">>,
      percentage => 100.0},
    #{feature_id => <<"resources/subscribe">>,
      percentage => 85.0,  % Needs optimization
      phase => 1},

    %% TOOLS (10 features)
    #{feature_id => <<"tools/list">>,
      percentage => 100.0},
    #{feature_id => <<"tools/call_with_schema">>,
      percentage => 80.0,   % Performance bottleneck
      phase => 1},

    %% [Additional 38 features...]
]).

%% Calculate section compliance
-spec calculate_section_compliance(binary()) -> float().
calculate_section_compliance(Section) ->
    Features = [F || F <- ?COMPLIANCE_MATRIX,
                     maps:get(spec_section, F) =:= Section],
    Total = length(Features),
    case Total of
        0 -> 0.0;
        _ ->
            Sum = lists:sum([maps:get(percentage, F) || F <- Features]),
            Sum / Total
    end.

%% Calculate overall compliance
-spec calculate_overall_compliance() -> float().
calculate_overall_compliance() ->
    AllPercentages = [maps:get(percentage, F) || F <- ?COMPLIANCE_MATRIX],
    Sum = lists:sum(AllPercentages),
    Sum / length(?COMPLIANCE_MATRIX).

%% Identify gaps
-spec identify_gaps() -> [feature_spec()].
identify_gaps() ->
    [F || F <- ?COMPLIANCE_MATRIX, maps:get(percentage, F) < 100.0].

%% Get phase targets
-spec get_phase_targets(1|2|3) -> [feature_spec()].
get_phase_targets(Phase) ->
    [F || F <- ?COMPLIANCE_MATRIX,
          maps:get(phase, F, 999) =:= Phase].
```

### 2.2 Feature Categories

**65 Total Features Across 11 Categories:**

| Category | Count | Current % | Target % | Phase |
|----------|-------|-----------|----------|-------|
| Core Protocol | 7 | 93% | 100% | 1 |
| Resources | 10 | 82% | 100% | 1 |
| Tools | 10 | 76% | 100% | 1 |
| Prompts | 7 | 73% | 100% | 1 |
| Sampling (LLM) | 12 | 18% | 100% | 2 |
| Logging | 4 | 100% | 100% | - |
| Completion | 5 | 42% | 100% | 2 |
| Roots | 3 | 40% | 100% | 2 |
| Cancellation | 3 | 100% | 100% | - |
| Progress | 3 | 100% | 100% | - |
| Tasks (Experimental) | 8 | 0% | 100% | 2 |

**Phase Timeline:**
- **Phase 1 (v2.2.0):** Core, Resources, Tools, Prompts → 75% overall
- **Phase 2 (v2.3.0):** Sampling, Completion, Roots, Tasks → 90% overall
- **Phase 3 (v3.0.0):** Experimental features, edge cases → 95%+

### 2.3 Validation Reporting

```erlang
%% Per-feature validation report
-type feature_validation_report() :: #{
    feature_id => binary(),
    tested => boolean(),
    passed => boolean(),
    compliance_percentage => float(),
    tests_passed => integer(),
    tests_failed => integer(),
    test_coverage => float(),
    last_verified => binary(),  % ISO 8601 timestamp
    status => pass | fail | warning | unknown,
    blockers => [binary()]      % Blocking other features
}.

%% Generate compliance scorecard
-spec generate_compliance_scorecard() -> map().
generate_compliance_scorecard() ->
    #{
        timestamp => erlmcp_utils:iso8601_now(),
        spec_version => <<"2025-11-25">>,
        erlmcp_version => <<"2.1.0">>,
        target_version => <<"3.0.0">>,
        overall_compliance => calculate_overall_compliance(),
        by_category => [
            {<<"Core Protocol">>, calculate_section_compliance(<<"Protocol">>)},
            {<<"Resources">>, calculate_section_compliance(<<"Resources">>)},
            {<<"Tools">>, calculate_section_compliance(<<"Tools">>)},
            %% [...]
        ],
        gaps => identify_gaps(),
        next_phase_items => get_phase_targets(2)
    }.
```

---

## 3. AUTOMATED COMPLIANCE REPORTING

### 3.1 Report Generation Pipeline

```erlang
%% Module: erlmcp_compliance_report_engine.erl

-export([
    run_compliance_check/0,
    generate_report/2,
    store_report/2,
    compare_reports/2
]).

%% Compliance Check Pipeline (runs end-to-end)
-spec run_compliance_check() -> {ok, compliance_report()} | {error, term()}.
run_compliance_check() ->
    %% Step 1: Collect spec requirements
    {ok, Specs} = erlmcp_spec_parser:parse_mcp_spec(<<"2025-11-25">>),

    %% Step 2: Run all conformance tests
    {ok, TestResults} = erlmcp_conformance_runner:run_all_tests(),

    %% Step 3: Run all feature tests
    {ok, FeatureResults} = erlmcp_feature_validator:validate_all_features(),

    %% Step 4: Collect evidence
    {ok, Evidence} = erlmcp_evidence_collector:collect_all(
        Specs, TestResults, FeatureResults),

    %% Step 5: Calculate metrics
    {ok, Metrics} = erlmcp_metrics_calculator:calculate_all(Evidence),

    %% Step 6: Identify gaps & regressions
    {ok, Gaps} = erlmcp_gap_analyzer:identify_gaps(Metrics),
    {ok, Regressions} = erlmcp_regression_detector:detect_regressions(Metrics),

    %% Step 7: Build comprehensive report
    Report = #{
        timestamp => erlmcp_utils:iso8601_now(),
        spec_version => <<"2025-11-25">>,
        erlmcp_version => erlmcp_app:get_version(),

        %% Metrics
        compliance_scores => Metrics,

        %% Evidence
        test_results => TestResults,
        feature_validation => FeatureResults,

        %% Analysis
        gaps => Gaps,
        regressions => Regressions,

        %% Traceability
        traceability_matrix => erlmcp_traceability:generate_matrix(
            Specs, TestResults),

        %% Recommendations
        recommendations => erlmcp_recommender:generate_recommendations(
            Gaps, Regressions)
    },

    {ok, Report}.

%% Generate report in multiple formats
-spec generate_report(compliance_report(), atom()) ->
    {ok, binary()} | {error, term()}.
generate_report(Report, Format) ->
    case Format of
        json ->
            {ok, erlmcp_report_json:format(Report)};
        html ->
            {ok, erlmcp_report_html:format(Report)};
        markdown ->
            {ok, erlmcp_report_markdown:format(Report)};
        text ->
            {ok, erlmcp_report_text:format(Report)};
        _ ->
            {error, {unknown_format, Format}}
    end.

%% Store report for historical tracking
-spec store_report(compliance_report(), file:filename()) ->
    {ok, file:filename()} | {error, term()}.
store_report(Report, BaseDir) ->
    %% Create versioned directory
    Timestamp = maps:get(timestamp, Report),
    Version = erlmcp_app:get_version(),
    ReportDir = filename:join([BaseDir, binary_to_list(Version), Timestamp]),

    ok = filelib:ensure_dir(filename:join([ReportDir, ".gitkeep"])),

    %% Store all formats
    {ok, _} = generate_report(Report, json),
    {ok, _} = generate_report(Report, html),
    {ok, _} = generate_report(Report, markdown),

    %% Store evidence bundle
    {ok, _} = erlmcp_compliance_report:store_evidence_bundle(
        filename:join([ReportDir, "evidence"]),
        maps:get(evidence, Report, [])),

    {ok, ReportDir}.
```

### 3.2 Report Formats

**JSON Format** (machine-readable):
```json
{
  "timestamp": "2026-02-02T14:30:00Z",
  "spec_version": "2025-11-25",
  "erlmcp_version": "2.1.0",
  "overall_compliance": 65.2,
  "by_category": {
    "Core Protocol": 93.0,
    "Resources": 82.0,
    "Tools": 76.0,
    "Sampling": 18.0
  },
  "gaps": [
    {
      "feature": "sampling/createMessage",
      "current": 40,
      "target": 100,
      "reason": "Streaming support not implemented",
      "phase": 2
    }
  ],
  "traceability_matrix": {...},
  "recommendations": [...]
}
```

**HTML Report** (executive dashboard):
```html
<div class="compliance-dashboard">
  <div class="gauge" data-value="65.2" data-target="95">
    Overall Compliance: 65.2% → 95%
  </div>
  <div class="category-breakdown">
    <div class="category-bar" data-pct="93">Core Protocol: 93%</div>
    <div class="category-bar" data-pct="82">Resources: 82%</div>
    <div class="category-bar warning" data-pct="18">Sampling: 18% ⚠️</div>
  </div>
  <div class="gaps-section">
    <h3>Critical Gaps</h3>
    <table>
      <tr><td>Sampling</td><td>18%</td><td>Phase 2</td></tr>
      <tr><td>Tasks</td><td>0%</td><td>Phase 2</td></tr>
    </table>
  </div>
</div>
```

**Markdown Report** (human-readable, version-controllable):
```markdown
# MCP Compliance Report
**Generated:** 2026-02-02
**Spec Version:** 2025-11-25
**erlmcp Version:** 2.1.0

## Overall Compliance: 65.2% → 95%

### By Category
| Category | Current | Target | Phase | Status |
|----------|---------|--------|-------|--------|
| Core Protocol | 93% | 100% | 1 | 🟢 On track |
| Resources | 82% | 100% | 1 | 🟡 Needs review |
| Sampling | 18% | 100% | 2 | 🔴 Critical gap |

### Gaps (7 total)
1. **Sampling/LLM Integration** (18%): Missing streaming support
2. **Tasks API** (0%): Experimental feature not implemented
...
```

---

## 4. SPEC VERSION TRACKING

### 4.1 Spec Change Detection

```erlang
%% Module: erlmcp_spec_tracker.erl

-type spec_change() :: #{
    change_id => binary(),
    spec_version => binary(),
    change_type => breaking | non_breaking | deprecated,
    affected_section => binary(),
    description => binary(),
    migration_path => binary(),
    detection_date => binary(),
    impact_level => critical | high | medium | low
}.

-export([
    track_spec_version/1,
    detect_spec_changes/2,
    classify_changes/1,
    generate_migration_guide/1
]).

%% Track which spec version erlmcp currently supports
-spec track_spec_version(binary()) -> {ok, map()} | {error, term()}.
track_spec_version(SpecVersion) ->
    %% Parse spec version (e.g., <<"2025-11-25">>)
    {Year, Month, Day} = parse_spec_version(SpecVersion),

    %% Fetch spec details
    {ok, SpecDetails} = erlmcp_spec_parser:fetch_spec(SpecVersion),

    %% Track implementation status
    Status = #{
        spec_version => SpecVersion,
        release_date => {Year, Month, Day},
        supported => is_supported(SpecVersion),
        compliance => calculate_overall_compliance(),
        tracked_at => erlmcp_utils:iso8601_now(),

        %% Backward compatibility info
        compatible_with => [<<"2025-11-01">>, <<"2025-10-15">>],
        breaking_changes => []
    },

    %% Store in version history
    ok = erlmcp_version_history:record(Status),

    {ok, Status}.

%% Detect differences between two spec versions
-spec detect_spec_changes(binary(), binary()) ->
    {ok, [spec_change()]} | {error, term()}.
detect_spec_changes(OldVersion, NewVersion) ->
    {ok, OldSpec} = erlmcp_spec_parser:fetch_spec(OldVersion),
    {ok, NewSpec} = erlmcp_spec_parser:fetch_spec(NewVersion),

    Changes = [],

    %% Compare each section
    Changes1 = compare_protocol(
        maps:get(protocol, OldSpec),
        maps:get(protocol, NewSpec),
        Changes),

    Changes2 = compare_resources(
        maps:get(resources, OldSpec),
        maps:get(resources, NewSpec),
        Changes1),

    Changes3 = compare_tools(
        maps:get(tools, OldSpec),
        maps:get(tools, NewSpec),
        Changes2),

    %% [Compare remaining sections...]

    %% Classify changes
    Classified = classify_changes(Changes3),

    {ok, Classified}.

%% Classify changes as breaking/non-breaking
-spec classify_changes([spec_change()]) -> [spec_change()].
classify_changes(Changes) ->
    lists:map(fun(Change) ->
        Classified = classify_single_change(Change),
        Classified
    end, Changes).

%% Example: detect breaking changes
classify_single_change(#{description := Desc} = Change) ->
    case Desc of
        <<"Removed field", _/binary>> ->
            Change#{change_type => breaking,
                   impact_level => critical};
        <<"Changed type", _/binary>> ->
            Change#{change_type => breaking,
                   impact_level => high};
        <<"Deprecated field", _/binary>> ->
            Change#{change_type => deprecated,
                   impact_level => medium};
        <<"Added optional field", _/binary>> ->
            Change#{change_type => non_breaking,
                   impact_level => low};
        _ ->
            Change#{change_type => non_breaking,
                   impact_level => low}
    end.

%% Generate migration guide for version upgrade
-spec generate_migration_guide(binary()) -> binary().
generate_migration_guide(ToVersion) ->
    {ok, FromVersion} = erlmcp_app:get_spec_version(),
    {ok, Changes} = detect_spec_changes(FromVersion, ToVersion),

    BreakingChanges = [C || C <- Changes,
                            maps:get(change_type, C) =:= breaking],

    Guide = iolist_to_binary([
        "# Migration Guide: ", FromVersion, " → ", ToVersion, "\n\n",
        case length(BreakingChanges) of
            0 -> "✅ No breaking changes\n\n";
            N -> io_lib:format("⚠️  %d breaking changes require action:\n\n", [N])
        end,
        lists:map(fun(Change) ->
            format_migration_step(Change)
        end, BreakingChanges)
    ]),

    Guide.
```

### 4.2 Version Compatibility Matrix

```erlang
%% Supported spec versions
-define(SUPPORTED_SPECS, [
    {<<"2025-11-25">>, #{
        name => <<"MCP 2025-11-25">>,
        released => {2025, 11, 25},
        compliance => 65,
        status => current,
        eol => {2026, 12, 1}
    }},
    {<<"2025-11-01">>, #{
        name => <<"MCP 2025-11-01">>,
        released => {2025, 11, 1},
        compliance => 62,
        status => deprecated,
        eol => {2026, 11, 1}
    }}
]).

%% Compatibility table
-spec is_compatible_with(binary(), binary()) -> boolean().
is_compatible_with(ClientVersion, ServerVersion) ->
    %% Extract major.minor (ignore patch)
    ClientBase = extract_version_base(ClientVersion),
    ServerBase = extract_version_base(ServerVersion),

    %% Semantic versioning: compatible if major matches
    ClientBase =:= ServerBase.
```

---

## 5. BREAKING CHANGE DETECTION

### 5.1 Breaking Change Analysis

```erlang
%% Module: erlmcp_breaking_change_detector.erl

-type breaking_change() :: #{
    change_id => binary(),
    category => term_removed | type_changed | behavior_changed |
                semantics_changed | error_code_removed |
                required_field_removed,
    before => map(),
    after => map(),
    impact_score => 1..10,
    affected_features => [binary()],
    mitigation => binary()
}.

-export([
    detect_breaking_changes/2,
    assess_impact/1,
    generate_compatibility_report/1
]).

%% Detect all breaking changes between versions
-spec detect_breaking_changes(map(), map()) -> [breaking_change()].
detect_breaking_changes(OldSpec, NewSpec) ->
    Changes = [],

    %% Check 1: Removed required fields
    Changes1 = detect_removed_required_fields(OldSpec, NewSpec, Changes),

    %% Check 2: Type changes in existing fields
    Changes2 = detect_type_changes(OldSpec, NewSpec, Changes1),

    %% Check 3: Removed enum values
    Changes3 = detect_removed_enum_values(OldSpec, NewSpec, Changes2),

    %% Check 4: Changed semantics
    Changes4 = detect_semantic_changes(OldSpec, NewSpec, Changes3),

    %% Check 5: Removed error codes
    Changes5 = detect_removed_error_codes(OldSpec, NewSpec, Changes4),

    %% Check 6: Changed method signatures
    Changes6 = detect_method_signature_changes(OldSpec, NewSpec, Changes5),

    %% Filter to breaking changes only
    BreakingChanges = [C || C <- Changes6,
                            is_breaking_change(C)],

    %% Assess impact score (1-10)
    Assessed = lists:map(fun(C) ->
        Impact = calculate_impact_score(C),
        C#{impact_score => Impact}
    end, BreakingChanges),

    Assessed.

%% Assess impact of a breaking change
-spec assess_impact(breaking_change()) -> atom().
assess_impact(#{impact_score := Score}) ->
    case Score of
        S when S >= 9 -> critical;
        S when S >= 7 -> severe;
        S when S >= 5 -> significant;
        S when S >= 3 -> moderate;
        _ -> minor
    end.

%% Calculate numeric impact (1-10)
-spec calculate_impact_score(breaking_change()) -> 1..10.
calculate_impact_score(#{category := Category,
                        affected_features := Features}) ->
    %% Base score by category
    BaseScore = case Category of
        term_removed -> 10;
        required_field_removed -> 9;
        type_changed -> 8;
        behavior_changed -> 7;
        semantics_changed -> 6;
        error_code_removed -> 5
    end,

    %% Adjust by feature criticality
    FeatureMultiplier = case Features of
        [<<"protocol/", _/binary>>|_] -> 1.5;  % Core protocol is critical
        [<<"resources/", _/binary>>|_] -> 1.2;
        [<<"tools/", _/binary>>|_] -> 1.2;
        _ -> 1.0
    end,

    Score = min(10, round(BaseScore * FeatureMultiplier)),
    Score.

%% Generate compatibility report
-spec generate_compatibility_report([breaking_change()]) -> map().
generate_compatibility_report(Changes) ->
    #{
        total_breaking_changes => length(Changes),
        by_impact => group_by_impact(Changes),
        affected_features => extract_affected_features(Changes),
        recommendations => generate_compatibility_recommendations(Changes)
    }.
```

### 5.2 Example Breaking Changes

```erlang
%% Example: Field removal
#{
    change_id => <<"BC-001">>,
    category => term_removed,
    before => #{field => <<"capabilities.resources.options">>,
               required => true},
    after => #{field => <<"capabilities.resources.options">>,
              exists => false},
    impact_score => 9,
    affected_features => [<<"resources/list">>, <<"resources/read">>],
    mitigation => <<"Update client to omit this field in new version">>
}

%% Example: Type change
#{
    change_id => <<"BC-002">>,
    category => type_changed,
    before => #{field => <<"maxTokens">>, type => <<"integer">>},
    after => #{field => <<"maxTokens">>, type => <<"string">>},
    impact_score => 8,
    affected_features => [<<"sampling/createMessage">>],
    mitigation => <<"Convert all integer maxTokens to strings">>
}
```

---

## 6. REGRESSION PREVENTION

### 6.1 Baseline Tracking

```erlang
%% Module: erlmcp_regression_detector.erl

-type baseline() :: #{
    version => binary(),
    timestamp => binary(),
    compliance_score => float(),
    test_results => map(),
    feature_scores => map(),
    performance_metrics => map()
}.

-type regression() :: #{
    regression_id => binary(),
    type => compliance_regression | performance_regression |
            feature_regression,
    feature => binary(),
    previous_score => float(),
    current_score => float(),
    threshold_violated => boolean(),
    severity => critical | high | medium | low
}.

-export([
    establish_baseline/0,
    detect_regressions/1,
    compare_with_baseline/1,
    enforce_thresholds/0
]).

%% Establish baseline (run after significant change)
-spec establish_baseline() -> {ok, baseline()} | {error, term()}.
establish_baseline() ->
    %% Run full compliance check
    {ok, Report} = erlmcp_compliance_report:run_compliance_check(),

    Baseline = #{
        version => erlmcp_app:get_version(),
        timestamp => erlmcp_utils:iso8601_now(),
        compliance_score => maps:get(overall_compliance, Report),
        test_results => maps:get(test_results, Report),
        feature_scores => extract_feature_scores(Report),
        performance_metrics => measure_performance()
    },

    %% Store baseline
    ok = erlmcp_baseline_store:save(Baseline),

    ct:log("Baseline established: ~.2f%",
           [maps:get(compliance_score, Baseline)]),

    {ok, Baseline}.

%% Detect regressions by comparing to baseline
-spec detect_regressions(map()) -> [regression()].
detect_regressions(CurrentReport) ->
    {ok, Baseline} = erlmcp_baseline_store:load(),

    Regressions = [],

    %% Check 1: Compliance score regression
    CurrentCompliance = maps:get(overall_compliance, CurrentReport),
    PreviousCompliance = maps:get(compliance_score, Baseline),

    Regressions1 = case CurrentCompliance < (PreviousCompliance - 0.5) of
        true ->
            [#{
                regression_id => erlmcp_utils:gen_uuid(),
                type => compliance_regression,
                feature => <<"overall">>,
                previous_score => PreviousCompliance,
                current_score => CurrentCompliance,
                threshold_violated => true,
                severity => critical
            } | Regressions];
        false ->
            Regressions
    end,

    %% Check 2: Per-feature regressions
    CurrentFeatures = extract_feature_scores(CurrentReport),
    PreviousFeatures = maps:get(feature_scores, Baseline),

    Regressions2 = detect_feature_regressions(
        CurrentFeatures, PreviousFeatures, Regressions1),

    %% Check 3: Test pass rate regressions
    Regressions3 = detect_test_regressions(
        maps:get(test_results, CurrentReport),
        maps:get(test_results, Baseline),
        Regressions2),

    %% Check 4: Performance regressions (>10% slower)
    CurrentPerf = measure_performance(),
    PreviousPerf = maps:get(performance_metrics, Baseline),

    Regressions4 = detect_performance_regressions(
        CurrentPerf, PreviousPerf, Regressions3),

    Regressions4.

%% Enforce minimum thresholds
-spec enforce_thresholds() -> {ok, passed} | {error, threshold_violated}.
enforce_thresholds() ->
    {ok, Report} = erlmcp_compliance_report:run_compliance_check(),

    Compliance = maps:get(overall_compliance, Report),

    %% Minimum thresholds
    Thresholds = #{
        minimum_compliance => 65.0,
        minimum_test_pass_rate => 95.0,
        maximum_performance_regression => 10.0
    },

    case Compliance >= maps:get(minimum_compliance, Thresholds) of
        true ->
            {ok, passed};
        false ->
            {error, {compliance_below_minimum, Compliance}}
    end.
```

### 6.2 Regression Prevention Gates

```erlang
%% Pre-commit gate: prevent regressions
-spec pre_commit_regression_check() -> {ok, passed} | {error, term()}.
pre_commit_regression_check() ->
    %% Run conformance tests
    {ok, TestResults} = erlmcp_test_runner:run_conformance_tests(),

    %% Check for regressions
    case any_tests_failed(TestResults) of
        true ->
            {error, {tests_failed, TestResults}};
        false ->
            %% Check for feature score regressions
            {ok, CurrentReport} = erlmcp_compliance_report:run_compliance_check(),
            Regressions = detect_regressions(CurrentReport),

            case length(Regressions) of
                0 ->
                    ct:log("✅ No regressions detected"),
                    {ok, passed};
                N ->
                    ct:log("❌ ~d regression(s) detected", [N]),
                    {error, {regressions_detected, Regressions}}
            end
    end.
```

---

## 7. MCP COMPATIBILITY TEST SUITE

### 7.1 Cross-Version Compatibility Testing

```erlang
%% Module: erlmcp_cross_version_tests_SUITE.erl

-export([
    all/0,

    %% Compatibility tests
    compatible_with_spec_2025_11_25/1,
    compatible_with_spec_2025_11_01/1,
    forward_compatible_with_future_specs/1,

    %% Interoperability tests
    client_server_same_version/1,
    client_newer_server_older/1,
    client_older_server_newer/1,

    %% Feature compatibility
    feature_coverage_2025_11_25/1,
    feature_coverage_2025_11_01/1
]).

%% Compatibility matrix test
-define(COMPATIBILITY_MATRIX, [
    % Version Combinations
    {<<"2025-11-25">>, <<"2025-11-25">>, true},   % Same version
    {<<"2025-11-25">>, <<"2025-11-01">>, true},   % Backward compat
    {<<"2025-11-01">>, <<"2025-11-25">>, maybe},  % Forward compat
]).

%% Test: Protocol compatibility with specific spec version
compatible_with_spec_2025_11_25(_Config) ->
    %% Spec requirements for 2025-11-25
    SpecReqs = [
        <<"jsonrpc_2_0_support">>,
        <<"protocol_version_negotiation">>,
        <<"error_codes_11_standard">>,
        <<"capabilities_exchange">>,
        <<"batch_requests">>,
        <<"notifications">>
    ],

    %% Verify each requirement
    lists:foreach(fun(Req) ->
        case erlmcp_spec_compliance:verify_requirement(Req) of
            {ok, passed} ->
                ct:log("✓ Requirement ~s passed", [Req]);
            {ok, warning, Msg} ->
                ct:log("⚠️  Requirement ~s warning: ~s", [Req, Msg]);
            {error, Msg} ->
                ct:fail({"Requirement not met", Req, Msg})
        end
    end, SpecReqs).

%% Test: Client/Server communication (same version)
client_server_same_version(_Config) ->
    {ok, Server} = erlmcp_test_utils:start_test_server(<<"2025-11-25">>),
    {ok, Client} = erlmcp_test_utils:start_test_client(<<"2025-11-25">>),

    %% Initialize
    {ok, ServerInfo} = erlmcp_test_utils:server_info(Server),
    {ok, ClientInfo} = erlmcp_test_utils:client_info(Client),

    %% Protocol versions should match
    ServerProto = maps:get(protocol_version, ServerInfo),
    ClientProto = maps:get(protocol_version, ClientInfo),

    ServerProto = ClientProto,

    %% Exchange capabilities
    {ok, ServerCaps} = erlmcp_test_utils:exchange_capabilities(Server, Client),

    ct:log("Capabilities exchanged: ~p", [ServerCaps]).

%% Test: Cross-version client/server (client newer)
client_newer_server_older(_Config) ->
    {ok, Server} = erlmcp_test_utils:start_test_server(<<"2025-11-01">>),
    {ok, Client} = erlmcp_test_utils:start_test_client(<<"2025-11-25">>),

    %% Should still communicate
    case erlmcp_test_utils:exchange_capabilities(Server, Client) of
        {ok, _Caps} ->
            ct:log("✓ Newer client compatible with older server");
        {error, Reason} ->
            ct:fail({"Backward compatibility failed", Reason})
    end.
```

### 7.2 Compatibility Test Matrix

```erlang
%% Define compatibility requirements
-define(SPEC_COMPATIBILITY, [
    #{
        client_spec => <<"2025-11-25">>,
        server_spec => <<"2025-11-25">>,
        expected => compatible,
        reason => <<"Same spec version">>
    },
    #{
        client_spec => <<"2025-11-25">>,
        server_spec => <<"2025-11-01">>,
        expected => compatible,
        reason => <<"Backward compatible">>
    },
    #{
        client_spec => <<"2025-11-01">>,
        server_spec => <<"2025-11-25">>,
        expected => maybe_compatible,
        reason => <<"Forward compatibility depends on changes">>
    }
]).

%% Feature compatibility: which features work across versions
-define(FEATURE_COMPATIBILITY, [
    #{feature => <<"jsonrpc_2_0">>,
      versions => all},
    #{feature => <<"protocol_version_negotiation">>,
      versions => [<<"2025-11-25">>, <<"2025-11-01">>]},
    #{feature => <<"tasks_api">>,
      versions => [<<"2025-11-25">>],
      note => <<"Experimental, not in 2025-11-01">>}
]).
```

---

## 8. IMPLEMENTATION ROADMAP

### 8.1 Phase 1: Compliance Framework (v2.2.0)
**Target: 75% compliance**

```
Week 1-2: Black-box testing infrastructure
├── erlmcp_spec_conformance_SUITE (Core Protocol)
├── erlmcp_protocol_conformance_SUITE
└── erlmcp_resources_conformance_SUITE

Week 3-4: Feature matrix validation
├── erlmcp_compliance_matrix (65 features)
├── erlmcp_feature_validator
└── Compliance scorecard generation

Week 5-6: Automated reporting
├── erlmcp_compliance_report_engine
├── JSON/HTML/Markdown formatters
└── Report storage & versioning

Week 7-8: Version tracking & release
├── erlmcp_spec_tracker
├── Compatibility matrix
└── Spec version support matrix
```

### 8.2 Phase 2: Advanced Analysis (v2.3.0)
**Target: 90% compliance**

```
Week 1-2: Breaking change detection
├── erlmcp_breaking_change_detector
├── Impact assessment
└── Migration guides

Week 3-4: Regression prevention
├── erlmcp_regression_detector
├── Baseline establishment
└── Threshold enforcement

Week 5-6: Cross-version testing
├── erlmcp_cross_version_tests_SUITE
├── Compatibility matrix tests
└── Feature compatibility tests

Week 7-8: Dashboard & tooling
├── Web dashboard (optional)
├── CLI tools
└── CI/CD integration
```

### 8.3 Phase 3: Automation & Hardening (v3.0.0)
**Target: 95%+ compliance**

```
Week 1-2: CI/CD integration
├── Automated compliance checks
├── Pre-commit hooks
└── Release gates

Week 3-4: Documentation generation
├── Auto-generate spec docs
├── Traceability matrices
└── Coverage reports

Week 5-6: Performance optimization
├── Test parallelization
├── Caching strategies
└── Report generation optimization

Week 7-8: Production hardening
├── Error handling
├── Edge cases
└── Production deployment
```

---

## 9. TEST CATEGORIES

### 9.1 Categorization Taxonomy

```erlang
%% Test categories (hierarchical)
-define(TEST_CATEGORIES, [
    %% Category 1: Conformance (Black-box)
    #{
        id => conformance,
        name => <<"Protocol Conformance">>,
        tests => [
            spec_req_001_jsonrpc_version,
            spec_req_002_protocol_version_negotiation,
            %% [60+ more]
        ],
        gate_requirement => true,
        failure_handling => fail
    },

    %% Category 2: Feature Validation
    #{
        id => feature_validation,
        name => <<"Feature Validation">>,
        tests => [
            feature_tools_call_with_schema,
            feature_resources_subscribe_fan_out,
            %% [20+ more]
        ],
        gate_requirement => true,
        failure_handling => fail_if_critical
    },

    %% Category 3: Regression Prevention
    #{
        id => regression,
        name => <<"Regression Detection">>,
        tests => [
            regression_compliance_score,
            regression_performance,
            %% [10+ more]
        ],
        gate_requirement => false,
        failure_handling => warn_if_significant
    },

    %% Category 4: Compatibility
    #{
        id => compatibility,
        name => <<"Cross-Version Compatibility">>,
        tests => [
            compat_spec_2025_11_25,
            compat_spec_2025_11_01,
            %% [5+ more]
        ],
        gate_requirement => false,
        failure_handling => report
    },

    %% Category 5: Performance
    #{
        id => performance,
        name => <<"Performance Benchmarks">>,
        tests => [
            perf_registry_throughput,
            perf_message_latency,
            %% [10+ more]
        ],
        gate_requirement => false,
        failure_handling => warn_if_regression
    },

    %% Category 6: Security
    #{
        id => security,
        name => <<"Security Validation">>,
        tests => [
            sec_auth_enforcement,
            sec_injection_prevention,
            %% [15+ more]
        ],
        gate_requirement => true,
        failure_handling => fail
    }
]).
```

### 9.2 Test Coverage By Category

| Category | Count | Gate | Priority |
|----------|-------|------|----------|
| Conformance | 65 | Yes | Critical |
| Feature Validation | 25 | Yes | Critical |
| Regression | 15 | No | High |
| Compatibility | 10 | No | Medium |
| Performance | 12 | No | Medium |
| Security | 20 | Yes | Critical |
| **Total** | **147** | - | - |

---

## 10. AUTOMATION STRATEGY

### 10.1 Execution Workflow

```
┌─────────────────────────────────────────────────────┐
│  Developer commits code                             │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  Pre-commit Hook: Run Fast Conformance Tests        │
│  - JSON-RPC validation (1 sec)                      │
│  - Protocol basics (2 sec)                          │
│  - No regressions (1 sec)                           │
│  Total: ~5 sec                                      │
└──────────────────┬──────────────────────────────────┘
                   │
              Pass? │ Fail?
              Yes │ No
                │ └─► Reject commit, show error
                ▼
┌─────────────────────────────────────────────────────┐
│  Commit Accepted → Push to Remote                   │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  CI Pipeline: Full Compliance Check                 │
│  - All 147 tests (60 sec parallel)                  │
│  - 11 feature categories (20 sec)                   │
│  - Regression detection (10 sec)                    │
│  Total: ~90 sec                                     │
└──────────────────┬──────────────────────────────────┘
                   │
              Pass? │
              Yes │ No
                │ └─► Build failed, notify team
                ▼
┌─────────────────────────────────────────────────────┐
│  Generate Compliance Report                         │
│  - JSON report                                      │
│  - HTML dashboard                                   │
│  - Markdown artifact                                │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  Store Report + Evidence Bundle                     │
│  - Version-tagged directory                         │
│  - Traceability matrix                              │
│  - Test results                                     │
│  - Performance metrics                              │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  Detect Regressions                                 │
│  - Compare against baseline                         │
│  - Identify breaking changes                        │
│  - Flag critical issues                             │
└──────────────────┬──────────────────────────────────┘
                   │
              Regressions? │
                Yes │ No
                   │ └─► Success! Build OK
                   ▼
           Notify team with
           regression report
```

### 10.2 CI/CD Integration

```yaml
# .github/workflows/compliance-check.yml
name: MCP Compliance Check

on: [push, pull_request]

jobs:
  conformance:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Erlang
        run: |
          # OTP 28.3.1
          ...

      - name: Run Conformance Tests
        run: |
          cd apps/erlmcp_validation
          rebar3 ct --suite=erlmcp_protocol_conformance_SUITE \
                     --suite=erlmcp_resources_conformance_SUITE \
                     --suite=erlmcp_tools_conformance_SUITE

      - name: Generate Compliance Report
        run: |
          erlang -eval "erlmcp_compliance_report:run_compliance_check()" \
                  -eval "erlmcp_report_generator:generate_all_formats()" \
                  -noshell

      - name: Upload Report
        uses: actions/upload-artifact@v3
        with:
          name: compliance-report
          path: .erlmcp/reports/

      - name: Check Thresholds
        run: |
          erlang -eval "erlmcp_regression_detector:enforce_thresholds()" \
                  -noshell
```

---

## 11. SUCCESS CRITERIA

### 11.1 Metrics

| Metric | Baseline | Target | Measurement |
|--------|----------|--------|-------------|
| Overall Compliance | 65% | 95%+ | Feature matrix score |
| Test Pass Rate | 92% | 99%+ | Conformance test results |
| Regression Rate | N/A | <0.5% per week | Regression detector |
| Breaking Changes Detected | 0 | 100% | Change detector coverage |
| Spec Version Support | 2 | 3+ | Version matrix |
| Test Execution Time | ~5min | <2min | CI/CD timing |
| Report Generation | Manual | <30sec | Automation timing |

### 11.2 Deliverables

- [ ] `erlmcp_spec_conformance_SUITE` - 65+ conformance tests
- [ ] `erlmcp_compliance_matrix` - 65 features tracked
- [ ] `erlmcp_compliance_report_engine` - Report generation pipeline
- [ ] `erlmcp_spec_tracker` - Version change tracking
- [ ] `erlmcp_breaking_change_detector` - Breaking change analysis
- [ ] `erlmcp_regression_detector` - Regression prevention
- [ ] `erlmcp_cross_version_tests_SUITE` - Compatibility tests
- [ ] Documentation & guides
- [ ] CI/CD integration
- [ ] Dashboard (optional)

---

## 12. APPENDIX: Spec Requirements Matrix

```erlang
%% Partial listing of 65 total features tracked

-define(MCP_SPEC_REQUIREMENTS, [
    %% CORE PROTOCOL (7)
    {<<"jsonrpc_2_0_support">>, <<"All responses use jsonrpc 2.0">>},
    {<<"protocol_version_negotiation">>, <<"Client specifies protocol version">>},
    {<<"capability_exchange">>, <<"Client/Server exchange capabilities">>},
    {<<"error_codes_standard">>, <<"11 standard JSON-RPC error codes">>},
    {<<"error_codes_mcp">>, <<"MCP custom error codes">>},
    {<<"batch_requests">>, <<"Support for batch requests">>},
    {<<"notifications">>, <<"Support for notifications">>},

    %% RESOURCES (10)
    {<<"resources_list">>, <<"resources/list method">>},
    {<<"resources_read">>, <<"resources/read method">>},
    {<<"resources_subscribe">>, <<"resources/subscribe method">>},
    {<<"resources_unsubscribe">>, <<"resources/unsubscribe method">>},
    {<<"resources_updated_notification">>, <<"notifications/resources/updated">>},
    {<<"resources_list_changed">>, <<"notifications/resources/list_changed">>},
    {<<"resource_uri_schemes">>, <<"Support all URI schemes">>},
    {<<"resource_metadata">>, <<"Resource metadata support">>},
    {<<"resource_icons">>, <<"Icon URL support (SEP-973)">>},

    %% [Remaining 38 features...]
]).
```

---

## 13. References

- MCP Specification: https://github.com/modelcontextprotocol/spec
- RFC 2119: https://tools.ietf.org/html/rfc2119
- JSON-RPC 2.0: https://www.jsonrpc.org/specification
- erlmcp Implementation: https://github.com/erlmcp/erlmcp
- CLAUDE.md Project Instructions: `/home/user/erlmcp/CLAUDE.md`

---

**Framework Status:** Ready for Phase 1 Implementation
**Last Updated:** 2026-02-02
**Next Review:** After Phase 1 completion (v2.2.0)
