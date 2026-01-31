# Compliance Reporting - Evidence Collection and Traceability

## Purpose and Scope

The `erlmcp_compliance_report` module provides comprehensive compliance reporting for MCP implementations. It handles:

- **Evidence Collection** - Gathering test results, coverage metrics, security scans
- **Traceability Matrix** - Mapping requirements to test cases
- **Compliance Calculation** - Computing compliance percentages
- **Gap Analysis** - Identifying non-compliant areas
- **Report Generation** - Creating human-readable reports in multiple formats

## Architecture

```
erlmcp_compliance_report (gen_server)
├── Evidence Collection      - Test results, coverage, security
├── Traceability Matrix      - Requirement-to-test mapping
├── Compliance Calculation   - Percentage computation
├── Gap Analysis             - Non-compliance identification
├── Hash Verification        - SHA-256 evidence integrity
└── Report Formats           - Text, Markdown, JSON, HTML
```

## API Reference

### Server Management

#### start_link/0

Start the compliance report gen_server.

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

**Example:**
```erlang
{ok, Pid} = erlmcp_compliance_report:start_link().
```

### Report Generation

#### generate_report/2

Generate a compliance report in the specified format.

```erlang
-spec generate_report(report_format(), compliance_data()) -> {ok, binary()}.
```

**Parameters:**
- `Format` - Report format: `text`, `markdown`, `json`, `html`
- `Data` - Compliance data map with test results

**Returns:**
- `{ok, ReportContent}` - Generated report as binary

**Example:**
```erlang
ComplianceData = #{
    spec_version => <<"2025-11-25">>,
    timestamp => erlang:system_time(second),
    test_results => [
        #{name => "JSON-RPC 2.0", status => <<"passed">>, evidence => "..."},
        #{name => "Error Codes", status => <<"passed">>, evidence => "..."}
    ]
},

{ok, Report} = erlmcp_compliance_report:generate_report(markdown, ComplianceData).
```

### Compliance Calculation

#### calculate_compliance/1

Calculate overall compliance percentage from test results.

```erlang
-spec calculate_compliance(compliance_data()) -> {ok, float(), map()}.
```

**Parameters:**
- `Data` - Compliance data with test results

**Returns:**
- `{ok, Compliance, Details}` - Compliance percentage and breakdown

**Example:**
```erlang
Data = #{
    test_results => [
        #{status => <<"passed">>},
        #{status => <<"passed">>},
        #{status => <<"failed">>}
    ]
},

{ok, Compliance, Details} = erlmcp_compliance_report:calculate_compliance(Data),
% Returns: {ok, 66.67, #{passed => 2, failed => 1, total => 3}}
```

#### calculate_section_compliance/2

Calculate compliance for a specific section.

```erlang
-spec calculate_section_compliance(binary(), compliance_data()) -> {ok, float(), map()}.
```

**Sections:**
- `protocol` - JSON-RPC and MCP protocol
- `transport` - Transport behavior
- `security` - Security features
- `performance` - Performance targets

### Traceability

#### create_traceability_matrix/1

Create a traceability matrix mapping requirements to tests.

```erlang
-spec create_traceability_matrix(compliance_data()) -> map().
```

**Returns:**
- Map with requirement IDs as keys and test mappings as values

**Example:**
```erlang
Matrix = erlmcp_compliance_report:create_traceability_matrix(Data),
% Returns: #{
%   <<"REQ-PROTO-001">> => #{
%       requirement => <<"JSON-RPC 2.0 version field">>,
%       tests => [<<"jsonrpc_version_test">>],
%       status => <<"passed">>
%   },
%   ...
% }
```

#### get_requirements_for_test/2

Get all requirements covered by a specific test.

```erlang
-spec get_requirements_for_test(binary(), compliance_data()) -> [binary()].
```

**Example:**
```erlang
Reqs = erlmcp_compliance_report:get_requirements_for_test(
    <<"jsonrpc_version_test">>,
    Data
),
% Returns: [<<"REQ-PROTO-001">>, <<"REQ-PROTO-002">>]
```

### Gap Analysis

#### identify_gaps/1

Identify compliance gaps from test results.

```erlang
-spec identify_gaps(compliance_data()) -> [gap_analysis()].
```

**Gap Analysis Structure:**
```erlang
#{
    requirement => binary(),
    status => <<"passed">> | <<"failed">> | <<"missing">>,
    severity => error | warning | info,
    description => binary(),
    remediation => binary()
}
```

**Example:**
```erlang
Gaps = erlmcp_compliance_report:identify_gaps(Data),
% Returns: [
%     #{requirement => <<"SSE retry field">>,
%       status => <<"missing">>,
%       severity => warning,
%       description => <<"SSE should include retry field">>,
%       remediation => <<"Add retry field to SSE events">>}
% ]
```

#### prioritize_gaps/1

Prioritize gaps by severity and impact.

```erlang
-spec prioritize_gaps([gap_analysis()]) -> {[critical], [high], [medium], [low]}.
```

**Example:**
```erlang
{Critical, High, Medium, Low} = erlmcp_compliance_report:prioritize_gaps(Gaps).
```

### Evidence Management

#### collect_evidence/2

Collect evidence for a specific type.

```erlang
-spec collect_evidence(evidence_type(), map()) -> {ok, evidence()}.
```

**Evidence Types:**
- `test_result` - EUnit/CT test results
- `coverage_metrics` - Code coverage data
- `security_scan` - Security scan results
- `performance_benchmark` - Performance benchmark data

**Example:**
```erlang
{ok, Evidence} = erlmcp_compliance_report:collect_evidence(
    test_result,
    #{
        module => erlmcp_json_rpc,
        test => jsonrpc_version_test,
        status => passed,
        output => <<"Test passed">>,
        timestamp => erlang:system_time(second)
    }
).
```

#### store_evidence_bundle/2

Store multiple evidence items in a bundle.

```erlang
-spec store_evidence_bundle(file:filename(), [evidence()]) -> {ok, file:filename()}.
```

**Example:**
```erlang
BundlePath = "/tmp/evidence_bundle_20250130.tar.gz",
EvidenceItems = [
    #{type => test_result, content => {...}},
    #{type => coverage_metrics, content => {...}},
    #{type => security_scan, content => {...}}
],

{ok, BundlePath} = erlmcp_compliance_report:store_evidence_bundle(
    BundlePath,
    EvidenceItems
).
```

#### hash_evidence/1

Generate SHA-256 hash of evidence for integrity verification.

```erlang
-spec hash_evidence(map() | binary()) -> {ok, binary()}.
```

**Example:**
```erlang
{ok, Hash} = erlmcp_compliance_report:hash_evidence(TestResult),
% Returns: {ok, <<"a1b2c3d4e5f6...">>}
```

#### verify_evidence_integrity/2

Verify evidence against expected hash.

```erlang
-spec verify_evidence_integrity(map() | binary(), binary()) -> {ok, boolean()}.
```

**Example:**
```erlang
{ok, true} = erlmcp_compliance_report:verify_evidence_integrity(
    TestResult,
    ExpectedHash
).
```

### Report Formats

#### generate_text_report/1

Generate plain text compliance report.

```erlang
-spec generate_text_report(compliance_data()) -> {ok, binary()}.
```

**Output Format:**
```
===============================================
MCP Compliance Report
===============================================
Spec Version: 2025-11-25
Generated: 2025-01-30T12:00:00Z

Summary:
--------
Total Requirements: 45
Passed: 42
Failed: 2
Missing: 1
Compliance: 93.33%

Results by Section:
-------------------
Protocol: 100.00% (15/15 passed)
Transport: 95.00% (19/20 passed)
Security: 85.71% (6/7 passed)
Performance: 100.00% (2/2 passed)

Gaps:
------
1. SSE retry field (warning)
   Requirement: SSE should include retry field
   Status: missing
   Remediation: Add retry field to SSE events
```

#### generate_markdown_report/1

Generate Markdown compliance report.

```erlang
-spec generate_markdown_report(compliance_data()) -> {ok, binary()}.
```

**Output Format:**
```markdown
# MCP Compliance Report

**Spec Version:** 2025-11-25
**Generated:** 2025-01-30T12:00:00Z

## Summary

| Metric | Value |
|--------|-------|
| Total Requirements | 45 |
| Passed | 42 |
| Failed | 2 |
| Missing | 1 |
| **Compliance** | **93.33%** |

## Results by Section

### Protocol
- ✅ 100.00% (15/15 passed)

### Transport
- ⚠️ 95.00% (19/20 passed)
- ❌ Missing: SSE retry field

...
```

#### generate_json_report/1

Generate JSON compliance report.

```erlang
-spec generate_json_report(compliance_data()) -> {ok, binary()}.
```

**Output Format:**
```json
{
  "spec_version": "2025-11-25",
  "timestamp": 1738252800,
  "summary": {
    "total_requirements": 45,
    "passed": 42,
    "failed": 2,
    "missing": 1,
    "compliance_percent": 93.33
  },
  "sections": [...],
  "gaps": [...]
}
```

#### generate_html_report/1

Generate HTML compliance report.

```erlang
-spec generate_html_report(compliance_data()) -> {ok, binary()}.
```

**Output:** Full HTML document with embedded CSS styling.

## Usage Examples

### Generating a Full Compliance Report

```erlang
%% Collect evidence
{ok, TestEvidence} = erlmcp_compliance_report:collect_evidence(
    test_result,
    #{module => erlmcp_json_rpc, status => passed}
),

{ok, CoverageEvidence} = erlmcp_compliance_report:collect_evidence(
    coverage_metrics,
    #{module => erlmcp_core, coverage => 85.5}
),

%% Build compliance data
Data = #{
    spec_version => <<"2025-11-25">>,
    timestamp => erlang:system_time(second),
    test_results => [#{name => "JSON-RPC 2.0", status => <<"passed">>}],
    spec_requirements => [
        #{id => "req1", name => "JSON-RPC 2.0", section => "Protocol"}
    ]
},

%% Generate report
{ok, Report} = erlmcp_compliance_report:generate_report(markdown, Data),

%% Write to file
file:write_file("compliance_report.md", Report).
```

### Creating a Traceability Matrix

```erlang
%% Create matrix
Matrix = erlmcp_compliance_report:create_traceability_matrix(Data),

%% Print requirements and their tests
maps:foreach(fun(ReqId, ReqInfo) ->
    io:format("~s: ~s~n", [ReqId, maps:get(requirement, ReqInfo)]),
    Tests = maps:get(tests, ReqInfo),
    lists:foreach(fun(Test) ->
        io:format("  - ~s~n", [Test])
    end, Tests)
end, Matrix).
```

### Gap Analysis Workflow

```erlang
%% Identify gaps
Gaps = erlmcp_compliance_report:identify_gaps(Data),

%% Prioritize by severity
{Critical, High, Medium, Low} = erlmcp_compliance_report:prioritize_gaps(Gaps),

%% Print prioritized gaps
io:format("~nCritical Gaps (~p):~n", [length(Critical)]),
print_gaps(Critical),

io:format("~nHigh Priority Gaps (~p):~n", [length(High)]),
print_gaps(High),

print_gaps(Gaps) ->
    lists:foreach(fun(Gap) ->
        #{
            requirement := Req,
            status := Status,
            description := Desc
        } = Gap,
        io:format("  - ~s (~s): ~s~n", [Req, Status, Desc])
    end, Gaps).
```

### Evidence Bundle for Release

```erlang
%% Create evidence bundle for release certification
create_release_evidence_bundle(ReleaseVersion) ->
    %% Collect all evidence
    {ok, TestResults} = collect_test_evidence(),
    {ok, CoverageData} = collect_coverage_evidence(),
    {ok, SecurityScan} = collect_security_evidence(),
    {ok, Benchmarks} = collect_benchmark_evidence(),

    %% Bundle path
    BundlePath = io_lib:format("/tmp/evidence_~s.tar.gz", [ReleaseVersion]),

    %% Store bundle
    {ok, Path} = erlmcp_compliance_report:store_evidence_bundle(
        BundlePath,
        [TestResults, CoverageData, SecurityScan, Benchmarks]
    ),

    io:format("Evidence bundle stored at: ~s~n", [Path]).
```

## Testing Guidance

### Unit Tests

```erlang
%% Test compliance calculation
compliance_calculation_test() ->
    Data = #{
        test_results => [
            #{status => <<"passed">>},
            #{status => <<"passed">>},
            #{status => <<"failed">>}
        ]
    },

    {ok, Compliance, _} = erlmcp_compliance_report:calculate_compliance(Data),
    ?assert(Compliance > 66.0 andalso Compliance < 67.0).

%% Test evidence hashing
evidence_hash_test() ->
    Evidence = #{test => "value", data => <<"binary">>},
    {ok, Hash1} = erlmcp_compliance_report:hash_evidence(Evidence),
    {ok, Hash2} = erlmcp_compliance_report:hash_evidence(Evidence),
    ?assertEqual(Hash1, Hash2).

%% Test report generation
report_generation_test() ->
    Data = #{
        spec_version => <<"2025-11-25">>,
        test_results => [#{status => <<"passed">>}]
    },

    {ok, Report} = erlmcp_compliance_report:generate_report(markdown, Data),
    ?assert(is_binary(Report)),
    ?assert(byte_size(Report) > 0).
```

## Troubleshooting

### Common Issues

#### Empty Compliance Data

**Symptom:** Compliance percentage is 0%

**Solution:** Ensure test_results array is populated:
```erlang
Data = #{
    test_results => [  % This must not be empty
        #{status => <<"passed">>}
    ]
}
```

#### Missing Evidence

**Symptom:** Evidence not found in bundle

**Solution:** Verify evidence types and structure:
```erlang
%% Correct structure
Evidence = #{
    type => test_result,
    content => #{...},
    timestamp => erlang:system_time(second)
}
```

#### Hash Verification Fails

**Symptom:** Integrity check returns false

**Solution:** Ensure content matches exactly:
```erlang
%% Hash must be from same content
{ok, Hash} = erlmcp_compliance_report:hash_evidence(Content),
{ok, true} = erlmcp_compliance_report:verify_evidence_integrity(Content, Hash).
```

## Related Documentation

- [MCP_SPEC_VALIDATION.md](MCP_SPEC_VALIDATION.md) - Validation overview
- [VALIDATION_TESTING.md](VALIDATION_TESTING.md) - Testing methodology
- [erlmcp_compliance_report](../apps/erlmcp_validation/src/erlmcp_compliance_report.erl)
