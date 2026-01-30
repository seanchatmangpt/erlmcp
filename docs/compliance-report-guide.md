# MCP Compliance Report Generator - Usage Guide

## Overview

The `erlmcp_compliance_report` module generates comprehensive evidence-based compliance reports that map MCP specification requirements to test results with concrete evidence.

## Features

- **Spec-to-Test Traceability**: Maps each spec requirement to its corresponding test
- **Evidence-Based Scoring**: Calculates compliance percentage based on test results
- **Multiple Output Formats**: JSON, Markdown, and HTML reports
- **Trend Analysis**: Tracks compliance over time
- **Gap Analysis**: Identifies untested requirements
- **Recommendations**: Provides prioritized improvement suggestions

## Quick Start

### 1. Start the Report Generator

```erlang
%% Start the compliance report generator
{ok, ReportGen} = erlmcp_compliance_report:start_link().
```

### 2. Generate a Report

```erlang
%% Prepare validation results
ValidationResults = #{
    total_tests => 100,
    passed_tests => 85,
    failed_tests => 10,
    skipped_tests => 5,
    details => #{
        <<"initialize_test">> => #{
            status => passed,
            test_type => method_call,
            timestamp => erlang:timestamp(),
            details => #{}
        },
        <<"tools/list_test">> => #{
            status => passed,
            test_type => method_call,
            timestamp => erlang:timestamp(),
            details => #{}
        }
    }
},

%% Generate report
{ok, Report} = erlmcp_compliance_report:generate_report(ValidationResults).
```

### 3. Format and Save

```erlang
%% Format as Markdown
{ok, Markdown} = erlmcp_compliance_report:format_report(Report, markdown),

%% Save to file
ok = erlmcp_compliance_report:save_report(Report, markdown, "compliance_report.md").
```

## Report Structure

### Executive Summary

Contains:
- Overall compliance percentage
- Test results breakdown (passed/failed/skipped)
- Compliance by category (methods, transports, etc.)

### Detailed Evidence by Spec Section

For each spec section:
- Requirement description
- Test name
- Test status (✅ passed / ❌ failed / ⏭️ skipped)
- Spec reference

### Gap Analysis

Identifies:
- Missing tests
- Non-compliant areas
- Severity levels (critical/high/medium/low)

### Recommendations

Prioritized improvements:
- Priority level
- Category
- Action item
- Expected impact

## Advanced Usage

### Get Spec Traceability Matrix

```erlang
%% Get the mapping from spec requirements to tests
{ok, Traceability} = erlmcp_compliance_report:get_spec_traceability(),

%% Traceability structure:
%% #{
%%     <<"methods">> => [#{
%%         test_name => <<"initialize_test">>,
%%         test_module => <<"erlmcp_spec_compliance_SUITE">>,
%%         validates => <<"Method: initialize">>,
%%         status => passed
%%     }],
%%     <<"notifications">> => [...],
%%     <<"error_codes">> => [...],
%%     <<"transports">> => [...]
%% }
```

### Get Test Evidence

```erlang
%% Get detailed evidence for a specific test
{ok, Evidence} = erlmcp_compliance_report:get_test_evidence(<<"initialize_test">>),

%% Evidence structure:
%% #{
%%     test_name => <<"initialize_test">>,
%%     test_type => <<"method_call">>,
%%     status => passed,
%%     evidence => #{...},
%%     timestamp => {...},
%%     spec_requirements => [<<"§3.1 / initialize">>]
%% }
```

### Calculate Compliance

```erlang
%% Calculate compliance percentage from validation results
{ok, Compliance} = erlmcp_compliance_report:calculate_compliance(ValidationResults),
%% Returns: {ok, 85.0}  % 85% compliance
```

### Get Trend Data

```erlang
%% Get historical compliance data
{ok, Trends} = erlmcp_compliance_report:get_trend_data(),

%% Trends structure:
%% [
%%     #{
%%         date => {...},
%%         compliance_percentage => 85.0,
%%         passed_tests => 85,
%%         failed_tests => 10,
%%         by_category => #{...}
%%     },
%%     ...
%% ]
```

### Get Recommendations

```erlang
%% Get prioritized recommendations
{ok, Recommendations} = erlmcp_compliance_report:get_recommendations(),

%% Recommendations structure:
%% [
%%     #{
%%         priority => high,
%%         category => <<"Test Coverage">>,
%%         action => <<"Increase test coverage to 100%">>,
%%         expected_impact => <<"Ensures all spec requirements are validated">>
%%     },
%%     ...
%% ]
```

## Output Formats

### JSON Format

Structured data suitable for programmatic processing:

```erlang
{ok, Json} = erlmcp_compliance_report:format_report(Report, json).
```

### Markdown Format

Human-readable documentation format:

```erlang
{ok, Markdown} = erlmcp_compliance_report:format_report(Report, markdown).
```

Example output:

```markdown
# MCP Compliance Report

## Metadata

- **Generated:** 2025-01-30 12:00:00.000000
- **Spec Version:** 2025-11-25
- **Total Requirements:** 50
- **Tested Requirements:** 45
- **Compliance:** 90.00%

## Executive Summary

### Overall Compliance

**Compliance Score:** 90.00%

### Test Results

| Metric | Count |
|--------|-------|
| Passed | 45 |
| Failed | 3 |
| Skipped | 2 |
| Total | 50 |

### Compliance by Category

- **methods:** 100.00%
- **notifications:** 85.00%
- **transports:** 80.00%
```

### HTML Format

Styled HTML report with CSS:

```erlang
{ok, Html} = erlmcp_compliance_report:format_report(Report, html).
```

## Integration with Validation Runner

```erlang
%% Run validation suite
{ok, ValidationResults} = erlmcp_validation_runner:run_all(),

%% Generate compliance report
{ok, Report} = erlmcp_compliance_report:generate_report(ValidationResults),

%% Save in multiple formats
ok = erlmcp_compliance_report:save_report(Report, markdown, "report.md"),
ok = erlmcp_compliance_report:save_report(Report, html, "report.html"),
ok = erlmcp_compliance_report:save_report(Report, json, "report.json").
```

## Best Practices

1. **Run After Every Test Suite**: Generate reports after each validation run
2. **Archive Historical Reports**: Keep track of compliance trends over time
3. **Review Gaps Regularly**: Address missing tests to improve coverage
4. **Follow Recommendations**: Prioritize improvements based on report suggestions
5. **Integrate with CI/CD**: Automate report generation in build pipelines

## Troubleshooting

### No Compliance Data

**Problem**: Report shows 0% compliance

**Solution**: Ensure validation results include `total_tests` and `passed_tests` fields

### Missing Spec References

**Problem**: Some tests don't have spec references

**Solution**: Update traceability matrix to include all spec requirements

### Empty Gap Analysis

**Problem**: Gap analysis section is empty

**Solution**: This is expected if all requirements have tests; otherwise check traceability mapping

## API Reference

### Core Functions

- `start_link/0` - Start the report generator
- `stop/1` - Stop the report generator
- `generate_report/1` - Generate compliance report
- `generate_report/2` - Generate report with specific generator
- `format_report/2` - Format report in specified format
- `save_report/3` - Save report to file

### Query Functions

- `get_spec_traceability/0` - Get spec-to-test traceability matrix
- `get_test_evidence/1` - Get evidence for specific test
- `calculate_compliance/1` - Calculate compliance percentage
- `get_trend_data/0` - Get historical trend data
- `get_recommendations/0` - Get improvement recommendations

## Examples

See `apps/erlmcp_validation/test/erlmcp_compliance_report_tests.erl` for comprehensive examples.

## Related Modules

- `erlmcp_spec_parser` - Parse MCP specification
- `erlmcp_protocol_validator` - Validate protocol compliance
- `erlmcp_validation_runner` - Run validation test suites
