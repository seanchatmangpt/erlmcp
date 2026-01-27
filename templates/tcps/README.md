# TCPS Templates - Toyota Code Production System

This directory contains Mustache templates for generating TCPS (Toyota Code Production System) artifacts. These templates transform ontology data into various production artifacts with deterministic, auditable outputs.

## Available Templates

### 1. receipt.json.mustache

**Purpose**: Generate Receipt JSON from ontology data as proof-of-work evidence.

**Output Format**: JSON

**Required Context**:
- `stage_name` - Production stage name
- `timestamp` - ISO 8601 timestamp
- `status` - Status ("passed", "failed", "skipped")
- `sku_id` - SKU identifier
- `evidence_data` - Stage-specific evidence (map/object)

**Optional Context**:
- `checksum` - SHA256 checksum
- `duration_ms` - Duration in milliseconds
- `work_order_id` - Work order reference
- `environment` - Build environment

**Example Usage**:
```erlang
Context = #{
    stage_name => <<"build">>,
    timestamp => <<"2026-01-26T17:30:00Z">>,
    status => <<"passed">>,
    sku_id => <<"erlmcp-v1.0.0">>,
    evidence_data => #{
        compiler => <<"erlc 25.3">>,
        warnings => 0,
        modules => 42
    }
},
{ok, Receipt} = erlmcp_templates:render_receipt(Context).
```

### 2. work_order.ttl.mustache

**Purpose**: Create WorkOrder RDF instances for pull-based production.

**Output Format**: RDF/Turtle

**Required Context**:
- `work_order_id` - Unique work order ID
- `demand_signal` - Source of demand
- `bucket` - Heijunka bucket (reliability/security/cost/compliance)
- `priority` - Priority level
- `created_by` - Creator identifier
- `created_at` - ISO 8601 timestamp
- `sku_id` - Target SKU identifier

**Optional Context**:
- `target_completion` - Target completion timestamp
- `description` - Human-readable description
- `dependencies` - List of work order IDs this depends on

**Example Usage**:
```erlang
Context = #{
    work_order_id => <<"wo-2026-001">>,
    demand_signal => <<"marketplace">>,
    bucket => <<"reliability">>,
    priority => <<"high">>,
    created_by => <<"mcp-orchestrator">>,
    created_at => <<"2026-01-26T17:30:00Z">>,
    sku_id => <<"erlmcp-http-transport-v1.0.0">>
},
{ok, WorkOrder} = erlmcp_templates:render_work_order(Context).
```

### 3. andon_event.ttl.mustache

**Purpose**: Create Andon "stop-the-line" event records with 5 Whys analysis.

**Output Format**: RDF/Turtle

**Required Context**:
- `andon_id` - Unique Andon event ID
- `failure_reason` - Short description of failure
- `affected_sku` - SKU identifier that failed
- `severity` - Severity level (critical/major/minor)
- `timestamp` - ISO 8601 timestamp
- `detected_by` - Stage/tool that detected failure
- `stage_name` - Production stage where failure occurred
- `error_details` - Detailed error message
- `remediation_status` - Status (pending/in_progress/resolved)

**Optional Context**:
- `work_order_id` - Associated work order
- `five_whys` - Array of `#{why => "...", answer => "..."}` maps
- `remediation_plan` - Description of fix
- `spec_delta_required` - Boolean
- `test_delta_required` - Boolean

**Example Usage**:
```erlang
Context = #{
    andon_id => <<"andon-2026-001">>,
    failure_reason => <<"SHACL validation failed">>,
    affected_sku => <<"erlmcp-broken-v1.0.0">>,
    severity => <<"critical">>,
    timestamp => <<"2026-01-26T19:00:00Z">>,
    detected_by => <<"shacl-validator">>,
    stage_name => <<"validate">>,
    error_details => <<"Shape violation: missing property">>,
    remediation_status => <<"pending">>,
    five_whys => [
        #{why => <<"Why did validation fail?">>,
          answer => <<"Missing required field">>},
        #{why => <<"Why was field missing?">>,
          answer => <<"Template bug">>}
    ]
},
{ok, AndonEvent} = erlmcp_templates:render_andon_event(Context).
```

### 4. sku_listing.md.mustache

**Purpose**: Generate marketplace listings from SKU ontology data.

**Output Format**: Markdown

**Required Context**:
- `sku_id` - SKU identifier
- `name` - Product name
- `version` - Semantic version
- `description` - Short description
- `category` - Category
- `author` - Author/maintainer
- `license` - License identifier
- `repository_url` - Source repository URL
- `features` - List of feature strings

**Optional Context**:
- `long_description` - Detailed description (markdown)
- `documentation_url` - Documentation link
- `requirements` - List of requirements
- `install_command` - Installation command
- `receipts` - List of receipt summaries
- `quality_metrics` - Quality metrics object
- `tags` - Searchable tags
- `created_at`, `updated_at` - Timestamps

**Example Usage**:
```erlang
Context = #{
    sku_id => <<"erlmcp-http-v1.0.0">>,
    name => <<"HTTP Transport">>,
    version => <<"1.0.0">>,
    description => <<"HTTP/HTTPS transport implementation">>,
    category => <<"Transport">>,
    author => <<"erlmcp Team">>,
    license => <<"Apache-2.0">>,
    repository_url => <<"https://github.com/example/erlmcp">>,
    features => [<<"HTTP/2 support">>, <<"TLS encryption">>]
},
{ok, Listing} = erlmcp_templates:render_sku_listing(Context).
```

### 5. standard_work.md.mustache

**Purpose**: Generate standard work documentation for production stages.

**Output Format**: Markdown

**Required Context**:
- `stage_name` - Production stage name
- `stage_id` - Unique stage identifier
- `description` - Purpose of stage
- `inputs` - List of input artifacts (maps with name/type/source)
- `outputs` - List of output artifacts (maps with name/type/destination)
- `steps` - Ordered list of steps (maps with step_name/action)
- `slo_time_budget_ms` - Maximum time budget (ms)
- `slo_success_rate` - Minimum success rate (0.0-1.0)

**Optional Context**:
- `receipts` - List of receipt types generated
- `failure_modes` - Known failure modes
- `tools` - Tools/commands used
- `dependencies` - Other stages this depends on
- `owner`, `last_updated`, `version` - Metadata

**Example Usage**:
```erlang
Context = #{
    stage_name => <<"Build Stage">>,
    stage_id => <<"build-001">>,
    description => <<"Compile Erlang code">>,
    inputs => [#{name => <<"Source">>, type => <<"erl">>, source => <<"src/">>}],
    outputs => [#{name => <<"BEAM">>, type => <<"beam">>, destination => <<"ebin/">>}],
    steps => [#{step_name => <<"Compile">>, action => <<"rebar3 compile">>}],
    slo_time_budget_ms => 30000,
    slo_success_rate => 0.99
},
{ok, StandardWork} = erlmcp_templates:render_standard_work(Context).
```

### 6. kaizen_report.md.mustache

**Purpose**: Generate continuous improvement reports from quality metrics.

**Output Format**: Markdown

**Required Context**:
- `report_id` - Unique report identifier
- `report_period_start` - ISO 8601 start date
- `report_period_end` - ISO 8601 end date
- `generated_at` - Report generation timestamp
- `generated_by` - Generator identifier
- `metrics_source` - SPARQL query or data source

**Optional Context**:
- `lead_time_data` - Lead time metrics object
- `defect_rate_data` - Defect rate metrics object
- `automation_data` - Automation coverage metrics
- `throughput_data` - Production throughput metrics
- `improvements` - Implemented improvements list
- `opportunities` - Improvement opportunities list
- `andon_events` - Andon events in period
- `recommendations` - Recommended actions

**Example Usage**:
```erlang
Context = #{
    report_id => <<"kaizen-2026-q1">>,
    report_period_start => <<"2026-01-01">>,
    report_period_end => <<"2026-03-31">>,
    generated_at => <<"2026-04-01T00:00:00Z">>,
    generated_by => <<"kaizen-agent">>,
    metrics_source => <<"quality_metrics.rq">>,
    lead_time_data => #{
        average_lead_time_ms => 25000,
        median_lead_time_ms => 22000
    }
},
{ok, Report} = erlmcp_templates:render_kaizen_report(Context).
```

## Template Engine

All templates use [bbmustache](https://github.com/soranoba/bbmustache) - a binary pattern match based Mustache template engine for Erlang/OTP.

**Features**:
- Logic-less templates (separation of concerns)
- Fast binary pattern matching
- No regular expressions
- Deterministic output
- Full Mustache spec compliance

## Determinism Guidelines

To ensure deterministic, reproducible builds:

1. **Timestamps**: Always use ISO 8601 UTC format (`2026-01-26T17:30:00Z`)
2. **Sorting**: Sort object keys/maps alphabetically before encoding
3. **Binary Data**: Use consistent encoding (hex or base64)
4. **Version Control**: Version templates alongside ontology schemas
5. **Checksums**: Include checksums in receipts for verification

## Testing

Run comprehensive template tests:

```bash
# Unit tests
rebar3 eunit --module=erlmcp_templates_tests

# With coverage
rebar3 as test cover
```

## Schema References

- Receipt: https://erlmcp.dev/schemas/tcps/receipt-v1.json
- Work Order: https://erlmcp.dev/schemas/tcps/work-order-v1.ttl
- Andon Event: https://erlmcp.dev/schemas/tcps/andon-event-v1.ttl

## TCPS Compliance

These templates implement Toyota Production System principles:

- **Pull-based**: Work orders trigger generation (Just-In-Time)
- **Built-in Quality**: Templates enforce complete data (Jidoka)
- **Evidence**: Receipts prove each stage passed (Proof of Work)
- **Stop-the-Line**: Andon events block releases (Quality Gates)
- **Continuous Improvement**: Kaizen reports drive optimization
- **Standard Work**: Documented procedures reduce variation

## License

Apache-2.0 - See LICENSE file for details.
