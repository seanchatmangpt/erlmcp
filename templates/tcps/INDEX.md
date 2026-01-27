# TCPS Templates - Quick Reference Index

**Version**: 1.0.0
**Status**: Production Ready
**Last Updated**: 2026-01-26

## Quick Links

- [📖 Complete Documentation](README.md) - Full template reference and usage guide
- [✅ Delivery Summary](DELIVERY_SUMMARY.md) - Implementation details and quality metrics
- [💻 Example Code](examples/example_usage.erl) - Working examples for all templates

## Templates at a Glance

### 1. Receipt Template
**File**: `receipt.json.mustache`
**Purpose**: Production stage evidence receipts
**Format**: JSON
**Use Case**: Prove each build/test/release stage completed successfully

### 2. Work Order Template
**File**: `work_order.ttl.mustache`
**Purpose**: Pull-based production triggers
**Format**: RDF/Turtle
**Use Case**: Create work orders from marketplace demand signals

### 3. Andon Event Template
**File**: `andon_event.ttl.mustache`
**Purpose**: Stop-the-line quality events
**Format**: RDF/Turtle
**Use Case**: Record failures and trigger root cause analysis (5 Whys)

### 4. SKU Listing Template
**File**: `sku_listing.md.mustache`
**Purpose**: Marketplace product listings
**Format**: Markdown
**Use Case**: Generate marketplace listings with quality receipts

### 5. Standard Work Template
**File**: `standard_work.md.mustache`
**Purpose**: Production stage procedures
**Format**: Markdown
**Use Case**: Document inputs, outputs, steps, SLOs, failure modes

### 6. Kaizen Report Template
**File**: `kaizen_report.md.mustache`
**Purpose**: Continuous improvement tracking
**Format**: Markdown
**Use Case**: Generate reports on lead time, defects, automation

## Quick Start

### Installation

```bash
# 1. Dependencies already added to rebar.config
rebar3 get-deps

# 2. Compile
rebar3 compile

# 3. Test
rebar3 eunit --module=erlmcp_templates_tests
```

### Basic Usage

```erlang
% Render a receipt
Context = #{
    stage_name => <<"build">>,
    timestamp => <<"2026-01-26T17:30:00Z">>,
    status => <<"passed">>,
    sku_id => <<"erlmcp-v0.6.0">>,
    evidence_data => #{compiler => <<"erlc 25.3">>}
},
{ok, Receipt} = erlmcp_templates:render_receipt(Context).
```

### Run Examples

```erlang
% Compile and run all examples
cd /Users/sac/erlmcp
rebar3 shell
> c("templates/tcps/examples/example_usage").
> example_usage:generate_all_examples().
```

## API Reference

### Template Renderer Module: `erlmcp_templates`

| Function | Purpose | Returns |
|----------|---------|---------|
| `render_receipt/1` | Generate receipt JSON | `{ok, Binary}` or `{error, Reason}` |
| `render_work_order/1` | Generate work order RDF | `{ok, Binary}` or `{error, Reason}` |
| `render_andon_event/1` | Generate Andon event RDF | `{ok, Binary}` or `{error, Reason}` |
| `render_sku_listing/1` | Generate SKU listing markdown | `{ok, Binary}` or `{error, Reason}` |
| `render_standard_work/1` | Generate standard work docs | `{ok, Binary}` or `{error, Reason}` |
| `render_kaizen_report/1` | Generate Kaizen report | `{ok, Binary}` or `{error, Reason}` |
| `render_template/2` | Generic renderer | `{ok, Binary}` or `{error, Reason}` |
| `list_templates/0` | List available templates | `[atom()]` |

## Context Requirements

### Minimal Receipt Context

```erlang
#{
    stage_name => <<"build">>,      % Required
    timestamp => <<"2026-01-26...">>,  % Required, ISO 8601
    status => <<"passed">>,         % Required: passed/failed/skipped
    sku_id => <<"erlmcp-v1.0.0">>, % Required
    evidence_data => #{}            % Required: stage-specific data
}
```

### Minimal Work Order Context

```erlang
#{
    work_order_id => <<"wo-001">>,  % Required
    demand_signal => <<"marketplace">>, % Required
    bucket => <<"reliability">>,    % Required: reliability/security/cost/compliance
    priority => <<"high">>,         % Required: critical/high/medium/low
    created_by => <<"agent">>,      % Required
    created_at => <<"2026...">>,    % Required, ISO 8601
    sku_id => <<"sku-id">>          % Required
}
```

### Minimal Andon Event Context

```erlang
#{
    andon_id => <<"andon-001">>,    % Required
    failure_reason => <<"...">>,    % Required
    affected_sku => <<"sku">>,      % Required
    severity => <<"critical">>,     % Required: critical/major/minor
    timestamp => <<"2026...">>,     % Required, ISO 8601
    detected_by => <<"validator">>, % Required
    stage_name => <<"validate">>,   % Required
    error_details => <<"...">>,     % Required
    remediation_status => <<"pending">> % Required: pending/in_progress/resolved
}
```

See [README.md](README.md) for complete context requirements for all templates.

## Testing

### Run All Tests

```bash
rebar3 eunit --module=erlmcp_templates_tests
```

### Test Coverage

- 20 test cases covering all templates
- Valid context (happy path)
- Missing context validation
- Optional field handling
- Deterministic rendering
- Edge cases and error handling

**Current Status**: ✅ 20/20 tests passing (100%)

## File Organization

```
/Users/sac/erlmcp/
├── templates/tcps/              ← Templates directory
│   ├── *.mustache               ← 6 template files
│   ├── README.md                ← Complete documentation
│   ├── DELIVERY_SUMMARY.md      ← Implementation details
│   ├── INDEX.md                 ← This file
│   └── examples/
│       └── example_usage.erl    ← Working examples
├── src/
│   └── erlmcp_templates.erl     ← Renderer module (221 lines)
└── test/
    └── erlmcp_templates_tests.erl ← Test suite (396 lines)
```

## Common Workflows

### Generate Build Receipt

```erlang
% 1. Create context with build evidence
Context = #{
    stage_name => <<"build">>,
    timestamp => erlmcp_util:iso8601_now(),
    status => <<"passed">>,
    sku_id => <<"my-sku-v1.0.0">>,
    evidence_data => #{
        compiler => <<"erlc">>,
        modules => 10
    }
},

% 2. Render template
{ok, Receipt} = erlmcp_templates:render_receipt(Context),

% 3. Save to receipts directory
file:write_file("receipts/build/my-sku-v1.0.0.json", Receipt).
```

### Trigger Andon Event on Failure

```erlang
% When validation fails
case validate_sku(SKU) of
    {error, Reason} ->
        % Create Andon event
        Context = #{
            andon_id => generate_andon_id(),
            failure_reason => Reason,
            affected_sku => SKU,
            severity => <<"critical">>,
            timestamp => iso8601_now(),
            detected_by => <<"validator">>,
            stage_name => <<"validate">>,
            error_details => format_error(Reason),
            remediation_status => <<"pending">>
        },
        {ok, Andon} = erlmcp_templates:render_andon_event(Context),

        % Save and block release
        file:write_file("receipts/andon/" ++ AndonId ++ ".ttl", Andon),
        block_release(SKU),
        notify_team(Andon);
    ok ->
        continue()
end.
```

### Generate Marketplace Listing

```erlang
% Extract SKU data from ontology
SKUData = extract_sku_from_ontology(SKUID),
QualityMetrics = calculate_quality_metrics(SKUID),
Receipts = collect_receipts(SKUID),

% Render listing
Context = maps:merge(SKUData, #{
    receipts => Receipts,
    quality_metrics => QualityMetrics
}),
{ok, Listing} = erlmcp_templates:render_sku_listing(Context),

% Publish to marketplace
file:write_file("dist/marketplace/" ++ SKUID ++ ".md", Listing),
publish_to_marketplace(Listing).
```

## Troubleshooting

### Missing Context Keys

```erlang
% Error
{error, {missing_context_keys, [timestamp, sku_id]}}

% Solution: Add all required keys
Context = maps:merge(Context, #{
    timestamp => iso8601_now(),
    sku_id => <<"my-sku">>
}).
```

### Template Not Found

```erlang
% Error
{error, {template_not_found, "templates/tcps/receipt.json.mustache", enoent}}

% Solution: Ensure templates directory exists and files are present
file:list_dir("templates/tcps").
```

### Invalid JSON in Evidence Data

```erlang
% Error during render
{error, {render_error, ...}}

% Solution: Evidence data must be a valid Erlang map
evidence_data => #{
    key => <<"value">>,  % Use binaries
    count => 42          % Numbers ok
}
```

## Best Practices

### 1. Deterministic Timestamps

```erlang
% Use ISO 8601 UTC format
timestamp => <<"2026-01-26T17:30:00Z">>

% Not local time or non-standard formats
```

### 2. Validate Before Rendering

```erlang
% Check required fields before calling render
RequiredKeys = [stage_name, timestamp, status, sku_id, evidence_data],
case lists:all(fun(K) -> maps:is_key(K, Context) end, RequiredKeys) of
    true -> erlmcp_templates:render_receipt(Context);
    false -> {error, incomplete_context}
end.
```

### 3. Store Receipts by Stage

```erlang
% Organize receipts by stage for easy retrieval
ReceiptPath = io_lib:format("receipts/~s/~s.json", [StageName, SKUID]),
file:write_file(ReceiptPath, Receipt).
```

### 4. Version Templates with Ontology

```erlang
% Track template versions alongside ontology versions
% When ontology schema changes, update templates and bump version
```

## Performance Tips

### Cache Parsed Templates

```erlang
% For high-volume rendering, cache parsed templates
% bbmustache supports compiled templates
```

### Batch Rendering

```erlang
% Render multiple receipts in parallel
Contexts = [Context1, Context2, Context3],
Receipts = pmap(fun erlmcp_templates:render_receipt/1, Contexts).
```

## Support

### Documentation
- **Template Reference**: [README.md](README.md)
- **Implementation Details**: [DELIVERY_SUMMARY.md](DELIVERY_SUMMARY.md)
- **TCPS Philosophy**: `/Users/sac/erlmcp/docs/TCPS.md`

### Examples
- **Working Code**: [examples/example_usage.erl](examples/example_usage.erl)

### Tests
- **Test Suite**: `/Users/sac/erlmcp/test/erlmcp_templates_tests.erl`

### External Resources
- **Mustache Spec**: https://mustache.github.io/
- **bbmustache**: https://github.com/soranoba/bbmustache

---

**Quick Navigation**: [📖 README](README.md) | [✅ Delivery Summary](DELIVERY_SUMMARY.md) | [💻 Examples](examples/example_usage.erl)

*Last Updated: 2026-01-26 | Version: 1.0.0*
