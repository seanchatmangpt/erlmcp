# ggen Template Directory

This directory contains Tera templates for generating documentation, code, and configuration from the erlmcp ontology using SPARQL queries.

## Templates Overview

### Documentation Templates

#### `api_reference.md.tera`
Comprehensive API reference documentation following the Diataxis framework.

**Sections**:
- **Tutorials**: Getting started guides for new users
- **How-To Guides**: Task-oriented guides for common operations
- **API Reference**: Complete method, type, and capability documentation
- **Explanation**: Architecture, design principles, and supervision trees
- **Migration Guide**: Version upgrade instructions

**Required Variables** (from SPARQL):
- `project_name`, `project_version`, `protocol_version`
- `methods` - Array of MCP methods with params, responses, examples
- `types` - Array of Erlang types with records and Dialyzer specs
- `capabilities` - Array of MCP capabilities
- `transports` - Array of transport implementations
- `supervision_tree` - Supervision hierarchy structure
- `state_machines` - FSM definitions for tasks/sessions
- `migration_guide` - Version migration information

**Usage**:
```bash
ggen generate \
  --ontology ontology/mcp_protocol.ttl \
  --queries templates/ggen/api_reference_queries.sparql \
  --template templates/ggen/api_reference.md.tera \
  --output docs/api-reference.md
```

**Features**:
- Mermaid diagrams for supervision trees and state machines
- Complete method signatures with Dialyzer specs
- Code examples for every operation
- Error code reference tables
- Performance benchmarks table
- Configuration reference
- Diataxis-compliant structure (tutorials, how-to, reference, explanation)

---

#### `quality_report.md.tera`
TCPS quality gate report with metrics and Andon events.

**Usage**:
```bash
ggen generate \
  --ontology ontology/quality_metrics.ttl \
  --template templates/ggen/quality_report.md.tera \
  --output reports/quality_$(date +%Y%m%d).md
```

---

#### `standard_work.md.tera`
Standard work procedures for production stages (Toyota methodology).

**Usage**:
```bash
ggen generate \
  --ontology ontology/production_stages.ttl \
  --template templates/ggen/standard_work.md.tera \
  --output docs/standard_work/$(stage_id).md
```

---

### Code Generation Templates

#### `erlang_types.hrl.tera`
Generates Erlang header files with type definitions from ontology.

**Usage**:
```bash
ggen generate \
  --ontology ontology/mcp_protocol.ttl \
  --template templates/ggen/erlang_types.hrl.tera \
  --output include/mcp_generated.hrl
```

---

### Data Templates

#### `receipt.json.tera`
SHA-256 receipt chain entries for audit trails.

#### `work_order.ttl.tera`
RDF/Turtle work order definitions for TCPS system.

#### `andon_event.ttl.tera`
Andon event ontology instances for quality violations.

#### `sku_listing.md.tera`
SKU release catalog with evidence bundles.

---

## Query Files

### `api_reference_queries.sparql`
20 SPARQL queries to extract API documentation data:

1. Project metadata
2. MCP methods
3. Error codes
4. Code examples
5. Type definitions
6. Type fields
7. Capability definitions
8. Capability-method mappings
9. Transport definitions
10. Transport use cases
11. Supervision tree structure
12. Supervisor children
13. State machines
14. State transitions
15. Migration breaking changes
16. Migration steps
17. Compatibility notes
18. Benchmark results
19. OTP design principles
20. Example applications

**Usage**:
```bash
# Run all queries and generate documentation
ggen generate \
  --ontology ontology/mcp_protocol.ttl \
  --queries templates/ggen/api_reference_queries.sparql \
  --template templates/ggen/api_reference.md.tera \
  --output docs/api-reference.md \
  --verbose
```

---

## Creating Custom Templates

### Tera Template Syntax

Tera uses Jinja2-like syntax:

**Variables**:
```tera
{{ variable_name }}
{{ nested.property }}
{{ array[0] }}
```

**Filters**:
```tera
{{ value | default(value="N/A") }}
{{ text | upper }}
{{ number | times: 100 }}
```

**Conditionals**:
```tera
{% if condition -%}
  Content when true
{% else -%}
  Content when false
{% endif %}
```

**Loops**:
```tera
{% for item in array -%}
  {{ loop.index }}. {{ item.name }}
{% endfor %}
```

**Comments**:
```tera
{# This is a comment #}
```

### Template Best Practices

1. **Document required variables** in header comment block
2. **Provide defaults** for optional variables: `{{ var | default(value="N/A") }}`
3. **Use descriptive variable names** matching ontology properties
4. **Include examples** in comments for complex structures
5. **Handle empty arrays** gracefully with conditionals
6. **Validate output** against expected format (Markdown, Erlang, JSON, etc.)

### Example Template Structure

```tera
{#
  Template: my_custom_template.md.tera
  Purpose: Generate X from Y ontology

  Context Variables:
    - var1: string - Description of var1
    - var2: array of {prop1, prop2} - Description of var2
    - var3: object - Description of var3 (optional)
#}
# {{ title | default(value="Untitled Document") }}

Generated: {{ timestamp }}

## Section 1

{% if items -%}
{% for item in items -%}
### {{ item.name }}

{{ item.description }}

{% endfor -%}
{% else -%}
No items found.
{% endif %}

---

*Generated by ggen v{{ generator_version | default(value="6.0.0") }}*
```

---

## Ontology Structure for API Documentation

The API reference template expects an ontology with the following structure:

### Minimal Example (`ontology/mcp_protocol.ttl`)

```turtle
@prefix mcp: <http://modelcontextprotocol.org/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix otp: <http://erlang.org/otp/ontology#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Project Metadata
mcp:ErlMCPProject a mcp:Project ;
    dcterms:title "erlmcp" ;
    mcp:version "2.0.0" ;
    mcp:protocolVersion "2025-11-25" ;
    dcterms:description "Production-grade Erlang/OTP MCP SDK" .

# Method Definition
mcp:InitializeMethod a mcp:Method ;
    mcp:methodName "initialize" ;
    rdfs:comment "Establish connection and negotiate capabilities" ;
    mcp:direction "client_to_server" ;
    mcp:phase "initialization" ;
    mcp:requestSchema """#{
        protocolVersion => binary(),
        capabilities => #mcp_client_capabilities{}
    }""" ;
    mcp:responseSchema """#{
        protocolVersion => binary(),
        capabilities => #mcp_server_capabilities{},
        serverInfo => #mcp_server_info{}
    }""" ;
    mcp:dialyzerSpec """-spec initialize(pid(), #mcp_client_capabilities{}) ->
    {ok, #mcp_init_result{}} | {error, term()}.""" .

# Type Definition
mcp:ServerCapabilities a mcp:Type ;
    mcp:typeName "mcp_server_capabilities" ;
    rdfs:comment "Server capability flags" ;
    mcp:erlangRecord """#mcp_server_capabilities{
    resources :: #mcp_capability{} | undefined,
    tools :: #mcp_capability{} | undefined,
    prompts :: #mcp_capability{} | undefined
}""" ;
    mcp:dialyzerSpec """-type mcp_server_capabilities() :: #mcp_server_capabilities{}.""" ;
    mcp:hasField mcp:ResourcesField , mcp:ToolsField , mcp:PromptsField .

mcp:ResourcesField a mcp:Field ;
    mcp:fieldName "resources" ;
    mcp:fieldType "#mcp_capability{} | undefined" ;
    rdfs:comment "Resource support capability" ;
    mcp:required false .

# Capability Definition
mcp:ToolsCapability a mcp:Capability ;
    mcp:capabilityName "tools" ;
    rdfs:comment "Enables tool execution" ;
    mcp:erlangRecord """#mcp_capability{
    enabled :: boolean()
}""" .

# Transport Definition
mcp:TcpTransport a mcp:Transport ;
    mcp:transportName "TCP" ;
    rdfs:comment "TCP socket transport using ranch" ;
    mcp:moduleName "erlmcp_transport_tcp" ;
    mcp:configExample """#{
    type => tcp,
    mode => server,
    port => 8080,
    num_acceptors => 10
}""" ;
    mcp:useCase "Long-lived server connections" ,
                "High-throughput messaging" .

# Supervision Tree
otp:ErlMCPSupervisor a otp:Supervisor ;
    otp:supervisorName "erlmcp_sup" ;
    otp:strategy "one_for_one" ;
    otp:hasChild otp:CoreSupChild , otp:TransportSupChild .

otp:CoreSupChild a otp:ChildSpec ;
    otp:name "erlmcp_core_sup" ;
    otp:type "supervisor" ;
    otp:restart "permanent" .
```

### Full Ontology Requirements

For complete API documentation generation, the ontology should include:

1. **Project metadata** (`mcp:Project`)
2. **All MCP methods** (`mcp:Method`) with request/response schemas
3. **Error definitions** (`mcp:Error`) linked to methods
4. **Code examples** (`ex:Example`) for each method
5. **Type definitions** (`mcp:Type`) with fields and Dialyzer specs
6. **Capability definitions** (`mcp:Capability`)
7. **Transport implementations** (`mcp:Transport`)
8. **Supervision tree** (`otp:Supervisor`, `otp:ChildSpec`)
9. **State machines** (`fsm:StateMachine`, `fsm:State`, `fsm:Transition`)
10. **Migration guide** (`migration:BreakingChange`, `migration:MigrationStep`)
11. **Benchmark results** (`bench:Benchmark`) (optional)
12. **Design principles** (`otp:DesignPrinciple`) (optional)

---

## Validation

### Template Syntax Validation

```bash
# Check Tera syntax
ggen validate-template templates/ggen/api_reference.md.tera

# Test with sample data
ggen test-template \
  --template templates/ggen/api_reference.md.tera \
  --test-data test/fixtures/sample_api_data.json
```

### Output Validation

```bash
# Generate and check Markdown links
ggen generate --template api_reference.md.tera --output /tmp/test.md
markdown-link-check /tmp/test.md

# Check generated Erlang syntax
ggen generate --template erlang_types.hrl.tera --output /tmp/test.hrl
erlc -o /tmp /tmp/test.hrl
```

---

## Contributing Templates

1. **Create template file**: `templates/ggen/my_template.ext.tera`
2. **Document variables**: Add header comment block
3. **Create SPARQL queries**: `templates/ggen/my_template_queries.sparql` (if needed)
4. **Add example**: `test/fixtures/my_template_example_data.json`
5. **Test generation**: Run `ggen generate` with test data
6. **Update this README**: Add template to overview section

---

## License

All templates in this directory are part of the erlmcp project and are licensed under the MIT License.

---

**Generated by**: erlmcp TCPS Quality System
**Last Updated**: 2026-01-30
**Version**: 2.0.0
