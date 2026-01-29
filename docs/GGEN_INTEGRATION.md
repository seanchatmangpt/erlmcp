# ggen Integration for erlmcp

This document explains how erlmcp integrates with ggen (Ontology-Driven Code Generation) to transform TCPS (Toyota Code Production System) ontologies into production artifacts.

## Overview

erlmcp uses ggen to achieve:

1. **Ontology-Driven Development**: Define domain models in RDF/Turtle
2. **SPARQL-Based Queries**: Extract and transform ontology data
3. **Template-Based Generation**: Generate code, documentation, and infrastructure
4. **Deterministic Outputs**: Reproducible builds with cryptographic receipts

## Quick Start

### Installation

**Option 1: Homebrew (macOS/Linux)**
```bash
brew install seanchatmangpt/ggen/ggen
```

**Option 2: Docker**
```bash
docker pull seanchatman/ggen:6.0.0
alias ggen='docker run --rm -v $(pwd):/workspace seanchatman/ggen:6.0.0'
```

**Option 3: Cargo (from source)**
```bash
# Core features only
cargo install ggen-cli

# With all features
cargo install ggen-cli --features full
```

### Basic Usage

```bash
# Validate ontologies and configuration
ggen validate

# Generate all artifacts
ggen sync

# Watch mode (auto-regenerate on changes)
ggen sync --watch

# Generate specific rules only
ggen generate --rule sku_listings
ggen generate --rule quality_reports
```

## Architecture

### Directory Structure

```
erlmcp/
├── ontology/              # RDF/Turtle ontologies (source of truth)
│   ├── tcps_core.ttl
│   ├── tcps_quality.ttl
│   ├── tcps_flow.ttl
│   └── work_orders.ttl
├── shapes/                # SHACL validation shapes
│   └── tcps_shapes.ttl
├── sparql/                # SPARQL queries
│   └── tcps_queries/
│       ├── sku_readiness.rq
│       ├── quality_metrics.rq
│       └── andon_active.rq
├── templates/             # Tera templates
│   └── ggen/
│       ├── sku_listing.md.tera
│       ├── receipt.json.tera
│       └── erlang_types.hrl.tera
├── generated/             # Generated artifacts (gitignored)
│   ├── marketplace/
│   ├── receipts/
│   └── docs/
└── ggen.toml              # ggen configuration
```

### Pipeline Flow

```
┌──────────────┐      ┌─────────────┐      ┌──────────────┐
│ RDF Ontology │──────▶│   SPARQL    │──────▶│ Tera Template│
│  (.ttl)      │      │  Queries    │      │   (.tera)    │
└──────────────┘      └─────────────┘      └──────────────┘
                              │                     │
                              │                     │
                              ▼                     ▼
                      ┌──────────────────────────────────┐
                      │     Generated Artifacts          │
                      │  (.erl, .hrl, .md, .json, .ttl)  │
                      └──────────────────────────────────┘
```

## Generation Rules

### 1. SKU Listings

**Purpose**: Generate marketplace SKU listings from ontology

**Query**: `sku_readiness.rq`
**Template**: `sku_listing.md.tera`
**Output**: `generated/marketplace/sku-{sku_id}.md`

**Example**:
```bash
ggen generate --rule sku_listings
```

Generated output includes:
- Product metadata (name, version, license)
- Features and requirements
- Quality receipts (TCPS proof of work)
- Installation instructions
- Quality metrics

### 2. Quality Reports

**Purpose**: Generate QA reports from quality metrics

**Query**: `quality_metrics.rq`
**Template**: `quality_report.md.tera`
**Output**: `generated/reports/quality-{report_id}.md`

**Example**:
```bash
ggen generate --rule quality_reports
```

### 3. Work Orders (RDF)

**Purpose**: Generate work order RDF instances

**Query**: `work_orders_pending.rq`
**Template**: `work_order.ttl.tera`
**Output**: `generated/work_orders/{work_order_id}.ttl`

**Example**:
```bash
ggen generate --rule work_orders
```

### 4. Andon Events (RDF)

**Purpose**: Generate Andon "stop-the-line" event records

**Query**: `andon_active.rq`
**Template**: `andon_event.ttl.tera`
**Output**: `generated/andon/{andon_id}.ttl`

**Example**:
```bash
ggen generate --rule andon_events
```

### 5. Production Receipts

**Purpose**: Generate cryptographic receipts for completed stages

**Query**: `receipts_by_stage.rq`
**Template**: `receipt.json.tera`
**Output**: `generated/receipts/{stage_name}-{timestamp}.json`

**Example**:
```bash
ggen generate --rule receipts
```

### 6. Erlang Type Definitions

**Purpose**: Generate type-safe Erlang headers from ontology

**Query**: `sku_readiness.rq`
**Template**: `erlang_types.hrl.tera`
**Output**: `include/generated_types.hrl`

**Example**:
```bash
ggen generate --rule erlang_types
```

Generated types:
```erlang
-type sku_id() :: binary().
-type receipt() :: #{
    stage_name := stage_name(),
    timestamp := timestamp(),
    status := status(),
    sku_id := sku_id()
}.
```

### 7. Standard Work Documentation

**Purpose**: Generate standard work procedures

**Query**: `quality_metrics.rq`
**Template**: `standard_work.md.tera`
**Output**: `generated/docs/standard-work/{stage_name}.md`

**Example**:
```bash
ggen generate --rule standard_work
```

## Workflow Integration

### Development Workflow

1. **Modify Ontology** (source of truth)
   ```bash
   vim ontology/tcps_core.ttl
   ```

2. **Validate Changes**
   ```bash
   ggen validate --shacl
   ```

3. **Regenerate Artifacts**
   ```bash
   ggen sync
   ```

4. **Compile Erlang Code**
   ```bash
   rebar3 compile
   ```

5. **Run Tests**
   ```bash
   rebar3 eunit
   ```

### CI/CD Integration

Add to `.gitlab-ci.yml` or GitHub Actions:

```yaml
validate:
  script:
    - ggen validate --shacl
    - ggen sync
    - git diff --exit-code generated/  # Ensure generated files are up-to-date
```

### Pre-Commit Hook

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/bash
set -e

# Validate ontologies
ggen validate --shacl

# Ensure generated files are current
ggen sync

# Check if any generated files changed
if ! git diff --quiet generated/; then
  echo "ERROR: Generated files are out of sync. Run 'ggen sync' and commit changes."
  exit 1
fi
```

## Configuration

### ggen.toml

The `ggen.toml` file configures:

- **Ontology sources**: Where to find RDF files
- **SPARQL queries**: Query directory and named queries
- **Templates**: Template engine and output directory
- **Generation rules**: Mapping of queries → templates → outputs
- **Quality gates**: SLO enforcement and Andon thresholds
- **Determinism**: Checksums, sorted keys, UTC timestamps

See `ggen.toml` for full configuration.

### Feature Flags

Enable optional features in `ggen.toml`:

```toml
[features]
ai_generation = false      # AI-powered template generation
paas_generation = false    # Infrastructure-as-code generation
experimental = false       # Experimental features
```

## Quality Gates

ggen enforces TCPS quality principles:

### Poka-Yoke (Error Proofing)

- ✅ SHACL validation before generation
- ✅ SLO timeout enforcement (default: 5s per rule)
- ✅ Andon signals on failures (🔴 RED = STOP)

### Determinism

- ✅ Cryptographic checksums (SHA256)
- ✅ Sorted JSON keys
- ✅ UTC timestamps (ISO 8601)
- ✅ Reproducible builds

### Andon Signals

- 🔴 **RED** (Critical): 1+ errors → STOP generation
- 🟡 **YELLOW** (Warning): 1-3 warnings → Investigate
- 🟢 **GREEN** (All Clear): 0 issues → Continue

## Examples

### Example 1: Generate SKU Listing

**Input Ontology** (`ontology/tcps_core.ttl`):
```turtle
@prefix tcps-core: <http://erlmcp.org/ontology/tcps/core#> .

<http://erlmcp.org/sku/erlmcp-http-v1.0.0>
    a tcps-core:SKU ;
    tcps-core:skuIdentifier "erlmcp-http-v1.0.0" ;
    tcps-core:name "HTTP Transport" ;
    tcps-core:version "1.0.0" ;
    tcps-core:category "Transport" .
```

**SPARQL Query** (`sparql/tcps_queries/sku_readiness.rq`):
```sparql
SELECT ?sku_id ?name ?version ?category
WHERE {
  ?sku a tcps-core:SKU ;
       tcps-core:skuIdentifier ?sku_id ;
       tcps-core:name ?name ;
       tcps-core:version ?version ;
       tcps-core:category ?category .
}
```

**Generate**:
```bash
ggen generate --rule sku_listings
```

**Output** (`generated/marketplace/sku-erlmcp-http-v1.0.0.md`):
```markdown
# HTTP Transport

**Version**: 1.0.0
**Category**: Transport
**Status**: ✅ TCPS Certified
```

### Example 2: Generate Erlang Types

**Generate**:
```bash
ggen generate --rule erlang_types
```

**Output** (`include/generated_types.hrl`):
```erlang
-type sku_id() :: binary().
-type receipt() :: #{
    stage_name := stage_name(),
    timestamp := timestamp(),
    status := status()
}.
```

## Troubleshooting

### Common Issues

**Issue 1: SHACL Validation Fails**
```
ERROR: SHACL validation failed: Missing required property
```

**Solution**: Fix the ontology to satisfy SHACL shapes
```bash
# View validation report
cat validation_report.md

# Edit ontology
vim ontology/tcps_core.ttl

# Revalidate
ggen validate --shacl
```

**Issue 2: Template Rendering Error**
```
ERROR: Template variable 'sku_id' not found
```

**Solution**: Ensure SPARQL query provides required variables
```bash
# Test query directly
ggen query sku_readiness

# Check template requirements
head -20 templates/ggen/sku_listing.md.tera
```

**Issue 3: Generation Timeout**
```
WARN: Generation rule 'receipts' exceeded SLO (5000ms)
```

**Solution**: Increase timeout in `ggen.toml`
```toml
[quality]
generation_timeout_ms = 10000  # 10 seconds
```

## Advanced Usage

### Watch Mode

Auto-regenerate on file changes:
```bash
ggen sync --watch
```

Watches:
- `ontology/**/*.ttl`
- `templates/**/*.tera`
- `sparql/**/*.rq`

### Custom Queries

Add custom SPARQL queries:
```bash
# Create query
vim sparql/tcps_queries/my_custom.rq

# Register in ggen.toml
[queries.named]
my_custom = "my_custom.rq"

# Add generation rule
[[generation.rules]]
name = "my_custom_rule"
query = "my_custom"
template = "my_template.tera"
output = "custom/{{ id }}.txt"
```

### Custom Templates

Create Tera templates:
```bash
vim templates/ggen/my_template.tera
```

Template syntax (Jinja2-like):
```jinja2
{# Comment #}
{{ variable }}
{% for item in items %}
  - {{ item.name }}
{% endfor %}
{% if condition %}
  ...
{% endif %}
```

## Resources

- **ggen Documentation**: [ggen GitHub](https://github.com/seanchatmangpt/ggen)
- **TCPS Ontology**: `ontology/tcps_core.ttl`
- **SPARQL Reference**: [W3C SPARQL 1.1](https://www.w3.org/TR/sparql11-query/)
- **Tera Templates**: [Tera Documentation](https://tera.netlify.app/)

## License

Apache-2.0 - Same as erlmcp
