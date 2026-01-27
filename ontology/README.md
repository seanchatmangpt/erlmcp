# TCPS Ontology - Toyota Code Production System

## Overview

The Toyota Code Production System (TCPS) ontology provides a semantic framework for manufacturing software using proven Toyota Production System (TPS) principles. This ontology enables knowledge graph representation of software production workflows, quality controls, and flow optimization.

## Ontology Files

### 1. `tcps_core.ttl` - Core Manufacturing Concepts

**Key Classes:**
- `SKU` - Stock Keeping Units (marketplace offerings as products)
- `WorkOrder` - Pull-based demand signals for production
- `ProductionStage` - Manufacturing pipeline steps (COMPILE, TEST, RELEASE, PUBLISH)
- `Receipt` - Immutable evidence of stage completion
- `AndonEvent` - Quality alerts and stop-the-line signals

**Production Stages:**
1. **COMPILE** - Build artifacts from source code
2. **TEST** - Execute automated quality validation
3. **RELEASE** - Create versioned, signed release artifacts
4. **PUBLISH** - Deploy to marketplace or distribution channels

### 2. `tcps_quality.ttl` - Quality and Error Proofing

**Key Classes:**
- `Jidoka` - Automation with human touch (built-in quality detection)
- `PokaYoke` - Error proofing mechanisms (prevention and detection)
- `QualityGate` - Stage exit criteria and validation rules
- `QualityMetric` - Measurable quality indicators

**Quality Mechanisms:**
- **Jidoka Types**: Static analysis, automated tests, security scans, performance monitoring
- **Poka-Yoke Types**: Prevention (design constraints), Detection (validation)
- **Quality Gates**: Blocking criteria for stage transitions
- **Metrics**: Code coverage, test pass rate, defect density, security scores

### 3. `tcps_flow.ttl` - Pull System and Flow Control

**Key Classes:**
- `Kanban` - Visual pull signals and WIP authorization
- `WIPLimit` - Work-in-progress constraints per stage
- `Heijunka` - Production leveling and demand smoothing
- `HeijunkaBucket` - Priority categories (RELIABILITY, SECURITY, COST, COMPLIANCE)
- `PullSignal` - Demand authorization messages
- `TaktTime` - Customer demand rate and production pacing

**Heijunka Buckets:**
1. **RELIABILITY** - Uptime, stability, bug fixes
2. **SECURITY** - Vulnerabilities, compliance, hardening
3. **COST** - Efficiency, resource optimization, waste reduction
4. **COMPLIANCE** - Regulatory, legal, policy requirements

## Usage

### Loading the Ontology

```turtle
@prefix tcps-core: <http://erlmcp.org/ontology/tcps/core#> .
@prefix tcps-quality: <http://erlmcp.org/ontology/tcps/quality#> .
@prefix tcps-flow: <http://erlmcp.org/ontology/tcps/flow#> .

# Import all TCPS ontologies
@import <http://erlmcp.org/ontology/tcps/core> .
@import <http://erlmcp.org/ontology/tcps/quality> .
@import <http://erlmcp.org/ontology/tcps/flow> .
```

### Example Instance Data

```turtle
# Define a SKU
:erlmcp-server-v1.0.0 a tcps-core:SKU ;
    tcps-core:skuIdentifier "erlmcp:mcp-server" ;
    tcps-core:skuVersion "1.0.0" ;
    tcps-core:inventoryLevel 5 ;
    tcps-core:reorderPoint 2 ;
    tcps-core:targetInventory 10 ;
    tcps-core:leadTime 120.0 .

# Create a WorkOrder
:wo-001 a tcps-core:WorkOrder ;
    tcps-core:workOrderId "WO-2026-001" ;
    tcps-core:produceSKU :erlmcp-server-v1.0.0 ;
    tcps-core:quantity 3 ;
    tcps-core:priority "HIGH" ;
    tcps-core:status "IN_PROGRESS" ;
    tcps-core:currentStage tcps-core:TestStage ;
    tcps-core:dueBy "2026-01-27T10:00:00Z"^^xsd:dateTime .

# Create a Receipt
:receipt-wo001-test a tcps-core:Receipt ;
    tcps-core:receiptId "RECEIPT-WO001-TEST" ;
    tcps-core:forWorkOrder :wo-001 ;
    tcps-core:atStage tcps-core:TestStage ;
    tcps-core:completedAt "2026-01-26T15:30:00Z"^^xsd:dateTime ;
    tcps-core:qualityStatus "PASSED" ;
    tcps-core:metrics "{\"coverage\": 0.85, \"tests_passed\": 142, \"tests_failed\": 0}" .

# Define a Kanban
:kanban-test-1 a tcps-flow:Kanban ;
    tcps-flow:kanbanId "KANBAN-TEST-001" ;
    tcps-flow:controlsStage tcps-core:TestStage ;
    tcps-flow:kanbanStatus "ACTIVE" ;
    tcps-flow:authorizesWorkOrder :wo-001 ;
    tcps-flow:activatedAt "2026-01-26T14:00:00Z"^^xsd:dateTime .

# Create an Andon Event
:andon-001 a tcps-core:AndonEvent ;
    tcps-core:andonId "ANDON-2026-001" ;
    tcps-core:severity "HIGH" ;
    tcps-core:triggeredAt "2026-01-26T16:00:00Z"^^xsd:dateTime ;
    tcps-core:affectsWorkOrder :wo-002 ;
    tcps-core:affectsStage tcps-core:TestStage ;
    tcps-core:reason "Test coverage dropped below 80% threshold" ;
    tcps-core:actionTaken "PRODUCTION_STOPPED" ;
    tcps-core:isResolved false .
```

## SPARQL Queries

See `example_queries.sparql` for 20 comprehensive SPARQL queries demonstrating:
- WorkOrder tracking and flow analysis
- Quality gate validation
- WIP limit monitoring
- Heijunka bucket allocation
- Andon event management
- Pull signal performance
- Takt time compliance

### Example Query: Find Active WorkOrders

```sparql
PREFIX tcps-core: <http://erlmcp.org/ontology/tcps/core#>

SELECT ?workOrder ?workOrderId ?stage ?priority ?dueBy
WHERE {
    ?workOrder a tcps-core:WorkOrder ;
               tcps-core:workOrderId ?workOrderId ;
               tcps-core:currentStage ?stage ;
               tcps-core:priority ?priority ;
               tcps-core:dueBy ?dueBy ;
               tcps-core:status "IN_PROGRESS" .
}
ORDER BY ?dueBy
```

## SHACL Validation

The ontology includes SHACL shapes for validation:

### Quality Gate Shape
```turtle
tcps-quality:QualityGateShape
    a sh:NodeShape ;
    sh:targetClass tcps-quality:QualityGate ;
    sh:property [
        sh:path tcps-quality:gateId ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
    ] .
```

### Validate Instance Data
```bash
# Using Apache Jena SHACL validator
shacl validate --shapes tcps_quality.ttl --data instance_data.ttl

# Using TopBraid SHACL API
shacl -datafile instance_data.ttl -shapesfile tcps_quality.ttl
```

## Integration with erlmcp

### Use Case: MCP Server Production

```turtle
# SKU for erlmcp MCP server
:erlmcp-mcp-server a tcps-core:SKU ;
    tcps-core:skuIdentifier "erlmcp:mcp-server" ;
    tcps-core:skuVersion "0.6.0" ;
    tcps-core:inventoryLevel 0 ;
    tcps-core:reorderPoint 1 .

# WorkOrder triggered by low inventory
:wo-erlmcp-001 a tcps-core:WorkOrder ;
    tcps-core:workOrderId "WO-ERLMCP-001" ;
    tcps-core:produceSKU :erlmcp-mcp-server ;
    tcps-core:quantity 1 ;
    tcps-core:priority "HIGH" ;
    tcps-core:status "PENDING" .

# Jidoka mechanism: Dialyzer type checking
:jidoka-dialyzer a tcps-quality:Jidoka ;
    tcps-quality:jidokaName "Dialyzer Type Checker" ;
    tcps-quality:jidokaType "STATIC_ANALYSIS" ;
    tcps-quality:appliesToStage tcps-core:CompileStage ;
    tcps-quality:preventsPropagation true ;
    tcps-quality:automatedAction "HALT_BUILD" .

# Quality Gate: Test stage requirements
:test-gate-erlmcp a tcps-quality:QualityGate ;
    tcps-quality:gateId "TEST_GATE_ERLMCP" ;
    tcps-quality:gateName "erlmcp Test Gate" ;
    tcps-quality:forStage tcps-core:TestStage ;
    tcps-quality:isBlocking true ;
    tcps-quality:minimumThreshold 0.80 ;
    tcps-quality:criteriaDescription "All tests pass, coverage >= 80%, no critical security issues" .
```

## Ontology Statistics

- **Classes**: 15 core classes across 3 ontologies
- **Object Properties**: 25+ relationships
- **Datatype Properties**: 80+ attributes
- **SHACL Shapes**: 7 validation shapes
- **Individuals**: 8 standard instances (production stages, buckets)

## Namespace URIs

- **Core**: `http://erlmcp.org/ontology/tcps/core#`
- **Quality**: `http://erlmcp.org/ontology/tcps/quality#`
- **Flow**: `http://erlmcp.org/ontology/tcps/flow#`

## Standards Compliance

- **RDF 1.1**: Full compliance with RDF syntax and semantics
- **OWL 2**: Uses OWL 2 DL profile for reasoning
- **SHACL**: W3C SHACL for data validation
- **SPARQL 1.1**: Compatible with all SPARQL 1.1 features

## Tools and Libraries

### Recommended RDF Tools
- **Apache Jena**: RDF processing, SPARQL queries, SHACL validation
- **RDFLib** (Python): Ontology manipulation and querying
- **Oxigraph**: Fast RDF triple store with SPARQL endpoint
- **Blazegraph**: Scalable graph database for production use

### Erlang/Elixir RDF Libraries
- **grax**: Elixir RDF graph data mapper
- **rdf-ex**: RDF toolkit for Elixir
- **sparql-ex**: SPARQL client for Elixir

## License

Apache 2.0 (same as erlmcp project)

## References

- Toyota Production System (TPS)
- Lean Manufacturing Principles
- Knowledge Graph Engineering
- W3C Semantic Web Standards
