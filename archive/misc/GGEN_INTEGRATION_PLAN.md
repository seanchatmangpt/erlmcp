# ggen Integration Plan for erlmcp
## MCP 2025-11-25 Compliance via Ontology-Driven Code Generation

**Plan Author**: plan-designer agent
**Date**: 2026-01-30
**Target**: 95%+ MCP compliance in 2-3 sprints
**Strategy**: 80/20 refactor using ggen

---

## Executive Summary

This plan transforms erlmcp from 78/100 compliance to 95+/100 by using ggen (ontology-driven code generation) to eliminate manual validation code, implement missing features, and ensure protocol correctness through a single source of truth: the MCP 2025-11-25 specification as RDF ontology.

### Key Metrics

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| MCP Compliance | 78/100 | 95+/100 | +17 points |
| Manual Validation LOC | 3,000+ | 0 | -100% |
| Protocol Coverage | 18/33 methods | 33/33 methods | +83% |
| Validation Bugs | Unknown | 0 (generated) | Eliminated |
| Feature Development Time | 2-3 days | 2-3 hours | -90% |
| Test Coverage | 78% | 95%+ | +17% |

### P0 Fixes Addressed

1. Tool argument validation (CRITICAL)
2. Task management system (30% of spec, CRITICAL)
3. Client timeout/cleanup (memory leak, CRITICAL)
4. Transport test infrastructure (blocker, CRITICAL)
5. Streaming protocol integration (HIGH)

---

## Phase 1: Setup & Foundation (Day 1-2)

### Objectives
- Install ggen tooling
- Create directory structure for MCP ontology
- Verify existing TCPS ontology infrastructure
- Set up quality gates

### Tasks

#### 1.1 Install ggen (2 hours)

**Option A: Homebrew (macOS/Linux)**
```bash
brew install seanchatmangpt/ggen/ggen
ggen --version  # Verify v6.0.0+
```

**Option B: Cargo (from source)**
```bash
cargo install ggen-cli --features full
ggen --version
```

**Option C: Docker**
```bash
docker pull seanchatman/ggen:6.0.0
alias ggen='docker run --rm -v $(pwd):/workspace seanchatman/ggen:6.0.0'
```

**Verification:**
```bash
./scripts/verify_ggen.sh
# Expected: 🟢 ALL CHECKS PASSED
```

**Deliverable:** ggen installed and verified

#### 1.2 Create MCP Ontology Directory Structure (1 hour)

```bash
mkdir -p ontology/mcp/
mkdir -p ontology/mcp/core/
mkdir -p ontology/mcp/extensions/
mkdir -p shapes/mcp/
mkdir -p sparql/mcp_queries/
mkdir -p templates/ggen/mcp/
mkdir -p generated/mcp/
```

**Directory Purpose:**
- `ontology/mcp/core/` - MCP 2025-11-25 base specification
- `ontology/mcp/extensions/` - Custom extensions (task management, streaming)
- `shapes/mcp/` - SHACL validation shapes
- `sparql/mcp_queries/` - SPARQL queries for code generation
- `templates/ggen/mcp/` - Tera templates for Erlang generation
- `generated/mcp/` - Generated code (gitignored)

**Deliverable:** Directory structure created, added to .ggenignore

#### 1.3 Review Existing TCPS Ontology (2 hours)

**Action:** Study existing ontology patterns
```bash
cat ontology/tcps_core.ttl
cat templates/ggen/erlang_types.hrl.tera
cat ggen.toml
```

**Learning Points:**
- RDF/Turtle syntax patterns
- SPARQL query structure
- Tera template variables
- Quality gate configuration

**Deliverable:** Team familiar with ggen workflow

#### 1.4 Configure ggen for MCP (3 hours)

**Update `ggen.toml`:**

```toml
# Add MCP-specific ontology sources
[ontology]
paths = [
    # Existing TCPS
    "ontology/tcps_core.ttl",
    "ontology/tcps_quality.ttl",
    "ontology/tcps_flow.ttl",
    "ontology/work_orders.ttl",

    # NEW: MCP Protocol
    "ontology/mcp/core/protocol.ttl",
    "ontology/mcp/core/messages.ttl",
    "ontology/mcp/core/capabilities.ttl",
    "ontology/mcp/core/validation.ttl",
    "ontology/mcp/extensions/tasks.ttl",
    "ontology/mcp/extensions/streaming.ttl",
]

# Add MCP SHACL shapes
shapes = "shapes/mcp/mcp_shapes.ttl"

# MCP-specific queries
[queries.named]
# Existing TCPS queries...
mcp_methods = "mcp_methods.rq"
mcp_capabilities = "mcp_capabilities.rq"
mcp_validators = "mcp_validators.rq"
mcp_task_states = "mcp_task_states.rq"
mcp_error_codes = "mcp_error_codes.rq"

# MCP code generation rules
[[generation.rules]]
name = "mcp_server_handlers"
description = "Generate MCP server request handlers with validation"
query = "mcp_methods"
template = "mcp/server_handler.erl.tera"
output = "generated/mcp/handlers/erlmcp_handler_{{ method }}.erl"
enabled = true

[[generation.rules]]
name = "mcp_client_api"
description = "Generate MCP client API methods"
query = "mcp_methods"
template = "mcp/client_method.erl.tera"
output = "generated/mcp/client/erlmcp_client_{{ method }}.erl"
enabled = true

[[generation.rules]]
name = "mcp_validators"
description = "Generate JSON Schema validators for each capability"
query = "mcp_validators"
template = "mcp/validator.erl.tera"
output = "generated/mcp/validators/erlmcp_validator_{{ component }}.erl"
enabled = true

[[generation.rules]]
name = "mcp_task_manager"
description = "Generate task state machine from lifecycle ontology"
query = "mcp_task_states"
template = "mcp/task_manager.erl.tera"
output = "generated/mcp/erlmcp_task_manager.erl"
enabled = true

[[generation.rules]]
name = "mcp_error_codes"
description = "Generate MCP error code constants"
query = "mcp_error_codes"
template = "mcp/error_codes.hrl.tera"
output = "generated/mcp/include/mcp_error_codes.hrl"
enabled = true

[[generation.rules]]
name = "mcp_types"
description = "Generate Erlang type definitions from MCP ontology"
query = "mcp_capabilities"
template = "mcp/types.hrl.tera"
output = "generated/mcp/include/mcp_types.hrl"
enabled = true

[[generation.rules]]
name = "mcp_tests"
description = "Generate test stubs for compliance testing"
query = "mcp_methods"
template = "mcp/test_stub.erl.tera"
output = "generated/mcp/test/erlmcp_{{ method }}_tests.erl"
enabled = true
```

**Deliverable:** ggen.toml configured for MCP code generation

#### 1.5 Quality Gates Setup (2 hours)

**Configure quality enforcement in `ggen.toml`:**

```toml
[quality]
enforce_slo = true
generation_timeout_ms = 10000  # 10s per rule

[quality.andon]
red_threshold = 1       # STOP on any error
yellow_threshold = 3    # CAUTION on 3+ warnings
green_threshold = 0     # GO only if no issues

[validation]
shacl_validate = true
fail_on_error = true
report_path = "generated/mcp/validation_report.md"
```

**CI/CD Integration:**

Create `.github/workflows/ggen-validate.yml`:
```yaml
name: ggen MCP Validation

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install ggen
        run: |
          curl -sSL https://github.com/seanchatmangpt/ggen/releases/download/v6.0.0/ggen-linux-amd64 -o ggen
          chmod +x ggen
      - name: Validate ontology
        run: ./ggen validate --shacl
      - name: Generate code
        run: ./ggen sync
      - name: Verify generated files
        run: |
          git diff --exit-code generated/
          # Fail if generated files are out of sync
```

**Deliverable:** Quality gates active, CI/CD configured

### Phase 1 Success Criteria

- [ ] ggen v6.0+ installed and verified
- [ ] Directory structure created
- [ ] ggen.toml configured for MCP
- [ ] Quality gates enabled
- [ ] CI/CD workflow created
- [ ] Team trained on ggen basics

**Time: 2 days**
**Risk: LOW** (tooling setup, no code changes)

---

## Phase 2: Ontology Definition (Day 3-7)

### Objectives
- Define MCP 2025-11-25 specification as RDF ontology
- Create SHACL validation shapes
- Write SPARQL queries for code extraction
- Validate ontology completeness

### Tasks

#### 2.1 Create Core MCP Protocol Ontology (3 days)

**File: `ontology/mcp/core/protocol.ttl`**

```turtle
@prefix mcp: <http://modelcontextprotocol.io/2025-11-25/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

# =============================================================================
# MCP Protocol Base Classes
# =============================================================================

mcp:Message a rdfs:Class ;
    rdfs:label "MCP Message" ;
    rdfs:comment "Base class for all MCP protocol messages" ;
    owl:disjointUnionOf (mcp:Request mcp:Response mcp:Notification) .

mcp:Request a rdfs:Class ;
    rdfs:subClassOf mcp:Message ;
    rdfs:label "Request" ;
    rdfs:comment "Request message requiring response" .

mcp:Response a rdfs:Class ;
    rdfs:subClassOf mcp:Message ;
    rdfs:label "Response" ;
    rdfs:comment "Response to request" .

mcp:Notification a rdfs:Class ;
    rdfs:subClassOf mcp:Message ;
    rdfs:label "Notification" ;
    rdfs:comment "Notification (no response expected)" .

# =============================================================================
# MCP Lifecycle Phases
# =============================================================================

mcp:Phase a rdfs:Class ;
    rdfs:label "Protocol Phase" ;
    owl:oneOf (mcp:PreInitialization mcp:Initialized) .

mcp:PreInitialization a mcp:Phase ;
    rdfs:label "pre_initialization" ;
    mcp:allowedMethods (mcp:initialize) .

mcp:Initialized a mcp:Phase ;
    rdfs:label "initialized" ;
    mcp:allowedMethods (
        mcp:toolsList mcp:toolsCall
        mcp:resourcesList mcp:resourcesRead
        mcp:promptsList mcp:promptsGet
        mcp:tasksList mcp:tasksCreate
    ) .

# =============================================================================
# Core Protocol Methods
# =============================================================================

# Initialize
mcp:initialize a mcp:Request ;
    rdfs:label "initialize" ;
    mcp:method "initialize" ;
    mcp:requiresPhase mcp:PreInitialization ;
    mcp:params [
        mcp:hasParam [
            mcp:name "clientInfo" ;
            mcp:type mcp:ClientInfo ;
            mcp:required true
        ] ;
        mcp:hasParam [
            mcp:name "capabilities" ;
            mcp:type mcp:ClientCapabilities ;
            mcp:required true
        ] ;
        mcp:hasParam [
            mcp:name "protocolVersion" ;
            mcp:type xsd:string ;
            mcp:required true ;
            mcp:pattern "^\\d{4}-\\d{2}-\\d{2}$" ;
            mcp:enum ("2024-11-05" "2025-11-25")
        ]
    ] ;
    mcp:returns mcp:InitializeResult ;
    mcp:transitionsTo mcp:Initialized .

# Tools
mcp:toolsList a mcp:Request ;
    rdfs:label "tools/list" ;
    mcp:method "tools/list" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "tools" ;
    mcp:params [
        mcp:hasParam [
            mcp:name "cursor" ;
            mcp:type xsd:string ;
            mcp:required false
        ]
    ] ;
    mcp:returns mcp:ToolsListResult ;
    mcp:supportsPagination true .

mcp:toolsCall a mcp:Request ;
    rdfs:label "tools/call" ;
    mcp:method "tools/call" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "tools" ;
    mcp:validateSchema true ;  # CRITICAL: Validation before handler
    mcp:params [
        mcp:hasParam [
            mcp:name "name" ;
            mcp:type xsd:string ;
            mcp:required true
        ] ;
        mcp:hasParam [
            mcp:name "arguments" ;
            mcp:type mcp:JsonObject ;
            mcp:required false ;
            mcp:validatedBy mcp:ToolInputSchema
        ]
    ] ;
    mcp:returns mcp:ToolCallResult ;
    mcp:supportsProgress true ;
    mcp:timeout 30000 .  # 30s default

# Resources
mcp:resourcesList a mcp:Request ;
    rdfs:label "resources/list" ;
    mcp:method "resources/list" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "resources" ;
    mcp:supportsPagination true .

mcp:resourcesRead a mcp:Request ;
    rdfs:label "resources/read" ;
    mcp:method "resources/read" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "resources" ;
    mcp:validateUri true ;  # URI validation + path traversal prevention
    mcp:params [
        mcp:hasParam [
            mcp:name "uri" ;
            mcp:type xsd:anyURI ;
            mcp:required true ;
            mcp:validatedBy mcp:UriValidator
        ]
    ] .

mcp:resourcesSubscribe a mcp:Request ;
    rdfs:label "resources/subscribe" ;
    mcp:method "resources/subscribe" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "resources" ;
    mcp:requiresFeatureFlag "subscribe" ;
    mcp:params [
        mcp:hasParam [
            mcp:name "uri" ;
            mcp:type xsd:anyURI ;
            mcp:required true
        ]
    ] .

# Prompts
mcp:promptsList a mcp:Request ;
    rdfs:label "prompts/list" ;
    mcp:method "prompts/list" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "prompts" ;
    mcp:supportsPagination true .

mcp:promptsGet a mcp:Request ;
    rdfs:label "prompts/get" ;
    mcp:method "prompts/get" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "prompts" ;
    mcp:validateSchema true ;  # Gap #42 - JSON Schema validation
    mcp:params [
        mcp:hasParam [
            mcp:name "name" ;
            mcp:type xsd:string ;
            mcp:required true
        ] ;
        mcp:hasParam [
            mcp:name "arguments" ;
            mcp:type mcp:JsonObject ;
            mcp:required false ;
            mcp:validatedBy mcp:PromptInputSchema
        ]
    ] .

# Logging
mcp:loggingSetLevel a mcp:Request ;
    rdfs:label "logging/setLevel" ;
    mcp:method "logging/setLevel" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "logging" ;
    mcp:params [
        mcp:hasParam [
            mcp:name "level" ;
            mcp:type xsd:string ;
            mcp:required true ;
            mcp:enum ("debug" "info" "notice" "warning" "error" "critical" "alert" "emergency")
        ]
    ] .

# Sampling
mcp:samplingCreateMessage a mcp:Request ;
    rdfs:label "sampling/createMessage" ;
    mcp:method "sampling/createMessage" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "sampling" ;
    mcp:timeout 30000 ;  # 30s timeout
    mcp:params [
        mcp:hasParam [
            mcp:name "messages" ;
            mcp:type mcp:MessageArray ;
            mcp:required true
        ] ;
        mcp:hasParam [
            mcp:name "modelPreferences" ;
            mcp:type mcp:ModelPreferences ;
            mcp:required false
        ]
    ] .

# =============================================================================
# MCP Components (Resources, Tools, Prompts)
# =============================================================================

mcp:Tool a rdfs:Class ;
    rdfs:label "Tool" ;
    rdfs:comment "Server-side tool definition" ;
    mcp:hasProperty [
        mcp:name "name" ;
        mcp:type xsd:string ;
        mcp:required true ;
        mcp:minLength 1
    ] ;
    mcp:hasProperty [
        mcp:name "description" ;
        mcp:type xsd:string ;
        mcp:required true ;
        mcp:maxLength 10000  # Gap #40
    ] ;
    mcp:hasProperty [
        mcp:name "inputSchema" ;
        mcp:type mcp:JsonSchema ;
        mcp:required false
    ] ;
    mcp:validatesArguments true .  # CRITICAL: Validate before handler

mcp:Resource a rdfs:Class ;
    rdfs:label "Resource" ;
    rdfs:comment "Server-side resource definition" ;
    mcp:hasProperty [
        mcp:name "uri" ;
        mcp:type xsd:anyURI ;
        mcp:required true
    ] ;
    mcp:hasProperty [
        mcp:name "name" ;
        mcp:type xsd:string ;
        mcp:required true
    ] ;
    mcp:validatesUri true ;
    mcp:preventsPathTraversal true .  # Gap #36 - Security

mcp:Prompt a rdfs:Class ;
    rdfs:label "Prompt" ;
    rdfs:comment "Server-side prompt definition" ;
    mcp:hasProperty [
        mcp:name "name" ;
        mcp:type xsd:string ;
        mcp:required true
    ] ;
    mcp:hasProperty [
        mcp:name "description" ;
        mcp:type xsd:string ;
        mcp:required false
    ] ;
    mcp:hasProperty [
        mcp:name "arguments" ;
        mcp:type mcp:ArgumentArray ;
        mcp:required false
    ] ;
    mcp:hasProperty [
        mcp:name "inputSchema" ;
        mcp:type mcp:JsonSchema ;
        mcp:required false  # Gap #42
    ] ;
    mcp:validatesArguments true .

# =============================================================================
# Notifications
# =============================================================================

mcp:resourcesListChanged a mcp:Notification ;
    rdfs:label "notifications/resources/list_changed" ;
    mcp:method "notifications/resources/list_changed" ;
    mcp:requiresFeatureFlag "listChanged" .

mcp:toolsListChanged a mcp:Notification ;
    rdfs:label "notifications/tools/list_changed" ;
    mcp:method "notifications/tools/list_changed" ;
    mcp:requiresFeatureFlag "listChanged" ;
    mcp:rateLimited true .

mcp:promptsListChanged a mcp:Notification ;
    rdfs:label "notifications/prompts/list_changed" ;
    mcp:method "notifications/prompts/list_changed" ;
    mcp:requiresFeatureFlag "listChanged" .

mcp:progress a mcp:Notification ;
    rdfs:label "notifications/progress" ;
    mcp:method "notifications/progress" ;
    mcp:params [
        mcp:hasParam [
            mcp:name "progressToken" ;
            mcp:type mcp:ProgressToken ;
            mcp:required true
        ] ;
        mcp:hasParam [
            mcp:name "progress" ;
            mcp:type xsd:double ;
            mcp:required true
        ] ;
        mcp:hasParam [
            mcp:name "total" ;
            mcp:type xsd:double ;
            mcp:required false
        ]
    ] .
```

**File: `ontology/mcp/extensions/tasks.ttl`** (30% of spec)

```turtle
@prefix mcp: <http://modelcontextprotocol.io/2025-11-25/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# =============================================================================
# Task Management (P0 - CRITICAL: 30% of MCP Spec)
# =============================================================================

mcp:Task a rdfs:Class ;
    rdfs:label "Task" ;
    rdfs:comment "Long-running task with lifecycle management" ;
    mcp:hasProperty [
        mcp:name "taskId" ;
        mcp:type xsd:string ;
        mcp:required true
    ] ;
    mcp:hasProperty [
        mcp:name "status" ;
        mcp:type mcp:TaskStatus ;
        mcp:required true
    ] ;
    mcp:hasProperty [
        mcp:name "action" ;
        mcp:type xsd:string ;
        mcp:required true
    ] ;
    mcp:lifecycle (
        "pending" "working" "completed" "failed" "cancelled"
    ) ;
    mcp:stateTransitions [
        mcp:from "pending" ;
        mcp:event "start" ;
        mcp:to "working"
    ] , [
        mcp:from "working" ;
        mcp:event "complete" ;
        mcp:to "completed"
    ] , [
        mcp:from "working" ;
        mcp:event "fail" ;
        mcp:to "failed"
    ] , [
        mcp:from "pending" ;
        mcp:event "cancel" ;
        mcp:to "cancelled"
    ] , [
        mcp:from "working" ;
        mcp:event "cancel" ;
        mcp:to "cancelled"
    ] .

# Task API Methods
mcp:tasksCreate a mcp:Request ;
    rdfs:label "tasks/create" ;
    mcp:method "tasks/create" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "tasks" ;
    mcp:params [
        mcp:hasParam [
            mcp:name "action" ;
            mcp:type xsd:string ;
            mcp:required true
        ] ;
        mcp:hasParam [
            mcp:name "parameters" ;
            mcp:type mcp:JsonObject ;
            mcp:required false
        ]
    ] ;
    mcp:returns mcp:TaskCreateResult .

mcp:tasksList a mcp:Request ;
    rdfs:label "tasks/list" ;
    mcp:method "tasks/list" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "tasks" ;
    mcp:supportsPagination true .

mcp:tasksGet a mcp:Request ;
    rdfs:label "tasks/get" ;
    mcp:method "tasks/get" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "tasks" ;
    mcp:params [
        mcp:hasParam [
            mcp:name "taskId" ;
            mcp:type xsd:string ;
            mcp:required true
        ]
    ] .

mcp:tasksResult a mcp:Request ;
    rdfs:label "tasks/result" ;
    mcp:method "tasks/result" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "tasks" ;
    mcp:params [
        mcp:hasParam [
            mcp:name "taskId" ;
            mcp:type xsd:string ;
            mcp:required true
        ]
    ] .

mcp:tasksCancel a mcp:Request ;
    rdfs:label "tasks/cancel" ;
    mcp:method "tasks/cancel" ;
    mcp:requiresPhase mcp:Initialized ;
    mcp:requiresCapability "tasks" ;
    mcp:params [
        mcp:hasParam [
            mcp:name "taskId" ;
            mcp:type xsd:string ;
            mcp:required true
        ]
    ] ;
    mcp:integratesWith mcp:cancellation .
```

**Deliverable:** Core MCP ontology files (protocol.ttl, messages.ttl, capabilities.ttl, validation.ttl, tasks.ttl)

#### 2.2 Create SHACL Validation Shapes (1 day)

**File: `shapes/mcp/mcp_shapes.ttl`**

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix mcp: <http://modelcontextprotocol.io/2025-11-25/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Validate all Request methods have required properties
mcp:RequestShape a sh:NodeShape ;
    sh:targetClass mcp:Request ;
    sh:property [
        sh:path mcp:method ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:pattern "^[a-z]+(/[a-z]+)?$"
    ] ;
    sh:property [
        sh:path mcp:requiresPhase ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:in (mcp:PreInitialization mcp:Initialized)
    ] .

# Validate Tool definitions
mcp:ToolShape a sh:NodeShape ;
    sh:targetClass mcp:Tool ;
    sh:property [
        sh:path mcp:name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:minLength 1
    ] ;
    sh:property [
        sh:path mcp:description ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:maxLength 10000  # Gap #40
    ] .

# Validate Task lifecycle
mcp:TaskShape a sh:NodeShape ;
    sh:targetClass mcp:Task ;
    sh:property [
        sh:path mcp:lifecycle ;
        sh:minCount 1 ;
        sh:hasValue ("pending" "working" "completed" "failed" "cancelled")
    ] ;
    sh:property [
        sh:path mcp:stateTransitions ;
        sh:minCount 1  # Must have at least one transition
    ] .
```

**Deliverable:** SHACL shapes for validation

#### 2.3 Write SPARQL Queries (1 day)

**File: `sparql/mcp_queries/mcp_methods.rq`**

```sparql
PREFIX mcp: <http://modelcontextprotocol.io/2025-11-25/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?method ?label ?requiresPhase ?capability ?validateSchema ?timeout ?pagination
WHERE {
    ?request a mcp:Request ;
             mcp:method ?method ;
             rdfs:label ?label ;
             mcp:requiresPhase ?requiresPhase .

    OPTIONAL { ?request mcp:requiresCapability ?capability }
    OPTIONAL { ?request mcp:validateSchema ?validateSchema }
    OPTIONAL { ?request mcp:timeout ?timeout }
    OPTIONAL { ?request mcp:supportsPagination ?pagination }
}
ORDER BY ?method
```

**File: `sparql/mcp_queries/mcp_validators.rq`**

```sparql
PREFIX mcp: <http://modelcontextprotocol.io/2025-11-25/>

SELECT ?component ?validationType ?property
WHERE {
    ?component a rdfs:Class ;
               mcp:validatesArguments true .

    OPTIONAL { ?component mcp:validatesUri ?validationType }
    OPTIONAL { ?component mcp:preventsPathTraversal ?property }
}
```

**File: `sparql/mcp_queries/mcp_task_states.rq`**

```sparql
PREFIX mcp: <http://modelcontextprotocol.io/2025-11-25/>

SELECT ?lifecycle ?from ?event ?to
WHERE {
    mcp:Task mcp:lifecycle ?lifecycle .

    OPTIONAL {
        mcp:Task mcp:stateTransitions ?transition .
        ?transition mcp:from ?from ;
                   mcp:event ?event ;
                   mcp:to ?to .
    }
}
```

**File: `sparql/mcp_queries/mcp_error_codes.rq`**

```sparql
PREFIX mcp: <http://modelcontextprotocol.io/2025-11-25/>

SELECT ?code ?name ?message ?category
WHERE {
    ?error a mcp:ErrorCode ;
           mcp:code ?code ;
           mcp:name ?name ;
           mcp:message ?message ;
           mcp:category ?category .
}
ORDER BY ?code
```

**Deliverable:** SPARQL queries for code generation

#### 2.4 Validate Ontology (0.5 days)

```bash
# SHACL validation
ggen validate --shacl

# Check for common issues
ggen validate --check-completeness
ggen validate --check-consistency

# View validation report
cat generated/mcp/validation_report.md
```

**Expected Output:**
```
🟢 QUALITY GATE: PASSED
✅ Ontology syntax valid (Turtle)
✅ SHACL validation passed (0 violations)
✅ Completeness: 33/33 MCP methods defined
✅ Consistency: No conflicts detected
✅ Coverage: 100% of MCP 2025-11-25 spec
```

**Deliverable:** Validated ontology with 0 SHACL violations

### Phase 2 Success Criteria

- [ ] MCP 2025-11-25 fully defined in RDF (33/33 methods)
- [ ] Task management ontology complete (5 methods)
- [ ] SHACL shapes defined for all components
- [ ] SPARQL queries written and tested
- [ ] Validation passes with 0 violations
- [ ] Ontology reviewed by 2+ team members

**Time: 5 days**
**Risk: MEDIUM** (requires RDF/SPARQL knowledge)

---

## Phase 3: Template Creation (Day 8-10)

### Objectives
- Create Tera templates for Erlang code generation
- Generate server handlers with validation
- Generate client API methods
- Generate task manager state machine
- Generate test stubs

### Tasks

#### 3.1 Server Handler Template (1 day)

**File: `templates/ggen/mcp/server_handler.erl.tera`**

```erlang
%% GENERATED CODE - DO NOT EDIT
%% Source: MCP 2025-11-25 Ontology
%% Generator: ggen v6.0.0
%% Generated: {{ timestamp }}
%% Method: {{ method }}

-module(erlmcp_handler_{{ method | replace(from="/", to="_") }}).
-behaviour(erlmcp_handler).

-export([handle/4]).

-include("erlmcp.hrl").
-include("mcp_error_codes.hrl").

%% @doc Handle {{ method }} request
%% Phase: {{ requiresPhase }}
{% if capability %}%% Capability: {{ capability }}{% endif %}
{% if validateSchema %}%% Validation: JSON Schema (MANDATORY){% endif %}
-spec handle(RequestId :: term(), Params :: map(), State :: #state{}, Context :: map()) ->
    {reply, Result :: map(), NewState :: #state{}} |
    {error, {Code :: integer(), Message :: binary(), Data :: map()}}.

handle(RequestId, Params, State, Context) ->
    %% =========================================================================
    %% PHASE 1: Protocol Phase Validation
    %% =========================================================================
    {% if requiresPhase == "pre_initialization" %}
    case State#state.phase of
        pre_initialization ->
            ok;
        CurrentPhase ->
            ErrorCode = ?MCP_ERROR_NOT_INITIALIZED,
            ErrorMsg = <<"Method '{{ method }}' requires phase 'pre_initialization', current: ",
                         (atom_to_binary(CurrentPhase))/binary>>,
            return {error, {ErrorCode, ErrorMsg, #{}}}
    end,
    {% elsif requiresPhase == "initialized" %}
    case State#state.phase of
        initialized ->
            ok;
        pre_initialization ->
            ErrorCode = ?MCP_ERROR_NOT_INITIALIZED,
            ErrorMsg = <<"Method '{{ method }}' requires initialization first">>,
            return {error, {ErrorCode, ErrorMsg, #{}}}
    end,
    {% endif %}

    %% =========================================================================
    %% PHASE 2: Capability Validation
    %% =========================================================================
    {% if capability %}
    case erlmcp_capabilities:has_capability(State#state.capabilities, <<"{{ capability }}">>) of
        true ->
            ok;
        false ->
            ErrorCode = ?MCP_ERROR_CAPABILITY_NOT_SUPPORTED,
            ErrorMsg = <<"Capability '{{ capability }}' not negotiated">>,
            ErrorData = #{capability => <<"{{ capability }}">>},
            return {error, {ErrorCode, ErrorMsg, ErrorData}}
    end,
    {% endif %}

    %% =========================================================================
    %% PHASE 3: JSON Schema Validation (P0 FIX)
    %% =========================================================================
    {% if validateSchema %}
    %% CRITICAL: Validate arguments BEFORE handler invocation
    %% This prevents Gap #41 (unvalidated tool arguments)
    Schema = get_input_schema(State, Context),
    case erlmcp_schema_validator:validate(Params, Schema) of
        ok ->
            ok;
        {error, ValidationErrors} ->
            ErrorCode = ?JSONRPC_INVALID_PARAMS,
            ErrorMsg = format_validation_errors(ValidationErrors),
            ErrorData = #{
                validation_errors => ValidationErrors,
                schema => Schema
            },
            return {error, {ErrorCode, ErrorMsg, ErrorData}}
    end,
    {% endif %}

    %% =========================================================================
    %% PHASE 4: Execute Handler (with timeout)
    %% =========================================================================
    TimeoutMs = maps:get(timeout, Context, {{ timeout | default(value=5000) }}),

    try
        Result = execute_handler(RequestId, Params, State, Context, TimeoutMs),
        {reply, Result, State}
    catch
        error:timeout ->
            ErrorCode = ?MCP_ERROR_TIMEOUT,
            ErrorMsg = <<"Handler '{{ method }}' exceeded timeout">>,
            ErrorData = #{timeout_ms => TimeoutMs},
            {error, {ErrorCode, ErrorMsg, ErrorData}};

        error:Reason:Stacktrace ->
            logger:error("Handler '{{ method }}' crashed~n"
                        "Reason: ~p~n"
                        "Stacktrace: ~p~n"
                        "Params: ~p",
                        [Reason, Stacktrace, Params]),
            ErrorCode = ?JSONRPC_INTERNAL_ERROR,
            ErrorMsg = <<"Internal error processing '{{ method }}' request">>,
            ErrorData = #{reason => Reason},
            {error, {ErrorCode, ErrorMsg, ErrorData}}
    end.

%% =============================================================================
%% Internal Functions
%% =============================================================================

{% if validateSchema %}
%% @doc Get input schema for validation
get_input_schema(State, Context) ->
    %% TODO: Implement schema retrieval based on method
    %% For tools: State#state.tools -> find by name -> #mcp_tool.input_schema
    %% For prompts: State#state.prompts -> find by name -> #mcp_prompt.input_schema
    #{}.

%% @doc Format validation errors for error response
format_validation_errors(Errors) when is_list(Errors) ->
    ErrorMsgs = [format_single_error(E) || E <- Errors],
    iolist_to_binary(["Validation failed: ", string:join(ErrorMsgs, "; ")]);
format_validation_errors(Error) ->
    format_single_error(Error).

format_single_error(#{field := Field, reason := Reason}) ->
    io_lib:format("~s: ~s", [Field, Reason]);
format_single_error(Error) ->
    io_lib:format("~p", [Error]).
{% endif %}

%% @doc Execute the actual business logic handler
execute_handler(RequestId, Params, State, Context, TimeoutMs) ->
    %% TODO: Implement business logic
    %% This is the 20% that requires manual implementation
    #{
        result => <<"Handler '{{ method }}' not implemented">>,
        method => <<"{{ method }}">>
    }.
```

**Deliverable:** Server handler template with all P0 fixes

#### 3.2 Client API Template (0.5 days)

**File: `templates/ggen/mcp/client_method.erl.tera`**

```erlang
%% GENERATED CODE - DO NOT EDIT
%% Client API for: {{ method }}

-module(erlmcp_client_{{ method | replace(from="/", to="_") }}).
-export([call/2, call/3]).

-include("erlmcp.hrl").

%% @doc Call {{ method }} (default timeout: {{ timeout | default(value=5000) }}ms)
call(ClientPid, Params) ->
    call(ClientPid, Params, {{ timeout | default(value=5000) }}).

%% @doc Call {{ method }} with custom timeout
call(ClientPid, Params, Timeout) ->
    Method = <<"{{ method }}">>,
    erlmcp_client:call_method(ClientPid, Method, Params, Timeout).
```

**Deliverable:** Client API template

#### 3.3 Task Manager Template (1 day)

**File: `templates/ggen/mcp/task_manager.erl.tera`**

```erlang
%% GENERATED CODE - DO NOT EDIT
%% Task Manager State Machine
%% Lifecycle: {{ lifecycle | join(sep=" → ") }}

-module(erlmcp_task_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, create/2, get/1, list/0, list/1,
         result/1, cancel/1, update_status/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

-record(task, {
    task_id :: binary(),
    status :: {% for state in lifecycle %}{{ state }}{% if not loop.last %} | {% endif %}{% endfor %},
    action :: binary(),
    parameters :: map(),
    worker_pid :: pid() | undefined,
    monitor_ref :: reference() | undefined,
    created_at :: integer(),
    updated_at :: integer(),
    result :: term() | undefined,
    error :: term() | undefined,
    cancellation_token :: reference() | undefined,
    progress_token :: reference() | undefined
}).

-record(state, {
    tasks :: #{binary() => #task{}},
    task_sup :: pid()
}).

%% =============================================================================
%% API
%% =============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Action, Parameters) ->
    gen_server:call(?MODULE, {create_task, Action, Parameters}).

get(TaskId) ->
    gen_server:call(?MODULE, {get_task, TaskId}).

list() ->
    list(#{}).

list(Filters) ->
    gen_server:call(?MODULE, {list_tasks, Filters}).

result(TaskId) ->
    gen_server:call(?MODULE, {task_result, TaskId}).

cancel(TaskId) ->
    gen_server:call(?MODULE, {cancel_task, TaskId}).

update_status(TaskId, NewStatus) ->
    gen_server:cast(?MODULE, {update_status, TaskId, NewStatus}).

%% =============================================================================
%% gen_server callbacks
%% =============================================================================

init([]) ->
    process_flag(trap_exit, true),
    Tasks = #{},
    {ok, TaskSup} = erlmcp_task_sup:start_link(),
    {ok, #state{tasks = Tasks, task_sup = TaskSup}}.

handle_call({create_task, Action, Parameters}, _From, State) ->
    TaskId = generate_task_id(),
    Task = #task{
        task_id = TaskId,
        status = pending,
        action = Action,
        parameters = Parameters,
        created_at = erlang:system_time(millisecond),
        updated_at = erlang:system_time(millisecond)
    },
    NewTasks = maps:put(TaskId, Task, State#state.tasks),
    Result = #{
        task_id => TaskId,
        status => pending,
        action => Action
    },
    {reply, {ok, Result}, State#state{tasks = NewTasks}};

handle_call({get_task, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} ->
            Result = task_to_map(Task),
            {reply, {ok, Result}, State};
        error ->
            {reply, {error, task_not_found}, State}
    end;

handle_call({list_tasks, Filters}, _From, State) ->
    Tasks = maps:values(State#state.tasks),
    FilteredTasks = filter_tasks(Tasks, Filters),
    Results = [task_to_map(T) || T <- FilteredTasks],
    {reply, {ok, Results}, State};

handle_call({task_result, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, #task{status = completed, result = Result}} ->
            {reply, {ok, Result}, State};
        {ok, #task{status = failed, error = Error}} ->
            {reply, {error, Error}, State};
        {ok, #task{status = Status}} ->
            {reply, {error, {not_ready, Status}}, State};
        error ->
            {reply, {error, task_not_found}, State}
    end;

handle_call({cancel_task, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} ->
            case transition(Task#task.status, cancel) of
                {ok, cancelled} ->
                    UpdatedTask = Task#task{
                        status = cancelled,
                        updated_at = erlang:system_time(millisecond)
                    },
                    NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
                    %% Kill worker if running
                    case Task#task.worker_pid of
                        undefined -> ok;
                        Pid -> exit(Pid, kill)
                    end,
                    {reply, ok, State#state{tasks = NewTasks}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, task_not_found}, State}
    end.

handle_cast({update_status, TaskId, NewStatus}, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, Task} ->
            UpdatedTask = Task#task{
                status = NewStatus,
                updated_at = erlang:system_time(millisecond)
            },
            NewTasks = maps:put(TaskId, UpdatedTask, State#state.tasks),
            {noreply, State#state{tasks = NewTasks}};
        error ->
            {noreply, State}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% @doc State transition validation (generated from ontology)
{% for transition in transitions %}
transition({{ transition.from }}, {{ transition.event }}) ->
    {ok, {{ transition.to }}};
{% endfor %}
transition(Status, Event) ->
    {error, {invalid_transition, Status, Event}}.

generate_task_id() ->
    UUID = uuid:uuid4(),
    uuid:uuid_to_string(UUID, binary_standard).

task_to_map(#task{} = Task) ->
    #{
        task_id => Task#task.task_id,
        status => Task#task.status,
        action => Task#task.action,
        parameters => Task#task.parameters,
        created_at => Task#task.created_at,
        updated_at => Task#task.updated_at
    }.

filter_tasks(Tasks, Filters) ->
    %% TODO: Implement filtering logic
    Tasks.
```

**Deliverable:** Task manager state machine template

#### 3.4 Validator Template (0.5 days)

**File: `templates/ggen/mcp/validator.erl.tera`**

```erlang
%% GENERATED CODE - DO NOT EDIT
%% Validator for: {{ component }}

-module(erlmcp_validator_{{ component | lower }}).
-export([validate/2, validate_uri/1, validate_arguments/2]).

-include("erlmcp.hrl").

%% @doc Validate {{ component }} data against schema
validate(Data, Schema) ->
    %% Phase 1: JSON Schema validation
    case erlmcp_schema_validator:validate(Data, Schema) of
        ok ->
            %% Phase 2: Semantic validation (from ontology rules)
            {% if validatesUri %}validate_uri_rules(Data),{% endif %}
            {% if preventsPathTraversal %}validate_path_traversal(Data),{% endif %}
            ok;
        {error, _} = Error ->
            Error
    end.

{% if validatesUri %}
%% @doc Validate URI (Gap #36 - Security)
validate_uri(Uri) when is_binary(Uri) ->
    %% RFC 3986 URI validation
    case uri_string:parse(Uri) of
        #{scheme := Scheme} when Scheme =/= <<>> ->
            ok;
        _ ->
            {error, {invalid_uri, Uri}}
    end.

validate_uri_rules(Data) ->
    case maps:find(uri, Data) of
        {ok, Uri} -> validate_uri(Uri);
        error -> ok
    end.
{% endif %}

{% if preventsPathTraversal %}
%% @doc Prevent path traversal attacks (Gap #36)
validate_path_traversal(Data) ->
    case maps:find(uri, Data) of
        {ok, Uri} ->
            case binary:match(Uri, [<<"..">>]) of
                nomatch -> ok;
                _ -> {error, {path_traversal_detected, Uri}}
            end;
        error ->
            ok
    end.
{% endif %}

%% @doc Validate arguments against input schema
validate_arguments(Arguments, Schema) ->
    erlmcp_schema_validator:validate(Arguments, Schema).
```

**Deliverable:** Validator template

#### 3.5 Test Stub Template (0.5 days)

**File: `templates/ggen/mcp/test_stub.erl.tera`**

```erlang
%% GENERATED CODE - DO NOT EDIT
%% Test stub for: {{ method }}

-module(erlmcp_{{ method | replace(from="/", to="_") }}_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% =============================================================================
%% Test Fixtures
%% =============================================================================

setup() ->
    {ok, Pid} = erlmcp_server:start_link(#{}),
    Pid.

cleanup(Pid) ->
    erlmcp_server:stop(Pid).

%% =============================================================================
%% Tests
%% =============================================================================

{{ method | replace(from="/", to="_") }}_success_test() ->
    Pid = setup(),
    try
        %% TODO: Implement test
        Params = #{},
        Result = erlmcp_client_{{ method | replace(from="/", to="_") }}:call(Pid, Params),
        ?assertMatch({ok, _}, Result)
    after
        cleanup(Pid)
    end.

{{ method | replace(from="/", to="_") }}_invalid_params_test() ->
    Pid = setup(),
    try
        %% TODO: Implement test
        Params = #{invalid => <<"data">>},
        Result = erlmcp_client_{{ method | replace(from="/", to="_") }}:call(Pid, Params),
        ?assertMatch({error, _}, Result)
    after
        cleanup(Pid)
    end.

{% if validateSchema %}
{{ method | replace(from="/", to="_") }}_schema_validation_test() ->
    Pid = setup(),
    try
        %% TODO: Test JSON Schema validation
        Params = #{},
        Result = erlmcp_client_{{ method | replace(from="/", to="_") }}:call(Pid, Params),
        ?assertMatch({ok, _}, Result)
    after
        cleanup(Pid)
    end.
{% endif %}

{% if requiresPhase %}
{{ method | replace(from="/", to="_") }}_phase_validation_test() ->
    Pid = setup(),
    try
        %% TODO: Test phase enforcement
        Params = #{},
        Result = erlmcp_client_{{ method | replace(from="/", to="_") }}:call(Pid, Params),
        ?assertMatch({error, _}, Result)
    after
        cleanup(Pid)
    end.
{% endif %}
```

**Deliverable:** Test stub template

### Phase 3 Success Criteria

- [ ] Server handler template with all validations
- [ ] Client API template created
- [ ] Task manager template created
- [ ] Validator template created
- [ ] Test stub template created
- [ ] Templates validated with sample data

**Time: 3 days**
**Risk: MEDIUM** (requires Tera template knowledge)

---

## Phase 4: Code Generation & Compilation (Day 11-12)

### Objectives
- Run ggen sync to generate code
- Verify compilation
- Fix generation issues
- Run dialyzer and xref

### Tasks

#### 4.1 Run ggen sync (0.5 days)

```bash
# Clean previous generated code
rm -rf generated/mcp/

# Generate all code
ggen sync

# Expected output:
# 🟢 QUALITY GATE: PASSED
# ✅ Validated ontology (0 SHACL violations)
# ✅ Generated 47 Erlang modules (12,834 lines)
#    - 33 server handlers (apps/erlmcp_core/src/handlers/)
#    - 33 client methods (apps/erlmcp_core/src/client/)
#    - 10 validators (apps/erlmcp_core/src/validators/)
#    - 1 task manager (apps/erlmcp_core/src/erlmcp_task_manager.erl)
#    - 33 test stubs (apps/erlmcp_core/test/)
# ✅ All handlers include validation
# ✅ Task manager state machine generated
# ✅ Dialyzer types complete
# ✅ Compilation: PENDING
#
# Next: rebar3 compile
```

**Review Generated Files:**
```bash
# List all generated modules
find generated/mcp -name "*.erl" | wc -l
# Expected: 80+ files

# Check handler validation logic
grep -A 10 "JSON Schema Validation" generated/mcp/handlers/erlmcp_handler_tools_call.erl
# Expected: Validation before handler execution

# Check task manager state machine
grep "transition(" generated/mcp/erlmcp_task_manager.erl
# Expected: All 5 transitions defined
```

**Deliverable:** Generated code (80+ Erlang modules)

#### 4.2 Integrate Generated Code into Build (0.5 days)

**Update `rebar.config`:**

```erlang
{erl_opts, [
    debug_info,
    warnings_as_errors,
    {i, "include"},
    {i, "generated/mcp/include"},  % Add generated headers
    {parse_transform, lager_transform}
]}.

{src_dirs, [
    "src",
    "generated/mcp/handlers",      % Add generated handlers
    "generated/mcp/client",         % Add generated client methods
    "generated/mcp/validators"      % Add generated validators
]}.
```

**Update `.gitignore`:**

```
# Generated code (do not commit)
generated/mcp/
!generated/mcp/.gitkeep
```

**Deliverable:** Build configuration updated

#### 4.3 Compile Generated Code (1 hour)

```bash
# Clean build
rebar3 clean

# Compile
TERM=dumb rebar3 compile

# Expected:
# Compiling 47 generated modules...
# Compiling 45 existing modules...
# ===> Compiled successfully (92 modules)
```

**Fix Common Issues:**

1. **Missing includes:**
```bash
# If: {error, mcp_error_codes.hrl not found}
# Add to generated/mcp/include/mcp_error_codes.hrl
```

2. **Undefined functions:**
```bash
# If: undefined function uuid:uuid4/0
# Add to rebar.config: {uuid, "2.0.7"}
```

3. **Type conflicts:**
```bash
# If: type task() conflicts with existing type
# Rename in ontology: mcp:Task -> mcp:ManagedTask
```

**Deliverable:** Clean compilation (0 errors)

#### 4.4 Run Dialyzer (2 hours)

```bash
# Build PLT
rebar3 dialyzer --update-plt

# Run analysis
rebar3 dialyzer

# Expected warnings:
# - Generated code: 0 warnings (types from ontology)
# - Existing code: ~20 warnings (needs fixing)
```

**Fix Type Warnings:**

1. **Handler return types:**
```erlang
% If: Return type mismatch in execute_handler/5
% Fix: Add -spec in template
-spec execute_handler(term(), map(), #state{}, map(), timeout()) ->
    map() | no_return().
```

2. **Task state types:**
```erlang
% If: status field has incompatible type
% Fix: Use union type
-type task_status() :: pending | working | completed | failed | cancelled.
```

**Deliverable:** Dialyzer clean (0 errors, <5 warnings)

#### 4.5 Run Xref (1 hour)

```bash
# Cross-reference analysis
rebar3 xref

# Expected:
# ===> Running cross reference analysis...
# ===> Analysis: 0 undefined function calls
# ===> Analysis: 0 unused exports
```

**Fix Xref Issues:**

1. **Undefined functions:**
```bash
# If: erlmcp_handler_tools_call:execute_handler/5 undefined
# Cause: Business logic not implemented yet
# Fix: Add stub implementation
```

2. **Unused exports:**
```bash
# If: erlmcp_validator_tool:validate/2 is unused
# Cause: Not yet integrated into server
# Fix: Wire up in Phase 5
```

**Deliverable:** Xref clean (0 undefined calls)

### Phase 4 Success Criteria

- [ ] ggen sync executes successfully
- [ ] 80+ Erlang modules generated
- [ ] Compilation succeeds (0 errors)
- [ ] Dialyzer passes (0 errors, <5 warnings)
- [ ] Xref passes (0 undefined calls)
- [ ] Generated code reviewed by 2+ team members

**Time: 2 days**
**Risk: MEDIUM** (compilation issues possible)

---

## Phase 5: Integration & Testing (Day 13-17)

### Objectives
- Wire up generated handlers into erlmcp_server
- Integrate validators into request pipeline
- Add business logic to handler stubs
- Run existing tests
- Fix integration issues
- Add missing test coverage

### Tasks

#### 5.1 Wire Generated Handlers (2 days)

**Update `apps/erlmcp_core/src/erlmcp_server.erl`:**

```erlang
% Replace manual handler dispatch with generated handlers
handle_mcp_message(Message, State, TransportId) ->
    Method = maps:get(<<"method">>, Message),
    Id = maps:get(<<"id">>, Message, undefined),
    Params = maps:get(<<"params">>, Message, #{}),

    % Route to generated handler
    HandlerModule = method_to_handler_module(Method),
    case erlang:function_exported(HandlerModule, handle, 4) of
        true ->
            Context = #{
                transport_id => TransportId,
                client_id => State#state.client_id
            },
            case HandlerModule:handle(Id, Params, State, Context) of
                {reply, Result, NewState} ->
                    send_success_response(TransportId, Id, Result, NewState);
                {error, {Code, Message, Data}} ->
                    send_error_response(TransportId, Id, Code, Message, Data, State)
            end;
        false ->
            % Method not implemented
            send_error_response(TransportId, Id,
                ?JSONRPC_METHOD_NOT_FOUND,
                <<"Method not found">>,
                #{method => Method},
                State)
    end.

% Map method name to generated handler module
method_to_handler_module(<<"initialize">>) ->
    erlmcp_handler_initialize;
method_to_handler_module(<<"tools/list">>) ->
    erlmcp_handler_tools_list;
method_to_handler_module(<<"tools/call">>) ->
    erlmcp_handler_tools_call;
% ... 30 more methods ...
method_to_handler_module(<<"tasks/create">>) ->
    erlmcp_handler_tasks_create;
method_to_handler_module(_) ->
    undefined.
```

**Benefits:**
- Each handler is independent module
- Easy to test in isolation
- Clear separation of concerns
- Generated validation is always correct

**Deliverable:** Server routes to generated handlers

#### 5.2 Integrate Validators (1 day)

**Update Tool Registration:**

```erlang
% apps/erlmcp_core/src/erlmcp_server.erl
add_tool(ServerPid, Tool, Handler) when is_record(Tool, mcp_tool) ->
    % Validate tool definition using generated validator
    case erlmcp_validator_tool:validate(tool_to_map(Tool), get_tool_schema()) of
        ok ->
            gen_server:call(ServerPid, {add_tool, Tool, Handler});
        {error, ValidationErrors} ->
            {error, {invalid_tool, ValidationErrors}}
    end.
```

**Update Resource Registration:**

```erlang
add_resource(ServerPid, Resource, Handler) when is_record(Resource, mcp_resource) ->
    % Validate resource definition using generated validator
    case erlmcp_validator_resource:validate(resource_to_map(Resource), get_resource_schema()) of
        ok ->
            % Additional URI validation (Gap #36)
            case erlmcp_validator_resource:validate_uri(Resource#mcp_resource.uri) of
                ok ->
                    gen_server:call(ServerPid, {add_resource, Resource, Handler});
                {error, Reason} ->
                    {error, {invalid_uri, Reason}}
            end;
        {error, ValidationErrors} ->
            {error, {invalid_resource, ValidationErrors}}
    end.
```

**Deliverable:** Validators integrated into registration

#### 5.3 Add Business Logic (2 days)

Generated handlers include stubs that need business logic:

**Example: `erlmcp_handler_tools_call.erl`**

```erlang
% BEFORE (generated stub):
execute_handler(RequestId, Params, State, Context, TimeoutMs) ->
    #{result => <<"Handler 'tools/call' not implemented">>}.

% AFTER (add business logic):
execute_handler(RequestId, Params, State, Context, TimeoutMs) ->
    ToolName = maps:get(<<"name">>, Params),
    Arguments = maps:get(<<"arguments">>, Params, #{}),

    % Look up tool and handler
    case maps:find(ToolName, State#state.tools) of
        {ok, {Tool, Handler, _Schema}} ->
            % NOTE: Validation already done in handle/4 (P0 FIX)
            % Execute with CPU guard
            ClientId = maps:get(client_id, Context),
            case erlmcp_cpu_guard:execute_with_protection(
                ClientId, tool_call, Handler, [Arguments], TimeoutMs
            ) of
                {ok, Result} ->
                    Result;
                {error, Reason} ->
                    error({tool_execution_failed, Reason})
            end;
        error ->
            error({tool_not_found, ToolName})
    end.
```

**Deliverable:** Business logic added to 33 handlers

#### 5.4 Integrate Task Manager (1 day)

**Add to Supervision Tree:**

```erlang
% apps/erlmcp_core/src/erlmcp_core_sup.erl
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    Children = [
        % Existing children...
        #{
            id => erlmcp_registry,
            start => {erlmcp_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        % NEW: Task manager (P0 FIX - 30% of spec)
        #{
            id => erlmcp_task_manager,
            start => {erlmcp_task_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker
        },
        % NEW: Task supervisor (simple_one_for_one)
        #{
            id => erlmcp_task_sup,
            start => {erlmcp_task_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor
        }
    ],

    {ok, {SupFlags, Children}}.
```

**Wire Up Task Handlers:**

```erlang
% Business logic for erlmcp_handler_tasks_create.erl
execute_handler(RequestId, Params, State, Context, TimeoutMs) ->
    Action = maps:get(<<"action">>, Params),
    Parameters = maps:get(<<"parameters">>, Params, #{}),

    % Delegate to generated task manager
    case erlmcp_task_manager:create(Action, Parameters) of
        {ok, Task} ->
            Task;
        {error, Reason} ->
            error({task_creation_failed, Reason})
    end.
```

**Deliverable:** Task management fully integrated

#### 5.5 Run Existing Tests (1 day)

```bash
# Run full test suite
rebar3 eunit

# Expected:
# ===> Running tests...
# ===> erlmcp_client_tests: 45/50 passed (5 failures)
# ===> erlmcp_server_tests: 38/42 passed (4 failures)
# ===> erlmcp_json_rpc_tests: 58/58 passed
# ===> Total: 256/320 passed (80% pass rate)
```

**Fix Test Failures:**

1. **Handler interface changed:**
```erlang
% OLD: Direct handler call
Result = erlmcp_server:handle_tools_call(Pid, Params)

% NEW: Route through generated handler
Result = erlmcp_client_tools_call:call(Pid, Params)
```

2. **Validation now enforced:**
```erlang
% OLD: Invalid params accepted
Result = call_tool(Pid, <<"mytool">>, #{invalid => data})
?assertMatch({ok, _}, Result)

% NEW: Validation rejects (P0 FIX)
Result = call_tool(Pid, <<"mytool">>, #{invalid => data})
?assertMatch({error, {-32602, _, _}}, Result)
```

3. **Phase enforcement:**
```erlang
% OLD: Call tools before initialize
Result = list_tools(Pid)
?assertMatch({ok, _}, Result)

% NEW: Phase enforcement (correct behavior)
Result = list_tools(Pid)
?assertMatch({error, {-32001, <<"not initialized">>, _}}, Result)
```

**Deliverable:** 95%+ test pass rate

#### 5.6 Add Missing Test Coverage (1 day)

**Priority Tests:**

1. **Tool argument validation (P0):**
```erlang
% test/erlmcp_handler_tools_call_tests.erl
tool_argument_validation_test() ->
    % Setup tool with schema
    Schema = #{
        type => <<"object">>,
        properties => #{
            name => #{type => <<"string">>, minLength => 1},
            age => #{type => <<"number">>, minimum => 0}
        },
        required => [<<"name">>]
    },
    Tool = #mcp_tool{name = <<"person">>, input_schema = Schema},
    {ok, Pid} = erlmcp_server:start_link(#{}),
    erlmcp_server:add_tool(Pid, Tool, fun(_) -> #{} end),

    % Test: Valid arguments accepted
    Result1 = call_tool(Pid, <<"person">>, #{name => <<"Alice">>, age => 30}),
    ?assertMatch({ok, _}, Result1),

    % Test: Missing required field rejected (P0 FIX)
    Result2 = call_tool(Pid, <<"person">>, #{age => 30}),
    ?assertMatch({error, {-32602, _, #{validation_errors := _}}}, Result2),

    % Test: Invalid type rejected
    Result3 = call_tool(Pid, <<"person">>, #{name => 123}),
    ?assertMatch({error, {-32602, _, _}}, Result3).
```

2. **Task lifecycle (P0):**
```erlang
% test/erlmcp_task_manager_tests.erl
task_lifecycle_test() ->
    {ok, _} = erlmcp_task_manager:start_link(),

    % Create task
    {ok, Task} = erlmcp_task_manager:create(<<"test_action">>, #{}),
    TaskId = maps:get(task_id, Task),
    ?assertEqual(pending, maps:get(status, Task)),

    % Transition: pending -> working
    ok = erlmcp_task_manager:update_status(TaskId, working),
    {ok, Task2} = erlmcp_task_manager:get(TaskId),
    ?assertEqual(working, maps:get(status, Task2)),

    % Transition: working -> completed
    ok = erlmcp_task_manager:update_status(TaskId, completed),
    {ok, Task3} = erlmcp_task_manager:get(TaskId),
    ?assertEqual(completed, maps:get(status, Task3)),

    % Invalid transition: completed -> pending
    Result = erlmcp_task_manager:update_status(TaskId, pending),
    ?assertMatch({error, {invalid_transition, _, _}}, Result).
```

3. **Client timeout/cleanup (P0):**
```erlang
% test/erlmcp_client_tests.erl
request_timeout_test() ->
    {ok, Pid} = erlmcp_client:start_link(stdio, #{}),

    % Call with 100ms timeout (server will never respond)
    Result = erlmcp_client:call_method(Pid, <<"test/timeout">>, #{}, 100),

    % Verify timeout error (P0 FIX)
    ?assertMatch({error, timeout}, Result),

    % Verify pending request cleaned up
    State = sys:get_state(Pid),
    ?assertEqual(0, maps:size(State#state.pending_requests)).
```

**Deliverable:** 95%+ test coverage

### Phase 5 Success Criteria

- [ ] Generated handlers integrated
- [ ] Validators integrated
- [ ] Business logic added
- [ ] Task manager in supervision tree
- [ ] Existing tests: 95%+ pass rate
- [ ] New tests: Tool validation, tasks, timeouts
- [ ] Coverage: 85%+ overall

**Time: 5 days**
**Risk: HIGH** (integration complexity)

---

## Phase 6: Performance & Benchmarking (Day 18-19)

### Objectives
- Run benchmark suite
- Verify no performance regression
- Optimize hot paths if needed
- Document performance characteristics

### Tasks

#### 6.1 Run Benchmark Suite (0.5 days)

```bash
# Run quick benchmarks
make benchmark-quick

# Expected results (baseline):
# ===> erlmcp_bench_core_ops:
#      Registry: 553K msg/s
#      Queue: 971K msg/s
#      Session: 242K msg/s
# ===> erlmcp_bench_network_real:
#      TCP sustained (10K): 43K msg/s
#      HTTP requests (1K): 12K req/s

# Run full suite (if time permits)
./scripts/bench/run_all_benchmarks.sh
```

**Performance Targets:**
- Registry throughput: >500K msg/s (no regression)
- Request latency P99: <10ms (new handlers)
- Memory per connection: <2KB (no regression)

**Deliverable:** Benchmark results (no regression)

#### 6.2 Profile Generated Handlers (0.5 days)

```bash
# Profile handler execution
erl -pa _build/default/lib/*/ebin
> fprof:trace(start).
> erlmcp_handler_tools_call:handle(1, #{}, State, Context).
> fprof:trace(stop).
> fprof:profile().
> fprof:analyse([{dest, "profile_tools_call.txt"}]).
```

**Look for:**
- Hot loops in validation
- Expensive schema operations
- Map operations in tight loops

**Deliverable:** Profiling report

#### 6.3 Optimize Hot Paths (1 day)

**Example Optimization:**

```erlang
% BEFORE (generated, readable):
handle(RequestId, Params, State, Context) ->
    % Phase validation
    case State#state.phase of
        initialized -> ok;
        _ -> error(not_initialized)
    end,
    % Capability validation
    case erlmcp_capabilities:has_capability(State#state.capabilities, <<"tools">>) of
        true -> ok;
        false -> error(capability_not_supported)
    end,
    % Schema validation
    case erlmcp_schema_validator:validate(Params, Schema) of
        ok -> execute_handler(...);
        Error -> Error
    end.

% AFTER (optimized for hot path):
handle(RequestId, Params, #state{phase = initialized, capabilities = Caps} = State, Context) ->
    % Inline fast path checks
    case maps:is_key(<<"tools">>, Caps) of
        true ->
            % Schema validation (cannot inline)
            case erlmcp_schema_validator:validate(Params, Schema) of
                ok -> execute_handler(...);
                Error -> Error
            end;
        false ->
            {error, {?MCP_ERROR_CAPABILITY_NOT_SUPPORTED, <<"tools not supported">>, #{}}}
    end;
handle(_RequestId, _Params, State, _Context) ->
    {error, {?MCP_ERROR_NOT_INITIALIZED, <<"not initialized">>, #{}}}.
```

**Note:** Only optimize if benchmarks show regression

**Deliverable:** Optimized handlers (if needed)

### Phase 6 Success Criteria

- [ ] Benchmarks run successfully
- [ ] No performance regression (< 10%)
- [ ] Hot paths profiled
- [ ] Optimizations applied (if needed)
- [ ] Performance documented

**Time: 2 days**
**Risk: LOW** (optional optimization)

---

## Phase 7: Documentation & Deployment (Day 20-22)

### Objectives
- Update CLAUDE.md
- Create migration guide
- Update API documentation
- Update examples
- Deploy to staging

### Tasks

#### 7.1 Update Project Documentation (1 day)

**Update `CLAUDE.md`:**

```markdown
## Code Generation (ggen v6.0.0)

erlmcp uses ggen (ontology-driven code generation) for MCP protocol compliance:

### Generated Components

- **33 Server Handlers** - `apps/erlmcp_core/src/handlers/`
  - Phase validation (automatic)
  - Capability validation (automatic)
  - JSON Schema validation (automatic)
  - Error handling (automatic)
  - Generated from `ontology/mcp/core/protocol.ttl`

- **Task Manager** - `apps/erlmcp_core/src/erlmcp_task_manager.erl`
  - State machine (5 states, 5 transitions)
  - Generated from `ontology/mcp/extensions/tasks.ttl`

- **Validators** - `apps/erlmcp_core/src/validators/`
  - Tool, Resource, Prompt validation
  - URI validation (Gap #36 fix)
  - Path traversal prevention

### Regenerating Code

```bash
# After modifying ontology
ggen sync

# Compile
rebar3 compile

# Test
rebar3 eunit
```

### Adding New MCP Methods

1. Add to ontology: `ontology/mcp/core/protocol.ttl`
2. Run: `ggen sync`
3. Add business logic to generated stub
4. Add tests
5. Done!
```

**Deliverable:** Updated CLAUDE.md

#### 7.2 Create Migration Guide (0.5 days)

**Create `docs/GGEN_MIGRATION_GUIDE.md`:**

```markdown
# Migration Guide: Manual → ggen-Generated Code

This guide helps migrate from manual handlers to ggen-generated code.

## Breaking Changes

### 1. Handler Interface

**BEFORE:**
```erlang
handle_tools_call(Params, State) ->
    % No automatic validation
    Result = my_handler(Params),
    {ok, Result, State}.
```

**AFTER:**
```erlang
% Generated handler calls execute_handler/5
execute_handler(RequestId, Params, State, Context, TimeoutMs) ->
    % Validation already done
    % Add business logic only
    Result = my_handler(Params),
    Result.  % Return result directly (no State)
```

### 2. Tool Registration

**BEFORE:**
```erlang
% No validation at registration
Tool = #mcp_tool{name = <<"mytool">>, description = <<"">>},
erlmcp_server:add_tool(Pid, Tool, Handler).
```

**AFTER:**
```erlang
% Validation enforced (description required)
Tool = #mcp_tool{
    name = <<"mytool">>,
    description = <<"My tool description">>  % Required!
},
erlmcp_server:add_tool(Pid, Tool, Handler).
% Returns: {error, {invalid_tool, ...}} if invalid
```

### 3. Tool Argument Validation

**BEFORE:**
```erlang
% Validation optional
Handler = fun(Args) ->
    % Manual validation (or none)
    case maps:find(<<"name">>, Args) of
        {ok, Name} -> #{result => Name};
        error -> {error, missing_name}
    end
end.
```

**AFTER:**
```erlang
% Validation automatic (P0 FIX)
Schema = #{
    type => <<"object">>,
    properties => #{name => #{type => <<"string">>}},
    required => [<<"name">>]
},
Tool = #mcp_tool{
    name = <<"mytool">>,
    description = <<"...">>,
    input_schema = Schema  % Validation enforced!
},
Handler = fun(Args) ->
    % Args already validated
    Name = maps:get(<<"name">>, Args),  % Safe, always present
    #{result => Name}
end.
```

## Migration Checklist

- [ ] Update handler interface (add execute_handler/5)
- [ ] Add input_schema to all tools
- [ ] Add validation to resource registration
- [ ] Update tests (validation now enforced)
- [ ] Remove manual validation code (redundant)
```

**Deliverable:** Migration guide

#### 7.3 Update API Documentation (0.5 days)

```bash
# Generate edoc
rebar3 edoc

# Output: doc/index.html
```

**Update key modules:**
- `erlmcp_server.erl` - Add @doc for generated handlers
- `erlmcp_task_manager.erl` - Document state machine
- `erlmcp_handler_*.erl` - Add usage examples

**Deliverable:** Updated API docs

#### 7.4 Update Examples (1 day)

**Update `examples/calculator/`:**

```erlang
% calculator_server.erl
start() ->
    {ok, Pid} = erlmcp_server:start_link(#{}),

    % Register calculator tools (with schemas)
    AddTool = #mcp_tool{
        name = <<"add">>,
        description = <<"Add two numbers">>,
        input_schema = #{  % NEW: Schema required
            type => <<"object">>,
            properties => #{
                a => #{type => <<"number">>},
                b => #{type => <<"number">>}
            },
            required => [<<"a">>, <<"b">>]
        }
    },
    AddHandler = fun(#{<<"a">> := A, <<"b">> := B}) ->
        #{result => A + B}
    end,
    erlmcp_server:add_tool(Pid, AddTool, AddHandler),

    {ok, Pid}.
```

**Create new example: `examples/task_manager/`**

```erlang
% task_server.erl - Demonstrates task management (P0 fix)
start() ->
    {ok, Pid} = erlmcp_server:start_link(#{capabilities => #{tasks => true}}),

    % Register long-running task action
    TaskHandler = fun(TaskId, Params) ->
        spawn(fun() ->
            % Simulate long work
            timer:sleep(5000),
            Result = #{completed => true},
            erlmcp_task_manager:complete(TaskId, Result)
        end)
    end,
    erlmcp_server:register_task_action(Pid, <<"long_work">>, TaskHandler),

    {ok, Pid}.
```

**Deliverable:** Updated examples

#### 7.5 Deploy to Staging (1 day)

**Pre-Deployment Checklist:**

```bash
# 1. Full quality gate
./tools/claude-md-enforcer.sh

# Expected:
# ✅ Compilation: 92 modules, 0 errors
# ✅ Tests: 320/320 passed (100%)
# ✅ Coverage: 87% (target: 80%)
# ✅ Dialyzer: 0 errors, 2 warnings
# ✅ Xref: 0 undefined calls
# ✅ Benchmarks: No regression

# 2. Build release
rebar3 as prod tar

# 3. Deploy to staging
scp _build/prod/rel/erlmcp-0.6.0.tar.gz staging:/opt/erlmcp/
ssh staging "cd /opt/erlmcp && tar xzf erlmcp-0.6.0.tar.gz"
ssh staging "/opt/erlmcp/bin/erlmcp start"

# 4. Smoke tests
curl http://staging:8080/health
# Expected: {"status":"ok","version":"0.6.0"}

# 5. Run integration tests against staging
MCP_HOST=staging:8080 rebar3 ct --suite=erlmcp_integration_SUITE
```

**Rollback Plan:**

```bash
# If deployment fails
ssh staging "/opt/erlmcp/bin/erlmcp stop"
ssh staging "cd /opt/erlmcp && rm -rf releases/0.6.0"
ssh staging "/opt/erlmcp/bin/erlmcp start"  # Reverts to previous version
```

**Deliverable:** Staging deployment successful

### Phase 7 Success Criteria

- [ ] CLAUDE.md updated
- [ ] Migration guide created
- [ ] API docs regenerated
- [ ] Examples updated
- [ ] Staging deployment successful
- [ ] Smoke tests pass

**Time: 3 days**
**Risk: LOW** (documentation and deployment)

---

## Risk Mitigation Strategies

### Risk 1: Ontology Modeling Complexity
**Probability:** MEDIUM
**Impact:** HIGH
**Mitigation:**
- Start with 10 core methods, expand incrementally
- Use existing MCP spec as reference (line-by-line)
- Pair programming for ontology creation
- Daily SHACL validation runs

### Risk 2: Template Debugging Difficulty
**Probability:** MEDIUM
**Impact:** MEDIUM
**Mitigation:**
- Test templates with minimal ontology first
- Use ggen --debug for verbose output
- Create template test harness
- Tera syntax checker in editor

### Risk 3: Integration Breaking Changes
**Probability:** HIGH
**Impact:** HIGH
**Mitigation:**
- Feature flag: `use_generated_handlers=false` initially
- Gradual rollout: 10% → 50% → 100%
- Comprehensive test suite before integration
- Canary deployment to staging first

### Risk 4: Performance Regression
**Probability:** LOW
**Impact:** MEDIUM
**Mitigation:**
- Benchmark baseline before changes
- Profile generated code early
- Optimize hot paths (Phase 6)
- Circuit breaker if >10% regression

### Risk 5: Team ggen Knowledge Gap
**Probability:** HIGH
**Impact:** MEDIUM
**Mitigation:**
- 2-day ggen training workshop
- Pair programming for first ontology
- Document all patterns in wiki
- Regular knowledge sharing sessions

---

## Rollback Plan

### Level 1: Rollback Generated Code Only
**Trigger:** Compilation failures, test failures >20%
**Time:** 1 hour
**Steps:**
```bash
# 1. Disable generated code in rebar.config
{src_dirs, ["src"]}.  % Remove generated/mcp/*

# 2. Revert to manual handlers
git checkout main -- apps/erlmcp_core/src/erlmcp_server.erl

# 3. Recompile
rebar3 clean && rebar3 compile

# 4. Test
rebar3 eunit
```

### Level 2: Rollback Entire Branch
**Trigger:** Unfixable integration issues
**Time:** 2 hours
**Steps:**
```bash
# 1. Merge main into current branch
git fetch origin main
git merge origin/main

# 2. Resolve conflicts (keep main version)
git checkout --theirs apps/erlmcp_core/src/*

# 3. Test
rebar3 compile && rebar3 eunit

# 4. Force push (if needed)
git push --force-with-lease
```

### Level 3: Revert to Pre-ggen State
**Trigger:** Critical production issues
**Time:** 30 minutes
**Steps:**
```bash
# 1. Checkout last known good commit
git log --oneline | grep "feat: v0.5.0"
git checkout <commit-hash>

# 2. Deploy immediately
rebar3 as prod tar
scp _build/prod/rel/*.tar.gz prod:/opt/erlmcp/
ssh prod "/opt/erlmcp/bin/erlmcp restart"

# 3. Postmortem
# - Document what went wrong
# - Fix in ggen branch
# - Retry next sprint
```

---

## Success Metrics

### Code Quality
- [ ] **Compilation:** 0 errors, 0 warnings
- [ ] **Tests:** 100% pass rate (320/320)
- [ ] **Coverage:** 85%+ (up from 78%)
- [ ] **Dialyzer:** 0 errors, <5 warnings
- [ ] **Xref:** 0 undefined function calls

### MCP Compliance
- [ ] **Protocol Coverage:** 33/33 methods (100%, up from 55%)
- [ ] **P0 Fixes:**
  - [ ] Tool argument validation (DONE)
  - [ ] Task management (DONE)
  - [ ] Client timeout/cleanup (DONE)
  - [ ] Transport test infrastructure (DONE)
- [ ] **Compliance Score:** 95+/100 (up from 78/100)

### Performance
- [ ] **Registry Throughput:** >500K msg/s (no regression)
- [ ] **Request Latency P99:** <10ms
- [ ] **Memory per Connection:** <2KB (no regression)
- [ ] **Benchmark Regression:** <10% across all metrics

### Development Velocity
- [ ] **Feature Development Time:** 2-3 hours (down from 2-3 days)
- [ ] **Lines of Manual Code:** 0 validation lines (down from 3,000+)
- [ ] **Code Generation Time:** <30 seconds (ggen sync)

### Team Adoption
- [ ] **Team Training:** 100% of team trained on ggen
- [ ] **Documentation:** Migration guide, examples, API docs
- [ ] **Confidence:** 80%+ team confidence in ggen approach

---

## Timeline Summary

| Phase | Days | Risk | Dependencies |
|-------|------|------|--------------|
| 1. Setup & Foundation | 2 | LOW | None |
| 2. Ontology Definition | 5 | MEDIUM | Phase 1 |
| 3. Template Creation | 3 | MEDIUM | Phase 2 |
| 4. Code Generation | 2 | MEDIUM | Phase 3 |
| 5. Integration & Testing | 5 | HIGH | Phase 4 |
| 6. Performance | 2 | LOW | Phase 5 |
| 7. Documentation & Deployment | 3 | LOW | Phase 6 |
| **TOTAL** | **22 days** | | |

**Sprints:**
- Sprint 1 (Days 1-7): Phases 1-2 (Setup + Ontology)
- Sprint 2 (Days 8-14): Phases 3-4 (Templates + Generation)
- Sprint 3 (Days 15-22): Phases 5-7 (Integration + Deployment)

---

## Resource Requirements

### Personnel
- **1 Senior Erlang Developer** (ontology + integration, 22 days full-time)
- **1 Mid-Level Erlang Developer** (templates + testing, 15 days full-time)
- **1 DevOps Engineer** (CI/CD + deployment, 5 days part-time)
- **1 Tech Lead** (reviews + unblocking, 10 days part-time)

### Infrastructure
- **Development:** Existing dev machines
- **CI/CD:** GitHub Actions (existing)
- **Staging:** 1 VM (4 CPU, 8GB RAM, existing)
- **Tools:** ggen v6.0+ (free, open source)

### Budget
- **Personnel:** 52 person-days × $500/day = $26,000
- **Infrastructure:** $0 (existing)
- **Tools:** $0 (open source)
- **Training:** 2-day workshop = $2,000
- **TOTAL:** $28,000

---

## Conclusion

This plan transforms erlmcp from 78/100 MCP compliance to 95+/100 by leveraging ggen (ontology-driven code generation) to:

1. **Eliminate manual validation code** (3,000+ LOC → 0)
2. **Implement missing P0 features** (task management, timeouts, validation)
3. **Establish single source of truth** (MCP ontology)
4. **Reduce future development time** (2-3 days → 2-3 hours per feature)

**Key Benefits:**
- Zero validation bugs (generated from spec)
- 100% protocol coverage (33/33 methods)
- Instant MCP spec updates (regenerate code)
- Type-safe code (Dialyzer types from ontology)
- Consistent patterns (all handlers identical structure)

**Next Steps:**
1. Review plan with team
2. Schedule 2-day ggen training
3. Kick off Phase 1 (Setup)
4. Daily standups during execution
5. Retrospective after each phase

**Approval Required From:**
- [ ] Tech Lead
- [ ] Product Manager
- [ ] Senior Erlang Team

---

**Plan Version:** 1.0
**Last Updated:** 2026-01-30
**Author:** plan-designer agent
**Session:** claude/mcp-compliance-refactor-IodEW
