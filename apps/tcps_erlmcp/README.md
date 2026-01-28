# tcps_erlmcp - Toyota Code Production System

**Version:** 2.0.0
**Application:** tcps_erlmcp
**Modules:** 63
**Status:** OPTIONAL add-on

Manufacturing-grade quality system for erlmcp. Implements Toyota Production System principles: SHACL ontology validation, quality gates, work orders (看板 Kanban), stop-the-line authority (自働化 Jidoka), and deterministic release certification.

## Overview

TCPS brings manufacturing discipline to software:
- **SHACL Validation** - Ontology-based correctness proofs (RDF/TTL shapes)
- **Quality Gates** - Automated pass/fail enforcement (80%+ test pass, 80%+ coverage)
- **Work Orders (作業指示)** - Structured task management with WIP limits
- **Jidoka (自働化)** - Stop-the-line on quality violations
- **Receipt Chains (レシート)** - SHA-256 hash chains for deterministic builds
- **SKU Certification (認証)** - Release artifact validation
- **Dashboard** - Real-time quality visualization (Cowboy HTTP)

## TCPS Modules (63 total)

### SHACL Ontology Validation
- **tcps_shacl_validator.erl** - SHACL shape validation against RDF ontology
- **tcps_shacl_loader.erl** - Load .ttl shape files from `shapes/` directory
- **tcps_ontology.erl** - Ontology graph management (RDF triples)

### Quality Gates (Poka-Yoke ポカヨケ)
- **tcps_quality_gates.erl** - Quality gate coordinator (test pass, coverage, complexity)
- **tcps_poka_yoke.erl** - Error-proofing mechanisms (input validation)
- **tcps_jidoka.erl** - Stop-the-line authority (build failure triggers Andon)
- **tcps_5_whys.erl** - Root cause analysis tool (なぜなぜ分析)

### Work Order System (看板 Kanban)
- **tcps_work_order.erl** - Work order creation and lifecycle
- **tcps_kanban.erl** - WIP limits and visual management
- **tcps_heijunka.erl** - Production leveling (平準化) - 40/30/20/10 work mix

### Receipt Chains & Certification
- **tcps_receipt_chain.erl** - SHA-256 deterministic hash chains
- **tcps_sku_certify.erl** - SKU (release) certification with evidence bundle
- **tcps_receipt_storage.erl** - Receipt persistence (file/ETS)

### Dashboard & API
- **tcps_dashboard.erl** - Cowboy HTTP dashboard server
- **tcps_api.erl** - REST API for TCPS operations

### Application (+ 51 additional modules)
- **tcps_erlmcp_app.erl** - OTP application callback
- **tcps_erlmcp_sup.erl** - TCPS supervisor
- **...** - 51 other modules for complete manufacturing system

## Dependencies

| Library | Version | Purpose |
|---------|---------|---------|
| **bbmustache** | 1.12.2 | Mustache template engine (receipt artifacts) |
| **cowboy** | 2.10.0 | HTTP server for dashboard |
| **jobs** | 0.10.0 | Work order queue management |
| **fs** | 0.9.2 | Filesystem monitoring (Roots enforcement) |
| **erlmcp_core** | 2.0.0 | Core MCP protocol |
| **erlmcp_observability** | 2.0.0 | Metrics and receipt chains |

## Usage Examples

### SHACL Validation (Pre-Compile Hook)

```bash
# Validate ontology before compilation
rebar3 tcps shacl_validate

# ✅ SHACL Validation: PASSED
#   - 15 shapes validated
#   - 0 violations
#   - Ontology: work_order_ontology.ttl
```

**Ontology Example (`shapes/work_order_shape.ttl`):**

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix tcps: <http://erlmcp.io/tcps#> .

tcps:WorkOrderShape
    a sh:NodeShape ;
    sh:targetClass tcps:WorkOrder ;
    sh:property [
        sh:path tcps:status ;
        sh:in (tcps:Pending tcps:InProgress tcps:Completed) ;
    ] .
```

### Quality Gates (Post-Test Hook)

```bash
# Run quality gate checks
rebar3 tcps check_quality_gates

# ✅ Quality Gates: PASSED
#   - Test Pass Rate: 95% (≥80% required)
#   - Coverage: 87% (≥80% required)
#   - Max Complexity: 12 (≤15 required)
#   - Max Function Length: 45 lines (≤50 required)
```

**Failure triggers Jidoka (stop-the-line):**

```bash
# ❌ Quality Gates: FAILED
#   - Test Pass Rate: 72% (≥80% required) ← VIOLATION
#   - Coverage: 65% (≥80% required) ← VIOLATION
#
# 🚨 Andon Event Triggered (行灯)
#    Build BLOCKED until quality gates pass
```

### Work Orders (Kanban)

```erlang
%% Create work order
{ok, WorkOrder} = tcps_work_order:create(#{
    type => feature,
    description => <<"Implement HTTP/2 transport">>,
    estimated_effort => 8  % hours
}),

%% Check WIP limits (看板)
WIP = tcps_kanban:get_wip_status(),
%%
%% #{
%%   design => 2/3,      % 2 active, 3 max
%%   coding => 5/5,      % 5 active, 5 max ← AT LIMIT
%%   testing => 4/7      % 4 active, 7 max
%% }

%% Cannot start new coding tasks until WIP < 5
case tcps_kanban:can_start(coding) of
    true -> tcps_work_order:assign(WorkOrder, coding);
    {false, at_limit} -> {error, wip_limit_reached}
end.
```

### Receipt Chains (Deterministic Builds)

```bash
# Generate compilation receipt (post-compile hook)
rebar3 tcps generate_receipt

# Receipt: e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
#   - Operation: compile
#   - Modules: 94
#   - Erlang: OTP-25.3
#   - Rebar3: 3.22.1
#   - Git SHA: a1b2c3d4
#   - Timestamp: 2026-01-27T10:30:45Z
```

**Verify chain integrity:**

```erlang
{ok, valid} = tcps_receipt_validator:verify_chain().
```

### SKU Certification (Release)

```bash
# Certify release with evidence bundle
rebar3 tcps sku_certify --version 2.0.0

# ✅ SKU Certified: erlmcp-2.0.0
#   - Build Receipt: e3b0c442...
#   - Test Results: 100% pass (150/150 tests)
#   - Coverage: 87%
#   - SHACL: Validated
#   - Evidence Bundle: _build/prod/erlmcp-2.0.0-certified.tar.gz
```

**Bundle contents:**
- Source code tarball
- Compiled BEAM files
- Receipt chain
- Test results
- SHACL validation report
- SHA-256 checksums

### Dashboard

```bash
# Start TCPS dashboard
rebar3 shell

# Access at http://localhost:8080/tcps/dashboard
```

**Dashboard features:**
- Real-time WIP visualization (Kanban board)
- Quality gate status
- Receipt chain viewer
- Work order timeline
- Andon event log

## Configuration

Default config in `tcps_erlmcp.app.src`:

```erlang
{quality_gates, #{
    min_test_pass_rate => 0.80,      % 80% minimum
    min_coverage => 0.80,            % 80% minimum
    max_cyclomatic_complexity => 15, % Per function
    max_function_length => 50        % Lines of code
}},
{shacl_defaults, #{
    shapes_dir => "shapes",
    ontology_dir => "ontology",
    strict_mode => true  % Fail on violations
}},
{receipt_chain, #{
    algorithm => sha256,
    storage => file,
    chain_file => "priv/tcps/receipt_chain.dat"
}},
{dashboard, #{
    port => 8080,
    host => "localhost"
}}
```

Override in `sys.config`:

```erlang
[
    {tcps_erlmcp, [
        {quality_gates, #{
            min_test_pass_rate => 0.90,  % Raise to 90%
            min_coverage => 0.85          % Raise to 85%
        }}
    ]}
].
```

## Build & Test

```bash
# Compile (with SHACL validation)
rebar3 compile

# Run tests (with quality gates)
rebar3 eunit

# Full TCPS pipeline
rebar3 tcps_full
# → SHACL validate → Compile → Test → Quality gates → Receipt

# Dashboard
rebar3 shell
# → http://localhost:8080/tcps/dashboard
```

## Quality Enforcement (Zero Tolerance)

TCPS enforces manufacturing-grade quality:

| Gate | Threshold | Action on Violation |
|------|-----------|---------------------|
| **Test Pass Rate** | ≥80% | Block build (Jidoka) |
| **Code Coverage** | ≥80% | Block build (Jidoka) |
| **Cyclomatic Complexity** | ≤15 | Warning (Kaizen suggestion) |
| **Function Length** | ≤50 lines | Warning (Kaizen suggestion) |
| **SHACL Validation** | 0 violations | Block build (Jidoka) |

**Jidoka (自働化) - Stop-the-Line:**
- Any gate failure triggers Andon event
- Build pipeline halts immediately
- Manual intervention required (fix violations)
- Cannot bypass gates (no "skip" flags)

## Japanese Manufacturing Terms

| Term | Romanization | English | TCPS Module |
|------|--------------|---------|-------------|
| 自働化 | Jidoka | Automation with human touch | tcps_jidoka |
| 看板 | Kanban | Signboard (WIP management) | tcps_kanban |
| 平準化 | Heijunka | Production leveling | tcps_heijunka |
| ポカヨケ | Poka-yoke | Error-proofing | tcps_poka_yoke |
| なぜなぜ分析 | Naze naze bunseki | 5 Whys analysis | tcps_5_whys |
| 作業指示 | Sagyou shiji | Work order | tcps_work_order |
| レシート | Reshīto | Receipt (evidence) | tcps_receipt_chain |
| 認証 | Ninshō | Certification | tcps_sku_certify |
| 行灯 | Andon | Light signal (problem alert) | tcps_jidoka |

## Optional Add-On

TCPS is **optional** in v2.0.0. To exclude from release:

```erlang
% rebar.config
{relx, [
    {release, {erlmcp, "2.0.0"},
     [erlmcp_core, erlmcp_transports, erlmcp_observability]
     % tcps_erlmcp omitted - no quality gates
    }
]}.
```

**Recommendation:** Use TCPS in production to enforce quality standards.

## See Also

- [erlmcp_core](../erlmcp_core/README.md) - Core MCP protocol
- [erlmcp_observability](../erlmcp_observability/README.md) - Receipt chains integrated
- [TCPS Guide](../../docs/tcps/TCPS.md) - Complete TCPS system documentation
- [Architecture](../../docs/architecture.md) - System design
