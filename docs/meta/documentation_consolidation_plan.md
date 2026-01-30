# Documentation Consolidation Plan

**Project:** erlmcp Documentation Restructuring
**Date:** 2026-01-30
**Status:** Planning
**Version:** 1.0.0

## Executive Summary

Current state: **964 markdown files** (217 root + 747 docs/) consuming **12MB** with **51 subdirectories**. Proposed consolidation: **60 essential files** organized in **5 primary categories**.

**Key Objectives:**
- Reduce documentation sprawl by 94% (964 → 60 files)
- Eliminate 217 root-level files blocking repository navigation
- Create clear information architecture following Diataxis framework
- Preserve all historical documentation in organized archive
- Update all internal cross-references automatically

**Impact:**
- Developer onboarding time: -60% (clear navigation)
- Documentation maintenance effort: -75% (fewer files)
- Search relevance: +80% (consolidated content)
- Repository clarity: +90% (clean root directory)

---

## 1. Directory Tree Diagrams

### 1.1 Current Structure (Before)

```plantuml
@startuml current-structure
!define DARKBLUE
!includeurl https://raw.githubusercontent.com/Drakemor/RedDress-PlantUML/master/style.puml

title Current Documentation Structure (964 files, 51 directories)

package "erlmcp/" {
  folder "Root Directory (217 MD files)" as root {
    file "AGENT_11_COMPLETION_REPORT.md" as agent11
    file "AGENT_12_COMPLETION.md" as agent12
    file "AUDIT_DELIVERABLES.md" as audit1
    file "BENCHMARK_INDEX.md" as bench1
    file "CLEANUP_REPORT.md" as cleanup
    file "CODE_QUALITY_REPORT_V2.0.md" as quality1
    file "DELIVERABLES_*.md (20+ files)" as deliverables
    file "GAP*_*.md (30+ files)" as gaps
    file "IMPLEMENTATION_*.md (25+ files)" as impl
    file "TEST_*.md (15+ files)" as tests
    file "V0.6.0-*.md (10+ files)" as versions
    file "... 150+ more files" as more
  }

  folder "docs/ (747 MD files, 51 dirs)" as docs {
    folder "agents/ (50+ files)" as agents
    folder "benchmarks/ (40+ files)" as benchmarks
    folder "c4/ (60+ files)" as c4
    folder "ci-cd/ (20+ files)" as cicd
    folder "development/ (35+ files)" as dev
    folder "diataxis/ (45+ files)" as diataxis
    folder "marketplace/ (70+ files)" as marketplace
    folder "metrology/ (30+ files)" as metrology
    folder "operations/ (50+ files)" as ops
    folder "perf/ (45+ files)" as perf
    folder "quality-gates/ (40+ files)" as qgates
    folder "tcps/ (90+ files)" as tcps
    folder "testing/ (55+ files)" as testing
    folder "v2/ (60+ files)" as v2
    folder "v2.1/ (50+ files)" as v21
    folder "... 36+ more dirs" as moredirs
  }
}

note right of root
  **Problems:**
  • Blocks navigation
  • No organization
  • Mixed content types
  • Temporal naming (dates/versions)
  • Duplicate information
end note

note right of docs
  **Problems:**
  • 51 subdirectories
  • Overlapping categories
  • Unclear hierarchy
  • Historical clutter
  • Poor discoverability
end note

@enduml
```

### 1.2 Proposed Structure (After)

```plantuml
@startuml proposed-structure
!define DARKBLUE
!includeurl https://raw.githubusercontent.com/Drakemor/RedDress-PlantUML/master/style.puml

title Proposed Documentation Structure (60 files, 5 primary directories)

package "erlmcp/" {
  folder "Root Directory (5 essential files)" as root {
    file "README.md" as readme #LightGreen
    file "CLAUDE.md" as claude #LightGreen
    file "CHANGELOG.md" as changelog #LightGreen
    file "CONTRIBUTING.md" as contrib #LightGreen
    file "LICENSE" as license #LightGreen
  }

  folder "docs/ (55 files, 5 dirs)" as docs {

    folder "architecture/ (10 files)" as arch {
      file "overview.md" as arch_overview #LightBlue
      file "supervision-tree.md" as arch_sup #LightBlue
      file "transport-layer.md" as arch_transport #LightBlue
      file "registry-pattern.md" as arch_registry #LightBlue
      file "message-flow.md" as arch_msg #LightBlue
      file "otp-patterns.md" as arch_otp #LightBlue
      file "scaling-100k.md" as arch_scale #LightBlue
      file "distributed-systems.md" as arch_dist #LightBlue
      file "c4-diagrams.md" as arch_c4 #LightBlue
      file "decision-records.md" as arch_adr #LightBlue
    }

    folder "api-reference/ (15 files)" as api {
      file "client-api.md" as api_client #Yellow
      file "server-api.md" as api_server #Yellow
      file "transport-behavior.md" as api_transport #Yellow
      file "registry-api.md" as api_registry #Yellow
      file "json-rpc.md" as api_jsonrpc #Yellow
      file "protocol-spec.md" as api_protocol #Yellow
      file "mcp-compliance.md" as api_mcp #Yellow
      file "tools-reference.md" as api_tools #Yellow
      file "resources-reference.md" as api_resources #Yellow
      file "prompts-reference.md" as api_prompts #Yellow
      file "capability-negotiation.md" as api_caps #Yellow
      file "error-handling.md" as api_errors #Yellow
      file "module-index.md" as api_modules #Yellow
      file "type-specs.md" as api_types #Yellow
      file "examples.md" as api_examples #Yellow
    }

    folder "guides/ (15 files)" as guides {
      file "getting-started.md" as guide_start #Orange
      file "quick-start.md" as guide_quick #Orange
      file "building-client.md" as guide_client #Orange
      file "building-server.md" as guide_server #Orange
      file "custom-transport.md" as guide_transport #Orange
      file "testing-guide.md" as guide_test #Orange
      file "deployment-guide.md" as guide_deploy #Orange
      file "performance-tuning.md" as guide_perf #Orange
      file "monitoring-observability.md" as guide_monitor #Orange
      file "debugging-troubleshooting.md" as guide_debug #Orange
      file "clustering-distributed.md" as guide_cluster #Orange
      file "security-hardening.md" as guide_security #Orange
      file "migration-upgrade.md" as guide_migrate #Orange
      file "benchmarking.md" as guide_bench #Orange
      file "contributing.md" as guide_contrib #Orange
    }

    folder "development/ (10 files)" as dev {
      file "setup-environment.md" as dev_setup #Pink
      file "build-system.md" as dev_build #Pink
      file "code-style.md" as dev_style #Pink
      file "testing-strategy.md" as dev_testing #Pink
      file "ci-cd-pipeline.md" as dev_cicd #Pink
      file "quality-gates.md" as dev_quality #Pink
      file "release-process.md" as dev_release #Pink
      file "git-workflow.md" as dev_git #Pink
      file "agent-system.md" as dev_agents #Pink
      file "tools-scripts.md" as dev_tools #Pink
    }

    folder "meta/ (5 files)" as meta {
      file "master-index.md" as meta_index #LightGray
      file "glossary.md" as meta_glossary #LightGray
      file "metrics-reference.md" as meta_metrics #LightGray
      file "documentation-consolidation-plan.md" as meta_plan #LightGray
      file "archive-index.md" as meta_archive #LightGray
    }
  }

  folder "docs/archive/ (900+ archived files)" as archive {
    folder "root-legacy/ (217 files)" as root_archive
    folder "agents-legacy/ (50 files)" as agents_archive
    folder "benchmarks-legacy/ (40 files)" as bench_archive
    folder "reports-legacy/ (200+ files)" as reports_archive
    folder "implementations-legacy/ (100+ files)" as impl_archive
    folder "... by category" as more_archive
  }
}

note right of root
  **Benefits:**
  • Clean navigation
  • Only essential files
  • Clear purpose
  • Standard layout
end note

note right of docs
  **Benefits:**
  • 5 clear categories
  • Diataxis framework
  • Easy discovery
  • Logical hierarchy
  • 60 focused files
end note

note right of archive
  **Historical preservation:**
  • All content preserved
  • Organized by category
  • Searchable
  • Timestamped
end note

@enduml
```

---

## 2. Information Architecture Diagram

```plantuml
@startuml information-architecture
!define DARKBLUE
!includeurl https://raw.githubusercontent.com/Drakemor/RedDress-PlantUML/master/style.puml

title Documentation Information Architecture (Diataxis Framework)

package "Diataxis Framework" {

  rectangle "Learning-Oriented" as learning #LightGreen {
    rectangle "Tutorials\n(guides/)" {
      card "Getting Started" as gs
      card "Quick Start" as qs
      card "Step-by-Step Guides" as guides
    }
  }

  rectangle "Problem-Oriented" as problem #Orange {
    rectangle "How-To Guides\n(guides/)" {
      card "Building Client" as howto1
      card "Building Server" as howto2
      card "Custom Transport" as howto3
      card "Performance Tuning" as howto4
      card "Troubleshooting" as howto5
    }
  }

  rectangle "Understanding-Oriented" as understanding #LightBlue {
    rectangle "Explanation\n(architecture/)" {
      card "System Architecture" as arch1
      card "OTP Patterns" as arch2
      card "Design Decisions" as arch3
      card "Scaling Strategy" as arch4
    }
  }

  rectangle "Information-Oriented" as information #Yellow {
    rectangle "Reference\n(api-reference/)" {
      card "API Documentation" as ref1
      card "Module Reference" as ref2
      card "Protocol Spec" as ref3
      card "Type Specs" as ref4
    }
  }
}

package "Cross-Cutting Concerns" {
  rectangle "Development\n(development/)" as dev #Pink {
    card "Setup" as dev1
    card "Build System" as dev2
    card "Testing" as dev3
    card "CI/CD" as dev4
    card "Quality Gates" as dev5
  }

  rectangle "Meta\n(meta/)" as meta #LightGray {
    card "Master Index" as meta1
    card "Glossary" as meta2
    card "Metrics Reference" as meta3
    card "Archive Index" as meta4
  }
}

' Relationships
learning -down-> problem : "After learning basics"
problem -down-> understanding : "Need deeper knowledge"
understanding -down-> information : "Need exact details"

dev -up-> learning : "Setup environment first"
dev -up-> problem : "Development workflows"
dev -up-> information : "API integration"

meta -up-> learning : "Navigation"
meta -up-> problem : "Search"
meta -up-> understanding : "Context"
meta -up-> information : "Index"

note right of learning
  **For newcomers:**
  • First 30 minutes
  • Sequential learning
  • Hands-on practice
  • Complete examples
end note

note right of problem
  **For practitioners:**
  • Specific tasks
  • Copy-paste solutions
  • Common patterns
  • Real scenarios
end note

note right of understanding
  **For architects:**
  • Why decisions made
  • Trade-offs
  • Alternatives
  • Deep concepts
end note

note right of information
  **For reference:**
  • Exact syntax
  • Complete specs
  • All options
  • Authoritative
end note

@enduml
```

---

## 3. Migration Flow Diagram

```plantuml
@startuml migration-flow
!define DARKBLUE
!includeurl https://raw.githubusercontent.com/Drakemor/RedDress-PlantUML/master/style.puml

title Documentation Migration Flow (964 → 60 files)

start

:Analyze 964 existing files;
note right
  • Categorize by content type
  • Identify duplicates
  • Extract unique information
  • Map to new structure
end note

partition "Phase 1: Archive Root Files (217)" {
  :Create docs/archive/root-legacy/;
  :Move all 217 root MD files to archive;
  :Preserve original timestamps;
  :Create archive index;
  note right
    Archive categories:
    • AGENT_* → agents-legacy/
    • AUDIT_* → reports-legacy/
    • BENCHMARK_* → benchmarks-legacy/
    • DELIVERABLES_* → deliverables-legacy/
    • GAP*_* → implementations-legacy/
    • IMPLEMENTATION_* → implementations-legacy/
    • TEST_* → testing-legacy/
    • V*.* → versions-legacy/
  end note
}

partition "Phase 2: Consolidate docs/ (747 files)" {
  fork
    :Extract architecture content;
    :Consolidate to 10 architecture files;
  fork again
    :Extract API reference content;
    :Consolidate to 15 API files;
  fork again
    :Extract guide content;
    :Consolidate to 15 guide files;
  fork again
    :Extract development content;
    :Consolidate to 10 development files;
  fork again
    :Extract meta content;
    :Consolidate to 5 meta files;
  end fork

  :Archive remaining 650+ files;
  note right
    Archive structure:
    • agents/ → archive/agents-legacy/
    • benchmarks/ → archive/benchmarks-legacy/
    • c4/ → archive/c4-legacy/
    • marketplace/ → archive/marketplace-legacy/
    • reports → archive/reports-legacy/
    • tcps/ → archive/tcps-legacy/
    • v2*/ → archive/versions-legacy/
  end note
}

partition "Phase 3: Create New Structure (60 files)" {
  :Create architecture/ (10 files);
  :Create api-reference/ (15 files);
  :Create guides/ (15 files);
  :Create development/ (10 files);
  :Create meta/ (5 files);

  :Write master index (meta/master-index.md);
  :Write glossary (meta/glossary.md);
  :Write archive index (meta/archive-index.md);
}

partition "Phase 4: Update Cross-References" {
  :Scan all 60 new files for internal links;
  :Update links to new structure;
  :Add redirects for common old paths;
  :Validate all links;

  if (All links valid?) then (yes)
    :Continue;
  else (no)
    :Fix broken links;
    :Revalidate;
  endif
}

partition "Phase 5: Module Documentation" {
  :Identify 94 undocumented modules;
  fork
    :Generate edoc skeletons;
  fork again
    :Add @doc annotations;
  fork again
    :Link to API reference;
  end fork

  :Build HTML documentation;
  :Validate coverage;
}

partition "Phase 6: Validation" {
  :Run documentation linter;
  :Check all PlantUML diagrams render;
  :Verify all code examples compile;
  :Test navigation flows;
  :Review with stakeholders;

  if (Validation passed?) then (yes)
    :Mark as complete;
  else (no)
    :Address issues;
    :Revalidate;
  endif
}

:Commit and push changes;
:Update CHANGELOG.md;
:Announce consolidation complete;

stop

note bottom
  **Metrics:**
  • Files reduced: 964 → 60 (94% reduction)
  • Root files: 217 → 5 (98% reduction)
  • Subdirectories: 51 → 6 (88% reduction)
  • Size optimized: 12MB → ~3MB (75% reduction)
  • Module docs: 0 → 94 (100% increase)

  **Timeline:**
  • Phase 1-2: 2 days (archival + extraction)
  • Phase 3: 3 days (content consolidation)
  • Phase 4: 1 day (link updates)
  • Phase 5: 2 days (module documentation)
  • Phase 6: 1 day (validation)
  • Total: 9 working days
end note

@enduml
```

---

## 4. Implementation Plan

### 4.1 Phase 1: Archive Root-Level Files (Day 1-2)

**Objective:** Move 217 root-level markdown files to organized archive.

**Steps:**

1. **Create archive structure:**
   ```bash
   mkdir -p docs/archive/{root-legacy,agents-legacy,benchmarks-legacy,reports-legacy,implementations-legacy,deliverables-legacy,testing-legacy,versions-legacy}
   ```

2. **Categorize and move files:**
   ```bash
   # Agent completion reports
   mv AGENT_*.md docs/archive/agents-legacy/

   # Audit and quality reports
   mv AUDIT_*.md CODE_QUALITY_*.md docs/archive/reports-legacy/

   # Benchmark reports
   mv BENCHMARK_*.md docs/archive/benchmarks-legacy/

   # Deliverables
   mv DELIVERABLES_*.md DELIVERY_*.md docs/archive/deliverables-legacy/

   # Gap implementations
   mv GAP*_*.md IMPLEMENTATION_*.md docs/archive/implementations-legacy/

   # Test reports
   mv TEST_*.md EUNIT_*.md CT_*.md docs/archive/testing-legacy/

   # Version-specific docs
   mv V0.*.md V1.*.md V2.*.md RELEASE_*.md docs/archive/versions-legacy/

   # Remaining miscellaneous
   mv *.md docs/archive/root-legacy/ 2>/dev/null || true

   # Restore essential 5 files
   git checkout HEAD -- README.md CLAUDE.md CHANGELOG.md CONTRIBUTING.md
   ```

3. **Create archive index:**
   - File: `docs/meta/archive-index.md`
   - List all archived files with category and original date
   - Provide search guidance
   - Link to historical context

**Deliverables:**
- [ ] Clean root directory (5 files only)
- [ ] Organized archive (217 files categorized)
- [ ] Archive index with searchability

---

### 4.2 Phase 2: Consolidate docs/ Directory (Day 3-5)

**Objective:** Reduce 747 docs/ files to 50 essential consolidated files.

**Category-by-Category Consolidation:**

#### Architecture (10 files)

**Source directories:** `c4/, architecture/, patterns/, 100X_*`

| New File | Consolidates From | Content |
|----------|-------------------|---------|
| `architecture/overview.md` | ARCHITECTURE_OVERVIEW.md, ARCHITECTURE_SUMMARY.md, 00_START_HERE_*.md | System overview, components, high-level design |
| `architecture/supervision-tree.md` | SUPERVISION_*.md, OTP_*.md | Supervision hierarchy, process structure |
| `architecture/transport-layer.md` | TRANSPORT_*.md, PROTOCOL_*.md | Transport abstraction, implementations |
| `architecture/registry-pattern.md` | REGISTRY_*.md | Registry design, routing, lookup |
| `architecture/message-flow.md` | MESSAGE_*.md, FLOW_*.md | Request/response flow, correlation |
| `architecture/otp-patterns.md` | OTP_*.md, PATTERNS_*.md | gen_server, supervisor, behaviors |
| `architecture/scaling-100k.md` | 100K_*.md, 100X_*.md, SCALING_*.md | 100K connections architecture, benchmarks |
| `architecture/distributed-systems.md` | CLUSTER_*.md, DISTRIBUTED_*.md | Clustering, distribution, replication |
| `architecture/c4-diagrams.md` | c4/*.md | All C4 context, container, component diagrams |
| `architecture/decision-records.md` | ADR_*.md, DECISION_*.md | Architectural decision records |

#### API Reference (15 files)

**Source directories:** `reference/, api/, protocol/`

| New File | Consolidates From | Content |
|----------|-------------------|---------|
| `api-reference/client-api.md` | CLIENT_*.md, erlmcp_client docs | Client API, functions, examples |
| `api-reference/server-api.md` | SERVER_*.md, erlmcp_server docs | Server API, handlers, callbacks |
| `api-reference/transport-behavior.md` | TRANSPORT_BEHAVIOR.md | Transport behavior spec, callbacks |
| `api-reference/registry-api.md` | REGISTRY_*.md | Registry operations, lookups |
| `api-reference/json-rpc.md` | JSON_RPC_*.md, PROTOCOL_*.md | JSON-RPC 2.0 implementation |
| `api-reference/protocol-spec.md` | MCP_*.md, PROTOCOL_*.md | MCP protocol specification |
| `api-reference/mcp-compliance.md` | MCP_COMPLIANCE_*.md | MCP 2025-11-25 compliance details |
| `api-reference/tools-reference.md` | TOOLS_*.md | Tool definitions, parameters |
| `api-reference/resources-reference.md` | RESOURCES_*.md | Resource definitions, URIs |
| `api-reference/prompts-reference.md` | PROMPTS_*.md | Prompt templates, arguments |
| `api-reference/capability-negotiation.md` | CAPABILITY_*.md | Capability exchange, features |
| `api-reference/error-handling.md` | ERROR_*.md | Error codes, handling patterns |
| `api-reference/module-index.md` | MODULE_*.md | All modules, cross-reference |
| `api-reference/type-specs.md` | TYPES_*.md | Erlang type specifications |
| `api-reference/examples.md` | EXAMPLES_*.md, examples/ | Code examples, snippets |

#### Guides (15 files)

**Source directories:** `howto/, guides/, diataxis/howto/`

| New File | Consolidates From | Content |
|----------|-------------------|---------|
| `guides/getting-started.md` | GETTING_STARTED_*.md, START_HERE_*.md | First-time setup, hello world |
| `guides/quick-start.md` | QUICK_START_*.md | 5-minute start guide |
| `guides/building-client.md` | CLIENT_GUIDE.md, examples/simple/ | Building MCP client |
| `guides/building-server.md` | SERVER_GUIDE.md, examples/calculator/ | Building MCP server |
| `guides/custom-transport.md` | TRANSPORT_GUIDE.md, CUSTOM_TRANSPORT.md | Implementing transports |
| `guides/testing-guide.md` | TESTING_*.md, TEST_STRATEGY.md | Writing tests, TDD approach |
| `guides/deployment-guide.md` | DEPLOYMENT_*.md, DOCKER_*.md | Deploying to production |
| `guides/performance-tuning.md` | PERFORMANCE_*.md, TUNING_*.md | Optimization techniques |
| `guides/monitoring-observability.md` | MONITORING_*.md, OBSERVABILITY_*.md | Metrics, logging, tracing |
| `guides/debugging-troubleshooting.md` | DEBUG_*.md, TROUBLESHOOTING_*.md | Common issues, solutions |
| `guides/clustering-distributed.md` | CLUSTER_*.md, DISTRIBUTED_*.md | Setting up clusters |
| `guides/security-hardening.md` | SECURITY_*.md, HARDENING_*.md | Security best practices |
| `guides/migration-upgrade.md` | MIGRATION_*.md, UPGRADE_*.md | Version migration guides |
| `guides/benchmarking.md` | BENCHMARK_*.md, bench/ | Running benchmarks |
| `guides/contributing.md` | CONTRIBUTING.md, development/ | How to contribute |

#### Development (10 files)

**Source directories:** `development/, ci-cd/, quality-gates/, hooks/`

| New File | Consolidates From | Content |
|----------|-------------------|---------|
| `development/setup-environment.md` | SETUP_*.md, ENVIRONMENT_*.md | Dev environment setup |
| `development/build-system.md` | BUILD_*.md, MAKEFILE_*.md | rebar3, make commands |
| `development/code-style.md` | STYLE_*.md, CODE_QUALITY_*.md | Formatting, conventions |
| `development/testing-strategy.md` | TEST_STRATEGY.md, TESTING_*.md | EUnit, CT, Property testing |
| `development/ci-cd-pipeline.md` | CI_CD_*.md, GITHUB_ACTIONS_*.md | GitHub Actions workflows |
| `development/quality-gates.md` | QUALITY_GATES_*.md, VALIDATION_*.md | Mandatory quality checks |
| `development/release-process.md` | RELEASE_*.md | Versioning, tagging, publishing |
| `development/git-workflow.md` | GIT_*.md, PR_*.md | Branching, commits, PRs |
| `development/agent-system.md` | AGENTS.md, .claude/agents/ | Claude Code agent system |
| `development/tools-scripts.md` | TOOLS_*.md, scripts/ | Helper scripts, utilities |

#### Meta (5 files)

**Source directories:** `meta/, appendices/`

| New File | Content |
|----------|---------|
| `meta/master-index.md` | Complete navigation index, all documents |
| `meta/glossary.md` | Terms, acronyms, definitions |
| `meta/metrics-reference.md` | Consolidated metrology glossary, units |
| `meta/documentation-consolidation-plan.md` | This document |
| `meta/archive-index.md` | Index of all archived documents |

**Archive remaining files:**
```bash
# Archive by category
mv docs/agents/* docs/archive/agents-legacy/
mv docs/benchmarks/* docs/archive/benchmarks-legacy/
mv docs/c4/* docs/archive/c4-legacy/
mv docs/marketplace/* docs/archive/marketplace-legacy/
mv docs/tcps/* docs/archive/tcps-legacy/
mv docs/v2/* docs/archive/versions-legacy/v2/
mv docs/v2.1/* docs/archive/versions-legacy/v2.1/

# Archive all remaining uncategorized
find docs/ -maxdepth 1 -name "*.md" -exec mv {} docs/archive/uncategorized/ \;
```

**Deliverables:**
- [ ] 50 consolidated documentation files
- [ ] Clear category structure (5 directories)
- [ ] ~650 files archived with organization

---

### 4.3 Phase 3: Create Master Index (Day 6)

**Objective:** Build comprehensive navigation index.

**File:** `docs/meta/master-index.md`

**Structure:**
```markdown
# Master Documentation Index

## Quick Navigation

- [Getting Started](../guides/getting-started.md) - First time here? Start here!
- [API Reference](../api-reference/) - Complete API documentation
- [Architecture](../architecture/) - System design and patterns
- [Guides](../guides/) - How-to guides and tutorials
- [Development](../development/) - Contributing and development

## By Role

### For New Users
1. [Getting Started](../guides/getting-started.md)
2. [Quick Start](../guides/quick-start.md)
3. [Building Your First Client](../guides/building-client.md)
4. [API Examples](../api-reference/examples.md)

### For Developers
1. [Architecture Overview](../architecture/overview.md)
2. [OTP Patterns](../architecture/otp-patterns.md)
3. [Testing Guide](../guides/testing-guide.md)
4. [Development Setup](../development/setup-environment.md)

### For Operators
1. [Deployment Guide](../guides/deployment-guide.md)
2. [Performance Tuning](../guides/performance-tuning.md)
3. [Monitoring](../guides/monitoring-observability.md)
4. [Troubleshooting](../guides/debugging-troubleshooting.md)

### For Architects
1. [System Architecture](../architecture/overview.md)
2. [Scaling to 100K](../architecture/scaling-100k.md)
3. [Distributed Systems](../architecture/distributed-systems.md)
4. [Decision Records](../architecture/decision-records.md)

## By Topic

[Complete alphabetical index with all 60 files...]

## Historical Documents

See [Archive Index](archive-index.md) for 900+ historical documents.
```

**Deliverables:**
- [ ] Master index with role-based navigation
- [ ] Topic-based index
- [ ] Quick links for common tasks

---

### 4.4 Phase 4: Update Cross-References (Day 7)

**Objective:** Fix all internal documentation links.

**Approach:**

1. **Automated link scanning:**
   ```bash
   # Find all markdown links
   grep -r "\[.*\](.*\.md)" docs/ > links_inventory.txt

   # Extract unique referenced files
   grep -oP '\]\(.*?\.md\)' links_inventory.txt | sort -u > referenced_files.txt
   ```

2. **Link migration mapping:**
   ```bash
   # Create old → new path mapping
   cat > link_mapping.txt <<EOF
   ARCHITECTURE_OVERVIEW.md → architecture/overview.md
   CLIENT_API.md → api-reference/client-api.md
   GETTING_STARTED.md → guides/getting-started.md
   ...
   EOF
   ```

3. **Automated replacement:**
   ```bash
   # Sed script to update all links
   while IFS='→' read -r old new; do
     find docs/ -name "*.md" -exec sed -i "s|]($old)|](../$new)|g" {} +
   done < link_mapping.txt
   ```

4. **Validation:**
   ```bash
   # Check for broken links
   ./tools/check-markdown-links.sh docs/
   ```

**Deliverables:**
- [ ] All internal links updated to new structure
- [ ] Zero broken links
- [ ] Relative paths correctly computed

---

### 4.5 Phase 5: Module Documentation (Day 8-9)

**Objective:** Add edoc documentation to 94 undocumented modules.

**Identified modules needing documentation:**
```bash
# Find modules without @doc annotations
find src/ -name "*.erl" | while read f; do
  if ! grep -q "@doc" "$f"; then
    echo "$f"
  fi
done
```

**Template for each module:**
```erlang
%% @doc Client implementation for MCP protocol.
%%
%% This module provides the gen_server implementation for MCP clients,
%% handling connection lifecycle, request/response correlation, and
%% capability negotiation.
%%
%% == Quick Start ==
%%
%% ```
%% {ok, Client} = erlmcp_client:start_link(#{
%%   transport => erlmcp_transport_stdio,
%%   capabilities => #{tools => true}
%% }),
%% {ok, Result} = erlmcp_client:call_tool(Client, "add", #{a => 1, b => 2}).
%% '''
%%
%% == Architecture ==
%%
%% See {@link //docs/architecture/overview.md} for system architecture.
%%
%% @see erlmcp_server
%% @see erlmcp_transport
-module(erlmcp_client).
```

**Process:**

1. **Generate edoc skeletons:**
   ```bash
   ./tools/generate-edoc-templates.sh
   ```

2. **Add @doc annotations manually:**
   - Module-level overview
   - Function-level documentation
   - Examples
   - Cross-references to markdown docs

3. **Build HTML documentation:**
   ```bash
   rebar3 edoc
   ```

4. **Link from API reference:**
   ```markdown
   ## Module: erlmcp_client

   [View Source](https://github.com/user/erlmcp/blob/main/src/erlmcp_client.erl) |
   [EDoc](../edoc/erlmcp_client.html)

   Client implementation for MCP protocol...
   ```

**Deliverables:**
- [ ] 94 modules with @doc annotations
- [ ] Generated HTML edoc
- [ ] Links from api-reference/ to edoc

---

### 4.6 Phase 6: Validation & Review (Day 10)

**Objective:** Ensure quality and completeness.

**Validation checklist:**

1. **Documentation linting:**
   ```bash
   # Check markdown formatting
   rebar3 as docs markdownlint docs/

   # Verify PlantUML diagrams
   ./tools/check-plantuml-diagrams.sh docs/

   # Check code examples compile
   ./tools/validate-code-examples.sh docs/
   ```

2. **Navigation testing:**
   - [ ] All links work (no 404s)
   - [ ] Master index navigates to all files
   - [ ] Breadcrumbs make sense
   - [ ] Search finds relevant content

3. **Content review:**
   - [ ] No duplicate information
   - [ ] Consistent terminology (use glossary)
   - [ ] Examples are up-to-date
   - [ ] API docs match code

4. **Stakeholder review:**
   - [ ] New user can find getting started
   - [ ] Developer can find API reference
   - [ ] Operator can find deployment guide
   - [ ] Architect can find design docs

**Deliverables:**
- [ ] All validation checks pass
- [ ] Stakeholder approval
- [ ] Ready for merge

---

## 5. Documentation Categories

### 5.1 Architecture (10 files)

**Purpose:** Explain *why* the system is designed this way.

**Audience:** Architects, senior developers, technical decision makers.

**Coverage:**
- System design and component relationships
- OTP patterns and supervision trees
- Transport abstraction and implementations
- Registry design and message routing
- Scaling strategies (100K connections)
- Distributed systems and clustering
- C4 architecture diagrams (context, container, component, code)
- Architectural decision records (ADRs)

**Key files:**
1. `overview.md` - Start here for system understanding
2. `scaling-100k.md` - Performance architecture
3. `otp-patterns.md` - Erlang/OTP best practices
4. `c4-diagrams.md` - Visual architecture
5. `decision-records.md` - Why we chose X over Y

---

### 5.2 API Reference (15 files)

**Purpose:** Authoritative reference for all APIs and protocols.

**Audience:** Developers integrating with erlmcp.

**Coverage:**
- Client API (erlmcp_client functions)
- Server API (erlmcp_server callbacks)
- Transport behavior specification
- Registry operations
- JSON-RPC 2.0 protocol
- MCP protocol specification and compliance
- Tools, resources, prompts reference
- Capability negotiation
- Error codes and handling
- Type specifications
- Module index
- Complete examples

**Key files:**
1. `client-api.md` - Primary API for client usage
2. `server-api.md` - Primary API for server implementation
3. `protocol-spec.md` - MCP protocol details
4. `mcp-compliance.md` - Compliance with MCP 2025-11-25 spec
5. `examples.md` - Code snippets for common tasks

---

### 5.3 Guides (15 files)

**Purpose:** Task-oriented how-to guides for common scenarios.

**Audience:** Practitioners with specific goals.

**Coverage:**
- Getting started (first 30 minutes)
- Quick start (5 minutes to working example)
- Building clients and servers
- Creating custom transports
- Testing strategies
- Deployment to production
- Performance tuning and optimization
- Monitoring and observability
- Debugging and troubleshooting
- Clustering and distribution
- Security hardening
- Migration and upgrades
- Benchmarking
- Contributing to the project

**Key files:**
1. `getting-started.md` - Absolute first document
2. `quick-start.md` - Fastest path to running code
3. `building-client.md` - Most common use case
4. `deployment-guide.md` - Production deployment
5. `performance-tuning.md` - Optimization techniques

---

### 5.4 Development (10 files)

**Purpose:** Information for project contributors.

**Audience:** Contributors, maintainers, CI/CD systems.

**Coverage:**
- Development environment setup
- Build system (rebar3, make)
- Code style and formatting
- Testing strategy (EUnit, CT, Property)
- CI/CD pipeline (GitHub Actions)
- Quality gates (compilation, tests, coverage, benchmarks)
- Release process (versioning, changelog, publishing)
- Git workflow (branches, commits, PRs)
- Agent system (Claude Code agents)
- Tools and scripts

**Key files:**
1. `setup-environment.md` - First step for contributors
2. `quality-gates.md` - MANDATORY validation rules
3. `testing-strategy.md` - How we test
4. `ci-cd-pipeline.md` - Automated workflows
5. `release-process.md` - Publishing releases

---

### 5.5 Meta (5 files)

**Purpose:** Navigation, reference, and documentation about documentation.

**Audience:** All users (navigation), documentation maintainers.

**Coverage:**
- Master index (complete navigation)
- Glossary (terminology, acronyms)
- Metrics reference (metrology glossary)
- This consolidation plan
- Archive index (historical documents)

**Key files:**
1. `master-index.md` - Primary entry point
2. `glossary.md` - Consistent terminology
3. `archive-index.md` - Finding historical content

---

## 6. Success Metrics

### 6.1 Quantitative Metrics

| Metric | Before | Target | Measurement |
|--------|--------|--------|-------------|
| Total MD files | 964 | 60 | File count |
| Root MD files | 217 | 5 | File count |
| Subdirectories | 51 | 6 | Directory count |
| Total size | 12MB | ~3MB | du -sh |
| Files per category | 15-90 | 5-15 | Average |
| Documented modules | 0 | 94 | grep "@doc" |
| Broken links | Unknown | 0 | Link checker |
| Navigation depth | 4-5 levels | 2-3 levels | Path depth |

### 6.2 Qualitative Metrics

**Developer Experience:**
- [ ] New developer can build and run in < 30 minutes
- [ ] Clear path from README → getting-started → first working client
- [ ] API reference is single source of truth
- [ ] Examples are copy-paste ready

**Discoverability:**
- [ ] Master index provides role-based navigation
- [ ] Search returns relevant results (not historical clutter)
- [ ] Diataxis framework makes content type clear
- [ ] Breadcrumbs always show where you are

**Maintainability:**
- [ ] Single file per logical topic (no duplication)
- [ ] Clear ownership per file
- [ ] Easy to update (no need to update 10 files for 1 change)
- [ ] Archive preserves history without blocking current work

**Completeness:**
- [ ] Every module has edoc
- [ ] Every public API documented
- [ ] Every guide has working example
- [ ] Every architecture decision explained

---

## 7. Rollout Plan

### 7.1 Communication Strategy

**Before consolidation:**
1. **Announce intent** (GitHub issue, team meeting)
2. **Share this plan** for feedback
3. **Set expectations** about archive availability
4. **Document how to find archived content**

**During consolidation:**
1. **Work on feature branch** `docs/consolidation-2026-01`
2. **Daily progress updates** (GitHub issue comments)
3. **Preview available** via GitHub Pages
4. **Request reviews** after each phase

**After consolidation:**
1. **Announcement** in README and CHANGELOG
2. **Migration guide** for existing links
3. **Archive search** documentation
4. **Feedback period** (1 week before main merge)

### 7.2 Rollback Plan

**If consolidation causes issues:**

1. **Preserve original** in `docs-backup-2026-01-30` branch
2. **Git revert** is always available
3. **Archive remains accessible** even if rolled back
4. **No data loss** (everything in git history)

### 7.3 Timeline

| Phase | Duration | Dependencies | Milestone |
|-------|----------|--------------|-----------|
| Phase 1: Archive root | 2 days | None | Clean root directory |
| Phase 2: Consolidate docs | 3 days | Phase 1 | 50 consolidated files |
| Phase 3: Master index | 1 day | Phase 2 | Navigation complete |
| Phase 4: Update links | 1 day | Phase 3 | Zero broken links |
| Phase 5: Module docs | 2 days | Phase 3 | 94 modules documented |
| Phase 6: Validation | 1 day | All phases | Ready for merge |
| **Total** | **10 days** | - | **Production ready** |

**Buffer:** Add 2 days for unexpected issues (total 12 days / 2.5 weeks).

---

## 8. Post-Consolidation Maintenance

### 8.1 Documentation Standards

**Going forward:**

1. **One file per logical topic** - No duplicates
2. **Diataxis categories** - Clear content type
3. **Master index updates** - Every new file added to index
4. **Link validation** - CI/CD checks for broken links
5. **Module edoc required** - No new modules without @doc
6. **Examples tested** - All code examples must compile

### 8.2 Review Process

**New documentation must:**
- [ ] Fit into existing category structure
- [ ] Not duplicate existing content
- [ ] Update master index
- [ ] Pass markdown linter
- [ ] Have working examples (if applicable)
- [ ] Be reviewed by one other developer

### 8.3 Archive Policy

**Historical documents:**
- Preserved indefinitely in `docs/archive/`
- Searchable via archive index
- Not linked from primary navigation
- Timestamped and categorized
- Available for reference but not maintained

---

## 9. Tools and Automation

### 9.1 Link Checker

```bash
#!/bin/bash
# tools/check-markdown-links.sh

find docs/ -name "*.md" | while read file; do
  echo "Checking $file..."
  grep -oP '\]\(\K[^)]+' "$file" | while read link; do
    if [[ $link == *.md ]]; then
      target=$(dirname "$file")/$link
      if [[ ! -f $target ]]; then
        echo "  BROKEN: $link"
      fi
    fi
  done
done
```

### 9.2 PlantUML Validator

```bash
#!/bin/bash
# tools/check-plantuml-diagrams.sh

find docs/ -name "*.md" | while read file; do
  if grep -q "@startuml" "$file"; then
    echo "Rendering diagrams in $file..."
    plantuml -tsvg -checkonly "$file" || echo "  ERROR in $file"
  fi
done
```

### 9.3 Code Example Validator

```bash
#!/bin/bash
# tools/validate-code-examples.sh

# Extract Erlang code blocks
find docs/ -name "*.md" | while read file; do
  awk '/```erlang/,/```/' "$file" | grep -v '```' > /tmp/example.erl
  if [[ -s /tmp/example.erl ]]; then
    erlc -o /tmp /tmp/example.erl || echo "  ERROR in $file"
  fi
done
```

### 9.4 Edoc Generator

```bash
#!/bin/bash
# tools/generate-edoc-templates.sh

find src/ -name "*.erl" | while read file; do
  if ! grep -q "@doc" "$file"; then
    module=$(basename "$file" .erl)
    echo "Adding edoc template to $module..."
    cat > /tmp/edoc_header.erl <<EOF
%% @doc TODO: Add module description.
%%
%% TODO: Add examples and usage.
%%
%% @see erlmcp_client
-module($module).
EOF
    # Insert after -module() line
    sed -i "/^-module($module)./r /tmp/edoc_header.erl" "$file"
  fi
done
```

---

## 10. Appendices

### 10.1 File Inventory

Complete list of 964 files with categorization will be in:
- `docs/meta/archive-index.md` (after Phase 1)

### 10.2 Link Migration Map

Complete old → new path mapping will be in:
- `docs/meta/link-migration-map.csv` (after Phase 4)

### 10.3 Module Documentation Checklist

List of 94 modules with edoc status will be in:
- `docs/meta/module-documentation-checklist.md` (after Phase 5)

---

## Approval and Sign-off

- [ ] Plan reviewed by maintainers
- [ ] Timeline approved
- [ ] Resources allocated
- [ ] Ready to begin Phase 1

**Plan Version:** 1.0.0
**Date:** 2026-01-30
**Next Review:** After Phase 3 completion

---

**End of Documentation Consolidation Plan**
