# Research: Create Migration Guide from v1 to v2

**Date**: 2026-01-28
**Item**: 047-create-migration-guide-from-v1-to-v2
**Section**: docs
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
Users need guidance upgrading from v1.x to v2.1.0 but no migration guide exists.

**Motivation:** Users may be on v1.x and need clear documentation on how to upgrade to v2.1.0.

**Success criteria:**
- MIGRATION.md created
- Breaking changes documented
- Upgrade steps provided
- Rollback plan documented

**Technical constraints:**
- List all API changes
- List config file changes
- List dependency changes
- Provide step-by-step migration process
- Include code examples
- Include testing checklist

**Signals:** priority: low, urgency: P3 - NICE TO HAVE

### Quality Gate Status
- **Gate Type**: Documentation
- **Current State**: 3 migration-related documents exist (V2_CLEANUP_STRATEGY.md, v2_migration_guide.md, V2_BREAKING_CHANGES.md) but no consolidated MIGRATION.md
- **Target State**: Single comprehensive MIGRATION.md at root level
- **Gap**: Missing unified migration guide that consolidates all v1→v2 changes

## Summary

The manufacturing objective is to create a comprehensive migration guide that consolidates all existing v1 to v2 migration documentation into a single, user-friendly MIGRATION.md file at the project root. This guide must provide clear, step-by-step instructions for users upgrading from v1.x to v2.1.0, including all breaking changes, configuration updates, dependency changes, and rollback procedures.

The technical approach involves:
1. **Consolidation**: Merge information from 3 existing migration docs into one authoritative guide
2. **Structure**: Follow standard migration guide patterns (overview → breaking changes → step-by-step → testing → rollback)
3. **Completeness**: Ensure all API changes, config changes, and dependency changes are documented with code examples
4. **Validation**: Include testing checklist to verify successful migration

The TCPS justification is that this guide serves as **Poka-yoke** (mistake-proofing) for users upgrading their systems. By providing clear, validated migration steps, we prevent configuration errors, API mismatches, and deployment failures that could result in production downtime. The guide embodies **Andon** (visible problem signaling) by clearly identifying breaking changes and potential issues before users encounter them.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/docs/migration/V2_CLEANUP_STRATEGY.md` (1,352 lines) - Technical cleanup strategy for monolith→umbrella migration
  - `/Users/sac/erlmcp/examples/v2_migration_guide.md` (341 lines) - Migration guide specific to examples directory
  - `/Users/sac/erlmcp/docs/V2_BREAKING_CHANGES.md` (524 lines) - Detailed breaking changes documentation
  - `/Users/sac/erlmcp/CHANGELOG.md` (500 lines) - Contains v1.0.0, v2.0.0, and v2.1.0 release notes
  - `/Users/sac/erlmcp/docs/v2/MIGRATION_PLAN.md` (286 lines) - Internal migration plan used during v2 development

- **Patterns**:
  - Migration docs follow **sequential phases** pattern (Phase 1, Phase 2, etc.)
  - **Before/After** code examples for breaking changes
  - **Checklist** format for migration steps
  - **Rollback** procedures documented

- **Tests**: No specific migration tests (documentation only)
- **Quality**: Docs exist but are scattered across multiple files with no single entry point

### Key Files

**Source Documents for Consolidation:**

- `docs/migration/V2_CLEANUP_STRATEGY.md:1-1352` - Complete technical migration strategy (monolith→umbrella)
  - Phase 1-10 execution plan
  - Module mapping tables (v1 → v2 locations)
  - Pre/post-deletion validation scripts
  - Rollback procedures

- `docs/V2_BREAKING_CHANGES.md:1-524` - All breaking changes with code examples
  - File system structure changes
  - Dependency declaration changes
  - Configuration format changes
  - Application start changes
  - Client API changes (capability encoding)
  - Transport behavior changes
  - Removed/deprecated modules
  - Metrology system changes
  - TCPS CLI command changes

- `examples/v2_migration_guide.md:1-341` - Migration guide for example applications
  - Architecture changes
  - Dependency changes
  - Application startup changes
  - Module organization table
  - Quick reference templates

- `CHANGELOG.md:290-500` - Version history and release notes
  - v1.0.0 (2026-01-26) - Initial release
  - v2.0.0 (2026-01-28) - Major release (umbrella architecture)
  - v2.1.0 (2026-01-28) - Minor release (cleanup + testing)

**App Source Files (for version verification):**

- `apps/erlmcp_core/src/erlmcp_core.app.src:1-47` - v2.1.0, depends on jsx, jesse, gproc
- `apps/erlmcp_transports/src/erlmcp_transports.app.src:1-45` - v2.1.0, depends on gun, ranch, poolboy, erlmcp_core
- `apps/erlmcp_observability/src/erlmcp_observability.app.src:1-41` - v2.1.0, depends on opentelemetry_*, erlmcp_core
- `apps/tcps_erlmcp/src/tcps_erlmcp.app.src:1-51` - v2.1.0, depends on bbmustache, cowboy, jobs, fs

**Root Configuration:**

- `rebar.config:1-100` - Umbrella project configuration with 4 apps
  - Lines 44-58: Top-level dependencies
  - Line 30: `project_app_dirs` = ["apps/*"]
  - Lines 69: Shell apps list

### OTP Patterns Observed

- **Behavior**: Umbrella application (4 independent OTP applications)
- **Supervision**: Each app has own supervisor (erlmcp_core_sup, erlmcp_transport_sup, etc.)
- **Process Pattern**: Separation of concerns (core, transports, observability, TCPS)
- **Test Pattern**: Per-app test directories (apps/*/test/)

## Technical Considerations

### Dependencies

**Internal Modules:**
- `erlmcp_core` - Foundation (35 modules: client, server, registry, json_rpc)
- `erlmcp_transports` - Transport layer (22 modules: stdio, tcp, http, ws)
- `erlmcp_observability` - Metrics/OTEL (26 modules: metrics, otel, receipts)
- `tcps_erlmcp` - Quality gates (68 modules: SHACL, jidoka, receipts)

**External Libraries (rebar.config deps):**
- **Core**: jsx 3.1.0, jesse 1.8.1, gproc 0.9.0
- **Transports**: gun 2.0.1, ranch 2.1.0, poolboy 1.5.2
- **Observability**: opentelemetry_api 1.5.0, opentelemetry 1.7.0, opentelemetry_exporter 1.10.0
- **TCPS**: bbmustache 1.12.2, cowboy 2.10.0, jobs 0.10.0, fs 0.9.2
- **Removed**: grpcbox (GraphQL dependency)

**OTP Applications:**
- kernel, stdlib, crypto, ssl, inets (standard OTP)

### TCPS Quality Gates to Pass

- [ ] **Compilation**: MIGRATION.md must be valid Markdown with no broken links
- [ ] **Completeness**: All breaking changes from V2_BREAKING_CHANGES.md included
- [ ] **Accuracy**: Version numbers match actual app versions (v2.1.0)
- [ ] **Code Examples**: All before/after examples are syntactically correct
- [ ] **Testing Checklist**: Migration validation steps are executable
- [ ] **Rollback Plan**: Clear rollback procedure documented
- [ ] **Clarity**: Non-expert users can follow the guide successfully

### Patterns to Follow

**Documentation Structure Pattern:**
```markdown
1. Overview (what and why)
2. Breaking Changes (what changed)
3. Migration Steps (how to upgrade)
4. Configuration Changes (before/after)
5. Code Changes (before/after)
6. Testing Checklist (verify migration)
7. Rollback Plan (if things go wrong)
8. Support Resources (where to get help)
```

**Code Example Pattern:**
```erlang
%% Before (v1.x)
{erlmcp, "1.0.0"}

%% After (v2.1.0)
{erlmcp_core, "2.1.0"},
{erlmcp_transports, "2.1.0"}
```

**Table Pattern:**
| Component | v1.x Location | v2.1.0 Location | Action Required |
|-----------|---------------|-----------------|-----------------|
| erlmcp_client | src/ | apps/erlmcp_core/src/ | Update imports |
| erlmcp_stdio | src/ | apps/erlmcp_transports/src/ | Update imports |

## Root Cause Analysis (5 Whys)

**Problem**: Users migrating from v1.x to v2.1.0 lack a unified migration guide.

1. **Why?** Migration information is scattered across 4 separate documents (V2_CLEANUP_STRATEGY.md, V2_BREAKING_CHANGES.md, v2_migration_guide.md, CHANGELOG.md).

2. **Why?** Each document was created for different audiences:
   - V2_CLEANUP_STRATEGY.md → Internal development team
   - V2_BREAKING_CHANGES.md → Technical architects
   - v2_migration_guide.md → Example developers
   - CHANGELOG.md → Release consumers

3. **Why?** Documentation was written incrementally during v2.0.0 development phases (foundation → extraction → integration).

4. **Why?** No single documentation deliverable was defined in the v2.0.0 release specification (focus was on code migration, not user-facing docs).

5. **ROOT CAUSE**: Missing **MIGRATION.md** deliverable from v2.0.0 release scope. The manufacturing process prioritized code migration over user-facing documentation, creating a gap for external users upgrading from v1.x.

**Solution**: Create MIGRATION.md that consolidates all scattered migration information into a single, user-friendly guide at project root. This addresses the root cause by providing the missing deliverable that should have been part of v2.0.0 release.

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Incomplete documentation** - Missing breaking change or API change | P1 (High) | Users encounter migration failures, production downtime | Cross-reference all 4 source docs, verify against actual v2.1.0 code, create validation checklist |
| **Outdated code examples** - Syntax that doesn't match v2.1.0 | P1 (High) | Users copy broken code, migration fails | Test all code examples by compiling with erlc, verify module names match v2.1.0 app.src files |
| **Conflicting information** - Source docs disagree on changes | P2 (Medium) | User confusion, which document to trust? | Use V2_BREAKING_CHANGES.md as source of truth (most detailed), resolve conflicts by checking actual code |
| **Missing rollback plan** - No way to revert failed migration | P1 (High) | Users stuck in broken state, cannot recover | Include detailed rollback section with commands to revert to v1.x, document rollback triggers |
| **Version mismatch** - Document mentions v2.0.0 but current is v2.1.0 | P2 (Medium) | Users confused about which version to target | Clearly state guide covers v1.x → v2.1.0, note that v2.0.0 → v2.1.0 is non-breaking |
| **Testing gaps** - No way to verify migration succeeded | P2 (Medium) | Silent failures, undetected issues | Include migration testing checklist with specific commands (rebar3 compile, eunit, dialyzer) |
| **Link rot** - References to deleted src/ directories | P3 (Low) | Broken links in guide | Use relative links to apps/ structure, verify all links exist before publishing |

**Severity Definitions:**
- **P1 (High):** Blocks successful migration - MUST fix before publishing
- **P2 (Medium):** Causes confusion or incomplete migration - SHOULD fix
- **P3 (Low):** Minor inconvenience - NICE to have

## Recommended Manufacturing Approach

**TCPS Methodology:**

1. **Specification** - Define MIGRATION.md structure and content requirements
2. **Pseudocode** - Outline sections and subsections before writing
3. **Architecture** - Identify source material (4 existing docs) and consolidation strategy
4. **Refinement** - Draft guide, validate code examples, cross-check for completeness
5. **Completion** - Review against quality gates, test migration steps, publish

**Implementation Strategy:**

**Phase 1: Specification (15 minutes)**
- Define target audience: Users upgrading from v1.x to v2.1.0
- Outline sections: Overview → Breaking Changes → Migration → Testing → Rollback
- Set quality gates: Completeness, accuracy, clarity, testability

**Phase 2: Content Extraction (30 minutes)**
- Extract breaking changes from V2_BREAKING_CHANGES.md (10 categories)
- Extract migration steps from V2_CLEANUP_STRATEGY.md (Phases 1-6)
- Extract code examples from v2_migration_guide.md (before/after patterns)
- Extract version history from CHANGELOG.md (v1.0.0 → v2.0.0 → v2.1.0)

**Phase 3: Consolidation (60 minutes)**
- Write MIGRATION.md with unified structure
- Consolidate duplicate information (e.g., module renames mentioned in multiple docs)
- Resolve conflicts (e.g., different descriptions of same change)
- Add cross-references between sections

**Phase 4: Validation (30 minutes)**
- Verify all breaking changes are included (checklist: 10 categories from V2_BREAKING_CHANGES.md)
- Test all code examples by running through erlc
- Verify version numbers match v2.1.0 app.src files
- Check all file paths exist (use Glob tool)

**Phase 5: Quality Gates (15 minutes)**
- [ ] Completeness: All 10 breaking change categories documented
- [ ] Accuracy: Version numbers = v2.1.0, paths point to apps/
- [ ] Code Examples: All compile successfully
- [ ] Testing Checklist: All commands executable (rebar3 compile, etc.)
- [ ] Rollback Plan: Clear steps to revert to v1.x
- [ ] Clarity: Non-expert can follow guide (readability test)

**Quality Validation:**

- **Automated**: None (documentation only)
- **Manual**:
  - Check all Markdown links resolve: `find . -name "MIGRATION.md" -exec grep -o '\[.*\](.*)' {} \;`
  - Compile all code examples: `erlc -pa /tmp -verify_examples.erl`
  - Verify file paths exist: `ls apps/erlmcp_core/src/erlmcp_client.erl`

- **Metrics**:
  - Document length: Target 500-800 lines (comprehensive but readable)
  - Code examples: ≥15 before/after pairs
  - Tables: ≥3 summary tables (breaking changes, module map, dependency changes)

## Open Questions
**NONE** - All research complete. Source documents identified, structure defined, quality gates specified.

## Manufacturing Checklist
- [x] Root cause identified (missing MIGRATION.md deliverable from v2.0.0)
- [x] Quality gates defined (completeness, accuracy, clarity, testability)
- [x] OTP patterns understood (umbrella with 4 apps, independent supervision)
- [x] Test strategy clear (validation checklist, code example compilation)
- [x] Risk assessment complete (7 risks identified with P1/P2/P3 severity)
- [x] No open questions (all research complete)

---

**Research Complete**
**Next Step**: Execute manufacturing approach (Phase 1-5) to create MIGRATION.md
