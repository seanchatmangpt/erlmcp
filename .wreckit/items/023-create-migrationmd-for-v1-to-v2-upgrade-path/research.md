# Research: Create MIGRATION.md for v1 to v2 upgrade path

**Date**: 2026-01-28
**Item**: 023-create-migrationmd-for-v1-to-v2-upgrade-path
**Section**: docs
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
No migration guide exists for v1 to v2 upgrade

**Motivation:** Users need clear documentation on how to upgrade from v1.x to v2.1.0. Breaking changes must be clearly documented.

**Success criteria:**
- MIGRATION.md created
- Breaking changes documented
- Upgrade steps provided with code examples
- Rollback plan documented

**Technical constraints:**
- Document all API changes
- Document config file changes
- Document dependency changes
- Provide step-by-step process
- Include testing checklist

**Signals:** priority: low, urgency: P3 - NICE TO HAVE

### Quality Gate Status
- **Gate Type**: Documentation/Quality
- **Current State**: No comprehensive migration guide exists for v1.x → v2.1.0
- **Target State**: Complete migration guide with all breaking changes, code examples, and rollback procedures
- **Gap**: Missing structured documentation for major version upgrade

## Summary

This research identifies the need for a comprehensive migration guide to help users upgrade from erlmcp v1.x (monolithic structure) to v2.1.0 (umbrella application architecture). The v2.0.0 release represented a major architectural transformation from a single-application structure to a 4-app umbrella, introducing breaking changes in repository structure, module organization, application startup, and dependency management.

The manufacturing objective is to create a **single, comprehensive MIGRATION.md** document that consolidates all existing migration information (currently scattered across multiple files in CHANGELOG.md, docs/v2/MIGRATION_PLAN.md, docs/library-migration-guide.md, and examples/v2_migration_guide.md) into a user-friendly, step-by-step guide.

The technical approach involves:
1. **Documenting all breaking changes** from v1.x → v2.1.0
2. **Providing code examples** for before/after comparisons
3. **Creating upgrade scripts** where possible
4. **Including rollback procedures** for failed migrations
5. **Adding testing checklist** to verify successful upgrade

The TCPS justification follows **Jidoka** (stop-the-line quality) and **Poka-yoke** (mistake-proofing) principles - a clear migration guide prevents user errors and ensures successful upgrades, reducing support burden and improving user satisfaction.

## Current State Analysis

### Existing Implementation
- **Files**:
  - `/Users/sac/erlmcp/CHANGELOG.md` - Contains v1.0.0, v2.0.0, v2.1.0 release notes with some migration info
  - `/Users/sac/erlmcp/docs/v2/MIGRATION_PLAN.md` - Developer-focused migration plan (228 modules to 4 apps)
  - `/Users/sac/erlmcp/docs/library-migration-guide.md` - v0.5 → v0.6.0 library integration (gproc, gun, ranch, poolboy)
  - `/Users/sac/erlmcp/examples/v2_migration_guide.md` - Example migration guide (module names unchanged, app startup changes)
  - `/Users/sac/erlmcp/examples/README.md` - v2.0 umbrella structure explanation

- **Patterns**: Documentation exists but is **fragmented** across multiple files
- **Tests**: No migration validation tests exist
- **Quality**: No single source of truth for v1 → v2 migration

### Key Files
- `CHANGELOG.md:143-287` - v2.0.0 breaking changes and upgrade instructions
- `CHANGELOG.md:39-141` - v2.1.0 release notes (non-breaking minor release)
- `docs/v2/MIGRATION_PLAN.md:8-112` - Module distribution mapping (v1 → v2)
- `docs/v2/MIGRATION_PLAN.md:197-220` - Breaking changes for library users
- `docs/library-migration-guide.md:1-755` - v0.5 → v0.6.0 library migration (gproc, gun, ranch, poolboy)
- `examples/v2_migration_guide.md:1-341` - Example migration with rebar.config changes

### OTP Patterns Observed
- **Behavior**: Application migration from monolithic to umbrella structure
- **Supervision**: 4 independent OTP applications with separate supervision trees
- **Process Pattern**: Each app has own application:start/2 callback
- **Test Pattern**: No migration tests exist (gap)

## Technical Considerations

### Dependencies
- **Internal Modules**:
  - `erlmcp_core` (35 modules) - Core MCP protocol, JSON-RPC, client/server, registry
  - `erlmcp_transports` (22 modules) - TCP, HTTP, WebSocket, STDIO transports
  - `erlmcp_observability` (26 modules) - OTEL, metrics, receipts
  - `tcps_erlmcp` (68 modules) - TCPS quality gates, SHACL (OPTIONAL)

- **External Libraries** (from `/Users/sac/erlmcp/rebar.config:44-58`):
  - **v1.x dependencies**: jsx 3.1.0, jesse 1.8.1, gproc 0.9.0, gun 2.0.1, ranch 2.1.0, poolboy 1.5.2
  - **v2.1.0 dependencies**: Same core libs + bbmustache 1.12.2, cowboy 2.10.0, opentelemetry_api 1.5.0, opentelemetry 1.7.0, opentelemetry_exporter 1.10.0, jobs 0.10.0, fs 0.9.2
  - **New in v2**: OpenTelemetry stack, bbmustache (templating), fs (file system monitoring)

- **OTP Applications**: kernel, stdlib, sasl, mnesia (unchanged)

### TCPS Quality Gates to Pass
- [ ] **Documentation**: Complete migration guide with all sections
- [ ] **Code Examples**: All before/after comparisons provided
- [ ] **Testing Checklist**: Comprehensive verification steps
- [ ] **Rollback Plan**: Clear procedure to revert changes
- [ ] **Accuracy**: All breaking changes documented

### Patterns to Follow
- **Migration Plan Structure**: Reference `docs/v2/MIGRATION_PLAN.md` (detailed module mapping)
- **Library Migration**: Reference `docs/library-migration-guide.md` (dependency changes)
- **Example Migration**: Reference `examples/v2_migration_guide.md` (rebar.config changes)
- **Changelog Format**: Reference `CHANGELOG.md` (release note structure)

## Breaking Changes Analysis (v1.x → v2.1.0)

### 1. Repository Structure (CRITICAL)

**v1.x Structure:**
```
erlmcp/
├── src/          (228 modules - ALL in one app)
├── test/         (343 test files)
├── rebar.config  (single app config)
```

**v2.1.0 Structure:**
```
erlmcp/
├── apps/
│   ├── erlmcp_core/          (35 modules)
│   ├── erlmcp_transports/    (22 modules)
│   ├── erlmcp_observability/ (26 modules)
│   └── tcps_erlmcp/          (68 modules - OPTIONAL)
├── rebar.config              (umbrella config)
└── config/sys.config
```

**Impact**: Users cannot directly copy modules from `src/` to v2. Must use umbrella structure.

### 2. Module Organization

| v1.x Location | v2.1.0 App | Module Examples |
|---------------|------------|-----------------|
| `src/erlmcp_client.erl` | `erlmcp_core` | `erlmcp_client`, `erlmcp_server`, `erlmcp_registry` |
| `src/erlmcp_stdio.erl` | `erlmcp_transports` | `erlmcp_transport_stdio`, `erlmcp_transport_tcp` |
| `src/erlmcp_metrics.erl` | `erlmcp_observability` | `erlmcp_metrics`, `erlmcp_otel` |
| N/A | `tcps_erlmcp` | TCPS modules (NEW in v2) |

**Impact**: Include paths change. Module names **unchanged** (backward compatible).

### 3. Application Startup (BREAKING CHANGE)

**v1.x (implicit):**
```erlang
%% Modules available directly
erlmcp_client:start_link({stdio, []}, #{}).
```

**v2.1.0 (explicit app dependencies):**
```erlang
%% MUST start applications first
application:ensure_all_started(erlmcp_core),
application:ensure_all_started(erlmcp_transports),
erlmcp_client:start_link({stdio, []}, #{}).
```

**Impact**: All user code must add explicit `application:ensure_all_started/1` calls.

### 4. Dependency Declaration (BREAKING CHANGE)

**v1.x rebar.config:**
```erlang
{deps, [
    {erlmcp, "1.0.0"}
]}.
```

**v2.1.0 rebar.config:**
```erlang
{deps, [
    {erlmcp_core, "2.1.0"},
    {erlmcp_transports, "2.1.0"},
    erlmcp_observability, "2.1.0"}  % Optional
]}.
```

**Impact**: Users must update dependency declarations in their applications.

### 5. Transport Configuration (ENHANCED)

**v1.x:**
```erlang
Config = #{type => tcp, host => "localhost", port => 8080}.
```

**v2.1.0 (ranch-enhanced):**
```erlang
Config = #{
    type => tcp,
    mode => client,            % NEW: client or server
    host => "localhost",
    port => 8080,
    num_acceptors => 10,       % NEW: ranch acceptor pool
    max_connections => 1000,   % NEW: ranch connection limit
    keepalive => true,
    nodelay => true
}.
```

**Impact**: Optional enhancements (backward compatible).

### 6. GraphQL Transport (REMOVED)

**v1.x:**
```erlang
erlmcp_transport_graphql:start_link(...).  % Available
```

**v2.1.0:**
```erlang
%% GraphQL transport REMOVED due to unstable grpcbox dependency
%% Use erlmcp_transport_http with JSON-RPC instead
```

**Impact**: GraphQL users must migrate to HTTP transport.

### 7. Configuration Structure (NEW)

**v1.x:** Minimal configuration in `erlmcp.app.src`

**v2.1.0:** Comprehensive `config/sys.config`:
```erlang
{erlmcp, [
    {log_level, info},
    {client_defaults, #{timeout => 5000}},
    {server_defaults, #{max_subscriptions_per_resource => 1000}},
    {transport_defaults, #{
        tcp => #{connect_timeout => 5000},
        http => #{request_timeout => 30000}
    }},
    {http_security, [...]},
    {localhost_binding, [...]},
    {https_config, [...]}
]}.
```

**Impact**: Users should review new configuration options.

### 8. Build System (CHANGED)

**v1.x:**
```bash
rebar3 compile
rebar3 release
```

**v2.1.0:**
```bash
# From root
rebar3 compile              # Builds all 4 apps
rebar3 as prod release      # Creates production release

# Per-app (optional)
rebar3 compile --app erlmcp_core
```

**Impact**: Build commands unchanged, but umbrella-aware.

## Root Cause Analysis (5 Whys)

**Problem**: No comprehensive migration guide for v1.x → v2.1.0

1. **Why?** Migration information is scattered across multiple files (CHANGELOG.md, MIGRATION_PLAN.md, library-migration-guide.md, examples/v2_migration_guide.md)

2. **Why?** Each document was created for different audiences:
   - CHANGELOG.md: Release notes for all users
   - MIGRATION_PLAN.md: Internal developer migration plan
   - library-migration-guide.md: v0.5 → v0.6.0 library integration
   - examples/v2_migration_guide.md: Example-specific migration

3. **Why?** Documentation was written incrementally during v2.0 development, not as a cohesive user-facing guide

4. **Why?** No documentation requirement for "user-facing migration guide" in the v2.0 release checklist (only internal migration plan)

5. **ROOT CAUSE**: Missing deliverable - No dedicated MIGRATION.md file that consolidates all migration information into a single, user-friendly guide

**Solution**: Create `/Users/sac/erlmcp/MIGRATION.md` that:
- Consolidates all breaking changes from scattered sources
- Provides step-by-step upgrade instructions
- Includes code examples for all breaking changes
- Documents rollback procedures
- Adds testing checklist

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Incomplete breaking changes documentation** | P1 (High) | Users encounter unexpected errors during upgrade | Comprehensive audit of all v1.x → v2.1.0 changes in CHANGELOG.md, MIGRATION_PLAN.md, and codebase |
| **Incorrect code examples** | P1 (High) | Users follow broken examples, migration fails | Verify all code examples by actually running them before publishing |
| **Missing rollback procedures** | P2 (Medium) | Users cannot recover from failed upgrades | Document clear rollback steps (revert to v1.x, restore backups) |
| **GraphQL migration unclear** | P2 (Medium) | GraphQL users stuck on v1.x | Provide specific HTTP+JSON-RPC migration path with examples |
| **Config changes not documented** | P2 (Medium) | Runtime errors due to missing config | Document all new sys.config options with defaults |
| **Application startup errors** | P1 (High) | Users get "undefined function" errors | Emphasize `application:ensure_all_started/1` requirement prominently |
| **Dependency conflicts** | P2 (Medium) | Version conflicts with user's other deps | Document exact versions required and conflict resolution |
| **Testing inadequate** | P3 (Low) | Migration guide has errors | Peer review by another developer, test in clean environment |

## Recommended Manufacturing Approach

**TCPS Methodology:**
1. **Specification** - Define MIGRATION.md structure and content requirements
2. **Pseudocode** - Outline sections: Overview, Breaking Changes, Upgrade Steps, Testing, Rollback
3. **Architecture** - Reference existing docs (CHANGELOG.md, MIGRATION_PLAN.md, library-migration-guide.md)
4. **Refinement** - Write user-friendly guide with code examples
5. **Completion** - Peer review and accuracy verification

**Implementation Strategy:**

**Phase 1: Information Consolidation**
1. Extract all breaking changes from CHANGELOG.md (v2.0.0 section)
2. Extract module mapping from docs/v2/MIGRATION_PLAN.md
3. Extract library changes from docs/library-migration-guide.md
4. Extract example changes from examples/v2_migration_guide.md

**Phase 2: Documentation Creation**
1. Create `/Users/sac/erlmcp/MIGRATION.md` with sections:
   - **Overview** - What changed and why
   - **Breaking Changes** - Detailed list with code examples
   - **Step-by-Step Upgrade** - Numbered procedure
   - **Configuration Migration** - sys.config changes
   - **Testing Checklist** - Verification steps
   - **Rollback Procedures** - How to revert
   - **Troubleshooting** - Common issues and solutions

2. For each breaking change:
   - Show v1.x code (before)
   - Show v2.1.0 code (after)
   - Explain impact
   - Provide migration snippet

**Phase 3: Quality Validation**
1. **Accuracy Check**: Verify all code examples compile
2. **Completeness Check**: Ensure all breaking changes covered
3. **Clarity Check**: Peer review for user-friendliness
4. **Testing Check**: Follow guide in clean environment

**Quality Validation:**
- **Automated**: No automated tests for documentation (manual review only)
- **Manual**: Peer review by 2+ developers
- **Metrics**:
  - All breaking changes documented (100%)
  - Code examples verified (100%)
  - Peer review approval (required)

## Open Questions
**NONE** - All source materials identified and analyzed

## Manufacturing Checklist
- [x] Root cause identified (not symptoms) - Missing MIGRATION.md deliverable
- [x] Quality gates defined (documentation completeness, accuracy)
- [x] OTP patterns understood (umbrella application migration)
- [x] Test strategy clear (manual verification, peer review)
- [x] Risk assessment complete (8 risks identified with P1-P3 severity)
- [x] No open questions (all research complete)

---

**Status**: RESEARCH COMPLETE ✅

**Next Steps**: Proceed to create `/Users/sac/erlmcp/MIGRATION.md` following the recommended manufacturing approach.
