# MCP Specification Compliance Strategy

## Overview

This document describes the GitHub Actions workflow strategy for ensuring **erlmcp** maintains full compliance with the Model Context Protocol (MCP) specification version **2025-11-25**.

**Reference:** `docs/MCP_SPECIFICATION_COMPLETE.md`

---

## MCP Specification Requirements Analysis

### 1. Release/Versioning Requirements (Section 16)

**From Spec:**
- Protocol version: `2025-11-25` (current stable)
- Version format: `YYYY-MM-DD`
- Version negotiation during `initialize` handshake
- Version history tracking with breaking change documentation

**Implementation:**
- Dual versioning: erlmcp version (SemVer) + MCP protocol version (date-based)
- Protocol version declared in:
  - `erlmcp_json_rpc.erl`
  - `erlmcp_capabilities.erl`
  - `erlmcp_client.erl` (initialize request)
  - `erlmcp_server.erl` (initialize response)

### 2. Backward Compatibility Constraints (Section 3)

**From Spec:**
- Capability negotiation ensures opt-in feature adoption
- Clients MUST handle unknown capabilities gracefully
- Error codes MUST remain stable within defined ranges
- Transport layer MUST maintain framing compatibility
- JSON-RPC 2.0 is the stable foundation (never changes)

**Implementation:**
- Capabilities can be added (backward compatible)
- Capabilities cannot be removed (breaking change)
- Error code ranges are immutable:
  - JSON-RPC standard: `-32700` to `-32603`
  - MCP custom: `-32001` to `-32113`
  - Refusal codes: `1001` to `1089`

### 3. Breaking Change Detection Requirements

**From Spec (Section 14 - Compliance Checklists):**

#### Critical Breaking Changes:
1. **Initialize Flow** - Any change to initialization handshake
2. **JSON-RPC Structure** - Removal of required fields (`jsonrpc`, `id`, `method`, `params`)
3. **Capability Removal** - Removing any declared capability
4. **Method Removal** - Removing any MCP method
5. **Error Code Changes** - Changing error code values
6. **Transport Behavior** - Changing transport callback signatures
7. **Message Framing** - Changing framing format (line-delimited, etc.)
8. **Message Size Limits** - Changing 16 MB default limit

#### Allowed (Non-Breaking):
1. Adding new capabilities
2. Adding new error codes (within unused ranges)
3. Adding optional fields to responses
4. Adding new transports
5. Performance optimizations (preserving behavior)

### 4. Deployment Validation Checkpoints (Section 14)

**From Spec - Server Implementation Checklist:**

| Checkpoint | Requirement | Test Method |
|------------|-------------|-------------|
| Initialize | Method implemented and enforced | Unit tests |
| Capabilities | All declared capabilities work | Validation suite |
| Error codes | Match specification | Code analysis |
| Notifications | Sent correctly | Integration tests |
| Message size | 16 MB limit enforced | Size validation |
| Request IDs | No reuse within session | State tracking |
| Timeouts | 5s default, 30s init | Benchmark tests |
| JSON-RPC 2.0 | Full compliance | Protocol validator |
| Transports | All tested (STDIO, TCP, HTTP, WS, SSE) | CT suites |
| Security | Input validation, auth | Security validator |
| Performance | < 1ms latency p99 | Benchmarks |
| Error recovery | Exponential backoff works | Chaos tests |

---

## GitHub Actions Workflow Architecture

### Workflow 1: `mcp-compliance.yml` - Core Compliance

**Purpose:** Comprehensive MCP specification compliance validation

**Trigger:**
- Push to `main`, `release/**`
- Pull requests to `main`
- Daily scheduled run (2 AM UTC)
- Manual dispatch with validation levels

**Jobs:**

#### Job 1: `compile` (BLOCKING)
- Multi-OTP compilation (25, 26, 27)
- Verifies all umbrella apps build
- **Gate:** Errors = 0

#### Job 2: `unit-tests` (BLOCKING)
- EUnit test suite across OTP versions
- Code coverage generation
- **Gate:** Failures = 0

#### Job 3: `coverage` (BLOCKING)
- Coverage report generation
- Threshold validation
- **Gate:** Coverage ≥ 80%

#### Job 4: `dialyzer` (BLOCKING)
- Type checking with PLT caching
- **Gate:** Warnings = 0

#### Job 5: `xref` (ADVISORY)
- Cross-reference analysis
- Undefined function detection
- **Gate:** Advisory (non-blocking)

#### Job 6: `mcp-compliance` (BLOCKING)
- Protocol validator (JSON-RPC 2.0 + MCP methods)
- Transport validator (all 5 transports)
- Security validator (auth + input validation)
- Performance validator (throughput + latency)
- **Gate:** All validators pass

#### Job 7: `benchmarks`
- Quick performance benchmarks
- Regression detection
- **Gate:** Performance within bounds

#### Job 8: `compliance-report`
- Aggregates all validation results
- Generates markdown report
- Posts to PR as comment
- Creates job summary

#### Job 9: `compliance-gate` (BLOCKING)
- Final gate evaluation
- All blocking jobs must pass
- **Decision:** MERGE ALLOWED / BLOCKED

#### Job 10: `integration-tests` (CONDITIONAL)
- Common Test suites
- Full integration validation
- **Trigger:** Full validation level or scheduled

**Artifacts:**
- EUnit results (14 days)
- Coverage reports (30 days)
- Compliance report (90 days)
- Benchmark results (14 days)
- Compliance badge SVG (90 days)

---

### Workflow 2: `release-compliance.yml` - Release Validation

**Purpose:** Validate release readiness and versioning compliance

**Trigger:**
- Push to `release/**` branches
- Version tags `v*`
- Pull requests to `release/**`
- Manual dispatch with version inputs

**Jobs:**

#### Job 1: `version-consistency`
- Extracts versions from codebase
- Validates SemVer format (erlmcp version)
- Validates date format (MCP protocol version)
- Cross-checks version declarations in multiple files
- **Output:** Version metadata for downstream jobs

#### Job 2: `protocol-version-check`
- Verifies MCP protocol version in all required modules
- Ensures `2025-11-25` appears in:
  - `erlmcp_json_rpc.erl`
  - `erlmcp_capabilities.erl`
  - `erlmcp_client.erl`
  - `erlmcp_server.erl`
- **Gate:** All files declare correct protocol version

#### Job 3: `backward-compatibility`
- Detects capability changes (removals = breaking)
- Detects error code changes (value changes = breaking)
- Detects transport interface changes (callback changes = breaking)
- Generates compatibility report
- **Gate:** No backward compatibility violations

#### Job 4: `release-checklist`
- Validates CHANGELOG.md updated
- Validates README.md updated
- Validates version bumped in `.app.src`
- Runs full test suite
- Generates release artifacts (tarball)
- **Gate:** All checklist items complete

#### Job 5: `release-gate` (FINAL)
- Evaluates all release validation jobs
- **Decision:** RELEASE APPROVED / BLOCKED

**Artifacts:**
- Backward compatibility report (90 days)
- Release artifacts (90 days)

---

### Workflow 3: `breaking-changes.yml` - Breaking Change Detection

**Purpose:** Automatic detection and validation of breaking changes

**Trigger:**
- Pull requests to `main`, `release/**`
- Push to `release/**` branches

**Jobs:**

#### Job 1: `api-surface-analysis`
- **Check 1:** Initialize flow changes
  - Detects modifications to `initialize` method
  - Flags removed initialization logic

- **Check 2:** JSON-RPC message structure
  - Detects removal of required fields
  - Validates `jsonrpc`, `id`, `method`, `params`, `result`, `error`

- **Check 3:** Capability removals
  - Compares capability declarations
  - Flags any removed capabilities

- **Check 4:** MCP method removals
  - Validates all core methods present:
    - `initialize`, `tools/list`, `tools/call`
    - `resources/list`, `resources/read`
    - `prompts/list`, `prompts/get`

- **Check 5:** Error code stability
  - Detects error code value changes
  - Validates ranges preserved

- **Check 6:** Transport behavior changes
  - Checks callback signature modifications
  - Flags breaking transport changes

- **Check 7:** Message framing changes
  - Detects framing logic modifications
  - Flags potential breaking changes

- **Check 8:** Message size limits
  - Validates 16 MB limit preserved
  - Flags spec non-compliance

- **Output:**
  - `has_breaking_changes` (boolean)
  - `breaking_change_count` (integer)
  - Detailed report in job summary

#### Job 2: `version-bump-check` (CONDITIONAL)
- **Trigger:** Only if breaking changes detected
- Validates MAJOR version bump (SemVer)
- Compares current vs previous version
- **Gate:** Major version incremented

#### Job 3: `changelog-check` (CONDITIONAL)
- **Trigger:** Only if breaking changes detected
- Validates CHANGELOG.md contains "BREAKING CHANGE"
- Ensures migration guide present
- **Gate:** Breaking changes documented

#### Job 4: `breaking-change-gate` (FINAL)
- Evaluates all breaking change validations
- If no breaking changes: ✅ PASS
- If breaking changes with proper handling: ✅ PASS
- If breaking changes without proper handling: ❌ FAIL

**Artifacts:**
- Breaking change analysis report

---

## Compliance Gate Enforcement Strategy

### Blocking Gates (Must Pass)

```
1. Compilation
2. Unit Tests
3. Coverage ≥ 80%
4. Dialyzer
5. MCP Compliance Validators
6. Version Consistency (releases only)
7. Backward Compatibility (releases only)
8. Breaking Change Validation (if applicable)
```

### Advisory Gates (Warnings Only)

```
1. Xref (undefined functions)
2. Benchmark regressions (if not performance code)
3. Message framing changes (manual review)
```

### Merge Decision Matrix

| Condition | Action |
|-----------|--------|
| All blocking gates PASS | ✅ MERGE ALLOWED |
| Any blocking gate FAIL | ❌ MERGE BLOCKED |
| Breaking changes + proper handling | ✅ MERGE ALLOWED (with review) |
| Breaking changes + improper handling | ❌ MERGE BLOCKED |
| Advisory warnings only | ✅ MERGE ALLOWED (with review) |

---

## MCP Specification Compliance Mapping

### Section 2: Protocol Fundamentals

| Requirement | Validation | Workflow Job |
|-------------|------------|--------------|
| JSON-RPC 2.0 structure | Protocol validator | `mcp-compliance` → `protocol` |
| UTF-8 encoding | Message parser tests | `unit-tests` |
| Message size limit (16 MB) | Size validator | `mcp-compliance` → `protocol` |
| Timeout handling (5s/30s) | Benchmark tests | `benchmarks` |

### Section 3: Initialization & Capability Negotiation

| Requirement | Validation | Workflow Job |
|-------------|------------|--------------|
| `initialize` first message | Protocol validator | `mcp-compliance` → `protocol` |
| Protocol version `2025-11-25` | Version check | `release-compliance` → `protocol-version-check` |
| Capability negotiation | Capability validator | `mcp-compliance` → `protocol` |
| `initialized` notification | Integration tests | `integration-tests` |

### Section 4: Core Capabilities

| Requirement | Validation | Workflow Job |
|-------------|------------|--------------|
| Tools (list, call) | Capability validator | `mcp-compliance` → `protocol` |
| Resources (list, read, subscribe) | Capability validator | `mcp-compliance` → `protocol` |
| Prompts (list, get) | Capability validator | `mcp-compliance` → `protocol` |
| All 30+ methods | Method coverage check | `breaking-changes` → `api-surface-analysis` |

### Section 5: Transport Layer

| Requirement | Validation | Workflow Job |
|-------------|------------|--------------|
| STDIO (line-delimited) | Transport validator | `mcp-compliance` → `transport` |
| TCP (Ranch) | Transport validator | `mcp-compliance` → `transport` |
| HTTP/SSE (Cowboy) | Transport validator | `mcp-compliance` → `transport` |
| WebSocket (mcp.v1) | Transport validator | `mcp-compliance` → `transport` |
| Behavior contract | Breaking change detector | `breaking-changes` → `api-surface-analysis` |

### Section 7: Error Handling & Status Codes

| Requirement | Validation | Workflow Job |
|-------------|------------|--------------|
| JSON-RPC errors (-32xxx) | Error code validator | `mcp-compliance` → `protocol` |
| MCP errors (-32001 to -32113) | Error range check | `release-compliance` → `backward-compatibility` |
| Refusal codes (1001-1089) | Refusal range check | `release-compliance` → `backward-compatibility` |
| Error code stability | Breaking change detector | `breaking-changes` → `api-surface-analysis` |

### Section 10: Security & Authorization

| Requirement | Validation | Workflow Job |
|-------------|------------|--------------|
| Authentication (API key, JWT, OAuth, mTLS) | Security validator | `mcp-compliance` → `security` |
| Input validation (JSON Schema) | Security validator | `mcp-compliance` → `security` |
| URI validation (path traversal) | Security validator | `mcp-compliance` → `security` |
| JOSE library present | Dependency check | `release-compliance` → `release-checklist` |

### Section 11: Performance & Benchmarks

| Requirement | Validation | Workflow Job |
|-------------|------------|--------------|
| Throughput (43K msg/s TCP) | Benchmark suite | `benchmarks` |
| Latency (p99 < 1ms) | Performance validator | `mcp-compliance` → `performance` |
| Scalability (40-50K conn) | Load tests | `integration-tests` |

### Section 14: Compliance Checklists

| Requirement | Validation | Workflow Job |
|-------------|------------|--------------|
| Initialize implemented | Protocol validator | `mcp-compliance` → `protocol` |
| All capabilities work | Capability validator | `mcp-compliance` → `protocol` |
| Error codes match spec | Error validator | `release-compliance` → `backward-compatibility` |
| Notifications correct | Integration tests | `integration-tests` |
| Message size enforced | Size validator | `mcp-compliance` → `protocol` |
| Request ID tracking | State validator | `mcp-compliance` → `protocol` |
| Timeout handling | Performance validator | `mcp-compliance` → `performance` |
| JSON-RPC 2.0 compliance | Protocol validator | `mcp-compliance` → `protocol` |
| All transports tested | Transport validator | `mcp-compliance` → `transport` |
| Security validation | Security validator | `mcp-compliance` → `security` |
| Performance targets | Performance validator | `mcp-compliance` → `performance` |
| Error recovery | Chaos tests | `benchmarks` (chaos module) |

---

## Workflow Execution Examples

### Example 1: Feature Branch → Main PR

```
Trigger: Pull request to main

Workflows:
1. mcp-compliance.yml
   - Compile ✅
   - Unit tests ✅
   - Coverage 85% ✅
   - Dialyzer ✅
   - MCP compliance ✅
   - Benchmarks ✅
   - Compliance gate: PASS

2. breaking-changes.yml
   - API surface analysis: No breaking changes ✅
   - Breaking change gate: PASS

Result: ✅ MERGE ALLOWED
```

### Example 2: Breaking Change PR

```
Trigger: Pull request with capability removal

Workflows:
1. breaking-changes.yml
   - API surface analysis: 1 breaking change detected ❌
   - Capability "tools" removed
   - Version bump check: MAJOR version not bumped ❌
   - CHANGELOG check: No BREAKING CHANGE notice ❌
   - Breaking change gate: FAIL

Result: ❌ MERGE BLOCKED
Required actions:
  1. Bump version 2.1.0 → 3.0.0
  2. Add BREAKING CHANGE to CHANGELOG.md
  3. Document migration guide
```

### Example 3: Release Branch

```
Trigger: Push to release/v2.2.0

Workflows:
1. mcp-compliance.yml
   - All gates ✅

2. release-compliance.yml
   - Version consistency ✅
   - Protocol version check ✅
   - Backward compatibility ✅
   - Release checklist ✅
   - Release gate: PASS

3. breaking-changes.yml
   - No breaking changes ✅

Result: ✅ RELEASE APPROVED
Artifacts:
  - Release tarball
  - Compliance report
  - Compatibility report
```

---

## Continuous Improvement

### Daily Scheduled Validation

**Purpose:** Detect drift from spec compliance

**Schedule:** 2 AM UTC daily

**Validation Level:** Full

**Actions:**
1. Run complete MCP compliance suite
2. Run all integration tests
3. Generate comprehensive report
4. Alert on failures

### Version Update Strategy

When MCP spec updates (e.g., `2026-XX-XX`):

1. **Create epic branch:** `epic/mcp-2026-XX-XX`
2. **Update protocol version** in all required files
3. **Run compliance workflows** to detect gaps
4. **Update validators** for new requirements
5. **Update tests** for new features
6. **Breaking change analysis** for incompatibilities
7. **Release planning** with migration guide

---

## Metrics & Reporting

### Compliance Dashboard Metrics

1. **Compliance Score:** % of passing gates
2. **Breaking Change Frequency:** Changes per release
3. **Test Coverage Trend:** Coverage over time
4. **Performance Regression:** Benchmark trends
5. **Error Code Stability:** Unchanged count
6. **Transport Coverage:** All transports tested

### Artifact Retention

| Artifact | Retention | Purpose |
|----------|-----------|---------|
| Compliance reports | 90 days | Audit trail |
| Release artifacts | 90 days | Deployment |
| Compatibility reports | 90 days | Migration planning |
| Coverage reports | 30 days | Quality tracking |
| Test results | 14 days | Debugging |
| Benchmark results | 14 days | Performance tracking |

---

## References

1. **MCP Specification:** `docs/MCP_SPECIFICATION_COMPLETE.md`
2. **CLAUDE.md:** Project-specific compliance rules
3. **GitHub Actions:** `.github/workflows/`
4. **Validation Modules:** `apps/erlmcp_validation/src/`
5. **Benchmark Suite:** `apps/erlmcp_observability/src/erlmcp_bench_*.erl`

---

## Summary

This compliance strategy ensures **erlmcp** maintains 100% MCP specification compliance through:

1. **Automated validation** at every commit
2. **Breaking change detection** before merge
3. **Release validation** with comprehensive checks
4. **Continuous monitoring** via daily schedules
5. **Audit trail** via artifacts and reports

**Philosophy:** Make non-compliance impossible to merge (Poka-Yoke principle).

**Result:** Production-ready MCP SDK with guaranteed specification compliance.
