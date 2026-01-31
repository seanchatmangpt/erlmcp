# erlmcp Quick Start Documentation

**Last Updated**: 2026-01-31
**Version**: v2.1.0
**Purpose**: Fast answers to common questions with direct links

---

## Getting Started

### "I'm new to the project, where do I start?"

1. **README.md** - Project overview, features, quick start
2. **DEVELOPMENT.md** - Environment setup, dependencies, build instructions
3. **CLAUDE.md** - System architecture, patterns, standards (deep dive)
4. **CONTRIBUTING.md** - How to contribute, PR process

**Time to productivity**: ~30 minutes (setup) + 2 hours (architecture understanding)

### "How do I set up my development environment?"

**Primary**: `DEVELOPMENT.md`

**Quick commands**:
```bash
# Install Erlang/OTP 28.3.1+
asdf install erlang 28.3.1

# Build project
rebar3 compile

# Run tests
rebar3 eunit
rebar3 ct

# Quality gates
make check
```

**Also see**:
- `archive/tasks/CLUSTER_SETUP.md` - Multi-node development
- `archive/tasks/DOCKER_GUIDE.md` - Docker-based development

### "What are the code standards?"

**Primary**: `CLAUDE.md` (sections: "OTP Pattern Enforcement", "Code Style Gates", "Development Invariants")

**Quick reference**: `CODE_REVIEW_QUICK_REFERENCE.md`

**Key rules**:
- Chicago School TDD (NO mocks/fakes)
- Black-box testing only
- Coverage ≥80%
- All gen_servers must be supervised
- init/1 never blocks

### "What's the latest release status?"

**Primary**: `RELEASE_NOTES_v2.1.0.md`

**Also see**:
- `CHANGELOG.md` - Full release history
- `CODE_QUALITY_REPORT_V2.1.md` - v2.1.0 quality metrics
- `archive/phase-reports/RELEASE_v2.1.0_EVIDENCE.md` - Complete release evidence

---

## Architecture & Design

### "What's the system architecture?"

**Primary**: `CLAUDE.md` (sections: "System Topology", "Supervision Hierarchy", "Architecture Invariants")

**Deep dive**:
- `docs/architecture/` - Detailed architecture docs
- `archive/misc/GGEN_INTEGRATION_PLAN.md` - Integration architecture (73KB)
- `archive/misc/100X_ARCHITECTURE_INDEX.md` - 100K connections architecture

**Quick summary**:
- 164 modules across 4 apps (core, transports, observability, validation)
- 3-tier supervision hierarchy
- Process-per-connection model
- gproc-based routing (O(log N))

### "How does supervision work?"

**Primary**: `CLAUDE.md` (section: "Supervision Hierarchy (3-Tier Invariant)")

**Pattern**:
```
TIER₁ (one_for_all): Registry + Infrastructure
  └─ TIER₂ (simple_one_for_one): Protocol servers
      └─ TIER₃ (isolated): Observability
```

**Also see**: `docs/architecture/` for supervision tree diagrams

### "What OTP patterns are used?"

**Primary**: `CLAUDE.md` (section: "OTP Pattern Enforcement")

**Reference**: `OTP_28_QUICK_REFERENCE.md` - OTP 28+ features

**Core patterns**:
- gen_server for all protocol handlers
- supervisor for all process trees
- gen_statem for complex state machines
- Process monitoring/linking for critical dependencies

### "How does the transport layer work?"

**Primary**: `CLAUDE.md` (section: "Transport Behavior (τ-Interface)")

**Template**: `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`

**Supported transports**: stdio, TCP, HTTP, WebSocket, SSE

**Also see**: `archive/benchmarks/TRANSPORT_BENCHMARKS_DELIVERABLE.md`

---

## Development & Testing

### "How do I run tests?"

**Quick commands**:
```bash
# Unit tests (specific module)
rebar3 eunit --module=erlmcp_server_tests

# All unit tests
rebar3 eunit

# Common Test suites
rebar3 ct --suite=test/erlmcp_lifecycle_SUITE

# All tests
make check
```

**Also see**:
- `archive/quality-reports/TEST_ASSESSMENT_QUICK_REF.md`
- `CLAUDE.md` (section: "Command Operators")

### "What's the test strategy?"

**Primary**: `CLAUDE.md` (section: "Development Invariants (Chicago School TDD)")

**Principles**:
- Black-box testing only (no mocks!)
- Real erlmcp processes in all tests
- Test all interfaces (JSON-RPC, stdio, HTTP, WS, TCP)
- Property-based testing with PropEr
- Coverage ≥80% (enforced)

**Also see**:
- `archive/quality-reports/TEST_COVERAGE_CROSS_VALIDATION_REPORT.md`
- `archive/quality-reports/TEST_ASSESSMENT_EXECUTIVE_SUMMARY.md`
- `archive/misc/BLACK_BOX_VALIDATION_REPORT.md`

### "How do I debug a test failure?"

**Primary**: `CLAUDE.md` (section: "Quick Index (Task → File Mapping)")

**Debug commands**:
```bash
# Verbose test output
rebar3 eunit --module=MODULE --verbose

# CT with detailed logging
rebar3 ct --suite=SUITE --verbose

# Observer (visual process inspection)
make observer
```

**Also see**:
- `archive/troubleshooting/` - 7 security/bug fix reports
- `archive/quality-reports/TEST_FIXES_SUMMARY.md`

### "I found a bug, how do I fix it?"

**Process**:
1. Write failing test first (TDD)
2. Fix implementation
3. Verify test passes
4. Run full quality gates: `make check`
5. Submit PR with test evidence

**Templates**: `apps/*/test/*_tests.erl` - Pattern examples

**Also see**:
- `CONTRIBUTING.md` - PR guidelines
- `CODE_REVIEW_QUICK_REFERENCE.md` - Review checklist

### "How do I add a new feature?"

**Primary**: `CONTRIBUTING.md`

**Process**:
1. Review existing features in `archive/phase-reports/`
2. Write tests (black-box, all interfaces)
3. Implement gen_server/supervisor
4. Update documentation
5. Run quality gates
6. Submit PR

**Feature examples**: `archive/phase-reports/` (44 implementation docs)

---

## Deployment & Operations

### "How do I deploy to production?"

**Primary**: `archive/tasks/DEPLOYMENT_GUIDE_100X.md`

**Pre-deployment checklist**:
- `archive/tasks/PRODUCTION_DEPLOYMENT_CHECKLIST.md`
- `archive/tasks/PRODUCTION_READINESS_SCORE.md`
- `archive/quality-reports/QUALITY_GATE_REPORT_v2.1.0.md`

**Deployment types**:
- Single node: `archive/tasks/DEPLOYMENT_INDEX.md`
- Cluster: `archive/tasks/CLUSTER_SETUP.md`
- Docker: `archive/tasks/DOCKER_GUIDE.md`
- Quick start: `archive/tasks/CLUSTER_QUICK_START.md`

### "How do I configure clustering?"

**Primary**: `archive/tasks/CLUSTER_SETUP.md`

**Quick start**: `archive/tasks/CLUSTER_QUICK_START.md`

**Architecture**: `archive/misc/100X_ARCHITECTURE_INDEX.md`

**Also see**:
- Session replication: `docs/SESSION_PERSISTENCE.md`
- Distributed registry: `erlmcp_registry_dist.erl`

### "How do I set up secrets management?"

**Primary**: `docs/SECRETS_MANAGEMENT.md`

**Quick start**: `archive/tasks/VAULT_QUICKSTART.md`

**Backends**:
- HashiCorp Vault (production)
- AWS Secrets Manager
- Local encrypted (fallback)

**Config example**: `CLAUDE.md` (section: "Secrets Management (Vault Abstraction)")

### "How do I monitor the system?"

**Primary**: `archive/tasks/DASHBOARD_QUICKSTART.md`

**Tools**:
- Health dashboard: `erlmcp_dashboard_server.erl`
- Metrics endpoint: `/metrics`
- Observer: `make observer`
- Production readiness: `archive/tasks/PRODUCTION_READINESS_DASHBOARD.md`

**Also see**: `CLAUDE.md` (section: "Observability Architecture")

---

## Performance & Optimization

### "What are the performance baselines?"

**Primary**: `CLAUDE.md` (section: "Performance Baseline (Jan 2026)")

**Quick numbers** (v2.1.0):
- Registry: 553K msg/s
- Queue: 971K msg/s
- Pool: 149K msg/s
- Session: 242K msg/s
- Network I/O: 43K msg/s
- Sustained: 372K msg/s
- Capacity: 40-50K concurrent connections/node

**Detailed reports**:
- `archive/benchmarks/OTP_28_BENCHMARK_SUMMARY.md`
- `archive/benchmarks/PERFORMANCE_ANALYSIS_REPORT.md`

### "How do I run benchmarks?"

**Primary**: `archive/benchmarks/BENCHMARK_EXECUTION_GUIDE.md`

**Quick commands**:
```bash
# Quick benchmarks
make benchmark-quick

# Full suite (10-15 min)
./scripts/bench/run_all_benchmarks.sh

# Specific benchmark
erlmcp_bench_core_ops:run(<<"core_ops_100k">>)
```

**Also see**:
- `archive/benchmarks/BENCHMARK_PERFORMANCE_ANALYSIS.md`
- `CLAUDE.md` (section: "Benchmark Suite (v1.5.0)")

### "How do I validate performance?"

**Primary**: `archive/benchmarks/PERFORMANCE_VALIDATION_SUMMARY.md`

**Tools**:
- Performance validator: `erlmcp_performance_validator.erl`
- Profiler: `erlmcp_profiler.erl`
- Memory analyzer: `erlmcp_memory_analyzer.erl`

**Also see**:
- `archive/benchmarks/PERFORMANCE_VALIDATOR_REPORT.md`
- `archive/benchmarks/PERFORMANCE_REGRESSION_CI_CD_SUMMARY.md`

### "How do I test resilience?"

**Primary**: `CLAUDE.md` (section: "Chaos Engineering (Controlled Failure Injection)")

**Tools**:
- Chaos module: `erlmcp_chaos.erl`
- 11 failure scenarios
- Recovery target: <5s

**Reports**:
- `archive/benchmarks/CHAOS_TESTS_REPORT.md`
- `archive/misc/CHAOS_TESTS_SUMMARY.md`
- `archive/benchmarks/STRESS_TEST_CHECKLIST.md`

**Stress tests**:
- `archive/benchmarks/STRESS_TEST_QUICKSTART.md`
- `archive/benchmarks/STRESS_TEST_RETEST_FINAL_REPORT.md`

---

## API & Protocol Reference

### "What are the MCP endpoints?"

**Primary**: `archive/misc/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md` (31KB comprehensive guide)

**Categories**:
- Lifecycle (initialize, ping, shutdown)
- Resources (list, read, subscribe, unsubscribe)
- Tools (list, call)
- Prompts (list, get)
- Sampling (createMessage, listChanged)
- Completion (complete)
- Logging (setLevel)
- Roots (list)

**Also see**: `docs/protocol.md` - MCP specification

### "How do I implement a new transport?"

**Primary**: `apps/erlmcp_transports/src/erlmcp_transport_tcp.erl` (template)

**Behavior contract**: `CLAUDE.md` (section: "Transport Implementation Contract")

**Callbacks required**: init/2, send/2, close/1

**Registration**: Automatic via `erlmcp_transport_registry`

**Also see**: `archive/misc/TRANSPORT_IMPLEMENTATION_GUIDE.md` (if exists)

### "What's the message format?"

**Primary**: `CLAUDE.md` (section: "Protocol Layer")

**Encoding**: JSON-RPC 2.0

**Key modules**:
- `erlmcp_json_rpc.erl` - Encode/decode with validation
- `erlmcp_message_parser.erl` - Parsing
- `erlmcp_message_size.erl` - Size validation (DoS protection)

### "How do I add authentication?"

**Primary**: `docs/SECRETS_MANAGEMENT.md`

**Modules**:
- `erlmcp_auth.erl` - Authentication/authorization
- `erlmcp_auth_mtls.erl` - Mutual TLS
- `erlmcp_auth_rate_limiter.erl` - Auth-specific rate limiting

**Also see**: `archive/misc/BATCH6_AUTH_REPORT.md`

---

## Troubleshooting & Debugging

### "Tests are failing, where do I start?"

**Process**:
1. Run verbose: `rebar3 eunit --module=MODULE --verbose`
2. Check recent fixes: `archive/quality-reports/TEST_FIXES_SUMMARY.md`
3. Review test patterns in `apps/*/test/`
4. Check for known issues in `archive/troubleshooting/`

**Common issues**:
- Mock usage (violates Chicago School)
- Blocking init/1 (use async cast)
- Missing supervision
- Untested transport interfaces

### "Compilation is failing, what's wrong?"

**Quick check**:
```bash
TERM=dumb rebar3 compile
```

**Common issues**:
- Missing dependencies (check `rebar.config`)
- Dialyzer type errors (`rebar3 dialyzer`)
- Undefined functions (`rebar3 xref`)

**Also see**: `archive/misc/COMPILATION_STATUS.md`

### "Performance is degraded, how do I diagnose?"

**Tools**:
1. `make observer` - Visual process inspection
2. `erlmcp_profiler.erl` - CPU profiling
3. `erlmcp_memory_analyzer.erl` - Memory analysis
4. `/metrics` endpoint - Real-time metrics

**Baselines**: `archive/benchmarks/OTP_28_BENCHMARK_SUMMARY.md`

**Also see**: `archive/benchmarks/PERFORMANCE_REGRESSION_CI_CD_SUMMARY.md`

### "Where are security fix reports?"

**Primary**: `archive/troubleshooting/` (7 files)

**Critical fixes**:
- JWT validation
- Authorization bugs
- Input validation
- Rate limiting

**Also see**: `archive/misc/BATCH6_AUTH_REPORT.md`

---

## Past Decisions & History

### "Can I see past implementation decisions?"

**Primary**: `archive/phase-reports/` (44 feature implementation docs)

**Categories**:
- Feature deliverables
- Architecture decisions
- Implementation evidence
- Release summaries

**Also see**: `CHANGELOG.md` - Version-by-version changes

### "What were the major milestones?"

**Timeline**:
- v0.6.0: Initial release
- v1.5.0: Benchmark suite, chaos testing
- v2.0.0: Production-ready features
- v2.1.0: Current release (2026-01-31)

**Evidence**:
- `archive/phase-reports/RELEASE_v2.1.0_EVIDENCE.md`
- `RELEASE_NOTES_v2.1.0.md`
- `archive/phase-reports/AGENT_TEAM_FINAL_STATUS.md` (latest)

### "How did we decide on the architecture?"

**Primary**: `archive/misc/GGEN_INTEGRATION_PLAN.md` (73KB architecture reasoning)

**Key decisions**:
- Process-per-connection (isolation)
- gproc registry (O(log N) lookup)
- 3-tier supervision (fault isolation)
- Transport polymorphism (behavior pattern)

**Also see**: `CLAUDE.md` (section: "Architecture Invariants")

### "What quality gates were applied?"

**Primary**: `archive/quality-reports/QUALITY_GATE_REPORT_v2.1.0.md`

**Gates**:
- Compilation (errors = 0)
- Tests (pass_rate = 1.0)
- Coverage (≥80%)
- Dialyzer (warnings → 0)
- Xref (undefined = ∅)
- Benchmarks (regression < 10%)

**Also see**:
- `archive/quality-reports/QUALITY_GATES_IMPLEMENTATION.md`
- `CLAUDE.md` (section: "Quality Gates (Mandatory Gate Sequence)")

### "Were there any major refactorings?"

**Primary**: `archive/quality-reports/` (22 files documenting quality improvements)

**Notable refactorings**:
- SSE notification system: `archive/quality-reports/SSE_NOTIFICATION_REFACTORING_SUMMARY.md`
- Transport validation: `archive/quality-reports/CT_INTEGRATION_TEST_ANALYSIS.md`
- Error handling: `archive/misc/ERROR_TESTING_SUMMARY.md`

---

## Advanced Topics

### "How does session persistence work?"

**Primary**: `docs/SESSION_PERSISTENCE.md`

**Backends**:
- ETS (in-memory, fastest, O(1))
- DETS (disk, durable)
- Mnesia (cluster, distributed)

**Config example**: `CLAUDE.md` (section: "Session Persistence Backend Hierarchy")

**Modules**:
- `erlmcp_session_backend.erl` - Behavior interface
- `erlmcp_session_manager.erl` - Lifecycle
- `erlmcp_session_failover.erl` - Failover orchestration

### "How does distributed registry work?"

**Primary**: `erlmcp_registry_dist.erl`

**Architecture**:
- gproc-based routing
- O(log N) lookup
- Cross-node coordination
- Split-brain detection: `erlmcp_split_brain_detector.erl`

**Also see**: `archive/misc/100X_ARCHITECTURE_INDEX.md`

### "How do I integrate with LLMs?"

**Providers**:
- `erlmcp_llm_provider_anthropic.erl` - Claude
- `erlmcp_llm_provider_openai.erl` - GPT
- `erlmcp_llm_provider_local.erl` - Local models
- `erlmcp_mock_llm.erl` - Testing

**Also see**: `archive/misc/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md`

### "How does the receipt chain work?"

**Primary**: `erlmcp_receipt_chain.erl`

**Purpose**: Immutable audit trail with cryptographic proof

**Use cases**:
- Compliance tracking
- Evidence collection
- Tamper detection

**Also see**: `archive/misc/EVIDENCE_COLLECTION_SUMMARY.md`

### "What's the Toyota Production System (TPS) integration?"

**Primary**: `CLAUDE.md` (section: "Toyota Production System as Quality Operators")

**Principles**:
- Andon (visible error signaling): `erlmcp_health_monitor`
- Poka-Yoke (mistake-proofing): Schema validation, bounded errors
- Jidoka (built-in quality): Pre-commit hooks, CI/CD gates
- Kaizen (continuous improvement): Chaos engineering, benchmarks

**Implementation**: `tcps_quality_gates.erl`

---

## Documentation Organization

### "How is documentation organized?"

**Primary**: `DOCUMENTATION_GUIDE.md` (comprehensive guide)

**Structure**:
```
Root/              10 essential files (README, CLAUDE.md, etc.)
docs/              917 official specification files
archive/           150 reference/historical files
  ├── phase-reports/      44 feature implementations
  ├── quality-reports/    23 testing/quality docs
  ├── benchmarks/         16 performance reports
  ├── tasks/              13 deployment guides
  ├── troubleshooting/     7 security/bug fixes
  ├── misc/               46 API/integration docs
  └── strategy/            1 release strategy
.claude/           ~30 agent/command specs
.roo/              30 role definitions
.wreckit/          82 work item tracking
```

### "Where should I add new documentation?"

**Decision tree**:
- Core reference? → Root level (but ask first, root is minimal)
- Official spec/guide? → `/docs/`
- Feature implementation? → During development, consider archival later
- Temporary report? → Transient (don't commit unless valuable)

**Also see**: `DOCUMENTATION_GUIDE.md` (section: "Adding New Documentation")

### "What documentation standards exist?"

**Primary**: `DOCUMENTATION_GUIDE.md` (section: "Documentation Standards")

**Status indicators**:
- Green: Actively maintained (root-level, /docs/)
- Yellow: Reference only (archive/)
- Gray: Archived (historical)

**Update frequency**: See `DOCUMENTATION_GUIDE.md` (section: "Who Updates What")

---

## Quick Command Reference

### Quality Gates
```bash
make check              # Full quality gate (compile + xref + dialyzer + tests)
TERM=dumb rebar3 compile  # Clean compilation check
rebar3 eunit            # Unit tests
rebar3 ct               # Common Test suites
rebar3 dialyzer         # Type checking
rebar3 xref             # Cross-reference analysis
```

### Benchmarks
```bash
make benchmark-quick    # Quick performance validation
./scripts/bench/run_all_benchmarks.sh  # Full suite (10-15 min)
```

### Development
```bash
make console            # Erlang REPL
make observer           # Visual process inspector
recon:info/1            # Runtime process inspection
```

### Deployment
```bash
rebar3 as prod release  # Production release
rebar3 as prod tar      # Distribution tarball
```

---

## Getting Help

### "I can't find what I need, where do I look?"

**Search order**:
1. This file (QUICK_START_DOCS.md) - FAQ
2. `DOCUMENTATION_GUIDE.md` - Comprehensive index
3. `CLAUDE.md` - System architecture and standards
4. `archive/` - Historical reference (use grep/ripgrep)
5. `/docs/` - Official specifications

**Grep examples**:
```bash
# Find all mentions of "session"
rg -i "session" archive/

# Find benchmark reports
rg -i "benchmark" archive/benchmarks/

# Find deployment guides
rg -i "deploy" archive/tasks/
```

### "Who do I ask about..."

**Architecture**: See `CLAUDE.md` (definitive reference)
**Testing**: See `archive/quality-reports/`
**Performance**: See `archive/benchmarks/`
**Deployment**: See `archive/tasks/`
**API/Protocol**: See `archive/misc/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md`
**Bugs**: See `archive/troubleshooting/`

---

## Document Status

**Created**: 2026-01-31
**Status**: Active Quick Reference
**Complements**: DOCUMENTATION_GUIDE.md (comprehensive index)
**Purpose**: Fast answers, scannable format, direct links
**Maintenance**: Update when new major features/docs added

**Feedback**: If you can't find an answer here, it should be added!

---

**Pro Tip**: Use `Ctrl+F` (or `/` in vim) to search this document for keywords.
