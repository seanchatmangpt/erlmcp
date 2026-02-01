# erlmcp Documentation Guide

**Last Updated**: 2026-02-01
**Status**: ✅ Current and Maintained
**Version**: v2.1.0

---

## Quick Navigation

### 🎯 For New Contributors

Start here for project orientation:
1. **README.md** - Project overview, quick start, and key features
2. **CLAUDE.md** - System architecture, OTP patterns, and development standards
3. **DEVELOPMENT.md** - Environment setup and development workflow
4. **CONTRIBUTING.md** - Contribution guidelines and code standards

### 📚 For Specific Topics

| Topic | Location |
|-------|----------|
| Cloud Execution & Autonomous Development | `docs/CLOUD_EXECUTION.md` |
| API Reference (MCP endpoints) | `archive/misc/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md` |
| Quality Gates & Testing | `archive/quality-reports/` (22 files) |
| Performance/Benchmarks | `archive/benchmarks/` (16 files) |
| Feature Implementation | `archive/phase-reports/` (44 files) |
| Deployment Strategy | `archive/tasks/DEPLOYMENT_GUIDE_100X.md` |
| Production Readiness | `archive/tasks/PRODUCTION_READINESS_*` |
| Release Process | `archive/strategy/RELEASE_STRATEGY.md` |
| Security Fixes | `archive/troubleshooting/` (7 files) |
| Formal Specification | `/docs/protocol.md` or `CLAUDE.md` |
| OTP 28 Quick Reference | `docs/OTP_28_QUICK_REFERENCE.md` |
| Migration Notes | `docs/MIGRATION_NOTES.md` |

### 🔍 Archive Categories

```
archive/
├── phase-reports/       ← Feature implementations, release evidence
├── quality-reports/     ← Testing strategies, coverage analysis
├── benchmarks/          ← Performance results, execution guides
├── troubleshooting/     ← Security fixes, critical resolutions
├── strategy/            ← Release strategy (1 file)
├── tasks/               ← Production deployment guides (13 files)
└── misc/                ← API references, integration plans (46 files)
```

---

## Documentation Structure

### Root Level (10 Essential Files)

These are the **core operational documents** you'll use most often:

#### 1. **CLAUDE.md** (1,000+ lines)
- **What**: System specification, architecture, development standards
- **Who**: Architects, senior developers, reviewers
- **Use Case**: Understanding system design, OTP patterns, quality gates
- **Status**: ✅ Current (v2.1.0)

#### 2. **README.md**
- **What**: Project overview, quick start, version info
- **Who**: New contributors, project overview
- **Use Case**: Initial onboarding, feature summary
- **Status**: ✅ Current (OTP 28.3.1+)

#### 3. **DEVELOPMENT.md**
- **What**: Development environment setup, workflow
- **Who**: Developers setting up local environment
- **Use Case**: Installing dependencies, running builds
- **Status**: ✅ Updated (OTP 28.3.1+)

#### 4. **CHANGELOG.md**
- **What**: Release history and version notes
- **Who**: Project managers, users
- **Use Case**: Understanding what changed between versions
- **Status**: ✅ Current

#### 5. **CONTRIBUTING.md**
- **What**: Contribution guidelines, code standards
- **Who**: Contributors
- **Use Case**: Understanding PR process, code requirements
- **Status**: ✅ Current

#### 6-10. **Reference Guides**
- `CODE_QUALITY_REPORT_V2.1.md` - Latest quality metrics
- `CODE_REVIEW_QUICK_REFERENCE.md` - Code review checklist
- `CLI_USAGE.md` - Command-line tool docs
- `DOCUMENTATION_GUIDE.md` - This navigation guide
- `RELEASE_NOTES_v2.1.0.md` - Latest release details

### Archive/ (150 Retained Files)

After audit and cleanup, we retained only files with ongoing reference value.

#### Critical Tier (Do Not Delete)

**Release & Quality**:
- `RELEASE_v2.1.0_EVIDENCE.md` - Official v2.1.0 release artifact
- `AGENT_TEAM_FINAL_STATUS.md` - Latest (2026-01-31) project status
- `QUALITY_GATE_REPORT_v2.1.0.md` - v2.1.0 quality assessment

**API & Architecture**:
- `MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md` - Comprehensive MCP API (31KB)
- `GGEN_INTEGRATION_PLAN.md` - Large integration architecture (73KB)

#### Reference Tier (Ongoing Use)

**Testing & Quality** (23 files):
- Testing strategy and best practices
- Coverage analysis and gap reports
- Quality assessment methodology

**Performance** (16 files):
- Benchmark execution guides
- Performance validation procedures
- OTP 28 baseline results

**Feature Documentation** (44 files):
- Implementation details for core features
- Architecture decisions (Registry, Supervision, etc.)
- Feature deliverables and proof of delivery

**Deployment** (13 files):
- Production deployment procedures
- Cluster configuration guides
- Readiness scoring and dashboards

**Security** (7 files):
- JWT and Authorization fixes
- Security assessment reports
- Blocker resolution summaries

### /docs/ (917+ Files)

Official documentation directory with subdirectories:

```
docs/
├── architecture/        ← System design, OTP patterns
├── api-reference/       ← Endpoint specifications
├── protocol/            ← MCP specification
├── testing/             ← Test methodologies
├── deployment/          ← Deployment guides
├── metrology/           ← Metrics glossary
├── benchmarks/          ← Historical benchmark reports
├── quality-gates/       ← Quality procedures
├── tcps/                ← TCPS documentation
├── security/            ← Security audits
├── observability/       ← Telemetry documentation
├── MIGRATION_NOTES.md   ← Documentation reorganization guide (2026-01-31)
└── OTP_28_QUICK_REFERENCE.md ← OTP 28+ benchmark quick reference
```

---

## What Was Deleted & Why

### Files Removed (222 deleted)

We removed files with no ongoing reference value:

| Category | Count | Reason |
|----------|-------|--------|
| Old versions (v0.6.0, v1.5.0, v2.0) | 7 | Superseded by v2.1.0 |
| Business strategy | 9 | No technical reference value |
| Interim fix reports | 22 | Once integrated, no longer needed |
| Test run snapshots | 12 | Superseded by latest test runs |
| Temporary setup guides | 16 | Environment-specific, transient |
| Interim analyses/audits | 120+ | Point-in-time reports, superseded |
| **Total** | **222** | Clean up transient documentation |

### Files Retained (150 retained)

We kept files with ongoing reference or policy value:

| Category | Count | Reason |
|----------|-------|--------|
| Release/quality evidence | 3 | Official audit trail |
| Feature implementations | 44 | Technical reference |
| Testing methodology | 23 | Best practices |
| Performance baselines | 16 | Performance tracking |
| Deployment guides | 13 | Operational procedures |
| Security/compliance | 7 | Policy implications |
| API references | 4 | Technical specification |
| **Total** | **150** | Valuable reference material |

---

## How to Find Things

### By Topic

**"How do I set up my environment?"**
→ `DEVELOPMENT.md`

**"What are the MCP endpoints?"**
→ `archive/misc/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md`

**"How do I deploy to production?"**
→ `archive/tasks/DEPLOYMENT_GUIDE_100X.md`

**"What are the quality standards?"**
→ `archive/quality-reports/QUALITY_GATE_REPORT_v2.1.0.md`

**"What's the system architecture?"**
→ `CLAUDE.md` or `docs/architecture/`

**"How do I run benchmarks?"**
→ `archive/benchmarks/BENCHMARK_EXECUTION_GUIDE.md` or `docs/OTP_28_QUICK_REFERENCE.md`

**"What security fixes were applied?"**
→ `archive/troubleshooting/` (7 files)

**"Was there a specific feature implemented?"**
→ `archive/phase-reports/` (search for feature name)

**"What was the documentation reorganization about?"**
→ `docs/MIGRATION_NOTES.md`

### By Development Phase

**Initial Setup**:
1. README.md
2. DEVELOPMENT.md
3. CONTRIBUTING.md

**Understanding Architecture**:
1. CLAUDE.md
2. docs/architecture/
3. archive/misc/GGEN_INTEGRATION_PLAN.md

**Feature Development**:
1. archive/phase-reports/ (implementation docs)
2. archive/misc/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md
3. docs/protocol.md (specification)

**Before Deployment**:
1. archive/tasks/PRODUCTION_READINESS_SCORE.md
2. archive/quality-reports/QUALITY_GATE_REPORT_v2.1.0.md
3. archive/tasks/DEPLOYMENT_GUIDE_100X.md

**Performance Tuning**:
1. archive/benchmarks/BENCHMARK_EXECUTION_GUIDE.md
2. archive/benchmarks/OTP_28_BENCHMARK_SUMMARY.md
3. CLAUDE.md (baseline metrics in metrology section)
4. docs/OTP_28_QUICK_REFERENCE.md (quick commands)

---

## Documentation Standards

### Maintained Documents (Green ✅)
- Root-level files (updated regularly)
- `/docs/` directory (official specifications)
- Core feature implementations in `archive/phase-reports/`
- Quality standards in `archive/quality-reports/`

### Reference Documents (Yellow ⚠️)
- Archive files (historical reference, not updated)
- Benchmark results (point-in-time measurements)
- Deployment guides (review before each major deploy)

### Archived Documents (Gray 📦)
- Old version docs (v0.6.0, v1.5.0, v2.0)
- Interim reports (superseded by final versions)
- One-time fixes (integrated into codebase)
- Temporary guides (environment-specific)

---

## File Counts & Cleanup Status

```
Root-level:     10 files (essential only)
archive/:       150 files (reference material)
/docs/:         917+ files (official documentation)
.claude/:       ~30 files (agent/command specs)
.roo/:          30 files (role definitions)
.wreckit/:      82 files (work item tracking)
────────────────────────────────────
Total:          ~1,200 active files
```

**Cleanup Status**: ✅ Complete
- Root clutter: Eliminated (382 → 10 files)
- Archive quality: Filtered (372 → 150 files)
- Old versions: Removed (v0.6.0, v1.5.0, v2.0)
- Transient docs: Deleted (220 files)

---

## Maintenance & Updates

### Who Updates What

| Document | Owner | Frequency |
|----------|-------|-----------|
| README.md | Project Lead | Per release |
| DEVELOPMENT.md | Dev Lead | Per major change |
| CLAUDE.md | Architecture | Quarterly |
| CHANGELOG.md | Release Manager | Per release |
| /docs/ | Documentation Team | Per feature |
| archive/ | None (immutable) | N/A |
| CONTRIBUTING.md | Project Lead | Per policy change |

### Adding New Documentation

**Should this be a root-level file?**
- ✅ YES if: Core reference (architecture, setup, standards)
- ❌ NO if: Feature-specific or temporary

**Should it go in /docs/?**
- ✅ YES if: Official specification or guide
- ❌ NO if: Temporary or point-in-time report

**Should it go in archive/?**
- Only if retained for reference after archival cleanup
- Add only during deliberate archival process

---

## Archive Index

For detailed information about all archived files:

→ **See archive/README.md** (comprehensive guide with file descriptions)

---

## Questions?

- **Architecture questions** → CLAUDE.md
- **How-to questions** → Relevant guide in /docs/ or archive/
- **Setup questions** → DEVELOPMENT.md
- **Contribution questions** → CONTRIBUTING.md
- **Past decisions** → Archive files (search by topic)

---

**Last Audit**: 2026-01-31
**Cleanup Update**: 2026-02-01 (6 files moved to proper locations)
**Next Review**: Q2 2026
**Status**: ✅ Clean and Current
