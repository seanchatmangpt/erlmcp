# erlmcp v2.1 Planning Index

**Purpose:** Central navigation for all v2.1 planning documents
**Status:** COMPLETE
**Date:** 2026-01-27

---

## Quick Links

| Document | Purpose | Size | Status |
|----------|---------|------|--------|
| **[ROADMAP.md](ROADMAP.md)** | 4-phase roadmap, timeline, priorities | 518 lines | ✅ Complete |
| **[FEATURE_PROPOSALS.md](FEATURE_PROPOSALS.md)** | 13 detailed technical designs | 1,627 lines | ✅ Complete |
| **[PLANNING_SUMMARY.md](PLANNING_SUMMARY.md)** | Analysis process, insights, next steps | 500+ lines | ✅ Complete |

**Total Documentation:** 2,645+ lines (69KB+)

---

## Document Purpose

### ROADMAP.md - Strategic Plan
**Use This For:**
- Understanding v2.1 timeline (6-8 weeks)
- Viewing top 20 prioritized features
- Seeing success metrics per phase
- Understanding risk mitigation strategies
- Resource planning (team size, dependencies)

**Audience:** Project managers, technical leads, stakeholders

**Key Sections:**
- Executive Summary
- Top 5 Critical Risks (from v2.0)
- Known Issues Classification (P0-P3)
- 4-Phase Roadmap (Critical → Performance → Features → DX)
- Feature Priority Matrix
- Success Metrics
- Risk Assessment

### FEATURE_PROPOSALS.md - Technical Designs
**Use This For:**
- Understanding how features will be implemented
- Reviewing technical architecture decisions
- Seeing API designs and code examples
- Understanding testing strategies
- Estimating effort for specific features

**Audience:** Engineers, architects, technical reviewers

**Key Sections:**
- 13 detailed feature proposals
- Code examples (50+ snippets)
- Testing strategies (unit, integration, property, benchmark)
- Risk analysis per feature
- Effort estimates with dependencies
- Concrete deliverables (65+)

### PLANNING_SUMMARY.md - Analysis Report
**Use This For:**
- Understanding how priorities were determined
- Reviewing analysis methodology
- Seeing key insights and recommendations
- Comparing v2.0 vs. v2.1 (planned)
- Getting high-level overview

**Audience:** Decision makers, reviewers, retrospective

**Key Sections:**
- Deliverables Summary
- Analysis Process (risks, issues, performance, DX)
- Feature Prioritization Methodology
- Resource Planning (team, timeline, dependencies)
- Success Metrics
- Key Insights (5 major takeaways)

---

## Reading Guide

### For Project Managers
1. Start with **PLANNING_SUMMARY.md** (executive summary)
2. Review **ROADMAP.md** Phase 1 (critical fixes)
3. Check success metrics and timeline
4. Review resource planning (team size)

### For Engineers
1. Start with **ROADMAP.md** (find your phase)
2. Deep-dive **FEATURE_PROPOSALS.md** (your assigned features)
3. Review code examples and testing strategies
4. Check dependencies and effort estimates

### For Stakeholders
1. Read **PLANNING_SUMMARY.md** (insights and comparison)
2. Review **ROADMAP.md** success metrics
3. Understand timeline and risks
4. See v2.0 vs. v2.1 improvements

### For Reviewers
1. All 3 documents (full context)
2. Validate prioritization methodology
3. Review effort estimates
4. Check risk mitigation strategies

---

## v2.1 at a Glance

### Timeline
- **Phase 1:** Week 1-2 (Critical Fixes)
- **Phase 2:** Week 3-4 (Performance)
- **Phase 3:** Week 5-6 (New Features)
- **Phase 4:** Week 7-8 (Developer Experience)
- **Total:** 6-8 weeks (March 2026 target)

### Top Priorities
1. Fix client capability encoding (P0, 1 hour)
2. Migrate test suites (P0, 2-3 days)
3. Implement WebSocket/SSE (P0, 1-2 days)
4. Validate performance (P1, benchmarks)
5. Add message batching (P1, 2-3x speedup)

### Success Criteria
- ✅ Zero P0/P1 issues
- ✅ Test coverage ≥80%
- ✅ Performance regression <10%
- ✅ 5 new transports/features
- ✅ 5+ comprehensive tutorials

### Team
- **2 core engineers:** Phase 1 + 2 (critical path)
- **1 features engineer:** Phase 3 (parallel)
- **1 DX engineer:** Phase 4 (parallel)
- **Total:** 3-4 engineers

---

## Key Decisions

### Prioritization Based On
1. **v2_risks.md analysis:** 8 risks, top 5 prioritized
2. **V2_IMPLEMENTATION_REPORT.md:** Known issues classified P0-P3
3. **Performance baselines:** v1.5.0 targets, <10% regression goal
4. **Developer feedback:** Error messages, documentation, tutorials

### Phase 1 Must-Haves (Non-Negotiable)
- Fix critical bugs (client encoding)
- Complete test migration (80%+ coverage)
- Finish transport stubs (WebSocket, SSE)
- Validate dependencies (xref, dialyzer)
- Expand integration tests (15+ scenarios)

### Performance Goals (Phase 2)
- **Batching:** >2x throughput improvement
- **Pipelining:** 30% latency reduction
- **Pooling:** 50K+ concurrent connections
- **Regression:** <10% vs. v1.5.0 baseline

### Feature Innovation (Phase 3)
- Distributed registry (horizontal scaling)
- Hot code reload (zero-downtime upgrades)
- Distributed tracing (end-to-end observability)
- Transport auto-discovery (simplified config)
- Client SDK generation (type-safe APIs)

---

## Related Documents

### v2.0 Foundation
- **[v2_risks.md](../v2/V2_DESIGN_INPUTS/v2_risks.md)** - 8 risks analyzed
- **[V2_IMPLEMENTATION_REPORT.md](../V2_IMPLEMENTATION_REPORT.md)** - v2.0 completion report
- **[v2_principles.md](../v2/V2_DESIGN_INPUTS/v2_principles.md)** - Architectural philosophy
- **[v2_required_modules.md](../v2/V2_DESIGN_INPUTS/v2_required_modules.md)** - Module classification

### Architecture
- **[L1-context.md](../v2/C4/L1-context.md)** - System context
- **[L2-containers.md](../v2/C4/L2-containers.md)** - Application structure
- **[L3-components-core.md](../v2/C4/L3-components-core.md)** - Core components
- **[L4-code-map.md](../v2/C4/L4-code-map.md)** - Module-level detail

### Performance
- **[erlmcp_bench_core_ops.erl](../../bench/erlmcp_bench_core_ops.erl)** - Core operations
- **[erlmcp_bench_network_real.erl](../../bench/erlmcp_bench_network_real.erl)** - Network I/O
- **[GLOSSARY.md](../v2/GLOSSARY.md)** - Canonical units (metrology)

---

## Next Actions

### Immediate (This Week)
1. **Review Meeting:** Schedule 1-hour review with erlmcp core team
2. **Create Milestones:** Set up GitHub milestones for v2.1 phases
3. **Assign Features:** Distribute top 20 features to engineers
4. **Begin Phase 1:** Start with #1 (client capability fix, 1 hour)

### Week 1-2 (Phase 1)
1. Fix critical bugs (3 P0 issues)
2. Migrate test suites (73 legacy suites)
3. Implement WebSocket/SSE transports
4. Run static dependency analysis
5. Expand integration tests (15+ scenarios)

### Week 3-4 (Phase 2)
1. Run benchmark validation (vs. v1.5.0)
2. Tune library integrations (gproc, gun, ranch, poolboy)
3. Implement message batching (>2x speedup)
4. Add request pipelining (30% latency reduction)
5. Improve connection pooling (50K+ concurrent)

---

## Status Dashboard

### Documentation
- [x] ROADMAP.md (518 lines)
- [x] FEATURE_PROPOSALS.md (1,627 lines)
- [x] PLANNING_SUMMARY.md (500+ lines)
- [x] INDEX.md (this document)

### Quality Gates
- [x] Top 5 risks analyzed and prioritized
- [x] All known issues classified (P0-P3)
- [x] Performance baselines documented
- [x] 20 features prioritized with effort
- [x] 4-phase roadmap with success metrics
- [x] 13 technical designs with code examples
- [x] Resource planning complete (team, timeline)

### Approvals
- [ ] Technical Lead Review (pending)
- [ ] Engineering Manager Review (pending)
- [ ] Stakeholder Sign-off (pending)

---

## Contact

**Questions about v2.1 planning?**
- Technical design: See FEATURE_PROPOSALS.md
- Timeline/priorities: See ROADMAP.md
- Methodology: See PLANNING_SUMMARY.md
- GitHub issues: Create milestone tracking issues

**Document Owners:**
- Plan Designer Agent (Agent 1)
- erlmcp Core Team

---

**Document Status:** FINAL
**Last Updated:** 2026-01-27
**Version:** 1.0
