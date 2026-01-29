# Documentation Archive

This directory contains historical documentation from completed development phases, sessions, and versions of erlmcp.

## Purpose

The archive preserves valuable context and evidence from past work while keeping the main docs directory focused on current, active project documentation.

## Organization

### agent-deliverables/
Historical reports from individual agent sessions during development.

**Contents:**
- Agent completion reports
- Deliverable inventories
- Integration delivery documentation

**Use Case:** Understanding what was delivered in each agent phase

**Files:** ~10 documents

---

### version-releases/
Version-specific planning, implementation, and release documentation.

**Organization by version:**
- `v0.6.0/` - Initial library integration phase
- `v1.5.0/` - Documentation reset and consolidation
- `v2.0.0/` - Major performance and feature release
- `v2.1.0/` - Refinement and optimization release

**Contents:**
- Implementation completion reports
- Planning documents
- Testing reports
- Production readiness assessments
- Release evidence and strategies

**Use Case:** Understanding decisions, testing, and implementation approach for each version

**Files:** ~26 documents organized by version

---

### phase-deliverables/
Phase completion documents, quality gate receipts, and audit reports from development cycles.

**Contents:**
- Audit reports and findings
- Quality gate receipts (build, test, CI/CD)
- Implementation phase summaries
- Performance benchmarking documentation
- Integration and validation results
- Infrastructure setup receipts

**Use Case:** Historical record of quality gates, validation steps, and phase deliverables

**Files:** ~34+ documents

---

## Accessing Archive Documents

**To find a specific document:**

1. **By agent phase:** Look in `agent-deliverables/`
2. **By version:** Look in `version-releases/[version]/`
3. **By delivery phase:** Look in `phase-deliverables/`
4. **By report type:** Use `grep` with keywords:
   ```bash
   grep -r "implementation" .
   grep -r "audit" .
   grep -r "validation" .
   grep -r "receipt" .
   ```

## Retention & Lifecycle

**Archive retention policy:**
- All documents retained indefinitely
- Organized by time period and topic
- Indexed for discoverability
- Searchable via grep/ripgrep

**When to use archive docs:**
- Understanding historical decisions
- Auditing implementation approach for a feature
- Reviewing how a previous version was tested
- Verifying completion of a development phase
- Legal/compliance requirements

**When to NOT use archive docs:**
- Current development questions → See main `/docs` directory
- API usage → See `docs/api-reference.md`
- Deployment guides → See `docs/deployment/`
- Project architecture → See `docs/architecture.md`

## Master Index

For a complete index of archive contents, see:
- Main archive README (this file)
- `agent-deliverables/README.md`
- `version-releases/README.md`
- `phase-deliverables/README.md`

## Contributing

When adding new documents to the archive:

1. **Session/phase deliverables:** Move to `phase-deliverables/` at end of session
2. **Version releases:** Create version-specific directory in `version-releases/`
3. **Agent deliverables:** Organize by agent ID in `agent-deliverables/`
4. **Update appropriate README:** Note document in category README

## Navigation

From archive to active docs:
- Main project docs: [`../`](../)
- Architecture: [`../architecture.md`](../architecture.md)
- API reference: [`../api-reference.md`](../api-reference.md)
- Deployment: [`../deployment/`](../deployment/)
- Development: [`../../DEVELOPMENT.md`](../../DEVELOPMENT.md)
- Quality gates: [`../quality-enforcement/`](../quality-enforcement/)

---

**Last updated:** 2026-01-28
**Archive structure created:** Markdown cleanup initiative
**Purpose:** Historical preservation and audit trail
