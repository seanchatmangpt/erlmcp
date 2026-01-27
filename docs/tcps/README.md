# TCPS Documentation Suite

**Toyota Code Production System for erlmcp v0.6.0**

---

## Overview

This directory contains the complete TCPS (Toyota Code Production System) documentation for erlmcp. TCPS applies Toyota's proven manufacturing principles directly to software production, treating code as a physical product moving through a factory line.

**Core Philosophy**: If a factory cannot prove a unit passed inspection, it does not ship. Same here.

---

## Documentation Index

### Getting Started

1. **[QUICKSTART.md](QUICKSTART.md)** - Start here
   - 15-minute setup guide
   - Your first work order
   - Running the production pipeline
   - Common workflows
   - **Audience**: Everyone new to TCPS
   - **Time**: 15-30 minutes

---

### Core Concepts

2. **[TCPS.md](TCPS.md)** - Main guide (31KB, 909 lines)
   - Complete TCPS philosophy
   - The 9 pillars explained
   - Factory metaphor applied to erlmcp
   - Why TCPS beats traditional support
   - Implementation architecture
   - **Audience**: Developers, architects, managers
   - **Time**: 60-90 minutes

---

### Operational Procedures

3. **[STANDARD_WORK.md](STANDARD_WORK.md)** - Process documentation (33KB, 1,401 lines)
   - 10 production stages defined
   - Inputs, outputs, SLOs per stage
   - Step-by-step procedures
   - Failure modes and recovery
   - **Audience**: Operators, developers, QA
   - **Time**: 2-3 hours (reference document)

4. **[DEFINITION_OF_DONE.md](DEFINITION_OF_DONE.md)** - Quality gates (16KB, 753 lines)
   - Shippability criteria
   - 10 quality gates
   - Receipt requirements
   - Acceptance criteria
   - Verification checklist
   - **Audience**: QA, release managers, developers
   - **Time**: 45-60 minutes

5. **[ANDON_RUNBOOK.md](ANDON_RUNBOOK.md)** - Stop-the-line procedures (20KB, 902 lines)
   - How to trigger Andon
   - Response workflow
   - 5 Whys template
   - Prevention framework
   - Common failure scenarios
   - **Audience**: All team members
   - **Time**: 60-75 minutes

---

### Production Planning

6. **[HEIJUNKA_SCHEDULING.md](HEIJUNKA_SCHEDULING.md)** - Work leveling (19KB, 774 lines)
   - 5 bucket definitions
   - Leveling algorithm
   - WIP limit configuration
   - Pull signal processing
   - Schedule optimization
   - **Audience**: Project managers, team leads
   - **Time**: 60-75 minutes

---

### Continuous Improvement

7. **[KAIZEN_GUIDE.md](KAIZEN_GUIDE.md)** - Improvement process (14KB, 528 lines)
   - Metrics tracked
   - Identifying waste (TIMWOODS)
   - Improvement proposal process
   - Trend analysis examples
   - Kaizen events
   - **Audience**: All team members
   - **Time**: 45-60 minutes

---

### Technical Specifications

8. **[RECEIPTS_SPEC.md](RECEIPTS_SPEC.md)** - Receipt schemas (8KB, 342 lines)
   - JSON schema definitions
   - Required fields per stage
   - Storage conventions
   - Linking to ontology
   - Verification procedures
   - **Audience**: Developers, automation engineers
   - **Time**: 30-45 minutes

---

## Document Relationships

```
QUICKSTART.md (start here)
    ↓
TCPS.md (understand philosophy)
    ↓
    ├─→ STANDARD_WORK.md (how to operate)
    │       ↓
    │       └─→ RECEIPTS_SPEC.md (technical details)
    │
    ├─→ DEFINITION_OF_DONE.md (quality gates)
    │
    ├─→ ANDON_RUNBOOK.md (problem response)
    │
    ├─→ HEIJUNKA_SCHEDULING.md (work planning)
    │
    └─→ KAIZEN_GUIDE.md (continuous improvement)
```

---

## Reading Paths

### Path 1: New to TCPS (4-5 hours)

1. QUICKSTART.md (15 min) - Get running
2. TCPS.md (90 min) - Understand philosophy
3. STANDARD_WORK.md (60 min) - Learn one stage deeply
4. ANDON_RUNBOOK.md (60 min) - Know how to respond to problems
5. KAIZEN_GUIDE.md (45 min) - Start improving

---

### Path 2: Developer/Operator (3 hours)

1. QUICKSTART.md (15 min)
2. STANDARD_WORK.md (90 min) - Focus on stages you'll run
3. DEFINITION_OF_DONE.md (45 min) - Know quality gates
4. ANDON_RUNBOOK.md (30 min) - Common scenarios only

---

### Path 3: Manager/Lead (2 hours)

1. TCPS.md (60 min) - Full philosophy
2. HEIJUNKA_SCHEDULING.md (45 min) - Work planning
3. KAIZEN_GUIDE.md (30 min) - Metrics and improvement

---

### Path 4: QA/Release Engineer (2.5 hours)

1. DEFINITION_OF_DONE.md (60 min)
2. STANDARD_WORK.md (60 min) - Test & release stages
3. RECEIPTS_SPEC.md (30 min) - Verification

---

## Key Concepts at a Glance

### The 9 Pillars

1. **JIT (Just-In-Time)**: Build only what's pulled
2. **Jidoka**: Built-in quality, stop-the-line authority
3. **Standard Work**: Documented procedures for every stage
4. **Kanban**: WIP limits maintain flow
5. **Heijunka**: Level production to avoid crises
6. **Poka-Yoke**: Error-proof in the specification
7. **Andon**: Visible stop-the-line signals
8. **5 Whys**: Root cause analysis
9. **Kaizen**: Continuous improvement

---

### The 10 Production Stages

1. **Pull**: Demand signals → Work orders
2. **Plan**: Heijunka scheduling
3. **Generate**: SHACL validation
4. **Extract**: SPARQL queries
5. **Render**: Template generation
6. **Build**: Compilation
7. **Test**: Test execution
8. **Release**: Artifact packaging
9. **Publish**: Distribution
10. **Verify**: Smoke tests

---

### The 5 Buckets (Heijunka)

1. **Reliability** (2 slots/day): Bug fixes, stability
2. **Security** (2 slots/day): CVE patches, hardening
3. **Features** (1 slot/day): New capabilities
4. **Compliance** (1 slot/day): Regulatory requirements
5. **Technical Debt** (1 slot/day): Refactoring, cleanup

---

## Implementation Status

### erlmcp v0.6.0 TCPS Features

✅ **Implemented**:
- Directory structure (`ontology/`, `shapes/`, `receipts/`)
- Work order ontology
- SHACL validation
- Receipt generation (manual)
- Andon triggering (manual)
- Standard work procedures (documented)

🚧 **In Progress**:
- Heijunka scheduler (partial)
- Automated receipt generation
- Receipt chain verification
- WIP limit enforcement

📋 **Planned**:
- Full automation of all stages
- Receipt-to-ontology RDF conversion
- Kaizen metrics dashboard
- Andon analytics

---

## Quick Reference

### Essential Tools

```bash
# Work Orders
./tools/work-order create --sku <name> --bucket <type> --priority <level>
./tools/work-order list
./tools/work-order update <wo-id> --field <value>

# Pipeline
make tcps-build              # Full pipeline
./tools/pipeline run-stage <stage>

# Receipts
./tools/receipt show-chain --work-order <wo-id>
./tools/receipt verify <receipt-file>

# Andon
./tools/andon trigger --stage <stage> --reason <code>
./tools/andon resolve <andon-id>

# Heijunka
./tools/heijunka schedule --days <n>

# WIP
./tools/wip status
```

---

### Key Metrics

| Metric | Target | Current (v0.6.0) | Status |
|--------|--------|------------------|--------|
| Lead Time | < 8 hours | 6.5 hours | ✅ |
| Defect Rate | < 5 per 100 | 4.6 per 100 | ✅ |
| Coverage | ≥ 80% | 87.3% | ✅ |
| Build Determinism | 100% | 98% | 🚧 |
| WIP Limit | ≤ 3 | 2-3 | ✅ |

---

## Contributing to TCPS Documentation

### Documentation Standards

- **Format**: Markdown
- **Line Length**: 100 characters (documentation), no limit (code blocks)
- **Diagrams**: Mermaid syntax
- **Examples**: Real erlmcp v0.6.0 code
- **Tone**: Direct, technical, no fluff

### Adding New Documents

1. Create in `/docs/tcps/`
2. Follow naming convention: `TOPIC_NAME.md`
3. Include version, date, purpose header
4. Add to this README index
5. Link from related documents

### Updating Existing Documents

1. Update "Last Updated" date
2. Increment version if major changes
3. Document changes in git commit message
4. Update cross-references if structure changed

---

## Support and Questions

### Internal Resources

- **erlmcp Architecture**: `../ARCHITECTURE_OVERVIEW.md`
- **Build System**: `../BUILD_SYSTEM.md`
- **OTP Patterns**: `../otp-patterns.md`
- **Testing**: `../TEST_STRATEGY.md`

### External Resources

- **Toyota Production System**: Taiichi Ohno's books
- **The Goal**: Eliyahu Goldratt (Theory of Constraints)
- **Lean Software Development**: Mary and Tom Poppendieck
- **Continuous Delivery**: Jez Humble and David Farley

---

## License

This documentation is part of erlmcp and follows the same license (see LICENSE in repository root).

---

## Changelog

### v1.0.0 (2026-01-26)

- Initial TCPS documentation suite
- 8 comprehensive guides (7,086 total lines)
- Complete coverage of all 9 pillars
- Production-ready procedures
- Real erlmcp v0.6.0 examples

---

**Welcome to the Toyota Code Production System.**

**No support queue. No heroics. No guessing. Just proven manufacturing discipline applied to code.**
