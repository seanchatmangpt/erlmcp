# Archive - High-Value Reference Material

This directory contains historical documentation, implementation reports, and reference material from erlmcp development. These files provide technical context, evidence trails, and insights into architectural decisions.

## Quick Directory Breakdown

```
archive/
├── benchmarks/         (16 files)  - Performance baselines, metrology validation
├── misc/               (46 files)  - MCP specs, integration plans, operational guides
├── phase-reports/      (44 files)  - Feature implementations, release evidence
├── quality-reports/    (23 files)  - Quality assessments, coverage analysis
├── strategy/           (1 file)    - Release strategy documentation
├── tasks/              (13 files)  - Task documentation and tracking
└── troubleshooting/    (7 files)   - Issue resolution guides

Total: 150 archived files
```

## How to Find Things

**For general navigation**: See `/home/user/erlmcp/DOCUMENTATION_GUIDE.md` for complete documentation organization.

**For detailed archive information**: See `/home/user/erlmcp/ARCHIVE_INDEX.md` for full file listings and retention rationale.

## Critical Files (Never Delete)

### Tier 1 - Essential Reference

**Phase Reports**:
- `phase-reports/RELEASE_v2.1.0_EVIDENCE.md` - Release evidence and validation
- `phase-reports/AGENT_TEAM_FINAL_STATUS.md` - Final team status (2026-01-31)

**Quality Reports**:
- `quality-reports/QUALITY_GATE_REPORT_v2.1.0.md` - Versioned quality gates

**Misc**:
- `misc/MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md` - Comprehensive MCP API reference
- `misc/GGEN_INTEGRATION_PLAN.md` - Large integration architecture details

## Quick Access Examples

### Find implementation reports
```bash
find archive/phase-reports -name "*IMPLEMENTATION*" -type f
```

### Search for specific topics
```bash
grep -r "chaos engineering" archive/
grep -r "benchmark" archive/benchmarks/
grep -r "quality gate" archive/quality-reports/
```

### List recent files
```bash
ls -lt archive/phase-reports/ | head -10
ls -lt archive/misc/ | head -10
```

### Find files by pattern
```bash
find archive/ -name "*RELEASE*"
find archive/ -name "*TCPS*"
find archive/ -name "*AGENT*"
```

## Archive Maintenance

**Retention Policy**: Files in this archive provide technical or historical value. Low-value interim reports have been deleted (see ARCHIVE_INDEX.md for details).

**When to Archive**: Move files here when they're no longer needed for active development but contain valuable reference material.

**When to Delete**: Only delete files marked as "transient" or "interim" in ARCHIVE_INDEX.md. Never delete Tier 1 critical files.

## Related Documentation

- `/home/user/erlmcp/DOCUMENTATION_GUIDE.md` - Complete documentation map
- `/home/user/erlmcp/ARCHIVE_INDEX.md` - Detailed archive inventory
- `/home/user/erlmcp/CLAUDE.md` - System specification (never archive)
- `/home/user/erlmcp/README.md` - Project overview (never archive)

---

**Archive Date**: 2026-01-31
**Total Files**: 150 (after cleanup of 220 interim/transient files)
**Purpose**: High-value reference material for erlmcp development
