# Markdown Cleanup - Quick Reference Guide

**TL;DR:** Move 90+ files from root to `docs/archive/`, keep 5 core files in root.

---

## What to KEEP in Root (5 files)

```
README.md                  ✅ Project overview
CONTRIBUTING.md            ✅ Contributor guidelines
DEVELOPMENT.md             ✅ Dev setup
CHANGELOG.md               ✅ Version history
CLAUDE.md                  ✅ Quality gates config
```

**Action:** DO NOTHING - these stay in root.

---

## What to KEEP in docs/ (Essential Project Docs)

```
docs/api-reference.md                    ✅ Keep
docs/architecture.md                     ✅ Keep
docs/protocol.md                         ✅ Keep
docs/otp-patterns.md                     ✅ Keep
docs/deployment/                         ✅ Keep
docs/examples/                           ✅ Keep
docs/getting-started/                    ✅ Keep
docs/quality-enforcement/                ✅ Keep
docs/metrics/                            ✅ Keep
docs/transport/                          ✅ Keep
docs/clustering/                         ✅ Keep
```

**Action:** DO NOTHING - these are already in docs/.

---

## What to MOVE to docs/archive/ (90+ files)

### Category A: Agent Deliverables (10 files)

Move these to `docs/archive/agent-deliverables/`:

```
AGENT_11_COMPLETION_REPORT.md
AGENT_11_INTEGRATION_DELIVERY.md
AGENT_12_COMPLETION.md
AGENT_15_CACHE_DELIVERABLES.md
AGENT_18_COMPLETE.md
AGENT_3_DELIVERY_INDEX.md
AGENT_4_BACKPRESSURE_DELIVERABLES.md
AGENT_5_MEMORY_OPTIMIZATION_DELIVERABLES.md
AGENT_6_DELIVERABLES.md
AGENT_7_LIFECYCLE_RESULTS.md
```

**Git Command:**
```bash
mkdir -p docs/archive/agent-deliverables
git mv AGENT_*.md docs/archive/agent-deliverables/
```

---

### Category B: Version Releases (26 files)

Move these to `docs/archive/version-releases/`:

```
V0.6.0-IMPLEMENTATION-COMPLETE.md
V0.6.0-PLANNING-COMPLETE.md
V0.6.0-SUBSYSTEM-INTEGRATION-REPORT.md
V0.6.0-TCPS-PRODUCTION-READINESS-REPORT.md
V0.6.0-TESTING-REPORT.md
V1_5_0_DOCUMENTATION_RESET_COMPLETE.md
V2.1_BASELINE_DELIVERABLE_SUMMARY.md
V2.1_INTEGRATION_SUMMARY.md
IMPLEMENTATION_COMPLETE_BENCHMARKS.md
IMPLEMENTATION_GAP30.md
IMPLEMENTATION_REPORT_GAP34.md
IMPLEMENTATION_SUMMARY_GAP_43.md
IMPLEMENTATION_SUMMARY_GAP10.md
IMPLEMENTATION_SUMMARY_GAP27.md
IMPLEMENTATION_SUMMARY_GAP33.md
IMPLEMENTATION_SUMMARY_GAP37.md
IMPLEMENTATION_SUMMARY_GAP4.md
IMPLEMENTATION_SUMMARY_MCP_APPS.md
RELEASE_MANAGEMENT_RECEIPT.md
RELEASE_NOTES_v2.1.0.md
RELEASE_STRATEGY.md
RELEASE_v2.1.0_EVIDENCE.md
GAP27_IMPLEMENTATION_VERIFICATION.md
GAP3_QUICK_START.md
GAP4_ACCEPTANCE_CHECKLIST.md
IMPLEMENTATION_SUMMARY.md
```

**Git Command:**
```bash
mkdir -p docs/archive/version-releases
git mv V*.md docs/archive/version-releases/
git mv RELEASE_*.md docs/archive/version-releases/
git mv GAP*.md docs/archive/version-releases/
git mv IMPLEMENTATION_*.md docs/archive/version-releases/
```

---

### Category C: Phase Deliverables & Receipts (34 files)

Move these to `docs/archive/phase-deliverables/`:

```
ADVERSARIAL_REVIEW_README.md
AUDIT_DELIVERABLES.md
AUDIT_FILES_MANIFEST.md
AUDIT_INDEX.md
AUDIT_REPORT_SYNTHETIC_ADVERSARIAL_REVIEW.md
BACKPRESSURE_COMMANDS.md
BACKPRESSURE_IMPLEMENTATION.md
BASELINE_SYSTEM_SUMMARY.md
BENCHMARK_100K_RESULTS.md
BENCHMARK_AUDIT_INDEX.md
BENCHMARK_AUTOMATION.md
BENCHMARK_CONSOLIDATION_ROADMAP.md
BENCHMARK_INDEX.md
BENCHMARK_MIGRATION_SUMMARY.md
BENCHMARK_PERFORMANCE_ANALYSIS.md
BENCHMARKING_SETUP_RECEIPT.md
BUILD_SYSTEM_RECEIPT.md
CI_CD_DELIVERY_RECEIPT.md
CHAOS_BENCHMARK_IMPLEMENTATION.md
CHAOS_ENGINEERING_DELIVERY.md
CLEANUP_REPORT.md
COLIMA_DELIVERABLES.md
COLIMA_DEPLOYMENT_README.md
DASHBOARD_IMPLEMENTATION_SUMMARY.md
DELIVERABLES_*.md (7 files)
DELIVERY_REPORT_AUTO_FIX.md
DOCKER_BUILD_RECEIPT.md
DOCS_LINTER_DELIVERY.md
DOCUMENTATION_CONSOLIDATION_RECEIPT.md
INTEGRATION_BENCHMARK_DELIVERY.md
INTEGRATION_FRAMEWORK_DELIVERY.md
INTEGRATION_VALIDATION_RESULTS.md
PROFILING_IMPLEMENTATION_REPORT.md
TEST_INFRASTRUCTURE_RECEIPT.md
```

**Git Commands:**
```bash
mkdir -p docs/archive/phase-deliverables
git mv AUDIT_*.md docs/archive/phase-deliverables/
git mv BACKPRESSURE_*.md docs/archive/phase-deliverables/
git mv BASELINE_*.md docs/archive/phase-deliverables/
git mv BENCHMARK_*.md docs/archive/phase-deliverables/
git mv BENCHMARKING_*.md docs/archive/phase-deliverables/
git mv BUILD_SYSTEM_*.md docs/archive/phase-deliverables/
git mv *_RECEIPT.md docs/archive/phase-deliverables/
git mv *_DELIVERY.md docs/archive/phase-deliverables/
git mv CHAOS_*.md docs/archive/phase-deliverables/
git mv CI_CD_*.md docs/archive/phase-deliverables/
git mv CLEANUP_*.md docs/archive/phase-deliverables/
git mv COLIMA_*.md docs/archive/phase-deliverables/
git mv DASHBOARD_*.md docs/archive/phase-deliverables/
git mv DELIVERABLES_*.md docs/archive/phase-deliverables/
git mv DOCS_LINTER_*.md docs/archive/phase-deliverables/
git mv DOCUMENTATION_*.md docs/archive/phase-deliverables/
git mv INTEGRATION_*.md docs/archive/phase-deliverables/
git mv PROFILING_*.md docs/archive/phase-deliverables/
git mv TEST_*.md docs/archive/phase-deliverables/
```

---

### Category D: Other Phase/Report Files (25+ files)

Move these to `docs/archive/phase-deliverables/`:

```
CAPABILITY_NEGOTIATION_USAGE.md
CIRCUIT_BREAKER_v1.3.0.md
CLI_USAGE.md
CLUSTER_FILES_MANIFEST.md
CLUSTER_QUICK_START.md
CLUSTER_SETUP.md
CODE_QUALITY_REPORT_V2.0.md
CODE_QUALITY_REPORT_V2.1.md
CODEGEN_IMPLEMENTATION_REPORT.md
COMPREHENSIVE_BENCHMARK_AUDIT.md
CONVERSATION_SUMMARY_SESSION_2.md
COWBOY_HTTP_AUDIT.md
COWBOY_RECOMMENDATIONS_IMPLEMENTATION.md
CRITICAL_FIXES_TECHNICAL_ROADMAP.md
DEPLOYMENT_CHECKLIST.md
DEPLOYMENT_GUIDE_100X.md
DEPLOYMENT_INDEX.md
DISTRIBUTED_LOGGING_IMPLEMENTATION.md
DOCKER_GUIDE.md
DOCKER_K8S_README.md
EXECUTIVE_SUMMARY_100K_VALIDATION.md
EXECUTIVE_SUMMARY_V1.2.0.md
FINAL_COMPLETION_STATUS.md
FINAL_DELIVERABLES_INDEX.md
FINAL_VALIDATION_REPORT_100K_CONCURRENT.md
GETTING_STARTED_WORKSPACE.md
HTTP_PROTOCOL_COMPLIANCE_CHECKLIST.md
KAIZEN_ASSESSMENT_INDEX.md
MAKEFILE_100K_GUIDE.md
MARKETPLACE_GENERATION_SYSTEM.md
MASTER_MANIFEST.md
MEMORY_ACCOUNTING_IMPLEMENTATION.md
MEMORY_ACCOUNTING_README.md
MEMORY_OPTIMIZATION_QUICK_START.md
MEMORY_PROFILING_DATA.md
MERGE_RESOLUTION_PLAN.md
METROLOGY_VIOLATIONS_INDEX.md
MULTI_ENVIRONMENT_SETUP_RECEIPT.md
PERFORMANCE_TARGETS.md
PHASE_1_CLEANUP_CHECKLIST.md
PHASE_4_DELIVERABLES_MANIFEST.md
PHASE3_TRANSPORT_PERFORMANCE_ASSESSMENT.md
PLAN_CONFORMANCE_QUICK_START.md
PRODUCTION_AUDIT_INDEX.md
PRODUCTION_HARDENING_CHECKLIST.md
PRODUCTION_READINESS_DASHBOARD.md
PROFILING_SYSTEM_DELIVERABLES.md
PROFILING_SYSTEM_REAL_NUMBERS.md
PROFILING_TOOLS_INDEX.md
PROFILING_VERIFICATION_CHECKLIST.md
QUALITY_GATES_IMPLEMENTATION.md
QUALITY_METRICS_DELIVERABLES.md
QUALITY_METRICS_VERIFICATION.md
QUICK_START_ENVIRONMENTS.md
QUICK_START_VALIDATION.md
RATE_LIMITING_IMPLEMENTATION.md
REBAR3_EUNIT_FIX_GUIDE.md
REBAR3_EUNIT_SUITE_ANALYSIS.md
REBAR3_INVESTIGATION_INDEX.md
REBAR3_INVESTIGATION_REFERENCE.md
REFACTORING_INDEX.md
REGISTRY_SHARDING_DELIVERABLES.md
REGISTRY_SHARDING_DELIVERY.md
SECURITY_AUDIT_DELIVERABLES_INDEX.md
STAGING_DEPLOYMENT_README.md
START_HERE_COLIMA.md
START_HERE_VALIDATION_REPORTS.md
STRESS_TEST_CHECKLIST.md
STRESS_TEST_QUICKSTART.md
SUPERVISION_EXTRACTION_COMPLETE.md
TCPS-IMPLEMENTATION-COMPLETE.md
TCPS-WAVE-3-COMPLETION-REPORT.md
TESTING_ERROR_HANDLING.md
TRANSPORT_COMPLIANCE_MATRIX.md
TRANSPORT_REGISTRATION_STATUS.md
transport_validation_report.md
VALIDATION_SUMMARY.md
VENDOR_MANIFEST.md
VENDOR_SETUP_RECEIPT.md
VULNERABILITY_INVENTORY.md
WORK-SUMMARY.md
WORKSPACE_INTEGRATION_CHECKLIST.md
WORKSPACE_VALIDATION_RECEIPT.md
WORKSPACE.md
```

**Action:**
```bash
# Move all these files
git mv *.md docs/archive/phase-deliverables/ 2>/dev/null || true

# Then move back the 5 core files
git mv docs/archive/phase-deliverables/README.md . 2>/dev/null || true
git mv docs/archive/phase-deliverables/CONTRIBUTING.md . 2>/dev/null || true
git mv docs/archive/phase-deliverables/DEVELOPMENT.md . 2>/dev/null || true
git mv docs/archive/phase-deliverables/CHANGELOG.md . 2>/dev/null || true
git mv docs/archive/phase-deliverables/CLAUDE.md . 2>/dev/null || true
```

---

## Files to DELETE (Duplicates/Temporary)

**Review & Delete:**
```
docs/api_reference.md          # Duplicate of docs/api-reference.md (lowercase)
AGENTS.md                      # Check if superseded by docs/agents/
BENCHMARK_RESULTS_V2.md        # Check if superseded by newer benchmarks
README_AUDIT_REPORTS.md        # Check if just an index
README_TCPS_PERSISTENCE.md     # Check if content moved to main docs
```

**Action:**
```bash
# Review first, then delete
rm docs/api_reference.md
# ... delete others after confirmation
```

---

## Summary of Changes

**Before:**
```
/Users/sac/erlmcp/
├── README.md                              ✅
├── CONTRIBUTING.md                        ✅
├── DEVELOPMENT.md                         ✅
├── CHANGELOG.md                           ✅
├── CLAUDE.md                              ✅
├── AGENT_*.md                    (10)     ❌ Move
├── V*.md, GAP*.md               (26)     ❌ Move
├── *_RECEIPT.md, *_DELIVERY.md  (34+)    ❌ Move
├── ... (100+ other report files)          ❌ Move
└── docs/
    ├── api-reference.md                   ✅
    ├── architecture.md                    ✅
    └── ... (other permanent docs)         ✅
```

**After:**
```
/Users/sac/erlmcp/
├── README.md                              ✅
├── CONTRIBUTING.md                        ✅
├── DEVELOPMENT.md                         ✅
├── CHANGELOG.md                           ✅
├── CLAUDE.md                              ✅
└── docs/
    ├── api-reference.md                   ✅
    ├── architecture.md                    ✅
    ├── ... (other permanent docs)         ✅
    └── archive/
        ├── agent-deliverables/            📂 New
        │   ├── AGENT_*.md                 (10)
        │   └── README.md
        ├── version-releases/              📂 New
        │   ├── v0.6.0/
        │   ├── v1.5.0/
        │   ├── v2.0.0/
        │   ├── v2.1.0/
        │   └── README.md
        └── phase-deliverables/            📂 New
            ├── AUDIT_*.md
            ├── *_RECEIPT.md
            ├── *_DELIVERY.md
            ├── ... (other phase files)
            └── README.md
```

---

## One-Liner Commands (Use Carefully!)

**Create archive structure:**
```bash
mkdir -p docs/archive/{agent-deliverables,version-releases,phase-deliverables}
```

**Move agent files:**
```bash
cd /Users/sac/erlmcp && git mv AGENT_*.md docs/archive/agent-deliverables/ 2>/dev/null || true
```

**Move version/gap files:**
```bash
cd /Users/sac/erlmcp && git mv V*.md RELEASE_*.md GAP*.md IMPLEMENTATION_*.md docs/archive/version-releases/ 2>/dev/null || true
```

**Move everything else (careful!):**
```bash
cd /Users/sac/erlmcp && for f in *.md; do
  case "$f" in
    README.md|CONTRIBUTING.md|DEVELOPMENT.md|CHANGELOG.md|CLAUDE.md) ;;
    *) git mv "$f" docs/archive/phase-deliverables/ 2>/dev/null || true ;;
  esac
done
```

---

## Verification Checklist

After moving files:

```bash
# Check root has only 5 files
ls -1 /Users/sac/erlmcp/*.md | wc -l        # Should be 5

# Check archive structure created
ls -R /Users/sac/erlmcp/docs/archive/       # Should show 3 subdirs

# Check no broken links in kept files
grep -r "AGENT_.*\.md\|V[0-9].*\.md\|GAP[0-9]" \
  README.md CONTRIBUTING.md DEVELOPMENT.md CHANGELOG.md CLAUDE.md

# Count files moved (should be ~90-95)
find docs/archive -name "*.md" | wc -l
```

---

## Next Steps

1. **Execute archive structure setup** (creates directories)
2. **Execute file movements** (git mv commands)
3. **Update any broken links** (if found in verification)
4. **Create git commit** with message:
   ```
   refactor: organize markdown files into archive structure

   - Move 10 agent deliverables to docs/archive/agent-deliverables/
   - Move 26 version/gap reports to docs/archive/version-releases/
   - Move 34+ phase deliverables to docs/archive/phase-deliverables/
   - Keep 5 core files in root: README, CONTRIBUTING, DEVELOPMENT, CHANGELOG, CLAUDE
   - Keep essential project docs in docs/
   - Reduces root directory from 171 to 5 markdown files
   ```

---

**See Also:** `/Users/sac/erlmcp/docs/MARKDOWN_ORGANIZATION_AUDIT.md` (full audit with rationale)
