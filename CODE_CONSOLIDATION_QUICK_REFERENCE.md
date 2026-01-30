# Code Consolidation Quick Reference
**Status Tracking for erlmcp v0.6.0 → v0.7.0 Cleanup**

## Phase Status Overview

| Phase | Duration | Status | Started | Completed | Notes |
|-------|----------|--------|---------|-----------|-------|
| Phase 1: Quick Wins | 1 day | ⏳ Pending | - | - | 80% of benefit |
| Phase 2: Consolidation | 5 days | ⏳ Pending | - | - | Module merging |
| Phase 3: Refactoring | 7 days | ⏳ Pending | - | - | Large module splits |
| Phase 4: Documentation | 4 days | ⏳ Pending | - | - | Docs + polish |

**Legend:** ⏳ Pending | 🔄 In Progress | ✅ Complete | ❌ Blocked

---

## Phase 1: Quick Wins (1 day)

### Task Checklist

- [ ] **1.1** Delete 36 root-level test scripts → Save 3,650 LOC
  - [ ] 1.1.1 Delete registry manual tests (2 files, 15min)
  - [ ] 1.1.2 Delete connection monitor tests (1 file, 10min)
  - [ ] 1.1.3 Delete progress/pagination tests (2 files, 15min)
  - [ ] 1.1.4 Delete schema/error tests (2 files, 15min)
  - [ ] 1.1.5 Delete one-off scripts (5 files, 30min)
  - [ ] 1.1.6 Delete OTEL/TCP tests (3 files, 15min)
  - [ ] 1.1.7 Delete metrics tests (2 files, 15min)
  - [ ] 1.1.8 Delete stress variants (6 files, 30min)
  - [ ] 1.1.9 Delete batch duplicates (8 files, 45min)
  - [ ] 1.1.10 Delete GCP/marketplace (5 files, 30min)

- [ ] **1.2** Delete broken modules (1 file, 25min)
  - [ ] 1.2.1 Delete erlmcp_pricing_upgrade.erl.broken (10min)
  - [ ] 1.2.2 Remove references (15min)

- [ ] **1.3** Delete obsolete markdown files (26 files, 1.5hr)
  - [ ] 1.3.1 Delete agent completion reports (8 files, 20min)
  - [ ] 1.3.2 Delete batch test results (6 files, 15min)
  - [ ] 1.3.3 Delete duplicate audits (5 files, 15min)
  - [ ] 1.3.4 Delete obsolete deliverables (7 files, 20min)

- [ ] **1.4** Fix xref warnings (40min)
  - [ ] 1.4.1 Run xref (5min)
  - [ ] 1.4.2 Fix warnings (30min)
  - [ ] 1.4.3 Verify clean (5min)

**Phase 1 Quality Gate:**
```bash
# Must pass before Phase 2
make check                    # ✅ Pass
rebar3 xref                   # ✅ 0 warnings
```

---

## Phase 2: Module Consolidation (5 days)

### Task Checklist

- [ ] **2.1** Consolidate rate limiter (10hr)
  - [ ] 2.1.1 Research implementations (2hr)
  - [ ] 2.1.2 Identify v2 improvements (1hr)
  - [ ] 2.1.3 Merge into v1 (4hr)
  - [ ] 2.1.4 Update tests (2hr)
  - [ ] 2.1.5 Update references (1hr)
  - [ ] 2.1.6 Delete v2 module (15min)

- [ ] **2.2** Add monitor facade (9hr)
  - [ ] 2.2.1 Design unified API (2hr)
  - [ ] 2.2.2 Create facade (3hr)
  - [ ] 2.2.3 Update client code (2hr)
  - [ ] 2.2.4 Add deprecation warnings (1hr)
  - [ ] 2.2.5 Update docs (1hr)

- [ ] **2.3** Consolidate guards (11hr)
  - [ ] 2.3.1 Create resource_guard (4hr)
  - [ ] 2.3.2 Migrate CPU logic (2hr)
  - [ ] 2.3.3 Migrate memory logic (2hr)
  - [ ] 2.3.4 Update supervision tree (1hr) **[HIGH RISK]**
  - [ ] 2.3.5 Update tests (2hr)
  - [ ] 2.3.6 Delete old guards (15min)

- [ ] **2.4** [OPTIONAL] Extract pricing app (7.5hr)
  - Defer to v0.8.0 if time constrained

- [ ] **2.5** Consolidate transport registry (4.5hr)
  - [ ] 2.5.1 Add discovery to registry (2hr)
  - [ ] 2.5.2 Update transport code (1hr)
  - [ ] 2.5.3 Update tests (1hr)
  - [ ] 2.5.4 Delete discovery module (15min)

**Phase 2 Quality Gate:**
```bash
make check                    # ✅ Pass
rebar3 dialyzer               # ✅ 0 errors
rebar3 cover                  # ✅ ≥80% coverage
```

---

## Phase 3: Refactoring (7 days)

### Task Checklist

- [ ] **3.1** Split erlmcp_server.erl (24hr) **[HIGH RISK]**
  - [ ] 3.1.1 Extract resources (4hr)
  - [ ] 3.1.2 Extract tools (4hr)
  - [ ] 3.1.3 Extract prompts (4hr)
  - [ ] 3.1.4 Extract utilities (2hr)
  - [ ] 3.1.5 Update server to delegate (4hr)
  - [ ] 3.1.6 Split tests (4hr)
  - [ ] 3.1.7 Verify API compatibility (2hr)

- [ ] **3.2** Split erlmcp_capabilities.erl (10hr)
  - [ ] 3.2.1 Extract types (2hr)
  - [ ] 3.2.2 Extract negotiation (3hr)
  - [ ] 3.2.3 Slim main module (2hr)
  - [ ] 3.2.4 Split tests (2hr)
  - [ ] 3.2.5 Verify API (1hr)

- [ ] **3.3** Refactor erlmcp_rate_limiter.erl (6hr)
  - [ ] 3.3.1 Extract statistics (2hr)
  - [ ] 3.3.2 Simplify config (2hr)
  - [ ] 3.3.3 Update tests (1hr)
  - [ ] 3.3.4 Verify performance (1hr)

- [ ] **3.4** Improve test organization (3.75hr)
  - [ ] 3.4.1 Move root tests to apps (2hr)
  - [ ] 3.4.2 Standardize naming (1hr)
  - [ ] 3.4.3 Update rebar.config (30min)
  - [ ] 3.4.4 Delete empty test/ dir (15min)

**Phase 3 Quality Gate:**
```bash
make check                    # ✅ Pass
make benchmark-quick          # ✅ <10% regression
cd examples/calculator && make run  # ✅ Works
cd examples/weather && make run     # ✅ Works
```

---

## Phase 4: Documentation (4 days)

### Task Checklist

- [ ] **4.1** Consolidate markdown (8hr)
  - [ ] 4.1.1 Archive implementation reports (1hr)
  - [ ] 4.1.2 Archive delivery receipts (1hr)
  - [ ] 4.1.3 Delete obsolete test reports (30min)
  - [ ] 4.1.4 Consolidate benchmarks (30min)
  - [ ] 4.1.5 Consolidate deployment guides (2hr)
  - [ ] 4.1.6 Consolidate checklists (2hr)
  - [ ] 4.1.7 Review miscellaneous (1hr)

- [ ] **4.2** Improve API documentation (10hr)
  - [ ] 4.2.1 Add module @doc headers (3hr)
  - [ ] 4.2.2 Add function @doc comments (6hr)
  - [ ] 4.2.3 Generate edoc (30min)
  - [ ] 4.2.4 Publish to docs/edoc/ (30min)

- [ ] **4.3** Standardize formatting (3.5hr)
  - [ ] 4.3.1 Add rebar3_format (15min)
  - [ ] 4.3.2 Run initial format (30min)
  - [ ] 4.3.3 Review changes (1hr)
  - [ ] 4.3.4 Commit formatting (15min)
  - [ ] 4.3.5 Add pre-commit hook (30min)
  - [ ] 4.3.6 Add CI check (30min)

- [ ] **4.4** Add quality badges (1hr)
  - [ ] 4.4.1 CI status badge (15min)
  - [ ] 4.4.2 Coverage badge (15min)
  - [ ] 4.4.3 Hex.pm version badge (15min)
  - [ ] 4.4.4 License badge (15min)

- [ ] **4.5** Create migration guide (4.5hr)
  - [ ] 4.5.1 Document rate_limiter changes (1hr)
  - [ ] 4.5.2 Document guard changes (1hr)
  - [ ] 4.5.3 Document monitor changes (1hr)
  - [ ] 4.5.4 Document server refactoring (30min)
  - [ ] 4.5.5 Create upgrade checklist (1hr)

**Phase 4 Quality Gate:**
```bash
rebar3 edoc                   # ✅ Generates HTML docs
rebar3 format --verify        # ✅ No changes needed
grep "@doc" apps/*/src/*.erl | wc -l  # ✅ ≥100 comments
```

---

## Quick Commands

### Before Starting Any Phase
```bash
# Create checkpoint
git checkout -b cleanup/phase-N
git add .
git commit -m "Checkpoint before Phase N"
```

### After Completing Any Phase
```bash
# Quality gates
make check                    # Compile + tests
rebar3 xref                   # Cross-reference
rebar3 dialyzer               # Type checking
rebar3 cover                  # Coverage
make benchmark-quick          # Performance (if applicable)

# Commit
git add .
git commit -m "Phase N complete: [description]"
```

### If Something Goes Wrong
```bash
# Rollback to checkpoint
git reset --hard HEAD~1

# Or start over from clean state
git checkout main
git branch -D cleanup/phase-N
```

---

## Critical Success Factors

### Must Pass (Every Phase)
- ✅ `make check` passes (0 errors)
- ✅ All tests pass (100% pass rate)
- ✅ `rebar3 xref` clean (0 warnings)
- ✅ Code coverage ≥ 80%

### Must Verify (Specific Phases)
- ✅ Phase 2.3: Supervision tree starts correctly
- ✅ Phase 3.1: Examples still work (calculator, weather)
- ✅ Phase 3.3: No performance regression (<10%)
- ✅ Phase 4.2: edoc generates HTML docs

---

## Risk Management

### High-Risk Tasks (Extra Caution Required)

1. **Phase 2.3.4** - Update supervision tree
   - **Risk:** Could break application startup
   - **Mitigation:** Test with `observer:start()`, verify process tree
   - **Rollback:** Keep old guards until verified

2. **Phase 3.1** - Split erlmcp_server.erl
   - **Risk:** 2,040 line module, many dependencies
   - **Mitigation:** Incremental extraction, comprehensive tests
   - **Rollback:** Keep old module as fallback

3. **Phase 3.3.4** - Verify rate limiter performance
   - **Risk:** Performance regression
   - **Mitigation:** Benchmark before and after
   - **Rollback:** Revert if >10% regression

---

## Progress Tracking

### Daily Standup Format
```
**Yesterday:**
- Completed: [list tasks]
- Blockers: [list issues]

**Today:**
- Working on: [current task]
- Expected completion: [time]

**Blockers:**
- [Any issues preventing progress]
```

### Weekly Summary Format
```
**Week [N] Summary:**
- Phases completed: [list]
- LOC removed: [count]
- Files deleted: [count]
- Tests passing: [X/X]
- Blockers resolved: [list]
- Next week plan: [Phase N]
```

---

## File Deletion Quick Copy-Paste

### Phase 1.1 - Delete Test Scripts
```bash
# Copy-paste this entire block
rm test_registry_manual.erl test_registry_new_functions.erl \
   test_connection_monitor.erl test_progress_manual.erl \
   test_pagination_manual.erl test_schema_error_fix.erl \
   test_encode_capabilities.erl test_error_module.erl \
   test_codegen.erl test_marketplace_gen.erl test_final.erl \
   test_monitor.erl test_simple_otel.erl test_tcp_transport.erl \
   test_supervision.erl test_trace.erl test_metrics.erl \
   test_quick_stress.erl run_stress_test.erl run_stress_retest.erl \
   quick_stress_test.erl run_collapse_test.erl run_sup_tests.erl \
   test_batch.erl test_batch4.erl test_batch4_db_ops.erl \
   test_batch9_mcp_roundtrip.erl run_batch14_test.erl \
   run_batch18_simple.erl run_batch18_test.erl test_batch14.sh \
   test_gcp_standalone.erl test_gcp_gen_server.erl \
   test_gcp_full_integration.erl gen_marketplace.erl test_100k_pooling.erl
```

### Phase 1.2 - Delete Broken Module
```bash
rm apps/erlmcp_core/src/pricing/erlmcp_pricing_upgrade.erl.broken
```

### Phase 1.3 - Delete Markdown Files
```bash
# Copy-paste this entire block
rm AGENT_11_COMPLETION_REPORT.md AGENT_11_INTEGRATION_DELIVERY.md \
   AGENT_12_COMPLETION.md AGENT_15_CACHE_DELIVERABLES.md \
   AGENT_15_PRODUCTION_READINESS_CERTIFICATION.txt \
   AGENT_18_COMPLETE.md AGENT_3_DELIVERY_INDEX.md \
   AGENT_4_BACKPRESSURE_DELIVERABLES.md BATCH16_RESULTS.txt \
   BATCH6_AUTH_REPORT.md BATCH6_RESULTS.md BATCH_18_SUMMARY.md \
   batch14_results.txt test_batch14.sh AUDIT_DELIVERABLES.md \
   AUDIT_FILES_MANIFEST.md AUDIT_FINDINGS.txt \
   AUDIT_SUMMARY_TABLE.txt COMPLETE_AUDIT_MANIFEST.txt \
   DELIVERABLES.md DELIVERABLES.txt \
   DELIVERABLES_ERROR_HANDLING.md DELIVERABLES_GAP26.md \
   DELIVERY.txt DELIVERY_MANIFEST.txt DELIVERY_REPORT_AUTO_FIX.md
```

---

## Contact & Support

**Questions?** Open discussion in GitHub or contact maintainers.

**Document Status:** ACTIVE
**Last Updated:** 2026-01-30
**Next Review:** After Phase 1 completion
