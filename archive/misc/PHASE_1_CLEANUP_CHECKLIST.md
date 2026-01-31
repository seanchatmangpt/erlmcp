# Phase 1 Cleanup Checklist - LOW RISK DELETION

**Estimated Time**: 4-6 hours
**Risk Level**: LOW (only removes development artifacts and duplicates)
**Reversibility**: 100% (all deletions are `git rm`)

---

## Pre-Cleanup Verification

- [ ] All current tests pass: `make test`
- [ ] Create pre-cleanup tag: `git tag erlmcp-pre-consolidation`
- [ ] Document current state: `git log --oneline | head -5`
- [ ] Verify no active benchmarks running
- [ ] Backup baseline files: `cp -r /tmp/erlmcp_baselines /tmp/erlmcp_baselines.backup`

---

## STEP 1: Verify Files to Delete (Read Before Delete)

### 1.1 Development Artifacts

#### test/erlmcp_simple_benchmark.erl
- [ ] Read file: Verify it's minimal (~50 LOC)
- [ ] Check: Is this used elsewhere? `grep -r "erlmcp_simple_benchmark" . --exclude-dir=_build`
- [ ] Expected: 0 references
- [ ] Delete: YES

#### test/erlmcp_simple_stress.erl
- [ ] Read file: Verify it's minimal (~60 LOC)
- [ ] Check: Is this used elsewhere? `grep -r "erlmcp_simple_stress" . --exclude-dir=_build`
- [ ] Expected: 0 references
- [ ] Delete: YES

#### test_logging_stress.erl
- [ ] Read file: Verify it's old version
- [ ] Check: Confirm swarm/stress-test/erlmcp_logging_100k_stress.erl exists and is newer
- [ ] Delete: YES (keep swarm version)

#### test_quick_stress.erl
- [ ] Read file: Verify it's minimal development version
- [ ] Check: Is this called from anywhere? `grep -r "test_quick_stress" . --exclude-dir=_build`
- [ ] Expected: 0 references
- [ ] Delete: YES

#### quick_stress_test.erl
- [ ] Read file: Verify it's minimal/old
- [ ] Check: Is this called from anywhere? `grep -r "quick_stress_test" . --exclude-dir=_build`
- [ ] Expected: 0 references
- [ ] Delete: YES

#### run_stress_test.erl
- [ ] Read file: Verify it's just a runner
- [ ] Check: Confirm erlmcp_master_stress_orchestrator exists and is newer
- [ ] Delete: YES (keep orchestrator)

### 1.2 Old Versions (Replaced by Newer)

#### test/erlmcp_master_stress_test.erl
- [ ] Read file: Verify it says "old version" or similar
- [ ] Check: Confirm erlmcp_master_stress_orchestrator exists
- [ ] Location of new: `/Users/sac/erlmcp/swarm/stress-test/erlmcp_master_stress_orchestrator.erl`
- [ ] Delete: YES (keep orchestrator)

### 1.3 Exact Duplicates (Same Code, Different Location)

#### test/registry_100k_stress.erl
- [ ] Read file: Check it's non-SUITE version
- [ ] Check: Is there a `_SUITE` version?
- [ ] Location of SUITE: `/Users/sac/erlmcp/test/erlmcp_registry_100k_stress_SUITE.erl`
- [ ] Compare: Are they testing same thing? YES
- [ ] Delete: YES (keep SUITE)

#### test/tcp_real_bench_tests.erl
- [ ] Read file: Verify it wraps tcp_real_bench
- [ ] Check: Verify bench/transport_real/tcp_real_bench.erl exists
- [ ] Compare: Are they same module?
- [ ] Delete: YES (keep bench version as source of truth)

---

## STEP 2: Execute Deletions (Reversible with git)

```bash
# Create cleanup branch
git checkout -b phase1-cleanup

# Delete development artifacts
git rm test/erlmcp_simple_benchmark.erl
git rm test/erlmcp_simple_stress.erl
git rm test_logging_stress.erl
git rm test_quick_stress.erl
git rm quick_stress_test.erl
git rm run_stress_test.erl

# Delete old versions
git rm test/erlmcp_master_stress_test.erl

# Delete duplicates
git rm test/registry_100k_stress.erl
git rm test/tcp_real_bench_tests.erl

# Verify deletions
git status

# Expected output:
# deleted: test/erlmcp_simple_benchmark.erl
# deleted: test/erlmcp_simple_stress.erl
# ... (all 9 files)
```

- [ ] Verify 9 files marked for deletion
- [ ] No accidental deletions

---

## STEP 3: Verify No Breakage (Critical!)

### 3.1 Check rebar.config

```bash
# Verify no deleted files referenced in rebar.config
grep -E "erlmcp_simple_benchmark|erlmcp_simple_stress|test_logging_stress|test_quick_stress|quick_stress_test|run_stress_test|erlmcp_master_stress_test|registry_100k_stress|tcp_real_bench_tests" rebar.config

# Expected: No matches
```

- [ ] No references in rebar.config

### 3.2 Check Test Configuration

```bash
# Verify no references in test files
grep -r "erlmcp_simple_benchmark\|erlmcp_simple_stress\|test_logging_stress\|test_quick_stress\|quick_stress_test\|run_stress_test" test/ --exclude-dir=_build

# Expected: No matches (except possibly in comments)
```

- [ ] No references in other test files

### 3.3 Check Source References

```bash
# Verify no source code calls these modules
grep -r "erlmcp_simple_benchmark\|erlmcp_simple_stress\|test_logging_stress\|test_quick_stress\|quick_stress_test\|run_stress_test" src/ --exclude-dir=_build

# Expected: No matches
```

- [ ] No references in source

### 3.4 Rebuild and Test

```bash
# Full rebuild
make clean
make compile

# Expected: Success

# Run quick test
make test-unit

# Expected: All tests pass
```

- [ ] Rebuild successful
- [ ] Unit tests pass
- [ ] No compiler warnings

---

## STEP 4: Commit Deletions

```bash
# Commit with clear message
git commit -m "Phase 1: Delete 9 development artifacts and duplicates

Removed:
  - test/erlmcp_simple_benchmark.erl (too basic)
  - test/erlmcp_simple_stress.erl (too basic)
  - test_logging_stress.erl (old version)
  - test_quick_stress.erl (development)
  - quick_stress_test.erl (development)
  - run_stress_test.erl (superseded by orchestrators)
  - test/erlmcp_master_stress_test.erl (old version)
  - test/registry_100k_stress.erl (duplicate SUITE)
  - test/tcp_real_bench_tests.erl (duplicate with bench/)

Impact:
  - Removed 750 LOC of dead code
  - No functionality loss (duplicates/artifacts only)
  - All tests still pass
  - 9 files deleted

Verified:
  - No external references
  - Rebuild successful
  - All tests pass
"
```

- [ ] Commit message is clear
- [ ] Commit successful

---

## STEP 5: Run Full Test Suite

```bash
# Full CI simulation
make check

# Expected: All checks pass
# - Compilation
# - Xref (cross-reference)
# - Dialyzer (type checking)
# - Unit tests
# - Integration tests
```

- [ ] Compilation passes
- [ ] Xref clean
- [ ] Dialyzer clean
- [ ] All tests pass

---

## STEP 6: Update Tracking

### 6.1 Create Progress Document

Create `/Users/sac/erlmcp/PHASE_1_CLEANUP_COMPLETE.md`:

```markdown
# Phase 1 Cleanup - COMPLETE

**Date Completed**: 2026-01-27
**Files Deleted**: 9
**LOC Removed**: 750
**Risk Level**: LOW

## Deleted Files

1. test/erlmcp_simple_benchmark.erl (50 LOC)
2. test/erlmcp_simple_stress.erl (60 LOC)
3. test_logging_stress.erl (40 LOC)
4. test_quick_stress.erl (30 LOC)
5. quick_stress_test.erl (40 LOC)
6. run_stress_test.erl (80 LOC)
7. test/erlmcp_master_stress_test.erl (150 LOC)
8. test/registry_100k_stress.erl (200 LOC)
9. test/tcp_real_bench_tests.erl (100 LOC)

## Verification Results

- [x] Rebuild successful
- [x] All tests pass
- [x] No external references
- [x] Xref clean
- [x] Dialyzer clean

## Next Phase

Ready for Phase 2: Metrology Standardization (Week 2)
```

- [ ] Progress document created

### 6.2 Update Main Audit Document

Add to COMPREHENSIVE_BENCHMARK_AUDIT.md:

```markdown
## CONSOLIDATION PROGRESS

### Phase 1: ✓ COMPLETE (2026-01-27)
- Deleted 9 files (750 LOC)
- Verified: All tests pass
- Next: Phase 2 (Metrology)
```

- [ ] Audit document updated

---

## STEP 7: Prepare for Phase 2

- [ ] Review BENCHMARK_CONSOLIDATION_ROADMAP.md Phase 2 section
- [ ] Identify team member for Phase 2 (metrology fixes)
- [ ] Schedule Phase 2 start date
- [ ] Create Phase 2 task list

---

## Rollback Plan (If Needed)

If anything breaks during cleanup:

```bash
# Reset to pre-cleanup state
git reset --hard erlmcp-pre-consolidation

# Or selectively restore deleted file
git checkout erlmcp-pre-consolidation -- test/erlmcp_simple_benchmark.erl
```

---

## Approval Gates

### Gate 1: Pre-Cleanup Verification
- [ ] Team lead reviews planned deletions
- [ ] Confirms no production dependencies
- [ ] Approves: PROCEED TO STEP 1

### Gate 2: Post-Deletion Testing
- [ ] All tests pass
- [ ] Xref clean
- [ ] Dialyzer clean
- [ ] Approves: PROCEED TO COMMIT

### Gate 3: Post-Commit
- [ ] Code review approved
- [ ] Merge to main approved
- [ ] Documentation updated
- [ ] Approves: PHASE 1 COMPLETE

---

## Success Criteria

- [x] 9 files deleted
- [x] 750 LOC removed
- [x] 100% reversible (git history clean)
- [x] All tests pass
- [x] No external references
- [x] Clear commit message
- [x] Documentation updated
- [x] Ready for Phase 2

---

## Time Tracking

- [ ] Pre-cleanup verification: ~30 min
- [ ] Step 1 (read files): ~30 min
- [ ] Step 2 (execute deletions): ~10 min
- [ ] Step 3 (verify no breakage): ~30 min
- [ ] Step 4 (commit): ~10 min
- [ ] Step 5 (full test): ~30 min
- [ ] Step 6 (documentation): ~20 min
- [ ] Step 7 (prepare phase 2): ~20 min

**Total Time**: 3-4 hours
**Target Completion**: Within 1 working day

---

## Sign-off

**Completed By**: _________________ (Name)
**Date Completed**: _________________ (Date)
**Reviewed By**: _________________ (Name)
**Date Reviewed**: _________________ (Date)

---

## Next Phase

**Phase 2: Metrology Standardization (Week 2)**
- Start date: [TBD after Phase 1 approval]
- Duration: 20 hours
- Focus: Fix measurement bugs, add missing percentiles
- Entry point: BENCHMARK_CONSOLIDATION_ROADMAP.md Phase 2 section

See BENCHMARK_CONSOLIDATION_ROADMAP.md for full 4-week plan.
