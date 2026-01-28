# erlmcp v2.0 Cleanup - Quick Start Guide

**5-Minute Summary for Busy Developers**

---

## What's Happening?

Migrating from **monolithic** structure → **umbrella** structure:
- **Old:** 127 files in `src/`, 349 files in `test/`
- **New:** 4 apps in `apps/` (core, transports, observability, tcps)
- **Remove:** GraphQL support (dependency unavailable)

---

## Pre-Flight Checklist (5 minutes)

```bash
cd /Users/sac/erlmcp

# 1. Run safety checks
./scripts/migration/pre_deletion_safety.sh

# Expected: All 8 checks pass ✅
```

**If any check fails:** STOP. Fix issues before deletion.

---

## Execute Cleanup (10 minutes)

```bash
# 2. Delete legacy structure (creates backups automatically)
./scripts/migration/delete_legacy_structure.sh

# Type: DELETE (when prompted)
# Expected: Backups created, compilation succeeds
```

---

## Validate Migration (15 minutes)

```bash
# 3. Run validation suite
./scripts/migration/post_deletion_validation.sh

# Expected: All 10 checks pass ✅
```

---

## If Something Goes Wrong

```bash
# Rollback to v1.x (5 minutes)
./scripts/migration/rollback_v2_migration.sh

# Type: ROLLBACK (when prompted)
```

---

## What Gets Deleted?

### ❌ Removed
- `src/` directory (127 .erl files)
- `test/` directory (349 entries, mostly .skip files)
- GraphQL modules (8 files):
  - `erlmcp_graphql_resolver.erl`
  - `erlmcp_graphql_schema.erl`
  - `erlmcp_transport_graphql.erl`

### ✅ Preserved
- `apps/erlmcp_core/` (35 modules)
- `apps/erlmcp_transports/` (22 modules, minus GraphQL)
- `apps/erlmcp_observability/` (26 modules)
- `apps/tcps_erlmcp/` (68 modules)

---

## Backups Location

All backups saved to: `../erlmcp_backups/`

Files created:
- `src_backup_YYYYMMDD_HHMMSS.tar.gz`
- `test_backup_YYYYMMDD_HHMMSS.tar.gz`
- `rebar.config.backup_YYYYMMDD_HHMMSS`

**Keep for 30 days** before deleting.

---

## Post-Cleanup Tasks

### 1. Update Documentation (20 minutes)

```bash
# Update CHANGELOG.md
# Update README.md (umbrella structure)
# Update docs/architecture.md (app boundaries)
```

### 2. Git Commit (5 minutes)

```bash
git add -A
git commit -m "feat: migrate to umbrella v2.0 structure

BREAKING CHANGE: Migrated from monolithic src/ to umbrella apps/

- erlmcp_core: 35 modules (JSON-RPC, Client/Server, Registry)
- erlmcp_transports: 22 modules (STDIO, TCP, HTTP, WebSocket)
- erlmcp_observability: 26 modules (OTEL, Metrics, Chaos)
- tcps_erlmcp: 68 modules (Quality gates)

REMOVED: GraphQL support (dependency unavailable)
DELETED: Legacy src/ (127 files) and test/ (349 files)

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>"

git tag -a v2.0.0 -m "Release v2.0.0: Umbrella Architecture"
```

### 3. Notify Team (2 minutes)

Send migration summary:
- ✅ Umbrella structure live
- ✅ All tests passing
- ✅ GraphQL removed
- 📚 Migration guide: `docs/migration/V2_CLEANUP_STRATEGY.md`

---

## Troubleshooting

### Q: Compilation fails after deletion
**A:** Run rollback script immediately:
```bash
./scripts/migration/rollback_v2_migration.sh
```

### Q: Tests failing in umbrella structure
**A:** Check test organization:
```bash
# Tests should be in apps/*/test/, not root test/
ls apps/*/test/
```

### Q: Missing modules
**A:** Verify migration map in `V2_CLEANUP_STRATEGY.md` Appendix A

### Q: Performance regression
**A:** Run benchmarks:
```bash
make benchmark-quick
# Compare to baseline in V2_CLEANUP_STRATEGY.md Phase 1.4
```

---

## Key Scripts

| Script | Purpose | Time |
|--------|---------|------|
| `pre_deletion_safety.sh` | Verify migration readiness | 5 min |
| `delete_legacy_structure.sh` | Execute deletion | 10 min |
| `post_deletion_validation.sh` | Validate system integrity | 15 min |
| `rollback_v2_migration.sh` | Restore v1.x structure | 5 min |

---

## Success Criteria

Before declaring "DONE":
- [x] All 8 pre-deletion checks passed
- [x] Deletion executed without errors
- [x] All 10 validation checks passed
- [x] Compilation succeeds (zero errors)
- [x] All tests passing (zero failures)
- [x] Dialyzer clean (zero errors)
- [x] Xref clean (no undefined functions)
- [x] No performance regression (±10%)
- [x] Documentation updated
- [x] Git committed and tagged

---

## Timeline

- **Preparation:** 5 min (safety checks)
- **Execution:** 10 min (deletion)
- **Validation:** 15 min (comprehensive checks)
- **Documentation:** 20 min (updates)
- **Git operations:** 5 min (commit/tag)
- **Total:** ~55 minutes

---

## Emergency Contacts

**If rollback fails or critical issue:**
1. Preserve all backup files in `../erlmcp_backups/`
2. Document exact error messages
3. Review full strategy: `docs/migration/V2_CLEANUP_STRATEGY.md`
4. Contact: Erlang Architect Agent

---

## References

- **Full Strategy:** `docs/migration/V2_CLEANUP_STRATEGY.md` (10 phases, 50 pages)
- **Migration Map:** Appendix A (module-by-module mapping)
- **Safety Checks:** Appendix B (pre-deletion validation)
- **Rollback Plan:** Phase 8 (emergency procedures)

---

**Ready to start?**
```bash
cd /Users/sac/erlmcp
./scripts/migration/pre_deletion_safety.sh
```

**Good luck!** 🚀
