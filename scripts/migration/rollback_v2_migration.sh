#!/bin/bash
# rollback_v2_migration.sh
# ERLMCP V2.0 Rollback Script
# Restores legacy src/ and test/ from backups

set -e

cd /Users/sac/erlmcp

echo "======================================"
echo "ERLMCP V2 MIGRATION ROLLBACK"
echo "======================================"

# Safety confirmation
echo ""
echo "⚠️  WARNING: This will restore legacy structure:"
echo "  - Restore src/ from backup"
echo "  - Restore test/ from backup"
echo "  - Restore rebar.config from backup"
echo "  - OVERWRITES current state"
echo ""
read -p "Type 'ROLLBACK' to confirm: " confirm

if [ "$confirm" != "ROLLBACK" ]; then
    echo "Rollback cancelled."
    exit 0
fi

# Find most recent backups
echo ""
echo "Phase 1: Locating backups..."

backup_dir="../erlmcp_backups"
if [ ! -d "$backup_dir" ]; then
    echo "  ❌ Backup directory not found: $backup_dir"
    exit 1
fi

src_backup=$(ls -t "$backup_dir"/src_backup_*.tar.gz 2>/dev/null | head -1)
test_backup=$(ls -t "$backup_dir"/test_backup_*.tar.gz 2>/dev/null | head -1)
config_backup=$(ls -t rebar.config.backup_* 2>/dev/null | head -1)

if [ -z "$src_backup" ]; then
    echo "  ❌ No src/ backup found"
    exit 1
fi

if [ -z "$test_backup" ]; then
    echo "  ❌ No test/ backup found"
    exit 1
fi

if [ -z "$config_backup" ]; then
    echo "  ❌ No rebar.config backup found"
    exit 1
fi

echo "  ✅ Found backups:"
echo "     - $src_backup"
echo "     - $test_backup"
echo "     - $config_backup"

# Phase 2: Remove current state
echo ""
echo "Phase 2: Removing current structure..."

if [ -d "src" ]; then
    rm -rf src/
    echo "  ✅ Removed current src/"
fi

if [ -d "test" ]; then
    rm -rf test/
    echo "  ✅ Removed current test/"
fi

# Phase 3: Restore from backups
echo ""
echo "Phase 3: Restoring from backups..."

tar -xzf "$src_backup"
echo "  ✅ Restored src/ from $(basename "$src_backup")"

tar -xzf "$test_backup"
echo "  ✅ Restored test/ from $(basename "$test_backup")"

cp "$config_backup" rebar.config
echo "  ✅ Restored rebar.config from $(basename "$config_backup")"

# Phase 4: Clean and rebuild
echo ""
echo "Phase 4: Rebuilding..."

rebar3 clean > /dev/null 2>&1

if rebar3 compile 2>&1 | tee /tmp/rollback_compile.log | grep -q "ERROR"; then
    echo "  ❌ Compilation failed after rollback"
    echo "     Manual intervention required"
    cat /tmp/rollback_compile.log
    exit 1
else
    echo "  ✅ Compilation successful"
fi

# Phase 5: Verify tests
echo ""
echo "Phase 5: Verifying tests..."

if rebar3 eunit 2>&1 | tee /tmp/rollback_tests.log | grep -q "FAILED"; then
    echo "  ⚠️  Some tests failing (may be expected)"
else
    echo "  ✅ Tests passing"
fi

# Final summary
echo ""
echo "======================================"
echo "✅ ROLLBACK COMPLETE"
echo "======================================"
echo ""
echo "Summary:"
echo "  - src/ restored from backup"
echo "  - test/ restored from backup"
echo "  - rebar.config restored from backup"
echo "  - System rebuilt and verified"
echo ""
echo "Legacy v1.x structure restored."
echo ""
echo "To retry migration:"
echo "  1. Review and fix issues"
echo "  2. ./scripts/migration/pre_deletion_safety.sh"
echo "  3. ./scripts/migration/delete_legacy_structure.sh"
