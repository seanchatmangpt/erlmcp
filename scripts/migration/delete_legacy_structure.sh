#!/bin/bash
# delete_legacy_structure.sh
# ERLMCP V2.0 Legacy Structure Deletion
# WARNING: This permanently deletes src/ and test/ directories

set -e  # Exit on any error

cd /Users/sac/erlmcp

echo "======================================"
echo "ERLMCP V2 LEGACY STRUCTURE DELETION"
echo "======================================"

# Safety confirmation
echo ""
echo "⚠️  WARNING: This will permanently delete:"
echo "  - src/ directory (127 .erl files)"
echo "  - test/ directory (349 entries)"
echo "  - GraphQL modules (8 files)"
echo ""
echo "Backups will be created in: ../erlmcp_backups/"
echo ""
read -p "Type 'DELETE' to confirm: " confirm

if [ "$confirm" != "DELETE" ]; then
    echo "Deletion cancelled."
    exit 0
fi

# Phase 1: Create backups
echo ""
echo "Phase 1: Creating backups..."
timestamp=$(date +%Y%m%d_%H%M%S)

if [ -d "src" ]; then
    tar -czf "../erlmcp_backups/src_backup_${timestamp}.tar.gz" src/
    echo "  ✅ src/ backed up to src_backup_${timestamp}.tar.gz"
fi

if [ -d "test" ]; then
    tar -czf "../erlmcp_backups/test_backup_${timestamp}.tar.gz" test/
    echo "  ✅ test/ backed up to test_backup_${timestamp}.tar.gz"
fi

# Backup rebar.config
cp rebar.config "rebar.config.backup_${timestamp}"
echo "  ✅ rebar.config backed up"

# Phase 2: Delete GraphQL modules
echo ""
echo "Phase 2: Removing GraphQL modules..."
cd apps/erlmcp_transports/src

graphql_count=0
for file in erlmcp_graphql_*.erl* erlmcp_transport_graphql.erl*; do
    if [ -f "$file" ]; then
        rm -v "$file"
        ((graphql_count++))
    fi
done

cd ../test
if [ -f "erlmcp_graphql_tests.erl" ]; then
    rm -v erlmcp_graphql_tests.erl
    ((graphql_count++))
fi

cd /Users/sac/erlmcp
echo "  ✅ Removed $graphql_count GraphQL files"

# Phase 3: Delete legacy test directory
echo ""
echo "Phase 3: Deleting legacy test/ directory..."
if [ -d "test" ]; then
    test_count=$(ls test/* 2>/dev/null | wc -l)
    rm -rf test/
    echo "  ✅ test/ deleted ($test_count entries removed)"
else
    echo "  ⚠️  test/ already deleted"
fi

# Phase 4: Delete legacy src directory
echo ""
echo "Phase 4: Deleting legacy src/ directory..."
if [ -d "src" ]; then
    src_count=$(ls src/*.erl 2>/dev/null | wc -l)
    rm -rf src/
    echo "  ✅ src/ deleted ($src_count files removed)"
else
    echo "  ⚠️  src/ already deleted"
fi

# Phase 5: Update rebar.config
echo ""
echo "Phase 5: Updating root rebar.config..."

# Create clean version without legacy directory references
awk '
    /^{src_dirs, \["src"\]}\.$/ { next }
    /^{test_dirs, \["test"\]}\.$/ { next }
    /^{include_dirs, \["include"\]}\.$/ { next }
    { print }
' rebar.config > rebar.config.new

mv rebar.config.new rebar.config
echo "  ✅ rebar.config updated (removed legacy directory references)"

# Phase 6: Clean and recompile
echo ""
echo "Phase 6: Clean rebuild..."
rebar3 clean

if rebar3 compile 2>&1 | tee /tmp/compile_v2.log | grep -q "ERROR"; then
    echo "  ❌ Compilation failed!"
    echo ""
    echo "ROLLBACK REQUIRED:"
    echo "  cd /Users/sac/erlmcp"
    echo "  tar -xzf ../erlmcp_backups/src_backup_${timestamp}.tar.gz"
    echo "  tar -xzf ../erlmcp_backups/test_backup_${timestamp}.tar.gz"
    echo "  cp rebar.config.backup_${timestamp} rebar.config"
    exit 1
else
    echo "  ✅ Compilation successful"
fi

# Final summary
echo ""
echo "======================================"
echo "✅ DELETION COMPLETE"
echo "======================================"
echo ""
echo "Summary:"
echo "  - GraphQL modules: REMOVED ($graphql_count files)"
echo "  - Legacy test/: DELETED"
echo "  - Legacy src/: DELETED"
echo "  - Backups: ../erlmcp_backups/"
echo "    - src_backup_${timestamp}.tar.gz"
echo "    - test_backup_${timestamp}.tar.gz"
echo "    - rebar.config.backup_${timestamp}"
echo ""
echo "Next steps:"
echo "  1. ./scripts/migration/post_deletion_validation.sh"
echo "  2. Update documentation (CHANGELOG, README, architecture)"
echo "  3. Git commit changes"
echo "  4. Tag release: v2.0.0"
