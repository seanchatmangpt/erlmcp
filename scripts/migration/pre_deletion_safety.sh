#!/bin/bash
# pre_deletion_safety.sh
# ERLMCP V2.0 Pre-Deletion Safety Checks
# Run this before executing deletion to verify migration readiness

set -e  # Exit on any error

cd /Users/sac/erlmcp

echo "======================================"
echo "ERLMCP V2 PRE-DELETION SAFETY CHECKS"
echo "======================================"

# Check 1: Umbrella apps exist
echo ""
echo "[1/8] Checking umbrella structure..."
for app in erlmcp_core erlmcp_transports erlmcp_observability tcps_erlmcp; do
    if [ -d "apps/$app/src" ]; then
        echo "  ✅ apps/$app/src exists"
    else
        echo "  ❌ MISSING: apps/$app/src"
        exit 1
    fi
done

# Check 2: Compilation works
echo ""
echo "[2/8] Testing compilation..."
if rebar3 compile 2>&1 | grep -q "ERROR"; then
    echo "  ❌ Compilation failed"
    exit 1
else
    echo "  ✅ Compilation succeeded"
fi

# Check 3: Critical modules present in apps
echo ""
echo "[3/8] Checking critical modules..."
for module in erlmcp_client erlmcp_server erlmcp_registry erlmcp_json_rpc; do
    if find apps/*/src -name "${module}.erl" | grep -q "."; then
        echo "  ✅ $module found"
    else
        echo "  ❌ MISSING: $module"
        exit 1
    fi
done

# Check 4: Tests exist in apps
echo ""
echo "[4/8] Checking test structure..."
for app in erlmcp_core erlmcp_transports erlmcp_observability tcps_erlmcp; do
    if [ -d "apps/$app/test" ]; then
        test_count=$(ls apps/$app/test/*.erl 2>/dev/null | wc -l)
        echo "  ✅ apps/$app/test: $test_count tests"
    else
        echo "  ⚠️  No test dir: apps/$app/test"
    fi
done

# Check 5: Backups directory ready
echo ""
echo "[5/8] Checking backup location..."
if [ ! -d "../erlmcp_backups" ]; then
    mkdir -p ../erlmcp_backups
    echo "  ✅ Created ../erlmcp_backups"
else
    echo "  ✅ ../erlmcp_backups exists"
fi

# Check 6: Legacy directories exist
echo ""
echo "[6/8] Checking legacy structure..."
if [ -d "src" ] && [ -d "test" ]; then
    src_count=$(ls src/*.erl 2>/dev/null | wc -l)
    test_count=$(ls test/* 2>/dev/null | wc -l)
    echo "  ✅ src/: $src_count files"
    echo "  ✅ test/: $test_count entries"
else
    echo "  ❌ Legacy directories missing (already deleted?)"
    exit 1
fi

# Check 7: Git status
echo ""
echo "[7/8] Checking git status..."
if git status --porcelain | grep -q "^??"; then
    echo "  ⚠️  Untracked files present (normal)"
else
    echo "  ✅ No untracked files"
fi

# Check 8: Running processes
echo ""
echo "[8/8] Checking for running erlmcp processes..."
if pgrep -f "erlmcp" > /dev/null; then
    echo "  ⚠️  Warning: erlmcp processes running"
    echo "     Consider stopping before deletion"
else
    echo "  ✅ No erlmcp processes running"
fi

echo ""
echo "======================================"
echo "✅ ALL SAFETY CHECKS PASSED"
echo "======================================"
echo ""
echo "Ready to proceed with deletion."
echo "Backups will be created before deletion."
echo ""
echo "Next steps:"
echo "  1. ./scripts/migration/delete_legacy_structure.sh"
echo "  2. Review deletion output"
echo "  3. ./scripts/migration/post_deletion_validation.sh"
