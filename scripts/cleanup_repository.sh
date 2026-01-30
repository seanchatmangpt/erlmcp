#!/bin/bash
set -euo pipefail

# erlmcp Repository Cleanup Script
# Usage: ./scripts/cleanup_repository.sh [--dry-run]

cd "$(dirname "$0")/.."

DRY_RUN=false
[[ "${1:-}" == "--dry-run" ]] && DRY_RUN=true && echo "=== DRY RUN MODE ==="

# Colors
G='\033[0;32m'; Y='\033[1;33m'; NC='\033[0m'
info() { echo -e "${G}[INFO]${NC} $1"; }
warn() { echo -e "${Y}[WARN]${NC} $1"; }

# Create archive
ARCHIVE="docs/archive/cleanup_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$ARCHIVE"
info "Archive: $ARCHIVE"

# Clean test results (keep 3 most recent)
info "=== Test Results ==="
if [[ -d test_results/quality_gates ]]; then
    cd test_results/quality_gates
    for type in coverage eunit xref summary; do
        for ext in log txt; do
            files=$(ls -t ${type}_*.${ext} 2>/dev/null | tail -n +4 || true)
            if [[ -n "$files" ]]; then
                if $DRY_RUN; then
                    echo "  Would delete: $files"
                else
                    echo "$files" | xargs rm -f
                fi
            fi
        done
    done
    cd ../..
    info "Kept 3 most recent of each type"
fi

# Archive markdown reports
info "=== Agent Documentation ==="
patterns=("AGENT_*.md" "*_SUMMARY.md" "*_REPORT.md" "MERGE_*.md" "*_FIX*.md" "*_ASSESSMENT.md" "*_ANALYSIS.md")
total=0
for pattern in "${patterns[@]}"; do
    count=$(find . -maxdepth 1 -type f -name "$pattern" 2>/dev/null | wc -l | tr -d ' ')
    total=$((total + count))
    if [[ $count -gt 0 ]]; then
        if $DRY_RUN; then
            echo "  Would archive $count files: $pattern"
        else
            find . -maxdepth 1 -type f -name "$pattern" -exec mv {} "$ARCHIVE/" \;
        fi
    fi
done
info "Processed $total documentation files"

# Clean backup files
info "=== Backup Files ==="
patterns=("*.bak" "*.broken" "*.crashdump" "*~" "*.swp")
for pattern in "${patterns[@]}"; do
    count=$(find apps -type f -name "$pattern" 2>/dev/null | wc -l | tr -d ' ')
    if [[ $count -gt 0 ]]; then
        if $DRY_RUN; then
            echo "  Would delete $count: $pattern"
        else
            find apps -type f -name "$pattern" -delete
        fi
    fi
done

# Mnesia database
info "=== Mnesia Database ==="
if [[ -d Mnesia.nonode@nohost ]]; then
    if $DRY_RUN; then
        echo "  Would archive: Mnesia.nonode@nohost/"
    else
        mv Mnesia.nonode@nohost "$ARCHIVE/" 2>/dev/null || rm -rf Mnesia.nonode@nohost
        info "Archived Mnesia database"
    fi
fi

# Summary
info "=== Summary ==="
if $DRY_RUN; then
    warn "DRY RUN - no changes made"
    warn "Run without --dry-run to execute"
else
    info "Cleanup complete"
    archive_files=$(find "$ARCHIVE" -type f 2>/dev/null | wc -l | tr -d ' ')
    if [[ $archive_files -gt 0 ]]; then
        info "Archived $archive_files files to: $ARCHIVE"
    else
        rmdir "$ARCHIVE" 2>/dev/null || true
    fi
fi

info "=== Git Status ==="
git status --short | head -n 15
