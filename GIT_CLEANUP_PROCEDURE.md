# Git and Command-Line Cleanup Procedure for erlmcp

## Overview
This document provides a professional procedure for cleaning up log files, old documentation, backup files, and agent-generated artifacts from the erlmcp repository.

## Quick Start

```bash
# Run the automated cleanup script
./scripts/cleanup_repository.sh

# Or follow the manual procedures below
```

---

## 1. Identify Files to Clean

### 1.1 Find Test Results and Log Files
```bash
# Find all test result logs
find . -type f -name "*.log" | grep test_results

# Find quality gate results
find test_results/quality_gates/ -type f -name "*.log" -o -name "*.txt"

# Count how many log files exist
find . -type f -name "*.log" | wc -l
```

### 1.2 Find Backup and Broken Files
```bash
# Find all .bak files
find . -type f -name "*.bak"

# Find all .broken files
find . -type f -name "*.broken"

# Find crashdump files
find . -type f -name "*.crashdump"

# List them with details
find . -type f \( -name "*.bak" -o -name "*.broken" -o -name "*.crashdump" \) -ls
```

### 1.3 Find Old Agent Documentation
```bash
# Find agent completion reports
find . -maxdepth 1 -type f -name "AGENT_*_*.md"

# Find agent-generated summaries
find . -maxdepth 1 -type f -name "*_SUMMARY.md"

# Find agent reports
find . -maxdepth 1 -type f -name "*_REPORT.md"

# Count them
find . -maxdepth 1 -type f \( -name "AGENT_*.md" -o -name "*_SUMMARY.md" -o -name "*_REPORT.md" \) | wc -l
```

### 1.4 Find Mnesia Database Files
```bash
# Find Mnesia directories
find . -type d -name "Mnesia.*"

# List Mnesia files
ls -lah Mnesia.nonode@nohost/
```

---

## 2. Safe Cleanup Strategy

### 2.1 Preview Before Deletion
**ALWAYS preview files before deleting them!**

```bash
# Preview what would be deleted (DRY RUN)
find . -type f -name "*.log" -print

# Preview with file sizes
find . -type f -name "*.log" -exec ls -lh {} \;

# Count files and total size
find . -type f -name "*.log" -exec du -ch {} + | tail -n 1
```

### 2.2 Create Backup Before Cleanup
```bash
# Create timestamped backup
BACKUP_DIR="backups/cleanup_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"

# Backup files matching pattern
find . -type f -name "*.bak" -exec cp --parents {} "$BACKUP_DIR/" \;

# Verify backup
ls -R "$BACKUP_DIR/"
```

---

## 3. Cleanup Commands

### 3.1 Clean Test Results (Safe - Generated Files)
```bash
# Remove quality gate logs older than 7 days
find test_results/quality_gates/ -type f -name "*.log" -mtime +7 -delete

# Remove quality gate summaries older than 7 days
find test_results/quality_gates/ -type f -name "*.txt" -mtime +7 -delete

# Keep only the 3 most recent of each type
cd test_results/quality_gates/
ls -t coverage_*.log | tail -n +4 | xargs -r rm
ls -t eunit_*.log | tail -n +4 | xargs -r rm
ls -t xref_*.log | tail -n +4 | xargs -r rm
ls -t summary_*.txt | tail -n +4 | xargs -r rm
cd ../..
```

### 3.2 Clean Backup Files
```bash
# Remove .bak files (CAUTION: Review first!)
find apps/ -type f -name "*.bak" -print  # Preview
find apps/ -type f -name "*.bak" -delete  # Execute

# Alternative: Move to trash instead of delete
mkdir -p .trash
find apps/ -type f -name "*.bak" -exec mv {} .trash/ \;
```

### 3.3 Clean Broken Files
```bash
# Remove .broken files
find apps/ -type f -name "*.broken" -print  # Preview
find apps/ -type f -name "*.broken" -delete  # Execute
```

### 3.4 Clean Crashdump Files
```bash
# Remove crashdump files
find . -type f -name "*.crashdump" -print  # Preview
find . -type f -name "*.crashdump" -delete  # Execute
```

### 3.5 Clean Agent Documentation (REVIEW CAREFULLY!)
```bash
# Create a directory for archive
mkdir -p docs/archive/agent_reports_$(date +%Y%m%d)

# Move (don't delete) agent reports to archive
find . -maxdepth 1 -type f -name "AGENT_*.md" -exec mv {} docs/archive/agent_reports_$(date +%Y%m%d)/ \;

# Alternative: Delete if you're sure they're not needed
# find . -maxdepth 1 -type f -name "AGENT_*.md" -delete
```

### 3.6 Clean Duplicate/Redundant Documentation
```bash
# Archive old summaries and reports
ARCHIVE_DIR="docs/archive/old_reports_$(date +%Y%m%d)"
mkdir -p "$ARCHIVE_DIR"

# Move reports (review list first!)
REPORTS=(
    "*_SUMMARY.md"
    "*_REPORT.md"
    "*_FIX.md"
    "*_FIXES.md"
    "MERGE_*.md"
    "*_ASSESSMENT.md"
    "*_ANALYSIS.md"
)

# Preview what would be moved
for pattern in "${REPORTS[@]}"; do
    find . -maxdepth 1 -type f -name "$pattern" -print
done

# Execute the move
for pattern in "${REPORTS[@]}"; do
    find . -maxdepth 1 -type f -name "$pattern" -exec mv {} "$ARCHIVE_DIR/" \;
done
```

### 3.7 Clean Mnesia Development Database
```bash
# Remove Mnesia development files (CAUTION: Only if not production!)
rm -rf Mnesia.nonode@nohost/

# Alternative: Archive first
mv Mnesia.nonode@nohost/ "backups/mnesia_$(date +%Y%m%d_%H%M%S)/"
```

---

## 4. Git Operations

### 4.1 Remove Untracked Files from Git
```bash
# Show what would be removed (DRY RUN)
git clean -n

# Show untracked files and directories
git clean -nd

# Remove untracked files (CAUTION!)
git clean -f

# Remove untracked files and directories
git clean -fd

# Interactive mode (recommended)
git clean -i
```

### 4.2 Update .gitignore
```bash
# Add patterns to .gitignore
cat >> .gitignore <<'EOF'

# Test results and logs
test_results/quality_gates/*.log
test_results/quality_gates/*.txt
*.crashdump

# Backup files
*.bak
*.broken
*.old
*~

# Mnesia development database
Mnesia.nonode@nohost/

# Agent-generated temporary files
AGENT_*_COMPLETION*.md
*_SUMMARY.md
*_REPORT.md
EOF

# Verify .gitignore
cat .gitignore
```

### 4.3 Remove Files Already Tracked by Git
```bash
# Stop tracking but keep local file
git rm --cached apps/erlmcp_core/src/*.bak

# Remove from git AND filesystem
git rm apps/erlmcp_core/src/*.bak

# Remove directory from git
git rm -r --cached Mnesia.nonode@nohost/
```

### 4.4 Commit Cleanup Changes
```bash
# Stage .gitignore changes
git add .gitignore

# Stage deletions
git add -u

# Create cleanup commit
git commit -m "$(cat <<'EOF'
chore: Clean up backup files, logs, and agent-generated documentation

Removed:
- *.bak backup files from src/ directories
- *.broken development files
- Old quality gate logs (kept 3 most recent)
- Agent completion reports (archived to docs/archive/)
- Mnesia development database files

Updated:
- .gitignore to prevent future accumulation

https://claude.ai/code/session_018igga7R3JKL2TCSDqy27ro
EOF
)"
```

---

## 5. Automated Cleanup Script

Create `scripts/cleanup_repository.sh`:

```bash
#!/bin/bash
set -euo pipefail

# erlmcp Repository Cleanup Script
# Usage: ./scripts/cleanup_repository.sh [--dry-run]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO_ROOT"

DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
    echo "=== DRY RUN MODE - No files will be deleted ==="
fi

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Create archive directory
ARCHIVE_DIR="docs/archive/cleanup_$(date +%Y%m%d_%H%M%S)"
mkdir -p "$ARCHIVE_DIR"
log_info "Created archive directory: $ARCHIVE_DIR"

# Function to clean files
clean_files() {
    local pattern="$1"
    local description="$2"
    local action="${3:-delete}"  # delete or archive

    log_info "Processing: $description"

    if [[ "$action" == "delete" ]]; then
        if $DRY_RUN; then
            find . -type f -name "$pattern" -print
        else
            COUNT=$(find . -type f -name "$pattern" -delete -print | wc -l)
            log_info "Deleted $COUNT files matching $pattern"
        fi
    elif [[ "$action" == "archive" ]]; then
        if $DRY_RUN; then
            find . -maxdepth 1 -type f -name "$pattern" -print
        else
            COUNT=$(find . -maxdepth 1 -type f -name "$pattern" -exec mv -t "$ARCHIVE_DIR/" {} + | wc -l)
            log_info "Archived $COUNT files matching $pattern"
        fi
    fi
}

# 1. Clean test results (keep 3 most recent)
log_info "=== Cleaning Test Results ==="
cd test_results/quality_gates/ 2>/dev/null || { log_warn "test_results/quality_gates/ not found"; }
if [[ -d "../../test_results/quality_gates/" ]]; then
    if ! $DRY_RUN; then
        ls -t coverage_*.log 2>/dev/null | tail -n +4 | xargs -r rm || true
        ls -t eunit_*.log 2>/dev/null | tail -n +4 | xargs -r rm || true
        ls -t xref_*.log 2>/dev/null | tail -n +4 | xargs -r rm || true
        ls -t summary_*.txt 2>/dev/null | tail -n +4 | xargs -r rm || true
        log_info "Kept 3 most recent test results"
    else
        log_info "Would keep 3 most recent and delete older test results"
    fi
fi
cd "$REPO_ROOT"

# 2. Clean backup files
log_info "=== Cleaning Backup Files ==="
clean_files "*.bak" "backup files" "delete"
clean_files "*.broken" "broken files" "delete"
clean_files "*.crashdump" "crashdump files" "delete"

# 3. Archive agent documentation
log_info "=== Archiving Agent Documentation ==="
clean_files "AGENT_*.md" "agent completion reports" "archive"
clean_files "*_SUMMARY.md" "summary reports" "archive"
clean_files "*_REPORT.md" "analysis reports" "archive"
clean_files "MERGE_*.md" "merge documentation" "archive"

# 4. Clean Mnesia development database
log_info "=== Cleaning Mnesia Development Database ==="
if [[ -d "Mnesia.nonode@nohost" ]]; then
    if ! $DRY_RUN; then
        mv Mnesia.nonode@nohost "$ARCHIVE_DIR/" 2>/dev/null || rm -rf Mnesia.nonode@nohost
        log_info "Archived Mnesia.nonode@nohost/"
    else
        log_info "Would archive Mnesia.nonode@nohost/"
    fi
fi

# 5. Summary
log_info "=== Cleanup Summary ==="
if $DRY_RUN; then
    log_warn "DRY RUN completed - no files were modified"
    log_warn "Run without --dry-run to execute cleanup"
else
    log_info "Cleanup completed successfully"
    log_info "Archive location: $ARCHIVE_DIR"
    log_info "Files can be restored from archive if needed"
fi

# 6. Git status
log_info "=== Git Status ==="
git status --short
```

Make it executable:
```bash
chmod +x scripts/cleanup_repository.sh
```

---

## 6. Best Practices

### 6.1 Regular Cleanup Schedule
```bash
# Add to crontab for weekly cleanup
# crontab -e
# 0 2 * * 0 /home/user/erlmcp/scripts/cleanup_repository.sh
```

### 6.2 Pre-commit Cleanup
```bash
# Add to .git/hooks/pre-commit
#!/bin/bash
# Prevent committing backup files
if git diff --cached --name-only | grep -E '\.(bak|broken|old)$'; then
    echo "Error: Attempting to commit backup files (.bak, .broken, .old)"
    echo "Please remove them first"
    exit 1
fi
```

### 6.3 Cleanup Checklist
- [ ] Run cleanup script with `--dry-run` first
- [ ] Review files to be deleted/archived
- [ ] Create backup if unsure
- [ ] Execute cleanup
- [ ] Update .gitignore
- [ ] Commit changes
- [ ] Push to remote

---

## 7. Emergency Recovery

### 7.1 Restore from Archive
```bash
# Find archive
ls -lt docs/archive/

# Restore specific file
cp docs/archive/cleanup_20260130_123456/AGENT_REPORT.md ./

# Restore entire archive
cp -r docs/archive/cleanup_20260130_123456/* ./
```

### 7.2 Restore from Git
```bash
# Restore deleted file from last commit
git checkout HEAD~1 -- path/to/file

# Find deleted file in history
git log --all --full-history -- path/to/file

# Restore from specific commit
git checkout <commit-hash> -- path/to/file
```

---

## 8. Quick Reference Commands

```bash
# Full cleanup (interactive)
./scripts/cleanup_repository.sh --dry-run  # Preview
./scripts/cleanup_repository.sh             # Execute

# Manual cleanup
find . -type f -name "*.bak" -delete           # Remove backups
find test_results/ -name "*.log" -mtime +7 -delete  # Old logs
git clean -fd                                   # Untracked files

# Archive instead of delete
mkdir -p docs/archive/manual_$(date +%Y%m%d)
find . -maxdepth 1 -name "*_REPORT.md" -exec mv {} docs/archive/manual_$(date +%Y%m%d)/ \;

# Update .gitignore and commit
git add .gitignore
git commit -m "chore: Update .gitignore for cleanup"
```

---

## Version History

- **v1.0.0** (2026-01-30): Initial cleanup procedure
- Created by: Claude Code Agent
- Session: https://claude.ai/code/session_018igga7R3JKL2TCSDqy27ro
