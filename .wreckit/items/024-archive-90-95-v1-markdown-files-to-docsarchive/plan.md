# Archive 90-95 v1 markdown files to docs/archive/ Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Archive 90-95 v1 markdown files from the repository root directory to `docs/archive/` to establish a clean, professional project structure that follows industry best practices.

**Impact:** Reduces root directory clutter from 171 files to 5 files (97% reduction), improves discoverability, maintains complete audit trail.

### Quality Gate Requirements

This is a documentation organization task (manual file operations), not a code compilation task. Quality validation focuses on:

- **Verification**: Root directory has exactly 5 markdown files after moves
- **Verification**: All moved files exist in archive directories
- **Verification**: No file history lost (git mv preserves history)
- **Verification**: Archive README updated with new file counts
- **Verification**: No broken links in remaining documentation
- **Manual**: Visual inspection of root directory
- **Manual**: Spot-check file contents in archive

**Note:** No compilation, EUnit, coverage, Dialyzer, or Xref gates apply to this task (no code changes).

## Current State

### What Exists Now

**Root Directory Markdown Files: 171 total**
- **5 core files** (must keep): README.md, CHANGELOG.md, CONTRIBUTING.md, DEVELOPMENT.md, CLAUDE.md
- **10 agent deliverables** (archive): AGENT_*.md files
- **26 version releases** (archive): V*.md, RELEASE_*.md, GAP*.md, IMPLEMENTATION_*.md
- **34+ phase deliverables** (archive): *_RECEIPT.md, *_DELIVERY*.md, AUDIT_*.md, *_REPORT.md, BENCHMARK_*.md
- **20+ other documentation** (archive): Working drafts, planning docs, scratch pads

**Audit Documentation Prepared:**
- `/Users/sac/erlmcp/docs/MARKDOWN_ORGANIZATION_AUDIT.md` - Comprehensive 461-line audit with complete file categorization
- `/Users/sac/erlmcp/MARKDOWN_CLEANUP_RECOMMENDATIONS.md` - 324-line executive recommendations with implementation plan
- `/Users/sac/erlmcp/docs/MARKDOWN_CLEANUP_QUICK_REFERENCE.md` - 427-line execution guide with exact git commands

**Archive Structure Already Exists:**
- `/Users/sac/erlmcp/docs/archive/README.md` - Archive structure documentation (132 lines)

**Quality:**
- **Current Gate Status**: PASS (preparation phase)
  - Audit complete: ✅ All 171 files categorized with rationale
  - Recommendations documented: ✅ Implementation plan prepared
  - Execution guide ready: ✅ Git commands prepared
- **Target Gate Status**: PASS (execution phase)
  - Files moved: ⏳ Pending execution
  - Root directory cleaned: ⏳ Pending execution
  - Archive updated: ⏳ Pending execution

### What's Missing

**Gap:** 166 files requiring archival (97% reduction from 171 to 5 files)

**Root Cause:** Missing Heijunka (production leveling) principle for documentation - no periodic cleanup intervals established during active development phases

**Impact:**
- Repository root appears cluttered and unprofessional
- New contributors have difficulty finding core project files
- Historical deliverables intermixed with active documentation
- Violates industry best practices for open-source project structure

### Key Discoveries from Research

**Finding 1:** Audit documentation is comprehensive and ready for execution
- Reference: `/Users/sac/erlmcp/docs/MARKDOWN_ORGANIZATION_AUDIT.md:1-461`
- All 171 files have been categorized with specific rationale
- File lists are organized by target archive directory

**Finding 2:** Git commands are pre-prepared for exact execution
- Reference: `/Users/sac/erlmcp/docs/MARKDOWN_CLEANUP_QUICK_REFERENCE.md:60-108`
- Agent deliverables: `git mv AGENT_*.md docs/archive/agent-deliverables/`
- Version releases: `git mv V*.md RELEASE_*.md GAP*.md IMPLEMENTATION_*.md docs/archive/version-releases/`
- Phase deliverables: Multiple git mv commands for categorized files

**Finding 3:** Archive structure uses simplified naming (not "v1-" prefix)
- Reference: `/Users/sac/erlmcp/docs/archive/README.md:10-61`
- Existing directories: agent-deliverables/, version-releases/, phase-deliverables/
- Note: Task title mentions "v1-" but archive structure uses simplified names
- Decision: Follow existing archive structure (simplified names), not task title

## Desired End State

### Specification

**Archive Directory Structure:**
```
docs/archive/
├── README.md (existing, update with new counts)
├── agent-deliverables/ (existing, add 10 files)
│   ├── AGENT_11_COMPLETION_REPORT.md
│   ├── AGENT_11_INTEGRATION_DELIVERY.md
│   ├── AGENT_12_COMPLETION.md
│   ├── AGENT_15_CACHE_DELIVERABLES.md
│   ├── AGENT_18_COMPLETE.md
│   ├── AGENT_3_DELIVERY_INDEX.md
│   ├── AGENT_4_BACKPRESSURE_DELIVERABLES.md
│   ├── AGENT_5_MEMORY_OPTIMIZATION_DELIVERABLES.md
│   ├── AGENT_6_DELIVERABLES.md
│   └── AGENT_7_LIFECYCLE_RESULTS.md
├── version-releases/ (existing, add 26 files)
│   ├── V0.6.0-IMPLEMENTATION-COMPLETE.md
│   ├── V0.6.0-PLANNING-COMPLETE.md
│   ├── V0.6.0-SUBSYSTEM-INTEGRATION-REPORT.md
│   ├── V0.6.0-TCPS-PRODUCTION-READINESS-REPORT.md
│   ├── V0.6.0-TESTING-REPORT.md
│   ├── V1_5_0_DOCUMENTATION_RESET_COMPLETE.md
│   ├── V2.1_BASELINE_DELIVERABLE_SUMMARY.md
│   ├── V2.1_INTEGRATION_SUMMARY.md
│   ├── RELEASE_NOTES_v2.1.0.md
│   ├── RELEASE_STRATEGY.md
│   ├── RELEASE_v2.1.0_EVIDENCE.md
│   ├── GAP*.md (27 files)
│   └── IMPLEMENTATION_*.md (8 files)
└── phase-deliverables/ (existing, add 34+ files)
    ├── AUDIT_*.md (5 files)
    *_RECEIPT.md (10 files)
    *_DELIVERY*.md (12 files)
    *_REPORT.md (7 files)
    └── BENCHMARK_*.md (10+ files)
```

**Root Directory After Cleanup:**
```
/Users/sac/erlmcp/
├── README.md                  ✅ Core project file
├── CHANGELOG.md               ✅ Version history
├── CONTRIBUTING.md            ✅ Contributor guidelines
├── DEVELOPMENT.md             ✅ Dev environment setup
└── CLAUDE.md                  ✅ Quality gates config
```

### Verification

**Pre-Execution Verification:**
```bash
# Count current markdown files in root
cd /Users/sac/erlmcp
find . -maxdepth 1 -name "*.md" -type f | wc -l
# Expected: 171
```

**Post-Execution Verification:**
```bash
# Count root markdown files (should be 5)
find /Users/sac/erlmcp -maxdepth 1 -name "*.md" -type f | wc -l
# Expected: 5

# List remaining files (verify whitelist)
ls -1 /Users/sac/erlmcp/*.md
# Expected: CHANGELOG.md CLAUDE.md CONTRIBUTING.md DEVELOPMENT.md README.md

# Verify archive contents
find /Users/sac/erlmcp/docs/archive -name "*.md" | wc -l
# Expected: 90-95

# Check git status
git status
# Expected: All moves shown as "renamed: old_path -> new_path"
```

**Manual Verification:**
- Visual inspection of root directory (`ls -1 *.md`)
- Spot-check 5-10 files in each archive subdirectory
- Verify archive README is accessible
- Check for broken links in remaining core files

### Manufacturing Output

**Files Moved:** 90-95 markdown files
**Files Modified:**
- `docs/archive/README.md` - Update file counts, add v1 archive sections
**Receipts Generated:**
- Verification script output (file counts before/after)
- Git commit with all moves

## What We're NOT Doing

**OUT OF SCOPE ITEMS:**

1. **Deleting files** - All files are being archived, not deleted
   - Reason: Preserve complete audit trail for historical reference
   - Only exception: Exact duplicates (e.g., api_reference.md vs api-reference.md)

2. **Modifying file contents** - Only moving files, not editing
   - Reason: Preserve historical authenticity of deliverables
   - Exception: Update docs/archive/README.md with new counts

3. **Reorganizing docs/ directory** - Only organizing root directory
   - Reason: docs/ is already well-organized (612 files in appropriate subdirectories)
   - Scope is specifically root → archive, not docs/ reorganization

4. **Changing archive directory names** - Using existing structure
   - Reason: Archive already exists with established naming (agent-deliverables, not v1-agent-deliverables)
   - Task title mentions "v1-" prefix but existing structure uses simplified names
   - Decision: Follow existing pattern for consistency

5. **Creating new documentation** - Only moving existing files
   - Reason: This is an archival task, not content creation
   - Archive README already exists, only needs updates

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** ✅ - Requirements with acceptance criteria
   - Archive 90-95 files to three existing directories
   - Keep 5 core files in root
   - Verify file counts before/after

2. **Pseudocode** ✅ - Algorithm design BEFORE execution
   - git mv commands pre-prepared in audit documentation
   - Execution order: agent → phase → version

3. **Architecture** ✅ - Integration points and dependencies
   - Archive structure already exists
   - No code dependencies (documentation-only task)
   - Git history preservation via git mv

4. **Refinement** ⏳ - Quality validation (manual verification)
   - Pre-execution file count
   - Post-execution file count
   - Spot-check archive contents

5. **Completion** ⏳ - All quality gates passing
   - Root directory has ≤5 markdown files
   - All moved files verified in archive
   - Archive README updated

### Implementation Strategy

**High-Level Approach:**
Execute file moves in three small phases (Heijunka - production leveling), each phase independently verifiable. Use git mv to preserve file history. Follow existing archive structure naming convention.

**Why This Strategy:**
- **Small phases**: Reduces risk, allows verification after each phase
- **git mv**: Preserves file history and attribution
- **Existing structure**: Follows established patterns, maintains consistency
- **Incremental verification**: Catches errors early, prevents big-bang failures

**Execution Order:**
1. Phase 1: Agent deliverables (10 files) - Simple pattern match
2. Phase 2: Phase deliverables (34+ files) - Categorized patterns
3. Phase 3: Version releases (26 files) - Multiple file patterns

### Quality Integration

**Pre-commit Quality Gates:**
- Manual file count verification (before and after)
- Git status check (verify all moves tracked)
- Spot-check file contents in archive

**CI Gates:** N/A (documentation-only task, no CI changes)

**Receipt Generation:**
- File count metrics (before: 171, after: 5)
- Git commit with descriptive message
- Archive README updated with new counts

**Andon Signaling:**
- Root directory becomes instantly cleaner (visible to all)
- Archive structure visible in docs/archive/
- Git commit shows clear before/after

---

## Phases

### Phase 1: Move Agent Deliverables (10 files)

#### Overview

Move all AGENT_*.md files from root to docs/archive/agent-deliverables/. These are completion reports and deliverable inventories from past agent sessions.

#### Specification

**Source Files (10):**
- AGENT_11_COMPLETION_REPORT.md
- AGENT_11_INTEGRATION_DELIVERY.md
- AGENT_12_COMPLETION.md
- AGENT_15_CACHE_DELIVERABLES.md
- AGENT_18_COMPLETE.md
- AGENT_3_DELIVERY_INDEX.md
- AGENT_4_BACKPRESSURE_DELIVERABLES.md
- AGENT_5_MEMORY_OPTIMIZATION_DELIVERABLES.md
- AGENT_6_DELIVERABLES.md
- AGENT_7_LIFECYCLE_RESULTS.md

**Target Directory:** docs/archive/agent-deliverables/

#### Pseudocode

```bash
# Navigate to project root
cd /Users/sac/erlmcp

# Verify agent files exist
ls -1 AGENT_*.md | wc -l
# Expected: 10

# Create archive directory if needed
mkdir -p docs/archive/agent-deliverables

# Move all agent deliverables
git mv AGENT_*.md docs/archive/agent-deliverables/

# Verify move completed
git status | grep "renamed: AGENT"
# Expected: 10 lines showing "renamed: AGENT_X -> docs/archive/agent-deliverables/AGENT_X"
```

#### Architecture

**Integration:** None (documentation-only task)

**Dependencies:** None (can be executed independently)

**Process Pattern:** Manual file operations using git

#### Changes Required

**Action:** Execute git mv commands

**Verification:**
- Pre-count: 10 AGENT_*.md files in root
- Post-count: 0 AGENT_*.md files in root, 10 in archive
- Git status shows 10 renames

#### Success Criteria

**Automated Verification (MUST ALL PASS):**
- [ ] Pre-execution count: `find . -maxdepth 1 -name "AGENT_*.md" | wc -l` = 10
- [ ] Git mv executed without errors
- [ ] Post-execution count: `find . -maxdepth 1 -name "AGENT_*.md" | wc -l` = 0
- [ ] Archive count: `find docs/archive/agent-deliverables -name "*.md" | wc -l` ≥ 10
- [ ] Git status shows all moves as "renamed:"

**Manual Verification:**
- [ ] Visual inspection: root directory no longer shows AGENT_*.md files
- [ ] Spot-check: Open 2-3 files in archive, verify content intact
- [ ] Git log: Verify file history preserved (`git log --follow docs/archive/agent-deliverables/AGENT_3_DELIVERY_INDEX.md`)

**Note:** Complete ALL verification BEFORE marking phase done. If ANY check fails, STOP and investigate.

---

### Phase 2: Move Phase Deliverables (34+ files)

#### Overview

Move phase completion documents, quality gate receipts, and audit reports from root to docs/archive/phase-deliverables/. These are historical records from completed work phases.

#### Specification

**Source Files (34+):**
- *_RECEIPT.md (10 files): BUILD_SYSTEM_RECEIPT.md, BENCHMARKING_SETUP_RECEIPT.md, CI_CD_DELIVERY_RECEIPT.md, DOCKER_BUILD_RECEIPT.md, DOCUMENTATION_CONSOLIDATION_RECEIPT.md, RELEASE_MANAGEMENT_RECEIPT.md, TEST_INFRASTRUCTURE_RECEIPT.md, GCP_SETUP_RECEIPT.md, MULTI_ENVIRONMENT_SETUP_RECEIPT.md, VENDOR_SETUP_RECEIPT.md
- *_DELIVERY*.md (12 files): Various delivery reports
- AUDIT_*.md (5 files): Audit reports and findings
- *_REPORT.md (7 files): Analysis and completion reports
- BENCHMARK_*.md (10+ files): Performance benchmarking documentation

**Target Directory:** docs/archive/phase-deliverables/

#### Pseudocode

```bash
# Navigate to project root
cd /Users/sac/erlmcp

# Create archive directory if needed
mkdir -p docs/archive/phase-deliverables

# Move phase deliverables (categorized)
git mv *_RECEIPT.md docs/archive/phase-deliverables/
git mv *_DELIVERY*.md docs/archive/phase-deliverables/
git mv AUDIT_*.md docs/archive/phase-deliverables/
git mv *_REPORT.md docs/archive/phase-deliverables/
git mv BENCHMARK_*.md docs/archive/phase-deliverables/
git mv BACKPRESSURE_*.md docs/archive/phase-deliverables/
git mv BASELINE_*.md docs/archive/phase-deliverables/
git mv CHAOS_*.md docs/archive/phase-deliverables/
git mv CLEANUP_*.md docs/archive/phase-deliverables/
git mv COLIMA_*.md docs/archive/phase-deliverables/
git mv DASHBOARD_*.md docs/archive/phase-deliverables/
git mv DELIVERABLES_*.md docs/archive/phase-deliverables/
git mv DOCS_LINTER_*.md docs/archive/phase-deliverables/
git mv INTEGRATION_*.md docs/archive/phase-deliverables/
git mv PROFILING_*.md docs/archive/phase-deliverables/
git mv TEST_*.md docs/archive/phase-deliverables/

# Verify move completed
git status | grep "renamed:" | wc -l
# Expected: 34+ lines
```

#### Architecture

**Integration:** None (documentation-only task)

**Dependencies:** Phase 1 should be complete first (establishes pattern)

**Process Pattern:** Manual file operations using git

#### Changes Required

**Action:** Execute git mv commands for categorized files

**Verification:**
- Pre-count: 34+ phase deliverable files in root
- Post-count: 0 phase deliverable files in root (except core 5)
- Git status shows 34+ renames

#### Success Criteria

**Automated Verification (MUST ALL PASS):**
- [ ] Pre-execution count: `find . -maxdepth 1 -name "*_RECEIPT.md" -o -name "*_DELIVERY*.md" -o -name "AUDIT_*.md" | wc -l` ≥ 34
- [ ] Git mv executed without errors
- [ ] Post-execution count: Phase deliverable files removed from root
- [ ] Archive count: `find docs/archive/phase-deliverables -name "*.md" | wc -l` ≥ 34
- [ ] Git status shows all moves as "renamed:"

**Manual Verification:**
- [ ] Visual inspection: root directory shows only core files remaining
- [ ] Spot-check: Open 5 files in archive, verify content intact
- [ ] Git log: Verify file history preserved for sample files

**Note:** Complete ALL verification BEFORE marking phase done. If ANY check fails, STOP and investigate.

---

### Phase 3: Move Version Releases (26 files)

#### Overview

Move version-specific planning, implementation, and release documentation from root to docs/archive/version-releases/. These are historical records of version development iterations.

#### Specification

**Source Files (26):**
- V*.md (6 files): V0.6.0-*.md, V1_5_0_*.md, V2.1_*.md
- RELEASE_*.md (3 files): RELEASE_NOTES_v2.1.0.md, RELEASE_STRATEGY.md, RELEASE_v2.1.0_EVIDENCE.md
- GAP*.md (27 files): GAP3_*.md, GAP4_*.md, GAP10_*.md, GAP27_*.md, etc.
- IMPLEMENTATION_*.md (8 files): Various gap implementation summaries

**Target Directory:** docs/archive/version-releases/

#### Pseudocode

```bash
# Navigate to project root
cd /Users/sac/erlmcp

# Create archive directory if needed
mkdir -p docs/archive/version-releases

# Move version release documentation
git mv V*.md docs/archive/version-releases/
git mv RELEASE_*.md docs/archive/version-releases/
git mv GAP*.md docs/archive/version-releases/
git mv IMPLEMENTATION_*.md docs/archive/version-releases/

# Verify move completed
git status | grep "renamed:" | wc -l
# Expected: 26+ lines
```

#### Architecture

**Integration:** None (documentation-only task)

**Dependencies:** Phases 1-2 should be complete (establishes pattern)

**Process Pattern:** Manual file operations using git

#### Changes Required

**Action:** Execute git mv commands for version files

**Verification:**
- Pre-count: 26 version files in root
- Post-count: 0 version files in root (except core 5)
- Git status shows 26+ renames

#### Success Criteria

**Automated Verification (MUST ALL PASS):**
- [ ] Pre-execution count: `find . -maxdepth 1 -name "V*.md" -o -name "RELEASE_*.md" -o -name "GAP*.md" -o -name "IMPLEMENTATION_*.md" | wc -l` ≥ 26
- [ ] Git mv executed without errors
- [ ] Post-execution count: Version files removed from root
- [ ] Archive count: `find docs/archive/version-releases -name "*.md" | wc -l` ≥ 26
- [ ] Git status shows all moves as "renamed:"

**Manual Verification:**
- [ ] Visual inspection: root directory shows only 5 core files
- [ ] Spot-check: Open 3-5 files in archive, verify content intact
- [ ] Git log: Verify file history preserved for sample files

**Note:** Complete ALL verification BEFORE marking phase done. If ANY check fails, STOP and investigate.

---

### Phase 4: Final Verification and Cleanup

#### Overview

Verify all moves completed successfully, update archive README with new file counts, and create final git commit.

#### Specification

**Verification Tasks:**
1. Count root markdown files (should be exactly 5)
2. List remaining files (verify whitelist matches)
3. Count archive files (should be 90-95)
4. Check git status (all moves tracked)
5. Update docs/archive/README.md with new counts
6. Create git commit

#### Pseudocode

```bash
# Navigate to project root
cd /Users/sac/erlmcp

# Step 1: Count root markdown files
find . -maxdepth 1 -name "*.md" -type f | wc -l
# Expected: 5

# Step 2: List remaining files (verify whitelist)
ls -1 *.md
# Expected: CHANGELOG.md CLAUDE.md CONTRIBUTING.md DEVELOPMENT.md README.md

# Step 3: Count archive files
find docs/archive -name "*.md" | wc -l
# Expected: 90-95

# Step 4: Check git status
git status
# Expected: All moves shown as "renamed: old_path -> new_path"

# Step 5: Update archive README with new counts
# Edit docs/archive/README.md, update file counts in each section

# Step 6: Stage all changes
git add docs/archive/

# Step 7: Create git commit
git commit -m "docs: archive 90-95 v1 markdown files to docs/archive/

- Move 10 agent deliverables to docs/archive/agent-deliverables/
- Move 34+ phase deliverables to docs/archive/phase-deliverables/
- Move 26 version releases to docs/archive/version-releases/
- Keep 5 core files in root: README, CHANGELOG, CONTRIBUTING, DEVELOPMENT, CLAUDE
- Reduce root directory from 171 to 5 markdown files (97% reduction)
- Preserve complete audit trail in archive structure
- Update archive README with new file counts

Ref: #024-archive-90-95-v1-markdown-files-to-docsarchive"
```

#### Architecture

**Integration:** None (documentation-only task)

**Dependencies:** Phases 1-3 must be complete

**Process Pattern:** Manual verification and git commit

#### Changes Required

**File:** docs/archive/README.md

**Current:** File counts show old numbers (archive just created, few files)

**Changes:** Update file counts in each section:
- agent-deliverables/: ~10 files
- version-releases/: ~26 files
- phase-deliverables/: ~34+ files

```markdown
### agent-deliverables/
Historical reports from individual agent sessions during development.

**Contents:**
- Agent completion reports
- Deliverable inventories
- Integration delivery documentation

**Use Case:** Understanding what was delivered in each agent phase

**Files:** ~10 documents
```

#### Success Criteria

**Automated Verification (MUST ALL PASS):**
- [ ] Root count: `find . -maxdepth 1 -name "*.md" -type f | wc -l` = 5
- [ ] Root list: `ls -1 *.md` shows exactly: CHANGELOG.md CLAUDE.md CONTRIBUTING.md DEVELOPMENT.md README.md
- [ ] Archive count: `find docs/archive -name "*.md" | wc -l` ≥ 90
- [ ] Git status: Clean (no uncommitted changes except README update)
- [ ] Git commit: Created with descriptive message

**Manual Verification:**
- [ ] Visual inspection: Root directory is clean (only 5 .md files visible)
- [ ] Archive README: File counts updated correctly
- [ ] Spot-check: 5 files in each archive subdirectory, content intact
- [ ] Git history: Verify `git log --follow` works for archived files

**Note:** Complete ALL verification BEFORE marking phase done. If ANY check fails, STOP and investigate.

---

## Testing Strategy

### Manual Verification (MANDATORY)

This is a documentation organization task. No automated tests (EUnit, Common Test) apply. Verification is entirely manual:

**Pre-Execution Verification:**
1. Count root markdown files
2. Verify archive directories exist
3. Verify audit documentation is accessible

**Post-Execution Verification:**
1. Count root markdown files (must be 5)
2. List root files (must match whitelist)
3. Count archive files (must be 90-95)
4. Spot-check file contents in archive
5. Verify git history preserved
6. Check for broken links in core files

**Verification Commands:**
```bash
# Count root markdown files
find /Users/sac/erlmcp -maxdepth 1 -name "*.md" -type f | wc -l

# List root files
ls -1 /Users/sac/erlmcp/*.md

# Count archive files
find /Users/sac/erlmcp/docs/archive -name "*.md" | wc -l

# Verify git history
git log --follow docs/archive/agent-deliverables/AGENT_3_DELIVERY_INDEX.md

# Check for broken links
grep -r "AGENT_.*\.md\|V[0-9].*\.md\|GAP[0-9]" \
  README.md CONTRIBUTING.md DEVELOPMENT.md CHANGELOG.md CLAUDE.md
```

### Quality Gates

Every phase MUST pass:

1. **Pre-execution count** - Verify files exist before moving
2. **Git mv execution** - All moves complete without errors
3. **Post-execution count** - Verify files removed from root
4. **Archive verification** - Verify files exist in archive
5. **Git status check** - Verify all moves tracked
6. **Manual spot-check** - Verify file contents intact

**Note:** No compilation, EUnit, coverage, Dialyzer, or Xref gates apply (no code changes).

## Manufacturing Checklist

### Before Implementation
- [x] Research verified (audit documentation reviewed)
- [x] Scope confirmed (IN: root → archive, OUT: docs/ reorganization)
- [x] No open questions (all decisions made)
- [x] Phases broken down (4 phases, each ≤30 minutes)
- [x] Acceptance criteria defined (file counts, git status, spot-checks)

### During Implementation
- [x] Pre-execution verification (file counts)
- [ ] Phase 1: Agent deliverables moved (10 files)
- [ ] Phase 2: Phase deliverables moved (34+ files)
- [ ] Phase 3: Version releases moved (26 files)
- [ ] Phase 4: Final verification and cleanup
- [ ] Git commit created with descriptive message

### After Implementation
- [ ] Root directory has exactly 5 markdown files
- [ ] Root files match whitelist (README, CHANGELOG, CONTRIBUTING, DEVELOPMENT, CLAUDE)
- [ ] Archive contains 90-95 markdown files
- [ ] Archive README updated with new counts
- [ ] Git status shows clean commit
- [ ] Git history preserved (verified with git log --follow)
- [ ] No broken links in remaining core files
- [ ] Manual spot-checks pass (5-10 files in archive)

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| **Broken internal links** after moving files | P2 (Medium) | Medium | Search for and update links in remaining files, create redirect pages if needed |
| **Git history confusion** if moves fail | P2 (Medium) | Low | Use `git mv` not `mv`, verify with `git log --follow`, commit moves in single batch |
| **Missed files** (not all 90-95 moved) | P3 (Low) | Low | Count files before/after, compare against audit lists, manual verification |
| **Archive directory name mismatch** (v1- prefix vs no prefix) | P3 (Low) | Low | Follow existing pattern (no v1 prefix), update archive README to clarify |
| **File name collisions** in archive (same filename from different categories) | P2 (Medium) | Very Low | Use subdirectories by category, check for duplicates before moves, rename if needed |
| **Loss of discoverability** (files become "buried" in archive) | P3 (Low) | Low | Comprehensive archive README, search keywords, maintain index |
| **Accidental deletion of core files** | P1 (High) | Very Low | Explicit whitelist of 5 core files, verify file names before any moves |

**Severity Definitions:**
- **P1 (High):** Major quality impact - MUST prevent
- **P2 (Medium):** Important but recoverable - SHOULD mitigate
- **P3 (Low):** Minor inconvenience - MAY address

### Rollback Plan

If something goes wrong:

**Git revert:**
```bash
# Revert the archival commit
git revert HEAD

# Or reset to previous commit (if commit not pushed)
git reset --hard HEAD~1
```

**File Recovery:**
- All files remain in git history via `git log --follow`
- Use `git checkout HEAD~1 -- <file>` to recover specific files
- Complete rollback restores root directory to 171 files

**Service Impact:** None (documentation-only task, no running services affected)

## References

- Research: `/Users/sac/erlmcp/.wreckit/items/024-archive-90-95-v1-markdown-files-to-docsarchive/research.md`
- Audit: `/Users/sac/erlmcp/docs/MARKDOWN_ORGANIZATION_AUDIT.md`
- Recommendations: `/Users/sac/erlmcp/MARKDOWN_CLEANUP_RECOMMENDATIONS.md`
- Quick Reference: `/Users/sac/erlmcp/docs/MARKDOWN_CLEANUP_QUICK_REFERENCE.md`
- Archive Guide: `/Users/sac/erlmcp/docs/archive/README.md`
- CLAUDE.md: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- TCPS: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
