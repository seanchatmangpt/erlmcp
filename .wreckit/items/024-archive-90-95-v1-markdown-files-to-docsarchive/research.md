# Research: Archive 90-95 v1 markdown files to docs/archive/

**Date**: 2026-01-29
**Item**: 024-archive-90-95-v1-markdown-files-to-docsarchive
**Section**: docs
**TCMS Compliance**: Lean Six Sigma Level Strictness

## Manufacturing Requirement
Root directory has 171 markdown files (should be ~5) - cluttered with v1 docs

**Motivation:** Repository root is cluttered with old v1 documentation. Should only have README, CHANGELOG, CONTRIBUTING, DEVELOPMENT, CLAUDE in root.

**Success criteria:**
- Archive directories created (v1-agent-deliverables, v1-phase-deliverables, v1-version-releases)
- 90-95 files moved to archive
- Root directory has ≤5 markdown files
- README, CHANGELOG, CONTRIBUTING, DEVELOPMENT, CLAUDE remain in root

**Technical constraints:**
- Create docs/archive/v1-agent-deliverables/
- Create docs/archive/v1-phase-deliverables/
- Create docs/archive/v1-version-releases/
- Delete duplicate files
- Remove transient docs (scratch pads, etc.)

**Signals:** priority: low, urgency: P3 - NICE TO HAVE

### Quality Gate Status
- **Gate Type**: Documentation Organization (Manual File Operations)
- **Current State**: 171 markdown files in root directory (measured 2026-01-28)
- **Target State**: 5 markdown files in root directory (README.md, CHANGELOG.md, CONTRIBUTING.md, DEVELOPMENT.md, CLAUDE.md)
- **Gap**: 166 files requiring archival (97% reduction)

## Summary

**Manufacturing Objective:** Archive 90-95 v1 markdown files from repository root to `docs/archive/` to establish a clean, professional project structure that follows industry best practices.

**Technical Approach:**
1. **Categorize and Sort**: Use existing audit documentation (`MARKDOWN_ORGANIZATION_AUDIT.md`, `MARKDOWN_CLEANUP_RECOMMENDATIONS.md`, `MARKDOWN_CLEANUP_INDEX.md`) to identify files by category
2. **Create Archive Structure**: Establish three archive subdirectories under `docs/archive/`:
   - `v1-agent-deliverables/` - Agent completion reports and deliverables
   - `v1-phase-deliverables/` - Quality gate receipts, validation evidence, phase reports
   - `v1-version-releases/` - Version-specific planning and implementation documentation
3. **Execute Moves**: Use `git mv` commands to preserve file history while relocating files
4. **Verify and Clean**: Confirm root directory has only 5 markdown files, update archive README

**TCPS Justification:**
- **Jidoka (Built-in Quality)**: Audit documentation already prepared with categorization, validation scripts exist to verify completion
- **Poka-yoke (Mistake-Proofing)**: Archive structure follows established pattern (already exists in `docs/archive/`), git mv prevents file loss
- **Kaizen (Continuous Improvement)**: This cleanup reduces cognitive load for developers, improves onboarding, establishes professional project hygiene
- **Heijunka (Production Leveling)**: Break into 3 small phases (agent deliverables → phase deliverables → version releases), no big-bang changes
- **Andon (Visible Signaling)**: Root directory becomes instantly cleaner, documentation organization visible to all contributors

## Current State Analysis

### Existing Implementation

**Audit Documentation Prepared (v1.5.0):**
- `/Users/sac/erlmcp/docs/MARKDOWN_ORGANIZATION_AUDIT.md` - Comprehensive 8-page audit with complete file categorization
- `/Users/sac/erlmcp/MARKDOWN_CLEANUP_RECOMMENDATIONS.md` - 5-page executive recommendations with implementation plan
- `/Users/sac/erlmcp/MARKDOWN_CLEANUP_INDEX.md` - Navigation index for audit documentation
- `/Users/sac/erlmcp/docs/MARKDOWN_CLEANUP_QUICK_REFERENCE.md` - 4-page execution guide with exact git commands

**Archive Structure Already Exists:**
- `/Users/sac/erlmcp/docs/archive/README.md` - Archive structure documentation (132 lines)
  - Documents purpose, organization, access patterns
  - Establishes retention policy
  - Provides navigation to active docs

**Root Directory Markdown Files:** 171 total
- **5 core files** (must keep): README.md, CHANGELOG.md, CONTRIBUTING.md, DEVELOPMENT.md, CLAUDE.md
- **10 agent deliverables** (archive): AGENT_*.md files
- **26 version releases** (archive): V*.md, RELEASE_*.md, GAP*.md, IMPLEMENTATION_*.md
- **34+ phase deliverables** (archive): *_RECEIPT.md, *_DELIVERY*.md, AUDIT_*.md, *_REPORT.md, BENCHMARK_*.md
- **20+ other documentation** (archive): Working drafts, planning docs, scratch pads

**Patterns:**
- File naming follows convention: PREFIX_TOPIC_SUFFIX.md (e.g., AGENT_11_COMPLETION_REPORT.md)
- Audit documentation uses "v1" terminology (v1 agent deliverables, v1 phase deliverables)
- Archive already uses simplified names (agent-deliverables, phase-deliverables, version-releases)

**Quality:**
- **Current Gate Status**: PASS (preparation phase)
  - Audit complete: ✅ All 171 files categorized with rationale
  - Recommendations documented: ✅ Implementation plan prepared
  - Execution guide ready: ✅ Git commands prepared
- **Target Gate Status**: PASS (execution phase)
  - Files moved: ⏳ Pending execution
  - Root directory cleaned: ⏳ Pending execution
  - Archive updated: ⏳ Pending execution

### Key Files

**Audit and Planning Documentation:**
- `/Users/sac/erlmcp/docs/MARKDOWN_ORGANIZATION_AUDIT.md:1-100` - Complete audit with file categorization, rationale for each category
- `/Users/sac/erlmcp/MARKDOWN_CLEANUP_RECOMMENDATIONS.md:65-100` - Recommendations 3A-3C with git commands for agent/phase/version deliverables
- `/Users/sac/erlmcp/docs/MARKDOWN_CLEANUP_QUICK_REFERENCE.md:40-150` - Execution guide with categorized file lists and git mv commands
- `/Users/sac/erlmcp/docs/archive/README.md:1-132` - Archive structure documentation, retention policy, navigation

**Root Directory Core Files (must remain):**
- `/Users/sac/erlmcp/README.md:1-460` - Project overview, quick start, installation (460 lines)
- `/Users/sac/erlmcp/CHANGELOG.md:1-500` - Version history, release notes (500 lines)
- `/Users/sac/erlmcp/CONTRIBUTING.md:1-465` - Contributor guidelines, code style, PR process (465 lines)
- `/Users/sac/erlmcp/DEVELOPMENT.md:1-503` - Dev environment setup, workflow, testing (503 lines)
- `/Users/sac/erlmcp/CLAUDE.md:1-194` - Project identity, commands, quality gates, agent index (194 lines)

**Example Files to Archive (Agent Deliverables):**
- `/Users/sac/erlmcp/AGENT_12_COMPLETION.md:1-30` - Agent 12 documentation consolidation delivery
- `/Users/sac/erlmcp/AGENT_11_COMPLETION_REPORT.md` - Metrics dashboard completion report
- `/Users/sac/erlmcp/AGENT_3_DELIVERY_INDEX.md` - Agent 3 delivery index

**Example Files to Archive (Phase Deliverables):**
- `/Users/sac/erlmcp/BENCHMARKING_SETUP_RECEIPT.md:1-30` - Performance benchmarking infrastructure setup
- `/Users/sac/erlmcp/BUILD_SYSTEM_RECEIPT.md:1-30` - Workspace build system implementation
- `/Users/sac/erlmcp/CI_CD_DELIVERY_RECEIPT.md` - CI/CD delivery receipt
- `/Users/sac/erlmcp/DOCKER_BUILD_RECEIPT.md` - Docker build setup receipt

**Example Files to Archive (Version Releases):**
- `/Users/sac/erlmcp/V0.6.0-IMPLEMENTATION-COMPLETE.md` - v0.6.0 implementation completion
- `/Users/sac/erlmcp/V0.6.0-PLANNING-COMPLETE.md` - v0.6.0 planning completion
- `/Users/sac/erlmcp/V0.6.0-SUBSYSTEM-INTEGRATION-REPORT.md` - v0.6.0 subsystem integration
- `/Users/sac/erlmcp/RELEASE_NOTES_v2.1.0.md` - v2.1.0 release notes
- `/Users/sac/erlmcp/RELEASE_STRATEGY.md` - Release strategy documentation

### OTP Patterns Observed

**Behavior**: N/A (This is a documentation organization task, not an Erlang/OTP implementation task)

**Supervision**: N/A (No supervision trees involved)

**Process Pattern**: Manual file operations using git

**Test Pattern**: N/A (No code tests required, manual verification needed)

## Technical Considerations

### Dependencies

**Internal Modules**: None (documentation-only task)

**External Libraries**: None (uses standard git commands)

**Tools Required**:
- `git` - Version control for file moves
- `bash` - Shell for executing move commands
- Standard Unix utilities: `ls`, `find`, `grep` for verification

**OTP Applications**: None (documentation-only task)

### TCPS Quality Gates to Pass

This is a documentation organization task, not a code compilation task. Quality validation focuses on:

- [ ] **Verification**: Root directory has exactly 5 markdown files after moves
- [ ] **Verification**: All moved files exist in archive directories
- [ ] **Verification**: No file history lost (git mv preserves history)
- [ ] **Verification**: Archive README updated with new file counts
- [ ] **Verification**: No broken links in remaining documentation
- [ ] **Manual**: Visual inspection of root directory
- [ ] **Manual**: Spot-check file contents in archive

### Patterns to Follow

**File Organization Pattern** (from existing archive):
- Reference: `/Users/sac/erlmcp/docs/archive/README.md:1-132`
- Pattern: Categorized by type (agent-deliverables, phase-deliverables, version-releases)
- Naming: Use descriptive category names, not version numbers
- Documentation: Each category has README explaining purpose and contents

**Git Move Pattern** (from quick reference):
- Reference: `/Users/sac/erlmcp/docs/MARKDOWN_CLEANUP_QUICK_REFERENCE.md:60-108`
- Pattern: Use `git mv` to preserve file history
- Group moves by category (agent → phase → version)
- Verify after each category move

**Verification Pattern** (from audit documentation):
- Reference: `/Users/sac/erlmcp/MARKDOWN_CLEANUP_RECOMMENDATIONS.md:95-100`
- Pattern: Count files before/after, verify git status, check for broken links

**Error Handling**:
- If `git mv` fails: Check if file already moved, check for filename conflicts
- If file count mismatch: Review moved files list, check for missed files
- If broken links found: Update links or create redirect pages in archive

**Type Specs**: N/A (documentation-only task)

## Root Cause Analysis (5 Whys)

**Problem**: Root directory has 171 markdown files (should be ~5)

1. **Why?** Historical accumulation of v1 deliverables, receipts, and reports during active development phases
2. **Why?** No established process for archiving completed phase deliverables during development cycles
3. **Why?** Focus on feature delivery overshadowed documentation hygiene (common in rapid development)
4. **Why?** Lack of automated quality gate for documentation organization (build gates don't check file count)
5. **ROOT CAUSE**: Missing Heijunka (production leveling) principle for documentation - no periodic cleanup intervals established

**Solution**: Implement this archival as planned cleanup, then establish Kaizen (continuous improvement) process:
- Add documentation organization check to release checklist
- Create git hook or CI check to warn if root file count exceeds threshold
- Schedule quarterly documentation hygiene reviews

## Risks and Mitigations

| Risk | Severity | Impact | Mitigation |
|------|----------|--------|------------|
| **Broken internal links** after moving files | P2 (Medium) | Documentation references become 404s | Run link checker before/after, update links in moved files, create README redirects |
| **Git history confusion** if moves fail | P2 (Medium) | Loss of file attribution context | Use `git mv` not `mv`, verify with `git log --follow`, commit moves in single batch |
| **Missed files** (not all 90-95 moved) | P3 (Low) | Root directory still cluttered | Count files before/after, compare against audit lists, manual verification |
| **Archive directory name mismatch** (v1- prefix vs no prefix) | P3 (Low) | Confusion about archive structure | Follow existing pattern (no v1 prefix), update archive README to clarify |
| **File name collisions** in archive (same filename from different categories) | P2 (Medium) | Git refuses to move, conflicts | Use subdirectories by category, check for duplicates before moves, rename if needed |
| **Loss of discoverability** (files become "buried" in archive) | P3 (Low) | Contributors can't find historical docs | Comprehensive archive README, search keywords, maintain index |
| **Accidental deletion of core files** | P1 (High) | Project becomes non-compliant with standards | Explicit whitelist of 5 core files, verify file names before any moves |

**Severity Definitions:**
- **P1 (High):** Major quality impact - MUST prevent
- **P2 (Medium):** Important but recoverable - SHOULD mitigate
- **P3 (Low):** Minor inconvenience - MAY address

## Recommended Manufacturing Approach

**TCPS Methodology:**

1. **Specification** (Requirements with acceptance criteria)
   - ✅ Complete: Audit documentation prepared (MARKDOWN_ORGANIZATION_AUDIT.md)
   - ✅ Complete: File categorization established (10 agent, 26 version, 34+ phase)
   - ✅ Complete: Target directories identified (docs/archive/)

2. **Pseudocode** (Algorithm design BEFORE coding)
   - ✅ Complete: Git commands prepared (MARKDOWN_CLEANUP_QUICK_REFERENCE.md)
   - ✅ Complete: Execution order defined (agent → phase → version)
   - ✅ Complete: Verification checklist established

3. **Architecture** (Integration points and dependencies)
   - ✅ Complete: Archive structure exists
   - ✅ Complete: No code dependencies (documentation-only)
   - ✅ Complete: Git history preservation approach

4. **Refinement** (Chicago School TDD - tests FIRST)
   - ⏳ Pending: Pre-execution verification (count root files, confirm archive structure)
   - ⏳ Pending: Execution of moves in phases
   - ⏳ Pending: Post-execution verification (count files, check links)

5. **Completion** (All quality gates passing)
   - ⏳ Pending: Root directory has ≤5 markdown files
   - ⏳ Pending: All moved files verified in archive
   - ⏳ Pending: Archive README updated

**Implementation Strategy:**

**Phase 1: Preparation (2 minutes)**
```bash
# Navigate to project root
cd /Users/sac/erlmcp

# Count current markdown files in root
find . -maxdepth 1 -name "*.md" -type f | wc -l
# Expected: 171

# Verify archive structure exists
ls -la docs/archive/
# Expected: agent-deliverables/, phase-deliverables/, version-releases/, README.md
```

**Phase 2: Execute Agent Deliverables Move (3 minutes)**
```bash
# Create subdirectory if needed
mkdir -p docs/archive/v1-agent-deliverables

# Move all AGENT_*.md files
git mv AGENT_*.md docs/archive/v1-agent-deliverables/

# Verify move
git status
find . -maxdepth 1 -name "AGENT_*.md" | wc -l
# Expected: 0
```

**Phase 3: Execute Phase Deliverables Move (5 minutes)**
```bash
# Create subdirectory if needed
mkdir -p docs/archive/v1-phase-deliverables

# Move phase deliverable files (from quick reference)
git mv *_RECEIPT.md docs/archive/v1-phase-deliverables/
git mv *_DELIVERY*.md docs/archive/v1-phase-deliverables/
git mv AUDIT_*.md docs/archive/v1-phase-deliverables/
git mv BENCHMARK_*.md docs/archive/v1-phase-deliverables/
# ... (see full list in MARKDOWN_CLEANUP_QUICK_REFERENCE.md)

# Verify move
git status
```

**Phase 4: Execute Version Releases Move (5 minutes)**
```bash
# Create subdirectory if needed
mkdir -p docs/archive/v1-version-releases

# Move version files
git mv V*.md docs/archive/v1-version-releases/
git mv RELEASE_*.md docs/archive/v1-version-releases/
git mv GAP*.md docs/archive/v1-version-releases/
git mv IMPLEMENTATION_*.md docs/archive/v1-version-releases/

# Verify move
git status
```

**Phase 5: Verification and Cleanup (5 minutes)**
```bash
# Count root markdown files (should be 5)
find . -maxdepth 1 -name "*.md" -type f | wc -l
# Expected: 5

# List remaining files (verify whitelist)
ls -1 *.md
# Expected: CHANGELOG.md CLAUDE.md CONTRIBUTING.md DEVELOPMENT.md README.md

# Commit changes
git add docs/archive/
git commit -m "docs: archive 90-95 v1 markdown files to docs/archive/

- Move agent deliverables to docs/archive/v1-agent-deliverables/
- Move phase deliverables to docs/archive/v1-phase-deliverables/
- Move version releases to docs/archive/v1-version-releases/
- Keep 5 core files in root: README, CHANGELOG, CONTRIBUTING, DEVELOPMENT, CLAUDE
- Reduce root directory clutter from 171 to 5 files (97% reduction)

Ref: #024-archive-90-95-v1-markdown-files-to-docsarchive"
```

**Quality Validation:**

**Automated:**
```bash
# Count root markdown files
find . -maxdepth 1 -name "*.md" -type f | wc -l

# Verify archive contents
find docs/archive/v1-* -name "*.md" | wc -l

# Check git status
git status
```

**Manual:**
- Visual inspection of root directory (`ls -1 *.md`)
- Spot-check 5-10 files in each archive subdirectory
- Verify archive README is accessible
- Check for broken links in remaining core files

**Metrics:**
- **Pre-cleanup**: 171 markdown files in root
- **Post-cleanup**: 5 markdown files in root
- **Archived**: 90-95 files moved
- **Reduction**: 97% reduction in root directory clutter
- **Time**: ~20 minutes total execution

## Open Questions

**NONE** - All research complete. Audit documentation prepared, execution plan defined, risks identified, verification approach established.

## Manufacturing Checklist

- [x] Root cause identified (not symptoms) → Missing Heijunka for documentation organization
- [x] Quality gates defined (specific thresholds) → Root ≤5 markdown files, 90-95 files archived
- [x] OTP patterns understood (behaviors, supervision) → N/A (documentation task)
- [x] Test strategy clear (Chicago School TDD) → Manual verification with counts and spot-checks
- [x] Risk assessment complete (severity P0-P3) → 7 risks identified with mitigations
- [x] No open questions (all research complete) → Audit docs prepared, execution plan ready

**TCPS Compliance Status:**
- **Jidoka**: ✅ Audit documentation validates categorization, verification gates confirm completion
- **Poka-yoke**: ✅ Git mv prevents file loss, explicit whitelist protects core files
- **Kaizen**: ✅ Metrics tracked (file counts), establishes baseline for future hygiene
- **Heijunka**: ✅ Broken into 5 small phases, no big-bang changes
- **Andon**: ✅ Visual improvement in root directory, archive structure visible to all

**Research Phase: COMPLETE ✅**
