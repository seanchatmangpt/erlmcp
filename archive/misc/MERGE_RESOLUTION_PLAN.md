# Merge Resolution Plan - erlmcp

**Date**: 2026-01-26
**Situation**: Merging local work (Wave 5 TCPS MCP Diataxis) with remote work (Phase 3 refactoring)
**Conflicts**: 15 files with merge conflicts

---

## Conflict Analysis

### Local Branch (HEAD - 3 commits)
1. `86a0cbd` - Wave 5: TCPS MCP Diataxis simulator (453 files, 197k+ insertions)
2. `55f3b28` - Workspace integration validation
3. `6b219e9` - TAIEA workspace initialization

### Remote Branch (origin/main - 3 commits)
1. `7077a6b` - Phase 3: Wrapper modules, transport API refactoring
2. `61e7458` - Phase 3 quick wins: stdio/registry tests
3. `0aee39c` - Snapshot checkpoint

### Nature of Conflicts

**Different Development Directions:**
- **Local**: Added massive TCPS educational platform (Wave 5) on top of v0.6.0 base
- **Remote**: Refactored core erlmcp architecture (Phase 3 cleanup/modularization)

**Key Differences:**
1. **Dependencies**: Local added many (gproc, gun, ranch, poolboy, bbmustache, cowboy), Remote simplified to core (jsx, jesse, opentelemetry)
2. **API Surface**: Remote simplified erlmcp.erl exports, Local kept fuller API
3. **Structure**: Local added tcps/ directories, Remote cleaned up core
4. **Configuration**: Different approaches to sys.config and rebar.config

---

## Conflicted Files (15 total)

### Category 1: Build Configuration (4 files)
1. `.gitignore` - Likely simple additions from both sides
2. `rebar.config` - **CRITICAL** - Completely different dependency lists
3. `rebar.lock` - **CRITICAL** - Lock file for different deps
4. `src/erlmcp.app.src` - Different module lists and dependencies

### Category 2: Documentation (1 file)
5. `CLAUDE.md` - Both sides added, likely complementary

### Category 3: Build System (1 file)
6. `Makefile` - Both sides added targets

### Category 4: Configuration (2 files)
7. `config/production.config` - Both sides added different configs
8. `config/sys.config.example` - Both sides added examples

### Category 5: Core Source Files (7 files)
9. `src/erlmcp.erl` - **CRITICAL** - Different export lists, Remote simplified
10. `src/erlmcp_registry.erl` - Core infrastructure changes
11. `src/erlmcp_transport.erl` - Transport behavior changes
12. `src/erlmcp_transport_http.erl` - HTTP transport implementation
13. `src/erlmcp_transport_stdio_new.erl` - Stdio transport changes
14. `src/erlmcp_transport_sup.erl` - Supervisor changes
15. `src/erlmcp_transport_tcp.erl` - TCP transport implementation

---

## Resolution Strategy

### Option A: Keep Both Lines of Work (RECOMMENDED)
**Goal**: Merge both development efforts without losing either side's work

**Approach**: Accept both sets of changes where compatible, choose strategically where incompatible

**Pros**:
- ✅ Preserves all Wave 5 TCPS work (197k+ lines)
- ✅ Incorporates Phase 3 improvements
- ✅ Most complete codebase

**Cons**:
- ⚠️ Most complex resolution
- ⚠️ May need significant testing
- ⚠️ Larger dependency footprint

### Option B: Favor Local (Wave 5 TCPS)
**Goal**: Keep your TCPS work as primary, ignore remote Phase 3 changes

**Approach**: Use `git checkout --ours` for all conflicts

**Pros**:
- ✅ Simple resolution
- ✅ Preserves all TCPS work
- ✅ Known working state

**Cons**:
- ❌ Loses Phase 3 improvements
- ❌ Misses upstream refactoring
- ❌ May diverge from upstream

### Option C: Favor Remote (Phase 3)
**Goal**: Accept upstream changes, re-apply TCPS work later

**Approach**: Use `git checkout --theirs` for conflicts, cherry-pick TCPS commits

**Pros**:
- ✅ Clean upstream base
- ✅ Modern refactored code
- ✅ Simpler core

**Cons**:
- ❌ Loses all Wave 5 work temporarily
- ❌ Need to re-apply 197k+ lines
- ❌ Significant rework required

---

## RECOMMENDED RESOLUTION: Option A (Detailed Steps)

### Phase 1: Critical Files (Core Functionality)

#### 1. `rebar.config` - Merge dependencies
**Strategy**: Combine both dependency lists

```erlang
{deps, [
    % Core (from remote - OpenTelemetry)
    {jsx, "3.1.0"},
    {jesse, "1.8.1"},
    {opentelemetry_api, "1.4.0"},
    {opentelemetry, "1.5.0"},
    {opentelemetry_exporter, "1.8.0"},

    % TCPS additions (from local)
    {gproc, "0.9.0"},
    {gun, "2.0.1"},
    {ranch, "2.1.0"},
    {poolboy, "1.5.2"},
    {bbmustache, "1.12.2"},
    {cowboy, "2.10.0"}
]}.
```

**Rationale**: TCPS needs its dependencies, but we also want OpenTelemetry from Phase 3

**Action**: Manual merge - combine both lists

---

#### 2. `src/erlmcp.erl` - Merge API exports
**Strategy**: Keep both simplified remote exports AND local extensions

**Remote has**: Simplified, cleaner API (fewer exports)
**Local has**: Fuller API with more functions

**Rationale**: Remote simplified to reduce surface area, but local needs extended API for TCPS

**Action**:
- Accept remote's simplified structure
- Add back local functions that TCPS depends on
- Mark TCPS-specific functions clearly

---

#### 3. `rebar.lock` - Regenerate
**Strategy**: After resolving rebar.config, regenerate lock file

**Action**:
```bash
rm rebar.lock
rebar3 update
```

---

### Phase 2: Configuration Files

#### 4. `.gitignore` - Union merge
**Strategy**: Keep all entries from both sides

**Action**: Combine both, remove duplicates

---

#### 5. `CLAUDE.md` - Append both
**Strategy**: Both sides added content, combine them

**Action**: Merge both sections, ensure no duplication

---

#### 6-8. Config files (`Makefile`, `config/*.config`)
**Strategy**: Keep local TCPS configs, add any remote improvements

**Action**: Manual inspection, keep TCPS configurations

---

### Phase 3: Core Source Files (7 files)

**Strategy for all core files**:
1. Accept remote's refactored structure as base
2. Add back local TCPS-specific modifications
3. Ensure TCPS functionality preserved

**Specific Actions**:

#### 9. `src/erlmcp_registry.erl`
- Base: Remote refactored version
- Add: Local TCPS registry extensions if any
- Test: Ensure TCPS modules can register

#### 10. `src/erlmcp_transport.erl`
- Base: Remote behavior improvements
- Add: Local transport extensions for TCPS
- Test: TCPS MCP server transport works

#### 11-15. Transport implementations (http, stdio, tcp, sup)
- Base: Remote refactored versions
- Add: Local TCPS-specific transport logic
- Test: All transports work with TCPS

---

### Phase 4: Validation & Testing

After resolution:

1. **Compile Check**
```bash
rebar3 compile
# Expect: Clean compilation or minor warnings only
```

2. **Core Tests**
```bash
rebar3 eunit --module=erlmcp_tests
# Verify: Core erlmcp still works
```

3. **TCPS Tests**
```bash
rebar3 eunit --module=tcps_quality_gates_tests
# Verify: TCPS functionality intact
```

4. **Integration Tests**
```bash
rebar3 ct
# Verify: No regressions
```

---

## Detailed File-by-File Resolution

### 1. `.gitignore`
**Conflict Type**: Both added lines
**Resolution**: Union merge
```bash
git checkout --ours .gitignore
git show origin/main:.gitignore >> .gitignore
sort -u .gitignore -o .gitignore
```

---

### 2. `rebar.config`
**Conflict Type**: Completely different
**Resolution**: Manual merge (see Phase 1, step 1)
**Critical Sections**:
- Dependencies: Combine both
- Source directories: Keep local's `["src", "src/tcps"]`
- Aliases: Keep local's TCPS aliases
- Relx: Keep local's release configuration

**Action**:
```bash
# Edit manually to combine:
# 1. Remote's OpenTelemetry deps + Local's TCPS deps
# 2. Keep local's src_dirs with tcps
# 3. Keep local's TCPS aliases
# 4. Keep local's release configuration
```

---

### 3. `rebar.lock`
**Resolution**: Regenerate after rebar.config resolved
```bash
rm rebar.lock
rebar3 get-deps
git add rebar.lock
```

---

### 4. `CLAUDE.md`
**Conflict Type**: Both added content
**Resolution**: Combine both - they're likely complementary
```bash
# Manual merge: keep both sections
# Phase 3 content + TCPS Wave 5 content
```

---

### 5. `Makefile`
**Conflict Type**: Both added targets
**Resolution**: Keep both sets of targets
**Action**: Manual merge, ensure no duplicate targets

---

### 6-8. Config files
**Resolution**: Keep local TCPS configurations
```bash
git checkout --ours config/production.config
git checkout --ours config/sys.config.example
```

---

### 9. `src/erlmcp.app.src`
**Conflict Type**: Different module lists and dependencies
**Resolution**: Combine both
**Action**:
- Dependencies: Merge lists from both sides
- Modules: Include all modules (remote refactored + local TCPS)
- Version: Use local version (0.6.0)

---

### 10. `src/erlmcp.erl`
**Conflict Type**: Different export lists
**Resolution**: Start with remote's cleaner structure, add local essentials
**Action**:
```erlang
% Start with remote's clean exports
% Add back ONLY the functions TCPS actually uses
% Document which are TCPS-specific
```

**Functions to verify TCPS needs**:
- Check what TCPS MCP server calls
- Check what TCPS modules import
- Keep only necessary functions

---

### 11-15. Transport files
**Resolution**: Accept remote refactoring, verify TCPS compatibility
**Action**:
```bash
# For each transport file:
git checkout --theirs src/erlmcp_transport*.erl
git checkout --theirs src/erlmcp_registry.erl

# Then test TCPS functionality
# If TCPS breaks, add back needed changes
```

---

## Success Criteria

After resolution, verify:

✅ **Compilation**
- [ ] `rebar3 compile` succeeds with 0 errors
- [ ] Minor warnings acceptable
- [ ] All modules load

✅ **Core erlmcp Tests**
- [ ] `rebar3 eunit` passes core tests
- [ ] Transport tests pass
- [ ] Registry tests pass

✅ **TCPS Tests**
- [ ] `rebar3 eunit --module=tcps_quality_gates_tests` passes (12/12)
- [ ] `rebar3 eunit --module=tcps_kanban_tests` passes (19/19)
- [ ] `rebar3 eunit --module=tcps_kaizen_tests` passes (43/43)
- [ ] Other TCPS tests pass

✅ **Integration**
- [ ] TCPS MCP server starts
- [ ] TCPS dashboard works
- [ ] Web UI accessible

✅ **No Data Loss**
- [ ] All 453 Wave 5 files present
- [ ] TCPS modules functional
- [ ] Documentation intact

---

## Timeline Estimate

- **Quick resolution** (favor one side): 5-10 minutes
- **Careful merge** (Option A): 30-60 minutes
- **Testing**: 30-45 minutes
- **Total**: 1-2 hours for complete resolution and validation

---

## Rollback Plan

If resolution fails:

```bash
# Abort merge
git merge --abort

# Option 1: Force push local (WARNING: loses remote changes)
git push --force origin main

# Option 2: Create new branch with local work
git checkout -b tcps-wave-5-preserved
git push origin tcps-wave-5-preserved

# Option 3: Cherry-pick approach
git checkout origin/main
git cherry-pick <commit-range>
```

---

## Recommendations

1. **RECOMMENDED**: Use Option A (careful merge)
   - Preserves both lines of work
   - Most complete codebase
   - Best long-term strategy

2. **Time permitting**: Take 1-2 hours to do it right
   - Worth the investment
   - Avoids future conflicts
   - Clean history

3. **Test thoroughly**: Both core and TCPS functionality
   - Run full test suite
   - Verify TCPS demos work
   - Check web UI

4. **Document changes**: Update CHANGELOG with merge resolution
   - Note what was combined
   - Note any breaking changes
   - Update version notes

---

## Next Steps

1. **Review this plan** - Confirm strategy
2. **Execute resolution** - Follow Phase 1-4 steps
3. **Test thoroughly** - Run all test suites
4. **Commit merge** - Clean commit message
5. **Push to remote** - Share combined work

---

**Status**: Plan ready for execution
**Requires**: Manual review and approval before proceeding
