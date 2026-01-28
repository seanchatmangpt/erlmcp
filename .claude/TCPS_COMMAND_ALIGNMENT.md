# TCPS Command Alignment - Japanese Task Terminology

**Date**: 2026-01-27
**Purpose**: Align erlmcp command system with Toyota Code Production System (TCPS) Japanese terminology
**Research Summary**: TCPS applies Toyota Production System principles to software manufacturing

---

## Executive Summary

erlmcp has a comprehensive Toyota Code Production System (TCPS) that treats software as physical products moving through a manufacturing line. The command system should align with TCPS's 9 pillars using authentic Japanese terminology for clarity and consistency.

### The 9 Pillars of TCPS (Japanese Terms)

1. **Just-In-Time (JIT)** - Pull-based work intake
2. **Jidoka** (自働化) - Built-in quality with stop-the-line authority
3. **Standard Work** - Documented processes for every stage
4. **Kanban** (看板) - Work-in-progress limits
5. **Heijunka** (平準化) - Production leveling
6. **5 Whys** - Root cause analysis
7. **Andon** (行灯) - Visible stop-the-line signaling
8. **Poka-yoke** (ポカヨケ) - Error-proofing mechanisms
9. **Kaizen** (改善) - Continuous improvement

---

## Current Command System vs TCPS Alignment

### Problem: Generic Command Names Lose Manufacturing Clarity

**Current commands** use generic software terminology:
- `/sparc spec` - Specification phase
- `/sparc architect` - Architecture design
- `/sparc test` - Testing
- `/swarm init` - Initialize swarm
- `/perf analyze` - Performance analysis

**Missing**: Direct connection to TCPS manufacturing metaphor

### Solution: Map Commands to Japanese TCPS Tasks

Commands should reflect the **actual manufacturing stages** in TCPS.

---

## Proposed TCPS-Aligned Command Structure

### Category 1: Manufacturing Line Operations (8 commands)

These replace the SPARC methodology commands with actual TCPS production stages.

#### `/tcps pull` (replaces `/sparc spec`)
**Japanese**: Just-In-Time (JIT)
**Manufacturing Stage**: Stage 1 - Pull demand signals
**Purpose**: Create work orders from demand signals (marketplace installs, GitHub issues, security advisories)
**Invokes**: `plan-designer` (creates work orders)
**Uses Rules**: `rules-spec-pseudocode/` (work order validation)
**Example**: `/tcps pull feature-http2` → creates work order from marketplace demand

#### `/tcps heijunka` (replaces `/sparc architect`)
**Japanese**: 平準化 (Production Leveling)
**Manufacturing Stage**: Stage 2 - Plan production schedule
**Purpose**: Level load production, balance reliability/security/cost work
**Invokes**: `erlang-architect` (designs heijunka schedule)
**Uses Rules**: `rules-architect/` (scheduling constraints)
**Example**: `/tcps heijunka balance` → creates balanced schedule

#### `/tcps kanban` (new - WIP management)
**Japanese**: 看板 (Work Signal)
**Manufacturing Stage**: Stages 3-5 - Limit work-in-progress
**Purpose**: Manage WIP limits, prevent bottlenecks
**Invokes**: `sparc-orchestrator` (coordinates WIP)
**Uses Rules**: `rules-kanban/` (WIP limits)
**Example**: `/tcps kanban status` → shows WIP limits (3 active builds)

#### `/tcps build` (replaces `/sparc code`)
**Japanese**: Standard Work
**Manufacturing Stage**: Stages 6-7 - Build & Compile
**Purpose**: Execute standard build process (compile, test)
**Invokes**: `erlang-otp-developer` (implements standard work)
**Uses Rules**: `rules-code/` + `rules-tdd/` (build standards)
**Example**: `/tcps build erlmcp_server` → follows standard work to build

#### `/tcps jidoka` (replaces `/sparc test`)
**Japanese**: 自働化 (Built-in Quality)
**Manufacturing Stage**: Stage 7 - Built-in quality checks
**Purpose**: Run quality checks with stop-the-line authority
**Invokes**: `erlang-test-engineer` (executes quality gates)
**Uses Rules**: `rules-tdd/` (quality standards)
**Example**: `/tcps jidoka` → runs all quality checks, stops on failure

#### `/tcps andon` (replaces `/sparc review`)
**Japanese**: 行灯 (Stop-the-Line Signal)
**Manufacturing Stage**: Cross-stage - Visible problem signaling
**Purpose**: Trigger stop-the-line events, quarantine SKUs
**Invokes**: `code-reviewer` (analyzes problems)
**Uses Rules**: `rules-security-review/` (andon triggers)
**Example**: `/tcps andon trigger test-failure` → stops line, quarantines SKU

#### `/tcps kaizen` (replaces `/sparc docs`)
**Japanese**: 改善 (Continuous Improvement)
**Manufacturing Stage**: Cross-stage - Improvement proposals
**Purpose**: Document improvements, update standard work
**Invokes**: `erlang-otp-developer` (documents kaizen)
**Uses Rules**: `rules-docs-writer/` (kaizen documentation)
**Example**: `/tcps kaizen propose KZ-2026-003` → creates kaizen proposal

#### `/tcps receipt` (replaces `/sparc deploy`)
**Japanese**: Receipt Chain (レシート)
**Manufacturing Stage**: Stage 8 - Release with evidence
**Purpose**: Generate immutable receipts, verify hash chains
**Invokes**: `erlang-github-ops` (creates receipts)
**Uses Rules**: `rules-devops/` (deployment standards)
**Example**: `/tcps receipt create team v1.4.0` → creates receipt

---

### Category 2: Quality Gates (4 commands)

These replace performance/analysis commands with TCPS quality terminology.

#### `/poka-yoke validate` (replaces `/perf analyze`)
**Japanese**: ポカヨケ (Error-Proofing)
**Purpose**: Validate plans, schemas, envelopes (prevent errors)
**Invokes**: `erlang-performance` (runs validation gates)
**Example**: `/poka-yoke validate team-plan` → validates plan spec

#### `/poka-yoke monitor` (replaces `/perf monitor`)
**Japanese**: ポカヨケ (Error-Proofing)
**Purpose**: Monitor SLA compliance, detect violations
**Invokes**: `erlang-performance` (monitors metrics)
**Example**: `/poka-yoke monitor sla team` → monitors Team tier SLA

#### `/poka-yoke test` (replaces `/perf optimize`)
**Japanese**: ポカヨケ (Error-Proofing)
**Purpose**: Run conformance tests, verify envelopes
**Invokes**: `erlang-performance` (runs benchmarks)
**Example**: `/poka-yoke test enterprise` → tests enterprise envelope

#### `/5-whys analyze` (replaces `/perf train`)
**Japanese**: なぜなぜ分析 (Root Cause Analysis)
**Purpose**: Analyze root causes, train patterns
**Invokes**: (neural system)
**Example**: `/5-whys analyze andon-20260127-001` → runs 5 whys analysis

---

### Category 3: Work Order Management (6 commands)

These replace swarm commands with work order terminology.

#### `/work-order create` (replaces `/swarm init`)
**Japanese**: 作業指示 (Work Order)
**Purpose**: Create work order from demand signal
**Invokes**: `sparc-orchestrator`
**Example**: `/work-order create sku=erlmcp-http2 priority=high`

#### `/work-order assign` (replaces `/swarm spawn`)
**Japanese**: 作業指示 (Work Order)
**Purpose**: Assign work order to agent
**Invokes**: (dynamic agent spawning)
**Example**: `/work-order assign wo-001 erlang-otp-developer`

#### `/work-order status` (replaces `/swarm status`)
**Japanese**: 作業指示 (Work Order)
**Purpose**: Show work order status, WIP limits
**Invokes**: (monitoring)
**Example**: `/work-order status` → shows active work orders

#### `/work-order schedule` (replaces `/swarm orchestrate`)
**Japanese**: 作業指示 (Work Order)
**Purpose**: Schedule work orders via heijunka
**Invokes**: `sparc-orchestrator`
**Example**: `/work-order schedule next-batch`

#### `/work-order receipt` (replaces `/swarm memory`)
**Japanese**: レシート (Receipt)
**Purpose**: Generate receipt for work order
**Invokes**: (memory system)
**Example**: `/work-order receipt wo-001` → creates receipt

#### `/work-order verify` (replaces `/swarm consensus`)
**Japanese**: 検証 (Verification)
**Purpose**: Verify work order meets acceptance criteria
**Invokes**: `sparc-orchestrator`
**Example**: `/work-order verify wo-001` → runs verification

---

### Category 4: SKU Management (3 commands)

These replace GitHub commands with SKU (product) terminology.

#### `/sku list` (replaces `/github repo`)
**Japanese**: SKU (Stock Keeping Unit)
**Purpose**: List manufactured SKUs (releases)
**Invokes**: `erlang-github-ops`
**Example**: `/sku list` → shows all released SKUs

#### `/sku build` (replaces `/github pr`)
**Japanese**: SKU (Stock Keeping Unit)
**Purpose**: Build and certify SKU
**Invokes**: `erlang-github-ops`
**Example**: `/sku build erlmcp-0.6.0` → builds SKU

#### `/sku certify` (replaces `/github issue`)
**Japanese**: 認証 (Certification)
**Purpose**: Certify SKU with evidence bundle
**Invokes**: `erlang-github-ops`
**Example**: `/sku certify team v1.4.0` → runs certification

---

### Category 5: Standard Work (5 commands)

These replace utility commands with standard work terminology.

#### `/standard-work show` (replaces `/hooks list`)
**Japanese**: 標準作業 (Standard Work)
**Purpose**: Show standard work documentation
**Invokes**: (documentation system)
**Example**: `/standard-work show build` → shows Stage 6 standard work

#### `/standard-work verify` (replaces `/agent list`)
**Japanese**: 標準作業 (Standard Work)
**Purpose**: Verify all stages have standard work
**Invokes**: (registry)
**Example**: `/standard-work verify` → checks all stages documented

#### `/receipt search` (replaces `/memory search`)
**Japanese**: レシート (Receipt)
**Purpose**: Search receipt chain
**Invokes**: (memory system)
**Example**: `/receipt search team v1.4.0` → searches receipts

#### `/template render` (replaces `/workflow execute`)
**Japanese**: 型 (Template/Die)
**Purpose**: Render templates with SPARQL data
**Invokes**: (workflow engine)
**Example**: `/template render transport-tcp` → renders template

#### `/demand signal` (replaces `/automate`)
**Japanese**: 需要信号 (Demand Signal)
**Purpose**: Detect demand signals (marketplace installs, refunds)
**Invokes**: (automation system)
**Example**: `/demand signal detect` → detects marketplace spikes

---

### Category 6: Top-Level (4 commands - unchanged)

Keep existing top-level commands as they provide system-wide operations.

```
/claude-flow-help       - System documentation
/claude-flow-memory     - Memory management
/claude-flow-swarm      - Swarm coordination
/sparc                  - SPARC orchestrator (delegates to TCPS commands)
```

---

## Complete TCPS Command Structure (30 total)

```
.claude/commands/
├── Top-Level (4)
│   ├── claude-flow-help.md
│   ├── claude-flow-memory.md
│   ├── claude-flow-swarm.md
│   └── sparc.md (delegates to TCPS)
├── tcps/ (8) - Manufacturing Line
│   ├── pull.md (JIT work orders)
│   ├── heijunka.md (production leveling)
│   ├── kanban.md (WIP management)
│   ├── build.md (standard work)
│   ├── jidoka.md (built-in quality)
│   ├── andon.md (stop-the-line)
│   ├── kaizen.md (continuous improvement)
│   └── receipt.md (evidence chain)
├── poka-yoke/ (4) - Quality Gates
│   ├── validate.md (error-proofing validation)
│   ├── monitor.md (SLA monitoring)
│   ├── test.md (conformance testing)
│   └── 5-whys.md (root cause analysis)
├── work-order/ (6) - Work Management
│   ├── create.md (create work order)
│   ├── assign.md (assign to agent)
│   ├── status.md (show status)
│   ├── schedule.md (heijunka scheduling)
│   ├── receipt.md (generate receipt)
│   └── verify.md (verify completion)
├── sku/ (3) - Product Management
│   ├── list.md (list SKUs)
│   ├── build.md (build SKU)
│   └── certify.md (certify with evidence)
└── standard-work/ (5) - Documentation
    ├── show.md (show standard work)
    ├── verify.md (verify completeness)
    ├── receipt-search.md (search receipts)
    ├── template-render.md (render templates)
    └── demand-signal.md (detect demand)
```

---

## Migration Map: Old Commands → TCPS Commands

### SPARC → TCPS Manufacturing Line

| Old Command | New TCPS Command | Japanese Term | Manufacturing Stage |
|-------------|------------------|---------------|---------------------|
| `/sparc spec` | `/tcps pull` | JIT | Stage 1: Pull work orders |
| `/sparc architect` | `/tcps heijunka` | 平準化 | Stage 2: Level production |
| `/sparc code` | `/tcps build` | Standard Work | Stages 6-7: Build |
| `/sparc test` | `/tcps jidoka` | 自働化 | Stage 7: Quality checks |
| `/sparc review` | `/tcps andon` | 行灯 | Cross-stage: Stop line |
| `/sparc docs` | `/tcps kaizen` | 改善 | Cross-stage: Improve |
| `/sparc deploy` | `/tcps receipt` | レシート | Stage 8: Release |
| `/sparc integrate` | `/tcps kanban` | 看板 | Stages 3-5: WIP management |

### Performance → Poka-yoke Quality Gates

| Old Command | New TCPS Command | Japanese Term | Purpose |
|-------------|------------------|---------------|---------|
| `/perf analyze` | `/poka-yoke validate` | ポカヨケ | Error-proof validation |
| `/perf monitor` | `/poka-yoke monitor` | ポカヨケ | SLA monitoring |
| `/perf optimize` | `/poka-yoke test` | ポカヨケ | Conformance testing |
| `/perf train` | `/5-whys analyze` | なぜなぜ分析 | Root cause analysis |

### Swarm → Work Order Management

| Old Command | New TCPS Command | Japanese Term | Purpose |
|-------------|------------------|---------------|---------|
| `/swarm init` | `/work-order create` | 作業指示 | Create work order |
| `/swarm spawn` | `/work-order assign` | 作業指示 | Assign work |
| `/swarm status` | `/work-order status` | 作業指示 | Show status |
| `/swarm orchestrate` | `/work-order schedule` | 作業指示 | Schedule work |
| `/swarm memory` | `/work-order receipt` | レシート | Generate receipt |
| `/swarm consensus` | `/work-order verify` | 検証 | Verify completion |

### GitHub → SKU Management

| Old Command | New TCPS Command | Japanese Term | Purpose |
|-------------|------------------|---------------|---------|
| `/github repo` | `/sku list` | SKU | List products |
| `/github pr` | `/sku build` | SKU | Build product |
| `/github issue` | `/sku certify` | 認証 | Certify product |

### Utility → Standard Work

| Old Command | New TCPS Command | Japanese Term | Purpose |
|-------------|------------------|---------------|---------|
| `/hooks list` | `/standard-work show` | 標準作業 | Show documentation |
| `/agent list` | `/standard-work verify` | 標準作業 | Verify completeness |
| `/memory search` | `/receipt search` | レシート | Search receipts |
| `/workflow execute` | `/template render` | 型 | Render templates |
| `/automate` | `/demand signal` | 需要信号 | Detect demand |

---

## Benefits of TCPS Alignment

### 1. Manufacturing Clarity
**Before**: Generic software terms (spec, architect, test)
**After**: Manufacturing terms (pull, heijunka, jidoka)
**Result**: Clear connection to Toyota Production System

### 2. Japanese Authenticity
**Before**: English translations lose precision
**After**: Japanese terms preserve original meaning
**Result**: Respects Toyota's manufacturing heritage

### 3. TCPS Documentation Consistency
**Before**: Commands don't match TCPS docs
**After**: Commands directly map to TCPS.md pillars
**Result**: Users can learn TCPS and commands together

### 4. Manufacturing Metaphor Reinforcement
**Before**: Users think "software development"
**After**: Users think "factory production line"
**Result**: Mindset shift to quality manufacturing

### 5. Receipt Chain Clarity
**Before**: "Deploy" sounds like IT operations
**After**: "Receipt" emphasizes audit trail
**Result**: Users understand immutable evidence

---

## Skills vs Commands

### Research: Skills System

Skills are **user-invocable commands** exposed via the Skill tool. They provide:
- Specialized workflows (e.g., `/commit`, `/review-pr`, `/pdf`)
- Integration with external systems
- Domain-specific operations

**Key difference**:
- **Commands** (`.claude/commands/`) - Internal workflow automation
- **Skills** - User-facing CLI shortcuts

**Recommendation**: TCPS commands should be **internal** (commands directory) while **skills** provide user-facing shortcuts.

Example skills that could invoke TCPS commands:
- `/tcps-pull` skill → calls `/tcps pull` command internally
- `/kaizen` skill → calls `/tcps kaizen` command internally
- `/andon` skill → calls `/tcps andon` command internally

---

## Implementation Recommendation

### Phase 1: Create TCPS Command Structure
1. Create `.claude/commands/tcps/` directory (8 commands)
2. Create `.claude/commands/poka-yoke/` directory (4 commands)
3. Create `.claude/commands/work-order/` directory (6 commands)
4. Create `.claude/commands/sku/` directory (3 commands)
5. Create `.claude/commands/standard-work/` directory (5 commands)

### Phase 2: Update Documentation
1. Update `COMMAND_INDEX.md` with TCPS terminology
2. Update `SYSTEM_GUIDE.md` with manufacturing metaphor
3. Update `CLAUDE.md` with TCPS command quick reference
4. Create `TCPS_COMMAND_GUIDE.md` for user onboarding

### Phase 3: Archive Old Commands
1. Move 30 old commands to `.claude/commands-archive/`
2. Update migration guide with TCPS mappings
3. Document Japanese terms in glossary

### Phase 4: Create User-Facing Skills (optional)
1. Create skills that wrap TCPS commands
2. Expose via Claude Code Skill tool
3. Provide short aliases (`/pull`, `/kaizen`, `/andon`)

---

## Glossary: Japanese TCPS Terms

| Japanese | Romaji | English | TCPS Usage |
|----------|--------|---------|------------|
| 自働化 | Jidoka | Automation with human touch | Built-in quality, stop-the-line |
| 行灯 | Andon | Lamp/Lantern | Visible problem signaling |
| 看板 | Kanban | Signboard | Work-in-progress visual management |
| 平準化 | Heijunka | Leveling | Production load balancing |
| 改善 | Kaizen | Improvement | Continuous incremental improvement |
| ポカヨケ | Poka-yoke | Mistake-proofing | Error prevention mechanisms |
| なぜなぜ分析 | Naze-naze bunseki | Why-why analysis | Root cause analysis (5 Whys) |
| 作業指示 | Sagyou shiji | Work order | Manufacturing order |
| レシート | Reshīto | Receipt | Audit trail record |
| 標準作業 | Hyōjun sagyō | Standard work | Documented process |
| 型 | Kata | Mold/Template | Production template |
| 需要信号 | Juyō shingō | Demand signal | Pull signal from market |
| 検証 | Kenshō | Verification | Quality verification |
| 認証 | Ninshō | Certification | Product certification |

---

## Next Steps

1. **User Decision**: Approve TCPS command structure (30 commands)
2. **Implementation**: Create 30 TCPS command files
3. **Documentation**: Update all docs with Japanese terminology
4. **Migration**: Archive old commands with migration guide
5. **Skills**: (Optional) Create user-facing skills

---

**Document**: TCPS_COMMAND_ALIGNMENT.md
**Created**: January 27, 2026
**Status**: Research Complete - Awaiting Approval
**Commands**: 30 TCPS-aligned commands (vs 30 generic commands)
**Benefit**: Manufacturing clarity + Japanese authenticity
