# Cloud Execution Guide for erlmcp

**Last Updated**: 2026-02-01
**Status**: Production-Ready
**Audience**: Cloud users, CI/CD engineers, autonomous developers

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Cloud Environment Overview](#cloud-environment-overview)
3. [SessionStart Hook](#sessionstart-hook)
4. [Quality Gates in Cloud](#quality-gates-in-cloud)
5. [State Persistence](#state-persistence)
6. [Session Teleportation](#session-teleportation)
7. [Autonomous Execution](#autonomous-execution)
8. [Error Recovery](#error-recovery)
9. [Practical Examples](#practical-examples)
10. [Best Practices](#best-practices)
11. [FAQ](#faq)

---

## Quick Start

### One-Page Cloud Workflow

**erlmcp is designed for autonomous execution in cloud environments.** Here's the typical workflow:

```bash
# Local terminal - Request cloud execution
$ claude "Fix the race condition in erlmcp_session_manager"

# Claude spawns cloud session
🌐 Cloud session created: session_015jLVUqHSQc86isYfzL4Byp
📥 SessionStart hook running (OTP 28.3.1 bootstrap, 60s)...
✅ Environment ready

# Cloud analyzes and implements
🔍 Analyzing codebase...
💻 Implementing fix with TDD...
🧪 Running tests...
✅ make check: All quality gates passed

# Cloud reports back
✅ Fixed race condition in erlmcp_session_manager:handle_call/3
✅ Added EUnit tests (100% coverage)
📊 Cost: $0.065 | Time: 6m 30s
🔗 Branch: claude/fix-session-race-DNaeK
💬 Ready for review. Create PR? (yes/no)
```

### Key Facts About Cloud Sessions

| Aspect | Details |
|--------|---------|
| **Duration** | 2-4 hours, auto-extends on activity |
| **Environment** | Ubuntu 20.04 VM, 4 vCPU, 8GB RAM |
| **OTP Version** | 28.3.1 (auto-installed, enforced) |
| **Bootstrap** | SessionStart hook runs automatically (~60s) |
| **Filesystem** | Ephemeral `/home/user/erlmcp` (state persisted via git) |
| **Network** | Outbound HTTPS (443) only, no inbound |
| **Cost Model** | $0.10/hour VM + $0.01/GB transfer |
| **Deterministic** | All tests produce identical results (cloud ≡ local) |

---

## Cloud Environment Overview

### What's Available in Cloud Sessions

**Pre-installed**:
- Erlang/OTP 28.3.1 (installed by SessionStart hook)
- rebar3 package manager
- Standard build tools (gcc, make, git, wget)
- Linux utilities

**Persistent Storage**:
- `/home/user/erlmcp/` - Project directory (ephemeral per session)
- `.erlmcp/cache/` - Dependency cache (persists 24h across sessions)
- `.erlmcp/work-orders/` - Task state serialization (JSON)
- `.erlmcp/receipts/` - Evidence artifacts (compliance reports)

**Network Access**:
- ✅ HTTPS (443) outbound to: packages.erlang-solutions.com, hex.pm, github.com, api.anthropic.com (optional)
- ❌ No inbound connections (security)
- ❌ No SMTP (25, 587) - no email sending
- ❌ No arbitrary TCP outbound

**Hardware**:
- 4 vCPU (CPU throttling varies by host load)
- 8GB RAM (shared, may fluctuate)
- SSD storage (fast I/O for compilation)

### Comparison: Cloud vs Local

| Aspect | Cloud (🌐) | Local (💻) |
|--------|-----------|-----------|
| Environment | Ubuntu 20.04 VM | User's hardware (varies) |
| OTP Version | 28.3.1 (enforced) | User-managed |
| Network | Restricted (HTTPS 443 only) | Unrestricted |
| Filesystem | Ephemeral per session | Persistent |
| Session Duration | 2-4 hours auto-extend | Unlimited |
| State Persistence | Git branch + state.json | Git + local changes |
| Cost | $0.10/hour + $0.01/GB transfer | Free (your hardware) |
| Determinism | ✅ All tests produce identical results | ⚠️ Hardware variance (benchmarks vary) |

### Environment Variables in Cloud

These are automatically set by SessionStart hook:

```bash
export CLAUDE_CODE_REMOTE=true          # Cloud execution marker
export ERLMCP_PROFILE=cloud             # Cloud-optimized config
export ERLMCP_CACHE="/home/user/erlmcp/.erlmcp/cache"
export TERM=dumb                        # Disable ANSI colors (rebar3 compat)
export REBAR_COLOR=none                 # Consistent output
export ERL_AFLAGS="-kernel shell_history enabled"
export ERLMCP_BUILD_HASH="<git-sha>"    # For traceability
```

These enable cloud-specific optimizations:
- Parallel quality gates (3x speedup)
- Dependency caching (80% faster builds)
- Cost tracking (per-gate cost breakdown)

---

## SessionStart Hook

### Automatic Bootstrap (Cloud Sessions)

**What**: The SessionStart hook initializes the cloud environment for erlmcp development. It runs **automatically** when a cloud session is created, and is **idempotent** (safe to run multiple times).

**Location**: `.claude/hooks/SessionStart.sh`

**Execution Time**: ~60 seconds (one-time per session)

### What SessionStart Does

```
┌─────────────────────────────────────────────────────────┐
│ Phase 1: OTP Version Detection (5s)                     │
│ - Check if erl binary exists                            │
│ - Parse current OTP version                             │
│ - Compare to required 28.3.1                            │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ Phase 2: OTP Installation (30-40s) [if needed]          │
│ - Download Erlang Solutions repository package          │
│ - Install via apt-get (with retries)                    │
│ - Verify installation                                   │
│ - Max 3 retries with exponential backoff                │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ Phase 3: Environment Setup (5s)                         │
│ - Create cache directories                              │
│ - Set environment variables                             │
│ - Source .erlmcp/cache/session.env                      │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ Phase 4: Pre-compilation (15-20s)                       │
│ - Fetch rebar3 dependencies (cached if available)       │
│ - Compile erlmcp_core (warm BEAM file cache)            │
│ - Non-fatal if fails (continues anyway)                 │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│ Phase 5: Idempotency Lock (1s)                          │
│ - Create lock file with OTP version                     │
│ - Prevents re-running if already initialized            │
└─────────────────────────────────────────────────────────┘
                          ↓
                    ✅ Ready for Development
```

### Caching Strategy

**SessionStart hook implements aggressive caching to minimize cloud time and cost:**

| Cache Target | Location | TTL | Hit Rate | Benefit |
|--------------|----------|-----|----------|---------|
| Dependencies | `_build/default/lib/` | 24h | 80% | Avoids hex.pm downloads |
| Compiled BEAM | `_build/default/` | Session | 95% | Incremental recompile |
| OTP 28.3.1 | /usr/lib/erlang/ | Global | 100% | Already installed |
| Session state | `.erlmcp/cache/session.env` | Session | 100% | Environment vars |

**Cost Impact**: With caching, a typical cloud session spends only 5-10 seconds initializing (vs 60s cold start).

### OTP 28.3.1 Installation Details

**If OTP is missing**, SessionStart installs OTP 28.3.1:

```bash
# Download Erlang Solutions repository
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb

# Install repository
sudo dpkg -i erlang-solutions_2.0_all.deb

# Install OTP 28.3.1
sudo apt-get update
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y esl-erlang=1:28.3.1-1

# Verify
erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)])' -s init stop
# Output: 28
```

**Retry Strategy**: If installation fails, SessionStart:
1. Waits 5 seconds
2. Clears rebar3 cache (`rm -rf _build/`)
3. Clears apt cache (`sudo apt-get clean`)
4. Retries (max 3 times)
5. Exits with error if all retries fail

### Idempotency Guarantee

SessionStart is **safe to run multiple times**. It checks:

```bash
# Check if already initialized
if [[ -f .erlmcp/cache/sessionstart.lock ]]; then
    LOCK_VERSION=$(cat .erlmcp/cache/sessionstart.lock)
    if version_meets_requirement $LOCK_VERSION; then
        echo "Already initialized, skipping..."
        exit 0
    fi
fi

# Create lock file after successful init
echo "28.3.1" > .erlmcp/cache/sessionstart.lock
```

This means you can safely call SessionStart multiple times without side effects.

### Monitoring SessionStart

**Real-time logging**:
```bash
# Watch SessionStart progress
tail -f .erlmcp/sessionstart.log

# Expected output
[2026-02-01T10:30:45Z] [INFO] SessionStart Hook Starting
[2026-02-01T10:30:45Z] [INFO] Required OTP version: 28.3.1
[2026-02-01T10:30:46Z] [INFO] Current OTP version: 28.0.0
[2026-02-01T10:30:46Z] [INFO] OTP version 28.0.0 < 28.3.1, installation required
[2026-02-01T10:31:15Z] [INFO] OTP 28.3.1 installation complete
[2026-02-01T10:31:15Z] [INFO] Setting up environment variables...
[2026-02-01T10:31:16Z] [INFO] Pre-compiling erlmcp_core...
[2026-02-01T10:31:35Z] [INFO] erlmcp_core compilation successful
[2026-02-01T10:31:36Z] [INFO] SessionStart Hook Complete
```

---

## Quality Gates in Cloud

### Deterministic Results

**Invariant**: ∀test. result(cloud) = result(local)

All quality gates produce identical results in cloud and local environments. This is enforced by:
- Chicago TDD: No mocks, all real processes
- Deterministic test suites: No timing or hardware dependencies
- Environment-agnostic validation: Same code paths everywhere

### Parallel Execution (3x Speedup)

Cloud gates execute in **parallel** by default:

```bash
# Sequential execution (local development loop)
$ time make verify-fast
  compile: 30s
  eunit:   60s
  ──────────
  Total:   90s real

# Parallel execution in cloud
$ time make check
  compile:   30s (wait)
  dialyzer:  90s ┐
  xref:      30s ├→ run in parallel
  eunit:     60s │
  ct:       120s ┘
  ──────────
  Total:   150s real (2.5x speedup vs sequential)
```

**Makefile implementation**:
```make
check-parallel: compile
	@$(MAKE) -j4 dialyzer xref eunit ct coverage
	@echo "✅ All gates passed"
```

### Quality Gate Matrix

| Gate | Sequential | Cloud Time | Local Time | Cost | Parallel? | Deterministic? |
|------|-----------|-----------|-----------|------|-----------|----------------|
| Compile | 1st | 30s | 30s | $0.01 | N/A | ✅ |
| EUnit | 60s | 60s | 30s | $0.02 | ✅ | ✅ |
| CT | 120s | 120s | 60s | $0.04 | ✅ | ✅ |
| Dialyzer | 90s | 90s | 90s | $0.03 | ✅ | ✅ |
| Xref | 30s | 30s | 30s | $0.01 | ✅ | ✅ |
| Coverage | 30s | 30s | 20s | $0.01 | ✅ | ✅ |
| **Total** | 360s | 150s | 260s | $0.12 | 2.4x | ✅ |

### Cost Breakdown

**Per-gate cost calculation** ($0.10/hour VM + $0.01/GB data transfer):

```
Compile:   30s / 3600s = 0.0083h × $0.10 = $0.0008
EUnit:     60s / 3600s = 0.0167h × $0.10 = $0.0017
CT:       120s / 3600s = 0.0333h × $0.10 = $0.0033
Dialyzer:  90s / 3600s = 0.0250h × $0.10 = $0.0025
Xref:      30s / 3600s = 0.0083h × $0.10 = $0.0008
Coverage:  30s / 3600s + 50MB × $0.01/GB = $0.0013
────────────────────────────────────────────────────
TOTAL:                                      $0.0104
```

**Real-world budget** (100 iterations/day × 22 days/month):
```
100 iterations/day × $0.0104 = $1.04/day
22 working days/month = $22.88/month (full development)
```

### Cloud-Only vs Local-Only Gates

**Cloud-Ready Gates** (all produce identical results):
- ✅ Compile
- ✅ EUnit
- ✅ CT (integration tests)
- ✅ Dialyzer (type checking)
- ✅ Xref (undefined functions)
- ✅ Coverage

**Local-Only Gates** (hardware-dependent):
- ❌ Benchmark (VM performance varies by host load, 10-30% variance)
- ❌ Observer GUI (requires X11 display)

**Cloud Alternative for Benchmarks**:
```bash
# In cloud: Run benchmarks for FUNCTIONAL correctness
rebar3 eunit --module=erlmcp_bench_core_ops

# Expected: Tests pass, no crashes
# NOT TESTED: Performance regression (too much VM variance)

# For performance regression: Run locally
make benchmark-quick  # 300s, consistent hardware
```

### Common Gate Commands

```bash
# Quick verification (compile + eunit, ~90s)
make verify-fast

# Full quality gates (parallel, ~150s in cloud)
make check

# Incremental testing (only modified modules, ~45s, 50% cost savings)
make test-changed

# Comprehensive report with evidence bundles
make quality-report  # ~240s, generates TPS evidence

# Individual gates
rebar3 compile                    # Syntax check
rebar3 eunit                      # Unit tests
rebar3 ct                         # Integration tests
rebar3 dialyzer                   # Type checking
rebar3 xref                       # Undefined functions
rebar3 cover                      # Coverage report
```

---

## State Persistence

### Preserving State Across Sessions

Cloud sessions are **ephemeral** (deleted after 2-4 hours), but your development state is **persistent** via git:

```
Local Changes
     ↓
Git Commit/Push
     ↓
Cloud Session (ephemeral)
     ↓
Git Branch Persists
     ↓
Pull into Next Session/Local Dev
```

### Git Branch Tracking

**All work is tracked on auto-created branches**:

```bash
# Cloud-created branch (auto-named)
claude/fix-session-race-DNaeK
claude/add-metrics-dashboard-KmL7P
claude/refactor-supervision-tree-9xYqZ

# All commits pushed to origin
# Branch persists after cloud session expires
```

### Work Order Serialization

**Work orders are stored as JSON** in `.erlmcp/work-orders/`:

```bash
.erlmcp/work-orders/
├── queue.json          # Pending tasks
├── wip.json            # In-progress tasks
└── completed.json      # Completed tasks (archive)
```

**Example work order**:
```json
{
  "id": "wo-001",
  "task": "Implement FSM supervision tree",
  "agent": "erlang-architect",
  "priority": "high",
  "dependencies": [],
  "constraints": {
    "time_budget": 300,
    "cost_budget": 0.10,
    "files": ["apps/erlmcp_core/src/erlmcp_core_sup.erl"],
    "branch": "claude/fsm-supervision-tree"
  },
  "status": "done",
  "result": {
    "exit_code": 0,
    "gates": {
      "compile": "pass",
      "eunit": "pass",
      "dialyzer": "pass",
      "xref": "pass",
      "coverage": "pass"
    },
    "artifacts": [
      ".erlmcp/receipts/wo-001-quality-report.json"
    ],
    "duration": 287
  }
}
```

### Chat History Sync

**Claude API maintains chat history**:
- Full conversation persisted server-side
- Available via session ID
- Can resume work orders from previous session
- Human-readable transcripts in `.erlmcp/transcripts/`

### Recovery on Failure

**If cloud session crashes**:
1. Git branch persists (push before crash)
2. Work order state serialized (last known state)
3. Resume via session ID or pull latest branch locally

```bash
# Resume from cloud session ID
claude --teleport session_015jLVUqHSQc86isYfzL4Byp

# Or pull branch locally
git fetch origin claude/fix-session-race-DNaeK
git checkout claude/fix-session-race-DNaeK
# Continue development locally
```

---

## Session Teleportation

### What is Teleportation?

**Teleportation** := Seamless transfer of complete development context (code, history, state) between cloud and local environments.

### When to Teleport

| Scenario | Cloud → Local | Local → Cloud |
|----------|---------------|---------------|
| Interactive debugging | ✅ (local debugger superior) | ❌ |
| REPL exploration | ✅ (observer, shell tools) | ❌ |
| Hardware profiling | ✅ (consistent hardware) | ❌ |
| Autonomous completion | ❌ | ✅ (cost-effective) |
| Parallel experiments | ❌ | ✅ (multi-session) |
| Network-dependent tests | ❌ | ✅ (cloud network) |

### Cloud → Local Teleportation

**Scenario**: You're in a cloud session, but need local debugger tools.

```bash
# In cloud session (or local terminal)
$ claude --teleport session_015jLVUqHSQc86isYfzL4Byp

# Claude validates prerequisites
✅ Git state: clean (all changes committed)
✅ OTP version: 28.3.1 (matches local)
✅ Work orders: 3 completed, 0 in progress
✅ Branch: claude/fix-session-race-DNaeK (pushed to origin)

# Fetch from cloud to local
Fetching branch claude/fix-session-race-DNaeK...
Downloading session state (.erlmcp/session-state.json)...
Syncing chat history (142 messages)...

# Verify integrity
Verifying git tree SHA: abc123def456... ✅
Verifying work order checksums... ✅

# Ready for local debugging
✅ Session teleported successfully!
📂 Branch: claude/fix-session-race-DNaeK
💬 Chat history: 142 messages loaded
🎯 Next: Continue development locally with debugger

# Use local tools
make observer                    # Visual process monitor
make console                     # Erlang shell
rebar3 eunit --module=M --verbose
```

### Local → Cloud Offloading

**Scenario**: You're developing locally, but want cloud to complete work autonomously while you sleep.

```bash
# Local terminal - Offload to cloud
$ claude --offload "Complete implementation and create PR"

# Claude validates local state
✅ Git state: clean
✅ Working branch: my-feature-branch
✅ All uncommitted changes: ready to push

# Push to cloud
Pushing branch my-feature-branch to origin...
Uploading session state...

# Cloud session created
🌐 Cloud session created: session_017kMNOpQrSTuv
📥 Loading local state...
✅ Ready for autonomous execution

# Cloud completes work autonomously
[Cloud] Resuming work orders:
  - wo-004: Complete FSM implementation (in progress)
  - wo-005: Create PR with quality gates (queued)
[Cloud] Executing wo-004... ✅
[Cloud] Executing wo-005... ✅
[Cloud] PR created: https://github.com/user/erlmcp/pull/42

# Notification back to local
✅ Implementation completed
✅ PR created: https://github.com/user/erlmcp/pull/42
💰 Cost: $0.12 | Time: 8m 30s
```

### Teleportation Requirements

**Pre-Teleport Checklist**:
1. ✅ Clean git state (no uncommitted changes) OR branch pushed to origin
2. ✅ All work orders in deterministic state (done or queued, not partial)
3. ✅ No running erlmcp processes (stop application before teleporting)
4. ✅ Session ID available (from cloud session URL or history)

**Post-Teleport Validation**:
```bash
# Verify integrity after teleport
git rev-parse HEAD                       # Check branch HEAD
cat .erlmcp/session-state.json           # Verify state file
git log --oneline -5                     # Check commit history
make verify-fast                         # Run quick tests
```

### Safety Guarantees

**Invariant**: ∀teleport. state(cloud, t) ≡ state(local, t+δ) (state preservation)

| Guarantee | Implementation |
|-----------|----------------|
| No data loss | All changes committed to git branch |
| Deterministic state | Work orders serialized to JSON |
| Chat continuity | Full history synced via Claude API |
| Environment parity | OTP version verified (fail if mismatch) |
| File integrity | Git tree SHA verified post-teleport |

**Rollback**: If teleport fails → stay in source environment (no partial state)

---

## Autonomous Execution

### What is Autonomous Execution?

**Autonomous execution** := System automatically implements, tests, and validates work without human intervention, using multi-agent coordination and work orders.

### Work Order Protocol

**Work orders** are the unit of autonomous work. Each has:
- **ID**: Unique identifier (wo-001, wo-002, etc.)
- **Task**: What to implement ("Implement FSM supervision tree")
- **Agent**: Which agent executes (erlang-architect, erlang-otp-developer, etc.)
- **Priority**: high, normal, or low
- **Dependencies**: Other work orders that must complete first
- **Constraints**: Time budget, cost budget, file locks, branch name
- **Status**: queued → wip → done (or failed)
- **Result**: Exit code, gate results, artifacts, duration

**Example work order**:
```erlang
#{
    id => "wo-001",
    task => "Implement FSM supervision tree",
    agent => erlang_architect,
    priority => high,
    dependencies => [],
    constraints => #{
        time_budget => 300,         % seconds
        cost_budget => 0.15,        % USD
        files => [
            "apps/erlmcp_core/src/erlmcp_core_sup.erl"
        ],
        branch => "claude/fsm-supervision"
    },
    status => done,
    result => #{
        exit_code => 0,
        gates => #{
            compile => pass,
            eunit => pass,
            dialyzer => pass,
            xref => pass,
            coverage => pass
        },
        artifacts => [
            ".erlmcp/receipts/wo-001-quality-report.json"
        ],
        duration => 287
    }
}
```

### Multi-Agent Coordination

**Agents** are specialized roles that execute work orders:

| Agent | Domain | Cloud-Ready | Examples |
|-------|--------|-------------|----------|
| erlang-architect | System design | ✅ | Supervision trees, FSM design |
| erlang-otp-developer | Implementation | ✅ | gen_server, behavior patterns |
| erlang-test-engineer | Testing | ✅ | EUnit, CT, Chicago TDD |
| erlang-researcher | Analysis | ✅ | Codebase exploration, patterns |
| code-reviewer | Quality | ✅ | OTP compliance, style |
| erlang-github-ops | Git/PR | ✅ | Branch creation, PR management |

### Execution Timeline

**Example**: "Implement Armstrong-style FSMs with full TDD"

```
t=0s:   wo-001 (architect) starts
        Design FSM supervision tree
        ↓ (180s later)

t=180s: wo-001 done → wo-002, wo-003 start in parallel
        wo-002 (developer): Implement FSM
        wo-003 (test-engineer): Write tests
        ↓ (120s later)

t=300s: wo-002, wo-003 done → wo-004 starts
        wo-004 (github-ops): Create PR
        ↓ (60s later)

t=360s: wo-004 done → wo-005 starts
        wo-005 (code-reviewer): Review compliance
        ↓ (60s later)

t=420s: wo-005 done → Report to user

Result: Complete FSM implementation + tests + PR in 7 minutes
Cost: ~$0.15 (7 minutes × $0.10/hour)
```

### Kanban Limits (Single-Tasking)

**Constraint**: ∀agent. |WIP(agent)| ≤ 1

Each agent works on exactly **one task at a time** for maximum quality:

```
Allowed (correct):
- Agent A: wo-001 (wip)
- Agent B: wo-002 (wip)
- Agent C: wo-003 (wip)

Forbidden (violates Kanban):
- Agent A: wo-001 (wip) + wo-002 (wip)  ❌ Too many tasks
```

### File-Level Locking

**Before modifying a file**, agents acquire a lock:

```erlang
%% Acquire lock before modifying src/erlmcp_client.erl
case erlmcp_work_order:acquire_lock("src/erlmcp_client.erl") of
    {ok, LockToken} ->
        %% Proceed with modification
        modify_file(...),
        erlmcp_work_order:release_lock(LockToken);
    {error, {locked_by, OtherAgent, OtherWO}} ->
        %% Conflict: other agent is modifying same file
        io:format("File locked by ~p in ~p~n", [OtherAgent, OtherWO]),
        {error, file_locked}
end
```

This prevents merge conflicts and concurrent modification of same file.

### Dependency Resolution

**Work orders can depend on other work orders**:

```erlang
WorkOrders = [
    #{id => "wo-001", task => "Implement FSM",
      dependencies => [], priority => high},

    #{id => "wo-002", task => "Test FSM",
      dependencies => ["wo-001"], priority => high},

    #{id => "wo-003", task => "Review FSM",
      dependencies => ["wo-001", "wo-002"], priority => normal}
].

%% Execution order:
%% wo-001 → wo-002 → wo-003
%% (wo-002 waits for wo-001, wo-003 waits for both)
```

### Decision Points (Human-in-the-Loop)

**Autonomous execution can pause for critical decisions**:

```
⚠️ Found ambiguity: Two possible approaches
   A) Add optional timeout parameter
   B) Fail fast without timeout

   Which approach? (A/B)

👤 User response: A
✅ Proceeding with approach A
```

**Decision categories**:
- **Design decisions**: Multiple valid solutions (user chooses)
- **Breaking changes**: Risk assessment (approval required)
- **Security credentials**: Missing API keys (user provides)
- **Merge conflicts**: Semantic conflicts (manual resolution)
- **Budget exceeded**: Cost limit reached (approve more budget)

---

## Error Recovery

### Automatic Recovery Errors

**These errors are auto-recovered** without user intervention:

| Error Class | Recovery Strategy | Timeout | Example |
|-------------|-------------------|---------|---------|
| Network timeout | Retry 3x with exponential backoff | 30s | `wget` fails |
| Dependency fetch | Clear cache, retry | 60s | hex.pm unavailable |
| Rate limit | Wait + retry (respect 429 headers) | 300s | GitHub API limit |
| Transient process crash | Supervisor restarts | 5s | Crashed worker |

**Exponential backoff example**:
```erlang
retry_with_backoff(Url, [1000, 2000, 4000]) ->
    % Attempt 1: wait 1s before retry
    timer:sleep(1000),
    case httpc:request(Url) of
        {ok, Response} -> {ok, Response};
        {error, timeout} ->
            % Attempt 2: wait 2s before retry
            timer:sleep(2000),
            case httpc:request(Url) of
                {ok, Response} -> {ok, Response};
                {error, timeout} ->
                    % Attempt 3: wait 4s before retry
                    timer:sleep(4000),
                    httpc:request(Url)
            end
    end.
```

### Manual Intervention Errors

**These require user approval**:

| Error Class | Reason | User Action |
|-------------|--------|-------------|
| API design decision | Ambiguity | Choose between options A/B |
| Breaking change | Risk assessment | Approve/reject |
| Security credential | Missing secret | Provide API key/token |
| Merge conflict | Semantic conflict | Resolve manually |
| License violation | Legal review | Approve dependency |
| Budget exceeded | Cost limit | Increase budget or abort |

**Merge conflict example**:
```
⚠️ Merge conflict detected in src/erlmcp_client.erl

Conflict: Lines 42-58 (both branches modified handle_call/3)

Branch A (yours): Added timeout parameter
Branch B (main): Added error handling

🤖 Auto-merge failed (semantic conflict)
👤 Manual intervention required

Options:
1. Resolve locally (claude --teleport)
2. Cherry-pick changes (keep A, apply B manually)
3. Abort (discard branch)

Your choice (1/2/3)?
```

### Contributing Recovery Hooks

**Add custom recovery logic** via shell hooks:

```bash
#!/usr/bin/env bash
# .claude/hooks/recovery/handle_dependency_error.sh
#
# Args: $1 = error_type, $2 = error_message, $3 = context_json
# Returns: 0 (recovered) | 1 (manual intervention) | 2 (fatal)

ERROR_TYPE="$1"
ERROR_MSG="$2"
CONTEXT="$3"

if [[ "$ERROR_TYPE" == "dependency_not_found" ]]; then
    # Attempt recovery: update rebar.config with hex.pm latest
    PACKAGE=$(echo "$CONTEXT" | jq -r '.package')
    rebar3 update "$PACKAGE"
    exit 0  # Recovered
fi

exit 1  # Manual intervention required
```

### Cost Tracking and Limits

**Cloud execution tracks costs per work order**:

```json
{
  "wo-001": {
    "task": "Implement FSM",
    "start_time": "2026-02-01T10:30:00Z",
    "end_time": "2026-02-01T10:35:00Z",
    "duration_seconds": 300,
    "cost_breakdown": {
      "compute_minutes": 5.0,
      "vm_cost": 0.00833,
      "data_transfer_mb": 50,
      "transfer_cost": 0.00050,
      "total": 0.00883
    }
  }
}
```

**Budget enforcement**:
```erlang
%% If cost exceeds budget, cloud pauses work order
{error, budget_exceeded, #{
    spent => 0.150,
    budget => 0.100,
    work_order => "wo-005"
}}

%% User must approve additional budget or abort work order
```

---

## Practical Examples

### Example 1: Quick Fix in Cloud

**Task**: Fix a simple bug and get feedback fast.

```bash
# Local terminal
$ claude "Fix the typo in erlmcp_session_manager:handle_call/3"

# Cloud executes (completely autonomous)
🌐 Cloud session started
📥 SessionStart hook: OTP 28.3.1 ready
🔍 Analyzing codebase...
🐛 Found typo: line 142, variable name
💻 Implementing fix...
🧪 Running tests...
  ✅ make verify-fast: passed (90s, $0.03)
📊 All gates passed
✅ Branch: claude/fix-typo-KmL7P

# Result reported
✅ Fixed typo in erlmcp_session_manager:handle_call/3 line 142
📊 Cost: $0.03 | Time: 2 minutes
💬 Create PR? (yes/no)
```

**Cost**: $0.03
**Time**: 2 minutes
**Effort**: Local user input only at start and end

### Example 2: Comprehensive Feature (Multi-Agent)

**Task**: Implement complete feature with tests, docs, and PR.

```bash
# Local terminal (multi-agent orchestration)
$ claude "Add rate limiting to session manager with full TDD and PR"

# Cloud spawns parallel work orders
🌐 Cloud session started

wo-001 (architect):    Design rate-limit FSM
wo-002 (developer):    Implement rate_limiter behavior
wo-003 (test-engineer): Write EUnit + CT tests
wo-004 (github-ops):   Create PR with changelog
wo-005 (reviewer):     Verify OTP compliance

# Execution timeline
t=0s:    wo-001 starts
t=180s:  wo-001 done → wo-002, wo-003 start
t=300s:  wo-002, wo-003 done → wo-004 starts
t=360s:  wo-004 done → wo-005 starts
t=420s:  wo-005 done → Report

# Final report
✅ Rate limiting feature complete
✅ Tests: 24 new tests, 100% coverage
✅ PR created: https://github.com/user/erlmcp/pull/99
📊 Cost: $0.25 | Time: 7 minutes
💬 Ready for review!
```

**Cost**: $0.25 (7 minutes × $0.10/hour ÷ 60)
**Time**: 7 minutes (parallel execution)
**Effort**: Local user input only at start; cloud is fully autonomous

### Example 3: Iterative Development (Test-Changed)

**Task**: Make 5 small changes, test only affected modules.

```bash
# Change 1: Modify erlmcp_cache.erl
make test-changed    # Runs only cache tests
Cost: $0.015 | Time: 45s

# Change 2: Modify erlmcp_registry.erl
make test-changed    # Runs only registry tests
Cost: $0.015 | Time: 45s

# Change 3: Modify both session and cache
make test-changed    # Runs both test suites + shared dependencies
Cost: $0.020 | Time: 60s

# ... 5 iterations later
Total cost for iterative development: $0.10 (50% savings vs full gates)
```

**Advantage**: Incremental cost savings for rapid iteration

### Example 4: Teleportation Scenario

**Task**: Start in cloud, need local debugger.

```bash
# Cloud session (make progress)
Cloud> rebar3 eunit --module=erlmcp_session_manager_tests
  FAILED: 2 test cases fail
  Need to debug with observer

# Teleport to local
Cloud> exit
Local$ claude --teleport session_015jLVUqHSQc86isYfzL4Byp

✅ Session teleported successfully!
📂 Branch: claude/debug-session-manager

# Use local tools
Local$ make observer      # Visual process monitoring
Local$ make console       # Erlang shell with full process introspection
Local> eunit:test(erlmcp_session_manager_tests, [{verbose}]).

[DEBUG in observer: trace process, watch messages]

✅ Found bug: race condition in handle_call/3

# Fix locally
Local$ vim apps/erlmcp_core/src/erlmcp_session_manager.erl
Local$ rebar3 eunit --module=erlmcp_session_manager_tests
  ✅ All tests pass

# Push back to cloud for autonomous completion
Local$ git push origin claude/debug-session-manager
Local$ claude --offload "Create PR and deploy"

🌐 Cloud resumed with latest changes
  ✅ Full quality gates passed
  ✅ PR created: https://github.com/.../pull/42
```

---

## Best Practices

### When to Use Cloud vs Local

| Task | Cloud (🌐) | Local (💻) |
|------|-----------|-----------|
| Autonomous completion | ✅ BEST | ❌ (need you watching) |
| Iterative development | ✅ GOOD | ✅ BETTER (no network latency) |
| Interactive debugging | ❌ | ✅ BEST (observer, shell) |
| Benchmarking | ❌ (variance) | ✅ BEST (consistent hardware) |
| Feature design | ❌ | ✅ BEST (collaborative) |
| Running tests | ✅ GOOD | ✅ GOOD (symmetric) |
| Parallel CI/CD | ✅ BEST | ⚠️ (depends on local CPU) |

### Cost Optimization Strategies

**Strategy 1: Incremental Testing**
```bash
# Develop locally, use make test-changed
make test-changed      # 45s, $0.015 (vs full test suite)
# 50% cost savings over full gates
```

**Strategy 2: Lazy Cloud Offloading**
```bash
# Develop locally, offload only when done
make check             # Local (free)
git push              # Local (free)
claude --offload "Create PR"  # Cloud ($0.025)
# Total cost: ~$0.025 (just for PR creation)
```

**Strategy 3: Batch Work Orders**
```bash
# Collect multiple small tasks
T1: Fix typo
T2: Add comment
T3: Update docstring

# Execute all at once in cloud (shared setup cost)
claude "Fix T1, T2, T3"
# Savings: Amortize SessionStart (60s) across 3 tasks
```

**Strategy 4: Parallel Gates**
```bash
# Always use parallel in cloud (automatic in make check)
make check  # 150s parallel (vs 360s sequential)
# 60% time savings (same cost)
```

### Time Budget Planning

**Typical work order durations**:

| Type | Minimum | Expected | Maximum |
|------|---------|----------|---------|
| Typo fix | 1m | 2m | 5m |
| Bug fix | 3m | 5m | 15m |
| Small feature | 5m | 10m | 30m |
| Feature + tests | 10m | 20m | 60m |
| Multi-agent feature | 5m | 10m | 20m (parallel) |

**Example budget calculation**:
```
Task: Implement rate limiting with tests + PR
  - Design (architect): 3m
  - Implementation (developer): 5m
  - Tests (test-engineer): 5m
  - PR (github-ops): 1m
  - Review (reviewer): 1m

Parallel execution: max(3, 5, 5, 1, 1) = 5m actual
Cost: 5m × $0.10/hour ÷ 60 = $0.0083

Total with SessionStart (1m overhead): 6m × $0.10/hour ÷ 60 = $0.01
```

### Documentation and Evidence

**Cloud sessions generate evidence artifacts**:

```bash
.erlmcp/receipts/
├── wo-001-quality-report.json    # Gate results + coverage
├── wo-002-compliance-report.json # MCP compliance check
├── wo-003-changelog.md           # Auto-generated changelog
└── wo-004-evidence-bundle.pdf    # Complete evidence packet
```

**Compliance reports** (auto-generated):
```json
{
  "work_order": "wo-001",
  "task": "Implement FSM",
  "gates": {
    "compile": { "status": "pass", "duration_ms": 30000 },
    "eunit": { "status": "pass", "duration_ms": 60000, "coverage": 0.95 },
    "ct": { "status": "pass", "duration_ms": 120000 },
    "dialyzer": { "status": "pass", "duration_ms": 90000 },
    "xref": { "status": "pass", "duration_ms": 30000 },
    "coverage": { "status": "pass", "target": 0.80, "actual": 0.95 }
  },
  "timestamp": "2026-02-01T10:35:00Z",
  "cost_usd": 0.00883
}
```

---

## FAQ

### Q: How long are cloud sessions available?

**A**: 2-4 hours, with automatic extension on activity. If you exceed 4 hours, the session is archived and your state is preserved via git branch.

### Q: Can I run benchmarks in cloud?

**A**: Yes for **functional correctness** (tests pass), but **not for performance regression** (results vary due to VM load). Run benchmarks locally for consistent hardware performance.

```bash
# Cloud: functional validation only
rebar3 eunit --module=erlmcp_bench_core_ops  # ✅ Pass/fail tests

# Local: performance regression
make benchmark-quick                          # ✅ Consistent results
```

### Q: What happens if my session expires?

**A**: Your state is preserved:
- All git commits are pushed to origin
- Work orders serialized to `.erlmcp/work-orders/completed.json`
- Chat history available via session ID
- You can resume with `claude --teleport <session_id>`

### Q: Can I pause and resume work?

**A**: Yes, completely:
```bash
# Cloud session
Cloud> [make progress]
Cloud> exit  # Pause

# Later, resume
Local$ claude --teleport session_015jLVUqHSQc86isYfzL4Byp
✅ Session restored with all context
```

### Q: How much does cloud execution cost?

**A**: $0.10/hour VM + $0.01/GB transfer. Typical work:
- Quick fix (2m): $0.003
- Medium feature (10m): $0.017
- Full development loop (30m): $0.05
- Entire workday (8h): $0.80

### Q: What network access does cloud have?

**A**: HTTPS (443) only to:
- packages.erlang-solutions.com (OTP installation)
- hex.pm (rebar3 packages)
- github.com (git operations)
- api.anthropic.com (optional, for LLM tests)

### Q: Can I use local LLM in cloud?

**A**: Yes, use `mock_llm` module (no network required). Real LLM calls to Anthropic API are optional and require network access.

### Q: How do I cancel a work order?

**A**: Interrupt the cloud session:
```bash
Ctrl+C  # Stops current work order

🌐 Cloud session interrupted
  ✅ Work order wo-001 saved (partially complete)
  ✅ State serialized to .erlmcp/work-orders/wip.json
  💬 Resume with: claude --teleport <session_id>
```

### Q: Can multiple agents work in parallel?

**A**: Yes, up to 4 agents in parallel (limited by 4 vCPU in cloud VM). Depends on work order dependencies:
- **Independent work orders**: Run in parallel
- **Dependent work orders**: Run sequentially (wo-002 waits for wo-001)

### Q: Does cloud have persistent storage?

**A**: Only `/home/user/erlmcp/.erlmcp/cache/` (24h TTL). Everything else (code, tests) persists via git. This is intentional to keep cloud sessions lightweight.

### Q: How do I debug failing tests in cloud?

**A**: Teleport to local:
```bash
Cloud> exit
Local$ claude --teleport session_015jLVUqHSQc86isYfzL4Byp
Local$ make console        # Interactive shell with full debugging
Local> erlmcp_session_manager:get_session(<<"test">>).
{ok, #{id => <<"test">>, ...}}
```

### Q: Can I use cloud for production deployments?

**A**: Cloud sessions are **development-only**. For production:
1. Develop in cloud (fast iteration)
2. Teleport to local (final review)
3. Merge to main (trigger production CI/CD)

---

## See Also

- **[CLAUDE.md](../CLAUDE.md)** - System specification, OTP patterns, development standards
- **[DEVELOPMENT.md](../DEVELOPMENT.md)** - Local development setup and workflow
- **[README.md](../README.md)** - Project overview and quick start
- **[.claude/hooks/SessionStart.sh](../.claude/hooks/SessionStart.sh)** - Cloud bootstrap script
- **[PROJECT_STATUS_REPORT_v3.0.0.md](../PROJECT_STATUS_REPORT_v3.0.0.md)** - Latest project evaluation

