# Fix erlmcp_registry_dist_SUITE distributed node initialization failure Manufacturing Plan

**TCPS Compliance**: Lean Six Sigma Level Strictness
**Zero Defects**: 99.99966% defect-free delivery (3.4 defects per million)

## Manufacturing Objective

Fix the Common Test suite `erlmcp_registry_dist_SUITE` to properly initialize distributed Erlang nodes for testing multi-node registry functionality. The suite currently fails with boot_timeout errors because slave nodes cannot initialize within the 10-second timeout, EPMD is not verified before startup, and node naming creates DNS resolution delays.

**Why this matters**: Cannot test distributed registry functionality if nodes can't communicate. This is critical for multi-node deployments and blocks all integration testing of cluster features.

### Quality Gate Requirements

- **Compilation**: 0 errors (MANDATORY)
- **Common Test**: 100% pass rate (9/9 test cases passing) - MANDATORY
- **Coverage**: ≥80% for test helper functions (MANDATORY)
- **Dialyzer**: 0 warnings (MANDATORY)
- **Xref**: 0 undefined function calls (MANDATORY)
- **Performance**: Slave node startup <120s on macOS/CI (MANDATORY)
- **EPMD Health**: Must verify EPMD running before distributed operations (MANDATORY)

## Current State

### What Exists Now

- **Modules**:
  - `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl` (361 lines)
    - Lines 60-75: `init_per_suite/1` - starts net_kernel without EPMD verification
    - Lines 87-94: `init_per_group(multi_node, 2)` - starts slave nodes without health checks
    - Lines 334-353: `start_slave_nodes/3` - 10s timeout, no hostname specification
  - `apps/erlmcp_core/src/erlmcp_registry_dist.erl` (343 lines) - ✅ Production-ready distributed registry
  - `apps/erlmcp_core/src/erlmcp_cluster_sup.erl` (72 lines) - ✅ Cluster supervisor
  - `apps/erlmcp_core/src/erlmcp_node_monitor.erl` (175 lines) - ✅ Node health monitoring
  - `apps/erlmcp_core/src/erlmcp_split_brain_detector.erl` - ✅ Split-brain detection

- **Tests**:
  - EUnit: 15/15 passing (100% pass rate in `erlmcp_registry_distributed_tests.erl`)
  - Common Test: 0/9 passing (0% pass rate - all multi_node tests fail)

- **Quality**:
  - ✅ Compilation: 0 errors
  - ✅ Dialyzer: 0 warnings (code has complete type specs)
  - ✅ Xref: 0 undefined function calls
  - ❌ Common Test: 0% pass rate (0/9 tests passing)

### What's Missing

- **Gap**: 7 test cases failing in multi_node group (78% failure rate)
- **Root Cause**: Missing Poka-yoke (mistake-proofing) in test infrastructure:
  1. No EPMD health verification before starting distributed Erlang
  2. Boot timeout too short (10s insufficient on macOS/CI)
  3. Node naming uses implicit hostname resolution (causes delays)
  4. No retry logic for node startup
  5. No configurable timeouts for different environments

- **Impact**:
  - Blocks ALL distributed registry testing
  - Cannot verify multi-node failover scenarios
  - Cannot test split-brain detection
  - Cannot validate node reconnection logic
  - Prevents integration testing of cluster features

### Key Discoveries from Code Verification

- **Line 340** (`erlmcp_registry_dist_SUITE.erl`): `NodeName = list_to_atom("slave" ++ integer_to_list(N))` - creates short names without hostname, net_kernel appends FQDN (e.g., `slave1@Seans-MacBook-Pro`)
- **Line 342** (`erlmcp_registry_dist_SUITE.erl`): `{boot_timeout, 10}` - 10 seconds is insufficient for VM boot on macOS/CI
- **Lines 60-75** (`erlmcp_registry_dist_SUITE.erl`): No EPMD verification before net_kernel:start
- **Line 351** (`erlmcp_registry_dist_SUITE.erl`): Has `pong = net_adm:ping(Node)` but runs AFTER boot succeeds
- **OTP Pattern to follow**: Test fixture setup from `quality_gates_SUITE.erl:43-54` (proper application startup)
- **Constraint**: Must work with Erlang/OTP 25+, rebar3, and ct_slave (deprecated but functional)

## Desired End State

### Specification

**Exact specification of what we're building:**

1. **EPMD Health Check Function** (NEW)
   - Module: `erlmcp_registry_dist_SUITE`
   - Function: `epmd_health_check() -> ok | {error, Reason}`
   - Behavior: Verify EPMD is responding on port 4369 with retry logic
   - Location: Add as internal helper function (lines 370-390)

2. **Enhanced init_per_suite/1** (MODIFY)
   - Module: `erlmcp_registry_dist_SUITE`
   - Function: `init_per_suite(Config) -> Config`
   - Behavior: Add EPMD health check BEFORE net_kernel:start, fail fast if EPMD not running
   - Location: Lines 60-75
   - Integration: Must call epmd_health_check() as first step

3. **Fixed start_slave_nodes/3** (MODIFY)
   - Module: `erlmcp_registry_dist_SUITE`
   - Function: `start_slave_nodes(Count, Config) -> [node()]`
   - Behavior: Use localhost short names, increase timeout to 120s, add startup logging
   - Location: Lines 334-353
   - Changes:
     - Use explicit `127.0.0.1` hostname (eliminate DNS delays)
     - Increase all timeouts to 120s/30s/30s
     - Add startup progress logging
     - Support configurable timeout via environment variable

4. **Enhanced init_per_group/2 for multi_node** (MODIFY)
   - Module: `erlmcp_registry_dist_SUITE`
   - Function: `init_per_group(multi_node, Config) -> Config`
   - Behavior: Add cluster connectivity verification after slave startup
   - Location: Lines 87-94
   - Integration: Must verify 3 nodes connected (master + 2 slaves)

5. **Safe end_per_group/2 for multi_node** (MODIFY)
   - Module: `erlmcp_registry_dist_SUITE`
   - Function: `end_per_group(multi_node, Config) -> ok`
   - Behavior: Graceful slave shutdown with error handling and logging
   - Location: Lines 99-105
   - Integration: Must handle ct_slave:stop errors gracefully

### Verification

**Automated verification commands:**

```bash
# 1. Verify EPMD is running
epmd -names

# 2. Run CT suite
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE

# 3. Verify all 9 tests pass (2 single_node + 7 multi_node)
grep -E "PASSED|FAILED" _build/test/logs/erlmcp_registry_dist_SUITE/index.html

# 4. Check compilation
TERM=dumb rebar3 compile

# 5. Check Dialyzer
rebar3 dialyzer

# 6. Check Xref
rebar3 xref
```

**Expected results:**
- All 9 test cases: PASSED
- Compilation: 0 errors, 0 warnings
- Dialyzer: 0 warnings
- Xref: 0 undefined function calls
- Slave node startup time: <30s (typically), max 120s
- Cluster connectivity: 3/3 nodes connected

### Manufacturing Output

- **Code**: Modified `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl` (add ~40 lines of health checks)
- **Tests**: Existing CT suite now passes (9/9 tests)
- **Documentation**: Updated inline comments explaining health check strategy
- **Receipts**: CT logs showing 100% pass rate, compilation receipts

## What We're NOT Doing

**Explicitly OUT OF SCOPE to prevent scope creep:**

1. **Migrate from ct_slave to peer module** - Reason: ct_slave is deprecated but functional in OTP 25-28, migration is P3 (low priority, separate issue)
2. **Add new test cases** - Reason: Focus is on fixing existing test infrastructure, not expanding test coverage
3. **Refactor distributed registry logic** - Reason: Registry code is production-ready, problem is only in test setup
4. **Add mock/stub support** - Reason: Chicago School TDD forbids mocks, we use real distributed nodes
5. **Performance optimization of node startup** - Reason: Current 120s timeout is sufficient, optimization is premature
6. **CI/CD pipeline changes** - Reason: Focus is local test execution, CI integration is separate concern
7. **Documentation updates** - Reason: Test infrastructure changes don't require user-facing docs

## Manufacturing Approach

### TCPS Methodology

Following Toyota Code Production System phases:

1. **Specification** - Requirements with acceptance criteria (EPMD running, nodes pingable, 9/9 tests passing)
2. **Pseudocode** - Algorithm design BEFORE coding (health check sequence, retry logic, timeout strategy)
3. **Architecture** - Integration points and dependencies (net_kernel, EPMD, ct_slave, gproc)
4. **Refinement** - Chicago School TDD (tests FIRST - verify init_per_suite, add health checks, run CT suite)
5. **Completion** - All quality gates passing (9/9 tests, 100% pass rate, 0 errors/warnings)

### Implementation Strategy

**High-level approach:**

**Strategy**: Add layered health checks (Jidoka - stop the line at each failure point)
- **Layer 1**: EPMD verification (infrastructure check)
- **Layer 2**: net_kernel startup (distributed Erlang check)
- **Layer 3**: Slave node boot (node initialization check)
- **Layer 4**: Cluster connectivity (network check)

**Why this strategy:**
- Matches TCPS principle of "quality at the source" - catch problems early
- Each layer has explicit failure mode with clear error message
- No implicit dependencies - each check is visible and measurable
- Supports both fast local execution (ideal case) and slow CI environments (worst case)

**Poka-yoke integration:**
- Health checks cannot be skipped (they're in init_per_suite)
- Timeouts are configurable but have safe defaults
- Node naming eliminates DNS issues (uses 127.0.0.1 explicitly)
- Ping verification prevents silent failures

**Heijunka (production leveling):**
- Break into 3 phases (EPMD check, slave startup, connectivity)
- Each phase independently verifiable
- No big-bang changes - can stop after any phase

### Quality Integration

- **Pre-commit Hooks**: `.claude/hooks/pre-task-validate.sh` - runs compilation and basic syntax checks
- **CI Gates**: GitHub Actions will run full CT suite with 100% pass requirement
- **Receipt Generation**: CT logs saved to `_build/test/logs/erlmcp_registry_dist_SUITE/`
- **Andon Signaling**:
  - EPMD failure: `ct:fail("EPMD not running: ~p~nStart EPMD with: epmd -daemon", [Reason])`
  - Node boot failure: `ct:fail("Slave node ~p failed to boot: ~p", [Node, Reason])`
  - Connectivity failure: `ct:fail("Expected 3 connected nodes, got ~p: ~p", [Count, Nodes])`

---

## Phases

### Phase 1: Add EPMD Health Verification (Jidoka - Stop the Line)

#### Overview

Add EPMD health verification in `init_per_suite/1` to fail fast if EPMD is not running. This prevents cryptic timeout errors later in slave node startup. Implements Jidoka principle: stop production immediately when infrastructure is missing.

**Time estimate**: 2 hours
**Risk level**: Low (additive change, doesn't break existing tests)

#### Specification

**WHAT we're building:**

1. New helper function `epmd_health_check() -> ok | {error, Reason}`
2. Modified `init_per_suite/1` to call EPMD check before net_kernel:start
3. Clear error messages guiding user to start EPMD if not running

**File**: `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl`

#### Pseudocode

**HOW it will work:**

```
FUNCTION epmd_health_check()
  FOR attempt IN 1..10
    RUN "epmd -names" command
    IF output contains "epmd: up and running"
      RETURN ok
    ELSE IF output contains "epmd: Cannot connect to"
      WAIT 100ms (retry)
    ELSE
      RETURN {error, {unexpected_output, Output}}
  END FOR
  RETURN {error, epmd_not_responding}
END FUNCTION

FUNCTION init_per_suite(Config)
  CALL epmd_health_check()
  IF error
    ct:fail("EPMD not running: ~p~nStart with: epmd -daemon", [Reason])

  CALL net_kernel:start([erlmcp_test@localhost, shortnames])
  SET cookie erlmcp_test_cookie
  START gproc application

  RETURN Config
END FUNCTION
```

#### Architecture

**INTEGRATION points:**

- **Dependencies**: `os:cmd/1` for EPMD check, `net_kernel:start/1` for distributed Erlang
- **Process structure**: None (test setup runs in CT master process)
- **Supervision tree**: None (test infrastructure, not production code)
- **Error handling**: Explicit `ct:fail/2` stops test suite with clear message

**Call sequence:**
```
CT Master -> init_per_suite/1
         -> epmd_health_check() (NEW)
         -> net_kernel:start/1 (EXISTING)
         -> erlang:set_cookie/2 (EXISTING)
         -> application:ensure_all_started(gproc) (EXISTING)
```

#### Changes Required:

##### 1. EPMD Health Check Function

**File**: `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl`
**Current**: Does not exist
**Changes**: Add new helper function at end of module (after line 361)
**Reason**: Verify EPMD infrastructure before attempting distributed operations

```erlang
%% BEFORE (does not exist)
%% [No EPMD health check]

%% AFTER (proposed code)
%% @doc Verify EPMD is running and responding on port 4369
%% Implements Jidoka: fail fast if infrastructure is missing
-spec epmd_health_check() -> ok | {error, term()}.
epmd_health_check() ->
    %% Retry up to 10 times (1 second total) to handle EPMD startup race
    lists:foreach(fun(_N) ->
        case os:cmd("epmd -names") of
            [] ->
                %% EPMD not responding
                case _N of
                    10 ->
                        %% Last attempt failed
                        {error, epmd_not_responding};
                    _ ->
                        %% Retry after delay
                        timer:sleep(100),
                        continue
                end;
            Output ->
                case string:find(Output, "epmd: up and running") of
                    nomatch ->
                        %% Unexpected output
                        {error, {epmd_unexpected_output, Output}};
                    _ ->
                        %% EPMD is healthy
                        ok
                end
        end
    end, lists:seq(1, 10)),
    ok.
```

**Correction**: The above pseudocode has a logic error. Let me fix it:

```erlang
%% CORRECTED version
%% @doc Verify EPMD is running and responding on port 4369
%% Implements Jidoka: fail fast if infrastructure is missing
%% @doc Verify EPMD is running and responding on port 4369
-spec epmd_health_check() -> ok | {error, term()}.
epmd_health_check() ->
    epmd_health_check(10).

epmd_health_check(0) ->
    {error, epmd_not_responding};
epmd_health_check(Attempts) ->
    case os:cmd("epmd -names") of
        [] ->
            timer:sleep(100),
            epmd_health_check(Attempts - 1);
        Output ->
            case string:find(Output, "epmd: up and running") of
                nomatch ->
                    {error, {epmd_unexpected_output, Output}};
                _ ->
                    ok
            end
    end.
```

##### 2. Enhanced init_per_suite/1

**File**: `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl`
**Current**: Lines 60-75 (no EPMD check)
**Changes**: Add EPMD health check as first step
**Reason**: Fail fast if distributed infrastructure is not available

```erlang
%% BEFORE (existing code at lines 60-75)
init_per_suite(Config) ->
    %% Start distributed Erlang if not already started
    case net_kernel:start([erlmcp_test@localhost, shortnames]) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        {error, Reason} ->
            ct:fail("Failed to start distributed Erlang: ~p", [Reason])
    end,

    %% Set cookie
    erlang:set_cookie(node(), erlmcp_test_cookie),

    %% Start gproc
    {ok, _Apps} = application:ensure_all_started(gproc),

    Config.

%% AFTER (proposed code)
init_per_suite(Config) ->
    %% 1. Verify EPMD is running (Jidoka: fail fast if infrastructure missing)
    case epmd_health_check() of
        ok ->
            ct:log("EPMD health check passed");
        {error, Reason} ->
            ct:fail("EPMD not running: ~p~nStart EPMD with: epmd -daemon", [Reason])
    end,

    %% 2. Start distributed Erlang if not already started
    case net_kernel:start([erlmcp_test@localhost, shortnames]) of
        {ok, _Pid} ->
            ct:log("Distributed Erlang started: ~p", [node()]);
        {error, {already_started, _Pid}} ->
            ct:log("Distributed Erlang already started: ~p", [node()]);
        {error, Reason} ->
            ct:fail("Failed to start distributed Erlang: ~p", [Reason])
    end,

    %% 3. Set cookie
    erlang:set_cookie(node(), erlmcp_test_cookie),
    ct:log("Cluster cookie set: ~p", [erlmcp_test_cookie]),

    %% 4. Start gproc
    {ok, _Apps} = application:ensure_all_started(gproc),
    ct:log("Gproc application started"),

    Config.
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **Common Test - single_node group**: `rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE --group=single_node` - 2/2 tests passing
- [ ] **EPMD Health Check**: Manual test - stop EPMD (`epmd -kill`), run test, verify clear error message
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Code review: EPMD check follows OTP error handling patterns
- [ ] Integration: Works with existing net_kernel startup
- [ ] Error messages: Clear and actionable (user knows how to fix)
- [ ] Retry logic: 10 attempts × 100ms = 1s total timeout is reasonable

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is Jidoka (built-in quality).

---

### Phase 2: Fix Slave Node Startup (Poka-yoke - Mistake-Proofing)

#### Overview

Fix slave node startup to use explicit localhost addressing, increase timeouts for slow systems, and add startup logging. Eliminates DNS resolution delays and boot timeout failures. Implements Poka-yoke: make it impossible to have DNS-related node startup failures.

**Time estimate**: 2 hours
**Risk level**: Medium (changes node naming scheme, must verify compatibility)

#### Specification

**WHAT we're building:**

1. Modified `start_slave_nodes/3` to use explicit `127.0.0.1` hostname
2. Increased boot_timeout from 10s to 120s
3. Added startup progress logging
4. Support for configurable timeout via `ERLMCP_CT_BOOT_TIMEOUT` environment variable

**File**: `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl`

#### Pseudocode

**HOW it will work:**

```
FUNCTION start_slave_nodes(Count, Config)
  BootTimeout = GET_ENV("ERLMCP_CT_BOOT_TIMEOUT", 120)
  RETURN start_slave_nodes(Count, 1, [], BootTimeout)

FUNCTION start_slave_nodes(0, _N, Acc, _Timeout)
  RETURN lists:reverse(Acc)

FUNCTION start_slave_nodes(Count, N, Acc, BootTimeout)
  %% Use localhost short names (Poka-yoke: eliminate DNS issues)
  NodeName = list_to_atom("slave" ++ integer_to_list(N) ++ "@127.0.0.1")

  ct:log("Starting slave node ~p (timeout ~pms)", [NodeName, BootTimeout * 1000])

  %% Start with increased timeout (Heijunka: proper pacing)
  {ok, Node} = ct_slave:start(NodeName, [
    {boot_timeout, BootTimeout},
    {init_timeout, 30},
    {startup_timeout, 30},
    {monitor_master, true},
    {kill_if_fail, true},
    {erl_flags, "-setcookie erlmcp_test_cookie"}
  ])

  %% Verify connectivity (Andon: visible problem signaling)
  CASE net_adm:ping(Node) OF
    pong ->
      ct:log("Slave node ~p started and connected", [Node]),
      start_slave_nodes(Count - 1, N + 1, [Node | Acc], BootTimeout)
    pang ->
      ct:fail("Slave node ~p started but not responding to ping", [Node])
  END
END FUNCTION
```

#### Architecture

**INTEGRATION points:**

- **Dependencies**: `ct_slave:start/2` for node creation, `net_adm:ping/1` for connectivity
- **Process structure**: Creates 2 slave Erlang nodes (ct_slave manages their lifecycles)
- **Supervision tree**: ct_slave monitors slave nodes, kills them if they fail
- **Error handling**: Explicit `ct:fail/2` on ping failure (no silent failures)

**Call sequence:**
```
init_per_group(multi_node, Config)
  -> start_slave_nodes(2, Config) (MODIFIED)
     -> ct_slave:start(NodeName, Options) FOR each slave
     -> net_adm:ping(Node) (EXISTING)
  -> Return [{slave_nodes, [Node1, Node2]} | Config]
```

#### Changes Required:

##### 1. Fixed start_slave_nodes/3

**File**: `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl`
**Current**: Lines 334-353
**Changes**: Change node naming, increase timeouts, add logging
**Reason**: Eliminate DNS delays, handle slow systems, improve visibility

```erlang
%% BEFORE (existing code at lines 334-353)
start_slave_nodes(Count, _Config) ->
    start_slave_nodes(Count, 1, []).

start_slave_nodes(0, _N, Acc) ->
    lists:reverse(Acc);
start_slave_nodes(Count, N, Acc) ->
    NodeName = list_to_atom("slave" ++ integer_to_list(N)),
    {ok, Node} = ct_slave:start(NodeName, [
        {boot_timeout, 10},
        {init_timeout, 10},
        {startup_timeout, 10},
        {monitor_master, true},
        {kill_if_fail, true},
        {erl_flags, "-setcookie erlmcp_test_cookie"}
    ]),

    %% Ensure node is connected
    pong = net_adm:ping(Node),

    start_slave_nodes(Count - 1, N + 1, [Node | Acc]).

%% AFTER (proposed code)
start_slave_nodes(Count, _Config) ->
    BootTimeout = case os:getenv("ERLMCP_CT_BOOT_TIMEOUT") of
        false -> 120;  % Default 120 seconds
        Val -> list_to_integer(Val)
    end,
    start_slave_nodes(Count, 1, [], BootTimeout).

start_slave_nodes(0, _N, Acc, _Timeout) ->
    lists:reverse(Acc);
start_slave_nodes(Count, N, Acc, BootTimeout) ->
    %% Use localhost short names (Poka-yoke: eliminate DNS issues)
    NodeName = list_to_atom("slave" ++ integer_to_list(N) ++ "@127.0.0.1"),

    ct:log("Starting slave node ~p (boot_timeout=~ps)", [NodeName, BootTimeout]),

    %% Start with increased timeout (Heijunka: proper pacing)
    case ct_slave:start(NodeName, [
        {boot_timeout, BootTimeout},        % Increased from 10s
        {init_timeout, 30},                 % Increased from 10s
        {startup_timeout, 30},              % Increased from 10s
        {monitor_master, true},
        {kill_if_fail, true},
        {erl_flags, "-setcookie erlmcp_test_cookie"}
    ]) of
        {ok, Node} ->
            %% Verify connectivity (Andon: visible problem signaling)
            case net_adm:ping(Node) of
                pong ->
                    ct:log("Slave node ~p started and connected", [Node]),
                    start_slave_nodes(Count - 1, N + 1, [Node | Acc], BootTimeout);
                pang ->
                    ct:fail("Slave node ~p started but not responding to ping", [Node])
            end;
        {error, Reason} ->
            ct:fail("Failed to start slave node ~p: ~p", [NodeName, Reason])
    end.
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **Common Test - single_node group**: `rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE --group=single_node` - 2/2 tests passing
- [ ] **Common Test - multi_node group init**: `rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE --group=multi_node` - init_per_group passes (slave nodes start successfully)
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls

##### Manual Verification:
- [ ] Slave node startup time: <30s on typical macOS/Linux system
- [ ] Node names: Verify `slave1@127.0.0.1` format in logs (no FQDN)
- [ ] Timeout flexibility: Test with `ERLMCP_CT_BOOT_TIMEOUT=60` environment variable
- [ ] Error messages: Clear indication of which node failed and why

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix.

---

### Phase 3: Add Cluster Connectivity Verification (Andon - Visible Problem Signaling)

#### Overview

Add cluster connectivity verification in `init_per_group(multi_node, 2)` and improve teardown in `end_per_group(multi_node, 2)` to ensure all nodes are properly connected before tests run and gracefully cleaned up after. Implements Andon: make cluster status visible at all times.

**Time estimate**: 1 hour
**Risk level**: Low (additive verification, improves robustness)

#### Specification

**WHAT we're building:**

1. Enhanced `init_per_group(multi_node, Config)` to verify 3 nodes connected
2. Improved `end_per_group(multi_node, Config)` with error handling and logging
3. Cluster state logging for debugging

**File**: `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl`

#### Pseudocode

**HOW it will work:**

```
FUNCTION init_per_group(multi_node, Config)
  %% Multi-node tests need cluster enabled
  SET_ENV(erlmcp_core, cluster_enabled, true)
  SET_ENV(erlmcp_core, cluster_cookie, erlmcp_test_cookie)

  %% Start slave nodes with health checks
  Nodes = start_slave_nodes(2, Config)

  %% Verify all nodes are connected (Kaizen: measure everything)
  ConnectedNodes = [node() | nodes()]
  ct:log("Connected cluster nodes: ~p", [ConnectedNodes])

  IF length(ConnectedNodes) != 3 THEN
    ct:fail("Expected 3 connected nodes, got ~p: ~p", [length(ConnectedNodes), ConnectedNodes])
  END IF

  RETURN [{slave_nodes, Nodes} | Config]
END FUNCTION

FUNCTION end_per_group(multi_node, Config)
  %% Stop slave nodes gracefully
  Nodes = ?config(slave_nodes, Config)

  lists:foreach(fun(Node) ->
    case ct_slave:stop(Node) of
      {error, Reason} ->
        ct:log("Warning: Failed to stop slave node ~p: ~p", [Node, Reason]);
      ok ->
        ct:log("Stopped slave node ~p", [Node])
    end
  end, Nodes),

  ok
END FUNCTION
```

#### Architecture

**INTEGRATION points:**

- **Dependencies**: `nodes()` for cluster list, `ct_slave:stop/1` for cleanup
- **Process structure**: Verifies 3-node cluster (1 master + 2 slaves)
- **Supervision tree**: None (test setup/teardown)
- **Error handling**: Explicit failure in init_per_group, graceful degradation in end_per_group

**Call sequence:**
```
CT Master -> init_per_group(multi_node, Config) (MODIFIED)
         -> start_slave_nodes(2, Config) (from Phase 2)
         -> nodes() to get connected nodes
         -> Verify count == 3
         -> Return [{slave_nodes, Nodes} | Config]

[Run all multi_node tests]

CT Master -> end_per_group(multi_node, Config) (MODIFIED)
         -> ct_slave:stop(Node) for each slave
         -> Log success/failure
         -> Return ok
```

#### Changes Required:

##### 1. Enhanced init_per_group/2 for multi_node

**File**: `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl`
**Current**: Lines 87-94
**Changes**: Add cluster connectivity verification
**Reason**: Ensure all nodes are connected before tests run (fail fast)

```erlang
%% BEFORE (existing code at lines 87-94)
init_per_group(multi_node, Config) ->
    %% Multi-node tests need cluster enabled
    application:set_env(erlmcp_core, cluster_enabled, true),
    application:set_env(erlmcp_core, cluster_cookie, erlmcp_test_cookie),

    %% Start slave nodes
    Nodes = start_slave_nodes(2, Config),
    [{slave_nodes, Nodes} | Config].

%% AFTER (proposed code)
init_per_group(multi_node, Config) ->
    %% Multi-node tests need cluster enabled
    application:set_env(erlmcp_core, cluster_enabled, true),
    application:set_env(erlmcp_core, cluster_cookie, erlmcp_test_cookie),
    ct:log("Multi-node cluster configuration: enabled=true, cookie=erlmcp_test_cookie"),

    %% Start slave nodes with health checks
    Nodes = start_slave_nodes(2, Config),
    ct:log("Slave nodes started: ~p", [Nodes]),

    %% Verify all nodes are connected (Kaizen: measure everything)
    ConnectedNodes = [node() | nodes()],
    ct:log("Connected cluster nodes: ~p", [ConnectedNodes]),

    ExpectedCount = 1 + length(Nodes),  % Master + slaves
    case length(ConnectedNodes) of
        ExpectedCount ->
            ct:log("Cluster connectivity verified: ~p nodes connected", [ExpectedCount]);
        ActualCount ->
            ct:fail("Expected ~p connected nodes, got ~p: ~p",
                    [ExpectedCount, ActualCount, ConnectedNodes])
    end,

    [{slave_nodes, Nodes} | Config].
```

##### 2. Improved end_per_group/2 for multi_node

**File**: `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl`
**Current**: Lines 99-105
**Changes**: Add error handling and logging
**Reason**: Graceful cleanup with visibility into shutdown process

```erlang
%% BEFORE (existing code at lines 99-105)
end_per_group(multi_node, Config) ->
    %% Stop slave nodes
    Nodes = ?config(slave_nodes, Config),
    lists:foreach(fun(Node) ->
        ct_slave:stop(Node)
    end, Nodes),
    ok.

%% AFTER (proposed code)
end_per_group(multi_node, Config) ->
    %% Stop slave nodes gracefully
    Nodes = ?config(slave_nodes, Config),
    ct:log("Stopping ~p slave nodes: ~p", [length(Nodes), Nodes]),

    lists:foreach(fun(Node) ->
        case ct_slave:stop(Node) of
            {error, Reason} ->
                ct:log("Warning: Failed to stop slave node ~p: ~p", [Node, Reason]);
            ok ->
                ct:log("Stopped slave node ~p", [Node])
        end
    end, Nodes),

    ct:log("Multi-node group teardown complete"),
    ok.
```

#### Success Criteria:

##### Automated Verification (MUST ALL PASS):
- [ ] **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
- [ ] **Common Test - full suite**: `rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE` - **ALL 9/9 TESTS PASSING**
  - 2 single_node tests: PASSED
  - 7 multi_node tests: PASSED
- [ ] **Dialyzer**: `rebar3 dialyzer` - 0 warnings
- [ ] **Xref**: `rebar3 xref` - 0 undefined function calls
- [ ] **Coverage**: `rebar3 cover` - ≥80% for modified functions

##### Manual Verification:
- [ ] CT logs show "Cluster connectivity verified: 3 nodes connected"
- [ ] Slave nodes start within 120s timeout
- [ ] All 7 multi_node test cases execute (no cascading failures from init_per_group)
- [ ] Teardown logs show successful slave shutdown
- [ ] No zombie processes after test completion

**Verification Commands:**

```bash
# Full verification sequence
TERM=dumb rebar3 compile && \
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE && \
grep "PASSED\|FAILED" _build/test/logs/erlmcp_registry_dist_SUITE/index.html && \
echo "Expected: 9 tests, 0 skipped, 0 failed"
```

**Expected output:**
```
PASSED: 9
FAILED: 0
SKIPPED: 0
```

**Note**: Complete ALL automated verification BEFORE marking phase done. If ANY gate fails, STOP and fix. This is the FINAL phase - all quality gates MUST pass.

---

## Testing Strategy

### Chicago School TDD (MANDATORY)

- **NO MOCKS** - Use real distributed Erlang nodes, real EPMD, real ct_slave
- **State-Based Verification** - Check cluster connectivity (nodes()), verify EPMD status
- **Integration Testing** - Test with real net_kernel, real gproc, real process communication
- **Race Condition Testing** - EPMD startup race (handled by retry loop)

### Unit Tests (EUnit)

**Not applicable** - This is test infrastructure code, tested by Common Test itself.

### Integration Tests (Common Test)

**End-to-End Scenarios:**

1. **EPMD not running** (Phase 1)
   - Stop EPMD: `epmd -kill`
   - Run CT suite
   - Verify: Clear error message "EPMD not running"
   - Start EPMD: `epmd -daemon`
   - Verify: Tests proceed

2. **Slow system boot** (Phase 2)
   - Run on heavily loaded system or CI
   - Verify: Slave nodes start within 120s timeout
   - Verify: All 9 tests pass

3. **DNS resolution failure** (Phase 2)
   - Test with explicit `127.0.0.1` addressing
   - Verify: No DNS queries, no hostname resolution delays
   - Verify: Nodes connect immediately after boot

4. **Cluster connectivity** (Phase 3)
   - Run full multi_node test suite
   - Verify: All 3 nodes connected before tests execute
   - Verify: Graceful teardown after tests complete

5. **Environment variable override** (Phase 2)
   - Set `ERLMCP_CT_BOOT_TIMEOUT=60`
   - Verify: 60s timeout used instead of 120s default

### Manual Testing Steps

1. **Verify EPMD health check**:
   ```bash
   epmd -kill
   rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE --group=single_node
   # Should fail with "EPMD not running"
   epmd -daemon
   rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE --group=single_node
   # Should pass (2/2 tests)
   ```

2. **Verify slave node startup**:
   ```bash
   rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE --group=multi_node
   # Check logs for "Slave node slave1@127.0.0.1 started and connected"
   # Verify all 7 multi_node tests pass
   ```

3. **Verify full suite**:
   ```bash
   rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE
   # Verify 9/9 tests pass (2 single_node + 7 multi_node)
   ```

### Quality Gates

Every phase MUST pass:

1. **Compilation**: `TERM=dumb rebar3 compile` - 0 errors, 0 warnings
2. **Common Test**: `rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE` - 100% pass rate (9/9)
3. **Coverage**: `rebar3 cover` - ≥80% for modified functions
4. **Dialyzer**: `rebar3 dialyzer` - 0 warnings
5. **Xref**: `rebar3 xref` - 0 undefined function calls

## Manufacturing Checklist

### Before Implementation
- [x] Root cause identified (not symptoms) - ✅ Missing EPMD verification, 10s timeout too short, hostname naming issues
- [x] Quality gates defined (specific thresholds) - ✅ 100% CT pass rate (9/9 tests), EPMD running, nodes pingable
- [x] OTP patterns understood (behaviors, supervision) - ✅ gen_server, supervisor, Common Test groups
- [x] Test strategy clear (Chicago School TDD) - ✅ Real distributed infrastructure, no mocks
- [x] Risk assessment complete (severity P0-P3) - ✅ 5 risks with mitigations
- [x] No open questions (all research complete) - ✅ All dependencies, constraints, and success criteria documented
- [x] Phases broken down (≤4 hours each) - ✅ Phase 1: 2h, Phase 2: 2h, Phase 3: 1h (total 5h)
- [x] Acceptance criteria defined (measurable, specific) - ✅ Each phase has automated verification commands

### During Implementation
- [ ] Chicago School TDD followed (tests FIRST) - Run CT suite after each phase
- [ ] OTP patterns followed (gen_server, supervisor) - N/A (test infrastructure)
- [ ] Type specs added (Dialyzer clean) - Add `-spec` for epmd_health_check/0
- [ ] Error handling complete (all paths) - ct:fail for infrastructure failures, ct:log for warnings
- [ ] Quality gates passing (compilation, tests, coverage) - Verify after each phase

### After Implementation
- [ ] All tests passing (100% rate) - 9/9 tests in CT suite
- [ ] Coverage ≥80% (verified) - Modified helper functions covered
- [ ] Dialyzer 0 warnings (verified) - Add type specs to new functions
- [ ] Xref 0 undefined calls (verified) - No new dependencies
- [ ] Performance no regression >10% (verified) - Slave startup <120s (was timeout before)
- [ ] Documentation updated (inline comments) - Add Jidoka/Poka-yoke annotations
- [ ] Code review complete (OTP patterns verified) - Verify error handling matches CT conventions

## Risk Management

### Known Risks

| Risk | Severity | Probability | Mitigation |
|------|----------|-------------|------------|
| EPMD not running (port 4369 blocked) | P0 (Critical) | High | Add EPMD health check in init_per_suite, fail with clear message "EPMD not running - start with 'epmd -daemon'" |
| Hostname resolution delays (DNS issues) | P1 (High) | Medium | Use localhost short names (127.0.0.1) instead of FQDN, explicit addressing eliminates DNS |
| Node startup too slow on CI | P2 (Medium) | Medium | Increase boot_timeout to 120s, support configurable timeout via ERLMCP_CT_BOOT_TIMEOUT |
| Cookie mismatch (nodes can't communicate) | P1 (High) | Low | Verify cookie set before slave startup, log cookie value, add net_adm:ping verification |
| Race condition - EPMD not ready | P2 (Medium) | Low | Add retry loop for EPMD verification (10 attempts × 100ms = 1s total) |
| ct_slave deprecation (OTP 29) | P3 (Low) | Low | Document migration path to peer module, add TODO comment (not in scope for this fix) |
| Firewall blocking 4369 | P2 (Medium) | Low | Error message guides user to check firewall, CT logs show EPMD connection status |

### Rollback Plan

**How to rollback if something goes wrong:**

- **Git revert**: Revert to commit before changes (record starting commit hash)
- **Data migration**: N/A (test infrastructure, no persistent data)
- **Service impact**: N/A (test suite only, no production impact)
- **Backward compatibility**: Changes are additive (EPMD check) or improve robustness (timeouts), no breaking changes

**Rollback verification:**
```bash
# If changes break tests, rollback:
git diff HEAD~1 apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl
git checkout HEAD~1 -- apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_registry_dist_SUITE
# Should restore original state (failing tests)
```

## References

- Research: `/Users/sac/erlmcp/.wreckit/items/016-fix-erlmcpregistrydistsuite-distributed-node-initi/research.md`
- CLAUDE.md: `/Users/sac/erlmcp/CLAUDE.md` (project rules)
- TCPS: `/Users/sac/erlmcp/docs/tcps/TCPS.md` (manufacturing principles)
- OTP Patterns: `/Users/sac/erlmcp/docs/otp-patterns.md`
- Test Reference:
  - `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl:60-75` (init_per_suite)
  - `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl:87-94` (init_per_group multi_node)
  - `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl:334-353` (start_slave_nodes)
  - `apps/erlmcp_core/test/erlmcp_registry_dist_SUITE.erl:99-105` (end_per_group multi_node)
  - `test/erlmcp_registry_distributed_tests.erl:1-436` (Chicago School TDD reference)

## Appendix: Test Case Inventory

### Single Node Group (2 test cases) - Currently PASSING
1. `single_node_disabled` - Verify distributed features disabled when cluster_enabled=false
2. `single_node_enabled` - Verify global registration works on single node

### Multi Node Group (7 test cases) - Currently FAILING (cascading from init_per_group failure)
1. `multi_node_registration` - Register on Node1, find from Node2
2. `multi_node_failover` - Process death cleanup, re-register on Node2
3. `split_brain_detection` - Detect network partition, resolve with strategy
4. `global_name_conflict` - Duplicate registration should fail
5. `node_reconnection` - Disconnect/reconnect node monitoring

**Expected After Fix**: All 9 test cases passing (100% pass rate)

## Sign-Off

**Manufacturing Plan Status**: ✅ COMPLETE

**Pre-Implementation Verification:**
- [x] Root cause analysis complete (5 Whys performed)
- [x] Quality gates defined (all 5 gates with specific thresholds)
- [x] Phases broken down (3 phases, ≤4 hours each, total 5h)
- [x] Acceptance criteria measurable (specific commands, pass/fail criteria)
- [x] No open questions (all research complete, all code verified)
- [x] Risk assessment complete (7 risks, all with mitigations)
- [x] TCPS principles applied (Jidoka, Poka-yoke, Heijunka, Andon, Kaizen)

**Ready for PRD creation**: YES
**Ready for implementation**: YES (awaiting PRD user story creation)
