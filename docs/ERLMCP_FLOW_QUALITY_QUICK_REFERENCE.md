# erlmcp-flow Quality Standards - Quick Reference

**Version:** 1.0.0 | **Date:** 2026-02-02 | **1-Page Cheat Sheet**

---

## 🚦 12 Quality Gates (All MUST Pass)

| # | Gate | Target | Command | Time |
|---|------|--------|---------|------|
| 1 | **Compilation** | 0 errors | `cd apps/erlmcp_flow && rebar3 compile` | 10s |
| 2 | **Xref** | 0 undefined | `rebar3 xref` | 15s |
| 3 | **Dialyzer** | 0 warnings | `rebar3 dialyzer` | 2min |
| 4 | **EUnit** | 0 failures | `rebar3 eunit --app erlmcp_flow` | 30s |
| 5 | **Common Test** | 0 failures | `rebar3 ct --dir test` | 1min |
| 6 | **Coverage** | ≥80% (≥85% core) | `rebar3 cover --verbose` | 30s |
| 7 | **Benchmarks** | All targets met | `rebar3 eunit --module erlmcp_flow_bench` | 2min |
| 8 | **Chaos** | 0% task loss | `rebar3 ct --suite erlmcp_flow_chaos_SUITE` | 1min |
| 9 | **Chicago TDD** | No violations | `./.github/scripts/chicago-tdd-scan-flow.sh` | 5s |
| 10 | **OTP** | No violations | `./.github/scripts/otp-compliance-scan-flow.sh` | 5s |
| 11 | **Format** | Formatted | `rebar3 format --verify` | 5s |
| 12 | **Docs** | 100% specs | `./.github/scripts/doc-coverage-scan-flow.sh` | 5s |

**Run All:** `./scripts/erlmcp-flow-quality-check.sh --full` (5-10 min)

---

## 📋 Code Review Checklist (Pre-Approval)

### Module Structure
- [ ] Module named `erlmcp_flow_*`
- [ ] Functions use `verb_noun` pattern (e.g., `register_agent/3`, `route_message/2`)
- [ ] Location: `apps/erlmcp_flow/src/*.erl`

### Routing Layer
- [ ] All routing through `erlmcp_flow_router`
- [ ] gproc used for O(log N) registry
- [ ] Correlation IDs tracked
- [ ] Retry + fallback + circuit breaker

### OTP Compliance
- [ ] All 6 gen_server callbacks exported
- [ ] init/1 never blocks (use `{continue, ...}`)
- [ ] All processes supervised
- [ ] Timeouts ≥ 5000ms

### Q-Learning
- [ ] Q-Learning used for agent selection
- [ ] Epsilon-greedy exploration (ε=0.1)
- [ ] Q-values updated after task completion
- [ ] Reward function considers latency + success

### Performance
- [ ] Registry lookup p99 < 100μs
- [ ] Routing decision p99 < 50ms
- [ ] Throughput > 50K routes/sec
- [ ] Memory < 512MB for 1000 agents

### Testing
- [ ] Chicago TDD (real processes, no mocks)
- [ ] Coverage ≥ 85% for core modules
- [ ] Property-based tests for invariants
- [ ] Chaos tests verify reliability

---

## ⛔ Anti-Patterns (BLOCKING)

```erlang
% ❌ VIOLATION: Mock usage
meck:new(erlmcp_flow_registry)

% ❌ VIOLATION: State inspection
sys:get_status(RouterPid)

% ❌ VIOLATION: Direct agent messaging (bypasses routing)
AgentPid ! {task, Task}

% ❌ VIOLATION: Unsupervised spawn
spawn(fun() -> worker_loop() end)

% ❌ VIOLATION: Blocking init/1
init(Opts) ->
    {ok, Conn} = connect_to_database(Opts),  % Blocks!
    {ok, #state{conn = Conn}}.

% ❌ VIOLATION: No correlation tracking
route_task(Task, AgentId) ->
    AgentId ! {task, Task}.  % Lost distributed context

% ✅ CORRECT: Async init, routing through router
init(Opts) ->
    self() ! initialize_async,
    {ok, #state{opts = Opts}}.

erlmcp_flow_router:route_task(Task, #{correlation_id => UUID}).
```

---

## 🔧 Quick Commands

### Fast Checks (Pre-Commit, 30-60s)
```bash
./scripts/erlmcp-flow-quality-check.sh --fast
# Gates: 1 (compile), 4 (eunit), 9 (chicago), 11 (format)
```

### Full Validation (Pre-Push, 5-10min)
```bash
./scripts/erlmcp-flow-quality-check.sh --full
# All 12 gates
```

### Specific Gates
```bash
./scripts/erlmcp-flow-quality-check.sh --gates=1,2,3,4
```

### Install Hooks
```bash
./scripts/install-erlmcp-flow-hooks.sh
```

---

## 🎯 Performance Targets

| Metric | Target | Measured |
|--------|--------|----------|
| Registry lookup (p99) | < 100μs | Benchmark |
| Routing decision (p99) | < 50ms | Benchmark |
| Message throughput | > 500K msg/s | Benchmark |
| Task throughput | > 50K tasks/s | Benchmark |
| Consensus latency (p99) | < 100ms | Benchmark |
| Agent failover (p99) | < 100ms | Benchmark |
| Total memory (1000 agents) | < 512MB | Benchmark |
| Zero task loss | 100% | Chaos test |

---

## 📝 Naming Conventions

### Modules
```erlang
erlmcp_flow_registry    % Registry with gproc
erlmcp_flow_router      % Routing logic
erlmcp_flow_q_learning  % Q-Learning
erlmcp_flow_agent       % Agent gen_server
erlmcp_flow_*_bridge    % Transport bridges
```

### Functions
```erlang
register_agent/3        % verb_noun
lookup_agent/1
route_message/2
balance_load/2
detect_failure/1

% Boolean predicates
is_agent_healthy/1
has_capability/2
can_handle_task/2

% Getters/setters
get_agent_load/1
set_routing_policy/2
```

---

## 🧪 Testing Patterns

### Chicago TDD
```erlang
% ✅ CORRECT: Real processes
test_routing() ->
    {ok, Agent1} = erlmcp_flow_agent_sup:start_child([agent1, #{}]),
    {ok, Agent2} = erlmcp_flow_agent_sup:start_child([agent2, #{}]),

    Task = #{task_id => <<"task1">>, method => <<"execute">>},
    {ok, AgentId} = erlmcp_flow_router:route_task(Task),

    ?assert(lists:member(AgentId, [agent1, agent2])),

    erlmcp_flow_agent:stop(Agent1),
    erlmcp_flow_agent:stop(Agent2).
```

### Property-Based (PropEr)
```erlang
prop_zero_task_loss() ->
    ?FORALL({NumAgents, NumTasks}, {range(1, 100), range(100, 10000)},
        begin
            Agents = setup_agents(NumAgents),
            Results = route_tasks(NumTasks),
            Completed = length([R || {ok, _} <- Results]),
            cleanup_agents(Agents),
            Completed =:= NumTasks  % Zero loss
        end).
```

---

## 🔗 Key Documents

1. **[Code Review Checklist](./ERLMCP_FLOW_CODE_REVIEW_CHECKLIST.md)** - Detailed review criteria
2. **[Quality Gates](./ERLMCP_FLOW_QUALITY_GATES.md)** - 12 gates with enforcement
3. **[OTP Compliance](./ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md)** - OTP patterns
4. **[Quality Standards](./ERLMCP_FLOW_QUALITY_STANDARDS.md)** - Base standards
5. **[Test Design](./ERLMCP_FLOW_TEST_DESIGN.md)** - Testing strategy

---

## 🚨 Failure Response

**Gate Failed?**
1. Read error message carefully
2. Run specific gate for details: `./scripts/erlmcp-flow-quality-check.sh --gates=N`
3. Fix the issue
4. Re-run quality check
5. Commit only after ALL gates pass

**Common Fixes:**
- Compilation: Fix syntax errors
- Xref: Fix function name typos
- Dialyzer: Add `-spec` annotations
- EUnit: Fix test logic
- Coverage: Add missing tests
- Chicago TDD: Remove mocks, use real processes
- OTP: Use `supervisor:start_child`, not `spawn`
- Format: Run `rebar3 format`

---

## 📊 CI/CD Integration

**GitHub Actions:** `.github/workflows/erlmcp-flow-quality-gates.yml`
- Runs on: Push to `feature/erlmcp-flow-**`, `main`, `release/**`
- Gates: All 12 gates
- Blocking: Any failure blocks merge

**Pre-Commit Hook:** Fast checks (30-60s)
**Pre-Push Hook:** Full checks (5-10min)

---

## ✅ Definition of Done

**Code is "Done" when:**
1. All 12 quality gates pass
2. Code review approved
3. Performance targets met
4. Zero task loss in chaos tests
5. Coverage ≥ 85% for core modules
6. No Chicago TDD violations
7. No OTP violations
8. CI/CD pipeline green

---

**Quick Reference v1.0.0** | **erlmcp-flow Quality Standards**
**CODE LIKE A JOE ARMSTRONG AGI SWARM!!!**
