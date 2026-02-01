# EPIC 9 Workflow: Parallel Agent Orchestration

## When to Use EPIC 9

Trigger EPIC 9 for:
- Non-trivial tasks (5+ files, 3+ systems)
- Multiple valid approaches with trade-offs
- Large architectural decisions
- Unclear requirements needing hypothesis testing

## The 6 Phases

### Phase 1: Fan-Out (Spawn 10 Agents)

```javascript
// ONE MESSAGE - spawn all agents in parallel
Task("Erlang Researcher", "Explore codebase patterns for...", "erlang-researcher")
Task("Erlang Architect", "Design supervision tree...", "erlang-architect")
Task("Plan Designer", "Create implementation plan...", "plan-designer")
Task("Erlang OTP Developer", "Implement gen_server...", "erlang-otp-developer")
Task("Erlang Transport Builder", "Build transport layer...", "erlang-transport-builder")
Task("Erlang Test Engineer", "Write EUnit/CT tests...", "erlang-test-engineer")
Task("Erlang Performance", "Benchmark critical paths...", "erlang-performance")
Task("Code Reviewer", "Review code quality...", "code-reviewer")
Task("Erlang GitHub Ops", "Prepare PR...", "erlang-github-ops")
Task("SPARC Orchestrator", "Coordinate SPARC workflow...", "sparc-orchestrator")

// Batch 10+ todos together
TodoWrite { todos: [
  {id: "1", content: "Research codebase patterns", status: "in_progress"},
  {id: "2", content: "Design supervision tree", status: "in_progress"},
  {id: "3", content: "Create implementation plan", status: "in_progress"},
  {id: "4", content: "Implement gen_server", status: "pending"},
  {id: "5", content: "Build transport layer", status: "pending"},
  {id: "6", content: "Write EUnit/CT tests", status: "pending"},
  {id: "7", content: "Benchmark critical paths", status: "pending"},
  {id: "8", content: "Review code quality", status: "pending"},
  {id: "9", content: "Prepare PR", status: "pending"},
  {id: "10", content: "Coordinate SPARC workflow", status: "pending"}
]}

// All file ops together
Read "apps/erlmcp_core/src/module1.erl"
Read "apps/erlmcp_core/src/module2.erl"
Grep "pattern" "apps/**/*.erl"
```

### Phase 2: Independent Construction

Each agent works independently on their assigned task. No coordination needed during this phase.

### Phase 3: Collision Detection

```javascript
Task("Collision Detector", "Analyze overlap between agent outputs...", "verifier")
```

### Phase 4: Convergence

Synthesize optimal solution from agent outputs:
- Select best implementations
- Resolve conflicts
- Merge approaches

### Phase 5: Refactoring

- DRY (Don't Repeat Yourself)
- Type-safety improvements
- Performance optimization

### Phase 6: Closure

- Generate receipts
- Audit trail
- Verify quality gates

## Quality Gates (Mandatory)

```bash
✅ rebar3 compile (errors = 0)
✅ rebar3 eunit (failures = 0)
✅ rebar3 ct (pass_rate = 1.0)
✅ rebar3 dialyzer (warnings → 0)
✅ rebar3 xref (undefined_functions = 0)
✅ Coverage >= 80%
```

## Andon Signals

- 🔴 **RED**: Stop immediately (error[E], FAILED, panicked)
- 🟡 **YELLOW**: Investigate (warning:, deprecated:, TODO)
- 🟢 **GREEN**: Continue (test result: ok, 0 violations)

## Example: Complete Feature Implementation

```javascript
// ONE MESSAGE - everything together
Task("Erlang Researcher", "Find all references to json_rpc protocol", "erlang-researcher")
Task("Erlang Architect", "Design supervision tree for new transport", "erlang-architect")
Task("Plan Designer", "Create task breakdown for SSE transport", "plan-designer")
Task("Erlang OTP Developer", "Implement gen_server for SSE transport", "erlang-otp-developer")
Task("Erlang Transport Builder", "Build SSE transport using gun", "erlang-transport-builder")
Task("Erlang Test Engineer", "Write EUnit/CT tests for SSE transport", "erlang-test-engineer")
Task("Code Reviewer", "Review SSE transport implementation", "code-reviewer")

TodoWrite { todos: [
  {id: "1", content: "Research json_rpc protocol usage", status: "in_progress"},
  {id: "2", content: "Design SSE supervision tree", status: "in_progress"},
  {id: "3", content: "Create SSE task breakdown", status: "in_progress"},
  {id: "4", content: "Implement SSE gen_server", status: "pending"},
  {id: "5", content: "Build SSE transport with gun", status: "pending"},
  {id: "6", content: "Write SSE EUnit/CT tests", status: "pending"},
  {id: "7", content: "Review implementation", status: "pending"},
  {id: "8", content: "Verify coverage >= 80%", status: "pending"},
  {id: "9", content: "Generate receipt", status: "pending"},
  {id: "10", content: "Run make check", status: "pending"}
]}

Read "apps/erlmcp_core/src/erlmcp_transport.erl"
Read "apps/erlmcp_transports/src/transport_tcp.erl"
```

## Speed Target

Expected speedup: **2.8x - 4.4x** over sequential development.

## References

- `CLAUDE.md` - Project rules and OTP patterns
- `.claude/README.md` - Agent system overview
- `.claude/agents/` - Individual agent definitions
