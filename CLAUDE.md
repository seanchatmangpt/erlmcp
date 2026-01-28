# CLAUDE.md - erlmcp Development Guide

## Project Identity
**erlmcp**: Erlang/OTP MCP SDK. Client+server for AI-to-service protocol. Language: Erlang/OTP 25+. Build: rebar3. Test: EUnit, CT, Proper.

## Essential Commands
```bash
# Build & Test (MANDATORY BEFORE "DONE")
TERM=dumb rebar3 compile                    # Compile (MUST pass)
rebar3 eunit --module=<module>_tests        # Unit tests (MUST run)
rebar3 ct --suite=test/<suite>              # Integration (if applicable)
make benchmark-quick                         # Benchmark (if perf-critical, < 2min)

# Development
make console        # Erlang shell
make check          # Full: compile, xref, dialyzer, tests
make observer       # Process visualization
rebar3 dialyzer     # Type checking
rebar3 xref         # Cross-reference

# Benchmarks (v1.5.0)
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).        # In-memory: 2.69M ops/sec
erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>). # TCP sockets
erlmcp_bench_stress:run(<<"stress_5min_100k_ops">>).   # Sustained load
erlmcp_bench_chaos:run(<<"chaos_memory_exhaustion">>).  # Failure injection
erlmcp_bench_integration:run(<<"mcp_tool_sequence">>).  # MCP e2e
./scripts/bench/run_all_benchmarks.sh                   # Full suite (10-15min)
```

## Directory Layout
- `src/` - Client (erlmcp_client.erl), Server (erlmcp_server.erl), Transports (stdio/tcp/http), Registry, JSON-RPC
- `test/` - *_tests.erl (EUnit), *_SUITE.erl (CT)
- `bench/` - 5 benchmarks: core_ops, network_real, stress, chaos, integration
- `examples/` - simple/, calculator/, weather/
- `docs/` - architecture.md, otp-patterns.md, protocol.md, api-reference.md

## Architecture Fundamentals
**Supervision Tree:** erlmcp_sup (one_for_all) → registry + client_sup (simple_one_for_one) + server_sup (simple_one_for_one)
**Pattern:** Process-per-connection. Request ID correlation. Registry-based routing. Let-it-crash + supervisor recovery.
**Transport:** Behavior interface. Messages: {transport_data, Binary}, {transport_connected, Info}, {transport_disconnected, Reason}.
**State:** Record-based. Tracks transport module, capabilities, request_id, pending requests map.

## Core Modules
- `erlmcp_client.erl` - Request-response correlation via #state.pending map
- `erlmcp_server.erl` - Resources/tools/prompts management with handler functions
- `erlmcp_registry.erl` - Central message routing (migrating to gproc)
- `erlmcp_json_rpc.erl` - JSON-RPC 2.0 encode/decode
- `erlmcp_transport_*.erl` - stdio (I/O), tcp (ranch), http (gun/cowboy)

## Quality Gates (MANDATORY)
**BEFORE saying "done", MUST execute + report:**

1. **Compilation:**
   ```
   ✅ Compiled: X modules, Y BEAM files
   ⚠️ Warnings: [list]
   ❌ Errors: [list]
   ```

2. **Tests:**
   ```
   ✅ Tests: X/X passed (0 failures)
   ⚠️ Skipped: [reason]
   ❌ Failed: [module:line]
   ```

3. **Benchmarks (if perf code changed):**
   ```
   ✅ Benchmark: core_ops_100k - 2.69M ops/sec (no regression)
   ⚠️ Not run: [reason]
   ❌ Regression: -X% throughput
   ```

**Targets:** 0 errors, 100% test pass, ≥80% coverage, <10% perf regression.

## Development Patterns
**TDD:** Write tests first (test/*.erl). Chicago School (no mocks, real processes).
**OTP:** Follow gen_server/supervisor behaviors. Don't block init/1. Monitor critical processes. Use supervisors (never unsupervised spawn).
**Transports:** Implement -behaviour(erlmcp_transport). Callbacks: init/2, send/2, close/1. Template: erlmcp_transport_tcp.erl.
**Style:** rebar3_format (100-char line). Dialyzer strict. xref clean. 80%+ coverage.

## Common Pitfalls
1. Blocking init/1 → Use async cast initialization
2. Large messages → Reference shared data instead
3. Unmonitored processes → Clean up on dependency death
4. Missing timeouts → Default 5000ms for clients
5. Unsupervised spawns → Always use supervisor

## Key Files Quick Index
- **MCP protocol** → docs/protocol.md + src/erlmcp_json_rpc.erl
- **Add server tool** → src/erlmcp_server.erl:add_tool*
- **Call tool from client** → src/erlmcp_client.erl:call_tool
- **New transport** → Copy src/erlmcp_transport_tcp.erl
- **Message routing** → src/erlmcp_registry.erl + docs/otp-patterns.md
- **Test failure debug** → rebar3 eunit --module=MODULE --verbose

## Dependencies (v0.6.0)
**Core:** jsx (JSON), jesse (JSON Schema), gproc 0.9.0 (registry), gun 2.0.1 (HTTP client), ranch 2.1.0 (TCP), poolboy 1.5.2 (pools)
**Test:** proper, meck, coveralls
**Dev:** recon, observer_cli, rebar3_format, rebar3_lint

**Library Migration:** Custom code → production libs saved 770 LOC (registry: -291, HTTP: -281, TCP: -199).

## Agents (10 focused, v1.0.0)
- **erlang-otp-developer** - Implement gen_server/supervisor
- **erlang-transport-builder** - Add stdio/tcp/http transport
- **erlang-test-engineer** - Write tests (Chicago TDD)
- **erlang-researcher** - Understand codebase (haiku)
- **erlang-architect** - Design architecture
- **erlang-performance** - Benchmark/optimize
- **erlang-github-ops** - PR/release/CI
- **sparc-orchestrator** - SPARC workflows
- **plan-designer** - Research → Plan → Execute
- **code-reviewer** - Pre-merge review

## Commands (30 total, v1.0.0)
**SPARC:** /sparc, /sparc spec, /sparc architect, /sparc code, /sparc test, /sparc review, /sparc docs, /sparc deploy
**Swarm:** /swarm init, /swarm spawn, /swarm status, /swarm orchestrate
**GitHub:** /github pr, /github issue, /github repo
**Perf:** /perf analyze, /perf monitor, /perf optimize

## Benchmarks (v1.5.0)
**5 consolidated modules (from 15+ legacy):**
1. **core_ops** - Registry/queue/pool/session. 4 workloads (1K→1M ops). Real: 2.69M ops/sec.
2. **network_real** - TCP/HTTP real sockets. 7 workloads. TCP: 100→100K conn. HTTP: 100→5K conn.
3. **stress** - Sustained load. 4 durations (30s→24hr). Time-series monitoring. Degradation detection.
4. **chaos** - 11 failure scenarios. Bounded refusal validation. Recovery <5s.
5. **integration** - 5 MCP workflows. E2e latency. Protocol compliance.

**Performance baseline (your machine, Jan 2026):** Registry 553K msg/s, Queue 971K msg/s, Pool 149K msg/s, Session 242K msg/s, Network I/O 43K msg/s (bottleneck: 4KB real packets), Sustained 372K msg/s (60M ops/30s).

**Honest capacity:** 40-50K concurrent active connections per node. 100K+ requires clustering.

## Metrology Compliance (v1.5.0)
All benchmarks output canonical units:
- `throughput_msg_per_s` (NOT ambiguous "req/s")
- `latency_p50_us`, `latency_p95_us`, `latency_p99_us` (raw microseconds)
- `memory_heap_mib_per_conn` (scope: per_connection_heap)
- `memory_rss_mib_per_node` (scope: per_node_total)
- Required: workload_id, transport, duration_s, scope, precision

Validated by erlmcp_metrology_validator before write. Zero ambiguities. See docs/metrology/METRICS_GLOSSARY.md.

## Version & Status
- **Version:** 0.5.0 (src/erlmcp.app.src)
- **Erlang:** OTP 25+
- **Phase:** v0.6.0 in progress (library integration: gproc, gun, ranch, poolboy)

## Critical Rules
1. **NEVER say "done" without running:** compile + tests + report status.
2. **Run benchmarks if perf-critical code changed.**
3. **Report format:** ✅ Compiled / ✅ Tests / ✅ Benchmark (or ⚠️/❌ with details).
4. **Quality non-negotiable:** 0 errors, 0 test failures, ≥80% coverage, <10% regression.
5. **Follow OTP patterns:** gen_server, supervision, process isolation, let-it-crash.

## Help Resources
- Architecture: docs/architecture.md, docs/otp-patterns.md
- API: docs/api-reference.md + examples/
- Tests: test/ for patterns
- Agents: .claude/AGENT_INDEX.md
- Commands: .claude/COMMAND_INDEX.md
