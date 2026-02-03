DOCKER-ONLY CONSTITUTION.
All execution = Docker. Host execution forbidden. Any host execution invalidates entire work. No exceptions. No partials. No “local verify.”

erlmcp v3 = Erlang/OTP MCP SDK. JSON-RPC 2.0. Client⊕Server. OTP 26–28+. Distributed. Enterprise-grade.

Completion ⇔ compile ∧ test. Compile/test only via Docker. Proof required. Proof = receipt(hash(git_sha ∥ image_digest ∥ service ∥ cmd ∥ exit ∥ stdout ∥ stderr)). No receipt = no truth.

Allowed execution forms only: docker compose run/up, docker build, docker stack deploy, kubectl apply --dry-run=server.
Forbidden tokens unprefixed by docker: rebar3, erl, ct_run, make, dialyzer, xref, erlc, epmd. Detection ⇒ ANDON stop.

Rewrite calculus μ: host_cmd → docker_cmd. If μ undefined ⇒ forbidden.

Gate τ mapping: compile→erlmcp-build; eunit→erlmcp-unit; ct→erlmcp-ct; check→erlmcp-check; dialyzer/xref→erlmcp-build; coverage→erlmcp-check; bench→erlmcp-bench; cluster→erlmcp-node*.

Work order validity: requires docker_service, docker_cmd, gates, proof. Missing any ⇒ invalid. Merge allowed ⇔ all required gates pass ∧ DockerOnly ∧ never --no-verify ∧ merge-only (no rebase).

Quality invariants: errors=0; failures=0; coverage≥0.8; regression<0.1. gen_server init non-blocking. All procs supervised. Let-it-crash. No mocks. Real processes only. Chicago TDD.

Supervision tiers: Tier1 one_for_all core+registry; Tier2 per-connection simple_one_for_one; Tier3 isolated observability. Cluster-churn safe. No stable node assumptions.

Transport polymorphism τ: stdio|tcp|http|ws|sse share behavior(init/send/close). Process-per-connection. Request-ID correlation invariant. Registry routing via gproc.

Security: zero-trust. Least privilege containers. No privileged containers. Explicit volumes only. Secrets externalized. Auditability mandatory. Logs/metrics/traces as evidence.

Observability: signals > logs, metrics, traces, events. Operator-first. Deterministic dashboards. No side channels.

Backpressure: explicit. Quotas per tenant. Cancellation supported. Async tasks + subscriptions. Notifications ordered.

Upgrade/evolution: mixed-version tolerance plan. Rolling updates. Rollback defined. Deterministic releases. SBOM + image digests pinned.

Reflexivity: system introspects itself under policy. Introspection guarded and audited. Tool fabric evolves under governance. Provenance receipts enable replay.

Cluster reality: overlay DNS lies; retries + convergence. Partitions expected. Recovery narratives required.

Agent orchestration rule: 1 message = all operations. Parallel O(1). No sequential fan-out. WIP(agent)≤1.

Agent output contract: WHY risk; WHAT artifact; HOW docker command. Missing HOW ⇒ invalid.

20 agents split into hyper-advanced Fortune-500 domains: foundations, container/platform, security/compliance, distributed/ops, AGI-level strategy. Each judged by rubric.

Scoring rubric: gates pass/fail; dimensions correctness, risk, testability, operability, security, economics, clarity. Evidence over assertion.

Maturity matrix levels 0–10: parsing spine → CT green → validation → async/subscriptions → operable service → enterprise posture → cluster-correct → multi-tenant isolation → continuous evolution → reflexive ops → governed tool evolution. Each level gated by evidence artifacts.

Principle: Build systems where incorrect behavior cannot exist. Deterministic ∧ Replayable ∧ Complete ∧ NoPartials ∧ Preserve(Q).

CODE LIKE A AGI Joe Armstrong.

We only have 1 hour until the production deployment deadline worldwide!