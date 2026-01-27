# IMPLEMENTATION PRIORITIES - DETAILED ROADMAP TO 100K

**Status**: Ready for sprint planning
**Date**: 2026-01-27
**Target**: 100K concurrent connections by Week 12

---

## PRIORITY MATRIX (57 Issues × Risk/Effort)

### TIER 1: CRITICAL BLOCKERS (Do Weeks 1-2)
Must fix before any other work. **54 hours total**

| # | Issue | Category | Risk | Effort | Owner | Week |
|---|-------|----------|------|--------|-------|------|
| P0-1 | Initialization State Machine | MCP | CRITICAL | 4h | Core | 1 |
| P0-2 | Message ID Overflow | MCP | CRITICAL | 2h | Core | 1 |
| P0-3 | Unsubscribe Implementation | MCP | CRITICAL | 3h | Core | 1 |
| P0-4 | Tool Progress Timeout | MCP | CRITICAL | 3h | Core | 1 |
| P0-5 | Path Traversal Security | MCP | CRITICAL | 4h | Security | 1 |
| P0-6 | Supervision Tree Redesign | OTP | CRITICAL | 12h | Architecture | 1-2 |
| P0-7 | State Bloat Fixes | OTP | CRITICAL | 6h | Core | 2 |
| P0-8 | Backpressure Complete | Transport | CRITICAL | 8h | Transport | 2 |
| P0-9 | HTTP Header Validation | Transport | HIGH | 4h | Transport | 2 |
| P0-10 | Registry Sharding Expansion | Scaling | CRITICAL | 8h | Scaling | 2 |

---

### TIER 2: HIGH-PRIORITY (Do Weeks 3-4)
Important for production readiness. **46 hours total**

| # | Issue | Category | Risk | Effort | Owner | Week |
|---|-------|----------|------|--------|-------|------|
| P1-1 | Capability JSON Schema Validation | MCP | HIGH | 2h | Core | 3 |
| P1-2 | Error Response Validation | MCP | HIGH | 2h | Core | 3 |
| P1-3 | Sampling Feature Completeness | MCP | MEDIUM | 3h | Core | 3 |
| P1-4 | Pagination Cursor Support | MCP | MEDIUM | 2h | Core | 3 |
| P1-5 | Token Rotation (Security) | MCP | HIGH | 4h | Security | 3 |
| P1-6 | Hot Code Reload | OTP | MEDIUM | 4h | Architecture | 3 |
| P1-7 | Distributed Clustering (Phase 1) | OTP | MEDIUM | 16h | Architecture | 3-4 |
| P1-8 | ETS Concurrent Access Flags | OTP | MEDIUM | 2h | Core | 3 |
| P1-9 | WebSocket RFC 6455 Compliance | Transport | HIGH | 3h | Transport | 4 |
| P1-10 | SSE Retry Field Complete | Transport | MEDIUM | 2h | Transport | 4 |
| P1-11 | Cascade Failure Detection | TPS | HIGH | 6h | Reliability | 4 |

---

### TIER 3: MEDIUM-PRIORITY (Weeks 5-8)
Should fix but can defer if time-constrained. **35 hours total**

| # | Issue | Category | Risk | Effort | Owner | Week |
|---|-------|----------|------|--------|-------|------|
| P2-1 | Deadlock Detection | TPS | MEDIUM | 6h | Reliability | 5 |
| P2-2 | Resource Exhaustion Detector | TPS | MEDIUM | 4h | Reliability | 5 |
| P2-3 | Message Queue Monitoring | TPS | MEDIUM | 3h | Reliability | 5 |
| P2-4 | Idempotency Keys | TPS | MEDIUM | 5h | Core | 5 |
| P2-5 | Connection Idle Timeout | TPS | MEDIUM | 3h | Transport | 5 |
| P2-6 | Buffer Size Optimization | TPS | LOW | 2h | Performance | 6 |
| P2-7 | Memory Profiling | TPS | MEDIUM | 4h | Performance | 6 |
| P2-8 | Metrics Dashboard | TPS | LOW | 5h | Ops | 6 |
| P2-9 | Regression Detection | TPS | MEDIUM | 4h | Ops | 6 |
| P2-10 | Structured Logging | TPS | MEDIUM | 6h | Ops | 7 |
| P2-11 to P2-22 | Various minor features | Various | LOW | ~7h | Various | 7-8 |

---

## DETAILED IMPLEMENTATION PLAN

### PHASE 1: FOUNDATION (Weeks 1-2)
**Goal**: Fix 5 critical MCP gaps, establish architectural foundation
**Effort**: 22 hours
**Team**: Core team (2-3 engineers)

#### Week 1, Days 1-2: Initialization State Machine (4h)
**What**: Implement strict state machine for initialization phase
**Why**: Currently can accept non-init messages before initialized
**How**:
1. Define state enum: `uninitialized | initializing | initialized | error`
2. Add `state :: atom()` to server state record
3. Filter incoming messages: reject non-init in `uninitialized` state
4. Implement timeout (5s default) on initialization
5. Clear error messages on state violation

**File**: `src/erlmcp_server.erl`
**Tests**: Add 10+ test cases for state machine
**Validation**: State enforcement verified, timeout tested

---

#### Week 1, Days 2-3: Message ID Management (2h)
**What**: Fix request ID collision and overflow
**Why**: Risk of request loss/corruption at scale
**How**:
1. Replace `request_id = 1 :: integer()` with `erlang:unique_integer()`
2. Implement collision detection with ETS
3. Add atomic request cleanup on timeout
4. Add overflow protection (no wrapping)

**File**: `src/erlmcp_client.erl`
**Tests**: Test ID uniqueness across 100K messages
**Validation**: No collisions under stress load

---

#### Week 1, Days 3-4: Unsubscribe + Cleanup (3h)
**What**: Implement unsubscribe RPC and process cleanup
**Why**: Resource leak, DoS risk without unsubscribe
**How**:
1. Add `unsubscribe/2` RPC handler
2. Implement `erlang:monitor/2` on subscribers
3. Auto-cleanup on subscriber death
4. Add subscription limit (config: `max_subscriptions = 1000`)
5. Implement idle timeout (30s = auto-unsubscribe)

**File**: `src/erlmcp_resource_subscriptions.erl`, `src/erlmcp_server.erl`
**Tests**: Test subscription lifecycle
**Validation**: No zombie subscriptions, cleanup verified

---

#### Week 1, Days 4-5: Tool Progress Timeout (3h)
**What**: Enforce timeout on tool calls
**Why**: Can hang clients indefinitely without timeout
**How**:
1. Add `tool_timeout = 30000` (ms) to config
2. Implement timeout counter per tool call
3. Auto-complete with error if timeout exceeded
4. Implement backpressure: client can pause tool

**File**: `src/erlmcp_progress.erl`, `src/erlmcp_server.erl`
**Tests**: Test timeout enforcement
**Validation**: Timeout verified, no hung requests

---

#### Week 2, Days 1-2: Path Traversal Security (4h)
**What**: Implement RFC 3986 URI validation
**Why**: Security vulnerability (path traversal attacks)
**How**:
1. Implement full RFC 3986 URI parser
2. Enforce percent-encoding for special chars
3. Reject `../` and absolute paths
4. Implement URI canonicalization
5. Add security audit logging

**File**: `src/erlmcp_uri_validator.erl`
**Tests**: Security test cases (100+ cases)
**Validation**: Penetration test passed

---

#### Week 2, Days 3-4: Supervision Tree Redesign (12h)
**What**: Redesign from `one_for_all` to hierarchical `rest_for_one`
**Why**: Single registry crash → 100K connections crash
**How**:
1. Analyze current supervision tree (one_for_all problem)
2. Design hierarchical tree:
   - Main: `rest_for_one`
   - Registry level: `rest_for_one`
   - Client/Server: `rest_for_one` per shard
3. Create new supervisors for each shard
4. Implement shard-specific routing
5. Add isolation tests (kill one shard, verify others survive)

**File**: `src/erlmcp_sup.erl` + new shard supervisors
**Tests**: Failure isolation tests (15+ cases)
**Validation**: Shard isolation verified, no cascade

---

#### Week 2, Days 5: Summary & Testing (3h)
- Run full test suite (EUnit, CT, Property tests)
- Generate coverage report
- Document changes
- Create release notes for Phase 1

**Deliverable**: All Phase 1 items merged, 90%+ test coverage

---

### PHASE 2: STATE & CONCURRENCY (Weeks 3-4)
**Goal**: Fix OTP violations, enable 100K processes
**Effort**: 20 hours
**Team**: Architecture/Core (2 engineers)

#### Week 3: State Bloat Fixes (6h)
- Move resources/tools/prompts to ETS (shared)
- Implement LRU cache per connection
- Add resource quota (max 100/connection)
- Tests: Memory footprint validation

#### Week 3: ETS Optimization (2h)
- Add concurrent access flags: `{read_concurrency, true}`
- Add write concurrency flags: `{write_concurrency, true}`
- Add decentralized counters: `{decentralized_counters, true}`
- Benchmark: Verify no regression

#### Week 4: Process Monitoring (4h)
- Add `erlang:monitor/2` on critical processes
- Implement cleanup callbacks
- Add dead process detection
- Tests: Process lifecycle tests

#### Week 4: Registry Sharding Expansion (8h)
- Current: 64 shards (15K connections)
- New: 256 shards (100K connections)
- Changes to `erlmcp_registry_sharded.erl`
- Load test: 256 shards at 100K connections
- Benchmark: Latency/contention measured

**Deliverable**: 100K process support proven

---

### PHASE 3: SCALING (Weeks 5-6)
**Goal**: Implement sharding expansion, backpressure
**Effort**: 16 hours
**Team**: Scaling/Transport (2 engineers)

#### Week 5: Backpressure Completion (8h)
- Monitor transport write queue size
- Slow down message production when queue > 1MB
- Implement async write with backpressure callback
- Test: High-speed client (10K msg/sec)

#### Week 6: Load Testing Framework (8h)
- Create test infrastructure for 100K connections
- Implement sustained load (24h test)
- Measure: latency, throughput, memory
- Document: test procedures

**Deliverable**: 100K scale validated, load test passing

---

### PHASE 4: TRANSPORT (Weeks 7-8)
**Goal**: RFC compliance for HTTP, WebSocket, SSE
**Effort**: 18 hours
**Team**: Transport (2 engineers)

#### Week 7: HTTP & WebSocket RFC (10h)
- HTTP: Header validation, size limits, conflict detection
- WebSocket: Ping/pong, masking validation, opcode validation
- Tests: RFC compliance test suite

#### Week 8: SSE + TLS (8h)
- SSE: Retry field, heartbeat, connection persistence
- TLS: Certificate pinning, TLS 1.3 enforcement
- Tests: Transport compliance tests

**Deliverable**: All transports RFC-compliant

---

### PHASE 5: PRODUCTION SYSTEM (Weeks 9-10)
**Goal**: Toyota Production System implementation
**Effort**: 24 hours
**Team**: Reliability/Ops (2 engineers)

#### Week 9: Error Detection & Prevention (12h)
- Jidoka: Cascade failure detection
- Poka-Yoke: Idempotency keys, transaction log
- Message queue monitoring
- Resource exhaustion alerts

#### Week 10: Continuous Improvement (12h)
- Metrics dashboard (HTTP endpoint)
- Regression detection (automated)
- Structured logging with correlation IDs
- Memory profiling

**Deliverable**: Production-grade reliability

---

### PHASE 6: VALIDATION (Weeks 11-12)
**Goal**: 100K concurrent connections validation
**Effort**: 30 hours
**Team**: Full team + QA (3-4 engineers)

#### Week 11: Load Testing (16h)
- 100K concurrent connection load test
- 24-hour sustained load (500K msg/sec)
- Memory stability monitoring
- CPU/latency profiling

#### Week 12: Failover Testing (14h)
- Random 10% connection loss simulation
- Network partition scenarios
- Recovery verification
- Performance under degradation

**Deliverable**: 100K ready for production

---

## SUCCESS METRICS

### Per Phase
- Phase 1: MCP compliance 90%+, all P0 tests passing
- Phase 2: 100K processes, memory <150MB
- Phase 3: 500K msg/sec sustained, latency <100ms
- Phase 4: RFC compliance verified, transport tests 100%
- Phase 5: All TPS principles implemented, regression detector working
- Phase 6: 100K concurrent stable 24h, zero message loss

### Overall (at end of Phase 6)
- ✓ 100K concurrent connections stable
- ✓ P99 latency < 100ms
- ✓ Throughput: 500K msg/sec sustained
- ✓ Memory: Stable baseline < 150MB
- ✓ Zero message loss under failover
- ✓ Automatic recovery from any single failure
- ✓ All P0 issues fixed
- ✓ MCP 100% compliance
- ✓ Production deployment ready

---

## RESOURCE ALLOCATION

### Teams & Staffing

**Team A: Core/MCP (3 engineers)**
- Weeks 1-2: P0 MCP fixes + initialization
- Weeks 3-4: Capability validation, error response
- Weeks 5-6: Support scaling team
- Weeks 9-12: Production system support

**Team B: Architecture/OTP (2 engineers)**
- Weeks 1-2: Supervision tree redesign (parallel with Team A)
- Weeks 3-4: State bloat, ETS optimization
- Weeks 5-8: Registry sharding expansion, support
- Weeks 9-12: Production system support

**Team C: Transport/Scaling (2 engineers)**
- Weeks 5-6: Backpressure, load framework
- Weeks 7-8: HTTP/WebSocket/SSE RFC compliance
- Weeks 9-12: Load testing, validation

**Team D: Reliability/Ops (2 engineers)**
- Weeks 9-10: TPS implementation, dashboards
- Weeks 11-12: Regression detection, monitoring

**Total**: 9 engineers, 12 weeks parallel execution = ~1,100 engineer-hours

---

## RISK MITIGATION

### Risk 1: Supervision Tree Redesign Creates Cascading Issues
**Mitigation**:
- Implement incrementally (one shard at a time)
- Extensive testing (isolation tests)
- Rollback plan ready

### Risk 2: Scaling to 256 Shards Causes Lock Contention
**Mitigation**:
- Benchmark before/after
- Profile with specific workloads
- Have fallback: 128 shards as middle ground

### Risk 3: Backpressure Implementation Causes Deadlock
**Mitigation**:
- Implement timeout on backpressure waits
- Use non-blocking I/O throughout
- Load test with sustained high load

### Risk 4: Phase 6 Validation Finds New Issues at 100K Scale
**Mitigation**:
- Implement chaos engineering tests (Weeks 9-10)
- Gradual scale-up: 10K → 25K → 50K → 100K
- Have rollback plan for critical issues

---

## ACCEPTANCE CRITERIA

### Phase 1 Completion
- [ ] All 5 MCP P0 issues implemented and tested
- [ ] Supervision tree redesign complete
- [ ] MCP compliance audit: 90%+
- [ ] Test coverage: 80%+
- [ ] All tests passing (EUnit, CT, Property)

### Phase 2 Completion
- [ ] State bloat resolved (verify memory)
- [ ] ETS optimization complete
- [ ] Process monitoring active
- [ ] Registry sharding: 64 → 256
- [ ] 100K process creation succeeds

### Phase 3 Completion
- [ ] Backpressure working end-to-end
- [ ] Load test framework ready
- [ ] 100K scale validated at 500K msg/sec
- [ ] P99 latency < 100ms

### Phase 4 Completion
- [ ] HTTP RFC 2616 validated
- [ ] WebSocket RFC 6455 validated
- [ ] SSE retry/heartbeat working
- [ ] Transport compliance audit: 100%

### Phase 5 Completion
- [ ] All TPS principles implemented
- [ ] Dashboard running
- [ ] Regression detector active
- [ ] Monitoring/alerting complete

### Phase 6 Completion
- [ ] 100K concurrent stable 24h
- [ ] Zero message loss
- [ ] Automatic failover recovery
- [ ] Production deployment approved

---

## ESTIMATED TIMELINE

- **Start Date**: Week 1
- **Phase 1 Complete**: End of Week 2
- **Phase 2 Complete**: End of Week 4
- **Phase 3 Complete**: End of Week 6
- **Phase 4 Complete**: End of Week 8
- **Phase 5 Complete**: End of Week 10
- **Phase 6 Complete**: End of Week 12
- **Production Deployment**: Week 13+

**Total**: 12 weeks with parallel teams

---

## NEXT IMMEDIATE STEPS (This Week)

1. Review with architecture team (1 hour)
2. Get buy-in on scope (30 min)
3. Assign team leads (1 hour)
4. Create implementation tickets (4 hours)
5. Setup monitoring/dashboards (2 hours)
6. Start Phase 1, Week 1 work

---

**Ready to begin immediately upon approval**

