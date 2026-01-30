# 80/20 Consolidation - Visual Summary

## The Pareto Principle in Action

```
┌─────────────────────────────────────────────────────────────────┐
│                    ERLMCP CODEBASE ANALYSIS                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  TOTAL: 106 modules (35,549 LOC)                                │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │               THE VITAL 20% (KEEP)                      │    │
│  │  21 modules deliver 80% of production value             │    │
│  │                                                          │    │
│  │  ┌────────────────────────────────────────────────┐    │    │
│  │  │ TIER 1: CRITICAL (9 modules, 4,500 LOC)       │    │    │
│  │  │ ────────────────────────────────────────────    │    │    │
│  │  │ • erlmcp_client.erl      (742 LOC)  [OPTIMIZE] │    │    │
│  │  │ • erlmcp_server.erl      (2,040 LOC) [SPLIT]   │    │    │
│  │  │ • erlmcp_json_rpc.erl    (469 LOC)  [COMPLETE] │    │    │
│  │  │ • erlmcp_registry.erl    (503 LOC)  [COMPLETE] │    │    │
│  │  │ • erlmcp_capabilities.erl (1,253 LOC) [KEEP]    │    │    │
│  │  │ • erlmcp_transport_behavior (819 LOC) [KEEP]     │    │    │
│  │  │ • erlmcp_transport_stdio  (324 LOC)  [OPTIMIZE]  │    │    │
│  │  │ • erlmcp_transport_tcp    (867 LOC)  [VALIDATE]  │    │    │
│  │  │ • erlmcp_transport_http   (300 LOC)  [DOCUMENT]  │    │    │
│  │  └────────────────────────────────────────────────┘    │    │
│  │                                                          │    │
│  │  ┌────────────────────────────────────────────────┐    │    │
│  │  │ TIER 2: ESSENTIAL (12 modules, 4,000 LOC)      │    │    │
│  │  │ ────────────────────────────────────────────    │    │    │
│  │  │ • erlmcp_rate_limiter.erl   (874 LOC) [SIMPLIFY]│    │    │
│  │  │ • erlmcp_auth.erl          (604 LOC) [COMPLETE] │    │    │
│  │  │ • erlmcp_circuit_breaker.erl (685 LOC) [KEEP]   │    │    │
│  │  │ • erlmcp_batch.erl         (485 LOC) [OPTIMIZE] │    │    │
│  │  │ • erlmcp_hooks.erl         (596 LOC) [VALIDATE] │    │    │
│  │  │ • erlmcp_session_manager.erl (381 LOC) [KEEP]   │    │    │
│  │  │ • erlmcp_connection_monitor.erl (500 LOC) [KEEP]│    │    │
│  │  │ • erlmcp_code_reload.erl   (565 LOC) [EVALUATE] │    │    │
│  │  │ • erlmcp_logging.erl       (382 LOC) [INTEGRATE]│    │    │
│  │  │ • erlmcp_transport_ws.erl  (724 LOC) [DOCUMENT] │    │    │
│  │  │ • erlmcp_transport_sse.erl (639 LOC) [VALIDATE] │    │    │
│  │  │ • erlmcp_pool_manager.erl  (579 LOC) [DOCUMENT] │    │    │
│  │  └────────────────────────────────────────────────┘    │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │           THE COSTLY 20% (REMOVE/CONSOLIDATE)           │    │
│  │  25 modules consume 80% of maintenance effort           │    │
│  │                                                          │    │
│  │  ┌────────────────────────────────────────────────┐    │    │
│  │  │ BROKEN FILES (14 files, 3,000 LOC)            │    │    │
│  │  │ ────────────────────────────────────────────    │    │    │
│  │  │ DELETE ALL *.broken files                      │    │    │
│  │  │ ❌ erlmcp_cache.erl.broken                     │    │    │
│  │  │ ❌ erlmcp_prompt_argument_validator.erl.broken │    │    │
│  │  │ ❌ erlmcp_rate_limiter_v2.erl.broken          │    │    │
│  │  │ ❌ erlmcp_schema_validator.erl.broken         │    │    │
│  │  │ ❌ erlmcp_state_migration.erl.broken          │    │    │
│  │  │ ❌ erlmcp_uri_validator.erl.broken            │    │    │
│  │  │ ❌ ... (8 more broken test files)              │    │    │
│  │  └────────────────────────────────────────────────┘    │    │
│  │                                                          │    │
│  │  ┌────────────────────────────────────────────────┐    │    │
│  │  │ INCOMPLETE FEATURES (9 modules, 4,000 LOC)     │    │    │
│  │  │ ────────────────────────────────────────────    │    │    │
│  │  │ ⚠️  erlmcp_icon_cache.erl         (TODO)      │    │    │
│  │  │ ⚠️  erlmcp_resource_subscriptions (4 TODOs)   │    │    │
│  │  │ ⚠️  erlmcp_session_failover.erl   (TODO)      │    │    │
│  │  │ ⚠️  erlmcp_session_replicator.erl (TODO)      │    │    │
│  │  │ ⚠️  erlmcp_secrets.erl           (3 TODOs)    │    │    │
│  │  │ ⚠️  erlmcp_progress.erl          (unused)     │    │    │
│  │  │ ⚠️  pricing/ (11 modules)       (6 TODOs)     │    │    │
│  │  └────────────────────────────────────────────────┘    │    │
│  │                                                          │    │
│  │  ┌────────────────────────────────────────────────┐    │    │
│  │  │ EXPERIMENTAL / UNUSED (8 modules, 2,500 LOC)   │    │    │
│  │  │ ────────────────────────────────────────────    │    │    │
│  │  │ 🔬 erlmcp_chaos.erl           → examples/      │    │    │
│  │  │ 🔬 erlmcp_debugger.erl        → DELETE        │    │    │
│  │  │ 🔬 erlmcp_profiler.erl        → DELETE        │    │    │
│  │  │ 🔬 erlmcp_evidence_path.erl   → tcps_erlmcp/  │    │    │
│  │  │ 🔬 erlmcp_recovery_manager.erl → DELETE        │    │    │
│  │  │ 🔬 erlmcp_cpu_guard.erl       → DELETE        │    │    │
│  │  │ 🔬 erlmcp_memory_guard.erl    → DELETE        │    │    │
│  │  │ 🔬 erlmcp_cpu_quota.erl       → DELETE        │    │    │
│  │  └────────────────────────────────────────────────┘    │    │
│  │                                                          │    │
│  │  ┌────────────────────────────────────────────────┐    │    │
│  │  │ REDUNDANT ABSTRACTIONS (6 modules, 1,500 LOC)  │    │    │
│  │  │ ────────────────────────────────────────────    │    │    │
│  │  │ 🔄 erlmcp_transport_registry.erl  → REMOVE     │    │    │
│  │  │ 🔄 erlmcp_transport_discovery.erl → REMOVE     │    │    │
│  │  │ 🔄 erlmcp_transport_pipeline.erl  → REMOVE     │    │    │
│  │  │ 🔄 erlmcp_transport_validation.erl → REMOVE    │    │    │
│  │  │ 🔄 erlmcp_pool_strategy.erl       → REMOVE     │    │    │
│  │  │ 🔄 erlmcp_transport_adapter.erl   → REMOVE     │    │    │
│  │  └────────────────────────────────────────────────┘    │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │           THE MIDDLE 60% (SIMPLIFY/STANDARDIZE)         │    │
│  │  60 modules need standardization and cleanup            │    │
│  │                                                          │    │
│  │  Actions:                                                │    │
│  │  ✓ Standardize error handling (erlmcp_errors.hrl)       │    │
│  │  ✓ Add type specs (100% coverage for Tier 1/2)          │    │
│  │  ✓ Complete documentation (@doc for all public APIs)    │    │
│  │  ✓ Consolidate observability (27 → 10 modules)          │    │
│  │  ✓ Simplify rate limiter (keep only sliding window)     │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Consolidation Roadmap Timeline

```
WEEK 1-2: PHASE 1 - CLEANUP
┌─────────────────────────────────────────────────────────────┐
│ Day 1-2:  Delete all *.broken files (14 files, 3,000 LOC)  │
│ Day 3-4:  Move experimental code to examples/ (8 modules)  │
│ Day 5-7:  Remove redundant abstractions (6 modules)        │
│ Day 8-10: Consolidate pricing modules (11 → 2 modules)     │
└─────────────────────────────────────────────────────────────┘
              ↓
WEEK 3-4: PHASE 2 - COMPLETE INCOMPLETE
┌─────────────────────────────────────────────────────────────┐
│ Day 11-14: Complete auth (JWT/OAuth2/mTLS) OR simplify     │
│ Day 15-17: Complete resource subscriptions (5 TODOs)       │
│ Day 18-20: Complete or remove session failover/replicator  │
│ Day 21:    Remove icon_cache, simplify secrets             │
└─────────────────────────────────────────────────────────────┘
              ↓
WEEK 5-6: PHASE 3 - CONSOLIDATE & SIMPLIFY
┌─────────────────────────────────────────────────────────────┐
│ Day 22-26: Split erlmcp_server.erl (2,040 → 3×500 LOC)     │
│ Day 27-30: Merge transport registry into erlmcp_registry    │
│ Day 31-33: Standardize error handling (erlmcp_errors.hrl)  │
│ Day 34-35: Simplify rate limiter (single algorithm)        │
└─────────────────────────────────────────────────────────────┘
              ↓
WEEK 7-8: PHASE 4 - OPTIMIZE CORE
┌─────────────────────────────────────────────────────────────┐
│ Day 36-38: Add type specs to Tier 1/2 modules (100%)       │
│ Day 39-41: Optimize client state machine                    │
│ Day 42-44: Optimize JSON-RPC batch processing               │
│ Day 45-47: Complete test coverage (90%+ target)            │
└─────────────────────────────────────────────────────────────┘
              ↓
WEEK 9-10: PHASE 5 - DOCUMENT
┌─────────────────────────────────────────────────────────────┐
│ Day 48-50: Add @doc to all Tier 1/2 public APIs            │
│ Day 51-53: Update architecture diagrams                     │
│ Day 54-56: Write integration guides                         │
│ Day 57-60: Document OTP patterns (supervision, lifecycle)   │
└─────────────────────────────────────────────────────────────┘
```

---

## Before/After Comparison

```
┌─────────────────────────────────────────────────────────────┐
│                    BEFORE CONSOLIDATION                     │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  📊 METRICS:                                                │
│  • Total modules:     106                                   │
│  • Total LOC:         35,549                                │
│  • TODO count:        24                                    │
│  • Broken files:      14                                    │
│  • Test coverage:     ~60%                                  │
│  • Type spec coverage: ~70%                                  │
│                                                              │
│  🚨 PROBLEMS:                                               │
│  • 20% of code consumes 80% of maintenance time             │
│  • High cognitive load (106 modules to understand)          │
│  • Incomplete implementations (24 TODOs)                    │
│  • Experimental code in production                          │
│  • Inconsistent error handling                              │
│  • Redundant abstractions                                   │
│                                                              │
│  💰 MAINTENANCE BURDEN:                                     │
│  • 80% of time spent on bottom 20% of code                  │
│  • Frequent regressions in experimental features            │
│  • Unclear which modules are production-ready               │
│                                                              │
└─────────────────────────────────────────────────────────────┘
                              ↓
                              ↓
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    AFTER CONSOLIDATION                      │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  📊 METRICS:                                                │
│  • Total modules:     75        (-29%)                     │
│  • Total LOC:         25,000    (-30%)                     │
│  • TODO count:        0         (-100%)                    │
│  • Broken files:      0         (-100%)                    │
│  • Test coverage:     90%+      (+50%)                     │
│  • Type spec coverage: 100%      (+43%)                    │
│                                                              │
│  ✅ IMPROVEMENTS:                                           │
│  • Vital 20% clearly identified and optimized               │
│  • Low cognitive load (75 focused modules)                  │
│  • All features complete (0 TODOs)                          │
│  • Experimental code isolated to examples/                  │
│  • Consistent error handling (erlmcp_errors.hrl)            │
│  • No redundant abstractions                                │
│                                                              │
│  🚀 MAINTENANCE BENEFIT:                                    │
│  • 20% of time on maintenance, 80% on features              │
│  • Stable, production-ready codebase                        │
│  • Clear module boundaries (core vs examples)               │
│  • Better testability (smaller, focused modules)            │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Risk Heatmap

```
                    HIGH IMPACT
                         │
                         │
    ┌────────────────────┼────────────────────┐
    │  Phase 1: Cleanup │  Phase 4: Optimize │
    │  Risk: LOW        │  Risk: MEDIUM      │
    │  Impact: HIGH     │  Impact: HIGH      │
    │                    │                    │
    │  ✅ Delete broken │  ⚠️  Split server  │
    │  ✅ Move experim't│  ⚠️  Optimize core │
    │  ✅ Remove redund │                    │
    └────────────────────┼────────────────────┘
                         │
                         │
MEDIUM IMPACT ───────────┼────────────────── LOW IMPACT
                         │
    ┌────────────────────┼────────────────────┐
    │  Phase 2: Complete │  Phase 5: Document │
    │  Risk: HIGH       │  Risk: LOW         │
    │  Impact: MEDIUM   │  Impact: MEDIUM    │
    │                    │                    │
    │  ⚠️  Complete auth│  ✅ Add @doc       │
    │  ⚠️  Complete subs│  ✅ Update docs    │
    │  ⚠️  Consolidate  │  ✅ Write guides   │
    └────────────────────┴────────────────────┘
                         │
                         │
                    LOW IMPACT

KEY:
  ✅ LOW RISK: Safe to proceed, clear rollback path
  ⚠️  MEDIUM RISK: Requires testing, backup plan needed
  🚨 HIGH RISK: Critical features, extensive testing required
```

---

## Quick Reference: Module Actions

```
┌──────────────────────────────────────────────────────────────┐
│                    MODULE ACTION MATRIX                      │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  KEEP & OPTIMIZE (21 modules, 20% of code, 80% of value)    │
│  ─────────────────────────────────────────────────────────── │
│  ✅ erlmcp_client.erl          → Optimize state machine      │
│  ✅ erlmcp_server.erl          → Split into 3 modules        │
│  ✅ erlmcp_json_rpc.erl        → Add batch RFC compliance    │
│  ✅ erlmcp_registry.erl        → Document gproc patterns     │
│  ✅ erlmcp_capabilities.erl    → Keep (complete)             │
│  ✅ erlmcp_transport_behavior  → Keep (complete)             │
│  ✅ erlmcp_transport_stdio.erl → Optimize buffer handling    │
│  ✅ erlmcp_transport_tcp.erl   → Validate ranch pooling      │
│  ✅ erlmcp_transport_http.erl  → Document HTTP/2 usage       │
│  ✅ erlmcp_rate_limiter.erl    → Simplify to 1 algorithm     │
│  ✅ erlmcp_auth.erl            → Complete JWT/OAuth2         │
│  ✅ erlmcp_circuit_breaker.erl → Document usage patterns     │
│  ✅ erlmcp_batch.erl           → Optimize batching           │
│  ✅ erlmcp_hooks.erl           → Validate ordering guarantees│
│  ✅ erlmcp_session_manager.erl → Add clustering support      │
│  ✅ erlmcp_connection_monitor  → Reduce false positives      │
│  ✅ erlmcp_code_reload.erl     → Evaluate usage              │
│  ✅ erlmcp_logging.erl         → Integrate with OTEL         │
│  ✅ erlmcp_transport_ws.erl    → Document lifecycle          │
│  ✅ erlmcp_transport_sse.erl   → Validate event ordering     │
│  ✅ erlmcp_pool_manager.erl    → Document strategy selection │
│                                                               │
│  REMOVE OR CONSOLIDATE (25 modules, 80% of maintenance)      │
│  ─────────────────────────────────────────────────────────── │
│  ❌ erlmcp_cache.erl.broken               → DELETE           │
│  ❌ erlmcp_prompt_argument_validator.broken → DELETE         │
│  ❌ erlmcp_rate_limiter_v2.erl.broken      → DELETE         │
│  ❌ erlmcp_schema_validator.erl.broken     → DELETE         │
│  ❌ erlmcp_state_migration.erl.broken      → DELETE         │
│  ❌ erlmcp_uri_validator.erl.broken        → DELETE         │
│  ❌ All *_tests.erl.broken                 → DELETE (8 files)│
│  ❌ erlmcp_icon_cache.erl                  → DELETE          │
│  ❌ erlmcp_session_failover.erl            → DELETE          │
│  ❌ erlmcp_session_replicator.erl          → DELETE          │
│  ❌ erlmcp_progress.erl                    → DEPRECATE       │
│  ❌ pricing/ (11 modules)                  → → 2 modules     │
│  ❌ erlmcp_chaos.erl                       → → examples/    │
│  ❌ erlmcp_debugger.erl                    → DELETE          │
│  ❌ erlmcp_profiler.erl                    → DELETE          │
│  ❌ erlmcp_evidence_path.erl               → → tcps_erlmcp/ │
│  ❌ erlmcp_recovery_manager.erl            → DELETE          │
│  ❌ erlmcp_cpu_guard.erl                   → DELETE          │
│  ❌ erlmcp_memory_guard.erl                → DELETE          │
│  ❌ erlmcp_cpu_quota.erl                   → DELETE          │
│  ❌ erlmcp_transport_registry.erl          → REMOVE          │
│  ❌ erlmcp_transport_discovery.erl         → REMOVE          │
│  ❌ erlmcp_transport_pipeline.erl          → REMOVE          │
│  ❌ erlmcp_transport_validation.erl        → REMOVE          │
│  ❌ erlmcp_pool_strategy.erl               → REMOVE          │
│  ❌ erlmcp_transport_adapter.erl           → REMOVE          │
│                                                               │
│  SIMPLIFY & STANDARDIZE (60 modules, middle 60%)             │
│  ─────────────────────────────────────────────────────────── │
│  🔧 Standardize error handling (erlmcp_errors.hrl)           │
│  🔧 Add type specs (100% coverage for Tier 1/2)              │
│  🔧 Complete documentation (@doc for all public APIs)        │
│  🔧 Consolidate observability (27 → 10 modules)              │
│  🔧 Complete auth TODOs (JWT, OAuth2, mTLS)                  │
│  🔧 Complete resource subscriptions (5 TODOs)                │
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

---

## Decision Tree: Should I Keep This Module?

```
                    ┌─────────────────────┐
                    │   Does this module  │
                    │   deliver value?    │
                    └─────────┬───────────┘
                              │
                 ┌────────────┴────────────┐
                 │                         │
              YES │                       NO │
                 │                         │
        ┌────────▼────────┐        ┌───────▼────────┐
        │ Is it complete? │        │ DELETE IT NOW │
        └────────┬────────┘        └────────────────┘
                 │
        ┌────────┴────────┐
        │                 │
    YES │               NO │
        │                 │
┌───────▼────────┐  ┌─────▼──────────────┐
│   KEEP IT      │  │ Can it be          │
│   & OPTIMIZE   │  │ completed?         │
└────────────────┘  └───┬────────────────┘
                           │
                  ┌────────┴────────┐
                  │                 │
               YES │              NO │
                  │                 │
         ┌────────▼────────┐  ┌────▼─────────┐
         │ Complete it     │  │ DELETE IT    │
         │ within 30 days  │  │ OR MOVE TO   │
         └─────────────────┘  │ examples/    │
                             └───────────────┘

SPECIAL CASES:
• If unused in 6 months → DELETE
• If experimental → MOVE to examples/
• If redundant with OTP → REMOVE
• If security-critical & incomplete → COMPLETE OR DELETE
```

---

## Conclusion

**The 80/20 consolidation plan identifies**:
- **21 modules** (20%) that deliver **80% of production value** → KEEP & OPTIMIZE
- **25 modules** (bottom 20%) that consume **80% of maintenance effort** → REMOVE or CONSOLIDATE
- **60 modules** (middle 60%) that need **standardization** → SIMPLIFY & STANDARDIZE

**Expected outcome**:
- **-30% LOC** (35,549 → 25,000)
- **-29% modules** (106 → 75)
- **-100% TODOs** (24 → 0)
- **+50% test coverage** (60% → 90%)
- **+43% type spec coverage** (70% → 100%)

**The goal**: Simpler, clearer, more maintainable codebase where **every module earns its keep**.

---

**Next steps**:
1. Review this plan with technical leadership
2. Prioritize phases based on team capacity
3. Start with Phase 1 (cleanup) - QUICK WIN
4. Track metrics throughout consolidation

**Documents**:
- Full analysis: `docs/80_20_CONSOLIDATION_PLAN.md`
- Action items: `docs/80_20_PRIORITIZED_ACTIONS.md`
- Visual summary: `docs/80_20_VISUAL_SUMMARY.md` (this file)
