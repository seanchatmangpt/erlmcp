# 80/20 Consolidation Visualization

## Current State Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    erlmcp (Current State)                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌────────────────┐  ┌──────────────────┐  ┌─────────────────┐ │
│  │  Core (1.5MB)  │  │ Transports (492K)│  │ Observability  │ │
│  │                │  │                  │  │   (384K)        │ │
│  │ • 59 modules   │  │ • 21 modules     │  │ • 17 modules    │ │
│  │ • 68 .broken   │  │ • stdio/tcp/http │  │ • metrics       │ │
│  │ • duplicates   │  │ • ws/sse/pool    │  │ • tracing       │ │
│  └────────────────┘  └──────────────────┘  │ • dashboard     │ │
│                                           └─────────────────┘ │
│                                                                  │
│  Technical Debt:                                                │
│  • 297 total modules (high complexity)                          │
│  • 68 broken/backup files                                       │
│  • Duplicate implementations (rate_limiter, validators)         │
│  • Legacy migration code                                        │
│  • Scattered test files                                         │
└─────────────────────────────────────────────────────────────────┘
```

## Target State Architecture (80/20)

```
┌─────────────────────────────────────────────────────────────────┐
│                   erlmcp (Target State)                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌────────────────┐  ┌──────────────────┐  ┌─────────────────┐ │
│  │  Core (750KB)  │  │ Transports (400K)│  │ Observability  │ │
│  │                │  │                  │  │   (300K)        │ │
│  │ • 30 modules   │  │ • 15 modules     │  │ • 12 modules    │ │
│  │ • 0 .broken    │  │ • stdio/tcp/http │  │ • metrics       │ │
│  │ • consolidated │  │ • ws/sse         │  │ • tracing       │ │
│  └────────────────┘  └──────────────────┘  │ • dashboard     │ │
│                                           └─────────────────┘ │
│                                                                  │
│  Improvements:                                                  │
│  • ~150 total modules (50% reduction)                           │
│  • 0 broken files                                               │
│  • Single source of truth per concern                           │
│  • Clear module boundaries                                      │
│  • Comprehensive test coverage                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Consolidation Flow

```
Phase 1: Foundation (Weeks 1-2)
┌─────────────────────────────────────────────────────────┐
│                                                         │
│  68 .broken files                                      │
│       │                                                │
│       ├─→ Restore (2): cache.erl, schema_validator.erl │
│       ├─→ Consolidate (4): validators, rate_limiter    │
│       ├─→ Archive (60): tests, docs → attic/          │
│       └─→ Delete (2): legacy duplicates                │
│                                                         │
│  Result: Clean codebase, 0 .broken files               │
└─────────────────────────────────────────────────────────┘
              ↓
Phase 2: Core Consolidation (Weeks 3-5)
┌─────────────────────────────────────────────────────────┐
│                                                         │
│  Cache (3 modules)                                     │
│       ├─→ erlmcp_cache.erl (general-purpose)           │
│       ├─→ erlmcp_icon_cache.erl (delete)               │
│       └─→ Result: 1 unified cache module               │
│                                                         │
│  Transports (21 modules)                               │
│       ├─→ HTTP (3 → 1): http + http_server + security │
│       ├─→ Pool (3 → 1): pool + strategy + manager      │
│       └─→ Result: 15 transport modules                 │
│                                                         │
│  Validators (5 modules)                                │
│       ├─→ prompt_validator.erl                         │
│       ├─→ schema_validator.erl                         │
│       ├─→ uri_validator.erl                            │
│       └─→ erlmcp_validator.erl (unified)               │
│                                                         │
│  Result: 50% reduction in core modules                 │
└─────────────────────────────────────────────────────────┘
              ↓
Phase 3: Observability (Weeks 6-7)
┌─────────────────────────────────────────────────────────┐
│                                                         │
│  Tracing (7 modules)                                   │
│       ├─→ Remove duplicate tracing.erl                 │
│       ├─→ Consolidate exporters (5 → 1)                │
│       └─→ Result: Single pluggable exporter            │
│                                                         │
│  Metrics (3 modules)                                   │
│       └─→ Keep as-is (good separation)                 │
│                                                         │
│  Dashboard (2 modules)                                 │
│       └─→ Keep as-is, add auth                         │
│                                                         │
│  Result: Streamlined observability stack               │
└─────────────────────────────────────────────────────────┘
              ↓
Phase 4: Testing & Docs (Weeks 8-9)
┌─────────────────────────────────────────────────────────┐
│                                                         │
│  Tests (68 broken files)                               │
│       ├─→ Port (5): Critical tests to modern framework │
│       ├─→ Archive (60): Legacy tests → attic/          │
│       └─→ Delete (3): Experimental tests               │
│                                                         │
│  Documentation (100+ files)                            │
│       ├─→ Consolidate duplicates                       │
│       ├─→ Create hierarchy (getting-started, api, ...) │
│       └─→ Archive historical docs                      │
│                                                         │
│  Result: 80% coverage, clear docs                      │
└─────────────────────────────────────────────────────────┘
              ↓
Phase 5: Production Rollout (Weeks 10-12)
┌─────────────────────────────────────────────────────────┐
│                                                         │
│  Staging (Week 10)                                     │
│       ├─→ Full test suite                             │
│       ├─→ Performance benchmarks                      │
│       ├─→ Load testing                                │
│       └─→ 24-hour soak test                           │
│                                                         │
│  Canary (Week 11)                                      │
│       ├─→ 10% traffic                                 │
│       ├─→ Monitor metrics                             │
│       ├─→ Validate no regression                      │
│       └─→ Auto-rollback on issues                     │
│                                                         │
│  Gradual Rollout (Week 12)                             │
│       ├─→ Day 1: 10%                                  │
│       ├─→ Day 2: 25%                                  │
│       ├─→ Day 3: 50%                                  │
│       ├─→ Day 4: 75%                                  │
│       └─→ Day 5: 100%                                 │
│                                                         │
│  Result: Production deployment, zero incidents         │
└─────────────────────────────────────────────────────────┘
```

## Module Consolidation Map

### Before (Current State)
```
Cache (3 modules)
├── erlmcp_cache.erl.broken          [BROKEN]
├── erlmcp_icon_cache.erl             [ACTIVE]
└── (ad-hoc caching in other modules) [SCATTERED]

Validators (5 modules)
├── erlmcp_prompt_argument_validator.erl.broken   [BROKEN]
├── erlmcp_schema_validator.erl.broken            [BROKEN]
├── erlmcp_uri_validator.erl.broken               [BROKEN]
├── erlmcp_transport_validation.erl               [ACTIVE]
└── erlmcp_http_header_validator.erl              [ACTIVE]

Rate Limiters (2 modules)
├── erlmcp_rate_limiter.erl          [ACTIVE]
└── erlmcp_rate_limiter_v2.erl.broken [BROKEN - DUPLICATE]

Transports (21 modules)
├── erlmcp_transport_stdio.erl       [ACTIVE - KEEP]
├── erlmcp_transport_tcp.erl         [ACTIVE - KEEP]
├── erlmcp_transport_http.erl        [ACTIVE - MERGE HTTP_SERVER]
├── erlmcp_transport_http_server.erl [ACTIVE - MERGE INTO HTTP]
├── erlmcp_transport_ws.erl          [ACTIVE - KEEP]
├── erlmcp_transport_sse.erl         [ACTIVE - KEEP]
├── erlmcp_transport_pool.erl        [ACTIVE - MERGE POOL MGR]
├── erlmcp_pool_strategy.erl         [ACTIVE - MERGE INTO POOL]
├── erlmcp_pool_manager.erl          [ACTIVE - MERGE INTO POOL]
├── erlmcp_transport_adapter.erl     [ACTIVE - KEEP]
├── erlmcp_transport_behavior.erl    [ACTIVE - KEEP]
├── erlmcp_transport_sup.erl         [ACTIVE - KEEP]
├── erlmcp_transport_registry.erl    [ACTIVE - KEEP]
├── erlmcp_transport_discovery.erl   [ACTIVE - KEEP]
├── erlmcp_transport_health.erl      [ACTIVE - KEEP]
├── erlmcp_transport_pipeline.erl    [ACTIVE - KEEP]
├── erlmcp_transport_validation.erl  [ACTIVE - MERGE INTO VALIDATOR]
├── erlmcp_security_headers.erl      [ACTIVE - MERGE INTO HTTP]
├── erlmcp_http_header_validator.erl [ACTIVE - MERGE INTO VALIDATOR]
└── erlmcp_origin_validator.erl      [ACTIVE - MERGE INTO VALIDATOR]

Observability (17 modules)
├── erlmcp_metrics.erl               [ACTIVE - KEEP]
├── erlmcp_metrics_server.erl        [ACTIVE - KEEP]
├── erlmcp_metrics_aggregator.erl    [ACTIVE - KEEP]
├── erlmcp_tracing.erl               [ACTIVE - REMOVE DUPLICATE]
├── erlmcp_otel.erl                  [ACTIVE - KEEP]
├── erlmcp_otel_jaeger.erl           [ACTIVE - MERGE INTO EXPORTER]
├── erlmcp_otel_datadog.erl          [ACTIVE - MERGE INTO EXPORTER]
├── erlmcp_otel_honeycomb.erl        [ACTIVE - MERGE INTO EXPORTER]
├── erlmcp_otel_middleware.erl       [ACTIVE - KEEP]
├── erlmcp_dashboard_server.erl      [ACTIVE - KEEP]
├── erlmcp_dashboard_http_handler.erl [ACTIVE - KEEP]
├── erlmcp_receipt_chain.erl         [ACTIVE - KEEP]
├── erlmcp_evidence_path.erl         [ACTIVE - KEEP]
├── erlmcp_health_monitor.erl        [ACTIVE - KEEP]
├── erlmcp_recovery_manager.erl      [ACTIVE - KEEP]
├── erlmcp_debugger.erl              [ACTIVE - KEEP]
└── erlmcp_profiler.erl              [ACTIVE - KEEP]
```

### After (Target State)
```
Cache (1 module)
└── erlmcp_cache.erl                 [ACTIVE - UNIFIED]
    ├── Generic caching (TTL, stats)
    ├── Icon caching (integrated)
    └── Telemetry hooks

Validators (1 module)
└── erlmcp_validator.erl             [ACTIVE - UNIFIED]
    ├── validate_prompt/1
    ├── validate_schema/2
    ├── validate_uri/1
    └── validate_transport/1

Rate Limiters (1 module)
└── erlmcp_rate_limiter.erl          [ACTIVE - KEEP]
    └── (v2 deleted - v1 sufficient)

Transports (15 modules)
├── erlmcp_transport_stdio.erl       [ACTIVE - KEEP]
├── erlmcp_transport_tcp.erl         [ACTIVE - KEEP]
├── erlmcp_transport_http.erl        [ACTIVE - MERGED]
│   └── Includes: http_server, security_headers
├── erlmcp_transport_ws.erl          [ACTIVE - KEEP]
├── erlmcp_transport_sse.erl         [ACTIVE - KEEP]
├── erlmcp_transport_pool.erl        [ACTIVE - MERGED]
│   └── Includes: pool_strategy, pool_manager
├── erlmcp_transport_adapter.erl     [ACTIVE - KEEP]
├── erlmcp_transport_behavior.erl    [ACTIVE - KEEP]
├── erlmcp_transport_sup.erl         [ACTIVE - KEEP]
├── erlmcp_transport_registry.erl    [ACTIVE - KEEP]
├── erlmcp_transport_discovery.erl   [ACTIVE - KEEP]
├── erlmcp_transport_health.erl      [ACTIVE - KEEP]
├── erlmcp_transport_pipeline.erl    [ACTIVE - KEEP]
├── erlmcp_client_transport.erl      [ACTIVE - KEEP]
└── (validation moved to erlmcp_validator.erl)

Observability (12 modules)
├── erlmcp_metrics.erl               [ACTIVE - KEEP]
├── erlmcp_metrics_server.erl        [ACTIVE - KEEP]
├── erlmcp_metrics_aggregator.erl    [ACTIVE - KEEP]
├── erlmcp_tracing.erl               [ACTIVE - CONSOLIDATED]
├── erlmcp_otel.erl                  [ACTIVE - KEEP]
├── erlmcp_otel_exporter.erl         [ACTIVE - UNIFIED]
│   └── Backends: jaeger, datadog, honeycomb, otlp
├── erlmcp_otel_middleware.erl       [ACTIVE - KEEP]
├── erlmcp_dashboard_server.erl      [ACTIVE - KEEP]
├── erlmcp_dashboard_http_handler.erl [ACTIVE - KEEP]
├── erlmcp_receipt_chain.erl         [ACTIVE - KEEP]
├── erlmcp_evidence_path.erl         [ACTIVE - KEEP]
├── erlmcp_health_monitor.erl        [ACTIVE - KEEP]
├── erlmcp_recovery_manager.erl      [ACTIVE - KEEP]
├── erlmcp_debugger.erl              [ACTIVE - KEEP]
└── erlmcp_profiler.erl              [ACTIVE - KEEP]
```

## Risk Timeline

```
Risk Level Over Time

High │
    │    R1         R2
    │    ┌─────────┐ ┌─────────┐
Med │    │ Cache   │ │Transport│
    │    └─────────┘ └─────────┘
    │                   R3         R4
Low │                   ┌─────────┐ ┌─────────┐
    │                   │ Tests   │ │  Docs   │
    │                   └─────────┘ └─────────┘
    │                                        R5         R6
    │                                        │Feature   │
    │                                        │Flags     │
    │                                        └─────────┘
    └─────────────────────────────────────────────────────►
      W1  W2  W3  W4  W5  W6  W7  W8  W9  W10 W11 W12

R1: Cache Consolidation (Week 3)
R2: Transport Consolidation (Week 5)
R3: Test Coverage (Week 8)
R4: Documentation (Week 9)
R5: Feature Flag Complexity (Week 10)
R6: Rollout Delays (Week 11-12)
```

## Success Metrics Dashboard

```
┌─────────────────────────────────────────────────────────┐
│              Consolidation Success Metrics              │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  Code Reduction                                         │
│  ┌───────────────────────────────────────────────────┐ │
│  │ Modules: 297 → 150 (50% reduction)  ████████████ │ │
│  │ LOC:     ~15K → ~10K (33% reduction) ████████    │ │
│  │ Broken: 68 → 0 (100% reduction)    ████████████ │ │
│  └───────────────────────────────────────────────────┘ │
│                                                         │
│  Performance                                           │
│  ┌───────────────────────────────────────────────────┐ │
│  │ Throughput: 43K → 43K+ (no regression) ████      │ │
│  │ Latency p99: 10ms → <10ms (no regression) ██████ │ │
│  │ Memory:    2GB → <2GB (improvement)    ████████  │ │
│  └───────────────────────────────────────────────────┘ │
│                                                         │
│  Quality                                               │
│  ┌───────────────────────────────────────────────────┐ │
│  │ Test Coverage: ?% → 80%+             ██████████  │ │
│  │ Dialyzer:      ? warnings → 0        ████████████│ │
│  │ Xref:          ? warnings → 0        ████████████│ │
│  │ Test Pass:     ?% → 100%             ████████████│ │
│  └───────────────────────────────────────────────────┘ │
│                                                         │
│  Reliability                                           │
│  ┌───────────────────────────────────────────────────┐ │
│  │ Production Incidents: 0 → 0           ████████████│ │
│  │ Rollback Success: N/A → 100%        ████████████│ │
│  │ Soak Test:       ? hrs → 168 hrs    ████████████│ │
│  └───────────────────────────────────────────────────┘ │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

## Rollout Progression

```
Production Rollout - 12 Weeks

Week 10: Staging Validation
┌─────────────────────────────────────┐
│ ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 100% │
│ ✓ Full test suite                  │
│ ✓ Performance benchmarks           │
│ ✓ Load testing                     │
│ ✓ 24-hour soak                     │
└─────────────────────────────────────┘

Week 11: Canary Deployment
┌─────────────────────────────────────┐
│ ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 10% │
│ ✓ Canary at 10%                    │
│ ✓ Monitoring all metrics           │
│ ✓ No regressions detected          │
└─────────────────────────────────────┘

Week 12: Gradual Rollout
┌─────────────────────────────────────┐
│ Day 1: ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 10%  │
│ Day 2: ████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 25%  │
│ Day 3: ██████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 50%  │
│ Day 4: ████████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ 75%  │
│ Day 5: ████████████████████████ 100% │
│                                     │
│ ✓ Automatic rollback on issues     │
│ ✓ Real-time monitoring             │
│ ✓ Zero incidents                   │
└─────────────────────────────────────┘
```

## Feature Flag Management

```
Feature Flags Timeline

Phase 1-2: Development
├── consolidated_cache: false
├── consolidated_validator: false
├── consolidated_http: false
└── consolidated_pool: false

Phase 3: Testing
├── consolidated_cache: true (test only)
├── consolidated_validator: true (test only)
├── consolidated_http: true (test only)
└── consolidated_pool: true (test only)

Phase 4: Staging
├── consolidated_cache: true
├── consolidated_validator: true
├── consolidated_http: true
└── consolidated_pool: true

Phase 5: Production (Canary)
├── consolidated_cache: true (10% traffic)
├── consolidated_validator: true (10% traffic)
├── consolidated_http: true (10% traffic)
└── consolidated_pool: true (10% traffic)

Phase 5: Production (Full Rollout)
├── consolidated_cache: true (100% traffic)
├── consolidated_validator: true (100% traffic)
├── consolidated_http: true (100% traffic)
└── consolidated_pool: true (100% traffic)

Phase 5: Post-Rollout (Cleanup)
└── Remove all feature flags (deprecated)
```

---

**Document Purpose:** Visual companion to CONSOLIDATION_8020_ROADMAP.md
**Last Updated:** 2026-01-30
**Version:** 1.0.0
