# 80/20 Consolidation Quick Reference

**TL;DR:** 12-week plan to reduce erlmcp from 297 → 150 modules (50% reduction)

---

## 🎯 Target Outcomes

| Metric | Current | Target | Improvement |
|--------|---------|--------|-------------|
| Modules | 297 | ~150 | 50% reduction |
| Broken files | 68 | 0 | 100% cleanup |
| Test coverage | ?% | 80%+ | Quality gate |
| Performance | 43K msg/s | 43K+ msg/s | No regression |

---

## 📋 5-Phase Plan

### Phase 1: Foundation (Weeks 1-2)
**Goal:** Clean up technical debt

```bash
# Audit broken files
find . -name "*.broken" -o -name "*.bak"

# Categorize:
# - RESTORE (2): cache.erl, schema_validator.erl
# - CONSOLIDATE (4): validators, rate_limiter
# - ARCHIVE (60): tests, docs → attic/
# - DELETE (2): legacy duplicates

# Validation:
rebar3 compile && rebar3 eunit  # Must pass
```

**Deliverables:**
- ✅ 0 `.broken` files in `src/`
- ✅ Consolidated validator module
- ✅ Clear consolidation patterns documented

---

### Phase 2: Core Consolidation (Weeks 3-5)
**Goal:** Unify core functional areas

#### Week 3: Cache
```erlang
% Before: 3 modules (cache.broken, icon_cache, ad-hoc)
% After: 1 module (unified cache)

-spec get(Key) -> {ok, Value} | {error, not_found}.
-spec set(Key, Value) -> ok.
-spec delete(Key) -> ok.
-spec get_stats() -> #{hit => integer(), miss => integer()}.
```

#### Week 4: Sessions
```erlang
% No code changes (already well-separated)
% Add documentation only
```

#### Week 5: Transports
```erlang
% Before: 21 modules
% After: 15 modules

% HTTP consolidation:
% - erlmcp_transport_http.erl (merged http_server)
% - erlmcp_transport_pool.erl (merged pool_manager)
```

**Deliverables:**
- ✅ Unified cache (TTL, stats)
- ✅ Consolidated transports (HTTP, pool)
- ✅ Performance: >= 43K msg/sec, p99 < 10ms

---

### Phase 3: Observability (Weeks 6-7)
**Goal:** Streamline metrics/tracing

```erlang
% OTEL exporters consolidation
% Before: jaeger.erl, datadog.erl, honeycomb.erl
% After: erlmcp_otel_exporter.erl (pluggable backends)

export(Spans, #{backend := jaeger} = Config) ->
    erlmcp_otel_jaeger:export(Spans, Config);
export(Spans, #{backend := otlp} = Config) ->
    opentelemetry_exporter:export(Spans, Config).
```

**Deliverables:**
- ✅ Single exporter module
- ✅ No loss of trace data

---

### Phase 4: Testing & Docs (Weeks 8-9)
**Goal:** 80% coverage, clear documentation

```bash
# Tests:
# - Port 5 critical tests from .broken
# - Archive 60 legacy tests
# - Delete 3 experimental tests

# Documentation:
# - Consolidate duplicates
# - Create hierarchy (getting-started/, api/, operations/)
# - Archive historical docs
```

**Deliverables:**
- ✅ Coverage >= 80%
- ✅ All tests passing
- ✅ Clear doc hierarchy

---

### Phase 5: Production Rollout (Weeks 10-12)
**Goal:** Deploy with confidence

#### Week 10: Staging
```bash
# Full validation
rebar3 compile && rebar3 eunit && rebar3 ct
make benchmark-full
make stress-test

# 24-hour soak test
```

#### Week 11: Canary (10%)
```yaml
feature_flags:
  consolidated_cache: true
  consolidated_validator: true
  consolidated_http: true
  consolidated_pool: true

# Monitor:
# - Error rate < 0.1%
# - Latency p99 < 10ms
# - Throughput > 40K msg/sec
```

#### Week 12: Gradual Rollout
```bash
# Day 1: 10%
# Day 2: 25%
# Day 3: 50%
# Day 4: 75%
# Day 5: 100%

# Auto-rollback if:
# - Error rate > 0.5%
# - Latency p99 > 20ms
# - Any process crash
```

**Deliverables:**
- ✅ Zero production incidents
- ✅ 7-day soak test passes
- ✅ All metrics stable

---

## ⚠️ Risk Management

### Critical Risks (🔴)
1. **Cache Consolidation** (Week 3)
   - Mitigation: Feature flag, parallel deployment, benchmarks

2. **Transport Regression** (Week 5)
   - Mitigation: Integration tests, gradual rollout, monitoring

### High Risks (🟡)
3. **Test Coverage Drop** (Week 8)
   - Mitigation: 80% threshold, CI/CD enforcement

4. **Documentation Gaps** (Week 9)
   - Mitigation: Tech writer review, user feedback

---

## 🚀 Quick Commands

### Build & Test
```bash
make check              # Compile + unit tests
make validate           # All quality gates
make benchmark-quick    # < 2 min
make benchmark-full     # 10-15 min
```

### Feature Flags
```erlang
% sys.config
{erlmcp, [
    {consolidated_cache, true},
    {consolidated_validator, true}
]}.
```

### Rollback
```bash
# Instant rollback via feature flags
{erlmcp, [{consolidated_cache, false}]}.

# Or via kubectl
kubectl rollout undo deployment/erlmcp
```

---

## 📊 Success Metrics

**Code Reduction:**
- [ ] 297 → 150 modules (50%)
- [ ] 68 → 0 broken files (100%)

**Performance:**
- [ ] No regression > 10% throughput
- [ ] No regression > 10% latency
- [ ] Memory stable or reduced

**Quality:**
- [ ] Coverage >= 80%
- [ ] Dialyzer warnings = 0
- [ ] Test pass rate = 100%

**Reliability:**
- [ ] Zero production incidents
- [ ] 7-day soak test passes

---

## 📞 Contacts

**Project Lead:** [Name]
**Tech Lead:** [Name]
**Slack:** #erlmcp-consolidation
**Issues:** https://github.com/erlmcp/erlmcp/issues

---

## 📚 Full Docs

- **Roadmap:** `CONSOLIDATION_8020_ROADMAP.md`
- **Visuals:** `CONSOLIDATION_8020_VISUALIZATION.md`
- **Patterns:** `CONSOLIDATION_PATTERNS.md` (to be created)

---

**Last Updated:** 2026-01-30
**Status:** ✅ Ready for Execution
