# erlmcp 80/20 Consolidation Quick Reference

## Codebase Stats
- **Total Modules:** 188
- **Total Lines (src):** ~70,759
- **Code Duplication:** 60% (error handling), 40% (validators), 35% (rate limiting)
- **Test Modules:** 58

## Top 5 Consolidation Opportunities (80% of value)

### CRITICAL - Rate Limiting Consolidation
**Impact:** 1,200+ lines saved | Risk: Medium | Effort: 20-25 hours

4 rate limiting modules → 1 unified + 3 wrappers
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_rate_limiter.erl` (845 lines)
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_auth_rate_limiter.erl` (604 lines)
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_connection_limiter.erl` (500 lines)
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_rate_limit_middleware.erl` (565 lines)

**Action:** Create `erlmcp_rate_limiter_unified.erl` with generic token bucket, merge others into wrappers

---

### HIGH - Error Formatting Utility
**Impact:** 400+ lines saved | Risk: Low | Effort: 15-20 hours

69 `format_error` functions → 1 utility module

**Action:** Create `erlmcp_error_util.erl` and replace duplicates across:
- erlmcp_schema_validator
- erlmcp_prompt_argument_validator
- erlmcp_server
- erlmcp_capabilities
- All pricing validators
- Transport modules

---

### HIGH - Validator Consolidation
**Impact:** 800+ lines saved | Risk: Low-Medium | Effort: 25-30 hours

5+ validator modules → 2-3 unified

**Action:**
1. Create `erlmcp_validator_base.erl` with common patterns (regex, range, dependencies)
2. Split erlmcp_schema_validator (1253 lines!) into core + helpers
3. Keep domain-specific validators as thin wrappers

**Target:** Reduce erlmcp_schema_validator from 1,253 → 700 lines

---

### HIGH - Split erlmcp_server.erl
**Impact:** Cognitive clarity | Risk: Low | Effort: 30-35 hours

2,040 lines → 500 lines + 3 new modules

**Action:**
1. Extract `erlmcp_server_resources.erl`
2. Extract `erlmcp_server_tools.erl`
3. Extract `erlmcp_server_prompts.erl`
4. Keep erlmcp_server.erl as thin coordinator

---

### MEDIUM - Monitor/Health Consolidation
**Impact:** 600+ lines saved | Risk: Medium | Effort: 25-30 hours

7 monitoring modules → 3-4 unified

**Action:**
1. Enhance `erlmcp_health_monitor.erl` as unified system
2. Create `erlmcp_component_monitor.erl` as generic monitor
3. Delegate specialized monitors (transport, SLA, connection)

**Modules Affected:**
- erlmcp_health_monitor.erl (716 lines)
- erlmcp_process_monitor.erl (409 lines)
- erlmcp_connection_monitor.erl (500 lines)
- erlmcp_transport_health.erl (331 lines)
- erlmcp_sla_monitor.erl (414 lines)
- erlmcp_node_monitor.erl (174 lines)

---

## Quick Win (LOW EFFORT, IMMEDIATE VALUE)

### Dead Code Removal
**Impact:** 50 lines | Risk: V.Low | Effort: 2-3 hours

- Delete `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_rate_limiter_v2.erl` (incomplete fragment)
- Run dialyzer to find unused exports
- Clean up immediately

---

## Summary Consolidation Table

| Priority | Change | Current | Target | Saves | Risk |
|----------|--------|---------|--------|-------|------|
| 1 | Rate limiting unification | 4 modules | 1 + wrappers | 1,200+ | Medium |
| 1 | Error formatting utility | 69 instances | 1 module | 400+ | Low |
| 1 | Validator consolidation | 5+ modules | 2-3 | 800+ | Low-Med |
| 2 | erlmcp_server split | 2,040 lines | 500 + 3 new | Clarity | Low |
| 2 | Monitor unification | 7 modules | 3-4 | 600+ | Medium |
| 2 | Registry consolidation | 3 modules | 2 | 150+ | Low |
| 2 | Session consolidation | 4 modules | 2 | 100+ | Low |
| 3 | Message handling | 4 modules | 2 | 300+ | Low |
| 3 | Notifier consolidation | 2 modules | 1 | 200+ | Low |
| 3 | Pricing module | 12 modules | 8-9 | 100-200 | Low |
| 4 | Dead code removal | Various | Clean | 50+ | V.Low |
| 4 | Supervisor flattening | 8 supervisors | 6-7 | 100+ | Medium |
| | **TOTAL** | **188 modules** | **~160** | **~4,000+** | - |

---

## Implementation Timeline

```
Week 1-2: Phase 1 - Foundation (Error & Validator utilities)
    ↓
Week 2-3: Phase 2 - Rate Limiting Unification
    ↓
Week 3-4: Phase 3 - Large Module Refactoring (server, registry, session)
    ↓
Week 4-5: Phase 4 - Monitoring & Message Handling
    ↓
Week 5: Phase 5 - Cleanup & Final Validation

Total: ~3 weeks for 2-person team
```

---

## Quality Targets

### Before
- Modules: 188
- LOC: ~70,759
- Duplication: 60% errors, 40% validators, 35% rate limiting
- Largest module: erlmcp_server.erl (2,040 lines)

### After
- Modules: ~160 (12% reduction)
- LOC: ~66,700 (6% reduction)
- Duplication: <10% errors, <15% validators, <10% rate limiting
- Largest module: erlmcp_server.erl (500 lines, 75% smaller)
- Cognitive complexity: 30-40% reduction

---

## Next Steps

1. **Review** CONSOLIDATION_ANALYSIS_80_20.md for full details
2. **Start Phase 1:** Create error_util and validator_base modules
3. **Write tests** for new utilities before refactoring
4. **Refactor incrementally** with regression testing
5. **Document** new architecture in docs/

---

## File Locations

**Analysis Document:** `/home/user/erlmcp/CONSOLIDATION_ANALYSIS_80_20.md`
**This Quick Reference:** `/home/user/erlmcp/CONSOLIDATION_QUICK_REFERENCE.md`

**Key Source Files:**
- Rate limiters: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_rate_limiter*.erl`
- Validators: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_*validator*.erl`
- Monitors: `/home/user/erlmcp/apps/erlmcp_observability/src/erlmcp_*monitor*.erl`
- Large modules: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`
