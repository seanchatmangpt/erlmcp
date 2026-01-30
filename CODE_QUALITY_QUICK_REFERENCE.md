# Code Quality Quick Reference

## 🚨 Critical Issues (Fix Immediately)

| Issue | File | Line | Fix Time | Risk |
|-------|------|------|----------|------|
| Unsupervised spawn | `erlmcp_cache.erl` | 404 | 1 hour | HIGH |
| Unsupervised spawn_link | `erlmcp_batch.erl` | 372 | 1 hour | HIGH |
| Infinity timeout | `erlmcp_client.erl` | 105 | 30 min | MEDIUM |

## 📊 Code Metrics

```
Lines of Code by Module (Top 5):
  2,040 lines - erlmcp_server.erl        ❌ REFACTOR NEEDED
  1,253 lines - erlmcp_capabilities.erl  ⚠️  REFACTOR RECOMMENDED
    845 lines - erlmcp_rate_limiter.erl  ✅ OK
    818 lines - erlmcp_cache.erl         ✅ OK
    742 lines - erlmcp_client.erl        ✅ OK

Tech Debt:
  35+ backup/temp files                  ❌ DELETE
  17  TODO comments                      ⚠️  TRACK IN ISSUES

Process Management:
  249 links                              ⚠️  HIGH (consider monitors)
  8   monitors                           ❌ LOW (should be more)
  2   unsupervised spawns                ❌ CRITICAL
```

## 🎯 Top 3 Refactoring Targets

### 1. erlmcp_server.erl (2,040 lines)
**Split into:**
- `erlmcp_server_resources.erl` (resource/template management)
- `erlmcp_server_tools.erl` (tool management)
- `erlmcp_server_prompts.erl` (prompt management)
- `erlmcp_server_subscriptions.erl` (subscription management)
- `erlmcp_server_protocol.erl` (MCP message handlers)

### 2. erlmcp_capabilities.erl (1,253 lines)
**Split into:**
- `erlmcp_capabilities_core.erl` (negotiation + state)
- `erlmcp_capabilities_extraction.erl` (parsing)
- `erlmcp_capabilities_validation.erl` (validation)
- `erlmcp_capabilities_conversion.erl` (conversion)

### 3. Process Spawn Hygiene
**Replace:**
```erlang
spawn(fun() -> ... end)              % ❌
spawn_link(fun() -> ... end)         % ❌
```
**With:**
```erlang
supervisor:start_child(worker_sup, ...) % ✅
```

## 📋 Immediate Action Checklist

- [ ] Fix `erlmcp_cache.erl:404` - Add supervision for cache warming
- [ ] Fix `erlmcp_batch.erl:372` - Use poolboy or supervised workers
- [ ] Fix `erlmcp_client.erl:105` - Add 30s timeout to initialize
- [ ] Archive all `.bak`, `.orig`, `.tmp` files to `attic/`
- [ ] Convert 17 TODOs to GitHub issues
- [ ] Document supervisor restart strategies
- [ ] Run: `rebar3 xref && rebar3 dialyzer`

## 🔍 Quick Audit Commands

```bash
# Find unsupervised spawns
grep -rn "spawn(" apps/*/src/*.erl | grep -v "spawn_link"

# Find infinity timeouts
grep -rn "infinity" apps/*/src/*.erl | grep "gen_server:call"

# Find large modules (>800 lines)
find apps/*/src -name "*.erl" -exec wc -l {} \; | sort -rn | head -10

# Count backup files
find apps -name "*.bak*" -o -name "*.orig" -o -name "*.tmp" | wc -l

# Find link vs monitor usage
grep -rc "monitor(process" apps/*/src/ | grep -v ":0"
grep -rc "link(" apps/*/src/ | grep -v ":0"
```

## ✅ Quality Gates

**Before Commit:**
- [ ] 0 unsupervised spawns
- [ ] 0 infinity timeouts (except explicit cases)
- [ ] Dialyzer clean
- [ ] Xref clean

**Before Merge:**
- [ ] All tests pass
- [ ] Coverage ≥80%
- [ ] No TODOs without GitHub issues
- [ ] All exports have specs

**Before Release:**
- [ ] No .bak/.tmp files
- [ ] All supervisors documented
- [ ] All critical paths use monitors
- [ ] Benchmarks within 10% of baseline

## 📈 Improvement Roadmap

**Week 1:** Critical fixes (spawns, timeouts)
**Week 2:** Clean tech debt (backup files, TODOs)
**Week 3-4:** Refactor erlmcp_server.erl
**Week 5:** Refactor erlmcp_capabilities.erl
**Week 6:** Monitor migration + documentation

**Target Grade:** A (from current B)
