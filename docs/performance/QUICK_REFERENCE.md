# ERLMCP PERFORMANCE OPTIMIZATION - QUICK REFERENCE

**Target:** 2.52M → 3.5M msg/s (+41% improvement in 4 weeks)

---

## At-a-Glance: 4-Week Plan

```
┌────────────────────────────────────────────────────────────────────┐
│ WEEK 1: Registry Benchmark Fix (P1 - Critical)                    │
├────────────────────────────────────────────────────────────────────┤
│ Problem:  Synthetic latencies (rand:uniform) invalidate baseline  │
│ Fix:      Real worker message collection                          │
│ Impact:   Accurate baseline (currently shows fake 553K msg/s)     │
│ Files:    bench/erlmcp_bench_core_ops.erl (lines 195-199)        │
│ Test:     erlmcp_bench_core_ops:run(<<"core_ops_100k">>)         │
└────────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────┐
│ WEEK 2: jiffy Integration (P1 - Critical)                         │
├────────────────────────────────────────────────────────────────────┤
│ Problem:  jsx is 2-3x slower (pure Erlang vs NIF)                 │
│ Fix:      Switch to jiffy:encode/1                                │
│ Impact:   2-3x JSON performance, 8.5% → 3% integration overhead   │
│ Files:    apps/erlmcp_core/src/erlmcp_json_rpc.erl (lines 389)   │
│ Test:     erlmcp_bench_integration:run_all()                      │
│ Gain:     +25% throughput (2.60M → 3.15M msg/s)                   │
└────────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────┐
│ WEEK 3: ETS Request Correlation (P2 - High Impact)                │
├────────────────────────────────────────────────────────────────────┤
│ Problem:  maps:take/2 is O(log N), slow at >1000 concurrent       │
│ Fix:      Use ETS for O(1) lookup                                 │
│ Impact:   10-20% faster response handling                         │
│ Files:    apps/erlmcp_core/src/erlmcp_client.erl (lines 53, 618) │
│ Test:     erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>) │
│ Gain:     +10% throughput (3.15M → 3.40M msg/s)                   │
└────────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────┐
│ WEEK 4: State & Binary Optimization (P3 - Medium Impact)          │
├────────────────────────────────────────────────────────────────────┤
│ Tasks:    1. Percentile calc (lists:nth → array:get) = 100x       │
│           2. Pre-allocate base maps = 5-10%                        │
│           3. Refactor large state records = 5-10%                  │
│           4. Fix metrology ambiguity (throughput_std_dev unit)    │
│ Impact:   5-10% cumulative improvement                            │
│ Files:    bench/erlmcp_bench_core_ops.erl (line 364-368)         │
│           apps/erlmcp_core/src/erlmcp_json_rpc.erl (line 391)    │
│           apps/erlmcp_core/src/erlmcp_server.erl (line 47-66)    │
│ Gain:     +5% throughput (3.40M → 3.55M msg/s)                    │
└────────────────────────────────────────────────────────────────────┘
```

---

## Performance Impact Matrix

| Optimization | Priority | Effort | Impact | Timeline | Files |
|--------------|----------|--------|--------|----------|-------|
| **jsx → jiffy** | P1 | 4h | **2-3x JSON** | Week 2 | erlmcp_json_rpc.erl |
| **Registry bug fix** | P1 | 6h | **Accurate baseline** | Week 1 | erlmcp_bench_core_ops.erl |
| **maps → ETS** | P2 | 8h | **10-20% high-concurrency** | Week 3 | erlmcp_client.erl |
| **Percentile array** | P2 | 2h | **100x for 1M samples** | Week 4 | erlmcp_bench_core_ops.erl |
| **Pre-allocate maps** | P3 | 2h | **5-10%** | Week 4 | erlmcp_json_rpc.erl |
| **Refactor state** | P3 | 6h | **5-10%** | Week 4 | erlmcp_server.erl |

---

## Benchmark Commands

### Baseline (Before ANY Changes)
```bash
# Capture baseline
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
erlmcp_bench_network_real:run(<<"tcp_sustained_10k_1kib">>).
erlmcp_bench_integration:run_all().

# Save results
cp bench/results/*.json bench/results/baseline/
```

### After Each Week
```bash
# Week 1: Validate registry fix
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).

# Week 2: Validate jiffy
erlmcp_bench_integration:run_all().
erlmcp_bench_network_real:run(<<"tcp_sustained_10k_1kib">>).

# Week 3: Validate ETS
erlmcp_bench_network_real:run(<<"tcp_sustained_10k_1kib">>).

# Week 4: Full validation
./scripts/bench/run_all_benchmarks.sh
```

### Regression Check
```bash
make benchmark-quick && ./scripts/bench/regression_check.sh
```

---

## Code Snippets (Copy-Paste Ready)

### Week 2: jiffy Integration
```erlang
%% File: apps/erlmcp_core/src/erlmcp_json_rpc.erl
%% Line: 389

%% BEFORE:
encode_message(Message) ->
    Map = build_message_map(Message),
    jsx:encode(Map).

%% AFTER:
encode_message(Message) ->
    Map = build_message_map(Message),
    jiffy:encode(Map).  % 2-3x faster!
```

### Week 3: ETS Request Correlation
```erlang
%% File: apps/erlmcp_core/src/erlmcp_client.erl
%% Lines: 53, 618

%% BEFORE:
-record(state, {
    pending_requests = #{} :: #{request_id() => {atom(), pid()}},
    ...
}).

handle_response(Id, Result, State) ->
    case maps:take(Id, State#state.pending_requests) of
        {{RequestType, From}, NewPending} ->
            gen_server:reply(From, Result),
            ...

%% AFTER:
-record(state, {
    pending_ets :: ets:tid(),
    pending_count = 0 :: non_neg_integer(),
    ...
}).

init([...]) ->
    PendingEts = ets:new(erlmcp_client_pending, [set, private]),
    {ok, #state{pending_ets = PendingEts, ...}}.

handle_response(Id, Result, State) ->
    case ets:take(State#state.pending_ets, Id) of
        [{Id, {RequestType, From}}] ->
            gen_server:reply(From, Result),
            NewCount = State#state.pending_count - 1,
            ...
```

### Week 4: Percentile Optimization
```erlang
%% File: bench/erlmcp_bench_core_ops.erl
%% Lines: 364-368

%% BEFORE:
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percentile))),
    lists:nth(Index, SortedList).  % O(N) - SLOW!

%% AFTER:
percentile(SortedList, Percentile) ->
    Arr = array:from_list(SortedList),  % O(N) once
    Len = array:size(Arr),
    Index = max(0, min(Len-1, round(Len * Percentile) - 1)),
    array:get(Index, Arr).  % O(1) - FAST!
```

---

## Expected Results Timeline

```
Baseline (Week 0):     2.52M msg/s ━━━━━━━━━━━━━━━━━━━━━━ 100%
Week 1 (Registry):     2.60M msg/s ━━━━━━━━━━━━━━━━━━━━━━━ 103%
Week 2 (jiffy):        3.15M msg/s ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 125%
Week 3 (ETS):          3.40M msg/s ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 135%
Week 4 (State/Binary): 3.55M msg/s ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 141%
                                                   ▲
                                           TARGET ACHIEVED
```

---

## Quality Gate Checklist

Before declaring "DONE" after each week:

- [ ] **Compilation:** `TERM=dumb rebar3 compile` (0 errors)
- [ ] **Tests:** `rebar3 eunit` (100% pass)
- [ ] **Dialyzer:** `rebar3 dialyzer` (0 new warnings)
- [ ] **Xref:** `rebar3 xref` (0 new issues)
- [ ] **Benchmarks:** Run and validate (no regression)
- [ ] **Documentation:** Update optimization_plan.md with "Actual" results

---

## Risk Mitigation

### jiffy NIF Risk
```erlang
%% Add feature flag for rollback
-define(USE_JIFFY, true).

encode_message(Message) ->
    Map = build_message_map(Message),
    case ?USE_JIFFY of
        true -> jiffy:encode(Map);
        false -> jsx:encode(Map)  % Fallback
    end.
```

### ETS Leak Risk
- Use `private` ETS (auto-deleted on process death)
- Test with chaos: `erlmcp_bench_chaos:run(<<"chaos_process_crash">>)`
- Monitor: `length(ets:all())` should not grow

### State Refactor Risk
- Run Dialyzer: `rebar3 dialyzer` (catch type mismatches)
- Run full test suite: `rebar3 eunit && rebar3 ct`
- Update incrementally (one module at a time)

---

## Success Metrics

| Metric | Baseline | Target | Pass/Fail |
|--------|----------|--------|-----------|
| Core throughput | 2.52M msg/s | 3.0M msg/s | TBD |
| JSON encoding (1KB) | 45us p95 | 16us p95 | TBD |
| JSON encoding (100KB) | 1850us p95 | 595us p95 | TBD |
| Request correlation (10K) | 100us p95 | 10us p95 | TBD |
| Percentile calc (1M) | 1000ms | 10ms | TBD |
| Integration overhead | 8.5% | 3.0% | TBD |
| TCP sustained (10K) | 100K msg/s | 120K msg/s | TBD |

---

## Key Files to Modify

```
apps/erlmcp_core/src/
├── erlmcp_json_rpc.erl      (Week 2: jiffy, Week 4: pre-allocate maps)
├── erlmcp_client.erl         (Week 3: ETS request correlation)
└── erlmcp_server.erl         (Week 4: refactor state record)

bench/
├── erlmcp_bench_core_ops.erl (Week 1: registry fix, Week 4: percentile)
└── erlmcp_bench_stress.erl   (Week 4: metrology unit fix)

rebar.config                   (Week 2: add jiffy dependency)
```

---

## Contact & References

- **Full Plan:** [optimization_plan.md](optimization_plan.md) (1283 lines)
- **Index:** [README.md](README.md)
- **Analysis:** [performance_analysis_2026-01-30.md](../performance_analysis_2026-01-30.md)
- **Agent:** erlang-performance
- **Date:** 2026-01-30

---

**REMEMBER:** Always run baseline benchmarks BEFORE any code changes!

```bash
# Save this as your first step:
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
cp bench/results/*.json bench/results/baseline/
```
