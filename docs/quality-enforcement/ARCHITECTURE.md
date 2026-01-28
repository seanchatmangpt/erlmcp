# Quality Gate Enforcement System - Architecture

**Version:** 1.0.0
**System:** erlmcp TCPS Quality Gates
**Type:** Production-Grade Quality Enforcement

## System Overview

The erlmcp Quality Gate Enforcement System is built on OTP principles with gen_server-based gate execution, ETS caching, and deterministic receipt chain for audit trails.

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                      TCPS Quality Gates                          │
│                      (gen_server)                                │
└────────────┬──────────────────────────────────────┬─────────────┘
             │                                       │
             │ Check Gates                           │ Store Receipts
             │                                       │
    ┌────────▼──────────┐                   ┌───────▼────────┐
    │  Gate Executor    │                   │ Receipt Chain  │
    │  (Sequential)     │                   │ (SHA-256)      │
    └────────┬──────────┘                   └───────┬────────┘
             │                                       │
             │                                       │
    ┌────────▼────────────────────────────────────────────┐
    │              Quality Gate Sequence                  │
    │                                                      │
    │  1. SHACL Validation    → Ontology conformance      │
    │  2. Compilation         → Zero-error build          │
    │  3. Test Execution      → 95% pass, 80% coverage    │
    │  4. Security Scan       → Zero vulnerabilities      │
    │  5. Deterministic Build → Reproducibility           │
    │  6. Quality Metrics     → Threshold compliance      │
    │  7. Release Verification→ SBOM, licenses            │
    │  8. Smoke Test          → Basic functionality       │
    └─────────────────────────────────────────────────────┘
             │
             │ Gate Failed?
             │
    ┌────────▼──────────┐
    │   Andon System    │
    │ (Stop-the-Line)   │
    └───────────────────┘
```

## Component Architecture

### 1. Core Components

#### tcps_quality_gates (gen_server)

**Responsibilities:**
- Execute quality gates sequentially
- Cache gate results in ETS
- Generate and store receipts
- Trigger Andon events on failures
- Track quality metrics

**State:**
```erlang
-record(state, {
    gate_cache :: ets:tid(),              % ETS table for result caching
    thresholds :: quality_thresholds(),   % Production quality thresholds
    receipts_dir :: string()              % Receipt storage location
}).
```

**API:**
```erlang
% Check single gate
check_gate(Gate :: gate_name(), SkuId :: sku_id()) -> gate_result().

% Check all gates (stop on first failure)
check_all_gates(SkuId :: sku_id()) ->
    {ok, [receipt()]} | {failed_at, gate_name(), violations()}.

% Get cached gate status
get_gate_status(Gate :: gate_name(), SkuId :: sku_id()) ->
    {passed, receipt()} | {failed, violations()} | not_run.

% Get aggregate quality metrics
get_quality_metrics() -> map().

% Validate stage transition
validate_stage_transition(SkuId, FromStage, ToStage) ->
    ok | {error, {blocked, term()}}.
```

#### tcps_poka_yoke

**Responsibilities:**
- Validate pricing plans (Poka-Yoke error-proofing)
- Check envelope bounds
- Validate refusal codes
- Verify evidence requirements

**Validation Rules:**
- Schema validation (required fields)
- Envelope consistency (throughput < 2x baseline, concurrent < 200K)
- Refusal code existence (erlmcp_refusal.erl)
- Evidence artifacts (SBOM, provenance, chaos, benchmarks)

#### tcps_poka_yoke_validator

**Responsibilities:**
- Runtime validation (request-level)
- Rate limiting checks
- Payload size validation
- Capability verification
- SLA enforcement

### 2. Gate Implementations

Each gate is implemented as a private function in tcps_quality_gates.erl:

```erlang
% Gate 1: SHACL Validation
-spec check_shacl_validation(sku_id(), #state{}) -> gate_result().

% Gate 2: Compilation
-spec check_compilation(sku_id(), #state{}) -> gate_result().

% Gate 3: Test Execution
-spec check_test_execution(sku_id(), #state{}) -> gate_result().

% Gate 4: Security Scan
-spec check_security_scan(sku_id(), #state{}) -> gate_result().

% Gate 5: Deterministic Build
-spec check_deterministic_build(sku_id(), #state{}) -> gate_result().

% Gate 6: Quality Metrics
-spec check_quality_metrics(sku_id(), #state{}) -> gate_result().

% Gate 7: Release Verification
-spec check_release_verification(sku_id(), #state{}) -> gate_result().

% Gate 8: Smoke Test
-spec check_smoke_test(sku_id(), #state{}) -> gate_result().
```

### 3. Data Flow

#### Gate Execution Flow

```
┌─────────────────┐
│   User/CI       │
│  Requests Gate  │
│     Check       │
└────────┬────────┘
         │
         ▼
┌──────────────────────────────────────────────┐
│  tcps_quality_gates:check_gate/2             │
│  or check_all_gates/1                        │
└────────┬─────────────────────────────────────┘
         │
         ▼
┌──────────────────────────────────────────────┐
│  Execute Gate Implementation                 │
│  (e.g., check_compilation/2)                 │
└────────┬─────────────────────────────────────┘
         │
         ▼
┌──────────────────────────────────────────────┐
│  Run External Tools                          │
│  - rebar3 compile                            │
│  - rebar3 eunit --cover                      │
│  - Security scanners                         │
│  - SHACL validator                           │
└────────┬─────────────────────────────────────┘
         │
         ▼
┌──────────────────────────────────────────────┐
│  Parse Results                               │
│  - Success/Failure                           │
│  - Violation Details                         │
│  - Metrics                                   │
└────────┬─────────────────────────────────────┘
         │
         ▼
     Gate Passed?
         │
    ┌────┴────┐
    │         │
   YES       NO
    │         │
    ▼         ▼
┌────────┐ ┌───────────────────┐
│Generate│ │ Trigger Andon     │
│Receipt │ │ Event             │
└────┬───┘ └────────┬──────────┘
     │              │
     ▼              ▼
┌─────────────────────────────────┐
│  Cache Result in ETS            │
│  Key: {gate, sku_id}            │
│  Value: {Status, Data}          │
└────────┬────────────────────────┘
         │
         ▼
┌─────────────────────────────────┐
│  Store Receipt to Disk          │
│  File: priv/receipts/RCPT-*.json│
└────────┬────────────────────────┘
         │
         ▼
┌─────────────────────────────────┐
│  Return Result to Caller        │
│  {pass, Receipt} or             │
│  {fail, Violations}             │
└─────────────────────────────────┘
```

#### Receipt Chain Flow

```
Gate 1 Execution → Receipt 1 Generated
                   ├─ receipt_id
                   ├─ gate: shacl_validation
                   ├─ sku_id
                   ├─ status: pass
                   ├─ timestamp
                   ├─ details
                   └─ SHA-256 hash
                           │
                           ▼
Gate 2 Execution → Receipt 2 Generated
                   ├─ receipt_id
                   ├─ gate: compilation
                   ├─ sku_id
                   ├─ status: pass
                   ├─ timestamp
                   ├─ details
                   ├─ previous_receipt_hash (from Receipt 1)
                   └─ SHA-256 hash
                           │
                           ▼
                         ...
                           │
                           ▼
Gate 8 Execution → Receipt 8 Generated
                   ├─ receipt_id
                   ├─ gate: smoke_test
                   ├─ sku_id
                   ├─ status: pass
                   ├─ timestamp
                   ├─ details
                   ├─ previous_receipt_hash (from Receipt 7)
                   └─ SHA-256 hash

Result: Immutable audit trail with tamper-proof hash chain
```

### 4. Integration Points

#### Andon System Integration

```erlang
% When gate fails, trigger Andon event
trigger_quality_gate_andon(Gate, SkuId, Violations) ->
    FailureType = gate_to_failure_type(Gate),
    Context = #{
        sku_id => SkuId,
        stage => gate_to_stage(Gate),
        details => #{gate => Gate, violations => Violations}
    },
    tcps_andon:trigger_andon(FailureType, Context).

% Before stage transition, check for open Andons
check_open_andons(SkuId, Stage) ->
    case tcps_andon:can_proceed_to_stage(SkuId, Stage) of
        {ok, proceed} -> {ok, proceed};
        {blocked, AndonIds} -> {blocked, AndonIds}
    end.
```

#### Receipt Chain Integration

```erlang
% Store receipt to disk and update chain
store_gate_receipt(Receipt, State) ->
    ReceiptId = maps:get(receipt_id, Receipt),
    Filename = binary_to_list(ReceiptId) ++ ".json",
    FullPath = filename:join(State#state.receipts_dir, Filename),

    JsonBin = jsx:encode(Receipt),
    file:write_file(FullPath, JsonBin),

    % Update receipt chain
    tcps_receipt_chain:append_receipt(Receipt).
```

#### SHACL Validation Integration

```erlang
% Call Python SHACL validator or tcps_rebar3_shacl
check_shacl_validation(SkuId, State) ->
    case tcps_rebar3_shacl:validate_sku(SkuId) of
        {ok, conforms} ->
            {pass, generate_gate_receipt(shacl_validation, SkuId, pass, #{})};
        {error, {violations, Violations}} ->
            {fail, format_shacl_violations(Violations)}
    end.
```

### 5. Caching Strategy

**ETS Table Schema:**
```erlang
% Table: tcps_quality_gates_cache
% Type: set (unique keys)
% Concurrency: {write_concurrency, true}, {read_concurrency, true}

% Entry format:
{{gate_name(), sku_id()}, Status :: passed | failed, Data :: term()}

% Example entries:
{{shacl_validation, <<"mysku-v1.0.0">>}, passed, Receipt}
{{compilation, <<"mysku-v1.0.0">>}, failed, Violations}
{{test_execution, <<"mysku-v1.0.0">>}, passed, Receipt}
```

**Cache Invalidation:**
- Manual: When SKU is rebuilt
- Automatic: On receipt chain verification failure
- TTL: N/A (cache survives gen_server lifetime)

**Cache Performance:**
- Read: O(1) constant time
- Write: O(1) constant time
- Concurrency: Lock-free reads, minimal write contention

### 6. Production Thresholds

```erlang
% Defined in tcps_quality_gates.erl
-define(PRODUCTION_THRESHOLDS, #{
    test_pass_rate => 0.95,        % 95% minimum
    test_coverage => 0.80,          % 80% minimum
    quality_gate_pass_rate => 0.95, % 95% minimum
    defect_rate => 0.05,            % 5% maximum
    first_pass_yield => 0.90        % 90% minimum
}).
```

**Threshold Enforcement:**
```erlang
check_metric_thresholds(Metrics, Thresholds) ->
    Checks = [
        {test_pass_rate, maps:get(test_pass_rate, Metrics),
         maps:get(test_pass_rate, Thresholds)},
        {test_coverage, maps:get(test_coverage, Metrics),
         maps:get(test_coverage, Thresholds)},
        {first_pass_yield, maps:get(first_pass_yield, Metrics),
         maps:get(first_pass_yield, Thresholds)}
    ],

    % Fail if any metric below threshold
    Failures = [{Metric, Actual, Expected} ||
                {Metric, Actual, Expected} <- Checks,
                Actual < Expected],

    case Failures of
        [] -> {ok, []};
        _ -> {error, format_violations(Failures)}
    end.
```

## Supervision Tree

```
erlmcp_sup (one_for_all)
    │
    ├─ tcps_erlmcp_sup (one_for_one)
    │   │
    │   ├─ tcps_quality_gates (gen_server) [CRITICAL]
    │   │   └─ ETS table: tcps_quality_gates_cache
    │   │
    │   ├─ tcps_andon (gen_server)
    │   │   └─ Andon event tracking
    │   │
    │   ├─ tcps_receipt_chain (gen_server)
    │   │   └─ Receipt chain verification
    │   │
    │   └─ tcps_deterministic (gen_server)
    │       └─ Build reproducibility checks
    │
    └─ Other subsystems...
```

**Restart Strategy:**
- tcps_quality_gates: permanent (critical service)
- Restart: 3 attempts in 60 seconds
- Failure: Crashes entire TCPS supervision tree (stop-the-line)

## Error Handling

### Gate Failure Handling

```erlang
% Stop on first failure
check_gates_recursive([Gate | Rest], SkuId, Receipts, State) ->
    case do_check_gate(Gate, SkuId, State) of
        {pass, Receipt} ->
            % Continue to next gate
            check_gates_recursive(Rest, SkuId, [Receipt | Receipts], State);
        {fail, Violations} ->
            % STOP - trigger Andon and block
            io:format("Quality Gate Failed: ~p~n", [Gate]),
            trigger_quality_gate_andon(Gate, SkuId, Violations),
            {failed_at, Gate, Violations}
    end.
```

### External Tool Failure Handling

```erlang
% Graceful degradation for optional tools
check_shacl_validation(SkuId, State) ->
    case erlang:function_exported(tcps_rebar3_shacl, validate_sku, 1) of
        true ->
            % Tool available - run validation
            tcps_rebar3_shacl:validate_sku(SkuId);
        false ->
            % Tool not available - skip with warning
            io:format("WARNING: SHACL validation not available, skipping~n"),
            Receipt = generate_gate_receipt(shacl_validation, SkuId, pass, #{
                skipped => true,
                reason => <<"SHACL validation module not available">>
            }),
            {pass, Receipt}
    end.
```

## Performance Characteristics

### Gate Execution Times

| Gate | Min | Typical | Max | Blocking |
|------|-----|---------|-----|----------|
| SHACL Validation | 1s | 3s | 10s | Yes |
| Compilation | 5s | 20s | 120s | Yes |
| Test Execution | 10s | 60s | 300s | Yes |
| Security Scan | 5s | 15s | 60s | Yes |
| Deterministic Build | 30s | 120s | 300s | Yes |
| Quality Metrics | 1s | 2s | 5s | Yes |
| Release Verification | 2s | 7s | 30s | Yes |
| Smoke Test | 10s | 40s | 120s | No |

**Total (all gates):** 5-10 minutes typical, 15-20 minutes worst case.

### Optimization Strategies

1. **Incremental Builds:** Cache compilation artifacts
2. **Test Parallelization:** Run independent tests concurrently
3. **Gate Caching:** Skip unchanged gates (with receipt verification)
4. **Fast-Fail:** Run fast gates first (compilation before tests)
5. **Dependency Caching:** Cache npm/hex/rebar3 dependencies

### Scalability

**Single Node:**
- Concurrent SKUs: 10-20 (sequential gate execution)
- Throughput: 6-12 SKUs/hour (depends on test suite size)

**Clustered (Future):**
- Distribute gate execution across nodes
- Shared ETS tables via Mnesia
- Coordinated receipt chain

## Security

### Receipt Integrity

**SHA-256 Hash Chain:**
```erlang
% Each receipt contains hash of previous receipt
Receipt = #{
    receipt_id => <<"RCPT-compilation-1706472000">>,
    previous_receipt_hash => <<"a3b5c7d9e1f2...">>,
    content_hash => crypto:hash(sha256, jsx:encode(Content)),
    ...
}.

% Verification
verify_receipt_chain(SkuId) ->
    Receipts = load_receipts_for_sku(SkuId),
    verify_chain_recursive(Receipts, undefined).

verify_chain_recursive([], _PrevHash) ->
    {ok, complete};
verify_chain_recursive([Receipt | Rest], PrevHash) ->
    case maps:get(previous_receipt_hash, Receipt) of
        PrevHash ->
            CurrentHash = crypto:hash(sha256, jsx:encode(Receipt)),
            verify_chain_recursive(Rest, CurrentHash);
        _ ->
            {error, chain_broken}
    end.
```

### Andon Authorization

**Stop-the-Line Authority:**
- Any quality gate failure triggers Andon
- Andon must be resolved before SKU can proceed
- Override requires:
  - Approval from authorized personnel
  - Documented justification
  - Audit trail entry

## Monitoring and Observability

### Metrics Collected

```erlang
% Per-SKU metrics
#{
    sku_id => <<"mysku-v1.0.0">>,
    gates_attempted => 8,
    gates_passed => 7,
    gates_failed => 1,
    total_duration_ms => 345000,
    first_pass_yield => 0.875  % 7/8 passed on first try
}

% Aggregate metrics (all SKUs)
#{
    test_pass_rate => 0.97,      % Average test pass rate
    test_coverage => 0.85,       % Average code coverage
    defect_rate => 0.02,         % Andon events / work orders
    first_pass_yield => 0.93,    % Gates passed first try / total
    gate_pass_rates => #{
        shacl_validation => 0.98,
        compilation => 0.95,
        test_execution => 0.90,
        ...
    }
}
```

### Logging

**Structured Logs:**
```erlang
logger:info("Quality gate check started", #{
    gate => compilation,
    sku_id => SkuId,
    timestamp => erlang:system_time(millisecond)
}),

logger:error("Quality gate failed", #{
    gate => test_execution,
    sku_id => SkuId,
    pass_rate => 0.87,
    required_pass_rate => 0.95,
    andon_id => <<"ANDON-12345">>
}).
```

### Tracing

**OpenTelemetry Integration:**
```erlang
% Span for gate execution
otel_span:start(#{
    name => <<"quality_gate.check">>,
    attributes => #{
        'gate.name' => compilation,
        'sku.id' => SkuId
    }
}),

% Record gate result
otel_span:set_attributes(#{
    'gate.status' => pass,
    'gate.duration_ms' => Duration
}),

otel_span:end().
```

## Future Enhancements

### Planned Features (v1.1.0)

1. **Parallel Gate Execution:** Run independent gates concurrently
2. **Gate Retries:** Automatic retry with exponential backoff
3. **Custom Gates:** Plugin system for project-specific gates
4. **Cloud Integration:** Upload receipts to S3/GCS
5. **Dashboard:** Real-time quality metrics visualization
6. **ML-Based Predictions:** Predict gate failure probability

### Experimental Features

1. **Chaos Engineering Gate:** Inject failures and verify resilience
2. **Performance Regression Gate:** Detect slowdowns
3. **Accessibility Gate:** WCAG compliance for web UIs
4. **License Compliance Gate:** Automated license checking

## References

- **Source Code:** `apps/tcps_erlmcp/src/tcps_quality_gates.erl`
- **Tests:** `apps/tcps_erlmcp/test/integration/tcps_quality_gates_SUITE.erl`
- **Poka-Yoke:** `apps/erlmcp_core/src/pricing/tcps_poka_yoke.erl`
- **Andon Integration:** `apps/tcps_erlmcp/src/tcps_andon.erl`
- **Receipt Chain:** `apps/tcps_erlmcp/src/tcps_receipt_chain.erl`

---

**Version History:**
- v1.0.0 (2026-01-28): Initial architecture documentation
