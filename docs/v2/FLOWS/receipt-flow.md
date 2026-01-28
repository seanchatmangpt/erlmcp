# Receipt & Evidence Data Flow

**Purpose:** Document the complete lifecycle of receipts from creation through verification to compliance reporting.

---

## 1. Receipt Lifecycle Stages

```
STAGE 1: CREATION
  Event occurs in application
    ↓
  Add receipt to chain
    ↓
  Compute checksum (SHA-256)
    ↓
  Persist to filesystem/ETS
    ↓
STAGE 2: CHAIN BUILDING
  Multiple receipts accumulated
    ↓
  Organize by SKU and stage
    ↓
  Sort chronologically
    ↓
STAGE 3: VERIFICATION
  Validate individual receipts
    ↓
  Verify chain completeness
    ↓
  Check chronological ordering
    ↓
  Detect tampering
    ↓
STAGE 4: COMPLIANCE
  Generate audit trail
    ↓
  Calculate Lean Six Sigma metrics
    ↓
  Compare against compliance thresholds
    ↓
STAGE 5: CERTIFICATION
  Bundle evidence artifacts
    ↓
  Mark as certified (immutable)
    ↓
  Export for auditors
```

---

## 2. Receipt Creation Flow (emit→chain→store)

### 2.1 Application Code Emits Event

**Example: During Compilation Stage**

```erlang
%% erlmcp_server.erl - handle_compilation_request
handle_compilation_request(Request, State) ->
    SkuId = maps:get(<<"sku_id">>, Request),
    StartTime = erlang:system_time(millisecond),

    try
        Result = compile_sku(SkuId),
        EndTime = erlang:system_time(millisecond),
        Duration = EndTime - StartTime,

        %% STEP 1: Emit receipt event
        Receipt = #{
            receipt_id => uuid:v4(),
            timestamp => EndTime,
            sku_id => SkuId,
            stage => <<"compilation">>,    % Binary because JSON
            status => <<"pass">>,
            evidence => #{
                duration_ms => Duration,
                output_size_bytes => byte_size(Result),
                warnings => 0,
                errors => 0
            },
            ontology_refs => [
                <<"https://erlmcp.example.com/ontology#Stage/Compilation">>
            ]
        },

        %% STEP 2: Store receipt
        {ok, ReceiptPath} = tcps_receipt_verifier:store_receipt(Receipt, SkuId),

        %% STEP 3: Add to receipt chain (ETS)
        erlmcp_receipt_chain:add_event(#{
            type => compilation_complete,
            sku_id => SkuId,
            receipt_path => ReceiptPath,
            timestamp => EndTime,
            status => success
        }),

        %% STEP 4: Record metrics
        erlmcp_metrics:record_server_operation(
            erlmcp_server_1,
            <<"compilation">>,
            Duration,
            #{<<"sku_id">> => SkuId}
        ),

        {ok, Result}
    catch
        Class:Reason:Stack ->
            %% STEP 5: Emit error receipt
            ErrorReceipt = Receipt#{
                status => <<"fail">>,
                evidence => #{error => {Class, Reason}}
            },
            {ok, _Path} = tcps_receipt_verifier:store_receipt(ErrorReceipt, SkuId),

            %% Add error event to chain
            erlmcp_receipt_chain:add_event(#{
                type => compilation_failed,
                sku_id => SkuId,
                error => {Class, Reason},
                timestamp => erlang:system_time(millisecond)
            }),

            erlang:raise(Class, Reason, Stack)
    end.
```

### 2.2 Receipt Computation & Storage

**Module: `tcps_receipt_verifier:store_receipt/2` (line 600)**

```erlang
store_receipt(Receipt, SkuId) ->
    %% Step 1: Ensure directory exists
    BaseDir = <<"priv/receipts">>,
    SkuDir = filename:join(BaseDir, SkuId),
    filelib:ensure_dir(binary_to_list(SkuDir) ++ "/"),

    %% Step 2: Generate filename from stage + timestamp
    Stage = maps:get(stage, Receipt, unknown),
    Timestamp = erlang:system_time(millisecond),
    Filename = iolist_to_binary(io_lib:format(
        "~s-~p.json",
        [Stage, Timestamp]
    )),
    ReceiptPath = filename:join(SkuDir, Filename),

    %% Step 3: Compute SHA-256 checksum
    Checksum = compute_checksum(Receipt),
    ReceiptWithChecksum = Receipt#{checksum => Checksum},

    %% Step 4: Serialize to JSON
    JsonBin = jsx:encode(ReceiptWithChecksum),

    %% Step 5: Write to filesystem
    ok = file:write_file(ReceiptPath, JsonBin),

    {ok, ReceiptPath}.


compute_checksum(Receipt) ->
    %% Canonical JSON representation
    JsonBin = jsx:encode(Receipt),

    %% SHA-256 hash
    Hash = crypto:hash(sha256, JsonBin),

    %% Base64 encode
    base64:encode(Hash).
```

### 2.3 Checksum Details

**Checksum Verification Flow:**
```
Receipt (Erlang map)
    ↓
jsx:encode() → JSON string (canonical form)
    ↓
crypto:hash(sha256, Json) → 32-byte binary
    ↓
base64:encode() → ASCII string (e.g., "abc123...")
    ↓
Store in receipt["checksum"]
```

**Tampering Detection:**
```erlang
verify_no_tampering(ReceiptPath) ->
    {ok, JsonBin} = file:read_file(ReceiptPath),
    Receipt = jsx:decode(JsonBin, [return_maps]),

    %% Extract stored checksum
    StoredChecksum = maps:get(<<"checksum">>, Receipt),

    %% Remove checksum field (must exclude for recalc)
    ReceiptWithoutChecksum = maps:remove(<<"checksum">>, Receipt),

    %% Recalculate checksum
    ComputedChecksum = compute_checksum(ReceiptWithoutChecksum),

    %% Compare
    case StoredChecksum =:= ComputedChecksum of
        true →
            {ok, authentic};
        false →
            {error, {tampered, #{
                stored => StoredChecksum,
                computed => ComputedChecksum
            }}}
    end.
```

---

## 3. Receipt Chain Building Flow (multiple→chain→organize)

### 3.1 Accumulation Phase

**Receipts are created across multiple stages:**
```
Compilation Stage
    ↓ generate receipt ✓
    ↓ priv/receipts/SKU-001/compilation-1234567890.json

Testing Stage
    ↓ generate receipt ✓
    ↓ priv/receipts/SKU-001/testing-1234567891.json

Validation Stage
    ↓ generate receipt ✓
    ↓ priv/receipts/SKU-001/validation-1234567892.json

Execution/Deploy Stage
    ↓ generate receipt ✓
    ↓ priv/receipts/SKU-001/execution-1234567893.json
```

### 3.2 ETS Chain Recording

**Module: `erlmcp_receipt_chain` (lines 1-92)**

Each receipt creation triggers event recording:
```erlang
%% Implicit via receipts, explicit via:
erlmcp_receipt_chain:add_event(#{
    type => stage_complete,
    sku_id => <<"SKU-001">>,
    stage => compilation,
    receipt_path => <<"priv/receipts/.../compilation-xxx.json">>,
    timestamp => 1234567890000,   % millisecond
    status => pass
}).


%% Internally in ETS table:
EventId = system_time(microsecond) = 1234567890000000
Entry in ETS: {EventId, Event#{id => EventId, recorded_at => ...}}


%% Can retrieve via:
erlmcp_receipt_chain:get_events_by_type(stage_complete)
    → [{1234567890000000, #{...}},
       {1234567891000000, #{...}},
       {1234567892000000, #{...}},
       {1234567893000000, #{...}}]

    → Ordered by EventId (microsecond precision)
    → Returned in reverse chronological order
```

---

## 4. Receipt Verification Flow (verify→chain→report)

### 4.1 Single Receipt Validation

**Module: `tcps_receipt_verifier:verify_receipt/1` (line 136)**

```erlang
verify_receipt(ReceiptPath) ->
    %% Load JSON from file
    {ok, JsonBin} = file:read_file(ReceiptPath),
    Receipt = jsx:decode(JsonBin, [return_maps]),

    %% Validate receipt structure
    validate_receipt_fields(Receipt).


validate_receipt_fields(Receipt) ->
    Errors = [],

    %% Check required fields present
    RequiredFields = [
        <<"receipt_id">>, <<"timestamp">>, <<"sku_id">>,
        <<"stage">>, <<"status">>
    ],
    Errors2 = case check_required_fields_present(Receipt, RequiredFields) of
        ok → Errors;
        {error, Missing} → [Missing | Errors]
    end,

    %% Validate timestamp
    Errors3 = case validate_timestamp_field(Receipt) of
        ok → Errors2;
        {error, Err} → [Err | Errors2]
    end,

    %% Validate SKU ID format
    Errors4 = case validate_sku_id_format(Receipt) of
        ok → Errors3;
        {error, Err} → [Err | Errors3]
    end,

    %% Validate stage
    Errors5 = case validate_stage_field(Receipt) of
        ok → Errors4;
        {error, Err} → [Err | Errors4]
    end,

    %% Validate status
    Errors6 = case validate_status_field(Receipt) of
        ok → Errors5;
        {error, Err} → [Err | Errors5]
    end,

    case Errors6 of
        [] → {ok, valid};
        _ → {error, {invalid, lists:reverse(Errors6)}}
    end.
```

### 4.2 Receipt Chain Verification

**Module: `tcps_receipt_verifier:verify_receipt_chain/1` (line 164)**

```
Verification Flow for SKU-001:

1. Load Receipts
   get all files in priv/receipts/SKU-001/
    → [compilation-xxx.json, testing-xxx.json, validation-xxx.json, execution-xxx.json]

2. Decode JSON → Erlang maps

3. Extract Stages
   Stage list = [compilation, testing, validation, execution]

4. Check Completeness
   Required = [compilation, testing, validation, execution]
   Actual = [compilation, testing, validation, execution]
   Missing = Required -- Actual = []
   ✓ All stages present

5. Verify Chronological Order
   Extract timestamps:
   - compilation: 1234567890000
   - testing: 1234567891000  ← MUST be > compilation timestamp
   - validation: 1234567892000 ← MUST be > testing timestamp
   - execution: 1234567893000 ← MUST be > validation timestamp

   IF all timestamps strictly increasing:
     ✓ Chronological order verified
   ELSE:
     ✗ NOT chronological → {error, {incomplete, [not_chronological]}}

6. Check for Gaps
   Compare consecutive timestamps:
   - testing - compilation = 1000ms ✓ < 24 hours
   - validation - testing = 1000ms ✓ < 24 hours
   - execution - validation = 1000ms ✓ < 24 hours

   IF all gaps < 86400000ms (24 hours):
     ✓ No suspicious gaps
   ELSE:
     ✗ Large gap → {error, {incomplete, [timestamp_gap_too_large]}}

7. Final Result
   IF (all complete AND chronological AND no gaps):
     {ok, complete}
   ELSE:
     {error, {incomplete, MissingOrErrors}}
```

### 4.3 Deterministic Build Verification

**Module: `tcps_receipt_verifier:verify_deterministic_build/1` (line 215)**

```erlang
verify_deterministic_build(SkuId) ->
    %% Build twice with same inputs
    {ok, Build1} = build_sku(SkuId),
    {ok, Build2} = build_sku(SkuId),

    %% Compute SHA-256 hash of each
    Hash1 = compute_hash(Build1),
    Hash2 = compute_hash(Build2),

    %% Compare
    case Hash1 =:= Hash2 of
        true →
            %% Builds are identical → build is deterministic
            {ok, deterministic};
        false →
            %% Builds differ → NON-DETERMINISTIC
            Diff = compute_diff(Build1, Build2),

            %% Trigger ANDON (stop-the-line, Lean manufacturing)
            trigger_non_determinism_andon(SkuId, Diff),

            {error, {non_deterministic, Diff}}
    end.

compute_hash(Build) ->
    Data = term_to_binary(Build),
    crypto:hash(sha256, Data).
```

**Andon Integration:**
```erlang
trigger_non_determinism_andon(SkuId, Diff) ->
    case whereis(tcps_andon) of
        undefined → {error, andon_not_running};
        _ →
            tcps_andon:trigger_andon(non_determinism, #{
                sku_id => SkuId,
                stage => compilation,
                details => #{
                    diff => Diff,
                    message => <<"Build is non-deterministic">>
                }
            })
    end.
```

---

## 5. Compliance Verification Flow (verify→audit→report)

### 5.1 Complete Chain Verification (All Checks)

**Module: `tcps_receipt_verifier:verify_complete_chain/1` (line 1250)**

```erlang
verify_complete_chain(SkuId) ->
    %% Run all verification checks
    Checks = [
        {receipt_chain, verify_receipt_chain(SkuId)},
        {chronological_order, verify_chronological_order_check(SkuId)},
        {timestamp_gaps, verify_no_timestamp_gaps_check(SkuId)},
        {deterministic_build, verify_deterministic_build(SkuId)},
        {quality_gates, verify_quality_gates_passed(SkuId)},
        {tampering, verify_no_tampering(SkuId)},
        {signatures, verify_signature_chain(SkuId)},
        {stage_transitions, verify_stage_transitions(SkuId)}
    ],

    %% Compile verification report
    compile_verification_report(Checks, SkuId).

compile_verification_report(Checks, SkuId) ->
    %% Partition into passes/failures
    {Passes, Failures} = lists:partition(
        fun({_Name, Result}) ->
            case Result of
                {ok, _} → true;
                ok → true;
                _ → false
            end
        end,
        Checks
    ),

    Report = #{
        sku_id => SkuId,
        total_checks => length(Checks),
        passed_checks => length(Passes),
        failed_checks => length(Failures),
        pass_rate => length(Passes) / length(Checks),
        checks => maps:from_list(Checks),
        verified_at => erlang:system_time(millisecond)
    },

    case Failures of
        [] →
            {ok, Report};  % All checks passed
        _ →
            {error, extract_violations(Failures)}  % Some checks failed
    end.
```

### 5.2 Audit Trail Generation

**Module: `tcps_receipt_verifier:generate_audit_trail/1` (line 1432)**

```erlang
generate_audit_trail(SkuId) ->
    %% 1. Load all receipts
    {ok, Receipts} = load_receipts_for_sku(SkuId),

    %% 2. Load Andon events (failures + resolutions)
    AndonEvents = load_andon_events(SkuId),

    %% 3. Load work order (creation timestamp)
    WorkOrder = load_work_order(SkuId),

    %% 4. Sort receipts chronologically
    SortedReceipts = lists:sort(
        fun(R1, R2) →
            get_timestamp(R1) =< get_timestamp(R2)
        end,
        Receipts
    ),

    %% 5. Extract production stages
    Stages = extract_production_stages(SortedReceipts),

    %% 6. Calculate lead time
    LeadTime = calculate_lead_time(WorkOrder, SortedReceipts),

    %% 7. Calculate stage durations
    StageDurations = calculate_stage_durations(SortedReceipts),

    %% 8. Extract quality gates (pass/fail counts)
    QualityGates = extract_quality_gates(SortedReceipts),

    %% 9. Determine publish status
    PublishStatus = determine_publish_status(SortedReceipts, AndonEvents),

    %% 10. Partition Andon events (open vs resolved)
    {AndonOpen, Resolutions} = partition_andon_events(AndonEvents),

    %% 11. Create audit trail map
    AuditTrail = #{
        sku_id => SkuId,
        work_order_created => WorkOrder,
        production_stages => Stages,
        receipts => format_receipts_for_audit(SortedReceipts),
        andon_events => format_andon_events(AndonOpen),
        resolutions => format_resolutions(Resolutions),
        publish_status => PublishStatus,
        total_lead_time_hours => LeadTime,
        stage_durations => StageDurations,
        quality_gates => QualityGates,
        generated_at => calendar:universal_time()
    },

    {ok, AuditTrail}.
```

**Audit Trail Structure:**
```erlang
#{
  sku_id => <<"SKU-001">>,
  work_order_created => #{
    created_at => 1234567890000,
    status => pending
  },
  production_stages => [
    #{stage => compilation, timestamp => 1234567890000, status => pass},
    #{stage => testing, timestamp => 1234567891000, status => pass},
    #{stage => validation, timestamp => 1234567892000, status => pass},
    #{stage => execution, timestamp => 1234567893000, status => pass}
  ],
  receipts => [...],
  andon_events => [
    #{event_id => E1, failure_type => test_timeout, stage => testing, timestamp => ...}
  ],
  resolutions => [
    #{event_id => E1, resolution => #{root_cause => <<"Resource leak">>}, resolution_timestamp => ...}
  ],
  publish_status => published,
  total_lead_time_hours => 1.5,
  stage_durations => #{
    compilation => 0.02,
    testing => 0.5,
    validation => 0.8,
    execution => 0.01
  },
  quality_gates => #{
    total => 4,
    passed => 4,
    failed => 0
  },
  generated_at => {{2026, 1, 27}, {10, 30, 45}}
}
```

### 5.3 Compliance Report Generation

**Module: `tcps_receipt_verifier:audit_compliance/1` (line 326)**

```erlang
audit_compliance({StartDate, EndDate}) ->
    %% 1. Load all receipts in time period
    AllReceipts = load_receipts_in_period(StartDate, EndDate),

    %% 2. Load Andon events in period
    AllAndonEvents = load_andon_events_in_period(StartDate, EndDate),

    %% 3. Extract unique SKUs
    UniqueSkus = extract_unique_skus(AllReceipts),
    TotalSkus = length(UniqueSkus),

    %% 4. Count receipts
    TotalReceipts = length(AllReceipts),

    %% 5. Count quality gates (PASS/FAIL)
    QualityGatesPassed = count_quality_gates(AllReceipts, passed),
    QualityGatesFailed = count_quality_gates(AllReceipts, failed),

    %% 6. Count Andon events
    TotalAndonEvents = length(AllAndonEvents),
    ResolvedAndonEvents = count_resolved_andon_events(AllAndonEvents),

    %% 7. Calculate average lead time
    AvgLeadTime = calculate_average_lead_time(UniqueSkus, AllReceipts),

    %% 8. Calculate Lean Six Sigma metrics

    % Defect Rate = (Andon Events / Total SKUs) * 100
    DefectRate = case TotalSkus of
        0 → 0.0;
        _ → (TotalAndonEvents / TotalSkus) * 100.0
    end,

    % First Pass Yield = (SKUs without Andon / Total SKUs) * 100
    SkusWithoutAndon = count_skus_without_andon(UniqueSkus, AllAndonEvents),
    FirstPassYield = case TotalSkus of
        0 → 0.0;
        _ → (SkusWithoutAndon / TotalSkus) * 100.0
    end,

    % Rework Rate = (SKUs with multiple receipts per stage / Total) * 100
    SkusWithRework = count_skus_with_rework(UniqueSkus, AllReceipts),
    ReworkRate = case TotalSkus of
        0 → 0.0;
        _ → (SkusWithRework / TotalSkus) * 100.0
    end,

    %% 9. Stage metrics (per-stage stats)
    StageMetrics = calculate_stage_metrics(AllReceipts),

    %% 10. Check compliance violations
    Violations = check_compliance_violations(
        TotalSkus, DefectRate, FirstPassYield, AvgLeadTime
    ),

    %% 11. Determine compliance status
    ComplianceStatus = case Violations of
        [] → compliant;
        _ → non_compliant
    end,

    %% 12. Return compliance report
    #{
        period => {StartDate, EndDate},
        total_skus_processed => TotalSkus,
        receipts_generated => TotalReceipts,
        quality_gates_passed => QualityGatesPassed,
        quality_gates_failed => QualityGatesFailed,
        andon_events_triggered => TotalAndonEvents,
        andon_events_resolved => ResolvedAndonEvents,
        average_lead_time_hours => AvgLeadTime,
        defect_rate_percent => DefectRate,
        first_pass_yield_percent => FirstPassYield,
        rework_rate_percent => ReworkRate,
        stage_metrics => StageMetrics,
        compliance_status => ComplianceStatus,
        violations => Violations,
        generated_at => calendar:universal_time()
    }.


%% Compliance Thresholds
check_compliance_violations(TotalSkus, DefectRate, FirstPassYield, AvgLeadTime) ->
    Violations = [],

    % Defect Rate < 2.0%
    Violations2 = case DefectRate > 2.0 of
        true →
            [#{
                type => high_defect_rate,
                severity => critical,
                value => DefectRate,
                threshold => 2.0,
                message => <<"Defect rate exceeds 2% threshold">>
            } | Violations];
        false → Violations
    end,

    % First Pass Yield > 95.0%
    Violations3 = case FirstPassYield < 95.0 of
        true →
            [#{
                type => low_first_pass_yield,
                severity => major,
                value => FirstPassYield,
                threshold => 95.0,
                message => <<"First pass yield below 95% threshold">>
            } | Violations2];
        false → Violations2
    end,

    % Lead Time < 24 hours
    Violations4 = case AvgLeadTime > 24.0 of
        true →
            [#{
                type => high_lead_time,
                severity => minor,
                value => AvgLeadTime,
                threshold => 24.0,
                message => <<"Average lead time exceeds 24 hours">>
            } | Violations3];
        false → Violations3
    end,

    % Minimum SKUs processed >= 1
    Violations5 = case TotalSkus < 1 of
        true →
            [#{
                type => insufficient_volume,
                severity => major,
                value => TotalSkus,
                threshold => 1,
                message => <<"Insufficient SKUs processed for compliance">>
            } | Violations4];
        false → Violations4
    end,

    lists:reverse(Violations5).
```

---

## 6. Evidence Certification Flow (artifacts→conform→certified)

### 6.1 Evidence Artifact Organization

**Module: `erlmcp_evidence_path` (lines 1-480)**

```
Release: v0.5.0, Plan: enterprise

STEP 1: Create evidence path
  erlmcp_evidence_path:create_evidence_path("0.5.0", enterprise)
    ↓
  mkdir -p dist/evidence/0.5.0/enterprise/
    ✓ Created

STEP 2: Generate benchmark report
  Run benchmarks → collect results
    ↓
  erlmcp_bench_core_ops:run(<<"core_ops_100k">>)
    → Generate: bench_report.json
      {throughput_msg_per_s: 2690000, latency_p99_us: 450, ...}

STEP 3: Generate chaos report
  Run chaos tests → collect failure scenarios
    ↓
  erlmcp_bench_chaos:run(<<"chaos_memory_exhaustion">>)
    → Generate: chaos_report.json
      {failover_time_seconds: 1.5, recovery_rate: 0.97, error_rate: 0.02, ...}

STEP 4: Generate conformance report
  Compare results against plan envelope
    ↓
  erlmcp_evidence_path:generate_conformance_report("0.5.0", enterprise, Results)
    ↓
  Plan envelope for enterprise:
    {
      throughput_req_s: 1500,
      p99_latency_ms: 100,
      failover_sla_seconds: 2,
      recovery_target: 0.95,
      max_error_rate: 0.05
    }
    ↓
  Compare benchmark results:
    actual_throughput (2690000) >= required (1500) ✓ PASS
    actual_p99 (450 us = 0.45 ms) <= required (100 ms) ✓ PASS
    ↓
  Compare chaos results:
    actual_failover (1.5 sec) <= required (2 sec) ✓ PASS
    actual_recovery (0.97) >= required (0.95) ✓ PASS
    actual_error_rate (0.02) <= required (0.05) ✓ PASS
    ↓
  Create: conformance_report.json
    {
      plan: "enterprise",
      benchmark_conformance: {passed: true, details: {...}},
      chaos_conformance: {passed: true, details: {...}},
      overall_status: "certified"
    }

STEP 5: Generate refusal audit
  Audit tool refusal handling during chaos
    ↓
  Create: refusal_audit.json
    {scenarios: [...], results: [...]}

STEP 6: Verify completeness
  erlmcp_evidence_path:verify_artifact_completeness("0.5.0", enterprise)
    ↓
  Check files in dist/evidence/0.5.0/enterprise/:
    ✓ bench_report.json (found)
    ✓ chaos_report.json (found)
    ✓ conformance_report.json (found)
    ✓ refusal_audit.json (found)
    ↓
  {ok, complete}

STEP 7: Mark as certified
  erlmcp_evidence_path:mark_certified("0.5.0", enterprise)
    ↓
  Create: .certified file with timestamp
    Content: 1234567890
    ↓
  chmod 444 (read-only, no write)
    ↓
  {ok, "/path/to/.certified"}

STEP 8: Validate immutability
  erlmcp_evidence_path:validate_immutability("0.5.0", enterprise)
    ↓
  Check all files have restricted permissions:
    - bench_report.json: mode & 0o200 == 0 ✓ (read-only)
    - chaos_report.json: mode & 0o200 == 0 ✓
    - conformance_report.json: mode & 0o200 == 0 ✓
    - refusal_audit.json: mode & 0o200 == 0 ✓
    - .certified: mode & 0o200 == 0 ✓
    ↓
  {ok, immutable}
```

### 6.2 Conformance Verification Details

**Module: `erlmcp_evidence_path:generate_conformance_report/3` (line 145)**

```erlang
generate_conformance_report(Version, Plan, Results) ->
    %% Get plan specifications
    Envelope = get_plan_envelope(Plan),
    BenchResults = maps:get(benchmark, Results, #{}),
    ChaosResults = maps:get(chaos, Results, #{}),

    %% Verify benchmark conformance
    BenchConformance = verify_benchmark_conformance(Envelope, BenchResults),
    % Returns: #{
    %   <<"throughput_req_s">> => #{limit => 1500, actual => 2690000, status => <<"pass">>},
    %   <<"p99_latency_ms">> => #{limit => 100, actual => 0.45, status => <<"pass">>},
    %   <<"memory_mb">> => #{limit => 2048, actual => 512, status => <<"pass">>}
    % }

    %% Verify chaos conformance
    ChaosConformance = verify_chaos_conformance(Envelope, ChaosResults),
    % Returns: #{
    %   <<"failover_sla_seconds">> => #{limit => 2, actual => 1.5, status => <<"pass">>},
    %   <<"recovery_rate">> => #{target => 0.95, actual => 0.97, status => <<"pass">>},
    %   <<"error_rate_during_chaos">> => #{max => 0.05, actual => 0.02, status => <<"pass">>}
    % }

    %% Determine overall status
    OverallStatus = determine_overall_status(Envelope, BenchResults, ChaosResults),
    % IF BenchPass AND ChaosPass → <<"certified">>
    % IF NOT BenchPass → <<"failed">>
    % IF NOT ChaosPass → <<"failed">>
    % ELSE → <<"pending">>

    %% Create report
    ConformanceReport = #{
        <<"plan">> => atom_to_binary(Plan),
        <<"timestamp">> => erlang:system_time(second),
        <<"envelope">> => Envelope,
        <<"benchmark_conformance">> => BenchConformance,
        <<"chaos_conformance">> => ChaosConformance,
        <<"overall_status">> => OverallStatus
    },

    %% Write to filesystem
    ReportPath = filename:join(
        get_evidence_path(Version, Plan),
        "conformance_report.json"
    ),
    {ok, Path} = file:write_file(ReportPath, jsx:encode(ConformanceReport)),

    {ok, ConformanceReport}.
```

---

## 7. Export & Distribution Flow (audit→export→external)

### 7.1 Audit Trail Export

**Module: `tcps_receipt_verifier:export_audit_trail/2` (line 1544)**

```erlang
export_audit_trail(SkuId, json) ->
    %% Generate complete audit trail
    {ok, AuditTrail} = generate_audit_trail(SkuId),

    %% Format as JSON (pretty-printed)
    Json = jsx:encode(AuditTrail, [{space, 2}, {indent, 2}]),

    %% Generate filename with timestamp
    FileName = io_lib:format(
        "audit-~s-~p.json",
        [SkuId, erlang:system_time(millisecond)]
    ),

    %% Ensure directory exists
    FilePath = filename:join("priv/tcps/audit_trails", FileName),
    ok = filelib:ensure_dir(FilePath),

    %% Write to file
    ok = file:write_file(FilePath, Json),

    {ok, list_to_binary(FilePath)}.
```

**Output Example:**
```json
{
  "sku_id": "SKU-001",
  "work_order_created": {
    "created_at": 1234567890000,
    "status": "pending"
  },
  "production_stages": [
    {
      "stage": "compilation",
      "timestamp": 1234567890000,
      "status": "pass"
    },
    {
      "stage": "testing",
      "timestamp": 1234567891000,
      "status": "pass"
    },
    {
      "stage": "validation",
      "timestamp": 1234567892000,
      "status": "pass"
    },
    {
      "stage": "execution",
      "timestamp": 1234567893000,
      "status": "pass"
    }
  ],
  "receipts": [
    {
      "receipt_id": "...",
      "stage": "compilation",
      "timestamp": 1234567890000,
      "status": "pass",
      "checksum": "abc123+/=="
    }
  ],
  "andon_events": [],
  "resolutions": [],
  "publish_status": "published",
  "total_lead_time_hours": 1.5,
  "stage_durations": {
    "compilation": 0.02,
    "testing": 0.5,
    "validation": 0.8,
    "execution": 0.01
  },
  "quality_gates": {
    "total": 4,
    "passed": 4,
    "failed": 0
  },
  "generated_at": "2026-01-27T10:30:45Z"
}
```

---

## 8. Query & Retrieval Flow (request→query→response)

### 8.1 Receipt Chain Queries

```erlang
%% Get all events (reverse chronological)
erlmcp_receipt_chain:get_all_events()
  → Returns [#{id => ..., recorded_at => ..., type => ...}, ...]

%% Get events by type
erlmcp_receipt_chain:get_events_by_type(request_processed)
  → Returns [#{type => request_processed, ...}]

%% Get specific event
erlmcp_receipt_chain:get_event_by_id(1234567890000000)
  → Returns {ok, #{id => 1234567890000000, ...}}
```

### 8.2 Verification Queries

```erlang
%% Check if certification exists
erlmcp_evidence_path:is_certified("0.5.0", enterprise)
  → Returns true | false

%% Get certification status
erlmcp_evidence_path:get_evidence_path("0.5.0", enterprise)
  → Returns {ok, "/path/to/dist/evidence/0.5.0/enterprise"} | {error, not_found}

%% Full chain verification
tcps_receipt_verifier:verify_complete_chain(<<"SKU-001">>)
  → Returns {ok, VerificationReport} | {error, Violations}
```

---

## 9. Error Handling & Recovery

### 9.1 Tampering Detection

```erlang
%% If checksum mismatch detected:
detect_tampering(ReceiptPath)
  → {error, {tampered, #{
      stored => <<"abc123..==">>,
      computed => <<"xyz789..=="">>
    }}}

%% Action: ALERT auditors
%% Do NOT trust this receipt for compliance
```

### 9.2 Non-Deterministic Builds

```erlang
%% If builds produce different hashes:
verify_deterministic_build(SkuId)
  → {error, {non_deterministic, Diff}}
  → trigger_andon(non_determinism, #{sku_id => SkuId, diff => Diff})

%% Action: STOP production line
%% Investigate build process
%% Fix before releasing
```

### 9.3 Chain Incompleteness

```erlang
%% If stages missing:
verify_receipt_chain(SkuId)
  → {error, {incomplete, [testing, validation]}}

%% Action: Re-run missing stages
%% Generate missing receipts
%% Re-verify chain
```

---

## 10. Performance & Metrics

### 10.1 Timing

| Operation | Time | Notes |
|-----------|------|-------|
| add_event (ETS) | <100µs | Microsecond precision IDs |
| get_all_events (ETS tab2list) | ~1ms | Full scan, O(N) |
| compute_checksum (SHA-256) | ~100µs | Per receipt |
| verify_receipt_chain | ~10ms | 4 stages × 2.5ms each |
| generate_audit_trail | ~100ms | Load + sort + format |
| generate_compliance_report | ~1s | Query period + aggregations |
| export_audit_trail (JSON) | ~50ms | Encoding only |

### 10.2 Storage

| Artifact | Size | Notes |
|----------|------|-------|
| Receipt (JSON) | ~500B | Sans evidence |
| ETS event entry | ~200B | With metadata |
| Audit trail JSON | ~10KB | For single SKU |
| Compliance report | ~100KB | For 10K SKUs |
| Evidence directory | ~1MB | Benchmark + chaos + reports |

---

**Generated:** 2026-01-27
**By:** Erlang Researcher Agent
**Status:** Complete - Ready for Developer Reference
