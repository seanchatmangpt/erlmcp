# TCPS SKU Lifecycle Management System

## Overview

The `tcps_sku` module implements complete SKU (Stock Keeping Unit) lifecycle management for the Toyota Composite Production System (TCPS). A SKU represents a manufactured software artifact - a single Marketplace offering that moves through a production pipeline.

## Architecture

### Core Concept

In TCPS, a **SKU = one Marketplace offering**. Each SKU:
- Originates from a work order (demand signal)
- Moves through 10 production stages sequentially
- Generates receipts at each stage transition
- Can be blocked by Andon events (stop-the-line)
- Publishes to marketplace upon completion

### Production Pipeline Stages

The SKU production pipeline follows Toyota Standard Work with 10 stages:

1. **shacl_validation** - RDF ontology validation
2. **compilation** - Erlang/OTP compilation
3. **test_execution** - Unit + integration tests (80%+ coverage requirement)
4. **security_scan** - Vulnerability detection
5. **deterministic_build** - Reproducible build verification
6. **quality_metrics** - Code quality assessment
7. **release_verification** - Pre-release validation
8. **smoke_test** - Deployment smoke tests
9. **marketplace_validation** - Marketplace compliance check
10. **publication** - Final marketplace publication

### State Machine

```
pending → in_production → completed → published
                ↓
              failed
                ↓
             aborted
```

## API Reference

### SKU Creation

#### `create_sku/1`

```erlang
-spec create_sku(WorkOrderId :: binary()) -> {ok, SkuId :: binary()} | {error, term()}.
```

Creates a new SKU from a work order. Initializes the SKU at the first stage (`shacl_validation`).

**Example:**
```erlang
{ok, SkuId} = tcps_sku:create_sku(<<"WO-2026-001">>).
```

**Actions:**
1. Validates work order exists
2. Generates unique SKU ID
3. Creates SKU record at `shacl_validation` stage
4. Stores in ETS and persists to storage
5. Updates work order status to `in_progress`
6. Broadcasts `sku_created` event

### Stage Management

#### `transition_stage/2`

```erlang
-spec transition_stage(SkuId :: binary(), NextStage :: production_stage()) ->
    ok | {error, term()}.
```

Transitions SKU to the next production stage with quality gate enforcement.

**Example:**
```erlang
ok = tcps_sku:transition_stage(SkuId, compilation).
```

**Validation Steps:**
1. **Stage sequence** - Verifies sequential progression (no skipping)
2. **Quality gates** - Checks stage completion criteria
3. **Andon events** - Blocks if open Andons exist
4. **Receipt generation** - Creates stage completion receipt
5. **History tracking** - Records transition in production history
6. **Event broadcasting** - Notifies dashboard of transition

**Error Cases:**
- `{error, invalid_sequence}` - Attempted to skip stages
- `{error, {quality_gate_failed, Reason}}` - Quality gate did not pass
- `{error, {blocked_by_andons, [AndonIds]}}` - Open Andon events blocking progress
- `{error, not_found}` - SKU does not exist

#### `get_sku_status/1`

```erlang
-spec get_sku_status(SkuId :: binary()) -> {ok, map()} | {error, not_found}.
```

Returns comprehensive SKU status.

**Example:**
```erlang
{ok, Status} = tcps_sku:get_sku_status(SkuId).
% Status = #{
%     sku_id => <<"SKU-WO-2026-001-...">>,
%     work_order_id => <<"WO-2026-001">>,
%     current_stage => compilation,
%     status => in_production,
%     completion_percent => 20.0,
%     receipts_count => 1,
%     open_andons_count => 0,
%     created_at => 1737934800,
%     updated_at => 1737934850,
%     production_history => [...]
% }
```

### Pipeline Execution

#### `process_sku_pipeline/1`

```erlang
-spec process_sku_pipeline(SkuId :: binary()) ->
    {ok, production_stage()} | {error, {failed_at, Stage, Reason}}.
```

Processes SKU through entire production pipeline **synchronously**. Stops on first failure.

**Example:**
```erlang
{ok, FinalStage} = tcps_sku:process_sku_pipeline(SkuId).
% Runs through all 10 stages sequentially
% Returns final stage reached (publication if successful)
```

**Process:**
1. Starts at current stage
2. For each stage:
   - Checks quality gate
   - Checks for Andon events
   - Generates receipt
   - Transitions to next stage
3. Stops on first failure
4. Returns final stage or error

#### `process_sku_pipeline_async/1`

```erlang
-spec process_sku_pipeline_async(SkuId :: binary()) -> {ok, pid()} | {error, term()}.
```

Processes SKU pipeline **asynchronously** in a worker process.

**Example:**
```erlang
{ok, WorkerPid} = tcps_sku:process_sku_pipeline_async(SkuId),
% Worker runs pipeline in background
% Use get_pipeline_progress/1 to check status
```

### Release & Publication

#### `mark_published/1`

```erlang
-spec mark_published(SkuId :: binary()) -> ok | {error, term()}.
```

Marks SKU as published to marketplace (final step).

**Example:**
```erlang
ok = tcps_sku:mark_published(SkuId).
```

**Preconditions:**
- All stages must be completed
- No open Andon events
- SKU at `publication` stage

**Actions:**
1. Verifies pipeline complete
2. Verifies no open Andons
3. Generates marketplace URL
4. Updates SKU status to `published`
5. Records publication timestamp
6. Marks work order as `completed`
7. Generates publication receipt

### Integration - High-Level Production

#### `produce_sku/1`

```erlang
-spec produce_sku(WorkOrderId :: binary()) -> {ok, SkuId :: binary()} | {error, term()}.
```

**Complete end-to-end SKU production** (one-step workflow).

**Example:**
```erlang
{ok, SkuId} = tcps_sku:produce_sku(<<"WO-2026-001">>).
% Creates SKU, runs pipeline, releases, publishes to marketplace
```

**Process:**
1. Creates SKU from work order
2. Processes through entire pipeline
3. Marks as released
4. Publishes to marketplace
5. Returns published SKU ID

This is the **recommended way** to manufacture SKUs.

## Quality Gates Integration

### Quality Gate Enforcement

Each stage transition checks quality gates via `check_quality_gate/2`:

```erlang
check_quality_gate(SkuId, Stage) -> {pass, Receipt} | {fail, Reason}.
```

**Current Implementation:**
- Mock implementation (always passes)
- Generates receipt with evidence

**Production Integration:**
Integration with `tcps_quality_gates` module would enforce:
- **shacl_validation**: RDF/Turtle ontology compliance
- **compilation**: Zero compilation errors
- **test_execution**: 80%+ code coverage, all tests pass
- **security_scan**: No high/critical vulnerabilities
- **deterministic_build**: Build reproducibility verified
- **quality_metrics**: Code quality thresholds met
- **release_verification**: Release checklist complete
- **smoke_test**: Deployment tests passed
- **marketplace_validation**: Marketplace requirements met

## Andon Integration

### Stop-the-Line Enforcement

Open Andon events **block stage transitions** (Toyota stop-the-line principle).

**Check Function:**
```erlang
check_andons(SkuId, NextStage) -> {ok, proceed} | {blocked, [AndonIds]}.
```

**Integration with `tcps_andon` module:**
- Queries for open Andon events for SKU
- If open Andons exist, transitions are blocked
- Pipeline halts until Andons resolved
- Ensures zero-defect delivery

**Example Andon Blocking:**
```erlang
% Andon triggered for test failure
{ok, AndonId} = tcps_andon:trigger_andon(test_failure, #{
    sku_id => SkuId,
    stage => test_execution,
    details => #{failures => 3}
}),

% Stage transition blocked
{error, {blocked_by_andons, [AndonId]}} =
    tcps_sku:transition_stage(SkuId, security_scan),

% Resolve Andon
ok = tcps_andon:resolve_andon(AndonId, #{
    root_cause => <<"Missing test fixtures">>,
    fix_applied => <<"Added test fixtures">>,
    prevention_added => <<"Automated fixture generation">>
}),

% Now can proceed
ok = tcps_sku:transition_stage(SkuId, security_scan).
```

## Receipt Chain Tracking

### Receipt Generation

Each stage transition generates a receipt:

```erlang
Receipt = #{
    receipt_id => <<"receipt-1737934850-123456">>,
    sku_id => SkuId,
    stage => compilation,
    timestamp => 1737934850,
    status => pass,
    evidence => <<"Quality gate passed for stage: compilation">>
}
```

### Receipt Chain Verification

```erlang
-spec verify_receipt_chain(SkuId :: binary()) ->
    {ok, complete} | {error, {missing_receipts, [Stage]}}.
```

Verifies all required stages have receipts.

**Example:**
```erlang
{ok, complete} = tcps_sku:verify_receipt_chain(SkuId).
% All 10 stages have receipts

{error, {missing_receipts, [security_scan, deterministic_build]}} =
    tcps_sku:verify_receipt_chain(IncompleteSkuId).
% Missing receipts for 2 stages
```

## Production History

### History Tracking

Each transition records entry in production history:

```erlang
HistoryEntry = #{
    stage => shacl_validation,
    timestamp => 1737934850,
    status => pass,
    receipt_id => <<"receipt-...">>,
    reason => <<"...">>,  % Only if failed
}
```

### Retrieving History

```erlang
{ok, History} = tcps_sku:get_production_history(SkuId).
% Returns list of all stage transitions (newest first)
```

## Production Metrics

### System-Wide Metrics

```erlang
Metrics = tcps_sku:get_production_metrics().
% #{
%     total_skus => 150,
%     in_production => 45,
%     completed => 90,
%     published => 85,
%     failed => 10,
%     success_rate => 56.67
% }
```

### Stage Statistics

```erlang
Stats = tcps_sku:get_stage_statistics().
% #{
%     shacl_validation => 12,
%     compilation => 8,
%     test_execution => 15,
%     ...
% }
% Count of SKUs at each stage
```

## Usage Patterns

### Pattern 1: Simple Production (Recommended)

```erlang
% One-step production
{ok, SkuId} = tcps_sku:produce_sku(WorkOrderId).
```

### Pattern 2: Manual Stage Control

```erlang
% Create SKU
{ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

% Transition through stages manually
ok = tcps_sku:transition_stage(SkuId, compilation),
ok = tcps_sku:transition_stage(SkuId, test_execution),
ok = tcps_sku:transition_stage(SkuId, security_scan),
...

% Publish
ok = tcps_sku:mark_published(SkuId).
```

### Pattern 3: Automated Pipeline with Async

```erlang
% Create SKU
{ok, SkuId} = tcps_sku:create_sku(WorkOrderId),

% Run pipeline asynchronously
{ok, WorkerPid} = tcps_sku:process_sku_pipeline_async(SkuId),

% Check progress
{ok, Progress} = tcps_sku:get_pipeline_progress(SkuId),
% #{
%     current_stage => test_execution,
%     percent_complete => 30.0,
%     status => in_production
% }

% Publish when complete
ok = tcps_sku:mark_published(SkuId).
```

### Pattern 4: Pipeline with Error Handling

```erlang
case tcps_sku:process_sku_pipeline(SkuId) of
    {ok, FinalStage} ->
        io:format("Pipeline complete at ~p~n", [FinalStage]),
        ok = tcps_sku:mark_published(SkuId);
    {error, {failed_at, Stage, Reason}} ->
        io:format("Pipeline failed at ~p: ~p~n", [Stage, Reason]),
        % Trigger Andon or handle failure
        tcps_andon:trigger_andon(pipeline_failure, #{
            sku_id => SkuId,
            stage => Stage,
            details => #{reason => Reason}
        })
end.
```

## Work Order Integration

### Work Order Status Updates

The SKU module integrates with `tcps_work_order` to update work order status:

1. **SKU Created** → Work order status: `in_progress`
2. **SKU Published** → Work order status: `completed`
3. **SKU Failed** → Work order status may remain `in_progress` (for retry)

### Work Order → SKU Flow

```
Work Order Created (from pull signal)
         ↓
   SKU Created (demand-driven)
         ↓
   Pipeline Processing
         ↓
   Quality Gates + Andons
         ↓
   Marketplace Publication
         ↓
   Work Order Completed
```

## Dashboard Integration

### Event Broadcasting

SKU lifecycle events are broadcast to `tcps_sse_manager` for real-time dashboard updates:

**Events:**
- `sku_created` - New SKU created
- `sku_stage_transition` - SKU moved to next stage
- `sku_completed` - SKU completed pipeline
- `sku_published` - SKU published to marketplace
- `sku_failed` - SKU failed quality gate

**Event Format:**
```erlang
Event = #{
    type => sku_stage_transition,
    sku_id => SkuId,
    from_stage => compilation,
    to_stage => test_execution,
    timestamp => 1737934850
}
```

## Configuration

### Gen Server Configuration

```erlang
Config = #{
    receipts_dir => "priv/receipts/skus",  % Receipt storage directory
    auto_persist => true,                   % Auto-save to tcps_persistence
    max_concurrent_pipelines => 10          % Max async pipeline workers
}.

{ok, Pid} = tcps_sku:start_link(Config).
```

## Error Handling

### Common Error Cases

1. **Invalid Stage Sequence**
   ```erlang
   {error, invalid_sequence}
   ```
   Attempted to skip stages (must be sequential)

2. **Quality Gate Failure**
   ```erlang
   {error, {quality_gate_failed, Reason}}
   ```
   Stage did not meet quality criteria

3. **Andon Blocking**
   ```erlang
   {error, {blocked_by_andons, [AndonIds]}}
   ```
   Open Andon events preventing progress

4. **Pipeline Incomplete**
   ```erlang
   {error, pipeline_incomplete}
   ```
   Attempted to publish before completing all stages

5. **SKU Not Found**
   ```erlang
   {error, not_found}
   ```
   Referenced SKU does not exist

## Testing

### Comprehensive Test Suite

The test suite (`tcps_sku_tests.erl`) provides:

- **28 test cases** covering all functionality
- SKU creation and lifecycle management
- Stage transition validation
- Quality gate enforcement
- Andon integration
- Receipt chain tracking
- Pipeline execution (sync/async)
- Error handling
- Concurrent operations
- Production metrics

**Run Tests:**
```bash
rebar3 eunit --module=tcps_sku_tests
```

## Performance Considerations

### ETS Storage

SKUs are stored in ETS table `tcps_skus` for fast lookups:
- Named table: `tcps_skus`
- Type: `set` (unique SKU IDs)
- Concurrency: `{read_concurrency, true}, {write_concurrency, true}`

### Async Pipeline Processing

- Supports concurrent pipeline workers
- Configurable max concurrent limit
- Workers monitored for failure
- Automatic cleanup on worker termination

### Scalability

- Each SKU is independent (no blocking dependencies)
- Stage transitions are atomic (gen_server calls)
- Receipt generation is lightweight
- Dashboard events are fire-and-forget (non-blocking)

## Future Enhancements

### Quality Gate Integration

Integrate with actual `tcps_quality_gates` module:
- SHACL validation via RDF engine
- Coverage analysis via `cover` module
- Security scanning via vulnerability database
- Build reproducibility verification

### Advanced Andon Integration

- Auto-trigger Andons on quality gate failures
- Root cause analysis integration
- Prevention measure tracking
- MTTR (Mean Time To Resolution) metrics

### Receipt Chain Cryptography

- SHA-256 checksums per receipt
- Chain integrity verification
- Tamper-evident audit trail
- Blockchain-style receipt linking

### Advanced Metrics

- Stage duration tracking
- Bottleneck identification
- Throughput analysis
- SLA compliance per stage
- Kaizen integration for continuous improvement

## References

- [TCPS Work Order Management](./tcps_work_order.md)
- [TCPS Andon System](./tcps_andon.md)
- [TCPS Quality Gates](./tcps_quality_gates.md)
- [TCPS Receipt Verification](./tcps_receipt_verifier.md)

## License

Apache-2.0 - See LICENSE file for details.
