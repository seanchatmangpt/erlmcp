# TCPS Diataxis Reference Documentation

**Information-Oriented Quadrant** - Comprehensive reference material for TCPS (Toyota Code Production System)

## Overview

This reference documentation provides complete, accurate, and searchable information about all TCPS modules, APIs, configuration options, data structures, CLI commands, and error codes. It is designed to be consulted during development for precise technical details.

## Contents

1. [API Reference](#api-reference)
2. [Configuration Reference](#configuration-reference)
3. [Data Structures](#data-structures)
4. [CLI Commands](#cli-commands)
5. [Error Codes](#error-codes)

---

## API Reference

### Module Index

- **[tcps_quality_gates](#tcps_quality_gates-api)** - Quality gate enforcement
- **[tcps_kanban](#tcps_kanban-api)** - WIP limits and Heijunka leveling
- **[tcps_andon](#tcps_andon-api)** - Stop-the-line events
- **[tcps_kaizen](#tcps_kaizen-api)** - Continuous improvement
- **[tcps_work_order](#tcps_work_order-api)** - Work order lifecycle
- **[tcps_sku](#tcps_sku-api)** - SKU production pipeline
- **[tcps_receipt](#tcps_receipt-api)** - Receipt generation and verification

### tcps_quality_gates API

**Category:** Quality Enforcement

#### `check_gate/2`

```erlang
-spec check_gate(Gate :: gate_name(), SkuId :: sku_id()) -> gate_result().
```

Executes a single quality gate for a SKU and returns pass/fail with receipt or violations.

**Parameters:**
- `Gate` (`gate_name()`) - Gate to execute (shacl_validation, compilation, test_execution, security_scan, deterministic_build, quality_metrics, release_verification, smoke_test)
- `SkuId` (`sku_id()`) - SKU identifier

**Returns:** `{pass, receipt()} | {fail, violations()}`

**Example:**
```erlang
{pass, Receipt} = tcps_quality_gates:check_gate(compilation, <<"sku_12345">>).
```

**Error Codes:**
- `unknown_gate` - Invalid gate name provided

**Since:** 0.1.0

**See Also:** `check_all_gates/1`, `get_gate_status/2`

---

#### `check_all_gates/1`

```erlang
-spec check_all_gates(SkuId :: sku_id()) ->
    {ok, [receipt()]} | {failed_at, gate_name(), violations()}.
```

Executes all quality gates in sequence (shacl_validation → compilation → test_execution → security_scan → deterministic_build → quality_metrics → release_verification → smoke_test). Stops on first failure.

**Parameters:**
- `SkuId` (`sku_id()`) - SKU identifier

**Returns:**
- `{ok, [receipt()]}` - All gates passed, list of all receipts
- `{failed_at, GateName, Violations}` - First failure with gate name and violations

**Example:**
```erlang
case tcps_quality_gates:check_all_gates(<<"sku_12345">>) of
    {ok, Receipts} ->
        io:format("All gates passed: ~p receipts~n", [length(Receipts)]);
    {failed_at, Gate, Violations} ->
        io:format("Failed at ~p: ~p~n", [Gate, Violations])
end.
```

**Since:** 0.1.0

---

#### `get_quality_metrics/0`

```erlang
-spec get_quality_metrics() -> map().
```

Returns aggregate quality metrics across all SKUs (last 30 days).

**Returns:** Map with keys:
- `test_pass_rate` (float) - Average test pass rate
- `test_coverage` (float) - Average code coverage
- `defect_rate` (float) - Andon events / work orders
- `first_pass_yield` (float) - Gates passed on first try / total attempts
- `gate_pass_rates` (map) - Per-gate pass rates

**Example:**
```erlang
Metrics = tcps_quality_gates:get_quality_metrics(),
#{test_pass_rate := PassRate, test_coverage := Coverage} = Metrics,
io:format("Pass rate: ~.1f%, Coverage: ~.1f%~n", [PassRate * 100, Coverage * 100]).
```

**Since:** 0.2.0

---

#### `validate_stage_transition/3`

```erlang
-spec validate_stage_transition(SkuId :: sku_id(),
                               FromStage :: stage(),
                               ToStage :: stage()) ->
    ok | {error, {blocked, term()}}.
```

Validates that a SKU can transition from one stage to another by checking:
1. No open Andon events exist for this SKU
2. Required quality gate for target stage has passed

**Parameters:**
- `SkuId` (`sku_id()`) - SKU identifier
- `FromStage` (`stage()`) - Current stage
- `ToStage` (`stage()`) - Target stage

**Returns:**
- `ok` - Transition allowed
- `{error, {blocked, Reason}}` - Transition blocked

**Blocking Reasons:**
- `{open_andons, [AndonIds]}` - Open Andon events
- `{gate_failed, GateName, Violations}` - Quality gate failed
- `{gate_not_run, GateName}` - Required gate not executed

**Example:**
```erlang
case tcps_quality_gates:validate_stage_transition(<<"sku_123">>, testing, deployment) of
    ok ->
        proceed_to_deployment();
    {error, {blocked, {open_andons, AndonIds}}} ->
        io:format("Blocked by Andons: ~p~n", [AndonIds])
end.
```

**Since:** 0.3.0

---

### tcps_kanban API

**Category:** WIP Management

#### `check_wip_limit/1`

```erlang
-spec check_wip_limit(Bucket :: bucket()) ->
    {ok, available} | {error, limit_reached}.
```

Checks if WIP limit allows new work in specified bucket.

**Parameters:**
- `Bucket` (`bucket()`) - reliability | security | cost | compliance

**Returns:**
- `{ok, available}` - Capacity available
- `{error, limit_reached}` - At or exceeds limit

**Example:**
```erlang
case tcps_kanban:check_wip_limit(reliability) of
    {ok, available} ->
        create_work_order();
    {error, limit_reached} ->
        queue_for_later()
end.
```

**Since:** 0.1.0

---

#### `set_wip_limit/2`

```erlang
-spec set_wip_limit(Bucket :: bucket(), Limit :: pos_integer() | infinity) -> ok.
```

Sets WIP limit for a bucket.

**Parameters:**
- `Bucket` (`bucket()`) - Bucket name
- `Limit` (`pos_integer() | infinity`) - Maximum work items (or infinity for unlimited)

**Example:**
```erlang
tcps_kanban:set_wip_limit(security, 10).  % Set security bucket to 10 items max
```

**Since:** 0.1.0

---

#### `process_pull_signal/1`

```erlang
-spec process_pull_signal(Signal :: pull_signal()) ->
    {ok, work_order_id()} | {error, limit_reached}.
```

Processes pull signal and creates work order if WIP limit allows.

**Parameters:**
- `Signal` (`pull_signal()`) - #{bucket => bucket(), priority => non_neg_integer(), payload => map()}

**Returns:**
- `{ok, WorkOrderId}` - Work order created
- `{error, limit_reached}` - WIP limit prevents new work

**Example:**
```erlang
Signal = #{
    bucket => security,
    priority => 9,
    payload => #{cve => "CVE-2024-1234", severity => critical}
},
{ok, WOId} = tcps_kanban:process_pull_signal(Signal).
```

**Since:** 0.2.0

---

#### `heijunka_schedule/0`

```erlang
-spec heijunka_schedule() -> [work_order()].
```

Returns leveled schedule of all work orders using Heijunka algorithm to prevent batching. Distributes work evenly across buckets and prevents more than `max_consecutive` items from same bucket.

**Returns:** List of work orders in optimized execution order

**Example:**
```erlang
Schedule = tcps_kanban:heijunka_schedule(),
[process_work_order(WO) || WO <- Schedule].
```

**Since:** 0.3.0

---

### tcps_andon API

**Category:** Stop-the-Line

#### `trigger_andon/2`

```erlang
-spec trigger_andon(FailureType :: failure_type(),
                   Context :: andon_context()) ->
    {ok, andon_event_id()}.
```

Triggers Andon stop-the-line event for a defect. Generates receipt, triggers dashboard broadcast, and blocks SKU progression.

**Parameters:**
- `FailureType` (`failure_type()`) - shacl_violation | test_failure | non_determinism | missing_receipt | compilation_failure
- `Context` (`andon_context()`) - #{sku_id => binary(), stage => stage(), details => map()}

**Returns:** `{ok, AndonEventId}`

**Example:**
```erlang
Context = #{
    sku_id => <<"sku_123">>,
    stage => testing,
    details => #{
        test_module => my_module_tests,
        test_function => test_authentication,
        expected => ok,
        actual => {error, timeout}
    }
},
{ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context).
```

**Since:** 0.1.0

---

#### `resolve_andon/2`

```erlang
-spec resolve_andon(AndonId :: andon_event_id(),
                   Resolution :: resolution()) ->
    ok | {error, term()}.
```

Resolves Andon event with root cause analysis and prevention measures.

**Parameters:**
- `AndonId` (`andon_event_id()`) - Andon event identifier
- `Resolution` (`resolution()`) - #{root_cause => binary(), fix_applied => binary(), prevention_added => binary()}

**Returns:** `ok` or `{error, Reason}`

**Error Codes:**
- `{already_resolved, AndonId}` - Event already resolved
- `{andon_not_found, AndonId}` - Invalid Andon ID

**Example:**
```erlang
Resolution = #{
    root_cause => <<"Authentication timeout due to slow database">>,
    fix_applied => <<"Increased timeout from 5s to 10s">>,
    prevention_added => <<"Added database health check before tests">>
},
ok = tcps_andon:resolve_andon(AndonId, Resolution).
```

**Since:** 0.1.0

---

#### `can_proceed_to_stage/2`

```erlang
-spec can_proceed_to_stage(SkuId :: sku_id(), Stage :: stage()) ->
    {ok, proceed} | {blocked, [andon_event_id()]}.
```

Checks if SKU can proceed to stage (no open Andon events blocking it).

**Parameters:**
- `SkuId` (`sku_id()`) - SKU identifier
- `Stage` (`stage()`) - Target stage

**Returns:**
- `{ok, proceed}` - No blocking events
- `{blocked, [AndonIds]}` - List of blocking Andon event IDs

**Example:**
```erlang
case tcps_andon:can_proceed_to_stage(<<"sku_123">>, deployment) of
    {ok, proceed} ->
        deploy_sku();
    {blocked, AndonIds} ->
        io:format("Blocked by: ~p~n", [AndonIds])
end.
```

**Since:** 0.2.0

---

### tcps_kaizen API

**Category:** Continuous Improvement

#### `collect_metrics/1`

```erlang
-spec collect_metrics(TimePeriod :: time_period()) -> metrics().
```

Collects quality metrics over time period from TCPS receipts.

**Parameters:**
- `TimePeriod` (`{StartDate, EndDate}`) - Date range for analysis

**Returns:** Map with:
- `lead_time` (float) - Hours from work_order → SKU published
- `defect_rate` (float) - Andon events per 100 SKUs
- `rework_pct` (float) - % SKUs requiring fixes
- `cycle_time` (float) - Average time per stage (hours)
- `first_pass_yield` (float) - % SKUs passing without rework
- `throughput` (float) - SKUs per day

**Example:**
```erlang
Metrics = tcps_kaizen:collect_metrics({{2024,1,1}, {2024,1,31}}),
#{lead_time := LT, defect_rate := DR} = Metrics,
io:format("Lead time: ~.1f hrs, Defect rate: ~.1f%~n", [LT, DR]).
```

**Since:** 0.1.0

---

#### `identify_waste_points/0`

```erlang
-spec identify_waste_points() -> [waste_point()].
```

Identifies waste in production system (last 7 days):
- Compilation slow (>5 min)
- Flaky tests (inconsistent results)
- Manual interventions
- Repeat Andon events (same root cause)
- Stage bottlenecks (>1 hour average)
- Excessive rework

**Returns:** List of waste points sorted by `total_waste` (highest first)

**Example:**
```erlang
WastePoints = tcps_kaizen:identify_waste_points(),
[#{stage := Stage, waste_type := Type, total_waste := Hours} | _] = WastePoints,
io:format("Top waste: ~p (~p) - ~.1f hrs/week~n", [Stage, Type, Hours]).
```

**Since:** 0.2.0

---

#### `propose_improvements/1`

```erlang
-spec propose_improvements(WastePoints :: [waste_point()]) -> [improvement()].
```

Generates improvement proposals with quantified expected benefits.

**Parameters:**
- `WastePoints` ([waste_point()]) - Identified waste

**Returns:** List of improvements sorted by ROI (benefit / effort)

**Example:**
```erlang
Waste = tcps_kaizen:identify_waste_points(),
Improvements = tcps_kaizen:propose_improvements(Waste),
[#{description := Desc, expected_benefit := Benefit} | _] = Improvements,
io:format("Top improvement: ~s - ~s~n", [Desc, Benefit]).
```

**Since:** 0.2.0

---

#### `generate_weekly_report/1`

```erlang
-spec generate_weekly_report(WeekEndDate :: date()) -> map().
```

Generates comprehensive weekly Kaizen report with:
- Current metrics vs targets
- Waste points (top 10)
- Improvements proposed and applied
- Trend analysis (week-over-week)
- Recommendations

**Parameters:**
- `WeekEndDate` (`date()`) - End date of reporting week

**Returns:** Report map with sections: `summary`, `waste_analysis`, `improvements`, `trends`, `week_over_week`, `recommendations`, `charts`

**Example:**
```erlang
Report = tcps_kaizen:generate_weekly_report({2024, 1, 26}),
#{summary := Summary, recommendations := Recs} = Report.
```

**Since:** 0.3.0

---

## Configuration Reference

### quality_gates Configuration

Quality gate thresholds for zero-defect delivery (Toyota Production System standards)

#### `test_pass_rate`

**Type:** float
**Default:** `0.95`

Minimum test pass rate required to pass test_execution gate. Tests must pass at this rate or higher.

**Examples:**
```erlang
0.95  % 95% pass rate (default)
0.90  % 90% pass rate (relaxed)
0.98  % 98% pass rate (strict)
```

**Valid Range:** 0.0 - 1.0
**Environment Variable:** `TCPS_TEST_PASS_RATE`
**Related:** `test_coverage`, `defect_rate`

---

#### `test_coverage`

**Type:** float
**Default:** `0.80`

Minimum code coverage required to pass test_execution gate. Industry best practice is 80%.

**Examples:**
```erlang
0.80  % 80% coverage (default)
0.85  % 85% coverage
0.90  % 90% coverage (high quality)
```

**Valid Range:** 0.0 - 1.0
**Environment Variable:** `TCPS_TEST_COVERAGE`
**Related:** `test_pass_rate`, `quality_gate_pass_rate`

---

#### `defect_rate`

**Type:** float
**Default:** `0.05`

Maximum acceptable defect rate (Andon events per work order). Toyota target is 5% maximum.

**Examples:**
```erlang
0.05  % 5% defect rate (default)
0.03  % 3% defect rate (strict)
0.10  % 10% defect rate (relaxed)
```

**Valid Range:** 0.0 - 1.0
**Environment Variable:** `TCPS_DEFECT_RATE`
**Related:** `first_pass_yield`, `test_pass_rate`

---

#### `first_pass_yield`

**Type:** float
**Default:** `0.90`

Minimum percentage of work orders that pass all gates on first attempt. Toyota standard is 90%.

**Examples:**
```erlang
0.90  % 90% first pass yield (default)
0.95  % 95% first pass yield (excellent)
```

**Valid Range:** 0.0 - 1.0
**Related:** `defect_rate`, `quality_gate_pass_rate`

---

### kanban Configuration

Kanban WIP (Work In Progress) limits and Heijunka leveling configuration

#### `wip_limit_reliability`

**Type:** integer
**Default:** `5`

Maximum concurrent work items in reliability bucket (bugs, production issues).

**Examples:**
```erlang
5   % 5 items (default)
10  % 10 items (larger team)
3   % 3 items (strict flow)
```

**Valid Range:** 1 - 100
**Environment Variable:** `TCPS_WIP_RELIABILITY`
**Related:** `wip_limit_security`, `wip_limit_cost`, `wip_limit_compliance`

---

#### `wip_limit_security`

**Type:** integer
**Default:** `5`

Maximum concurrent work items in security bucket (CVE, vulnerabilities).

**Examples:**
```erlang
5   % 5 items (default)
8   % 8 items (security-focused)
3   % 3 items (minimal WIP)
```

**Valid Range:** 1 - 100
**Environment Variable:** `TCPS_WIP_SECURITY`

---

#### `max_consecutive_same_bucket`

**Type:** integer
**Default:** `2`

Maximum consecutive work orders from same bucket in Heijunka schedule (prevents batching).

**Examples:**
```erlang
2  % Max 2 consecutive (default)
1  % Strict alternation
3  % Allow 3 consecutive
```

**Valid Range:** 1 - 10
**Related:** `enable_heijunka_leveling`

---

### andon Configuration

Andon stop-the-line event configuration and severity levels

#### `severity_critical`

**Type:** list
**Default:** `[shacl_violation, compilation_failure, non_determinism]`

Failure types classified as critical severity (blocks all progression).

**Examples:**
```erlang
[shacl_violation, compilation_failure, non_determinism]  % Default
[shacl_violation, compilation_failure]  % Without non_determinism
```

**Related:** `severity_warning`, `severity_info`

---

#### `auto_resolve_timeout_hours`

**Type:** integer
**Default:** `0`

Auto-resolve Andon events after N hours (0 = disabled, requires manual resolution).

**Examples:**
```erlang
0   % Disabled (default, manual resolution required)
24  % Auto-resolve after 24 hours
72  % Auto-resolve after 3 days
```

**Valid Range:** 0 - 720
**Environment Variable:** `TCPS_ANDON_AUTO_RESOLVE`

---

### receipts Configuration

Receipt generation and storage configuration for audit trail

#### `storage_backend`

**Type:** atom
**Default:** `filesystem`

Receipt storage backend (filesystem, rdf, database).

**Examples:**
```erlang
filesystem  % Store as JSON files (default)
rdf         % Store in RDF ontology
database    % Store in database
```

**Valid Values:** `filesystem`, `rdf`, `database`
**Environment Variable:** `TCPS_RECEIPT_BACKEND`
**Related:** `storage_path`, `enable_ontology_linking`

---

#### `storage_path`

**Type:** string
**Default:** `"priv/receipts"`

Base directory for receipt storage (filesystem backend only).

**Examples:**
```erlang
"priv/receipts"  % Default
"/var/lib/tcps/receipts"  % Production
"./receipts"  % Current directory
```

**Environment Variable:** `TCPS_RECEIPTS_PATH`

---

## Data Structures

### gate_name()

```erlang
-type gate_name() :: shacl_validation
                   | compilation
                   | test_execution
                   | security_scan
                   | deterministic_build
                   | quality_metrics
                   | release_verification
                   | smoke_test.
```

Quality gate types in execution order.

---

### bucket()

```erlang
-type bucket() :: reliability | security | cost | compliance.
```

Kanban bucket types for work categorization.

---

### failure_type()

```erlang
-type failure_type() :: shacl_violation
                      | test_failure
                      | non_determinism
                      | missing_receipt
                      | compilation_failure.
```

Types of failures that trigger Andon events.

---

### work_order()

```erlang
-type work_order() :: #{
    id := work_order_id(),
    bucket := bucket(),
    priority := non_neg_integer(),
    status := pending | in_progress | completed,
    created_at := erlang:timestamp(),
    payload := map()
}.
```

Work order structure with bucket assignment, priority, and status tracking.

---

### metrics()

```erlang
-type metrics() :: #{
    lead_time => float(),              % Hours from work_order to SKU published
    defect_rate => float(),            % Andon events per 100 SKUs
    rework_pct => float(),             % % of SKUs requiring fixes
    cycle_time => float(),             % Average time per stage
    first_pass_yield => float(),       % SKUs passing without rework
    throughput => float()              % SKUs per day
}.
```

Quality metrics structure.

---

## CLI Commands

### tcps andon

Andon stop-the-line event management

#### `tcps andon trigger`

Trigger an Andon stop-the-line event.

**Options:**
- `--type, -t` (required) - Failure type (shacl_violation, test_failure, non_determinism, missing_receipt, compilation_failure)
- `--sku, -s` (required) - SKU identifier
- `--stage` (optional) - Production stage (auto-inferred if not provided)
- `--message, -m` (optional) - Failure description

**Examples:**
```bash
tcps andon trigger --type test_failure --sku sku_123
tcps andon trigger -t shacl_violation -s sku_456 --stage validation
tcps andon trigger -t compilation_failure -s sku_789 -m "Missing dependency"
```

---

#### `tcps andon list`

List Andon events.

**Options:**
- `--all, -a` (optional) - Show all events including resolved (default: open only)
- `--sku, -s` (optional) - Filter by SKU ID

**Examples:**
```bash
tcps andon list                    # List open Andon events
tcps andon list --all              # List all events (open + resolved)
tcps andon list --sku sku_123      # List events for specific SKU
```

---

#### `tcps andon show`

Show detailed information about an Andon event.

**Usage:**
```bash
tcps andon show <andon-id>
```

**Example:**
```bash
tcps andon show ANDON-1234567-890
```

---

#### `tcps andon resolve`

Resolve an Andon event with root cause analysis.

**Options:**
- `--root-cause` (required) - Root cause analysis
- `--fix` (required) - Fix applied
- `--prevention` (required) - Prevention measures added

**Usage:**
```bash
tcps andon resolve <andon-id> --root-cause "..." --fix "..." --prevention "..."
```

**Example:**
```bash
tcps andon resolve ANDON-123 \
  --root-cause "Timeout in auth module" \
  --fix "Increased timeout from 5s to 10s" \
  --prevention "Added health check before tests"
```

---

### tcps kanban

Kanban WIP management and Heijunka scheduling

#### `tcps kanban status`

Show Kanban WIP status.

**Options:**
- `--bucket, -b` (optional) - Filter by bucket (reliability, security, cost, compliance, all)

**Examples:**
```bash
tcps kanban status                     # All buckets
tcps kanban status --bucket reliability
tcps kanban status -b security
```

**Output:**
```
Bucket          Current  Limit  Available  Utilization
--------------  -------  -----  ---------  -----------
reliability     3        5      2          60.0%
security        5        5      0          100.0%
cost            1        5      4          20.0%
compliance      2        5      3          40.0%
```

---

#### `tcps kanban set-limit`

Set WIP limit for a bucket.

**Usage:**
```bash
tcps kanban set-limit <bucket> <limit>
```

**Examples:**
```bash
tcps kanban set-limit security 10
tcps kanban set-limit reliability 8
```

---

#### `tcps kanban schedule`

Display Heijunka leveled schedule.

**Usage:**
```bash
tcps kanban schedule
```

Shows work orders in optimized execution order with leveling applied.

---

### tcps quality

Quality gate execution and validation

#### `tcps quality check`

Check quality gates for a SKU.

**Options:**
- `--sku, -s` (required) - SKU identifier
- `--gate, -g` (optional) - Specific gate to check (default: all gates)

**Examples:**
```bash
tcps quality check --sku sku_123           # Check all gates
tcps quality check -s sku_123 -g compilation
tcps quality check -s sku_123 -g test_execution
```

---

#### `tcps quality metrics`

Display quality metrics.

**Usage:**
```bash
tcps quality metrics
```

**Output:**
```
Quality Metrics (Last 30 Days)
===============================
Test Pass Rate:      95.2%
Code Coverage:       85.1%
Defect Rate:         3.2%
First Pass Yield:    92.5%
```

---

### tcps kaizen

Continuous improvement commands

#### `tcps kaizen waste`

Identify waste points in production system.

**Usage:**
```bash
tcps kaizen waste
```

**Output:**
```
Waste Points (Last 7 Days)
===========================
1. compilation_slow (compile) - 5.2 hrs/week
   Root cause: Compilation time exceeds 5-minute threshold

2. flaky_test (test) - 3.1 hrs/week
   Root cause: Tests with inconsistent results
```

---

#### `tcps kaizen improve`

Generate improvement proposals from waste.

**Usage:**
```bash
tcps kaizen improve
```

---

#### `tcps kaizen report`

Generate weekly Kaizen report.

**Options:**
- `--week-ending` (optional) - Week ending date (YYYY-MM-DD, default: today)

**Examples:**
```bash
tcps kaizen report
tcps kaizen report --week-ending 2024-01-26
```

---

## Error Codes

### Quality Gates

| Code | Meaning | Resolution |
|------|---------|------------|
| `unknown_gate` | Invalid gate name provided | Use one of: shacl_validation, compilation, test_execution, security_scan, deterministic_build, quality_metrics, release_verification, smoke_test |
| `blocked` | Stage transition blocked | Resolve open Andon events and ensure quality gates pass |

### Kanban

| Code | Meaning | Resolution |
|------|---------|------------|
| `limit_reached` | WIP limit at capacity for bucket | Complete existing work orders or increase WIP limit |

### Andon

| Code | Meaning | Resolution |
|------|---------|------------|
| `already_resolved` | Andon event already resolved | Cannot modify resolved Andon events |
| `andon_not_found` | Invalid Andon ID | Verify Andon ID is correct |

---

## Generated Reference

This documentation is generated from:
- `tcps_diataxis_reference.erl` - Reference engine
- `tcps_api_reference.erl` - API documentation generator
- `tcps_config_reference.erl` - Configuration reference generator

To regenerate:
```erlang
ApiRef = tcps_api_reference:generate_full_reference(),
ConfigRef = tcps_config_reference:generate_full_config_reference(),
tcps_diataxis_reference:to_markdown(ApiRef).
```

---

**Last Updated:** 2026-01-26
**Version:** 0.6.0
**License:** Apache 2.0
