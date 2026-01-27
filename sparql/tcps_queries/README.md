# TCPS SPARQL Queries

This directory contains SPARQL queries for Toyota Commercial Production System (TCPS) work order processing and quality management.

## Query Inventory

### 1. work_orders_pending.rq
**Purpose**: Extract all pending work orders ordered by priority

**Key Features**:
- Filters by status (default: "pending")
- Orders by priority (critical > high > medium > low)
- Secondary sort by creation timestamp (oldest first)

**Parameters**:
- `?status_filter`: Optional status value (default: "pending")

**Returns**: work_order_id, demand_signal, bucket, created_timestamp, priority

**Usage Example**:
```sparql
# Get all pending work orders
# No parameters needed - defaults to "pending" status

# To filter by different status:
# VALUES ?status_filter { "ready" }
# FILTER(?status = ?status_filter)
```

---

### 2. heijunka_schedule.rq
**Purpose**: Create balanced production schedule using Toyota Heijunka leveling

**Key Features**:
- Groups work orders by TPS bucket (reliability/security/cost/compliance)
- Selects N items from each bucket to prevent batch processing
- Interleaves buckets for smooth mixed-model production
- Ranks items within each bucket by priority

**Parameters**:
- `?items_per_bucket`: Number to select from each bucket (default: 5)
- `?max_total_items`: Maximum total schedule size (default: 20)

**Returns**: work_order_id, bucket, priority, rank_in_bucket, created_timestamp

**Usage Example**:
```sparql
# Default: 5 items per bucket, max 20 total
# Override with:
BIND(3 AS ?items_per_bucket)  # Select 3 from each bucket
```

---

### 3. sku_readiness.rq
**Purpose**: Validate if SKU is ready for production deployment

**Key Features**:
- Checks all required receipts are present and passed
- Validates no open Andon (quality alert) events
- Calculates missing/failed receipt counts
- Returns specific blocking issues

**Parameters**:
- `?sku_id`: SKU identifier (REQUIRED)

**Returns**: sku_id, is_ready, missing_receipts, failed_receipts, open_andon_count, blocking_issues

**Usage Example**:
```sparql
BIND("SKU-12345" AS ?sku_id)

# Returns:
# is_ready = true/false
# blocking_issues = "Missing required receipts, Active Andon quality alerts"
```

---

### 4. quality_metrics.rq
**Purpose**: Calculate quality metrics for Kaizen (continuous improvement) analysis

**Key Features**:
- Computes lead time (creation to completion in hours)
- Calculates defect rate (percentage with quality issues)
- Measures rework percentage
- Supports daily/weekly/monthly aggregation
- Counts Andon quality alert events

**Parameters**:
- `?start_date`: Analysis period start (ISO 8601 format, REQUIRED)
- `?end_date`: Analysis period end (ISO 8601 format, REQUIRED)
- `?grouping_period`: Aggregation level - "daily", "weekly", "monthly" (default: "daily")

**Returns**: time_period, work_orders_completed, avg_lead_time_hours, defect_rate, rework_percentage, total_andon_events

**Usage Example**:
```sparql
BIND("2024-01-01T00:00:00Z"^^xsd:dateTime AS ?start_date)
BIND("2024-01-31T23:59:59Z"^^xsd:dateTime AS ?end_date)
BIND("daily" AS ?grouping_period)

# Returns daily metrics for January 2024
```

---

### 5. andon_active.rq
**Purpose**: List all active quality alert (Andon) events

**Key Features**:
- Shows currently open quality issues
- Orders by severity (critical first) and elapsed time
- Includes failure reason and affected SKU
- Calculates time since alert triggered

**Parameters**:
- `?min_severity`: Optional minimum severity ("low", "medium", "high", "critical")
- `?sku_filter`: Optional SKU ID to filter alerts

**Returns**: andon_id, sku_id, failure_reason, severity, triggered_timestamp, elapsed_hours, work_order_id, stage

**Usage Example**:
```sparql
# Show all active alerts
# No parameters needed

# Filter to critical/high only:
BIND("high" AS ?min_severity)

# Filter to specific SKU:
BIND("SKU-12345" AS ?sku_filter)
```

---

### 6. receipts_by_stage.rq
**Purpose**: Extract all quality receipts for a SKU grouped by production stage

**Key Features**:
- Shows all validation receipts in stage order
- Includes pass/fail status for audit trail
- Orders by stage sequence then timestamp
- Shows failure reasons for failed receipts

**Parameters**:
- `?sku_id`: SKU identifier (REQUIRED)
- `?stage_filter`: Optional stage name filter
- `?status_filter`: Optional status filter ("PASS", "FAIL", "PENDING")

**Returns**: receipt_id, sku_id, stage, stage_sequence, status, receipt_timestamp, validator, validation_data, failure_reason

**Usage Example**:
```sparql
BIND("SKU-12345" AS ?sku_id)

# Filter to test stage only:
BIND("test" AS ?stage_filter)

# Show only failures:
BIND("FAIL" AS ?status_filter)
```

---

## Common Patterns

### Binding Parameters
All queries use SPARQL BIND for parameterization:
```sparql
BIND("SKU-12345" AS ?sku_id)
BIND("2024-01-01T00:00:00Z"^^xsd:dateTime AS ?start_date)
BIND(5 AS ?items_per_bucket)
```

### Optional Filters
Most queries support optional filters that default if not bound:
```sparql
FILTER(!BOUND(?filter_var) || ?field = ?filter_var)
```

### Date Handling
Timestamps use xsd:dateTime format:
```sparql
"2024-01-01T00:00:00Z"^^xsd:dateTime
```

### Aggregations
Queries use standard SPARQL aggregations:
```sparql
COUNT(?item) AS ?count
AVG(?value) AS ?average
SUM(?value) AS ?total
```

## Integration with TCPS System

These queries integrate with:
- **erlmcp Knowledge Graph**: RDF store for work order data
- **TCPS Ontology**: Defines tcps: vocabulary (WorkOrder, Receipt, AndonEvent, etc.)
- **UNRDF Engine**: Executes queries via MCP server
- **DSPy Runtime**: Uses query results for AI-driven decision-making
- **OpenTelemetry**: Queries produce spans/traces for observability

## Testing

Query tests should be created in `/tests/sparql/tcps_queries/`:
- Test with sample RDF data
- Validate parameter binding
- Verify aggregation calculations
- Check edge cases (empty results, null values)
- Ensure query performance on large datasets

## References

- **Toyota Production System**: Heijunka (leveling), Andon (quality alerts), Kaizen (continuous improvement)
- **SPARQL 1.1**: https://www.w3.org/TR/sparql11-query/
- **TCPS Ontology**: `/src/kgcl/ontology/tcps.ttl`
- **UNRDF Integration**: `/src/kgcl/unrdf_engine/`
