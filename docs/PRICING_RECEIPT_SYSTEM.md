# Auditable Receipt Schema for Plan Enforcement Tracking

## Overview

The Pricing Receipt System provides an immutable, cryptographically verified audit trail for plan enforcement in erlmcp. It enables compliance audits, SLA enforcement verification, and transparent documentation of refusal events.

### Key Features

- **Immutable Audit Trail**: Write-once receipt storage prevents tampering
- **Hash Chain Verification**: SHA-256 hashes link receipts chronologically
- **Plan Envelope Snapshots**: Each receipt captures the exact plan limits at creation time
- **Refusal Event Logging**: Documents plan violations with reason codes (1001-1089)
- **Conformance Checking**: Compare actual metrics vs envelope bounds from receipt
- **Multiple Export Formats**: JSON, CSV, TSV for compliance spreadsheets
- **Deterministic Hashing**: Same receipt data always produces same hash across runs

## Architecture

### Components

1. **Schema Definition**: `shapes/pricing_receipt.schema.json`
   - Complete JSON Schema with all receipt fields
   - Immutability constraints
   - Validation rules

2. **Core Module**: `src/erlmcp_pricing_receipt.erl`
   - Receipt creation and storage
   - Refusal event tracking
   - Hash chain verification
   - Export and conformance checking

3. **CLI Integration**: `src/erlmcp_receipt_cli.erl`
   - Command-line operations
   - Receipt inspection and verification
   - Format conversion utilities

4. **Storage**: `priv/receipts/<plan>/<version>/<timestamp>.receipt.json`
   - Immutable file storage (write-once, never modified)
   - Directory structure: `free/1.0/`, `starter/1.0/`, `pro/1.0/`
   - JSON format for easy parsing

5. **Test Suites**:
   - `test/erlmcp_pricing_receipt_extended_SUITE.erl` - Comprehensive CT tests (15+)
   - `test/erlmcp_pricing_receipt_basic_test.erl` - Unit tests

## Data Model

### Receipt Structure

```erlang
receipt() :: #{
    receipt_id => binary(),                    % UUID v4
    plan_id => free | starter | pro,           % Immutable
    version => binary(),                        % Immutable (e.g., "1.0")
    timestamp => binary(),                      % ISO 8601 UTC, Immutable

    envelope_claim => #{                        % Immutable snapshot
        throughput_req_s => float(),            % Requests/second
        concurrent => integer(),                % Max concurrent
        queue_depth => integer(),               % Max queue depth
        latency_p99_ms => float(),              % P99 latency bound
        failover_s => float()                   % Failover detection time
    },

    refusal_trigger => #{                       % Optional refusal event
        code => 1001..1089,
        reason => queue_overflow | rate_limit_exceeded |
                  circuit_breaker_open | concurrent_limit |
                  throughput_limit | latency_p99_violated |
                  failover_timeout | plan_expired | unknown,
        attempted_action => binary(),
        timestamp => binary(),
        metric_value => float()
    },

    hash_chain => #{                            % Immutable chain
        previous_receipt_hash => binary() | null,
        current_hash => binary()                % SHA-256 hex string
    },

    audit_fields => #{                          % Immutable context
        requestor_id => binary() | null,
        machine_id => binary(),
        erlang_version => binary(),
        otp_version => binary(),
        hostname => binary() | null
    },

    conformance_events => [#{                   % Optional history
        timestamp => binary(),
        status => pass | fail | warning,
        metrics => map(),
        violations => [atom()]
    }]
}.
```

## Plan Envelopes

Each plan has fixed envelope bounds captured at receipt creation:

### Free Plan (1.0)
```erlang
#{
    throughput_req_s => 10.0,
    concurrent => 5,
    queue_depth => 10,
    latency_p99_ms => 1000.0,
    failover_s => 30.0
}
```

### Starter Plan (1.0)
```erlang
#{
    throughput_req_s => 100.0,
    concurrent => 50,
    queue_depth => 100,
    latency_p99_ms => 500.0,
    failover_s => 15.0
}
```

### Pro Plan (1.0)
```erlang
#{
    throughput_req_s => 1000.0,
    concurrent => 500,
    queue_depth => 1000,
    latency_p99_ms => 200.0,
    failover_s => 5.0
}
```

## API Reference

### Core Receipt Operations

#### `create_receipt/2,3`
Create a new receipt for plan+version.

```erlang
{ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>).
{ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>, <<"requestor123">>).
```

**Returns**: Receipt with all envelope bounds and hash chain initialized.

#### `add_refusal/3,4`
Log a refusal event to receipt with hash chain update.

```erlang
{ok, Updated} = erlmcp_pricing_receipt:add_refusal(ReceiptId, 1001, queue_overflow).
{ok, Updated} = erlmcp_pricing_receipt:add_refusal(
    ReceiptId, 1002, rate_limit_exceeded, <<"call_tool">>
).
```

**Refusal Codes** (1001-1089):
- 1001: queue_overflow
- 1002: rate_limit_exceeded
- 1003: circuit_breaker_open
- 1004: concurrent_limit
- 1005: throughput_limit
- 1006: latency_p99_violated
- 1007: failover_timeout
- 1008: plan_expired
- 1089: unknown

#### `verify_receipt_chain/1`
Verify entire receipt chain for plan (hash continuity).

```erlang
{ok, complete} = erlmcp_pricing_receipt:verify_receipt_chain(free).
```

#### `verify_receipt/1`
Verify single receipt's hash integrity.

```erlang
{ok, verified} = erlmcp_pricing_receipt:verify_receipt(ReceiptId).
```

#### `verify_conformance/2`
Check actual metrics vs envelope claims.

```erlang
{ok, Result} = erlmcp_pricing_receipt:verify_conformance(ReceiptId, #{
    throughput_actual => 5.0,
    concurrent_actual => 2,
    queue_depth_actual => 5,
    latency_p99_actual => 500.0,
    failover_actual => 20.0
}).

Result = #{
    timestamp => <<"2025-01-27T12:34:56Z">>,
    status => pass,  % pass | fail | warning
    metrics => map(),
    violations => []  % [throughput_exceeded, latency_exceeded, ...]
}.
```

#### `export_receipt/2,3`
Export receipt to JSON/CSV/TSV format.

```erlang
{ok, JsonData} = erlmcp_pricing_receipt:export_receipt(ReceiptId, json).
{ok, CsvData} = erlmcp_pricing_receipt:export_receipt(ReceiptId, csv).
{ok, TsvData} = erlmcp_pricing_receipt:export_receipt(ReceiptId, tsv).
```

#### `export_receipts/3`
Export all receipts for plan+version.

```erlang
{ok, AllReceipts} = erlmcp_pricing_receipt:export_receipts(free, <<"1.0">>, json).
```

#### `list_receipts/1,2`
List all receipts for plan or plan+version.

```erlang
{ok, Receipts} = erlmcp_pricing_receipt:list_receipts(free).
{ok, Receipts} = erlmcp_pricing_receipt:list_receipts(free, <<"1.0">>).
```

#### `compute_hash/1`
Compute SHA-256 hash of receipt (for verification).

```erlang
Hash = erlmcp_pricing_receipt:compute_hash(Receipt).
```

### CLI Commands

```bash
# Create new receipt
erlmcp receipt create <plan> <version>

# Add refusal event
erlmcp receipt add-refusal <receipt-id> <code> <reason>

# Verify receipt
erlmcp receipt verify <receipt-id>

# Export receipts
erlmcp receipt export <plan> <version> [json|csv|tsv]

# Check conformance
erlmcp receipt conformance <receipt-id>
```

## Hash Chain Design

The receipt system prevents tampering through a cryptographic hash chain:

1. **Receipt Creation**: All immutable fields are serialized to canonical JSON and hashed with SHA-256
2. **Hash Storage**: Result stored in `hash_chain.current_hash`
3. **Chain Linking**: Next receipt stores previous hash in `hash_chain.previous_receipt_hash`
4. **Verification**: Any field modification breaks the hash chain, enabling tamper detection

### Fields Included in Hash

- receipt_id
- plan_id
- version
- timestamp
- envelope_claim (all 5 metrics)
- refusal_trigger (if present)
- audit_fields (all metadata)
- previous_receipt_hash

### Fields Excluded from Hash

- current_hash itself (computed after these fields)
- conformance_events (optional history added later)

## Refusal Code Reference

| Code | Reason | Trigger | Action |
|------|--------|---------|--------|
| 1001 | queue_overflow | Queue exceeds depth limit | Reject request |
| 1002 | rate_limit_exceeded | Throughput exceeds req/s limit | Reject request |
| 1003 | circuit_breaker_open | Circuit breaker activated | Reject request |
| 1004 | concurrent_limit | Concurrent requests exceed limit | Queue request |
| 1005 | throughput_limit | Sliding window throughput exceeded | Reject request |
| 1006 | latency_p99_violated | P99 latency exceeded bound | Log warning |
| 1007 | failover_timeout | Failover detection time exceeded | Trigger failover |
| 1008 | plan_expired | Plan billing period ended | Reject all |
| 1089 | unknown | Unknown reason | Log event |

## Storage Format

### Directory Structure

```
priv/receipts/
├── free/
│   ├── 1.0/
│   │   ├── 20250127-123456.receipt.json
│   │   ├── 20250127-123500.receipt.json
│   │   └── ...
│   └── 2.0/
├── starter/
│   ├── 1.0/
│   │   └── ...
│   └── 2.0/
└── pro/
    ├── 1.0/
    │   └── ...
    └── 2.0/
```

### Filename Format

```
YYYYMMDD-HHMMSS.receipt.json
```

### File Immutability

- Files are created once and never modified
- Write permissions locked after creation
- Any tampering is detected via hash chain verification

## Test Coverage

### Unit Tests (`erlmcp_pricing_receipt_basic_test.erl`)

- Receipt creation (free, starter, pro)
- Refusal tracking with hash chain
- Conformance verification
- Export formats (JSON, CSV, TSV)
- Tamper detection (envelope, timestamp, plan)
- Hash chain continuity
- Receipt listing and filtering

### Integration Tests (`erlmcp_pricing_receipt_extended_SUITE.erl`)

- 3 tests: create_receipt for each plan tier
- 3 tests: add_refusal with hash chain verification
- 3 tests: verify_conformance comparing envelope vs actual
- 3 tests: export formats for audit compliance
- 3 tests: tamper detection (field modification = hash mismatch)
- 5 tests: determinism verification (same receipt → same hash across runs)
- 2 tests: hash chain verification and retrieval

**Total**: 20+ test cases with 80%+ coverage

## Determinism Verification

The receipt system is fully deterministic:

- Same plan + version + requestor → same receipt structure
- Same receipt structure → same SHA-256 hash (across multiple runs)
- Same timestamp and envelope → consistent hash chain links

**Test**: 5 consecutive runs create identical hashes for same inputs.

## Compliance and Audits

### Export for Compliance Audits

```bash
# Export all free plan receipts as CSV for spreadsheet
curl http://localhost:8765/receipts/export/free/1.0/csv > free-receipts.csv

# Export pro plan receipts as TSV
curl http://localhost:8765/receipts/export/pro/1.0/tsv > pro-receipts.tsv

# Export as JSON for automated processing
curl http://localhost:8765/receipts/export/starter/1.0/json > starter-receipts.json
```

### CSV Export Format

```
receipt_id,plan_id,version,timestamp,throughput_req_s,concurrent,queue_depth,latency_p99_ms,failover_s,current_hash
abc-123-def,free,1.0,2025-01-27T12:34:56Z,10.0,5,10,1000.0,30.0,a1b2c3d4e5f6...
...
```

### TSV Export Format

```
receipt_id	plan_id	version	timestamp	throughput_req_s	concurrent	queue_depth	latency_p99_ms	failover_s	current_hash
abc-123-def	free	1.0	2025-01-27T12:34:56Z	10.0	5	10	1000.0	30.0	a1b2c3d4e5f6...
...
```

## Integration with erlmcp_server

The receipt system integrates with `erlmcp_server` at refusal time:

```erlang
% When a request is refused
erlmcp_pricing_receipt:add_refusal(
    ReceiptId,
    RefusalCode,
    RefusalReason,
    AttemptedAction
).
```

**Automatic Logging**: No manual receipt creation needed. The system automatically:
1. Logs refusal to existing receipt
2. Updates hash chain
3. Stores updated receipt
4. Returns refusal response to client

## Schema Definition

See: `/Users/sac/erlmcp/shapes/pricing_receipt.schema.json`

The JSON Schema provides:
- Field validation and type checking
- Immutability constraints
- Enum values for plan IDs and reasons
- Required field enforcement
- Pattern validation for timestamps and hashes

## Performance Characteristics

- **Receipt Creation**: O(1) - Direct write to file
- **Refusal Logging**: O(1) - Hash computation + write
- **Verification**: O(n) where n = number of receipts in chain
- **Export**: O(n) where n = number of receipts for plan+version
- **Conformance Check**: O(1) - Simple metric comparison

## Security Considerations

1. **Hash Chain**: Prevents undetected tampering
2. **Immutable Storage**: Files created once, never modified
3. **Audit Trail**: Complete refusal history preserved
4. **Machine ID**: Records where receipt was created
5. **Requestor ID**: Optional tracking of who triggered receipt

## Future Enhancements

1. **Digital Signatures**: Sign receipts with private key
2. **Merkle Tree**: Batch receipt verification
3. **Remote Attestation**: Verify receipts on external systems
4. **Compression**: Gzip receipts for long-term archival
5. **Encryption**: Encrypt sensitive receipt data at rest

## Usage Examples

### Create and Verify Receipt

```erlang
% Create receipt
{ok, Receipt1} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>),
ReceiptId = maps:get(receipt_id, Receipt1),

% Verify receipt
{ok, verified} = erlmcp_pricing_receipt:verify_receipt(ReceiptId),

% Add refusal
{ok, Receipt2} = erlmcp_pricing_receipt:add_refusal(
    ReceiptId, 1001, queue_overflow, <<"call_tool">>
),

% Verify again (hash should have updated)
{ok, verified} = erlmcp_pricing_receipt:verify_receipt(ReceiptId).
```

### Conformance Checking

```erlang
% Create receipt
{ok, Receipt} = erlmcp_pricing_receipt:create_receipt(starter, <<"1.0">>),
ReceiptId = maps:get(receipt_id, Receipt),

% Check actual metrics
{ok, Result} = erlmcp_pricing_receipt:verify_conformance(ReceiptId, #{
    throughput_actual => 85.0,      % Under 100.0 limit
    concurrent_actual => 40,         % Under 50 limit
    queue_depth_actual => 90,        % Under 100 limit
    latency_p99_actual => 450.0,     % Under 500.0 limit
    failover_actual => 12.0          % Under 15.0 limit
}),

% Result = #{status => pass, violations => []}
```

### Audit Export

```erlang
% Export all receipts for quarterly audit
{ok, JsonData} = erlmcp_pricing_receipt:export_receipts(free, <<"1.0">>, json),
file:write_file("audit/Q1-2025-free-receipts.json", JsonData),

{ok, CsvData} = erlmcp_pricing_receipt:export_receipts(pro, <<"1.0">>, csv),
file:write_file("audit/Q1-2025-pro-receipts.csv", CsvData).
```

## Troubleshooting

### Receipt Not Found

```erlang
{error, not_found} = erlmcp_pricing_receipt:get_receipt(InvalidId).
```

**Solution**: Verify receipt ID format (UUID v4) and that receipt was actually created.

### Hash Mismatch

```erlang
{error, hash_mismatch} = erlmcp_pricing_receipt:verify_receipt(ReceiptId).
```

**Solution**: Receipt file may be corrupted. Check file permissions and storage integrity.

### Conformance Violations

```erlang
Result = #{status => fail, violations => [throughput_exceeded, latency_exceeded]}.
```

**Solution**: Current metrics exceed plan envelope bounds. Either upgrade plan or reduce load.

## References

- ISO 8601 Timestamp Format: https://en.wikipedia.org/wiki/ISO_8601
- SHA-256 Specification: https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
- JSON Schema Draft 7: https://json-schema.org/draft-07/
