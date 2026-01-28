# Receipt Tooling and Evidence Plane Inspection

## Overview

erlmcp receipt tooling provides command-line utilities for inspecting, verifying, and exporting evidence from the receipt system. Receipts are immutable audit records that document each stage of the build/release pipeline with cryptographic integrity.

## Receipt Schema

### Core Fields

```json
{
  "receipt_id": "RCPT-1769534117799767-449012-15362",
  "timestamp": 1769534117799,
  "timestamp_iso": "2026-01-27T17:15:17.799Z",
  "receipt_type": "andon_event",
  "sku_id": "SKU-PERF-317",
  "stage": "testing",
  "status": "pass",
  "failure_type": "test_failure",
  "andon_event_id": "ANDON-1769534117799671-870154-15298",
  "ontology_refs": [
    "http://example.org/tcps/ontology#AndonEvent",
    "http://example.org/tcps/ontology#test_failure",
    "http://example.org/tcps/ontology#testing",
    "http://example.org/tcps/ontology#StopTheLine"
  ],
  "evidence": "Optional evidence data",
  "details": {}
}
```

### Field Types

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `receipt_id` | binary | yes | Unique receipt identifier (timestamp-based) |
| `timestamp` | integer | yes | Unix timestamp in milliseconds |
| `timestamp_iso` | binary | yes | ISO 8601 formatted timestamp |
| `receipt_type` | binary | yes | Type of receipt (andon_event, test_receipt, quality_gate, etc.) |
| `sku_id` | binary | yes | Product SKU identifier |
| `stage` | binary | no | Pipeline stage (compile, test, release, deploy) |
| `status` | binary | no | Pass/fail status |
| `evidence` | binary/map | no | Evidence data (test results, logs, metrics) |
| `details` | map | no | Additional structured details |
| `ontology_refs` | [binary] | no | References to TCPS ontology concepts |

## CLI Commands

### `erlmcp_receipt_cli:tail(N)`

Display the N most recent receipts in table format (sorted newest first).

**Usage:**
```erlang
{ok, Receipts, Formatted} = erlmcp_receipt_cli:tail(10).
io:format("~s", [Formatted]).
```

**Parameters:**
- `N` (integer): Number of receipts to show (0 for empty)

**Returns:**
- `{ok, [receipt()], iodata()}` - List of receipts and formatted output
- `{error, term()}` - Error with reason

**Output Example:**
```
RECEIPT_ID                             | TYPE      | SKU_ID                    | STAGE     | STATUS | TIMESTAMP
------------------------------------------------------------------------------------------------------------
RCPT-1769534117799767-449012-15362     | andon_event | SKU-PERF-317              | testing   | pass   | 2026-01-27T17:15:17.799Z
RCPT-1769496344904-472155-4624         | test_receipt | SKU-AUDIT-GEN-001        | execution | pass   | 2026-01-27T06:45:44.904Z
```

### `erlmcp_receipt_cli:tail(N, Format)`

Display N most recent receipts in specified format.

**Formats:**
- `json` - JSON array with full receipt data
- `csv` - Comma-separated values (RFC 4180)
- `tsv` - Tab-separated values
- `table` - ASCII table (default)

**Usage:**
```erlang
% JSON output
{ok, Receipts, JsonData} = erlmcp_receipt_cli:tail(5, json).
io:format("~s", [JsonData]).

% CSV for spreadsheet import
{ok, Receipts, CsvData} = erlmcp_receipt_cli:tail(100, csv).
file:write_file("receipts.csv", CsvData).
```

### `erlmcp_receipt_cli:show(ReceiptId)`

Display full details for a single receipt with hash verification.

**Usage:**
```erlang
{ok, Receipt} = erlmcp_receipt_cli:show(<<"RCPT-1769534117799767-449012-15362">>).

% Check hash verification
case maps:get(hash_verified, Receipt) of
    true -> io:format("Hash verified~n");
    false -> io:format("WARNING: Hash mismatch!~n")
end.
```

**Returns:**
- `{ok, receipt()}` - Complete receipt with computed hash and verification status
  - Additional field: `computed_hash` (base64-encoded SHA256)
  - Additional field: `hash_verified` (boolean)
- `{error, {not_found, ReceiptId}}` - Receipt doesn't exist
- `{error, term()}` - Other error

**Example Output:**
```erlang
#{
  <<"receipt_id">> => <<"RCPT-1769534117799767-449012-15362">>,
  <<"timestamp">> => 1769534117799,
  <<"timestamp_iso">> => <<"2026-01-27T17:15:17.799Z">>,
  <<"receipt_type">> => <<"andon_event">>,
  <<"sku_id">> => <<"SKU-PERF-317">>,
  <<"stage">> => <<"testing">>,
  <<"status">> => <<"pass">>,
  computed_hash => <<"a1b2c3d4e5f6...(base64)...">>,
  hash_verified => true
}
```

### `erlmcp_receipt_cli:verify(ReceiptId)`

Verify receipt integrity with hash chain validation and schema checking.

**Verification Checks:**
1. All required fields present (receipt_id, timestamp, receipt_type, sku_id)
2. Field types valid (binary for IDs, integer for timestamp)
3. Timestamp within 30 days (not future, not too old)
4. SHA-256 hash matches (if hash stored in receipt)

**Usage:**
```erlang
case erlmcp_receipt_cli:verify(<<"RCPT-1769534117799767-449012-15362">>) of
    {ok, verified} ->
        io:format("Receipt verified~n");
    {error, Reason} ->
        io:format("Verification failed: ~p~n", [Reason])
end.
```

**Returns:**
- `{ok, verified}` - All checks passed
- `{error, missing_required_fields}` - Missing receipt_id, timestamp, receipt_type, or sku_id
- `{error, {invalid_type, Field, ExpectedType}}` - Field has wrong type
- `{error, future_timestamp}` - Timestamp is in the future
- `{error, {timestamp_too_old, DeltaMs}}` - Timestamp older than 30 days
- `{error, hash_mismatch}` - Computed hash doesn't match stored value
- `{error, {not_found, ReceiptId}}` - Receipt doesn't exist

### `erlmcp_receipt_cli:export(ReceiptId, Format)`

Export a single receipt in specified format.

**Formats:**
- `json` - JSON object (deterministic key order for reproducible hashing)
- `csv` - Single row with header (7 key fields)
- `tsv` - Single row with tab separation

**Usage:**
```erlang
% Export as JSON for integration with external systems
{ok, JsonData} = erlmcp_receipt_cli:export(<<"RCPT-123">>, json).
file:write_file("receipt.json", JsonData).

% Export as CSV for data pipelines
{ok, CsvData} = erlmcp_receipt_cli:export(<<"RCPT-123">>, csv).
file:write_file("receipt.csv", CsvData).
```

**Returns:**
- `{ok, iodata()}` - Formatted receipt data
- `{error, {not_found, ReceiptId}}` - Receipt doesn't exist
- `{error, {unsupported_format, Format}}` - Format not recognized
- `{error, term()}` - Other error

**Export Fields (CSV/TSV):**
1. receipt_id
2. receipt_type
3. sku_id
4. stage
5. status
6. timestamp
7. timestamp_iso

## Deterministic Output

All receipt operations support deterministic output for CI/CD pipelines:

- **JSON:** Keys are sorted canonically for reproducible hashing
- **CSV/TSV:** Fixed field order, consistent quoting rules
- **Hash:** SHA-256 of canonical JSON form (deterministic across runs)

**Verification:**
```erlang
% Run export 5 times - all outputs should be identical
Output1 = erlmcp_receipt_cli:export(<<"RCPT-123">>, json),
Output2 = erlmcp_receipt_cli:export(<<"RCPT-123">>, json),
Output3 = erlmcp_receipt_cli:export(<<"RCPT-123">>, json),
true = (Output1 =:= Output2) andalso (Output2 =:= Output3).  % Deterministic
```

## Hash Chain Validation

Receipts can optionally store a computed hash for tampering detection.

**Hash Computation:**
1. Remove existing hash fields from receipt
2. Sort keys alphabetically (deterministic ordering)
3. Encode to JSON without whitespace
4. Compute SHA-256 hash
5. Base64-encode the hash

**Example:**
```erlang
% Manual hash verification
Receipt = #{
  <<"receipt_id">> => <<"RCPT-123">>,
  <<"timestamp">> => 1769534117799,
  <<"receipt_type">> => <<"test">>,
  <<"sku_id">> => <<"SKU-ABC">>
},

% Show computes the hash
{ok, ReceiptWithHash} = erlmcp_receipt_cli:show(<<"RCPT-123">>),
ComputedHash = maps:get(computed_hash, ReceiptWithHash),
io:format("Computed hash: ~s~n", [ComputedHash]).
```

## File Organization

Receipts are stored in the `priv/receipts/` directory (configurable via `ERLMCP_RECEIPTS_DIR`):

```
priv/
├── receipts/
│   ├── RCPT-1769534117799767-449012-15362.json
│   ├── RCPT-1769496344904-472155-4624.json
│   ├── execution-1769496344904.json
│   └── ... (one file per receipt)
```

**File Naming Convention:**
- Pattern: `{receipt_id}.json`
- Example: `RCPT-1769534117799767-449012-15362.json`

## Environment Variables

- `ERLMCP_RECEIPTS_DIR` - Directory containing receipt JSON files (default: `./priv/receipts`)

## Error Handling

All operations return `{ok, Result}` or `{error, Reason}` for consistent error handling:

```erlang
case erlmcp_receipt_cli:verify(ReceiptId) of
    {ok, verified} ->
        handle_valid_receipt();
    {error, {not_found, _}} ->
        handle_missing_receipt();
    {error, hash_mismatch} ->
        handle_tampered_receipt();
    {error, Reason} ->
        handle_other_error(Reason)
end.
```

## Performance

- **tail(N):** O(m*log(m)) where m = total receipts (sorts by timestamp)
- **show(Id):** O(1) disk read
- **verify(Id):** O(1) disk read + O(n) hash computation (n = receipt size)
- **export(Id, Format):** O(1) disk read + O(n) JSON/CSV encoding

## Testing

Run comprehensive test suite:

```bash
# Run all receipt CLI tests
rebar3 ct --suite=erlmcp_receipt_cli_SUITE

# Run with verbose output
rebar3 ct --suite=erlmcp_receipt_cli_SUITE --verbose

# Run single test
rebar3 ct --suite=erlmcp_receipt_cli_SUITE --testcase=test_verify_valid_receipt
```

## Known Limitations

1. **File system only** - Receipts must be stored as JSON files on disk
2. **No cleanup** - Old receipts are never automatically deleted (manual pruning required)
3. **No compression** - Large receipts not compressed in storage
4. **Single machine** - No distributed receipt querying across cluster
5. **Timestamp precision** - 30-day window for timestamp validation is fixed
6. **No encryption** - Receipts stored in plain JSON (use filesystem permissions)

## Integration Examples

### Automated Verification Pipeline

```erlang
verify_build_receipts(SkuId) ->
    case erlmcp_receipt_cli:tail(100, json) of
        {ok, Receipts, _} ->
            SkuReceipts = [R || R <- Receipts,
                                maps:get(<<"sku_id">>, R) =:= SkuId],
            verify_each(SkuReceipts);
        {error, Reason} ->
            {error, {failed_to_list_receipts, Reason}}
    end.

verify_each(Receipts) ->
    Results = [erlmcp_receipt_cli:verify(
                   maps:get(<<"receipt_id">>, R))
               || R <- Receipts],
    case lists:all(fun(R) -> R =:= {ok, verified} end, Results) of
        true -> {ok, all_verified};
        false -> {error, verification_failures}
    end.
```

### Export for Audit Trail

```erlang
export_audit_trail(SkuId) ->
    case erlmcp_receipt_cli:tail(1000, csv) of
        {ok, Receipts, CsvData} ->
            SkuReceipts = [R || R <- Receipts,
                                maps:get(<<"sku_id">>, R) =:= SkuId],
            {ok, CsvData};
        {error, Reason} ->
            {error, Reason}
    end.
```

## See Also

- `erlmcp_receipt.erl` - Receipt storage and chain verification
- `tcps_receipt_verifier.erl` - Comprehensive verification system
- `docs/architecture.md` - System architecture overview
