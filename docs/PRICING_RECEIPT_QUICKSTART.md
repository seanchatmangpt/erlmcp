# Pricing Receipt System - Quick Start Guide

## 5-Minute Overview

The Pricing Receipt System creates immutable, auditable records of plan enforcement. Each receipt captures plan envelope bounds and logs any refusal events with cryptographic verification.

## Installation

The receipt system is built into erlmcp:

```bash
# Receipt storage is automatically created
make check  # Compiles and tests everything

# Receipts stored in
ls -la priv/receipts/
```

## Quick Start: 3 Steps

### Step 1: Create a Receipt

```erlang
{ok, Receipt} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>).
ReceiptId = maps:get(receipt_id, Receipt).
% Receipt created with plan envelope bounds snapshot
```

### Step 2: Log Refusal Events

```erlang
% Request exceeded queue depth
{ok, Updated} = erlmcp_pricing_receipt:add_refusal(
    ReceiptId,
    1001,
    queue_overflow,
    <<"call_tool">>
).
% Hash chain automatically updated
```

### Step 3: Export for Audit

```erlang
% Export all receipts as CSV
{ok, CsvData} = erlmcp_pricing_receipt:export_receipts(free, <<"1.0">>, csv).
file:write_file("receipts.csv", CsvData).
```

Done! You now have an immutable audit trail.

## Common Tasks

### Verify a Receipt

```erlang
% Check that receipt hasn't been tampered with
{ok, verified} = erlmcp_pricing_receipt:verify_receipt(ReceiptId).
```

### Check Conformance

```erlang
% Did actual metrics stay within envelope bounds?
{ok, Result} = erlmcp_pricing_receipt:verify_conformance(ReceiptId, #{
    throughput_actual => 5.0,
    concurrent_actual => 2,
    queue_depth_actual => 5,
    latency_p99_actual => 500.0,
    failover_actual => 20.0
}).

case maps:get(status, Result) of
    pass -> io:format("Within limits~n");
    fail -> io:format("Violations: ~p~n", [maps:get(violations, Result)])
end.
```

### List All Receipts

```erlang
% All receipts for free plan
{ok, Receipts} = erlmcp_pricing_receipt:list_receipts(free).

% Only version 1.0
{ok, ReceiptsV1} = erlmcp_pricing_receipt:list_receipts(free, <<"1.0">>).
```

### Verify Chain Integrity

```erlang
% Check that hash chain is unbroken for entire plan
{ok, complete} = erlmcp_pricing_receipt:verify_receipt_chain(free).
```

## Plan Envelopes at a Glance

| Plan | Throughput | Concurrent | Queue | Latency P99 | Failover |
|------|-----------|-----------|-------|------------|----------|
| Free | 10 req/s | 5 | 10 | 1000ms | 30s |
| Starter | 100 req/s | 50 | 100 | 500ms | 15s |
| Pro | 1000 req/s | 500 | 1000 | 200ms | 5s |

## Export Formats

All three formats preserve the same data, just different structure:

```bash
# JSON (for APIs)
erlmcp receipt export free 1.0 json > free-receipts.json

# CSV (for Excel)
erlmcp receipt export free 1.0 csv > free-receipts.csv

# TSV (for Tab-separated spreadsheets)
erlmcp receipt export free 1.0 tsv > free-receipts.tsv
```

## Refusal Codes Quick Reference

| Code | Reason | Means |
|------|--------|-------|
| 1001 | queue_overflow | Too many queued requests |
| 1002 | rate_limit_exceeded | Requests per second too high |
| 1003 | circuit_breaker_open | Service temporarily unavailable |
| 1004 | concurrent_limit | Too many concurrent requests |
| 1005 | throughput_limit | Throughput limit exceeded |
| 1006 | latency_p99_violated | Response time too slow |
| 1007 | failover_timeout | Failover detection triggered |
| 1008 | plan_expired | Billing period ended |

## File Structure

Receipts are stored as JSON files (one per timestamp):

```
priv/receipts/
├── free/1.0/20250127-123456.receipt.json
├── free/1.0/20250127-123500.receipt.json
├── starter/1.0/20250127-124000.receipt.json
└── pro/1.0/20250127-130000.receipt.json
```

Each file is immutable (write-once, never modified).

## Key Concepts

### Immutability

Once a receipt is created, it's written to disk and never changed. New refusal events are recorded by:
1. Creating new hash chain entry
2. Updating previous_hash to current receipt's hash
3. Computing new hash that includes the refusal

This prevents tampering - any modification breaks the hash chain.

### Hash Chain

Each receipt links to the previous one:

```
Receipt 1: hash = H1, previous = null
Receipt 2: hash = H2, previous = H1
Receipt 3: hash = H3, previous = H2
```

If Receipt 2 is modified, H2 changes, breaking the link to Receipt 3. Verification catches this.

### Deterministic Hashing

Same receipt data always produces same SHA-256 hash. This means:
- Auditors can independently verify hashes
- Reproducible across runs
- Tamper-proof

## Compliance Use Cases

### Quarterly Audit Report

```erlang
% Generate audit spreadsheet
{ok, CsvData} = erlmcp_pricing_receipt:export_receipts(pro, <<"1.0">>, csv),
file:write_file("audit/Q1-2025-pro-usage.csv", CsvData),

% Then verify chain integrity
{ok, complete} = erlmcp_pricing_receipt:verify_receipt_chain(pro),
io:format("Chain verified - no tampering detected~n").
```

### SLA Verification

```erlang
% Check if customer stayed within SLA bounds
{ok, Receipt} = erlmcp_pricing_receipt:get_receipt(ReceiptId),
{ok, ConformResult} = erlmcp_pricing_receipt:verify_conformance(ReceiptId, ActualMetrics),

case maps:get(status, ConformResult) of
    pass ->
        io:format("Customer within SLA~n");
    fail ->
        Violations = maps:get(violations, ConformResult),
        io:format("SLA violations: ~p~n", [Violations])
end.
```

### Refusal Event Investigation

```erlang
% Find all refusal events for a period
{ok, Receipts} = erlmcp_pricing_receipt:list_receipts(free),

RefusalsWithDetails = [
    {maps:get(receipt_id, R), maps:get(refusal_trigger, R)}
    || R <- Receipts,
       maps:is_key(refusal_trigger, R)
],

io:format("Total refusals: ~w~n", [length(RefusalsWithDetails)]).
```

## Testing

### Run All Tests

```bash
rebar3 eunit --module=erlmcp_pricing_receipt_basic_test
rebar3 ct --suite=erlmcp_pricing_receipt_extended_SUITE
```

### Quick Test in Shell

```erlang
% Start erlmcp
rebar3 shell

% Test basic functionality
(erlmcp@localhost)1> {ok, R} = erlmcp_pricing_receipt:create_receipt(free, <<"1.0">>).
(erlmcp@localhost)2> maps:get(receipt_id, R).
(erlmcp@localhost)3> erlmcp_pricing_receipt:verify_receipt(RId).
{ok, verified}
```

## Storage Details

### Receipt File Location

```
priv/receipts/
  plan_id/
    version/
      YYYYMMDD-HHMMSS.receipt.json
```

Example: `priv/receipts/pro/1.0/20250127-143022.receipt.json`

### File Permissions

```bash
# Receipts are created with read-only permissions
ls -la priv/receipts/free/1.0/
-r--r--r-- 1 erlmcp staff 2048 Jan 27 14:30 20250127-143022.receipt.json
```

### Storage Limits

No built-in limits, but typical usage:
- ~2-3 KB per receipt
- 1000 receipts/month per plan = ~2-3 MB
- Recommend archiving yearly

## Troubleshooting

### "Receipt not found"

```erlang
{error, not_found} = erlmcp_pricing_receipt:get_receipt(WrongId).
```

**Fix**: Verify the receipt ID is correct and formatted as UUID.

### "Hash mismatch"

```erlang
{error, hash_mismatch} = erlmcp_pricing_receipt:verify_receipt(Id).
```

**Fix**: Receipt file was modified. Restore from backup or investigate tampering.

### "Conformance violations"

```erlang
Result = #{status => fail, violations => [throughput_exceeded]}.
```

**Fix**: Customer exceeded plan limits. Either:
1. Upgrade to higher tier plan
2. Reduce load
3. Investigate if legitimate spike or attack

## Next Steps

1. **Integration**: Receipt system auto-logs in erlmcp_server at refusal time
2. **Monitoring**: Set up alerts when violations detected
3. **Archival**: Archive quarterly receipts for compliance records
4. **Audit**: Run verification checks regularly

## API Reference Cheat Sheet

```erlang
% Create
{ok, R} = erlmcp_pricing_receipt:create_receipt(Plan, Version).

% Verify
{ok, verified} = erlmcp_pricing_receipt:verify_receipt(ReceiptId).

% Refusal
{ok, R2} = erlmcp_pricing_receipt:add_refusal(Id, Code, Reason, Action).

% Conformance
{ok, CR} = erlmcp_pricing_receipt:verify_conformance(Id, Metrics).

% Export
{ok, Data} = erlmcp_pricing_receipt:export_receipt(Id, json|csv|tsv).

% List
{ok, Rs} = erlmcp_pricing_receipt:list_receipts(Plan).
{ok, Rs} = erlmcp_pricing_receipt:list_receipts(Plan, Version).

% Hash
Hash = erlmcp_pricing_receipt:compute_hash(Receipt).

% Chain
{ok, complete} = erlmcp_pricing_receipt:verify_receipt_chain(Plan).
```

## Questions?

See full documentation: `/Users/sac/erlmcp/docs/PRICING_RECEIPT_SYSTEM.md`

Schema reference: `/Users/sac/erlmcp/shapes/pricing_receipt.schema.json`

Test examples: `/Users/sac/erlmcp/test/erlmcp_pricing_receipt_*`
