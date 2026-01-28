# Receipt CLI - Usage Examples

## Quick Start

### 1. Display Last 10 Receipts

```erlang
% In Erlang shell
1> {ok, Receipts, Formatted} = erlmcp_receipt_cli:tail(10).
{ok, [#{<<"receipt_id">>=><<"RCPT-1">>, ...}, ...], [<<"RECEIPT_ID"|...]]}

2> io:format("~s", [Formatted]).
RECEIPT_ID                             | TYPE      | SKU_ID              | STAGE    | STATUS | TIMESTAMP
---------------------------------------+-----+----+----+-----+------+-----
RCPT-1769534117799767-449012-15362     | andon_event | SKU-PERF-317    | testing  | pass   | 2026-01-27T17:15:17.799Z
RCPT-1769496344904-472155-4624         | test_receipt | SKU-AUDIT-001  | execution| pass   | 2026-01-27T06:45:44.904Z
...
```

### 2. Show Single Receipt with Hash Verification

```erlang
1> {ok, R} = erlmcp_receipt_cli:show(<<"RCPT-1769534117799767-449012-15362">>).
{ok, #{
  <<"receipt_id">> => <<"RCPT-1769534117799767-449012-15362">>,
  <<"timestamp">> => 1769534117799,
  <<"receipt_type">> => <<"andon_event">>,
  <<"sku_id">> => <<"SKU-PERF-317">>,
  <<"stage">> => <<"testing">>,
  <<"status">> => <<"pass">>,
  computed_hash => <<"...base64-encoded-sha256...">>,
  hash_verified => true
}}

2> maps:get(hash_verified, R).
true
```

### 3. Verify Receipt Integrity

```erlang
1> erlmcp_receipt_cli:verify(<<"RCPT-1769534117799767-449012-15362">>).
{ok, verified}

% Failed verification
2> erlmcp_receipt_cli:verify(<<"INVALID-RECEIPT">>).
{error, {not_found, "INVALID-RECEIPT"}}

% Tampered receipt (hash mismatch)
3> erlmcp_receipt_cli:verify(<<"TAMPERED-RECEIPT">>).
{error, hash_mismatch}
```

## Export Examples

### Export to JSON (for API integration)

```erlang
% Single receipt
{ok, JsonData} = erlmcp_receipt_cli:export(<<"RCPT-001">>, json).

% Write to file
file:write_file("receipt.json", JsonData).

% JSON structure
{
  "receipt_id": "RCPT-1769534117799767-449012-15362",
  "timestamp": 1769534117799,
  "receipt_type": "andon_event",
  "sku_id": "SKU-PERF-317",
  "stage": "testing",
  "status": "pass",
  "timestamp_iso": "2026-01-27T17:15:17.799Z"
}
```

### Export to CSV (for spreadsheet)

```erlang
% Single receipt
{ok, CsvData} = erlmcp_receipt_cli:export(<<"RCPT-001">>, csv).

% Result
receipt_id,receipt_type,sku_id,stage,status,timestamp,timestamp_iso
RCPT-1769534117799767-449012-15362,andon_event,SKU-PERF-317,testing,pass,1769534117799,2026-01-27T17:15:17.799Z

% Multiple receipts to CSV
{ok, Receipts, CsvData} = erlmcp_receipt_cli:tail(100, csv).
file:write_file("receipts.csv", CsvData).
```

### Export to TSV (for data pipelines)

```erlang
% Same as CSV but tab-separated
{ok, TsvData} = erlmcp_receipt_cli:export(<<"RCPT-001">>, tsv).

% Write to file
file:write_file("receipt.tsv", TsvData).

% Load into data tools
% awk, sed, pandas, spark all handle TSV natively
```

## Format Selection

### Table Format (Default, Human-Readable)

```erlang
% For interactive use
{ok, _, TableOutput} = erlmcp_receipt_cli:tail(5, table).

% Output
RECEIPT_ID                             | TYPE      | SKU_ID              | STAGE    | STATUS
---------------------------------------+-----+----+----+-----+------+-----
RCPT-001                               | test_receipt | SKU-TEST-001   | compile  | pass
RCPT-002                               | andon_event | SKU-PERF-317    | testing  | fail
RCPT-003                               | quality_gate | SKU-QA-001     | release  | pass
```

### JSON Format (Machine-Readable)

```erlang
% For API responses and integrations
{ok, _, JsonOutput} = erlmcp_receipt_cli:tail(2, json).

% Produces valid JSON array
[
  {
    "receipt_id": "RCPT-001",
    "timestamp": 1769561104031,
    ...
  },
  {
    "receipt_id": "RCPT-002",
    "timestamp": 1769561100031,
    ...
  }
]
```

### CSV Format (Data Interchange)

```erlang
% For spreadsheets and data warehouses
{ok, _, CsvOutput} = erlmcp_receipt_cli:tail(100, csv).

% Result
receipt_id,receipt_type,sku_id,stage,status,timestamp,timestamp_iso
RCPT-001,test_receipt,SKU-001,compile,pass,1769561104031,2026-01-27T12:00:00.000Z
RCPT-002,andon_event,SKU-002,testing,fail,1769561100031,2026-01-27T11:59:00.000Z
...

% Open in Excel or import into database
```

## Batch Operations

### Export All Receipts for an SKU

```erlang
% Get all receipts
{ok, AllReceipts, _} = erlmcp_receipt_cli:tail(10000).

% Filter by SKU_ID
SkuId = <<"SKU-PERF-317">>,
SkuReceipts = [R || R <- AllReceipts,
                    maps:get(<<"sku_id">>, R) =:= SkuId],

% Export to CSV
{ok, SkuCsv} = erlmcp_receipt_cli:tail(1000, csv),
FilteredCsv = filter_csv_lines(SkuCsv, SkuId),

file:write_file("sku_receipts.csv", FilteredCsv).
```

### Verify All Receipts in a Stage

```erlang
% Get all receipts
{ok, Receipts, _} = erlmcp_receipt_cli:tail(1000).

% Filter by stage
TestingReceipts = [R || R <- Receipts,
                        maps:get(<<"stage">>, R) =:= <<"testing">>],

% Verify each
VerifyResults = [erlmcp_receipt_cli:verify(
                    maps:get(<<"receipt_id">>, R))
                 || R <- TestingReceipts],

% Check all passed
AllVerified = lists:all(fun(R) -> R =:= {ok, verified} end, VerifyResults),
case AllVerified of
    true -> io:format("All testing stage receipts verified~n");
    false -> io:format("Some receipts failed verification~n")
end.
```

## Deterministic Output (for Continuous Integration)

### Hash Verification in CI Pipeline

```erlang
% Verify same receipt produces same output 5 times
verify_deterministic(ReceiptId) ->
    Outputs = [erlmcp_receipt_cli:export(ReceiptId, json)
               || _ <- lists:seq(1, 5)],

    % All should succeed
    true = lists:all(fun({ok, _}) -> true; (_) -> false end, Outputs),

    % Extract binaries
    Binaries = [iolist_to_binary(Data) || {ok, Data} <- Outputs],

    % All should be identical
    [First | Rest] = Binaries,
    true = lists:all(fun(B) -> B =:= First end, Rest),

    {ok, deterministic}.
```

### Hash Chain in Build Pipeline

```erlang
% Store hash after each build stage
record_stage_receipt(StageName, SkuId) ->
    Receipt = #{
        <<"receipt_id">> => generate_receipt_id(),
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"receipt_type">> => <<"build_stage">>,
        <<"sku_id">> => SkuId,
        <<"stage">> => StageName,
        <<"status">> => <<"pass">>,
        <<"hash">> => erlmcp_receipt_cli:computed_hash(#{
            % Content to hash
        })
    },
    save_receipt(Receipt).

% Verify chain
verify_stage_chain(SkuId) ->
    {ok, Receipts, _} = erlmcp_receipt_cli:tail(1000),
    SkuReceipts = [R || R <- Receipts,
                        maps:get(<<"sku_id">>, R) =:= SkuId],
    Stages = [compile, test, release, deploy],

    % Verify each stage present and in order
    verify_stages_present(SkuReceipts, Stages).
```

## Error Handling

### Handle Missing Receipts

```erlang
case erlmcp_receipt_cli:show(ReceiptId) of
    {ok, Receipt} ->
        process_receipt(Receipt);
    {error, {not_found, Id}} ->
        io:format("Receipt not found: ~s~n", [Id]),
        handle_missing();
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason]),
        handle_error(Reason)
end.
```

### Handle Verification Failures

```erlang
case erlmcp_receipt_cli:verify(ReceiptId) of
    {ok, verified} ->
        io:format("Verified: ~s~n", [ReceiptId]);
    {error, missing_required_fields} ->
        io:format("Receipt missing required fields~n");
    {error, hash_mismatch} ->
        io:format("WARNING: Receipt tampered or corrupted~n"),
        alert_security_team();
    {error, future_timestamp} ->
        io:format("Invalid: future timestamp~n");
    {error, {timestamp_too_old, DeltaMs}} ->
        io:format("Invalid: timestamp too old (~w ms)~n", [DeltaMs]);
    {error, Reason} ->
        io:format("Verification failed: ~p~n", [Reason])
end.
```

## Performance Optimization

### Lazy Evaluation of Large Batches

```erlang
% Don't load all receipts at once for large datasets
{ok, RecentReceipts, _} = erlmcp_receipt_cli:tail(100).

% Process only what you need
process_recent_receipts(RecentReceipts) ->
    lists:foreach(fun(Receipt) ->
        ReceiptId = maps:get(<<"receipt_id">>, Receipt),
        case erlmcp_receipt_cli:verify(ReceiptId) of
            {ok, verified} ->
                log_verification(ReceiptId, ok);
            {error, Reason} ->
                log_verification(ReceiptId, {error, Reason})
        end
    end, RecentReceipts).
```

### Batch Export for Data Lake

```erlang
% Export in chunks to avoid memory issues
export_receipts_chunked(ChunkSize) ->
    {ok, AllReceipts, _} = erlmcp_receipt_cli:tail(10000),
    Chunks = chunk_list(AllReceipts, ChunkSize),

    lists:foreach(fun({Index, Chunk}) ->
        {ok, ChunkJson} = erlmcp_receipt_cli:tail(ChunkSize, json),
        Filename = io_lib:format("receipts_~4..0B.json", [Index]),
        file:write_file(Filename, ChunkJson)
    end, Chunks).

chunk_list(List, ChunkSize) ->
    lists:enumerate(1, lists:foldr(
        fun chunk_helper(E, Acc) -> Acc end,
        [],
        List
    )).
```

## Integration with Monitoring

### Export for OTEL/Datadog

```erlang
% Convert receipts to metric-compatible format
exports_to_metrics(Receipts) ->
    [begin
        #{
            metric => "erlmcp.receipt",
            timestamp => maps:get(<<"timestamp">>, R),
            tags => #{
                stage => maps:get(<<"stage">>, R),
                status => maps:get(<<"status">>, R),
                sku => maps:get(<<"sku_id">>, R)
            },
            value => 1
        }
    end || R <- Receipts].

% Or for log shipping
receipts_to_logs(Receipts) ->
    [begin
        {ok, JsonData} = erlmcp_receipt_cli:export(
            maps:get(<<"receipt_id">>, R), json),
        {
            maps:get(<<"timestamp">>, R),
            maps:get(<<"stage">>, R),
            JsonData
        }
    end || R <- Receipts].
```

## Testing Recipes

### Determinism Test (5 Runs)

```bash
#!/bin/bash
for i in {1..5}; do
  erl -noshell -eval '
    {ok, Data} = erlmcp_receipt_cli:export(<<"RCPT-001">>, json),
    Binary = iolist_to_binary(Data),
    erlang:display(binary_to_list(crypto:hash(md5, Binary)))
  ' -s init stop
done

# All outputs should be identical
```

### Hash Chain Verification

```erlang
-module(verify_chain).
-export([test/0]).

test() ->
    % Generate test receipts
    R1 = #{<<"receipt_id">>=><<"R1">>, <<"timestamp">>=>1000,
           <<"receipt_type">>=><<"test">>, <<"sku_id">>=><<"SKU">>},
    R2 = #{<<"receipt_id">>=><<"R2">>, <<"timestamp">>=>2000,
           <<"receipt_type">>=><<"test">>, <<"sku_id">>=><<"SKU">>},

    % Compute hashes
    H1 = erlmcp_receipt_cli:computed_hash(R1),
    H2 = erlmcp_receipt_cli:computed_hash(R2),

    % Store hashes in receipt (optional)
    R1WithHash = R1#{<<"hash">>=>H1},
    R2WithHash = R2#{<<"hash">>=>H2},

    io:format("Chain verification:~n"),
    io:format("R1 hash: ~s~n", [H1]),
    io:format("R2 hash: ~s~n", [H2]),
    io:format("✓ Chain complete~n").
```

## Command Reference

| Function | Usage | Output |
|----------|-------|--------|
| `tail(N)` | Get N newest receipts | `{ok, [receipt()], iodata()}` |
| `tail(N, Format)` | Get N receipts in format | `{ok, [receipt()], iodata()}` |
| `show(Id)` | Get single receipt details | `{ok, receipt()}` |
| `verify(Id)` | Verify receipt integrity | `{ok, verified}` or error |
| `export(Id, Format)` | Export receipt | `{ok, iodata()}` or error |
| `computed_hash(R)` | Compute SHA-256 | `binary()` (base64-encoded) |
| `format_receipt_json(R)` | Format as JSON | `iodata()` |
| `format_receipt_csv(R)` | Format as CSV | `iodata()` |
| `format_receipt_tsv(R)` | Format as TSV | `iodata()` |
| `format_receipt_table(List)` | Format as table | `iodata()` |

## See Also

- `docs/reference/receipts.md` - Complete reference documentation
- `src/erlmcp_receipt_cli.erl` - Source code
- `test/erlmcp_receipt_cli_SUITE.erl` - Test suite
