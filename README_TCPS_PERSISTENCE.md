# TCPS Persistence System

Comprehensive dual storage system for TCPS receipts with JSON and RDF ontology persistence.

## Features

### 1. Receipt Storage (JSON + Checksums)
- **Path**: `priv/receipts/<sku_id>/<stage>-<timestamp>.json`
- **SHA-256 checksums** for integrity verification
- **Automatic validation** on load
- **Sorted retrieval** by timestamp

```erlang
% Store receipt
Receipt = #{
    sku_id => <<"SKU-001">>,
    stage => compile,
    timestamp => <<"2026-01-26T10:00:00Z">>,
    status => pass,
    evidence => <<"Compilation successful">>
},
{ok, Path} = tcps_persistence:store_receipt(Receipt).

% Load and verify
{ok, LoadedReceipt} = tcps_persistence:load_receipt(Path),
ok = tcps_persistence:verify_receipt(LoadedReceipt).

% Get all receipts for SKU
AllReceipts = tcps_persistence:get_all_receipts(<<"SKU-001">>).
```

### 2. RDF Ontology Persistence
- **Receipts**: `ontology/receipts.ttl`
- **Work Orders**: `ontology/work_orders.ttl`
- **Andon Events**: `ontology/andon_events.ttl`
- **Root Cause Analyses**: `ontology/root_cause_analyses.ttl`
- **Change Log**: `ontology/changes.ttl`

```erlang
% Persist receipt to ontology
ok = tcps_persistence:persist_to_ontology(Receipt).

% Persist work order
WorkOrder = #{
    id => <<"WO-001">>,
    sku_id => <<"SKU-001">>,
    bucket => production,
    priority => 1,
    sla => 3600,
    created_at => <<"2026-01-26T10:00:00Z">>
},
ok = tcps_persistence:persist_work_order(WorkOrder).

% Persist Andon event
AndonEvent = #{
    id => <<"ANDON-001">>,
    sku_id => <<"SKU-001">>,
    severity => critical,
    status => open,
    created_at => <<"2026-01-26T10:00:00Z">>
},
ok = tcps_persistence:persist_andon(AndonEvent).

% Persist root cause analysis
Analysis = #{
    id => <<"RCA-001">>,
    problem => <<"Build failure">>,
    root_cause => <<"Missing dependency">>,
    countermeasure => <<"Add dependency to manifest">>
},
ok = tcps_persistence:persist_root_cause(Analysis).
```

### 3. SPARQL Query Interface

```erlang
% Query receipts by SKU
Receipts = tcps_persistence:query_receipts_by_sku(<<"SKU-001">>).

% Query work orders by bucket
WorkOrders = tcps_persistence:query_work_orders_by_bucket(production).

% Query open Andon events
OpenAndons = tcps_persistence:query_open_andons().

% Custom SPARQL query
Query = <<"PREFIX tcps: <http://example.org/tcps#>
          SELECT ?receipt ?stage ?timestamp
          WHERE {
            ?receipt rdf:type tcps:Receipt ;
                     tcps:stage ?stage ;
                     tcps:timestamp ?timestamp .
            FILTER(?timestamp > \"2026-01-26T00:00:00Z\"^^xsd:dateTime)
          }
          ORDER BY ?timestamp DESC">>,
{ok, Results} = tcps_persistence:query_ontology(Query).
```

### 4. Backup and Restore

```erlang
% Backup all data
BackupPath = <<"backups/backup-2026-01-26">>,
ok = tcps_persistence:backup_all(BackupPath).
% Creates: backups/backup-2026-01-26.tar.gz

% Restore from backup
ok = tcps_persistence:restore_from_backup(<<"backups/backup-2026-01-26.tar.gz">>).
```

### 5. Ontology Validation (SHACL)

```erlang
% Validate ontology
case tcps_persistence:validate_ontology() of
    {ok, valid} ->
        io:format("Ontology is valid~n");
    {error, Violations} ->
        io:format("Violations found: ~p~n", [Violations]),
        % Attempt automatic repair
        {ok, repaired} = tcps_persistence:repair_ontology(Violations)
end.
```

### 6. Change Log and Audit Trail

```erlang
% Log change
Change = #{
    action => created,
    timestamp => <<"2026-01-26T10:00:00Z">>
},
ok = tcps_persistence:log_change(receipt, <<"RECEIPT-001">>, Change).

% Get change history
History = tcps_persistence:get_change_history(work_order, <<"WO-001">>).
```

### 7. Export Functions

```erlang
% Export ontology in different formats
{ok, TurtleContent} = tcps_persistence:export_ontology(turtle).
{ok, NTriplesContent} = tcps_persistence:export_ontology(ntriples).
{ok, JsonLdContent} = tcps_persistence:export_ontology(jsonld).

% Export receipts
{ok, JsonContent} = tcps_persistence:export_receipts(<<"SKU-001">>, json).
{ok, CsvContent} = tcps_persistence:export_receipts(<<"SKU-001">>, csv).
{ok, PdfContent} = tcps_persistence:export_receipts(<<"SKU-001">>, pdf).
```

## Setup

### 1. Install Python Dependencies

```bash
cd priv/python
pip3 install -r requirements.txt
```

### 2. Configure Application

Edit `config/sys.config`:

```erlang
{tcps, [
    {persistence, #{
        ontology_dir => "ontology",
        receipts_dir => "priv/receipts",
        backup_dir => "backups",
        rdf_format => turtle,
        auto_backup => daily,
        backup_retention => 30
    }}
]}
```

### 3. Initialize Persistence

```erlang
% In your application startup
tcps_persistence:init().
```

## RDF Ontology Structure

### Receipt Example (Turtle)

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix tcps: <http://example.org/tcps#> .

tcps:Receipt_COMPILE_2026-01-26T10:00:00Z a tcps:Receipt ;
    tcps:stage tcps:COMPILE ;
    tcps:sku tcps:SKU_001 ;
    tcps:timestamp "2026-01-26T10:00:00Z"^^xsd:dateTime ;
    tcps:status "pass" ;
    tcps:evidence "Compilation successful" .
```

### Work Order Example

```turtle
tcps:WorkOrder_WO-001 a tcps:WorkOrder ;
    tcps:id "WO-001" ;
    tcps:sku tcps:SKU_001 ;
    tcps:bucket "production" ;
    tcps:priority 1 ;
    tcps:sla 3600 ;
    tcps:createdAt "2026-01-26T10:00:00Z"^^xsd:dateTime .
```

### Andon Event Example

```turtle
tcps:Andon_ANDON-001 a tcps:AndonEvent ;
    tcps:id "ANDON-001" ;
    tcps:sku tcps:SKU_001 ;
    tcps:severity "critical" ;
    tcps:status "open" ;
    tcps:createdAt "2026-01-26T10:00:00Z"^^xsd:dateTime .
```

## Testing

### Run Comprehensive Test Suite

```bash
# Run all persistence tests
rebar3 ct --suite tests/tcps_persistence_SUITE

# Run RDF utils tests
rebar3 ct --suite tests/rdf_utils_SUITE

# Run specific test group
rebar3 ct --suite tests/tcps_persistence_SUITE --group receipt_storage
rebar3 ct --suite tests/tcps_persistence_SUITE --group sparql_queries
rebar3 ct --suite tests/tcps_persistence_SUITE --group backup_restore
rebar3 ct --suite tests/tcps_persistence_SUITE --group concurrent
```

### Test Coverage

The test suite includes:

- **Receipt Storage**: Store, load, verify, checksum validation
- **RDF Ontology**: Persist receipts, work orders, Andon events, root cause analyses
- **SPARQL Queries**: Query by SKU, bucket, status, custom queries
- **Backup/Restore**: Create backups, restore, integrity verification
- **Validation**: SHACL validation, automatic repair
- **Change Log**: Log changes, get history
- **Export**: Turtle, N-Triples, JSON-LD, JSON, CSV, PDF
- **Round-Trip**: JSON → RDF → JSON conversion
- **Concurrent Access**: Parallel receipt storage, ontology persistence

## Architecture

### Dual Storage Design

```
┌─────────────────────────────────────────────────────────┐
│                   TCPS Persistence                      │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  ┌──────────────────┐        ┌──────────────────┐      │
│  │  JSON Storage    │        │  RDF Ontology    │      │
│  │  (Receipts)      │        │  (Semantic Data) │      │
│  ├──────────────────┤        ├──────────────────┤      │
│  │ - Fast lookup    │        │ - SPARQL queries │      │
│  │ - SHA-256 verify │        │ - SHACL validate │      │
│  │ - Per-SKU files  │        │ - Relationships  │      │
│  └──────────────────┘        └──────────────────┘      │
│         │                           │                   │
│         └───────────┬───────────────┘                   │
│                     │                                   │
│         ┌───────────▼───────────┐                       │
│         │  Backup/Restore       │                       │
│         │  (Tarball + Manifest) │                       │
│         └───────────────────────┘                       │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### Python Integration (Port Communication)

```
┌──────────────┐                    ┌──────────────┐
│   Erlang     │  JSON over port    │   Python     │
│   Process    │ ◄─────────────────►│   rdflib     │
├──────────────┤                    ├──────────────┤
│ - SPARQL     │                    │ - Parse TTL  │
│ - Convert    │                    │ - Execute    │
│ - Validate   │                    │ - Convert    │
└──────────────┘                    └──────────────┘
```

## Performance Considerations

1. **Receipt Storage**: O(1) write, O(n) read per SKU
2. **SPARQL Queries**: Indexed via rdflib, performance depends on query complexity
3. **Concurrent Access**: File-level locking, safe for parallel writes
4. **Backup**: Compressed tarballs, incremental backups recommended
5. **Validation**: SHACL validation is compute-intensive, run periodically

## Future Enhancements

1. **Triple Store**: Migrate to persistent triple store (Apache Jena, Virtuoso)
2. **Incremental Backup**: Track changes since last backup
3. **Distributed Storage**: Support for distributed RDF storage
4. **GraphQL Interface**: Query ontology via GraphQL
5. **Visualization**: Generate knowledge graph visualizations
6. **Provenance**: PROV-O for full data lineage tracking

## References

- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [SHACL](https://www.w3.org/TR/shacl/)
- [rdflib Documentation](https://rdflib.readthedocs.io/)
- [pyshacl Documentation](https://github.com/RDFLib/pySHACL)
