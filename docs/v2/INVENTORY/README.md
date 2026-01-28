# Module Inventory System - erlmcp v2

This directory contains the complete module inventory system for erlmcp, generated from source code analysis.

## Files in This Directory

### 1. `module_inventory.md`
**Purpose:** Complete catalog of all active modules organized by family
**Generated from:** `inventory.json` + source code scan
**Contents:**
- Executive summary (244 active modules across 29 families)
- Detailed listing of each module family
- Exports for each module
- File sizes and statistics
- Identification of broken files
- Identification of duplicate files

**Use case:** Understanding the complete module structure, finding related modules, planning refactoring

### 2. `duplicate_families.md`
**Purpose:** Analysis of all module families with duplicates, broken versions, or backups
**Generated from:** `inventory.json` + source code analysis
**Contents:**
- Detailed review of 10 module families with duplicates
- Identification of active (canonical) vs broken vs backup versions
- Clear recommendations for each family (which to keep, which to delete)
- Automated cleanup script
- Summary statistics

**Use case:** Planning v2 cleanup, removing technical debt, deciding canonical versions

### 3. `inventory.json` (root level)
**Location:** `/Users/sac/erlmcp/inventory.json`
**Purpose:** Machine-readable module inventory database
**Format:** JSON with complete metadata for all modules
**Contents:**
```json
{
  "timestamp": <unix_time>,
  "src_directory": "src",
  "summary": {
    "total_modules": 244,
    "num_families": 29,
    "num_broken": 6,
    "num_duplicates": 6
  },
  "families": {
    "family_name": [
      {
        "file": "src/module.erl",
        "module": "module_name",
        "family": "family_name",
        "exports": ["func/1", "func/2", ...],
        "num_exports": N,
        "size_bytes": N
      },
      ...
    ],
    ...
  },
  "broken": [...],
  "duplicates": [...],
  "all_modules": [...]
}
```

## Module Families (29 Total)

| Family | Count | Purpose |
|--------|-------|---------|
| **utilities** | 28 | General utilities, app management, audio, etc. |
| **tcps** | 42 | Toyota Code Production System (TCPS) implementation |
| **transport** | 10 | Transport layer abstraction (stdio, tcp, http, websocket, sse) |
| **cli** | 5 | CLI command implementations |
| **server** | 3 | Core server implementations |
| **client** | 1 | MCP client implementation |
| **json_rpc** | 1 | JSON-RPC 2.0 protocol encoding/decoding |
| **configuration** | 7 | Config loading, validation, profiles, schemas |
| **metrics** | 6 | Metrics collection and reporting |
| **monitoring** | 4 | Health monitoring, dashboards, SLA tracking |
| **http** | 7 | HTTP-specific headers, security, auth, middleware |
| **logging** | 1 | Structured logging and log level management |
| **tracing** | 2 | Distributed tracing and trace propagation |
| **reporting** | 3 | Report generation and visualization |
| **planning** | 6 | Plan/envelope/tier management |
| **supervision** | 5 | Supervisor and process supervision |
| **resilience** | 5 | Circuit breaker, backpressure, graceful degradation |
| **health** | 2 | Health checks and monitoring |
| **pooling** | 3 | Connection pools and buffer pools |
| **memory** | 4 | Memory accounting and profiling |
| **profiling** | 3 | Performance profiling (CPU, memory, latency) |
| **benchmarking** | 1 | Benchmark planning and validation |
| **queuing** | 4 | Queue implementations (bounded, optimized) |
| **receipts** | 3 | Receipt chain and verification |
| **registry** | 3 | Message registry and routing |
| **sessions** | 3 | Session management and failover |
| **routing** | 2 | Message routing and complex routing |
| **pricing** | 5 | Pricing plans, tiers, and SLA monitoring |

## Statistics

### Size Analysis
- **Total codebase:** ~4.3 MB of Erlang source
- **Average module size:** ~17.6 KB
- **Largest family:** tcps (42 modules)
- **Smallest family:** benchmarking, client, json_rpc, logging (1 module each)

### Cleanup Needed
- **Broken files:** 6 (need to be deleted)
  - `erlmcp_alert_manager.erl.broken`
  - `erlmcp_metrics_aggregator.erl.broken`
  - `erlmcp_metrics_collector.erl.broken`
  - `erlmcp_monitor.erl.broken`
  - `erlmcp_trace_analyzer.erl.broken`
  - `erlmcp_transport_tcp.erl.broken`

- **Duplicate/backup files:** 6 (need to be deleted)
  - `erlmcp_marketplace_copy.erl`
  - `erlmcp_server.erl.backup`
  - `erlmcp_server_refactored.erl`
  - `erlmcp_transport_http.erl.backup`
  - `erlmcp_transport_http_new.erl`
  - `tcps/tcps_rebar3_quality.erl.bak`

- **Families with duplicates:** 10

## Regenerating the Inventory

To update the inventory after module changes:

```bash
# Run the Python extractor
python3 tools/v2_arch/extract_module_inventory.py

# Or run the Erlang escript (older, less reliable JSON output)
escript tools/v2_arch/extract_module_inventory.escript

# Then regenerate markdown docs
python3 docs/v2/INVENTORY/regenerate_docs.py  # (to be created)
```

## Key Insights for v2 Architecture

1. **Large number of utilities:** 28 modules in "utilities" family should be categorized more precisely
2. **TCPS dominance:** 42 modules dedicated to Toyota Code Production System
3. **Transport abstraction:** 10 transport modules provide good abstraction
4. **Configuration complexity:** 7 configuration modules suggest this may need simplification
5. **Monitoring breadth:** 13 modules across monitoring/health/metrics suggest comprehensive observability
6. **Duplicate burden:** 12 files (6 broken + 6 backups) should be cleaned up immediately

## Recommended v2 Actions

1. **Delete broken files** - 6 files with compilation errors
2. **Delete backups** - 6 backup/variant files (keep canonical versions)
3. **Rationalize utilities** - 28 utility modules should be grouped into 5-7 families
4. **Consider TCPS split** - 42 TCPS modules might benefit from sub-family organization
5. **Simplify configuration** - 7 configuration modules could potentially be consolidated to 3-4

## Architecture Decisions Made

- **Family classification by naming pattern:** Modules are grouped into families based on their prefix (e.g., `erlmcp_http_*` → http family)
- **No backward compatibility constraints:** Broken/backup files are clearly marked for deletion
- **Canonical version selection:** For families with duplicates, the version without suffix/extension is canonical
- **Complete export tracking:** All function exports are extracted and listed

## Related Documentation

- **Architecture:** See `/docs/v2/architecture.md` (when created)
- **OTP patterns:** See `/docs/otp-patterns.md`
- **Transport abstraction:** See `/docs/protocol.md`
- **Planning system:** See `/docs/planning.md`

---

**Generated:** 2026-01-27
**Source tool:** `/tools/v2_arch/extract_module_inventory.py`
**Next steps:** Review duplicate_families.md, execute cleanup script, plan v2 consolidation
