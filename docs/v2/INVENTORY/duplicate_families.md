# Duplicate Families Analysis - erlmcp v2

**Purpose:** Identify all module families with multiple versions (active, broken, backup)
**Source:** inventory.json + src/ scan
**Date:** 2026-01-27

## Overview

This document identifies all module families with duplicates, broken versions, or backups. For v2 cleanup, a canonical version should be selected and others deleted.

## Duplicate Family Groups


### erlmcp_alert_manager

**Total variants:** 1 files


#### Broken (DELETE)

- `src/erlmcp_alert_manager.erl.broken` (20,069 bytes) ✗ COMPILATION ERROR

**Recommendation:**
- **No active version found!** Choose one of the variants and activate it, delete the rest.

---

### erlmcp_marketplace

**Total variants:** 1 files


#### Backups/Variants (DELETE)

- `src/erlmcp_marketplace_copy.erl` (27,514 bytes, marker: `.erl`)

**Recommendation:**
- **No active version found!** Choose one of the variants and activate it, delete the rest.

---

### erlmcp_metrics_aggregator

**Total variants:** 1 files


#### Broken (DELETE)

- `src/erlmcp_metrics_aggregator.erl.broken` (11,410 bytes) ✗ COMPILATION ERROR

**Recommendation:**
- **No active version found!** Choose one of the variants and activate it, delete the rest.

---

### erlmcp_metrics_collector

**Total variants:** 1 files


#### Broken (DELETE)

- `src/erlmcp_metrics_collector.erl.broken` (21,660 bytes) ✗ COMPILATION ERROR

**Recommendation:**
- **No active version found!** Choose one of the variants and activate it, delete the rest.

---

### erlmcp_monitor

**Total variants:** 1 files


#### Broken (DELETE)

- `src/erlmcp_monitor.erl.broken` (21,921 bytes) ✗ COMPILATION ERROR

**Recommendation:**
- **No active version found!** Choose one of the variants and activate it, delete the rest.

---

### erlmcp_server

**Total variants:** 3 files

#### Active (CANONICAL)

- `src/erlmcp_server.erl` (67,387 bytes) ← **KEEP THIS**

#### Backups/Variants (DELETE)

- `src/erlmcp_server.erl.backup` (47,538 bytes, marker: `.backup`)
- `src/erlmcp_server_refactored.erl` (34,723 bytes, marker: `.erl`)

**Recommendation:**
- Keep `src/erlmcp_server.erl` (active version)
- Delete 2 file(s): erlmcp_server.erl.backup, erlmcp_server_refactored.erl

---

### erlmcp_trace_analyzer

**Total variants:** 1 files


#### Broken (DELETE)

- `src/erlmcp_trace_analyzer.erl.broken` (31,005 bytes) ✗ COMPILATION ERROR

**Recommendation:**
- **No active version found!** Choose one of the variants and activate it, delete the rest.

---

### erlmcp_transport_http

**Total variants:** 3 files

#### Active (CANONICAL)

- `src/erlmcp_transport_http.erl` (1,609 bytes) ← **KEEP THIS**

#### Backups/Variants (DELETE)

- `src/erlmcp_transport_http.erl.backup` (18,518 bytes, marker: `.backup`)
- `src/erlmcp_transport_http_new.erl` (17,136 bytes, marker: `.erl`)

**Recommendation:**
- Keep `src/erlmcp_transport_http.erl` (active version)
- Delete 2 file(s): erlmcp_transport_http.erl.backup, erlmcp_transport_http_new.erl

---

### erlmcp_transport_tcp

**Total variants:** 2 files

#### Active (CANONICAL)

- `src/erlmcp_transport_tcp.erl` (20,022 bytes) ← **KEEP THIS**

#### Broken (DELETE)

- `src/erlmcp_transport_tcp.erl.broken` (29,999 bytes) ✗ COMPILATION ERROR

**Recommendation:**
- Keep `src/erlmcp_transport_tcp.erl` (active version)
- Delete 1 file(s): erlmcp_transport_tcp.erl.broken

---

### tcps_rebar3_quality

**Total variants:** 2 files

#### Active (CANONICAL)

- `src/tcps/tcps_rebar3_quality.erl` (8,384 bytes) ← **KEEP THIS**

#### Backups/Variants (DELETE)

- `src/tcps/tcps_rebar3_quality.erl.bak` (6,135 bytes, marker: `.bak`)

**Recommendation:**
- Keep `src/tcps/tcps_rebar3_quality.erl` (active version)
- Delete 1 file(s): tcps_rebar3_quality.erl.bak

---

## Summary

| Category | Count |
|----------|-------|
| **Module families with duplicates** | 10 |
| **Broken files to delete** | 6 |
| **Backup/variant files to delete** | 6 |
| **Total cleanup candidates** | 12 |

## Automated Cleanup Script

```bash
#!/bin/bash
# Delete all broken and backup files

rm 'src/erlmcp_alert_manager.erl.broken'
rm 'src/erlmcp_marketplace_copy.erl'
rm 'src/erlmcp_metrics_aggregator.erl.broken'
rm 'src/erlmcp_metrics_collector.erl.broken'
rm 'src/erlmcp_monitor.erl.broken'
rm 'src/erlmcp_server.erl.backup'
rm 'src/erlmcp_server_refactored.erl'
rm 'src/erlmcp_trace_analyzer.erl.broken'
rm 'src/erlmcp_transport_http.erl.backup'
rm 'src/erlmcp_transport_http_new.erl'
rm 'src/erlmcp_transport_tcp.erl.broken'
rm 'src/tcps/tcps_rebar3_quality.erl.bak'

# Verify no .broken/.backup/.bak files remain
find src -name '*.broken' -o -name '*.backup' -o -name '*.bak'
```
