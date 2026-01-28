# Evidence Index Contract (index.json)

## Overview

The Evidence Index (`index.json`) is the authoritative metadata contract for each benchmark run. It captures execution provenance, environment, and result location in canonical form, enabling reproducibility, regression tracking, and CI/CD integration.

**Location:** `bench/results/{TIMESTAMP}/index.json`

**Rule:** ONE index.json per run. No summary narratives. Machine-parseable only.

## Schema

### Root Object

```json
{
  "run_id": "run_20260127_143022",
  "git_sha": "abc1234567890def",
  "otp_version": "25.3.2.8",
  "os": "Darwin",
  "hostname": "node-42.bench",
  "command": "./scripts/bench/run_all_benchmarks.sh quick",
  "start_timestamp": "2026-01-27T14:30:22Z",
  "end_timestamp": "2026-01-27T14:35:45Z",
  "duration_s": 323,
  "workload_ids": [
    "core/registry_contention_10k",
    "core/registry_contention_100k",
    "integration/tool_sequence",
    "network/tcp_burst_100",
    "stress/sustained_30s",
    "chaos/memory_exhaustion"
  ],
  "result_files": [
    "bench/results/20260127_143022/core_registry_contention_10k.json",
    "bench/results/20260127_143022/core_registry_contention_100k.json",
    "bench/results/20260127_143022/integration_tool_sequence.json",
    "bench/results/20260127_143022/network_tcp_burst_100.json",
    "bench/results/20260127_143022/stress_sustained_30s.json",
    "bench/results/20260127_143022/chaos_memory_exhaustion.json"
  ]
}
```

## Field Definitions

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| `run_id` | string | Unique run identifier (timestamp-based). Format: `run_YYYYMMDD_HHMMSS` | `run_20260127_143022` |
| `git_sha` | string | Git commit SHA of current HEAD. Used for reproducibility and code traceability. Value: `unknown` if not in git repo | `abc1234567890def` or `unknown` |
| `otp_version` | string | Erlang/OTP version (from `erlang:system_info(otp_release)`) | `25.3.2.8` |
| `os` | string | Operating system (from `uname -s`) | `Darwin`, `Linux`, etc. |
| `hostname` | string | Machine hostname | `node-42.bench` |
| `command` | string | Full invocation command including arguments | `./scripts/bench/run_all_benchmarks.sh quick` |
| `start_timestamp` | string | ISO 8601 UTC timestamp when run started | `2026-01-27T14:30:22Z` |
| `end_timestamp` | string | ISO 8601 UTC timestamp when run ended | `2026-01-27T14:35:45Z` |
| `duration_s` | number | Total execution time in seconds (integer) | `323` |
| `workload_ids` | array | Array of executed workload identifiers in format `category/workload_name` | `["core/registry_contention_10k", ...]` |
| `result_files` | array | Paths to result JSON files (excluding index.json, including summary.json) | `["bench/results/.../core_registry_contention_10k.json", ...]` |

## Parsing with jq

### Extract run metadata

```bash
jq '.run_id, .git_sha, .otp_version' bench/results/20260127_143022/index.json
```

Output:
```
"run_20260127_143022"
"abc1234567890def"
"25.3.2.8"
```

### List all executed workloads

```bash
jq -r '.workload_ids[]' bench/results/20260127_143022/index.json
```

Output:
```
core/registry_contention_10k
core/registry_contention_100k
integration/tool_sequence
network/tcp_burst_100
stress/sustained_30s
chaos/memory_exhaustion
```

### Count workloads by category

```bash
jq -r '.workload_ids[] | split("/")[0]' bench/results/20260127_143022/index.json | sort | uniq -c
```

Output:
```
  2 core
  1 integration
  1 network
  1 stress
  1 chaos
```

### Extract result file paths

```bash
jq -r '.result_files[]' bench/results/20260127_143022/index.json
```

Output:
```
bench/results/20260127_143022/core_registry_contention_10k.json
bench/results/20260127_143022/core_registry_contention_100k.json
...
```

### Build regression analysis command

```bash
# Show duration and workload count from index
jq '{run_id, duration_s, workload_count: (.workload_ids | length)}' bench/results/20260127_143022/index.json
```

Output:
```json
{
  "run_id": "run_20260127_143022",
  "duration_s": 323,
  "workload_count": 6
}
```

### Verify reproducibility

```bash
# Compare git SHA and OTP version across runs
jq '.git_sha, .otp_version' bench/results/*/index.json | sort | uniq -c
```

### Generate audit trail (timestamp + SHA)

```bash
jq '{timestamp: .start_timestamp, git_sha, duration_s, workload_count: (.workload_ids | length)}' bench/results/20260127_143022/index.json
```

## CI/CD Integration

### Verify index.json exists and is valid

```bash
# Check file exists
[ -f bench/results/20260127_143022/index.json ] || exit 1

# Validate JSON syntax
jq empty bench/results/20260127_143022/index.json || exit 1

# Verify required fields
jq -e '.run_id and .git_sha and .otp_version and .start_timestamp and .duration_s' \
  bench/results/20260127_143022/index.json > /dev/null || exit 1

echo "✓ Evidence index valid"
```

### Extract metadata for artifact tagging

```bash
INDEX_FILE="bench/results/20260127_143022/index.json"
RUN_ID=$(jq -r '.run_id' "$INDEX_FILE")
GIT_SHA=$(jq -r '.git_sha' "$INDEX_FILE")
DURATION=$(jq -r '.duration_s' "$INDEX_FILE")

echo "Benchmark: $RUN_ID (commit $GIT_SHA, duration ${DURATION}s)"
```

### Collect all result files from index

```bash
INDEX_FILE="bench/results/20260127_143022/index.json"

# Copy all results referenced in index to artifact storage
jq -r '.result_files[]' "$INDEX_FILE" | while read result_file; do
  cp "$result_file" /artifacts/
done
```

## Design Principles

1. **Single Source of Truth:** Index.json is THE authoritative list of what was executed and where results are stored.
2. **Machine-Readable Only:** No human narratives, no summaries. Pure metadata.
3. **Immutable:** Once written, never modified. Each run has its own index.
4. **Reproducible:** Includes git SHA, OTP version, timestamp, and command. Enables exact reproduction.
5. **Provenance:** Every file location and workload ID traced back to exact run via run_id and index.
6. **No Narrative Spam:** Excludes summary.txt, execution.log, and other prose. Only canonical JSON metadata.

## Validation

When index.json is written:

1. All referenced `result_files` MUST exist on disk
2. All `workload_ids` MUST be in format `category/workload_name`
3. `start_timestamp` and `end_timestamp` MUST be ISO 8601 UTC
4. `duration_s` MUST match calculated difference
5. `git_sha` MUST be 40-char hex or `unknown`

Example validation script:

```bash
#!/bin/bash
INDEX_FILE="$1"

# Validate JSON syntax
jq empty "$INDEX_FILE" || { echo "Invalid JSON"; exit 1; }

# Validate required fields
jq -e '.run_id and .git_sha and .otp_version and .start_timestamp and .end_timestamp and .duration_s' \
  "$INDEX_FILE" > /dev/null || { echo "Missing required fields"; exit 1; }

# Validate result files exist
jq -r '.result_files[]' "$INDEX_FILE" | while read file; do
  [ -f "$file" ] || { echo "Missing result file: $file"; exit 1; }
done

# Validate workload_ids format
jq -r '.workload_ids[]' "$INDEX_FILE" | grep -E '^[a-z_]+/[a-z_0-9]+$' > /dev/null || \
  { echo "Invalid workload_id format"; exit 1; }

echo "✓ Index validation passed"
```

## Examples

### Example 1: Quick mode run

```bash
$ ./scripts/bench/run_all_benchmarks.sh quick
[... execution output ...]
Evidence Index:    bench/results/20260127_143022/index.json

$ cat bench/results/20260127_143022/index.json
{
  "run_id": "run_20260127_143022",
  "git_sha": "2aa5a2cf1234567890abc",
  "otp_version": "25.3.2.8",
  "os": "Darwin",
  "hostname": "macbook.local",
  "command": "./scripts/bench/run_all_benchmarks.sh quick",
  "start_timestamp": "2026-01-27T14:30:22Z",
  "end_timestamp": "2026-01-27T14:35:45Z",
  "duration_s": 323,
  "workload_ids": [
    "core/registry_contention_10k",
    "integration/tool_sequence",
    "network/tcp_burst_100",
    "stress/sustained_30s",
    "chaos/memory_exhaustion"
  ],
  "result_files": [
    "bench/results/20260127_143022/core_registry_contention_10k.json",
    "bench/results/20260127_143022/integration_tool_sequence.json",
    "bench/results/20260127_143022/network_tcp_burst_100.json",
    "bench/results/20260127_143022/stress_sustained_30s.json",
    "bench/results/20260127_143022/chaos_memory_exhaustion.json",
    "bench/results/20260127_143022/summary.json"
  ]
}
```

### Example 2: Extract performance metrics per workload

```bash
# For each result file referenced in index, extract throughput
INDEX="bench/results/20260127_143022/index.json"

jq -r '.result_files[]' "$INDEX" | while read result; do
  WORKLOAD=$(basename "$result" .json)
  THROUGHPUT=$(jq '.metrics.throughput_msg_per_s // "N/A"' "$result")
  echo "$WORKLOAD: $THROUGHPUT msg/s"
done
```

Output:
```
core_registry_contention_10k: 553000 msg/s
core_registry_contention_100k: 487000 msg/s
integration_tool_sequence: 12500 msg/s
network_tcp_burst_100: 8700 msg/s
stress_sustained_30s: 372000 msg/s
chaos_memory_exhaustion: 15600 msg/s
```

## Relationship to Other Files

- **summary.json** - Derived summary of all results (includes narrative analysis, regressions)
- **summary.txt** - Human-readable prose summary
- **execution.log** - Verbose execution transcript
- **{category}_{workload}.json** - Individual benchmark result (metrology-validated)

**Index.json is THE index for the entire run.** Other files are auxiliary.
