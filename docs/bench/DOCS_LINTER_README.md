# Documentation Linter for erlmcp

The documentation linter (`tools/docs_lint.sh`) is a CI/CD tool that enforces documentation quality standards, preventing legacy references, ambiguous terms, and invalid workload IDs from being committed.

## Purpose

Maintain documentation consistency and accuracy by:

1. **Detecting legacy file references** - Prevents references to deprecated benchmark files
2. **Enforcing metric terminology standards** - Ensures unambiguous performance measurements
3. **Validating workload ID references** - Confirms all referenced workloads exist
4. **Flagging hardcoded numbers** - Alerts to unsourced performance claims

## Running the Linter

### Local Development

```bash
# Run all checks
./tools/docs_lint.sh

# Run with verbose output
./tools/docs_lint.sh --verbose

# Run specific checks
./tools/docs_lint.sh --check-legacy      # Only legacy file references
./tools/docs_lint.sh --check-terms       # Only ambiguous terms
./tools/docs_lint.sh --check-workloads   # Only workload IDs
./tools/docs_lint.sh --check-hardcoded   # Only hardcoded numbers
```

### CI/CD Pipeline

The linter runs automatically on every PR as part of the `docs-lint` job in `.github/workflows/ci.yml`:

```yaml
docs-lint:
  name: Documentation Linter
  runs-on: ubuntu-22.04
  steps:
    - uses: actions/checkout@v4
    - name: Run Documentation Linter
      run: |
        chmod +x tools/docs_lint.sh
        ./tools/docs_lint.sh
```

If the linter fails in a PR, a comment is automatically posted with remediation instructions.

## Check Details

### Check 1: Legacy File References

Detects references to deprecated benchmark files that have been consolidated or removed.

**Forbidden files**:
- `benchmark_100k` - Consolidated into `erlmcp_bench_core_ops.erl`
- `benchmark_100k_registry` - Consolidated into core_ops
- `benchmark_100k_SUITE` - Consolidated into core_ops
- `throughput_SUITE` - Consolidated into `erlmcp_bench_network_real.erl`
- `latency_SUITE` - Consolidated into network_real
- `transport_real/` - Directory structure consolidated
- `bench_stdio` - Consolidated into network transport benchmarks
- `bench_tcp` - Consolidated into network transport benchmarks
- `bench_http` - Consolidated into network transport benchmarks

**Fix**: Replace with references to consolidated benchmark modules:
- `erlmcp_bench_core_ops` - Core operations (registry, queue, pool, session)
- `erlmcp_bench_network_real` - Transport benchmarks (TCP, HTTP, stdio)
- `erlmcp_bench_stress` - Sustained load testing
- `erlmcp_bench_chaos` - Failure injection scenarios
- `erlmcp_bench_integration` - End-to-end MCP workflows

**Example violation**:
```markdown
See `bench/throughput_SUITE.erl` for performance testing.
```

**Correct version**:
```markdown
See `bench/erlmcp_bench_network_real.erl` for transport performance testing.
```

### Check 2: Ambiguous Metric Terms

Ensures metric terms are specific and unambiguous.

**Term: "req/s"** (ambiguous)
- ❌ "System handles 450 req/s"
- ✅ "System handles 450 jsonrpc_req_per_s"
- ✅ "System handles 450 http_req_per_s"

**Term: "connections"** (ambiguous)
- ❌ "Tested at 1K connections"
- ✅ "Tested at 1K sockets_open"
- ✅ "Tested at 1K tcp_connections"
- ✅ "Tested at 1K concurrent_connections"

Acceptable context for "connections":
- Scaled parameter descriptions: "1000 connections configured"
- Comparison statements: "at X connections" (numeric context required)
- API parameters: `max_connections` (config key)

### Check 3: Workload ID Validation

Validates that all referenced workload IDs are defined in benchmark modules.

**Valid workload IDs**:

**Core Operations**:
- `core_ops_1k`, `core_ops_10k`, `core_ops_100k`, `core_ops_1m`

**Network (TCP)**:
- `network_tcp_100_100k`, `network_tcp_500_100k`, `network_tcp_1k_100k`, `network_tcp_5k_100k`, `network_tcp_10k_100k`

**Network (HTTP)**:
- `network_http_100_5k`, `network_http_500_5k`, `network_http_1k_5k`, `network_http_5k_5k`

**Stress**:
- `stress_30s_100k_ops`, `stress_5min_100k_ops`, `stress_24h_100k_ops`

**Chaos**:
- `chaos_memory_exhaustion`, `chaos_process_crash`, `chaos_network_failure`, etc. (11 total)

**Integration**:
- `integration_basic_initialize`, `integration_tool_sequence`, `integration_prompts_workflow`, `integration_resources_workflow`, `integration_complete_flow`

See [docs/bench/WORKLOADS.md](./WORKLOADS.md) for complete reference.

### Check 4: Hardcoded Performance Numbers

Warns about hardcoded performance metrics without source attribution.

**Example violations**:
```markdown
The system achieves 2.69M operations per second.
```

Better approach:
```markdown
The system achieves [2.69M operations per second](../bench/BENCH_INDEX.md#core-operations-benchmarks) under `core_ops_100k` workload.
```

## Exit Codes

- `0` - All checks pass
- `1` - One or more violations found

## Fixing Violations

### Legacy File References

Find and replace references:

```bash
# Find all occurrences
grep -r "throughput_SUITE\|latency_SUITE\|benchmark_100k" docs/

# Replace with consolidated modules
sed -i 's/throughput_SUITE/erlmcp_bench_network_real/g' docs/*.md
sed -i 's/latency_SUITE/erlmcp_bench_network_real/g' docs/*.md
sed -i 's/benchmark_100k/erlmcp_bench_core_ops/g' docs/*.md
```

### Ambiguous Terms

Use specific metric names from the [Metrology Glossary](metrology/METRICS_GLOSSARY.md):

```bash
# Replace "req/s" with qualified term
sed -i 's/ req\/s/ jsonrpc_req_per_s/g' docs/*.md

# Replace ambiguous "connections" with specific term
sed -i 's/\b([0-9]+K?)\s+connections\b/\1 tcp_connections/g' docs/*.md
```

### Invalid Workload IDs

1. Verify workload exists in benchmark module
2. Check `docs/bench/WORKLOADS.md` for valid IDs
3. Update reference or add workload to benchmark module
4. Run linter again to validate

## CI Integration

### GitHub Actions

The linter is integrated into the main CI pipeline:

```yaml
# .github/workflows/ci.yml
docs-lint:
  name: Documentation Linter
  runs-on: ubuntu-22.04
  steps:
    - uses: actions/checkout@v4
    - name: Run Documentation Linter
      run: |
        chmod +x tools/docs_lint.sh
        ./tools/docs_lint.sh
    - name: Comment on PR with lint results
      if: failure() && github.event_name == 'pull_request'
      uses: actions/github-script@v7
      with:
        script: |
          github.rest.issues.createComment({
            issue_number: context.issue.number,
            owner: context.repo.owner,
            repo: context.repo.repo,
            body: '❌ Documentation linting failed. See logs above for details.'
          })
```

### Failing a PR

If the linter detects violations:

1. The `docs-lint` job fails
2. PR status shows "Checks failed"
3. GitHub comments with remediation instructions
4. Developer must fix and re-push
5. Linter runs again automatically

## Customization

### Adding New Legacy Files

Edit `tools/docs_lint.sh`:

```bash
declare -a LEGACY_FILES=(
    "benchmark_100k"
    "my_deprecated_file"  # Add here
)
```

### Adding New Workload IDs

1. Define workload in benchmark module (`bench/erlmcp_bench_*.erl`)
2. Document in `docs/bench/WORKLOADS.md`
3. Update linter:

```bash
declare -a VALID_WORKLOADS=(
    "core_ops_1k"
    "my_new_workload"  # Add here
)
```

### Adding New Ambiguous Terms

Edit `tools/docs_lint.sh`:

```bash
declare -a AMBIGUOUS_TERMS=(
    "req/s"
    "my_ambiguous_term"  # Add here
)
```

And update the check logic to detect it.

## Troubleshooting

### "Found legacy reference to X"

The documentation references a deprecated benchmark file. Replace with the consolidated module:

- `benchmark_100k*` → `erlmcp_bench_core_ops`
- `throughput_SUITE` → `erlmcp_bench_network_real`
- `latency_SUITE` → `erlmcp_bench_network_real`
- `transport_real/` → `erlmcp_bench_network_real`

### "Invalid workload_id reference: X"

The workload ID is not defined in any benchmark module. Either:

1. Use a valid workload ID from `docs/bench/WORKLOADS.md`
2. Add the workload to the appropriate benchmark module
3. Update `VALID_WORKLOADS` in `tools/docs_lint.sh`

### "Found ambiguous 'connections' without proper context"

Replace bare "connections" with specific terms:

- `sockets_open` - Number of open TCP/HTTP sockets
- `tcp_connections` - TCP connection count
- `http_connections` - HTTP connection count
- `concurrent_connections` - Currently active connections
- Or use with numeric context: "at 1K connections" (number required)

### "Found hardcoded performance number"

Performance numbers should reference their source:

```markdown
❌ The system achieves 2.69M ops/sec.

✅ The system achieves [2.69M ops/sec](./BENCH_INDEX.md) under the `core_ops_100k` workload.
```

## References

- [Benchmark Index](./BENCH_INDEX.md) - All benchmarks and workloads
- [Workloads Reference](./WORKLOADS.md) - Valid workload IDs
- [Metrology Glossary](./metrology/METRICS_GLOSSARY.md) - Standard metric terms
- [CI/CD Configuration](.github/workflows/ci.yml) - Linter integration
