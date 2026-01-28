## Pre-Completion Verification (MANDATORY)

Before reporting task completion, this agent MUST:

### 1. Run Tests
- **EUnit tests**: `rebar3 eunit --module=<relevant_module>`
- **Common Test**: `rebar3 ct --suite=<relevant_suite>`
- **Property tests**: `rebar3 proper -c --module=<relevant_module>` (if applicable)
- **All tests must pass** - 0 failures, 0 errors

### 2. Run Quality Checks
- **Compilation**: `rebar3 compile` - must succeed with 0 warnings
- **Dialyzer**: `rebar3 dialyzer` - must pass with 0 type errors
- **Xref**: `rebar3 xref` - must pass with 0 undefined functions
- **Format check**: `rebar3 format --verify` - code must be properly formatted

### 3. Run Benchmarks (if applicable)
- **Performance regression check**: Verify no significant performance degradation
- **For transport agents**: Benchmark throughput and latency
- **For server/client agents**: Benchmark request-response times
- **For library integrators**: Benchmark memory usage and overhead

### 4. Verification Report Format
Agent must output to STDOUT:
```
✅ Tests: X/X passed (EUnit: Y, CT: Z, Proper: W)
✅ Quality: Compile clean, Dialyzer clean, Xref clean, Format verified
✅ Benchmarks: [metrics if applicable, or "N/A"]
✅ Coverage: X% (minimum 80% required)

Ready for review: [brief summary of what was implemented]
```

### 5. Post-Task Hook Command
```bash
#!/bin/bash
# Runs automatically via settings.json post-task hook
make check  # Runs: compile, xref, dialyzer, all tests
```
