# Auto-Fix System Quick Reference

## Quick Start

```bash
# Run full auto-fix orchestration
./tools/auto-fix/orchestrator.sh orchestrate

# Interactive mode
./tools/auto-fix/orchestrator.sh interactive

# Run all quality gates with auto-fix
./tools/auto-fix/gate-failure-dispatcher.sh run-all

# Check current status
./tools/auto-fix/gate-failure-dispatcher.sh status

# Reset state
./tools/auto-fix/gate-failure-dispatcher.sh reset
```

## Components

### 1. Orchestrator
**Main entry point** - Coordinates all fix agents through multiple iterations.

```bash
./tools/auto-fix/orchestrator.sh orchestrate [iterations]
./tools/auto-fix/orchestrator.sh validate
./tools/auto-fix/orchestrator.sh interactive
```

### 2. Gate Failure Dispatcher
**Quality gate monitor** - Detects failures and dispatches appropriate fix agents.

```bash
./tools/auto-fix/gate-failure-dispatcher.sh run-all
./tools/auto-fix/gate-failure-dispatcher.sh monitor <gate> <command>
./tools/auto-fix/gate-failure-dispatcher.sh status
./tools/auto-fix/gate-failure-dispatcher.sh reset
```

### 3. Fix Agents

#### Syntax Fix Agent
Fixes compilation errors (unused variables, missing exports, typos).

```bash
./tools/auto-fix/syntax-fix-agent.sh <error_file> [attempt]
```

#### Type Fix Agent
Fixes Dialyzer warnings (missing specs, type mismatches).

```bash
./tools/auto-fix/type-fix-agent.sh <error_file> [attempt]
```

#### Test Fix Agent
Fixes test failures (assertions, setup/teardown, mocks).

```bash
./tools/auto-fix/test-fix-agent.sh <error_file> [attempt]
```

#### Coverage Agent
Generates test templates for uncovered code.

```bash
./tools/auto-fix/test-coverage-agent.sh <error_file> [attempt]
```

#### Performance Agent
Analyzes performance regressions and suggests optimizations.

```bash
./tools/auto-fix/performance-agent.sh <error_file> [attempt]
```

#### XRef Agent
Fixes cross-reference issues (unused exports, undefined functions).

```bash
./tools/auto-fix/xref-fix-agent.sh <error_file> [attempt]
```

## Quality Gates (Order of Execution)

1. **Compilation** - `TERM=dumb rebar3 compile`
2. **Dialyzer** - `rebar3 dialyzer`
3. **XRef** - `rebar3 xref`
4. **Tests** - `rebar3 eunit`
5. **Coverage** - `rebar3 cover --verbose`

## Workflow Examples

### Development Workflow

```bash
# Make changes
vim src/my_module.erl

# Run auto-fix
./tools/auto-fix/orchestrator.sh orchestrate

# If successful, continue
# If escalated, review logs/auto-fix/escalation-*.txt
```

### CI/CD Integration

```bash
#!/bin/bash
# .github/workflows/ci.yml step

./tools/auto-fix/orchestrator.sh orchestrate 3 || {
    echo "Quality gates failed"
    cat logs/auto-fix/escalation-*.txt
    exit 1
}
```

### Pre-Commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

./tools/auto-fix/gate-failure-dispatcher.sh run-all || {
    echo "Auto-fix failed. Fix manually or use: git commit --no-verify"
    exit 1
}
```

## Configuration

### Max Attempts per Gate
Edit `tools/auto-fix/gate-failure-dispatcher.sh`:
```bash
MAX_FIX_ATTEMPTS=3  # Change to desired value
```

### Max Iterations
Edit `tools/auto-fix/orchestrator.sh`:
```bash
MAX_ITERATIONS=5  # Change to desired value
```

### Log Directory
Default: `logs/auto-fix/`

Change in each script:
```bash
LOG_DIR="logs/auto-fix"  # Change to desired location
```

## Logs and Reports

### Log Files
- `logs/auto-fix/dispatcher.log` - Gate dispatcher activity
- `logs/auto-fix/orchestrator.log` - Orchestration activity
- `logs/auto-fix/syntax-fix.log` - Syntax fix agent
- `logs/auto-fix/type-fix.log` - Type fix agent
- `logs/auto-fix/test-fix.log` - Test fix agent
- `logs/auto-fix/coverage-fix.log` - Coverage agent
- `logs/auto-fix/performance-fix.log` - Performance agent
- `logs/auto-fix/xref-fix.log` - XRef agent

### State Files
- `logs/auto-fix/dispatcher-state.json` - Current attempt counts

### Reports
- `logs/auto-fix/escalation-*.txt` - Escalation reports
- `logs/auto-fix/*-suggestions-*.txt` - Manual fix suggestions
- `logs/auto-fix/*-errors-*.txt` - Temporary error captures

## Troubleshooting

### Reset Everything

```bash
rm -rf logs/auto-fix/*.json
rm -rf logs/auto-fix/*-errors-*.txt
./tools/auto-fix/gate-failure-dispatcher.sh reset
```

### View Recent Escalations

```bash
ls -lt logs/auto-fix/escalation-*.txt | head -5
cat $(ls -t logs/auto-fix/escalation-*.txt | head -1)
```

### Check What Failed

```bash
./tools/auto-fix/gate-failure-dispatcher.sh status
tail -50 logs/auto-fix/orchestrator.log
```

### Manual Gate Execution

```bash
# Run gates manually without auto-fix
TERM=dumb rebar3 compile
rebar3 dialyzer
rebar3 xref
rebar3 eunit
rebar3 cover --verbose
```

## What Auto-Fix Can Do

✅ Fix unused variables (prefix with `_`)
✅ Add missing exports
✅ Add basic type specs
✅ Update simple test assertions
✅ Add setup/teardown fixtures
✅ Fix mock cleanup issues
✅ Remove unused exports
✅ Generate test templates

## What Auto-Fix Cannot Do

❌ Fix logic errors
❌ Infer correct business requirements
❌ Resolve complex type relationships
❌ Fix race conditions
❌ Optimize performance automatically
❌ Write meaningful tests
❌ Resolve circular dependencies
❌ Fix architectural issues

## Best Practices

1. **Run locally first** - Test auto-fix before using in CI
2. **Review auto-fixes** - Always check what was changed
3. **Use iteration limits wisely** - Don't rely on too many iterations
4. **Monitor escalations** - Track which gates escalate most
5. **Keep logs** - Preserve escalation reports for learning
6. **Reset state regularly** - After resolving issues
7. **Don't over-rely** - Some issues need human judgment

## Support

For detailed documentation, see:
- [docs/auto-fix/AUTO_FIX_SYSTEM.md](../../docs/auto-fix/AUTO_FIX_SYSTEM.md)
- [CLAUDE.md](../../CLAUDE.md)

For issues:
1. Check logs in `logs/auto-fix/`
2. Review escalation reports
3. Consult documentation
4. Open issue in erlmcp repository
