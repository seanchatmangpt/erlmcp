# erlmcp CLI Documentation Index

Complete index and guide to all CLI documentation. Use this to quickly find what you're looking for.

**Goal**: Master the erlmcp CLI in under 30 minutes. Start with Quick Start below.

---

## Quick Start (5 minutes)

New to erlmcp? Start here:

```bash
# 1. Initialize
erlmcp init

# 2. Check readiness
erlmcp doctor

# 3. Start interactive shell
erlmcp start

# 4. In the REPL
1> i().                    % List processes
2> erlmcp_registry:list(). % List servers
3> q().                    % Exit
```

**Next**: Read [CLI_REFERENCE.md](#command-reference-guide) for complete command list

---

## Learning Paths

Choose your learning path based on your needs:

### Path 1: Basic Usage (15 minutes)

Perfect for developers wanting to get started quickly.

1. **Read** ([CLI_REFERENCE.md](CLI_REFERENCE.md) - Overview section): 5 min
   - Command summary table
   - When to use each command

2. **Try** ([bin/erlmcp](../bin/erlmcp)): 5 min
   - Run: `erlmcp init`
   - Run: `erlmcp doctor`
   - Run: `erlmcp test-100k`

3. **Reference** ([CLI_REFERENCE.md](CLI_REFERENCE.md)): Ongoing
   - Look up command details as needed
   - Check exit codes and error messages

---

### Path 2: Interactive Development (30 minutes)

For developers who need the REPL for testing and debugging.

1. **Read** ([CLI_INTERACTIVE_GUIDE.md](CLI_INTERACTIVE_GUIDE.md)): 10 min
   - REPL basics
   - Essential commands
   - Working with processes

2. **Study** ([examples/cli/interactive-session.txt](../examples/cli/interactive-session.txt)): 10 min
   - Complete recorded session
   - Real-world examples
   - Patterns and workflows

3. **Practice** in REPL: 10 min
   - Recreate examples from session
   - Experiment with commands
   - Check system state

---

### Path 3: Performance & Diagnostics (45 minutes)

For developers who need to profile, debug, and optimize.

1. **Read** ([DIAGNOSTICS_GUIDE.md](DIAGNOSTICS_GUIDE.md)): 15 min
   - Doctor command deep dive
   - Profiling tools
   - Tracing and monitoring

2. **Study** scripts ([examples/cli/](../examples/cli/)): 15 min
   - validate-spec.sh (validation workflows)
   - connect-server.sh (stress testing, health checks)

3. **Practice** hands-on: 15 min
   - Run `erlmcp doctor --verbose`
   - Run validation script: `./validate-spec.sh quick`
   - Monitor system: `./connect-server.sh health`

---

### Path 4: Plugin Development (60 minutes)

For developers building extensions and integrations.

1. **Read** ([PLUGIN_DEVELOPMENT_GUIDE.md](PLUGIN_DEVELOPMENT_GUIDE.md)): 20 min
   - Plugin architecture
   - Behavior interfaces
   - Lifecycle and registration

2. **Study** examples ([examples/plugins/](../examples/plugins/)): 20 min
   - Transformer plugin: JSON→CSV
   - Validator plugin: Security checks
   - Test patterns

3. **Build** your own: 20 min
   - Create simple transformer
   - Write tests
   - Register and use

---

### Path 5: Expert Integration (90+ minutes)

For architects building complex systems with erlmcp.

1. All of Path 1-4 (60 min)

2. **Read** ([SHELL_COMPLETIONS_GUIDE.md](SHELL_COMPLETIONS_GUIDE.md)): 10 min
   - Shell setup
   - Custom completions

3. **Read** ([../README.md](../README.md)): 10 min
   - Architecture overview
   - Performance baselines
   - Known pitfalls

4. **Advanced integration** (10+ min)
   - CI/CD pipelines
   - Monitoring setup
   - Custom plugins
   - Performance tuning

---

## Documentation Map

### Core Documentation

| Document | Purpose | Read Time | Best For |
|----------|---------|-----------|----------|
| [CLI_REFERENCE.md](CLI_REFERENCE.md) | Command reference | 15 min | Quick lookups, command details |
| [CLI_INTERACTIVE_GUIDE.md](CLI_INTERACTIVE_GUIDE.md) | REPL workflows | 20 min | Interactive development |
| [DIAGNOSTICS_GUIDE.md](DIAGNOSTICS_GUIDE.md) | Profiling & monitoring | 25 min | Performance analysis |
| [SHELL_COMPLETIONS_GUIDE.md](SHELL_COMPLETIONS_GUIDE.md) | Shell setup | 10 min | Tab completion |
| [PLUGIN_DEVELOPMENT_GUIDE.md](PLUGIN_DEVELOPMENT_GUIDE.md) | Plugin creation | 30 min | Building extensions |

### Examples

| File | Type | Demonstrates |
|------|------|---------------|
| [interactive-session.txt](../examples/cli/interactive-session.txt) | Recording | 20+ REPL examples |
| [validate-spec.sh](../examples/cli/validate-spec.sh) | Script | 10 validation workflows |
| [connect-server.sh](../examples/cli/connect-server.sh) | Script | 5 server interaction patterns |
| [formatter-json-to-csv.erl](../examples/plugins/formatter-json-to-csv.erl) | Plugin | Transformer implementation |
| [validator-custom.erl](../examples/plugins/validator-custom.erl) | Plugin | Validator implementation |

---

## Quick Reference by Task

### "I want to..."

#### Get help
- **Quick help**: `erlmcp help`
- **Full reference**: [CLI_REFERENCE.md](CLI_REFERENCE.md)
- **Search docs**: Use Ctrl+F to search in markdown

#### Set up my environment
- **Initialize**: `erlmcp init` → [CLI_REFERENCE.md#erlmcp-init](CLI_REFERENCE.md#erlmcp-init)
- **Check readiness**: `erlmcp doctor` → [CLI_REFERENCE.md#erlmcp-doctor](CLI_REFERENCE.md#erlmcp-doctor)
- **Setup completions**: [SHELL_COMPLETIONS_GUIDE.md](SHELL_COMPLETIONS_GUIDE.md)

#### Start coding interactively
- **Start REPL**: `erlmcp start` → [CLI_REFERENCE.md#erlmcp-start](CLI_REFERENCE.md#erlmcp-start)
- **Learn REPL**: [CLI_INTERACTIVE_GUIDE.md](CLI_INTERACTIVE_GUIDE.md)
- **See examples**: [examples/cli/interactive-session.txt](../examples/cli/interactive-session.txt)

#### Test performance
- **Quick test**: `erlmcp test-100k` → [CLI_REFERENCE.md#erlmcp-test-100k](CLI_REFERENCE.md#erlmcp-test-100k)
- **Full benchmark**: `erlmcp benchmark` → [CLI_REFERENCE.md#erlmcp-benchmark](CLI_REFERENCE.md#erlmcp-benchmark)
- **Analyze results**: [DIAGNOSTICS_GUIDE.md#analyzing-performance-output](DIAGNOSTICS_GUIDE.md#analyzing-performance-output)

#### Debug issues
- **System diagnostics**: `erlmcp doctor` → [DIAGNOSTICS_GUIDE.md#doctor-command](DIAGNOSTICS_GUIDE.md#doctor-command)
- **Profile code**: [DIAGNOSTICS_GUIDE.md#profiling-tools](DIAGNOSTICS_GUIDE.md#profiling-tools)
- **Trace execution**: [DIAGNOSTICS_GUIDE.md#tracing](DIAGNOSTICS_GUIDE.md#tracing)
- **Monitor performance**: [DIAGNOSTICS_GUIDE.md#watchmonitor](DIAGNOSTICS_GUIDE.md#watchmonitor)

#### Validate specifications
- **Quick validation**: `./examples/cli/validate-spec.sh 7` (quick check)
- **All validations**: `./examples/cli/validate-spec.sh 1` (comprehensive)
- **Generate report**: `./examples/cli/validate-spec.sh 4` (markdown report)
- **Guide**: [examples/cli/validate-spec.sh](../examples/cli/validate-spec.sh) comments

#### Manage servers
- **Create server**: [CLI_INTERACTIVE_GUIDE.md#server-management](CLI_INTERACTIVE_GUIDE.md#server-management)
- **List servers**: [CLI_INTERACTIVE_GUIDE.md#querying-system-state](CLI_INTERACTIVE_GUIDE.md#querying-system-state)
- **Monitor servers**: [DIAGNOSTICS_GUIDE.md#real-time-process-monitoring](DIAGNOSTICS_GUIDE.md#real-time-process-monitoring)

#### Build plugins
- **Plugin guide**: [PLUGIN_DEVELOPMENT_GUIDE.md](PLUGIN_DEVELOPMENT_GUIDE.md)
- **Transformer example**: [examples/plugins/formatter-json-to-csv.erl](../examples/plugins/formatter-json-to-csv.erl)
- **Validator example**: [examples/plugins/validator-custom.erl](../examples/plugins/validator-custom.erl)
- **Test plugins**: [PLUGIN_DEVELOPMENT_GUIDE.md#testing-plugins](PLUGIN_DEVELOPMENT_GUIDE.md#testing-plugins)

#### Set up CI/CD
- **Validation in CI**: See [examples/cli/validate-spec.sh](../examples/cli/validate-spec.sh) for integration examples
- **Performance tracking**: [DIAGNOSTICS_GUIDE.md#integration-with-external-tools](DIAGNOSTICS_GUIDE.md#integration-with-external-tools)
- **Automated testing**: Run `./validate-spec.sh quick` in CI pipeline

---

## Common Workflows

### Workflow 1: Daily Development

```bash
# Morning: Check everything
erlmcp doctor

# Code
erlmcp start
# [Interactive REPL work]
q().

# Evening: Validate
erlmcp test-100k
erlmcp benchmark
```

**Time**: 30 minutes | **Docs**: [CLI_REFERENCE.md](CLI_REFERENCE.md), [CLI_INTERACTIVE_GUIDE.md](CLI_INTERACTIVE_GUIDE.md)

### Workflow 2: Performance Analysis

```bash
# Profile
erlmcp doctor

# Benchmark
erlmcp benchmark

# Diagnose
erlmcp start
# [Use diagnostics tools in REPL]
q().
```

**Time**: 45 minutes | **Docs**: [DIAGNOSTICS_GUIDE.md](DIAGNOSTICS_GUIDE.md)

### Workflow 3: Validation & Release

```bash
# Run validation script
./examples/cli/validate-spec.sh 7    # Quick check
./examples/cli/validate-spec.sh 1    # Full validation
./examples/cli/validate-spec.sh 4    # Generate report

# If all pass, proceed with release
```

**Time**: 15 minutes | **Docs**: [examples/cli/validate-spec.sh](../examples/cli/validate-spec.sh)

### Workflow 4: Stress Testing

```bash
# Run stress test
./examples/cli/connect-server.sh 2 100   # 100 clients
./examples/cli/connect-server.sh 3       # Monitor health

# Analyze results
erlmcp benchmark
```

**Time**: 5 minutes | **Docs**: [examples/cli/connect-server.sh](../examples/cli/connect-server.sh)

### Workflow 5: Plugin Development

```bash
# Start with example
cp examples/plugins/formatter-json-to-csv.erl my-plugin.erl

# Modify
[Edit my-plugin.erl]

# Test
erlmcp start
c(my_plugin).
my_plugin:run_tests().
q().

# Deploy
# [Add to project]
```

**Time**: 30 minutes | **Docs**: [PLUGIN_DEVELOPMENT_GUIDE.md](PLUGIN_DEVELOPMENT_GUIDE.md)

---

## Troubleshooting

### "I'm stuck"

1. **Check system status**: `erlmcp doctor`
2. **Read relevant guide**:
   - Basic questions → [CLI_REFERENCE.md](CLI_REFERENCE.md)
   - REPL issues → [CLI_INTERACTIVE_GUIDE.md](CLI_INTERACTIVE_GUIDE.md)
   - Performance issues → [DIAGNOSTICS_GUIDE.md](DIAGNOSTICS_GUIDE.md)
   - Plugin issues → [PLUGIN_DEVELOPMENT_GUIDE.md](PLUGIN_DEVELOPMENT_GUIDE.md)
3. **Check examples**: Look at [examples/](../examples/) directory
4. **Search docs**: Use Ctrl+F in markdown files
5. **File an issue**: Use [GitHub Issues](https://github.com/yourusername/erlmcp/issues)

### Command not found

**Problem**: `erlmcp` command not found

**Solution**: Add to PATH:
```bash
export PATH="/path/to/erlmcp/bin:$PATH"
```
See [CLI_REFERENCE.md#troubleshooting](CLI_REFERENCE.md#troubleshooting)

### Tests failing

**Problem**: `erlmcp test-100k` or `erlmcp benchmark` fails

**Solution**:
1. Check system load: `top` or `htop`
2. Close other applications
3. Run again
4. See [CLI_REFERENCE.md#test-100k](CLI_REFERENCE.md#test-100k) for details

### REPL errors

**Problem**: Error in interactive shell

**Solution**: See [CLI_INTERACTIVE_GUIDE.md#error-handling](CLI_INTERACTIVE_GUIDE.md#error-handling)

---

## Performance Summary

| Operation | Time | Notes |
|-----------|------|-------|
| `erlmcp init` | 10-30s | Compiles project |
| `erlmcp doctor` | 5-15s | Runs diagnostics |
| `erlmcp start` | 5-10s | Launches REPL |
| `erlmcp test-100k` | 30-60s | Performance test |
| `erlmcp benchmark` | 2-5m | Full suite |
| `erlmcp help` | <1s | Show help |

---

## Documentation Statistics

| Metric | Value |
|--------|-------|
| Total docs | 6 guides |
| Total examples | 5 files |
| Total lines | 3000+ |
| Average read time | 15-20 min |
| Learning paths | 5 |
| Workflows | 5+ |

---

## Version Information

- **CLI Version**: See `erlmcp help`
- **Erlang Requirement**: OTP 28.3.1+
- **Documentation Version**: 1.0.0 (2026-02)
- **Status**: Complete and tested

---

## Quick Links

- [CLI Reference Guide](CLI_REFERENCE.md) - All commands
- [Interactive Mode Guide](CLI_INTERACTIVE_GUIDE.md) - REPL workflows
- [Diagnostics Guide](DIAGNOSTICS_GUIDE.md) - Profiling & monitoring
- [Shell Completions](SHELL_COMPLETIONS_GUIDE.md) - Tab setup
- [Plugin Development](PLUGIN_DEVELOPMENT_GUIDE.md) - Build plugins
- [CLI Examples](../examples/cli/) - Scripts & recordings
- [Plugin Examples](../examples/plugins/) - Plugin implementations
- [Project README](../README.md) - Project overview

---

## Getting Help

1. **Quick question?** → Search in [CLI_REFERENCE.md](CLI_REFERENCE.md)
2. **How do I...?** → Check [Quick Reference by Task](#quick-reference-by-task)
3. **See examples** → Browse [examples/](../examples/)
4. **Detailed help** → Read the relevant guide
5. **File a bug** → GitHub Issues

---

## Contributing

Found an issue or want to improve the docs?

1. Check existing issues: [GitHub Issues](https://github.com/yourusername/erlmcp/issues)
2. Read [CONTRIBUTING.md](../CONTRIBUTING.md)
3. Submit PR with improvements

---

**Last Updated**: 2026-02-01 | **Status**: Complete | **Tested**: ✅
