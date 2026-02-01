# erlmcp CLI Examples

This directory contains example scripts and recordings demonstrating how to use the erlmcp CLI.

## Files

### Interactive Sessions

**`interactive-session.txt`** - Recorded example REPL session

A complete example session showing:
- Starting the REPL
- Creating servers and clients
- Managing resources
- Monitoring performance
- Cleaning up processes
- Exiting the shell

Learn how to:
- Use basic REPL commands
- Create and manage server instances
- Register resources
- Communicate between processes
- Monitor system state

### Shell Scripts

**`validate-spec.sh`** - Specification validation workflows

Complete example of using the validator CLI with 10 different workflows:

1. Validate all specifications
2. Validate specific section (protocol, security, etc.)
3. Validate specific transport (stdio, http, sse, tcp, ws)
4. Generate markdown compliance report
5. Generate JSON compliance report
6. Verbose validation with detailed output
7. Quick compliance check (compile + test + spec)
8. Validate all transport implementations
9. Continuous validation in watch mode
10. Compare with baseline report

**Usage**:
```bash
./validate-spec.sh 1              # Run workflow 1
./validate-spec.sh 3 http         # Validate HTTP transport
./validate-spec.sh 4              # Generate markdown report
./validate-spec.sh all-transports # Check all transports
./validate-spec.sh watch          # Watch for changes
```

See comments in script for more details.

---

**`connect-server.sh`** - Server interaction and testing

Complete example of automation scripts for erlmcp with 5 different workflows:

1. Create and list resources
2. Stress test with many concurrent clients
3. Continuous health monitoring
4. Integration test workflow
5. Memory leak detection

**Usage**:
```bash
./connect-server.sh 1              # Resource demo
./connect-server.sh 2 50           # Stress test with 50 clients
./connect-server.sh 3              # Monitor health (Ctrl+C to stop)
./connect-server.sh 4              # Run integration tests
./connect-server.sh 5              # Check for memory leaks
```

See comments in script for more details.

## Learning Path

### Beginner (5 minutes)

1. Read: [CLI_REFERENCE.md](../../docs/CLI_REFERENCE.md) - Overview section
2. Try: `erlmcp help` and `erlmcp doctor`
3. Watch: `interactive-session.txt` first 10 examples

### Intermediate (15 minutes)

1. Read: [CLI_INTERACTIVE_GUIDE.md](../../docs/CLI_INTERACTIVE_GUIDE.md)
2. Try: Run `erlmcp start` and follow examples 1-10 from interactive session
3. Run: `erlmcp test-100k` and `erlmcp benchmark`

### Advanced (30 minutes)

1. Read: [DIAGNOSTICS_GUIDE.md](../../docs/DIAGNOSTICS_GUIDE.md)
2. Read: [PLUGIN_DEVELOPMENT_GUIDE.md](../../docs/PLUGIN_DEVELOPMENT_GUIDE.md)
3. Try: Run example shell scripts:
   - `./validate-spec.sh 1` - Full spec validation
   - `./connect-server.sh 1` - Resource management demo
   - `./validate-spec.sh 4` - Generate compliance report

### Expert (45+ minutes)

1. Study: Plugin examples in `examples/plugins/`
2. Create: Your own plugin following PLUGIN_DEVELOPMENT_GUIDE.md
3. Test: Using the test examples in each plugin
4. Run: Advanced workflows from shell scripts

## Integration Examples

### CI/CD Integration

```bash
# In .github/workflows/validate.yml
- name: Validate specification
  run: |
    cd erlmcp/examples/cli
    ./validate-spec.sh quick
```

### Monitoring Scripts

```bash
# Monitor system health
cd erlmcp/examples/cli
./connect-server.sh health &  # Background monitoring
```

### Performance Baseline

```bash
# Generate baseline report
cd erlmcp/examples/cli
./validate-spec.sh baseline
```

## Customization

### Adapting validate-spec.sh for your project

The script is designed to be easily customized:

1. Change the PROJECT_ROOT detection
2. Add custom validation steps
3. Modify report formats
4. Add new workflows as case statements

### Adapting connect-server.sh for your use case

Common customizations:

1. Change number of clients in stress test
2. Modify monitoring loop interval
3. Add custom validation logic to integration tests
4. Extend memory leak detection parameters

## Troubleshooting

### "Script not executable"
```bash
chmod +x *.sh
```

### "Command not found: erlmcp"
```bash
export PATH="/path/to/erlmcp/bin:$PATH"
```

### "Build failed" messages
```bash
# In erlmcp root directory
rebar3 clean && rebar3 compile
```

### Scripts hang or timeout
- Check system load: `top` or `htop`
- Increase timeouts in script
- Run in background with `&`

## See Also

- [CLI Reference Guide](../../docs/CLI_REFERENCE.md) - Complete command documentation
- [Interactive Mode Guide](../../docs/CLI_INTERACTIVE_GUIDE.md) - REPL examples
- [Diagnostics Guide](../../docs/DIAGNOSTICS_GUIDE.md) - Advanced profiling
- [Plugin Development Guide](../../docs/PLUGIN_DEVELOPMENT_GUIDE.md) - Create plugins
- [Shell Completions](../../docs/SHELL_COMPLETIONS_GUIDE.md) - Tab completion setup
