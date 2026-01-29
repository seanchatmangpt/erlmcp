# GCP Simulator Quick Start

## Installation and Testing

### Prerequisites

Ensure you have Erlang/OTP 25+ and rebar3 installed:

```bash
# Check Erlang version
erl -version

# Check rebar3
rebar3 version
```

### Compilation

From the erlmcp root directory:

```bash
# Compile all applications including examples
make compile

# Or using rebar3 directly
rebar3 compile
```

The GCP simulator modules will be compiled as part of the build:
- `examples/gcp_simulator/gcp_simulator_server.erl`
- `examples/gcp_simulator/gcp_demo.erl`
- `test/gcp_simulator_tests.erl`

### Running Tests

Run the comprehensive EUnit test suite:

```bash
# Run all GCP simulator tests
rebar3 eunit --module=gcp_simulator_tests

# Run with verbose output
rebar3 eunit --module=gcp_simulator_tests --verbose

# Run a specific test
rebar3 eunit --module=gcp_simulator_tests --test=compute_create_instance_test_
```

Expected output:
```
Test passed: compute_create_instance_test_
Test passed: compute_list_instances_test_
Test passed: storage_create_bucket_test_
... (more tests)
All 24 tests passed.
```

### Running the Server

#### Option 1: Using rebar3 shell

```bash
# Start an Erlang shell with all dependencies
rebar3 shell

% In the Erlang shell:
1> gcp_simulator_server:start().
```

#### Option 2: Standalone execution

```bash
# Compile and run directly
erlc -pa _build/default/lib/*/ebin -I include examples/gcp_simulator/*.erl
erl -pa _build/default/lib/*/ebin -pa examples/gcp_simulator -noshell -s gcp_simulator_server start
```

### Running the Demo

From the rebar3 shell:

```erlang
% Run all demo scenarios
1> gcp_demo:run().

% Run specific scenarios
2> gcp_demo:run_web_app_deployment().
3> gcp_demo:run_data_pipeline().
```

## Verification Checklist

✅ Compilation succeeds with no errors
```bash
make compile
# Expected: "Compiled src/gcp_simulator_server.erl"
```

✅ All tests pass
```bash
rebar3 eunit --module=gcp_simulator_tests
# Expected: "All X tests passed"
```

✅ Server starts successfully
```bash
rebar3 shell
1> gcp_simulator_server:start().
# Expected: "GCP Simulator setup complete"
```

✅ Demo runs without errors
```bash
2> gcp_demo:run().
# Expected: Formatted output showing all scenarios
```

## Quality Gates

Before committing changes, ensure:

1. **Zero compilation errors**
   ```bash
   make compile
   ```

2. **All tests pass**
   ```bash
   rebar3 eunit --module=gcp_simulator_tests
   ```

3. **No dialyzer warnings**
   ```bash
   rebar3 dialyzer
   ```

4. **Code coverage ≥80%**
   ```bash
   rebar3 cover --verbose
   ```

## Troubleshooting

### Issue: "command not found: rebar3"

Solution: Install rebar3
```bash
# On Ubuntu/Debian
sudo apt-get install rebar3

# On macOS
brew install rebar3

# Or build from source
git clone https://github.com/erlang/rebar3.git
cd rebar3
./bootstrap
```

### Issue: "erlmcp_stdio not found"

Solution: Ensure all dependencies are compiled
```bash
rebar3 compile
```

### Issue: Tests fail with "table already exists"

Solution: ETS tables from previous runs
```erlang
% In the shell:
ets:delete(gcp_compute_instances).
ets:delete(gcp_storage_buckets).
% ... delete all GCP tables
```

### Issue: "function not exported"

Solution: Recompile the modules
```bash
rebar3 clean
rebar3 compile
```

## Next Steps

After successful verification:

1. Review the [README](README.md) for detailed API documentation
2. Explore the test suite in `test/gcp_simulator_tests.erl` for usage examples
3. Extend the simulator with additional GCP services
4. Integrate with your MCP client application

## Support

For issues:
- Check test files for working examples
- Review erlmcp documentation
- Use `gcp://help` resource for command reference
