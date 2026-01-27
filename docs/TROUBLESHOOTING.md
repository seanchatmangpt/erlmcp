# Troubleshooting Guide

**Estimated read time: 10 minutes**

Solutions for common issues encountered during development, testing, and production deployment.

## Build Issues

### Problem: "rebar3 not found"

**Symptoms**: Command not found error when running make

**Solutions**:
```bash
# macOS
brew install rebar3

# Ubuntu/Debian
sudo apt-get install rebar3
apt-get install erlang rebar3

# From source
git clone https://github.com/erlang/rebar3.git
cd rebar3 && ./bootstrap && sudo mv rebar3 /usr/local/bin/
```

**Verify**:
```bash
rebar3 version
# Should show: rebar 3.22.x
```

---

### Problem: "Erlang/OTP version mismatch"

**Symptoms**: Compilation error about incompatible Erlang version

**Solutions**:
```bash
# Check current version
erl -version

# Update Erlang (macOS)
brew upgrade erlang

# Using asdf (recommended)
asdf plugin add erlang
asdf install erlang 25.3          # Install specific version
asdf local erlang 25.3            # Set for project
erl -version                       # Verify
```

**Verify**:
```bash
erl -version
# Should show: Erlang/OTP 25+
```

---

### Problem: "Compilation warning: expression results are not used"

**Symptoms**: Build fails with warnings_as_errors

```
Error: expression results are not used
```

**Solution 1**: Fix the actual issue
```erlang
% BAD - result not used
my_function(X) ->
    foo:bar(X).

% GOOD - use or ignore result
my_function(X) ->
    _ = foo:bar(X),
    ok.
```

**Solution 2**: Suppress if necessary
```erlang
-compile({nowarn_unused_function, [my_helper/0]}).
```

---

### Problem: "dependency not found"

**Symptoms**: Error during rebar3 get-deps

**Solutions**:
```bash
# Clean and retry
rebar3 unlock
rebar3 get-deps

# Force update
rebar3 update

# Check network
curl https://hex.pm/

# Use mirror (if primary is down)
export HEX_MIRROR=https://repo.hex.pm/
```

---

## Test Issues

### Problem: "Test timeout"

**Symptoms**: Tests hang or timeout after 60 seconds

**Solutions**:

1. **Increase timeout in rebar.config**:
```erlang
{ct_opts, [
    {ct_hooks, []},
    {ct_runtime, 600}     % 10 minutes
]}.

{eunit_opts, [
    {timeout, 600000}     % 10 seconds per test
]}.
```

2. **Run single test**:
```bash
rebar3 eunit --module=slow_test
rebar3 ct --suite=slow_SUITE
```

3. **Profile test**:
```bash
# In test, add timing
{Time, Result} = timer:tc(fun() -> slow_operation() end),
?debugMsg(io_lib:format("Operation took ~p ms", [Time / 1000])).
```

---

### Problem: "All tests fail with 'node not started'"

**Symptoms**: Common Test crashes on startup

**Solutions**:
```bash
# Clean and rebuild
make distclean
make workspace-build

# Check sys.config
cat config/sys.config

# Run verbose
rebar3 ct -v --name 'test@127.0.0.1'
```

---

### Problem: "Test passes locally but fails in CI"

**Symptoms**: Intermittent test failures

**Solutions**:

1. **Check environment**:
```bash
env | grep ERL
echo $ERL_FLAGS
echo $ERL_LIBS
```

2. **Run multiple times**:
```bash
for i in {1..10}; do make test || break; done
```

3. **Check for timing issues**:
```erlang
% BAD - assumes timing
ok = expect_message(),

% GOOD - wait with timeout
receive
    Message -> ok
after 5000 ->
    {error, timeout}
end
```

4. **Check for process leaks**:
```erlang
% Clean up in test cleanup
end_per_testcase(_TestCase, Config) ->
    % Verify all processes stopped
    InitialProcesses = proplists:get_value(initial_processes, Config),
    CurrentProcesses = erlang:processes(),
    case length(CurrentProcesses) - length(InitialProcesses) of
        0 -> ok;
        N -> {error, {process_leak, N}}
    end.
```

---

## Compilation Issues

### Problem: "Undefined function or macro"

**Symptoms**:
```
src/module.erl:42: function foo/1 undefined
```

**Solutions**:

1. **Check export**:
```erlang
-module(my_module).
-export([foo/1]).  % ← Add export

foo(X) -> X.
```

2. **Check include**:
```erlang
-include("erlmcp.hrl").  % ← May be needed
```

3. **Check spelling**:
```erlang
% Check module and function names match exactly
my_module:my_function()  % Correct
my_module:my_Function()  % Wrong (capital F)
```

---

### Problem: "Type error"

**Symptoms**:
```
src/module.erl:42: The call ... does not have the right type
```

**Solutions**:

1. **Add type hints**:
```erlang
% BAD - no type hint
my_function(X) -> X.

% GOOD - type hint
-spec my_function(integer()) -> integer().
my_function(X) -> X.
```

2. **Fix type mismatch**:
```erlang
% BAD - mixing types
my_function(X) ->
    case X of
        A when is_atom(A) -> atom_to_list(X);  % Returns list
        _ -> X                                   % Returns term
    end.

% GOOD - consistent return
-spec my_function(atom() | binary()) -> string().
my_function(X) when is_atom(X) ->
    atom_to_list(X);
my_function(X) when is_binary(X) ->
    binary_to_list(X).
```

3. **Run dialyzer separately**:
```bash
rebar3 dialyzer --verbose
```

---

## Runtime Issues

### Problem: "Application failed to start"

**Symptoms**: Application crashes on startup

**Solutions**:

1. **Check logs**:
```bash
tail -f /var/log/erlmcp/erlmcp.log
tail -f /var/log/erlmcp/crash.log
```

2. **Start interactively**:
```bash
make console
# Watch startup messages
```

3. **Check config**:
```bash
cat config/sys.config
rebar3 shell -s erlmcp
```

4. **Verify dependencies**:
```bash
rebar3 tree
rebar3 deps
```

---

### Problem: "Port already in use"

**Symptoms**: `{error, eaddrinuse}`

**Solutions**:

1. **Find process using port**:
```bash
lsof -i :5005          # erlang distribution
lsof -i :8080          # HTTP
netstat -an | grep 5005
```

2. **Kill process**:
```bash
pkill -f erlmcp
pkill -f "erl -name"
```

3. **Change port** (in sys.config):
```erlang
{erlmcp, [
    {listen_port, 5006}  % Changed from 5005
]}.
```

---

### Problem: "Out of memory"

**Symptoms**: VM crashes with OOM error

**Solutions**:

1. **Check memory usage**:
```bash
make console

% In shell:
erlang:memory().
% Shows: total, processes, atom, atom_used, ...

erlang:memory(total).  % Overall memory
```

2. **Find memory leak**:
```erlang
% Monitor growth
observer:start().
% Watch Memory tab
```

3. **Increase limits**:
```erlang
% In vm.args:
+hms 256     % Heap min size
+hml 256     % Heap max size
```

4. **Reduce connections**:
```erlang
% In sys.config:
{erlmcp, [
    {max_connections, 1000}  % Reduced from 10000
]}.
```

---

### Problem: "Supervisor shutdown"

**Symptoms**: Application stops with supervisor error

**Solutions**:

1. **Check supervisor tree**:
```erlang
% In console:
observer:start().
% View Supervision tab
```

2. **Verify callback**:
```erlang
% Make sure your callback handles all cases:
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} |
    {stop, term(), term(), state()}.

handle_call(Request, _From, State) ->
    {reply, {error, unknown_request}, State}.  % Don't crash
```

3. **Check for infinite loops**:
```erlang
% BAD - infinite recursion
loop() -> loop().

% GOOD - proper gen_server
-behavior(gen_server).
```

---

## Deployment Issues

### Problem: "Container fails to start"

**Symptoms**: Docker container exits immediately

**Solutions**:

1. **Check logs**:
```bash
docker logs container_id
```

2. **Run with terminal**:
```bash
docker run -it \
    --entrypoint /bin/bash \
    erlmcp:latest
```

3. **Verify image**:
```bash
docker inspect erlmcp:latest
docker exec container_id /app/erlmcp/bin/erlmcp pid
```

---

### Problem: "Kubernetes pod keeps crashing"

**Symptoms**: Pod restarts repeatedly

**Solutions**:

1. **Check events**:
```bash
kubectl describe pod erlmcp-xxx
kubectl logs erlmcp-xxx --previous
```

2. **Check resource limits**:
```bash
kubectl top pods
# If memory > limit, pod killed

# Increase limit in deployment.yaml:
resources:
  limits:
    memory: "2Gi"
```

3. **Check health probes**:
```bash
# Disable initially for debugging
livenessProbe: null
readinessProbe: null

# Then test manually
kubectl exec erlmcp-xxx -- /app/erlmcp/bin/erlmcp pid
```

---

### Problem: "High latency/slow response"

**Symptoms**: Requests take >1 second

**Solutions**:

1. **Profile code**:
```bash
make profile
# Uses recon tracing
```

2. **Check process queue**:
```erlang
(erlmcp@host)1> erlang:process_info(whereis(erlmcp_server), message_queue_len).
```

3. **Trace calls**:
```erlang
recon:trace(erlmcp_server, handle_call, 3, [{scope, g}])
```

4. **Check system load**:
```bash
top -p $(pidof erl)
watch -n 1 'top -bn1 | grep erl'
```

---

## Monitoring & Metrics

### Problem: "No metrics available"

**Solutions**:

1. **Enable metrics export**:
```erlang
{erlmcp, [
    {metrics_enabled, true},
    {metrics_port, 9090}
]}.
```

2. **Check Prometheus endpoint**:
```bash
curl http://localhost:9090/metrics
```

3. **Verify telemetry handler**:
```erlang
% Register handler
telemetry:attach("erlmcp-handler",
    [:erlmcp, :request, :complete],
    fun handle_event/4,
    []).
```

---

## Common Solutions Checklist

When troubleshooting, systematically check:

- [ ] Error logs: `tail -f logs/*.log`
- [ ] Process status: `ps aux | grep erl`
- [ ] Network: `netstat -an | grep 5005`
- [ ] Resources: `top`, `df -h`, `free -h`
- [ ] Configuration: `cat config/sys.config`
- [ ] Dependencies: `rebar3 tree`
- [ ] Erlang version: `erl -version`
- [ ] rebar3 version: `rebar3 version`
- [ ] Environment: `env | grep ERL`
- [ ] Kernel limits: `ulimit -a`

## Getting Help

1. **Check documentation**:
   - [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md)
   - [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md)
   - [FOR_OPERATORS.md](FOR_OPERATORS.md)

2. **Search existing issues**:
   - GitHub Issues
   - Stack Overflow (tag: erlang)

3. **Reproduce issue**:
   - Minimal test case
   - Step-by-step reproduction
   - Environment details

4. **Create detailed issue**:
   ```
   Title: Clear description

   Environment:
   - Erlang/OTP version: 25.x
   - rebar3 version: 3.22.x
   - OS: macOS/Linux/Windows

   Steps to Reproduce:
   1. ...
   2. ...
   3. ...

   Expected Behavior:
   ...

   Actual Behavior:
   ...

   Logs/Output:
   ...
   ```

## Next Steps

- **For deployment help**: See [DEPLOYMENT.md](DEPLOYMENT.md)
- **For operator help**: See [FOR_OPERATORS.md](FOR_OPERATORS.md)
- **For dev help**: See [FOR_DEVELOPERS.md](FOR_DEVELOPERS.md)
- **For GCP help**: See [GCP_SETUP.md](GCP_SETUP.md)

---

**Last Updated**: 2026-01-26
**Status**: Comprehensive troubleshooting guide
**Covers**: Build, Test, Runtime, Deployment
