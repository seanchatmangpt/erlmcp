# [Category] Troubleshooting Template

**Version**: 3.0.0
**Last Updated**: YYYY-MM-DD
**Maintainer**: [Name]

---

## Overview

This guide provides troubleshooting information for [category] issues in erlmcp.

**Scope**: [What this covers]
**Quick Diagnosis**: Use the table below to quickly identify your issue.

---

## Quick Diagnosis

| Symptom | Likely Cause | See Section |
|---------|--------------|-------------|
| [Symptom 1] | [Cause 1] | [Section link] |
| [Symptom 2] | [Cause 2] | [Section link] |
| [Symptom 3] | [Cause 3] | [Section link] |

---

## Diagnostic Tools

### Built-in Diagnostics

```bash
# Health check
./bin/erlmcp health

# Status check
./bin/erlmcp status

# Diagnostics
./bin/erlmcp diagnostics

# Log location
./bin/erlmcp log-path
```

### Erlang Shell Diagnostics

```erlang
% Connect to running node
erl -name debug@127.0.0.1 -setcookie erlmcp -remsh erlmcp@hostname

% Once connected
% Check process count
(erlmcp@host)1> erlang:system_info(process_count).

% Check memory
(erlmcp@host)2> erlang:memory().

% Check ports
(erlmcp@host)3> length(erlang:ports()).

% Check messages
(erlmcp@host)4> erlang:statistics(message_queue_len).

% Start observer
(erlmcp@host)5> observer:start().
```

---

## Problem: [Issue Title]

### Symptoms

- [Symptom 1]
- [Symptom 2]
- [Symptom 3]

**When it occurs**: [Conditions that trigger the issue]

**Impact**: [What is affected]

---

### Diagnosis

#### Step 1: Check [Resource/Component]

```bash
# Command to check
[Command]

# Expected output
[Expected output]

# If you see different output, proceed to step 2
```

#### Step 2: Verify [Condition]

```bash
# Command to verify
[Command]

# Check for
[What to look for]
```

#### Step 3: Examine Logs

```bash
# Look for these log messages
grep "[pattern]" /var/log/erlmcp/erlmcp.log
```

**Confirm the issue if**: [Confirmation criteria]

---

### Solution

#### Solution 1: [Fix Title]

**When to use**: [Conditions]

**Steps**:

1. [Step 1]
   ```bash
   # Command or code
   ```

2. [Step 2]
   ```bash
   # Command or code
   ```

3. [Step 3]
   ```bash
   # Command or code
   ```

4. Verify fix:
   ```bash
   # Verification command
   ```

**Expected Result**: [What you should see]

---

#### Solution 2: [Alternative Fix]

**When to use**: [When Solution 1 doesn't apply]

**Steps**:

1. [Step 1]
2. [Step 2]
3. [Step 3]

**Expected Result**: [What you should see]

---

### Prevention

To prevent this issue from recurring:

1. **Configure [Setting]**
   ```erlang
   %% Configuration
   {setting, value}
   ```

2. **Monitor [Metric]**
   ```bash
   # Add to monitoring
   ```

3. **Regular Maintenance**
   - [Task 1]: [Frequency]
   - [Task 2]: [Frequency]

---

## Problem: [Another Issue Title]

### Symptoms

- Service fails to start
- Error: `[specific error message]`
- [Additional symptoms]

---

### Diagnosis

```bash
# Check 1
[Command]

# Check 2
[Command]

# Check 3
[Command]
```

**Look for**: [What indicates the issue]

---

### Solution

```bash
# Fix command or code
```

**Explanation**: [Why this works]

---

### Prevention

- [Prevention tip 1]
- [Prevention tip 2]

---

## Common Error Messages

### Error: `{error, eaddrinuse}`

**Meaning**: Port already in use

**Solution**:
```bash
# Find process using port
lsof -i :5005

# Kill process if appropriate
kill -9 [PID]

# Or change port in configuration
```

### Error: `{error, timeout}`

**Meaning**: Operation timed out

**Solution**:
```erlang
%% Increase timeout in configuration
{timeout_ms, 30000}  % 30 seconds
```

### Error: `{error, connection_refused}`

**Meaning**: Cannot connect to remote node

**Solution**:
```bash
# Check network connectivity
ping [hostname]

# Check EPMD is running
epmd -names

# Check node name matches
```

---

## Performance Issues

### Slow Response Times

**Symptoms**: Requests take longer than expected

**Diagnosis**:

```erlang
% Check message queue
(erlmcp@host)1> recon:proc_count(message_queue_len, 10).

% Check for blocking operations
(erlmcp@host)2> recon:trace(erlmcp_server, handle_call, 3, [{scope, g}]).
```

**Solutions**:

1. Add more schedulers:
   ```
   +S 16:16
   ```

2. Increase process pool:
   ```erlang
   {pool_size, 20}
   ```

3. Profile and optimize:
   ```erlang
   fprof:apply(Module, Function, Args).
   ```

---

### High Memory Usage

**Symptoms**: Memory usage growing over time

**Diagnosis**:

```erlang
% Check memory breakdown
(erlmcp@host)1> recon:memory(allocated).

% Find largest processes
(erlmcp@host)2> recon:bin_leak(100).
```

**Solutions**:

1. Restart leaking processes
2. Adjust memory limits
3. Check for large binaries
4. Verify ETS tables are bounded

---

## Network Issues

### Cannot Connect to Remote Node

**Symptoms**: Connection refused, timeout

**Diagnosis**:

```bash
# Check EPMD
epmd -names

# Check port
telnet [hostname] 4369

# Check name resolution
getent hosts [hostname]
```

**Solutions**:

1. Verify node name format: `name@fully.qualified.domain.name`
2. Check firewall rules allow EPMD (4369) and distribution
3. Ensure cookies match

### Connection Drops

**Symptoms**: Intermittent disconnections

**Diagnosis**:

```bash
# Check network stability
ping -i 0.1 [hostname]

# Check TCP settings
sysctl net.ipv4.tcp_keepalive_time
```

**Solutions**:

1. Enable keepalive:
   ```erlang
   {tcp_keepalive, true}
   ```

2. Increase timeouts:
   ```erlang
   {connect_timeout, 30000}
   ```

---

## Getting More Help

### Information to Collect

When seeking help, collect:

1. **System Information**
   ```bash
   erl -version
   uname -a
   ```

2. **erlmcp Version**
   ```bash
   ./bin/erlmcp version
   ```

3. **Configuration**
   ```bash
   cat /etc/erlmcp/sys.config
   ```

4. **Recent Logs**
   ```bash
   journalctl -u erlmcp -n 100
   ```

5. **Error Messages**
   - Full error text
   - Stack traces
   - Context around error

### Where to Get Help

- **Documentation**: [Link to relevant docs]
- **GitHub Issues**: [Link to issue tracker]
- **Discussions**: [Link to discussions]
- **Community**: [Link to community forum/chat]

### Bug Report Template

```markdown
## Description
[Clear description of the problem]

## Steps to Reproduce
1. [Step 1]
2. [Step 2]
3. [Step 3]

## Expected Behavior
[What should happen]

## Actual Behavior
[What actually happens]

## Environment
- erlmcp version: [version]
- Erlang/OTP version: [version]
- OS: [distribution and version]

## Logs
[Relevant log excerpts]

## Additional Context
[Any other relevant information]
```

---

## Escalation Path

1. **Self-Service**: Use this guide
2. **Community**: Post in discussions (response within 24 hours)
3. **Issues**: Create GitHub issue for bugs (response within 1 week)
4. **Support**: Contact support team for enterprise customers (response within 4 hours)

---

## Appendix: Diagnostic Commands Reference

### System Commands

```bash
# Process info
ps aux | grep beam

# Open files
lsof -p [PID]

# Network connections
netstat -an | grep [PORT]

# Disk usage
df -h

# Memory usage
free -h
```

### Erlang Commands

```erlang
% Process info
erlang:process_info(Pid).

% Memory info
erlang:memory().

% System info
erlang:system_info(allocated_areas).

% GC statistics
erlang:statistics(garbage_collection).

% Module info
module:module_info().
```

---

**Status**: [Draft | Review | Published]
**Review Date**: [YYYY-MM-DD]
**Last Updated**: [YYYY-MM-DD]
