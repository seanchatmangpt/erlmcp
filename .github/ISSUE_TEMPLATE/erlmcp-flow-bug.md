---
name: erlmcp-flow Bug Report
about: Report a bug in erlmcp-flow agent coordination transport
title: '[erlmcp-flow] '
labels: 'bug, erlmcp-flow'
assignees: ''
---

# erlmcp-flow Bug Report

## Bug Description
<!-- Clear and concise description of the bug -->

**Component**:
- [ ] Registry (`erlmcp_flow_registry`)
- [ ] Router (`erlmcp_flow_router`)
- [ ] Transport (`erlmcp_flow_transport`)
- [ ] Agent (`erlmcp_flow_agent`)
- [ ] Bridge (stdio/TCP/HTTP)
- [ ] Flow Control (`erlmcp_flow_backpressure`)
- [ ] Serializer (`erlmcp_flow_serializer`)
- [ ] Supervisor
- [ ] Other: _________

## Environment

**erlmcp Version**: _____ (e.g., 2.1.0)
**erlmcp_flow Version**: _____ (e.g., 1.0.0)
**Erlang/OTP Version**: _____ (run `erl -eval '{ok, V} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(V), halt().' -noshell`)
**Operating System**: _____ (e.g., Ubuntu 22.04, macOS 14.2)
**Architecture**: _____ (e.g., x86_64, arm64)

## Steps to Reproduce

<!-- Provide detailed steps to reproduce the bug -->

1. Start erlmcp_flow application:
   ```erlang
   application:start(erlmcp_flow).
   ```

2. Register agent:
   ```erlang
   erlmcp_flow_registry:register_agent(
       <<"agent-1">>,
       self(),
       #{type => <<"test-agent">>}
   ).
   ```

3. Trigger bug:
   ```erlang
   % Your code here
   ```

## Expected Behavior
<!-- What should happen -->



## Actual Behavior
<!-- What actually happens -->



## Error Messages / Logs

<!-- Include any error messages, crash dumps, or relevant logs -->

```
=CRASH REPORT====
...
```

## Reproducibility

- [ ] Always reproducible
- [ ] Intermittent (occurs sometimes)
- [ ] Rare (hard to reproduce)
- [ ] Environment-specific

**Frequency**: _____ (e.g., 100%, 10%, once)

## Impact

**Severity**:
- [ ] Critical (production down, data loss)
- [ ] High (major feature broken)
- [ ] Medium (feature partially broken)
- [ ] Low (minor issue, workaround available)

**Affected Users**: _____ (e.g., all users, specific use case)

## Additional Context

### Related Issues
<!-- Link to related issues -->

- Related to #
- Duplicate of #
- Blocks #

### Workaround
<!-- Is there a workaround? -->

```erlang
% Workaround code
```

### Recent Changes
<!-- Were there recent changes that might have caused this? -->

- Commit: _________
- PR: _________
- Version: _________

### Debugging Information

**Process Info**:
```erlang
% erlang:process_info(Pid) output
```

**System Info**:
```erlang
% erlang:system_info(schedulers_online)
% erlang:memory()
```

**gproc Registry State** (if registry-related):
```erlang
% gproc:info() output
```

## Performance Impact (if applicable)

- [ ] High CPU usage
- [ ] High memory usage
- [ ] Increased latency
- [ ] Reduced throughput

**Metrics**:
- Latency: _____ (before bug) → _____ (with bug)
- Throughput: _____ (before bug) → _____ (with bug)
- Memory: _____ (before bug) → _____ (with bug)

## Possible Root Cause
<!-- Optional: Your analysis of the root cause -->



## Suggested Fix
<!-- Optional: Proposed solution -->



## Quality Gate Impact

Does this bug cause quality gates to fail?

- [ ] Compilation fails
- [ ] Tests fail
- [ ] Dialyzer warnings
- [ ] Xref errors
- [ ] Coverage below 82%
- [ ] Benchmarks fail

## Checklist

- [ ] I have searched existing issues to avoid duplicates
- [ ] I have provided complete steps to reproduce
- [ ] I have included error messages and logs
- [ ] I have specified the environment
- [ ] I have indicated the severity

---

**Reporter**: @_________
**Date**: 2026-02-02
