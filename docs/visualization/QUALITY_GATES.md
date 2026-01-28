# Quality Gates Visualization

Comprehensive visualization system for erlmcp's 8 TCPS quality gates with real-time monitoring, WebSocket updates, and ASCII terminal output.

## Overview

The quality gates visualization provides:
- **Real-time HTML dashboard** with WebSocket updates
- **ASCII terminal visualization** for CLI/CI monitoring
- **JSON API** for integration with other tools
- **Historical pass rate tracking** (last 10 runs)
- **Mobile-responsive design** for monitoring on any device

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Quality Gate System                        │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────┐      ┌──────────────┐      ┌───────────┐ │
│  │ gate-status- │─────▶│  quality_    │─────▶│  HTML     │ │
│  │  json.sh     │      │  gate_server │      │ Dashboard │ │
│  │ (8 gates)    │      │  (Cowboy)    │      │ (port     │ │
│  └──────────────┘      │              │      │  9091)    │ │
│         │               │  WebSocket   │      └───────────┘ │
│         │               │  Broadcast   │             │       │
│         ▼               └──────────────┘             │       │
│  ┌──────────────┐              │                    │       │
│  │ gate-status- │              │                    │       │
│  │  ascii.sh    │◀─────────────┘                    │       │
│  │ (Terminal)   │                                   │       │
│  └──────────────┘                                   │       │
│         │                                            │       │
│         ▼                                            ▼       │
│   Terminal Output                            Browser Client │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

## Components

### 1. HTML Dashboard (`quality-gates.html`)

**Features:**
- Real-time WebSocket updates (no polling)
- 8 quality gate status cards with visual indicators
- Overall status banner (pass/fail/blocked/running)
- Pass rate, passed count, failed count metrics
- Historical pass rate chart (last 10 runs)
- Mobile-responsive grid layout
- Auto-reconnect on connection loss

**Status Icons:**
- ✅ Pass - Green gradient, all checks passed
- ❌ Fail - Red gradient, critical failure
- ⏳ Running - Orange pulse animation
- ⚠️ Warning - Yellow, non-critical issues
- 🚫 Blocked - Magenta, dependencies not met

**Access:**
```bash
# Open in browser
open http://localhost:9091/
```

### 2. Erlang WebSocket Server (`quality_gate_server.erl`)

**OTP gen_server behavior:**
- Manages gate status in state
- Broadcasts updates to all connected WebSocket clients
- Serves static HTML dashboard
- Provides REST API endpoint

**API:**
```erlang
%% Start server
quality_gate_server:start().

%% Update gate status
quality_gate_server:update_gate_status(#{
    id => 1,
    name => <<"Compilation">>,
    status => <<"pass">>,
    details => <<"0 errors">>
}).

%% Get current status
Status = quality_gate_server:get_status().

%% Stop server
quality_gate_server:stop().
```

**Endpoints:**
- `GET /` - HTML dashboard
- `GET /api/quality-gates/status` - JSON status
- `WebSocket /ws/quality-gates` - Real-time updates

### 3. JSON Status Generator (`gate-status-json.sh`)

**Runs all 8 gates and outputs JSON:**

```bash
# Run all gates
./tools/visualize/gate-status-json.sh

# Run specific gate
./tools/visualize/gate-status-json.sh --gate=1

# Sample output
{
  "gates": [
    {
      "id": 1,
      "name": "Compilation",
      "status": "pass",
      "timestamp": "2026-01-28T10:30:45Z",
      "duration": "1.2s",
      "details": "0 errors"
    },
    ...
  ],
  "overall": "pass",
  "pass_rate": 100,
  "passed_count": 8,
  "failed_count": 0,
  "timestamp": "2026-01-28T10:30:45Z"
}
```

**Gate Checks:**
1. **Compilation** - `rebar3 compile` (0 errors)
2. **Unit Tests** - `rebar3 eunit` (100% pass)
3. **Integration Tests** - `rebar3 ct` (all suites)
4. **Coverage** - `rebar3 cover` (≥80%)
5. **Dialyzer** - `rebar3 dialyzer` (0 warnings)
6. **Xref** - `rebar3 xref` (0 undefined calls)
7. **Format Check** - `rebar3 format --verify` (all formatted)
8. **Benchmarks** - Quick validation (modules present)

### 4. ASCII Terminal Visualization (`gate-status-ascii.sh`)

**Terminal-friendly status display:**

```bash
# One-time display
./tools/visualize/gate-status-ascii.sh

# Watch mode (refresh every 2s)
./tools/visualize/gate-status-ascii.sh --watch

# Compact mode
./tools/visualize/gate-status-ascii.sh --compact

# From JSON file
./tools/visualize/gate-status-ascii.sh --json-file=status.json
```

**Output Example:**

```
┌────────────────────────────────────────────────────────────┐
│ 🏭 erlmcp Quality Gates - TCPS Dashboard                  │
├────────────────────────────────────────────────────────────┤
│ [✅] 1. Compilation           0 errors                     │
│ [✅] 2. Unit Tests            45 tests passed              │
│ [✅] 3. Integration Tests     All suites passed            │
│ [✅] 4. Coverage (≥80%)       87% coverage                 │
│ [⚠️] 5. Dialyzer              2 warnings                   │
│ [✅] 6. Xref                  0 undefined calls            │
│ [✅] 7. Format Check          All files formatted          │
│ [✅] 8. Benchmarks            Benchmark modules present    │
├────────────────────────────────────────────────────────────┤
│ Overall: ✅ PASS                                           │
│ Pass Rate: 100%  │  Passed: 8  │  Failed: 0               │
│ Last updated: 2026-01-28T10:30:45Z                        │
└────────────────────────────────────────────────────────────┘
```

## Usage Patterns

### Development Workflow

**Terminal monitoring during development:**
```bash
# Watch mode in separate terminal
./tools/visualize/gate-status-ascii.sh --watch

# Or in tmux split
tmux split-window -h './tools/visualize/gate-status-ascii.sh --watch'
```

**Browser dashboard for detailed analysis:**
```bash
# Start server
rebar3 shell
> quality_gate_server:start().

# Open dashboard
open http://localhost:9091/
```

### CI/CD Integration

**GitHub Actions:**
```yaml
- name: Run Quality Gates
  run: |
    ./tools/visualize/gate-status-json.sh > gates.json
    ./tools/visualize/gate-status-ascii.sh --json-file=gates.json

- name: Upload Gate Results
  uses: actions/upload-artifact@v3
  with:
    name: quality-gates
    path: gates.json
```

**Jenkins Pipeline:**
```groovy
stage('Quality Gates') {
    steps {
        sh './tools/visualize/gate-status-json.sh > gates.json'
        sh './tools/visualize/gate-status-ascii.sh --json-file=gates.json'
        archiveArtifacts artifacts: 'gates.json'
    }
}
```

### Pre-commit Hook

**`.git/hooks/pre-commit`:**
```bash
#!/bin/bash
echo "Running quality gates..."
./tools/visualize/gate-status-ascii.sh --compact

if [ $? -ne 0 ]; then
    echo "❌ Quality gates failed. Commit blocked."
    exit 1
fi
```

## TCPS Integration (自働化 - Jidoka)

### Stop-the-Line Authority

The quality gate system embodies **自働化 (Jidoka)** - automation with human touch:

1. **Automatic Detection** - Gates run automatically
2. **Visible Signaling** - Clear status indicators (看板 - Kanban)
3. **Stop Authority** - Any failure blocks progression
4. **Root Cause** - Detailed failure information provided

### Error-Proofing (ポカヨケ - Poka-yoke)

**Built-in mistake prevention:**
- Cannot commit with failing tests
- Cannot deploy with <80% coverage
- Cannot merge with Dialyzer warnings
- Cannot release with failing benchmarks

### WIP Limits (看板 - Kanban)

**Gate progression rules:**
1. Gates 1-3 must pass before Gates 4-6
2. Gates 4-6 must pass before Gates 7-8
3. All gates must pass for deployment
4. Historical pass rate tracked for trends

## Configuration

### Server Configuration

**Start with custom port:**
```erlang
%% In erlmcp.app.src or sys.config
{quality_gate_server, [
    {port, 9091},
    {max_history, 10},
    {broadcast_interval_ms, 1000}
]}
```

### Gate Thresholds

**Edit `gate-status-json.sh`:**
```bash
# Coverage threshold
if [ "${coverage:-0}" -ge 80 ]; then  # Change 80 to desired %
    status="pass"
fi

# Dialyzer warnings
if [ "$warnings" -eq 0 ]; then  # Change to allow warnings
    status="pass"
fi
```

## Troubleshooting

### WebSocket Connection Issues

**Problem:** Dashboard shows "Disconnected"

**Solution:**
```bash
# Check if server is running
lsof -i :9091

# Restart server
rebar3 shell
> quality_gate_server:stop().
> quality_gate_server:start().

# Check firewall
sudo lsof -i :9091
```

### Shell Scripts Not Executable

**Problem:** Permission denied

**Solution:**
```bash
chmod +x tools/visualize/*.sh
```

### JSON Parsing Errors

**Problem:** Invalid JSON output

**Solution:**
```bash
# Validate JSON
./tools/visualize/gate-status-json.sh 2>/dev/null | jq .

# Check stderr separately
./tools/visualize/gate-status-json.sh 2>errors.log | jq .
cat errors.log
```

### ASCII Display Issues

**Problem:** Box characters not rendering

**Solution:**
```bash
# Check terminal encoding
echo $LANG  # Should be UTF-8

# Use compact mode
./tools/visualize/gate-status-ascii.sh --compact

# Disable colors
./tools/visualize/gate-status-ascii.sh --no-color
```

## Performance

**Metrics:**
- Gate execution: ~10-15 seconds (full suite)
- WebSocket latency: <10ms
- JSON generation: <100ms
- Dashboard load time: <500ms
- Browser memory: ~15MB

**Optimization:**
```bash
# Skip slow benchmarks in development
./tools/visualize/gate-status-json.sh --skip=8

# Run only critical gates
for gate in 1 2 4; do
    ./tools/visualize/gate-status-json.sh --gate=$gate
done
```

## Future Enhancements

- [ ] Notification system (email/Slack on failure)
- [ ] Gate dependency graph visualization
- [ ] Time-series metrics (Prometheus integration)
- [ ] Gate execution history (database storage)
- [ ] Multi-project dashboard (umbrella apps)
- [ ] Custom gate definitions (plugin system)
- [ ] Performance trend analysis
- [ ] Automated remediation suggestions

## References

- [TCPS System](../tcps/TCPS.md) - Toyota Code Production System
- [OTP Patterns](../otp-patterns.md) - erlmcp OTP conventions
- [Benchmarking](../metrology/BENCHMARKING.md) - Performance testing
- [CI/CD Integration](../ci-cd/INTEGRATION.md) - Pipeline setup

## See Also

- **Dashboard Server**: `tools/visualize/quality_gate_server.erl`
- **JSON Generator**: `tools/visualize/gate-status-json.sh`
- **ASCII Display**: `tools/visualize/gate-status-ascii.sh`
- **HTML Template**: `tools/visualize/quality-gates.html`
- **TCPS Commands**: `.claude/COMMAND_INDEX.md`
