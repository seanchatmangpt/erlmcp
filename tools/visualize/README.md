# Quality Gates Visualization System

Real-time monitoring and visualization of erlmcp's 8 TCPS quality gates.

## Quick Start

```bash
# 1. Generate gate status (runs all 8 quality checks)
./gate-status-json.sh > gates.json

# 2. View in terminal (Python - recommended)
./gate-status-ascii.py --json-file=gates.json

# 3. OR view with bash script (requires jq or python3)
./gate-status-ascii.sh --json-file=gates.json

# 4. Start web dashboard
./start-dashboard.sh
# Open http://localhost:9091/
```

## Components

### 1. HTML Dashboard (`quality-gates.html`)

**Real-time web dashboard with WebSocket updates**

Features:
- Live gate status updates (no polling)
- Overall status banner
- Historical pass rate chart (last 10 runs)
- Mobile-responsive design
- Auto-reconnect on disconnect

Access: `http://localhost:9091/` (after starting server)

### 2. Erlang WebSocket Server (`quality_gate_server.erl`)

**OTP gen_server for real-time updates**

```erlang
%% Start server
quality_gate_server:start().

%% Update gate status
quality_gate_server:update_gate_status(#{
    id => 1,
    status => <<"pass">>,
    details => <<"0 errors">>
}).

%% Get current status
Status = quality_gate_server:get_status().
```

Endpoints:
- `GET /` - HTML dashboard
- `GET /api/quality-gates/status` - JSON API
- `WebSocket /ws/quality-gates` - Real-time updates

### 3. JSON Status Generator (`gate-status-json.sh`)

**Runs all 8 quality gates and outputs JSON**

```bash
# Run all gates
./gate-status-json.sh

# Run specific gate
./gate-status-json.sh --gate=1

# Save to file
./gate-status-json.sh > gates.json 2>errors.log
```

Gate checks:
1. Compilation (`rebar3 compile`)
2. Unit Tests (`rebar3 eunit`)
3. Integration Tests (`rebar3 ct`)
4. Coverage ≥80% (`rebar3 cover`)
5. Dialyzer (`rebar3 dialyzer`)
6. Xref (`rebar3 xref`)
7. Format Check (`rebar3 format --verify`)
8. Benchmarks (validation)

Output:
```json
{
  "gates": [
    {"id": 1, "name": "Compilation", "status": "pass", "duration": "1.2s", "details": "0 errors"}
  ],
  "overall": "pass",
  "pass_rate": 100,
  "timestamp": "2026-01-28T10:00:00Z"
}
```

### 4. ASCII Terminal Visualization

**Two implementations:**

#### Python Version (Recommended) - `gate-status-ascii.py`

```bash
# Normal mode
./gate-status-ascii.py --json-file=gates.json

# Compact mode
./gate-status-ascii.py --compact --json-file=gates.json

# No colors
./gate-status-ascii.py --no-color --json-file=gates.json

# From stdin
./gate-status-json.sh 2>/dev/null | ./gate-status-ascii.py
```

Advantages:
- No external dependencies (pure Python 3)
- Fast and reliable
- Better Unicode support

#### Bash Version - `gate-status-ascii.sh`

```bash
# Normal mode
./gate-status-ascii.sh --json-file=gates.json

# Compact mode
./gate-status-ascii.sh --compact --json-file=gates.json

# Watch mode (refresh every 2s)
./gate-status-ascii.sh --watch --json-file=gates.json
```

Requirements:
- `jq` OR `python3` (fallback)
- Bash 4+

Output:
```
┌────────────────────────────────────────────────────────────┐
│ 🏭 erlmcp Quality Gates - TCPS Dashboard                  │
├────────────────────────────────────────────────────────────┤
│ [✅] 1. Compilation          0 errors                      │
│ [✅] 2. Unit Tests           45 tests passed               │
│ [❌] 3. Integration Tests    2 suites failed               │
├────────────────────────────────────────────────────────────┤
│ Overall: ❌ FAIL                                            │
│ Pass Rate: 75%  │  Passed: 6  │  Failed: 2                │
└────────────────────────────────────────────────────────────┘
```

### 5. Dashboard Launcher (`start-dashboard.sh`)

**One-command server startup**

```bash
# Start server and open browser
./start-dashboard.sh

# Start without browser
./start-dashboard.sh --no-browser

# Run in background
./start-dashboard.sh --detach

# Custom port
./start-dashboard.sh --port=8080
```

### 6. Examples (`example-usage.sh`)

**Demonstrates all usage patterns**

```bash
# Run all examples
./example-usage.sh

# Demo mode (simulated gate updates)
./example-usage.sh demo-mode
```

## Usage Patterns

### Development Workflow

**Terminal monitoring:**
```bash
# Watch mode in split terminal
tmux split-window -h './gate-status-ascii.py --watch'

# One-time check
./gate-status-json.sh 2>/dev/null | ./gate-status-ascii.py
```

**Browser dashboard:**
```bash
# Start server
./start-dashboard.sh

# Update gate from Erlang shell
rebar3 shell
> quality_gate_server:update_gate_status(#{id => 1, status => <<"pass">>}).
```

### CI/CD Integration

**GitHub Actions:**
```yaml
- name: Quality Gates
  run: |
    cd tools/visualize
    ./gate-status-json.sh > ../../gates.json
    ./gate-status-ascii.py --json-file=../../gates.json

- name: Upload Results
  uses: actions/upload-artifact@v3
  with:
    name: quality-gates
    path: gates.json
```

**Pre-commit Hook:**
```bash
#!/bin/bash
# .git/hooks/pre-commit
cd tools/visualize
./gate-status-json.sh | ./gate-status-ascii.py --compact

if [ ${PIPESTATUS[0]} -ne 0 ]; then
    echo "❌ Quality gates failed"
    exit 1
fi
```

### Real-time Monitoring

**Start dashboard server:**
```bash
# Terminal 1: Start server
./start-dashboard.sh --no-browser

# Terminal 2: Run gates and update
while true; do
    ./gate-status-json.sh > /tmp/gates.json 2>/dev/null
    curl -X POST http://localhost:9091/api/update \
        -d @/tmp/gates.json \
        -H "Content-Type: application/json"
    sleep 30
done
```

**WebSocket client:**
```javascript
const ws = new WebSocket('ws://localhost:9091/ws/quality-gates');
ws.onmessage = (event) => {
    const status = JSON.parse(event.data);
    console.log('Gate status:', status.overall);
};
```

## TCPS Integration (自働化)

### Japanese Manufacturing Terms

| Term | Pronunciation | Meaning | Implementation |
|------|---------------|---------|----------------|
| 自働化 | Jidoka | Automation with human touch | Stop-the-line on gate failure |
| 看板 | Kanban | Signboard/Visual management | Real-time status visualization |
| ポカヨケ | Poka-yoke | Mistake-proofing | Cannot commit with failures |
| 改善 | Kaizen | Continuous improvement | Historical tracking |

### Stop-the-Line Authority

Quality gates embody **Jidoka** principles:

1. **Automatic Detection** - Gates run on every change
2. **Visible Signaling** - Clear status indicators
3. **Stop Authority** - Any failure blocks progression
4. **Root Cause** - Detailed failure information

### Error-Proofing (ポカヨケ)

Built-in mistake prevention:
- ❌ Cannot commit with failing tests
- ❌ Cannot deploy with <80% coverage
- ⚠️ Cannot merge with Dialyzer warnings
- ❌ Cannot release with failing benchmarks

## Troubleshooting

### Server Won't Start

```bash
# Check if port is in use
lsof -i :9091

# Kill existing server
pkill -f quality_gate_server

# Restart
./start-dashboard.sh
```

### ASCII Display Issues

```bash
# Use Python version (more reliable)
./gate-status-ascii.py --json-file=gates.json

# Or compact mode
./gate-status-ascii.py --compact --json-file=gates.json

# Disable colors for piping
./gate-status-ascii.py --no-color --json-file=gates.json
```

### JSON Generation Errors

```bash
# Run gates with error output
./gate-status-json.sh 2>errors.log | ./gate-status-ascii.py

# Check specific gate
./gate-status-json.sh --gate=1

# Validate JSON
./gate-status-json.sh 2>/dev/null | python3 -m json.tool
```

## File Structure

```
tools/visualize/
├── README.md                    # This file
├── quality-gates.html           # HTML dashboard
├── quality_gate_server.erl      # Erlang WebSocket server
├── gate-status-json.sh          # JSON generator
├── gate-status-ascii.sh         # Bash ASCII viewer
├── gate-status-ascii.py         # Python ASCII viewer (recommended)
├── json-parse.py                # Fallback JSON parser
├── start-dashboard.sh           # Server launcher
└── example-usage.sh             # Usage examples
```

## Dependencies

### Required
- Erlang/OTP 25+
- rebar3
- Python 3 (for ASCII viewer)

### Optional
- jq (for bash ASCII script)
- curl (for API testing)

## Performance

- Gate execution: ~10-15 seconds (full suite)
- WebSocket latency: <10ms
- JSON generation: <100ms
- Dashboard load: <500ms
- Browser memory: ~15MB

## Documentation

- Full documentation: `../../docs/visualization/QUALITY_GATES.md`
- TCPS system: `../../docs/tcps/TCPS.md`
- OTP patterns: `../../docs/otp-patterns.md`

## License

Apache 2.0 - See project LICENSE file

## Support

- Issues: https://github.com/banyan-platform/erlmcp/issues
- Documentation: https://github.com/banyan-platform/erlmcp/docs
