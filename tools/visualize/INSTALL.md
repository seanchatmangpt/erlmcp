# Quality Gates Visualization - Installation Guide

Quick setup guide for the erlmcp quality gates visualization system.

## Prerequisites

**Required:**
- Erlang/OTP 25+
- rebar3
- Python 3.6+

**Optional:**
- jq (for bash ASCII script - not needed if using Python version)

## Installation

### 1. Verify Prerequisites

```bash
# Check Erlang
erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:fwrite(Version), halt().'

# Check Python
python3 --version

# Check rebar3
rebar3 version
```

### 2. Compile Server Module

```bash
cd /Users/sac/erlmcp
TERM=dumb rebar3 compile
```

Expected output:
```
===> Compiling erlmcp_core
===> Compiling erlmcp_transports
===> Compiling erlmcp_observability
```

### 3. Test JSON Generator

```bash
cd tools/visualize
./gate-status-json.sh --gate=1 2>/dev/null
```

Expected output:
```json
{
  "gates": [
    {"id": 1, "name": "Compilation", "status": "pass", ...}
  ],
  "overall": "pass",
  ...
}
```

### 4. Test ASCII Visualization

```bash
# Python version (recommended)
./gate-status-json.sh --gate=1 2>/dev/null | ./gate-status-ascii.py --compact
```

Expected output:
```
🏭 erlmcp Quality Gates
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ 1. Compilation              0 errors
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Overall: ✅ PASS
```

### 5. Test Web Dashboard (Optional)

```bash
# Compile and start server
cd /Users/sac/erlmcp
rebar3 shell

# In Erlang shell:
application:ensure_all_started(cowboy).
quality_gate_server:start().
```

Open browser to: `http://localhost:9091/`

## Quick Test

**Run full test suite:**

```bash
cd tools/visualize
./example-usage.sh
```

This will:
1. Run all 8 quality gates
2. Display results in terminal
3. Show JSON output
4. Demonstrate compact mode
5. Show CI/CD integration examples

## Troubleshooting

### Python Not Found

```bash
# macOS with Homebrew
brew install python3

# Ubuntu/Debian
sudo apt-get install python3

# Verify
python3 --version
```

### Compilation Errors

```bash
# Clean and rebuild
cd /Users/sac/erlmcp
rebar3 clean
TERM=dumb rebar3 compile

# Check for missing dependencies
rebar3 tree
```

### Port 9091 Already in Use

```bash
# Find process using port
lsof -i :9091

# Kill process
kill -9 <PID>

# Or use different port
./start-dashboard.sh --port=8080
```

### Permission Denied

```bash
# Make scripts executable
cd tools/visualize
chmod +x *.sh *.py
```

### No Color Output

```bash
# Check terminal
echo $TERM

# Force colors
export TERM=xterm-256color

# Or disable colors
./gate-status-ascii.py --no-color --json-file=gates.json
```

## Verification Checklist

- [ ] Erlang/OTP 25+ installed
- [ ] Python 3.6+ installed
- [ ] rebar3 working
- [ ] `rebar3 compile` succeeds
- [ ] JSON script generates valid output
- [ ] ASCII visualization displays correctly
- [ ] Web dashboard accessible (optional)

## Next Steps

1. **Run gates:** `./gate-status-json.sh`
2. **View results:** `./gate-status-ascii.py --json-file=gates.json`
3. **Start dashboard:** `./start-dashboard.sh`
4. **Read docs:** `docs/visualization/QUALITY_GATES.md`

## Support

- README: `tools/visualize/README.md`
- Documentation: `docs/visualization/QUALITY_GATES.md`
- Examples: `./example-usage.sh`
- Issues: https://github.com/banyan-platform/erlmcp/issues
