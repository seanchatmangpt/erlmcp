# Quality Dashboard Quick Start

## Installation

All tools are already installed in `/Users/sac/erlmcp/tools/`.

### Make Scripts Executable

```bash
chmod +x tools/quality-dashboard.sh
chmod +x tools/quality-gate-json.sh
chmod +x tools/quality-watch.sh
```

## Basic Usage

### 1. Run Dashboard Once

```bash
./tools/quality-dashboard.sh
```

**Output:**
```
═══════════════════════════════════════════════════════════════
  ERLMCP QUALITY DASHBOARD
═══════════════════════════════════════════════════════════════
Generated: 2026-01-28 14:30:00
Project: /Users/sac/erlmcp

▶ Compilation Status
───────────────────────────────────────────────────────────────
  ✅ PASS - Compiled 42 modules

▶ Test Status
───────────────────────────────────────────────────────────────
  ✅ PASS - All 156 tests passed (100%)

▶ Code Coverage
───────────────────────────────────────────────────────────────
  ✅ PASS - Coverage: 84% (≥80% required)

═══════════════════════════════════════════════════════════════
  OVERALL STATUS
═══════════════════════════════════════════────════════════════
✅ ALL QUALITY GATES PASSED
Project is ready for deployment
```

### 2. Generate JSON Report

```bash
./tools/quality-gate-json.sh > quality-report.json
```

**Check status:**
```bash
jq '.overall_status' quality-report.json
# Output: "pass"
```

### 3. Continuous Monitoring

```bash
./tools/quality-watch.sh
```

**Features:**
- Updates every 30 seconds
- Beeps on failures
- Desktop notifications
- Press Ctrl+C to stop

## Common Commands

```bash
# Run dashboard
./tools/quality-dashboard.sh

# Run with custom project root
PROJECT_ROOT=/path/to/project ./tools/quality-dashboard.sh

# Generate JSON for CI/CD
./tools/quality-gate-json.sh | jq .

# Watch with 60-second refresh
./tools/quality-watch.sh --interval 60

# Watch without sound/notifications
./tools/quality-watch.sh --no-sound --no-notify
```

## Quality Gates

| Gate | Command | Pass Criteria |
|------|---------|---------------|
| Compilation | `rebar3 compile` | 0 errors |
| Tests | `rebar3 eunit` | 100% pass rate |
| Coverage | `rebar3 cover` | ≥80% |
| Dialyzer | `rebar3 dialyzer` | 0 warnings |
| Xref | `rebar3 xref` | 0 undefined calls |
| Benchmarks | `bench/results/*.json` | <10% regression |

## Exit Codes

- `0` - All gates passed
- `1` - One or more gates failed

## Integration

### Pre-commit Hook

```bash
cat > .git/hooks/pre-commit << 'HOOK'
#!/bin/bash
./tools/quality-dashboard.sh || exit 1
HOOK

chmod +x .git/hooks/pre-commit
```

### Makefile

```makefile
.PHONY: quality

quality:
	@./tools/quality-dashboard.sh
```

### GitHub Actions

```yaml
- name: Quality Gates
  run: ./tools/quality-dashboard.sh
```

## Troubleshooting

### Dashboard Shows "No coverage data"

```bash
# Generate coverage first
rebar3 eunit
rebar3 cover
```

### Watch Notifications Not Working

**macOS:**
- System Preferences → Notifications → Terminal → Allow

**Linux:**
```bash
sudo apt-get install libnotify-bin
```

### jq not installed

```bash
# macOS
brew install jq

# Ubuntu/Debian
sudo apt-get install jq

# Fedora
sudo dnf install jq
```

## Next Steps

1. Read comprehensive guide: [DASHBOARD.md](DASHBOARD.md)
2. Set up CI/CD integration: [README.md](README.md)
3. Customize thresholds: Edit scripts directly

## Support

- Documentation: `docs/quality-gates/`
- Examples: `.github/workflows/`
- Issues: GitHub Issues

