# Deterministic Build Wrapper for erlmcp

**Version:** 1.5.0
**Mission:** Eliminate rebar3 colorizer crashes and provide deterministic builds with metrology

## Problem Statement

rebar3's terminal colorizer can crash during compilation, causing:
- Build process to return exit code 0 even when builds fail
- CI pipelines to pass incorrectly when builds actually failed
- Intermittent "function clause in io_lib_format:collect_cc_new/2" errors
- Inconsistent build behavior across environments

## Solution

The deterministic build wrapper (`build.sh`) neutralizes these issues by:

1. **Disabling colorization completely**
   - `TERM=dumb` - Disables terminal capabilities
   - `NO_COLOR=1` - Standard env var to disable color
   - `REBAR_COLOR=none` - rebar3-specific color control

2. **Preserving exit codes correctly**
   - Uses `${PIPESTATUS[0]}` to capture rebar3's exit code, not tee's
   - Properly handles pipe failures
   - Returns correct exit code to make/CI

3. **Providing build metrology**
   - Generates JSON build receipts with metadata
   - Logs all output to `build.log`
   - Validates build success with metrology checks

## Quick Start

### Basic Usage

```bash
# Use wrapper directly
tools/build/build.sh rebar3 compile

# Or via Makefile
make deterministic-compile
```

### Integration with Existing Makefile

```makefile
# Include deterministic build support
include Makefile.deterministic

# Or override REBAR3 variable
REBAR3 = tools/build/build.sh rebar3
```

### CI Integration (GitHub Actions)

```yaml
- name: Build with deterministic wrapper
  run: tools/build/build.sh rebar3 compile

- name: Validate build receipt
  run: |
    LATEST=$(ls -t tools/build/receipts/*.json | head -1)
    EXIT_CODE=$(cat "$LATEST" | python3 -c "import sys, json; print(json.load(sys.stdin)['exit_code'])")
    if [ "$EXIT_CODE" != "0" ]; then
      exit 1
    fi
```

## Build Receipts

Every build generates a JSON receipt in `tools/build/receipts/`:

```json
{
  "build_id": "v1.5.0-abc1234-1735689600",
  "timestamp": "2026-01-27T12:00:00Z",
  "git_sha": "abc1234567890...",
  "git_branch": "main",
  "rebar3_version": "rebar 3.24.0 on Erlang/OTP 27",
  "erlang_version": "27",
  "targets": "compile",
  "exit_code": 0,
  "duration_s": 45,
  "metrology_validated": true,
  "environment": {
    "term": "dumb",
    "no_color": "1",
    "rebar_color": "none"
  },
  "host": {
    "hostname": "build-server",
    "os": "Linux",
    "os_version": "5.15.0",
    "arch": "x86_64"
  }
}
```

## Testing the Wrapper

Run the comprehensive test suite:

```bash
tools/build/test_wrapper.sh
```

This tests:
- ✅ Successful builds preserve exit code 0
- ✅ Failed builds preserve non-zero exit codes
- ✅ Clean targets work correctly
- ✅ Build receipts are generated

## Makefile Targets

```bash
# Deterministic builds
make deterministic-compile    # Compile with wrapper
make deterministic-test       # Test with wrapper
make deterministic-check      # Full quality check

# Wrapper management
make test-wrapper            # Test wrapper functionality
make verify-wrapper          # Verify wrapper configuration
make show-receipts           # Show build receipts
make clean-receipts          # Clean receipt directory

# CI targets
make ci-compile              # CI compile with receipt validation
make ci-test                 # CI test with receipt validation
make ci-check                # CI full quality check
make ci-gate                 # Validate all quality gates
```

## How It Works

### 1. Environment Neutralization

```bash
export TERM=dumb          # Disable terminal capabilities
export NO_COLOR=1         # Disable color globally
export REBAR_COLOR=none   # Disable rebar3 color specifically
```

### 2. Exit Code Preservation

```bash
# CRITICAL: Use PIPEFAIL to capture rebar3 exit code
set -euo pipefail
rebar3 "$@" 2>&1 | tee -a build.log
REBAR3_EXIT_CODE=${PIPESTATUS[0]}  # Capture from pipe element 0
```

### 3. Metrology Validation

```bash
# Check build output for success indicators
if grep -q -E "(Compiled |All tests passed|PASSED)" build_output; then
    METROLOGY_VALIDATED=true
fi
```

### 4. Receipt Generation

```bash
cat > receipt.json <<EOF
{
  "exit_code": $REBAR3_EXIT_CODE,
  "metrology_validated": $METROLOGY_VALIDATED,
  ...
}
EOF
```

## Benefits

| Feature | Before | After |
|---------|--------|-------|
| **Colorizer crashes** | Random failures | Eliminated |
| **Exit codes** | Sometimes wrong | Always correct |
| **CI reliability** | Intermittent | Deterministic |
| **Build logs** | Scattered | Centralized |
| **Metrology** | None | Full receipts |
| **Debugging** | Difficult | Complete logs |

## Files

- `tools/build/build.sh` - Main wrapper script
- `tools/build/test_wrapper.sh` - Test suite
- `tools/build/build.log` - Consolidated build log
- `tools/build/receipts/` - Build receipt directory
- `Makefile.deterministic` - Makefile integration
- `.github/workflows/deterministic-build.yml` - CI integration

## Troubleshooting

### Wrapper not executable

```bash
chmod +x tools/build/build.sh
chmod +x tools/build/test_wrapper.sh
```

### No receipts generated

Check that the receipts directory exists:
```bash
mkdir -p tools/build/receipts
```

### Build still fails

Check the build log for details:
```bash
tail -100 tools/build/build.log
```

### Receipt validation fails in CI

Ensure Python 3 is available:
```bash
python3 --version
```

## Version History

- **v1.5.0** (2026-01-27) - Initial deterministic build wrapper
  - Environment neutralization (TERM=dumb, NO_COLOR=1)
  - Exit code preservation (PIPEFAIL)
  - Build receipt generation
  - CI integration

## Future Enhancements

- [ ] Build cache invalidation based on git SHA
- [ ] Receipt-based build artifact tracking
- [ ] Parallel build support with receipt aggregation
- [ ] Build performance metrics (compile time, test duration)
- [ ] Receipt-based dependency tracking

## License

Same as erlmcp (Apache 2.0)
