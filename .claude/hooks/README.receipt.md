# Receipt Hook Documentation

## Overview

The receipt hook (`receipt.sh`) is a SessionEnd event handler that generates audit trail receipts for each erlmcp cloud session.

**Specification**: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-005
**Technical Reference**: CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md (lines 191-225)

## Purpose

Generate JSON receipts with:
- Session metadata (OTP version, erlmcp version, build hash)
- Quality gate results (compile, eunit, ct, coverage)
- Cost estimates based on session duration
- Audit trail for compliance and billing

## Usage

### Automatic (SessionEnd Event)

The hook runs automatically at the end of each cloud session via Claude Code Web's SessionEnd event.

**Configuration** (in `.claude/settings.json`):
```json
{
  "hooks": {
    "SessionEnd": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "./.claude/hooks/receipt.sh",
            "timeout": 30
          }
        ]
      }
    ]
  }
}
```

### Manual

```bash
# Run from project root
./.claude/hooks/receipt.sh

# Or with custom session ID
SESSION_ID="my-session-123" ./.claude/hooks/receipt.sh
```

## Output

### Receipt File

**Location**: `.erlmcp/receipts/<timestamp>.json`

**Example**:
```json
{
  "session_id": "session_015jLVUqHSQc86isYfzL4Byp",
  "timestamp": "2026-02-01T12:34:56+00:00",
  "otp_version": "28",
  "erlmcp_version": "2.1.0",
  "build_hash": "7688dadecded852aa4a0b805cf1ddffba158eb7e",
  "quality_gates": {
    "compile": "pass",
    "eunit": "pass",
    "ct": "pass",
    "coverage": "pass"
  },
  "cost_estimate": "$0.0083",
  "time_seconds": 300
}
```

### Transcript Archive

If `TRANSCRIPT_PATH` environment variable is set, the session transcript is archived to:

**Location**: `.erlmcp/transcripts/session_<session_id>_<timestamp>.log`

## Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `CLAUDE_SESSION_ID` or `SESSION_ID` | No | "unknown" | Unique session identifier |
| `TRANSCRIPT_PATH` | No | None | Path to session transcript (auto-archived if set) |

## Quality Gate Detection

The hook parses build and test logs to determine quality gate status:

### Compile Gate

**Source**: `.erlmcp/build.log`

**Pass**: Contains "Compiling" but no "error"
**Fail**: Contains "Compiling" and "error"
**Unknown**: Log doesn't exist or no compilation detected

### EUnit Gate

**Source**: `.erlmcp/test.log`

**Pass**: Contains "All N tests passed" or "Test passed"
**Fail**: Contains "Failed: N" where N > 0
**Unknown**: Log doesn't exist or no EUnit output

### CT Gate

**Source**: `.erlmcp/test.log`

**Pass**: Contains "All tests successful"
**Fail**: Contains "FAILED"
**Unknown**: Log doesn't exist or no CT output

### Coverage Gate

**Source**: `.erlmcp/test.log` or `_build/test/cover/index.html`

**Pass**: Coverage ≥ 80%
**Fail**: Coverage < 80%
**Unknown**: No coverage data found

## Cost Calculation

**Model**: $0.10/hour compute = $0.00002778/second

**Calculation**:
1. Determine session start time (from build log timestamp or default 300s)
2. Calculate duration: `end_time - start_time`
3. Compute cost: `duration * $0.00002778`
4. Format: `$0.XXXX` (4 decimal places)

**Range**: Typical sessions cost $0.0001 - $0.5000

## JSON Schema

**Location**: `.claude/hooks/receipt.schema.json`

**Validation**:
```bash
# With ajv-cli (if installed)
ajv validate -s .claude/hooks/receipt.schema.json -d .erlmcp/receipts/1234567890.json

# With jq (basic)
jq empty .erlmcp/receipts/1234567890.json
```

## Testing

### Test Suite

**Location**: `test/receipt_hook_tests.sh`

**Run**:
```bash
bash test/receipt_hook_tests.sh
```

**Coverage**: 10 test cases
1. Script exists and is executable
2. Generates valid JSON
3. Contains all required fields
4. Timestamp format is ISO-8601
5. Cost estimate is reasonable
6. Metadata accuracy
7. Quality gates (pass scenario)
8. Quality gates (fail scenario)
9. Transcript archival
10. Idempotent execution

### Manual Testing

```bash
# Create mock logs
cat > .erlmcp/build.log <<EOF
Compiling erlmcp_core
Build successful
EOF

cat > .erlmcp/test.log <<EOF
All 42 tests passed
Coverage: 85%
EOF

# Run hook
SESSION_ID="test-123" ./.claude/hooks/receipt.sh

# View receipt
cat .erlmcp/receipts/*.json | jq .
```

## Troubleshooting

### Receipt not generated

**Check**:
1. Script is executable: `ls -l .claude/hooks/receipt.sh`
2. Project root detection: Ensure `apps/erlmcp_core/src/erlmcp_core.app.src` exists
3. Directory permissions: `.erlmcp/receipts/` should be writable

### Invalid JSON

**Fix**:
- Install `jq` for validation: `apt-get install jq` (Ubuntu) or `brew install jq` (macOS)
- Check script output for errors: `./.claude/hooks/receipt.sh 2>&1 | tail -20`

### Quality gates always "unknown"

**Cause**: Build/test logs not found or don't match expected patterns

**Fix**:
1. Ensure PostToolUse hook is logging to `.erlmcp/build.log` and `.erlmcp/test.log`
2. Check log format matches expected patterns (see "Quality Gate Detection" section)
3. Run `make check` to generate logs with expected format

### Cost estimate zero or too high

**Cause**: Session duration calculation failed

**Fix**:
1. Ensure `.erlmcp/build.log` exists and has valid timestamp
2. Check system time: `date +%s`
3. Manually set duration: Edit script or use default (300s)

## Integration

### With CI/CD

```yaml
# .github/workflows/ci.yml
- name: Generate receipt
  run: |
    SESSION_ID="${{ github.run_id }}" ./.claude/hooks/receipt.sh
    cat .erlmcp/receipts/*.json
```

### With Makefile

```make
# Add to Makefile
.PHONY: receipt
receipt:
	@./.claude/hooks/receipt.sh
	@echo "Latest receipt:"
	@cat .erlmcp/receipts/*.json | jq .

.PHONY: receipts-list
receipts-list:
	@echo "Recent session receipts:"
	@find .erlmcp/receipts -name "*.json" | sort -r | head -5 | xargs -I{} jq -r '"\(.timestamp) | Session: \(.session_id) | Cost: \(.cost_estimate) | Gates: \(.quality_gates.compile)/\(.quality_gates.eunit)/\(.quality_gates.ct)/\(.quality_gates.coverage)"' {}
```

## Security

### Sensitive Data

The receipt hook **does not** capture:
- Source code
- Environment variables (except `SESSION_ID` and `TRANSCRIPT_PATH`)
- Credentials or secrets
- User input

### Transcript Privacy

Transcripts are only archived if `TRANSCRIPT_PATH` is explicitly set. Archive files are:
- Stored locally in `.erlmcp/transcripts/` (gitignored by default)
- Not uploaded or transmitted
- Readable only by session owner

## Maintenance

### Log Rotation

Receipts accumulate over time. Recommended cleanup:

```bash
# Keep last 30 days
find .erlmcp/receipts -name "*.json" -mtime +30 -delete

# Keep last 100 receipts
ls -t .erlmcp/receipts/*.json | tail -n +101 | xargs rm -f
```

### Schema Updates

When updating the receipt format:

1. Update `receipt.sh` to generate new fields
2. Update `receipt.schema.json` to validate new structure
3. Update tests in `test/receipt_hook_tests.sh`
4. Bump version in commit message
5. Document breaking changes in CHANGELOG.md

## References

- Work Order: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-005 (lines 104-142)
- Governance System: CLAUDE_CODE_WEB_GOVERNANCE_SYSTEM.md (lines 191-225)
- JSON Schema Spec: https://json-schema.org/
- ISO-8601 Format: https://en.wikipedia.org/wiki/ISO_8601

## License

Apache-2.0 (same as erlmcp)
