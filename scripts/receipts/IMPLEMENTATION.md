# ERLMCP v3 Receipt System - Implementation Summary

## Overview

The cryptographically-verifiable receipt generation system provides **immutable audit trails** for all Docker-based operations in erlmcp v3. This follows the **TCPS Receipt Chain** pattern - every operation generates a tamper-evident receipt that can be verified later.

## Files Created

| File | Purpose |
|------|---------|
| `scripts/receipts/generate.sh` | Main receipt generation script |
| `scripts/receipts/verify.sh` | Receipt verification script |
| `scripts/receipts/docker-wrapper.sh` | Docker-only execution wrapper with automatic receipt generation |
| `scripts/receipts/quick-test.sh` | Quick integration test |
| `scripts/receipts/test-receipt-system.sh` | Full validation test suite |
| `scripts/receipts/README.md` | User documentation |

## Cryptographic Features

### SHA-256 Hashing
- All receipts have SHA-256 checksums
- stdout/stderr outputs are hashed
- Receipt contains embedded hash of all critical data

### GPG Signing (Optional)
- Set `GPG_KEY_ID` environment variable
- Supports ed25519, RSA, ECDSA
- Detached ASCII-armored signatures (`.asc` files)

### Receipt Hash Chain
```
receipt_hash = SHA256(git_sha + image_digest + command + exit_code + stdout_hash + stderr_hash)
```

## Receipt Structure

```json
{
  "receipt_version": "1.0.0",
  "receipt_id": "R-<service>-<timestamp>-<random>",
  "generated_at": "ISO-8601-UTC",
  "metadata": {
    "hostname": "...",
    "username": "...",
    "working_directory": "...",
    "receipt_hash": "sha256:..."
  },
  "source": {
    "git_sha": "40-char-SHA",
    "git_branch": "...",
    "git_commit_message": "...",
    "git_author": "..."
  },
  "container": {
    "service": "erlmcp-build|erlmcp-unit|erlmcp-ct|...",
    "image_digest": "sha256:...",
    "image_size": "..."
  },
  "execution": {
    "command": "...",
    "exit_code": 0,
    "success": true|false
  },
  "outputs": {
    "stdout": { "hash": "sha256:...", "lines": N },
    "stderr": { "hash": "sha256:...", "lines": N }
  },
  "signature": {
    "algorithm": "ed25519",
    "key_id": "...",
    "status": "signed|unsigned"
  },
  "constitution": {
    "docker_only": true,
    "host_execution": "forbidden"
  }
}
```

## Usage Examples

### Basic Receipt Generation
```bash
# Using predefined gates
./scripts/receipts/docker-wrapper.sh compile
./scripts/receipts/docker-wrapper.sh unit
./scripts/receipts/docker-wrapper.sh ct

# Custom commands
./scripts/receipts/generate.sh generate erlmcp-build "rebar3 compile"
```

### Verification
```bash
# Verify all receipts
./scripts/receipts/verify.sh

# Verify specific receipt
./scripts/receipts/verify.sh R-erlmcp-build-1738473600-abc123

# Strict mode (fail on any issue)
./scripts/receipts/verify.sh --strict
```

### Listing and Inspection
```bash
# List all receipts
./scripts/receipts/generate.sh list

# Show receipt details
./scripts/receipts/generate.sh show R-erlmcp-build-1738473600-abc123

# Chain report
./scripts/receipts/generate.sh chain
```

## Directory Structure

```
.erlmcp/receipts/
├── pending/          # Newly generated receipts
├── verified/         # Cryptographically verified receipts
├── archive/          # Old receipts (30+ days)
├── quality-gates/    # Quality gate receipts
├── R-*.json         # Receipt files
├── R-*.json.sha256  # SHA-256 checksums
└── R-*.json.asc     # GPG signatures (if signed)
```

## Quality Gates

| Gate | Service | Command |
|------|---------|---------|
| `compile` | erlmcp-build | rebar3 compile |
| `unit` | erlmcp-unit | rebar3 eunit |
| `ct` | erlmcp-ct | rebar3 ct |
| `check` | erlmcp-check | dialyzer + xref |
| `dialyzer` | erlmcp-build | rebar3 dialyzer |
| `xref` | erlmcp-build | rebar3 xref |
| `coverage` | erlmcp-check | rebar3 cover |
| `bench` | erlmcp-bench | rebar3 machi |

## Docker-Only Constitution

**HOST EXECUTION IS FORBIDDEN.**

The `docker-wrapper.sh` enforces this by detecting forbidden tokens:
- `rebar3`
- `erl`
- `ct_run`
- `erlc`
- `epmd`
- `dialyzer`
- `typer`
- `make`

All commands MUST execute via Docker containers.

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `VERSION` | ERLMCP version | 3.0.0 |
| `GPG_KEY_ID` | GPG signing key | (none) |
| `SIGN_ALGO` | Signature algorithm | ed25519 |
| `DOCKER_COMPOSE_FILE` | Compose file | docker-compose.yml |
| `COMPOSE_PROJECT_NAME` | Project name | erlmcp |
| `DEBUG` | Debug output | 0 |

## Testing

```bash
# Quick integration test
./scripts/receipts/quick-test.sh

# Full validation suite
./scripts/receipts/test-receipt-system.sh --verbose
```

## Integration with CI/CD

### GitHub Actions
```yaml
- name: Execute quality gate
  run: ./scripts/receipts/docker-wrapper.sh compile

- name: Verify receipts
  run: ./scripts/receipts/verify.sh --strict

- name: Upload receipts
  uses: actions/upload-artifact@v3
  with:
    name: receipts
    path: .erlmcp/receipts/
```

## Security Properties

1. **Tamper-Evidence** - Any modification breaks hash chain
2. **Non-Repudiation** - GPG signatures provide proof of origin
3. **Traceability** - Git SHA links to source code
4. **Immutability** - Append-only storage pattern
5. **Verification** - Multi-level cryptographic checks

## Verification Levels

1. **Checksum** - SHA-256 of receipt file
2. **Signature** - GPG verification (if signed)
3. **Hash Chain** - Embedded hash matches computed hash
4. **Outputs** - stdout/stderr hashes match files
5. **Constitution** - Docker-only flag validated

## Operational Notes

- Receipts accumulate over time - use `archive` command to clean up old receipts
- GPG signing is optional but recommended for production
- Verification happens automatically during CI/CD
- Receipt IDs follow format: `R-<service>-<timestamp>-<random>`
