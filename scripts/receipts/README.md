# ERLMCP v3 Receipt System

Cryptographically-verifiable receipt generation system for immutable audit trails.

## Overview

The receipt system provides **tamper-evident proof** of all operations, following the TCPS (Toyota Production System + Computer Science) **Receipt Chain** pattern. Every Docker-based operation generates a signed receipt containing:

- **Git SHA** - Source code commit fingerprint
- **Docker Image Digest** - Container fingerprint
- **Service Name** - Which container executed
- **Command** - What was executed
- **Exit Code** - Success/failure status
- **SHA-256 Hashes** - Of stdout/stderr outputs
- **Timestamp** - ISO 8601 UTC
- **GPG Signature** - Cryptographic proof (optional)

## Constitution

**DOCKER-ONLY.** Host execution is **FORBIDDEN**.

All operations MUST execute via Docker containers. Direct execution of commands like `rebar3`, `erl`, `ct_run` on the host is a **constitution violation** that invalidates receipts.

## Quick Start

### Initialize Receipt System

```bash
./scripts/receipts/generate.sh init
```

### Execute Commands with Receipts

```bash
# Using the Docker wrapper (recommended)
./scripts/receipts/docker-wrapper.sh compile
./scripts/receipts/docker-wrapper.sh unit
./scripts/receipts/docker-wrapper.sh ct

# Or use generate.sh directly
./scripts/receipts/generate.sh generate erlmcp-build "rebar3 compile"
```

### Verify Receipts

```bash
# Verify all receipts
./scripts/receipts/verify.sh

# Verify specific receipt
./scripts/receipts/verify.sh R-erlmcp-build-1738473600-abc123

# Strict mode (fail on any issue)
./scripts/receipts/verify.sh --strict
```

### List Receipts

```bash
./scripts/receipts/generate.sh list
```

### Show Receipt Details

```bash
./scripts/receipts/generate.sh show R-erlmcp-build-1738473600-abc123
```

## Directory Structure

```
.erlmcp/receipts/
├── pending/          # Newly generated receipts
├── verified/         # Cryptographically verified receipts
├── archive/          # Old receipts (30+ days)
├── R-*.json         # Receipt files
├── R-*.json.sha256  # Checksums
└── R-*.json.asc     # GPG signatures (if signed)
```

## Receipt Structure

```json
{
  "receipt_version": "1.0.0",
  "receipt_id": "R-erlmcp-build-1738473600-a1b2c3d4",
  "generated_at": "2026-02-02T12:00:00.000Z",
  "erlmcp_version": "3.0.0",
  "metadata": {
    "hostname": "build-server-01",
    "username": "ci-bot",
    "working_directory": "/Users/sac/erlmcp",
    "receipt_hash": "abc123..."
  },
  "source": {
    "git_sha": "2f494c2...",
    "git_branch": "main",
    "git_commit_message": "Add receipt system",
    "git_author": "System"
  },
  "container": {
    "service": "erlmcp-build",
    "image_digest": "sha256:abc123...",
    "image_size": "850MB"
  },
  "execution": {
    "command": "rebar3 compile",
    "exit_code": 0,
    "success": true
  },
  "outputs": {
    "stdout": {
      "hash": "sha256:...",
      "hash_algorithm": "sha256",
      "lines": 42
    },
    "stderr": {
      "hash": "sha256:...",
      "hash_algorithm": "sha256",
      "lines": 0
    }
  },
  "signature": {
    "algorithm": "ed25519",
    "key_id": "ABC123...",
    "status": "signed"
  },
  "constitution": {
    "docker_only": true,
    "host_execution": "forbidden"
  }
}
```

## Cryptographic Verification

### Checksum Verification

Each receipt has a `.sha256` file:

```bash
shasum -c .erlmcp/receipts/R-*.sha256
```

### Signature Verification

If GPG signing is enabled:

```bash
# Set up GPG key
export GPG_KEY_ID="your-key-id"

# Generate signed receipt
./scripts/receipts/generate.sh generate erlmcp-build "rebar3 compile"

# Verify signature
gpg --verify .erlmcp/receipts/R-*.json.asc
```

### Hash Chain Verification

The embedded `receipt_hash` is computed from:

```
hash(git_sha + image_digest + command + exit_code + stdout_hash + stderr_hash)
```

This creates an **immutable chain** - any tampering breaks the hash.

## Quality Gates

Predefined quality gates map to Docker services:

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

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `VERSION` | ERLMCP version | 3.0.0 |
| `GPG_KEY_ID` | GPG key for signing | (none) |
| `SIGN_ALGO` | Signature algorithm | ed25519 |
| `DOCKER_COMPOSE_FILE` | Compose file path | docker-compose.yml |
| `COMPOSE_PROJECT_NAME` | Compose project | erlmcp |
| `DEBUG` | Enable debug output | 0 |

## Testing

Run the receipt system test suite:

```bash
./scripts/receipts/test-receipt-system.sh --verbose
```

Tests validate:
- Docker availability
- Hash functions
- Receipt ID generation
- Git information collection
- JSON structure validity
- Checksum generation/verification
- Docker-only enforcement

## CI/CD Integration

### GitHub Actions Example

```yaml
- name: Generate receipt
  run: |
    ./scripts/receipts/generate.sh generate erlmcp-build "rebar3 compile"

- name: Verify receipt
  run: |
    ./scripts/receipts/verify.sh --strict

- name: Upload receipts
  uses: actions/upload-artifact@v3
  with:
    name: receipts
    path: .erlmcp/receipts/
```

### GitLab CI Example

```yaml
compile:
  script:
    - ./scripts/receipts/docker-wrapper.sh compile
  artifacts:
    paths:
      - .erlmcp/receipts/
```

## Security Considerations

1. **Immutable Storage** - Once written, receipts should never be modified
2. **Append-Only Logs** - Archive old receipts, don't delete
3. **GPG Signing** - Use in production for non-repudiation
4. **Hash Verification** - Always verify before trusting receipts
5. **Docker-Only** - Host execution breaks the security model

## Troubleshooting

### Receipt verification fails

- Check that `.sha256` file exists
- Verify no files were modified after generation
- Ensure output files (`.stdout.log`, `.stderr.log`) are present

### Docker wrapper says "forbidden token"

- You're trying to execute directly on host
- Use Docker wrapper instead: `./scripts/receipts/docker-wrapper.sh compile`

### GPG signing fails

- Ensure `GPG_KEY_ID` is set
- Verify key exists: `gpg --list-keys $GPG_KEY_ID`
- Check GPG agent is running

## License

MIT License - See project root for details.
