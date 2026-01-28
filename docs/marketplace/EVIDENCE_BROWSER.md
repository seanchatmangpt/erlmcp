# Evidence Bundle Browser

## Overview

The Evidence Browser enables transparent access to all supporting documentation for erlmcp pricing tiers. Every plan includes comprehensive evidence of performance, security, and compliance.

## Evidence Bundle Structure

Evidence bundles are organized by version and tier:

```
dist/evidence/
└── v1.4.0/
    ├── team/
    │   ├── sbom.json
    │   ├── provenance.json
    │   ├── chaos-report.md
    │   └── benchmark-report.md
    ├── enterprise/
    │   ├── sbom.json
    │   ├── provenance.json
    │   ├── chaos-report.md
    │   ├── benchmark-report.md
    │   └── audit-schema.json
    └── gov/
        ├── sbom.json
        ├── provenance.json
        ├── chaos-report.md
        ├── benchmark-report.md
        ├── audit-schema.json
        ├── fips-140-2-cert.txt
        └── compliance-report.md
```

## Evidence Types

### 1. Software Bill of Materials (SBOM)

**File**: `sbom.json`

**Purpose**: Complete inventory of all dependencies

**Content**:
- Package name and version
- License information
- Vulnerability status (CVE references)
- Source repository
- Dependency tree

**Format**: SPDX JSON format

**Example**:
```json
{
  "SPDXID": "SPDXRef-erlmcp-1.4.0",
  "name": "erlmcp-1.4.0",
  "version": "1.4.0",
  "packages": [
    {
      "name": "jsx",
      "version": "3.0.0",
      "SPDXID": "SPDXRef-jsx-3.0.0",
      "licenseConcluded": "BSD-3-Clause",
      "externalRefs": [...]
    }
  ]
}
```

**Use Case**: Compliance audits, dependency tracking, supply chain analysis

### 2. Provenance

**File**: `provenance.json`

**Purpose**: Build and release chain of custody

**Content**:
- Build timestamp
- Git commit hash and tag
- Build environment details
- Signed artifacts
- Release metadata

**Format**: SLSA Provenance format

**Example**:
```json
{
  "predicateType": "https://slsa.dev/provenance/v0.2",
  "predicate": {
    "buildType": "https://erlang.org/rebar3",
    "builder": {
      "id": "https://github.com/erlmcp/erlmcp/actions"
    },
    "sourceUri": "https://github.com/erlmcp/erlmcp@v1.4.0",
    "invocation": {
      "configSource": {
        "uri": "https://github.com/erlmcp/erlmcp",
        "digest": {
          "sha256": "abc123..."
        }
      }
    }
  }
}
```

**Use Case**: Supply chain security, release verification, build reproducibility

### 3. Chaos Report

**File**: `chaos-report.md`

**Purpose**: Failure mode testing and resilience validation

**Content**:
- Chaos scenarios tested
- Failure types and causes
- Recovery behavior
- Time-to-recovery metrics
- Circuit breaker activation logs

**Format**: Markdown with tables and metrics

**Example Scenarios**:
1. **Connection Failures**: Random connection drops
2. **Timeouts**: Delayed responses (100ms, 500ms, 5s)
3. **Resource Exhaustion**: Memory pressure, CPU saturation
4. **Network Partitions**: Split-brain scenarios
5. **Rate Limiting**: Request throttling
6. **Message Corruption**: Invalid message handling
7. **Cascading Failures**: Multi-component outages

**Metrics Collected**:
- Time to detect failure
- Time to recover
- Requests lost
- Requests deferred
- Error rates

**Use Case**: Reliability assessment, failure scenario understanding, SLA validation

### 4. Benchmark Report

**File**: `benchmark-report.md`

**Purpose**: Performance characterization and capacity planning

**Content**:
- Hardware configuration
- Test methodology
- Throughput results (req/s)
- Latency distribution (p50, p99, p99.9)
- Memory usage
- CPU utilization
- Connection scalability
- Message size impact

**Format**: Markdown with graphs and tables

**Example Benchmark Sections**:

```markdown
## Throughput

| Load | Team | Enterprise | Gov |
|------|------|-----------|-----|
| Sustained | 450 req/s | 1500 req/s | 900 req/s |
| Peak | 550 req/s | 2000 req/s | 1100 req/s |

## Latency

| Percentile | Team | Enterprise | Gov |
|-----------|------|-----------|-----|
| P50 | 50ms | 20ms | 30ms |
| P99 | 250ms | 100ms | 150ms |
| P99.9 | 350ms | 200ms | 250ms |

## Resource Usage

| Metric | Team | Enterprise | Gov |
|--------|------|-----------|-----|
| Memory (256 conn) | 250MB | 800MB | 400MB |
| CPU (100 req/s) | 15% | 20% | 18% |
```

**Use Case**: Capacity planning, SLA validation, performance optimization

### 5. Audit Schema (Enterprise+)

**File**: `audit-schema.json`

**Purpose**: Audit logging event structure and format

**Content**:
- Event types and codes
- Field definitions
- Metadata requirements
- Timestamp format
- User identification
- Action categorization

**Format**: JSON Schema

**Example Events**:
```json
{
  "event_type": "OPERATION_EXECUTED",
  "event_code": "OP_001",
  "timestamp": "2025-01-27T12:34:56Z",
  "user_id": "user@example.com",
  "action": "tool_call",
  "resource_uri": "mcp://tool/calculate",
  "status": "success",
  "duration_ms": 125,
  "request_id": "req-uuid",
  "metadata": {}
}
```

**Use Case**: Audit configuration, log analysis, compliance verification

### 6. FIPS 140-2 Certification (Government)

**File**: `fips-140-2-cert.txt`

**Purpose**: FIPS compliance certification document

**Content**:
- Certification level (1, 2, 3, 4)
- Cryptographic modules certified
- Validation date and validity period
- Test reports
- Implementation guidance

**Format**: Text/PDF

**Use Case**: Government compliance, FIPS requirement validation

### 7. Compliance Report (Government)

**File**: `compliance-report.md`

**Purpose**: Comprehensive compliance audit results

**Content**:
- FIPS 140-2 compliance status
- Encryption strength verification
- Audit logging completeness
- Key rotation procedures
- Incident response procedures
- Security control assessment

**Format**: Markdown with detailed sections

**Use Case**: Government/compliance audits, security assessments

## Accessing Evidence

### Via Pricing Portal

1. Navigate to erlmcp pricing portal
2. Select plan (Team/Enterprise/Government)
3. Click "Evidence" button on plan card
4. Evidence files listed for that plan

### Via Direct Links

Evidence files accessible at standard URLs:

```
https://erlmcp.dev/dist/evidence/v1.4.0/team/sbom.json
https://erlmcp.dev/dist/evidence/v1.4.0/team/provenance.json
https://erlmcp.dev/dist/evidence/v1.4.0/team/chaos-report.md
https://erlmcp.dev/dist/evidence/v1.4.0/team/benchmark-report.md
```

### Via GitHub Releases

All evidence included in GitHub release attachments:

```
erlmcp-1.4.0-evidence-team.tar.gz
erlmcp-1.4.0-evidence-enterprise.tar.gz
erlmcp-1.4.0-evidence-gov.tar.gz
```

### Programmatically

Access evidence index:

```bash
# Get all evidence metadata
curl https://erlmcp.dev/dist/marketplace/evidence-index.json

# Download specific evidence
curl https://erlmcp.dev/dist/evidence/v1.4.0/team/sbom.json
```

## Evidence Verification

### Checksum Verification

All evidence files include checksums:

```
sbom.json.sha256: abc123def456...
provenance.json.sha256: def456abc123...
```

Verify integrity:
```bash
sha256sum -c sbom.json.sha256
```

### Signature Verification

All evidence includes GPG signatures:

```
sbom.json.asc  (GPG signature)
provenance.json.asc
```

Verify authenticity:
```bash
gpg --verify sbom.json.asc sbom.json
```

### Certificate Chain Verification

Provenance includes certificate chain for build verification:

```bash
# Verify build artifacts
curl https://erlmcp.dev/dist/evidence/v1.4.0/team/provenance.json | \
  jq '.predicate.materials'
```

## Understanding Evidence

### For Developers

**Most Relevant**:
- Benchmark Report (capacity planning)
- Chaos Report (reliability assessment)
- SBOM (dependency analysis)

**Questions Answered**:
- "Will this handle 1000 req/s?" → Benchmark Report
- "What happens when connections fail?" → Chaos Report
- "What dependencies are included?" → SBOM

### For Operations

**Most Relevant**:
- Benchmark Report (resource sizing)
- Audit Schema (log configuration)
- Compliance Report (government deployments)

**Questions Answered**:
- "How much memory for 500 connections?" → Benchmark
- "What audit events are logged?" → Audit Schema
- "Is this FIPS compliant?" → FIPS Cert + Compliance Report

### For Security Teams

**Most Relevant**:
- SBOM (vulnerability scanning)
- Provenance (supply chain verification)
- Compliance Report (security controls)
- FIPS 140-2 Cert (encryption strength)

**Questions Answered**:
- "Any known vulnerabilities?" → SBOM + scan databases
- "How was this built?" → Provenance
- "Is encryption validated?" → FIPS Cert
- "What are security controls?" → Compliance Report

### For Compliance Officers

**Most Relevant**:
- FIPS 140-2 Certification
- Compliance Report
- Audit Schema
- Chaos Report (SLA validation)

**Questions Answered**:
- "Is this FIPS compliant?" → Yes, see FIPS Cert
- "What controls are in place?" → See Compliance Report
- "How are events audited?" → See Audit Schema
- "What's the SLA reliability?" → See Chaos Report

## Evidence Auto-Generation

All evidence is **deterministically generated** from plan specifications:

### Generation Process

1. Load plan specs (`plans/team.plan.json`, etc.)
2. Extract plan parameters (throughput, latency targets, etc.)
3. Generate evidence artifacts:
   - SBOM from dependency manifest
   - Provenance from build system
   - Run chaos tests against specifications
   - Run benchmarks against specifications
   - Validate audit schema against plan requirements
4. Sign all artifacts with release key
5. Publish to dist/evidence/ directory

### Determinism Guarantee

```bash
# Running generation twice produces identical evidence
make generate-marketplace
md5sum dist/evidence/v1.4.0/team/sbom.json  # hash1
make clean && make generate-marketplace
md5sum dist/evidence/v1.4.0/team/sbom.json  # hash1 (identical)
```

No manual edits - all evidence is reproducible from specs.

## Evidence Retention Policy

### Team Tier
- Retention: 90 days
- Availability: Latest 3 releases

### Enterprise Tier
- Retention: 1 year
- Availability: Latest 10 releases

### Government Tier
- Retention: 7 years
- Availability: All releases (permanent archive)

## Accessing Historical Evidence

Evidence for past versions accessible via version-specific URLs:

```
v1.3.0 evidence: https://erlmcp.dev/dist/evidence/v1.3.0/
v1.2.0 evidence: https://erlmcp.dev/dist/evidence/v1.2.0/
v1.1.0 evidence: https://erlmcp.dev/dist/evidence/v1.1.0/
```

## Evidence API

### List All Evidence

```bash
curl https://erlmcp.dev/dist/evidence/index.json
```

Response:
```json
{
  "versions": [
    {
      "version": "1.4.0",
      "released_at": "2025-01-27T00:00:00Z",
      "tiers": ["team", "enterprise", "gov"],
      "evidence": {
        "team": ["sbom.json", "provenance.json", ...],
        "enterprise": [...],
        "gov": [...]
      }
    }
  ]
}
```

### Get Tier Evidence Summary

```bash
curl https://erlmcp.dev/dist/evidence/v1.4.0/team/manifest.json
```

Response:
```json
{
  "tier": "team",
  "version": "1.4.0",
  "generated_at": "2025-01-27T12:00:00Z",
  "files": {
    "sbom.json": {"size": 45230, "sha256": "abc..."},
    "provenance.json": {"size": 12340, "sha256": "def..."},
    ...
  }
}
```

## Troubleshooting

### Cannot Find Evidence File

1. Check version number: `https://erlmcp.dev/dist/evidence/v1.4.0/`
2. Check tier name: `team`, `enterprise`, or `gov`
3. Check file name: `sbom.json`, `provenance.json`, etc.
4. List available versions: `curl https://erlmcp.dev/dist/evidence/`

### Evidence File Corrupted

1. Download again: Evidence CDN may have served incomplete file
2. Verify checksum: `sha256sum -c file.sha256`
3. Check file size: Compare to manifest
4. Report issue: File may need regeneration

### Cannot Verify Signature

1. Import GPG key: `gpg --import erlmcp-release-key.asc`
2. Trust key: `gpg --edit-key <keyid>` then `trust`
3. Verify: `gpg --verify file.asc file`

## Best Practices

1. **Verify Checksums**: Always verify integrity of downloaded evidence
2. **Keep Copies**: Archive evidence bundles for audit trail
3. **Check Signatures**: Verify GPG signatures for authenticity
4. **Regular Audits**: Review evidence periodically (especially audit logs)
5. **Version Tracking**: Note which version evidence corresponds to
6. **Security Analysis**: Use SBOM with vulnerability scanners (Grype, Syft)

## Questions?

For evidence-related questions:
- **Technical**: support@erlmcp.dev
- **Compliance**: compliance@erlmcp.dev
- **Security**: security@erlmcp.dev
