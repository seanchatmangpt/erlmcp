---
name: tcps-receipt
description: Receipt Chain - Immutable audit trail with SHA-256 hash
category: tcps
invokes_agent: erlang-github-ops
uses_rules: rules-devops + rules-post-deployment-monitoring-mode
japanese_term: レシート (Reshīto - Receipt)
manufacturing_stage: "Stage 8: Release"
consolidates: [sparc/deploy, sparc/devops, sparc/post-deployment-monitoring-mode]
---

# Command: /tcps-receipt

## Purpose
**Japanese**: レシート (Reshīto - Receipt)
**Manufacturing Stage**: Stage 8 - Release with immutable evidence
**TCPS Pillar**: Receipt Chain (auditable evidence at every stage)

Generate immutable receipts with SHA-256 hash chains. Every stage emits auditable evidence. If factory cannot prove unit passed inspection, it does not ship.

## Usage
```bash
/tcps-receipt [create|verify|export]
```

## Examples

### Create Receipt
```bash
/tcps-receipt create team v1.4.0
```

Generates:
- `priv/receipts/team/1.4.0/20260127-103000.receipt.json`
- Evidence bundle: `dist/evidence/v1.4.0/team/`
- Hash chain: SHA-256 linking all stages

### Verify Receipt Chain
```bash
/tcps-receipt verify team-20260127-001
```

Validates:
- ✓ All hashes match
- ✓ No tampering detected
- ✓ Complete chain from pull → release

### Export Receipt
```bash
/tcps-receipt export team v1.4.0 json
```

Formats: JSON (Salesforce/Oracle), CSV (Excel), TSV (BigQuery)

## Receipt Structure

```json
{
  "receipt_id": "release-20260127-001",
  "timestamp": "2026-01-27T15:00:00Z",
  "stage": "release",
  "plan": "team",
  "version": "v1.4.0",
  "sku": "erlmcp-team-v1.4.0.tar.gz",
  "evidence_bundle": "dist/evidence/v1.4.0/team/",
  "hash_chain": [
    "pull:sha256:abc123...",
    "heijunka:sha256:def456...",
    "build:sha256:ghi789...",
    "jidoka:sha256:jkl012...",
    "release:sha256:mno345..."
  ],
  "certified": true,
  "next_stage": "marketplace",
  "hash": "sha256:mno345..."
}
```

## See Also
- Related: `/sku-certify` - Certify SKU with evidence
- Related: `/poka-yoke-validate` - Validate plans
- Docs: `docs/tcps/RECEIPTS_SPEC.md`

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /sparc deploy, /sparc devops, /sparc post-deployment-monitoring-mode
**Manufacturing Stage**: Stage 8 of 8
