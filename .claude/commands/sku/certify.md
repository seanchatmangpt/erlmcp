---
name: sku-certify
description: Certify SKU with evidence bundle
category: sku
japanese_term: 認証 (Ninshō - Certification)
consolidates: [github/issue, github/issue-triage]
---

# Command: /sku-certify

## Purpose
**Japanese**: 認証 (Ninshō - Certification)
Certify SKU with complete evidence bundle (bench, chaos, conformance, refusal).

## Usage
```bash
/sku-certify <tier> <version>
```

## Examples
```bash
/sku-certify team v1.4.0
```

Generates evidence bundle in `dist/evidence/v1.4.0/team/` with `.certified` marker.

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /github issue, /github/issue-triage
