---
name: sku-list
description: List manufactured SKUs (releases)
category: sku
japanese_term: SKU (Stock Keeping Unit)
consolidates: [github/repo, github/repo-analyze, github/github-swarm]
---

# Command: /sku-list

## Purpose
**Japanese**: SKU (Stock Keeping Unit)
List all manufactured SKUs (product releases).

## Usage
```bash
/sku-list [tier]
```

## Examples
```bash
/sku-list team
```

Output:
```
Team Tier SKUs:
- erlmcp-team-v1.3.0.tar.gz (certified ✓)
- erlmcp-team-v1.4.0.tar.gz (certified ✓)
```

---

**Command Version**: 1.0.0 (TCPS-aligned)
**Replaces**: /github repo, /github/repo-analyze
