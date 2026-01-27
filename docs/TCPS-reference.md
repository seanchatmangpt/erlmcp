# TCPS Reference

Quick-reference guide for the Toyota Code Production System. Use this document when you need canonical definitions, artifact locations, and signal mappings.

## Glossary

| Term | Meaning | Artifact |
|------|---------|---------|
| SKU | Marketplace offering manufactured by TCPS | `ontology/work_orders.ttl`, `dist/<sku>/` |
| Work Order | RDF individual describing pull request for SKU | `ontology/work_orders.ttl` |
| Template | Tera or other rendering asset used to produce code/docs | `templates/` |
| SPARQL Query | Extraction jig for ontology data | `sparql/` |
| Receipt | JSON/YAML evidence that a stage completed | `receipts/<stage>/` |
| Andon | Stop-the-line event + root cause packet | `receipts/andon/` |
| Heijunka Bucket | Scheduling bucket (reliability/security/cost/compliance) | `receipts/plan/` |
| WIP | Count of concurrently active SKUs | `receipts/plan/wip.json` |

## Stage → Command Matrix

| Stage | Commands / Scripts | Primary Outputs | Receipts |
|-------|--------------------|-----------------|----------|
| Pull | `tools/demand_ingest.sh` | Updated work orders | `receipts/pull/` |
| Plan | `tools/heijunka.py` | Schedule json | `receipts/plan/` |
| Generate | `tools/run-ggen.sh` | Rendered artifacts | `receipts/generate/` |
| Validate | `make lint`, `make test-unit`, `make test-integration` | Test logs | `receipts/build/` |
| Build | `make release` | Tarball/container | `receipts/release/` |
| Publish | `tools/publish.sh` | Marketplace payloads | `receipts/publish/` |
| Verify | `tools/smoke.sh` | Smoke output | `receipts/verify/` |
| Kaizen | `tools/andon.sh`, `tools/5whys.sh` | RCA docs | `receipts/andon/` |

## Filesystem Layout Requirements

```
ontology/
shapes/
sparql/
templates/
receipts/
  pull/
  plan/
  generate/
  build/
  release/
  publish/
  verify/
  andon/
dist/
tools/
```

## Links
- Tutorial: `docs/TCPS.md`
- How-to: `docs/TCPS-howto.md`
- Checklist: `docs/TCPS-checklist.md`
- Certification ledger: `docs/TCPS-certification.md`
- Explanation: `docs/TCPS-explanation.md`

