# TCPS Validation Checklist (Phase 1)

Use this checklist as a validation gate for every TAIEA SKU. MCP agents must refuse to ship if any item fails.

| Stage | Validation Items | Receipt |
|-------|------------------|---------|
| Pull | Work order exists in `ontology/work_orders.ttl`; demand signal recorded | `receipts/pull/*.json` |
| Plan | Heijunka slot assigned; WIP within limit | `receipts/plan/*.json` |
| Generate | Ontology load succeeded; SHACL shapes validated; SPARQL extracts deterministic | `receipts/generate/*.json` |
| Template | Tera rendering completed; templates match locked version; no manual edits | `receipts/template/*.json` |
| Build | `rebar3 compile` + tests pass; coverage receipts updated | `receipts/build/*.json` |
| Release | Release artifact built from receipts (ERTS-included tarball/container); checksum logged | `receipts/release/*.json` |
| Publish | Marketplace listing updated; entitlement routes configured; `/health`, `/pubsub`, `/marketplace` verifications pass | `receipts/publish/*.json` |
| Verify | Smoke tests executed against shipped artifact; entitlement refusal path tested | `receipts/verify/*.json` |
| Kaizen | Any failure produced Andon + 5 Whys + spec/test delta | `receipts/andon/*.json` |

## Definition of Done Gate

A SKU is shippable only when:
1. `shapes/` SHACL suite passes for the work order.
2. Build + tests pass with deterministic outputs.
3. Receipts exist for every stage above (no gaps).
4. `/health`, `/pubsub`, `/marketplace` endpoints pass functional verification.
5. Entitlement gating rejects inactive accounts (record refusal receipt).
6. Smoke test succeeds using the exact release artifact from `dist/`.

If any check fails → emit Andon receipt, quarantine the SKU, and block publishing until resolved.
