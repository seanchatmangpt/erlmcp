# TCPS Certification Program (Definition of Done)

To consider a subsystem “TCPS certified,” every stage of the Toyota Code Production System must be demonstrably satisfied with receipts. This file maps each erlmcp subsystem to the TCPS Definition of Done (DoD) and lists the evidence required for certification.

## Subsystems in Scope

1. MCP Ontology + Shapes (`ontology/`, `shapes/`)
2. Registry (gproc integration)
3. Transports (stdio, TCP, HTTP)
4. Taiea governance layer (when protocol semantics change)
5. Build/Test infrastructure (`Makefile`, `rebar.config`, `tests/`)
6. Release + distribution pipeline (`dist/`, Marketplace assets)

## Certification Checklist

For **each subsystem**, the following must be true:

| TCPS Stage | Certification Evidence |
|------------|------------------------|
| Pull | Work order referencing subsystem exists in `ontology/work_orders.ttl`; receipts in `receipts/pull/` link to the subsystem SKU. |
| Plan | Heijunka slot assigned; WIP count recorded; receipts in `receipts/plan/` show subsystem bucket allocation. |
| Generate | Ontology+SHACL validation covers subsystem shapes; `sparql/` extracts reference subsystem-specific triples; receipts recorded under `receipts/generate/`. |
| Template | Templates or code generators (if any) for the subsystem are versioned; deterministic rendering receipts live under `receipts/template/`. |
| Build | `rebar3 compile` + targeted tests pass with receipts stored under `receipts/build/{subsystem}.json`. |
| Release | Release artifact includes subsystem binaries/docs, checksums logged in `receipts/release/`. |
| Publish | `/health`, `/pubsub`, `/marketplace` verification receipts demonstrate subsystem behavior; entitlement gating receipts show refusal on inactive users. |
| Verify | Smoke tests run against the shipping artifact, receipts stored under `receipts/verify/`. |
| Kaizen | Any Andon events tied to the subsystem have a completed 5-Whys record plus spec/test delta receipts. |

## Status Tracking Template

Record certification runs using the table below (one per subsystem & SKU):

| Subsystem | SKU / Work Order | DoD Evidence Path | Status | Notes |
|-----------|------------------|-------------------|--------|-------|
| Registry | `work_orders.ttl#registry` | `receipts/*/registry-*` | Pending | need receipts for gproc tests |
| HTTP Transport | `work_orders.ttl#http-transport` | `receipts/*/http-transport-*` | Pending | awaiting gun integration receipts |
| ... | ... | ... | ... | ... |

**Rule:** Do not mark “Complete” until receipts exist for every TCPS stage listed. If any evidence is missing or a failure occurs, raise an Andon event, quarantine the subsystem SKU, and block release.

## Action Items

1. Instantiate `receipts/` subdirectories per stage if not already present.
2. Update CI pipelines so each stage emits receipts and links to work orders.
3. For each subsystem, run through the TCPS daily loop and capture proof artifacts.
4. Maintain this file as the authoritative certification ledger.

