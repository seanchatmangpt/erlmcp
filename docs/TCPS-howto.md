# TCPS How-To: Running the Daily Loop

This guide walks MCP agents through the Toyota Code Production System daily loop using the repo assets. Follow each step sequentially; emit receipts as specified in `docs/TCPS-checklist.md`.

## Prerequisites
- `ontology/`, `shapes/`, `sparql/`, `templates/`, `receipts/`, `dist/`, `tools/` exist.
- Work orders declared in `ontology/work_orders.ttl`.
- `make setup` already executed; dependencies cached.

## Steps

### 1. Pull Demand → Work Orders
1. Run demand ingestion agent (Marketplace telemetry).
2. Convert signals to `WorkOrder` instances in `ontology/work_orders.ttl`.
3. Commit updated ontology and log receipt `receipts/pull/<sku>.json`.

### 2. Plan with Heijunka
1. Read open work orders.
2. Assign each to reliability/security/cost/compliance buckets per `docs/TCPS.md`.
3. Update `receipts/plan/<date>.json` with schedule + WIP count.

### 3. Generate Artifacts
1. Execute `tools/run-ggen.sh` (or equivalent) to load ontology & run SHACL.
2. Execute SPARQL queries → JSON fragments.
3. Render Tera templates into `src/`, `docs/`, `priv/` as required (remember: generated files are immutable by humans).
4. Capture deterministic hashes + receipts `receipts/generate/<sku>.json`.

### 4. Validate
1. Run `make lint` to ensure formatting/xref/dialyzer health.
2. Run `make test-unit`, `make test-integration` as required.
3. Store results in `receipts/build/<sku>.json` and update certification table (`docs/TCPS-certification.md`).

### 5. Build Release Artifact
1. Build release via `make release` or `rebar3 as prod release` (ERTS included).
2. Compute checksum; store artifact path in `dist/<sku>/` and receipt `receipts/release/<sku>.json`.

### 6. Publish + Verify
1. Deploy artifact to staging.
2. Hit `/health`, `/pubsub`, `/marketplace` endpoints; confirm entitlement gating refuses inactive accounts.
3. Run smoke tests from `tools/smoke.sh` using shipped artifact.
4. Store results under `receipts/publish/` and `receipts/verify/`.

### 7. Close Work Order
1. Ensure every stage has receipts.
2. Update `docs/TCPS-certification.md` entry to `Complete` with artifact references.
3. Close work order in ontology (set `state = fulfilled`).
4. Archive receipts with the release tag.

### 8. Handle Failures (Andon)
- If any command fails, stop immediately.
- Emit Andon receipt `receipts/andon/<sku>.json`.
- Run 5-Whys (template in `docs/TCPS-certification.md`).
- Patch ontology/templates/tests before restarting the loop.

## Notes
- Keep WIP within configured limit; refuse new work orders when limit hit.
- Maintenance tasks (template linting, dependency updates) run via `make tpm` (define per repo) and log under `receipts/maintenance/`.
- For fast changeovers (SMED), maintain separate ontology/template profiles under `profiles/` and swap by toggling env vars.

Use this how-to in tandem with the tutorial (`docs/TCPS.md`), the checklist (`docs/TCPS-checklist.md`), and the certification ledger (`docs/TCPS-certification.md`).
