# Repository Guidelines

## Project Structure & Module Organization
Operate with the workspace split: `src/` + `include/` house the MCP runtime, `test/` and `tests/` contain eunit/Common Test suites, and `priv/` stores runtime assets. `examples/` mirrors supported integration patterns—treat it as the canonical reference before designing new transports. The `taiea/` umbrella (apps under `taiea/apps/*`) implements governance and receipt logic; only touch it when protocol semantics change. Build artifacts aggregate in `_build/`; configuration inputs are under `config/`, `vm.args`, and top-level `rebar.config`.

## Build, Test, and Development Commands
Use `make setup` once to hydrate deps. During focused work, prefer `make build` for the erlmcp app or `make workspace-build` when taiea involvement is explicit. Validation stack: `make test-unit`, `make test-integration`, and `make test-property` for targeted coverage, `make test` for the combined suite, and the `workspace-*` variants for cross-system drift detection. Enforce quality gates with `make lint`, `make xref`, `make dialyzer`, and `make format`; `make check` / `make workspace-check` run the entire pipeline pre-commit.

## Coding Style & Naming Conventions
The formatter (see `rebar.config:format`) enforces 4-space base indentation, 2-space breaks, and a 100-column paper width. Modules, files, and variables use snake_case, records live in `include/*.hrl`, and macros stay uppercase. Exported functions demand `-spec` annotations plus Dialyzer-clean contracts. Prefer pattern-matched data pipelines, explicit error tuples, and Guard-first clause ordering to keep observer traces readable.

## Testing Guidelines
Eunit modules adopt the `{module}_tests.erl` suffix; Common Test suites end with `_SUITE.erl`. Property checks belong in PropEr modules co-located with the code they stress. Reproduce failures with `rebar3 eunit --module foo`, `rebar3 ct --suite bar_SUITE`, or `rebar3 proper --mod baz_props`. Generate coverage via `make coverage-report` and drop `_build/test/cover/index.html` diffs into review notes for any behavior fix.

## Commit & Pull Request Guidelines
Commits follow Conventional Commit verbs with optional scopes (`feat(workspace):`, `fix(transport):`). Rebase before pushing to avoid noisy merge history. Every PR must describe the issue context, the architectural decision, and the validation commands executed; include taiea impact if applicable. Treat `make check` results as blocking, attach coverage or dialyzer artifacts when they influenced the change, and ensure linked issues or receipts resolve acceptance criteria before requesting review.

## Security & Configuration Notes
Never commit secrets—leverage placeholder configs under `config/`. When touching transports, validate TLS or capability toggles inside `docs/` so downstream MCP agents inherit the change. Update `docs/architecture.md` whenever supervision topology or restart policy shifts, because client agents depend on those guarantees for resilience planning.

## TCPS Expectations for Agents

Stop being lazy. Every agent is expected to:

1. Read the outstanding work orders (`ontology/work_orders.ttl`) before touching code.
2. Follow the TCPS daily loop (docs/TCPS-howto.md) and emit receipts for every stage.
3. Keep C4 diagrams (docs/c4/*.md) in sync when changing architecture.
4. Update `docs/ARCHITECTURE_OVERVIEW.md` and other Diataxis docs when introducing or modifying subsystems.
5. Never skip tests; if the pipeline is red, raise an Andon receipt and fix root causes instead of papering over failures.
6. Use the TCPS checklist (docs/TCPS-checklist.md) and certification ledger (docs/TCPS-certification.md) when shipping subsystems.

If you’re ignoring these rules, you’re not helping—you’re adding debt. EOF
