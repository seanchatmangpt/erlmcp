# MCP Gap Closure: Erlang Library Recommendations

This report identifies Erlang/OTP libraries that can accelerate implementation of the major gaps called out in `docs/MCP_PROTOCOL_COVERAGE_ANALYSIS.md`. Each section maps a missing MCP capability to mature libraries or ecosystems that erlmcp can adopt.

## 1. OAuth 2.0 & Resource Indicators
**Gap:** HTTP-based transports lack OAuth 2.0 authorization and RFC 8707 Resource Indicators.

### Recommended Libraries
- **[oauth2](https://github.com/kivra/oauth2)** (Kivra) – battle-tested Erlang OAuth 2.0 server/client. Supports authorization code flow and token handling.
- **[eldap](https://www.erlang.org/doc/man/eldap.html)** – built-in OTP LDAP client, useful if backing authorization metadata with directory services.
- **[apns4erl/erlcloud](https://github.com/erlcloud/erlcloud)** – while cloud-focused, they demonstrate secure credential storage patterns.

**Plan:** Embed oauth2 client inside HTTP transport (`erlmcp_transport_http`). Use Resource Indicator parameters when requesting tokens and cache tokens per transport instance.

## 2. Task Management & Long-Running Operations
**Gap:** MCP `tasks/*` API missing (task creation, status, cancellation, notifications).

### Recommended Libraries
- **[ra](https://github.com/rabbitmq/ra)** (Raft) – consensus-backed task coordination if distributed reliability is required.
- **[jobs](https://github.com/uwiger/jobs)** – lightweight job queue with monitoring.
- **[poolboy](https://github.com/devinus/poolboy)** (already used) – can back task workers once wrapped with task metadata.

**Plan:** Introduce a `erlmcp_task_manager` built on `jobs` or `ra`, persisting task state. Use PubSub (gproc or `pg`) to notify clients via `notifications/tasks/status`.

## 3. Elicitation & MCP Apps (Interactive UI)
**Gap:** No support for the new elicitation API or MCP Apps (interactive UIs).

### Recommended Libraries
- **[cowboy](https://github.com/ninenines/cowboy)** – already a dependency; can serve sandboxed iframes/UI resources.
- **[cowlib/websocket](https://ninenines.eu/docs/en/cowboy/2.10/guide/ws/)** – WebSocket support for bidirectional UI communication.
- **[jsx/absinthe](https://github.com/talentdeficit/jsx)** – JSON helpers for UI metadata.
- **[opencensus_erlang](https://github.com/census-instrumentation/opencensus-erlang)** – instrumentation for interactive flows if OpenTelemetry isn’t enough.

**Plan:** Extend server modules to declare UI resources (Mustache templates under `templates/tcps`). Expose them via Cowboy handlers and notify clients with elicitation metadata.

## 4. Browser-Friendly Transports (WebSocket & SSE)
**Gap:** No WebSocket or Server-Sent Events transports.

### Recommended Libraries
- **Cowboy WebSocket handlers** – first-class WS support.
- **[gun](https://github.com/ninenines/gun)** – already in use for HTTP; supports WebSocket client mode.
- **[elli/elli_websocket](https://github.com/elli-lib/elli)** – alternative minimal servers if needed.

**Plan:** Implement `erlmcp_transport_ws` using Cowboy for server-side and Gun for client-side, plus `erlmcp_transport_sse` using Cowboy for SSE endpoints.

## 5. Roots & Filesystem Security
**Gap:** Roots capability is incomplete (no boundary enforcement).

### Recommended Libraries
- **[fs](https://github.com/synrc/fs)** – cross-platform file system watcher.
- **[sumo_db](https://github.com/inaka/sumo_db)** or ETS/Mnesia – maintain whitelisted paths.

**Plan:** Introduce `erlmcp_roots` module enforcing canonicalized paths using `filename:absname/1` + ETS/`fs` watchers to monitor changes.

## 6. Icons & Static Assets
**Gap:** Resources/tools/prompts lack icon metadata and validation.

### Recommended Libraries
- **[recon](https://github.com/ferd/recon)** – not for icons but for validation/logging around suspicious URIs.
- **[hackney](https://github.com/benoitc/hackney)** – optional HTTP client to fetch/validate icon payloads if remote.

**Plan:** Extend data structures (records in `include/erlmcp.hrl`) to add icon metadata. Validate URI schemes using `uri_string` (stdlib) and restrict to HTTPS/data URIs. Serve icons through Cowboy.

## 7. Completion/Autocomplete API
**Gap:** No support for `completion/complete`.

### Recommended Libraries
- **[eqc](https://www.quviq.com/product/eqc/)** (QuickCheck) – inspire property-based tests for completions.
- **Existing JSON Schema** – can auto-generate completions via `jesse` introspection.

**Plan:** Build a `erlmcp_completion` module that leverages `jesse` schemas and ETS caches to return argument suggestions.

## 8. Logging Control & Observability
**Gap:** `logging/setLevel` partially implemented.

### Recommended Libraries
- **[lager](https://github.com/erlang-lager/lager)** – advanced logging with runtime level changes.
- **[telemetry](https://github.com/beam-telemetry/telemetry)** – integrate with OpenTelemetry.

**Plan:** Replace ad-hoc logging with Lager or OTP `logger` metadata; expose `logging/setLevel` by calling `logger:set_primary_config/2`.

## 9. OAuth Storage & Secrets
**Gap:** Secure credential storage not specified.

### Recommended Libraries
- **[vault](https://github.com/tsloughter/erlvault)** – HashiCorp Vault client for Erlang.
- **[cloudi_service_oauth2](https://github.com/CloudI/cloudi_service_oauth2)** – example of secure token storage.

**Plan:** Use Vault or OS keyrings via `ffi` to store OAuth secrets; integrate with HTTP transport.

## Implementation Strategy
1. Adopt the libraries above per gap, starting with security (OAuth + Resource Indicators) to meet MCP MUST requirements.
2. For each addition, tie changes back to `docs/MCP_PROTOCOL_COVERAGE_ANALYSIS.md` and update receipts (`docs/TCPS-checklist.md`).
3. Extend automated tests to cover new features, referencing relevant libraries (e.g., Cowboy WebSocket integration tests).

