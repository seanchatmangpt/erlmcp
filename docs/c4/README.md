# C4 Diagram Index for MCP Gap Implementation

Use these diagrams when implementing the coverage roadmap:

1. `context.md` – Level 1 context of erlmcp + TCPS within TAIEA and Marketplace signals.
2. `container.md` – Level 2 container relationships between supervisors, transports, TCPS pipeline, TAIEA.
3. `components-transports.md` – Level 3 component view for stdio/TCP/HTTP transports.
4. `feature-oauth.md` – OAuth 2.0 + Resource Indicator flow within the HTTP transport.
5. `feature-tasks.md` – Task manager integration sequence + component view.
6. `feature-elicitation-apps.md` – Architecture for elicitation API and MCP Apps (Cowboy WebSockets/SSE + sandboxed UI).
7. `feature-web-transports.md` – Design for WebSocket and SSE transports (Cowboy/Gun integration).
8. `feature-roots.md` – Roots enforcement flow (path validation, ETS storage, watchers).
9. `feature-completion.md` – Completion/autocomplete module interacting with Jesse schemas.
10. `feature-icons.md` – Icon metadata validation and serving.
11. `feature-logging.md` – Logging/setLevel control linking OTP logger to MCP requests.

Render each Mermaid diagram to guide coding agents and keep them synchronized with code changes.
