# Feature Diagram: Elicitation & MCP Apps

```mermaid
graph LR
    client[MCP Client]
    server[erlmcp_server]
    elicitation[erlmcp_elicitation]
    cowboy[Cowboy HTTP]
    ui_handler[erlmcp_ui_handler]
    iframe[Sandboxed Iframe]

    client -->|elicitation/create| server
    server --> elicitation
    elicitation --> cowboy
    cowboy --> ui_handler
    ui_handler --> iframe
    iframe -->|user input| ui_handler --> elicitation --> server --> client (notifications/elicitation/complete)
```

```mermaid
flowchart TB
    subgraph Server
        server_core[erlmcp_server]
        elicitation_mod[erlmcp_elicitation\n(form + URL definitions)]
        app_manifest[MCP App Manifest]
    end

    subgraph Web Tier
        cowboy_router[Cowboy Router]
        ws_handler[WebSocket Handler]
        sse_handler[SSE Handler]
        static_assets[Mustache Templates]
    end

    subgraph Client Runtime
        sandbox_iframe[Sandboxed UI]
        mcp_client[MCP Client]
    end

    server_core --> elicitation_mod
    elicitation_mod --> app_manifest
    app_manifest --> cowboy_router
    cowboy_router --> ws_handler
    cowboy_router --> sse_handler
    cowboy_router --> static_assets
    ws_handler --> sandbox_iframe
    sse_handler --> sandbox_iframe
    sandbox_iframe --> mcp_client
```
