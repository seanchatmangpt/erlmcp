# Feature Diagram: WebSocket & SSE Transports

```mermaid
graph LR
    subgraph Server Side (Cowboy)
        ws_listener[Cowboy WebSocket Listener]
        sse_listener[Cowboy SSE Handler]
        ws_transport[erlmcp_transport_ws]
        sse_transport[erlmcp_transport_sse]
    end

    subgraph Client Side
        gun_client[gun WebSocket Client]
        http_sse_client[HTTP SSE Client]
    end

    ws_listener --> ws_transport
    sse_listener --> sse_transport
    ws_transport --> registry[erlmcp_registry]
    sse_transport --> registry
    registry --> erlmcp_server

    gun_client --> ws_listener
    http_sse_client --> sse_listener
```

```mermaid
sequenceDiagram
    participant ClientWS
    participant WSListener as Cowboy WS Handler
    participant WSTransport
    participant Registry
    participant Server

    ClientWS->>WSListener: WebSocket Upgrade
    WSListener->>WSTransport: init/1
    WSTransport->>Registry: register_transport
    ClientWS->>WSTransport: JSON-RPC frames
    WSTransport->>Registry: route_to_server
    Registry->>Server: {mcp_message,...}
    Server->>Registry: {mcp_response,...}
    Registry->>WSTransport: route_to_transport
    WSTransport->>ClientWS: JSON-RPC frames
```
