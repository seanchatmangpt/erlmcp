# Feature Diagram: MCP Tasks Integration

```mermaid
graph TB
    server[erlmcp_server]
    taskmgr[erlmcp_task_manager]
    queue[jobs queue]
    ets[ETS: task_state]
    registry[erlmcp_registry]
    client[MCP Client]

    client -->|tasks/create (tool call)| server
    server --> taskmgr
    taskmgr --> queue
    queue --> workers[Task Workers]
    workers --> ets
    ets --> taskmgr
    taskmgr --> registry
    registry --> client

    client -->|tasks/get/result/cancel| server
    server --> taskmgr
```

```mermaid
sequenceDiagram
    participant Client
    participant Server
    participant TaskMgr
    participant Worker

    Client->>Server: tools/call (async)
    Server->>TaskMgr: create_task(tool, params)
    TaskMgr->>Client: {task_id, status=pending}
    TaskMgr->>Worker: enqueue
    Worker-->>TaskMgr: progress update
    TaskMgr->>Client: notifications/tasks/status
    Worker-->>TaskMgr: result
    TaskMgr->>Server: task_complete(task_id, result)
    Server->>Client: tasks/result or notifications/tool_result
```
