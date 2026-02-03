# erlmcp v3 API Reference

## Overview

This document provides a comprehensive reference for the erlmcp v3 API, covering all endpoints, data structures, and authentication mechanisms.

## Table of Contents

1. [Authentication](#authentication)
2. [Client Management](#client-management)
3. [Tools](#tools)
4. [Resources](#resources)
5. [Prompts](#prompts)
6. [Sessions](#sessions)
7. [Events](#events)
8. [Monitoring](#monitoring)
9. [Error Handling](#error-handling)
10. [Examples](#examples)

## Authentication

### Overview

All API requests must include valid authentication credentials. erlmcp supports multiple authentication methods:

- Bearer Token (JWT)
- OAuth 2.0
- API Keys

### Authentication Headers

```http
Authorization: Bearer <jwt_token>
Authorization: Bearer <oauth_token>
Authorization: ApiKey <api_key>
```

### Token Refresh

```http
POST /auth/refresh
Content-Type: application/json

{
  "refresh_token": "refresh_token_value"
}
```

**Response:**
```json
{
  "access_token": "new_access_token",
  "refresh_token": "new_refresh_token",
  "expires_in": 3600,
  "token_type": "Bearer"
}
```

## Client Management

### Initialize Client

Initialize a new client connection.

**Endpoint:** `POST /initialize`

**Request:**
```json
{
  "protocolVersion": "2024-11-05",
  "protocol": "stdio",
  "capabilities": ["tools", "resources", "prompts"]
}
```

**Response:**
```json
{
  "protocolVersion": "2024-11-05",
  "capabilities": ["tools", "resources", "prompts", "sessions"],
  "serverInfo": {
    "name": "erlmcp-v3",
    "version": "3.0.0",
    "maxSessions": 10000
  },
  "authentication": {
    "required": true,
    "methods": ["bearer", "oauth2"],
    "scopes": ["read", "write", "admin"]
  }
}
```

## Tools

### Call Tool

Execute a tool with the provided arguments.

**Endpoint:** `POST /tools/call`

**Request:**
```json
{
  "name": "calculator",
  "arguments": {
    "expression": "2 + 2"
  },
  "inputSchema": {
    "type": "object",
    "properties": {
      "expression": {"type": "string"}
    },
    "required": ["expression"]
  },
  "outputSchema": {
    "type": "number"
  }
}
```

**Response:**
```json
{
  "requestId": "req-12345",
  "status": "success",
  "content": {
    "result": 4,
    "type": "number",
    "message": "Calculation completed"
  },
  "executionTime": 15.5,
  "usage": {
    "tokens": 100,
    "cost": 0.01
  }
}
```

### List Tools

List all available tools.

**Endpoint:** `GET /tools`

**Response:**
```json
{
  "tools": [
    {
      "name": "calculator",
      "description": "Perform mathematical calculations",
      "parameters": {
        "type": "object",
        "properties": {
          "expression": {
            "type": "string",
            "description": "Mathematical expression"
          }
        },
        "required": ["expression"]
      },
      "category": "math"
    },
    {
      "name": "weather",
      "description": "Get weather information",
      "parameters": {
        "type": "object",
        "properties": {
          "location": {
            "type": "string",
            "description": "Location name"
          }
        },
        "required": ["location"]
      },
      "category": "utilities"
    }
  ]
}
```

## Resources

### Subscribe to Resource

Subscribe to changes for a specific resource.

**Endpoint:** `POST /resources/subscribe`

**Request:**
```json
{
  "uri": "resource://example.com/data/123",
  "events": ["create", "update", "delete"],
  "label": "user-data-updates",
  "options": {
    "batchSize": 100,
    "throttleMs": 1000
  }
}
```

**Response:**
```json
{
  "requestId": "req-12345",
  "subscriptionId": "sub-12345",
  "uri": "resource://example.com/data/123",
  "events": ["update", "delete"],
  "status": "active",
  "expiresAt": "2024-01-01T00:00:00Z"
}
```

### Unsubscribe from Resource

Unsubscribe from a resource subscription.

**Endpoint:** `POST /resources/unsubscribe`

**Request:**
```json
{
  "uri": "resource://example.com/data/123",
  "subscriptionId": "sub-12345"
}
```

### List Resources

List all available resources.

**Endpoint:** `GET /resources`

**Response:**
```json
{
  "resources": [
    {
      "uri": "resource://example.com/users",
      "type": "collection",
      "description": "User collection",
      "metadata": {
        "count": 1000,
        "lastUpdated": "2024-01-01T00:00:00Z"
      }
    },
    {
      "uri": "resource://example.com/users/123",
      "type": "item",
      "description": "User data",
      "metadata": {
        "schema": "user"
      }
    }
  ]
}
```

## Prompts

### Create Prompt Template

Create a reusable prompt template.

**Endpoint:** `POST /prompts/create`

**Request:**
```json
{
  "name": "summarize-email",
  "description": "Summarizes an email thread",
  "template": "Summarize the following email:\n\n{{content}}",
  "variables": [
    {
      "name": "content",
      "type": "string",
      "description": "Email content to summarize",
      "required": true
    },
    {
      "name": "author",
      "type": "string",
      "description": "Email author",
      "required": false
    }
  ],
  "validation": {
    "required": ["content"],
    "maxLength": 10000
  },
  "metadata": {
    "category": "email-processing",
    "version": "1.0"
  }
}
```

**Response:**
```json
{
  "requestId": "req-12345",
  "promptId": "prompt-12345",
  "name": "summarize-email",
  "template": "Summarize the following email:\n\n{{content}}",
  "variables": [
    {
      "name": "content",
      "type": "string",
      "description": "Email content to summarize",
      "required": true
    },
    {
      "name": "author",
      "type": "string",
      "description": "Email author",
      "required": false
    }
  ],
  "metadata": {
    "category": "email-processing",
    "version": "1.0",
    "created": "2024-01-01T00:00:00Z"
  }
}
```

### Render Prompt

Render a prompt template with provided variables.

**Endpoint:** `POST /prompts/render`

**Request:**
```json
{
  "template": "summarize-email",
  "variables": {
    "content": "Meeting about Q4 planning...",
    "author": "John Doe"
  },
  "format": "text"
}
```

**Response:**
```json
{
  "prompt": "Summarize the following email:\n\nMeeting about Q4 planning...",
  "metadata": {
    "renderTime": 5.2,
    "variablesUsed": ["content", "author"]
  }
}
```

### List Prompts

List all prompt templates.

**Endpoint:** `GET /prompts`

**Response:**
```json
{
  "prompts": [
    {
      "id": "prompt-12345",
      "name": "summarize-email",
      "description": "Summarizes an email thread",
      "category": "email-processing",
      "variables": 2,
      "usageCount": 100
    },
    {
      "id": "prompt-67890",
      "name": "analyze-sentiment",
      "description": "Analyzes sentiment of text",
      "category": "nlp",
      "variables": 1,
      "usageCount": 50
    }
  ]
}
```

## Sessions

### Create Session

Create a new session.

**Endpoint:** `POST /sessions`

**Request:**
```json
{
  "clientId": "client-123",
  "metadata": {
    "device": "web",
    "platform": "chrome"
  }
}
```

**Response:**
```json
{
  "sessionId": "session-12345",
  "clientId": "client-123",
  "createdAt": "2024-01-01T00:00:00Z",
  "expiresAt": "2024-01-01T01:00:00Z",
  "metadata": {
    "device": "web",
    "platform": "chrome"
  }
}
```

### Get Session

Retrieve session information.

**Endpoint:** `GET /sessions/{sessionId}`

**Response:**
```json
{
  "sessionId": "session-12345",
  "clientId": "client-123",
  "state": "active",
  "createdAt": "2024-01-01T00:00:00Z",
  "lastActivity": "2024-01-01T00:30:00Z",
  "metadata": {
    "device": "web",
    "platform": "chrome"
  }
}
```

### Update Session

Update session metadata.

**Endpoint:** `PUT /sessions/{sessionId}`

**Request:**
```json
{
  "metadata": {
    "userAgent": "Mozilla/5.0...",
    "ipAddress": "192.168.1.1"
  }
}
```

### Delete Session

Close a session.

**Endpoint:** `DELETE /sessions/{sessionId}`

## Events

### Poll Events

Poll for pending events.

**Endpoint:** `GET /events/poll`

**Parameters:**
- `lastEventId` (string): ID of the last received event
- `timeout` (integer): Poll timeout in milliseconds (default: 30000)
- `batchSize` (integer): Maximum events to return (default: 100)

**Response:**
```json
{
  "events": [
    {
      "id": "event-12345",
      "type": "resource.update",
      "timestamp": "2024-01-01T00:00:00Z",
      "source": "resource://example.com/data/123",
      "data": {
        "oldValue": null,
        "newValue": { "name": "John", "age": 30 }
      },
      "correlationId": "corr-12345",
      "metadata": {
        "traceId": "trace-12345"
      }
    }
  ],
  "nextPollTimeout": 30000,
  "totalPending": 5
}
```

### Push Events

Receive real-time events via WebSocket.

**WebSocket Connection:**
```javascript
const ws = new WebSocket('wss://api.erlmcp.com/v3/events');

ws.onmessage = function(event) {
  const event = JSON.parse(event.data);
  console.log('Received event:', event);
};

// Subscribe to events
ws.send(JSON.stringify({
  method: 'subscribe',
  params: {
    uri: 'resource://example.com/data/123',
    events: ['update', 'delete']
  }
}));
```

## Monitoring

### Health Check

Check system health.

**Endpoint:** `GET /health`

**Response:**
```json
{
  "status": "healthy",
  "timestamp": "2024-01-01T00:00:00Z",
  "components": {
    "database": {
      "status": "healthy",
      "latencyMs": 5.2
    },
    "authentication": {
      "status": "healthy",
      "latencyMs": 10.5
    },
    "registry": {
      "status": "healthy",
      "registeredResources": 12345
    }
  }
}
```

### Metrics

Get system metrics.

**Endpoint:** `GET /metrics`

**Response:**
```json
{
  "timestamp": "2024-01-01T00:00:00Z",
  "metrics": {
    "sessions": {
      "active": 1000,
      "total": 5000,
      "rate": 50
    },
    "requests": {
      "total": 100000,
      "errors": 100,
      "avgLatency": 45.5
    },
    "resources": {
      "total": 10000,
      "subscriptions": 5000
    }
  }
}
```

### Logs

Get application logs.

**Endpoint:** `GET /logs`

**Parameters:**
- `level` (string): Log level (debug, info, warn, error)
- `limit` (integer): Number of logs to return (default: 100)
- `offset` (integer): Log offset (default: 0)

**Response:**
```json
{
  "logs": [
    {
      "timestamp": "2024-01-01T00:00:00Z",
      "level": "info",
      "message": "HTTP request processed",
      "metadata": {
        "method": "GET",
        "path": "/v3/health",
        "status": 200
      }
    },
    {
      "timestamp": "2024-01-01T00:00:01Z",
      "level": "error",
      "message": "Database connection failed",
      "metadata": {
        "error": "Connection timeout",
        "retries": 3
      }
    }
  ],
  "total": 1000,
  "limit": 100,
  "offset": 0
}
```

## Error Handling

### Error Response Format

```json
{
  "jsonrpc": "2.0",
  "id": "req-12345",
  "error": {
    "code": -32601,
    "message": "Method not found",
    "data": {
      "details": "Method resources/subscribe not found",
      "request_id": "req-12345",
      "timestamp": "2024-01-01T00:00:00Z"
    }
  }
}
```

### Error Codes

| Code | Message | Description |
|------|---------|-------------|
| -32700 | Parse Error | Invalid JSON-RPC request |
| -32600 | Invalid Request | Invalid request structure |
| -32601 | Method Not Found | Requested method not found |
| -32602 | Invalid Params | Invalid method parameters |
| -32603 | Internal Error | Internal JSON-RPC error |
| -32000 to -32099 | Server Error | Application-specific errors |

### Common Errors

#### Authentication Error

```json
{
  "error": {
    "code": -32001,
    "message": "Authentication required",
    "data": {
      "details": "Bearer token required",
      "error_type": "authentication"
    }
  }
}
```

#### Authorization Error

```json
{
  "error": {
    "code": -32002,
    "message": "Insufficient permissions",
    "data": {
      "details": "User does not have 'write' permission",
      "required_permission": "write"
    }
  }
}
```

#### Rate Limit Error

```json
{
  "error": {
    "code": -32003,
    "message": "Rate limit exceeded",
    "data": {
      "details": "1000 requests per minute limit exceeded",
      "retry_after": 60,
      "limit": 1000,
      "window": 60
    }
  }
}
```

#### Validation Error

```json
{
  "error": {
    "code": -32004,
    "message": "Validation failed",
    "data": {
      "details": "Invalid email format",
      "field": "email",
      "value": "invalid-email"
    }
  }
}
```

## Examples

### Example 1: Complete Workflow

```bash
# 1. Initialize client
curl -X POST https://api.erlmcp.com/v3/initialize \
  -H "Content-Type: application/json" \
  -d '{
    "protocolVersion": "2024-11-05",
    "protocol": "stdio"
  }'

# Response:
# {
#   "protocolVersion": "2024-11-05",
#   "capabilities": ["tools", "resources", "prompts", "sessions"],
#   "serverInfo": {...}
# }

# 2. Call a tool
curl -X POST https://api.erlmcp.com/v3/tools/call \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "calculator",
    "arguments": {
      "expression": "2 + 2 * 3"
    }
  }'

# Response:
# {
#   "requestId": "req-12345",
#   "status": "success",
#   "content": {
#     "result": 8,
#     "type": "number"
#   }
# }

# 3. Subscribe to a resource
curl -X POST https://api.erlmcp.com/v3/resources/subscribe \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "uri": "resource://example.com/data/123",
    "events": ["update", "delete"]
  }'

# Response:
# {
#   "requestId": "req-12345",
#   "subscriptionId": "sub-12345",
#   "uri": "resource://example.com/data/123",
#   "events": ["update", "delete"],
#   "status": "active"
# }

# 4. Poll for events
curl -X GET "https://api.erlmcp.com/v3/events/poll?lastEventId=event-12345" \
  -H "Authorization: Bearer $TOKEN"

# Response:
# {
#   "events": [...],
#   "nextPollTimeout": 30000
# }
```

### Example 2: Python Client

```python
import requests
import json
import websocket
import time

class erlmcpClient:
    def __init__(self, base_url, token):
        self.base_url = base_url
        self.token = token
        self.headers = {
            'Authorization': f'Bearer {token}',
            'Content-Type': 'application/json'
        }

    def initialize(self):
        response = requests.post(
            f"{self.base_url}/initialize",
            json={
                "protocolVersion": "2024-11-05",
                "protocol": "stdio"
            }
        )
        response.raise_for_status()
        return response.json()

    def call_tool(self, name, arguments=None):
        data = {
            "name": name,
            "arguments": arguments or {}
        }
        response = requests.post(
            f"{self.base_url}/tools/call",
            headers=self.headers,
            json=data
        )
        response.raise_for_status()
        return response.json()

    def subscribe_resource(self, uri, events):
        data = {
            "uri": uri,
            "events": events
        }
        response = requests.post(
            f"{self.base_url}/resources/subscribe",
            headers=self.headers,
            json=data
        )
        response.raise_for_status()
        return response.json()

    def poll_events(self, last_event_id=None, timeout=30):
        params = {}
        if last_event_id:
            params['lastEventId'] = last_event_id
        params['timeout'] = timeout

        response = requests.get(
            f"{self.base_url}/events/poll",
            headers=self.headers,
            params=params
        )
        response.raise_for_status()
        return response.json()

# Usage
client = erlmcpClient(
    base_url="https://api.erlmcp.com/v3",
    token="your_token_here"
)

# Initialize
init_response = client.initialize()
print("Capabilities:", init_response['capabilities'])

# Call tool
result = client.call_tool(
    name="calculator",
    arguments={"expression": "2 + 2 * 3"}
)
print("Calculation result:", result['content']['result'])

# Subscribe to resource
subscription = client.subscribe_resource(
    uri="resource://example.com/data/123",
    events=["update", "delete"]
)
print("Subscription ID:", subscription['subscriptionId'])

# Poll for events
events = client.poll_events()
for event in events['events']:
    print(f"Event {event['id']}: {event['type']}")
    if 'newValue' in event['data']:
        print(f"New value: {event['data']['newValue']}")
```

### Example 3: WebSocket Event Handler

```javascript
const erlmcpWebSocket = class {
    constructor(url, token) {
        this.url = url.replace('http', 'ws') + '/events';
        this.token = token;
        this.subscriptions = new Map();
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 5;
    }

    connect() {
        this.ws = new WebSocket(this.url);

        this.ws.onopen = () => {
            console.log('Connected to erlmcp WebSocket');
            this.reconnectAttempts = 0;
            this.resubscribe();
        };

        this.ws.onmessage = (event) => {
            const message = JSON.parse(event.data);
            this.handleMessage(message);
        };

        this.ws.onclose = () => {
            console.log('WebSocket connection closed');
            this.reconnect();
        };

        this.ws.onerror = (error) => {
            console.error('WebSocket error:', error);
        };
    }

    handleMessage(message) {
        if (method === 'subscription_created') {
            const { subscriptionId, uri, events } = params;
            this.subscriptions.set(subscriptionId, { uri, events });
            console.log('Subscribed to', uri, 'for events', events);
        } else if (method === 'event_received') {
            const { event } = params;
            console.log('Received event:', event);
            // Handle the event based on its type
            this.processEvent(event);
        }
    }

    subscribe(uri, events, label) {
        const request = {
            method: 'subscribe',
            params: {
                uri,
                events,
                label
            }
        };
        this.ws.send(JSON.stringify(request));
    }

    unsubscribe(subscriptionId) {
        const request = {
            method: 'unsubscribe',
            params: {
                subscriptionId
            }
        };
        this.ws.send(JSON.stringify(request));
        this.subscriptions.delete(subscriptionId);
    }

    resubscribe() {
        for (const [subscriptionId, sub] of this.subscriptions) {
            this.subscribe(sub.uri, sub.events);
        }
    }

    reconnect() {
        if (this.reconnectAttempts < this.maxReconnectAttempts) {
            this.reconnectAttempts++;
            console.log(`Reconnecting in ${this.reconnectAttempts * 2} seconds...`);
            setTimeout(() => this.connect(), this.reconnectAttempts * 2000);
        } else {
            console.error('Max reconnection attempts reached');
        }
    }

    processEvent(event) {
        switch (event.type) {
            case 'resource.update':
                this.handleResourceUpdate(event);
                break;
            case 'resource.delete':
                this.handleResourceDelete(event);
                break;
            case 'tool.completed':
                this.handleToolCompleted(event);
                break;
            default:
                console.log('Unknown event type:', event.type);
        }
    }

    handleResourceUpdate(event) {
        console.log('Resource updated:', event.source);
        if (event.data && event.data.newValue) {
            // Update UI or trigger business logic
        }
    }

    handleResourceDelete(event) {
        console.log('Resource deleted:', event.source);
        // Clean up related data
    }

    handleToolCompleted(event) {
        console.log('Tool completed:', event.toolId);
        // Update tool status
    }
};

// Usage
const client = new erlmcpWebSocket(
    'https://api.erlmcp.com/v3',
    'your_token_here'
);

client.connect();

// Subscribe to resource updates
client.subscribe(
    'resource://example.com/users/*',
    ['create', 'update', 'delete'],
    'user-changes'
);

// Handle events
client.on('event', (event) => {
    console.log('Received event:', event);
});
```

## API Versioning

### Version Support

- **v1**: Legacy version (deprecated)
- **v2**: Current stable version
- **v3**: Latest version (recommended)

### Version Header

```http
Accept: application/vnd.erlmcp.v3+json
```

### Version Migration

When upgrading between versions:

1. **v2 to v3**:
   - Protocol version must be specified in initialization
   - All endpoints use `/v3` prefix
   - Response format has changed

2. **v1 to v2**:
   - Authentication method changed
   - Session management improved
   - Added resource subscriptions

## Rate Limiting

### Rate Limits

| Endpoint | Limit | Window |
|----------|-------|--------|
| `/tools/call` | 1000/min | 1 minute |
| `/resources/subscribe` | 100/min | 1 minute |
| `/prompts/*` | 500/min | 1 minute |
| `/sessions/*` | 50/min | 1 minute |
| `/events/poll` | 1000/min | 1 minute |

### Rate Limit Headers

```http
X-RateLimit-Limit: 1000
X-RateLimit-Remaining: 999
X-RateLimit-Reset: 1640995200
```

### Handling Rate Limits

```javascript
// Check remaining requests
const remaining = parseInt(response.headers['x-ratelimit-remaining']);
const resetTime = parseInt(response.headers['x-ratelimit-reset']) * 1000;

if (remaining <= 10) {
    // Wait until reset time
    const waitTime = resetTime - Date.now();
    setTimeout(() => continueRequests(), waitTime);
}
```

---
*Last Updated: February 2024*
*Version: 3.0.0*