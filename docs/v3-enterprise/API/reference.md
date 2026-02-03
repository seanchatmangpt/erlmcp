# API Reference Documentation

## Overview

This document provides a comprehensive reference for the erlmcp v3 API, covering all endpoints, authentication methods, and usage patterns.

## Authentication

### JWT Authentication

```bash
# Get JWT token
curl -X POST "https://api.erlmcp.com/auth/token" \
  -H "Content-Type: application/json" \
  -d '{
    "username": "john.doe",
    "password": "securepassword"
  }'

# Response
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "expires_in": 3600,
  "token_type": "Bearer"
}
```

### API Key Authentication

```bash
curl -X GET "https://api.erlmcp.com/resources" \
  -H "X-API-Key: your-api-key"
```

## Resources API

### List Resources

```bash
curl -X GET "https://api.erlmcp.com/resources?limit=50&type=file" \
  -H "Authorization: Bearer $TOKEN"
```

**Response:**
```json
{
  "resources": [
    {
      "uri": "file:///workspace/README.md",
      "name": "README.md",
      "description": "Project documentation",
      "mime_type": "text/markdown",
      "capabilities": ["read", "write"],
      "metadata": {
        "size": 2048,
        "modified": "2024-02-01T12:00:00Z"
      }
    }
  ]
}
```

### Subscribe to Resource Changes

```bash
curl -X POST "https://api.erlmcp.com/resources/subscribe" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "uri": "file:///workspace/README.md",
    "events": ["created", "updated"]
  }'

# Response
{
  "subscription_id": "550e8400-e29b-41d4-a716-446655440000",
  "expires_at": "2024-02-01T13:00:00Z"
}
```

## Tools API

### List Available Tools

```bash
curl -X GET "https://api.erlmcp.com/tools?category=system&limit=20" \
  -H "Authorization: Bearer $TOKEN"
```

**Response:**
```json
{
  "tools": [
    {
      "name": "file_search",
      "description": "Search for files in workspace",
      "input_schema": {
        "type": "object",
        "properties": {
          "query": {
            "type": "string",
            "description": "Search query"
          },
          "max_results": {
            "type": "integer",
            "default": 10,
            "description": "Maximum results to return"
          }
        },
        "required": ["query"]
      },
      "category": "system",
      "namespace": "default"
    }
  ]
}
```

### Call a Tool

```bash
curl -X POST "https://api.erlmcp.com/tools/call" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "file_search",
    "arguments": {
      "query": "TODO items",
      "max_results": 5
    }
  }'

# Response
{
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Found 3 TODO items:\n1. Fix authentication bug\n2. Add logging\n3. Update documentation"
      }
    ],
    "metadata": {
      "files_found": 3,
      "query_time": 0.5
    }
  }
}
```

## Sessions API

### Create Session

```bash
curl -X POST "https://api.erlmcp.com/sessions" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "name": "Development Session",
    "metadata": {
      "user": "john.doe",
      "project": "erlmcp-integration"
    }
  }'

# Response
{
  "session": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "name": "Development Session",
    "status": "active",
    "created_at": "2024-02-01T12:00:00Z",
    "last_active": "2024-02-01T12:00:00Z",
    "metadata": {
      "user": "john.doe",
      "project": "erlmcp-integration"
    }
  },
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
}
```

### List Sessions

```bash
curl -X GET "https://api.erlmcp.com/sessions?limit=10" \
  -H "Authorization: Bearer $TOKEN"
```

**Response:**
```json
{
  "sessions": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440000",
      "name": "Development Session",
      "status": "active",
      "created_at": "2024-02-01T12:00:00Z",
      "last_active": "2024-02-01T12:30:00Z",
      "metadata": {
        "user": "john.doe",
        "project": "erlmcp-integration"
      }
    }
  ]
}
```

## WebSocket API

### Connect with WebSocket

```javascript
// JavaScript WebSocket example
const ws = new WebSocket('wss://api.erlmcp.com/rpc');

ws.onopen = function() {
    // Send authentication
    ws.send(JSON.stringify({
        jsonrpc: '2.0',
        method: 'auth',
        params: {
            token: 'your-jwt-token'
        },
        id: 1
    });
};

ws.onmessage = function(event) {
    const response = JSON.parse(event.data);
    console.log('Received:', response);
};

// Send tool request
ws.send(JSON.stringify({
    jsonrpc: '2.0',
    method: 'tools/call',
    params: {
        name: 'file_search',
        arguments: {
            query: 'TODO items'
        }
    },
    id: 2
}));
```

## SSE API

### Subscribe to Server-Sent Events

```bash
curl "https://api.erlmcp.com/stream?session_id=550e8400-e29b-41d4-a716-446655440000" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Accept: text/event-stream"
```

### Handle SSE Events

```javascript
// JavaScript SSE example
const eventSource = new EventSource(
    'https://api.erlmcp.com/stream?session_id=550e8400-e29b-41d4-a716-446655440000',
    {
        headers: {
            'Authorization': 'Bearer your-jwt-token'
        }
    }
);

eventSource.onmessage = function(event) {
    const data = JSON.parse(event.data);
    console.log('Event:', data);
};

eventSource.onerror = function(error) {
    console.error('SSE Error:', error);
    eventSource.close();
};
```

## JSON-RPC 2.0 API

### Single Request

```bash
curl -X POST "https://api.erlmcp.com/rpc" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/call",
    "params": {
      "name": "file_search",
      "arguments": {
        "query": "README",
        "max_results": 5
      }
    },
    "id": 1
  }'
```

### Batch Request

```bash
curl -X POST "https://api.erlmcp.com/rpc" \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '[
    {
      "jsonrpc": "2.0",
      "method": "tools/call",
      "params": {
        "name": "file_search",
        "arguments": {
          "query": "README",
          "max_results": 5
        }
      },
      "id": 1
    },
    {
      "jsonrpc": "2.0",
      "method": "resources/list",
      "params": {
        "type": "file"
      },
      "id": 2
    }
  ]'
```

## Client Libraries

### Python Client

```python
import requests
from typing import Optional, Dict, Any

class ErlmcpClient:
    def __init__(self, base_url: str, token: str):
        self.base_url = base_url
        self.token = token
        self.headers = {
            'Authorization': f'Bearer {token}',
            'Content-Type': 'application/json'
        }

    def list_resources(self, type: Optional[str] = None, limit: int = 100):
        params = {'limit': limit}
        if type:
            params['type'] = type

        response = requests.get(
            f"{self.base_url}/resources",
            headers=self.headers,
            params=params
        )
        response.raise_for_status()
        return response.json()

    def call_tool(self, tool_name: str, arguments: Dict[str, Any]):
        payload = {
            'jsonrpc': '2.0',
            'method': 'tools/call',
            'params': {
                'name': tool_name,
                'arguments': arguments
            },
            'id': 1
        }

        response = requests.post(
            f"{self.base_url}/rpc",
            headers=self.headers,
            json=payload
        )
        response.raise_for_status()
        return response.json()

    def create_session(self, name: str, metadata: Optional[Dict[str, Any]] = None):
        payload = {'name': name}
        if metadata:
            payload['metadata'] = metadata

        response = requests.post(
            f"{self.base_url}/sessions",
            headers=self.headers,
            json=payload
        )
        response.raise_for_status()
        return response.json()

# Usage
client = ErlmcpClient('https://api.erlmcp.com', 'your-jwt-token')

# List resources
resources = client.list_resources(type='file')
print(f"Found {len(resources['resources'])} resources")

# Call tool
result = client.call_tool('file_search', {
    'query': 'TODO items',
    'max_results': 5
})
print(f"Tool result: {result['result']['content'][0]['text']}")
```

### JavaScript Client

```javascript
class ErlmcpClient {
    constructor(baseUrl, token) {
        this.baseUrl = baseUrl;
        this.token = token;
        this.headers = {
            'Authorization': `Bearer ${token}`,
            'Content-Type': 'application/json'
        };
    }

    async listResources(type, limit = 100) {
        const params = new URLSearchParams({ limit });
        if (type) params.append('type', type);

        const response = await fetch(
            `${this.baseUrl}/resources?${params.toString()}`,
            { headers: this.headers }
        );
        return await response.json();
    }

    async callTool(toolName, arguments) {
        const payload = {
            jsonrpc: '2.0',
            method: 'tools/call',
            params: {
                name: toolName,
                arguments
            },
            id: 1
        };

        const response = await fetch(
            `${this.baseUrl}/rpc`,
            {
                method: 'POST',
                headers: this.headers,
                body: JSON.stringify(payload)
            }
        );
        return await response.json();
    }

    async createSession(name, metadata = {}) {
        const payload = { name, metadata };

        const response = await fetch(
            `${this.baseUrl}/sessions`,
            {
                method: 'POST',
                headers: this.headers,
                body: JSON.stringify(payload)
            }
        );
        return await response.json();
    }

    // WebSocket support
    connectWebSocket() {
        const ws = new WebSocket(`wss://api.erlmcp.com/rpc`);

        ws.onopen = () => {
            // Authenticate
            ws.send(JSON.stringify({
                jsonrpc: '2.0',
                method: 'auth',
                params: { token: this.token },
                id: 1
            }));
        };

        return ws;
    }
}

// Usage
const client = new ErlmcpClient('https://api.erlmcp.com', 'your-jwt-token');

// List resources
client.listResources('file', 50).then(resources => {
    console.log(`Found ${resources.resources.length} resources`);
});

// Call tool
client.callTool('file_search', {
    query: 'TODO items',
    maxResults: 5
}).then(result => {
    console.log('Result:', result.result.content[0].text);
});
```

## Error Handling

### Common Error Codes

| Code | Message | Description |
|------|--------|-------------|
| -32600 | Invalid Request | JSON-RPC request is malformed |
| -32601 | Method Not Found | Method does not exist |
| -32602 | Invalid Params | Invalid parameters passed |
| -32603 | Internal Error | Internal server error |
| -32000 | Tool Execution Error | Tool execution failed |
| -32001 | Authentication Error | Invalid or expired token |
| -32002 | Permission Error | Insufficient permissions |
| -32003 | Resource Not Found | Requested resource not found |
| -32004 | Rate Limit Exceeded | Too many requests |

### Error Response Example

```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": -32602,
    "message": "Invalid parameters",
    "data": {
      "details": "Query parameter is required",
      "validation_errors": [
        {
          "field": "query",
          "message": "Query is required"
        }
      ]
    }
  },
  "id": 1
}
```

## Rate Limiting

### Rate Limit Headers

```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1706736000
```

### Handling Rate Limits

```javascript
class RateLimitedClient {
    constructor(client, requestsPerSecond = 10) {
        this.client = client;
        this.requestsPerSecond = requestsPerSecond;
        this.lastRequestTime = 0;
    }

    async request(method, endpoint, data) {
        const now = Date.now();
        const timeSinceLast = now - this.lastRequestTime;
        const delay = Math.max(0, 1000 / this.requestsPerSecond - timeSinceLast);

        if (delay > 0) {
            await new Promise(resolve => setTimeout(resolve, delay));
        }

        this.lastRequestTime = Date.now();
        return await this.client.request(method, endpoint, data);
    }
}
```

## Webhooks

### Configure Webhook

```bash
curl -X POST "https://api.erlmcp.com/webhooks" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "url": "https://your-webhook-endpoint.com/events",
    "events": ["resource.created", "tool.executed", "session.created"],
    "secret": "your-webhook-secret"
  }'
```

### Validate Webhook Signature

```python
import hmac
import hashlib

def verify_webhook_signature(payload, signature, secret):
    expected_signature = 'sha256=' + hmac.new(
        secret.encode(),
        payload,
        hashlib.sha256
    ).hexdigest()

    return hmac.compare_digest(signature, expected_signature)
```

## Metrics and Monitoring

### Get System Metrics

```bash
curl -X GET "https://api.erlmcp.com/metrics" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Accept: application/json"
```

**Response:**
```json
{
  "timestamp": "2024-02-01T12:00:00Z",
  "metrics": {
    "connections": 100,
    "requests_total": 12345,
    "errors_total": 5,
    "duration_ms": {
      "p50": 10,
      "p95": 50,
      "p99": 100
    }
  }
}
```

### Prometheus Format

```bash
curl -X GET "https://api.erlmcp.com/metrics/prometheus" \
  -H "Accept: text/plain"
```

## Best Practices

### 1. Authentication

- Always use HTTPS
- Store tokens securely
- Implement token refresh
- Use short-lived tokens

### 2. Requests

- Use appropriate HTTP methods
- Implement retry logic
- Handle errors gracefully
- Implement circuit breakers

### 3. Performance

- Use connection pooling
- Implement caching
- Batch when possible
- Monitor performance metrics

### 4. Security

- Validate all inputs
- Sanitize outputs
- Use rate limiting
- Monitor for suspicious activity

## Troubleshooting

### Common Issues

**Authentication Failures**
```bash
# Check token expiration
curl -v "https://api.erlmcp.com/resources" \
  -H "Authorization: Bearer $TOKEN" 2>&1 | grep -i "www-authenticate"
```

**Connection Issues**
```bash
# Test connectivity
curl -I "https://api.erlmcp.com/health"

# Check DNS resolution
nslookup api.erlmcp.com

# Test SSL connection
openssl s_client -connect api.erlmcp.com:443
```

**Rate Limiting**
```bash
# Check rate limit headers
curl -I "https://api.erlmcp.com/resources" \
  -H "Authorization: Bearer $TOKEN"
```

### Debug Mode

```python
# Enable debug logging
import logging
logging.basicConfig(level=logging.DEBUG)

# Make request with debug info
response = requests.get(
    f"{client.base_url}/resources",
    headers=client.headers,
    params={'debug': 'true'}
)
```

## Support

For API support:
- Documentation: https://docs.erlmcp.com/api
- API Status: https://status.erlmcp.com
- Support: api-support@erlmcp.com

For issues:
- Report bugs: https://github.com/erlmcp/erlmcp/issues
- Feature requests: https://feedback.erlmcp.com