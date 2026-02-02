# ERLMCP Integration Guide

This guide covers how to integrate erlmcp CLI with MCP servers, including connection setup, authentication, session management, and resource subscription workflows.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Connection Setup](#connection-setup)
3. [Authentication Configuration](#authentication-configuration)
4. [Session Management](#session-management)
5. [Resource Subscription](#resource-subscription)
6. [Multi-Transport Integration](#multi-transport-integration)
7. [Advanced Integration Patterns](#advanced-integration-patterns)
8. [Monitoring & Observability](#monitoring--observability)

---

## Prerequisites

### System Requirements
- **Erlang/OTP**: 25+
- **rebar3**: Build tool
- **Network access** to target MCP servers
- **SSL certificates** for HTTPS connections

### Dependencies
```bash
# Install required packages
rebar3 deps

# Build CLI
rebar3 escriptize

# Verify installation
./erlmcp validate spec-check
```

---

## Connection Setup

### Basic TCP Connection

```bash
# Start erlmcp server
./erlmcp start

# Connect via TCP
nc localhost 8080

# Send MCP request
{
  "jsonrpc": "2.0",
  "id": "conn-test-001",
  "method": "tools/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "conn-test-001",
  "result": {
    "tools": [...]
  }
}
```

### HTTP Connection Setup

```bash
# Test HTTP endpoint
curl -X POST http://localhost:8080 \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer your-api-key" \
  -d '{
    "jsonrpc": "2.0",
    "id": "http-test-001",
    "method": "tools/list"
  }'
```

### WebSocket Connection

```javascript
// WebSocket client example
class ERLMCPWebSocket {
  constructor(url, options = {}) {
    this.url = url;
    this.options = options;
    this.ws = null;
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 5;
  }

  connect() {
    this.ws = new WebSocket(this.url);

    this.ws.onopen = () => {
      console.log('Connected to erlmcp');
      this.reconnectAttempts = 0;
      this.send({
        jsonrpc: "2.0",
        id: "connect-001",
        method: "tools/list"
      });
    };

    this.ws.onmessage = (event) => {
      const response = JSON.parse(event.data);
      this.handleResponse(response);
    };

    this.ws.onclose = () => {
      console.log('Disconnected from erlmcp');
      this.reconnect();
    };

    this.ws.onerror = (error) => {
      console.error('WebSocket error:', error);
    };
  }

  send(request) {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(request));
    }
  }

  handleResponse(response) {
    if (response.error) {
      console.error('Error:', response.error);
    } else {
      console.log('Response:', response.result);
    }
  }

  reconnect() {
    if (this.reconnectAttempts < this.maxReconnectAttempts) {
      this.reconnectAttempts++;
      setTimeout(() => {
        console.log(`Reconnecting... (attempt ${this.reconnectAttempts})`);
        this.connect();
      }, 1000 * this.reconnectAttempts);
    }
  }
}

// Usage
const client = new ERLMCPWebSocket('ws://localhost:8080');
client.connect();
```

---

## Authentication Configuration

### API Key Authentication

```bash
# Set API key in environment
export ERLMCP_API_KEY="your-api-key-here"

# Or pass directly in requests
curl -X POST http://localhost:8080 \
  -H "Authorization: Bearer your-api-key" \
  -d '{
    "jsonrpc": "2.0",
    "id": "auth-test-001",
    "method": "tools/list"
  }'
```

### JWT Token Authentication

```javascript
// Generate JWT token
const jwt = require('jsonwebtoken');
const token = jwt.sign(
  { sub: 'user-123', role: 'admin' },
  'your-secret-key',
  { expiresIn: '1h' }
);

// Use in HTTP request
fetch('http://localhost:8080', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Authorization': `Bearer ${token}`
  },
  body: JSON.stringify({
    jsonrpc: "2.0",
    id: "jwt-test-001",
    method: "tools/list"
  })
});
```

### Certificate-Based Authentication (HTTPS)

```bash
# Generate client certificate
openssl req -newkey rsa:2048 -nodes -keyout client.key -out client.csr
openssl x509 -req -days 365 -in client.csr -signkey client.key -out client.crt

# Use with curl
curl -X POST https://localhost:8443 \
  --key client.key \
  --cert client.crt \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "ssl-test-001",
    "method": "tools/list"
  }'
```

### Authentication Configuration File

```yaml
# ~/.erlmcp/auth.yaml
servers:
  production:
    url: "https://api.erlmcp.com"
    auth:
      type: "api_key"
      key: "prod-api-key-123"
    ssl:
      cert_file: "/path/to/client.crt"
      key_file: "/path/to/client.key"

  development:
    url: "http://localhost:8080"
    auth:
      type: "none"
```

---

## Session Management

### Creating Sessions

```bash
# Create session via HTTP
curl -X POST http://localhost:8080/sessions \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "session-create-001",
    "method": "sessions/create",
    "params": {
      "ttl": 3600
    }
  }'
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "session-create-001",
  "result": {
    "sessionId": "session-12345",
    "createdAt": "2026-02-01T10:00:00Z",
    "ttl": 3600,
    "authToken": "session-token-abc123"
  }
}
```

### Using Sessions

```bash
# Session-aware tool calls
curl -X POST http://localhost:8080 \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer session-token-abc123" \
  -d '{
    "jsonrpc": "2.0",
    "id": "session-tool-call-001",
    "method": "tools/call",
    "params": {
      "name": "file_system",
      "arguments": {
        "operation": "write",
        "path": "/tmp/session-test.txt",
        "content": "Hello from session"
      }
    }
  }'
```

### Session Lifecycle Management

```javascript
class SessionManager {
  constructor(baseUrl) {
    this.baseUrl = baseUrl;
    this.currentSession = null;
    this.sessionTimeout = null;
  }

  async createSession(ttl = 3600) {
    const response = await fetch(`${this.baseUrl}/sessions`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: "session-create-001",
        method: "sessions/create",
        params: { ttl }
      })
    });

    const data = await response.json();
    if (data.error) {
      throw new Error(`Session creation failed: ${data.error.message}`);
    }

    this.currentSession = data.result;
    this.scheduleSessionRefresh(ttl);
    return this.currentSession;
  }

  async refreshSession() {
    if (!this.currentSession) return;

    const response = await fetch(`${this.baseUrl}/sessions/refresh`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.currentSession.authToken}`
      },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: "session-refresh-001",
        method: "sessions/refresh"
      })
    });

    if (response.ok) {
      const data = await response.json();
      this.currentSession = data.result;
      this.scheduleSessionRefresh(data.result.ttl);
    }
  }

  scheduleSessionRefresh(ttl) {
    if (this.sessionTimeout) {
      clearTimeout(this.sessionTimeout);
    }

    // Refresh 5 minutes before expiration
    const refreshTime = (ttl - 300) * 1000;
    this.sessionTimeout = setTimeout(() => {
      this.refreshSession();
    }, refreshTime);
  }

  async closeSession() {
    if (!this.currentSession) return;

    await fetch(`${this.baseUrl}/sessions/${this.currentSession.sessionId}`, {
      method: 'DELETE',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.currentSession.authToken}`
      }
    });

    if (this.sessionTimeout) {
      clearTimeout(this.sessionTimeout);
    }
    this.currentSession = null;
  }
}
```

---

## Resource Subscription

### Basic Resource Subscription

```bash
# Subscribe to a resource
curl -X POST http://localhost:8080/subscribe \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer your-token" \
  -d '{
    "jsonrpc": "2.0",
    "id": "subscribe-001",
    "method": "resources/subscribe",
    "params": {
      "uri": "file:///tmp/watch.txt",
      "events": ["updated", "created"],
      "ttl": 300
    }
  }'
```

### SSE-Based Resource Monitoring

```javascript
class ResourceMonitor {
  constructor(url, authToken) {
    this.url = url;
    this.authToken = authToken;
    this.eventSource = null;
    this.subscriptions = new Map();
  }

  subscribeToResource(uri, events, callback) {
    const subscriptionId = `sub-${Date.now()}`;

    // First, create the subscription
    fetch(`${this.url}/resources/subscribe`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.authToken}`
      },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: subscriptionId,
        method: "resources/subscribe",
        params: {
          uri,
          events,
          ttl: 300
        }
      })
    });

    // Then, set up SSE connection
    const sseUrl = `${this.url}/resources/subscribe/sse?subscription_id=${subscriptionId}`;
    this.eventSource = new EventSource(sseUrl);

    this.eventSource.onmessage = (event) => {
      const data = JSON.parse(event.data);
      callback(data, uri);
    };

    this.eventSource.onerror = (error) => {
      console.error('SSE error:', error);
      this.reconnect();
    };

    this.subscriptions.set(subscriptionId, { uri, events, callback });
    return subscriptionId;
  }

  unsubscribe(subscriptionId) {
    if (this.subscriptions.has(subscriptionId)) {
      fetch(`${this.url}/resources/subscribe/${subscriptionId}`, {
        method: 'DELETE',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${this.authToken}`
        }
      });

      this.subscriptions.delete(subscriptionId);
    }
  }

  reconnect() {
    if (this.eventSource) {
      this.eventSource.close();
    }

    // Re-subscribe to all resources
    this.subscriptions.forEach((sub, id) => {
      this.subscribeToResource(sub.uri, sub.events, sub.callback);
    });
  }

  close() {
    if (this.eventSource) {
      this.eventSource.close();
    }
    this.subscriptions.clear();
  }
}

// Usage
const monitor = new ResourceMonitor('http://localhost:8080', 'your-token');

monitor.subscribeToResource(
  'file:///tmp/log.txt',
  ['updated'],
  (event, uri) => {
    console.log(`Resource ${uri} updated:`, event.content);
  }
);
```

---

## Multi-Transport Integration

### Transport Selection Strategy

```javascript
class TransportManager {
  constructor(config) {
    this.config = config;
    this.transports = new Map();
    this.currentTransport = null;
  }

  async connect(transportType = 'auto') {
    if (transportType === 'auto') {
      transportType = this.detectBestTransport();
    }

    switch (transportType) {
      case 'stdio':
        await this.connectStdio();
        break;
      case 'tcp':
        await this.connectTCP();
        break;
      case 'http':
        await this.connectHTTP();
        break;
      case 'websocket':
        await this.connectWebSocket();
        break;
      case 'sse':
        await this.connectSSE();
        break;
      default:
        throw new Error(`Unsupported transport: ${transportType}`);
    }

    this.currentTransport = transportType;
    return transportType;
  }

  detectBestTransport() {
    // Heuristic for best transport selection
    if (typeof WebSocket !== 'undefined') {
      return 'websocket';
    } else if (typeof EventSource !== 'undefined') {
      return 'sse';
    } else {
      return 'http';
    }
  }

  async connectWebSocket() {
    return new Promise((resolve, reject) => {
      const ws = new WebSocket(this.config.websocket_url);

      ws.onopen = () => {
        this.transports.set('websocket', ws);
        resolve('websocket');
      };

      ws.onerror = (error) => {
        reject(new Error(`WebSocket connection failed: ${error}`));
      };
    });
  }

  async connectHTTP() {
    // HTTP fallback implementation
    this.transports.set('http', {
      post: async (path, data) => {
        const response = await fetch(`${this.config.http_url}${path}`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${this.config.auth_token}`
          },
          body: JSON.stringify(data)
        });
        return response.json();
      }
    });
    resolve('http');
  }

  async send(request, transport = null) {
    transport = transport || this.currentTransport;

    switch (transport) {
      case 'websocket':
        return this.sendWebSocket(request);
      case 'http':
        return this.sendHTTP(request);
      case 'sse':
        return this.sendSSE(request);
      default:
        throw new Error(`Unsupported transport: ${transport}`);
    }
  }
}
```

---

## Advanced Integration Patterns

### Circuit Breaker Pattern

```javascript
class CircuitBreaker {
  constructor(service, options = {}) {
    this.service = service;
    this.threshold = options.threshold || 5;
    this.timeout = options.timeout || 60000;
    this.recoveryTimeout = options.recoveryTimeout || 30000;

    this.state = 'CLOSED'; // CLOSED, OPEN, HALF_OPEN
    this.failureCount = 0;
    this.lastFailureTime = null;
  }

  async execute(request) {
    if (this.state === 'OPEN') {
      if (Date.now() - this.lastFailureTime > this.recoveryTimeout) {
        this.state = 'HALF_OPEN';
      } else {
        throw new Error('Service unavailable - circuit breaker open');
      }
    }

    try {
      const result = await this.service(request);
      this.onSuccess();
      return result;
    } catch (error) {
      this.onFailure();
      throw error;
    }
  }

  onSuccess() {
    this.failureCount = 0;
    this.state = 'CLOSED';
  }

  onFailure() {
    this.failureCount++;
    this.lastFailureTime = Date.now();

    if (this.failureCount >= this.threshold) {
      this.state = 'OPEN';
    }
  }
}

// Usage
const circuitBreaker = new CircuitBreaker(
  async (request) => {
    return await fetch('/api', {
      method: 'POST',
      body: JSON.stringify(request)
    });
  },
  { threshold: 3, timeout: 5000 }
);

try {
  const response = await circuitBreaker.execute(mcpRequest);
} catch (error) {
  console.error('Request failed:', error);
}
```

### Retry Pattern

```javascript
class RetryManager {
  constructor(options = {}) {
    this.maxAttempts = options.maxAttempts || 3;
    this.baseDelay = options.baseDelay || 1000;
    this.maxDelay = options.maxDelay || 10000;
    this.backoffFactor = options.backoffFactor || 2;
    this.retryableErrors = options.retryableErrors || [
      'connection_error',
      'timeout',
      'rate_limit_exceeded'
    ];
  }

  async execute(request, service) {
    let attempt = 0;
    let lastError;

    while (attempt < this.maxAttempts) {
      try {
        const result = await service(request);
        return result;
      } catch (error) {
        lastError = error;

        if (!this.shouldRetry(error)) {
          throw error;
        }

        attempt++;
        const delay = this.calculateDelay(attempt);

        await this.sleep(delay);
      }
    }

    throw lastError;
  }

  shouldRetry(error) {
    return this.retryableErrors.includes(error.code) ||
           this.retryableErrors.includes(error.message);
  }

  calculateDelay(attempt) {
    const delay = this.baseDelay * Math.pow(this.backoffFactor, attempt - 1);
    return Math.min(delay, this.maxDelay);
  }

  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// Usage
const retryManager = new RetryManager({
  maxAttempts: 5,
  baseDelay: 1000,
  retryableErrors: ['timeout', 'connection_error']
});

try {
  const result = await retryManager.execute(request, mcpService);
} catch (error) {
  console.error('All retry attempts failed:', error);
}
```

### Request Aggregation Pattern

```javascript
class RequestAggregator {
  constructor(batchSize = 10, batchTimeout = 1000) {
    this.batchSize = batchSize;
    this.batchTimeout = batchTimeout;
    this.pendingRequests = [];
    this.batchTimer = null;
  }

  async add(request) {
    return new Promise((resolve, reject) => {
      const item = { request, resolve, reject };

      this.pendingRequests.push(item);

      if (this.pendingRequests.length >= this.batchSize) {
        this.processBatch();
      } else if (!this.batchTimer) {
        this.batchTimer = setTimeout(() => {
          this.processBatch();
        }, this.batchTimeout);
      }
    });
  }

  async processBatch() {
    if (this.batchTimer) {
      clearTimeout(this.batchTimer);
      this.batchTimer = null;
    }

    const batch = this.pendingRequests;
    this.pendingRequests = [];

    try {
      const batchRequests = batch.map(item => item.request);
      const batchResponse = await this.sendBatch(batchRequests);

      batch.forEach((item, index) => {
        const response = batchResponse[index];
        if (response.error) {
          item.reject(response.error);
        } else {
          item.resolve(response);
        }
      });
    } catch (error) {
      batch.forEach(item => item.reject(error));
    }
  }

  async sendBatch(requests) {
    // Implement batch sending logic
    const promises = requests.map(req => this.sendRequest(req));
    return Promise.all(promises);
  }
}
```

---

## Monitoring & Observability

### OpenTelemetry Integration

```javascript
const { trace, context } = require('@opentelemetry/api');
const { NodeTracerProvider } = require('@opentelemetry/node');
const { Resource } = require('@opentelemetry/resources');
const { SemanticResourceAttributes } = require('@opentelemetry/semantic-conventions');

// Initialize OpenTelemetry
const provider = new NodeTracerProvider({
  resource: new Resource({
    [SemanticResourceAttributes.SERVICE_NAME]: 'erlmcp-client',
    [SemanticResourceAttributes.SERVICE_VERSION]: '1.0.0'
  })
});

provider.register();

const tracer = trace.getTracer('erlmcp-client');

class TracedMCPClient {
  constructor(baseUrl, authToken) {
    this.baseUrl = baseUrl;
    this.authToken = authToken;
  }

  async callTool(toolName, arguments) {
    const span = tracer.startSpan('mcp-tool-call');

    try {
      span.setAttribute('tool.name', toolName);
      span.setAttribute('tool.arguments', JSON.stringify(arguments));

      const response = await fetch(`${this.baseUrl}/tools/call`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'Authorization': `Bearer ${this.authToken}`
        },
        body: JSON.stringify({
          jsonrpc: "2.0",
          id: span.spanContext().traceId,
          method: "tools/call",
          params: {
            name: toolName,
            arguments
          }
        })
      });

      const data = await response.json();

      if (data.error) {
        span.recordException(new Error(data.error.message));
        span.setStatus({ code: trace.StatusCode.ERROR });
        throw data.error;
      }

      span.setAttribute('response.success', true);
      return data.result;
    } finally {
      span.end();
    }
  }
}
```

### Metrics Collection

```javascript
class MetricsCollector {
  constructor() {
    this.counters = new Map();
    this.gauges = new Map();
    this.histograms = new Map();
  }

  incrementCounter(name, tags = {}) {
    const key = this.getKey(name, tags);
    const current = this.counters.get(key) || 0;
    this.counters.set(key, current + 1);
  }

  setGauge(name, value, tags = {}) {
    const key = this.getKey(name, tags);
    this.gauges.set(key, value);
  }

  recordHistogram(name, value, tags = {}) {
    const key = this.getKey(name, tags);
    const values = this.histograms.get(key) || [];
    values.push(value);
    this.histograms.set(key, values);
  }

  getKey(name, tags) {
    const tagString = Object.entries(tags)
      .sort(([a], [b]) => a.localeCompare(b))
      .map(([k, v]) => `${k}=${v}`)
      .join(',');
    return `${name}${tagString ? `|${tagString}` : ''}`;
  }

  getMetrics() {
    return {
      counters: Object.fromEntries(this.counters),
      gauges: Object.fromEntries(this.gauges),
      histograms: Object.fromEntries(
        Array.from(this.histograms.entries()).map(([key, values]) => [
          key,
          {
            count: values.length,
            sum: values.reduce((a, b) => a + b, 0),
            avg: values.reduce((a, b) => a + b, 0) / values.length,
            min: Math.min(...values),
            max: Math.max(...values)
          }
        ])
      )
    };
  }
}

// Usage with MCP client
const metrics = new MetricsCollector();

class InstrumentedMCPClient {
  constructor(baseUrl, authToken) {
    this.baseUrl = baseUrl;
    this.authToken = authToken;
    this.metrics = metrics;
  }

  async callTool(toolName, arguments) {
    const startTime = Date.now();
    this.metrics.incrementCounter('mcp.calls', { tool: toolName });

    try {
      const response = await fetch(`${this.baseUrl}/tools/call`, {
        method: 'POST',
        headers: {
          'Content-Type': application/json',
          'Authorization': `Bearer ${this.authToken}`
        },
        body: JSON.stringify({
          jsonrpc: "2.0",
          id: "tool-call-001",
          method: "tools/call",
          params: {
            name: toolName,
            arguments
          }
        })
      });

      const duration = Date.now() - startTime;
      this.metrics.recordHistogram('mcp.call.duration', duration, { tool: toolName });
      this.metrics.setGauge('mcp.last_call_duration', duration);

      if (!response.ok) {
        this.metrics.incrementCounter('mcp.errors', { tool: toolName, status: response.status });
        throw new Error(`HTTP ${response.status}`);
      }

      const data = await response.json();

      if (data.error) {
        this.metrics.incrementCounter('mcp.errors', { tool: toolName, error: data.error.code });
        throw data.error;
      }

      this.metrics.incrementCounter('mcp.success', { tool: toolName });
      return data.result;
    } catch (error) {
      this.metrics.incrementCounter('mcp.errors', { tool: toolName, error: error.message });
      throw error;
    }
  }
}
```

This integration guide provides comprehensive coverage of erlmcp CLI integration patterns. For specific use cases or advanced scenarios, refer to the examples section or consult the API reference.