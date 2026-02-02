# ERLMCP Configuration Guide

This guide covers all configuration options for erlmcp CLI, including environment variables, configuration files, transport settings, OTEL integration, and security configurations.

## Table of Contents

1. [Environment Variables](#environment-variables)
2. [Configuration Files](#configuration-files)
3. [Transport-Specific Settings](#transport-specific-settings)
4. [OTEL Integration Settings](#otel-integration-settings)
5. [Security Configurations](#security-configurations)
6. [Profile Management](#profile-management)
7. [Configuration Validation](#configuration-validation)

---

## Environment Variables

### Core Configuration

```bash
# Basic connection settings
export ERLMCP_HOST="localhost"
export ERLMCP_PORT="8080"
export ERLMCP_TRANSPORT="tcp"

# Authentication
export ERLMCP_API_KEY="your-api-key-here"
export ERLMCP_AUTH_TYPE="bearer_token"

# Session management
export ERLMCP_SESSION_TTL="3600"
export ERLMCP_MAX_REQUESTS="1000"

# Logging
export ERLMCP_LOG_LEVEL="info"
export ERLMCP_LOG_FORMAT="json"

# Performance tuning
export ERLMCP_TIMEOUT="30000"
export ERLMCP_MAX_RETRIES="3"
export ERLMCP_POOL_SIZE="10"
```

### Development Environment

```bash
# Development mode settings
export ERLMCP_ENVIRONMENT="development"
export ERLMCP_DEBUG="true"
export ERLMCP_TRACE_ENABLED="true"
export ERLMCP_METRICS_ENABLED="true"

# Testing configuration
export ERLMCP_TEST_MODE="true"
export ERLMCP_TEST_TIMEOUT="5000"
export ERLMCP_TEST_VERBOSE="true"
```

### Production Environment

```bash
# Production settings
export ERLMCP_ENVIRONMENT="production"
export ERLMCP_RATE_LIMIT="1000"
export ERLMCP_CIRCUIT_BREAKER_ENABLED="true"

# Security
export ERLMCP_SSL_ENABLED="true"
export ERLMCP_SSL_CERT_FILE="/path/to/cert.pem"
export ERLMCP_SSL_KEY_FILE="/path/to/key.pem"

# Monitoring
export ERLMCP_HEALTH_CHECK_ENABLED="true"
export ERLMCP_HEALTH_CHECK_INTERVAL="30"
```

### Complete Environment Variable Reference

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `ERLMCP_HOST` | string | "localhost" | Server host address |
| `ERLMCP_PORT` | integer | 8080 | Server port number |
| `ERLMCP_TRANSPORT` | string | "tcp" | Transport type (stdio, tcp, http, ws, sse) |
| `ERLMCP_API_KEY` | string | - | API key for authentication |
| `ERLMCP_AUTH_TYPE` | string | "none" | Authentication type (none, bearer_token, jwt, cert) |
| `ERLMCP_SESSION_TTL` | integer | 3600 | Session time-to-live in seconds |
| `ERLMCP_LOG_LEVEL` | string | "info" | Log level (debug, info, warn, error) |
| `ERLMCP_TIMEOUT` | integer | 30000 | Request timeout in milliseconds |
| `ERLMCP_MAX_RETRIES` | integer | 3 | Maximum retry attempts |
| `ERLMCP_POOL_SIZE` | integer | 10 | Connection pool size |
| `ERLMCP_RATE_LIMIT` | integer | 1000 | Requests per second limit |
| `ERLMCP_SSL_ENABLED` | boolean | false | Enable SSL/TLS |
| `ERLMCP_SSL_CERT_FILE` | string | - | SSL certificate file path |
| `ERLMCP_SSL_KEY_FILE` | string | - | SSL private key file path |
| `ERLMCP_OTEL_ENABLED` | boolean | false | Enable OpenTelemetry |
| `ERLMCP_OTEL_EXPORTER` | string | "console" | OTEL exporter type (console, jaeger, zipkin) |

---

## Configuration Files

### Global Configuration (`~/.erlmcp/config.yaml`)

```yaml
# Global settings for all erlmcp instances
version: "1.0"
environment: "production"
logging:
  level: "info"
  format: "json"
  file: "/var/log/erlmcp.log"
  rotation:
    max_size: "100MB"
    max_files: 5

transport:
  default: "tcp"
  tcp:
    host: "localhost"
    port: 8080
    pool_size: 10
    timeout: 30000
  http:
    host: "localhost"
    port: 8081
    ssl: true
    headers:
      "User-Agent": "erlmcp-client/1.0"
  websocket:
    url: "ws://localhost:8080"
    reconnect: true
    max_reconnect_attempts: 5

auth:
  type: "bearer_token"
  api_key: "${ERLMCP_API_KEY}"
  jwt:
    secret: "${ERLMCP_JWT_SECRET}"
    algorithm: "HS256"
  cert:
    cert_file: "/path/to/client.crt"
    key_file: "/path/to/client.key"

sessions:
  default_ttl: 3600
  max_requests: 1000
  timeout: 30000

performance:
  timeout: 30000
  max_retries: 3
  retry_delay: 1000
  circuit_breaker:
    enabled: true
    threshold: 0.8
    timeout: 60000
    recovery_timeout: 30000

security:
  ssl_enabled: true
  allowed_origins: ["*"]
  rate_limiting:
    enabled: true
    limit: 1000
    window: 60
```

### Project-Specific Configuration (`./erlmcp.yaml`)

```yaml
# Project-specific configuration
version: "1.0"
name: "my-project"
description: "My erlmcp project"

# Override global settings for this project
environment: "development"
transport:
  default: "http"
  http:
    host: "localhost"
    port: 8081
    ssl: false

# Project-specific tools
tools:
  - name: "calculator"
    endpoint: "http://localhost:8081/tools/calculator"
  - name: "file_system"
    endpoint: "http://localhost:8081/tools/filesystem"

# Custom resources
resources:
  - uri: "file:///tmp/project-data"
    name: "project_data"
    description: "Project data directory"
  - uri: "file:///tmp/logs"
    name: "project_logs"
    description: "Project logs directory"

# Development-specific settings
development:
  debug: true
  trace_enabled: true
  metrics_enabled: true
  profiling_enabled: true
```

### Environment-Specific Configuration

#### Development (`erlmcp-dev.yaml`)
```yaml
environment: "development"
logging:
  level: "debug"
  format: "text"
  console: true

transport:
  default: "stdio"
  stdio:
    encoding: "utf8"

auth:
  type: "none"

performance:
  timeout: 10000
  max_retries: 1
```

#### Production (`erlmcp-prod.yaml`)
```yaml
environment: "production"
logging:
  level: "info"
  format: "json"
  file: "/var/log/erlmcp/erlmcp.log"
  rotation:
    max_size: "100MB"
    max_files: 5

transport:
  default: "tcp"
  tcp:
    host: "prod-api.erlmcp.com"
    port: 443
    ssl: true
    pool_size: 50
    timeout: 30000

auth:
  type: "bearer_token"
  api_key: "${ERLMCP_PROD_API_KEY}"

security:
  ssl_enabled: true
  allowed_origins: ["https://myapp.com"]
  rate_limiting:
    enabled: true
    limit: 500
    window: 60

performance:
  timeout: 30000
  max_retries: 5
  circuit_breaker:
    enabled: true
    threshold: 0.8
    timeout: 60000
```

---

## Transport-Specific Settings

### TCP Transport Configuration

```yaml
transport:
  tcp:
    # Basic connection settings
    host: "localhost"
    port: 8080

    # Connection pooling
    pool_size: 10
    max_overflow: 20
    idle_timeout: 30000

    # Socket options
    socket_opts:
      backlog: 128
      nodelay: true
      reuseaddr: true
      keepalive: true
      packet_size: 65536

    # Timeout settings
    connect_timeout: 5000
    send_timeout: 30000
    receive_timeout: 30000

    # Security
    ssl: false
    ssl_opts:
      verify: true
      fail_if_no_peer_cert: false
      secure_renegotiate: true
      server_name_indication: "localhost"
```

### HTTP Transport Configuration

```yaml
transport:
  http:
    # Basic settings
    host: "localhost"
    port: 8081
    ssl: true

    # SSL configuration
    ssl_opts:
      certfile: "/path/to/client.crt"
      keyfile: "/path/to/client.key"
      cacertfile: "/path/to/ca.crt"
      verify: verify_none
      secure_renegotiate: true

    # HTTP headers
    headers:
      "User-Agent": "erlmcp-client/1.0"
      "Accept": "application/json"
      "Content-Type": "application/json"
      "X-Request-ID": "${uuid}"

    # Timeout settings
    timeout: 30000
    connect_timeout: 5000
    read_timeout: 30000

    # Authentication
    auth:
      type: "bearer_token"
      token: "${ERLMCP_API_KEY}"

    # Proxy settings
    proxy:
      enabled: false
      host: "proxy.company.com"
      port: 8080
      username: "${PROXY_USERNAME}"
      password: "${PROXY_PASSWORD}"
```

### WebSocket Transport Configuration

```yaml
transport:
  websocket:
    # Connection settings
    url: "ws://localhost:8080/mcp"

    # Headers
    headers:
      "Authorization": "Bearer ${ERLMCP_API_KEY}"
      "User-Agent": "erlmcp-client/1.0"

    # Reconnection settings
    reconnect:
      enabled: true
      max_attempts: 5
      delay: 1000
      max_delay: 30000

    # Protocol settings
    protocols: ["json-rpc-2.0"]
    subprotocols: []

    # Event handling
    events:
      open: "onWebSocketOpen"
      message: "onWebSocketMessage"
      error: "onWebSocketError"
      close: "onWebSocketClose"

    # Timeout settings
    connect_timeout: 5000
    ping_timeout: 30000
```

### SSE Transport Configuration

```yaml
transport:
  sse:
    # Base URL
    url: "http://localhost:8080/stream"

    # Headers
    headers:
      "Authorization": "Bearer ${ERLMCP_API_KEY}"

    # Event stream settings
    events:
      "tool_updates": "handleToolUpdate"
      "resource_changes": "handleResourceChange"
      "session_events": "handleSessionEvent"

    # Connection settings
    reconnect:
      enabled: true
      retry_delay: 1000
      max_retry_delay: 30000

    # Buffer settings
    buffer_size: 1000
    max_message_size: 1048576

    # Heartbeat
    heartbeat:
      enabled: true
      interval: 30000
      timeout: 5000
```

---

## OTEL Integration Settings

### Basic OTEL Configuration

```yaml
observability:
  opentelemetry:
    enabled: true
    service_name: "erlmcp-client"
    service_version: "1.0.0"
    namespace: "my-company"

    # Resource attributes
    resource_attributes:
      host.name: "${HOSTNAME}"
      instance.id: "${INSTANCE_ID}"
      deployment.environment: "${ERLMCP_ENVIRONMENT}"

    # Tracer configuration
    tracer:
      sampler: "always_on"
      attributes:
        "erlmcp.tool.name": "${tool_name}"
        "erlmcp.tool.arguments": "${arguments}"
        "erlmcp.response.status": "${response_status}"

    # Meter configuration
    meter:
      instruments:
        - name: "erlmcp.calls.count"
          type: "counter"
          description: "Number of MCP calls"
        - name: "erlmcp.calls.duration"
          type: "histogram"
          description: "Duration of MCP calls"

    # Exporter configuration
    exporter:
      type: "otlp"  # otlp, jaeger, zipkin, console
      otlp:
        endpoint: "http://localhost:4317"
        headers:
          "Authorization": "Bearer ${OTEL_AUTH_TOKEN}"
        timeout: 10000
        compression: "gzip"

    # Batch processor configuration
    batch:
      max_export_timeout: 5000
      max_export_size: 512
      max_queue_size: 2048
```

### Jaeger Integration

```yaml
observability:
  opentelemetry:
    enabled: true
    exporter:
      type: "jaeger"
      jaeger:
        agent_host: "localhost"
        agent_port: 6831
        service_name: "erlmcp-client"
        tags:
          version: "1.0.0"
          environment: "production"
```

### Zipkin Integration

```yaml
observability:
  opentelemetry:
    enabled: true
    exporter:
      type: "zipkin"
      zipkin:
        endpoint: "http://localhost:9411/api/v2/spans"
        service_name: "erlmcp-client"
        timeout: 5000
```

---

## Security Configurations

### Authentication Configuration

```yaml
security:
  authentication:
    # Bearer token authentication
    bearer_token:
      enabled: true
      token: "${ERLMCP_API_KEY}"
      header: "Authorization"
      prefix: "Bearer"

    # JWT authentication
    jwt:
      enabled: false
      secret: "${ERLMCP_JWT_SECRET}"
      algorithm: "HS256"
      issuer: "erlmcp"
      audience: "erlmcp-clients"
      expiration: 3600

    # Certificate authentication
    certificate:
      enabled: false
      cert_file: "/path/to/client.crt"
      key_file: "/path/to/client.key"
      ca_file: "/path/to/ca.crt"
      verify_peer: true

    # API key authentication
    api_key:
      enabled: false
      header: "X-API-Key"
      key: "${ERLMCP_API_KEY}"

# Rate limiting configuration
security:
  rate_limiting:
    enabled: true
    limit: 1000  # requests per minute
    window: 60   # seconds
    burst: 50    # burst limit
    storage: "memory"  # memory, redis

    # Rate limiting rules
    rules:
      - path: "/tools/*"
        limit: 1000
        window: 60
      - path: "/resources/*"
        limit: 500
        window: 60
      - path: "/sessions/*"
        limit: 100
        window: 60
```

### SSL/TLS Configuration

```yaml
security:
  ssl:
    enabled: true
    # Server certificate validation
    verify:
      enabled: true
      depth: 3
      fail_if_no_peer_cert: true
      fail_if_peer_cert_invalid: true

    # SSL options
    ssl_opts:
      versions: [tlsv1.2, tlsv1.3]
      ciphers: [
        "ECDHE-ECDSA-AES256-GCM-SHA384",
        "ECDHE-RSA-AES256-GCM-SHA384",
        "ECDHE-ECDSA-CHACHA20-POLY1305",
        "ECDHE-RSA-CHACHA20-POLY1305"
      ]
      secure_renegotiate: true
      server_name_indication: "localhost"
      hibernate_after: 300000  # 5 minutes

    # Certificate management
    certificates:
      # Client certificate
      client:
        cert_file: "/path/to/client.crt"
        key_file: "/path/to/client.key"
        password: "${CERT_PASSWORD}"

      # CA certificates
      ca:
        cert_file: "/path/to/ca.crt"
        cache_size: 1000

      # Certificate validation
      validation:
        enabled: true
        cache_duration: 3600
        ocsp:
          enabled: true
          timeout: 10000
```

### Security Headers and CORS

```yaml
security:
  headers:
    enabled: true
    custom_headers:
      "X-Content-Type-Options": "nosniff"
      "X-Frame-Options": "DENY"
      "X-XSS-Protection": "1; mode=block"
      "Strict-Transport-Security": "max-age=31536000; includeSubDomains"
      "Content-Security-Policy": "default-src 'self'"

  cors:
    enabled: true
    allowed_origins:
      - "https://myapp.com"
      - "https://api.myapp.com"
    allowed_methods: ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    allowed_headers: ["Content-Type", "Authorization", "X-API-Key"]
    exposed_headers: ["X-Request-ID", "X-Rate-Limit"]
    allow_credentials: true
    max_age: 3600
    options_passthrough: false
```

---

## Profile Management

### Built-in Profiles

#### Development Profile
```yaml
profiles:
  development:
    description: "Development configuration with debug features"
    environment: "development"
    logging:
      level: "debug"
      format: "text"
    transport:
      default: "stdio"
    auth:
      type: "none"
    performance:
      timeout: 10000
      max_retries: 1
    security:
      rate_limiting:
        enabled: false
```

#### Production Profile
```yaml
profiles:
  production:
    description: "Production configuration with performance optimizations"
    environment: "production"
    logging:
      level: "info"
      format: "json"
      file: "/var/log/erlmcp.log"
    transport:
      default: "tcp"
      tcp:
        host: "prod-api.erlmcp.com"
        port: 443
        ssl: true
        pool_size: 50
    auth:
      type: "bearer_token"
      api_key: "${ERLMCP_PROD_API_KEY}"
    security:
      rate_limiting:
        enabled: true
        limit: 500
      circuit_breaker:
        enabled: true
        threshold: 0.8
```

#### Government Profile
```yaml
profiles:
  government:
    description: "Government compliance configuration"
    environment: "production"
    logging:
      level: "warn"
      format: "json"
      audit: true
    transport:
      default: "tcp"
      tcp:
        host: "gov-api.erlmcp.gov"
        port: 8443
        ssl: true
    auth:
      type: "certificate"
      certificate:
        cert_file: "/path/to/gov-cert.crt"
        key_file: "/path/to/gov-key.key"
    security:
      rate_limiting:
        enabled: true
        limit: 100
      audit_logging:
        enabled: true
        include_headers: true
        include_body: false  # Redact sensitive data
```

### Custom Profile Configuration

```yaml
# Custom project profile
profiles:
  my-project:
    extends: "production"
    overrides:
      transport:
        http:
          host: "localhost"
          port: 8081
          ssl: false
      auth:
        type: "api_key"
        api_key: "${ERLMCP_PROJECT_API_KEY}"
      tools:
        - name: "project_calculator"
          endpoint: "http://localhost:8081/tools/project-calculator"
        - name: "project_data"
          endpoint: "http://localhost:8081/tools/project-data"
```

---

## Configuration Validation

### Configuration File Validation

```bash
# Validate configuration syntax
erlmcp config validate --file ./erlmcp.yaml

# Validate configuration with test connection
erlmcp config validate --file ./erlmcp.yaml --test-connection

# Validate configuration with specific profile
erlmcp config validate --file ./erlmcp.yaml --profile production
```

### Configuration Validation Commands

```bash
# Check configuration syntax
erlmcp config check

# Show current configuration
erlmcp config show

# Export configuration
erlmcp config export --format json > config.json

# Import configuration
erlmcp config import --file config.json

# Test configuration
erlmcp config test --profile production
```

### Configuration Error Handling

```yaml
# Example of invalid configuration
invalid_config:
  transport:
    tcp:
      host: "localhost"
      port: "invalid_port"  # Should be integer
```

**Validation Error:**
```
Configuration validation failed:
- transport.tcp.port: Invalid port number "invalid_port", expected integer
- Field transport.tcp.ssl should be boolean, got string
```

### Configuration Testing

```bash
# Test configuration with actual server
erlmcp config test --config ./erlmcp.yaml --endpoint http://localhost:8080

# Test configuration with sample requests
erlmcp config test --config ./erlmcp.yaml --requests sample-requests.json

# Test configuration with performance load
erlmcp config test --config ./erlmcp.yaml --load-test --duration 60
```

### Configuration Monitoring

```yaml
# Configuration monitoring settings
monitoring:
  config:
    enabled: true
    check_interval: 60
    alert_threshold: 5

    # Configuration drift detection
    drift_detection:
      enabled: true
      check_interval: 300
      alert_on_change: true

    # Configuration validation
    validation:
      enabled: true
      check_interval: 3600
      alert_on_failure: true
```

This configuration guide provides comprehensive coverage of all erlmcp CLI configuration options. For specific use cases or advanced scenarios, refer to the examples section or consult the API reference.