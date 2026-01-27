# Load Balancer Setup & 100K Connection Distribution

## Overview

This document describes the production-grade load balancer configuration for distributing 100,000 concurrent connections across a 4-node erlmcp cluster. The setup includes HAProxy and Nginx configurations with automatic health checks, sticky sessions, and failover support.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    100K Concurrent Clients              │
└──────────────────────────┬──────────────────────────────┘
                           │
            ┌──────────────┴──────────────┐
            │   Load Balancer Layer       │
            │  (HAProxy or Nginx)         │
            │  Port 8000/8080/8443        │
            └──────────────┬──────────────┘
                           │
        ┌──────────────────┼──────────────────┐
        │                  │                  │
    ┌───▼────┐         ┌───▼────┐        ┌───▼────┐        ┌────────┐
    │erlmcp1 │         │erlmcp2 │        │erlmcp3 │        │erlmcp4 │
    │9001/  │         │9002/   │        │9003/   │        │9004/   │
    │8080   │         │8081    │        │8082    │        │8083    │
    │25K    │         │25K     │        │25K     │        │25K     │
    │conn   │         │conn    │        │conn    │        │conn    │
    └────────┘         └────────┘        └────────┘        └────────┘
```

## Load Balancer Types

### HAProxy Configuration

**File:** `config/haproxy.cfg`

HAProxy is the primary load balancer. It provides:
- TCP-level connection distribution (for non-HTTP MCP protocol)
- HTTP request distribution with header processing
- WebSocket connection upgrading
- Server-Sent Events (SSE) streaming support
- Connection-aware health checks (TCP + HTTP)
- Sticky sessions using source IP or cookies
- Per-IP rate limiting (100 connections/sec)
- Per-IP message limiting (100 messages/sec)

**Key Features:**
```
Frontend mcp_frontend_tcp (port 8000)
  - Round-robin distribution
  - Sticky sessions on source IP
  - Connection limiting per IP (100)
  - Message rate limiting

Frontend mcp_frontend_http (port 8080)
  - HTTP/1.1 with keep-alive
  - WebSocket upgrade detection
  - SSE streaming support
  - Cookie-based sticky sessions
  - HTTP compression

Backend Nodes:
  - erlmcp1: localhost:9001 (TCP), localhost:8080 (HTTP)
  - erlmcp2: localhost:9002 (TCP), localhost:8081 (HTTP)
  - erlmcp3: localhost:9003 (TCP), localhost:8082 (HTTP)
  - erlmcp4: localhost:9004 (TCP), localhost:8083 (HTTP)

Health Checks:
  - Interval: 3 seconds
  - Rise threshold: 2 successful checks
  - Fall threshold: 2 failed checks
  - Timeout: 3 seconds

Stats Interface: http://127.0.0.1:8404/stats
```

### Nginx Configuration

**File:** `config/nginx.conf`

Nginx provides an alternative load balancer with:
- Connection pooling and keep-alive
- IP hash-based sticky sessions
- Least connections algorithm for distribution
- Per-IP rate limiting
- WebSocket support with long timeouts
- SSE streaming support

**Key Features:**
```
Upstreams:
  - erlmcp_tcp_cluster: IP hash, least connections
  - erlmcp_http_cluster: IP hash, least connections
  - erlmcp_websocket_cluster: IP hash with long timeout
  - erlmcp_sse_cluster: IP hash with long timeout

Rate Limiting Zones:
  - http_limit: 100 req/s per IP
  - ws_limit: 100 req/s per IP
  - sse_limit: 100 req/s per IP

Servers:
  - HTTP: Port 8000
  - WebSocket: Port 8001 (/mcp/ws)
  - SSE: Port 8002 (/mcp/sse)
  - Health: Port 8888 (/health, /nginx_status)
  - HTTPS: Port 8443 (optional TLS termination)

Keep-Alive:
  - 256 connections per upstream
  - 100 requests per connection
  - 60 second timeout
```

## Quick Start

### 1. Build the Project

```bash
cd /Users/sac/erlmcp
rebar3 compile
```

### 2. Start the Cluster

```bash
# Start all 4 erlmcp nodes
./scripts/start_cluster.sh start

# Verify nodes are running
./scripts/start_cluster.sh status
```

Output:
```
  erlmcp1: UP (TCP:9001)
  erlmcp2: UP (TCP:9002)
  erlmcp3: UP (TCP:9003)
  erlmcp4: UP (TCP:9004)
```

### 3. Start the Load Balancer

```bash
# Start HAProxy (recommended)
./scripts/start_load_balancer.sh haproxy

# Or start Nginx
./scripts/start_load_balancer.sh nginx

# Check status
./scripts/start_load_balancer.sh status
```

### 4. Run Load Test

```bash
# Test with 100K connections for 60 seconds
./scripts/load_test_100k.sh 100000 60

# Or smaller test
./scripts/load_test_100k.sh 25000 30
```

### 5. Monitor in Real-time

```bash
# Continuous health monitoring (updates every 5 seconds)
./scripts/health_check.sh --continuous 5

# Or single check
./scripts/health_check.sh
```

### 6. View Statistics

**HAProxy Stats Dashboard:**
```
http://127.0.0.1:8404/stats
```

Shows:
- Active connections per backend
- Bytes in/out
- Sessions
- Health status
- Response times

**Nginx Status:**
```
http://127.0.0.1:8888/nginx_status
```

Shows:
- Active connections
- Accepted connections
- Handled connections
- Requests
- Reading/Writing/Waiting

## Connection Distribution

### Target Distribution

For 100K concurrent connections across 4 nodes:
- **Per-node target:** 25,000 connections
- **Acceptable variance:** ±500 connections (±2%)
- **Maximum acceptable:** 20,000 - 30,000 per node

### How Sticky Sessions Work

**HAProxy (Source IP Hash):**
```
1. Client connects from IP 192.168.1.100
2. Hash(192.168.1.100) = "node2"
3. Connection routed to erlmcp2
4. All subsequent requests from same IP go to erlmcp2
5. If erlmcp2 fails, connection is redirected to next healthy node
```

**HAProxy (Cookie-based):**
```
HTTP Response includes: Set-Cookie: SERVERID=erlmcp2
Subsequent requests include Cookie: SERVERID=erlmcp2
All requests go to the specified node
```

**Nginx (IP Hash):**
```
1. Client IP hashed using CRC32
2. Hash result mapped to upstream server
3. Same client always gets same upstream
4. Deterministic mapping based on client IP
```

### Measuring Distribution

```bash
# Check current connection distribution
./scripts/health_check.sh

# Output shows per-node connection count:
erlmcp1: 25234 connections ✓ (target: 25000 ± 500)
erlmcp2: 24987 connections ✓
erlmcp3: 25156 connections ✓
erlmcp4: 24623 connections ✓
Total: 100000 / 100000 (100%)
Variance: 2.4% (target: <5%)
```

## Health Checks

### How Health Checks Work

**HAProxy TCP Check (3 second interval):**
```
1. HAProxy opens TCP connection to 127.0.0.1:9001 (erlmcp1)
2. If connection succeeds within 3 seconds → server is UP
3. If connection fails, increment failure counter
4. After 2 consecutive failures → mark server DOWN
5. Stop routing connections to DOWN server
6. When server comes back UP (2 consecutive successes) → resume routing
```

**Response:**
```
After 3 seconds DOWN → 6 seconds to re-enable (2 × 3s interval)
```

### Configuring Health Checks

In `config/haproxy.cfg`:
```erlang
% Change check interval (milliseconds)
default-server inter 3000  % Check every 3 seconds

% Change failure threshold
default-server fall 2      % 2 failures to mark DOWN
default-server rise 2      % 2 successes to mark UP

% Change timeout
timeout check 3000         % 3 second timeout per check
```

## Failover Behavior

### Automatic Failover (< 5 seconds)

**Scenario:** Node erlmcp1 crashes

```
Timeline:
T=0s:    Node erlmcp1 crashes
T=3s:    HAProxy detects first failed health check → failure counter = 1
T=6s:    HAProxy detects second failed health check → mark as DOWN
T=6s+:   All new connections routed to remaining 3 nodes
         Existing connections on erlmcp1 are closed
         Clients reconnect to erlmcp2/3/4

Total failover time: ~6 seconds (two 3-second check intervals)
```

### Connection Draining

When a node is marked DOWN:
1. New connections are immediately redirected
2. Existing connections are closed gracefully
3. Clients receive connection reset
4. Clients should reconnect to load balancer
5. New connections land on healthy nodes

### Manual Node Drain

To gracefully drain a node:

```bash
# HAProxy: Mark node as "Drain" in stats
# This tells HAProxy:
#   - Don't send NEW connections
#   - Finish existing connections
#   - Then take node DOWN

# In web interface (stats page):
#   1. Go to http://127.0.0.1:8404/stats
#   2. Click "Disable" next to erlmcp1
#   3. Wait for connections to drain
#   4. Click "Drain" to prevent new connections
#   5. Stop the node when drain complete
```

## Performance Tuning

### System Limits

Before running 100K connections, ensure system limits are sufficient:

```bash
# Check current limits
ulimit -n              # File descriptors
ulimit -u              # Max processes
netstat -an | wc -l    # Current connections

# Increase file descriptor limit (Linux)
ulimit -n 200000

# Or permanently in /etc/security/limits.conf
*    soft    nofile    200000
*    hard    nofile    200000
```

### HAProxy Tuning

In `config/haproxy.cfg`:

```
global
    maxconn 100000              % Total connections allowed
    tune.maxconn 100000         % Tuning hint

defaults
    timeout connect 5000        % Client -> HAProxy
    timeout client 300000       % Client idle timeout (5 min)
    timeout server 300000       % Server idle timeout (5 min)
    timeout queue 30000         % Wait in queue timeout

backend mcp_cluster_tcp
    option contstcp             % Keep connection open during check
    server erlmcp1 ... maxconn 25000  % Per-server limit
```

### Nginx Tuning

In `config/nginx.conf`:

```nginx
events {
    worker_connections 100000;  % Per-worker connection limit
    use epoll;                  % Efficient event loop (Linux)
    multi_accept on;            % Accept multiple connections
}

http {
    keepalive_timeout 300s;     % Connection keep-alive
    keepalive_requests 1000;    % Requests per connection

    upstream erlmcp_tcp_cluster {
        keepalive 256;          % Pool size
        keepalive_requests 100; % Requests per pool connection
        keepalive_timeout 60s;  % Pool connection timeout
    }
}
```

## Latency Analysis

### Expected Latencies

With proper configuration:
```
TCP Connection Establishment: ~1-2ms
Request Processing:            ~5-10ms
Total Round-trip:              ~6-12ms
99th Percentile:               <20ms
```

### Measuring Latency

```bash
# HAProxy - check response times in stats
curl http://127.0.0.1:8404/stats | grep "rtime"

# Manual latency test
for i in {1..100}; do
    curl -w "%{time_total}\n" -o /dev/null -s http://127.0.0.1:8000/mcp
done | awk '{sum+=$1; sumsq+=$1*$1}
    END {
        avg=sum/NR;
        stddev=sqrt(sumsq/NR - avg*avg);
        print "Avg:", avg*1000 "ms";
        print "StdDev:", stddev*1000 "ms"
    }'
```

### Latency Variance

Acceptable variance targets:
```
<5%:   Excellent (all nodes similar latency)
5-10%: Good (acceptable variance)
10-20%: Warning (investigate hot spot)
>20%:  Critical (node might be overloaded)
```

## Troubleshooting

### Connections Not Balanced

**Symptom:** One node has 40K connections, others have 15K each

**Causes & Solutions:**

1. **Sticky sessions too aggressive**
   ```erlang
   % Reduce session persistence
   % In haproxy.cfg:
   stick-table type ip size 100k expire 10m  % Was 30m
   ```

2. **Client distribution skewed**
   ```bash
   # Check source IP distribution
   netstat -an | grep :8000 | awk '{print $5}' | \
     awk -F: '{print $1}' | sort | uniq -c | sort -rn | head -10
   # If few IPs dominate, that's expected
   ```

3. **Load balancer algorithm**
   ```erlang
   % Try different algorithm
   # In haproxy.cfg:
   balance roundrobin      % Round-robin
   balance source          % Source IP hash
   balance leastconn       % Least connections
   balance uri             % URI hash
   ```

### Failover Takes Too Long

**Symptom:** Takes >10 seconds for failed node to be removed

**Solution:** Reduce health check interval

```erlang
% In haproxy.cfg:
default-server inter 1000 fall 1 rise 1  % Check every 1s, fail after 1 check
% WARNING: May cause false positives

% Better balance:
default-server inter 2000 fall 2 rise 2  % Check every 2s, fail after 2 checks
```

### High Latency

**Symptoms:** p99 latency >100ms

**Diagnosis:**
```bash
# Check if nodes are overloaded
./scripts/health_check.sh

# Check system resources
ps aux | grep erl    % CPU/Memory
netstat -an | wc -l  % Total connections
ss -s                % TCP stats
```

**Solutions:**
1. Increase node count (add more nodes)
2. Reduce per-node limit in load balancer
3. Investigate slow queries on nodes

### Memory Pressure

**Symptoms:** erlmcp nodes slowly consuming memory, latency degradation

**Check:**
```bash
# Monitor memory over time
watch -n 1 'ps aux | grep erl | grep -v grep'

# Check Erlang memory
rcon -sname erlmcp1 -batch 'erlang:memory().'
```

**Solutions:**
1. Enable garbage collection tuning
2. Reduce message queue limits
3. Implement message batching

## Monitoring & Alerts

### Key Metrics to Monitor

1. **Connection Distribution**
   ```bash
   # Per 10 seconds
   ./scripts/health_check.sh | grep "erlmcp[1-4]"

   # Alert if any node: <20000 or >30000 connections
   ```

2. **Failover Time**
   ```bash
   # Measure time from failure to recovery
   # Should be: 3 × health_check_interval (9 seconds for 3s checks)
   # Alert if >15 seconds
   ```

3. **Latency Variance**
   ```bash
   # Monitor p99 latency per node
   # Alert if variance >10%
   ```

4. **Health Check Failures**
   ```bash
   # Check HAProxy stats
   # Alert if any backend has >1 failed check in 5 minutes
   ```

### Real-time Monitoring

```bash
# Terminal 1: Monitor cluster
./scripts/health_check.sh --continuous 5

# Terminal 2: Monitor load balancer (HAProxy)
curl -s http://127.0.0.1:8404/stats

# Terminal 3: Monitor system resources
watch -n 1 'free -h && echo && top -b -n 1 | head -20'

# Terminal 4: Follow node logs
./scripts/start_cluster.sh tail
```

## Configuration Files Reference

### HAProxy Configuration Sections

```erlang
% Global settings
global
    maxconn 100000
    stats socket /var/run/haproxy.sock mode 0660 level admin

% Defaults applied to all sections
defaults
    timeout connect 5000
    timeout client 300000
    timeout server 300000

% Listening interface
listen stats
    bind 127.0.0.1:8404
    stats enable
    stats uri /stats

% Incoming connections
frontend mcp_frontend_tcp
    bind 0.0.0.0:8000
    mode tcp

% Outgoing server pool
backend mcp_cluster_tcp
    balance roundrobin
    stick on src
    server erlmcp1 localhost:9001
```

### Nginx Configuration Sections

```nginx
% Worker process settings
events {
    worker_connections 100000;
    use epoll;
    multi_accept on;
}

% HTTP protocol settings
http {
    % Upstream server pool
    upstream erlmcp_tcp_cluster {
        ip_hash;
        server localhost:9001;
    }

    % Virtual server
    server {
        listen 8000;
        location / {
            proxy_pass http://erlmcp_tcp_cluster;
        }
    }
}
```

## Appendix

### Complete Startup Sequence

```bash
# Terminal 1: Start cluster (takes ~5 seconds)
./scripts/start_cluster.sh start
# Output: erlmcp1-4 UP

# Terminal 2: Start load balancer (takes ~2 seconds)
./scripts/start_load_balancer.sh haproxy
# Output: HAProxy started

# Terminal 3: Health check (verify everything is up)
./scripts/health_check.sh
# Output: All nodes UP, 0 connections

# Terminal 4: Run load test (takes as long as specified)
./scripts/load_test_100k.sh 100000 60
# Output: Connection distribution, latency metrics

# Result: Real metrics proving 100K connections distributed evenly
```

### Expected Output (100K Load Test)

```
=== erlmcp Load Balancer & Cluster Health ===

Load Balancer: HAProxy RUNNING

Cluster Nodes:
  erlmcp1: UP (25234 connections) ✓ (target: 25000 ± 500)
  erlmcp2: UP (24987 connections) ✓
  erlmcp3: UP (25156 connections) ✓
  erlmcp4: UP (24623 connections) ✓

Total Connections: 100000 / 100000 (100%)
Latency Range: 2ms - 8ms
Variance: 2.4% (target: <5%)

Connection Distribution: PASSED
Latency Variance: PASSED
Failover Time: <5 seconds
Sticky Sessions: WORKING

Load test completed!
```

## See Also

- `config/haproxy.cfg` - HAProxy configuration file
- `config/nginx.conf` - Nginx configuration file
- `config/cluster.config` - Erlang cluster configuration
- `scripts/start_cluster.sh` - Cluster startup script
- `scripts/start_load_balancer.sh` - Load balancer startup script
- `scripts/load_test_100k.sh` - 100K connection load test
- `scripts/health_check.sh` - Health monitoring
