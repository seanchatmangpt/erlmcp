# For Operators: Deployment & Operations Guide

**Estimated read time: 15 minutes**

This guide is tailored for operators who deploy, monitor, and manage erlmcp in production.

## Prerequisites

- Docker (for containerization)
- GCP account (for deployment)
- kubectl (for Kubernetes, if using GKE)
- Basic familiarity with command-line tools

## Pre-Deployment Checklist

Before deploying, verify:

```bash
# Code is built and tested
make workspace-check            # Must pass

# All tests pass
make workspace-test             # 0 failures

# No warnings
make workspace-lint             # Clean output

# Production release builds
make workspace-release          # No errors

# Check release integrity
ls -lh taiea/_build/prod/rel/taiea/
ls -lh _build/prod/rel/erlmcp/
```

## Building for Deployment

### Create Production Release

```bash
# Build both erlmcp and taiea production releases
make workspace-release

# Output locations:
# - erlmcp: _build/prod/rel/erlmcp/
# - taiea: taiea/_build/prod/rel/taiea/

# Verify releases
_build/prod/rel/erlmcp/bin/erlmcp pid
taiea/_build/prod/rel/taiea/bin/taiea pid
```

### Docker Image

Create `Dockerfile`:

```dockerfile
FROM erlang:25-slim

WORKDIR /app

# Copy release artifacts
COPY _build/prod/rel/erlmcp /app/erlmcp
COPY taiea/_build/prod/rel/taiea /app/taiea

# Copy config
COPY config/sys.config /app/sys.config
COPY config/vm.args /app/vm.args

# Health check
HEALTHCHECK --interval=10s --timeout=5s \
    CMD /app/erlmcp/bin/erlmcp pid || exit 1

# Start application
CMD ["/app/erlmcp/bin/erlmcp", "foreground"]
```

Build image:
```bash
docker build -t erlmcp:latest .
docker build -t erlmcp:v0.6.0 .
```

## Deployment Models

### Model 1: Single Server (Development/Testing)

Suitable for: Development, CI testing, small POCs

```bash
# Build release
make workspace-release

# Start servers
_build/prod/rel/erlmcp/bin/erlmcp start
taiea/_build/prod/rel/taiea/bin/taiea start

# Check status
_build/prod/rel/erlmcp/bin/erlmcp ping
taiea/_build/prod/rel/taiea/bin/taiea ping

# Stop
_build/prod/rel/erlmcp/bin/erlmcp stop
taiea/_build/prod/rel/taiea/bin/taiea stop
```

### Model 2: Docker Containers (Staging/Production)

Suitable for: Container-based deployment, cloud environments

```bash
# Build image
docker build -t erlmcp:latest .

# Run container
docker run -d \
    --name erlmcp \
    -p 5005:5005 \
    -p 8080:8080 \
    -e ERLANG_COOKIE=secret \
    erlmcp:latest

# Check logs
docker logs erlmcp

# Stop container
docker stop erlmcp
docker rm erlmcp
```

### Model 3: Kubernetes (Production)

Suitable for: Large-scale deployment, auto-scaling, high availability

See [DEPLOYMENT.md](DEPLOYMENT.md) for Kubernetes manifests.

## Configuration

### Environment Variables

Set in shell or Dockerfile:

```bash
# Node name (for clustering)
export ERLANG_NODENAME=erlmcp@hostname

# Cookie (for inter-node communication)
export ERLANG_COOKIE=your_secret_cookie

# VM arguments
export ERL_FLAGS="+P 262144 +K true +A 8"

# Library paths
export ERL_LIBS="/app/erlmcp/lib:/app/taiea/lib"
```

### System Configuration (sys.config)

Location: `config/sys.config`

```erlang
[
    % erlmcp configuration
    {erlmcp, [
        {listen_port, 5005},
        {transport, {tcp, []}},
        {workers, 10}
    ]},

    % TAIEA configuration
    {taiea, [
        {governor_enabled, true},
        {receipts_enabled, true},
        {health_check_interval, 30000}  % 30 seconds
    ]},

    % Lager logging
    {lager, [
        {handlers, [
            {lager_console_backend, [
                {level, info},
                {formatter_config, [
                    timestamp, " [", severity, "] ",
                    {module, [module, ":", line], ""},
                    " - ", message, "\n"
                ]}
            ]},
            {lager_file_backend, [
                {file, "/var/log/erlmcp/erlmcp.log"},
                {level, info},
                {size, 10485760},      % 10 MB
                {date, "$D0"},
                {count, 5}
            ]}
        ]},
        {suppress_supervisor_start_stop, true}
    ]},

    % Kernel configuration
    {kernel, [
        {logger, [
            {handler, default, logger_std_h, #{}},
            {default_formatter_level, all}
        ]}
    ]}
].
```

### VM Arguments (vm.args)

Location: `vm.args`

```
% Node name
-sname erlmcp

% Cookie
-setcookie erlmcp_secret_cookie

% Process limit
+P 262144

% Kernel poll
+K true

% Async I/O threads
+A 8

% Max ports
+Q 65536

% Scheduling
+S 4:4

% Memory
+hms 256 +hml 256
```

## Monitoring & Health Checks

### Health Check Endpoint

For HTTP transport, implement health check:

```erlang
-module(erlmcp_health).
-export([status/0]).

status() ->
    {ok, #{
        status => up,
        timestamp => erlang:system_time(millisecond),
        erlmcp => erlmcp_status(),
        taiea => taiea_status()
    }}.

erlmcp_status() ->
    case whereis(erlmcp_sup) of
        undefined -> {error, not_running};
        Pid -> {ok, erlang:process_info(Pid, memory)}
    end.

taiea_status() ->
    case application:get_env(taiea, governor_enabled) of
        {ok, true} -> {ok, enabled};
        _ -> {ok, disabled}
    end.
```

### Key Metrics to Monitor

```bash
# Memory usage
observer:start()    # Watch memory growth

# Process count
(erlmcp@host)1> length(processes()).

# CPU usage
top -p $(pidof erl)

# Network connections
netstat -an | grep 5005

# Disk usage
df -h /var/log/erlmcp/
```

### Log Files

Default locations:
- **Console**: stdout/stderr
- **File**: `/var/log/erlmcp/erlmcp.log`
- **TAIEA**: `/var/log/erlmcp/taiea.log`

Rotate logs:
```bash
# logrotate configuration
cat > /etc/logrotate.d/erlmcp <<EOF
/var/log/erlmcp/*.log {
    daily
    rotate 7
    compress
    delaycompress
    notifempty
    create 0640 erlmcp erlmcp
    sharedscripts
    postrotate
        /app/erlmcp/bin/erlmcp pid > /dev/null || true
    endscript
}
EOF
```

## Scaling

### Horizontal Scaling (Multiple Servers)

Deploy multiple instances behind load balancer:

```
Client → Load Balancer → [Server 1, Server 2, Server 3]
```

Configuration:
```erlang
% In sys.config on each node
{erlmcp, [
    {listen_port, 5005},
    {transport, {tcp, []}},
    {cluster_enabled, true},
    {cluster_peers, ['erlmcp@server2', 'erlmcp@server3']}
]}.
```

Join cluster:
```erlang
% On node 2
net_kernel:connect_node('erlmcp@server1').

% Verify
nodes().
```

### Vertical Scaling (Single Server Tuning)

Tune VM arguments for more capacity:

```
% Increase process limit
+P 1000000          % 1 million processes

% Increase I/O threads
+A 32               % 32 async I/O threads

% Increase scheduling
+S 16:16            # 16 schedulers × 2

% More ports
+Q 262144           # 256K ports
```

### Auto-Scaling (Kubernetes)

In Kubernetes, use HPA (Horizontal Pod Autoscaler):

```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: erlmcp-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: erlmcp
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

## Upgrades & Patches

### Rolling Update (Kubernetes)

```bash
# Update deployment image
kubectl set image deployment/erlmcp \
    erlmcp=erlmcp:v0.6.1 \
    --record

# Watch rollout
kubectl rollout status deployment/erlmcp

# Rollback if needed
kubectl rollout undo deployment/erlmcp
```

### In-Place Update (Single Server)

```bash
# Build new release
make workspace-release

# Graceful shutdown
_build/prod/rel/erlmcp/bin/erlmcp stop

# Replace binary
cp -r _build/prod/rel/erlmcp/* /app/erlmcp/

# Restart
_build/prod/rel/erlmcp/bin/erlmcp start
```

## Disaster Recovery

### Backup Strategy

```bash
# Backup release
tar czf erlmcp-backup-$(date +%Y%m%d).tar.gz \
    _build/prod/rel/erlmcp \
    config/

# Backup logs
tar czf logs-backup-$(date +%Y%m%d).tar.gz \
    /var/log/erlmcp/
```

### Restore Procedure

```bash
# Extract backup
tar xzf erlmcp-backup-20260126.tar.gz

# Restore config
cp -r config/* /app/config/

# Restart services
make workspace-release
_build/prod/rel/erlmcp/bin/erlmcp start
```

## Troubleshooting

### Application Won't Start

```bash
# Check logs
tail -f /var/log/erlmcp/erlmcp.log

# Verify configuration
cat config/sys.config

# Check port availability
netstat -an | grep 5005

# Verify release integrity
ls -la _build/prod/rel/erlmcp/lib/
```

### High Memory Usage

```bash
# Check process breakdown
(erlmcp@host)1> lists:sort([{size, erlang:memory(T), T} || T <- erlang:memory()]).

# Profile memory
observer:start()    # Use Allocators tab

# Find leaks
recon:memory([allocated]).
```

### Slow Response Times

```bash
# Check queue depth
(erlmcp@host)1> erlang:process_info(whereis(erlmcp_sup), messages_queued).

# Profile hot spots
recon:trace(erlmcp_server, handle_call, 3, [{scope, g}])

# Check system load
top -p $(pidof erl)
```

For more detailed troubleshooting, see [TROUBLESHOOTING.md](TROUBLESHOOTING.md).

## Operational Runbooks

### Daily Operations

**Morning check:**
```bash
# Verify both systems running
_build/prod/rel/erlmcp/bin/erlmcp ping
taiea/_build/prod/rel/taiea/bin/taiea ping

# Check logs for errors
grep ERROR /var/log/erlmcp/*.log

# Monitor resource usage
top
```

**Performance baseline:**
```bash
# Record baseline metrics
date > metrics.log
(erlmcp@host)1> erlang:memory() >> metrics.log
(erlmcp@host)2> erlang:statistics(wall_clock) >> metrics.log
```

### Weekly Operations

**Backup:**
```bash
make workspace-release
tar czf backup-$(date +%Y%m%d).tar.gz \
    _build/prod/rel/ taiea/_build/prod/rel/ config/
```

**Update dependencies:**
```bash
rebar3 get-deps
rebar3 update
make workspace-check
make workspace-release
```

**Review logs:**
```bash
# Check for warnings/errors
grep -i warning /var/log/erlmcp/*.log
grep -i error /var/log/erlmcp/*.log
```

## Integration with Monitoring Systems

### Prometheus Metrics

Export metrics for Prometheus:

```erlang
-module(erlmcp_metrics).

prometheus_gauge(gauge_processes_total, erlang:system_info(process_count)).
prometheus_gauge(gauge_memory_bytes, erlang:memory(total)).
prometheus_gauge(gauge_ports_total, length(erlang:ports())).
```

### ELK Stack Integration

Send logs to Elasticsearch:

```erlang
{lager, [
    {handlers, [
        {lager_elasticsearch_backend, [
            {endpoint, "elasticsearch:9200"},
            {index, "erlmcp"},
            {type, "_doc"}
        ]}
    ]}
]}.
```

## Next Steps

- **To deploy to GCP**: See [DEPLOYMENT.md](DEPLOYMENT.md)
- **To troubleshoot issues**: See [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
- **To understand architecture**: See [ARCHITECTURE_OVERVIEW.md](ARCHITECTURE_OVERVIEW.md)
- **For GCP setup**: See [GCP_SETUP.md](GCP_SETUP.md)

---

**Last Updated**: 2026-01-26
**Status**: Production-ready
**Target Environment**: Docker / Kubernetes / GCP
