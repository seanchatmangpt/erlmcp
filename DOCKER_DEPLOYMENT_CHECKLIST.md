# Docker Deployment Checklist for erlmcp v3.0.0

Use this checklist to verify production readiness before deploying erlmcp.

---

## Phase 1: Pre-Deployment Preparation

### 1.1 System Requirements

| Requirement | Minimum | Recommended | Status |
|-------------|---------|-------------|--------|
| Docker Version | 24.0+ | 24.0+ | [ ] |
| Docker Compose | v2.27+ | v2.27+ | [ ] |
| CPU Cores | 2 | 4+ | [ ] |
| RAM | 4GB | 8GB+ | [ ] |
| Disk Space | 10GB | 50GB+ | [ ] |
| Network | 100Mbps | 1Gbps+ | [ ] |

### 1.2 Port Availability

| Port | Service | Required | Status |
|------|---------|----------|--------|
| 8080 | HTTP API | Yes | [ ] |
| 9090 | Health Check | Yes | [ ] |
| 9100 | Metrics/Distribution | Yes | [ ] |
| 4369 | EPMD (if using) | No (EPMD-less) | [ ] |
| 3000 | Grafana (monitoring) | Optional | [ ] |
| 9090 | Prometheus (monitoring) | Optional | [ ] |
| 9093 | Alertmanager (monitoring) | Optional | [ ] |

**Verify ports are available:**
```bash
# Check if ports are in use
lsof -i :8080
lsof -i :9090
lsof -i :9100
```

### 1.3 Environment Configuration

| Configuration | Required | Default | Status |
|---------------|----------|---------|--------|
| ERLMCP_ENV | Yes | production | [ ] |
| ERLANG_COOKIE | Yes | Auto-generated | [ ] |
| ERLMCP_NODE_NAME | Yes | erlmcp@hostname | [ ] |
| LOG_LEVEL | Optional | info | [ ] |
| CPU_LIMIT | Optional | 2.0 | [ ] |
| MEMORY_LIMIT | Optional | 4G | [ ] |

**Create environment file:**
```bash
cp .env.prod.template .env
# Edit .env with your values
```

---

## Phase 2: Build Verification

### 2.1 Image Build

| Step | Command | Expected Result | Status |
|------|---------|-----------------|--------|
| Build runtime image | `docker build --target runtime -t erlmcp:3.0.0 .` | Exit 0 | [ ] |
| Verify image size | `docker images erlmcp:3.0.0` | <200MB | [ ] |
| Scan for vulnerabilities | `docker scan erlmcp:3.0.0` | 0 critical | [ ] |

### 2.2 Quality Gates

| Gate | Command | Expected Result | Status |
|------|---------|-----------------|--------|
| Compilation | `docker compose run --rm erlmcp-build rebar3 compile` | 0 errors | [ ] |
| Unit Tests | `docker compose run --rm erlmcp-unit make eunit` | 100% pass | [ ] |
| Integration Tests | `docker compose run --rm erlmcp-ct make ct` | 100% pass | [ ] |
| Dialyzer | `docker compose run --rm erlmcp-check make dialyzer` | 0 warnings | [ ] |
| Xref | `docker compose run --rm erlmcp-check make xref` | 0 undefined | [ ] |
| Coverage | `docker compose run --rm erlmcp-check make coverage` | >=80% | [ ] |

**Run full quality pipeline:**
```bash
docker compose run --rm erlmcp-build make compile && \
docker compose run --rm erlmcp-unit make eunit && \
docker compose run --rm erlmcp-ct make ct && \
docker compose run --rm erlmcp-check make check
```

---

## Phase 3: Deployment

### 3.1 Single-Node Deployment (Docker Compose)

| Step | Command | Expected Result | Status |
|------|---------|-----------------|--------|
| Create volumes | `docker volume create erlmcp-data` | Volume created | [ ] |
| Start runtime | `docker compose --profile runtime up -d` | Containers up | [ ] |
| Verify containers | `docker compose ps` | All healthy | [ ] |
| Check logs | `docker logs erlmcp` | No errors | [ ] |

**Verify deployment:**
```bash
# Container status
docker compose ps

# Expected output:
# NAME      IMAGE             STATUS          PORTS
# erlmcp    erlmcp:3.0.0      Up 30 seconds   0.0.0.0:8080->8080/tcp, ...
```

### 3.2 Multi-Node Deployment (Docker Swarm)

| Step | Command | Expected Result | Status |
|------|---------|-----------------|--------|
| Initialize swarm | `docker swarm init` | Swarm initialized | [ ] |
| Create networks | `docker network create -d overlay --opt encrypted erlmcp-overlay` | Network created | [ ] |
| Create secrets | `echo "cookie" \| docker secret create erlang_cookie -` | Secret created | [ ] |
| Label nodes | `docker node update --label-add erlmcp.enabled=true <node>` | Node labeled | [ ] |
| Deploy stack | `docker stack deploy -c docker/docker-stack.yml erlmcp-swarm` | Stack deployed | [ ] |
| Verify services | `docker service ls` | All running | [ ] |

**Verify swarm deployment:**
```bash
# Service status
docker service ls

# Expected output:
# ID             NAME                      REPLICAS   IMAGE
# abc123         erlmcp-swarm_erlmcp       5/5        erlmcp:3.0.0
# def456         erlmcp-swarm_prometheus   1/1        prom/prometheus
# ...
```

### 3.3 Kubernetes Deployment

| Step | Command | Expected Result | Status |
|------|---------|-----------------|--------|
| Create namespace | `kubectl create namespace erlmcp` | Created | [ ] |
| Create secrets | `kubectl create secret generic erlmcp-secrets ...` | Created | [ ] |
| Deploy (Helm) | `helm install erlmcp ./helm/erlmcp-enterprise` | Deployed | [ ] |
| Verify pods | `kubectl get pods -n erlmcp` | All ready | [ ] |

---

## Phase 4: Health Verification

### 4.1 Health Check Levels

| Level | Check | Command | Expected | Status |
|-------|-------|---------|----------|--------|
| 1 | HTTP Health | `curl http://localhost:8080/health` | 200 OK | [ ] |
| 2 | Container Health | `docker inspect erlmcp --format='{{.State.Health.Status}}'` | healthy | [ ] |
| 3 | Process Running | `docker exec erlmcp pgrep beam.smp` | Process found | [ ] |

**Run all health checks:**
```bash
# Level 1: HTTP health endpoint
curl -f http://localhost:8080/health

# Level 2: Container health status
docker ps --format "table {{.Names}}\t{{.Status}}"

# Level 3: Process check
docker exec erlmcp /opt/erlmcp/bin/healthcheck.sh
```

### 4.2 API Verification

| Endpoint | Method | Expected | Status |
|----------|--------|----------|--------|
| /health | GET | 200, status=healthy | [ ] |
| /metrics | GET | 200, Prometheus format | [ ] |
| /mcp | POST | JSON-RPC response | [ ] |

**Test MCP endpoint:**
```bash
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
      "protocolVersion": "2025-11-25",
      "capabilities": {},
      "clientInfo": {"name": "test", "version": "1.0"}
    }
  }'
```

### 4.3 Metrics Verification

| Metric | Query | Expected | Status |
|--------|-------|----------|--------|
| Uptime | `erlmcp_uptime_seconds` | >0 | [ ] |
| Requests | `http_requests_total` | Incrementing | [ ] |
| Errors | `http_errors_total` | 0 (baseline) | [ ] |
| Memory | `erlmcp_memory_words` | Stable | [ ] |

---

## Phase 5: Monitoring Setup

### 5.1 Start Monitoring Stack

| Step | Command | Expected Result | Status |
|------|---------|-----------------|--------|
| Start monitoring | `docker compose --profile monitoring up -d` | Containers up | [ ] |
| Access Grafana | `curl http://localhost:3000` | Login page | [ ] |
| Access Prometheus | `curl http://localhost:9090` | UI loads | [ ] |

### 5.2 Verify Dashboards

| Dashboard | URL | Expected | Status |
|-----------|-----|----------|--------|
| erlmcp Overview | http://localhost:3000/d/erlmcp-overview | Data visible | [ ] |
| Performance | http://localhost:3000/d/performance | Graphs populated | [ ] |
| Resources | http://localhost:3000/d/resources | Metrics present | [ ] |

### 5.3 Configure Alerts

| Alert | Condition | Action | Status |
|-------|-----------|--------|--------|
| High Error Rate | >5% errors | Notify | [ ] |
| High Latency | P99 >100ms | Notify | [ ] |
| Low Memory | <10% free | Notify | [ ] |
| Service Down | Health check fails | Critical | [ ] |

---

## Phase 6: Security Verification

### 6.1 Container Security

| Check | Command | Expected | Status |
|-------|---------|----------|--------|
| Run as non-root | `docker exec erlmcp id` | uid=1000 | [ ] |
| Read-only rootfs | `docker inspect erlmcp --format='{{.HostConfig.ReadonlyRootfs}}'` | true | [ ] |
| No privileged mode | `docker inspect erlmcp --format='{{.HostConfig.Privileged}}'` | false | [ ] |
| Drop capabilities | `docker inspect erlmcp --format='{{.HostConfig.CapDrop}}'` | ALL | [ ] |

### 6.2 Secrets Management

| Secret | Location | Required | Status |
|--------|----------|----------|--------|
| Erlang Cookie | Docker secret / env | Yes | [ ] |
| TLS Cert | /run/secrets/tls.crt | Optional | [ ] |
| TLS Key | /run/secrets/tls.key | Optional | [ ] |
| DB Password | /run/secrets/db.password | If using DB | [ ] |

### 6.3 Network Security

| Check | Expected | Status |
|-------|----------|--------|
| Overlay network encrypted | true | [ ] |
| Only required ports exposed | 8080, 9090, 9100 | [ ] |
| Firewall rules configured | Allow only needed | [ ] |

---

## Phase 7: Performance Verification

### 7.1 Baseline Performance

| Metric | Target | Actual | Status |
|--------|--------|--------|--------|
| Startup Time | <60s | ___s | [ ] |
| Health Check Latency | <10ms | ___ms | [ ] |
| Request Throughput | >1000 req/s | ___ req/s | [ ] |
| P50 Latency | <5ms | ___ms | [ ] |
| P95 Latency | <20ms | ___ms | [ ] |
| P99 Latency | <50ms | ___ms | [ ] |

**Run performance test:**
```bash
# Quick performance test
docker compose run --rm erlmcp-bench make benchmark
```

### 7.2 Resource Usage

| Resource | Limit | Expected | Actual | Status |
|----------|-------|----------|--------|--------|
| CPU | 2.0 | <50% | ___% | [ ] |
| Memory | 4GB | <2GB | ___GB | [ ] |
| Disk I/O | <100MB/s | <50MB/s | ___MB/s | [ ] |
| Network | <1Gbps | <100Mbps | ___Mbps | [ ] |

**Check resource usage:**
```bash
docker stats erlmcp --no-stream
```

---

## Phase 8: Backup & Recovery

### 8.1 Backup Configuration

| Data Type | Location | Backup Method | Status |
|-----------|----------|---------------|--------|
| Application Data | /var/lib/erlmcp | Volume snapshot | [ ] |
| Logs | /var/log/erlmcp | Log rotation | [ ] |
| Metrics | Prometheus TSDB | Prometheus snapshot | [ ] |
| Grafana Dashboards | grafana-data | Dashboard export | [ ] |

**Create backup:**
```bash
# Backup application data
docker run --rm -v erlmcp_erlmcp-data:/data -v $(pwd):/backup \
  alpine tar czf /backup/erlmcp-data-$(date +%Y%m%d).tar.gz -C /data .

# Backup metrics
docker exec erlmcp-prometheus promtool tsdb snapshot /prometheus
```

### 8.2 Recovery Test

| Scenario | Recovery Time | Status |
|----------|---------------|--------|
| Container restart | <30s | [ ] |
| Volume restore | <5min | [ ] |
| Full disaster recovery | <30min | [ ] |

---

## Phase 9: Documentation

### 9.1 Deployment Documentation

| Document | Required | Status |
|----------|----------|--------|
| Runbook created | Yes | [ ] |
| On-call procedures | Yes | [ ] |
| Escalation contacts | Yes | [ ] |
| Architecture diagram | Yes | [ ] |

### 9.2 Runbook Sections

| Section | Content | Status |
|---------|---------|--------|
| Start/Stop procedures | Commands documented | [ ] |
| Health check commands | Provided | [ ] |
| Log locations | Specified | [ ] |
| Common issues & fixes | Listed | [ ] |
| Rollback procedures | Documented | [ ] |

---

## Phase 10: Go/No-Go Decision

### 10.1 Pre-Go Checklist

| Category | Items Passed | Items Total | Go/No-Go |
|----------|--------------|-------------|----------|
| Build & Test | ___ | 6 | |
| Deployment | ___ | 3 | |
| Health | ___ | 3 | |
| Monitoring | ___ | 3 | |
| Security | ___ | 3 | |
| Performance | ___ | 2 | |
| Backup | ___ | 2 | |
| Documentation | ___ | 2 | |
| **TOTAL** | **___** | **24** | |

### 10.2 Approval Sign-Off

| Role | Name | Approved | Date |
|------|------|----------|------|
| Technical Lead | | | |
| DevOps Engineer | | | |
| Security Lead | | | |
| Product Owner | | | |

---

## Post-Deployment Monitoring

### First Hour Checklist

- [ ] Monitor error rate (target: <1%)
- [ ] Monitor latency (target: P95 <20ms)
- [ ] Check logs for errors
- [ ] Verify alerts are working
- [ ] Check Grafana dashboards

### First Day Checklist

- [ ] Review resource usage trends
- [ ] Check backup completion
- [ ] Verify log rotation
- [ ] Review security scan results
- [ ] Document any issues

---

## Troubleshooting

### Common Issues

| Issue | Symptoms | Solution |
|-------|----------|----------|
| Port conflict | Container exits | Change port in .env |
| OOM killed | Container restarts | Increase MEMORY_LIMIT |
| Health check fails | 503 response | Check application logs |
| High latency | Slow responses | Check resource limits |

### Emergency Rollback

```bash
# Docker Compose rollback
docker compose down
docker compose --profile runtime up -d

# Docker Swarm rollback
docker service update --rollback erlmcp-swarm_erlmcp

# Kubernetes rollback
kubectl rollout undo deployment/erlmcp -n erlmcp
```

---

## Support

- **Documentation**: [Full Documentation](README.md)
- **Issues**: [GitHub Issues](https://github.com/banyan-platform/erlmcp/issues)
- **Discussions**: [GitHub Discussions](https://github.com/banyan-platform/erlmcp/discussions)
