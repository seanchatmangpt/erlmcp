# erlmcp v0.6.0 + TCPS Deployment Runbook

This runbook provides step-by-step instructions for deploying erlmcp v0.6.0 with TCPS (Toyota Code Production System) to production environments.

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Pre-Deployment Checklist](#pre-deployment-checklist)
4. [Deployment Methods](#deployment-methods)
5. [Post-Deployment Verification](#post-deployment-verification)
6. [Rollback Procedure](#rollback-procedure)
7. [Common Issues](#common-issues)
8. [Emergency Contacts](#emergency-contacts)

---

## Overview

**Version**: 0.6.0
**Components**:
- erlmcp MCP Server
- TCPS Coordinator
- Dashboard
- PostgreSQL Database (optional)
- OpenTelemetry Collector (optional)

**Deployment Strategies**:
- Direct deployment (bare metal/VM)
- Docker Compose (development/staging)
- Kubernetes (production)

---

## Prerequisites

### System Requirements

**Minimum**:
- CPU: 2 cores
- RAM: 2GB
- Disk: 10GB
- OS: Linux, macOS, or Windows with WSL2

**Recommended Production**:
- CPU: 4+ cores
- RAM: 8GB+
- Disk: 50GB+ (with monitoring/logging)
- OS: Ubuntu 22.04 LTS or RHEL 8+

### Software Dependencies

- **Erlang/OTP**: 26.0 or higher
- **rebar3**: 3.22.0 or higher
- **Docker**: 24.0+ (for Docker deployments)
- **Kubernetes**: 1.27+ (for K8s deployments)
- **PostgreSQL**: 15+ (if using persistence)

### Access Requirements

- SSH access to target servers
- `sudo` privileges (for system-level operations)
- Database credentials (if using persistence)
- TLS certificates (for production)
- Secrets management access (JWT keys, API tokens, etc.)

---

## Pre-Deployment Checklist

### Code Readiness

- [ ] All tests passing (`rebar3 as test do eunit, ct`)
- [ ] Code reviewed and approved
- [ ] Git repository clean (no uncommitted changes for production)
- [ ] Version bumped in `src/erlmcp.app.src`
- [ ] CHANGELOG.md updated
- [ ] Release notes prepared

### Infrastructure Readiness

- [ ] Target servers provisioned and accessible
- [ ] Database schema migrated (if applicable)
- [ ] Load balancer configured (if applicable)
- [ ] DNS records updated (if needed)
- [ ] Firewall rules configured
  - Port 8080: HTTP API
  - Port 3000: Dashboard
  - Port 9090: Prometheus metrics
- [ ] Monitoring/alerting configured
- [ ] Log aggregation configured

### Configuration Readiness

- [ ] Environment-specific configuration files prepared
  - `config/dev.config` for development
  - `config/staging.config` for staging
  - `config/production.config` for production
- [ ] Secrets stored securely (not in git)
  - Database passwords
  - JWT secrets
  - API keys
  - TLS certificates
- [ ] Environment variables documented

### Team Readiness

- [ ] Deployment window scheduled
- [ ] Stakeholders notified
- [ ] On-call engineer assigned
- [ ] Rollback plan reviewed
- [ ] Communication channels ready (Slack, PagerDuty, etc.)

---

## Deployment Methods

### Method 1: Direct Deployment (Bare Metal/VM)

#### Step 1: Build Release

```bash
# Clone repository
git clone https://github.com/your-org/erlmcp.git
cd erlmcp

# Checkout release tag
git checkout v0.6.0

# Build production release
rebar3 as prod release
```

#### Step 2: Deploy Release

```bash
# Run deployment script
./scripts/deploy.sh production

# Options:
# --skip-tests      Skip test execution (not recommended)
# --force           Force deployment despite warnings
# --dry-run         Preview deployment without executing
```

#### Step 3: Verify Deployment

```bash
# Check application status
sudo /opt/erlmcp/bin/erlmcp ping

# Check health endpoint
curl http://localhost:8080/health

# View logs
tail -f /var/log/erlmcp/production.log
```

---

### Method 2: Docker Compose Deployment

#### Step 1: Prepare Environment

```bash
# Clone repository
git clone https://github.com/your-org/erlmcp.git
cd erlmcp

# Create .env file
cat > .env <<EOF
ERLMCP_DB_PASSWORD=secure_password_here
GRAFANA_PASSWORD=grafana_admin_password
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
EOF
```

#### Step 2: Build Images

```bash
# Build Docker images
docker compose -f docker/docker-compose.yml build
```

#### Step 3: Deploy Stack

```bash
# Start all services
docker compose -f docker/docker-compose.yml up -d

# Check service status
docker compose -f docker/docker-compose.yml ps

# View logs
docker compose -f docker/docker-compose.yml logs -f erlmcp
```

#### Step 4: Verify Deployment

```bash
# Check erlmcp health
curl http://localhost:8080/health

# Check dashboard
curl http://localhost:3000/health

# Check Jaeger UI
open http://localhost:16686

# Check Grafana
open http://localhost:3001
```

---

### Method 3: Kubernetes Deployment

#### Step 1: Prepare Cluster

```bash
# Create namespace
kubectl apply -f k8s/namespace.yaml

# Create secrets
kubectl create secret generic erlmcp-secrets \
  --from-literal=db-name=erlmcp \
  --from-literal=db-user=erlmcp \
  --from-literal=db-password=secure_password \
  --from-literal=jwt-secret=your_jwt_secret \
  -n erlmcp

# Create TLS certificates (if not using cert-manager)
kubectl create secret tls erlmcp-tls \
  --cert=path/to/tls.crt \
  --key=path/to/tls.key \
  -n erlmcp
```

#### Step 2: Deploy Application

```bash
# Deploy all resources
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/hpa.yaml
kubectl apply -f k8s/ingress.yaml

# Check rollout status
kubectl rollout status deployment/erlmcp-tcps -n erlmcp

# Check pods
kubectl get pods -n erlmcp
```

#### Step 3: Verify Deployment

```bash
# Check pod status
kubectl get pods -n erlmcp

# Check logs
kubectl logs -f deployment/erlmcp-tcps -n erlmcp

# Check health
kubectl port-forward svc/erlmcp 8080:8080 -n erlmcp
curl http://localhost:8080/health

# Check metrics
kubectl port-forward svc/erlmcp 9090:9090 -n erlmcp
curl http://localhost:9090/metrics
```

---

## Post-Deployment Verification

### Functional Verification

#### 1. Basic Health Check

```bash
# Run health check script
./scripts/health_check.sh production http://localhost:8080

# Expected output: HTTP 200 with JSON health status
```

#### 2. Endpoint Testing

```bash
# Test MCP protocol endpoints
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}'

# Expected: Valid JSON-RPC response
```

#### 3. Dashboard Access

```bash
# Access dashboard
open http://localhost:3000

# Verify:
# - Dashboard loads
# - TCPS metrics visible
# - Work orders displayed
```

#### 4. TCPS Validation

```bash
# Check TCPS status
./tools/tcps status

# Verify:
# - Quality gates configured
# - Andon system operational
# - Observability enabled
```

### Performance Verification

#### 1. Response Time

```bash
# Measure response time
time curl http://localhost:8080/health

# Expected: < 100ms
```

#### 2. Throughput

```bash
# Run load test (requires wrk or similar)
wrk -t4 -c100 -d30s http://localhost:8080/health

# Expected: > 1000 req/s
```

#### 3. Resource Usage

```bash
# Check memory usage
curl http://localhost:8080/health | jq '.metrics.memory_total'

# Check process count
curl http://localhost:8080/health | jq '.metrics.process_count'
```

### Monitoring Verification

#### 1. Metrics Collection

```bash
# Check Prometheus targets
open http://localhost:9091/targets

# Verify erlmcp target is UP
```

#### 2. Tracing

```bash
# Check Jaeger for traces
open http://localhost:16686

# Verify traces are being collected
```

#### 3. Logs

```bash
# Check log output
tail -f /var/log/erlmcp/production.log

# Or for Docker:
docker compose logs -f erlmcp

# Or for Kubernetes:
kubectl logs -f deployment/erlmcp-tcps -n erlmcp
```

---

## Rollback Procedure

### Automatic Rollback

If health checks fail during deployment, the deployment script will automatically rollback:

```bash
# Automatic rollback happens if:
# - Health check fails
# - Application won't start
# - Smoke tests fail
```

### Manual Rollback

#### Method 1: Using Rollback Script

```bash
# Rollback to latest backup
./scripts/rollback.sh production

# Rollback to specific backup
./scripts/rollback.sh production erlmcp-backup-20260126-120000

# For Kubernetes:
kubectl rollout undo deployment/erlmcp-tcps -n erlmcp
```

#### Method 2: Manual Rollback

```bash
# Stop current version
sudo /opt/erlmcp/bin/erlmcp stop

# Restore previous backup
sudo cp -r /opt/erlmcp/backups/erlmcp-backup-LATEST /opt/erlmcp

# Start previous version
sudo /opt/erlmcp/bin/erlmcp start

# Verify
./scripts/health_check.sh production
```

### Rollback Verification

After rollback, verify:

1. **Application Status**
   ```bash
   sudo /opt/erlmcp/bin/erlmcp ping
   ```

2. **Health Check**
   ```bash
   curl http://localhost:8080/health
   ```

3. **Functionality**
   ```bash
   # Run smoke tests
   rebar3 as test ct --suite=smoke_test_SUITE
   ```

4. **Monitoring**
   - Check Grafana dashboards
   - Verify error rates decreased
   - Confirm metrics are normal

---

## Common Issues

### Issue 1: Application Won't Start

**Symptoms**:
- `erlmcp ping` returns `pang`
- Health endpoint not responding
- Crash dumps generated

**Diagnosis**:
```bash
# Check crash dump
cat /var/log/erlmcp/erl_crash.dump

# Check logs
tail -100 /var/log/erlmcp/production.log

# Check port conflicts
sudo lsof -i :8080
```

**Solutions**:
1. Check configuration files for syntax errors
2. Verify all dependencies are available
3. Ensure ports are not already in use
4. Check file permissions
5. Verify database connectivity (if applicable)

---

### Issue 2: High Memory Usage

**Symptoms**:
- Memory usage > 80%
- Application slow to respond
- OOM errors in logs

**Diagnosis**:
```bash
# Check memory metrics
curl http://localhost:8080/health | jq '.metrics.memory_total'

# Check process count
curl http://localhost:8080/health | jq '.metrics.process_count'
```

**Solutions**:
1. Increase memory limits in configuration
2. Enable aggressive garbage collection
3. Review application for memory leaks
4. Scale horizontally (add more instances)

---

### Issue 3: Database Connection Failures

**Symptoms**:
- Errors in logs about database connectivity
- TCPS persistence not working
- Health check shows database unhealthy

**Diagnosis**:
```bash
# Test database connection
psql -h localhost -U erlmcp -d erlmcp -c "SELECT 1"

# Check connection pool
curl http://localhost:8080/health | jq '.checks[] | select(.name=="database")'
```

**Solutions**:
1. Verify database credentials
2. Check network connectivity
3. Verify database is running
4. Check connection pool configuration
5. Review firewall rules

---

### Issue 4: TLS Certificate Issues

**Symptoms**:
- HTTPS endpoints not working
- Certificate validation errors
- Secure connections failing

**Diagnosis**:
```bash
# Check certificate validity
openssl x509 -in /etc/erlmcp/certs/server.crt -noout -dates

# Test TLS connection
openssl s_client -connect localhost:8080 -servername erlmcp.example.com
```

**Solutions**:
1. Verify certificate paths in configuration
2. Check certificate expiration
3. Ensure certificate chain is complete
4. Verify private key permissions
5. Regenerate certificates if necessary

---

## Emergency Contacts

### Escalation Path

**Severity Levels**:
- **P0 (Critical)**: Complete outage, data loss risk
- **P1 (High)**: Major functionality broken, degraded performance
- **P2 (Medium)**: Minor functionality issues
- **P3 (Low)**: Cosmetic issues, feature requests

### Contact Information

**Primary On-Call Engineer**:
- Name: [Your Name]
- Phone: [Phone Number]
- Email: [Email Address]
- Slack: @[username]

**Backup On-Call Engineer**:
- Name: [Backup Name]
- Phone: [Phone Number]
- Email: [Email Address]
- Slack: @[username]

**Team Lead**:
- Name: [Lead Name]
- Email: [Email Address]
- Slack: @[username]

**Infrastructure Team**:
- Slack: #infrastructure
- Email: infra@example.com

**Database Team**:
- Slack: #database
- Email: dba@example.com

### Incident Management

**PagerDuty**: https://yourorg.pagerduty.com
**Status Page**: https://status.example.com
**Runbook Repository**: https://github.com/your-org/runbooks

---

## Deployment Checklist Summary

Use this checklist during deployment:

- [ ] Pre-deployment checklist completed
- [ ] Deployment window scheduled and communicated
- [ ] Backup created
- [ ] Release built and tested
- [ ] Deployment executed (choose method)
- [ ] Health checks passed
- [ ] Functional verification completed
- [ ] Performance verification completed
- [ ] Monitoring verification completed
- [ ] Documentation updated
- [ ] Team notified of successful deployment
- [ ] Post-deployment review scheduled

---

## Additional Resources

- **Architecture Documentation**: [docs/ARCHITECTURE.md](./ARCHITECTURE.md)
- **Configuration Guide**: [docs/CONFIGURATION.md](./CONFIGURATION.md)
- **Monitoring Setup**: [docs/MONITORING.md](./MONITORING.md)
- **TCPS Guide**: [docs/TCPS.md](./TCPS.md)
- **API Documentation**: [docs/API.md](./API.md)

---

**Last Updated**: 2026-01-26
**Version**: 0.6.0
**Maintained By**: erlmcp Team
