# erlmcp Production Deployment Checklist

Complete checklist for deploying erlmcp to production environments (Docker & Kubernetes).

## Pre-Deployment Verification

### Code Quality
- [ ] All unit tests passing: `make test`
- [ ] Type checking passing: `rebar3 dialyzer`
- [ ] Cross-reference check passing: `rebar3 xref`
- [ ] Code coverage >= 80%: `make coverage-report`
- [ ] No lint warnings: `make lint`
- [ ] Security scan clean: `bandit` (if available)

### Documentation
- [ ] API documentation updated
- [ ] Architecture documentation reviewed
- [ ] CHANGELOG.md updated with version
- [ ] README.md reflects current version
- [ ] Deployment guide reviewed

### Version Management
- [ ] Version updated in `src/erlmcp.app.src`
- [ ] Docker image tag matches version
- [ ] Git tag created: `git tag v0.7.0`
- [ ] Version documented in deployment manifests

---

## Docker Image Preparation

### Build and Testing
- [ ] Docker image builds successfully: `docker build -t erlmcp:0.7.0 .`
- [ ] Image size acceptable (<200MB): `docker images erlmcp:0.7.0`
- [ ] Health check works: `docker run -p 8080:8080 erlmcp:0.7.0`
- [ ] Container logs show no errors
- [ ] Port mappings work (8080, 9090, 4369)

### Security Hardening
- [ ] Non-root user configured (UID 1000)
- [ ] Read-only filesystem where applicable
- [ ] Security scanning passed: `trivy image erlmcp:0.7.0`
- [ ] No hardcoded secrets in image
- [ ] Base image (Alpine) is up-to-date

### Image Validation
- [ ] Docker validation script passes: `./scripts/docker-validation.sh`
- [ ] Multi-stage build optimized
- [ ] Healthcheck probe configured
- [ ] Signal handling correct (SIGTERM)
- [ ] Image works offline (no external dependencies)

### Registry Push
- [ ] Image tagged for registry: `docker tag erlmcp:0.7.0 registry.example.com/erlmcp:0.7.0`
- [ ] Image pushed to registry: `docker push registry.example.com/erlmcp:0.7.0`
- [ ] Image signature verified (if using signed images)
- [ ] Image scan results reviewed in registry

---

## Kubernetes Configuration

### Manifests
- [ ] All manifest files validated: `kubectl apply --dry-run=client -f k8s/`
- [ ] Manifest syntax correct (YAML)
- [ ] API versions compatible with cluster
- [ ] No hardcoded values (use ConfigMaps/Secrets)
- [ ] Resource requests/limits appropriate

### Secrets and Configuration
- [ ] Database password generated and stored: `openssl rand -base64 32`
- [ ] JWT secret generated: `openssl rand -base64 32`
- [ ] ERLANG_COOKIE generated: `openssl rand -base64 32`
- [ ] Secret manifest created (DO NOT commit secrets)
- [ ] TLS certificates created/renewed
- [ ] Certificate expiration date noted (renewal before expiry)
- [ ] ConfigMap reviewed for environment-specific values

### RBAC
- [ ] ServiceAccount created
- [ ] Role and RoleBinding created
- [ ] Permissions minimal (principle of least privilege)
- [ ] RBAC tested: `kubectl auth can-i get pods --as=system:serviceaccount:erlmcp:erlmcp -n erlmcp`

### Network Security
- [ ] Network policies created (zero-trust model)
- [ ] Ingress rules reviewed
- [ ] Egress rules reviewed
- [ ] Pod-to-pod communication allowed
- [ ] External access restricted to Ingress

### Storage
- [ ] PersistentVolume configured
- [ ] PersistentVolumeClaim created with correct size
- [ ] Storage class selected appropriate for environment
- [ ] Backup strategy defined
- [ ] Restore procedure tested

---

## Cluster Preparation

### Cluster Requirements
- [ ] Kubernetes 1.20+ available
- [ ] kubectl configured and authenticated
- [ ] Cluster has sufficient resources (3+ nodes recommended)
- [ ] Ingress controller installed (Nginx recommended)
- [ ] Storage provisioner available

### Namespace Setup
- [ ] Namespace created: `kubectl create namespace erlmcp`
- [ ] Namespace labels applied
- [ ] Pod security standards enforced
- [ ] Resource quotas configured (optional)
- [ ] Network policies namespace scoped

### Add-ons
- [ ] Prometheus Operator installed (for monitoring)
- [ ] Cert-Manager installed (for TLS)
- [ ] Sealed Secrets operator installed (for secret encryption)
- [ ] Optional: Fluentd/Loki for log aggregation
- [ ] Optional: Jaeger for distributed tracing

---

## Pre-Launch Validation

### Manifest Deployment
- [ ] Secrets created in namespace
- [ ] ConfigMap applied
- [ ] RBAC manifests deployed
- [ ] Network policies deployed
- [ ] Deployment/StatefulSet deployed
- [ ] Services created
- [ ] HPA configured
- [ ] PDB created
- [ ] Ingress configured

### Pod Health
- [ ] Pods starting successfully (check for ImagePullBackOff)
- [ ] Pods reaching healthy status within 2 minutes
- [ ] Liveness probe passes
- [ ] Readiness probe passes
- [ ] Startup probe completes
- [ ] No restart loops

### Service Connectivity
- [ ] Service endpoint ready
- [ ] Internal DNS resolves: `nslookup erlmcp.erlmcp.svc.cluster.local`
- [ ] Port-forward works: `kubectl port-forward svc/erlmcp 8080:8080`
- [ ] Health endpoint responds
- [ ] Metrics endpoint responds

### External Access
- [ ] Ingress rule created
- [ ] DNS record points to Ingress IP
- [ ] TLS certificate issued by cert-manager
- [ ] HTTPS access works
- [ ] Redirect HTTP to HTTPS works

---

## Testing and Validation

### Functional Testing
- [ ] API health endpoint: `curl https://erlmcp.example.com/health`
- [ ] Metrics endpoint: `curl https://erlmcp.example.com/metrics`
- [ ] Sample MCP request/response works
- [ ] Database connectivity verified
- [ ] Redis connectivity verified (if used)

### Performance Testing
- [ ] Load testing completed (ab, wrk, k6, etc.)
- [ ] Response time acceptable (<1s p95)
- [ ] Error rate <0.1%
- [ ] Memory usage stable
- [ ] CPU usage acceptable

### Security Testing
- [ ] Port scanning shows only required ports
- [ ] Network policies enforced (ingress/egress)
- [ ] Secret scanning clean
- [ ] SQL injection tests pass
- [ ] XSS tests pass
- [ ] Authentication required for protected endpoints

### Kubernetes Validation
- [ ] Validation script passes: `./scripts/k8s-validation.sh`
- [ ] RBAC working correctly
- [ ] Network policies enforced
- [ ] Pod disruption budgets working
- [ ] Horizontal pod autoscaler scaling works

---

## Monitoring and Alerting

### Prometheus
- [ ] Prometheus scraping endpoints
- [ ] Metrics flowing into Prometheus
- [ ] Dashboards created/imported
- [ ] Alert rules configured

### Alerts
- [ ] High error rate alert configured (>5%)
- [ ] High latency alert configured (p95 > 1s)
- [ ] High memory usage alert configured (>80%)
- [ ] Pod crash loop alert configured
- [ ] Database connection error alert configured

### Logging
- [ ] Application logs collected
- [ ] Container logs accessible via kubectl
- [ ] Error logs monitored
- [ ] Log retention policy configured
- [ ] Log search/analysis working

### Observability
- [ ] OpenTelemetry traces exported
- [ ] Jaeger/Zipkin accessible
- [ ] Service map visible in tracing backend
- [ ] Slow request tracing working
- [ ] Distributed tracing correlation IDs present

---

## Backup and Disaster Recovery

### Data Backup
- [ ] Database backup strategy defined
- [ ] Automated backup job created (CronJob)
- [ ] Backup retention policy set (7+ days)
- [ ] Backup storage encrypted
- [ ] Backup location off-cluster

### Restore Procedure
- [ ] Restore procedure documented
- [ ] Test restore procedure (at least once)
- [ ] Restore time RPO documented
- [ ] Restore recovery RTO documented
- [ ] Restore failover procedure tested

### Disaster Recovery
- [ ] Multi-zone deployment confirmed
- [ ] Pod disruption budgets ensure high availability
- [ ] StatefulSet persistent volume snapshots created
- [ ] Disaster recovery runbook documented
- [ ] Failover procedure tested

---

## Operational Readiness

### Documentation
- [ ] Deployment guide reviewed
- [ ] Troubleshooting guide updated
- [ ] Runbook created for common operations
- [ ] On-call procedures documented
- [ ] Escalation contact information documented

### Team Training
- [ ] Team trained on deployment procedure
- [ ] Team trained on troubleshooting
- [ ] Team trained on scaling procedures
- [ ] Team trained on backup/restore
- [ ] Team trained on incident response

### Monitoring Setup
- [ ] Dashboard shared with team
- [ ] Alert notifications configured
- [ ] On-call rotation configured
- [ ] Incident tracking system configured
- [ ] Post-mortem procedure defined

---

## Go/No-Go Decision

### Success Criteria
- [ ] All pre-deployment checks passed
- [ ] All validation tests passed
- [ ] Performance tests acceptable
- [ ] Security tests passed
- [ ] Team trained and ready
- [ ] Runbooks documented

### Launch Decision
- [ ] Architecture review passed
- [ ] Security review passed
- [ ] Performance review passed
- [ ] Operational readiness review passed
- [ ] **GO/NO-GO** decision made by: _________________
- [ ] **Launch approved by**: _______________________ (Date: ________)

---

## Post-Deployment

### Immediate Checks (First Hour)
- [ ] Pods healthy and stable
- [ ] No error spikes in logs
- [ ] Request latency normal
- [ ] Error rate <0.1%
- [ ] Database operations normal
- [ ] Alert notifications working

### First Day Monitoring
- [ ] Memory usage stable (no leaks)
- [ ] CPU usage normal
- [ ] Disk usage acceptable
- [ ] Network traffic normal
- [ ] All health checks passing
- [ ] Metrics quality good

### First Week
- [ ] No unexpected resource scaling
- [ ] No security issues reported
- [ ] Performance meets SLA
- [ ] Zero critical issues
- [ ] Team confidence high
- [ ] Rollback plan shelved

### Production Stabilization
- [ ] Tune autoscaler parameters
- [ ] Adjust alert thresholds
- [ ] Fine-tune resource limits
- [ ] Optimize database indexes
- [ ] Plan next improvements

---

## Version History

| Version | Date | Deployed By | Status |
|---------|------|------------|--------|
| 0.7.0 | YYYY-MM-DD | User Name | Ready for Deployment |
| | | | |

---

## Sign-Off

**Deployment Engineer**: _______________________ **Date**: ________

**Operations Manager**: _______________________ **Date**: ________

**Security Officer**: _______________________ **Date**: ________

---

## Emergency Contact

**On-Call Engineer**: _________________________ **Phone**: __________

**Escalation Contact**: _______________________ **Phone**: __________

**Incident Commander**: _______________________ **Phone**: __________

---

## Notes

Use this space for deployment notes and issues encountered:

```
[deployment notes go here]
```

