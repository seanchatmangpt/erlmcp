# ErlMCP Multi-Environment Configuration Guide

## Overview

This guide explains the three-tier environment strategy for ErlMCP:
- **Development**: Local machine, minimal external dependencies
- **Staging**: GCP dev project, production-like but with relaxed constraints
- **Production**: GCP prod project, hardened, fully monitored

## Architecture Decision: Why Three Tiers?

### 1. Development (Local)
- **Target**: Developers on macOS/Linux
- **Deployment**: Direct to localhost via Docker Compose
- **Database**: Local PostgreSQL (optional), no Firestore
- **Configuration**: `.env` files, verbose logging, debug endpoints enabled

**Rationale**: Fastest iteration, no cloud dependencies, full debugging access

### 2. Staging
- **Target**: QA, integration testing, customer demos
- **Deployment**: GCP dev project (taiea-dev) on GKE
- **Database**: Cloud SQL test database, Firestore on dev project
- **Configuration**: Production-like infrastructure, experimental features enabled
- **Cost**: ~$500-1000/month (development project, minimal replicas)

**Rationale**: Full infrastructure testing, catch issues before prod, features testing

### 3. Production
- **Target**: Customer-facing SaaS service
- **Deployment**: GCP prod project (taiea-prod) on GKE
- **Database**: Cloud SQL prod database, Firestore on prod project
- **Configuration**: Hardened security, minimal logging, comprehensive monitoring
- **Cost**: ~$5000+/month (HA setup, full monitoring, backups)

**Rationale**: Customer trust, regulatory compliance, reliability guarantees

## Environment Configuration Files

### 1. Environment Variables (`.env`)

Located in `/config/`:
- `dev.env` - Local development settings
- `staging.env` - GCP dev project settings
- `production.env` - GCP prod project settings

#### Key Differences

| Variable | Dev | Staging | Production |
|----------|-----|---------|------------|
| `LOG_LEVEL` | debug | info | warn |
| `SERVER_CONCURRENCY` | 4 | 8 | 16 |
| `DB_POOL_SIZE` | 5 | 15 | 25 |
| `FIRESTORE_ENABLED` | false | true | true |
| `REDIS_ENABLED` | false | true | true |
| `OTEL_ENABLED` | false | true | true |
| `FEATURE_RATE_LIMITING` | false | true | true |
| `ALERT_THRESHOLD_ERROR_RATE` | N/A | 0.05 (5%) | 0.001 (0.1%) |

### 2. Erlang Configuration (`sys.config.*`)

Located in `/config/`:
- `sys.config.dev` - Local Erlang runtime settings
- `sys.config.staging` - GCP staging Erlang settings
- `sys.config.prod` - GCP production Erlang settings

#### Key Differences

| Setting | Dev | Staging | Production |
|---------|-----|---------|------------|
| `logger_level` | debug | info | warn |
| `enable_debug_endpoints` | true | true | false |
| `cache_enabled` | false | true | true |
| `max_pending_requests` | 500 | 250 | 100 |
| `file_logging_path` | logs/erlmcp.log | /var/logs/ | /var/logs/ |
| `max_log_file_bytes` | 50 MB | 100 MB | 100 MB |

## Switching Environments

### Local Development Setup

```bash
# 1. Start local PostgreSQL (optional)
docker-compose -f docker-compose.yml up -d postgres

# 2. Build with development config
export ENV=dev
cargo build

# 3. Run with development settings
ENV=dev ./target/debug/erlmcp_server

# 4. Verify with health check
curl http://localhost:8080/health
```

### Deploying to Staging

```bash
# 1. Authenticate with GCP
gcloud auth application-default login
gcloud config set project taiea-dev

# 2. Build Docker image
docker build -t erlmcp:staging .

# 3. Push to Artifact Registry
docker tag erlmcp:staging us-central1-docker.pkg.dev/taiea-dev/erlmcp-tai-repo/erlmcp:staging
docker push us-central1-docker.pkg.dev/taiea-dev/erlmcp-tai-repo/erlmcp:staging

# 4. Deploy using script
./tools/deploy-env.sh staging latest

# 5. Verify deployment
kubectl get pods -n taiea-staging
kubectl logs -n taiea-staging deployment/taiea-staging -f
```

### Deploying to Production

**CRITICAL**: Production deployments require multiple approval steps

```bash
# 1. Authenticate and select prod project
gcloud auth application-default login
gcloud config set project taiea-prod

# 2. Tag image as production
docker tag erlmcp:staging us-central1-docker.pkg.dev/taiea-prod/erlmcp-tai-repo/erlmcp:v1.2.3
docker push us-central1-docker.pkg.dev/taiea-prod/erlmcp-tai-repo/erlmcp:v1.2.3

# 3. Validation only (REQUIRED before actual deployment)
./tools/deploy-env.sh production v1.2.3 --validate-only

# 4. Review validation output, ensure no errors

# 5. Deploy to production (requires manual approval)
./tools/deploy-env.sh production v1.2.3

# 6. Monitor deployment
kubectl logs -n taiea-prod deployment/taiea-prod -f
```

## Environment-Specific Features

### Development

```
✓ Debug endpoints enabled (/debug, /profile)
✓ SQL query logging enabled
✓ HTTP request/response logging enabled
✓ In-memory cache (no Redis)
✓ Profiling endpoints active
✗ Rate limiting disabled
✗ Firestore disabled
```

**Use For**: Local development, testing, debugging

### Staging

```
✓ Rate limiting enabled (500 req/s)
✓ Firestore enabled (test project)
✓ Redis caching enabled
✓ OpenTelemetry enabled
✓ Experimental features enabled
✓ Smoke tests after deployment
✗ Debug endpoints enabled (but monitored)
✗ PII masking disabled
```

**Use For**: Integration testing, feature validation, performance testing, customer demos

### Production

```
✓ Rate limiting strict (1000 req/s aggregate)
✓ Firestore enabled (prod project)
✓ Redis cluster with sentinel
✓ Full OpenTelemetry with Datadog
✓ Backup automation (hourly)
✓ Disaster recovery enabled
✓ Audit logging enabled
✓ PII data masking enabled
✗ Debug endpoints disabled
✗ Experimental features disabled
✗ SQL logging disabled (except errors)
```

**Use For**: Customer-facing service, SaaS operations

## Database Configuration

### Development
- **Type**: PostgreSQL (local, via Docker)
- **Database**: `erlmcp_dev`
- **Pool Size**: 5
- **Connection String**: `postgres://erlmcp:dev_password@localhost:5432/erlmcp_dev`
- **Firestore**: Disabled
- **Emulator**: Local Firestore emulator on port 8081 (optional)

### Staging
- **Type**: Cloud SQL (GCP dev project)
- **Database**: `erlmcp_staging`
- **Pool Size**: 15
- **Connection String**: `postgres://erlmcp-sa:password@cloudsql-staging.c.taiea-dev.internal:5432/erlmcp_staging`
- **Firestore**: Enabled, test database on taiea-dev
- **Backups**: Daily automated backups

### Production
- **Type**: Cloud SQL (GCP prod project)
- **Database**: `erlmcp_prod`
- **Pool Size**: 25
- **Connection String**: `postgres://erlmcp-sa:password@cloudsql-prod.c.taiea-prod.internal:5432/erlmcp_prod`
- **Firestore**: Enabled, production database on taiea-prod
- **Backups**: Hourly automated backups with 30-day retention
- **Replication**: Read replicas in alternate regions

## Security Configuration

### Development
- **TLS**: Disabled (http://)
- **Auth**: Basic/mock authentication
- **Secrets**: .env file (NEVER commit sensitive values)
- **CORS**: Allow all origins

### Staging
- **TLS**: Enabled (https://), self-signed OK
- **Auth**: OAuth 2.0 with test credentials
- **Secrets**: GCP Secret Manager with test values
- **CORS**: Allow staging origins only

### Production
- **TLS**: Enabled with valid certificate
- **Auth**: OAuth 2.0 with production credentials
- **Secrets**: GCP Secret Manager with production encryption
- **CORS**: Allow production domains only
- **Rate Limiting**: Enabled at multiple levels
- **DDoS Protection**: Cloud Armor enabled

## Observability

### Development
```
Logging: stdout, file (50MB max)
Metrics: None
Tracing: None
Health Check: Manual (curl)
```

### Staging
```
Logging: stdout, file (/var/logs/erlmcp-staging.log)
Metrics: Prometheus (push to gateway)
Tracing: Jaeger (probabilistic 10%)
Health Check: Kubernetes readiness probe (5s interval)
Alerts: Basic (error threshold 5%)
```

### Production
```
Logging: structured JSON to /var/logs/, rotated hourly
Metrics: Prometheus + Datadog agent
Tracing: Jaeger (probabilistic 1%)
Health Check: Kubernetes liveness/readiness (10s intervals)
Alerts: Strict thresholds:
  - Error rate > 0.1%
  - P99 latency > 500ms
  - P999 latency > 1000ms
  - Memory > 75%
  - CPU > 70%
```

## Scaling Considerations

### Development
- **Replicas**: 1 (local container)
- **Resources**: 256MB RAM, 100m CPU
- **Scaling**: Manual, not automated

### Staging
- **Replicas**: 2 (basic HA)
- **Resources**: 512MB RAM, 250m CPU
- **Scaling**: Manual, horizontal pod autoscaler optional

### Production
- **Replicas**: 3-5 (minimum HA)
- **Resources**: 2GB RAM, 1000m CPU per pod
- **Scaling**: Automatic based on metrics:
  - Scale up if CPU > 70% or memory > 80%
  - Scale down if CPU < 30% for 5 minutes
  - Min replicas: 3
  - Max replicas: 20

## Cost Estimation

### Development
- **Compute**: $0 (local)
- **Database**: $0 (local)
- **Monitoring**: $0 (local)
- **Total**: $0/month

### Staging (GCP Dev Project)
- **GKE Cluster**: $150 (1 small cluster)
- **Cloud SQL**: $200 (shared dev instance)
- **Firestore**: $100 (light usage)
- **Artifact Registry**: $50
- **Monitoring/Logging**: $100
- **Total**: ~$600/month

### Production (GCP Prod Project)
- **GKE Cluster**: $1000 (HA setup)
- **Cloud SQL**: $2000 (prod instance + replicas)
- **Firestore**: $1000+ (customer data)
- **Cloud Load Balancer**: $500
- **Cloud CDN**: $200
- **Cloud Armor**: $300
- **Artifact Registry**: $100
- **Monitoring/Logging**: $500
- **Backup Storage**: $300
- **Total**: ~$6500+/month

## Environment Parity Checklist

Use this checklist to ensure staging matches production as closely as possible:

- [ ] Database version matches (PostgreSQL version)
- [ ] Firestore configuration matches
- [ ] Redis configuration matches
- [ ] Network topology similar (VPC/firewall rules)
- [ ] IAM roles and permissions match
- [ ] TLS/certificate configuration matches
- [ ] Logging format matches
- [ ] Metrics/tracing configuration matches
- [ ] Health check endpoints match
- [ ] Resource limits similar (scaled down for staging)
- [ ] Backup strategy matches
- [ ] Secret management strategy matches
- [ ] Load balancer configuration similar
- [ ] DNS/routing configuration matches

## Troubleshooting Guide

### Development Issues

**Problem**: Service won't start locally
```bash
# Check if port 8080 is in use
lsof -i :8080

# Kill existing process if needed
kill -9 <PID>

# Check PostgreSQL is running
docker-compose ps
```

**Problem**: Can't connect to local database
```bash
# Verify connection string
echo $DATABASE_URL

# Test connection
psql "postgres://erlmcp:dev_password@localhost:5432/erlmcp_dev"
```

### Staging Issues

**Problem**: Deployment hangs
```bash
# Check deployment status
kubectl describe deployment taiea-staging -n taiea-staging

# Check pod events
kubectl describe pod <POD_NAME> -n taiea-staging

# Check image pull
kubectl get events -n taiea-staging
```

**Problem**: Pod crashes
```bash
# View logs
kubectl logs <POD_NAME> -n taiea-staging

# Check resource limits
kubectl top pod -n taiea-staging

# View Kubernetes events
kubectl describe pod <POD_NAME> -n taiea-staging
```

### Production Issues

**Problem**: Service degradation
```bash
# Check all replicas healthy
kubectl get pods -n taiea-prod

# View metrics (CPU, memory)
kubectl top nodes
kubectl top pods -n taiea-prod

# Check service logs
kubectl logs -f -n taiea-prod deployment/taiea-prod --all-containers=true
```

**Problem**: Database connection errors
```bash
# Check Cloud SQL instance
gcloud sql instances describe erlmcp-prod --project taiea-prod

# Verify IAM permissions
gcloud projects get-iam-policy taiea-prod

# Test connectivity from pod
kubectl exec -it <POD_NAME> -n taiea-prod -- \
  psql "postgres://user:pass@host:5432/db"
```

## Related Documentation

- [GCP Infrastructure Setup](../gcp/README.md)
- [Docker Configuration](./DOCKER_GUIDE.md)
- [Operations Runbooks](./OPERATIONS_RUNBOOKS.md)
- [Deployment Checklist](#deployment-checklist)

## Deployment Checklist

### Before Staging Deployment
- [ ] All tests passing locally
- [ ] Code review completed
- [ ] Environment variables validated
- [ ] Database migrations prepared
- [ ] Configuration files reviewed
- [ ] Docker image builds successfully
- [ ] No hardcoded secrets in image

### Before Production Deployment
- [ ] Staging deployment successful
- [ ] Smoke tests passing
- [ ] Performance tests acceptable
- [ ] Security scan passed
- [ ] Backup strategy verified
- [ ] Rollback plan documented
- [ ] On-call engineer assigned
- [ ] Deployment window scheduled
- [ ] Status page prepared
- [ ] Post-deployment validation plan

## Next Steps

1. **Setup Local Development**
   ```bash
   cp config/dev.env .env.local
   docker-compose up -d
   cargo build
   ```

2. **Configure GCP Projects**
   - Review `gcp/main.tf`
   - Run `terraform apply` to create infrastructure

3. **Deploy to Staging**
   ```bash
   ./tools/deploy-env.sh staging latest --validate-only
   ./tools/deploy-env.sh staging latest
   ```

4. **Monitor Production**
   - Set up dashboards
   - Configure alerts
   - Establish on-call rotation

---

**Last Updated**: January 2026
**Version**: 1.0.0
**Maintainer**: DevOps Team
