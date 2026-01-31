# ErlMCP Multi-Environment Quick Start

## TL;DR - Get Started in 5 Minutes

### 1. Local Development

```bash
# Copy development config
cp config/dev.env .env

# Start dependencies
docker-compose up -d postgres redis

# Run server
ENV=dev cargo run --bin erlmcp_server

# Verify health
curl http://localhost:8080/health
```

### 2. Deploy to Staging

```bash
# Authenticate with GCP
gcloud auth application-default login
gcloud config set project taiea-dev

# Validate configuration
./tools/deploy-env.sh staging latest --validate-only

# Deploy (automatic with GitHub Actions on push to main)
# Or manual:
./tools/deploy-env.sh staging latest
```

### 3. Deploy to Production

```bash
# Switch to prod project
gcloud config set project taiea-prod

# Validate only (ALWAYS do this first)
./tools/deploy-env.sh production v1.2.3 --validate-only

# Review output carefully, then deploy
./tools/deploy-env.sh production v1.2.3
```

---

## Environment Comparison Matrix

| Feature | Dev | Staging | Production |
|---------|-----|---------|------------|
| **Location** | localhost | GCP dev | GCP prod |
| **Log Level** | DEBUG | INFO | WARN |
| **Database** | local/optional | Cloud SQL | Cloud SQL HA |
| **Firestore** | ✗ | ✓ test | ✓ prod |
| **Redis** | ✗ | ✓ basic | ✓ cluster |
| **Monitoring** | none | basic | full stack |
| **Cost** | $0 | ~$600/mo | ~$6500+/mo |
| **Uptime SLA** | N/A | N/A | 99.95% |

---

## Key Files Reference

```
Configuration:
  config/dev.env                          # Dev environment variables
  config/staging.env                      # Staging environment variables
  config/production.env                   # Production environment variables
  config/sys.config.dev                   # Dev Erlang runtime
  config/sys.config.staging               # Staging Erlang runtime
  config/sys.config.prod                  # Production Erlang runtime

Deployment:
  tools/deploy-env.sh                     # Unified deployment script
  .github/workflows/deploy-staging.yml    # Staging CI/CD pipeline

Infrastructure:
  gcp/environments.tf                     # Terraform code
  gcp/terraform.dev.tfvars                # Dev variables
  gcp/terraform.staging.tfvars            # Staging variables
  gcp/terraform.production.tfvars         # Production variables

Documentation:
  docs/ENVIRONMENT_GUIDE.md               # Complete operations guide
  MULTI_ENVIRONMENT_SETUP_RECEIPT.md      # Detailed receipt
```

---

## Common Commands

### Development

```bash
# Setup
cp config/dev.env .env
docker-compose up -d

# Run
cargo run --bin erlmcp_server

# Test
cargo test --lib
curl http://localhost:8080/health
```

### Staging

```bash
# Validate
./tools/deploy-env.sh staging latest --validate-only

# Deploy
./tools/deploy-env.sh staging latest

# Monitor
kubectl logs -n taiea-staging deployment/taiea-staging -f
kubectl get pods -n taiea-staging
```

### Production

```bash
# Validate (REQUIRED)
./tools/deploy-env.sh production v1.2.3 --validate-only

# Deploy (after validation)
./tools/deploy-env.sh production v1.2.3

# Monitor
kubectl logs -n taiea-prod deployment/taiea-prod -f
kubectl top pods -n taiea-prod
```

---

## Troubleshooting

### Can't connect to staging service?

```bash
# Get service IP
kubectl get svc -n taiea-staging

# Check if pending
kubectl describe service taiea-staging -n taiea-staging

# Wait 2-3 minutes for Load Balancer IP assignment
```

### Pod crashes on deploy?

```bash
# View logs
kubectl logs -n taiea-staging deployment/taiea-staging --all-containers=true

# Check events
kubectl describe pod <POD_NAME> -n taiea-staging

# Check resources
kubectl top pod -n taiea-staging
```

### Database connection error?

```bash
# Verify Cloud SQL instance is running
gcloud sql instances list --project taiea-staging

# Check service account has cloudsql.client role
gcloud projects get-iam-policy taiea-staging
```

---

## Next Steps

1. **Read**: `docs/ENVIRONMENT_GUIDE.md` (complete reference)
2. **Review**: `MULTI_ENVIRONMENT_SETUP_RECEIPT.md` (detailed breakdown)
3. **Setup GCP**: Follow GCP setup in `gcp/README.md`
4. **Deploy**: Use `./tools/deploy-env.sh` to deploy

---

## Support

- Detailed guide: See `docs/ENVIRONMENT_GUIDE.md`
- Troubleshooting: See section 11 of `ENVIRONMENT_GUIDE.md`
- Terraform docs: See `gcp/README.md`
- Deployment details: See `MULTI_ENVIRONMENT_SETUP_RECEIPT.md`

---

**Quick Links**:
- [Local Development](docs/ENVIRONMENT_GUIDE.md#development-environment-setup)
- [Staging Deployment](docs/ENVIRONMENT_GUIDE.md#deploying-to-staging)
- [Production Deployment](docs/ENVIRONMENT_GUIDE.md#deploying-to-production)
- [Troubleshooting](docs/ENVIRONMENT_GUIDE.md#troubleshooting-guide)
- [Cost Estimation](docs/ENVIRONMENT_GUIDE.md#cost-estimation)
