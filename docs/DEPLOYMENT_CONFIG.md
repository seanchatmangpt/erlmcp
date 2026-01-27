# erlmcp Deployment Configuration Guide

This guide provides deployment-specific configuration instructions for erlmcp v0.6.0+. The codebase has been hardened against credential exposure through environment variable configuration.

**Version**: 0.6.0+
**Last Updated**: 2026-01-27
**Security Status**: ✅ No hardcoded credentials

## Quick Start: Development

### 1. Create Environment File

Create a `.env.development` file in the project root (DO NOT COMMIT TO GIT):

```bash
# Email Configuration
ERLMCP_EMAIL_PASSWORD="your-smtp-password"

# Monitoring Integration
ERLMCP_PAGERDUTY_KEY="your-pagerduty-integration-key"
ERLMCP_DATADOG_API_KEY="your-datadog-api-key"
ERLMCP_NEWRELIC_API_KEY="your-newrelic-api-key"
ERLMCP_NEWRELIC_ACCOUNT_ID="your-account-id"
ERLMCP_GRAFANA_USERNAME="your-grafana-username"
ERLMCP_GRAFANA_PASSWORD="your-grafana-password"

# Alerting
ERLMCP_SLACK_WEBHOOK="https://hooks.slack.com/services/YOUR/WEBHOOK/URL"
ERLMCP_WEBHOOK_AUTH_TOKEN="Bearer your-actual-token"

# Path Configuration
ERLMCP_ALLOWED_PATHS="/Users/sac/projects:/tmp"
```

### 2. Source Environment Variables

```bash
# Before starting the application
source .env.development

# Start the application
make dev-console
```

### 3. Verify Configuration

```bash
# In the Erlang shell
> application:get_env(erlmcp, tcps_health).
{ok, [{email_password, "your-smtp-password"}, ...]}

> application:get_env(erlmcp, roots).
{ok, [{allowed_paths, ["/Users/sac/projects", "/tmp"]}, ...]}
```

---

## Staging Deployment

### Docker Compose Setup

Create `docker-compose.staging.yml`:

```yaml
version: '3.8'

services:
  erlmcp:
    image: erlmcp:staging
    environment:
      ERLMCP_EMAIL_PASSWORD: ${STAGING_EMAIL_PASSWORD}
      ERLMCP_PAGERDUTY_KEY: ${STAGING_PAGERDUTY_KEY}
      ERLMCP_DATADOG_API_KEY: ${STAGING_DATADOG_API_KEY}
      ERLMCP_NEWRELIC_API_KEY: ${STAGING_NEWRELIC_API_KEY}
      ERLMCP_NEWRELIC_ACCOUNT_ID: ${STAGING_NEWRELIC_ACCOUNT_ID}
      ERLMCP_GRAFANA_USERNAME: ${STAGING_GRAFANA_USERNAME}
      ERLMCP_GRAFANA_PASSWORD: ${STAGING_GRAFANA_PASSWORD}
      ERLMCP_SLACK_WEBHOOK: ${STAGING_SLACK_WEBHOOK}
      ERLMCP_WEBHOOK_AUTH_TOKEN: ${STAGING_WEBHOOK_AUTH_TOKEN}
      ERLMCP_ALLOWED_PATHS: /data:/tmp
    volumes:
      - /data/erlmcp:/data
    ports:
      - "8080:8080"
      - "8081:8081"
```

### Deploy to Staging

```bash
# Create staging .env file
cp .env.staging.example .env.staging
# Edit with actual staging credentials
vim .env.staging

# Deploy
docker-compose -f docker-compose.staging.yml up -d

# Verify
docker logs erlmcp-staging-1
```

---

## Production Deployment

### Prerequisites

- Secrets management system (AWS Secrets Manager, HashiCorp Vault, or Kubernetes Secrets)
- Access to all third-party API endpoints
- Certificate files for TLS (if HTTPS enabled)

### Environment Variables Setup

#### AWS ECS/Fargate

1. Store secrets in AWS Secrets Manager:

```bash
aws secretsmanager create-secret --name erlmcp/email-password \
  --secret-string "your-smtp-password"

aws secretsmanager create-secret --name erlmcp/pagerduty-key \
  --secret-string "your-pagerduty-key"

# ... repeat for all credentials
```

2. Reference in ECS task definition:

```json
{
  "name": "erlmcp",
  "image": "erlmcp:v0.6.0",
  "essential": true,
  "environment": [
    {
      "name": "ERLMCP_ALLOWED_PATHS",
      "value": "/var/mcp:/data:/tmp"
    }
  ],
  "secrets": [
    {
      "name": "ERLMCP_EMAIL_PASSWORD",
      "valueFrom": "arn:aws:secretsmanager:us-east-1:123456789:secret:erlmcp/email-password"
    },
    {
      "name": "ERLMCP_PAGERDUTY_KEY",
      "valueFrom": "arn:aws:secretsmanager:us-east-1:123456789:secret:erlmcp/pagerduty-key"
    }
    // ... repeat for all secrets
  ]
}
```

#### Kubernetes (Recommended)

1. Create secrets:

```bash
kubectl create secret generic erlmcp-credentials \
  --from-literal=email-password='your-smtp-password' \
  --from-literal=pagerduty-key='your-pagerduty-key' \
  --from-literal=datadog-api-key='your-datadog-key' \
  # ... repeat for all credentials
```

2. Create ConfigMap for non-sensitive config:

```bash
kubectl create configmap erlmcp-config \
  --from-literal=allowed-paths='/var/mcp:/data:/tmp'
```

3. Define Deployment with secret injection:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp
spec:
  replicas: 3
  template:
    spec:
      containers:
      - name: erlmcp
        image: erlmcp:v0.6.0
        env:
        # Non-sensitive environment variables
        - name: ERLMCP_ALLOWED_PATHS
          valueFrom:
            configMapKeyRef:
              name: erlmcp-config
              key: allowed-paths

        # Sensitive environment variables from secrets
        - name: ERLMCP_EMAIL_PASSWORD
          valueFrom:
            secretKeyRef:
              name: erlmcp-credentials
              key: email-password

        - name: ERLMCP_PAGERDUTY_KEY
          valueFrom:
            secretKeyRef:
              name: erlmcp-credentials
              key: pagerduty-key

        - name: ERLMCP_DATADOG_API_KEY
          valueFrom:
            secretKeyRef:
              name: erlmcp-credentials
              key: datadog-api-key

        - name: ERLMCP_NEWRELIC_API_KEY
          valueFrom:
            secretKeyRef:
              name: erlmcp-credentials
              key: newrelic-api-key

        - name: ERLMCP_NEWRELIC_ACCOUNT_ID
          valueFrom:
            secretKeyRef:
              name: erlmcp-credentials
              key: newrelic-account-id

        - name: ERLMCP_GRAFANA_USERNAME
          valueFrom:
            secretKeyRef:
              name: erlmcp-credentials
              key: grafana-username

        - name: ERLMCP_GRAFANA_PASSWORD
          valueFrom:
            secretKeyRef:
              name: erlmcp-credentials
              key: grafana-password

        - name: ERLMCP_SLACK_WEBHOOK
          valueFrom:
            secretKeyRef:
              name: erlmcp-credentials
              key: slack-webhook

        - name: ERLMCP_WEBHOOK_AUTH_TOKEN
          valueFrom:
            secretKeyRef:
              name: erlmcp-credentials
              key: webhook-auth-token
```

#### HashiCorp Vault

1. Store secrets:

```bash
vault kv put secret/erlmcp/prod \
  email_password="your-smtp-password" \
  pagerduty_key="your-pagerduty-key" \
  datadog_api_key="your-datadog-api-key" \
  # ... repeat for all credentials
```

2. Use Vault agent to inject:

```hcl
vault {
  policies = ["erlmcp-prod"]
}

auto {
  auth {
    method "approle" {
      mount_path = "auth/approle"
    }
  }

  templating {
    source = "/vault/templates/erlmcp.env.tpl"
    destination = "/etc/erlmcp/.env"
  }
}
```

### Pre-Deployment Checklist

- [ ] All secrets created in secrets management system
- [ ] All environment variables properly configured
- [ ] TLS certificates deployed (if HTTPS enabled)
- [ ] Database credentials configured separately
- [ ] Redis credentials configured separately
- [ ] Logging configured to use `/var/log/erlmcp` or cloud logging
- [ ] Health check endpoint configured
- [ ] Monitoring/alerting configured
- [ ] Disaster recovery plan documented
- [ ] Security review completed

### Deployment Steps

```bash
# 1. Build production image
docker build -t erlmcp:v0.6.0 .

# 2. Tag for registry
docker tag erlmcp:v0.6.0 myregistry.azurecr.io/erlmcp:v0.6.0

# 3. Push to registry
docker push myregistry.azurecr.io/erlmcp:v0.6.0

# 4. Deploy to Kubernetes
kubectl apply -f deployment.yaml

# 5. Verify deployment
kubectl rollout status deployment/erlmcp

# 6. Check logs
kubectl logs -f deployment/erlmcp

# 7. Run health check
curl http://erlmcp:8080/health
```

### Post-Deployment Validation

```bash
# 1. Verify no hardcoded credentials in logs
kubectl logs deployment/erlmcp | grep -i "changeme\|password\|secret" || echo "✓ No exposed credentials"

# 2. Check configuration loaded
kubectl exec -it erlmcp-pod -- erl -eval 'application:get_env(erlmcp, roots), halt().'

# 3. Run smoke tests
kubectl run smoke-tests --image=erlmcp:v0.6.0 --restart=Never -- test

# 4. Monitor alerting systems
# - Check Datadog dashboard
# - Check New Relic APM
# - Check Grafana dashboard
# - Check PagerDuty integration

# 5. Verify secret rotation capability
# - Update secret in secrets manager
# - Restart deployment
# - Verify new credentials used
```

---

## Environment Variables Reference

### Email Configuration
**Purpose**: SMTP server credentials for email alerts

| Variable | Example | Required |
|----------|---------|----------|
| `ERLMCP_EMAIL_PASSWORD` | `"secure-password"` | If email alerts enabled |

**Default**: None - must be set if email alerts used

---

### Monitoring Integration
**Purpose**: API keys for monitoring and observability platforms

| Variable | Example | Required |
|----------|---------|----------|
| `ERLMCP_PAGERDUTY_KEY` | `"pagerduty-integration-key"` | If PagerDuty enabled |
| `ERLMCP_DATADOG_API_KEY` | `"datadog-api-key"` | If Datadog enabled |
| `ERLMCP_NEWRELIC_API_KEY` | `"newrelic-api-key"` | If New Relic enabled |
| `ERLMCP_NEWRELIC_ACCOUNT_ID` | `"account-id"` | If New Relic enabled |
| `ERLMCP_GRAFANA_USERNAME` | `"grafana-user"` | If Grafana Cloud enabled |
| `ERLMCP_GRAFANA_PASSWORD` | `"grafana-password"` | If Grafana Cloud enabled |

**Defaults**: Not set - integrations disabled if variables not provided

---

### Alerting Configuration
**Purpose**: Third-party alerting webhook credentials

| Variable | Example | Required |
|----------|---------|----------|
| `ERLMCP_SLACK_WEBHOOK` | `"https://hooks.slack.com/..."` | If Slack alerts enabled |
| `ERLMCP_WEBHOOK_AUTH_TOKEN` | `"Bearer token"` | If webhook enabled |

**Defaults**: Not set - alerting disabled if variables not provided

---

### Path Configuration
**Purpose**: Allowed resource directories for file operations

| Variable | Example | Required |
|----------|---------|----------|
| `ERLMCP_ALLOWED_PATHS` | `"/var/mcp:/data:/tmp"` | No |

**Format**: Colon-separated (Unix) or semicolon-separated (Windows)

**Default**: `["/tmp"]` - Safe shared temp directory only

**Important**:
- Never hardcode developer-specific paths
- Use `/var` or `/data` in production
- Always include `/tmp` for temporary operations
- Restrict to necessary directories only

---

## Security Best Practices

### 1. Credential Rotation

**Monthly Schedule**:
```bash
# Update secret in secrets manager
aws secretsmanager update-secret --secret-id erlmcp/email-password \
  --secret-string "new-password"

# Or in Vault
vault kv put secret/erlmcp/prod email_password="new-password"

# Or in Kubernetes
kubectl patch secret erlmcp-credentials \
  -p '{"data":{"email-password":"'$(echo -n new-password | base64)'"}}'

# Restart deployment to pick up new credentials
kubectl rollout restart deployment/erlmcp
```

### 2. Audit & Monitoring

Track:
- When credentials are accessed
- Who accessed them
- What operations used them
- Any failed authentication attempts

```bash
# CloudTrail for AWS
aws cloudtrail lookup-events --lookup-attributes AttributeKey=ResourceName,AttributeValue=erlmcp

# Vault audit
vault audit list

# Kubernetes audit
kubectl logs -l audit=true
```

### 3. Access Control

- Restrict environment variable access to CI/CD systems only
- Use IAM roles instead of long-lived credentials
- Enable MFA for secrets management access
- Audit who can view/modify secrets

### 4. Documentation

- Maintain current list of all environment variables
- Document rotation procedures
- Keep deployment playbooks updated
- Record any credential exposure incidents

---

## Troubleshooting

### Environment Variable Not Found

```bash
# Verify variable is set
echo $ERLMCP_EMAIL_PASSWORD

# Check in running container
docker exec erlmcp-container env | grep ERLMCP_EMAIL_PASSWORD

# Check in Kubernetes pod
kubectl exec erlmcp-pod -- env | grep ERLMCP_EMAIL_PASSWORD
```

### Configuration Load Errors

```bash
# Check logs for configuration issues
make dev-console
> application:get_env(erlmcp, roots).
% Should return {ok, Config} with loaded values
```

### Missing Default Fallback

```bash
# If variable not set, default should be used
# Example: ERLMCP_ALLOWED_PATHS defaults to ["/tmp"]
> application:get_env(erlmcp, roots).
{ok, [{allowed_paths, ["/tmp"]}, ...]}
```

---

## Configuration Examples

### Development (Local)

```bash
# .env.development
ERLMCP_EMAIL_PASSWORD="dev-password"
ERLMCP_PAGERDUTY_KEY="dev-key"
ERLMCP_ALLOWED_PATHS="/Users/sac/projects:/tmp"
```

### Staging (Docker)

```bash
# .env.staging
ERLMCP_EMAIL_PASSWORD="${STAGING_EMAIL_PASSWORD}"
ERLMCP_PAGERDUTY_KEY="${STAGING_PAGERDUTY_KEY}"
ERLMCP_ALLOWED_PATHS="/data:/tmp"
```

### Production (Kubernetes)

```yaml
env:
- name: ERLMCP_ALLOWED_PATHS
  value: "/var/mcp:/data:/tmp"
- name: ERLMCP_EMAIL_PASSWORD
  valueFrom:
    secretKeyRef:
      name: erlmcp-secrets
      key: email-password
# ... all other secrets
```

---

## Migration from Hardcoded Config

If upgrading from erlmcp < 0.6.0:

1. **Identify hardcoded values** in old config
2. **Create secrets** in your secrets management system
3. **Update deployment** to use environment variables
4. **Test in staging** with new configuration
5. **Rotate credentials** that were exposed
6. **Deploy to production**
7. **Verify** environment variables loaded correctly

### Before (OLD - Don't use)
```erlang
{email_password, "changeme"}  % ❌ UNSAFE
```

### After (NEW - Current)
```erlang
{email_password, {env, "ERLMCP_EMAIL_PASSWORD"}}  % ✅ SECURE
```

---

## Support

For deployment questions or issues:
1. Check `docs/HARDCODED_VALUES_AUDIT.md` for security details
2. Review test suite: `test/erlmcp_config_security_tests.erl`
3. File an issue with deployment logs (sanitized)
4. Contact security team for credential concerns

---

**Last Updated**: 2026-01-27
**Maintained By**: Agent 4 (Security Remediation)
**Security Review**: Quarterly
