# Hardcoded Values Security Audit - erlmcp v0.6.0

**Report Date**: 2026-01-27
**Severity**: HIGH
**Status**: REMEDIATED

## Executive Summary

This audit documents all hardcoded credentials and paths found in the erlmcp codebase and the remediation steps taken to move them to environment variables and secure configuration.

### Audit Findings

**Total Issues Found**: 9 hardcoded credentials + 1 hardcoded path
**All Issues**: REMEDIATED ✓
**Deployment Impact**: Can now deploy to any system without code changes

## Detailed Findings

### 1. Email SMTP Credentials (config/sys.config)

**Location**: `config/sys.config:232`

**Finding**:
```erlang
{email_password, "changeme"},
```

**Risk**: Placeholder credential visible in configuration file. Cannot be deployed to production without hardcoding real password into code.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{email_password, {env, "ERLMCP_EMAIL_PASSWORD"}},
```

**Migration Guide**: Set environment variable before deployment:
```bash
export ERLMCP_EMAIL_PASSWORD="your-smtp-password"
```

---

### 2. PagerDuty Integration Key (config/sys.config)

**Location**: `config/sys.config:236`

**Finding**:
```erlang
{pagerduty_integration_key, "changeme"},
```

**Risk**: API key for critical alerting system exposed in config.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{pagerduty_integration_key, {env, "ERLMCP_PAGERDUTY_KEY"}},
```

**Migration Guide**:
```bash
export ERLMCP_PAGERDUTY_KEY="your-pagerduty-key"
```

---

### 3. Generic Webhook Auth Header (config/sys.config)

**Location**: `config/sys.config:241`

**Finding**:
```erlang
{webhook_auth_header, "Bearer changeme"},
```

**Risk**: Bearer token for webhook authentication exposed.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{webhook_auth_header, {env, "ERLMCP_WEBHOOK_AUTH_TOKEN"}},
```

**Migration Guide**:
```bash
export ERLMCP_WEBHOOK_AUTH_TOKEN="Bearer your-actual-token"
```

---

### 4. Datadog API Key (config/sys.config)

**Location**: `config/sys.config:266`

**Finding**:
```erlang
{datadog_api_key, "changeme"},
```

**Risk**: Datadog monitoring API key exposed.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{datadog_api_key, {env, "ERLMCP_DATADOG_API_KEY"}},
```

**Migration Guide**:
```bash
export ERLMCP_DATADOG_API_KEY="your-datadog-api-key"
```

---

### 5. New Relic API Key (config/sys.config)

**Location**: `config/sys.config:270`

**Finding**:
```erlang
{newrelic_api_key, "changeme"},
```

**Risk**: New Relic monitoring API key exposed.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{newrelic_api_key, {env, "ERLMCP_NEWRELIC_API_KEY"}},
```

**Migration Guide**:
```bash
export ERLMCP_NEWRELIC_API_KEY="your-newrelic-api-key"
```

---

### 6. New Relic Account ID (config/sys.config)

**Location**: `config/sys.config:271`

**Finding**:
```erlang
{newrelic_account_id, "changeme"},
```

**Risk**: Account identifier for external monitoring service exposed.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{newrelic_account_id, {env, "ERLMCP_NEWRELIC_ACCOUNT_ID"}},
```

**Migration Guide**:
```bash
export ERLMCP_NEWRELIC_ACCOUNT_ID="your-account-id"
```

---

### 7. Grafana Cloud Username (config/sys.config)

**Location**: `config/sys.config:275`

**Finding**:
```erlang
{grafana_cloud_username, "changeme"},
```

**Risk**: Grafana Cloud credentials exposed.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{grafana_cloud_username, {env, "ERLMCP_GRAFANA_USERNAME"}},
```

**Migration Guide**:
```bash
export ERLMCP_GRAFANA_USERNAME="your-grafana-username"
```

---

### 8. Grafana Cloud Password (config/sys.config)

**Location**: `config/sys.config:276`

**Finding**:
```erlang
{grafana_cloud_password, "changeme"},
```

**Risk**: Grafana Cloud password exposed.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{grafana_cloud_password, {env, "ERLMCP_GRAFANA_PASSWORD"}},
```

**Migration Guide**:
```bash
export ERLMCP_GRAFANA_PASSWORD="your-grafana-password"
```

---

### 9. Slack Webhook URL (config/sys.config)

**Location**: `config/sys.config:223`

**Finding**:
```erlang
{slack_webhook, "https://hooks.slack.com/services/YOUR/WEBHOOK/URL"},
```

**Risk**: Placeholder webhook URL that would be replaced with real URL containing sensitive token.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{slack_webhook, {env, "ERLMCP_SLACK_WEBHOOK"}},
```

**Migration Guide**:
```bash
export ERLMCP_SLACK_WEBHOOK="https://hooks.slack.com/services/YOUR/WEBHOOK/URL"
```

---

### 10. Hardcoded Path: /Users/sac/projects (config/sys.config)

**Location**: `config/sys.config:344`

**Finding**:
```erlang
{allowed_paths, [
    "/Users/sac/projects",
    "/tmp"
]},
```

**Risk**: Developer machine-specific path hardcoded. Cannot be deployed to production or other systems without modifying config.

**Remediation**: ✓ MOVED TO ENVIRONMENT VARIABLE
```erlang
{allowed_paths, {env, "ERLMCP_ALLOWED_PATHS", ["/tmp"]}},
```

**Migration Guide**:
```bash
# For development
export ERLMCP_ALLOWED_PATHS="/Users/sac/projects:/tmp"

# For production
export ERLMCP_ALLOWED_PATHS="/var/mcp:/data:/tmp"

# Path separator: colon (:) on Unix, semicolon (;) on Windows
```

**Default Behavior**: If not set, defaults to `["/tmp"]` for safety.

---

## Implementation Details

### Environment Variable Loading

All environment variables are loaded using Erlang's configuration variable substitution syntax:

```erlang
%% Example: {key, {env, "ENV_VAR_NAME"}}
{email_password, {env, "ERLMCP_EMAIL_PASSWORD"}},

%% With default fallback: {key, {env, "ENV_VAR_NAME", DefaultValue}}
{allowed_paths, {env, "ERLMCP_ALLOWED_PATHS", ["/tmp"]}},
```

### Configuration File Changes

**Modified**: `config/sys.config`

All 10 hardcoded values replaced with environment variable references.

### Path Handling

The `allowed_paths` setting now accepts environment variable input as a colon-separated string (Unix) or semicolon-separated string (Windows):

```erlang
%% Environment variable: "ERLMCP_ALLOWED_PATHS=/path1:/path2:/path3"
%% Parsed as: ["/path1", "/path2", "/path3"]
```

---

## Environment Variables Reference

### Email Configuration
- `ERLMCP_EMAIL_PASSWORD` - SMTP password for email alerts

### Monitoring Integration
- `ERLMCP_PAGERDUTY_KEY` - PagerDuty API integration key
- `ERLMCP_DATADOG_API_KEY` - Datadog API key
- `ERLMCP_NEWRELIC_API_KEY` - New Relic API key
- `ERLMCP_NEWRELIC_ACCOUNT_ID` - New Relic account ID
- `ERLMCP_GRAFANA_USERNAME` - Grafana Cloud username
- `ERLMCP_GRAFANA_PASSWORD` - Grafana Cloud password
- `ERLMCP_SLACK_WEBHOOK` - Slack webhook URL

### Security & Paths
- `ERLMCP_WEBHOOK_AUTH_TOKEN` - Bearer token for webhook authentication
- `ERLMCP_ALLOWED_PATHS` - Colon-separated list of allowed resource paths (default: "/tmp")

---

## Migration Checklist

### Development Environment
- [ ] Create `.env.development` file with all environment variables
- [ ] Source the file before starting: `source .env.development`
- [ ] Verify configuration loads correctly with `make dev-console`

### Staging Environment
- [ ] Set all environment variables in staging deployment platform
- [ ] For Docker: Use `--env-file` or set in Dockerfile/docker-compose.yml
- [ ] For Kubernetes: Use ConfigMaps and Secrets
- [ ] Verify no "changeme" values in logs

### Production Environment
- [ ] Set all environment variables in production deployment platform
- [ ] Use secrets management (AWS Secrets Manager, HashiCorp Vault, Kubernetes Secrets)
- [ ] Enable audit logging for credential access
- [ ] Rotate credentials on deployment
- [ ] Monitor for credential exposure

---

## Testing & Validation

### Automated Tests
See `test/erlmcp_config_security_tests.erl` for comprehensive test suite covering:
- No hardcoded credentials in configuration
- No hardcoded paths except /tmp default
- Environment variables loaded correctly
- Sensible defaults when variables not set
- Config validation rejects invalid paths

### Manual Verification
```bash
# Ensure no "changeme" strings in compiled code
grep -r "changeme" /Users/sac/erlmcp/_build/ || echo "✓ No hardcoded credentials found"

# Verify environment variable loading
make dev-console
> erlmcp_config:get_email_password().
{ok, "your-actual-password"}

# Check path configuration
> erlmcp_config:get_allowed_paths().
{ok, ["/path1", "/path2"]}
```

---

## Security Best Practices

### 1. Never Commit Credentials to Git
```bash
# Add to .gitignore
echo ".env*" >> .gitignore
echo "*.secret" >> .gitignore
```

### 2. Use Secrets Management
- **Development**: `.env` files (gitignored)
- **Staging**: Secrets manager (AWS Secrets Manager, Vault)
- **Production**: Kubernetes Secrets or cloud secrets service

### 3. Credential Rotation
- Rotate API keys monthly
- Use time-limited tokens when possible
- Audit credential usage in logs

### 4. Access Control
- Restrict environment variable access to deployment systems
- Use IAM roles for cloud credentials
- Audit who can view secrets

### 5. Monitoring
- Monitor for credential exposure in logs
- Alert on unusual API key usage
- Track credential lifecycle

---

## Deployment Examples

### Docker (Development)
```dockerfile
FROM erlang:25-alpine
WORKDIR /app
COPY . .
ARG ERLMCP_EMAIL_PASSWORD
ARG ERLMCP_PAGERDUTY_KEY
ENV ERLMCP_EMAIL_PASSWORD=${ERLMCP_EMAIL_PASSWORD}
ENV ERLMCP_PAGERDUTY_KEY=${ERLMCP_PAGERDUTY_KEY}
RUN make compile
CMD ["make", "dev-console"]
```

### Docker Compose
```yaml
services:
  erlmcp:
    image: erlmcp:latest
    environment:
      ERLMCP_EMAIL_PASSWORD: ${ERLMCP_EMAIL_PASSWORD}
      ERLMCP_PAGERDUTY_KEY: ${ERLMCP_PAGERDUTY_KEY}
      ERLMCP_ALLOWED_PATHS: /data:/tmp
```

### Kubernetes
```yaml
apiVersion: v1
kind: Secret
metadata:
  name: erlmcp-secrets
type: Opaque
stringData:
  email_password: "your-smtp-password"
  pagerduty_key: "your-pagerduty-key"
---
apiVersion: v1
kind: ConfigMap
metadata:
  name: erlmcp-config
data:
  allowed_paths: "/var/mcp:/data:/tmp"
---
apiVersion: v1
kind: Pod
metadata:
  name: erlmcp
spec:
  containers:
  - name: erlmcp
    image: erlmcp:latest
    env:
    - name: ERLMCP_EMAIL_PASSWORD
      valueFrom:
        secretKeyRef:
          name: erlmcp-secrets
          key: email_password
    - name: ERLMCP_PAGERDUTY_KEY
      valueFrom:
        secretKeyRef:
          name: erlmcp-secrets
          key: pagerduty_key
    - name: ERLMCP_ALLOWED_PATHS
      valueFrom:
        configMapKeyRef:
          name: erlmcp-config
          key: allowed_paths
```

---

## Conclusion

All hardcoded credentials and paths have been remediated and moved to environment variable configuration. The codebase is now:

- ✅ **Secure**: No credentials visible in code
- ✅ **Portable**: Can deploy to any system without code changes
- ✅ **Configurable**: All settings through environment variables
- ✅ **Compliant**: Follows security best practices
- ✅ **Tested**: Comprehensive test suite validates configuration

### Remaining Actions
1. Update deployment pipelines to set environment variables
2. Update documentation with deployment-specific instructions
3. Rotate all credentials that were exposed
4. Monitor logs for any remaining hardcoded values

---

**Auditor**: Agent 4 (Security Remediation)
**Review Date**: 2026-01-27
**Next Audit**: Quarterly security review recommended
