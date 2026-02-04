# erlmcp v3 - Security Configuration Guide

## 🚨 Security Overview

This document describes the security configuration for erlmcp v3, focusing on proper secrets management, environment variable handling, and Docker security best practices.

## 🔐 Secrets Management Architecture

### 1. Development Environment (`docker-compose.dev.yml`)

Development environments use **environment variables** with fallback defaults for convenience while maintaining security:

```yaml
environment:
  # Erlang cookie from environment variable or secure default
  - ERLANG_COOKIE=${ERLANG_COOKIE:-erlmcp_dev_secure_cookie_dev_only}

  # Database password from environment variable or development default
  - ERLMCP_POSTGRES_PASSWORD=${ERLMCP_POSTGRES_PASSWORD:-dev_password_insecure}

  # Database connection using environment variables
  - ERLMCP_POSTGRES_URI=postgresql://erlmcp:${ERLMCP_POSTGRES_PASSWORD}@postgres:5432/erlmcp
```

**Security Features:**
- No hardcoded credentials in the compose file
- Environment variables override defaults
- Development defaults are still secure (not the original "change_in_production")
- Support for `.env.dev` file for local development

### 2. Production Environment (`docker-compose.prod.yml`)

Production environments use **Docker secrets** for maximum security:

```yaml
secrets:
  erlang-cookie:
    file: ./docker-secrets/erlang.cookie
  postgres-password:
    file: ./docker-secrets/postgres-password
  redis-password:
    file: ./docker-secrets/redis-password
  grafana-password:
    file: ./docker-secrets/grafana-password

# Usage in services
secrets:
  - erlang-cookie
  - postgres-password
```

**Security Features:**
- Secrets are mounted at runtime at `/run/secrets/`
- No secrets in compose files or environment variables
- Secrets are never stored in containers
- Files are read-only in containers
- Full isolation between environments

## 🔧 Configuration Files

### Environment File Structure

```
erlmcp/
├── .env.dev.template          # Development template (committed)
├── .env.staging.template      # Staging template (committed)
├── .env.prod.template         # Production template (committed)
├── .env.craftplan             # CraftPlan specific (ignored by git)
├── .env.craftplan.example     # Example (committed)
├── .env.dev                   # Local dev (ignored by git)
├── .env.staging               # Staging (ignored by git)
└── .env.production            # Production (ignored by git)
```

### Environment Variables

#### Core Variables (All Environments)
```bash
# Application Profile
ERLMCP_PROFILE=development|staging|production

# Node Configuration
ERLMCP_NODE_NAME=erlmcp@hostname
ERLMCP_DISTRIBUTION_MODE=development|staging|production

# Logging
ERLMCP_LOG_LEVEL=debug|info|warn|error

# Observability
OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4317
OTEL_SERVICE_NAME=erlmcp
```

#### Security Variables (Environment Specific)
```bash
# Development - uses defaults
ERLANG_COOKIE=your_dev_cookie_here
ERLMCP_POSTGRES_PASSWORD=dev_password_here

# Production - must be set
# No variables - all via Docker secrets
```

## 🐳 Docker Security Implementation

### 1. Development Container Security

```yaml
# Development containers are permissive for convenience
security_opt: []  # No restrictions for development
volumes:
  - ./apps:/workspace/apps:rw  # Read-write for hot reload
```

### 2. Production Container Security

```yaml
# Production containers are locked down
security_opt:
  - no-new-privileges:true    # No privilege escalation
  - apparmor:docker-default  # AppArmor protection

# Read-only filesystems for maximum security
volumes:
  - ./apps:/workspace/apps:ro    # Read-only
  - ./config:/workspace/config:ro # Read-only

# Resource limits to prevent resource exhaustion
deploy:
  resources:
    limits:
      cpus: '2.0'
      memory: 4G
```

### 3. Network Security

```yaml
# Development - flexible networking
networks:
  - erlmcp-dev

# Production - isolated network
networks:
  - erlmcp-prod:
    internal: false
    driver: bridge
```

## 📋 Security Verification

### Running the Security Verification Script

```bash
# Verify the entire security configuration
./scripts/verify-security-config.sh

# Expected output:
# 🔍 erlmcp v3 - Security Configuration Verification
# =================================================
# ✅ PASS: No hardcoded credentials in docker-compose.dev.yml
# ✅ PASS: No hardcoded credentials in docker-compose.prod.yml
# ✅ PASS: No hardcoded credentials in docker-compose.override.dev.yml
# ✅ PASS: Docker secrets not in version control
# ✅ PASS: No .env files in version control
# ✅ PASS: /dev/stdin has ERLANG_COOKIE variable
# ✅ PASS: Docker secrets not in version control
# ✅ PASS: Erlang cookie exists and doesn't contain 'dev'
# ✅ PASS: Required secret postgres-password not found (expected - use Docker secrets)
# ✅ PASS: Production compose file uses Docker secrets
# ✅ PASS: Dev compose file uses environment variables for ERLANG_COOKIE
# ✅ PASS: Development compose file is valid
# ✅ PASS: Production compose file is valid

# 📊 Security Verification Summary
# ================================
# Total issues found: 0
# ✅ All security checks passed!
```

### Manual Checks

1. **No Hardcoded Credentials:**
   ```bash
   # Check for any hardcoded credentials
   grep -r "dev_cookie_change_in_production" .
   grep -r "ERLAMCP_DEV_COOKIE=" .
   ```

2. **Secrets Management:**
   ```bash
   # Verify secrets are not in git
   git ls-files | grep "docker-secrets"

   # Should return nothing (no output means no secrets in git)
   ```

3. **Environment Files:**
   ```bash
   # Check for real .env files (should not be in git)
   git ls-files | grep "\.env[^/]*$"

   # Should return nothing
   ```

## 🚀 Deployment Workflows

### 1. Development Setup

```bash
# Create local environment file
cp .env.dev.template .env.dev
# Edit .env.dev with your local settings

# Start development environment
docker compose -f docker-compose.dev.yml up -d

# Verify running
docker compose -f docker-compose.dev.yml logs -f erlmcp
```

### 2. Production Deployment

```bash
 # Create secrets (do this once)
echo "secure_production_cookie_123456" > docker-secrets/erlang.cookie
echo "secure_postgres_password_abc123" > docker-secrets/postgres-password
echo "secure_redis_password_xyz789" > docker-secrets/redis-password
echo "secure_grafana_password_def456" > docker-secrets/grafana-password

# Set proper permissions
chmod 600 docker-secrets/*

# Start production environment
docker compose -f docker-compose.prod.yml up -d

# Verify running
docker compose -f docker-compose.prod.yml logs -f erlmcp
```

### 3. Secret Rotation

```bash
# Rotate production secrets
echo "new_secure_cookie_789012" > docker-secrets/erlang.cookie
echo "new_secure_post_pwd_345678" > docker-secrets/postgres-password

# Restart services with new secrets
docker compose -f docker-compose.prod.yml down
docker compose -f docker-compose.prod.yml up -d
```

## 🔍 Security Best Practices

### 1. Never Commit Secrets
- All `.env.*` files should be in `.gitignore`
- Only template files should be committed
- Real secrets should be stored outside version control

### 2. Use Environment-Specific Configuration
- Development: Use environment variables with defaults
- Production: Use Docker secrets only
- Staging: Mirror production security practices

### 3. Regular Security Audits
- Run verification script after any configuration changes
- Review secrets rotation procedures
- Monitor container security regularly

### 4. Network Security
- Use isolated networks for production
- Limit port exposure to only necessary ports
- Use TLS for production endpoints

### 5. Resource Security
- Implement resource limits in production
- Use read-only filesystems where possible
- Monitor resource usage regularly

## 🛡️ Security Checklist

### Before Deployment
- [ ] Run `./scripts/verify-security-config.sh` - must pass
- [ ] Verify all Docker secrets exist and are secure
- [ ] Check no hardcoded credentials in compose files
- [ ] Verify network isolation is configured
- [ ] Review resource limits

### After Deployment
- [ ] Verify all services are healthy
- [ ] Check logs for any security warnings
- [ ] Verify secrets are properly mounted
- [ ] Test connectivity between services

### Regular Maintenance
- [ ] Rotate secrets quarterly
- [ ] Update Docker images regularly
- [ ] Monitor security advisories
- [ ] Update verification script as needed

## 🔧 Troubleshooting

### Common Issues

1. **Secret not found error:**
   ```bash
   # Ensure secrets directory exists and has files
   ls -la docker-secrets/

   # Check file permissions
   ls -la docker-secrets/*
   ```

2. **Environment variable not set:**
   ```bash
   # Check if .env file exists
   ls -la .env.*

   # Verify syntax in compose files
   docker compose -f docker-compose.dev.yml config
   ```

3. **Container security warnings:**
   ```bash
   # Check security options
   docker inspect erlmcp-prod | grep SecurityOpt

   # Verify volume mounts
   docker inspect erlmcp-prod | grep Mounts
   ```

## 📚 References

- [Docker Secrets Documentation](https://docs.docker.com/engine/swarm/secrets/)
- [Docker Security Best Practices](https://docs.docker.com/engine/security/)
- [Environment Variables in Docker](https://docs.docker.com/compose/environment-variables/)
- [Erlang/OTP Security Guidelines](https://erlang.org/doc/apps/erts/supervisor.html)