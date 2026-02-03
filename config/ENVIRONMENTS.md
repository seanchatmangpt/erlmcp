# Environment Configuration Templates for erlmcp v3

This directory contains environment-specific configuration templates for erlmcp v3. Each environment (dev, staging, production) has its own set of configuration files optimized for that environment's purpose.

## Quick Start

```bash
# Development
cp .env.dev.template .env.dev
docker compose -f docker-compose.yml -f docker-compose.override.dev.yml --env-file .env.dev up

# Staging
cp .env.staging.template .env.staging
# Create secrets first (see below)
docker compose -f docker-compose.yml -f docker-compose.override.staging.yml --env-file .env.staging up

# Production
cp .env.prod.template .env.prod
# Create secrets first (see below)
docker stack deploy -c docker-compose.yml -c docker-compose.override.prod.yml --env-file .env.prod erlmcp
```

## File Structure

```
config/
├── sys.env.dev          # Development system configuration (Erlang term format)
├── sys.env.staging      # Staging system configuration
├── sys.env.prod         # Production system configuration
├── dev-vm.args          # Development VM arguments
├── staging-vm.args      # Staging VM arguments
└── prod-vm.args         # Production VM arguments

.env.dev.template        # Development environment variables template
.env.staging.template    # Staging environment variables template
.env.prod.template       # Production environment variables template

docker-compose.override.dev.yml    # Development Docker Compose override
docker-compose.override.staging.yml # Staging Docker Compose override
docker-compose.override.prod.yml    # Production Docker Compose override
```

## Environment Comparison

| Setting | Development | Staging | Production |
|---------|-------------|---------|------------|
| Log Level | debug | info | warn |
| Debug Endpoints | enabled | enabled | disabled |
| TLS/SSL | disabled | enabled | required |
| Rate Limiting | disabled | enabled | enabled |
| Experimental Features | enabled | enabled | disabled |
| Resource Limits | low | moderate | high |
| Health Checks | disabled | enabled | enabled |
| Profiling | enabled | enabled | disabled |
| Monitoring | optional | enabled | required |

## Development Environment

**Purpose**: Fast local development with minimal external dependencies.

**Features**:
- Hot-reload code mounting
- Debug ports exposed (debugger, profiler)
- All security features disabled for easy testing
- Minimal resource limits
- Extensive logging

**Usage**:
```bash
# Set up environment
cp .env.dev.template .env.dev
# Edit .env.dev as needed

# Start services
docker compose -f docker-compose.yml -f docker-compose.override.dev.yml --env-file .env.dev up

# Start with database and monitoring
docker compose -f docker-compose.yml -f docker-compose.override.dev.yml --env-file .env.dev --profile postgres --profile redis --profile monitoring up

# Interactive development
docker compose -f docker-compose.yml -f docker-compose.override.dev.yml --env-file .env.dev run --rm erlmcp /bin/bash
```

**Key Settings**:
- Cookie: `dev_cookie_12345` (change for team development)
- Ports: 8080 (API), 9110 (dist), 9091 (health)
- No TLS required
- No rate limiting

## Staging Environment

**Purpose**: Production-like environment for testing and validation.

**Features**:
- Production-like resource limits
- Monitoring fully enabled
- TLS/SSL configured
- Health checks active
- Debug endpoints available for troubleshooting
- Experimental features enabled

**Prerequisites**:
```bash
# Create required secrets for staging
echo "your-staging-cookie" | docker secret create erlmcp-staging-erlang-cookie -
echo "your-staging-db-password" | docker secret create erlmcp-staging-db-password -
echo "your-staging-redis-password" | docker secret create erlmcp-staging-redis-password -
echo "your-staging-jwt-secret" | docker secret create erlmcp-staging-jwt-secret -
cat staging-tls.crt | docker secret create erlmcp-staging-tls-cert -
cat staging-tls.key | docker secret create erlmcp-staging-tls-key -
```

**Usage**:
```bash
# Set up environment
cp .env.staging.template .env.staging
# Edit .env.staging with your staging values

# Deploy
docker compose -f docker-compose.yml -f docker-compose.override.staging.yml --env-file .env.staging up -d
```

**Key Settings**:
- Cookie: From Docker secret
- Ports: 8080 (API), 9100 (dist), 9090 (health)
- TLS enabled
- Rate limiting: 500 req/s
- Sampling rate: 10%

## Production Environment

**Purpose**: Hardened production deployment with full security and monitoring.

**Features**:
- Full TLS/SSL enforcement
- All secrets externalized to Docker secrets
- Health checks with restart policies
- Resource limits enforced
- No debug endpoints
- Comprehensive monitoring
- High availability support

**Prerequisites**:
```bash
# Create all required secrets for production
openssl rand -base64 48 | tr -d '\n' | docker secret create erlmcp-erlang-cookie -
echo "your-db-password" | docker secret create erlmcp-db-password -
echo "your-redis-password" | docker secret create erlmcp-redis-password -
cat tls.crt | docker secret create erlmcp-tls-cert -
cat tls.key | docker secret create erlmcp-tls-key -
cat ca-bundle.crt | docker secret create erlmcp-tls-ca -
cat jwt-private.pem | docker secret create erlmcp-jwt-private-key -
cat jwt-public.pem | docker secret create erlmcp-jwt-public-key -
echo "your-grafana-password" | docker secret create erlmcp-grafana-password -
cat otel-ca.crt | docker secret create erlmcp-otel-ca-cert -
```

**Usage**:
```bash
# Set up environment
cp .env.prod.template .env.prod
# Edit .env.prod with your production values

# Initialize swarm (if not already)
docker swarm init

# Deploy stack
docker stack deploy -c docker-compose.yml -c docker-compose.override.prod.yml --env-file .env.prod erlmcp

# Verify deployment
docker stack ps erlmcp
docker service logs erlmcp_erlmcp -f

# Update stack
docker stack deploy -c docker-compose.yml -c docker-compose.override.prod.yml --env-file .env.prod erlmcp

# Remove stack
docker stack rm erlmcp
```

**Key Settings**:
- Cookie: From Docker secret (REQUIRED)
- Ports: 8080 (API), 9100 (dist), 9090 (health)
- TLS required
- Rate limiting: 1000 req/s
- Sampling rate: 1%
- Replicas: 5

## Secret Management

### Development Secrets
Development uses simple environment variables in `.env.dev`:
```bash
ERLANG_COOKIE=dev_cookie_12345
DB_PASSWORD=dev_password
```

### Staging/Production Secrets
Staging and production use Docker secrets for security:

```bash
# Create a secret
echo "secret-value" | docker secret create secret-name -

# List secrets
docker secret ls

# Remove a secret
docker secret rm secret-name

# Update a secret (remove and recreate)
docker secret rm secret-name
echo "new-value" | docker secret create secret-name -
```

## Configuration File Details

### sys.env.{env}
Erlang term format configuration files that define application behavior:
- Logging levels and handlers
- Client/server defaults
- Transport settings
- Connection pooling
- Feature flags
- Observability settings

### vm.args.{env}
Erlang VM arguments that control the BEAM runtime:
- Node naming and distribution
- Memory allocation
- Process/port limits
- Scheduler settings
- Garbage collection
- SSL/TLS configuration

### .env.{env}.template
Environment variable templates for Docker Compose:
- Application settings
- Database connection
- Redis connection
- OpenTelemetry settings
- Feature flags
- Resource limits

### docker-compose.override.{env}.yml
Environment-specific Docker Compose overrides:
- Service configuration
- Port mappings
- Volume mounts
- Secret references
- Resource limits
- Health checks

## Troubleshooting

### Connection Issues
```bash
# Check node connectivity
docker exec erlmcp-prod erl -name debug@127.0.0.1 -setcookie $(cat .cookie) -remsh erlmcp@hostname

# Check distribution
docker exec erlmcp-prod netstat -tuln | grep 9100
```

### Cookie Issues
```bash
# Verify cookie is set
docker exec erlmcp-prod cat /run/secrets/erlang.cookie

# Check nodes can see each other
docker exec erlmcp-prod epmd -names
```

### Health Check Failures
```bash
# Check health status
docker inspect erlmcp-prod | jq '.[0].State.Health'

# Run health check manually
docker exec erlmcp-prod /opt/erlmcp/bin/healthcheck.sh
```

## Security Best Practices

1. **Never commit secrets**: Always use `.env.{env}.template` files, never `.env.{env}` with actual values
2. **Use Docker secrets**: All staging/production secrets should be Docker secrets
3. **Rotate credentials**: Regularly rotate cookies, passwords, and TLS certificates
4. **TLS required**: Always use TLS in production and staging
5. **Limit debug access**: Debug endpoints should be disabled in production
6. **Audit logs**: Enable audit logging in production
7. **Network isolation**: Use overlay networks with encryption in production

## Migration Guide

### Migrating from v2 to v3

1. Review new configuration structure
2. Create environment-specific files
3. Migrate custom settings to new locations
4. Update deployment scripts
5. Test in staging first
6. Roll out to production with monitoring

## Additional Resources

- [Docker Compose Documentation](https://docs.docker.com/compose/)
- [Docker Secrets](https://docs.docker.com/engine/swarm/secrets/)
- [Erlang VM Flags](https://erlang.org/doc/man/erl.html)
- [Erlang Configuration](https://erlang.org/doc/design_principles/config_principles.html)
