# Enterprise CI/CD Pipeline for erlmcp v3

## Overview

This guide explains the comprehensive CI/CD pipeline designed for erlmcp v3, featuring enterprise-grade reliability, security, and automation capabilities.

## Pipeline Architecture

### Multi-Stage Build Pipeline

1. **Build & Validation Phase**
   - OTP compilation with version matrix testing
   - Security scanning with Trivy
   - Dependency vulnerability analysis
   - Artifacts integrity verification

2. **Testing Phase**
   - Multi-platform testing matrix (Ubuntu, macOS)
   - Multi-version Erlang/OTP testing
   - Chicago School TDD compliance
   - EUnit and Common Test execution

3. **Performance & Quality Gates**
   - Benchmark execution and regression detection
   - MCP compliance validation
   - Documentation completeness check
   - Performance baseline comparison

4. **Deployment Phase**
   - Staging deployment with canary releases
   - Production deployment with blue-green strategy
   - Automated rollback procedures
   - Comprehensive health checks

### Quality Gates

| Gate | Description | Threshold | Action |
|------|-------------|-----------|--------|
| Security | Vulnerability count | 0 | Fail deployment |
| Tests | EUnit/CT failures | 0 | Fail pipeline |
| Performance | P99 latency | <500ms | Warn |
| Compliance | MCP compliance | 100% | Fail deployment |
| Coverage | Code coverage | ≥80% | Warn |

## Deployment Strategies

### Blue-Green Deployment

```bash
# Manual switch (production)
./scripts/deploy/blue-green-switch.sh production

# Automated rollback
./scripts/rollback/automated-rollback.sh production
```

### Canary Releases

- **5% traffic** for canary initially
- Gradual increase: 5% → 15% → 50% → 100%
- Automatic rollback on failure
- Monitoring and validation at each stage

### Feature Flags

```yaml
# Staging features
experimental.new_api: true
websocket: false
grpc: true

# Production features
experimental.new_api: false
websocket: false
grpc: false
```

## Infrastructure as Code

### Terraform Configuration

The pipeline uses Terraform for infrastructure provisioning:

```bash
# Initialize terraform
terraform init

# Plan deployment
terraform plan -var="environment=production"

# Apply configuration
terraform apply -var="environment=production"
```

### Key Components

1. **VPC & Networking**
   - Public, private, and database subnets
   - Security groups for load balancer and nodes
   - NAT gateway for outbound connectivity

2. **EKS Cluster**
   - Multi-AZ node group
   - Auto-scaling configuration
   - OIDC integration for IAM roles

3. **Load Balancing**
   - Application Load Balancer with SSL
   - Route 53 DNS management
   - SSL certificates via ACM

## Kubernetes Deployment

### Helm Charts

The deployment uses Helm for package management:

```bash
# Install erlmcp
helm install erlmcp ./charts/erlmcp \
  --set environment=production \
  --set replicaCount=3 \
  --set global.registry=ghcr.io/your-org
```

### Deployment Manifests

- **Staging**: Blue-green deployment with canary
- **Production**: Automated rollout with canary
- **Monitoring**: Service monitors for Prometheus

### Health Checks

```yaml
livenessProbe:
  httpGet:
    path: /mcp/health
  initialDelaySeconds: 60
  periodSeconds: 10

readinessProbe:
  httpGet:
    path: /mcp/health?ready=1
  initialDelaySeconds: 30
```

## Monitoring & Observability

### Metrics Collection

- **Application metrics**: HTTP requests, response times, error rates
- **System metrics**: CPU, memory, network I/O
- **Business metrics**: Active sessions, throughput, API usage

### Alerting

- **Critical**: Service unavailable, high error rate
- **Warning**: High latency, resource exhaustion
- **Info**: Deployment events, configuration changes

### Tracing

- **Distributed tracing** via OpenTelemetry
- **Request correlation** for end-to-end tracking
- **Performance analysis** with Jaeger

## Security Features

### Scanning

1. **Container Security**
   - Trivy vulnerability scanner
   - OWASP dependency check
   - Image signing and verification

2. **Code Security**
   - Static application security testing (SAST)
   - Secret detection
   - Compliance validation

### Network Security

- **Network policies** for pod isolation
- **TLS encryption** for all communications
- **Certificate management** with Let's Encrypt

### Access Control

- **IAM roles** for service accounts
- **Least privilege principle**
- **RBAC for Kubernetes access**

## Disaster Recovery

### Backup Strategy

1. **Infrastructure backups**
   - EKS cluster state
   - Terraform state files
   - Configuration management

2. **Application backups**
   - Database snapshots
   - Configuration archives
   - Artifact repositories

### Recovery Procedures

1. **Infrastructure Recovery**
   ```bash
   # Restore from state
   terraform apply -auto-approve

   # Recreate resources
   kubectl apply -f manifests/
   ```

2. **Application Recovery**
   ```bash
   # Deploy from latest artifacts
   ./scripts/deploy/blue-green-switch.sh production

   # Verify deployment
   ./scripts/health/smoke_tests.sh production
   ```

## Cost Optimization

### Reserved Instances

- **3-year reserved instances** for steady-state workloads
- **Convertible reserved instances** for flexibility
- **Savings plan** for predictable spend

### Spot Instances

- **Spot instances** for fault-tolerant workloads
- **Price-based scaling** to optimize costs
- **Backup capacity** for critical workloads

### Monitoring Costs

- **Budget alerts** for cost thresholds
- **Resource utilization** monitoring
- **Rightsizing recommendations**

## Performance Tuning

### Cluster Scaling

- **Horizontal pod autoscaling** based on CPU/memory
- **Vertical pod autoscaling** for optimal resource allocation
- **Cluster autoscaling** for node capacity

### Load Balancing

- **Load-based routing** for traffic distribution
- **Session affinity** for stateful applications
- **Health checks** for endpoint validation

### Caching

- **Redis** for application-level caching
- **CDN** for static content
- **In-memory caching** for frequently accessed data

## Troubleshooting

### Common Issues

1. **Deployment Failures**
   - Check resource limits
   - Verify image availability
   - Review configuration files

2. **Performance Issues**
   - Monitor resource utilization
   - Check for bottlenecks
   - Review logs for errors

3. **Connectivity Issues**
   - Verify network policies
   - Check DNS resolution
   - Test load balancer health

### Debug Commands

```bash
# Check deployment status
kubectl get deployment erlmcp -n erlmcp-production

# View pod logs
kubectl logs -f deployment/erlmcp -n erlmcp-production

# Check resource usage
kubectl top pods -n erlmcp-production

# Debug ingress
kubectl describe ingress erlmcp-green-ingress -n erlmcp-production
```

## Support

For issues and questions:
- GitHub Issues: https://github.com/your-org/erlmcp/issues
- Documentation: https://docs.erlmcp.com
- Support: support@erlmcp.com

## Contributing

1. Fork the repository
2. Create a feature branch
3. Submit a pull request
4. Review and merge

## License

This project is licensed under the MIT License.