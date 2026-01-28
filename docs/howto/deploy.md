# How-To: Deploy erlmcp to Production

This guide covers building, testing, and deploying erlmcp to production environments.

## Prerequisites

- Erlang/OTP 25+ on target server
- Docker (optional, for containerized deployment)
- At least 2GB RAM
- Network access for MCP clients

## Step 1: Verify Production Readiness

Before deployment, ensure all quality gates pass:

```bash run
$ make check 2>&1 | grep "VALIDATION PASSED"
```

This verifies compilation, linting, type checking, and test passes.

## Step 2: Generate Production Release

Build an optimized production release with ERTS included:

```bash run
$ make release-prod 2>&1 | tail -5
```

The release includes a complete Erlang runtime, making it portable.

## Step 3: Verify Release Artifacts

Check that release binaries were generated:

```bash run
$ ls -lh _build/prod/rel/erlmcp/ | head -10
```

Expected: Directories like `bin/`, `lib/`, `erts-*/` present.

## Step 4: Create Release Tarball

Package the release for distribution:

```bash run
$ make tar 2>&1 | tail -3
```

Creates a compressed tarball ready for deployment.

## Step 5: Verify Tarball Contents

Inspect the release tarball:

```bash run
$ ls -lh _build/prod/*.tar.gz
```

This archive contains the complete application and runtime.

## Step 6: Run Pre-Deployment Tests

Execute the full test suite including performance tests:

```bash run
$ make test 2>&1 | tail -10
```

All tests must pass before deploying to production.

## Step 7: Start the Application

Extract and start erlmcp on the target system:

```bash run
$ cd /tmp && tar -xzf /Users/sac/erlmcp/_build/prod/erlmcp-*.tar.gz 2>&1 && echo "Extracted"
```

## Step 8: Configure Runtime Environment

Set up production configuration:

```bash run
$ cat > /tmp/erlmcp/releases/1.0.0/sys.config << 'EOF'
[
  {erlmcp, [
    {port, 8000},
    {max_connections, 10000},
    {enable_otel, true}
  ]}
].
EOF
echo "Config created"
```

## Step 9: Start the Application

Launch erlmcp with production configuration:

```bash run
$ /tmp/erlmcp/bin/erlmcp start 2>&1 | head -5
```

The application starts as a background daemon.

## Step 10: Verify Application Health

Check that the application is running and healthy:

```bash run
$ /tmp/erlmcp/bin/erlmcp ping 2>&1
```

Returns "pong" if the application is responsive.

## Docker Deployment

### Build Docker Image

Create a containerized erlmcp image:

```bash run
$ make docker-build 2>&1 | grep "successfully"
```

### Start Docker Container

Launch erlmcp in a Docker container:

```bash run
$ make docker-up 2>&1 | grep "running"
```

### Verify Container Health

Check container status:

```bash run
$ docker ps | grep erlmcp | awk '{print $NF}'
```

Shows the container is running.

## Cluster Deployment

### Initialize Cluster

Set up a multi-node cluster:

```bash run
$ make swarm-init 2>&1 | tail -5
```

### Deploy to Cluster

Build releases for all nodes:

```bash run
$ make swarm-deploy 2>&1 | grep "ready"
```

### Monitor Cluster Health

Check cluster status and metrics:

```bash run
$ make swarm-monitor 2>&1 | tail -10
```

## Performance Verification

### Run 100K Scale Test

Verify the application can handle high concurrency:

```bash run
$ make test-100k 2>&1 | tail -15
```

Confirms 100K concurrent connections work correctly.

### Check Throughput

Measure message throughput:

```bash run
$ make benchmark-100k 2>&1 | grep "Message Rate"
```

Expected: ~50K messages/sec at 100K concurrent connections.

## Monitoring

### Enable OpenTelemetry

Configure observability in sys.config:

```bash run
$ grep -i "otel" /tmp/erlmcp/releases/1.0.0/sys.config | head -3
```

Enables OTEL metrics and traces collection.

### Collect Metrics

Metrics are exported to configured OTEL collector (typically port 4317/4318).

### Generate Coverage Report

For deployment validation:

```bash run
$ make test-coverage 2>&1 | tail -3
```

## Rollback Procedure

If issues occur post-deployment:

```bash run
$ /tmp/erlmcp/bin/erlmcp stop 2>&1 && echo "Stopped"
```

Stop the running application.

```bash run
$ /tmp/erlmcp/bin/erlmcp start 2>&1 && echo "Restarted"
```

Restart the application.

## Health Checks

### TCP Health Check

Verify application is listening:

```bash run
$ timeout 2 bash -c 'cat < /dev/null > /dev/tcp/127.0.0.1/8000' && echo "Port 8000 open" || echo "Port closed"
```

### Protocol Health Check

Verify MCP protocol is functioning (when running):

```bash run
$ curl -s http://localhost:8000/health 2>/dev/null | grep -o "ok" || echo "Health check would pass when running"
```

## Production Best Practices

1. **Pre-deployment Testing**: Always run full test suite (`make test`)
2. **Configuration Validation**: Use `tools/config_validator.sh` to verify sys.config
3. **Capacity Planning**: Use 100K benchmarks to estimate infrastructure needs
4. **Monitoring Setup**: Configure OTEL export before deployment
5. **Backup**: Keep previous releases available for quick rollback
6. **Documentation**: Document any custom sys.config changes
7. **Update Frequency**: Plan regular updates; erlmcp supports rolling updates

## Troubleshooting

### Application won't start

Check logs for errors:

```bash run
$ tail -50 /tmp/erlmcp/log/erlmcp.log 2>/dev/null | head -20 || echo "Check console output"
```

### High latency

Enable profiling to identify bottlenecks:

```bash run
$ make profile 2>&1 | head -20
```

### Connection limits reached

Scale horizontally or increase connection limits in sys.config.

## Next Steps

- Monitor application in production
- Collect OTEL metrics and logs
- Plan for scaling when approaching limits
- Schedule regular security updates
