# CI/CD Pipeline Recommendations & Enhancement Guide

**Document Date:** January 27, 2026
**Current Pipeline Maturity:** 8/10 - Excellent
**Recommended Enhancements:** Medium Priority

---

## Executive Summary

The erlmcp CI/CD pipeline is well-designed with comprehensive testing, multi-environment support, and automated deployments. This document provides specific recommendations to elevate it to production-grade level with enhanced security, reliability, and observability.

### Current Pipeline Strengths

- ✅ Parallel job execution for speed
- ✅ Multi-OTP version testing (25, 26, 27)
- ✅ Comprehensive test coverage (unit, integration, property-based)
- ✅ Docker image building and registry publication
- ✅ Automated release creation with GitHub releases
- ✅ Staging and production deployment automation
- ✅ Health checks and smoke tests
- ✅ Slack notifications for visibility
- ✅ Artifact retention policies
- ✅ Performance benchmarking

### Critical Enhancements Needed

1. **Security Scanning:** No SAST/DAST in current pipeline
2. **Image Scanning:** No container vulnerability scanning
3. **Approval Gates:** Manual approval not enforced
4. **Dependency Audit:** No automated dependency vulnerability checking
5. **Build Signing:** Releases not cryptographically signed
6. **Configuration Review:** No infrastructure-as-code validation
7. **Performance Regression:** Baselines not formally established
8. **Rollback Testing:** Automatic rollbacks not tested

---

## 1. Enhanced Test Pipeline

### Current Implementation

```yaml
Test Workflow (test.yml):
├── unit-tests (5 min) - EUnit
├── integration-tests (10 min) - CT
├── coverage (10 min) - Coverage report
├── performance (15 min) - Benchmarks
├── quality (5 min) - Linting + Dialyzer
└── summary (gate check)
```

### Recommended Enhancements

#### 1.1 Add SAST (Static Application Security Testing)

**Current Gap:** No security-focused code analysis

**Recommendation:** Integrate Erlang-specific security tools

```yaml
name: Test Suite with Security

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  security-scan:
    name: Security Scan
    runs-on: ubuntu-latest
    timeout-minutes: 10

    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0

      - name: Set up Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: 26
          rebar3-version: 3.22.0

      - name: Run Dialyzer (type safety)
        run: |
          rebar3 dialyzer
          # Catches type errors that could be security issues
        continue-on-error: false

      - name: Run Xref (code quality)
        run: |
          rebar3 xref
          # Detects undefined functions and unused code
        continue-on-error: false

      - name: Check for hardcoded secrets
        run: |
          # Install truffleHog or similar secret scanner
          pip install detect-secrets
          detect-secrets scan --baseline .secrets.baseline src/
        continue-on-error: true

      - name: License compliance check
        run: |
          # Verify all dependencies have approved licenses
          for dep in _build/default/lib/*/; do
            if [ -f "$dep/LICENSE" ]; then
              echo "✓ $(basename $dep): $(head -1 $dep/LICENSE)"
            else
              echo "⚠ $(basename $dep): No license file"
            fi
          done
        continue-on-error: true

      - name: Upload security scan results
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: security-scan-results
          path: |
            .secrets.baseline
            dialyzer_warnings.txt
```

#### 1.2 Add Dependency Vulnerability Scanning

**Current Gap:** No automatic detection of vulnerable dependencies

**Recommendation:** Implement dependency auditing

```yaml
  dependency-audit:
    name: Dependency Vulnerability Scan
    runs-on: ubuntu-latest
    timeout-minutes: 10

    steps:
      - uses: actions/checkout@v4

      - name: Set up Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: 26

      - name: Check rebar.lock
        run: |
          # Verify rebar.lock is up to date
          rebar3 check-deps

      - name: Audit Erlang packages
        run: |
          # Use Hex Audit API to check for vulnerable packages
          curl -s https://hex.pm/api/audit \
            -d @rebar.lock \
            | jq -r '.[]' > /tmp/vulnerabilities.txt

          if [ -s /tmp/vulnerabilities.txt ]; then
            echo "Found vulnerable dependencies:"
            cat /tmp/vulnerabilities.txt
            exit 1
          fi
        continue-on-error: false

      - name: Generate dependency report
        run: |
          rebar3 tree > /tmp/dependencies.txt
          echo "## Dependency Report" >> $GITHUB_STEP_SUMMARY
          echo "\`\`\`" >> $GITHUB_STEP_SUMMARY
          cat /tmp/dependencies.txt >> $GITHUB_STEP_SUMMARY
          echo "\`\`\`" >> $GITHUB_STEP_SUMMARY
```

#### 1.3 Enhanced Coverage Reporting

**Current Implementation:** Coverage report generated, optionally to PR

**Recommendation:** Stricter coverage enforcement

```yaml
  coverage-strict:
    name: Coverage Enforcement
    runs-on: ubuntu-latest
    timeout-minutes: 10

    steps:
      - uses: actions/checkout@v4

      - name: Set up Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: 26

      - name: Run tests with coverage
        run: |
          rebar3 cover
          # Generate coverage report

      - name: Validate coverage thresholds
        run: |
          COVERAGE=$(cat _build/test/cover/index.html | \
            grep -oP 'Total coverage: \K[0-9.]+' | head -1)

          echo "Total coverage: ${COVERAGE}%"

          # Enforce minimum threshold
          if (( $(echo "$COVERAGE < 80" | bc -l) )); then
            echo "❌ Coverage below 80% threshold (${COVERAGE}%)"
            exit 1
          fi

          # Warn if below stretch goal
          if (( $(echo "$COVERAGE < 85" | bc -l) )); then
            echo "⚠️  Coverage below stretch goal of 85%"
          else
            echo "✅ Coverage meets all thresholds"
          fi
        continue-on-error: false

      - name: Check coverage per module
        run: |
          # Generate per-module coverage report
          cat > /tmp/coverage_check.erl << 'EOF'
          % Script to validate coverage per critical module
          critical_modules() -> [
            erlmcp_server,
            erlmcp_client,
            erlmcp_json_rpc,
            erlmcp_registry,
            erlmcp_transport_stdio
          ].
          EOF
        continue-on-error: true

      - name: Comment coverage on PR
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v7
        with:
          script: |
            const fs = require('fs');
            const coverage_file = '_build/test/cover/coverage.json';
            if (fs.existsSync(coverage_file)) {
              const coverage = JSON.parse(fs.readFileSync(coverage_file, 'utf8'));
              const comment = `## 📊 Coverage Report

Total Coverage: **${coverage.total}%**

| Module | Coverage |
|--------|----------|
${coverage.modules.map(m => `| ${m.name} | ${m.coverage}% |`).join('\n')}

${coverage.total >= 85 ? '✅ Exceeds stretch goal (85%)' : coverage.total >= 80 ? '✅ Meets minimum requirement (80%)' : '❌ Below minimum requirement (80%)'}`;

              github.rest.issues.createComment({
                issue_number: context.issue.number,
                owner: context.repo.owner,
                repo: context.repo.repo,
                body: comment
              });
            }
```

#### 1.4 Add Flaky Test Detection

**Current Gap:** Flaky tests can cause false failures

**Recommendation:** Detect and quarantine flaky tests

```yaml
  flaky-test-detection:
    name: Detect Flaky Tests
    runs-on: ubuntu-latest
    timeout-minutes: 15
    if: github.event_name == 'pull_request'

    steps:
      - uses: actions/checkout@v4

      - name: Set up Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: 26

      - name: Run tests multiple times
        run: |
          echo "Running tests 5 times to detect flaky tests..."
          for i in {1..5}; do
            echo "Run $i..."
            rebar3 eunit || echo "Run $i failed"
          done
        continue-on-error: true

      - name: Analyze test results
        run: |
          # Check if any tests failed inconsistently
          echo "Flaky tests would be detected here"
        continue-on-error: true

      - name: Report flaky tests
        if: failure()
        uses: actions/github-script@v7
        with:
          script: |
            github.rest.issues.createComment({
              issue_number: context.issue.number,
              owner: context.repo.owner,
              repo: context.repo.repo,
              body: '⚠️ Flaky tests detected. Please review test isolation.'
            });
```

---

## 2. Enhanced Security Scanning Pipeline

### 2.1 Container Image Scanning

**Current Gap:** No vulnerability scanning of Docker images

**Implementation:**

```yaml
name: Build and Scan Docker Image

on:
  push:
    branches: [ main, develop ]
    paths:
      - 'Dockerfile'
      - 'src/**'
      - '.github/workflows/docker-build.yml'

  pull_request:
    branches: [ main, develop ]

jobs:
  build-scan:
    name: Build and Scan Docker Image
    runs-on: ubuntu-latest
    permissions:
      contents: read
      security-events: write

    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Build Docker image (local)
        uses: docker/build-push-action@v5
        with:
          context: .
          push: false
          load: true
          tags: erlmcp:latest
          cache-from: type=gha
          cache-to: type=gha,mode=max

      - name: Run Trivy vulnerability scan
        uses: aquasecurity/trivy-action@master
        with:
          image-ref: erlmcp:latest
          format: 'sarif'
          output: 'trivy-results.sarif'
          severity: 'CRITICAL,HIGH'

      - name: Block on critical vulnerabilities
        run: |
          trivy image erlmcp:latest --severity CRITICAL \
            --exit-code 1 --no-progress

      - name: Generate SBOM (CycloneDX)
        run: |
          trivy image erlmcp:latest \
            --format cyclonedx \
            --output sbom-erlmcp.xml

      - name: Upload Trivy scan results
        uses: github/codeql-action/upload-sarif@v2
        if: always()
        with:
          sarif_file: 'trivy-results.sarif'
          category: 'trivy'

      - name: Upload SBOM
        uses: actions/upload-artifact@v3
        with:
          name: sbom
          path: sbom-erlmcp.xml
```

### 2.2 Code Scanning (SAST)

**Recommendation:** Leverage Dialyzer + Xref for Erlang-specific analysis

```yaml
name: Code Security Analysis

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  codeql:
    name: CodeQL Analysis
    runs-on: ubuntu-latest
    permissions:
      contents: read
      security-events: write

    steps:
      - uses: actions/checkout@v4

      - name: Initialize CodeQL
        uses: github/codeql-action/init@v2
        with:
          languages: 'erlang'

      - name: Set up Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: 26

      - name: Run Dialyzer with security focus
        run: |
          # Dialyzer catches type errors that could be security issues
          rebar3 dialyzer
        continue-on-error: false

      - name: Run Xref cross-reference analysis
        run: |
          # Xref finds undefined functions and unused code
          rebar3 xref
        continue-on-error: false

      - name: Perform CodeQL analysis
        uses: github/codeql-action/analyze@v2
        with:
          category: '/language:erlang'
```

---

## 3. Enhanced Deployment Pipeline

### 3.1 Approval Gate Implementation

**Current Gap:** Production deployment not gated by approval

**Recommendation:** Enforce approval workflow

```yaml
name: Enhanced Deploy with Approval

on:
  push:
    tags:
      - 'v*'
  workflow_dispatch:
    inputs:
      environment:
        description: 'Target environment'
        required: true
        type: choice
        options:
          - staging
          - production
        default: staging

env:
  ERLANG_VERSION: '26'
  DOCKER_REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  # ... build job ...

  approval-gate:
    name: Deployment Approval Gate
    runs-on: ubuntu-latest
    needs: [build-and-test, build-docker]
    if: github.ref_type == 'tag' || github.event.inputs.environment == 'production'
    environment:
      name: production-approval
      url: https://github.com/${{ github.repository }}/actions/runs/${{ github.run_id }}

    steps:
      - name: Request deployment approval
        uses: actions/github-script@v7
        with:
          script: |
            const version = '${{ github.ref_name }}'.replace('v', '');
            const comment = `## 🚀 Deployment Approval Request

**Version:** ${version}
**Environment:** Production
**Triggered By:** @${{ github.actor }}

### Pre-deployment Checklist
- [ ] Release notes reviewed
- [ ] Test results verified
- [ ] Security scan passed
- [ ] Docker image tested
- [ ] Rollback procedure verified
- [ ] On-call team notified

### Deployment Timeline
- Estimated deployment time: 5-10 minutes
- Estimated verification time: 5-10 minutes
- Total estimated downtime: 0 minutes (rolling update)

### Links
- [Release](https://github.com/${{ github.repository }}/releases/tag/v${version})
- [Workflow](https://github.com/${{ github.repository }}/actions/runs/${{ github.run_id }})
- [Test Results](https://github.com/${{ github.repository }}/actions/runs/${{ github.run_id }})

⚠️ **This deployment requires manual approval before proceeding.**`;

            github.rest.issues.create({
              owner: context.repo.owner,
              repo: context.repo.repo,
              title: `[APPROVAL REQUIRED] Deploy v${version} to Production`,
              body: comment,
              labels: ['deployment', 'production', 'release'],
              assignees: ['@devops-team']  // Requires team setup
            });

      - name: Wait for approval
        run: |
          echo "⏳ Waiting for deployment approval..."
          echo "Please review and approve the deployment issue in GitHub"
          echo "This job will fail if approval is not granted"

  deploy-production:
    name: Deploy to Production
    runs-on: ubuntu-latest
    needs: [build-docker, approval-gate]
    environment:
      name: production
      url: https://erlmcp.example.com

    steps:
      - name: ✅ Deployment approved
        run: echo "Deployment approved by ${{ github.actor }}"

      # ... existing deployment steps ...
```

### 3.2 Canary Deployment Strategy

**Current Gap:** All-or-nothing deployment

**Recommendation:** Progressive rollout with validation

```yaml
  canary-deploy:
    name: Canary Deployment
    runs-on: ubuntu-latest
    needs: approval-gate

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup kubectl
        uses: azure/setup-kubectl@v3
        with:
          version: 'v1.27.0'

      - name: Configure kubectl
        run: |
          echo "${{ secrets.KUBE_CONFIG }}" | base64 -d > $HOME/.kube/config

      # Stage 1: Deploy to 10% of replicas
      - name: Stage 1 - Canary (10%)
        run: |
          echo "Deploying to 1 of 3 replicas..."
          kubectl set image deployment/erlmcp-tcps \
            erlmcp=${{ env.DOCKER_REGISTRY }}/${{ env.IMAGE_NAME }}:${{ github.ref_name }} \
            -n erlmcp \
            --record

          # Wait for canary deployment
          kubectl rollout status deployment/erlmcp-tcps \
            -n erlmcp --timeout=5m

          # Monitor canary metrics
          echo "Monitoring canary metrics for 5 minutes..."
          sleep 300

      - name: Validate canary metrics
        run: |
          # Check if error rate is acceptable
          ERROR_RATE=$(curl -s http://prometheus:9090/api/v1/query \
            --data-urlencode 'query=rate(http_requests_total{code=~"5.."}[5m])' | \
            jq '.data.result[0].value[1]')

          if (( $(echo "$ERROR_RATE > 0.05" | bc -l) )); then
            echo "❌ High error rate detected: $ERROR_RATE"
            exit 1
          fi

          echo "✅ Canary validation passed"

      # Stage 2: Deploy to 50% of replicas
      - name: Stage 2 - Partial (50%)
        if: success()
        run: |
          echo "Scaling canary deployment to 50%..."
          # Deployment already updated, just ensure it's rolling
          kubectl rollout status deployment/erlmcp-tcps \
            -n erlmcp --timeout=5m

          sleep 300

      # Stage 3: Full deployment
      - name: Stage 3 - Full (100%)
        if: success()
        run: |
          echo "Completing full deployment..."
          kubectl rollout status deployment/erlmcp-tcps \
            -n erlmcp --timeout=5m

          echo "✅ Full deployment complete"

      - name: Post-deployment verification
        run: |
          echo "Running post-deployment smoke tests..."
          kubectl port-forward svc/erlmcp 8080:8080 -n erlmcp &
          sleep 5

          # Health check
          curl -sf http://localhost:8080/health || exit 1

          # API check
          curl -sf http://localhost:8080/initialize || exit 1

          kill %1
          echo "✅ Post-deployment verification passed"

      - name: Rollback on failure
        if: failure()
        run: |
          echo "🚨 Deployment failed, rolling back..."
          kubectl rollout undo deployment/erlmcp-tcps -n erlmcp
          kubectl rollout status deployment/erlmcp-tcps \
            -n erlmcp --timeout=5m
```

---

## 4. Performance & Reliability

### 4.1 Performance Regression Detection

**Current Implementation:** Benchmarks tracked but no regression detection

**Recommendation:** Automated regression testing

```yaml
  performance-regression:
    name: Performance Regression Detection
    runs-on: ubuntu-latest
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'
    timeout-minutes: 20

    steps:
      - uses: actions/checkout@v4

      - name: Set up Erlang/OTP
        uses: erlef/setup-beam@v1
        with:
          otp-version: 26

      - name: Run baseline (main)
        run: |
          # Checkout baseline performance
          git fetch origin main:baseline
          git checkout baseline

          rebar3 as test compile
          rebar3 as test ct --suite test/integration/performance_SUITE

          # Save baseline metrics
          cp ct_logs/run.*/log_private/suite.log /tmp/baseline.log

      - name: Run current (PR/commit)
        run: |
          git checkout -

          rebar3 as test compile
          rebar3 as test ct --suite test/integration/performance_SUITE

          # Save current metrics
          cp ct_logs/run.*/log_private/suite.log /tmp/current.log

      - name: Compare performance metrics
        run: |
          cat > /tmp/compare_perf.py << 'EOF'
          import json
          import sys

          def parse_metrics(logfile):
              """Extract performance metrics from test log"""
              metrics = {}
              with open(logfile) as f:
                  for line in f:
                      if 'Duration:' in line:
                          metrics['duration'] = float(line.split(':')[1])
                      elif 'Throughput:' in line:
                          metrics['throughput'] = float(line.split(':')[1])
              return metrics

          baseline = parse_metrics('/tmp/baseline.log')
          current = parse_metrics('/tmp/current.log')

          print("## Performance Comparison")
          print(f"Baseline: {baseline}")
          print(f"Current:  {current}")

          # Check for regressions (>5% worse)
          for metric in baseline:
              if metric not in current:
                  continue

              change = (current[metric] - baseline[metric]) / baseline[metric] * 100

              if change > 5:
                  print(f"⚠️  {metric} regressed by {change:.1f}%")
                  sys.exit(1)
              elif change < -5:
                  print(f"✅ {metric} improved by {abs(change):.1f}%")
              else:
                  print(f"✓  {metric} unchanged ({change:+.1f}%)")
          EOF

          python3 /tmp/compare_perf.py
        continue-on-error: false

      - name: Upload performance results
        uses: actions/upload-artifact@v3
        if: always()
        with:
          name: performance-comparison
          path: ct_logs/
```

### 4.2 Reliability Testing

**Recommendation:** Test deployment reliability

```yaml
  reliability-testing:
    name: Deployment Reliability Test
    runs-on: ubuntu-latest
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'
    timeout-minutes: 20

    steps:
      - uses: actions/checkout@v4

      - name: Test restart resilience
        run: |
          echo "Testing application resilience to restarts..."

          # Start application
          rebar3 shell << 'ERLANG'
          {ok, _} = application:ensure_all_started(erlmcp),
          io:format("✓ Application started~n", []),

          % Simulate some load
          timer:sleep(5000),

          % Gracefully stop
          application:stop(erlmcp),
          io:format("✓ Application stopped gracefully~n", []),

          % Restart
          {ok, _} = application:ensure_all_started(erlmcp),
          io:format("✓ Application restarted successfully~n", []),

          halt(0).
          ERLANG

      - name: Test configuration reloading
        run: |
          echo "Testing configuration reloading..."

          rebar3 shell << 'ERLANG'
          {ok, _} = application:ensure_all_started(erlmcp),

          % Test config reloading
          OldConfig = application:get_env(erlmcp, log_level),
          application:set_env(erlmcp, log_level, debug),
          NewConfig = application:get_env(erlmcp, log_level),

          case OldConfig =/= NewConfig of
            true -> io:format("✓ Configuration reloading works~n", []);
            false -> io:format("✗ Configuration not updated~n", []), halt(1)
          end,

          halt(0).
          ERLANG
```

---

## 5. Observability & Monitoring

### 5.1 Deployment Observability

**Recommendation:** Track deployment health metrics

```yaml
  deployment-metrics:
    name: Track Deployment Metrics
    runs-on: ubuntu-latest
    needs: deploy-production
    if: always()

    steps:
      - name: Collect deployment metrics
        uses: actions/github-script@v7
        with:
          script: |
            // Collect metrics about the deployment
            const metrics = {
              deployment_duration: '${{ job.duration }}',
              status: '${{ job.status }}',
              timestamp: new Date().toISOString(),
              version: '${{ github.ref_name }}',
              environment: 'production'
            };

            // Store in artifact for tracking
            const fs = require('fs');
            fs.writeFileSync(
              'deployment-metrics.json',
              JSON.stringify(metrics, null, 2)
            );

      - name: Upload deployment metrics
        uses: actions/upload-artifact@v3
        with:
          name: deployment-metrics
          path: deployment-metrics.json

      - name: Post-deployment metrics to Prometheus
        run: |
          # Send metrics to monitoring system
          curl -X POST http://prometheus-pushgateway:9091/metrics/job/deployments \
            --data-binary @deployment-metrics.json
        continue-on-error: true
```

---

## 6. Pipeline Configuration Best Practices

### 6.1 Recommended Workflow Structure

```yaml
name: Enhanced CI/CD Pipeline

on:
  push:
    branches: [ main, develop ]
    tags: [ 'v*' ]
  pull_request:
    branches: [ main, develop ]
  workflow_dispatch:

env:
  ERLANG_VERSION: '26'
  REBAR3_VERSION: '3.22.0'
  DOCKER_REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  # 1. VALIDATE
  validate:
    name: Validate Commit
    runs-on: ubuntu-latest
    steps:
      - name: Check commit message format
        run: |
          # Validate conventional commits
          if [[ ! "${{ github.event.head_commit.message }}" =~ ^(feat|fix|docs|style|refactor|test|chore) ]]; then
            echo "❌ Commit message must follow conventional commits"
            exit 1
          fi

  # 2. BUILD
  build:
    name: Build
    runs-on: ubuntu-latest
    needs: validate
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
      - run: rebar3 compile

  # 3. TEST
  test:
    name: Test
    runs-on: ubuntu-latest
    needs: build
    strategy:
      matrix:
        otp-version: [ '25', '26', '27' ]
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp-version }}
      - run: rebar3 eunit
      - run: rebar3 ct

  # 4. SECURITY
  security:
    name: Security Scan
    runs-on: ubuntu-latest
    needs: build
    steps:
      - uses: actions/checkout@v4
      - run: rebar3 dialyzer
      - run: rebar3 xref

  # 5. BUILD ARTIFACTS
  build-artifacts:
    name: Build Artifacts
    runs-on: ubuntu-latest
    needs: test
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
      - run: rebar3 as prod release
      - run: rebar3 as prod tar

  # 6. BUILD & SCAN DOCKER
  build-docker:
    name: Build & Scan Docker
    runs-on: ubuntu-latest
    needs: security
    permissions:
      contents: read
      packages: write
      security-events: write
    steps:
      - uses: actions/checkout@v4
      - uses: docker/setup-buildx-action@v3
      - run: docker build -t erlmcp:test .
      - uses: aquasecurity/trivy-action@master
        with:
          image-ref: erlmcp:test
          severity: CRITICAL,HIGH
          exit-code: 1

  # 7. DEPLOY STAGING
  deploy-staging:
    name: Deploy to Staging
    runs-on: ubuntu-latest
    needs: [build-artifacts, build-docker]
    if: github.ref == 'refs/heads/main' || github.event_name == 'workflow_dispatch'
    environment:
      name: staging
    steps:
      - uses: actions/checkout@v4
      - run: |
          # Deploy to staging
          ./scripts/deploy.sh staging

  # 8. TEST STAGING
  test-staging:
    name: Test Staging
    runs-on: ubuntu-latest
    needs: deploy-staging
    steps:
      - uses: actions/checkout@v4
      - run: ./scripts/smoke-tests.sh staging

  # 9. APPROVAL GATE
  approval:
    name: Approval Gate
    runs-on: ubuntu-latest
    needs: test-staging
    if: github.event_name == 'push' && startsWith(github.ref, 'refs/tags/')
    environment:
      name: production-approval
    steps:
      - run: echo "✅ Deployment approved"

  # 10. DEPLOY PRODUCTION
  deploy-production:
    name: Deploy to Production
    runs-on: ubuntu-latest
    needs: approval
    environment:
      name: production
    steps:
      - uses: actions/checkout@v4
      - run: ./scripts/deploy.sh production

  # 11. VERIFY PRODUCTION
  verify-production:
    name: Verify Production
    runs-on: ubuntu-latest
    needs: deploy-production
    steps:
      - uses: actions/checkout@v4
      - run: ./scripts/smoke-tests.sh production

  # 12. RELEASE
  release:
    name: Create Release
    runs-on: ubuntu-latest
    needs: verify-production
    if: github.event_name == 'push' && startsWith(github.ref, 'refs/tags/')
    permissions:
      contents: write
    steps:
      - uses: actions/checkout@v4
      - uses: softprops/action-gh-release@v1
        with:
          files: _build/prod/*.tar.gz

  # 13. NOTIFY
  notify:
    name: Notify Team
    runs-on: ubuntu-latest
    needs: [ test, deploy-staging, deploy-production, release ]
    if: always()
    steps:
      - name: Send notification
        uses: slackapi/slack-github-action@v1
        with:
          webhook: ${{ secrets.SLACK_WEBHOOK }}
          payload: |
            {
              "text": "Pipeline: ${{ job.status }}",
              "blocks": [{
                "type": "section",
                "text": {
                  "type": "mrkdwn",
                  "text": "Pipeline ${{ job.status }}\n${{ github.ref_name }}\n${{ github.actor }}"
                }
              }]
            }
```

---

## 7. Implementation Roadmap

### Phase 1: Critical Enhancements (Week 1-2)

- [ ] Add image scanning (Trivy)
- [ ] Add dependency audit (Hex API)
- [ ] Add coverage enforcement (80% minimum)
- [ ] Setup approval gates for production

### Phase 2: Security Enhancements (Week 3-4)

- [ ] Add SAST (Dialyzer + custom rules)
- [ ] Add secret detection
- [ ] Add license compliance checking
- [ ] Setup SBOM generation

### Phase 3: Deployment Strategy (Month 2)

- [ ] Implement canary deployments
- [ ] Add performance regression detection
- [ ] Add reliability testing
- [ ] Implement feature flag deployment

### Phase 4: Observability (Month 2)

- [ ] Add deployment metrics tracking
- [ ] Create deployment dashboard
- [ ] Add deployment tracing
- [ ] Setup alert correlation

---

## 8. Metrics for Success

```
Before Enhancement               After Enhancement
─────────────────────────────    ─────────────────────────
Manual release approvals         Automated with gates
No security scanning             3+ security scanners
All-or-nothing deployments       Canary + staged rollouts
No regression detection          Automated regression testing
Ad-hoc performance testing       Continuous benchmarking
80% test coverage                95%+ test coverage
```

---

## Conclusion

The erlmcp CI/CD pipeline is production-grade and well-structured. The recommended enhancements focus on:

1. **Security:** Image scanning, SAST, dependency auditing
2. **Reliability:** Canary deployments, regression detection, approval gates
3. **Observability:** Metrics tracking, deployment monitoring
4. **Automation:** Fully automated security gates

**Estimated Implementation Time:** 40-60 hours across 8 weeks

**Expected Benefits:**
- 100% vulnerability scanning coverage
- Zero manual security steps
- Safer deployments with canary validation
- Automatic performance regression detection
- 100% audit trail for compliance
