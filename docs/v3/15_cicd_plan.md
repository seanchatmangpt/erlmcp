# erlmcp v3 OSS CI/CD Plan

**Version:** 3.0.0
**Status:** Draft
**Last Updated:** 2025-01-31
**Author:** GitHub Ops Agent

---

## Executive Summary

This document defines the CI/CD strategy for erlmcp v3.0 open-source release, separating **OSS (community)** workflows from **commercial (enterprise)** workflows while maintaining quality gates and release automation.

**Key Principles:**
1. **OSS-First**: Public GitHub Actions, transparent quality gates
2. **Minimal Build**: Fast feedback for contributors (5-10 min)
3. **Full Build**: Comprehensive validation before merge (30-45 min)
4. **Release Automation**: Automated releases with manual approval gate
5. **Separate Workflows**: OSS vs Commercial features in distinct pipelines

---

## Architecture Overview

### Workflow Taxonomy (30 Workflows → 2 Categories)

```
┌─────────────────────────────────────────────────────────────┐
│                    erlmcp v3 CI/CD Architecture              │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  ┌──────────────────────┐      ┌──────────────────────┐    │
│  │   OSS Workflows      │      │ Commercial Workflows  │    │
│  │   (Public, GitHub)   │      │ (Private, Internal)   │    │
│  ├──────────────────────┤      ├──────────────────────┤    │
│  │ • Minimal CI         │      │ • Full Validation     │    │
│  │ • PR Checks          │      │ • Enterprise Features │    │
│  │ • Release Automation │      │ • Deploy Automation   │    │
│  │ • Quality Gates      │      │ • Commercial Testing  │    │
│  └──────────────────────┘      └──────────────────────┘    │
│                                                               │
│  Shared Infrastructure (Both Categories):                   │
│  • Docker images (ghcr.io)                                  │
│  • Hex.pm packages (OSS only)                               │
│  • MCP compliance validators                                │
│  • Benchmark baselines                                      │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

### Current vs Target State

| Metric | Current (v2.1) | Target (v3.0 OSS) |
|--------|----------------|-------------------|
| **Total Workflows** | 30 | 15 (OSS) + 15 (Commercial) |
| **PR Check Time** | 30-45 min | 5-10 min (minimal) |
| **Full Validation** | 30-45 min | 30-45 min (on merge to main) |
| **Release Trigger** | Manual tag | Automated on tag |
| **Hex.pm Publishing** | Manual | Automated (OSS only) |
| **Docker Builds** | Manual PR | Automated (multi-arch) |
| **Commercial Features** | Mixed in | Separate workflows |

---

## Part I: OSS CI/CD Workflows

### 1. Minimal Build (Fast Feedback for PRs)

**File:** `.github/workflows/oss-minimal-ci.yml`

**Purpose:** Provide rapid feedback on PRs (<10 min) to encourage contributions.

**Triggers:**
- Pull requests to `main`, `feature/**`, `bugfix/**`, `epic/**`
- Push to `feature/**`, `bugfix/**` (for contributor feedback)

**Jobs:** 4 parallel jobs, ~8-10 min total

```yaml
jobs:
  compile-otp-28:
    name: Compile (OTP 28.3.1+)
    runs-on: ubuntu-22.04
    timeout-minutes: 10
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'  # OSS target: OTP 28+
          rebar3-version: '3.25'
      - name: Compile minimal
        run: |
          TERM=dumb rebar3 compile
          # Verify umbrella apps
          ls -lh _build/default/lib/erlmcp_*/ebin

  unit-tests-quick:
    name: Unit Tests (Quick)
    runs-on: ubuntu-22.04
    timeout-minutes: 15
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run EUnit (no cover)
        run: |
          rebar3 as test do compile, eunit

  lint-check:
    name: Lint (rebar3_lint)
    runs-on: ubuntu-22.04
    timeout-minutes: 10
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run linter
        run: |
          rebar3 lint

  format-check:
    name: Format Check
    runs-on: ubuntu-22.04
    timeout-minutes: 5
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Verify formatting
        run: |
          rebar3 format --verify
```

**Quality Gates (Minimal):**
- ✅ Compilation: Zero errors (BLOCKING)
- ✅ Unit tests: 90% pass rate (BLOCKING)
- ✅ Lint: Zero critical issues (BLOCKING)
- ✅ Format: 100% compliant (BLOCKING)

**Artifacts:**
- None (fast feedback, no persistence)

---

### 2. Full Build (Comprehensive Validation)

**File:** `.github/workflows/oss-full-ci.yml`

**Purpose:** Complete validation before merge to `main` (30-45 min).

**Triggers:**
- Push to `main`
- Push to `release/**`
- Manual dispatch
- Dependabot PRs (after minimal passes)

**Jobs:** 7 parallel jobs, ~35 min total

```yaml
jobs:
  compile-matrix:
    name: Compile (OTP ${{ matrix.otp }})
    strategy:
      matrix:
        otp: ['28']  # OSS: OTP 28+ only
    runs-on: ubuntu-22.04
    timeout-minutes: 15
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
      - name: Compile full
        run: |
          TERM=dumb rebar3 compile
          # Verify all apps
          for app in erlmcp_core erlmcp_transports erlmcp_observability erlmcp_validation; do
            [ -d "_build/default/lib/$app/ebin" ] || exit 1
          done

  xref-check:
    name: Xref (Cross-Reference)
    runs-on: ubuntu-22.04
    timeout-minutes: 10
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run xref
        run: |
          rebar3 xref
          # Zero undefined functions (blocking)

  dialyzer-check:
    name: Dialyzer (Type Checking)
    runs-on: ubuntu-22.04
    timeout-minutes: 20
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run dialyzer
        run: |
          rebar3 dialyzer
          # Zero type errors (blocking)

  test-suite:
    name: Test Suite (OTP 28)
    runs-on: ubuntu-22.04
    timeout-minutes: 25
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run EUnit with cover
        run: |
          rebar3 as test do compile, eunit --cover
      - name: Run Common Test
        run: |
          rebar3 ct --dir=test/integration
      - name: Upload coverage
        uses: actions/upload-artifact@v3
        with:
          name: coverage-report
          path: _build/test/cover/
          retention-days: 30

  coverage-check:
    name: Coverage (≥80%)
    runs-on: ubuntu-22.04
    needs: test-suite
    timeout-minutes: 5
    steps:
      - uses: actions/checkout@v4
      - uses: actions/download-artifact@v3
        with:
          name: coverage-report
      - name: Verify coverage
        run: |
          if [ -f scripts/check_coverage_threshold.sh ]; then
            ./scripts/check_coverage_threshold.sh 80
          fi

  mcp-compliance:
    name: MCP Compliance (OSS)
    runs-on: ubuntu-22.04
    timeout-minutes: 20
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run MCP validators
        run: |
          rebar3 escriptize
          ./_build/default/bin/erlmcp_validate run --section protocol
          ./_build/default/bin/erlmcp_validate run --section transport
      - name: Generate compliance report
        run: |
          ./_build/default/bin/erlmcp_validate report --format markdown > mcp-compliance-report.md
      - name: Upload report
        uses: actions/upload-artifact@v3
        with:
          name: mcp-compliance-report
          path: mcp-compliance-report.md
          retention-days: 90

  benchmark-smoke:
    name: Benchmark (Smoke Test)
    runs-on: ubuntu-22.04
    timeout-minutes: 10
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run quick benchmark
        run: |
          rebar3 shell --eval "
            application:ensure_all_started(erlmcp_core),
            Result = erlmcp_bench_core_ops:run(<<\"core_ops_1k\">>),
            io:format(\"Benchmark: ~p~n\", [Result]),
            init:stop().
          " --name bench@localhost --setcookie bench
```

**Quality Gates (Full):**
- ✅ Compilation: Zero errors (BLOCKING)
- ✅ Xref: Zero undefined functions (BLOCKING)
- ✅ Dialyzer: Zero type errors (BLOCKING)
- ✅ Tests: 90% pass rate (BLOCKING)
- ✅ Coverage: ≥80% (BLOCKING)
- ✅ MCP Compliance: 95%+ (BLOCKING)
- ⚠️ Benchmark: No regression >10% (advisory)

**Artifacts:**
- Coverage reports (30 days)
- MCP compliance reports (90 days)
- Test logs (14 days)

---

### 3. Release Automation

**File:** `.github/workflows/oss-release.yml`

**Purpose:** Automated release process for OSS (Hex.pm + GitHub).

**Triggers:**
- Version tags: `v3.0.0`, `v3.1.0-rc.1`

**Jobs:** 5 sequential jobs with approval gate

```yaml
jobs:
  pre-release-checks:
    name: Pre-Release Validation
    runs-on: ubuntu-22.04
    outputs:
      version: ${{ steps.extract.outputs.version }}
      is-prerelease: ${{ steps.extract.outputs.is-prerelease }}
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - name: Extract version
        id: extract
        run: |
          VERSION=${GITHUB_REF#refs/tags/v}
          echo "version=$VERSION" >> $GITHUB_OUTPUT
          [[ $VERSION =~ -(alpha|beta|rc) ]] && echo "is-prerelease=true" || echo "is-prerelease=false"
      - name: Verify version consistency
        run: |
          VERSION=${{ steps.extract.outputs.version }}
          grep "{release, {erlmcp, \"$VERSION\"}" rebar.config
          for app in erlmcp_core erlmcp_transports erlmcp_observability erlmcp_validation; do
            grep "{vsn, \"$VERSION\"}" apps/$app/src/${app}.app.src
          done
      - name: Check CHANGELOG
        run: |
          grep "## \[${{ steps.extract.outputs.version }}\]" CHANGELOG.md

  build-release:
    name: Build Release Artifacts
    needs: pre-release-checks
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Build production release
        run: |
          rebar3 as prod compile
          rebar3 as prod release
      - name: Create tarball
        run: |
          VERSION=${{ needs.pre-release-checks.outputs.version }}
          tar -czf erlmcp-${VERSION}.tar.gz -C _build/prod/rel erlmcp
          sha256sum erlmcp-${VERSION}.tar.gz > erlmcp-${VERSION}.tar.gz.sha256
      - name: Upload tarball
        uses: actions/upload-artifact@v3
        with:
          name: release-tarball
          path: |
            erlmcp-${VERSION}.tar.gz
            erlmcp-${VERSION}.tar.gz.sha256
          retention-days: 90

  docker-build:
    name: Build Docker Image
    needs: pre-release-checks
    runs-on: ubuntu-22.04
    strategy:
      matrix:
        platform: [linux/amd64, linux/arm64]
    steps:
      - uses: actions/checkout@v4
      - uses: docker/setup-buildx-action@v2
      - uses: docker/login-action@v2
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}
      - uses: docker/build-push-action@v4
        with:
          context: .
          platforms: ${{ matrix.platform }}
          push: true
          tags: |
            ghcr.io/${{ github.repository }}:${{ needs.pre-release-checks.outputs.version }}
            ghcr.io/${{ github.repository }}:latest
          cache-from: type=registry,ref=ghcr.io/${{ github.repository }}:buildcache
          cache-to: type=registry,ref=ghcr.io/${{ github.repository }}:buildcache,mode=max
          build-args: |
            VERSION=${{ needs.pre-release-checks.outputs.version }}

  hex-publish:
    name: Publish to Hex.pm
    needs: [pre-release-checks, build-release]
    if: ${{ needs.pre-release-checks.outputs.is-prerelease == 'false' }}
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Publish umbrella apps
        env:
          HEX_API_KEY: ${{ secrets.HEX_API_KEY }}
        run: |
          for app in erlmcp_core erlmcp_transports erlmcp_observability erlmcp_validation; do
            cd apps/$app
            rebar3 hex publish --yes
            cd ../..
          done
          rebar3 hex publish --yes

  github-release:
    name: Create GitHub Release
    needs: [pre-release-checks, build-release, docker-build]
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/download-artifact@v3
      - name: Generate release notes
        id: notes
        run: |
          VERSION=${{ needs.pre-release-checks.outputs.version }}
          NOTES=$(sed -n "/^## \[$VERSION\]/,/^## \[/p" CHANGELOG.md | sed '$d')
          echo "content<<EOF" >> $GITHUB_OUTPUT
          echo "$NOTES" >> $GITHUB_OUTPUT
          echo "EOF" >> $GITHUB_OUTPUT
      - uses: softprops/action-gh-release@v1
        with:
          tag_name: v${{ needs.pre-release-checks.outputs.version }}
          name: erlmcp v${{ needs.pre-release-checks.outputs.version }}
          body: ${{ steps.notes.outputs.content }}
          draft: false
          prerelease: ${{ needs.pre-release-checks.outputs.is-prerelease }}
          files: |
            release-tarball/erlmcp-${{ needs.pre-release-checks.outputs.version }}.tar.gz
            release-tarball/erlmcp-${{ needs.pre-release-checks.outputs.version }}.tar.gz.sha256
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

**Release Artifacts:**
1. **Tarball:** `erlmcp-${VERSION}.tar.gz` (90-day retention)
2. **Docker:** `ghcr.io/${{ repo }}:${VERSION}` (multi-arch: amd64, arm64)
3. **Hex.pm:** 5 packages (erlmcp + 4 umbrella apps)
4. **GitHub Release:** Tagged release with notes from CHANGELOG.md

---

### 4. Quality Gate Definitions

**File:** `.github/workflows/oss-quality-gates.yml`

**Purpose:** Enforce OSS quality standards (separate from commercial).

**Triggers:**
- Pull requests (after minimal CI passes)
- Push to `main` (after full CI passes)

**Blocking Gates (OSS):**

| Gate | Requirement | Threshold | Blocking |
|------|-------------|-----------|----------|
| **Compilation** | Zero errors | 0 | ✅ YES |
| **Xref** | Undefined functions | 0 | ✅ YES |
| **Dialyzer** | Type errors | 0 | ✅ YES |
| **Unit Tests** | Pass rate | ≥90% | ✅ YES |
| **Coverage** | Code coverage | ≥80% | ✅ YES |
| **MCP Compliance** | Spec adherence | ≥95% | ✅ YES |
| **Format** | Code formatting | 100% | ✅ YES |
| **Lint** | Critical issues | 0 | ✅ YES |

**Non-Blocking Gates (Advisory):**

| Gate | Requirement | Threshold | Action |
|------|-------------|-----------|--------|
| **Integration Tests** | CT pass rate | 100% | Warning only |
| **Benchmark** | Performance regression | <10% | Comment on PR |
| **Documentation** | Doc coverage | ≥70% | Warning only |
| **Security** | Vulnerability scan | 0 high | Comment on PR |

**Quality Gate Summary Report:**

```markdown
## 🚦 OSS Quality Gate Report

### Blocking Gates (MUST PASS)
- ✅ Compilation: Zero errors
- ✅ Xref: Zero undefined functions
- ✅ Dialyzer: Zero type errors
- ✅ Unit Tests: 95% pass rate (≥90% required)
- ✅ Coverage: 82% (≥80% required)
- ✅ MCP Compliance: 96% (≥95% required)
- ✅ Format: 100% compliant
- ✅ Lint: Zero critical issues

### Non-Blocking Gates (ADVISORY)
- ⚠️ Integration Tests: 1 suite skipped (non-critical)
- ✅ Benchmark: No regression detected
- ⚠️ Documentation: 68% coverage (target: ≥70%)
- ✅ Security: Zero high-severity vulnerabilities

### Result: ✅ MERGE ALLOWED
```

---

## Part II: Commercial CI/CD Workflows

### 5. Commercial Feature Validation

**File:** `.github/workflows/commercial-validation.yml` (Private repository)

**Purpose:** Validate enterprise-only features (NOT in OSS release).

**Scope:**
- Multi-region deployment
- Enterprise authentication (OIDC, SAML)
- Advanced observability (custom OTEL exporters)
- Commercial transport adapters
- License enforcement
- SLA guarantees

**Jobs:**

```yaml
jobs:
  license-compliance:
    name: License Compliance Check
    runs-on: [self-hosted, commercial]
    steps:
      - name: Verify commercial license headers
        run: |
          # Check enterprise files have license headers
          grep -r "Copyright.*Commercial" apps/erlmcp_enterprise/

  enterprise-auth:
    name: Enterprise Authentication Tests
    runs-on: [self-hosted, commercial]
    steps:
      - name: Test OIDC integration
        run: |
          rebar3 ct --suite=test/enterprise_oidc_SUITE
      - name: Test SAML integration
        run: |
          rebar3 ct --suite=test/enterprise_saml_SUITE

  multi-region:
    name: Multi-Region Deployment Tests
    runs-on: [self-hosted, commercial]
    strategy:
      matrix:
        region: [us-east-1, us-west-2, eu-west-1]
    steps:
      - name: Deploy to ${{ matrix.region }}
        run: |
          terraform apply -var="region=${{ matrix.region }}"
      - name: Run cross-region tests
        run: |
          rebar3 ct --suite=test/multi_region_suite

  sla-validation:
    name: SLA Validation (99.9%)
    runs-on: [self-hosted, commercial]
    steps:
      - name: Run 24h soak test
        run: |
          rebar3 proper -c --duration=24h
      - name: Verify SLA metrics
        run: |
          # Check uptime, latency, error rate
          python tools/validate_sla.py --threshold=99.9
```

**Note:** Commercial workflows run in private infrastructure with proprietary dependencies.

---

### 6. Commercial Release Automation

**File:** `.github/workflows/commercial-release.yml` (Private repository)

**Purpose:** Release commercial version with enterprise features.

**Triggers:**
- Version tags: `v3.0.0-commercial`
- Manual approval after OSS release

**Artifacts:**
1. **Commercial Docker Image:** `ghcr.io/${{ repo }}:${VERSION}-enterprise`
2. **Enterprise Tarball:** Includes commercial modules
3. **License Keys:** Generated for customers
4. **SLA Reports:** 24h soak test results
5. **Deployment Guides:** Customer-facing documentation

---

## Part III: Version Tagging Strategy

### Semantic Versioning (SemVer 2.0.0)

**Format:** `vMAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]`

**Examples:**
- `v3.0.0` - Stable OSS release
- `v3.1.0-rc.1` - Release candidate
- `v3.0.0-commercial` - Commercial release
- `v3.1.0-beta.2+20250131` - Development build

### Version Bump Rules

| Change Type | Bump | Example |
|-------------|------|---------|
| **Breaking API change** | MAJOR | 3.0.0 → 4.0.0 |
| **New feature (backward compatible)** | MINOR | 3.0.0 → 3.1.0 |
| **Bug fix (backward compatible)** | PATCH | 3.0.0 → 3.0.1 |
| **Pre-release** | PRERELEASE | 3.1.0-rc.1 |
| **Commercial build** | SUFFIX | 3.0.0-commercial |

### Automated Version Extraction

**Script:** `scripts/release/extract_version.sh`

```bash
#!/bin/bash
# Extract version from git tag and validate

VERSION=${GITHUB_REF#refs/tags/v}

# Validate SemVer format
if [[ ! $VERSION =~ ^v?[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9.]+)?$ ]]; then
  echo "::error::Invalid SemVer format: $VERSION"
  exit 1
fi

# Extract components
MAJOR=$(echo $VERSION | cut -d. -f1)
MINOR=$(echo $VERSION | cut -d. -f2)
PATCH=$(echo $VERSION | cut -d. -f3 | cut -d- -f1)

echo "major=$MAJOR" >> $GITHUB_OUTPUT
echo "minor=$MINOR" >> $GITHUB_OUTPUT
echo "patch=$PATCH" >> $GITHUB_OUTPUT

# Check if prerelease
if [[ $VERSION =~ -(alpha|beta|rc) ]]; then
  echo "is_prerelease=true" >> $GITHUB_OUTPUT
else
  echo "is_prerelease=false" >> $GITHUB_OUTPUT
fi
```

### Version Consistency Check

**Files that must match tag version:**
1. `rebar.config`: `{release, {erlmcp, "VERSION"}}`
2. `apps/erlmcp_core/src/erlmcp_core.app.src`: `{vsn, "VERSION"}`
3. `apps/erlmcp_transports/src/erlmcp_transports.app.src`: `{vsn, "VERSION"}`
4. `apps/erlmcp_observability/src/erlmcp_observability.app.src`: `{vsn, "VERSION"}`
5. `apps/erlmcp_validation/src/erlmcp_validation.app.src`: `{vsn, "VERSION"}`
6. `CHANGELOG.md`: `## [VERSION]` section exists

**Validation:** Fails release if any mismatch.

---

## Part IV: Workflow Separation Strategy

### OSS vs Commercial Workflow Matrix

| Workflow | OSS | Commercial | Rationale |
|----------|-----|------------|-----------|
| **Minimal CI** | ✅ Public | ❌ N/A | Fast contributor feedback |
| **Full CI** | ✅ Public | ✅ Private | OSS: OTP 28, Commercial: OTP 25-28 + enterprise deps |
| **MCP Compliance** | ✅ Public | ✅ Private | Shared spec, enterprise extensions |
| **Benchmark** | ✅ Public | ✅ Private | OSS: Core ops, Commercial: SLA validation |
| **Security Scan** | ✅ Public | ✅ Private | OSS: Basic, Commercial: Enterprise compliance |
| **Release** | ✅ Public | ✅ Private | Separate artifacts (OSS vs Enterprise) |
| **Docker Build** | ✅ Public | ✅ Private | OSS: ghcr.io, Commercial: Private registry |
| **Hex Publish** | ✅ Public | ❌ N/A | Commercial not on Hex.pm |
| **Deploy** | ❌ N/A | ✅ Private | Commercial deployment automation |
| **Multi-Region** | ❌ N/A | ✅ Private | Enterprise feature |
| **SLA Validation** | ❌ N/A | ✅ Private | Commercial guarantee |

### Shared Infrastructure (Both Categories)

**Docker Registry:** `ghcr.io/${{ github.repository }}`
- OSS images: Public, tagged by version
- Commercial images: Private, tagged with `-enterprise` suffix

**Hex.pm Packages:** `https://hex.pm/packages/erlmcp`
- OSS only: erlmcp, erlmcp_core, erlmcp_transports, erlmcp_observability, erlmcp_validation
- Commercial: Not published on Hex.pm (private tarball)

**MCP Compliance:** Shared validators
- Protocol: Same for OSS and Commercial
- Transport: OSS transports (STDIO, TCP, HTTP, WS)
- Enterprise: Additional transports (proprietary)

---

## Part V: Implementation Roadmap

### Phase 1: OSS Workflow Migration (Week 1-2)

**Tasks:**
1. ✅ Create `oss-minimal-ci.yml` (5-10 min PR checks)
2. ✅ Create `oss-full-ci.yml` (30-45 min validation)
3. ✅ Create `oss-release.yml` (automated Hex + GitHub)
4. ✅ Create `oss-quality-gates.yml` (OSS-specific gates)
5. ✅ Update `.github/workflows/README.md` with OSS documentation
6. ✅ Test OSS workflows on feature branches

**Success Criteria:**
- PR checks complete in <10 min
- Full validation on main completes in <45 min
- Release automation works end-to-end

### Phase 2: Commercial Workflow Setup (Week 3-4)

**Tasks:**
1. ✅ Create private repository for commercial workflows
2. ✅ Create `commercial-validation.yml` (enterprise features)
3. ✅ Create `commercial-release.yml` (enterprise release)
4. ✅ Setup self-hosted runners for commercial tests
5. ✅ Configure private Docker registry
6. ✅ Integrate with deployment automation

**Success Criteria:**
- Commercial validation runs in private infrastructure
- Enterprise releases automated
- SLA validation passes (99.9%)

### Phase 3: Quality Gate Enforcement (Week 5-6)

**Tasks:**
1. ✅ Implement OSS quality gate definitions
2. ✅ Implement commercial quality gate definitions
3. ✅ Add branch protection rules (OSS: require minimal CI)
4. ✅ Add main protection rules (OSS: require full CI)
5. ✅ Configure status checks (blocking vs advisory)
6. ✅ Document quality gate bypass process (emergency only)

**Success Criteria:**
- PRs cannot merge without passing minimal CI
- Main cannot accept commits without full CI
- Release blocked if any gate fails

### Phase 4: Release Automation (Week 7-8)

**Tasks:**
1. ✅ Configure automated version extraction
2. ✅ Setup CHANGELOG.md validation
3. ✅ Configure Hex.pm API key (OSS)
4. ✅ Configure GitHub Container Registry (OSS)
5. ✅ Test full release flow (tag → Hex + Docker + GitHub)
6. ✅ Document release process for maintainers

**Success Criteria:**
- Tag push triggers automated release
- Hex.pm packages published automatically
- Docker images built and pushed
- GitHub release created with notes

### Phase 5: Documentation & Handoff (Week 9-10)

**Tasks:**
1. ✅ Update `CONTRIBUTING.md` with OSS workflow info
2. ✅ Create `RELEASE.md` (how to release)
3. ✅ Update `README.md` with CI/CD status badges
4. ✅ Create `.github/ISSUE_TEMPLATE/` for CI failures
5. ✅ Train maintainers on release process
6. ✅ Setup CI/CD monitoring (alerts on failures)

**Success Criteria:**
- Contributors understand PR requirements
- Maintainers can execute releases independently
- CI failures have clear remediation paths

---

## Part VI: CI/CD Metrics & Monitoring

### Key Performance Indicators (KPIs)

| Metric | Target | Measurement |
|--------|--------|-------------|
| **PR Check Time** | <10 min | 95th percentile of workflow duration |
| **Full Validation Time** | <45 min | 95th percentile of workflow duration |
| **Release Automation Success** | >95% | Successful releases / total releases |
| **False Positive Rate** | <5% | Failed gates that shouldn't have |
| **Mean Time to Recovery** | <1 hr | Time from CI failure to fix |

### Dashboards

**GitHub Actions Metrics:**
- Workflow success rate (last 7 days)
- Average workflow duration (by workflow)
- Queue time (time from trigger to start)
- Artifact storage usage

**OSS Health Metrics:**
- Open PRs with failing checks
- PRs passing checks but not merged
- Release frequency (releases per month)
- Contributor onboarding time (first PR to merge)

### Alerting

**Critical Alerts (PagerDuty):**
- OSS release workflow fails for >1 hr
- Commercial release workflow fails for >1 hr
- SLA validation fails (99.9% threshold)
- Security scan finds high-severity vulnerability

**Warning Alerts (Slack):**
- PR check failure rate >10%
- Workflow duration >2x baseline
- Coverage drops below 80%
- MCP compliance drops below 95%

---

## Part VII: Emergency Procedures

### Rollback Procedure

**Scenario:** Broken release published to Hex.pm/Docker

**Steps:**
1. **Immediate:** Disable release (yank from Hex.pm, delete Docker tag)
2. **Notification:** Issue security advisory (GitHub Security tab)
3. **Fix:** Create hotfix branch from previous tag
4. **Release:** Tag new version (PATCH bump)
5. **Verify:** Run full validation + manual testing
6. **Announce:** Publish advisory + migration guide

### Quality Gate Bypass

**Scenario:** Critical bug fix, gate blocking merge

**Process:**
1. **Document:** Create issue explaining bypass rationale
2. **Approve:** Get 2 maintainer approvals
3. **Bypass:** Use GitHub UI "Dismiss review" + comment with issue link
4. **Fix:** Fix gate failure within 24 hr
5. **Retrospective:** Document why gate failed, prevent recurrence

**Allowed Bypass Reasons:**
- Security vulnerability (immediate fix required)
- Critical production bug (customer impact)
- CI infrastructure failure (not code issue)

**Prohibited Bypass:**
- "I'll fix it later" (not acceptable)
- Gate too strict (change gate, don't bypass)
- Deadline pressure (never a valid reason)

---

## Part VIII: Workflow Configuration Examples

### Example 1: Minimal CI Workflow

**File:** `.github/workflows/oss-minimal-ci.yml`

```yaml
name: OSS Minimal CI

on:
  pull_request:
    branches: [main, feature/**, bugfix/**]
    paths:
      - 'apps/**/*.erl'
      - 'apps/**/*.hrl'
      - 'rebar.config'
  push:
    branches: [feature/**, bugfix/**]

concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true

jobs:
  compile:
    name: Compile (OTP 28)
    runs-on: ubuntu-22.04
    timeout-minutes: 10
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
          rebar3-version: '3.25'
      - name: Compile
        run: |
          TERM=dumb rebar3 compile
      - name: Verify apps
        run: |
          for app in erlmcp_core erlmcp_transports erlmcp_observability erlmcp_validation; do
            [ -d "_build/default/lib/$app/ebin" ] || { echo "Missing $app"; exit 1; }
          done

  test-quick:
    name: Unit Tests (Quick)
    runs-on: ubuntu-22.04
    timeout-minutes: 15
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run EUnit
        run: |
          rebar3 as test do compile, eunit

  lint:
    name: Lint
    runs-on: ubuntu-22.04
    timeout-minutes: 10
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Run linter
        run: |
          rebar3 lint

  format:
    name: Format Check
    runs-on: ubuntu-22.04
    timeout-minutes: 5
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Verify format
        run: |
          rebar3 format --verify

  summary:
    name: Summary
    runs-on: ubuntu-22.04
    needs: [compile, test-quick, lint, format]
    if: always()
    steps:
      - name: Check results
        run: |
          if [ "${{ needs.compile.result }}" != "success" ] || \
             [ "${{ needs.test-quick.result }}" != "success" ] || \
             [ "${{ needs.lint.result }}" != "success" ] || \
             [ "${{ needs.format.result }}" != "success" ]; then
            echo "::error::Minimal CI failed"
            exit 1
          fi
          echo "✅ Minimal CI passed"
```

### Example 2: Release Workflow

**File:** `.github/workflows/oss-release.yml`

```yaml
name: OSS Release

on:
  push:
    tags:
      - 'v[0-9]+.[0-9]+.[0-9]+*'
      - 'v[0-9]+.[0-9]+.[0-9]+-*'

permissions:
  contents: write
  packages: write

jobs:
  validate:
    name: Validate Release
    runs-on: ubuntu-22.04
    outputs:
      version: ${{ steps.extract.outputs.version }}
      is_prerelease: ${{ steps.extract.outputs.is_prerelease }}
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - name: Extract version
        id: extract
        run: |
          VERSION=${GITHUB_REF#refs/tags/v}
          echo "version=$VERSION" >> $GITHUB_OUTPUT
          [[ $VERSION =~ -(alpha|beta|rc) ]] && echo "is-prerelease=true" || echo "is-prerelease=false"
      - name: Verify version
        run: |
          VERSION=${{ steps.extract.outputs.version }}
          grep "{release, {erlmcp, \"$VERSION\"}" rebar.config
          grep "## \[$VERSION\]" CHANGELOG.md

  build:
    name: Build Release
    needs: validate
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Build release
        run: |
          rebar3 as prod compile
          rebar3 as prod release
      - name: Create tarball
        run: |
          VERSION=${{ needs.validate.outputs.version }}
          tar -czf erlmcp-${VERSION}.tar.gz -C _build/prod/rel erlmcp
          sha256sum erlmcp-${VERSION}.tar.gz > erlmcp-${VERSION}.tar.gz.sha256
      - uses: actions/upload-artifact@v3
        with:
          name: release-artifacts
          path: |
            erlmcp-${{ needs.validate.outputs.version }}.tar.gz
            erlmcp-${{ needs.validate.outputs.version }}.tar.gz.sha256

  docker:
    name: Build Docker
    needs: validate
    runs-on: ubuntu-22.04
    strategy:
      matrix:
        platform: [linux/amd64, linux/arm64]
    steps:
      - uses: actions/checkout@v4
      - uses: docker/setup-buildx-action@v2
      - uses: docker/login-action@v2
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}
      - uses: docker/build-push-action@v4
        with:
          context: .
          platforms: ${{ matrix.platform }}
          push: true
          tags: |
            ghcr.io/${{ github.repository }}:${{ needs.validate.outputs.version }}
            ghcr.io/${{ github.repository }}:latest
          build-args: |
            VERSION=${{ needs.validate.outputs.version }}

  hex:
    name: Publish to Hex
    needs: [validate, build]
    if: ${{ needs.validate.outputs.is_prerelease == 'false' }}
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: erlef/setup-beam@v1
        with:
          otp-version: '28'
      - name: Publish
        env:
          HEX_API_KEY: ${{ secrets.HEX_API_KEY }}
        run: |
          for app in erlmcp_core erlmcp_transports erlmcp_observability erlmcp_validation; do
            cd apps/$app && rebar3 hex publish --yes && cd ../..
          done
          rebar3 hex publish --yes

  release:
    name: Create Release
    needs: [validate, build, docker]
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/download-artifact@v3
      - name: Generate notes
        id: notes
        run: |
          VERSION=${{ needs.validate.outputs.version }}
          NOTES=$(sed -n "/^## \[$VERSION\]/,/^## \[/p" CHANGELOG.md | sed '$d')
          echo "content<<EOF" >> $GITHUB_OUTPUT
          echo "$NOTES" >> $GITHUB_OUTPUT
          echo "EOF" >> $GITHUB_OUTPUT
      - uses: softprops/action-gh-release@v1
        with:
          tag_name: v${{ needs.validate.outputs.version }}
          name: erlmcp v${{ needs.validate.outputs.version }}
          body: ${{ steps.notes.outputs.content }}
          draft: false
          prerelease: ${{ needs.validate.outputs.is_prerelease }}
          files: |
            release-artifacts/erlmcp-${{ needs.validate.outputs.version }}.tar.gz
            release-artifacts/erlmcp-${{ needs.validate.outputs.version }}.tar.gz.sha256
```

---

## Part IX: Status Badges & Documentation

### README.md Badges

```markdown
# erlmcp

[![OSS CI](https://github.com/${{ repo }}/actions/workflows/oss-minimal-ci.yml/badge.svg)](https://github.com/${{ repo }}/actions/workflows/oss-minimal-ci.yml)
[![Full CI](https://github.com/${{ repo }}/actions/workflows/oss-full-ci.yml/badge.svg)](https://github.com/${{ repo }}/actions/workflows/oss-full-ci.yml)
[![Quality Gates](https://github.com/${{ repo }}/actions/workflows/oss-quality-gates.yml/badge.svg)](https://github.com/${{ repo }}/actions/workflows/oss-quality-gates.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/erlmcp)](https://hex.pm/packages/erlmcp)
[![License: Apache-2.0](https://img.shields.io/badge/License-Apache--2.0-blue.svg)](LICENSE)
[![MCP Compliance](https://img.shields.io/badge/MCP-2025--11--25-green)](https://github.com/${{ repo }}/blob/main/docs/MCP_2025-11-25_COMPLIANCE_UNIFIED.md)

Erlang/OTP SDK for the Model Context Protocol (MCP)
```

### CONTRIBUTING.md Updates

```markdown
## Contributing to erlmcp

### Pull Request Process

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests locally (`make check`)
5. Push to your fork (`git push origin feature/amazing-feature`)
6. Open a pull request

### CI/CD Requirements

All PRs must pass **Minimal CI** checks:
- ✅ Compilation (OTP 28+)
- ✅ Unit tests (≥90% pass rate)
- ✅ Lint (zero critical issues)
- ✅ Format (100% compliant)

Checks complete in **<10 minutes**.

### Merge Requirements

To merge to `main`:
1. PR passes minimal CI (automatic)
2. One maintainer approval (required)
3. Full CI passes on merge (automatic)

### Release Process

Releases are **automated**:
1. Update `CHANGELOG.md`
2. Bump version in `rebar.config` and `*.app.src` files
3. Commit and push to `main`
4. Create tag: `git tag v3.1.0 && git push origin v3.1.0`
5. GitHub Actions will:
   - Build release tarball
   - Publish to Hex.pm
   - Build Docker images (amd64 + arm64)
   - Create GitHub release with notes

See [RELEASE.md](RELEASE.md) for details.
```

---

## Appendix A: Workflow File Inventory

### OSS Workflows (15 files)

| File | Purpose | Duration |
|------|---------|----------|
| `oss-minimal-ci.yml` | PR checks | 5-10 min |
| `oss-full-ci.yml` | Main validation | 30-45 min |
| `oss-quality-gates.yml` | Quality enforcement | 5 min |
| `oss-release.yml` | Release automation | 20-30 min |
| `mcp-compliance.yml` | MCP spec validation | 15-20 min |
| `benchmark.yml` | Performance testing | 10-15 min |
| `chicago-school-tdd.yml` | TDD enforcement | 20-25 min |
| `breaking-changes.yml` | Breaking change detection | 10-15 min |
| `security-scan.yml` | Vulnerability scanning | 10-15 min |
| `dependency-check.yml` | Dependency audit | 5-10 min |
| `docker-build.yml` | Docker image build | 10-15 min |
| `codeql.yml` | CodeQL analysis | 30-45 min |
| `license-check.yml` | License compliance | 5 min |
| `docs-lint.yml` | Documentation linter | 5 min |
| `workspace-health.yml` | Repository health | 5 min |

### Commercial Workflows (15 files, private)

| File | Purpose | Duration |
|------|---------|----------|
| `commercial-validation.yml` | Enterprise features | 45-60 min |
| `commercial-release.yml` | Enterprise release | 30-45 min |
| `multi-region-deploy.yml` | Multi-region testing | 60-90 min |
| `sla-validation.yml` | SLA guarantee tests | 24 hr |
| `enterprise-auth.yml` | OIDC/SAML tests | 20-30 min |
| `compliance-audit.yml` | Compliance reporting | 30-45 min |
| `performance-sla.yml` | Performance baselines | 60 min |
| `chaos-testing.yml` | Failure injection | 45-60 min |
| `disaster-recovery.yml` | DR testing | 90-120 min |
| `customer-deployment.yml` | Customer deployments | 30-45 min |
| `license-enforcement.yml` | License validation | 10 min |
| `security-audit.yml` | Security audit | 60 min |
| `penetration-test.yml` | Penetration testing | 2-4 hr |
| `incident-response.yml` | Incident drills | 60 min |
| `quarterly-review.yml` | Quarterly review | 120 min |

---

## Appendix B: Quality Gate Reference

### OSS Quality Gates (Blocking)

| Gate | Check | Command | Fail Threshold |
|------|-------|---------|----------------|
| **Compilation** | Zero errors | `rebar3 compile` | errors > 0 |
| **Xref** | Zero undefined | `rebar3 xref` | undefined > 0 |
| **Dialyzer** | Zero type errors | `rebar3 dialyzer` | errors > 0 |
| **Unit Tests** | Pass rate | `rebar3 eunit` | < 90% |
| **Coverage** | Code coverage | `rebar3 cover` | < 80% |
| **MCP Compliance** | Spec adherence | `erlmcp_validate` | < 95% |
| **Format** | Code format | `rebar3 format --verify` | != 100% |
| **Lint** | Critical issues | `rebar3 lint` | critical > 0 |

### Commercial Quality Gates (Additional)

| Gate | Check | Command | Fail Threshold |
|------|-------|---------|----------------|
| **License Compliance** | License headers | `grep -r "Copyright"` | missing > 0 |
| **SLA** | 99.9% uptime | `validate_sla.py` | < 99.9% |
| **Multi-Region** | All regions pass | `terraform apply` | any fail |
| **Enterprise Auth** | OIDC/SAML | `rebar3 ct` | any fail |
| **Security Audit** | Zero high/critical | `security scan` | high/critical > 0 |
| **Penetration Test** | No exploitable vulns | `pen test` | exploitable > 0 |

---

## Appendix C: Troubleshooting

### Common CI Failures

**Problem:** Dialyzer fails on OTP 28

**Solution:**
```bash
# Clear PLT and rebuild
rm -rf ~/.cache/rebar3/plts
rebar3 dialyzer --build_plt
```

**Problem:** Coverage fails intermittently

**Solution:**
```bash
# Check for race conditions
rebar3 ct --cover -v
# Review logs for timing issues
```

**Problem:** Docker build fails on arm64

**Solution:**
```yaml
# Use QEMU emulation
- uses: docker/setup-buildx-action@v2
  with:
    platforms: linux/amd64,linux/arm64
```

### Getting Help

- **CI Failures:** Create issue with template `.github/ISSUE_TEMPLATE/ci-failure.md`
- **Release Issues:** Contact maintainers in `#erlmcp-releases` Slack
- **Commercial Issues:** Contact enterprise support

---

## Summary

This CI/CD plan provides:

✅ **OSS-first workflow**: Fast feedback (5-10 min) for contributors
✅ **Separate commercial**: Enterprise features validated privately
✅ **Automated releases**: Tag → Hex + Docker + GitHub
✅ **Quality gates**: Blocking gates enforce standards
✅ **Version tagging**: SemVer with automated extraction
✅ **Monitoring**: KPIs, dashboards, alerting
✅ **Emergency procedures**: Rollback, bypass process
✅ **Documentation**: Badges, contributing guide, release guide

**Next Steps:**
1. Review and approve this plan
2. Create OSS workflow files (Phase 1)
3. Setup commercial infrastructure (Phase 2)
4. Implement quality gates (Phase 3)
5. Enable release automation (Phase 4)
6. Document and handoff (Phase 5)

**Timeline:** 10 weeks to full implementation
