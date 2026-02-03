#!/bin/bash
# ============================================================================
# Evidence Summary Generator
# Generates a comprehensive summary of build evidence
# ============================================================================

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# ============================================================================
# Configuration
# ============================================================================

BUILD_ID="${BUILD_ID:-$(date +%s)}"
PROJECT_ID="${PROJECT_ID:-}"
OUTPUT_FILE="${OUTPUT_FILE:-SUMMARY.md}"
EVIDENCE_DIR="${EVIDENCE_DIR:-/workspace/build-evidence}"

# ============================================================================
# Generate Summary
# ============================================================================

generate_summary() {
    log_info "Generating evidence summary for build $BUILD_ID..."

    cat > "$OUTPUT_FILE" << EOF
# GCP Marketplace Build Evidence Summary

**Build ID:** \`${BUILD_ID}\`
**Generated:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")
**Project:** ${PROJECT_ID:-"N/A"}

---

## Executive Summary

This document provides a comprehensive summary of all evidence collected during the GCP Marketplace validation build.

---

## Build Information

| Field | Value |
|-------|-------|
| Build ID | \`${BUILD_ID}\` |
| Project ID | \`${PROJECT_ID:-"N/A"}\` |
| Timestamp | $(date -u +"%Y-%m-%dT%H:%M:%SZ") |
| Evidence Location | \`gs://${PROJECT_ID:-"PROJECT_ID"}-marketplace-evidence/${BUILD_ID}/\` |

---

## Security Scan Results

### Vulnerability Summary

EOF

    # Parse Trivy results if available
    if [ -f "$EVIDENCE_DIR/security/trivy-report.json" ]; then
        TOTAL_VULNS=$(jq '[.Results[].Vulnerabilities // []] | add | length' "$EVIDENCE_DIR/security/trivy-report.json" 2>/dev/null || echo "0")
        HIGH_VULNS=$(jq '[.Results[].Vulnerabilities // []] | add | [.[] | select(.Severity == "HIGH")] | length' "$EVIDENCE_DIR/security/trivy-report.json" 2>/dev/null || echo "0")
        CRITICAL_VULNS=$(jq '[.Results[].Vulnerabilities // []] | add | [.[] | select(.Severity == "CRITICAL")] | length' "$EVIDENCE_DIR/security/trivy-report.json" 2>/dev/null || echo "0")
        MEDIUM_VULNS=$(jq '[.Results[].Vulnerabilities // []] | add | [.[] | select(.Severity == "MEDIUM")] | length' "$EVIDENCE_DIR/security/trivy-report.json" 2>/dev/null || echo "0")
        LOW_VULNS=$(jq '[.Results[].Vulnerabilities // []] | add | [.[] | select(.Severity == "LOW")] | length' "$EVIDENCE_DIR/security/trivy-report.json" 2>/dev/null || echo "0")

        cat >> "$OUTPUT_FILE" << EOF
| Severity | Count | Status |
|----------|-------|--------|
| CRITICAL | $CRITICAL_VULNS | $([ "$CRITICAL_VULNS" -eq 0 ] && echo "✓ PASS" || echo "✗ FAIL") |
| HIGH | $HIGH_VULNS | $([ "$HIGH_VULNS" -eq 0 ] && echo "✓ PASS" || echo "⚠ WARNING") |
| MEDIUM | $MEDIUM_VULNS | - |
| LOW | $LOW_VULNS | - |
| **Total** | **$TOTAL_VULNS** | - |

EOF
    else
        cat >> "$OUTPUT_FILE" << EOF
| Severity | Count | Status |
|----------|-------|--------|
| CRITICAL | N/A | No scan data |
| HIGH | N/A | No scan data |
| TOTAL | N/A | No scan data |

EOF
    fi

    cat >> "$OUTPUT_FILE" << EOF
### Security Artifacts

- \`security/trivy-report.json\` - Full Trivy scan results
- \`security/trivy-report.txt\` - Human-readable Trivy report
- \`security/gcp-scan.json\` - GCP Container Analysis results
- \`security/secret-scan.json\` - Secret scan results
- \`security/git-secret-scan.txt\` - Git history secret scan

---

## SBOM (Software Bill of Materials)

EOF

    # SBOM information
    if [ -f "$EVIDENCE_DIR/security/sbom-cyclonedx.json" ]; then
        PACKAGE_COUNT=$(jq '.metadata.component.properties | .["total-dependencies"]' "$EVIDENCE_DIR/security/sbom-cyclonedx.json" 2>/dev/null || echo "N/A")
        cat >> "$OUTPUT_FILE" << EOF
- **Total Packages:** $PACKAGE_COUNT
- **Format:** CycloneDX + SPDX
- **Files:**
  - \`security/sbom-cyclonedx.json\` - CycloneDX SBOM
  - \`security/sbom-spdx.json\` - SPDX SBOM
  - \`security/sbom-summary.txt\` - Human-readable summary

EOF
    else
        cat >> "$OUTPUT_FILE" << EOF
No SBOM data available.

EOF
    fi

    cat >> "$OUTPUT_FILE" << EOF
---

## Validation Results

### Terraform Validation

EOF

    # Terraform validation results
    TF_PASSED=0
    TF_FAILED=0
    if [ -d "$EVIDENCE_DIR/validation/terraform" ]; then
        for log in "$EVIDENCE_DIR/validation/terraform"/*-validate.log; do
            if [ -f "$log" ]; then
                if grep -q "Success! The configuration is valid" "$log" 2>/dev/null; then
                    TF_PASSED=$((TF_PASSED + 1))
                else
                    TF_FAILED=$((TF_FAILED + 1))
                fi
            fi
        done
    fi

    cat >> "$OUTPUT_FILE" << EOF
| Metric | Count |
|--------|-------|
| Modules Validated | $((TF_PASSED + TF_FAILED)) |
| Passed | $TF_PASSED |
| Failed | $TF_FAILED |

### Schema Validation

EOF

    # Schema validation results
    SCHEMA_STATUS="✓ PASSED"
    if [ -f "$EVIDENCE_DIR/validation/schema-validation.log" ]; then
        if grep -q "failed\|error" "$EVIDENCE_DIR/validation/schema-validation.log" 2>/dev/null; then
            SCHEMA_STATUS="✗ FAILED"
        fi
    fi

    cat >> "$OUTPUT_FILE" << EOF
- Marketplace Schema: $SCHEMA_STATUS

### Helm Chart Lint

EOF

    # Helm lint results
    HELM_STATUS="✓ PASSED"
    if [ -f "$EVIDENCE_DIR/validation/helm-lint.log" ]; then
        if grep -q "ERROR\|FAILED" "$EVIDENCE_DIR/validation/helm-lint.log" 2>/dev/null; then
            HELM_STATUS="✗ FAILED"
        fi
    fi

    cat >> "$OUTPUT_FILE" << EOF
- Helm Chart: $HELM_STATUS

### Packer Validation

EOF

    # Packer validation results
    PACKER_STATUS="⊘ SKIPPED"
    if [ -f "$EVIDENCE_DIR/validation/packer-validate.log" ]; then
        if grep -q "The configuration is valid" "$EVIDENCE_DIR/validation/packer-validate.log" 2>/dev/null; then
            PACKER_STATUS="✓ PASSED"
        else
            PACKER_STATUS="✗ FAILED"
        fi
    fi

    cat >> "$OUTPUT_FILE" << EOF
- Packer Templates: $PACKER_STATUS

---

## Deployment Test Results

EOF

    # Deployment test results
    if [ -d "$EVIDENCE_DIR/deployment-tests" ]; then
        for test_type in cloudrun gke gce; do
            if [ -d "$EVIDENCE_DIR/deployment-tests/$test_type" ]; then
                TEST_LOG="$EVIDENCE_DIR/deployment-tests/$test_type/deploy.log"
                TEST_STATUS="⊘ NOT RUN"

                if [ -f "$TEST_LOG" ]; then
                    if grep -q "passed\|success" "$TEST_LOG" 2>/dev/null; then
                        TEST_STATUS="✓ PASSED"
                    elif grep -q "failed\|error" "$TEST_LOG" 2>/dev/null; then
                        TEST_STATUS="✗ FAILED"
                    fi
                fi

                TEST_NAME="${test_type^^}"
                cat >> "$OUTPUT_FILE" << EOF
- $TEST_NAME Test: $TEST_STATUS
EOF
            fi
        done
    else
        cat >> "$OUTPUT_FILE" << EOF
No deployment tests were run (skipped or no evidence directory).

EOF
    fi

    cat >> "$OUTPUT_FILE" << EOF
---

## Build Receipt

EOF

    # Include receipt if available
    if [ -f "$EVIDENCE_DIR/receipts/build-receipt.json" ]; then
        cat >> "$OUTPUT_FILE" << EOF
A signed build receipt is available at \`receipts/build-receipt.json\`.

EOF
    fi

    cat >> "$OUTPUT_FILE" << EOF
---

## Evidence Package Contents

\`\`\`
$EVIDENCE_DIR/
├── build-info.json              # Build metadata
├── image-digest.txt             # Container image digest
├── SUMMARY.md                   # This summary
├── security/                    # Security scan artifacts
│   ├── trivy-report.json       # Trivy scan results
│   ├── trivy-report.txt        # Trivy human-readable report
│   ├── gcp-scan.json           # GCP Container Analysis
│   ├── secret-scan.json        # Container secret scan
│   ├── git-secret-scan.txt     # Git history secret scan
│   ├── sbom-cyclonedx.json     # CycloneDX SBOM
│   ├── sbom-spdx.json          # SPDX SBOM
│   └── vuln-summary.txt        # Vulnerability summary
├── validation/                  # Validation artifacts
│   ├── terraform/              # Terraform validation logs
│   ├── schema-validation.log   # Schema validation results
│   ├── helm-lint.log           # Helm lint results
│   ├── packer-init.log         # Packer initialization
│   └── packer-validate.log     # Packer validation
├── deployment-tests/            # Deployment test results
│   ├── cloudrun/               # Cloud Run test artifacts
│   ├── gke/                    # GKE test artifacts
│   └── gce/                    # Compute Engine test artifacts
└── receipts/                    # Build receipts
    ├── build-receipt.json      # Build receipt data
    └── build-receipt.sig       # KMS signature (if signed)
\`\`\`

---

## Verification

To verify this build:

\`\`\`bash
# Download evidence package
gsutil -m cp -r gs://${PROJECT_ID:-"PROJECT_ID"}-marketplace-evidence/${BUILD_ID}/ ./evidence/

# Verify image digest
docker inspect REGION-docker.pkg.dev/${PROJECT_ID:-"PROJECT_ID"}/${_IMAGE_NAME:-"erlmcp/erlmcp"}:${_IMAGE_TAG:-"tag"} \\
  --format='{{index .RepoDigests 0}}'

# Verify receipt signature (if signed)
gcloud kms asymmetric-verify \\
  --project ${PROJECT_ID:-"PROJECT_ID"} \\
  --location global \\
  --keyring cloudbuild \\
  --key build-signing-key \\
  --version 1 \\
  --signature-file evidence/receipts/build-receipt.sig \\
  --digest-file evidence/receipts/build-receipt.json
\`\`\`

---

## Marketplace Readiness Checklist

- [ ] No CRITICAL vulnerabilities
- [ ] No HIGH vulnerabilities (or documented exceptions)
- [ ] All Terraform modules validate
- [ ] Marketplace schema validates
- [ ] Helm chart lints without errors
- [ ] Deployment tests pass (if run)
- [ ] Evidence package is complete
- [ ] Build receipt is signed (if KMS enabled)

---

## Next Steps

1. **Review Security Scan Results:** Check for vulnerabilities that need remediation
2. **Address Validation Failures:** Fix any Terraform or schema validation issues
3. **Run Deployment Tests:** Execute full deployment tests for final validation
4. **Generate Marketplace Package:** Create the final marketplace deployment package
5. **Submit to Marketplace:** Submit the solution to Google Cloud Marketplace

---

*This summary was automatically generated by the erlmcp Marketplace CI/CD Pipeline.*
EOF

    log_info "Summary generated: $OUTPUT_FILE"
}

# ============================================================================
# Main
# ============================================================================

main() {
    generate_summary
    cat "$OUTPUT_FILE"
}

main "$@"
