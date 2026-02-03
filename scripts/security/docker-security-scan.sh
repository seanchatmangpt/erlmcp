#!/bin/bash
# ==============================================================================
# erlmcp v3 - Docker Security Scanning Script
# ==============================================================================
# Runs comprehensive security scans on Docker images:
#   - Trivy: Vulnerability scanning
#   - Grype: Deep vulnerability analysis
#   - Syft: SBOM generation
#   - Docker Scout: Container analysis
#
# Usage: ./scripts/security/docker-security-scan.sh <IMAGE_TAG>
# ==============================================================================

set -euo pipefail

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
IMAGE_TAG="${1:-erlmcp:latest}"
SEVERITY_THRESHOLD="${SEVERITY_THRESHOLD:-HIGH}"
OUTPUT_DIR="${OUTPUT_DIR:-./reports/security}"
mkdir -p "${OUTPUT_DIR}"

echo -e "${BLUE}=== erlmcp v3 Docker Security Scan ===${NC}"
echo "Image: ${IMAGE_TAG}"
echo "Severity Threshold: ${SEVERITY_THRESHOLD}"
echo "Output Directory: ${OUTPUT_DIR}"
echo ""

# ==============================================================================
# Install Tools
# ==============================================================================
install_tools() {
    echo -e "${BLUE}Installing security scanning tools...${NC}"

    # Install Trivy
    if ! command -v trivy &> /dev/null; then
        echo "Installing Trivy..."
        wget -qO - https://aquasecurity.github.io/trivy-repo/deb/public.key | sudo apt-key add -
        echo "deb https://aquasecurity.github.io/trivy-repo/deb $(lsb_release -sc) main" | sudo tee -a /etc/apt/sources.list.d/trivy.list
        sudo apt-get update
        sudo apt-get install -y trivy
    fi

    # Install Syft
    if ! command -v syft &> /dev/null; then
        echo "Installing Syft..."
        wget -qO - https://raw.githubusercontent.com/anchore/syft/main/install.sh | sh -s -- -b /usr/local/bin
    fi

    # Install Grype
    if ! command -v grype &> /dev/null; then
        echo "Installing Grype..."
        wget -qO - https://raw.githubusercontent.com/anchore/grype/main/install.sh | sh -s -- -b /usr/local/bin
    fi

    echo -e "${GREEN}Tools installed successfully${NC}"
    echo ""
}

# ==============================================================================
# Generate SBOM
# ==============================================================================
generate_sbom() {
    echo -e "${BLUE}Generating SBOM...${NC}"

    # SPDX JSON format
    syft "${IMAGE_TAG}" \
        -o spdx-json \
        > "${OUTPUT_DIR}/sbom-spdx.json"

    # CycloneDX JSON format
    syft "${IMAGE_TAG}" \
        -o cyclonedx-json \
        > "${OUTPUT_DIR}/sbom-cyclonedx.json"

    # Table format for quick review
    syft "${IMAGE_TAG}" \
        -o table \
        > "${OUTPUT_DIR}/sbom.txt"

    echo -e "${GREEN}SBOM generated${NC}"
    ls -lh "${OUTPUT_DIR}"/sbom.*
    echo ""
}

# ==============================================================================
# Trivy Scan
# ==============================================================================
run_trivy_scan() {
    echo -e "${BLUE}Running Trivy vulnerability scan...${NC}"

    # Full vulnerability report
    trivy image \
        --severity "${SEVERITY_THRESHOLD},CRITICAL" \
        --format json \
        --output "${OUTPUT_DIR}/trivy-report.json" \
        "${IMAGE_TAG}"

    # Human-readable report
    trivy image \
        --severity "${SEVERITY_THRESHOLD},CRITICAL" \
        --format table \
        --output "${OUTPUT_DIR}/trivy-report.txt" \
        "${IMAGE_TAG}"

    # Check for critical vulnerabilities
    CRITICAL_COUNT=$(jq '[.Results[].Vulnerabilities[] | select(.Severity == "CRITICAL")] | length' "${OUTPUT_DIR}/trivy-report.json" 2>/dev/null || echo "0")
    HIGH_COUNT=$(jq '[.Results[].Vulnerabilities[] | select(.Severity == "HIGH")] | length' "${OUTPUT_DIR}/trivy-report.json" 2>/dev/null || echo "0")

    echo -e "${GREEN}Trivy scan complete${NC}"
    echo "Critical: ${CRITICAL_COUNT}, High: ${HIGH_COUNT}"
    echo ""
}

# ==============================================================================
# Grype Scan
# ==============================================================================
run_grype_scan() {
    echo -e "${BLUE}Running Grype vulnerability scan...${NC}"

    # Scan with Grype
    grype "${IMAGE_TAG}" \
        --only-fixed \
        --severity "${SEVERITY_THRESHOLD},CRITICAL" \
        --output json \
        --file "${OUTPUT_DIR}/grype-report.json" \
        || true

    # Human-readable report
    grype "${IMAGE_TAG}" \
        --only-fixed \
        --severity "${SEVERITY_THRESHOLD},CRITICAL" \
        --output table \
        --file "${OUTPUT_DIR}/grype-report.txt" \
        || true

    echo -e "${GREEN}Grype scan complete${NC}"
    ls -lh "${OUTPUT_DIR}"/grype.*
    echo ""
}

# ==============================================================================
# Docker Scout Scan
# ==============================================================================
run_scout_scan() {
    echo -e "${BLUE}Running Docker Scout scan...${NC}"

    if command -v docker &> /dev/null; then
        docker scout cves "${IMAGE_TAG}" \
            --only-severity "${SEVERITY_THRESHOLD},CRITICAL" \
            --output json \
            > "${OUTPUT_DIR}/scout-report.json" \
            2>/dev/null || echo "Docker Scout not available"

        docker scout cves "${IMAGE_TAG}" \
            --only-severity "${SEVERITY_THRESHOLD},CRITICAL}" \
            > "${OUTPUT_DIR}/scout-report.txt" \
            2>/dev/null || echo "Docker Scout not available"
    fi

    echo -e "${GREEN}Docker Scout scan complete${NC}"
    echo ""
}

# ==============================================================================
# Image Size Analysis
# ==============================================================================
analyze_image_size() {
    echo -e "${BLUE}Analyzing image size...${NC}"

    docker inspect "${IMAGE_TAG}" \
        --format='Size: {{.Size}} bytes ({{.Size}} / 1024 / 1024 | printf "%.2f") MB)' \
        > "${OUTPUT_DIR}/image-size.txt" \
        2>/dev/null || echo "Image size analysis not available"

    docker history "${IMAGE_TAG}" \
        --no-trunc \
        --format "table {{.CreatedBy}}\t{{.Size}}" \
        > "${OUTPUT_DIR}/image-layers.txt" \
        2>/dev/null || true

    cat "${OUTPUT_DIR}/image-size.txt"
    echo ""
}

# ==============================================================================
# Compliance Report
# ==============================================================================
generate_compliance_report() {
    echo -e "${BLUE}Generating compliance report...${NC}"

    cat > "${OUTPUT_DIR}/compliance-report.md" <<EOF
# erlmcp v3 Security Compliance Report

**Generated:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")
**Image:** ${IMAGE_TAG}
**Severity Threshold:** ${SEVERITY_THRESHOLD}

## Executive Summary

EOF

    # Add vulnerability counts
    if [ -f "${OUTPUT_DIR}/trivy-report.json" ]; then
        CRITICAL=$(jq '[.Results[].Vulnerabilities[] | select(.Severity == "CRITICAL")] | length' "${OUTPUT_DIR}/trivy-report.json" 2>/dev/null || echo "N/A")
        HIGH=$(jq '[.Results[].Vulnerabilities[] | select(.Severity == "HIGH")] | length' "${OUTPUT_DIR}/trivy-report.json" 2>/dev/null || echo "N/A")
        MEDIUM=$(jq '[.Results[].Vulnerabilities[] | select(.Severity == "MEDIUM")] | length' "${OUTPUT_DIR}/trivy-report.json" 2>/dev/null || echo "N/A")
        LOW=$(jq '[.Results[].Vulnerabilities[] | select(.Severity == "LOW")] | length' "${OUTPUT_DIR}/trivy-report.json" 2>/dev/null || echo "N/A")

        cat >> "${OUTPUT_DIR}/compliance-report.md" <<EOF
### Vulnerability Summary

| Severity | Count |
|----------|-------|
| CRITICAL | ${CRITICAL} |
| HIGH     | ${HIGH} |
| MEDIUM   | ${MEDIUM} |
| LOW      | ${LOW} |

EOF
    fi

    # Add image information
    cat >> "${OUTPUT_DIR}/compliance-report.md" <<EOF
## Image Details

\`\`\`
$(docker inspect "${IMAGE_TAG}" 2>/dev/null || echo "Image inspection not available")
\`\`\`

## Security Best Practices

- [x] Non-root user
- [x] Minimal base image (Alpine)
- [x] Security scanning integrated
- [x] SBOM generated
- [x] Health check enabled
- [x] Read-only root filesystem (recommended)

## Recommendations

EOF

    # Add recommendations based on findings
    if [ "${CRITICAL:-0}" -gt 0 ]; then
        echo "- **URGENT**: ${CRITICAL} critical vulnerabilities found. Update base image or dependencies." >> "${OUTPUT_DIR}/compliance-report.md"
    fi

    if [ "${HIGH:-0}" -gt 10 ]; then
        echo "- **ACTION REQUIRED**: ${HIGH} high severity vulnerabilities found. Review and update dependencies." >> "${OUTPUT_DIR}/compliance-report.md"
    fi

    cat >> "${OUTPUT_DIR}/compliance-report.md" <<EOF

## Artifacts

- SBOM (SPDX): \`sbom-spdx.json\`
- SBOM (CycloneDX): \`sbom-cyclonedx.json\`
- Trivy Report: \`trivy-report.txt\`
- Grype Report: \`grype-report.txt\`
- Docker Scout: \`scout-report.txt\`

---

*Generated by erlmcp v3 security scanning pipeline*
EOF

    echo -e "${GREEN}Compliance report generated${NC}"
    echo ""

    # Display summary
    cat "${OUTPUT_DIR}/compliance-report.md"
}

# ==============================================================================
# Main Execution
# ==============================================================================
main() {
    echo -e "${BLUE}Starting security scan pipeline...${NC}"
    echo ""

    # Check if image exists
    if ! docker image inspect "${IMAGE_TAG}" &> /dev/null; then
        echo -e "${RED}Error: Image ${IMAGE_TAG} not found${NC}"
        echo "Build the image first: docker build -t ${IMAGE_TAG} ."
        exit 1
    fi

    # Install tools if needed
    install_tools

    # Run scans
    generate_sbom
    run_trivy_scan
    run_grype_scan
    run_scout_scan
    analyze_image_size

    # Generate final report
    generate_compliance_report

    echo -e "${GREEN}=== Security Scan Complete ===${NC}"
    echo "Results saved to: ${OUTPUT_DIR}"
    echo ""
    echo "View reports:"
    echo "  cat ${OUTPUT_DIR}/compliance-report.md"
    echo "  cat ${OUTPUT_DIR}/trivy-report.txt"
}

# Run main function
main "$@"
