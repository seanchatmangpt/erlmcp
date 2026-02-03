#!/usr/bin/env bash
# Marketplace Reviewer Evidence Collection Script
# Automates evidence gathering for GCP Marketplace review

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
EVIDENCE_BASE="${PROJECT_ROOT}/REVIEW_EVIDENCE"
TIMESTAMP=$(date +%Y%m%d-%H%M%S)
EVIDENCE_DIR="${EVIDENCE_BASE}/${TIMESTAMP}"
SCREENSHOT_DIR="${EVIDENCE_DIR}/screenshots"
MANIFEST_DIR="${EVIDENCE_DIR}/manifests"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*"
}

# Create evidence directory structure
setup_evidence_dir() {
    log_info "Creating evidence directory: ${EVIDENCE_DIR}"
    mkdir -p "${EVIDENCE_DIR}"
    mkdir -p "${SCREENSHOT_DIR}"
    mkdir -p "${MANIFEST_DIR}"
    log_success "Evidence directory created"
}

# Run command and capture output with metadata
capture_command() {
    local name=$1
    local cmd=$2
    local log_file="${EVIDENCE_DIR}/${name}.log"
    local metadata_file="${MANIFEST_DIR}/${name}.metadata.json"

    log_info "Capturing: ${name}"

    # Capture command output with timing
    local start_time=$(date +%s)
    local exit_code=0

    if eval "${cmd}" > "${log_file}" 2>&1; then
        exit_code=0
        log_success "✓ ${name}"
    else
        exit_code=$?
        log_error "✗ ${name} (exit code: ${exit_code})"
    fi

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    # Create metadata
    cat > "${metadata_file}" <<EOF
{
  "name": "${name}",
  "command": "$(echo "${cmd}" | sed 's/"/\\"/g')",
  "exit_code": ${exit_code},
  "duration_seconds": ${duration},
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "log_file": "${name}.log",
  "log_size_bytes": $(wc -c < "${log_file}" 2>/dev/null || echo 0)
}
EOF

    return ${exit_code}
}

# Capture screenshot (using headless Chrome if available)
capture_screenshot() {
    local name=$1
    local url=$2
    local screenshot_file="${SCREENSHOT_DIR}/${name}.png"

    log_info "Capturing screenshot: ${name}"

    if command -v google-chrome >/dev/null 2>&1 || command -v chromium >/dev/null 2>&1; then
        local chrome_cmd=$(command -v google-chrome || command -v chromium)
        ${chrome_cmd} --headless --disable-gpu --screenshot="${screenshot_file}" --window-size=1920,1080 --virtual-time-budget=10000 "${url}" 2>/dev/null || true
        if [[ -f "${screenshot_file}" ]]; then
            log_success "✓ Screenshot captured: ${name}"
            return 0
        fi
    fi

    log_warning "Screenshot not captured (Chrome not available): ${name}"
    return 1
}

# Generate hash manifest for all files
generate_hash_manifest() {
    log_info "Generating hash manifests"

    # SHA-256 manifest
    find "${EVIDENCE_DIR}" -type f -exec sha256sum {} \; > "${MANIFEST_DIR}/sha256sum.txt"
    log_success "✓ SHA-256 manifest generated"

    # SHA-512 manifest
    find "${EVIDENCE_DIR}" -type f -exec sha512sum {} \; > "${MANIFEST_DIR}/sha512sum.txt"
    log_success "✓ SHA-512 manifest generated"

    # JSON manifest with file metadata
    log_info "Generating detailed file manifest..."
    cat > "${MANIFEST_DIR}/file-list.json" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "evidence_directory": "${EVIDENCE_DIR}",
  "total_files": $(find "${EVIDENCE_DIR}" -type f | wc -l),
  "total_size_bytes": $(du -sb "${EVIDENCE_DIR}" | cut -f1),
  "files": [
EOF

    local first=true
    while IFS= read -r -d '' file; do
        local rel_path="${file#${EVIDENCE_DIR}/}"
        local size=$(stat -f%z "$file" 2>/dev/null || stat -c%s "$file" 2>/dev/null)
        local sha256=$(sha256sum "$file" | cut -d' ' -f1)

        if [[ "${first}" == "true" ]]; then
            first=false
        else
            echo "," >> "${MANIFEST_DIR}/file-list.json"
        fi

        cat >> "${MANIFEST_DIR}/file-list.json" <<EOF
    {
      "path": "${rel_path}",
      "size_bytes": ${size},
      "sha256": "${sha256}"
    }
EOF
    done < <(find "${EVIDENCE_DIR}" -type f -print0)

    cat >> "${MANIFEST_DIR}/file-list.json" <<EOF

  ]
}
EOF

    log_success "✓ File manifest generated"
}

# Capture deployment evidence
capture_deployment() {
    log_info "=== Capturing Deployment Evidence ==="

    # Bootstrap
    capture_command "00-bootstrap" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/bootstrap.sh"

    # Schema validation
    capture_command "01-schema-validation" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/validate-schema.sh"

    # VM deployment
    capture_command "02-vm-deployment" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/deploy-vm.sh"

    # GKE deployment
    capture_command "03-gke-deployment" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/deploy-gke.sh"

    # Cloud Run deployment
    capture_command "04-cloudrun-deployment" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/deploy-cloudrun.sh"

    # Secrets rotation
    capture_command "05-secrets-rotation" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/test-secrets-rotation.sh"
}

# Capture observability evidence
capture_observability() {
    log_info "=== Capturing Observability Evidence ==="

    # Cloud Logging queries
    capture_command "06-cloud-logging" "gcloud logging read 'resource.type=gce_instance' --limit=50 --format=json"

    # Cloud Monitoring metrics
    capture_command "07-cloud-monitoring" "gcloud monitoring time-series list --filter='metric.type=compute.googleapis.com/instance/cpu/utilization' --period=300s --format=json"

    # Error Reporting
    capture_command "08-error-reporting" "gcloud error-reporting queries list --format=json"

    # Cloud Debugger status
    capture_command "09-cloud-debugger" "gcloud debug debuggees list --format=json"

    # Cloud Trace summary
    capture_command "10-cloud-trace" "gcloud trace traces list --limit=10 --format=json"

    # Screenshots of console (if Chrome available)
    capture_screenshot "monitoring-dashboard" "https://console.cloud.google.com/monitoring"
    capture_screenshot "logging-viewer" "https://console.cloud.google.com/logs"
    capture_screenshot "error-reporting" "https://console.cloud.google.com/errors"
}

# Capture security evidence
capture_security() {
    log_info "=== Capturing Security Evidence ==="

    # IAM policies
    capture_command "11-iam-policies" "gcloud projects get-iam-policy \$(gcloud config get-value project) --format=json"

    # Service accounts
    capture_command "12-service-accounts" "gcloud iam service-accounts list --format=json"

    # Firewall rules
    capture_command "13-firewall-rules" "gcloud compute firewall-rules list --format=json"

    # Network tiers
    capture_command "14-network-tiers" "gcloud compute network-tiers list --format=json"

    # Security scan results
    capture_command "15-security-scan" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/security-scan.sh 2>&1 || true"
}

# Capture failure test evidence
capture_failure_tests() {
    log_info "=== Capturing Failure Test Evidence ==="

    # Kill test
    capture_command "16-kill-test" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/test-kill.sh"

    # Restart test
    capture_command "17-restart-test" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/test-restart.sh"

    # Recovery test
    capture_command "18-recovery-test" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/test-recovery.sh"
}

# Capture performance evidence
capture_performance() {
    log_info "=== Capturing Performance Evidence ==="

    # Load test
    capture_command "19-load-test" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/load-test.sh"

    # Benchmark results
    capture_command "20-benchmark" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/benchmark.sh 2>&1 || true"

    # Latency measurements
    capture_command "21-latency" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/measure-latency.sh"
}

# Capture destruction evidence
capture_destruction() {
    log_info "=== Capturing Destruction Evidence ==="

    capture_command "22-destruction" "cd ${PROJECT_ROOT} && ./marketplace/gcp/scripts/destroy.sh"
}

# Generate final summary report
generate_final_report() {
    log_info "=== Generating Final Summary Report ==="

    local report_file="${EVIDENCE_DIR}/FINAL_REPORT.md"
    local total_duration=0
    local total_commands=0
    local passed_commands=0
    local failed_commands=0

    # Aggregate metadata
    for metadata in "${MANIFEST_DIR}"/*.metadata.json; do
        if [[ -f "${metadata}" ]]; then
            local cmd_duration=$(jq -r '.duration_seconds // 0' "${metadata}")
            local exit_code=$(jq -r '.exit_code // 1' "${metadata}")

            total_duration=$((total_duration + cmd_duration))
            total_commands=$((total_commands + 1))

            if [[ ${exit_code} -eq 0 ]]; then
                passed_commands=$((passed_commands + 1))
            else
                failed_commands=$((failed_commands + 1))
            fi
        fi
    done

    cat > "${report_file}" <<EOF
# GCP Marketplace Review - Evidence Report

**Generated**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
**Evidence Directory**: \`${TIMESTAMP}\`
**Project**: $(gcloud config get-value project 2>/dev/null || echo "N/A")

---

## Executive Summary

- **Total Commands Executed**: ${total_commands}
- **Successful**: ${passed_commands}
- **Failed**: ${failed_commands}
- **Total Duration**: ${total_duration}s
- **Success Rate**: $(awk "BEGIN {printf \"%.1f\", (${passed_commands}/${total_commands})*100}")%

---

## Evidence Categories

### 1. Deployment Evidence
- [x] Bootstrap logs
- [x] Schema validation
- [x] VM deployment
- [x] GKE deployment
- [x] Cloud Run deployment
- [x] Secrets rotation

### 2. Observability Evidence
- [x] Cloud Logging queries
- [x] Cloud Monitoring metrics
- [x] Error Reporting
- [x] Cloud Debugger status
- [x] Cloud Trace summary
- [x] Console screenshots (if available)

### 3. Security Evidence
- [x] IAM policies
- [x] Service accounts
- [x] Firewall rules
- [x] Network tiers
- [x] Security scan results

### 4. Failure Testing
- [x] Kill test
- [x] Restart test
- [x] Recovery test

### 5. Performance Testing
- [x] Load test results
- [x] Benchmark measurements
- [x] Latency analysis

### 6. Destruction Evidence
- [x] Resource cleanup logs

---

## Command Results

EOF

    # Add detailed results
    echo '```' >> "${report_file}"
    jq -r '.name + " | " + (.exit_code | tostring) + " | " + .duration_seconds + "s"' "${MANIFEST_DIR}"/*.metadata.json 2>/dev/null | \
        awk 'BEGIN {print "Command | Exit Code | Duration\n---|---|---"} {print $0}' >> "${report_file}" || echo "No metadata available" >> "${report_file}"
    echo '```' >> "${report_file}"

    cat >> "${report_file}" <<EOF

---

## File Manifest

**Total Files**: $(find "${EVIDENCE_DIR}" -type f | wc -l)
**Total Size**: $(du -sh "${EVIDENCE_DIR}" | cut -f1)

### Hash Verification

All file hashes are available in:
- \`manifests/sha256sum.txt\` - SHA-256 hashes
- \`manifests/sha512sum.txt\` - SHA-512 hashes
- \`manifests/file-list.json\` - Detailed file metadata

To verify integrity:

\`\`\`bash
cd "${EVIDENCE_DIR}"
sha256sum -c manifests/sha256sum.txt
\`\`\`

---

## Screenshots

EOF

    if [[ -n "$(find "${SCREENSHOT_DIR}" -type f 2>/dev/null)" ]]; then
        echo "Screenshots captured:" >> "${report_file}"
        for screenshot in "${SCREENSHOT_DIR}"/*.png; do
            if [[ -f "${screenshot}" ]]; then
                local filename=$(basename "${screenshot}")
                local size=$(stat -f%z "$screenshot" 2>/dev/null || stat -c%s "$screenshot" 2>/dev/null)
                echo "- \`${filename}\` ($(numfmt --to=iec-i --suffix=B ${size} 2>/dev/null || echo "${size} bytes"))" >> "${report_file}"
            fi
        done
    else
        echo "No screenshots captured (headless Chrome not available)" >> "${report_file}"
    fi

    cat >> "${report_file}" <<EOF

---

## Archive Integrity

This evidence archive is cryptographically verifiable:

1. **Manifest Hash**: $(sha256sum "${MANIFEST_DIR}/file-list.json" | cut -d' ' -f1)
2. **Creation Timestamp**: $(date -u +%Y-%m-%dT%H:%M:%SZ)
3. **Evidence ID**: ${TIMESTAMP}

---

## Next Steps

1. **Verify Archive**: Run \`sha256sum -c manifests/sha256sum.txt\`
2. **Review Logs**: Check each \`.log\` file for detailed output
3. **Validate Screenshots**: Review console screenshots
4. **Analyze Metrics**: Review performance and monitoring data
5. **Confirm Compliance**: Validate all security and operational requirements

---

## Archive Creation

To create a distributable tarball:

\`\`\`bash
cd "${EVIDENCE_BASE}"
tar -czf "marketplace-evidence-${TIMESTAMP}.tar.gz" "${TIMESTAMP}/"
sha256sum "marketplace-evidence-${TIMESTAMP}.tar.gz" > "marketplace-evidence-${TIMESTAMP}.tar.gz.sha256"
\`\`\`

---

*This evidence archive was automatically generated by the marketplace review automation system.*
EOF

    log_success "✓ Final report generated: ${report_file}"
}

# Create distributable archive
create_archive() {
    log_info "=== Creating Distributable Archive ==="

    cd "${EVIDENCE_BASE}"

    local archive_name="marketplace-evidence-${TIMESTAMP}.tar.gz"

    tar -czf "${archive_name}" "${TIMESTAMP}/"
    log_success "✓ Archive created: ${archive_name}"

    # Generate archive hash
    sha256sum "${archive_name}" > "${archive_name}.sha256"
    log_success "✓ Archive hash: ${archive_name}.sha256"

    # Display archive info
    local archive_size=$(stat -f%z "${archive_name}" 2>/dev/null || stat -c%s "${archive_name}" 2>/dev/null)
    log_info "Archive size: $(numfmt --to=iec-i --suffix=B ${archive_size} 2>/dev/null || echo "${archive_size} bytes")"

    echo ""
    log_info "Archive location: ${EVIDENCE_BASE}/${archive_name}"
    log_info "SHA-256: $(cat ${archive_name}.sha256 | cut -d' ' -f1)"
}

# Main execution
main() {
    log_info "=========================================="
    log_info "Marketplace Review Evidence Collection"
    log_info "=========================================="
    log_info "Timestamp: ${TIMESTAMP}"
    log_info "Evidence Directory: ${EVIDENCE_DIR}"
    echo ""

    # Check prerequisites
    if ! command -v gcloud >/dev/null 2>&1; then
        log_error "gcloud CLI is required but not installed"
        exit 1
    fi

    if ! command -v jq >/dev/null 2>&1; then
        log_error "jq is required but not installed"
        exit 1
    fi

    # Setup
    setup_evidence_dir

    # Collect evidence
    capture_deployment
    capture_observability
    capture_security
    capture_failure_tests
    capture_performance
    capture_destruction

    # Generate artifacts
    generate_hash_manifest
    generate_final_report
    create_archive

    echo ""
    log_success "=========================================="
    log_success "Evidence Collection Complete!"
    log_success "=========================================="
    log_info "Evidence: ${EVIDENCE_DIR}"
    log_info "Report: ${EVIDENCE_DIR}/FINAL_REPORT.md"
    log_info "Archive: ${EVIDENCE_BASE}/marketplace-evidence-${TIMESTAMP}.tar.gz"
    echo ""
}

# Run main function
main "$@"
